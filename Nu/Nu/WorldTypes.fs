// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Debug
open System
module internal World =

#if DEBUG
    let mutable internal Frozen = 0
    let mutable internal Chosen = obj ()
#endif
    let mutable internal viewGame = fun (_ : obj) -> Array.create 0 (String.Empty, obj ())
    let mutable internal viewScreen = fun (_ : obj) (_ : obj) -> Array.create 0 (String.Empty, obj ())
    let mutable internal viewLayer = fun (_ : obj) (_ : obj) -> Array.create 0 (String.Empty, obj ())
    let mutable internal viewEntity = fun (_ : obj) (_ : obj) -> Array.create 0 (String.Empty, obj ())

namespace Nu
open System
open System.Collections.Generic
#if DEBUG
open System.Diagnostics
#endif
open TiledSharp
open Prime
open Nu

/// Describes a Tiled tile.
type [<StructuralEquality; NoComparison>] TileDescriptor =
    { Tile : TmxLayerTile
      I : int
      J : int
      Gid : int
      GidPosition : int
      Gid2 : Vector2i
      TileSetTileOpt : TmxTilesetTile option
      TilePosition : Vector2i }

/// Describes a Tiled tile map.
type [<StructuralEquality; NoComparison>] TileMapDescriptor =
    { TileMap : TmxMap
      TileSizeI : Vector2i
      TileSizeF : Vector2
      TileMapSizeM : Vector2i
      TileMapSizeI : Vector2i
      TileMapSizeF : Vector2
      TileSet : TmxTileset
      TileSetSize : Vector2i }

/// The type of a screen transition. Incoming means a new screen is being shown, and Outgoing
/// means an existing screen being hidden.
type TransitionType =
    | Incoming
    | Outgoing

/// The state of a screen's transition.
type TransitionState =
    | IncomingState
    | OutgoingState
    | IdlingState

/// Describes one of a screen's transition processes.
type [<StructuralEquality; NoComparison; CLIMutable>] Transition =
    { TransitionType : TransitionType
      TransitionLifetime : int64
      DissolveImageOpt : Image AssetTag option
      SongOpt : SongDescriptor option }

    /// Make a screen transition.
    static member make transitionType =
        { TransitionType = transitionType
          TransitionLifetime = 0L
          DissolveImageOpt = None
          SongOpt = None }

/// Describes the behavior of the screen dissolving algorithm.
type [<StructuralEquality; NoComparison>] DissolveDescriptor =
    { IncomingTime : int64
      OutgoingTime : int64
      DissolveImage : Image AssetTag }

/// Describes the behavior of the screen splash algorithm.
type [<StructuralEquality; NoComparison>] SplashDescriptor =
    { DissolveDescriptor : DissolveDescriptor
      IdlingTime : int64
      SplashImage : Image AssetTag }

/// Describes the shape of a desired overlay.
type [<StructuralEquality; StructuralComparison>] OverlayNameDescriptor =
    | NoOverlay
    | RoutedOverlay
    | DefaultOverlay
    | ExplicitOverlay of string

/// Describes the origin of a piece of simulnat content.
type [<StructuralEquality; NoComparison>] ContentOrigin =
    | SimulantOrigin of Simulant
    | FacetOrigin of Simulant * string

/// Describes the content of a simulant.
type SimulantContent =
    interface end

/// Specifies that a module contains functions that need to be considered for binding generation.
type [<AttributeUsage (AttributeTargets.Class); AllowNullLiteral>]
    ModuleBindingAttribute () =
    inherit Attribute ()

/// Specifies that a module contains functions that need to be considered for binding generation.
type [<AttributeUsage (AttributeTargets.Method); AllowNullLiteral>]
    FunctionBindingAttribute (bindingName : string) =
    inherit Attribute ()
    member this.BindingName = bindingName
    new () = FunctionBindingAttribute ""

/// Configuration parameters for Nu.
type [<StructuralEquality; NoComparison>] NuConfig =
    { RunSynchronously : bool }

    /// The default configuration for Nu.
    static member defaultConfig =
        { RunSynchronously = false }

/// Configuration parameters for the world.
type [<StructuralEquality; NoComparison>] WorldConfig =
    { NuConfig : NuConfig
      SdlConfig : SdlConfig
      TickRate : int64
      StandAlone : bool }

    /// The default configuration of the world.
    static member defaultConfig =
        { NuConfig = NuConfig.defaultConfig
          SdlConfig = SdlConfig.defaultConfig
          TickRate = 1L
          StandAlone = true }

[<AutoOpen>]
module WorldTypes =

    // Property category reach-arounds.
    let mutable internal getPropertyOpt = Unchecked.defaultof<string -> Propertied -> obj -> obj option>
    let mutable internal setPropertyOpt = Unchecked.defaultof<string -> Propertied -> obj option -> Type -> obj -> obj>
    let mutable internal handlePropertyChange = Unchecked.defaultof<string -> Propertied -> obj -> obj -> obj * obj>

    // EventSystem reach-arounds.
    let mutable internal handleUserDefinedCallback = Unchecked.defaultof<obj -> obj -> obj -> Handling * obj>

    /// Represents an unsubscription operation for an event.
    type Unsubscription =
        World -> World
    
    /// The payload that is passed with the lens as a hack to performance from the Elmish implementation.
    and internal Payload =
        int array * Guid * (ChangeData -> obj option -> World -> obj)

    /// The data for a change in the world's ambient state.
    and [<StructuralEquality; NoComparison>] AmbientChangeData = 
        { OldWorldWithOldState : World }

    /// Store origination information about a simulant physics body.
    and [<StructuralEquality; NoComparison>] BodySource =
        { Entity : Entity
          BodyId : Guid }
        static member internal fromInternal (internal_ : BodySourceInternal) =
            { Entity = internal_.Simulant :?> Entity
              BodyId = internal_.BodyId }
    
    /// Store origination information about a simulant physics shape body.
    and [<StructuralEquality; NoComparison>] BodyShapeSource =
        { Entity : Entity
          BodyId : Guid
          BodyShapeId : Guid }
        static member internal fromInternal (internal_ : BodyShapeSourceInternal) =
            { Entity = internal_.Simulant :?> Entity
              BodyId = internal_.BodyId
              BodyShapeId = internal_.ShapeId }

    /// Describes the information needed to sort simulants.
    /// OPTIMIZATION: carries related simulant to avoid GC pressure.
    and [<CustomEquality; CustomComparison>] SortPriority =
        { SortDepth : single
          SortPositionY : single
          SortTarget : Simulant }

        static member equals left right =
            left.SortDepth = right.SortDepth &&
            left.SortTarget = right.SortTarget

        static member compare left right =
            if left.SortDepth < right.SortDepth then 1
            elif left.SortDepth > right.SortDepth then -1
            elif left.SortPositionY < right.SortPositionY then -1
            elif left.SortPositionY > right.SortPositionY then 1
            else 0

        override this.GetHashCode () =
            this.SortDepth.GetHashCode ()

        override this.Equals that =
            match that with
            | :? SortPriority as that -> SortPriority.equals this that
            | _ -> failwithumf ()

        interface SortPriority IComparable with
            member this.CompareTo that =
                SortPriority.compare this that

        interface IComparable with
            member this.CompareTo that =
                match that with
                | :? SortPriority as that -> (this :> SortPriority IComparable).CompareTo that
                | _ -> failwithumf ()

    /// Generalized interface tag for dispatchers.
    and Dispatcher = interface end

    /// Generalized interface tag for simulant dispatchers.
    and SimulantDispatcher () = interface Dispatcher

    /// The default dispatcher for games.
    and GameDispatcher () =
        inherit SimulantDispatcher ()

        /// Register a game when adding it to the world.
        abstract Register : Game * World -> World
        default this.Register (_, world) = world

        /// Unregister a game when finished with the world.
        abstract Unregister : Game * World -> World
        default this.Unregister (_, world) = world

        /// Update a game.
        abstract Update : Game * World -> World
        default this.Update (_, world) = world

        /// Post-update a game.
        abstract PostUpdate : Game * World -> World
        default this.PostUpdate (_, world) = world

        /// Actualize a game.
        abstract Actualize : Game * World -> World
        default this.Actualize (_, world) = world

        /// Try to send a signal to a game.
        abstract TrySignal : obj * Game * World -> World
        default this.TrySignal (_, _, world) = world

    /// The default dispatcher for screens.
    and ScreenDispatcher () =
        inherit SimulantDispatcher ()

        /// Register a screen when adding it to the world.
        abstract Register : Screen * World -> World
        default this.Register (_, world) = world

        /// Unregister a screen when removing it from the world.
        abstract Unregister : Screen * World -> World
        default this.Unregister (_, world) = world

        /// Update a screen.
        abstract Update : Screen * World -> World
        default this.Update (_, world) = world

        /// Post-update a screen.
        abstract PostUpdate : Screen * World -> World
        default this.PostUpdate (_, world) = world

        /// Actualize a screen.
        abstract Actualize : Screen * World -> World
        default this.Actualize (_, world) = world

        /// Try to send a signal to a screen.
        abstract TrySignal : obj * Screen * World -> World
        default this.TrySignal (_, _, world) = world

    /// The default dispatcher for layers.
    and LayerDispatcher () =
        inherit SimulantDispatcher ()

        /// Register a layer when adding it to a screen.
        abstract Register : Layer * World -> World
        default this.Register (_, world) = world

        /// Unregister a layer when removing it from a screen.
        abstract Unregister : Layer * World -> World
        default this.Unregister (_, world) = world

        /// Update a layer.
        abstract Update : Layer * World -> World
        default this.Update (_, world) = world

        /// Post-update a layer.
        abstract PostUpdate : Layer * World -> World
        default this.PostUpdate (_, world) = world

        /// Actualize a layer.
        abstract Actualize : Layer * World -> World
        default this.Actualize (_, world) = world

        /// Try to send a signal to a layer.
        abstract TrySignal : obj * Layer * World -> World
        default this.TrySignal (_, _, world) = world

    /// The default dispatcher for entities.
    and EntityDispatcher () =
        inherit SimulantDispatcher ()

        static member Properties =
            [Define? Position Vector2.Zero
             Define? Size Constants.Engine.DefaultEntitySize
             Define? Rotation 0.0f
             Define? Depth 0.0f
             Define? Omnipresent false
             Define? Absolute false
             Define? Model { DesignerType = typeof<unit>; DesignerValue = () }
             Define? Overflow Vector2.Zero
             Define? PublishChanges false
             Define? Visible true
             Define? Enabled true
             Define? AlwaysUpdate false
             Define? PublishUpdates false
             Define? PublishPostUpdates false
             Define? Persistent true]

        /// Register an entity when adding it to a layer.
        abstract Register : Entity * World -> World
        default this.Register (_, world) = world

        /// Unregister an entity when removing it from a layer.
        abstract Unregister : Entity * World -> World
        default this.Unregister (_, world) = world

        /// Update an entity.
        abstract Update : Entity * World -> World
        default this.Update (_, world) = world

#if !DISABLE_ENTITY_POST_UPDATE
        /// Post-update an entity.
        abstract PostUpdate : Entity * World -> World
        default this.PostUpdate (_, world) = world
#endif

        /// Actualize an entity.
        abstract Actualize : Entity * World -> World
        default this.Actualize (_, world) = world

        /// Get the quick size of an entity (the appropriate user-defined size for an entity).
        abstract GetQuickSize : Entity * World -> Vector2
        default this.GetQuickSize (_, _) = Constants.Engine.DefaultEntitySize

        /// Try to send a signal to an entity's facet.
        abstract TrySignalFacet : obj * string * Entity * World -> World
        default this.TrySignalFacet (_, _, _, world) = world

        /// Try to send a signal to an entity.
        abstract TrySignal : obj * Entity * World -> World
        default this.TrySignal (_, _, world) = world

    /// Dynamically augments an entity's behavior in a composable way.
    and Facet () =

        /// Register a facet when adding it to an entity.
        abstract Register : Entity * World -> World
        default this.Register (_, world) = world

        /// Unregister a facet when removing it from an entity.
        abstract Unregister : Entity * World -> World
        default this.Unregister (_, world) = world

        /// Participate in the registration of an entity's physics with the physics subsystem.
        abstract RegisterPhysics : Entity * World -> World
        default this.RegisterPhysics (_, world) = world

        /// Participate in the unregistration of an entity's physics from the physics subsystem.
        abstract UnregisterPhysics : Entity * World -> World
        default this.UnregisterPhysics (_, world) = world

        /// Update a facet.
        abstract Update : Entity * World -> World
        default this.Update (_, world) = world

#if !DISABLE_ENTITY_POST_UPDATE
        /// Post-update a facet.
        abstract PostUpdate : Entity * World -> World
        default this.PostUpdate (_, world) = world
#endif

        /// Actualize a facet.
        abstract Actualize : Entity * World -> World
        default this.Actualize (_, world) = world

        /// Participate in getting the priority with which an entity is picked in the editor.
        abstract GetQuickSize : Entity * World -> Vector2
        default this.GetQuickSize (_, _) = Constants.Engine.DefaultEntitySize

        /// Try to send a signal to a facet.
        abstract TrySignal : obj * Entity * World -> World
        default this.TrySignal (_, _, world) = world

    /// Generalized interface for simulant state.
    and SimulantState =
        interface
            abstract member GetXtension : unit -> Xtension
            end

    /// Hosts the ongoing state of a game.
    and [<NoEquality; NoComparison; CLIMutable>] GameState =
        { Dispatcher : GameDispatcher
          mutable Xtension : Xtension
          mutable Model : DesignerProperty
          mutable OmniScreenOpt : Screen option
          mutable SelectedScreenOpt : Screen option
          mutable ScreenTransitionDestinationOpt : Screen option
          mutable EyeCenter : Vector2
          mutable EyeSize : Vector2
          mutable ScriptFrame : Scripting.DeclarationFrame
          CreationTimeStamp : int64
          Id : Guid }

        interface SimulantState with
            member this.GetXtension () = this.Xtension

        /// Make a game state value.
        static member make (dispatcher : GameDispatcher) =
            let eyeCenter = Vector2.Zero
            let eyeSize = Vector2 (single Constants.Render.DefaultResolutionX, single Constants.Render.DefaultResolutionY)
            { Id = Gen.id
              Dispatcher = dispatcher
              Xtension = Xtension.makeImperative ()
              Model = { DesignerType = typeof<unit>; DesignerValue = () }
              OmniScreenOpt = None
              SelectedScreenOpt = None
              ScreenTransitionDestinationOpt = None
              EyeCenter = eyeCenter
              EyeSize = eyeSize
              ScriptFrame = Scripting.DeclarationFrame HashIdentity.Structural
              CreationTimeStamp = Core.getUniqueTimeStamp () }

        /// Try to get an xtension property and its type information.
        static member tryGetProperty propertyName gameState =
            Xtension.tryGetProperty propertyName gameState.Xtension

        /// Get an xtension property and its type information.
        static member getProperty propertyName gameState =
            Xtension.getProperty propertyName gameState.Xtension

        /// Try to set an xtension property with explicit type information.
        static member trySetProperty propertyName property gameState =
            let (changed, xtension) = Xtension.trySetProperty propertyName property gameState.Xtension
            gameState.Xtension <- xtension
            changed

        /// Set an xtension property with explicit type information.
        static member setProperty propertyName property gameState =
            let xtension = Xtension.setProperty propertyName property gameState.Xtension
            gameState.Xtension <- xtension

        /// Attach an xtension property.
        static member attachProperty name property gameState =
            let xtension = Xtension.attachProperty name property gameState.Xtension
            gameState.Xtension <- xtension
            true

        /// Detach an xtension property.
        static member detachProperty name gameState =
            let xtension = Xtension.detachProperty name gameState.Xtension
            gameState.Xtension <- xtension
            true

        /// Copy a game such as when, say, you need it to be mutated with reflection but you need to preserve persistence.
        static member copy this =
            { this with GameState.Dispatcher = this.Dispatcher }

    /// Hosts the ongoing state of a screen.
    and [<NoEquality; NoComparison; CLIMutable>] ScreenState =
        { Dispatcher : ScreenDispatcher
          mutable Xtension : Xtension
          mutable Model : DesignerProperty
          Ecs : World Ecs
          mutable TransitionState : TransitionState
          mutable TransitionTicks : int64
          mutable Incoming : Transition
          mutable Outgoing : Transition
          mutable Persistent : bool
          mutable ScriptFrame : Scripting.DeclarationFrame
          CreationTimeStamp : int64
          Id : Guid
          Name : string }

        interface SimulantState with
            member this.GetXtension () = this.Xtension

        /// Make a screen state value.
        static member make nameOpt (dispatcher : ScreenDispatcher) ecs =
            let (id, name) = Gen.idAndNameIf nameOpt
            { Dispatcher = dispatcher
              Xtension = Xtension.makeImperative ()
              Model = { DesignerType = typeof<unit>; DesignerValue = () }
              Ecs = ecs
              TransitionState = IdlingState
              TransitionTicks = 0L // TODO: roll this field into Incoming/OutgoingState values
              Incoming = Transition.make Incoming
              Outgoing = Transition.make Outgoing
              Persistent = true
              ScriptFrame = Scripting.DeclarationFrame HashIdentity.Structural
              CreationTimeStamp = Core.getUniqueTimeStamp ()
              Id = id
              Name = name }

        /// Try to get an xtension property and its type information.
        static member tryGetProperty propertyName screenState =
            Xtension.tryGetProperty propertyName screenState.Xtension

        /// Get an xtension property and its type information.
        static member getProperty propertyName screenState =
            Xtension.getProperty propertyName screenState.Xtension

        /// Try to set an xtension property with explicit type information.
        static member trySetProperty propertyName property screenState =
            let (changed, xtension) = Xtension.trySetProperty propertyName property screenState.Xtension
            screenState.Xtension <- xtension
            changed

        /// Set an xtension property with explicit type information.
        static member setProperty propertyName property screenState =
            let xtension = Xtension.setProperty propertyName property screenState.Xtension
            screenState.Xtension <- xtension

        /// Attach an xtension property.
        static member attachProperty name property screenState =
            let xtension = Xtension.attachProperty name property screenState.Xtension
            screenState.Xtension <- xtension
            true

        /// Detach an xtension property.
        static member detachProperty name screenState =
            let xtension = Xtension.detachProperty name screenState.Xtension
            screenState.Xtension <- xtension
            true

        /// Copy a screen such as when, say, you need it to be mutated with reflection but you need to preserve persistence.
        static member copy this =
            { this with ScreenState.Dispatcher = this.Dispatcher }

    /// Hosts the ongoing state of a layer.
    and [<NoEquality; NoComparison; CLIMutable>] LayerState =
        { Dispatcher : LayerDispatcher
          mutable Xtension : Xtension
          mutable Model : DesignerProperty
          mutable Visible : bool
          mutable Persistent : bool
          mutable ScriptFrame : Scripting.DeclarationFrame
          CreationTimeStamp : int64
          Id : Guid
          Name : string }

        interface SimulantState with
            member this.GetXtension () = this.Xtension

        /// Make a layer state value.
        static member make nameOpt (dispatcher : LayerDispatcher) =
            let (id, name) = Gen.idAndNameIf nameOpt
            { Dispatcher = dispatcher
              Xtension = Xtension.makeImperative ()
              Model = { DesignerType = typeof<unit>; DesignerValue = () }
              Visible = true
              Persistent = true
              ScriptFrame = Scripting.DeclarationFrame HashIdentity.Structural
              CreationTimeStamp = Core.getUniqueTimeStamp ()
              Id = id
              Name = name }

        /// Try to get an xtension property and its type information.
        static member tryGetProperty propertyName layerState =
            Xtension.tryGetProperty propertyName layerState.Xtension

        /// Get an xtension property and its type information.
        static member getProperty propertyName layerState =
            Xtension.getProperty propertyName layerState.Xtension

        /// Try to set an xtension property with explicit type information.
        static member trySetProperty propertyName property layerState =
            let (changed, xtension) = Xtension.trySetProperty propertyName property layerState.Xtension
            layerState.Xtension <- xtension
            changed

        /// Set an xtension property with explicit type information.
        static member setProperty propertyName property layerState =
            let xtension = Xtension.setProperty propertyName property layerState.Xtension
            layerState.Xtension <- xtension

        /// Attach an xtension property.
        static member attachProperty name property layerState =
            let xtension = Xtension.attachProperty name property layerState.Xtension
            layerState.Xtension <- xtension
            true

        /// Detach an xtension property.
        static member detachProperty name layerState =
            let xtension = Xtension.detachProperty name layerState.Xtension
            layerState.Xtension <- xtension
            true

        /// Copy a layer such as when, say, you need it to be mutated with reflection but you need to preserve persistence.
        static member copy this =
            { this with LayerState.Dispatcher = this.Dispatcher }

    /// Hosts the ongoing state of an entity.
    and [<NoEquality; NoComparison; CLIMutable>] EntityState =
        { // cache line 1
          Dispatcher : EntityDispatcher
          mutable Transform : Transform
          // cache line 2 start at second-to-last field of Transform
          mutable Facets : Facet array
          mutable Xtension : Xtension
          mutable Model : DesignerProperty
          mutable Overflow : Vector2
          mutable OverlayNameOpt : string option
          // cache line 3
          mutable FacetNames : string Set
          mutable ScriptFrame : Scripting.DeclarationFrame
          CreationTimeStamp : int64 // just needed for ordering writes to reduce diff volumes
          Id : Guid
          Name : string }

        interface SimulantState with
            member this.GetXtension () = this.Xtension

        /// Make an entity state value.
        static member make nameOpt overlayNameOpt (dispatcher : EntityDispatcher) =
            let (id, name) = Gen.idAndNameIf nameOpt
            { Transform =
                { Position = Vector2.Zero
                  Size = Constants.Engine.DefaultEntitySize
                  Rotation = 0.0f
                  Depth = 0.0f
                  Flags = 0b010001100000
                  RefCount = 0 }
              Dispatcher = dispatcher
              Facets = [||]
              Xtension = Xtension.makeImperative ()
              Model = { DesignerType = typeof<unit>; DesignerValue = () }
              Overflow = Vector2.Zero
              OverlayNameOpt = overlayNameOpt
              FacetNames = Set.empty
              ScriptFrame = Scripting.DeclarationFrame HashIdentity.Structural
              CreationTimeStamp = Core.getUniqueTimeStamp ()
              Id = id
              Name = name }

        /// Try to get an xtension property and its type information.
        static member tryGetProperty propertyName entityState =
            Xtension.tryGetProperty propertyName entityState.Xtension

        /// Get an xtension property and its type information.
        static member getProperty propertyName entityState =
            Xtension.getProperty propertyName entityState.Xtension

        /// Try to set an xtension property with explicit type information.
        static member trySetProperty propertyName property entityState =
            let (changed, xtension) = Xtension.trySetProperty propertyName property entityState.Xtension
            entityState.Xtension <- xtension
            changed

        /// Set an xtension property with explicit type information.
        static member setProperty propertyName property entityState =
            let xtension = Xtension.setProperty propertyName property entityState.Xtension
            entityState.Xtension <- xtension

        /// Attach an xtension property.
        static member attachProperty name property entityState =
            let xtension = Xtension.attachProperty name property entityState.Xtension
            entityState.Xtension <- xtension
            true

        /// Detach an xtension property.
        static member detachProperty name entityState =
            let xtension = Xtension.detachProperty name entityState.Xtension
            entityState.Xtension <- xtension
            true

        /// Copy an entity such as when, say, you need it to be mutated with reflection but you need to preserve persistence.
        static member copy (entityState : EntityState) =
            { entityState with EntityState.Dispatcher = entityState.Dispatcher }

        // Member properties; only for use by internal reflection facilities.
        member this.Position with get () = this.Transform.Position and set value = this.Transform.Position <- value
        member this.Size with get () = this.Transform.Size and set value = this.Transform.Size <- value
        member this.Rotation with get () = this.Transform.Rotation and set value = this.Transform.Rotation <- value
        member this.Depth with get () = this.Transform.Depth and set value = this.Transform.Depth <- value
        member internal this.Dirty with get () = this.Transform.Dirty and set value = this.Transform.Dirty <- value
        member internal this.Invalidated with get () = this.Transform.Invalidated and set value = this.Transform.Invalidated <- value
        member this.Omnipresent with get () = this.Transform.Omnipresent and set value = this.Transform.Omnipresent <- value
        member this.Absolute with get () = this.Transform.Absolute and set value = this.Transform.Absolute <- value
        member this.PublishChanges with get () = this.Transform.PublishChanges and set value = this.Transform.PublishChanges <- value
        member this.Enabled with get () = this.Transform.Enabled and set value = this.Transform.Enabled <- value
        member this.Visible with get () = this.Transform.Visible and set value = this.Transform.Visible <- value
        member this.AlwaysUpdate with get () = this.Transform.AlwaysUpdate and set value = this.Transform.AlwaysUpdate <- value
        member this.PublishUpdates with get () = this.Transform.PublishUpdates and set value = this.Transform.PublishUpdates <- value
        member this.PublishPostUpdates with get () = this.Transform.PublishPostUpdates and set value = this.Transform.PublishPostUpdates <- value
        member this.Persistent with get () = this.Transform.Persistent and set value = this.Transform.Persistent <- value
        member this.Optimized with get () = this.Transform.Optimized

    /// The game type that hosts the various screens used to navigate through a game.
    and Game (gameAddress) =

        // check that address is of correct length for a game
        do if Address.length gameAddress <> 0 then failwith "Game address must be length of 0."

        /// Create a game reference.
        new () = Game (Address.empty)

        /// The address of the game.
        member this.GameAddress = gameAddress

#if DEBUG
        /// Get the latest value of a game's properties.
        [<DebuggerBrowsable (DebuggerBrowsableState.RootHidden)>]
        member private this.View = Debug.World.viewGame Debug.World.Chosen
#endif
        
        /// Helper for accessing game lenses.
        static member Lens = Unchecked.defaultof<Game>

        /// Concatenate an address with a game's address, forcing the type of first address.
        static member (-->) (address : 'a Address, game : Game) =
            match box game with
            | null -> address // HACK: this case is a hack to be able to insert events into the elmish event handler
            | _ -> acatff address game.GameAddress

        override this.ToString () =
            scstring this.GameAddress

        override this.Equals that =
            match that with
            | :? Game as that -> this.GameAddress = that.GameAddress
            | _ -> false

        override this.GetHashCode () =
            Address.hash this.GameAddress

        interface Simulant with
            member this.SimulantAddress = atoa<Game, Simulant> this.GameAddress
            end

        interface Game IComparable with
            member this.CompareTo that =
                Address.compare this.GameAddress that.GameAddress

        interface IComparable with
            member this.CompareTo that =
                match that with
                | :? Game as that -> (this :> Game IComparable).CompareTo that
                | _ -> failwith "Invalid Game comparison (comparee not of type Game)."

    /// The screen type that allows transitioning to and from other screens, and also hosts the
    /// currently interactive layers of entities.
    and Screen (screenAddress) =

        // check that address is of correct length for a screen
        do if Address.length screenAddress <> 1 then failwith "Screen address must be length of 1."

        /// Create a screen reference from a name string.
        new (screenName : string) = Screen (ntoa screenName)

        /// The address of the screen.
        member this.ScreenAddress = screenAddress

        /// The parent game of the screen.
        member this.Parent = Game ()

        /// Get the name of a screen.
        member this.Name = this.ScreenAddress.Names.[0]

#if DEBUG
        /// Get the latest value of a screen's properties.
        [<DebuggerBrowsable (DebuggerBrowsableState.RootHidden)>]
        member private this.View = Debug.World.viewScreen (this :> obj) Debug.World.Chosen
#endif

        /// Helper for accessing screen lenses.
        static member Lens = Unchecked.defaultof<Screen>

        /// Derive a layer from its screen.
        static member (/) (screen : Screen, layerName) = Layer (atoa<Screen, Layer> screen.ScreenAddress --> ntoa layerName)

        /// Concatenate an address with a screen's address, forcing the type of first address.
        static member (-->) (address : 'a Address, screen : Screen) =
            match box screen with
            | null -> address // HACK: this case is a hack to be able to insert events into the elmish event handler
            | _ -> acatff address screen.ScreenAddress

        override this.ToString () =
            scstring this.ScreenAddress

        override this.Equals that =
            match that with
            | :? Screen as that -> this.ScreenAddress = that.ScreenAddress
            | _ -> false

        override this.GetHashCode () =
            Address.hash this.ScreenAddress

        interface Simulant with
            member this.SimulantAddress = atoa<Screen, Simulant> this.ScreenAddress
            end

        interface Screen IComparable with
            member this.CompareTo that =
                Address.compare this.ScreenAddress that.ScreenAddress

        interface IComparable with
            member this.CompareTo that =
                match that with
                | :? Screen as that -> (this :> Screen IComparable).CompareTo that
                | _ -> failwith "Invalid Screen comparison (comparee not of type Screen)."

    /// Forms a logical layer of entities.
    and Layer (layerAddress) =

        // check that address is of correct length for a layer
        do if Address.length layerAddress <> 2 then failwith "Layer address must be length of 2."

        /// Create a layer reference from an address string.
        new (layerAddressStr : string) = Layer (stoa layerAddressStr)

        /// Create a layer reference from a list of names.
        new (layerNames : string array) = Layer (rtoa layerNames)

        /// Create a layer reference from a list of names.
        new (layerNames : string list) = Layer (ltoa layerNames)

        /// Create a layer reference from a the required names.
        new (screenName : string, layerName : string) = Layer [screenName; layerName]

        /// The address of the layer.
        member this.LayerAddress = layerAddress

        /// The parent screen of the layer.
        member this.Parent = let names = this.LayerAddress.Names in Screen names.[0]

        /// Get the name of a layer.
        member this.Name = Address.getName this.LayerAddress

#if DEBUG
        /// Get the latest value of a layer's properties.
        [<DebuggerBrowsable (DebuggerBrowsableState.RootHidden)>]
        member private this.View = Debug.World.viewLayer (this :> obj) Debug.World.Chosen
#endif

        /// Derive an entity from its layer.
        static member (/) (layer : Layer, entityName) = Entity (atoa<Layer, Entity> layer.LayerAddress --> ntoa entityName)

        /// Concatenate an address with a layer's address, forcing the type of first address.
        static member (-->) (address : 'a Address, layer : Layer) =
            match box layer with
            | null -> address // HACK: this case is a hack to be able to insert events into the elmish event handler
            | _ -> acatff address layer.LayerAddress

        /// Helper for accessing layer lenses.
        static member Lens = Unchecked.defaultof<Layer>

        override this.ToString () =
            scstring this.LayerAddress

        override this.Equals that =
            match that with
            | :? Layer as that -> this.LayerAddress = that.LayerAddress
            | _ -> false

        override this.GetHashCode () =
            Address.hash this.LayerAddress

        interface Simulant with
            member this.SimulantAddress = atoa<Layer, Simulant> this.LayerAddress
            end

        interface Layer IComparable with
            member this.CompareTo that =
                Address.compare this.LayerAddress that.LayerAddress

        interface IComparable with
            member this.CompareTo that =
                match that with
                | :? Layer as that -> (this :> Layer IComparable).CompareTo that
                | _ -> failwith "Invalid Layer comparison (comparee not of type Layer)."

    /// The type around which the whole game engine is based! Used in combination with dispatchers
    /// to implement things like buttons, characters, blocks, and things of that sort.
    /// OPTIMIZATION: Includes pre-constructed entity change and update event addresses to avoid
    /// reconstructing new ones for each entity every frame.
    and Entity (entityAddress) =

        // check that address is of correct length for an entity
        do if Address.length entityAddress <> 3 then
            failwith "Entity address must be length of 3."

        let updateEvent =
            let entityNames = Address.getNames entityAddress
            rtoa<unit> [|"Update"; "Event"; entityNames.[0]; entityNames.[1]; entityNames.[2]|]

#if !DISABLE_ENTITY_POST_UPDATE
        let postUpdateEvent =
            let entityNames = Address.getNames entityAddress
            rtoa<unit> [|"PostUpdate"; "Event"; entityNames.[0]; entityNames.[1]; entityNames.[2]|]
#endif

        let mutable entityStateOpt =
            Unchecked.defaultof<EntityState>

        // Create an entity reference from an address string.
        new (entityAddressStr : string) = Entity (stoa entityAddressStr)

        // Create an entity reference from an array of names.
        new (entityNames : string array) = Entity (rtoa entityNames)

        // Create an entity reference from a list of names.
        new (entityNames : string list) = Entity (ltoa entityNames)

        // Create an entity reference from a the required names.
        new (screenName : string, layerName : string, entityName : string) = Entity [screenName; layerName; entityName]

        /// The address of the entity.
        member this.EntityAddress = entityAddress

        /// The parent layer of the entity.
        member this.Parent = let names = this.EntityAddress.Names in Layer [names.[0]; names.[1]]

        /// The address of the entity's update event.
        member this.UpdateEventCached = updateEvent

#if !DISABLE_ENTITY_POST_UPDATE
        /// The address of the entity's post-update event.
        member this.PostUpdateEventCached = postUpdateEvent
#endif

        /// The cached entity state.
        member this.EntityStateOpt
            with get () = entityStateOpt
            and set value = entityStateOpt <- value

        /// Get the name of an entity.
        member this.Name = Address.getName this.EntityAddress

#if DEBUG
        /// Get the latest value of an entity's properties.
        [<DebuggerBrowsable (DebuggerBrowsableState.RootHidden)>]
        member private this.View = Debug.World.viewEntity (this :> obj) Debug.World.Chosen
#endif

        /// Helper for accessing entity lenses.
        static member Lens = Unchecked.defaultof<Entity>

        /// Concatenate an address with an entity, forcing the type of first address.
        static member (-->) (address : 'a Address, entity : Entity) =
            match box entity with
            | null -> address // HACK: this case is a hack to be able to insert events into the elmish event handler
            | _ -> acatff address entity.EntityAddress

        override this.ToString () =
            scstring this.EntityAddress

        override this.Equals that =
            match that with
            | :? Entity as that -> this.EntityAddress = that.EntityAddress
            | _ -> false

        override this.GetHashCode () =
            Address.hash this.EntityAddress

        interface Simulant with
            member this.SimulantAddress = atoa<Entity, Simulant> this.EntityAddress
            end

        interface Entity IComparable with
            member this.CompareTo that =
                Address.compare this.EntityAddress that.EntityAddress

        interface IComparable with
            member this.CompareTo that =
                match that with
                | :? Entity as that -> (this :> Entity IComparable).CompareTo that
                | _ -> failwith "Invalid Entity comparison (comparee not of type Entity)."

    /// The world's dispatchers (including facets).
    /// 
    /// I would prefer this type to be inlined in World, but it has been extracted to its own white-box
    /// type for efficiency reasons.
    and [<ReferenceEquality; NoComparison>] internal Dispatchers =
        { GameDispatchers : Map<string, GameDispatcher>
          ScreenDispatchers : Map<string, ScreenDispatcher>
          LayerDispatchers : Map<string, LayerDispatcher>
          EntityDispatchers : Map<string, EntityDispatcher>
          Facets : Map<string, Facet>
          TryGetExtrinsic : string -> World ScriptingTrinsic option
          UpdateEntityInEntityTree : bool -> bool -> Vector4 -> Entity -> World -> World -> World
          RebuildEntityTree : World -> Entity SpatialTree }

    /// The subsystems encapsulated by the engine.
    and [<ReferenceEquality; NoComparison>] internal Subsystems =
        { PhysicsEngine : PhysicsEngine
          Renderer : Renderer
          AudioPlayer : AudioPlayer }

    /// The world, in a functional programming sense. Hosts the game object, the dependencies needed
    /// to implement a game, messages to by consumed by the various engine sub-systems, and general
    /// configuration data.
    ///
    /// Unfortunately, in a 64-bit context, this is larger than a single cache line. We can decompose
    /// this type further, but the trade-off is introducing additional pointer indrections in some
    /// cases. Fortunately, we currently have the most accessed parts in a single cache line. Still,
    /// it might be worth the extra decomposition.
    ///
    /// NOTE: this would be better as private, but there is just too much code to fit in this file
    /// for that.
    and [<ReferenceEquality; NoComparison>] World =
        internal
            { // cache line begin
              EventSystemDelegate : World EventSystemDelegate
              EntityCachedOpt : KeyedCache<KeyValuePair<Entity Address, UMap<Entity Address, EntityState>>, EntityState option>
              EntityTree : Entity SpatialTree MutantCache
              EntityStates : UMap<Entity Address, EntityState>
              LayerStates : UMap<Layer Address, LayerState>
              ScreenStates : UMap<Screen Address, ScreenState>
              GameState : GameState
              AmbientState : World AmbientState
              // cache line end
              Subsystems : Subsystems
              ScreenDirectory : UMap<string, KeyValuePair<Screen Address, UMap<string, KeyValuePair<Layer Address, UMap<string, Entity Address>>>>>
              Dispatchers : Dispatchers
              ScriptingEnv : Scripting.Env
              ScriptingContext : Simulant
              Plugin : NuPlugin }

        interface World EventSystem with

            member this.GetLiveness () =
                AmbientState.getLiveness this.AmbientState

            member this.GetGlobalSimulantSpecialized () =
                EventSystemDelegate.getGlobalSimulantSpecialized this.EventSystemDelegate

            member this.GetGlobalSimulantGeneralized () =
                EventSystemDelegate.getGlobalSimulantGeneralized this.EventSystemDelegate

            member this.SimulantExists simulant =
                match simulant with
                | :? Entity as entity -> UMap.containsKey entity.EntityAddress this.EntityStates
                | :? Layer as layer -> UMap.containsKey layer.LayerAddress this.LayerStates
                | :? Screen as screen -> UMap.containsKey screen.ScreenAddress this.ScreenStates
                | :? Game | :? GlobalSimulantGeneralized -> true
                | _  -> false

            member this.GetPropertyOpt<'a> propertyName simulant =
                match getPropertyOpt propertyName simulant (box this) with
                | Some a -> Some (a :?> 'a)
                | None -> None

            member this.SetPropertyOpt<'a> propertyName simulant (value : 'a option) =
                let world =
                    match value with
                    | Some a -> setPropertyOpt propertyName simulant (Some (box a)) typeof<'a> (box this)
                    | None -> setPropertyOpt propertyName simulant None typeof<'a> (box this)
                world :?> World

            member this.HandlePropertyChange propertyName simulant handler =
                let (unsubscribe, world) = handlePropertyChange propertyName simulant (box handler) (box this)
                (unsubscribe :?> World -> World, world :?> World)

            member this.GetEventSystemDelegateHook () =
                this.EventSystemDelegate

            member this.UpdateEventSystemDelegateHook updater =
                let this = { this with EventSystemDelegate = updater this.EventSystemDelegate }
#if DEBUG
                if Debug.World.Frozen > 0 then failwith "Invalid operation on a frozen world."
                Debug.World.Chosen <- this
#endif
                this

            member this.HandleUserDefinedCallback userDefined data world =
                let (handling, worldObj) = handleUserDefinedCallback userDefined data (world :> obj)
                (handling, worldObj :?> World)

            member this.PublishEventHook (simulant : Simulant) publisher eventData eventAddress eventTrace subscription world =
                let (handling, world) =
                    match simulant with
                    | :? Entity -> EventSystem.publishEvent<'a, 'p, Entity, World> simulant publisher eventData eventAddress eventTrace subscription world
                    | :? Layer -> EventSystem.publishEvent<'a, 'p, Layer, World> simulant publisher eventData eventAddress eventTrace subscription world
                    | :? Screen -> EventSystem.publishEvent<'a, 'p, Screen, World> simulant publisher eventData eventAddress eventTrace subscription world
                    | :? Game -> EventSystem.publishEvent<'a, 'p, Game, World> simulant publisher eventData eventAddress eventTrace subscription world
                    | :? GlobalSimulantGeneralized -> EventSystem.publishEvent<'a, 'p, Simulant, World> simulant publisher eventData eventAddress eventTrace subscription world
                    | _ -> failwithumf ()
#if DEBUG
                if Debug.World.Frozen > 0 then failwith "Invalid operation on a frozen world."
                Debug.World.Chosen <- world
#endif
                (handling, world)

        interface World ScriptingSystem with

            member this.GetEnv () =
                this.ScriptingEnv

            member this.TryGetExtrinsic fnName =
                this.Dispatchers.TryGetExtrinsic fnName

            member this.TryImport ty value =
                match (ty.Name, value) with
                | ("Vector2", (:? Vector2 as v2)) -> let v2p = { Vector2 = v2 } in v2p :> Scripting.Pluggable |> Scripting.Pluggable |> Some
                | ("Vector4", (:? Vector4 as v4)) -> let v4p = { Vector4 = v4 } in v4p :> Scripting.Pluggable |> Scripting.Pluggable |> Some
                | ("Vector2i", (:? Vector2i as v2i)) -> let v2ip = { Vector2i = v2i } in v2ip :> Scripting.Pluggable |> Scripting.Pluggable |> Some
                | ("Vector4i", (:? Vector4i as v4i)) -> let v4ip = { Vector4i = v4i } in v4ip :> Scripting.Pluggable |> Scripting.Pluggable |> Some
                | ("Game", (:? Game as game)) -> game.GameAddress |> atos |> Scripting.Keyword |> Some
                | ("Screen", (:? Screen as screen)) -> screen.ScreenAddress |> atos |> Scripting.Keyword |> Some
                | ("Layer", (:? Layer as layer)) -> layer.LayerAddress |> atos |> Scripting.Keyword |> Some
                | ("Entity", (:? Entity as entity)) -> entity.EntityAddress |> atos |> Scripting.Keyword |> Some
                | ("Simulant", (:? Simulant as simulant)) -> simulant.SimulantAddress |> atos |> Scripting.Keyword |> Some
                | (_, _) -> None

            member this.TryExport ty value =
                match (ty.Name, value) with
                | ("Vector2", Scripting.Pluggable pluggable) -> let v2 = pluggable :?> Vector2Pluggable in v2.Vector2 :> obj |> Some
                | ("Vector4", Scripting.Pluggable pluggable) -> let v4 = pluggable :?> Vector4Pluggable in v4.Vector4 :> obj |> Some
                | ("Vector2i", Scripting.Pluggable pluggable) -> let v2i = pluggable :?> Vector2iPluggable in v2i.Vector2i :> obj |> Some
                | ("Vector4i", Scripting.Pluggable pluggable) -> let v4i = pluggable :?> Vector4iPluggable in v4i.Vector4i :> obj |> Some
                | ("Game", Scripting.String str) | ("Game", Scripting.Keyword str) -> str |> stoa |> Game :> obj |> Some
                | ("Screen", Scripting.String str) | ("Screen", Scripting.Keyword str) -> str |> stoa |> Screen :> obj |> Some
                | ("Layer", Scripting.String str) | ("Layer", Scripting.Keyword str) -> str |> stoa |> Layer :> obj |> Some
                | ("Entity", Scripting.String str) | ("Entity", Scripting.Keyword str) -> str |> stoa |> Entity :> obj |> Some
                | ("Simulant", Scripting.String _) | ("Simulant", Scripting.Keyword _) -> None // TODO: P1: see if this should be failwithumf or a violation instead.
                | (_, _) -> None

        interface Freezable with

            member this.Frozen with get () =
#if DEBUG
                Debug.World.Frozen > 0
#else
                false
#endif

            member this.Freeze () =
#if DEBUG
                Debug.World.Frozen <- inc Debug.World.Frozen
#endif
                ()

            member this.Thaw () =
#if DEBUG
                Debug.World.Frozen <- dec Debug.World.Frozen
                if Debug.World.Frozen < 0 then failwith "World Freeze and Thaw operation mismatch."
#endif
                ()

        override this.ToString () =
            ""

    /// Provides a way to make user-defined dispatchers, facets, and various other sorts of game-
    /// specific values.
    and NuPlugin () =

        /// The game dispatcher that Nu will utilize when running outside the editor.
        abstract GetGameDispatcher : unit -> Type
        default this.GetGameDispatcher () = typeof<GameDispatcher>

        /// The screen dispatcher that Nu will utilize when running inside the editor.
        abstract GetEditorScreenDispatcher : unit -> Type
        default this.GetEditorScreenDispatcher () = typeof<ScreenDispatcher>

        /// Make the overlay routes that will allow Nu to use different overlays for the specified
        /// dispatcher name.
        abstract MakeOverlayRoutes : unit -> (string * string option) list
        default this.MakeOverlayRoutes () = []

        /// Make a list of keyed values to hook into the engine.
        abstract MakeKeyedValues : World -> ((Guid * obj) list) * World
        default this.MakeKeyedValues world = ([], world)

        /// Make the Ecs for each screen.
        abstract MakeEcs : unit -> World Ecs
        default this.MakeEcs () = Ecs<World> ()

        /// A call-back at the beginning of each frame.
        abstract PreFrame : World -> World
        default this.PreFrame world = world

        /// A call-back during each frame.
        abstract PerFrame : World -> World
        default this.PerFrame world = world

        /// A call-back at the end of each frame.
        abstract PostFrame : World -> World
        default this.PostFrame world = world

        /// Birth facets / dispatchers of type 'a from plugin.
        member internal this.Birth<'a> () =
            let assembly = (this.GetType ()).Assembly;
            let types = (assembly.GetTypes ()) |> Array.filter (fun ty -> ty.IsSubclassOf typeof<'a>) |> Array.filter (fun ty -> not ty.IsAbstract)
            let instances = types |> Array.map (fun ty -> (ty.Name, Activator.CreateInstance ty :?> 'a)) 
            Array.toList instances

/// Represents an unsubscription operation for an event.
type Unsubscription = WorldTypes.Unsubscription

/// The payload that is passed with the lens as a hack to performance from the Elmish implementation.
type internal Payload = WorldTypes.Payload

/// The data for a change in the world's ambient state.
type AmbientChangeData = WorldTypes.AmbientChangeData

/// Generalized interface tag for dispatchers.
type Dispatcher = WorldTypes.Dispatcher

/// Generalized interface tag for simulant dispatchers.
type SimulantDispatcher = WorldTypes.SimulantDispatcher

/// The default dispatcher for games.
type GameDispatcher = WorldTypes.GameDispatcher

/// The default dispatcher for screens.
type ScreenDispatcher = WorldTypes.ScreenDispatcher

/// The default dispatcher for layers.
type LayerDispatcher = WorldTypes.LayerDispatcher

/// The default dispatcher for entities.
type EntityDispatcher = WorldTypes.EntityDispatcher

/// Dynamically augments an entity's behavior in a composable way.
type Facet = WorldTypes.Facet

/// The game type that hosts the various screens used to navigate through a game.
type Game = WorldTypes.Game

/// The screen type that allows transitioning to and from other screens, and also hosts the
/// currently interactive layers of entities.
type Screen = WorldTypes.Screen

/// Forms a logical layer of entities.
type Layer = WorldTypes.Layer

/// The type around which the whole game engine is based! Used in combination with dispatchers
/// to implement things like buttons, characters, blocks, and things of that sort.
/// OPTIMIZATION: Includes pre-constructed entity event addresses to avoid reconstructing
/// new ones for each entity every frame.
type Entity = WorldTypes.Entity

/// The world, in a functional programming sense. Hosts the game object, the dependencies needed
/// to implement a game, messages to by consumed by the various engine sub-systems, and general
/// configuration data.
///
/// For efficiency, this type is kept under 64 bytes on 32-bit machines as to not exceed the size
/// of a typical cache line.
type World = WorldTypes.World

/// Provides a way to make user-defined dispatchers, facets, and various other sorts of game-
/// specific values.
type NuPlugin = WorldTypes.NuPlugin