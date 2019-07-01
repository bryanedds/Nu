// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Debug
open System
module internal World =

    /// The value of the world currently chosen for debugging in an IDE. Not to be used for anything else.
    let mutable internal Chosen = obj ()
    let mutable internal viewGame = fun (_ : obj) -> Array.create 0 (String.Empty, obj ())
    let mutable internal viewScreen = fun (_ : obj) (_ : obj) -> Array.create 0 (String.Empty, obj ())
    let mutable internal viewLayer = fun (_ : obj) (_ : obj) -> Array.create 0 (String.Empty, obj ())
    let mutable internal viewEntity = fun (_ : obj) (_ : obj) -> Array.create 0 (String.Empty, obj ())

namespace Nu
open System
open System.Diagnostics
open System.Collections.Generic
open OpenTK
open TiledSharp
open Prime
open Nu

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
type [<CLIMutable; StructuralEquality; NoComparison>] Transition =
    { TransitionType : TransitionType
      TransitionLifetime : int64
      DissolveImageOpt : Image AssetTag option }

    /// Make a screen transition.
    static member make transitionType =
        { TransitionType = transitionType
          TransitionLifetime = 0L
          DissolveImageOpt = None }

/// Describes the behavior of the screen dissolving algorithm.
type [<StructuralEquality; NoComparison>] DissolveData =
    { IncomingTime : int64
      OutgoingTime : int64
      DissolveImage : Image AssetTag }

/// Describes the behavior of the screen splash algorithm.
type [<StructuralEquality; NoComparison>] SplashData =
    { DissolveData : DissolveData
      IdlingTime : int64
      SplashImage : Image AssetTag }

/// The data needed to describe a Tiled tile map.
type [<StructuralEquality; NoComparison>] TileMapData =
    { Map : TmxMap
      MapSize : Vector2i
      TileSize : Vector2i
      TileSizeF : Vector2
      TileMapSize : Vector2i
      TileMapSizeF : Vector2
      TileSet : TmxTileset
      TileSetSize : Vector2i }

/// The data needed to describe a Tiled tile.
type [<StructuralEquality; NoComparison>] TileData =
    { Tile : TmxLayerTile
      I : int
      J : int
      Gid : int
      GidPosition : int
      Gid2 : Vector2i
      TileSetTileOpt : TmxTilesetTile option
      TilePosition : Vector2i }

/// Describes the shape of a desired overlay.
type OverlayNameDescriptor =
    | NoOverlay
    | RoutedOverlay
    | DefaultOverlay
    | ExplicitOverlay of string

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

[<AutoOpen>]
module WorldTypes =

    let mutable internal getPropertyOpt = Unchecked.defaultof<string -> Propertied -> obj -> obj option>
    let mutable internal setPropertyOpt = Unchecked.defaultof<string -> Propertied -> obj option -> Type -> obj -> obj>
    let mutable internal handlePropertyChange = Unchecked.defaultof<string -> Propertied -> obj -> obj -> obj * obj>
        
    /// Represents an unsubscription operation for an event.
    type Unsubscription = World -> World

    /// The data for a change in the world's ambient state.
    and [<Struct; StructuralEquality; NoComparison>] AmbientChangeData = 
        { OldWorldWithOldState : World }

    /// Describes the information needed to sort simulants.
    /// OPTIMIZATION: carries related simulant to avoid GC pressure.
    and [<Struct; CustomEquality; CustomComparison>] SortPriority =
        { SortDepth : single
          SortTarget : Simulant }

        static member equals left right =
            left.SortDepth = right.SortDepth &&
            left.SortTarget = right.SortTarget

        static member compare left right =
            if left.SortDepth < right.SortDepth then 1
            elif left.SortDepth > right.SortDepth then -1
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
        default dispatcher.Register (_, world) = world

        /// Unregister a game when finished with the world.
        abstract Unregister : Game * World -> World
        default dispatcher.Unregister (_, world) = world

        /// Update a game.
        abstract Update : Game * World -> World
        default dispatcher.Update (_, world) = world

        /// Post-update a game.
        abstract PostUpdate : Game * World -> World
        default dispatcher.PostUpdate (_, world) = world

        /// Actualize a game.
        abstract Actualize : Game * World -> World
        default dispatcher.Actualize (_, world) = world

        /// Send a signal to a game.
        abstract Signal : Symbol * Game * World -> World
        default dispatcher.Signal (_, _, world) = world

        /// Try to get a calculated property with the given name.
        abstract TryGetCalculatedProperty : string * Game * World -> Property option
        default dispatcher.TryGetCalculatedProperty (_, _, _) = None
    
    /// The default dispatcher for screens.
    and ScreenDispatcher () =
        inherit SimulantDispatcher ()
    
        /// Register a screen when adding it to the world.
        abstract Register : Screen * World -> World
        default dispatcher.Register (_, world) = world
    
        /// Unregister a screen when removing it from the world.
        abstract Unregister : Screen * World -> World
        default dispatcher.Unregister (_, world) = world
    
        /// Update a screen.
        abstract Update : Screen * World -> World
        default dispatcher.Update (_, world) = world
    
        /// Post-update a screen.
        abstract PostUpdate : Screen * World -> World
        default dispatcher.PostUpdate (_, world) = world
    
        /// Actualize a screen.
        abstract Actualize : Screen * World -> World
        default dispatcher.Actualize (_, world) = world
    
        /// Send a signal to a screen.
        abstract Signal : Symbol * Screen * World -> World
        default dispatcher.Signal (_, _, world) = world

        /// Try to get a calculated property with the given name.
        abstract TryGetCalculatedProperty : string * Screen * World -> Property option
        default dispatcher.TryGetCalculatedProperty (_, _, _) = None
    
    /// The default dispatcher for layers.
    and LayerDispatcher () =
        inherit SimulantDispatcher ()
    
        /// Register a layer when adding it to a screen.
        abstract Register : Layer * World -> World
        default dispatcher.Register (_, world) = world
    
        /// Unregister a layer when removing it from a screen.
        abstract Unregister : Layer * World -> World
        default dispatcher.Unregister (_, world) = world
    
        /// Update a layer.
        abstract Update : Layer * World -> World
        default dispatcher.Update (_, world) = world
    
        /// Post-update a layer.
        abstract PostUpdate : Layer * World -> World
        default dispatcher.PostUpdate (_, world) = world
    
        /// Actualize a layer.
        abstract Actualize : Layer * World -> World
        default dispatcher.Actualize (_, world) = world
    
        /// Send a signal to a layer.
        abstract Signal : Symbol * Layer * World -> World
        default dispatcher.Signal (_, _, world) = world

        /// Try to get a calculated property with the given name.
        abstract TryGetCalculatedProperty : string * Layer * World -> Property option
        default dispatcher.TryGetCalculatedProperty (_, _, _) = None
    
    /// The default dispatcher for entities.
    and EntityDispatcher () =
        inherit SimulantDispatcher ()
    
        static member Properties =
            [Define? Imperative false
             Define? Persistent true
             Define? Position Vector2.Zero
             Define? Size Constants.Engine.DefaultEntitySize
             Define? Rotation 0.0f
             Define? Depth 0.0f
             Define? Overflow Vector2.Zero
             Define? ViewType Relative
             Define? Visible true
             Define? Enabled true
             Define? Omnipresent false
             Define? AlwaysUpdate false
             Define? PublishChanges false
             Define? PublishUpdates false
             Define? PublishPostUpdates false]
    
        /// Register an entity when adding it to a layer.
        abstract Register : Entity * World -> World
        default dispatcher.Register (_, world) = world
    
        /// Unregister an entity when removing it from a layer.
        abstract Unregister : Entity * World -> World
        default dispatcher.Unregister (_, world) = world
    
        /// Propagate an entity's physics properties from the physics subsystem.
        abstract PropagatePhysics : Entity * World -> World
        default dispatcher.PropagatePhysics (_, world) = world
    
        /// Update an entity.
        abstract Update : Entity * World -> World
        default dispatcher.Update (_, world) = world
    
        /// Post-update an entity.
        abstract PostUpdate : Entity * World -> World
        default dispatcher.PostUpdate (_, world) = world
    
        /// Actualize an entity.
        abstract Actualize : Entity * World -> World
        default dispatcher.Actualize (_, world) = world
    
        /// Send a signal to an entity.
        abstract Signal : Symbol * Entity * World -> World
        default dispatcher.Signal (_, _, world) = world
    
        /// Get the quick size of an entity (the appropriate user-defined size for an entity).
        abstract GetQuickSize : Entity * World -> Vector2
        default dispatcher.GetQuickSize (_, _) = Constants.Engine.DefaultEntitySize

        /// Try to get a calculated property with the given name.
        abstract TryGetCalculatedProperty : string * Entity * World -> Property option
        default dispatcher.TryGetCalculatedProperty (_, _, _) = None

    /// Dynamically augments an entity's behavior in a composable way.
    and Facet () =
    
        /// Register a facet when adding it to an entity.
        abstract Register : Entity * World -> World
        default facet.Register (entity, world) = facet.RegisterPhysics (entity, world)
    
        /// Unregister a facet when removing it from an entity.
        abstract Unregister : Entity * World -> World
        default facet.Unregister (entity, world) = facet.UnregisterPhysics (entity, world)
    
        /// Participate in the registration of an entity's physics with the physics subsystem.
        abstract RegisterPhysics : Entity * World -> World
        default facet.RegisterPhysics (_, world) = world
    
        /// Participate in the unregistration of an entity's physics from the physics subsystem.
        abstract UnregisterPhysics : Entity * World -> World
        default facet.UnregisterPhysics (_, world) = world
    
        /// Participate in the propagation an entity's physics properties from the physics subsystem.
        abstract PropagatePhysics : Entity * World -> World
        default facet.PropagatePhysics (_, world) = world
    
        /// Update a facet.
        abstract Update : Entity * World -> World
        default facet.Update (_, world) = world
    
        /// Post-update a facet.
        abstract PostUpdate : Entity * World -> World
        default facet.PostUpdate (_, world) = world
    
        /// Actualize a facet.
        abstract Actualize : Entity * World -> World
        default facet.Actualize (_, world) = world
    
        /// Send a signal to a facet.
        abstract Signal : Symbol * Entity * World -> World
        default facet.Signal (symbol, entity, world) = facet.Signal (symbol, entity, world)
    
        /// Participate in getting the priority with which an entity is picked in the editor.
        abstract GetQuickSize : Entity * World -> Vector2
        default facet.GetQuickSize (_, _) = Constants.Engine.DefaultEntitySize

        /// Try to get a calculated property with the given name.
        abstract TryGetCalculatedProperty : string * Entity * World -> Property option
        default facet.TryGetCalculatedProperty (_, _, _) = None

    /// Generalized interface for simulant state.
    and SimulantState =
        interface
            abstract member GetXtension : unit -> Xtension
            end

    /// Hosts the ongoing state of a game. The end-user of this engine should never touch this
    /// type, and it's public _only_ to make [<CLIMutable>] work.
    /// NOTE: The properties here have duplicated representations in WorldModuleGame that exist
    /// for performance that must be kept in sync.
    and [<CLIMutable; NoEquality; NoComparison>] GameState =
        { Id : Guid
          Xtension : Xtension
          Dispatcher : GameDispatcher
          CreationTimeStamp : int64
          ScriptOpt : Symbol AssetTag option
          Script : Scripting.Expr array
          ScriptFrame : Scripting.DeclarationFrame
          ScriptUnsubscriptions : Unsubscription list
          OnRegister : Scripting.Expr
          OnUnregister : Scripting.Expr
          OnUpdate : Scripting.Expr
          OnPostUpdate : Scripting.Expr
          OnSignal : Scripting.Expr
          OmniScreenOpt : Screen option
          SelectedScreenOpt : Screen option
          ScreenTransitionDestinationOpt : Screen option
          EyeCenter : Vector2
          EyeSize : Vector2 }

        /// Make a game state value.
        static member make (dispatcher : GameDispatcher) =
            let eyeCenter = Vector2.Zero
            // TODO: P1: consider if eyeSize is too hard-coded
            let eyeSize = Vector2 (single Constants.Render.DefaultResolutionX, single Constants.Render.DefaultResolutionY)
            { Id = makeGuid ()
              Xtension = Xtension.makeSafe ()
              Dispatcher = dispatcher
              CreationTimeStamp = Core.getTimeStamp ()
              ScriptOpt = None
              Script = [||]
              ScriptFrame = Scripting.DeclarationFrame HashIdentity.Structural
              ScriptUnsubscriptions = []
              OnRegister = Scripting.Unit
              OnUnregister = Scripting.Unit
              OnUpdate = Scripting.Unit
              OnPostUpdate = Scripting.Unit
              OnSignal = Scripting.Unit
              OmniScreenOpt = None
              SelectedScreenOpt = None
              ScreenTransitionDestinationOpt = None
              EyeCenter = eyeCenter
              EyeSize = eyeSize }

        /// Try to get an xtension property and its type information.
        static member tryGetProperty propertyName gameState =
            Xtension.tryGetProperty propertyName gameState.Xtension

        /// Get an xtension property and its type information.
        static member getProperty propertyName gameState =
            Xtension.getProperty propertyName gameState.Xtension

        /// Try to set an xtension property with explicit type information.
        static member trySetProperty propertyName property gameState =
            match Xtension.trySetProperty propertyName property gameState.Xtension with
            | (true, xtension) -> (true, { gameState with Xtension = xtension })
            | (false, _) -> (false, gameState)

        /// Set an xtension property with explicit type information.
        static member setProperty propertyName property gameState =
            { gameState with GameState.Xtension = Xtension.setProperty propertyName property gameState.Xtension }

        /// Attach an xtension property.
        static member attachProperty name property gameState =
            { gameState with GameState.Xtension = Xtension.attachProperty name property gameState.Xtension }

        /// Detach an xtension property.
        static member detachProperty name gameState =
            let xtension = Xtension.detachProperty name gameState.Xtension
            { gameState with GameState.Xtension = xtension }

        /// Copy a game such as when, say, you need it to be mutated with reflection but you need to preserve persistence.
        static member copy this =
            { this with GameState.Id = this.Id }

        interface SimulantState with
            member this.GetXtension () = this.Xtension
    
    /// Hosts the ongoing state of a screen. The end-user of this engine should never touch this
    /// type, and it's public _only_ to make [<CLIMutable>] work.
    /// NOTE: The properties here have duplicated representations in WorldModuleScreen that exist
    /// for performance that must be kept in sync.
    and [<CLIMutable; NoEquality; NoComparison>] ScreenState =
        { Id : Guid
          Name : string
          Xtension : Xtension
          Dispatcher : ScreenDispatcher
          Persistent : bool
          CreationTimeStamp : int64
          ScriptOpt : Symbol AssetTag option
          Script : Scripting.Expr array
          ScriptFrame : Scripting.DeclarationFrame
          ScriptUnsubscriptions : Unsubscription list
          OnRegister : Scripting.Expr
          OnUnregister : Scripting.Expr
          OnUpdate : Scripting.Expr
          OnPostUpdate : Scripting.Expr
          OnSignal : Scripting.Expr
          EntityTree : Entity SpatialTree MutantCache
          TransitionState : TransitionState
          TransitionTicks : int64
          Incoming : Transition
          Outgoing : Transition }
          
        /// Make a screen state value.
        static member make nameOpt (dispatcher : ScreenDispatcher) =
            let (id, name) = Reflection.deriveIdAndName nameOpt
            let spatialTree = SpatialTree.make Constants.Engine.EntityTreeGranularity Constants.Engine.EntityTreeDepth Constants.Engine.EntityTreeBounds
            { Id = id
              Name = name
              Xtension = Xtension.makeSafe ()
              Dispatcher = dispatcher
              Persistent = true
              CreationTimeStamp = Core.getTimeStamp ()
              ScriptOpt = None
              Script = [||]
              ScriptFrame = Scripting.DeclarationFrame HashIdentity.Structural
              ScriptUnsubscriptions = []
              OnRegister = Scripting.Unit
              OnUnregister = Scripting.Unit
              OnUpdate = Scripting.Unit
              OnPostUpdate = Scripting.Unit
              OnSignal = Scripting.Unit
              EntityTree = MutantCache.make Operators.id spatialTree
              TransitionState = IdlingState
              TransitionTicks = 0L // TODO: roll this field into Incoming/OutgoingState values
              Incoming = Transition.make Incoming
              Outgoing = Transition.make Outgoing }

        /// Try to get an xtension property and its type information.
        static member tryGetProperty propertyName screenState =
            Xtension.tryGetProperty propertyName screenState.Xtension

        /// Get an xtension property and its type information.
        static member getProperty propertyName screenState =
            Xtension.getProperty propertyName screenState.Xtension

        /// Try to set an xtension property with explicit type information.
        static member trySetProperty propertyName property screenState =
            match Xtension.trySetProperty propertyName property screenState.Xtension with
            | (true, xtension) -> (true, { screenState with Xtension = xtension })
            | (false, _) -> (false, screenState)

        /// Set an xtension property with explicit type information.
        static member setProperty propertyName property screenState =
            { screenState with ScreenState.Xtension = Xtension.setProperty propertyName property screenState.Xtension }

        /// Attach an xtension property.
        static member attachProperty name property screenState =
            { screenState with ScreenState.Xtension = Xtension.attachProperty name property screenState.Xtension }

        /// Detach an xtension property.
        static member detachProperty name screenState =
            let xtension = Xtension.detachProperty name screenState.Xtension
            { screenState with ScreenState.Xtension = xtension }

        /// Copy a screen such as when, say, you need it to be mutated with reflection but you need to preserve persistence.
        static member copy this =
            { this with ScreenState.Id = this.Id }

        interface SimulantState with
            member this.GetXtension () = this.Xtension
    
    /// Hosts the ongoing state of a layer. The end-user of this engine should never touch this
    /// type, and it's public _only_ to make [<CLIMutable>] work.
    /// NOTE: The properties here have duplicated representations in WorldModuleLayer that exist
    /// for performance that must be kept in sync.
    and [<CLIMutable; NoEquality; NoComparison>] LayerState =
        { Id : Guid
          Name : string
          Xtension : Xtension
          Dispatcher : LayerDispatcher
          Persistent : bool
          CreationTimeStamp : int64
          ScriptOpt : Symbol AssetTag option
          Script : Scripting.Expr array
          ScriptFrame : Scripting.DeclarationFrame
          ScriptUnsubscriptions : Unsubscription list
          OnRegister : Scripting.Expr
          OnUnregister : Scripting.Expr
          OnUpdate : Scripting.Expr
          OnPostUpdate : Scripting.Expr
          OnSignal : Scripting.Expr
          Depth : single
          Visible : bool }

        /// Make a layer state value.
        static member make nameOpt (dispatcher : LayerDispatcher) =
            let (id, name) = Reflection.deriveIdAndName nameOpt
            { LayerState.Id = id
              Name = name
              Xtension = Xtension.makeSafe ()
              Dispatcher = dispatcher
              Persistent = true
              CreationTimeStamp = Core.getTimeStamp ()
              ScriptOpt = None
              Script = [||]
              ScriptFrame = Scripting.DeclarationFrame HashIdentity.Structural
              ScriptUnsubscriptions = []
              OnRegister = Scripting.Unit
              OnUnregister = Scripting.Unit
              OnUpdate = Scripting.Unit
              OnPostUpdate = Scripting.Unit
              OnSignal = Scripting.Unit
              Depth = 0.0f
              Visible = true }

        /// Try to get an xtension property and its type information.
        static member tryGetProperty propertyName layerState =
            Xtension.tryGetProperty propertyName layerState.Xtension

        /// Get an xtension property and its type information.
        static member getProperty propertyName layerState =
            Xtension.getProperty propertyName layerState.Xtension

        /// Try to set an xtension property with explicit type information.
        static member trySetProperty propertyName property layerState =
            match Xtension.trySetProperty propertyName property layerState.Xtension with
            | (true, xtension) -> (true, { layerState with Xtension = xtension })
            | (false, _) -> (false, layerState)

        /// Set an xtension property with explicit type information.
        static member setProperty propertyName property layerState =
            { layerState with LayerState.Xtension = Xtension.setProperty propertyName property layerState.Xtension }

        /// Attach an xtension property.
        static member attachProperty name property layerState =
            { layerState with LayerState.Xtension = Xtension.attachProperty name property layerState.Xtension }

        /// Detach an xtension property.
        static member detachProperty name layerState =
            let xtension = Xtension.detachProperty name layerState.Xtension
            { layerState with LayerState.Xtension = xtension }

        /// Copy a layer such as when, say, you need it to be mutated with reflection but you need to preserve persistence.
        static member copy this =
            { this with LayerState.Id = this.Id }

        interface SimulantState with
            member this.GetXtension () = this.Xtension

    /// Hosts the ongoing state of an entity. The end-user of this engine should never touch this
    /// type, and it's public _only_ to make [<CLIMutable>] work.
    /// NOTE: The properties here have duplicated representations in WorldModuleEntity that exist
    /// for performance that must be kept in sync.
    and [<CLIMutable; NoEquality; NoComparison>] EntityState =
        { Id : Guid
          Name : string
          mutable Xtension : Xtension
          Dispatcher : EntityDispatcher
          mutable Imperative : bool
          mutable Persistent : bool
          CreationTimeStamp : int64 // just needed for ordering writes to reduce diff volumes
          Cachable : bool
          mutable OverlayNameOpt : string option
          mutable Position : Vector2 // NOTE: will become a Vector3 if Nu gets 3d capabilities
          mutable Size : Vector2 // NOTE: will become a Vector3 if Nu gets 3d capabilities
          mutable Rotation : single // NOTE: will become a Vector3 if Nu gets 3d capabilities
          mutable Depth : single // NOTE: will become part of position if Nu gets 3d capabilities
          mutable Overflow : Vector2
          mutable ViewType : ViewType
          mutable Visible : bool
          mutable Enabled : bool
          mutable Omnipresent : bool
          mutable AlwaysUpdate : bool
          mutable PublishChanges : bool
          mutable PublishUpdates : bool
          mutable PublishPostUpdates : bool
          mutable FacetNames : string Set
          mutable Facets : Facet list }

        /// Make an entity state value.
        static member make nameOpt overlayNameOpt (dispatcher : EntityDispatcher) =
            let (id, name) = Reflection.deriveIdAndName nameOpt
            { Id = id
              Name = name
              Xtension = Xtension.makeSafe ()
              Dispatcher = dispatcher
              Imperative = false
              Persistent = true
              CreationTimeStamp = Core.getTimeStamp ()
              Cachable = String.endsWithGuid name
              OverlayNameOpt = overlayNameOpt
              Position = Vector2.Zero
              Size = Constants.Engine.DefaultEntitySize
              Rotation = 0.0f
              Depth = 0.0f
              Overflow = Vector2.Zero
              ViewType = Relative
              Visible = true
              Enabled = true
              Omnipresent = false
              AlwaysUpdate = false
              PublishChanges = false
              PublishUpdates = false
              PublishPostUpdates = false
              FacetNames = Set.empty
              Facets = [] }

        /// Try to get an xtension property and its type information.
        static member tryGetProperty propertyName entityState =
            Xtension.tryGetProperty propertyName entityState.Xtension

        /// Get an xtension property and its type information.
        static member getProperty propertyName entityState =
            Xtension.getProperty propertyName entityState.Xtension

        /// Try to set an xtension property with explicit type information.
        static member trySetProperty propertyName property entityState =
            match Xtension.trySetProperty propertyName property entityState.Xtension with
            | (true, xtension) -> (true, { entityState with Xtension = xtension })
            | (false, _) -> (false, entityState)

        /// Set an xtension property with explicit type information.
        static member setProperty propertyName property entityState =
            { entityState with EntityState.Xtension = Xtension.setProperty propertyName property entityState.Xtension }

        /// Attach an xtension property.
        static member attachProperty name property entityState =
            { entityState with EntityState.Xtension = Xtension.attachProperty name property entityState.Xtension }

        /// Detach an xtension property.
        static member detachProperty name entityState =
            let xtension = Xtension.detachProperty name entityState.Xtension
            if Xtension.getImperative entityState.Xtension then entityState.Xtension <- xtension; entityState
            else { entityState with EntityState.Xtension = xtension }

        /// Get an entity state's transform.
        static member getTransform this =
            { Transform.Position = this.Position
              Size = this.Size
              Rotation = this.Rotation
              Depth = this.Depth }

        /// Set an entity state's transform.
        static member setTransform (value : Transform) (this : EntityState) =
            if Xtension.getImperative this.Xtension then
                this.Position <- value.Position
                this.Size <- value.Size
                this.Rotation <- value.Rotation
                this.Depth <- value.Depth
                this
            else
                { this with
                    Position = value.Position
                    Size = value.Size
                    Rotation = value.Rotation
                    Depth = value.Depth }

        /// Copy an entity such as when, say, you need it to be mutated with reflection but you need to preserve persistence.
        static member copy this =
            if Xtension.getImperative this.Xtension then this
            else { this with EntityState.Id = this.Id }

        interface SimulantState with
            member this.GetXtension () = this.Xtension

    /// A simulant in the world.
    and Simulant =
        interface
            inherit Participant
            abstract member SimulantAddress : Simulant Address
            end

    /// Operators for the Simulant type.
    and SimulantOperators =
        private
            | SimulantOperators

        /// Concatenate two addresses, forcing the type of first address.
        static member acatff<'a> (address : 'a Address) (simulant : Simulant) = acatff address simulant.SimulantAddress

        /// Concatenate two addresses, forcing the type of first address.
        static member (-->) (address, simulant : Simulant) = SimulantOperators.acatff address simulant

    /// The game type that hosts the various screens used to navigate through a game.
    and Game (gameAddress) =

        // check that address is of correct length for a game
        do if Address.length gameAddress <> 0 then failwith "Game address must be length of 0."

        /// Create a game reference.
        new () = Game (Address.empty)

        /// The address of the game.
        member this.GameAddress = gameAddress
        
        /// Helper for accessing strongly-type entity property tags.
        static member Prop = Unchecked.defaultof<Game>
        static member op_Implicit () = Game ()
        static member op_Implicit (gameAddress : Game Address) = Game gameAddress

        interface Simulant with
            member this.ParticipantAddress = atoa<Game, Participant> this.GameAddress
            member this.SimulantAddress = atoa<Game, Simulant> this.GameAddress
            end

        override this.Equals that =
            match that with
            | :? Game as that -> this.GameAddress.Equals that.GameAddress
            | _ -> false

        override this.GetHashCode () = this.GameAddress.GetHashCode ()

        interface Game IComparable with
            member this.CompareTo that =
                Address.compare this.GameAddress that.GameAddress

        interface IComparable with
            member this.CompareTo that =
                match that with
                | :? Game as that -> (this :> Game IComparable).CompareTo that
                | _ -> failwith "Invalid Game comparison (comparee not of type Game)."

        override this.ToString () = scstring this.GameAddress

        /// Get the latest value of a game's properties.
        [<DebuggerBrowsable (DebuggerBrowsableState.RootHidden)>]
        member private this.View = Debug.World.viewGame Debug.World.Chosen

        /// Concatenate two addresses, forcing the type of first address.
        static member acatff<'a> (address : 'a Address) (game : Game) = acatff address game.GameAddress

        /// Concatenate two addresses, forcing the type of first address.
        static member (-->) (address : 'a Address, game) = Game.acatff address game

    /// The screen type that allows transitioning to and from other screens, and also hosts the
    /// currently interactive layers of entities.
    and Screen (screenAddress) =

        // check that address is of correct length for a screen
        do if Address.length screenAddress <> 1 then failwith "Screen address must be length of 1."

        /// Create a screen reference from a name string.
        new (screenName : string) = Screen (ntoa screenName)

        /// The address of the screen.
        member this.ScreenAddress = screenAddress

        /// Helper for accessing strongly-type entity property tags.
        static member Prop = Unchecked.defaultof<Screen>
        static member op_Implicit (screenName : string) = Screen screenName
        static member op_Implicit (screenAddress : Screen Address) = Screen screenAddress

        interface Simulant with
            member this.ParticipantAddress = atoa<Screen, Participant> this.ScreenAddress
            member this.SimulantAddress = atoa<Screen, Simulant> this.ScreenAddress
            end

        override this.Equals that =
            match that with
            | :? Screen as that -> this.ScreenAddress.Equals that.ScreenAddress
            | _ -> false

        override this.GetHashCode () = this.ScreenAddress.GetHashCode ()

        interface Screen IComparable with
            member this.CompareTo that =
                Address.compare this.ScreenAddress that.ScreenAddress

        interface IComparable with
            member this.CompareTo that =
                match that with
                | :? Screen as that -> (this :> Screen IComparable).CompareTo that
                | _ -> failwith "Invalid Screen comparison (comparee not of type Screen)."

        override this.ToString () = scstring this.ScreenAddress

        /// Get the name of a screen.
        member this.ScreenName = Address.getName this.ScreenAddress

        /// Get the latest value of a screen's properties.
        [<DebuggerBrowsable (DebuggerBrowsableState.RootHidden)>]
        member private this.View = Debug.World.viewScreen (this :> obj) Debug.World.Chosen

        /// Concatenate two addresses, forcing the type of first address.
        static member acatff<'a> (address : 'a Address) (screen : Screen) = acatff address screen.ScreenAddress

        /// Concatenate two addresses, forcing the type of first address.
        static member (-->) (address : 'a Address, screen) = Screen.acatff address screen

        /// Derive a layer from its screen.
        static member (/) (screen : Screen, layerName) = Layer (atoa<Screen, Layer> screen.ScreenAddress --> ntoa layerName)
    
    /// Forms a logical layer of entities.
    and Layer (layerAddress) =

        // check that address is of correct length for a layer
        do if Address.length layerAddress <> 2 then failwith "Layer address must be length of 2."

        /// Create a layer reference from an address string.
        new (layerAddressStr : string) = Layer (stoa layerAddressStr)

        /// Create a layer reference from a list of names.
        new (layerNames : string list) = Layer (ltoa layerNames)

        /// Create a layer reference from a the required names.
        new (screenName : string, layerName : string) = Layer [screenName; layerName]

        /// The address of the layer.
        member this.LayerAddress = layerAddress

        /// Helper for accessing strongly-type entity property tags.
        static member Prop = Unchecked.defaultof<Layer>
        static member op_Implicit (layerName : string) = Layer layerName
        static member op_Implicit (layerAddress : Layer Address) = Layer layerAddress

        interface Simulant with
            member this.ParticipantAddress = atoa<Layer, Participant> this.LayerAddress
            member this.SimulantAddress = atoa<Layer, Simulant> this.LayerAddress
            end

        override this.Equals that =
            match that with
            | :? Layer as that -> this.LayerAddress.Equals that.LayerAddress
            | _ -> false

        override this.GetHashCode () = this.LayerAddress.GetHashCode ()

        interface Layer IComparable with
            member this.CompareTo that =
                Address.compare this.LayerAddress that.LayerAddress

        interface IComparable with
            member this.CompareTo that =
                match that with
                | :? Layer as that -> (this :> Layer IComparable).CompareTo that
                | _ -> failwith "Invalid Layer comparison (comparee not of type Layer)."
    
        override this.ToString () = scstring this.LayerAddress
    
        /// Get the name of a layer.
        member this.LayerName = Address.getName this.LayerAddress
    
        /// Get the latest value of a layer's properties.
        [<DebuggerBrowsable (DebuggerBrowsableState.RootHidden)>]
        member private this.View = Debug.World.viewLayer (this :> obj) Debug.World.Chosen

        /// Concatenate two addresses, forcing the type of first address.
        static member acatff<'a> (address : 'a Address) (layer : Layer) = acatff address layer.LayerAddress
    
        /// Concatenate two addresses, forcing the type of first address.
        static member (-->) (address : 'a Address, layer) = Layer.acatff address layer
        
        /// Derive an entity from its layer.
        static member (/) (layer : Layer, entityName) = Entity (atoa<Layer, Entity> layer.LayerAddress --> ntoa entityName)
    
    /// The type around which the whole game engine is based! Used in combination with dispatchers
    /// to implement things like buttons, characters, blocks, and things of that sort.
    /// OPTIMIZATION: Includes pre-constructed entity change and update event addresses to avoid
    /// reconstructing new ones for each entity every frame.
    and Entity (entityAddress) =

        // check that address is of correct length for an entity
        do if Address.length entityAddress <> 3 then failwith "Entity address must be length of 3."
        let updateEvent = ltoa<unit> ["Update"; "Event"] --> entityAddress
        let postUpdateEvent = ltoa<unit> ["PostUpdate"; "Event"] --> entityAddress
        let mutable entityStateOpt = Unchecked.defaultof<EntityState>

        // Create an entity reference from an address string.
        new (entityAddressStr : string) = Entity (stoa entityAddressStr)

        // Create an entity reference from a list of names.
        new (entityNames : string list) = Entity (ltoa entityNames)

        // Create an entity reference from a the required names.
        new (screenName : string, layerName : string, entityName : string) = Entity [screenName; layerName; entityName]

        /// The address of the entity.
        member this.EntityAddress = entityAddress

        /// The address of the entity's update event.
        member this.UpdateEvent = updateEvent

        /// The address of the entity's post-update event.
        member this.PostUpdateEvent = postUpdateEvent

        /// The cached entity state for imperative entities.
        member this.EntityStateOpt
            with get () = entityStateOpt
            and set value = entityStateOpt <- value

        /// Helper for accessing strongly-type entity property tags.
        static member Prop = Unchecked.defaultof<Entity>
        static member op_Implicit (entityName : string) = Entity entityName
        static member op_Implicit (entityAddress : Entity Address) = Entity entityAddress
        
        interface Simulant with
            member this.ParticipantAddress = atoa<Entity, Participant> this.EntityAddress
            member this.SimulantAddress = atoa<Entity, Simulant> this.EntityAddress
            end

        override this.Equals that =
            match that with
            | :? Entity as that -> this.EntityAddress.Equals that.EntityAddress
            | _ -> false

        override this.GetHashCode () = this.EntityAddress.GetHashCode ()

        interface Entity IComparable with
            member this.CompareTo that =
                Address.compare this.EntityAddress that.EntityAddress

        interface IComparable with
            member this.CompareTo that =
                match that with
                | :? Entity as that -> (this :> Entity IComparable).CompareTo that
                | _ -> failwith "Invalid Entity comparison (comparee not of type Entity)."

        override this.ToString () = scstring this.EntityAddress

        /// Get the name of an entity.
        member this.EntityName = Address.getName this.EntityAddress
    
        /// Get the latest value of an entity's properties.
        [<DebuggerBrowsable (DebuggerBrowsableState.RootHidden)>]
        member private this.View = Debug.World.viewEntity (this :> obj) Debug.World.Chosen

        /// Concatenate two addresses, forcing the type of first address.
        static member acatff<'a> (address : 'a Address) (entity : Entity) = acatff address entity.EntityAddress
    
        /// Concatenate two addresses, forcing the type of first address.
        static member (-->) (address : 'a Address, entity) = Entity.acatff address entity
    
    /// The world's dispatchers (including facets).
    /// 
    /// I would prefer this type to be inlined in World, but it has been extracted to its own white-box
    /// type for efficiency reasons.
    and [<ReferenceEquality>] internal Dispatchers =
        { GameDispatchers : Map<string, GameDispatcher>
          ScreenDispatchers : Map<string, ScreenDispatcher>
          LayerDispatchers : Map<string, LayerDispatcher>
          EntityDispatchers : Map<string, EntityDispatcher>
          Facets : Map<string, Facet>
          TryGetExtrinsic : string -> World ScriptingTrinsic FOption
          UpdateEntityInEntityTree : bool -> ViewType -> Vector4 -> Entity -> World -> World -> World
          RebuildEntityTree : Screen -> World -> Entity SpatialTree }
    
    /// The world, in a functional programming sense. Hosts the game object, the dependencies needed
    /// to implement a game, messages to by consumed by the various engine sub-systems, and general
    /// configuration data.
    ///
    /// For efficiency, this type is kept under 64 bytes on 32-bit machines as to not exceed the size
    /// of a typical cache line.
    ///
    /// NOTE: this would be better as private, but there is just too much code to fit in this file
    /// for that.
    and [<ReferenceEquality>] World =
        internal
            { EventSystemDelegate : World EventSystemDelegate
              Dispatchers : Dispatchers
              Subsystems : World Subsystems
              ScriptingEnv : Scripting.Env
              ScriptingContext : Simulant
              ScreenCachedOpt : KeyedCache<KeyValuePair<Screen Address, UMap<Screen Address, ScreenState>>, ScreenState FOption>
              LayerCachedOpt : KeyedCache<KeyValuePair<Layer Address, UMap<Layer Address, LayerState>>, LayerState FOption>
              EntityCachedOpt : KeyedCache<KeyValuePair<Entity Address, UMap<Entity Address, EntityState>>, EntityState FOption>
              ScreenDirectory : UMap<string, KeyValuePair<Screen Address, UMap<string, KeyValuePair<Layer Address, UMap<string, Entity Address>>>>>
              AmbientState : World AmbientState
              GameState : GameState
              ScreenStates : UMap<Screen Address, ScreenState>
              LayerStates : UMap<Layer Address, LayerState>
              EntityStates : UMap<Entity Address, EntityState> }

        interface EventSystem<World> with
            
            member this.GetLiveness () =
                AmbientState.getLiveness this.AmbientState
            
            member this.GetGlobalParticipantSpecialized () =
                EventSystemDelegate.getGlobalParticipantSpecialized this.EventSystemDelegate
            
            member this.GetGlobalParticipantGeneralized () =
                EventSystemDelegate.getGlobalParticipantGeneralized this.EventSystemDelegate
            
            member this.ParticipantExists participant =
                match participant with
                | :? GlobalParticipantGeneralized
                | :? Game -> true
                | :? Screen as screen -> UMap.containsKey screen.ScreenAddress this.ScreenStates
                | :? Layer as layer -> UMap.containsKey layer.LayerAddress this.LayerStates
                | :? Entity as entity -> UMap.containsKey entity.EntityAddress this.EntityStates
                | _  -> false
            
            member this.GetPropertyOpt<'a> propertyName participant =
                match getPropertyOpt propertyName participant (box this) with
                | Some a -> Some (a :?> 'a)
                | None -> None
            
            member this.SetPropertyOpt<'a> propertyName participant (value : 'a option) =
                let world =
                    match value with
                    | Some a -> setPropertyOpt propertyName participant (Some (box a)) typeof<'a> (box this)
                    | None -> setPropertyOpt propertyName participant None typeof<'a> (box this)
                world :?> World
            
            member this.HandlePropertyChange propertyName participant handler =
                let (unsubscribe, world) = handlePropertyChange propertyName participant (box handler) (box this)
                (unsubscribe :?> World -> World, world :?> World)
            
            member this.GetEventSystemDelegateHook () =
                this.EventSystemDelegate
            
            member this.UpdateEventSystemDelegateHook updater =
                let this = { this with EventSystemDelegate = updater this.EventSystemDelegate }
#if DEBUG
                // inlined choose
                Debug.World.Chosen <- this :> obj
#endif
                this
            
            member this.PublishEventHook (participant : Participant) publisher eventData eventAddress eventTrace subscription world =
                let (handling, world) =
                    match participant with
                    | :? GlobalParticipantGeneralized -> EventSystem.publishEvent<'a, 'p, Participant, World> participant publisher eventData eventAddress eventTrace subscription world
                    | :? Game -> EventSystem.publishEvent<'a, 'p, Game, World> participant publisher eventData eventAddress eventTrace subscription world
                    | :? Screen -> EventSystem.publishEvent<'a, 'p, Screen, World> participant publisher eventData eventAddress eventTrace subscription world
                    | :? Layer -> EventSystem.publishEvent<'a, 'p, Layer, World> participant publisher eventData eventAddress eventTrace subscription world
                    | :? Entity -> EventSystem.publishEvent<'a, 'p, Entity, World> participant publisher eventData eventAddress eventTrace subscription world
                    | _ -> failwithumf ()
#if DEBUG
                // inlined choose
                Debug.World.Chosen <- world :> obj
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
                | ("Game", Scripting.String str) | ("Game", Scripting.Keyword str) -> str |> stoa |> Game :> obj |> Some
                | ("Screen", Scripting.String str) | ("Screen", Scripting.Keyword str) -> str |> stoa |> Screen :> obj |> Some
                | ("Layer", Scripting.String str) | ("Layer", Scripting.Keyword str) -> str |> stoa |> Layer :> obj |> Some
                | ("Entity", Scripting.String str) | ("Entity", Scripting.Keyword str) -> str |> stoa |> Entity :> obj |> Some
                | ("Simulant", Scripting.String _) | ("Simulant", Scripting.Keyword _) -> None // TODO: P1: see if this should be failwithumf or a violation instead.
                | (_, _) -> None

    /// Provides a way to make user-defined dispatchers, facets, and various other sorts of game-
    /// specific values.
    type NuPlugin () =

        /// Make user-defined subsystems such that Nu can utilitze them at run-time.
        abstract MakeSubsystems : unit -> (string * World Subsystem) list
        default this.MakeSubsystems () = []
    
        /// Make user-defined game dispatcher such that Nu can utilize them at run-time.
        abstract MakeGameDispatchers : unit -> GameDispatcher list
        default this.MakeGameDispatchers () = []
    
        /// Make user-defined screen dispatchers such that Nu can utilize them at run-time.
        abstract MakeScreenDispatchers : unit -> ScreenDispatcher list
        default this.MakeScreenDispatchers () = []
    
        /// Make user-defined layer dispatchers such that Nu can utilize them at run-time.
        abstract MakeLayerDispatchers : unit -> LayerDispatcher list
        default this.MakeLayerDispatchers () = []
    
        /// Make user-defined entity dispatchers such that Nu can utilize them at run-time.
        abstract MakeEntityDispatchers : unit -> EntityDispatcher list
        default this.MakeEntityDispatchers () = []
    
        /// Make user-defined assets such that Nu can utilize them at run-time.
        abstract MakeFacets : unit -> Facet list
        default this.MakeFacets () = []
    
        /// The name of the game dispatcher that Nu will utilize when running inside the editor.
        abstract GetEditorGameDispatcherName : unit -> string
        default this.GetEditorGameDispatcherName () = typeof<GameDispatcher>.Name
    
        /// The name of the game dispatcher that Nu will utilize when running outside the editor.
        abstract GetStandAloneGameDispatcherName : unit -> string
        default this.GetStandAloneGameDispatcherName () = typeof<GameDispatcher>.Name
    
        /// The name of the screen dispatcher that Nu will utilize when running inside the editor.
        abstract GetEditorScreenDispatcherName : unit -> string
        default this.GetEditorScreenDispatcherName () = typeof<ScreenDispatcher>.Name
    
        /// The name of the screen dispatcher that Nu will utilize when running outside the editor.
        abstract GetStandAloneScreenDispatcherName : unit -> string
        default this.GetStandAloneScreenDispatcherName () = typeof<ScreenDispatcher>.Name
    
        /// Make the overlay routes that will allow Nu to use different overlays for the specified
        /// dispatcher name.
        abstract MakeOverlayRoutes : unit -> (string * string option) list
        default this.MakeOverlayRoutes () = []

/// Represents an unsubscription operation for an event.
type Unsubscription = WorldTypes.Unsubscription

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

/// A simulant in the world.
type Simulant = WorldTypes.Simulant

/// Operators for the Simulant type.
type SimulantOperators = WorldTypes.SimulantOperators

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