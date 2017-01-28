// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

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
      DissolveImageOpt : AssetTag option }

    /// Make a screen transition.
    static member make transitionType =
        { TransitionType = transitionType
          TransitionLifetime = 0L
          DissolveImageOpt = None }

/// Describes the behavior of the screen dissolving algorithm.
type [<StructuralEquality; NoComparison>] DissolveData =
    { IncomingTime : int64
      OutgoingTime : int64
      DissolveImage : AssetTag }

/// Describes the behavior of the screen splash algorithm.
type [<StructuralEquality; NoComparison>] SplashData =
    { DissolveData : DissolveData
      IdlingTime : int64
      SplashImage : AssetTag }

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

[<AutoOpen>]
module WorldTypes =

    /// The data for a change in the world's ambient state.
    type [<StructuralEquality; NoComparison>] AmbientChangeData = 
        { OldWorldWithOldState : World }

    /// Describes the information needed to sort simulants.
    /// OPTIMIZATION: implemented as a struct and carries related entity to avoid GC pressure.
    /// TODO: see if there's a better file to place this in.
    and [<CustomEquality; CustomComparison>] SortPriority =
        { SortDepth : single
          SortPositionY : single
          SortTarget : Simulant }

        static member equals left right =
            left.SortDepth = right.SortDepth &&
            left.SortPositionY = right.SortPositionY &&
            left.SortTarget = right.SortTarget

        static member compare left right =
            if left.SortDepth < right.SortDepth then 1
            elif left.SortDepth > right.SortDepth then -1
            elif left.SortPositionY < right.SortPositionY then -1
            elif left.SortPositionY > right.SortPositionY then 1
            else 0

        override this.GetHashCode () =
            this.SortDepth.GetHashCode () ^^^ (this.SortPositionY.GetHashCode () * 13)

        override this.Equals that =
            match that with
            | :? SortPriority as that -> SortPriority.equals this that
            | _ -> failwithumf ()

        interface IComparable<SortPriority> with
            member this.CompareTo that =
                SortPriority.compare this that

        interface IComparable with
            member this.CompareTo that =
                match that with
                | :? SortPriority as that -> (this :> IComparable<SortPriority>).CompareTo that
                | _ -> failwithumf ()

    /// Signifies at run-time that a type has imperative semantics.
    and Imperative = interface end

    /// Generalized interface tag for dispatchers.
    and Dispatcher = interface end

    /// Generalized interface tag for simulant dispatchers.
    and SimulantDispatcher () =
        interface Dispatcher

        /// Determine that this dispatcher has imperative semantics.
        member this.GetImperative () = this :> obj :? Imperative
    
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
    
    /// The default dispatcher for entities.
    and EntityDispatcher () =
        inherit SimulantDispatcher ()
    
        static member PropertyDefinitions =
            [Define? Specialization Constants.Engine.VanillaSpecialization
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
             Define? PublishChanges true
             Define? PublishUpdatesNp false
             Define? PublishPostUpdatesNp false]
    
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
    
        /// Get the quick size of an entity (the appropriate user-defined size for an entity).
        abstract GetQuickSize : Entity * World -> Vector2
        default dispatcher.GetQuickSize (_, _) = Constants.Engine.DefaultEntitySize

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
    
        /// Participate in getting the priority with which an entity is picked in the editor.
        abstract GetQuickSize : Entity * World -> Vector2
        default facet.GetQuickSize (_, _) = Constants.Engine.DefaultEntitySize

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
        static member acatf<'a> (address : 'a Address) (simulant : Simulant) = acatf address (atooa simulant.SimulantAddress)

        /// Concatenate two addresses, takings the type of first address.
        static member (->-) (address, simulant : Simulant) = SimulantOperators.acatf address simulant

    /// Generalized interface for simulant state.
    and SimulantState =
        interface
            abstract member GetXtension : unit -> Xtension
            end

    /// Hosts the ongoing state of a game. The end-user of this engine should never touch this
    /// type, and it's public _only_ to make [<CLIMutable>] work.
    and [<CLIMutable; NoEquality; NoComparison>] GameState =
        { Id : Guid
          Xtension : Xtension
          DispatcherNp : GameDispatcher
          Specialization : string
          CreationTimeStampNp : int64
          ScriptOpt : AssetTag option
          Script : Scripting.Expr list
          ScriptFrameNp : Scripting.DeclarationFrame
          ScriptProperties : UMap<string, Scripting.Expr>
          OnRegister : Scripting.Expr
          OnUnregister : Scripting.Expr
          OnUpdate : Scripting.Expr
          OnPostUpdate : Scripting.Expr
          OnActualize : Scripting.Expr
          SelectedScreenOpt : Screen option
          ScreenTransitionDestinationOpt : Screen option
          EyeCenter : Vector2
          EyeSize : Vector2 }

        /// Try to get a dynamic property and its type information.
        static member tryGetProperty propertyName gameState =
            match Xtension.tryGetProperty propertyName gameState.Xtension with
            | Some xProperty -> Some (xProperty.PropertyValue, xProperty.PropertyType)
            | None -> None

        /// Get a dynamic property and its type information.
        static member getProperty propertyName gameState =
            let xProperty = Xtension.getProperty propertyName gameState.Xtension
            (xProperty.PropertyValue, xProperty.PropertyType)

        /// Try to set a dynamic property with explicit type information.
        static member trySetProperty propertyName property gameState =
            let xProperty = { PropertyValue = fst property; PropertyType = snd property }
            match Xtension.trySetProperty propertyName xProperty gameState.Xtension with
            | (true, xtension) -> (true, { gameState with Xtension = xtension })
            | (false, _) -> (false, gameState)

        /// Set a dynamic property with explicit type information.
        static member setProperty propertyName property gameState =
            let xProperty = { PropertyValue = fst property; PropertyType = snd property }
            { gameState with GameState.Xtension = Xtension.setProperty propertyName xProperty gameState.Xtension }

        /// The dynamic look-up operator.
        static member get propertyName gameState : 'a =
            Xtension.(?) (gameState.Xtension, propertyName)

        /// The dynamic assignment operator.
        static member set propertyName (value : 'a) gameState =
            { gameState with GameState.Xtension = Xtension.(?<-) (gameState.Xtension, propertyName, value) }

        /// Attach a dynamic property.
        static member attachProperty name value gameState =
            { gameState with GameState.Xtension = Xtension.attachProperty name { PropertyValue = value; PropertyType = getType value } gameState.Xtension }
    
        /// Make a game state value.
        static member make specializationOpt (dispatcher : GameDispatcher) =
            let eyeCenter = Vector2.Zero
            let eyeSize = Vector2 (single Constants.Render.ResolutionXDefault, single Constants.Render.ResolutionYDefault)
            { Id = makeGuid ()
              Xtension = if dispatcher.GetImperative () then Xtension.makeImperative () else Xtension.safe
              DispatcherNp = dispatcher
              Specialization = Option.getOrDefault Constants.Engine.VanillaSpecialization specializationOpt
              CreationTimeStampNp = Core.getTimeStamp ()
              ScriptOpt = None
              Script = []
              ScriptFrameNp = Scripting.DeclarationFrame HashIdentity.Structural
              ScriptProperties = UMap.makeEmpty None
              OnRegister = Scripting.Unit
              OnUnregister = Scripting.Unit
              OnUpdate = Scripting.Unit
              OnPostUpdate = Scripting.Unit
              OnActualize = Scripting.Unit
              SelectedScreenOpt = None
              ScreenTransitionDestinationOpt = None
              EyeCenter = eyeCenter
              EyeSize = eyeSize }

        /// Copy a game such as when, say, you need it to be mutated with reflection but you need to preserve persistence.
        static member copy this =
            { this with GameState.Id = this.Id }

        interface SimulantState with
            member this.GetXtension () = this.Xtension
    
    /// Hosts the ongoing state of a screen. The end-user of this engine should never touch this
    /// type, and it's public _only_ to make [<CLIMutable>] work.
    and [<CLIMutable; NoEquality; NoComparison>] ScreenState =
        { Id : Guid
          Name : Name
          Xtension : Xtension
          DispatcherNp : ScreenDispatcher
          Specialization : string
          Persistent : bool
          CreationTimeStampNp : int64
          ScriptOpt : AssetTag option
          Script : Scripting.Expr list
          ScriptFrameNp : Scripting.DeclarationFrame
          OnRegister : Scripting.Expr
          OnUnregister : Scripting.Expr
          OnUpdate : Scripting.Expr
          OnPostUpdate : Scripting.Expr
          OnActualize : Scripting.Expr
          EntityTreeNp : Entity SpatialTree MutantCache
          TransitionStateNp : TransitionState
          TransitionTicksNp : int64
          Incoming : Transition
          Outgoing : Transition }

        /// Get a dynamic property and its type information.
        static member getProperty propertyName screenState =
            let xProperty = Xtension.getProperty propertyName screenState.Xtension
            (xProperty.PropertyValue, xProperty.PropertyType)

        /// Try to get a dynamic property and its type information.
        static member tryGetProperty propertyName screenState =
            match Xtension.tryGetProperty propertyName screenState.Xtension with
            | Some xProperty -> Some (xProperty.PropertyValue, xProperty.PropertyType)
            | None -> None

        /// Try to set a dynamic property with explicit type information.
        static member trySetProperty propertyName property screenState =
            let xProperty = { PropertyValue = fst property; PropertyType = snd property }
            match Xtension.trySetProperty propertyName xProperty screenState.Xtension with
            | (true, xtension) -> (true, { screenState with Xtension = xtension })
            | (false, _) -> (false, screenState)

        /// Set a dynamic property with explicit type information.
        static member setProperty propertyName property screenState =
            let xProperty = { PropertyValue = fst property; PropertyType = snd property }
            { screenState with ScreenState.Xtension = Xtension.setProperty propertyName xProperty screenState.Xtension }

        /// The dynamic look-up operator.
        static member get propertyName screenState : 'a =
            Xtension.(?) (screenState.Xtension, propertyName)

        /// The dynamic assignment operator.
        static member set propertyName (value : 'a) screenState =
            { screenState with ScreenState.Xtension = Xtension.(?<-) (screenState.Xtension, propertyName, value) }

        /// Attach a dynamic property.
        static member attachProperty name value screenState =
            { screenState with ScreenState.Xtension = Xtension.attachProperty name { PropertyValue = value; PropertyType = getType value } screenState.Xtension }
    
        /// Make a screen state value.
        static member make specializationOpt nameOpt (dispatcher : ScreenDispatcher) =
            let (id, name) = Reflection.deriveIdAndName nameOpt
            let screenState =
                { Id = id
                  Name = name
                  Xtension = if dispatcher.GetImperative () then Xtension.makeImperative () else Xtension.safe
                  DispatcherNp = dispatcher
                  Specialization = Option.getOrDefault Constants.Engine.VanillaSpecialization specializationOpt
                  Persistent = true
                  CreationTimeStampNp = Core.getTimeStamp ()
                  ScriptOpt = None
                  Script = []
                  ScriptFrameNp = Scripting.DeclarationFrame HashIdentity.Structural
                  OnRegister = Scripting.Unit
                  OnUnregister = Scripting.Unit
                  OnUpdate = Scripting.Unit
                  OnPostUpdate = Scripting.Unit
                  OnActualize = Scripting.Unit
                  EntityTreeNp = Unchecked.defaultof<Entity SpatialTree MutantCache>
                  TransitionStateNp = IdlingState
                  TransitionTicksNp = 0L // TODO: roll this field into Incoming/OutcomingState values
                  Incoming = Transition.make Incoming
                  Outgoing = Transition.make Outgoing }
            let spatialTree = SpatialTree.make Constants.Engine.EntityTreeGranularity Constants.Engine.EntityTreeDepth Constants.Engine.EntityTreeBounds
            { screenState with EntityTreeNp = MutantCache.make Operators.id spatialTree }

        /// Copy a screen such as when, say, you need it to be mutated with reflection but you need to preserve persistence.
        static member copy this =
            { this with ScreenState.Id = this.Id }

        interface SimulantState with
            member this.GetXtension () = this.Xtension
    
    /// Hosts the ongoing state of a layer. The end-user of this engine should never touch this
    /// type, and it's public _only_ to make [<CLIMutable>] work.
    and [<CLIMutable; NoEquality; NoComparison>] LayerState =
        { Id : Guid
          Name : Name
          Xtension : Xtension
          DispatcherNp : LayerDispatcher
          Specialization : string
          Persistent : bool
          CreationTimeStampNp : int64
          ScriptFrameNp : Scripting.DeclarationFrame
          ScriptOpt : AssetTag option
          Script : Scripting.Expr list
          OnRegister : Scripting.Expr
          OnUnregister : Scripting.Expr
          OnUpdate : Scripting.Expr
          OnPostUpdate : Scripting.Expr
          OnActualize : Scripting.Expr
          Depth : single
          Visible : bool }

        /// Try to get a dynamic property and its type information.
        static member tryGetProperty propertyName layerState =
            match Xtension.tryGetProperty propertyName layerState.Xtension with
            | Some xProperty -> Some (xProperty.PropertyValue, xProperty.PropertyType)
            | None -> None

        /// Get a dynamic property and its type information.
        static member getProperty propertyName layerState =
            let xProperty = Xtension.getProperty propertyName layerState.Xtension
            (xProperty.PropertyValue, xProperty.PropertyType)

        /// Try to set a dynamic property with explicit type information.
        static member trySetProperty propertyName property layerState =
            let xProperty = { PropertyValue = fst property; PropertyType = snd property }
            match Xtension.trySetProperty propertyName xProperty layerState.Xtension with
            | (true, xtension) -> (true, { layerState with Xtension = xtension })
            | (false, _) -> (false, layerState)

        /// Set a dynamic property with explicit type information.
        static member setProperty propertyName property layerState =
            let xProperty = { PropertyValue = fst property; PropertyType = snd property }
            { layerState with LayerState.Xtension = Xtension.setProperty propertyName xProperty layerState.Xtension }

        /// The dynamic look-up operator.
        static member get propertyName layerState : 'a =
            Xtension.(?) (layerState.Xtension, propertyName)

        /// The dynamic assignment operator.
        static member set propertyName (value : 'a) layerState =
            { layerState with LayerState.Xtension = Xtension.(?<-) (layerState.Xtension, propertyName, value) }

        /// Attach a dynamic property.
        static member attachProperty name value layerState =
            { layerState with LayerState.Xtension = Xtension.attachProperty name { PropertyValue = value; PropertyType = getType value } layerState.Xtension }
    
        /// Make a layer state value.
        static member make specializationOpt nameOpt (dispatcher : LayerDispatcher) =
            let (id, name) = Reflection.deriveIdAndName nameOpt
            { LayerState.Id = id
              Name = name
              Xtension = if dispatcher.GetImperative () then Xtension.makeImperative () else Xtension.safe
              DispatcherNp = dispatcher
              Specialization = Option.getOrDefault Constants.Engine.VanillaSpecialization specializationOpt
              Persistent = true
              CreationTimeStampNp = Core.getTimeStamp ()
              ScriptOpt = None
              Script = []
              ScriptFrameNp = Scripting.DeclarationFrame HashIdentity.Structural
              OnRegister = Scripting.Unit
              OnUnregister = Scripting.Unit
              OnUpdate = Scripting.Unit
              OnPostUpdate = Scripting.Unit
              OnActualize = Scripting.Unit
              Depth = 0.0f
              Visible = true }

        /// Copy a layer such as when, say, you need it to be mutated with reflection but you need to preserve persistence.
        static member copy this =
            { this with LayerState.Id = this.Id }

        interface SimulantState with
            member this.GetXtension () = this.Xtension

    /// Hosts the ongoing state of an entity. The end-user of this engine should never touch this
    /// type, and it's public _only_ to make [<CLIMutable>] work.
    and [<CLIMutable; NoEquality; NoComparison>] EntityState =
        { Id : Guid
          Name : Name
          mutable Xtension : Xtension
          DispatcherNp : EntityDispatcher
          Specialization : string
          mutable Persistent : bool
          CreationTimeStampNp : int64 // just needed for ordering writes to reduce diff volumes
          CachableNp : bool
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
          mutable PublishChanges : bool
          mutable PublishUpdatesNp : bool
          mutable PublishPostUpdatesNp : bool
          mutable FacetNames : string Set
          mutable FacetsNp : Facet list }

        /// Try to get a dynamic property and its type information.
        static member tryGetProperty propertyName entityState =
            match Xtension.tryGetProperty propertyName entityState.Xtension with
            | Some xProperty -> Some (xProperty.PropertyValue, xProperty.PropertyType)
            | None -> None

        /// Get a dynamic property and its type information.
        static member getProperty propertyName entityState =
            let xProperty = Xtension.getProperty propertyName entityState.Xtension
            (xProperty.PropertyValue, xProperty.PropertyType)

        /// Try to set a dynamic property with explicit type information.
        static member trySetProperty propertyName property entityState =
            let xProperty = { PropertyValue = fst property; PropertyType = snd property }
            match Xtension.trySetProperty propertyName xProperty entityState.Xtension with
            | (true, xtension) -> (true, { entityState with Xtension = xtension })
            | (false, _) -> (false, entityState)

        /// Set a dynamic property with explicit type information.
        static member setProperty propertyName property entityState =
            let xProperty = { PropertyValue = fst property; PropertyType = snd property }
            let xtension = Xtension.setProperty propertyName xProperty entityState.Xtension
            if Xtension.getImperative entityState.Xtension then entityState
            else { entityState with EntityState.Xtension = xtension }

        /// The dynamic look-up operator.
        static member get propertyName entityState : 'a =
            Xtension.(?) (entityState.Xtension, propertyName)

        /// The dynamic assignment operator.
        static member set propertyName (value : 'a) entityState =
            let xtension = Xtension.(?<-) (entityState.Xtension, propertyName, value)
            if Xtension.getImperative entityState.Xtension then entityState
            else { entityState with EntityState.Xtension = xtension }

        /// Attach a dynamic property.
        static member attachProperty name value entityState =
            let xtension = Xtension.attachProperty name value entityState.Xtension
            if Xtension.getImperative entityState.Xtension then entityState.Xtension <- xtension; entityState
            else { entityState with EntityState.Xtension = xtension }

        /// Detach a dynamic property.
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

        /// Make an entity state value.
        static member make specializationOpt nameOpt overlayNameOpt (dispatcher : EntityDispatcher) =
            let (id, name) = Reflection.deriveIdAndName nameOpt
            { Id = id
              Name = name
              Xtension = if dispatcher.GetImperative () then Xtension.makeImperative () else Xtension.safe
              DispatcherNp = dispatcher
              Specialization = Option.getOrDefault Constants.Engine.VanillaSpecialization specializationOpt
              Persistent = true
              CreationTimeStampNp = Core.getTimeStamp ()
              CachableNp = Name.endsWithGuid name
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
              PublishChanges = true
              PublishUpdatesNp = false
              PublishPostUpdatesNp = false
              FacetNames = Set.empty
              FacetsNp = [] }

        /// Copy an entity such as when, say, you need it to be mutated with reflection but you need to preserve persistence.
        static member copy this =
            if Xtension.getImperative this.Xtension then this
            else { this with EntityState.Id = this.Id }

        interface SimulantState with
            member this.GetXtension () = this.Xtension

    /// Describes a game value independent of the engine.
    and [<NoComparison>] GameDescriptor =
        { GameDispatcher : string
          GameProperties : Map<string, Symbol>
          Screens : ScreenDescriptor list }

        /// The empty game descriptor.
        static member empty =
            { GameDispatcher = String.Empty
              GameProperties = Map.empty
              Screens = [] }

    /// Describes a screen value independent of the engine.
    and [<NoComparison>] ScreenDescriptor =
        { ScreenDispatcher : string
          ScreenProperties : Map<string, Symbol>
          Layers : LayerDescriptor list }

        /// The empty screen descriptor.
        static member empty =
            { ScreenDispatcher = String.Empty
              ScreenProperties = Map.empty
              Layers = [] }

    /// Describes a layer value independent of the engine.
    and [<NoComparison>] LayerDescriptor =
        { LayerDispatcher : string
          LayerProperties : Map<string, Symbol>
          Entities : EntityDescriptor list }

        /// The empty layer descriptor.
        static member empty =
            { LayerDispatcher = String.Empty
              LayerProperties = Map.empty
              Entities = [] }

    /// Describes an entity value independent of the engine.
    and [<NoComparison>] EntityDescriptor =
        { EntityDispatcher : string
          EntityProperties : Map<string, Symbol> }

        /// The empty entity descriptor.
        static member empty =
            { EntityDispatcher = String.Empty
              EntityProperties = Map.empty }

    /// The null simulant.
    and private NullSimulant () =
        interface Simulant with
            member this.ParticipantAddress = Address.empty
            member this.SimulantAddress = Address.empty
            end
    
    /// The game type that hosts the various screens used to navigate through a game.
    and [<CustomEquality; NoComparison>] Game =
        { GameAddress : Game Address }
    
        interface Simulant with
            member this.ParticipantAddress = atoa<Game, Participant> this.GameAddress
            member this.SimulantAddress = atoa<Game, Simulant> this.GameAddress
            end

        override this.Equals that =
            match that with
            | :? Game as that -> this.GameAddress.Equals that.GameAddress
            | _ -> failwithumf ()

        override this.GetHashCode () = this.GameAddress.GetHashCode ()

        override this.ToString () = scstring this.GameAddress

        /// Get the latest value of a game's properties.
        [<DebuggerBrowsable (DebuggerBrowsableState.RootHidden)>]
        member private this.View = Debug.World.viewGame Debug.World.Chosen

        /// Create a Game proxy from an address.
        static member proxy address =
#if DEBUG
            if Address.notEmpty address then failwith "Game address must not have any names."
#endif
            { GameAddress = address }

        /// Concatenate two addresses, taking the type of first address.
        static member acatf<'a> (address : 'a Address) (game : Game) = acatf address (atooa game.GameAddress)
        
        /// Concatenate two addresses, forcing the type of first address.
        static member acatff<'a> (address : 'a Address) (game : Game) = acatff address game.GameAddress

        /// Concatenate two addresses, taking the type of first address.
        static member (->-) (address, game) = Game.acatf address game

        /// Concatenate two addresses, forcing the type of first address.
        static member (->>-) (address, game) = acatff address game

    /// The screen type that allows transitioning to and from other screens, and also hosts the
    /// currently interactive layers of entities.
    and [<CustomEquality; NoComparison>] Screen =
        { ScreenAddress : Screen Address }
    
        interface Simulant with
            member this.ParticipantAddress = atoa<Screen, Participant> this.ScreenAddress
            member this.SimulantAddress = atoa<Screen, Simulant> this.ScreenAddress
            end

        override this.Equals that =
            match that with
            | :? Screen as that -> this.ScreenAddress.Equals that.ScreenAddress
            | _ -> failwithumf ()

        override this.GetHashCode () = this.ScreenAddress.GetHashCode ()
    
        override this.ToString () = scstring this.ScreenAddress
    
        /// Get the name of a screen proxy.
        member this.ScreenName = Address.getName this.ScreenAddress
    
        /// Get the latest value of a screen's properties.
        [<DebuggerBrowsable (DebuggerBrowsableState.RootHidden)>]
        member private this.View = Debug.World.viewScreen (this :> obj) Debug.World.Chosen
    
        /// Create a Screen proxy from an address.
        static member proxy address =
#if DEBUG
            if Address.length address <> 1 then failwith "Screen address must have 1 name."
#endif
            { ScreenAddress = address }
    
        /// Concatenate two addresses, taking the type of first address.
        static member acatf<'a> (address : 'a Address) (screen : Screen) = acatf address (atooa screen.ScreenAddress)
        
        /// Concatenate two addresses, forcing the type of first address.
        static member acatff<'a> (address : 'a Address) (screen : Screen) = acatff address screen.ScreenAddress
    
        /// Concatenate two addresses, taking the type of first address.
        static member (->-) (address, screen) = Screen.acatf address screen
    
        /// Concatenate two addresses, forcing the type of first address.
        static member (->>-) (address, screen) = acatff address screen
    
        /// Convert a name string to a screen's proxy.
        static member (!>) screenNameStr = Screen.proxy ^ ntoa !!screenNameStr
    
        /// Convert a screen's proxy to a layer's by appending the layer's name at the end.
        static member (=>) (screen, layerName) = Layer.proxy ^ atoa<Screen, Layer> screen.ScreenAddress ->- ntoa layerName
    
        /// Convert a screen's proxy to a layer's by appending the layer's name at the end.
        static member (=>) (screen : Screen, layerNameStr) = screen => !!layerNameStr
    
    /// Forms a logical layer of entities.
    and [<CustomEquality; NoComparison>] Layer =
        { LayerAddress : Layer Address }
    
        interface Simulant with
            member this.ParticipantAddress = atoa<Layer, Participant> this.LayerAddress
            member this.SimulantAddress = atoa<Layer, Simulant> this.LayerAddress
            end

        override this.Equals that =
            match that with
            | :? Layer as that -> this.LayerAddress.Equals that.LayerAddress
            | _ -> failwithumf ()

        override this.GetHashCode () = this.LayerAddress.GetHashCode ()
    
        override this.ToString () = scstring this.LayerAddress
    
        /// Get the name of a layer proxy.
        member this.LayerName = Address.getName this.LayerAddress
    
        /// Get the latest value of a layer's properties.
        [<DebuggerBrowsable (DebuggerBrowsableState.RootHidden)>]
        member private this.View = Debug.World.viewLayer (this :> obj) Debug.World.Chosen
    
        /// Create a Layer proxy from an address.
        static member proxy address =
#if DEBUG
            if Address.length address <> 2 then failwith "Layer address must have 2 names."
#endif
            { LayerAddress = address }
    
        /// Concatenate two addresses, taking the type of first address.
        static member acatf<'a> (address : 'a Address) (layer : Layer) = acatf address (atooa layer.LayerAddress)
        
        /// Concatenate two addresses, forcing the type of first address.
        static member acatff<'a> (address : 'a Address) (layer : Layer) = acatff address layer.LayerAddress
    
        /// Convert a layer's proxy to its screen's.
        static member (!<) layer = Screen.proxy ^ Address.allButLast layer.LayerAddress
    
        /// Convert a layer's proxy to an entity's by appending the entity's name at the end.
        static member (=>) (layer, entityName) = Entity.proxy ^ atoa<Layer, Entity> layer.LayerAddress ->- ntoa entityName
    
        /// Convert a layer's proxy to an entity's by appending the entity's name at the end.
        static member (=>) (layer : Layer, entityNameStr) = layer => !!entityNameStr
    
        /// Concatenate two addresses, taking the type of first address.
        static member (->-) (address, layer) = Layer.acatf address layer
    
        /// Concatenate two addresses, forcing the type of first address.
        static member (->>-) (address, layer) = acatff address layer
    
    /// The type around which the whole game engine is based! Used in combination with dispatchers
    /// to implement things like buttons, characters, blocks, and things of that sort.
    /// OPTIMIZATION: Includes pre-constructed entity change and update event addresses to avoid
    /// reconstructing new ones for each entity every frame.
    and [<CustomEquality; NoComparison>] Entity =
        { mutable EntityStateOpt : EntityState
          EntityAddress : Entity Address
          UpdateAddress : unit Address
          PostUpdateAddress : unit Address }
    
        interface Simulant with
            member this.ParticipantAddress = atoa<Entity, Participant> this.EntityAddress
            member this.SimulantAddress = atoa<Entity, Simulant> this.EntityAddress
            end

        override this.Equals that =
            match that with
            | :? Entity as that -> this.EntityAddress.Equals that.EntityAddress
            | _ -> failwithumf ()

        override this.GetHashCode () = this.EntityAddress.GetHashCode ()
    
        override this.ToString () = scstring this.EntityAddress
    
        /// Get the name of an entity proxy.
        member this.EntityName = Address.getName this.EntityAddress
    
        /// Get the latest value of an entity's properties.
        [<DebuggerBrowsable (DebuggerBrowsableState.RootHidden)>]
        member private this.View = Debug.World.viewEntity (this :> obj) Debug.World.Chosen
    
        /// Create an Entity proxy from an address.
        static member proxy address =
#if DEBUG
            if Address.length address <> 3 then failwith "Entity address must have 2 names."
#endif
            { EntityStateOpt = Unchecked.defaultof<EntityState>
              EntityAddress = address
              UpdateAddress = ltoa [!!"Update"; !!"Event"] ->>- address
              PostUpdateAddress = ltoa [!!"PostUpdate"; !!"Event"] ->>- address }
    
        /// Concatenate two addresses, taking the type of first address.
        static member acatf<'a> (address : 'a Address) (entity : Entity) = acatf address (atooa entity.EntityAddress)
        
        /// Concatenate two addresses, forcing the type of first address.
        static member acatff<'a> (address : 'a Address) (entity : Entity) = acatff address entity.EntityAddress
    
        /// Convert an entity's proxy to its layer's.
        static member (!<) entity = Layer.proxy ^ Address.allButLast entity.EntityAddress
    
        /// Concatenate two addresses, taking the type of first address.
        static member (->-) (address, entity) = Entity.acatf address entity
    
        /// Concatenate two addresses, forcing the type of first address.
        static member (->>-) (address, entity) = acatff address entity
    
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
          UpdateEntityInEntityTree : Entity -> World -> World -> World
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
            { EventSystem : World EventSystem
              Dispatchers : Dispatchers
              Subsystems : World Subsystems
              ScriptEnv : Scripting.Env
              ScriptContext : Simulant
              ScreenCachedOpt : KeyedCache<Screen Address * UMap<Screen Address, ScreenState>, ScreenState option>
              LayerCachedOpt : KeyedCache<Layer Address * UMap<Layer Address, LayerState>, LayerState option>
              EntityCachedOpt : KeyedCache<Entity Address * UMap<Entity Address, EntityState>, EntityState option>
              ScreenDirectory : UMap<Name, Screen Address * UMap<Name, Layer Address * UMap<Name, Entity Address>>>
              AmbientState : World AmbientState
              GameState : GameState
              ScreenStates : UMap<Screen Address, ScreenState>
              LayerStates : UMap<Layer Address, LayerState>
              EntityStates : UMap<Entity Address, EntityState> }

        interface EventWorld<Game, World> with
            member this.GetLiveness () = AmbientState.getLiveness this.AmbientState
            member this.GetEventSystem () = this.EventSystem
            member this.UpdateEventSystem updater = { this with EventSystem = updater this.EventSystem }
            member this.ContainsParticipant participant =
                match participant with
                | :? Entity as entity -> UMap.containsKey entity.EntityAddress this.EntityStates
                | :? Layer as layer -> UMap.containsKey layer.LayerAddress this.LayerStates
                | :? Screen as screen -> UMap.containsKey screen.ScreenAddress this.ScreenStates
                | :? Game -> true
                | _  -> false
            member this.PublishEvent (participant : Participant) publisher eventData eventAddress eventTrace subscription world =
                match participant with
                | :? Entity -> EventWorld.publishEvent<'a, 'p, Entity, Game, World> participant publisher eventData eventAddress eventTrace subscription world
                | :? Layer -> EventWorld.publishEvent<'a, 'p, Layer, Game, World> participant publisher eventData eventAddress eventTrace subscription world
                | :? Screen -> EventWorld.publishEvent<'a, 'p, Screen, Game, World> participant publisher eventData eventAddress eventTrace subscription world
                | :? Game -> EventWorld.publishEvent<'a, 'p, Game, Game, World> participant publisher eventData eventAddress eventTrace subscription world
                | _ -> failwithumf ()

    /// Provides a way to make user-defined dispatchers, facets, and various other sorts of game-
    /// specific values.
    type NuPlugin () =

        /// Make user-defined subsystems such that Nu can utilitze them at run-time.
        abstract MakeSubsystems : unit -> (string * World Subsystem) list
        default this.MakeSubsystems () = []
    
        /// Optionally make a user-defined game dispatchers such that Nu can utilize it at run-time.
        abstract MakeGameDispatcherOpt : unit -> GameDispatcher option
        default this.MakeGameDispatcherOpt () = None
    
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
    
        /// Make the overlay routes that will allow Nu to use different overlays for the specified
        /// classifications.
        abstract MakeOverlayRoutes : unit -> (string * OverlayDescriptor) list
        default this.MakeOverlayRoutes () = []

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

/// Describes a game value independent of the engine.
type GameDescriptor = WorldTypes.GameDescriptor

/// Describes a screen value independent of the engine.
type ScreenDescriptor = WorldTypes.ScreenDescriptor

/// Describes a layer value independent of the engine.
type LayerDescriptor = WorldTypes.LayerDescriptor

/// Describes an entity value independent of the engine.
type EntityDescriptor = WorldTypes.EntityDescriptor

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