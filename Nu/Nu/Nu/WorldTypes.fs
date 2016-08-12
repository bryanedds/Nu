// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Debug
open System
module internal World =

    /// The value of the world currently chosen for debugging in an IDE. Not to be used for anything else.
    let mutable internal Chosen = obj ()
    let mutable internal viewGame = fun (_ : obj) -> Array.create 0 (String.Empty, obj ())
    let mutable internal viewScreen = fun (_ : obj) (_ : obj) -> Array.create 0 (String.Empty, obj ())
    let mutable internal viewGroup = fun (_ : obj) (_ : obj) -> Array.create 0 (String.Empty, obj ())
    let mutable internal viewEntity = fun (_ : obj) (_ : obj) -> Array.create 0 (String.Empty, obj ())

namespace Nu
open System
open System.Diagnostics
open FSharpx.Collections
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
      OptDissolveImage : AssetTag option }

    /// Make a screen transition.
    static member make transitionType =
        { TransitionType = transitionType
          TransitionLifetime = 0L
          OptDissolveImage = None }

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
      OptTileSetTile : TmxTilesetTile option
      TilePosition : Vector2i }

[<AutoOpen>]
module WorldTypes =

    /// A simulant in the world.
    type Simulant =
        interface
            inherit Participant
            abstract member SimulantAddress : Simulant Address
            end

    /// Operators for the Simulant type.
    type SimulantOperators =
        private
            | SimulantOperators

        /// Concatenate two addresses, forcing the type of first address.
        static member acatf<'a> (address : 'a Address) (simulant : Simulant) = acatf address (atooa simulant.SimulantAddress)

        /// Concatenate two addresses, takings the type of first address.
        static member (->-) (address, simulant : Simulant) = SimulantOperators.acatf address simulant

    /// The data for a change in the world's ambient state.
    type [<StructuralEquality; NoComparison>] AmbientChangeData = 
        { OldWorldWithOldState : World }
    
    /// The default dispatcher for games.
    and GameDispatcher () =
    
        static member PropertyDefinitions =
            [Define? Specialization Constants.Engine.VanillaSpecialization]
    
        /// Register a game when adding it to the world. Note that there is no corresponding
        /// Unregister method due to the inability to remove a game from the world.
        abstract Register : Game * World -> World
        default dispatcher.Register (_, world) = world
    
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
    
        static member PropertyDefinitions =
            [Define? Specialization Constants.Engine.VanillaSpecialization
             Define? Persistent true]
    
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
    
    /// The default dispatcher for groups.
    and GroupDispatcher () =
    
        static member PropertyDefinitions =
            [Define? Specialization Constants.Engine.VanillaSpecialization
             Define? Persistent true]
    
        /// Register a group when adding it to a screen.
        abstract Register : Group * World -> World
        default dispatcher.Register (_, world) = world
    
        /// Unregister a group when removing it from a screen.
        abstract Unregister : Group * World -> World
        default dispatcher.Unregister (_, world) = world
    
        /// Update a group.
        abstract Update : Group * World -> World
        default dispatcher.Update (_, world) = world
    
        /// Post-update a group.
        abstract PostUpdate : Group * World -> World
        default dispatcher.PostUpdate (_, world) = world
    
        /// Actualize a group.
        abstract Actualize : Group * World -> World
        default dispatcher.Actualize (_, world) = world
    
    /// The default dispatcher for entities.
    and EntityDispatcher () =
    
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
             Define? Omnipresent false
             Define? PublishChanges true
             Define? PublishUpdatesNp false
             Define? PublishPostUpdatesNp false]
    
        /// Register an entity when adding it to a group.
        abstract Register : Entity * World -> World
        default dispatcher.Register (_, world) = world
    
        /// Unregister an entity when removing it from a group.
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
    
        /// Get the quick size of an entity (the appropriate user-define size for an entity).
        abstract GetQuickSize : Entity * World -> Vector2
        default dispatcher.GetQuickSize (_, _) = Vector2.One
    
        /// Get the priority with which an entity is picked in the editor.
        abstract GetPickingPriority : Entity * single * World -> single
        default dispatcher.GetPickingPriority (_, depth, _) = depth
    
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

    /// Interface tag for simulant state.
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
          OptScriptAsset : AssetTag option
          OptScriptAssetLc : AssetTag option
          Script : Script
          CreationTimeStampNp : int64
          OptSelectedScreen : Screen option
          OptScreenTransitionDestination : Screen option
          EyeCenter : Vector2
          EyeSize : Vector2 }

        /// Get a dynamic property and its type information.
        static member getProperty propertyName gameState =
            let xProperty = Xtension.getProperty propertyName gameState.Xtension
            (xProperty.PropertyValue, xProperty.PropertyType)

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
        static member make optSpecialization dispatcher =
            let eyeCenter = Vector2.Zero
            let eyeSize = Vector2 (single Constants.Render.ResolutionXDefault, single Constants.Render.ResolutionYDefault)
            { Id = makeGuid ()
              Xtension = Xtension.safe
              DispatcherNp = dispatcher
              Specialization = Option.getOrDefault Constants.Engine.VanillaSpecialization optSpecialization
              OptScriptAsset = None
              OptScriptAssetLc = None
              Script = Script.empty
              CreationTimeStampNp = Core.getTimeStamp ()
              OptSelectedScreen = None
              OptScreenTransitionDestination = None
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
          OptScriptAsset : AssetTag option
          OptScriptAssetLc : AssetTag option
          Script : Script
          CreationTimeStampNp : int64
          EntityTreeNp : Entity QuadTree MutantCache
          TransitionStateNp : TransitionState
          TransitionTicksNp : int64
          Incoming : Transition
          Outgoing : Transition }

        /// Get a dynamic property and its type information.
        static member getProperty propertyName screenState =
            let xProperty = Xtension.getProperty propertyName screenState.Xtension
            (xProperty.PropertyValue, xProperty.PropertyType)

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
        static member make optSpecialization optName dispatcher =
            let (id, name) = Reflection.deriveNameAndId optName
            let screenState =
                { Id = id
                  Name = name
                  Xtension = Xtension.safe
                  DispatcherNp = dispatcher
                  Specialization = Option.getOrDefault Constants.Engine.VanillaSpecialization optSpecialization
                  Persistent = true
                  OptScriptAsset = None
                  OptScriptAssetLc = None
                  Script = Script.empty
                  CreationTimeStampNp = Core.getTimeStamp ()
                  EntityTreeNp = Unchecked.defaultof<Entity QuadTree MutantCache>
                  TransitionStateNp = IdlingState
                  TransitionTicksNp = 0L // TODO: roll this field into Incoming/OutcomingState values
                  Incoming = Transition.make Incoming
                  Outgoing = Transition.make Outgoing }
            let quadTree = QuadTree.make Constants.Engine.EntityTreeDepth Constants.Engine.EntityTreeBounds
            { screenState with EntityTreeNp = MutantCache.make Operators.id quadTree }

        /// Copy a screen such as when, say, you need it to be mutated with reflection but you need to preserve persistence.
        static member copy this =
            { this with ScreenState.Id = this.Id }

        interface SimulantState with
            member this.GetXtension () = this.Xtension
    
    /// Hosts the ongoing state of a group. The end-user of this engine should never touch this
    /// type, and it's public _only_ to make [<CLIMutable>] work.
    and [<CLIMutable; NoEquality; NoComparison>] GroupState =
        { Id : Guid
          Name : Name
          Xtension : Xtension
          DispatcherNp : GroupDispatcher
          Specialization : string
          Persistent : bool
          OptScriptAsset : AssetTag option
          OptScriptAssetLc : AssetTag option
          Script : Script
          CreationTimeStampNp : int64 }

        /// Get a dynamic property and its type information.
        static member getProperty propertyName groupState =
            let xProperty = Xtension.getProperty propertyName groupState.Xtension
            (xProperty.PropertyValue, xProperty.PropertyType)

        /// Set a dynamic property with explicit type information.
        static member setProperty propertyName property groupState =
            let xProperty = { PropertyValue = fst property; PropertyType = snd property }
            { groupState with GroupState.Xtension = Xtension.setProperty propertyName xProperty groupState.Xtension }

        /// The dynamic look-up operator.
        static member get propertyName groupState : 'a =
            Xtension.(?) (groupState.Xtension, propertyName)

        /// The dynamic assignment operator.
        static member set propertyName (value : 'a) groupState =
            { groupState with GroupState.Xtension = Xtension.(?<-) (groupState.Xtension, propertyName, value) }

        /// Attach a dynamic property.
        static member attachProperty name value groupState =
            { groupState with GroupState.Xtension = Xtension.attachProperty name { PropertyValue = value; PropertyType = getType value } groupState.Xtension }
    
        /// Make a group state value.
        static member make optSpecialization optName dispatcher =
            let (id, name) = Reflection.deriveNameAndId optName
            { GroupState.Id = id
              Name = name
              Xtension = Xtension.safe
              DispatcherNp = dispatcher
              Specialization = Option.getOrDefault Constants.Engine.VanillaSpecialization optSpecialization
              Persistent = true
              OptScriptAsset = None
              OptScriptAssetLc = None
              Script = Script.empty
              CreationTimeStampNp = Core.getTimeStamp () }

        /// Copy a group such as when, say, you need it to be mutated with reflection but you need to preserve persistence.
        static member copy this =
            { this with GroupState.Id = this.Id }

        interface SimulantState with
            member this.GetXtension () = this.Xtension
    
    /// Hosts the ongoing state of an entity. The end-user of this engine should never touch this
    /// type, and it's public _only_ to make [<CLIMutable>] work.
    and [<CLIMutable; NoEquality; NoComparison>] EntityState =
        { Id : Guid
          Name : Name
          Xtension : Xtension
          DispatcherNp : EntityDispatcher
          Specialization : string
          Persistent : bool
          CreationTimeStampNp : int64 // just needed for ordering writes to reduce diff volumes
          OptOverlayName : string option
          Position : Vector2 // NOTE: will become a Vector3 if Nu gets 3d capabilities
          Size : Vector2 // NOTE: will become a Vector3 if Nu gets 3d capabilities
          Rotation : single // NOTE: will become a Vector3 if Nu gets 3d capabilities
          Depth : single // NOTE: will become part of position if Nu gets 3d capabilities
          Overflow : Vector2
          ViewType : ViewType
          Visible : bool
          Omnipresent : bool
          PublishChanges : bool
          PublishUpdatesNp : bool
          PublishPostUpdatesNp : bool
          FacetNames : string Set
          FacetsNp : Facet list }

        /// Get a dynamic property and its type information.
        static member getProperty propertyName entityState =
            let xProperty = Xtension.getProperty propertyName entityState.Xtension
            (xProperty.PropertyValue, xProperty.PropertyType)

        /// Set a dynamic property with explicit type information.
        static member setProperty propertyName property entityState =
            let xProperty = { PropertyValue = fst property; PropertyType = snd property }
            { entityState with EntityState.Xtension = Xtension.setProperty propertyName xProperty entityState.Xtension }

        /// The dynamic look-up operator.
        static member get propertyName entityState : 'a =
            Xtension.(?) (entityState.Xtension, propertyName)

        /// The dynamic assignment operator.
        static member set propertyName (value : 'a) entityState =
            { entityState with EntityState.Xtension = Xtension.(?<-) (entityState.Xtension, propertyName, value) }

        /// Attach a dynamic property.
        static member attachProperty name value entityState =
            { entityState with EntityState.Xtension = Xtension.attachProperty name value entityState.Xtension }

        /// Detach a dynamic property.
        static member detachProperty name entityState =
            { entityState with EntityState.Xtension = Xtension.detachProperty name entityState.Xtension }
    
        /// Get an entity state's transform.
        static member getTransform this =
            { Transform.Position = this.Position
              Size = this.Size
              Rotation = this.Rotation
              Depth = this.Depth }

        /// Set an entity state's transform.
        static member setTransform (value : Transform) (this : EntityState) =
            { this with
                Position = value.Position
                Size = value.Size
                Rotation = value.Rotation
                Depth = value.Depth }

        /// Make an entity state value.
        static member make optSpecialization optName optOverlayName dispatcher =
            let (id, name) = Reflection.deriveNameAndId optName
            { Id = id
              Name = name
              Xtension = Xtension.safe
              DispatcherNp = dispatcher
              Specialization = Option.getOrDefault Constants.Engine.VanillaSpecialization optSpecialization
              Persistent = true
              CreationTimeStampNp = Core.getTimeStamp ()
              OptOverlayName = optOverlayName
              Position = Vector2.Zero
              Size = Constants.Engine.DefaultEntitySize
              Rotation = 0.0f
              Depth = 0.0f
              Overflow = Vector2.Zero
              ViewType = Relative
              Visible = true
              Omnipresent = false
              PublishChanges = true
              PublishUpdatesNp = false
              PublishPostUpdatesNp = false
              FacetNames = Set.empty
              FacetsNp = [] }

        /// Copy an entity such as when, say, you need it to be mutated with reflection but you need to preserve persistence.
        static member copy this =
            { this with EntityState.Id = this.Id }

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
          Groups : GroupDescriptor list }

        /// The empty screen descriptor.
        static member empty =
            { ScreenDispatcher = String.Empty
              ScreenProperties = Map.empty
              Groups = [] }

    /// Describes a group value independent of the engine.
    and [<NoComparison>] GroupDescriptor =
        { GroupDispatcher : string
          GroupProperties : Map<string, Symbol>
          Entities : EntityDescriptor list }

        /// The empty group descriptor.
        static member empty =
            { GroupDispatcher = String.Empty
              GroupProperties = Map.empty
              Entities = [] }

    /// Describes an entity value independent of the engine.
    and [<NoComparison>] EntityDescriptor =
        { EntityDispatcher : string
          EntityProperties : Map<string, Symbol> }

        /// The empty entity descriptor.
        static member empty =
            { EntityDispatcher = String.Empty
              EntityProperties = Map.empty }

    /// The null simulant. You should never encounter this as the user.
    and private NullSimulant () =
        interface Simulant with
            member this.ParticipantAddress = Address.makeFromName !!"Null"
            member this.SimulantAddress = Address.makeFromName !!"Null"
            member this.GetPublishingPriority _ _ = 0.0f
            end
    
    /// The game type that hosts the various screens used to navigate through a game.
    and [<StructuralEquality; NoComparison>] Game =
        { GameAddress : Game Address }
    
        interface Simulant with
            member this.ParticipantAddress = atoa<Game, Participant> this.GameAddress
            member this.SimulantAddress = atoa<Game, Simulant> this.GameAddress
            member this.GetPublishingPriority _ _ = Constants.Engine.GamePublishingPriority
            end
    
        /// View as address string.
        override this.ToString () = scstring this.GameAddress
    
        /// Get the full name of a game proxy.
        member this.GameFullName = atos this.GameAddress
    
        /// Get the latest value of a game's properties.
        [<DebuggerBrowsable (DebuggerBrowsableState.RootHidden)>]
        member private this.View = Debug.World.viewGame Debug.World.Chosen
    
        /// Create a Game proxy from an address.
        static member proxy address = { GameAddress = address }
    
        /// Concatenate two addresses, taking the type of first address.
        static member acatf<'a> (address : 'a Address) (game : Game) = acatf address (atooa game.GameAddress)
        
        /// Concatenate two addresses, forcing the type of first address.
        static member acatff<'a> (address : 'a Address) (entity : Entity) = acatff address entity.EntityAddress
    
        /// Concatenate two addresses, taking the type of first address.
        static member (->-) (address, game) = Game.acatf address game
    
        /// Concatenate two addresses, forcing the type of first address.
        static member (->>-) (address, game) = acatff address game
    
    /// The screen type that allows transitioning to and from other screens, and also hosts the
    /// currently interactive groups of entities.
    and [<StructuralEquality; NoComparison>] Screen =
        { ScreenAddress : Screen Address }
    
        interface Simulant with
            member this.ParticipantAddress = atoa<Screen, Participant> this.ScreenAddress
            member this.SimulantAddress = atoa<Screen, Simulant> this.ScreenAddress
            member this.GetPublishingPriority _ _ = Constants.Engine.ScreenPublishingPriority
            end
    
        /// View as address string.
        override this.ToString () = scstring this.ScreenAddress
    
        /// Get the full name of a screen proxy.
        member this.ScreenFullName = atos this.ScreenAddress
    
        /// Get the name of a screen proxy.
        member this.ScreenName = Address.getName this.ScreenAddress
    
        /// Get the latest value of a screen's properties.
        [<DebuggerBrowsable (DebuggerBrowsableState.RootHidden)>]
        member private this.View = Debug.World.viewScreen (this :> obj) Debug.World.Chosen
    
        /// Create a Screen proxy from an address.
        static member proxy address = { ScreenAddress = address }
    
        /// Concatenate two addresses, taking the type of first address.
        static member acatf<'a> (address : 'a Address) (screen : Screen) = acatf address (atooa screen.ScreenAddress)
        
        /// Concatenate two addresses, forcing the type of first address.
        static member acatff<'a> (address : 'a Address) (screen : Entity) = acatff address screen.EntityAddress
    
        /// Concatenate two addresses, taking the type of first address.
        static member (->-) (address, screen) = Screen.acatf address screen
    
        /// Concatenate two addresses, forcing the type of first address.
        static member (->>-) (address, screen) = acatff address screen
    
        /// Convert a name string to a screen's proxy.
        static member (!>) screenNameStr = Screen.proxy ^ ntoa !!screenNameStr
    
        /// Convert a screen's proxy to a group's by appending the group's name at the end.
        static member (=>) (screen, groupName) = Group.proxy ^ atoa<Screen, Group> screen.ScreenAddress ->- ntoa groupName
    
        /// Convert a screen's proxy to a group's by appending the group's name at the end.
        static member (=>) (screen : Screen, groupNameStr) = screen => !!groupNameStr
    
    /// Forms a logical group of entities.
    and [<StructuralEquality; NoComparison>] Group =
        { GroupAddress : Group Address }
    
        interface Simulant with
            member this.ParticipantAddress = atoa<Group, Participant> this.GroupAddress
            member this.SimulantAddress = atoa<Group, Simulant> this.GroupAddress
            member this.GetPublishingPriority _ _ = Constants.Engine.GroupPublishingPriority
            end
    
        /// View as address string.
        override this.ToString () = scstring this.GroupAddress
    
        /// Get the full name of a group proxy.
        member this.GroupFullName = atos this.GroupAddress
    
        /// Get the name of a group proxy.
        member this.GroupName = Address.getName this.GroupAddress
    
        /// Get the latest value of a group's properties.
        [<DebuggerBrowsable (DebuggerBrowsableState.RootHidden)>]
        member private this.View = Debug.World.viewGroup (this :> obj) Debug.World.Chosen
    
        /// Create a Group proxy from an address.
        static member proxy address = { GroupAddress = address }
    
        /// Concatenate two addresses, taking the type of first address.
        static member acatf<'a> (address : 'a Address) (group : Group) = acatf address (atooa group.GroupAddress)
        
        /// Concatenate two addresses, forcing the type of first address.
        static member acatff<'a> (address : 'a Address) (group : Entity) = acatff address group.EntityAddress
    
        /// Convert a group's proxy to its screen's.
        static member (!<) group = Screen.proxy ^ Address.allButLast group.GroupAddress
    
        /// Convert a group's proxy to an entity's by appending the entity's name at the end.
        static member (=>) (group, entityName) = Entity.proxy ^ atoa<Group, Entity> group.GroupAddress ->- ntoa entityName
    
        /// Convert a group's proxy to an entity's by appending the entity's name at the end.
        static member (=>) (group : Group, entityNameStr) = group => !!entityNameStr
    
        /// Concatenate two addresses, taking the type of first address.
        static member (->-) (address, group) = Group.acatf address group
    
        /// Concatenate two addresses, forcing the type of first address.
        static member (->>-) (address, group) = acatff address group
    
    /// The type around which the whole game engine is based! Used in combination with dispatchers
    /// to implement things like buttons, characters, blocks, and things of that sort.
    /// OPTIMIZATION: Includes pre-constructed entity change and update event addresses to avoid
    /// reconstructing new ones for each entity every frame.
    and [<StructuralEquality; NoComparison>] Entity =
        { EntityAddress : Entity Address
          UpdateAddress : unit Address
          PostUpdateAddress : unit Address }
    
        interface Simulant with
            member this.ParticipantAddress = atoa<Entity, Participant> this.EntityAddress
            member this.SimulantAddress = atoa<Entity, Simulant> this.EntityAddress
            member this.GetPublishingPriority getEntityPublishingPriority world = getEntityPublishingPriority this world
            end
    
        /// View as address string.
        override this.ToString () = scstring this.EntityAddress
    
        /// Get the name of an entity proxy.
        member this.EntityFullName = atos this.EntityAddress
    
        /// Get the name of an entity proxy.
        member this.EntityName = Address.getName this.EntityAddress
    
        /// Get the latest value of an entity's properties.
        [<DebuggerBrowsable (DebuggerBrowsableState.RootHidden)>]
        member private this.View = Debug.World.viewEntity (this :> obj) Debug.World.Chosen
    
        /// Create an Entity proxy from an address.
        static member proxy address =
            { EntityAddress = address
              UpdateAddress = ntoa !!"Update" ->>- address
              PostUpdateAddress = ntoa !!"PostUpdate" ->>- address }
    
        /// Concatenate two addresses, taking the type of first address.
        static member acatf<'a> (address : 'a Address) (entity : Entity) = acatf address (atooa entity.EntityAddress)
        
        /// Concatenate two addresses, forcing the type of first address.
        static member acatff<'a> (address : 'a Address) (entity : Entity) = acatff address entity.EntityAddress
    
        /// Convert an entity's proxy to its group's.
        static member (!<) entity = Group.proxy ^ Address.allButLast entity.EntityAddress
    
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
          GroupDispatchers : Map<string, GroupDispatcher>
          EntityDispatchers : Map<string, EntityDispatcher>
          Facets : Map<string, Facet>
          RebuildEntityTree : Screen -> World -> Entity QuadTree }
    
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
              OptEntityCache : KeyedCache<Entity Address * World, EntityState option>
              ScreenDirectory : Vmap<Name, Screen Address * Vmap<Name, Group Address * Vmap<Name, Entity Address>>>
              AmbientState : World AmbientState
              GameState : GameState
              ScreenStates : Vmap<Screen Address, ScreenState>
              GroupStates : Vmap<Group Address, GroupState>
              EntityStates : Vmap<Entity Address, EntityState> }

        interface World EventWorld with
            member this.GetLiveness () = AmbientState.getLiveness this.AmbientState
            member this.GetEventSystem () = this.EventSystem
            member this.GetNullParticipant () = NullSimulant () :> Participant
            member this.GetGlobalParticipant () = Game.proxy Address.empty :> Participant
            member this.UpdateEventSystem updater = { this with EventSystem = updater this.EventSystem }
            member this.ContainsParticipant participant =
                match participant with
                | :? Entity as entity -> Vmap.containsKey entity.EntityAddress this.EntityStates
                | :? Group as group -> Vmap.containsKey group.GroupAddress this.GroupStates
                | :? Screen as screen -> Vmap.containsKey screen.ScreenAddress this.ScreenStates
                | :? Game -> true
                | _  -> false
            member this.PublishEvent (participant : Participant) publisher eventData eventAddress eventTrace subscription world =
                match participant with
                | :? Entity -> EventWorld.publishEvent<'a, 'p, Entity, World> participant publisher eventData eventAddress eventTrace subscription world
                | :? Group -> EventWorld.publishEvent<'a, 'p, Group, World> participant publisher eventData eventAddress eventTrace subscription world
                | :? Screen -> EventWorld.publishEvent<'a, 'p, Screen, World> participant publisher eventData eventAddress eventTrace subscription world
                | :? Game -> EventWorld.publishEvent<'a, 'p, Game, World> participant publisher eventData eventAddress eventTrace subscription world
                | _ -> EventWorld.publishEvent<'a, 'p, Participant, World> participant publisher eventData eventAddress eventTrace subscription world

    /// Provides a way to make user-defined dispatchers, facets, and various other sorts of game-
    /// specific values.
    type NuPlugin () =

        /// Make user-defined subsystems such that Nu can utilitze them at run-time.
        abstract MakeSubsystems : unit -> (string * World Subsystem) list
        default this.MakeSubsystems () = []
    
        /// Optionally make a user-defined game dispatchers such that Nu can utilize it at run-time.
        abstract MakeOptGameDispatcher : unit -> GameDispatcher option
        default this.MakeOptGameDispatcher () = None
    
        /// Make user-defined screen dispatchers such that Nu can utilize them at run-time.
        abstract MakeScreenDispatchers : unit -> ScreenDispatcher list
        default this.MakeScreenDispatchers () = []
    
        /// Make user-defined group dispatchers such that Nu can utilize them at run-time.
        abstract MakeGroupDispatchers : unit -> GroupDispatcher list
        default this.MakeGroupDispatchers () = []
    
        /// Make user-defined entity dispatchers such that Nu can utilize them at run-time.
        abstract MakeEntityDispatchers : unit -> EntityDispatcher list
        default this.MakeEntityDispatchers () = []
    
        /// Make user-defined assets such that Nu can utilize them at run-time.
        abstract MakeFacets : unit -> Facet list
        default this.MakeFacets () = []
    
        /// Make the overlay routes that will allow Nu to use different overlays for the specified
        /// classifications.
        abstract MakeOverlayRoutes : unit -> (Classification * string option) list
        default this.MakeOverlayRoutes () = []

/// A simulant in the world.
type Simulant = WorldTypes.Simulant

/// The data for a change in the world's ambient state.
type AmbientChangeData = WorldTypes.AmbientChangeData

/// The default dispatcher for games.
type GameDispatcher = WorldTypes.GameDispatcher

/// The default dispatcher for screens.
type ScreenDispatcher = WorldTypes.ScreenDispatcher

/// The default dispatcher for groups.
type GroupDispatcher = WorldTypes.GroupDispatcher

/// The default dispatcher for entities.
type EntityDispatcher = WorldTypes.EntityDispatcher

/// Describes a game value independent of the engine.
type GameDescriptor = WorldTypes.GameDescriptor

/// Describes a screen value independent of the engine.
type ScreenDescriptor = WorldTypes.ScreenDescriptor

/// Describes a group value independent of the engine.
type GroupDescriptor = WorldTypes.GroupDescriptor

/// Describes an entity value independent of the engine.
type EntityDescriptor = WorldTypes.EntityDescriptor

/// Dynamically augments an entity's behavior in a composable way.
type Facet = WorldTypes.Facet

/// The game type that hosts the various screens used to navigate through a game.
type Game = WorldTypes.Game

/// The screen type that allows transitioning to and from other screens, and also hosts the
/// currently interactive groups of entities.
type Screen = WorldTypes.Screen

/// Forms a logical group of entities.
type Group = WorldTypes.Group

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