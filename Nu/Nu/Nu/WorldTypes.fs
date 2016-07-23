// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

// NOTE: this file is about twice as long as should be permissible, but the fact that I have to define mutally
// recursive types in the same file, as is also the case for abstract data types and their functions, prevents me from
// separating things out. This is doesn't really have a big impact on modularity - mostly it just hampers code
// organization.

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
open System.Reflection
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

    // Mutable clipboard that allows its state to persist beyond undo / redo.
    let private RefClipboard = ref<obj option> None

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
    
        /// Register a game when adding it to the world. Note that there is no corresponding
        /// Unregister method due to the inability to remove a game from the world.
        abstract Register : Game * World -> World
        default dispatcher.Register (_, world) = world
    
        /// Update a game.
        abstract Update : Game * World -> World
        default dispatcher.Update (_, world) = world
    
        /// Actualize a game.
        abstract Actualize : Game * World -> World
        default dispatcher.Actualize (_, world) = world
    
    /// The default dispatcher for screens.
    and ScreenDispatcher () =
    
        static member PropertyDefinitions =
            [Define? OptSpecialization (None : string option)
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
    
        /// Actualize a screen.
        abstract Actualize : Screen * World -> World
        default dispatcher.Actualize (_, world) = world
    
    /// The default dispatcher for groups.
    and GroupDispatcher () =
    
        static member PropertyDefinitions =
            [Define? OptSpecialization (None : string option)
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
    
        /// Actualize a group.
        abstract Actualize : Group * World -> World
        default dispatcher.Actualize (_, world) = world
    
    /// The default dispatcher for entities.
    and EntityDispatcher () =
    
        static member PropertyDefinitions =
            [Define? OptSpecialization (None : string option)
             Define? Position Vector2.Zero
             Define? Size Constants.Engine.DefaultEntitySize
             Define? Rotation 0.0f
             Define? Depth 0.0f
             Define? Overflow Vector2.Zero
             Define? ViewType Relative
             Define? Visible true
             Define? Omnipresent false
             Define? PublishUpdatesNp false
             Define? PublishChangesNp false
             Define? Persistent true]
    
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
    
        /// Actualize a facet.
        abstract Actualize : Entity * World -> World
        default facet.Actualize (_, world) = world
    
        /// Participate in getting the priority with which an entity is picked in the editor.
        abstract GetQuickSize : Entity * World -> Vector2
        default facet.GetQuickSize (_, _) = Constants.Engine.DefaultEntitySize

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
          CreationTimeStampNp : int64
          OptSelectedScreen : Screen option
          OptScreenTransitionDestination : Screen option }

        /// The dynamic look-up operator.
        static member get gameState propertyName : 'a =
            Xtension.(?) (gameState.Xtension, propertyName)

        /// The dynamic assignment operator.
        static member set gameState propertyName (value : 'a) =
            { gameState with GameState.Xtension = Xtension.(?<-) (gameState.Xtension, propertyName, value) }

        /// Attach a dynamic property.
        static member attachProperty name value gameState =
            { gameState with GameState.Xtension = Xtension.attachProperty name { PropertyValue = value; PropertyType = getType value } gameState.Xtension }
    
        /// Make a game state value.
        static member make dispatcher =
            { Id = makeGuid ()
              Xtension = Xtension.safe
              DispatcherNp = dispatcher
              CreationTimeStampNp = Core.getTimeStamp ()
              OptSelectedScreen = None
              OptScreenTransitionDestination = None }

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
          CreationTimeStampNp : int64
          EntityTreeNp : Entity QuadTree MutantCache
          OptSpecialization : string option
          TransitionStateNp : TransitionState
          TransitionTicksNp : int64
          Incoming : Transition
          Outgoing : Transition
          Persistent : bool }

        /// The dynamic look-up operator.
        static member get screenState propertyName : 'a =
            Xtension.(?) (screenState.Xtension, propertyName)

        /// The dynamic assignment operator.
        static member set screenState propertyName (value : 'a) =
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
                  CreationTimeStampNp = Core.getTimeStamp ()
                  EntityTreeNp = Unchecked.defaultof<Entity QuadTree MutantCache>
                  OptSpecialization = optSpecialization 
                  TransitionStateNp = IdlingState
                  TransitionTicksNp = 0L // TODO: roll this field into Incoming/OutcomingState values
                  Incoming = Transition.make Incoming
                  Outgoing = Transition.make Outgoing
                  Persistent = true }
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
          CreationTimeStampNp : int64
          OptSpecialization : string option
          Persistent : bool }

        /// The dynamic look-up operator.
        static member get groupState propertyName : 'a =
            Xtension.(?) (groupState.Xtension, propertyName)

        /// The dynamic assignment operator.
        static member set groupState propertyName (value : 'a) =
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
              CreationTimeStampNp = Core.getTimeStamp ()
              OptSpecialization = optSpecialization
              Persistent = true }

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
          CreationTimeStampNp : int64 // just needed for ordering writes to reduce diff volumes
          OptSpecialization : string option
          OptOverlayName : string option
          Position : Vector2 // NOTE: will become a Vector3 if Nu gets 3d capabilities
          Size : Vector2 // NOTE: will become a Vector3 if Nu gets 3d capabilities
          Rotation : single // NOTE: will become a Vector3 if Nu gets 3d capabilities
          Depth : single // NOTE: will become part of position if Nu gets 3d capabilities
          Overflow : Vector2
          ViewType : ViewType
          Visible : bool
          Omnipresent : bool
          PublishUpdatesNp : bool
          PublishChangesNp : bool
          Persistent : bool
          FacetNames : string Set
          FacetsNp : Facet list }

        /// Get a dynamic property and its type information.
        static member getProperty entityState propertyName =
            Xtension.getProperty propertyName entityState.Xtension

        /// The dynamic look-up operator.
        static member get entityState propertyName : 'a =
            Xtension.(?) (entityState.Xtension, propertyName)

        /// The dynamic assignment operator.
        static member set entityState propertyName (value : 'a) =
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
              CreationTimeStampNp = Core.getTimeStamp ()
              OptSpecialization = optSpecialization
              OptOverlayName = optOverlayName
              Position = Vector2.Zero
              Size = Constants.Engine.DefaultEntitySize
              Rotation = 0.0f
              Depth = 0.0f
              Overflow = Vector2.Zero
              ViewType = Relative
              Visible = true
              Omnipresent = false
              PublishUpdatesNp = false
              PublishChangesNp = false
              Persistent = true
              FacetNames = Set.empty
              FacetsNp = [] }

        /// Copy an entity such as when, say, you need it to be mutated with reflection but you need to preserve persistence.
        static member copy this =
            { this with EntityState.Id = this.Id }

        interface SimulantState with
            member this.GetXtension () = this.Xtension
    
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
        member this.GameFullName = Address.getFullName this.GameAddress
    
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
        member this.ScreenFullName = Address.getFullName this.ScreenAddress
    
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
        member this.GroupFullName = Address.getFullName this.GroupAddress
    
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
    /// OPTIMIZATION: Includes pre-constructed entity change and update event address to avoid
    /// reconstructing new ones for each entity every frame.
    and [<StructuralEquality; NoComparison>] Entity =
        { EntityAddress : Entity Address
          ChangeAddress : ParticipantChangeData<Entity, World> Address
          UpdateAddress : unit Address }
    
        interface Simulant with
            member this.ParticipantAddress = atoa<Entity, Participant> this.EntityAddress
            member this.SimulantAddress = atoa<Entity, Simulant> this.EntityAddress
            member this.GetPublishingPriority getEntityPublishingPriority world = getEntityPublishingPriority this world
            end
    
        /// View as address string.
        override this.ToString () = scstring this.EntityAddress
    
        /// Get the name of an entity proxy.
        member this.EntityFullName = Address.getFullName this.EntityAddress
    
        /// Get the name of an entity proxy.
        member this.EntityName = Address.getName this.EntityAddress
    
        /// Get the latest value of an entity's properties.
        [<DebuggerBrowsable (DebuggerBrowsableState.RootHidden)>]
        member private this.View = Debug.World.viewEntity (this :> obj) Debug.World.Chosen
    
        /// Create an Entity proxy from an address.
        static member proxy address =
            { EntityAddress = address
              ChangeAddress = ltoa [!!"Entity"; !!"Change"] ->>- address
              UpdateAddress = ntoa !!"Update" ->>- address }
    
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
    and [<ReferenceEquality>] World =
        private
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
            member this.GetEmptyParticipant () = Game.proxy Address.empty :> Participant
            member this.UpdateEventSystem updater = { this with EventSystem = updater this.EventSystem }
            member this.ContainsParticipant participant =
                match participant with
                | :? Game -> true
                | :? Screen as screen -> Vmap.containsKey screen.ScreenAddress this.ScreenStates
                | :? Group as group -> Vmap.containsKey group.GroupAddress this.GroupStates
                | :? Entity as entity -> Vmap.containsKey entity.EntityAddress this.EntityStates
                | _ -> failwithumf ()
            member this.PublishEvent (participant : Participant) publisher eventData eventAddress eventTrace subscription world = 
                match Address.getNames participant.ParticipantAddress with
                | [] -> EventWorld.publishEvent<'a, 'p, Game, World> participant publisher eventData eventAddress eventTrace subscription world
                | [_] -> EventWorld.publishEvent<'a, 'p, Screen, World> participant publisher eventData eventAddress eventTrace subscription world
                | [_; _] -> EventWorld.publishEvent<'a, 'p, Group, World> participant publisher eventData eventAddress eventTrace subscription world
                | [_; _; _] -> EventWorld.publishEvent<'a, 'p, Entity, World> participant publisher eventData eventAddress eventTrace subscription world
                | _ -> failwithumf ()

        (* Debug *)

        /// Choose a world to be used for debugging. Call this whenever the most recently constructed
        /// world value is to be discarded in favor of the given world value.
        static member choose (world : World) =
#if DEBUG
            Debug.World.Chosen <- world :> obj
#endif
            world

        (* EntityTree *)

        /// Rebuild the entity tree if needed.
        static member internal rebuildEntityTree screen world =
            world.Dispatchers.RebuildEntityTree screen world

        (* EventSystem *)

        /// Get event subscriptions.
        static member getSubscriptions world =
            EventWorld.getSubscriptions<World> world

        /// Get event unsubscriptions.
        static member getUnsubscriptions world =
            EventWorld.getUnsubscriptions<World> world

        /// Add event state to the world.
        static member addEventState key state world =
            EventWorld.addEventState<'a, World> key state world

        /// Remove event state from the world.
        static member removeEventState key world =
            EventWorld.removeEventState<World> key world

        /// Get event state from the world.
        static member getEventState<'a> key world =
            EventWorld.getEventState<'a, World> key world

        /// Get whether events are being traced.
        static member getEventTracing world =
            EventWorld.getEventTracing world

        /// Set whether events are being traced.
        static member setEventTracing tracing world =
            EventWorld.setEventTracing tracing world

        /// Get the state of the event filter.
        static member getEventFilter world =
            EventWorld.getEventFilter world

        /// Set the state of the event filter.
        static member setEventFilter filter world =
            EventWorld.setEventFilter filter world

        /// TODO: document.
        static member getSubscriptionsSorted (publishSorter : SubscriptionSorter<World>) eventAddress world =
            EventWorld.getSubscriptionsSorted publishSorter eventAddress world

        /// TODO: document.
        static member getSubscriptionsSorted3 (publishSorter : SubscriptionSorter<World>) eventAddress world =
            EventWorld.getSubscriptionsSorted3 publishSorter eventAddress world

        /// Sort subscriptions using categorization via the 'by' procedure.
        static member sortSubscriptionsBy by (subscriptions : SubscriptionEntry list) (world : World) =
            EventWorld.sortSubscriptionsBy by subscriptions world

        /// Sort subscriptions by their place in the world's simulant hierarchy.
        static member sortSubscriptionsByHierarchy subscriptions world =
            World.sortSubscriptionsBy
                (fun _ _ -> Constants.Engine.EntityPublishingPriority)
                subscriptions
                world

        /// A 'no-op' for subscription sorting - that is, performs no sorting at all.
        static member sortSubscriptionsNone (subscriptions : SubscriptionEntry list) (world : World) =
            EventWorld.sortSubscriptionsNone subscriptions world

        /// Publish an event, using the given getSubscriptions and publishSorter procedures to arrange the order to which subscriptions are published.
        static member publish7<'a, 'p when 'p :> Simulant> getSubscriptions publishSorter (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) world =
            EventWorld.publish7<'a, 'p, World> getSubscriptions publishSorter eventData eventAddress eventTrace publisher world

        /// Publish an event, using the given publishSorter procedure to arrange the order to which subscriptions are published.
        static member publish6<'a, 'p when 'p :> Simulant> publishSorter (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) world =
            EventWorld.publish6<'a, 'p, World> publishSorter eventData eventAddress eventTrace publisher world

        /// Publish an event.
        static member publish<'a, 'p when 'p :> Simulant>
            (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) world =
            EventWorld.publish6<'a, 'p, World> World.sortSubscriptionsByHierarchy eventData eventAddress eventTrace publisher world

        /// Unsubscribe from an event.
        static member unsubscribe subscriptionKey world =
            EventWorld.unsubscribe<World> subscriptionKey world

        /// Subscribe to an event using the given subscriptionKey, and be provided with an unsubscription callback.
        static member subscribePlus5<'a, 's when 's :> Simulant>
            subscriptionKey (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
            EventWorld.subscribePlus5<'a, 's, World> subscriptionKey subscription eventAddress subscriber world

        /// Subscribe to an event, and be provided with an unsubscription callback.
        static member subscribePlus<'a, 's when 's :> Simulant>
            (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
            EventWorld.subscribePlus<'a, 's, World> subscription eventAddress subscriber world

        /// Subscribe to an event using the given subscriptionKey.
        static member subscribe5<'a, 's when 's :> Simulant>
            subscriptionKey (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
            EventWorld.subscribe5<'a, 's, World> subscriptionKey subscription eventAddress subscriber world

        /// Subscribe to an event.
        static member subscribe<'a, 's when 's :> Simulant>
            (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
            EventWorld.subscribe<'a, 's, World> subscription eventAddress subscriber world

        /// Keep active a subscription for the lifetime of a simulant, and be provided with an unsubscription callback.
        static member monitorPlus<'a, 's when 's :> Simulant>
            (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
            EventWorld.monitorPlus<'a, 's, World> subscription eventAddress subscriber world

        /// Keep active a subscription for the lifetime of a simulant.
        static member monitor<'a, 's when 's :> Simulant>
            (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
            EventWorld.monitor<'a, 's, World> subscription eventAddress subscriber world

        (* Dispatchers *)

        /// Get the game dispatchers of the world.
        static member getGameDispatchers world =
            world.Dispatchers.GameDispatchers

        /// Get the screen dispatchers of the world.
        static member getScreenDispatchers world =
            world.Dispatchers.ScreenDispatchers

        /// Get the group dispatchers of the world.
        static member getGroupDispatchers world =
            world.Dispatchers.GroupDispatchers

        /// Get the entity dispatchers of the world.
        static member getEntityDispatchers world =
            world.Dispatchers.EntityDispatchers

        /// Get the facets of the world.
        static member getFacets world =
            world.Dispatchers.Facets

        (* Subsystems *)

        static member internal getSubsystemMap world =
            Subsystems.getSubsystemMap world.Subsystems

        static member internal getSubsystem<'s when 's :> World Subsystem> name world : 's =
            Subsystems.getSubsystem name world.Subsystems

        static member internal getSubsystemBy<'s, 't when 's :> World Subsystem> (by : 's -> 't) name world : 't =
            Subsystems.getSubsystemBy by name world.Subsystems

        // NOTE: it'd be nice to get rid of this function to improve encapsulation, but I can't seem to do so in practice...
        static member internal setSubsystem<'s when 's :> World Subsystem> (subsystem : 's) name world =
            World.choose { world with Subsystems = Subsystems.setSubsystem subsystem name world.Subsystems }

        static member internal updateSubsystem<'s when 's :> World Subsystem> (updater : 's -> World -> 's) name world =
            World.choose { world with Subsystems = Subsystems.updateSubsystem updater name world.Subsystems world }

        static member internal updateSubsystems (updater : World Subsystem -> World -> World Subsystem) world =
            World.choose { world with Subsystems = Subsystems.updateSubsystems updater world.Subsystems world }

        static member internal clearSubsystemsMessages world =
            World.choose { world with Subsystems = Subsystems.clearSubsystemsMessages world.Subsystems world }

        (* AmbientState *)

        static member internal getAmbientState world =
            world.AmbientState

        static member internal getAmbientStateBy by world =
            by world.AmbientState

        static member internal updateAmbientState updater world =
            World.choose { world with AmbientState = updater world.AmbientState }

        static member internal updateAmbientStateWithoutEvent updater world =
            let _ = world
            let world = World.choose { world with AmbientState = updater world.AmbientState }
            let _ = EventTrace.record "World" "updateAmbientState" EventTrace.empty
            world
    
        /// Get the tick rate.
        static member getTickRate world =
            World.getAmbientStateBy AmbientState.getTickRate world

        /// Get the tick rate as a floating-point value.
        static member getTickRateF world =
            World.getAmbientStateBy AmbientState.getTickRateF world

        /// Set the tick rate without waiting for the end of the current update. Only use
        /// this if you need it and understand the engine internals well enough to know the
        /// consequences.
        static member setTickRateImmediately tickRate world =
            World.updateAmbientState (AmbientState.setTickRateImmediately tickRate) world

        /// Set the tick rate.
        static member setTickRate tickRate world =
            World.updateAmbientState
                (AmbientState.addTasklet
                    { ScheduledTime = World.getTickTime world; Command = { Execute = fun world -> World.setTickRateImmediately tickRate world }}) world

        /// Reset the tick time to 0.
        static member resetTickTime world =
            World.updateAmbientState
                (AmbientState.addTasklet
                    { ScheduledTime = World.getTickTime world; Command = { Execute = fun world -> World.updateAmbientState AmbientState.resetTickTime world }}) world

        /// Get the world's tick time.
        static member getTickTime world =
            World.getAmbientStateBy AmbientState.getTickTime world

        /// Query that the world is ticking.
        static member isTicking world =
            World.getAmbientStateBy AmbientState.isTicking world

        static member internal updateTickTime world =
            World.updateAmbientStateWithoutEvent AmbientState.updateTickTime world

        /// Get the world's update count.
        static member getUpdateCount world =
            World.getAmbientStateBy AmbientState.getUpdateCount world

        static member internal incrementUpdateCount world =
            World.updateAmbientStateWithoutEvent AmbientState.incrementUpdateCount world

        /// Get the the liveness state of the engine.
        static member getLiveness world =
            World.getAmbientStateBy AmbientState.getLiveness world

        /// Place the engine into a state such that the app will exit at the end of the current update.
        static member exit world =
            World.updateAmbientState AmbientState.exit world

        static member internal getTasklets world =
            World.getAmbientStateBy AmbientState.getTasklets world

        static member internal clearTasklets world =
            World.updateAmbientStateWithoutEvent AmbientState.clearTasklets world

        static member internal restoreTasklets tasklets world =
            World.updateAmbientStateWithoutEvent (AmbientState.restoreTasklets tasklets) world

        /// Add a tasklet to be executed by the engine at the scheduled time.
        static member addTasklet tasklet world =
            World.updateAmbientStateWithoutEvent (AmbientState.addTasklet tasklet) world

        /// Add multiple tasklets to be executed by the engine at the scheduled times.
        static member addTasklets tasklets world =
            World.updateAmbientStateWithoutEvent (AmbientState.addTasklets tasklets) world

        /// Get a value from the camera used to view the world.
        static member getCameraBy by world =
            World.getAmbientStateBy (AmbientState.getCameraBy by) world

        /// Get the camera used to view the world.
        static member getCamera world =
            World.getAmbientStateBy AmbientState.getCamera world

        /// Update the camera used to view the world.
        static member updateCamera updater world =
            World.updateAmbientState (AmbientState.updateCamera updater) world

        /// Get the asset metadata map.
        static member getAssetMetadataMap world =
            AmbientState.getAssetMetadataMap ^ World.getAmbientState world

        static member internal setAssetMetadataMap assetMetadataMap world =
            World.updateAmbientState (AmbientState.setAssetMetadataMap assetMetadataMap) world

        static member internal getOverlayerBy by world =
            let overlayer = World.getAmbientStateBy AmbientState.getOverlayer world
            by overlayer

        static member internal getOverlayer world =
            World.getOverlayerBy id world

        static member internal setOverlayer overlayer world =
            World.updateAmbientState (AmbientState.setOverlayer overlayer) world

        /// Get intrinsic overlays.
        static member getIntrinsicOverlays world =
            World.getOverlayerBy Overlayer.getIntrinsicOverlays world

        /// Get extrinsic overlays.
        static member getExtrinsicOverlays world =
            World.getOverlayerBy Overlayer.getExtrinsicOverlays world

        static member internal getOverlayRouter world =
            World.getAmbientStateBy AmbientState.getOverlayRouter world

        static member internal getSymbolStoreBy by world =
            World.getAmbientStateBy (AmbientState.getSymbolStoreBy by) world

        static member internal getSymbolStore world =
            World.getAmbientStateBy AmbientState.getSymbolStore world

        static member internal setSymbolStore symbolStore world =
            World.updateAmbientState (AmbientState.setSymbolStore symbolStore) world

        static member internal updateSymbolStore updater world =
            World.updateAmbientState (AmbientState.updateSymbolStore updater) world

        /// Try to load a symbol store package with the given name.
        static member tryLoadSymbolStorePackage packageName world =
            World.updateSymbolStore (SymbolStore.tryLoadSymbolStorePackage packageName) world

        /// Unload a symbol store package with the given name.
        static member unloadSymbolStorePackage packageName world =
            World.updateSymbolStore (SymbolStore.unloadSymbolStorePackage packageName) world

        /// Try to find a symbol with the given asset tag.
        static member tryFindSymbol assetTag world =
            let symbolStore = World.getSymbolStore world
            let (symbol, symbolStore) = SymbolStore.tryFindSymbol assetTag symbolStore
            let world = World.setSymbolStore symbolStore world
            (symbol, world)

        /// Try to find symbols with the given asset tags.
        static member tryFindSymbols assetTags world =
            let symbolStore = World.getSymbolStore world
            let (symbol, symbolStore) = SymbolStore.tryFindSymbols assetTags symbolStore
            let world = World.setSymbolStore symbolStore world
            (symbol, world)

        /// Reload all the symbols in the symbol store.
        static member reloadSymbols world =
            World.updateSymbolStore SymbolStore.reloadSymbols world

        /// Get the user state of the world, casted to 'u.
        static member getUserState world : 'u =
            World.getAmbientStateBy AmbientState.getUserState world

        /// Update the user state of the world.
        static member updateUserState (updater : 'u -> 'v) world =
            World.updateAmbientState (AmbientState.updateUserState updater) world

        (* OptEntityCache *)

        /// Get the opt entity cache.
        static member internal getOptEntityCache world =
            world.OptEntityCache

        /// Get the opt entity cache.
        static member internal setOptEntityCache optEntityCache world =
            World.choose { world with OptEntityCache = optEntityCache }

        (* ScreenDirectory *)

        /// Get the opt entity cache.
        static member internal getScreenDirectory world =
            world.ScreenDirectory

        (* Facet *)

        static member private tryGetFacet facetName world =
            let facets = World.getFacets world
            match Map.tryFind facetName facets with
            | Some facet -> Right facet
            | None -> Left ^ "Invalid facet name '" + facetName + "'."

        static member private isFacetCompatibleWithEntity entityDispatcherMap facet (entityState : EntityState) =
            // Note a facet is incompatible with any other facet if it contains any properties that has
            // the same name but a different type.
            let facetType = facet.GetType ()
            let facetPropertyDefinitions = Reflection.getPropertyDefinitions facetType
            if Reflection.isFacetCompatibleWithDispatcher entityDispatcherMap facet entityState then
                List.notExists
                    (fun definition ->
                        match Xtension.tryGetProperty definition.PropertyName entityState.Xtension with
                        | Some property -> property.GetType () <> definition.PropertyType
                        | None -> false)
                    facetPropertyDefinitions
            else false

        static member private getFacetNamesToAdd oldFacetNames newFacetNames =
            Set.difference newFacetNames oldFacetNames

        static member private getFacetNamesToRemove oldFacetNames newFacetNames =
            Set.difference oldFacetNames newFacetNames

        static member private getEntityPropertyDefinitionNamesToDetach entityState facetToRemove =

            // get the property definition name counts of the current, complete entity
            let propertyDefinitions = Reflection.getReflectivePropertyDefinitionMap entityState
            let propertyDefinitionNameCounts = Reflection.getPropertyDefinitionNameCounts propertyDefinitions

            // get the property definition name counts of the facet to remove
            let facetType = facetToRemove.GetType ()
            let facetPropertyDefinitions = Map.singleton facetType.Name ^ Reflection.getPropertyDefinitions facetType
            let facetPropertyDefinitionNameCounts = Reflection.getPropertyDefinitionNameCounts facetPropertyDefinitions

            // compute the difference of the counts
            let finalPropertyDefinitionNameCounts =
                Map.map
                    (fun propertyName propertyCount ->
                        match Map.tryFind propertyName facetPropertyDefinitionNameCounts with
                        | Some facetPropertyCount -> propertyCount - facetPropertyCount
                        | None -> propertyCount)
                    propertyDefinitionNameCounts

            // build a set of all property names where the final counts are negative
            Map.fold
                (fun propertyNamesToDetach propertyName propertyCount ->
                    if propertyCount = 0
                    then Set.add propertyName propertyNamesToDetach
                    else propertyNamesToDetach)
                Set.empty
                finalPropertyDefinitionNameCounts

        static member private tryRemoveFacet facetName entityState optEntity world =
            match List.tryFind (fun facet -> getTypeName facet = facetName) entityState.FacetsNp with
            | Some facet ->
                let (entityState, world) =
                    match optEntity with
                    | Some entity ->
                        let world = facet.Unregister (entity, world)
                        let entityState = World.getEntityState entity world
                        (entityState, world)
                    | None -> (entityState, world)
                let facetNames = Set.remove facetName entityState.FacetNames
                let facetsNp = List.remove ((=) facet) entityState.FacetsNp
                let entityState = { entityState with FacetNames = facetNames }
                let entityState = { entityState with FacetsNp = facetsNp }
                let propertyNames = World.getEntityPropertyDefinitionNamesToDetach entityState facet
                let entityState = Reflection.detachPropertiesViaNames EntityState.copy propertyNames entityState
                match optEntity with
                | Some entity ->
                    let oldWorld = world
                    let world = World.setEntityStateWithoutEvent entityState entity world
                    let world = World.updateEntityInEntityTree entity oldWorld world
                    let world = World.publishEntityChange entityState entity oldWorld world
                    Right (World.getEntityState entity world, world)
                | None -> Right (entityState, world)
            | None -> let _ = World.choose world in Left ^ "Failure to remove facet '" + facetName + "' from entity."

        static member private tryAddFacet facetName (entityState : EntityState) optEntity world =
            match World.tryGetFacet facetName world with
            | Right facet ->
                let entityDispatchers = World.getEntityDispatchers world
                if World.isFacetCompatibleWithEntity entityDispatchers facet entityState then
                    let facetNames = Set.add facetName entityState.FacetNames
                    let facetsNp = facet :: entityState.FacetsNp
                    let entityState = { entityState with FacetNames = facetNames }
                    let entityState = { entityState with FacetsNp = facetsNp }
                    let entityState = Reflection.attachProperties EntityState.copy facet entityState
                    match optEntity with
                    | Some entity ->
                        let oldWorld = world
                        let world = World.setEntityStateWithoutEvent entityState entity world
                        let world = World.updateEntityInEntityTree entity oldWorld world
                        let world = World.publishEntityChange entityState entity oldWorld world
                        let world = facet.Register (entity, world)
                        Right (World.getEntityState entity world, world)
                    | None -> Right (entityState, world)
                else let _ = World.choose world in Left ^ "Facet '" + getTypeName facet + "' is incompatible with entity '" + scstring entityState.Name + "'."
            | Left error -> Left error

        static member private tryRemoveFacets facetNamesToRemove entityState optEntity world =
            Set.fold
                (fun eitherEntityWorld facetName ->
                    match eitherEntityWorld with
                    | Right (entityState, world) -> World.tryRemoveFacet facetName entityState optEntity world
                    | Left _ as left -> left)
                (Right (entityState, world))
                facetNamesToRemove

        static member private tryAddFacets facetNamesToAdd entityState optEntity world =
            Set.fold
                (fun eitherEntityStateWorld facetName ->
                    match eitherEntityStateWorld with
                    | Right (entityState, world) -> World.tryAddFacet facetName entityState optEntity world
                    | Left _ as left -> left)
                (Right (entityState, world))
                facetNamesToAdd

        static member internal trySetFacetNames facetNames entityState optEntity world =
            let facetNamesToRemove = World.getFacetNamesToRemove entityState.FacetNames facetNames
            let facetNamesToAdd = World.getFacetNamesToAdd entityState.FacetNames facetNames
            match World.tryRemoveFacets facetNamesToRemove entityState optEntity world with
            | Right (entityState, world) -> World.tryAddFacets facetNamesToAdd entityState optEntity world
            | Left _ as left -> left

        static member internal trySynchronizeFacetsToNames oldFacetNames entityState optEntity world =
            let facetNamesToRemove = World.getFacetNamesToRemove oldFacetNames entityState.FacetNames
            let facetNamesToAdd = World.getFacetNamesToAdd oldFacetNames entityState.FacetNames
            match World.tryRemoveFacets facetNamesToRemove entityState optEntity world with
            | Right (entityState, world) -> World.tryAddFacets facetNamesToAdd entityState optEntity world
            | Left _ as left -> left

        static member internal attachIntrinsicFacetsViaNames entityState world =
            let entityDispatchers = World.getEntityDispatchers world
            let facets = World.getFacets world
            Reflection.attachIntrinsicFacets EntityState.copy entityDispatchers facets entityState.DispatcherNp entityState

        (* SimulantState *)

        /// View the member properties of some SimulantState.
        static member internal viewMemberProperties (state : SimulantState) =
            state |>
            getType |>
            getProperties |>
            Array.map (fun (property : PropertyInfo) -> (property.Name, property.GetValue state))

        /// View the xtension properties of some SimulantState.
        static member internal viewXProperties (state : SimulantState) =
            state.GetXtension () |>
            Xtension.toSeq |>
            Array.ofSeq |>
            Array.sortBy fst |>
            Array.map (fun (name, property) -> (name, property.PropertyValue))

        /// Provides a full view of all the member values of some SimulantState.
        static member internal view state =
            Array.append
                (World.viewMemberProperties state)
                (World.viewXProperties state)

        (* EntityState *)

        static member private optEntityStateKeyEquality 
            (entityAddress : Entity Address, world : World)
            (entityAddress2 : Entity Address, world2 : World) =
            refEq entityAddress entityAddress2 && refEq world world2

        static member private optEntityGetFreshKeyAndValue entity world =
            let optEntityState = Vmap.tryFind entity.EntityAddress ^ world.EntityStates
            ((entity.EntityAddress, world), optEntityState)

        static member private optEntityStateFinder entity world =
            KeyedCache.getValue
                World.optEntityStateKeyEquality
                (fun () -> World.optEntityGetFreshKeyAndValue entity world)
                (entity.EntityAddress, world)
                (World.getOptEntityCache world)

        static member private entityStateSetter entityState entity world =
#if DEBUG
            if not ^ Vmap.containsKey entity.EntityAddress world.EntityStates then
                failwith ^ "Cannot set the state of a non-existent entity '" + scstring entity.EntityAddress + "'"
#endif
            let entityStates = Vmap.add entity.EntityAddress entityState world.EntityStates
            World.choose { world with EntityStates = entityStates }

        static member private entityStateAdder entityState entity world =
            let screenDirectory =
                match Address.getNames entity.EntityAddress with
                | [screenName; groupName; entityName] ->
                    match Vmap.tryFind screenName world.ScreenDirectory with
                    | Some (screenAddress, groupDirectory) ->
                        match Vmap.tryFind groupName groupDirectory with
                        | Some (groupAddress, entityDirectory) ->
                            let entityDirectory = Vmap.add entityName entity.EntityAddress entityDirectory
                            let groupDirectory = Vmap.add groupName (groupAddress, entityDirectory) groupDirectory
                            Vmap.add screenName (screenAddress, groupDirectory) world.ScreenDirectory
                        | None -> failwith ^ "Cannot add entity '" + scstring entity.EntityAddress + "' to non-existent group."
                    | None -> failwith ^ "Cannot add entity '" + scstring entity.EntityAddress + "' to non-existent screen."
                | _ -> failwith ^ "Invalid entity address '" + scstring entity.EntityAddress + "'."
            let entityStates = Vmap.add entity.EntityAddress entityState world.EntityStates
            World.choose { world with ScreenDirectory = screenDirectory; EntityStates = entityStates }

        static member private entityStateRemover entity world =
            let screenDirectory =
                match Address.getNames entity.EntityAddress with
                | [screenName; groupName; entityName] ->
                    match Vmap.tryFind screenName world.ScreenDirectory with
                    | Some (screenAddress, groupDirectory) ->
                        match Vmap.tryFind groupName groupDirectory with
                        | Some (groupAddress, entityDirectory) ->
                            let entityDirectory = Vmap.remove entityName entityDirectory
                            let groupDirectory = Vmap.add groupName (groupAddress, entityDirectory) groupDirectory
                            Vmap.add screenName (screenAddress, groupDirectory) world.ScreenDirectory
                        | None -> failwith ^ "Cannot remove entity '" + scstring entity.EntityAddress + "' from non-existent group."
                    | None -> failwith ^ "Cannot remove entity '" + scstring entity.EntityAddress + "' from non-existent screen."
                | _ -> failwith ^ "Invalid entity address '" + scstring entity.EntityAddress + "'."
            let entityStates = Vmap.remove entity.EntityAddress world.EntityStates
            World.choose { world with ScreenDirectory = screenDirectory; EntityStates = entityStates }

        /// Query that the world contains an entity.
        static member containsEntity entity world =
            Option.isSome ^ World.getOptEntityState entity world

        static member private getEntityStateBoundsMax entityState =
            // TODO: get up off yer arse and write an algorithm for tight-fitting bounds...
            match entityState.Rotation with
            | 0.0f ->
                let boundsOverflow = Math.makeBoundsOverflow entityState.Position entityState.Size entityState.Overflow
                boundsOverflow // no need to transform is unrotated
            | _ ->
                let boundsOverflow = Math.makeBoundsOverflow entityState.Position entityState.Size entityState.Overflow
                let position = boundsOverflow.Xy
                let size = Vector2 (boundsOverflow.Z, boundsOverflow.W) - position
                let center = position + size * 0.5f
                let corner = position + size
                let centerToCorner = corner - center
                let quaternion = Quaternion.FromAxisAngle (Vector3.UnitZ, Constants.Math.DegreesToRadiansF * 45.0f)
                let newSizeOver2 = Vector2 (Vector2.Transform (centerToCorner, quaternion)).Y
                let newPosition = center - newSizeOver2
                let newSize = newSizeOver2 * 2.0f
                Vector4 (newPosition.X, newPosition.Y, newPosition.X + newSize.X, newPosition.Y + newSize.Y)

        static member private publishEntityChange entityState (entity : Entity) oldWorld world =
            if entityState.PublishChangesNp then
                let eventTrace = EventTrace.record "World" "publishEntityChange" EventTrace.empty
                World.publish { Participant = entity; OldWorld = oldWorld } entity.ChangeAddress eventTrace entity world
            else world

        static member private getOptEntityState entity world =
            World.optEntityStateFinder entity world

        static member private getEntityState entity world =
            match World.getOptEntityState entity world with
            | Some entityState -> entityState
            | None -> failwith ^ "Could not find entity with address '" + scstring entity.EntityAddress + "'."

        static member private addEntityState entityState entity world =
            World.entityStateAdder entityState entity world

        static member private removeEntityState entity world =
            World.entityStateRemover entity world

        static member private setEntityStateWithoutEvent entityState entity world =
            World.entityStateSetter entityState entity world

        static member private setEntityState entityState (entity : Entity) world =
            let oldWorld = world
            let world = World.entityStateSetter entityState entity world
            World.publishEntityChange entityState entity oldWorld world

        static member private updateEntityStateWithoutEvent updater entity world =
            let entityState = World.getEntityState entity world
            let entityState = updater entityState
            World.setEntityStateWithoutEvent entityState entity world

        static member private updateEntityState updater entity world =
            let entityState = World.getEntityState entity world
            let entityState = updater entityState
            World.setEntityState entityState entity world

        static member private updateEntityStatePlus updater entity world =
            let oldWorld = world
            let world = World.updateEntityStateWithoutEvent updater entity world
            let world = World.updateEntityInEntityTree entity oldWorld world
            World.publishEntityChange (World.getEntityState entity world) entity oldWorld world

        static member internal getEntityId entity world = (World.getEntityState entity world).Id
        static member internal getEntityName entity world = (World.getEntityState entity world).Name
        static member internal getEntityXtension entity world = (World.getEntityState entity world).Xtension // TODO: try to get rid of this
        static member internal getEntityDispatcherNp entity world = (World.getEntityState entity world).DispatcherNp
        static member internal getEntityCreationTimeStampNp entity world = (World.getEntityState entity world).CreationTimeStampNp
        static member internal getEntityOptSpecialization entity world = (World.getEntityState entity world).OptSpecialization
        static member internal getEntityOptOverlayName entity world = (World.getEntityState entity world).OptOverlayName
        static member internal setEntityOptOverlayName value entity world = World.updateEntityState (fun entityState -> { entityState with OptOverlayName = value }) entity world
        static member internal getEntityPosition entity world = (World.getEntityState entity world).Position
        static member internal setEntityPosition value entity world = World.updateEntityStatePlus (fun entityState -> { entityState with EntityState.Position = value }) entity world
        static member internal getEntitySize entity world = (World.getEntityState entity world).Size
        static member internal setEntitySize value entity world = World.updateEntityStatePlus (fun entityState -> { entityState with Size = value }) entity world
        static member internal getEntityRotation entity world = (World.getEntityState entity world).Rotation
        static member internal setEntityRotation value entity world = World.updateEntityStatePlus (fun entityState -> { entityState with Rotation = value }) entity world
        static member internal getEntityDepth entity world = (World.getEntityState entity world).Depth
        static member internal setEntityDepth value entity world = World.updateEntityState (fun entityState -> { entityState with Depth = value }) entity world
        static member internal getEntityOverflow entity world = (World.getEntityState entity world).Overflow
        static member internal setEntityOverflow value entity world = World.updateEntityStatePlus (fun entityState -> { entityState with Overflow = value }) entity world
        static member internal getEntityViewType entity world = (World.getEntityState entity world).ViewType
        static member internal setEntityViewType value entity world = World.updateEntityStatePlus (fun entityState -> { entityState with ViewType = value }) entity world
        static member internal getEntityVisible entity world = (World.getEntityState entity world).Visible
        static member internal setEntityVisible value entity world = World.updateEntityState (fun entityState -> { entityState with Visible = value }) entity world
        static member internal getEntityOmnipresent entity world = (World.getEntityState entity world).Omnipresent
        static member internal setEntityOmnipresent value entity world = World.updateEntityStatePlus (fun entityState -> { entityState with Omnipresent = value }) entity world
        static member internal getEntityPublishUpdatesNp entity world = (World.getEntityState entity world).PublishUpdatesNp
        static member internal setEntityPublishUpdatesNp value entity world = World.updateEntityState (fun entityState -> { entityState with PublishUpdatesNp = value }) entity world
        static member internal getEntityPublishChangesNp entity world = (World.getEntityState entity world).PublishChangesNp
        static member internal setEntityPublishChangesNp value entity world = World.updateEntityState (fun entityState -> { entityState with PublishChangesNp = value }) entity world
        static member internal getEntityPersistent entity world = (World.getEntityState entity world).Persistent
        static member internal setEntityPersistent value entity world = World.updateEntityState (fun entityState -> { entityState with Persistent = value }) entity world
        static member internal getEntityFacetNames entity world = (World.getEntityState entity world).FacetNames
        static member internal getEntityFacetsNp entity world = (World.getEntityState entity world).FacetsNp
        static member internal getEntityTransform entity world = EntityState.getTransform (World.getEntityState entity world)
        static member internal setEntityTransform value entity world = World.updateEntityStatePlus (EntityState.setTransform value) entity world
        static member internal attachEntityProperty name value entity world = World.setEntityState (EntityState.attachProperty name value ^ World.getEntityState entity world) entity world
        static member internal detachEntityProperty name entity world = World.setEntityState (EntityState.detachProperty name ^ World.getEntityState entity world) entity world

        /// Get an entity's property.
        static member internal getEntityProperty propertyName entity world =
            match propertyName with // NOTE: string match for speed
            | "Id" -> (World.getEntityId entity world :> obj, typeof<Guid>)
            | "Name" -> (World.getEntityName entity world :> obj, typeof<Name>)
            | "Xtension" -> (World.getEntityXtension entity world :> obj, typeof<Xtension>)
            | "DispatcherNp" -> (World.getEntityDispatcherNp entity world :> obj, typeof<EntityDispatcher>)
            | "CreationTimeStampNp" -> (World.getEntityCreationTimeStampNp entity world :> obj, typeof<int64>)
            | "OptSpecialization" -> (World.getEntityOptSpecialization entity world :> obj, typeof<string option>)
            | "Position" -> (World.getEntityPosition entity world :> obj, typeof<Vector2>)
            | "Size" -> (World.getEntitySize entity world :> obj, typeof<Vector2>)
            | "Rotation" -> (World.getEntityRotation entity world :> obj, typeof<single>)
            | "Depth" -> (World.getEntityDepth entity world :> obj, typeof<single>)
            | "Overflow" -> (World.getEntityOverflow entity world :> obj, typeof<Vector2>)
            | "ViewType" -> (World.getEntityViewType entity world :> obj, typeof<ViewType>)
            | "Visible" -> (World.getEntityVisible entity world :> obj, typeof<bool>)
            | "Omnipresent" -> (World.getEntityOmnipresent entity world :> obj, typeof<bool>)
            | "PublishUpdatesNp" -> (World.getEntityPublishUpdatesNp entity world :> obj, typeof<bool>)
            | "PublishChangesNp" -> (World.getEntityPublishChangesNp entity world :> obj, typeof<bool>)
            | "Persistent" -> (World.getEntityPersistent entity world :> obj, typeof<bool>)
            | "FacetNames" -> (World.getEntityFacetNames entity world :> obj, typeof<string Set>)
            | "FacetsNp" -> (World.getEntityFacetsNp entity world :> obj, typeof<Facet list>)
            | "Transform" -> (World.getEntityTransform entity world :> obj, typeof<Transform>)
            | _ ->
                let property = EntityState.getProperty (World.getEntityState entity world) propertyName
                (property.PropertyValue, property.PropertyType)

        /// Get an entity's property value.
        static member internal getEntityPropertyValue propertyName entity world : 'a =
            let property = World.getEntityProperty propertyName entity world
            fst property :?> 'a

        /// Set an entity's property value.
        static member internal setEntityPropertyValue propertyName (value : 'a) entity world =
            match propertyName with // NOTE: string match for speed
            | "Id" -> failwith "Cannot change entity id."
            | "Name" -> failwith "Cannot change entity name."
            | "DispatcherNp" -> failwith "Cannot change entity dispatcher."
            | "CreationTimeStampNp" -> failwith "Cannot change entity creation time stamp."
            | "OptSpecialization" -> failwith "Cannot change entity specialization."
            | "Position" -> World.setEntityPosition (value :> obj :?> Vector2) entity world
            | "Size" -> World.setEntitySize (value :> obj :?> Vector2) entity world
            | "Rotation" -> World.setEntityRotation (value :> obj :?> single) entity world
            | "Depth" -> World.setEntityDepth (value :> obj :?> single) entity world
            | "Overflow" -> World.setEntityOverflow (value :> obj :?> Vector2) entity world
            | "ViewType" -> World.setEntityViewType (value :> obj :?> ViewType) entity world
            | "Visible" -> World.setEntityVisible (value :> obj :?> bool) entity world
            | "Omnipresent" -> World.setEntityOmnipresent (value :> obj :?> bool) entity world
            | "PublishUpdatesNp" -> failwith "Cannot change entity publish updates."
            | "PublishChangesNp" -> failwith "Cannot change entity publish updates."
            | "Persistent" -> World.setEntityPersistent (value :> obj :?> bool) entity world
            | "FacetNames" -> failwith "Cannot change entity facet names with a property setter."
            | "FacetsNp" -> failwith "Cannot change entity facets with a property setter."
            | "Transform" -> World.setEntityTransform (value :> obj :?> Transform) entity world
            | _ -> World.setEntityState (EntityState.set (World.getEntityState entity world) propertyName value) entity world

        /// Get the maxima bounds of the entity as determined by size, position, rotation, and overflow.
        static member getEntityBoundsMax entity world =
            let entityState = World.getEntityState entity world
            World.getEntityStateBoundsMax entityState

        /// Get an entity's picking priority.
        static member getEntityPickingPriority (participant : Participant) world =
            match participant with
            | :? Entity as entity ->
                let entityState = World.getEntityState entity world
                let dispatcher = entityState.DispatcherNp
                dispatcher.GetPickingPriority (entity, entityState.Depth, world)
            | _ -> failwithumf ()

        /// Get an entity's facet names via reflection.
        static member private getEntityFacetNamesReflectively entityState =
            List.map getTypeName entityState.FacetsNp

        static member private updateEntityPublishChanges entity world =
            let entityChangeEventAddress = entity.ChangeAddress |> atooa
            let publishChanges =
                let subscriptions = Vmap.tryFind entityChangeEventAddress (World.getSubscriptions world)
                match subscriptions with
                | Some [] -> failwithumf () // NOTE: implementation of event system should clean up all empty subscription entries, AFAIK
                | Some (_ :: _) -> true
                | None -> false
            if World.containsEntity entity world
            then World.setEntityPublishChangesNp publishChanges entity world
            else world

        static member private updateEntityPublishUpdates entity world =
            let entityUpdateEventAddress = entity.UpdateAddress |> atooa
            let publishUpdates =
                let subscriptions = Vmap.tryFind entityUpdateEventAddress (World.getSubscriptions world)
                match subscriptions with
                | Some [] -> failwithumf () // NOTE: implementation of event system should clean up all empty subscription entries, AFAIK
                | Some (_ :: _) -> true
                | None -> false
            if World.containsEntity entity world
            then World.setEntityPublishUpdatesNp publishUpdates entity world
            else world

        static member private addEntity mayReplace entityState entity world =

            // add entity only if it is new or is explicitly able to be replaced
            let isNew = not ^ World.containsEntity entity world
            if isNew || mayReplace then

                // get old world for entity tree rebuild
                let oldWorld = world
                
                // adding entity to world
                let world = World.addEntityState entityState entity world
                
                // pulling out screen state
                let screen = entity.EntityAddress |> Address.head |> ntoa<Screen> |> Screen.proxy
                let screenState = World.getScreenState screen world

                // mutate entity tree
                let entityTree =
                    MutantCache.mutateMutant
                        (fun () -> World.rebuildEntityTree screen oldWorld)
                        (fun entityTree ->
                            let entityState = World.getEntityState entity world
                            let entityMaxBounds = World.getEntityStateBoundsMax entityState
                            QuadTree.addElement (entityState.Omnipresent || entityState.ViewType = Absolute) entityMaxBounds entity entityTree
                            entityTree)
                        screenState.EntityTreeNp
                let screenState = { screenState with EntityTreeNp = entityTree }
                let world = World.setScreenState screenState screen world

                // register entity if needed
                if isNew then
                    let dispatcher = World.getEntityDispatcherNp entity world : EntityDispatcher
                    let facets = World.getEntityFacetsNp entity world
                    let world = dispatcher.Register (entity, world)
                    let world = List.fold (fun world (facet : Facet) -> facet.Register (entity, world)) world facets
                    let world = World.updateEntityPublishChanges entity world
                    let world = World.updateEntityPublishUpdates entity world
                    let eventTrace = EventTrace.record "World" "addEntity" EventTrace.empty
                    World.publish () (ftoa<unit> !!"Entity/Add" ->- entity) eventTrace entity world
                else world

            // handle failure
            else failwith ^ "Adding an entity that the world already contains at address '" + scstring entity.EntityAddress + "'."

        /// Destroy an entity in the world immediately. Can be dangerous if existing in-flight publishing depends on
        /// the entity's existence. Consider using World.destroyEntity instead.
        static member destroyEntityImmediate entity world =
            World.removeEntity entity world

        /// Create an entity and add it to the world.
        static member createEntity dispatcherName optSpecialization optName group world =

            // grab overlay dependencies
            let overlayer = World.getOverlayer world
            let overlayRouter = World.getOverlayRouter world

            // find the entity's dispatcher
            let dispatchers = World.getEntityDispatchers world
            let dispatcher = Map.find dispatcherName dispatchers
            
            // compute the default opt overlay name
            let intrinsicOverlayName = dispatcherName
            let defaultOptOverlayName = OverlayRouter.findOptOverlayName intrinsicOverlayName overlayRouter

            // make the bare entity state (with name as id if none is provided)
            let entityState = EntityState.make optSpecialization optName defaultOptOverlayName dispatcher

            // attach the entity state's intrinsic facets and their properties
            let entityState = World.attachIntrinsicFacetsViaNames entityState world

            // apply the entity state's overlay to its facet names
            let entityState =
                match defaultOptOverlayName with
                | Some defaultOverlayName ->

                    // apply overlay to facets
                    let entityState = Overlayer.applyOverlayToFacetNames EntityState.copy intrinsicOverlayName defaultOverlayName entityState overlayer overlayer

                    // synchronize the entity's facets (and attach their properties)
                    match World.trySynchronizeFacetsToNames Set.empty entityState None world with
                    | Right (entityState, _) -> entityState
                    | Left error -> Log.debug error; entityState
                | None -> entityState

            // attach the entity state's dispatcher properties
            let entityState = Reflection.attachProperties EntityState.copy dispatcher entityState

            // apply the entity state's overlay
            let entityState =
                match entityState.OptOverlayName with
                | Some overlayName ->
                    // OPTIMIZATION: apply overlay only when it will change something (EG - when it's not the intrinsic overlay)
                    if intrinsicOverlayName <> overlayName then
                        let facetNames = World.getEntityFacetNamesReflectively entityState
                        Overlayer.applyOverlay EntityState.copy intrinsicOverlayName overlayName facetNames entityState overlayer
                    else entityState
                | None -> entityState

            // add entity's state to world
            let entity = group.GroupAddress -<<- ntoa<Entity> entityState.Name |> Entity.proxy
            let world = World.addEntity false entityState entity world
            (entity, world)

        /// Remove an entity in the world. Can be dangerous if existing in-flight publishing depends on the entity's
        /// existence. Consider using World.destroyEntity instead.
        static member private removeEntity entity world =
            
            // ensure entity exists in the world
            if World.containsEntity entity world then
                
                // publish event and unregister entity
                let eventTrace = EventTrace.record "World" "removeEntity" EventTrace.empty
                let world = World.publish () (ftoa<unit> !!"Entity/Removing" ->- entity) eventTrace entity world
                let dispatcher = World.getEntityDispatcherNp entity world : EntityDispatcher
                let facets = World.getEntityFacetsNp entity world
                let world = dispatcher.Unregister (entity, world)
                let world = List.fold (fun world (facet : Facet) -> facet.Unregister (entity, world)) world facets

                // get old world for entity tree rebuild
                let oldWorld = world
                
                // pulling out screen state
                let screen = entity.EntityAddress |> Address.head |> ntoa<Screen> |> Screen.proxy
                let screenState = World.getScreenState screen world

                // mutate entity tree
                let entityTree =
                    MutantCache.mutateMutant
                        (fun () -> World.rebuildEntityTree screen oldWorld)
                        (fun entityTree ->
                            let entityState = World.getEntityState entity oldWorld
                            let entityMaxBounds = World.getEntityStateBoundsMax entityState
                            QuadTree.removeElement (entityState.Omnipresent || entityState.ViewType = Absolute) entityMaxBounds entity entityTree
                            entityTree)
                        screenState.EntityTreeNp
                let screenState = { screenState with EntityTreeNp = entityTree }
                let world = World.setScreenState screenState screen world

                // remove the entity from the world
                World.removeEntityState entity world

            // pass
            else world

        /// Read an entity from an entity descriptor.
        static member readEntity entityDescriptor optName group world =

            // grab overlay dependencies
            let overlayer = World.getOverlayer world
            let overlayRouter = World.getOverlayRouter world

            // create the dispatcher
            let dispatcherName = entityDescriptor.EntityDispatcher
            let dispatchers = World.getEntityDispatchers world
            let (dispatcherName, dispatcher) =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> (dispatcherName, dispatcher)
                | None ->
                    Log.info ^ "Could not locate dispatcher '" + dispatcherName + "'."
                    let dispatcherName = typeof<EntityDispatcher>.Name
                    let dispatcher = Map.find dispatcherName dispatchers
                    (dispatcherName, dispatcher)

            // compute the default overlay names
            let intrinsicOverlayName = dispatcherName
            let defaultOptOverlayName = OverlayRouter.findOptOverlayName intrinsicOverlayName overlayRouter

            // make the bare entity state with name as id
            let entityState = EntityState.make None None defaultOptOverlayName dispatcher

            // attach the entity state's intrinsic facets and their properties
            let entityState = World.attachIntrinsicFacetsViaNames entityState world

            // read the entity state's overlay and apply it to its facet names if applicable
            let entityState = Reflection.tryReadOptOverlayNameToTarget EntityState.copy entityDescriptor.EntityProperties entityState
            let entityState =
                match (defaultOptOverlayName, entityState.OptOverlayName) with
                | (Some defaultOverlayName, Some overlayName) -> Overlayer.applyOverlayToFacetNames EntityState.copy defaultOverlayName overlayName entityState overlayer overlayer
                | (_, _) -> entityState

            // read the entity state's facet names
            let entityState = Reflection.readFacetNamesToTarget EntityState.copy entityDescriptor.EntityProperties entityState

            // attach the entity state's dispatcher properties
            let entityState = Reflection.attachProperties EntityState.copy dispatcher entityState
            
            // synchronize the entity state's facets (and attach their properties)
            let entityState =
                match World.trySynchronizeFacetsToNames Set.empty entityState None world with
                | Right (entityState, _) -> entityState
                | Left error -> Log.debug error; entityState

            // attempt to apply the entity state's overlay
            let entityState =
                match entityState.OptOverlayName with
                | Some overlayName ->
                    // OPTIMIZATION: applying overlay only when it will change something (EG - when it's not the default overlay)
                    if intrinsicOverlayName <> overlayName then
                        let facetNames = World.getEntityFacetNamesReflectively entityState
                        Overlayer.applyOverlay EntityState.copy intrinsicOverlayName overlayName facetNames entityState overlayer
                    else entityState
                | None -> entityState

            // read the entity state's values
            let entityState = Reflection.readPropertiesToTarget EntityState.copy entityDescriptor.EntityProperties entityState

            // apply the name if one is provided
            let entityState =
                match optName with
                | Some name -> { entityState with Name = name }
                | None -> entityState

            // add entity state to the world
            let entity = group.GroupAddress -<<- ntoa<Entity> entityState.Name |> Entity.proxy
            let world = World.addEntity true entityState entity world
            (entity, world)

        /// Write an entity to an entity descriptor.
        static member writeEntity (entity : Entity) entityDescriptor world =
            let entityState = World.getEntityState entity world
            let entityDispatcherName = getTypeName entityState.DispatcherNp
            let entityDescriptor = { entityDescriptor with EntityDispatcher = entityDispatcherName }
            let shouldWriteProperty = fun propertyName propertyType (propertyValue : obj) ->
                if propertyName = "OptOverlayName" && propertyType = typeof<string option> then
                    let overlayRouter = World.getOverlayRouter world
                    let defaultOptOverlayName = OverlayRouter.findOptOverlayName entityDispatcherName overlayRouter
                    defaultOptOverlayName <> (propertyValue :?> string option)
                else
                    let overlayer = World.getOverlayer world
                    let facetNames = World.getEntityFacetNamesReflectively entityState
                    Overlayer.shouldPropertySerialize5 facetNames propertyName propertyType entityState overlayer
            let getEntityProperties = Reflection.writePropertiesFromTarget shouldWriteProperty entityDescriptor.EntityProperties entityState
            { entityDescriptor with EntityProperties = getEntityProperties }

        /// Reassign an entity's identity and / or group. Note that since this destroys the reassigned entity
        /// immediately, you should not call this inside an event handler that involves the reassigned entity itself.
        static member reassignEntity entity optName group world =
            let entityState = World.getEntityState entity world
            let world = World.removeEntity entity world
            let id = makeGuid ()
            let name = match optName with Some name -> name | None -> Name.make ^ scstring id
            let entityState = { entityState with Id = id; Name = name }
            let transmutedEntity = group.GroupAddress -<<- ntoa<Entity> name |> Entity.proxy
            let world = World.addEntity false entityState transmutedEntity world
            (transmutedEntity, world)

        static member internal updateEntityPublishingFlags eventAddress world =
            let eventNames = Address.getNames eventAddress
            match eventNames with
            | head :: neck :: tail when Name.getNameStr head = "Entity" && Name.getNameStr neck = "Change" ->
                let publishChanges =
                    match Vmap.tryFind eventAddress (EventWorld.getSubscriptions world) with
                    | Some [] -> failwithumf () // NOTE: implementation of event system should clean up all empty subscription entries, AFAIK
                    | Some (_ :: _) -> true
                    | None -> false
                let entity = Entity.proxy ^ ltoa<Entity> tail
                let world = if World.containsEntity entity world then World.setEntityPublishChangesNp publishChanges entity world else world
                world
            | head :: tail when Name.getNameStr head = "Update" ->
                let publishUpdates =
                    match Vmap.tryFind eventAddress (EventWorld.getSubscriptions world) with
                    | Some [] -> failwithumf () // NOTE: implementation of event system should clean up all empty subscription entries, AFAIK
                    | Some (_ :: _) -> true
                    | None -> false
                let entity = Entity.proxy ^ ltoa<Entity> tail
                let world = if World.containsEntity entity world then World.setEntityPublishUpdatesNp publishUpdates entity world else world
                world
            | _ -> world

        /// Try to set an entity's optional overlay name.
        static member trySetEntityOptOverlayName optOverlayName entity world =
            let oldEntityState = World.getEntityState entity world
            let oldOptOverlayName = oldEntityState.OptOverlayName
            let entityState = { oldEntityState with OptOverlayName = optOverlayName }
            match (oldOptOverlayName, optOverlayName) with
            | (Some oldOverlayName, Some overlayName) ->
                let overlayer = World.getOverlayer world
                let (entityState, world) =
                    let entityState = Overlayer.applyOverlayToFacetNames EntityState.copy oldOverlayName overlayName entityState overlayer overlayer
                    match World.trySynchronizeFacetsToNames entityState.FacetNames entityState (Some entity) world with
                    | Right (entityState, world) -> (entityState, world)
                    | Left error -> Log.debug error; (entityState, world)
                let facetNames = World.getEntityFacetNamesReflectively entityState
                let entityState = Overlayer.applyOverlay EntityState.copy oldOverlayName overlayName facetNames entityState overlayer
                let oldWorld = world
                let world = World.setEntityStateWithoutEvent entityState entity world
                let world = World.updateEntityInEntityTree entity oldWorld world
                let world = World.publishEntityChange entityState entity oldWorld world
                Right world
            | (_, _) -> let _ = World.choose world in Left "Could not set the entity's overlay name."

        /// Try to set the entity's facet names.
        static member trySetEntityFacetNames facetNames entity world =
            let entityState = World.getEntityState entity world
            match World.trySetFacetNames facetNames entityState (Some entity) world with
            | Right (entityState, world) ->
                let oldWorld = world
                let world = World.setEntityStateWithoutEvent entityState entity world
                let world = World.updateEntityInEntityTree entity oldWorld world
                let world = World.publishEntityChange entityState entity oldWorld world
                Right world
            | Left error -> Left error

        static member viewEntityMemberProperties entity world =
            let state = World.getEntityState entity world
            World.viewMemberProperties state

        static member viewEntityXProperties entity world =
            let state = World.getEntityState entity world
            World.viewXProperties state

        static member viewEntity entity world =
            let state = World.getEntityState entity world
            World.view state

        static member applyEntityOverlay oldOverlayer overlayer world entity =
            let entityState = World.getEntityState entity world
            match entityState.OptOverlayName with
            | Some overlayName ->
                let oldFacetNames = entityState.FacetNames
                let entityState = Overlayer.applyOverlayToFacetNames EntityState.copy overlayName overlayName entityState oldOverlayer overlayer
                match World.trySynchronizeFacetsToNames oldFacetNames entityState (Some entity) world with
                | Right (entityState, world) ->
                    let oldWorld = world
                    let facetNames = World.getEntityFacetNamesReflectively entityState
                    let entityState = Overlayer.applyOverlay6 EntityState.copy overlayName overlayName facetNames entityState oldOverlayer overlayer
                    let world = World.setEntityStateWithoutEvent entityState entity world
                    World.updateEntityInEntityTree entity oldWorld world
                | Left error -> Log.info ^ "There was an issue in applying a reloaded overlay: " + error; world
            | None -> world

        (* GroupState *)

        static member private groupStateSetter groupState group world =
#if DEBUG
            if not ^ Vmap.containsKey group.GroupAddress world.GroupStates then
                failwith ^ "Cannot set the state of a non-existent group '" + scstring group.GroupAddress + "'"
#endif
            let groupStates = Vmap.add group.GroupAddress groupState world.GroupStates
            World.choose { world with GroupStates = groupStates }

        static member private groupStateAdder groupState group world =
            let screenDirectory =
                match Address.getNames group.GroupAddress with
                | [screenName; groupName] ->
                    match Vmap.tryFind screenName world.ScreenDirectory with
                    | Some (screenAddress, groupDirectory) ->
                        match Vmap.tryFind groupName groupDirectory with
                        | Some (groupAddress, entityDirectory) ->
                            let groupDirectory = Vmap.add groupName (groupAddress, entityDirectory) groupDirectory
                            Vmap.add screenName (screenAddress, groupDirectory) world.ScreenDirectory
                        | None ->
                            let entityDirectory = Vmap.makeEmpty ()
                            let groupDirectory = Vmap.add groupName (group.GroupAddress, entityDirectory) groupDirectory
                            Vmap.add screenName (screenAddress, groupDirectory) world.ScreenDirectory
                    | None -> failwith ^ "Cannot add group '" + scstring group.GroupAddress + "' to non-existent screen."
                | _ -> failwith ^ "Invalid group address '" + scstring group.GroupAddress + "'."
            let groupStates = Vmap.add group.GroupAddress groupState world.GroupStates
            World.choose { world with ScreenDirectory = screenDirectory; GroupStates = groupStates }

        static member private groupStateRemover group world =
            let screenDirectory =
                match Address.getNames group.GroupAddress with
                | [screenName; groupName] ->
                    match Vmap.tryFind screenName world.ScreenDirectory with
                    | Some (screenAddress, groupDirectory) ->
                        let groupDirectory = Vmap.remove groupName groupDirectory
                        Vmap.add screenName (screenAddress, groupDirectory) world.ScreenDirectory
                    | None -> failwith ^ "Cannot remove group '" + scstring group.GroupAddress + "' from non-existent screen."
                | _ -> failwith ^ "Invalid group address '" + scstring group.GroupAddress + "'."
            let groupStates = Vmap.remove group.GroupAddress world.GroupStates
            World.choose { world with ScreenDirectory = screenDirectory; GroupStates = groupStates }

        static member inline internal getOptGroupState group world =
            Vmap.tryFind group.GroupAddress world.GroupStates

        static member internal getGroupState group world =
            match World.getOptGroupState group world with
            | Some groupState -> groupState
            | None -> failwith ^ "Could not find group with address '" + scstring group.GroupAddress + "'."

        static member internal addGroupState groupState group world =
            World.groupStateAdder groupState group world

        static member internal removeGroupState group world =
            World.groupStateRemover group world

        static member inline internal setGroupStateWithoutEvent groupState group world =
            World.groupStateSetter groupState group world

        static member internal setGroupState groupState group world =
            let _ = world
            let world = World.groupStateSetter groupState group world
            let _ = EventTrace.record "World" "setGroupState" EventTrace.empty
            world

        static member internal updateGroupState updater group world =
            let groupState = World.getGroupState group world
            let groupState = updater groupState
            World.setGroupState groupState group world

        (* ScreenState *)

        static member private screenStateSetter screenState screen world =
#if DEBUG
            if not ^ Vmap.containsKey screen.ScreenAddress world.ScreenStates then
                failwith ^ "Cannot set the state of a non-existent screen '" + scstring screen.ScreenAddress + "'"
#endif
            let screenStates = Vmap.add screen.ScreenAddress screenState world.ScreenStates
            World.choose { world with ScreenStates = screenStates }

        static member private screenStateAdder screenState screen world =
            let screenDirectory =
                match Address.getNames screen.ScreenAddress with
                | [screenName] ->
                    match Vmap.tryFind screenName world.ScreenDirectory with
                    | Some (_, groupDirectory) ->
                        // NOTE: this is logically a redundant operation...
                        Vmap.add screenName (screen.ScreenAddress, groupDirectory) world.ScreenDirectory
                    | None ->
                        let groupDirectory = Vmap.makeEmpty ()
                        Vmap.add screenName (screen.ScreenAddress, groupDirectory) world.ScreenDirectory
                | _ -> failwith ^ "Invalid screen address '" + scstring screen.ScreenAddress + "'."
            let screenStates = Vmap.add screen.ScreenAddress screenState world.ScreenStates
            World.choose { world with ScreenDirectory = screenDirectory; ScreenStates = screenStates }

        static member private screenStateRemover screen world =
            let screenDirectory =
                match Address.getNames screen.ScreenAddress with
                | [screenName] -> Vmap.remove screenName world.ScreenDirectory
                | _ -> failwith ^ "Invalid screen address '" + scstring screen.ScreenAddress + "'."
            let screenStates = Vmap.remove screen.ScreenAddress world.ScreenStates
            World.choose { world with ScreenDirectory = screenDirectory; ScreenStates = screenStates }

        static member inline internal getOptScreenState screen world =
            Vmap.tryFind screen.ScreenAddress world.ScreenStates

        static member internal getScreenState screen world =
            match World.getOptScreenState screen world with
            | Some screenState -> screenState
            | None -> failwith ^ "Could not find screen with address '" + scstring screen.ScreenAddress + "'."

        static member internal addScreenState screenState screen world =
            World.screenStateAdder screenState screen world

        static member internal removeScreenState screen world =
            World.screenStateRemover screen world

        static member inline internal setScreenStateWithoutEvent screenState screen world =
            World.screenStateSetter screenState screen world

        static member internal setScreenState screenState screen world =
            let _ = world
            let world = World.screenStateSetter screenState screen world
            let _ = EventTrace.record "World" "setScreenState" EventTrace.empty
            world

        static member internal updateScreenState updater screen world =
            let screenState = World.getScreenState screen world
            let screenState = updater screenState
            World.setScreenState screenState screen world

        static member internal updateEntityInEntityTree entity oldWorld world =

            // OPTIMIZATION: attempt to avoid constructing a screen address on each call to decrease address hashing
            // OPTIMIZATION: assumes a valid entity address with List.head on its names
            let screen =
                match (World.getGameState world).OptSelectedScreen with
                | Some screen when screen.ScreenName = List.head ^ Address.getNames entity.EntityAddress -> screen
                | Some _ | None -> entity.EntityAddress |> Address.getNames |> List.head |> ntoa<Screen> |> Screen.proxy

            // proceed with updating entity in entity tree
            let screenState = World.getScreenState screen world
            let entityTree =
                MutantCache.mutateMutant
                    (fun () -> World.rebuildEntityTree screen oldWorld)
                    (fun entityTree ->
                        let oldEntityState = World.getEntityState entity oldWorld
                        let oldEntityBoundsMax = World.getEntityStateBoundsMax oldEntityState
                        let entityState = World.getEntityState entity world
                        let entityBoundsMax = World.getEntityStateBoundsMax entityState
                        QuadTree.updateElement
                            (oldEntityState.Omnipresent || oldEntityState.ViewType = Absolute) oldEntityBoundsMax
                            (entityState.Omnipresent || entityState.ViewType = Absolute) entityBoundsMax
                            entity entityTree
                        entityTree)
                    screenState.EntityTreeNp
            let screenState = { screenState with EntityTreeNp = entityTree }
            World.setScreenStateWithoutEvent screenState screen world

        (* GameState *)

        static member internal getGameState world =
            world.GameState

        static member internal setGameState gameState world =
            let _ = world
            let world = World.choose { world with GameState = gameState }
            let _ = EventTrace.record "World" "setGameState" EventTrace.empty
            world

        static member internal updateGameState updater world =
            let gameState = World.getGameState world
            let gameState = updater gameState
            World.setGameState gameState world

        /// Get the current destination screen if a screen transition is currently underway.
        static member getOptScreenTransitionDestination world =
            (World.getGameState world).OptScreenTransitionDestination

        /// Set the current destination screen on the precondition that no screen transition is currently underway.
        static member internal setOptScreenTransitionDestination destination world =
            World.updateGameState
                (fun gameState -> { gameState with OptScreenTransitionDestination = destination })
                world
                
        // Make the world.
        static member internal make eventSystem dispatchers subsystems ambientState gameState =
            let world =
                { EventSystem = eventSystem
                  Dispatchers = dispatchers
                  Subsystems = subsystems
                  OptEntityCache = Unchecked.defaultof<KeyedCache<Entity Address * World, EntityState option>>
                  ScreenDirectory = Vmap.makeEmpty ()
                  AmbientState = ambientState
                  GameState = gameState
                  ScreenStates = Vmap.makeEmpty ()
                  GroupStates = Vmap.makeEmpty ()
                  EntityStates = Vmap.makeEmpty () }
            World.choose world

        (* Clipboard *)
        
        /// Copy an entity to the clipboard.
        static member copyToClipboard entity world =
            let entityState = World.getEntityState entity world
            RefClipboard := Some (entityState :> obj)

        /// Cut an entity to the clipboard.
        static member cutToClipboard entity world =
            World.copyToClipboard entity world
            World.destroyEntityImmediate entity world

        /// Paste an entity from the clipboard.
        static member pasteFromClipboard atMouse rightClickPosition positionSnap rotationSnap group world =
            match !RefClipboard with
            | Some entityStateObj ->
                let entityState = entityStateObj :?> EntityState
                let id = makeGuid ()
                let name = Name.make ^ scstring id
                let entityState = { entityState with Id = id; Name = name }
                let camera = World.getCamera world
                let position =
                    if atMouse
                    then Camera.mouseToWorld entityState.ViewType rightClickPosition camera
                    else Camera.mouseToWorld entityState.ViewType (camera.EyeSize * 0.5f) camera
                let transform = { EntityState.getTransform entityState with Position = position }
                let transform = Math.snapTransform positionSnap rotationSnap transform
                let entityState = EntityState.setTransform transform entityState
                let entity = group.GroupAddress -<<- ntoa<Entity> name |> Entity.proxy
                let world = World.addEntity false entityState entity world
                (Some entity, world)
            | None -> (None, world)

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
        /// types. For example, a returned router of (typeof<ButtonDispatcher>.Name, Some "CustomButtonOverlay")
        /// will cause all buttons to use the overlay with the name "CustomButtonOverlay" rather
        /// than the default "ButtonDispatcher" overlay.
        abstract MakeOverlayRoutes : unit -> (string * string option) list
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
/// OPTIMIZATION: Includes pre-constructed entity change and update event address to avoid
/// reconstructing new ones for each entity every frame.
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