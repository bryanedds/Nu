// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open OpenTK
open TiledSharp
open Prime
open Nu

/// Allows for easier watching of simulant fields in a debugging context.
type Watchable (Properties, XFields) =
    member this.F () = ignore (Properties, XFields)

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

/// The default dispatcher for games.
type GameDispatcher () =

    static member FieldDefinitions =
        [Define? PublishChanges true]

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

    static member FieldDefinitions =
        [Define? OptSpecialization (None : string option)
         Define? PublishChanges true
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

    static member FieldDefinitions =
        [Define? OptSpecialization (None : string option)
         Define? PublishChanges true
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

    static member FieldDefinitions =
        [Define? OptSpecialization (None : string option)
         Define? Position Vector2.Zero
         Define? Size Constants.Engine.DefaultEntitySize
         Define? Rotation 0.0f
         Define? Depth 0.0f
         Define? Overflow Vector2.Zero
         Define? ViewType Relative
         Define? Visible true
         Define? Omnipresent false
         Define? PublishUpdates true
         Define? PublishChanges false
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

/// Hosts the ongoing state of a game. The end-user of this engine should never touch this
/// type, and it's public _only_ to make [<CLIMutable>] work.
and [<CLIMutable; NoEquality; NoComparison>] GameState =
    { Id : Guid
      OptSelectedScreen : Screen option
      OptScreenTransitionDestination : Screen option
      PublishChanges : bool
      CreationTimeStampNp : int64
      DispatcherNp : GameDispatcher
      Xtension : Xtension }

    /// Make a game state value.
    static member make dispatcher =
        { Id = makeGuid ()
          OptSelectedScreen = None
          OptScreenTransitionDestination = None
          PublishChanges = true
          CreationTimeStampNp = Core.getTimeStamp ()
          DispatcherNp = dispatcher
          Xtension = Xtension.safe }

/// Hosts the ongoing state of a screen. The end-user of this engine should never touch this
/// type, and it's public _only_ to make [<CLIMutable>] work.
and [<CLIMutable; NoEquality; NoComparison>] ScreenState =
    { Id : Guid
      Name : Name
      OptSpecialization : string option
      TransitionStateNp : TransitionState
      TransitionTicksNp : int64
      Incoming : Transition
      Outgoing : Transition
      PublishChanges : bool
      Persistent : bool
      CreationTimeStampNp : int64
      DispatcherNp : ScreenDispatcher
      EntityTreeNp : Entity QuadTree MutantCache
      Xtension : Xtension }

    /// Make a screen state value.
    static member make optSpecialization optName dispatcher =
        let (id, name) = Reflection.deriveIdAndName optName
        let screenState =
            { Id = id
              Name = name
              OptSpecialization = optSpecialization 
              TransitionStateNp = IdlingState
              TransitionTicksNp = 0L // TODO: roll this field into Incoming/OutcomingState values
              Incoming = Transition.make Incoming
              Outgoing = Transition.make Outgoing
              PublishChanges = true
              Persistent = true
              CreationTimeStampNp = Core.getTimeStamp ()
              DispatcherNp = dispatcher
              EntityTreeNp = Unchecked.defaultof<Entity QuadTree MutantCache>
              Xtension = Xtension.safe }
        let quadTree = QuadTree.make Constants.Engine.EntityTreeDepth Constants.Engine.EntityTreeBounds
        { screenState with EntityTreeNp = MutantCache.make Operators.id quadTree }

/// Hosts the ongoing state of a group. The end-user of this engine should never touch this
/// type, and it's public _only_ to make [<CLIMutable>] work.
and [<CLIMutable; NoEquality; NoComparison>] GroupState =
    { Id : Guid
      Name : Name
      OptSpecialization : string option
      PublishChanges : bool
      Persistent : bool
      CreationTimeStampNp : int64
      DispatcherNp : GroupDispatcher
      Xtension : Xtension }

    /// Make a group state value.
    static member make optSpecialization optName dispatcher =
        let (id, name) = Reflection.deriveIdAndName optName
        { GroupState.Id = id
          Name = name
          OptSpecialization = optSpecialization
          PublishChanges = true
          Persistent = true
          CreationTimeStampNp = Core.getTimeStamp ()
          DispatcherNp = dispatcher
          Xtension = Xtension.safe }

/// Hosts the ongoing state of an entity. The end-user of this engine should never touch this
/// type, and it's public _only_ to make [<CLIMutable>] work.
and [<CLIMutable; NoEquality; NoComparison>] EntityState =
    { Id : Guid
      Name : Name
      OptSpecialization : string option
      Position : Vector2 // NOTE: will become a Vector3 if Nu gets 3d capabilities
      Size : Vector2 // NOTE: will become a Vector3 if Nu gets 3d capabilities
      Rotation : single // NOTE: will become a Vector3 if Nu gets 3d capabilities
      Depth : single // NOTE: will become part of position if Nu gets 3d capabilities
      Overflow : Vector2
      ViewType : ViewType
      Visible : bool
      Omnipresent : bool
      PublishUpdates : bool
      PublishChanges : bool
      Persistent : bool
      CreationTimeStampNp : int64 // just needed for ordering writes to reduce diff volumes
      DispatcherNp : EntityDispatcher
      FacetNames : string Set
      FacetsNp : Facet list
      OptOverlayName : string option
      Xtension : Xtension }

    /// Make an entity state value.
    static member make optSpecialization optName optOverlayName dispatcher =
        let (id, name) = Reflection.deriveIdAndName optName
        { Id = id
          Name = name
          OptSpecialization = optSpecialization
          Position = Vector2.Zero
          Size = Constants.Engine.DefaultEntitySize
          Rotation = 0.0f
          Depth = 0.0f
          Overflow = Vector2.Zero
          ViewType = Relative
          Visible = true
          Omnipresent = false
          PublishUpdates = true
          PublishChanges = false
          Persistent = true
          CreationTimeStampNp = Core.getTimeStamp ()
          DispatcherNp = dispatcher
          FacetNames = Set.empty
          FacetsNp = []
          OptOverlayName = optOverlayName
          Xtension = Xtension.safe }

/// The game type that hosts the various screens used to navigate through a game.
and [<StructuralEquality; NoComparison>] Game =
    { GameAddress : Game Address }

    interface Simulant with
        member this.ParticipantAddress = atoa<Game, Participant> this.GameAddress
        member this.SimulantAddress = atoa<Game, Simulant> this.GameAddress
        member this.GetPublishingPriority _ _ = Constants.Engine.GamePublishingPriority
        end

    /// Get the full name of a game proxy.
    member this.GameFullName = Address.getFullName this.GameAddress

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

    /// Get the full name of a screen proxy.
    member this.ScreenFullName = Address.getFullName this.ScreenAddress

    /// Get the name of a screen proxy.
    member this.ScreenName = Address.getName this.ScreenAddress

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

    /// Get the full name of a group proxy.
    member this.GroupFullName = Address.getFullName this.GroupAddress

    /// Get the name of a group proxy.
    member this.GroupName = Address.getName this.GroupAddress

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
/// OPTIMIZATION: Includes pre-constructed entity update event address to avoid reconstructing one
/// for each entity every frame.
and [<StructuralEquality; NoComparison>] Entity =
    { EntityAddress : Entity Address
      UpdateAddress : unit Address }

    interface Simulant with
        member this.ParticipantAddress = atoa<Entity, Participant> this.EntityAddress
        member this.SimulantAddress = atoa<Entity, Simulant> this.EntityAddress
        member this.GetPublishingPriority getEntityPublishingPriority world = getEntityPublishingPriority this world
        end

    /// Get the name of an entity proxy.
    member this.EntityFullName = Address.getFullName this.EntityAddress

    /// Get the name of an entity proxy.
    member this.EntityName = Address.getName this.EntityAddress

    /// Create an Entity proxy from an address.
    static member proxy address =
        { EntityAddress = address
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
      Facets : Map<string, Facet> }

/// The world, in a functional programming sense. Hosts the game object, the dependencies needed
/// to implement a game, messages to by consumed by the various engine sub-systems, and general
/// configuration data.
///
/// For efficiency, this type is kept under 64 bytes on 32-bit machines as to not exceed the size
/// of a typical cache line.
and [<ReferenceEquality>] World =
    private
        { Subsystems : World Subsystems
          Dispatchers : Dispatchers
          EventSystem : World EventSystem
          OptEntityCache : KeyedCache<Entity Address * World, EntityState option>
          AmbientState : World AmbientState
          GameState : GameState
          ScreenStates : Vmap<Screen Address, ScreenState>
          GroupStates : Vmap<Group Address, GroupState>
          EntityStates : Vmap<Entity Address, EntityState>
          ScreenDirectory : Vmap<Name, Screen Address * Vmap<Name, Group Address * Vmap<Name, Entity Address>>> }

    interface World Eventable with
        member this.GetLiveness () = AmbientState.getLiveness this.AmbientState
        member this.GetEventSystem () = this.EventSystem
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
            | [] -> Eventable.publishEvent<'a, 'p, Game, World> participant publisher eventData eventAddress eventTrace subscription world
            | [_] -> Eventable.publishEvent<'a, 'p, Screen, World> participant publisher eventData eventAddress eventTrace subscription world
            | [_; _] -> Eventable.publishEvent<'a, 'p, Group, World> participant publisher eventData eventAddress eventTrace subscription world
            | [_; _; _] -> Eventable.publishEvent<'a, 'p, Entity, World> participant publisher eventData eventAddress eventTrace subscription world
            | _ -> failwithumf ()