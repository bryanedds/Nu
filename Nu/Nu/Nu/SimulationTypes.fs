// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open FSharpx.Collections
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
type [<CLIMutable; StructuralEquality; NoComparison>] TransitionDescriptor =
    { TransitionType : TransitionType
      TransitionLifetime : int64
      OptDissolveImage : AssetTag option }

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

/// Represents an untyped message to a subsystem.
type SubsystemMessage = obj

/// Represents an untyped result of a subsystem.
type SubsystemResult = obj

/// The type of subsystem. Dictates where subsystem's processing happens in the game loop.
type SubsystemType =
    | UpdateType
    | RenderType
    | AudioType

/// Describes whether an event has been resolved or should cascade to down-stream handlers.
type EventHandling =
    | Resolve
    | Cascade

/// The data for a mouse move event.
type [<StructuralEquality; NoComparison>] MouseMoveData =
    { Position : Vector2 }

/// The data for a mouse button event.
type [<StructuralEquality; NoComparison>] MouseButtonData =
    { Position : Vector2
      Button : MouseButton
      Down : bool }

/// The data for a keyboard key event.
type [<StructuralEquality; NoComparison>] KeyboardKeyData =
    { ScanCode : int
      Repeated : bool
      Down : bool }

/// The data for a collision event.
type [<StructuralEquality; NoComparison>] CollisionData =
    { Normal : Vector2
      Speed : single
      Collidee : Entity }

/// The data for a world state change event.
and [<StructuralEquality; NoComparison>] WorldStateChangeData =
    { OldWorld : World }

/// The data for a simulant change event.
and [<StructuralEquality; NoComparison>] SimulantChangeData<'s when 's :> Simulant> =
    { Simulant : 's
      OldWorld : World }

/// An event used by the game engine's purely-functional event system.
and [<ReferenceEquality>] Event<'a, 's when 's :> Simulant> =
    { Subscriber : 's
      Publisher : Simulant // TODO: consider making this a list so that Observer can provide all useful addresses
      EventAddress : 'a Address
      Data : 'a }

/// Describes an event subscription.
and internal Subscription<'a, 's when 's :> Simulant> =
    Event<'a, 's> -> World -> EventHandling * World

/// Describes an event subscription that can be boxed / unboxed.
and internal BoxableSubscription =
    obj -> World -> EventHandling * World

/// An entry in the subscription map.
and internal SubscriptionEntry =
    Guid * Simulant * obj

/// A map of event subscriptions.
and internal SubscriptionEntries =
    Map<obj Address, SubscriptionEntry rQueue>

/// Abstracts over a subscription sorting procedure.
and internal SubscriptionSorter =
    SubscriptionEntry rQueue -> World -> SubscriptionEntry rQueue

/// A map of subscription keys to unsubscription data.
and internal UnsubscriptionEntries =
    Map<Guid, obj Address * Simulant>

/// A tasklet to be completed at the given time, with time being accounted for by the world
/// state's TickTime value.
/// TODO: Make this an abstract data type.
and [<ReferenceEquality>] Tasklet =
    // private
        { ScheduledTime : int64
          Operation : World -> World }

/// Represents a subsystem by which additional engine-level subsystems such as AI, optimized
/// special FX, and the like can be added.
and Subsystem =
    interface
        /// The type of subsystem. Dictates where its processing happens in the game loop.
        abstract SubsystemType : SubsystemType
        /// The ordering by which the subsystem will be processed relative to other subsystems of the same type.
        abstract SubsystemOrder : single
        /// Clear the messages queued by subsystem.
        abstract ClearMessages : unit -> Subsystem
        /// Enqueue a message for the subsystem.
        abstract EnqueueMessage : SubsystemMessage -> Subsystem
        /// Processed the queued messages with the subsystem.
        abstract ProcessMessages : World -> SubsystemResult * Subsystem * World
        /// Apply the result of the message processing to the world.
        abstract ApplyResult : SubsystemResult * World -> World
        /// Clean up any resources used by the subsystem.
        abstract CleanUp : World -> Subsystem * World
        end

/// The default dispatcher for games.
and GameDispatcher () =

    static member FieldDefinitions =
        [define? PublishChanges true]

    /// Register a game when adding it to the world. Note that there is not corresponding
    /// Unregister method due to the inability to remove a game from the world.
    abstract Register : Game * World -> World
    default dispatcher.Register (_, world) = world

/// The default dispatcher for screens.
and ScreenDispatcher () =

    static member FieldDefinitions =
        [define? PublishChanges true
         define? Persistent true]

    /// Register a screen when adding it to the world.
    abstract Register : Screen * World -> World
    default dispatcher.Register (_, world) = world

    /// Unregister a screen when removing it from the world.
    abstract Unregister : Screen * World -> World
    default dispatcher.Unregister (_, world) = world

/// The default dispatcher for groups.
and GroupDispatcher () =

    static member FieldDefinitions =
        [define? PublishChanges true
         define? Persistent true]

    /// Register a group when adding it to a screen.
    abstract Register : Group * World -> World
    default dispatcher.Register (_, world) = world

    /// Unregister a group when removing it from a screen.
    abstract Unregister : Group * World -> World
    default dispatcher.Unregister (_, world) = world

/// The default dispatcher for entities.
and EntityDispatcher () =

    static member FieldDefinitions =
        [define? Position Vector2.Zero
         define? Depth 0.0f
         define? Size Constants.Engine.DefaultEntitySize
         define? Rotation 0.0f
         define? Visible true
         define? ViewType Relative
         define? Omnipresent false
         define? PublishChanges false
         define? Persistent true]

    /// Register an entity when adding it to a group.
    abstract Register : Entity * World -> World
    default dispatcher.Register (_, world) = world

    /// Unregister an entity when removing it from a group.
    abstract Unregister : Entity * World -> World
    default dispatcher.Unregister (_, world) = world

    /// Propagate an entity's physics properties from the physics subsystem.
    abstract PropagatePhysics : Entity * World -> World
    default dispatcher.PropagatePhysics (_, world) = world

    /// Get the render descriptors needed to render an entity.
    abstract GetRenderDescriptors : Entity * World -> RenderDescriptor list
    default dispatcher.GetRenderDescriptors (_, _) = []

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

    /// Participate in getting the render descriptors needed to render an entity.
    abstract GetRenderDescriptors : Entity * World -> RenderDescriptor list
    default facet.GetRenderDescriptors (_, _) = []

    /// Participate in getting the priority with which an entity is picked in the editor.
    abstract GetQuickSize : Entity * World -> Vector2
    default facet.GetQuickSize (_, _) = Constants.Engine.DefaultEntitySize

/// A marker interface for the simulation state types (GameState, ScreenState, GroupState,
/// and EntityState).
and internal SimulantState = interface end

/// Hosts the ongoing state of a game. The end-user of this engine should never touch this
/// type, and it's public _only_ to make [<CliMutable>] work.
and [<CLIMutable; NoEquality; NoComparison>] GameState =
    { Id : Guid
      OptSelectedScreen : Screen option
      PublishChanges : bool
      CreationTimeStampNp : int64
      DispatcherNp : GameDispatcher
      Xtension : Xtension }
    interface SimulantState

/// Hosts the ongoing state of a screen. The end-user of this engine should never touch this
/// type, and it's public _only_ to make [<CliMutable>] work.
and [<CLIMutable; NoEquality; NoComparison>] ScreenState =
    { Id : Guid
      Name : string
      OptSpecialization : string option
      TransitionStateNp : TransitionState
      TransitionTicksNp : int64
      Incoming : TransitionDescriptor
      Outgoing : TransitionDescriptor
      PublishChanges : bool
      Persistent : bool
      CreationTimeStampNp : int64
      DispatcherNp : ScreenDispatcher
      EntityTreeNp : Entity QuadTree MutantCache
      Xtension : Xtension }
    interface SimulantState

/// Hosts the ongoing state of a group. The end-user of this engine should never touch this
/// type, and it's public _only_ to make [<CliMutable>] work.
and [<CLIMutable; NoEquality; NoComparison>] GroupState =
    { Id : Guid
      Name : string
      OptSpecialization : string option
      PublishChanges : bool
      Persistent : bool
      CreationTimeStampNp : int64
      DispatcherNp : GroupDispatcher
      Xtension : Xtension }
    interface SimulantState

/// Hosts the ongoing state of an entity. The end-user of this engine should never touch this
/// type, and it's public _only_ to make [<CliMutable>] work.
and [<CLIMutable; NoEquality; NoComparison>] EntityState =
    { Id : Guid
      Name : string
      OptSpecialization : string option
      Position : Vector2 // NOTE: will become a Vector3 if Nu gets 3d capabilities
      Depth : single // NOTE: will become part of position if Nu gets 3d capabilities
      Size : Vector2 // NOTE: will become a Vector3 if Nu gets 3d capabilities
      Rotation : single // NOTE: will become a Vector3 if Nu gets 3d capabilities
      Visible : bool
      ViewType : ViewType
      Omnipresent : bool
      PublishChanges : bool
      Persistent : bool
      CreationTimeStampNp : int64 // just needed for ordering writes to reduce diff volumes
      DispatcherNp : EntityDispatcher
      FacetNames : string list
      FacetsNp : Facet list
      OptOverlayName : string option
      Xtension : Xtension }
    interface SimulantState

/// A marker interface for the simulation types (Game, Screen, Group, and Entity).
/// The only methods that belong here are those used internally by Nu's event system.
and Simulant =
    interface
        /// Get the entity's publishing priority.
        abstract GetPublishingPriority : (Entity -> World -> single) -> World -> single
        /// Get the simulant's address.
        abstract SimulantAddress : Simulant Address
        end

/// Operators for the Simulant type.
and SimulantOperators =
    private
        | SimulantOperators

    /// Concatenate two addresses, forcing the type of first address.
    static member acatf<'a> (address : 'a Address) (simulant : Simulant) = acatf address (atooa simulant.SimulantAddress)

    /// Concatenate two addresses, takings the type of first address.
    static member (->-) (address, simulant : Simulant) = SimulantOperators.acatf address simulant

/// The game type that hosts the various screens used to navigate through a game.
and [<StructuralEquality; NoComparison>] Game =
    { GameAddress : Game Address }

    interface Simulant with
        member this.GetPublishingPriority _ _ = Constants.Engine.GamePublishingPriority
        member this.SimulantAddress = atoa<Game, Simulant> this.GameAddress
        end

    /// Get the full name of a game proxy.
    member this.GameFullName = Address.getFullName this.GameAddress

    /// Create a Game proxy from an address.
    static member proxy address = { GameAddress = address }

    /// Concatenate two addresses, taking the type of first address.
    static member acatf<'a> (address : 'a Address) (game : Game) = acatf address (atooa game.GameAddress)

    /// Concatenate two addresses, taking the type of first address.
    static member (->-) (address, game) = Game.acatf address game

/// The screen type that allows transitioning to and from other screens, and also hosts the
/// currently interactive groups of entities.
and [<StructuralEquality; NoComparison>] Screen =
    { ScreenAddress : Screen Address }

    interface Simulant with
        member this.GetPublishingPriority _ _ = Constants.Engine.ScreenPublishingPriority
        member this.SimulantAddress = atoa<Screen, Simulant> this.ScreenAddress
        end

    /// Get the full name of a screen proxy.
    member this.ScreenFullName = Address.getFullName this.ScreenAddress

    /// Get the name of a screen proxy.
    member this.ScreenName = Address.getName this.ScreenAddress

    /// Create a Screen proxy from an address.
    static member proxy address = { ScreenAddress = address }

    /// Concatenate two addresses, taking the type of first address.
    static member acatf<'a> (address : 'a Address) (screen : Screen) = acatf address (atooa screen.ScreenAddress)

    /// Concatenate two addresses, taking the type of first address.
    static member (->-) (address, screen) = Screen.acatf address screen

    /// Convert a screen's name to it's proxy.
    static member (!>) screenName = Screen.proxy ^ ntoa screenName

    /// Convert a screen's proxy to a group's by appending the group's name at the end.
    static member (=>) (screen, groupName : string) = Group.proxy ^ atoa<Screen, Group> screen.ScreenAddress ->- ntoa groupName

/// Forms a logical group of entities.
and [<StructuralEquality; NoComparison>] Group =
    { GroupAddress : Group Address }

    interface Simulant with
        member this.GetPublishingPriority _ _ = Constants.Engine.GroupPublishingPriority
        member this.SimulantAddress = atoa<Group, Simulant> this.GroupAddress
        end

    /// Get the full name of a group proxy.
    member this.GroupFullName = Address.getFullName this.GroupAddress

    /// Get the name of a group proxy.
    member this.GroupName = Address.getName this.GroupAddress

    /// Create a Group proxy from an address.
    static member proxy address = { GroupAddress = address }

    /// Concatenate two addresses, taking the type of first address.
    static member acatf<'a> (address : 'a Address) (group : Group) = acatf address (atooa group.GroupAddress)

    /// Convert a group's proxy to its screen's.
    static member (!<) group = Screen.proxy ^ Address.allButLast group.GroupAddress

    /// Convert a group's proxy to an entity's by appending the entity's name at the end.
    static member (=>) (group, entityName) = Entity.proxy ^ atoa<Group, Entity> group.GroupAddress ->- ntoa entityName

    /// Concatenate two addresses, taking the type of first address.
    static member (->-) (address, group) = Group.acatf address group

/// The type around which the whole game engine is based! Used in combination with dispatchers
/// to implement things like buttons, characters, blocks, and things of that sort.
and [<StructuralEquality; NoComparison>] Entity =
    { EntityAddress : Entity Address }

    interface Simulant with
        member this.GetPublishingPriority getEntityPublishingPriority world = getEntityPublishingPriority this world
        member this.SimulantAddress = atoa<Entity, Simulant> this.EntityAddress
        end

    /// Get the name of an entity proxy.
    member this.EntityFullName = Address.getFullName this.EntityAddress

    /// Get the name of an entity proxy.
    member this.EntityName = Address.getName this.EntityAddress

    /// Create an Entity proxy from an address.
    static member proxy address = { EntityAddress = address }

    /// Concatenate two addresses, taking the type of first address.
    static member acatf<'a> (address : 'a Address) (entity : Entity) = acatf address (atooa entity.EntityAddress)

    /// Convert an entity's proxy to its group's.
    static member (!<) entity = Group.proxy ^ Address.allButLast entity.EntityAddress

    /// Concatenate two addresses, taking the type of first address.
    static member (->-) (address, entity) = Entity.acatf address entity

/// Provides a way to make user-defined dispatchers, facets, and various other sorts of game-
/// specific values.
and NuPlugin () =

    /// Make user-defined subsystems such that Nu can utilitze them at run-time.
    abstract MakeSubsystems : unit -> (string * Subsystem) list
    default this.MakeSubsystems () = []
    
    /// Optionally make a user-defined game dispatchers such that Nu can utililize it at run-time.
    abstract MakeOptGameDispatcher : unit -> GameDispatcher option
    default this.MakeOptGameDispatcher () = None
    
    /// Make user-defined screen dispatchers such that Nu can utililize them at run-time.
    abstract MakeScreenDispatchers : unit -> ScreenDispatcher list
    default this.MakeScreenDispatchers () = []
    
    /// Make user-defined group dispatchers such that Nu can utililize them at run-time.
    abstract MakeGroupDispatchers : unit -> GroupDispatcher list
    default this.MakeGroupDispatchers () = []
    
    /// Make user-defined entity dispatchers such that Nu can utililize them at run-time.
    abstract MakeEntityDispatchers : unit -> EntityDispatcher list
    default this.MakeEntityDispatchers () = []
    
    /// Make user-defined assets such that Nu can utililize them at run-time.
    abstract MakeFacets : unit -> Facet list
    default this.MakeFacets () = []
    
    /// Make the overlay routes that will allow Nu to use different overlays for the specified
    /// types. For example, a returned router of (typeof<ButtonDispatcher>.Name, Some "CustomButtonOverlay")
    /// will cause all buttons to use the overlay with the name "CustomButtonOverlay" rather
    /// than the default "ButtonDispatcher" overlay.
    abstract MakeOverlayRoutes : unit -> (string * string option) list
    default this.MakeOverlayRoutes () = []

/// The world's subsystems.
/// TODO: Make this an abstract data type.
and [<ReferenceEquality>] internal Subsystems =
    // private
        { SubsystemMap : Map<string, Subsystem> }

/// The world's components.
/// TODO: Make this an abstract data type.
and [<ReferenceEquality>] internal Components =
    // private
        { EntityDispatchers : Map<string, EntityDispatcher>
          GroupDispatchers : Map<string, GroupDispatcher>
          ScreenDispatchers : Map<string, ScreenDispatcher>
          GameDispatchers : Map<string, GameDispatcher>
          Facets : Map<string, Facet> }

/// The world's simple callback facilities.
/// TODO: Make this an abstract data type.
and [<ReferenceEquality>] internal Callbacks =
    // private
        { Subscriptions : SubscriptionEntries
          Unsubscriptions : UnsubscriptionEntries
          Tasklets : Tasklet Queue
          CallbackStates : Map<Guid, obj> }

/// The world's state.
/// TODO: Make this an abstract data type.
and [<ReferenceEquality>] internal WorldState =
    // private
        { TickRate : int64
          TickTime : int64
          UpdateCount : int64
          Liveness : Liveness
          OptScreenTransitionDestination : Screen option // TODO: move this into Game?
          AssetMetadataMap : AssetMetadataMap
          AssetGraphFilePath : string
          Overlayer : Overlayer
          OverlayRouter : OverlayRouter
          OverlayFilePath : string
          Camera : Camera
          OptEntityCache : KeyedCache<Entity Address * World, EntityState option>
          RefClipboard : EntityState option ref // NOTE: this makes for suboptimal value semantics for WorldState
          UserState : obj }

/// The world, in a functional programming sense. Hosts the game object, the dependencies
/// needed to implement a game, messages to by consumed by the various engine sub-systems,
/// and general configuration data.
and [<ReferenceEquality>] World =
    private
        { Subsystems : Subsystems
          Components : Components
          Callbacks : Callbacks
          State : WorldState
          SimulantStates : GameState * Map<string, ScreenState * Map<string, GroupState * Map<string, EntityState>>> }