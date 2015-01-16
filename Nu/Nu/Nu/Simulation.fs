// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2014.

namespace Nu
open System
open System.Collections.Generic
open System.ComponentModel
open System.Reflection
open System.Xml
open FSharpx
open SDL2
open OpenTK
open Prime
open Nu
open Nu.Constants

[<AutoOpen>]
module InterativityModule =

    /// Describes the game engine's current level of 'interactivity'.
    type Interactivity =
        | GuiOnly
        | GuiAndPhysics
        | GuiAndPhysicsAndGamePlay

[<RequireQualifiedAccess>]
module Interactivity =

    /// Query that the engine is in game-playing mode.
    let isGamePlaying interactivity =
        match interactivity with
        | GuiOnly -> false
        | GuiAndPhysics -> false
        | GuiAndPhysicsAndGamePlay -> true

    /// Query that the physics system is running.
    let isPhysicsRunning interactivity =
        match interactivity with
        | GuiOnly -> false
        | GuiAndPhysics -> true
        | GuiAndPhysicsAndGamePlay -> true

[<AutoOpen>]
module TransitionTypeModule =

    /// The type of a screen transition. Incoming means a new screen is being shown, and Outgoing
    /// means an existing screen being hidden.
    type TransitionType =
        | Incoming
        | Outgoing

[<AutoOpen>]
module ScreenStateModule =

    /// The state of a screen in regards to its transition.
    type ScreenState =
        | IncomingState
        | OutgoingState
        | IdlingState

[<AutoOpen>]
module SimulationModule =

    let private AnyEventAddressesCache =
        Dictionary<obj Address, obj Address list> HashIdentity.Structural

    /// Describes one of a screen's transition processes.
    type [<CLIMutable; StructuralEquality; NoComparison>] Transition =
        { TransitionLifetime : int64
          TransitionType : TransitionType
          OptDissolveImage : AssetTag option }

        static member make transitionType =
            { TransitionLifetime = 0L
              TransitionType = transitionType
              OptDissolveImage = None }

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
          Collidee : Entity Address }

    /// The data for a world state change event.
    and [<StructuralEquality; NoComparison>] WorldStateChangeData =
        { OldWorldState : WorldState }

    /// The data for a simulant change event.
    and [<StructuralEquality; NoComparison>] SimulantChangeData<'a when 'a :> Simulant> =
        { OldSimulant : 'a }

    /// An event used by Nu's purely functional event system.
    and [<ReferenceEquality>] Event<'a, 's when 's :> Simulant> =
        { SubscriberAddress : 's Address
          PublisherAddress : Simulant Address // TODO: consider making this a list so that Observer can provide all useful addresses
          EventAddress : 'a Address
          Data : 'a }

    /// Describes whether an event has been resolved or should cascade.
    and EventHandling =
        | Resolve
        | Cascade

    /// Describes a game event subscription.
    and Subscription<'a, 's when 's :> Simulant> =
        Event<'a, 's> -> World -> EventHandling * World

    /// Describes a game event subscription that can be boxed / unboxed.
    and BoxableSubscription = obj -> World -> EventHandling * World

    /// An entry into the world's subscription map.
    and SubscriptionEntry = Guid * obj Address * obj

    /// A map of event subscriptions.
    and SubscriptionEntries = Map<obj Address, SubscriptionEntry rQueue>

    /// Abstracts over a subscription sorting procedure.
    and SubscriptionSorter = SubscriptionEntry rQueue -> World -> SubscriptionEntry rQueue

    /// A map of subscription keys to unsubscription data.
    and UnsubscriptionEntries = Map<Guid, obj Address * obj Address>

    /// A task to be completed at the given time, with time being represented by the world's tick
    /// field.
    and [<ReferenceEquality>] Task =
        { ScheduledTime : int64
          Operation : World -> World }

    /// The default dispatcher for games.
    and GameDispatcher () =

        static member FieldDefinitions =
            [define? PublishChanges true]

        /// Register a game when adding it to the world. Note that there is not corresponding
        /// Unregister method due to the inability to remove a game from the world.
        abstract Register : Game * World -> Game * World
        default dispatcher.Register (game, world) = (game, world)

    /// The default dispatcher for screens.
    and ScreenDispatcher () =

        static member FieldDefinitions =
            [define? PublishChanges true
             define? Persistent true]

        /// Register a screen when adding it to the world.
        abstract Register : Screen * Screen Address * World -> Screen * World
        default dispatcher.Register (screen, _, world) = (screen, world)

        /// Unregister a screen when removing it from the world.
        abstract Unregister : Screen * Screen Address * World -> Screen * World
        default dispatcher.Unregister (screen, _, world) = (screen, world)

    /// The default dispatcher for groups.
    and GroupDispatcher () =

        static member FieldDefinitions =
            [define? PublishChanges true
             define? Persistent true]

        /// Register a group when adding it to a screen.
        abstract Register : Group * Group Address * World -> Group * World
        default dispatcher.Register (group, _, world) = (group, world)

        /// Unregister a group when removing it from a screen.
        abstract Unregister : Group * Group Address * World -> Group * World
        default dispatcher.Unregister (group, _, world) = (group, world)

    /// The default dispatcher for entities.
    and EntityDispatcher () =

        static member FieldDefinitions =
            [define? Position Vector2.Zero
             define? Depth 0.0f
             define? Size DefaultEntitySize
             define? Rotation 0.0f
             define? Visible true
             define? ViewType Relative
             define? PublishChanges true
             define? Persistent true]

        /// Register an entity when adding it to a group.
        abstract Register : Entity * Entity Address * World -> Entity * World
        default dispatcher.Register (entity, _, world) = (entity, world)

        /// Unregister an entity when removing it from a group.
        abstract Unregister : Entity * Entity Address * World -> Entity * World
        default dispatcher.Unregister (entity, _, world) = (entity, world)

        /// Propagate an entity's physics properties from the physics subsystem.
        abstract PropagatePhysics : Entity * Entity Address * World -> World
        default dispatcher.PropagatePhysics (_, _, world) = world

        /// Get the render descriptors needed to render an entity.
        abstract GetRenderDescriptors : Entity * World -> RenderDescriptor list
        default dispatcher.GetRenderDescriptors (_, _) = []

        /// Get the quick size of an entity (the appropriate user-define size for an entity).
        abstract GetQuickSize : Entity * World -> Vector2
        default dispatcher.GetQuickSize (_, _) = Vector2.One

        /// Get the priority with which an entity is picked in the editor.
        abstract GetPickingPriority : Entity * World -> single
        default dispatcher.GetPickingPriority (entity, _) = entity.Depth

    /// Dynamically augments an entity's behavior in a composable way.
    and Facet () =

        /// Register a facet when adding it to an entity.
        abstract Register : Entity * Entity Address * World -> Entity * World
        default facet.Register (entity, address, world) =
            let world = facet.RegisterPhysics (entity, address, world)
            (entity, world)

        /// Unregister a facet when removing it from an entity.
        abstract Unregister : Entity * Entity Address * World -> Entity * World
        default facet.Unregister (entity, address, world) =
            let world = facet.UnregisterPhysics (entity, address, world)
            (entity, world)

        /// Participate in the registration of an entity's physics with the physics subsystem.
        abstract RegisterPhysics : Entity * Entity Address * World -> World
        default facet.RegisterPhysics (_, _, world) = world

        /// Participate in the unregistration of an entity's physics from the physics subsystem.
        abstract UnregisterPhysics : Entity * Entity Address * World -> World
        default facet.UnregisterPhysics (_, _, world) = world

        /// Participate in the propagation an entity's physics properties from the physics subsystem.
        abstract PropagatePhysics : Entity * Entity Address * World -> World
        default facet.PropagatePhysics (_, _, world) = world

        /// Participate in getting the render descriptors needed to render an entity.
        abstract GetRenderDescriptors : Entity * World -> RenderDescriptor list
        default facet.GetRenderDescriptors (_, _) = []

        /// Participate in getting the priority with which an entity is picked in the editor.
        abstract GetQuickSize : Entity * World -> Vector2
        default facet.GetQuickSize (_, _) = DefaultEntitySize

    /// A marker interface for simulation types (Game, Screen, Group, Entity).
    /// The only methods that have a place in here are those used internally by Nu's event system.
    and Simulant =
        interface
            /// Get the entity's publishing priority.
            abstract GetPublishingPriority : (Entity -> World -> single) -> World -> single
        end

    /// The game type that hosts the various screens used to navigate through a game.
    and [<CLIMutable; StructuralEquality; NoComparison>] Game =
        { Id : Guid
          OptSelectedScreenAddress : Screen Address option
          PublishChanges : bool
          CreationTimeNp : DateTime
          DispatcherNp : GameDispatcher
          Xtension : Xtension }

        interface Simulant with
            member this.GetPublishingPriority _ _ = GamePublishingPriority

        /// Access a game's dynamic member.
        static member (?) (this : Game, memberName) =
            Xtension.(?) (this.Xtension, memberName)

        /// Update a game's dynamic member.
        static member (?<-) (this : Game, memberName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
            { this with Xtension = xtension }

        static member getPublishChanges (game : Game) = game.PublishChanges
        static member setPublishChanges value (game : Game) = { game with PublishChanges = value }
        static member getOptSelectedScreenAddress game = game.OptSelectedScreenAddress
        static member setOptSelectedScreenAddress optSelectedScreenAddress game = { game with OptSelectedScreenAddress = optSelectedScreenAddress }

        /// Register a game when adding it to the world. Note that there is not corresponding
        /// Unregister method due to the inability to remove a game from the world.
        static member register (game : Game) (world : World) : Game * World =
            game.DispatcherNp.Register (game, world)

        /// Query that a screen dispatches in the same manner as the dispatcher with the target type.
        static member dispatchesAs (dispatcherTargetType : Type) (game : Game) =
            Reflection.dispatchesAs dispatcherTargetType game.DispatcherNp

        /// Make a game.
        static member make dispatcher =
            { Id = Core.makeId ()
              OptSelectedScreenAddress = None
              PublishChanges = true
              CreationTimeNp = DateTime.UtcNow
              DispatcherNp = dispatcher
              Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true }}

    /// The screen type that allows transitioning to and from other screens, and also hosts the
    /// currently interactive groups of entities.
    and [<CLIMutable; StructuralEquality; NoComparison>] Screen =
        { Id : Guid
          Name : string
          ScreenStateNp : ScreenState
          TransitionTicksNp : int64
          Incoming : Transition
          Outgoing : Transition
          PublishChanges : bool
          Persistent : bool
          CreationTimeNp : DateTime
          DispatcherNp : ScreenDispatcher
          Xtension : Xtension }

        interface Simulant with
            member this.GetPublishingPriority _ _ = ScreenPublishingPriority

        /// Access screen's dynamic member.
        static member (?) (this : Screen, memberName) =
            Xtension.(?) (this.Xtension, memberName)

        /// Update screen's dynamic member.
        static member (?<-) (this : Screen, memberName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
            { this with Xtension = xtension }

        static member getName (screen : Screen) = screen.Name
        static member getScreenStateNp (screen : Screen) = screen.ScreenStateNp
        static member setScreenStateNp value screen = { screen with ScreenStateNp = value }
        static member getTransitionTicksNp (screen : Screen) = screen.TransitionTicksNp
        static member setTransitionTicksNp value screen = { screen with TransitionTicksNp = value }
        static member getIncoming (screen : Screen) = screen.Incoming
        static member setIncoming value screen = { screen with Incoming = value }
        static member getOutgoing (screen : Screen) = screen.Outgoing
        static member setOutgoing value screen = { screen with Outgoing = value }
        static member getPublishChanges (screen : Screen) = screen.PublishChanges
        static member setPublishChanges value (screen : Screen) = { screen with PublishChanges = value }
        static member getPersistent (screen : Screen) = screen.Persistent
        static member setPersistent value (screen : Screen) = { screen with Persistent = value }

        /// Register a screen when adding it to the world.
        static member register (screen : Screen) address world =
            screen.DispatcherNp.Register (screen, address, world)

        /// Unregister a screen when removing it from the world.
        static member unregister (screen : Screen) address world =
            screen.DispatcherNp.Unregister (screen, address, world)

        /// Query that a screen idling (that is, not currently transitioning in or out via another screen).
        static member isIdling screen =
            screen.ScreenStateNp = IdlingState

        /// Query that a screen dispatches in the same manner as the dispatcher with the target type.
        static member dispatchesAs (dispatcherTargetType : Type) (screen : Screen) =
            Reflection.dispatchesAs dispatcherTargetType screen.DispatcherNp

        /// Make a screen.
        static member make dispatcher optName =
            let id = Core.makeId ()
            { Id = id
              Name = match optName with None -> acstring id | Some name -> name
              ScreenStateNp = IdlingState
              TransitionTicksNp = 0L // TODO: roll this field into Incoming/OutcomingState values
              Incoming = Transition.make Incoming
              Outgoing = Transition.make Outgoing
              PublishChanges = true
              Persistent = true
              CreationTimeNp = DateTime.UtcNow
              DispatcherNp = dispatcher
              Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true }}

    /// Forms logical groups of entities.
    and [<CLIMutable; StructuralEquality; NoComparison>] Group =
        { Id : Guid
          Name : string
          PublishChanges : bool
          Persistent : bool
          CreationTimeNp : DateTime
          DispatcherNp : GroupDispatcher
          Xtension : Xtension }

        interface Simulant with
            member this.GetPublishingPriority _ _ = GroupPublishingPriority

        /// Access a group's dynamic member.
        static member (?) (this : Group, memberName) =
            Xtension.(?) (this.Xtension, memberName)

        /// Update a group's dynamic member.
        static member (?<-) (this : Group, memberName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
            { this with Xtension = xtension }

        static member getName (group : Group) = group.Name
        static member getPublishChanges (group : Group) = group.PublishChanges
        static member setPublishChanges value (group : Group) = { group with PublishChanges = value }
        static member getPersistent (group : Group) = group.Persistent
        static member setPersistent value (group : Group) = { group with Persistent = value }

        /// Register a group when adding it to a screen.
        static member register (group : Group) address world =
            group.DispatcherNp.Register (group, address, world)
        
        /// Unregister a group when removing it from a screen.
        static member unregister (group : Group) address world =
            group.DispatcherNp.Unregister (group, address, world)

        /// Query that a group dispatches in the same manner as the dispatcher with the target type.
        static member dispatchesAs (dispatcherTargetType : Type) (group : Group) =
            Reflection.dispatchesAs dispatcherTargetType group.DispatcherNp

        /// Make a group.
        static member make dispatcher optName =
            let id = Core.makeId ()
            { Group.Id = id
              Name = match optName with None -> acstring id | Some name -> name
              PublishChanges = true
              Persistent = true
              CreationTimeNp = DateTime.UtcNow
              DispatcherNp = dispatcher
              Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true }}

    /// The type around which the whole game engine is based! Used in combination with dispatchers
    /// to implement things like buttons, characters, blocks, and things of that sort.
    and [<CLIMutable; StructuralEquality; NoComparison>] Entity =
        { Id : Guid
          Name : string
          Position : Vector2 // NOTE: will become a Vector3 if Nu gets 3d capabilities
          Depth : single // NOTE: will become part of position if Nu gets 3d capabilities
          Size : Vector2 // NOTE: will become a Vector3 if Nu gets 3d capabilities
          Rotation : single // NOTE: will become a Vector3 if Nu gets 3d capabilities
          Visible : bool
          ViewType : ViewType
          PublishChanges : bool
          Persistent : bool
          CreationTimeNp : DateTime // just needed for ordering writes to reduce diff volumes
          DispatcherNp : EntityDispatcher
          FacetNames : string list
          FacetsNp : Facet list
          OptOverlayName : string option
          Xtension : Xtension }

        interface Simulant with
            member this.GetPublishingPriority getEntityPublishingPriority world =
                getEntityPublishingPriority this world

        /// Access an entity's dynamic member.
        static member (?) (this : Entity, memberName) =
            Xtension.(?) (this.Xtension, memberName)

        /// Update an entity's dynamic member.
        static member (?<-) (this : Entity, memberName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
            { this with Xtension = xtension }

        static member getName (entity : Entity) = entity.Name
        static member getFacetNames (entity : Entity) = entity.FacetNames
        static member getOptOverlayName (entity : Entity) = entity.OptOverlayName
        static member getPosition (entity : Entity) = entity.Position
        static member setPosition value (entity : Entity) = { entity with Position = value }
        static member getDepth (entity : Entity) = entity.Depth
        static member setDepth value (entity : Entity) = { entity with Depth = value }
        static member getSize (entity : Entity) = entity.Size
        static member setSize value (entity : Entity) = { entity with Size = value }
        static member getRotation (entity : Entity) = entity.Rotation
        static member setRotation value (entity : Entity) = { entity with Rotation = value }
        static member getVisible (entity : Entity) = entity.Visible
        static member setVisible value (entity : Entity) = { entity with Visible = value }
        static member getViewType (entity : Entity) = entity.ViewType
        static member setViewType value (entity : Entity) = { entity with ViewType = value }
        static member getPublishChanges (entity : Entity) = entity.PublishChanges
        static member setPublishChanges value (entity : Entity) = { entity with PublishChanges = value }
        static member getPersistent (entity : Entity) = entity.Persistent
        static member setPersistent value (entity : Entity) = { entity with Persistent = value }

        /// Register an entity when adding it to a group.
        static member register (entity : Entity) address world =
            let (entity, world) = entity.DispatcherNp.Register (entity, address, world)
            List.fold
                (fun (entity, world) (facet : Facet) -> facet.Register (entity, address, world))
                (entity, world)
                entity.FacetsNp
        
        /// Unregister an entity when removing it from a group.
        static member unregister (entity : Entity) address world =
            let (entity, world) = entity.DispatcherNp.Unregister (entity, address, world)
            List.fold
                (fun (entity, world) (facet : Facet) -> facet.Unregister (entity, address, world))
                (entity, world)
                entity.FacetsNp
        
        /// Propagate an entity's physics properties from the physics subsystem.
        static member propagatePhysics (entity : Entity) address world =
            let world = entity.DispatcherNp.PropagatePhysics (entity, address, world)
            List.fold
                (fun world (facet : Facet) -> facet.PropagatePhysics (entity, address, world))
                world
                entity.FacetsNp
        
        /// Get the render descriptors needed to render an entity.
        static member getRenderDescriptors (entity : Entity) world =
            let renderDescriptors = entity.DispatcherNp.GetRenderDescriptors (entity, world)
            List.fold
                (fun renderDescriptors (facet : Facet) ->
                    let descriptors = facet.GetRenderDescriptors (entity, world)
                    descriptors @ renderDescriptors)
                renderDescriptors
                entity.FacetsNp
        
        /// Get the quick size of an entity (the appropriate user-define size for an entity).
        static member getQuickSize  (entity : Entity) world =
            let quickSize = entity.DispatcherNp.GetQuickSize (entity, world)
            List.fold
                (fun (maxSize : Vector2) (facet : Facet) ->
                    let quickSize = facet.GetQuickSize (entity, world)
                    Vector2(
                        Math.Max (quickSize.X, maxSize.X),
                        Math.Max (quickSize.Y, maxSize.Y)))
                quickSize
                entity.FacetsNp
        
        /// Get the priority with which an entity is picked in the editor.
        static member getPickingPriority (entity : Entity) world =
            entity.DispatcherNp.GetPickingPriority (entity, world)

        /// Get the names of all facets used by an entity via reflection.
        /// TODO: see if this should be used as often as it is, and if it is needed in only one or
        /// two cases, just inline it.
        static member getFacetNamesReflectively entity =
            List.map Reflection.getTypeName entity.FacetsNp

        /// Query that a facet is compatible with those already being used by an entity.
        /// Note a facet is incompatible with any other facet if it contains any fields that has
        /// the same name but a different type.
        static member isFacetCompatible entityDispatcherMap facet (entity : Entity) =
            let facetType = facet.GetType ()
            let facetFieldDefinitions = Reflection.getFieldDefinitions facetType
            if Reflection.isFacetCompatibleWithDispatcher entityDispatcherMap facet entity then
                List.notExists
                    (fun definition ->
                        match Map.tryFind definition.FieldName entity.Xtension.XFields with
                        | Some field -> field.GetType () <> definition.FieldType
                        | None -> false)
                    facetFieldDefinitions
            else false

        /// Query that an entity dispatches in the same manner as the dispatcher with the target type.
        static member dispatchesAs (dispatcherTargetType : Type) (entity : Entity) =
            Reflection.dispatchesAs dispatcherTargetType entity.DispatcherNp

        /// Make an entity.
        static member make dispatcher optOverlayName optName =
            let id = Core.makeId ()
            { Id = id
              Name = match optName with None -> acstring id | Some name -> name
              Position = Vector2.Zero
              Depth = 0.0f
              Size = DefaultEntitySize
              Rotation = 0.0f
              Visible = true
              ViewType = Relative
              PublishChanges = true
              Persistent = true
              CreationTimeNp = DateTime.UtcNow
              DispatcherNp = dispatcher
              FacetNames = []
              FacetsNp = []
              OptOverlayName = optOverlayName
              Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true }}

    /// Represents an untyped message to a subsystem.
    and SubsystemMessage = obj

    /// Represents an untype result of a subsystem.
    and SubsystemResult = obj

    /// The type of subsystem. Dictates where subsystem's processing happens in the game loop.
    and SubsystemType =
        | UpdateType
        | RenderType
        | AudioType

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
            abstract ProcessMessages : World -> SubsystemResult * Subsystem
            /// Apply the result of the message processing to the world.
            abstract ApplyResult : SubsystemResult * World -> World
            /// Clean up any resources used by the subsystem.
            abstract CleanUp : World -> Subsystem * World
        end

    /// The world's subsystems.
    and Subsystems = Map<string, Subsystem>

    /// The world's components.
    and [<ReferenceEquality>] Components =
        { EntityDispatchers : Map<string, EntityDispatcher>
          GroupDispatchers : Map<string, GroupDispatcher>
          ScreenDispatchers : Map<string, ScreenDispatcher>
          GameDispatchers : Map<string, GameDispatcher>
          Facets : Map<string, Facet> }

    /// The world's higher order facilities.
    and [<ReferenceEquality>] Callbacks =
        { Tasks : Task list
          Subscriptions : SubscriptionEntries
          Unsubscriptions : UnsubscriptionEntries
          CallbackStates : Map<Guid, obj> }

    /// The world's state.
    and [<ReferenceEquality>] WorldState =
        { TickTime : int64
          Liveness : Liveness
          Interactivity : Interactivity
          OptScreenTransitionDestinationAddress : Screen Address option
          Camera : Camera
          AssetMetadataMap : AssetMetadataMap
          AssetGraphFilePath : string
          Overlayer : Overlayer
          OverlayRouter : OverlayRouter
          OverlayFilePath : string
          UserState : obj }

    /// The world, in a functional programming sense. Hosts the game object, the dependencies
    /// needed to implement a game, messages to by consumed by the various engine sub-systems,
    /// and general configuration data.
    ///
    /// TODO: attempt to implement with Fsharpx.PersistentHashMap with hash cached in Address type.
    and [<ReferenceEquality>] World =
        { Simulants : Game * Map<string, Screen * Map<string, Group * Map<string, Entity>>>
          Subsystems : Subsystems
          Components : Components
          Callbacks : Callbacks
          State : WorldState }

        static member internal atoua address = Address.changeType<'a, Simulant> address
        static member internal atosa address = Address.changeType<'a, Screen> address
        static member internal atoga address = Address.changeType<'a, Group> address
        static member internal atoea address = Address.changeType<'a, Entity> address
        static member internal gatoea groupAddress entityName = Address.changeType<Group, Entity> groupAddress ->- ntoa entityName
        static member internal satoga screenAddress groupName = Address.changeType<Screen, Group> screenAddress ->- ntoa groupName
        static member internal satoea screenAddress groupName entityName = World.gatoea (World.satoga screenAddress groupName) entityName
        static member internal eatoga entityAddress = Address.take<Entity, Group> 2 entityAddress
        static member internal gatosa groupAddress = Address.take<Group, Screen> 1 groupAddress
        static member internal eatosa entityAddress = Address.take<Entity, Screen> 1 entityAddress

        static member internal GameAddress = Address<Game>.empty
        static member internal DefaultScreenAddress = ntoa<Screen> DefaultGroupName
        static member internal DefaultGroupAddress = World.satoga World.DefaultScreenAddress DefaultGroupName
        static member internal DefaultEntityAddress = World.gatoea World.DefaultGroupAddress DefaultEntityName
        static member internal AnyEventAddress = ntoa<obj> "*"
        static member internal TickEventAddress = ntoa<unit> "Tick"
        static member internal SelectEventAddress = ntoa<unit> "Select"
        static member internal DeselectEventAddress = ntoa<unit> "Deselect"
        static member internal DownEventAddress = ntoa<unit> "Down"
        static member internal UpEventAddress = ntoa<unit> "Up"
        static member internal ClickEventAddress = ntoa<unit> "Click"
        static member internal OnEventAddress = ntoa<unit> "On"
        static member internal OffEventAddress = ntoa<unit> "Off"
        static member internal TouchEventAddress = ntoa<Vector2> "Touch"
        static member internal UntouchEventAddress = ntoa<Vector2> "Untouch"
        static member internal MouseEventAddress = ntoa<obj> "Mouse"
        static member internal MouseMoveEventAddress = World.MouseEventAddress -<- ntoa<MouseMoveData> "Move"
        static member internal MouseDragEventAddress = World.MouseEventAddress -<- ntoa<MouseMoveData> "Drag"
        static member internal MouseLeftEventAddress = World.MouseEventAddress -<- ntoa<MouseButtonData> "Left"
        static member internal MouseCenterEventAddress = World.MouseEventAddress -<- ntoa<MouseButtonData> "Center"
        static member internal MouseRightEventAddress = World.MouseEventAddress -<- ntoa<MouseButtonData> "Right"
        static member internal MouseX1EventAddress = World.MouseEventAddress -<- ntoa<MouseButtonData> "X1"
        static member internal MouseX2EventAddress = World.MouseEventAddress -<- ntoa<MouseButtonData> "X2"
        static member internal MouseLeftDownEventAddress = World.MouseLeftEventAddress -|- ntoa "Down"
        static member internal MouseLeftUpEventAddress = World.MouseLeftEventAddress -|- ntoa "Up"
        static member internal MouseLeftChangeEventAddress = World.MouseLeftEventAddress -|- ntoa "Change"
        static member internal MouseCenterDownEventAddress = World.MouseCenterEventAddress -|- ntoa "Down"
        static member internal MouseCenterUpEventAddress = World.MouseCenterEventAddress -|- ntoa "Up"
        static member internal MouseCenterChangeEventAddress = World.MouseCenterEventAddress -|- ntoa "Change"
        static member internal MouseRightDownEventAddress = World.MouseRightEventAddress -|- ntoa "Down"
        static member internal MouseRightUpEventAddress = World.MouseRightEventAddress -|- ntoa "Up"
        static member internal MouseRightChangeEventAddress = World.MouseRightEventAddress -|- ntoa "Change"
        static member internal MouseX1DownEventAddress = World.MouseX1EventAddress -|- ntoa "Down"
        static member internal MouseX1UpEventAddress = World.MouseX1EventAddress -|- ntoa "Up"
        static member internal MouseX1ChangeEventAddress = World.MouseX1EventAddress -|- ntoa "Change"
        static member internal MouseX2DownEventAddress = World.MouseX2EventAddress -|- ntoa "Down"
        static member internal MouseX2UpEventAddress = World.MouseX2EventAddress -|- ntoa "Up"
        static member internal MouseX2ChangeEventAddress = World.MouseX2EventAddress -|- ntoa "Change"
        static member internal KeyboardKeyEventAddress = ntoa<obj> "KeyboardKey"
        static member internal KeyboardKeyDownEventAddress = World.MouseEventAddress -<- ntoa<KeyboardKeyData> "Down"
        static member internal KeyboardKeyUpEventAddress = World.MouseEventAddress -<- ntoa<KeyboardKeyData> "Up"
        static member internal KeyboardKeyChangeEventAddress = World.MouseEventAddress -<- ntoa<KeyboardKeyData> "Change"
        static member internal CollisionEventAddress = ntoa<CollisionData> "Collision"
        static member internal IncomingEventAddress = ntoa<unit> "Incoming"
        static member internal IncomingStartEventAddress = World.IncomingEventAddress -|- ntoa "Start"
        static member internal IncomingFinishEventAddress = World.IncomingEventAddress -|- ntoa "Finish"
        static member internal OutgoingEventAddress = ntoa<unit> "Outgoing"
        static member internal OutgoingStartEventAddress = World.OutgoingEventAddress -|- ntoa "Start"
        static member internal OutgoingFinishEventAddress = World.OutgoingEventAddress -|- ntoa "Finish"
        static member internal WorldStateEventAddress = ntoa<obj> "WorldState"
        static member internal WorldStateChangeEventAddress = World.WorldStateEventAddress -<- ntoa<WorldStateChangeData> "Change"
        static member internal GameEventAddress = ntoa<obj> "Game"
        static member internal GameChangeEventAddress = World.GameEventAddress -<- ntoa<Game SimulantChangeData> "Change"
        static member internal ScreenEventAddress = ntoa<obj> "Screen"
        static member internal ScreenAddEventAddress = World.ScreenEventAddress -<- ntoa<unit> "Add"
        static member internal ScreenRemovingEventAddress = World.ScreenEventAddress -<- ntoa<unit> "Removing"
        static member internal ScreenChangeEventAddress = World.ScreenEventAddress -<- ntoa<Screen SimulantChangeData> "Change"
        static member internal GroupEventAddress = ntoa<obj> "Group"
        static member internal GroupAddEventAddress = World.GroupEventAddress -<- ntoa<unit> "Add"
        static member internal GroupRemovingEventAddress = World.GroupEventAddress -<- ntoa<unit> "Removing"
        static member internal GroupChangeEventAddress = World.GroupEventAddress -<- ntoa<Group SimulantChangeData> "Change"
        static member internal EntityEventAddress = ntoa<obj> "Entity"
        static member internal EntityAddEventAddress = World.EntityEventAddress -<- ntoa<unit> "Add"
        static member internal EntityRemovingEventAddress = World.EntityEventAddress -<- ntoa<unit> "Removing"
        static member internal EntityChangeEventAddress = World.EntityEventAddress -<- ntoa<Entity SimulantChangeData> "Change"
        static member internal DefaultDissolveImage = { PackageName = DefaultPackageName; AssetName = "Image8" }

        /// Make a key used to track an unsubscription with a subscription.
        static member makeSubscriptionKey () =
            Guid.NewGuid ()

        /// Make a callback key used to track callback states.
        static member makeCallbackKey () =
            Guid.NewGuid ()

        // OPTIMIZATION: priority annotated as single to decrease GC pressure.
        static member private sortFstDesc (priority : single, _) (priority2 : single, _) =
            if priority > priority2 then -1
            elif priority < priority2 then 1
            else 0
    
        static member private boxSubscription<'a, 's when 's :> Simulant> (subscription : Subscription<'a, 's>) =
            let boxableSubscription = fun (event : obj) world ->
                try subscription (event :?> Event<'a, 's>) world
                with
                | :? InvalidCastException ->
                    // NOTE: If you've reached this exception, then you've probably inadvertantly mixed
                    // up an event type parameter for some form of World.publish or subscribe.
                    reraise ()
                | _ -> reraise ()
            box boxableSubscription

        static member private getSortableSubscriptions
            getEntityPublishingPriority (subscriptions : SubscriptionEntry list) world :
            (single * SubscriptionEntry) list =
            List.fold
                (fun subscriptions (key, address, subscription) ->
                    match World.getOptSimulantForPublishing (World.atoua address) world with
                    | Some (simulant : Simulant) ->
                        let priority = simulant.GetPublishingPriority getEntityPublishingPriority world
                        let subscription = (priority, (key, address, subscription))
                        subscription :: subscriptions
                    | None -> (0.0f, (key, address, subscription)) :: subscriptions)
                []
                subscriptions

        /// Sort subscriptions using categorization via the 'by' procedure.
        static member sortSubscriptionsBy by (subscriptions : SubscriptionEntry list) world =
            let subscriptions = World.getSortableSubscriptions by subscriptions world
            let subscriptions = List.sortWith World.sortFstDesc subscriptions
            List.map snd subscriptions

        /// Sort subscriptions by their editor picking priority.
        static member sortSubscriptionsByPickingPriority subscriptions world =
            World.sortSubscriptionsBy
                (fun (entity : Entity) world -> entity.DispatcherNp.GetPickingPriority (entity, world))
                subscriptions
                world

        /// Sort subscriptions by their place in the world's simulant hierarchy.
        static member sortSubscriptionsByHierarchy subscriptions world =
            World.sortSubscriptionsBy
                (fun _ _ -> EntityPublishingPriority)
                subscriptions
                world

        /// A 'no-op' for subscription sorting - that is, performs no sorting at all.
        static member sortSubscriptionsNone (subscriptions : SubscriptionEntry list) (_ : World) =
            subscriptions

        static member private getAnyEventAddresses eventAddress =
            // OPTIMIZATION: uses memoization.
            if not <| Address.isEmpty eventAddress then
                let anyEventAddressesKey = Address.allButLast eventAddress
                match AnyEventAddressesCache.TryGetValue anyEventAddressesKey with
                | (true, anyEventAddresses) -> anyEventAddresses
                | (false, _) ->
                    let eventAddressList = eventAddress.Names
                    let anyEventAddressList = World.AnyEventAddress.Names
                    let anyEventAddresses =
                        [for i in 0 .. List.length eventAddressList - 1 do
                            let subNameList = List.take i eventAddressList @ anyEventAddressList
                            yield Address.make subNameList]
                    AnyEventAddressesCache.Add (anyEventAddressesKey, anyEventAddresses)
                    anyEventAddresses
            else failwith "Event name cannot be empty."

        static member private getSubscriptionsSorted (publishSorter : SubscriptionSorter) eventAddress world =
            let anyEventAddresses = World.getAnyEventAddresses eventAddress
            let optSubLists = List.map (fun anyEventAddress -> Map.tryFind anyEventAddress world.Callbacks.Subscriptions) anyEventAddresses
            let optSubLists = Map.tryFind eventAddress world.Callbacks.Subscriptions :: optSubLists
            let subLists = List.definitize optSubLists
            let subList = List.concat subLists
            let subListRev = List.rev subList
            publishSorter subListRev world

        static member private publishEvent<'a, 'p, 's when 'p :> Simulant and 's :> Simulant>
            (subscriberAddress : obj Address) (publisherAddress : 'p Address) (eventAddress : 'a Address) (eventData : 'a) subscription world =
            let event =
                { SubscriberAddress = Address.changeType<obj, 's> subscriberAddress
                  PublisherAddress = Address.changeType<'p, Simulant> publisherAddress
                  EventAddress = eventAddress
                  Data = eventData }
            let callableSubscription = unbox<BoxableSubscription> subscription
            let result = callableSubscription event world
            Some result

        /// Publish an event, using the given publishSorter procedure to arranging the order to which subscriptions are published.
        static member publish<'a, 'p when 'p :> Simulant>
            publishSorter (eventData : 'a) (eventAddress : 'a Address) (publisherAddress : 'p Address) world =
            let objEventAddress = atooa eventAddress
            let subscriptions = World.getSubscriptionsSorted publishSorter objEventAddress world
            let (_, world) =
                List.foldWhile
                    (fun (eventHandling, world) (_, subscriberAddress : obj Address, subscription) ->
                        if  (match eventHandling with Cascade -> true | Resolve -> false) &&
                            (match world.State.Liveness with Running -> true | Exiting -> false) then
                            match subscriberAddress.Names with
                            | [] -> World.publishEvent<'a, 'p, Game> subscriberAddress publisherAddress eventAddress eventData subscription world
                            | [_] -> World.publishEvent<'a, 'p, Screen> subscriberAddress publisherAddress eventAddress eventData subscription world
                            | [_; _] -> World.publishEvent<'a, 'p, Group> subscriberAddress publisherAddress eventAddress eventData subscription world
                            | [_; _; _] -> World.publishEvent<'a, 'p, Entity> subscriberAddress publisherAddress eventAddress eventData subscription world
                            | _ -> failwith "Unexpected match failure in 'Nu.World.publish.'"
                        else None)
                    (Cascade, world)
                    subscriptions
            world

        /// Publish an event.
        static member publish4<'a, 'p when 'p :> Simulant>
            (eventData : 'a) (eventAddress : 'a Address) (publisherAddress : 'p Address) world =
            World.publish World.sortSubscriptionsByHierarchy eventData eventAddress publisherAddress world

        /// Subscribe to an event.
        static member subscribe<'a, 's when 's :> Simulant>
            subscriptionKey (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriberAddress : 's Address) world =
            if not <| Address.isEmpty eventAddress then
                let objEventAddress = atooa eventAddress
                let subscriptions =
                    let subscriptionEntry = (subscriptionKey, atooa subscriberAddress, World.boxSubscription subscription)
                    match Map.tryFind objEventAddress world.Callbacks.Subscriptions with
                    | Some subscriptionEntries -> Map.add objEventAddress (subscriptionEntry :: subscriptionEntries) world.Callbacks.Subscriptions
                    | None -> Map.add objEventAddress [subscriptionEntry] world.Callbacks.Subscriptions
                let unsubscriptions = Map.add subscriptionKey (objEventAddress, atooa subscriberAddress) world.Callbacks.Unsubscriptions
                let callbacks = { world.Callbacks with Subscriptions = subscriptions; Unsubscriptions = unsubscriptions }
                { world with Callbacks = callbacks }
            else failwith "Event name cannot be empty."

        /// Subscribe to an event.
        static member subscribe4<'a, 's when 's :> Simulant>
            (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriberAddress : 's Address) world =
            World.subscribe (World.makeSubscriptionKey ()) subscription eventAddress subscriberAddress world

        /// Unsubscribe from an event.
        static member unsubscribe subscriptionKey world =
            match Map.tryFind subscriptionKey world.Callbacks.Unsubscriptions with
            | Some (eventAddress, subscriberAddress) ->
                match Map.tryFind eventAddress world.Callbacks.Subscriptions with
                | Some subscriptionList ->
                    let subscriptionList =
                        List.remove
                            (fun (subscriptionKey', subscriberAddress', _) ->
                                subscriptionKey' = subscriptionKey &&
                                subscriberAddress' = subscriberAddress)
                            subscriptionList
                    let subscriptions = 
                        match subscriptionList with
                        | [] -> Map.remove eventAddress world.Callbacks.Subscriptions
                        | _ -> Map.add eventAddress subscriptionList world.Callbacks.Subscriptions
                    let unsubscriptions = Map.remove subscriptionKey world.Callbacks.Unsubscriptions
                    let callbacks = { world.Callbacks with Subscriptions = subscriptions; Unsubscriptions = unsubscriptions }
                    { world with Callbacks = callbacks }
                | None -> world
            | None -> world

        /// Keep active a subscription for the lifetime of a simulant.
        static member monitor<'a, 's when 's :> Simulant>
            (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriberAddress : 's Address) world =
            if not <| Address.isEmpty subscriberAddress then
                let monitorKey = World.makeSubscriptionKey ()
                let removalKey = World.makeSubscriptionKey ()
                let world = World.subscribe<'a, 's> monitorKey subscription eventAddress subscriberAddress world
                let subscription' = fun _ world ->
                    let world = World.unsubscribe removalKey world
                    let world = World.unsubscribe monitorKey world
                    (Cascade, world)
                let removingEventAddress = stoa<unit> (typeof<'s>.Name + "/" + "Removing") ->>- subscriberAddress
                World.subscribe<unit, 's> removalKey subscription' removingEventAddress subscriberAddress world
            else failwith "Cannot monitor events with an anonymous subscriber."

        static member internal getSubsystem<'s when 's :> Subsystem> name world =
            Map.find name world.Subsystems :?> 's

        static member internal getSubsystemBy<'s, 't when 's :> Subsystem> by name world : 't =
            let subsystem = World.getSubsystem<'s> name world
            by subsystem

        static member internal setSubsystem<'s when 's :> Subsystem> (subsystem : 's) name world =
            let subsystems = Map.add name (subsystem :> Subsystem) world.Subsystems
            { world with Subsystems = subsystems }

        static member internal updateSubsystem<'s when 's :> Subsystem> (updater : 's -> World -> 's) name world =
            let subsystem = World.getSubsystem<'s> name world
            let subsystem = updater subsystem world
            World.setSubsystem subsystem name world

        /// Clear the physics messages.
        static member internal clearPhysicsMessages world =
            World.updateSubsystem (fun is _ -> is.ClearMessages ()) IntegratorSubsystemName world

        /// Clear the rendering messages.
        static member internal clearRenderMessages world =
            World.updateSubsystem (fun rs _ -> rs.ClearMessages ()) RendererSubsystemName world

        /// Clear the audio messages.
        static member internal clearAudioMessages world =
            World.updateSubsystem (fun aps _ -> aps.ClearMessages ()) AudioPlayerSubsystemName world

        /// Add a physics message to the world.
        static member addPhysicsMessage message world =
            World.updateSubsystem (fun is _ -> is.EnqueueMessage message) IntegratorSubsystemName world

        /// Add a rendering message to the world.
        static member addRenderMessage message world =
            World.updateSubsystem (fun rs _ -> rs.EnqueueMessage message) RendererSubsystemName world

        /// Add an audio message to the world.
        static member addAudioMessage (message : AudioMessage) world =
            World.updateSubsystem (fun aps _ -> aps.EnqueueMessage message) AudioPlayerSubsystemName world

        /// Add a task to be executed by the engine at the specified task tick.
        static member addTask task world =
            let callbacks = { world.Callbacks with Tasks = task :: world.Callbacks.Tasks }
            { world with Callbacks = callbacks }

        /// Add multiple task to be executed by the engine at the specified task tick.
        static member addTasks tasks world =
            let callbacks = { world.Callbacks with Tasks = tasks @ world.Callbacks.Tasks }
            { world with Callbacks = callbacks }

        /// Restore tasks to be executed by the engine at the specified task tick.
        static member internal restoreTasks tasks world =
            let callbacks = { world.Callbacks with Tasks = world.Callbacks.Tasks @ tasks }
            { world with Callbacks = callbacks }

        /// Clear all tasks.
        static member internal clearTasks world =
            let callbacks = { world.Callbacks with Tasks = [] }
            { world with Callbacks = callbacks }

        /// Add callback state to the world.
        static member addCallbackState key state world =
            let callbacks = { world.Callbacks with CallbackStates = Map.add key (state :> obj) world.Callbacks.CallbackStates }
            { world with Callbacks = callbacks }

        /// Remove callback state from the world.
        static member removeCallbackState key world =
            let callbacks = { world.Callbacks with CallbackStates = Map.remove key world.Callbacks.CallbackStates }
            { world with Callbacks = callbacks }

        /// Get callback state from the world.
        static member getCallbackState<'a> key world =
            let state = Map.find key world.Callbacks.CallbackStates
            state :?> 'a

        /// Get the state of the world.
        static member getState world = world.State

        /// Set the state of the world.
        static member setState state world =
            let oldState = world.State
            let world = { world with State = state }
            World.publish4 { OldWorldState = oldState } World.WorldStateChangeEventAddress World.GameAddress world

        /// Update the state of the world and the world.
        static member updateStateAndW updater world =
            let (state, world) = updater world.State world
            World.setState state world

        /// Update the state of the world.
        static member updateStateW updater world =
            World.updateStateAndW (fun state world -> (updater state world, world)) world

        /// Update the state of the world.
        static member updateState updater world =
            World.updateStateW (fun state _ -> updater state) world

        /// Update the world by its state.
        static member updateByState updater world : World =
            updater world.State world

        /// Lens the state of the world.
        static member lensState =
            { Get = World.getState
              Set = World.setState }

        /// Get the tick time.
        static member getTickTime world =
            world.State.TickTime

        /// Increment the tick time.
        static member internal incrementTickTime world =
            let state = { world.State with TickTime = world.State.TickTime + 1L }
            World.setState state world

        /// Get the the liveness state of the world.
        static member getLiveness world =
            world.State.Liveness

        /// Place the world into a state such that the app will exit at the end of the current frame.
        static member exit world =
            let state = { world.State with Liveness = Exiting }
            World.setState state world

        /// Query that the engine is in game-playing mode.
        static member isGamePlaying world =
            Interactivity.isGamePlaying world.State.Interactivity

        /// Query that the physics system is running.
        static member isPhysicsRunning world =
            Interactivity.isPhysicsRunning world.State.Interactivity

        /// Get the interactivity state of the world.
        static member getInteractivity world =
            world.State.Interactivity

        /// Set the level of the world's interactivity.
        static member setInteractivity interactivity world =
            let state = { world.State with Interactivity = interactivity }
            World.setState state world

        /// Update the the level of the world's interactivity.
        static member updateInteractivity updater world =
            let interactivity = updater <| World.getInteractivity world
            World.setInteractivity interactivity world

        /// Get the camera used to view the world.
        static member getCamera world =
            world.State.Camera

        /// Set the camera used to view the world.
        static member setCamera camera world =
            let state = { world.State with Camera = camera }
            World.setState state world

        /// Update the camera used to view the world.
        static member updateCamera updater world =
            let camera = updater <| World.getCamera world
            World.setCamera camera world

        /// Get the OptScreenTransitionDestinationAddress field of the world.
        static member getOptScreenTransitionDestinationAddress world =
            world.State.OptScreenTransitionDestinationAddress

        /// Set the OptScreenTransitionDestinationAddress field of the world.
        static member internal setOptScreenTransitionDestinationAddress address world =
            let state = { world.State with OptScreenTransitionDestinationAddress = address  }
            World.setState state world

        /// Get the asset metadata map.
        static member getAssetMetadataMap world =
            world.State.AssetMetadataMap

        /// Set the asset metadata map.
        static member internal setAssetMetadataMap assetMetadataMap world =
            let state = { world.State with AssetMetadataMap = assetMetadataMap }
            World.setState state world

        /// Set the Overlayer field of the world.
        static member internal setOverlayer overlayer world =
            let state = { world.State with Overlayer = overlayer }
            World.setState state world

        /// Get the user state of the world, casted to 'u.
        static member getUserState world : 'u =
            world.State.UserState :?> 'u

        /// Set the user state of the world.
        static member setUserState (userState : 'u) world =
            let state = { world.State with UserState = userState }
            World.setState state world

        /// Update the user state of the world.
        static member updateUserState (updater : 'u -> 'v) world =
            let state = World.getUserState world
            let state = updater state
            World.setUserState state world

        /// Ignore all handled events.
        static member handleAsPass<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Cascade, world)

        /// Swallow all handled events.
        static member handleAsSwallow<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Resolve, world)
        
        /// Handle event by exiting app.
        static member handleAsExit<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Resolve, World.exit world)

        /// Lens the world.
        static member lens = Lens.id

        (* Entity *)

        static member private optEntityFinder (address : Entity Address) world =
            match address.Names with
            | [screenName; groupName; entityName] ->
                let (_, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (_, groupMap) ->
                    match Map.tryFind groupName groupMap with
                    | Some (_, entityMap) -> Map.tryFind entityName entityMap
                    | None -> None
                | None -> None
            | _ -> failwith <| "Invalid entity address '" + acstring address + "'."

        static member private entityAdder (entity : Entity) (address : Entity Address) world =
            match address.Names with
            | [screenName; groupName; entityName] ->
                let (game, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (screen, groupMap) ->
                    match Map.tryFind groupName groupMap with
                    | Some (group, entityMap) ->
                        let entityMap = Map.add entityName entity entityMap
                        let groupMap = Map.add groupName (group, entityMap) groupMap
                        let screenMap = Map.add screenName (screen, groupMap) screenMap
                        { world with Simulants = (game, screenMap) }
                    | None -> failwith <| "Cannot add entity '" + acstring address + "' to non-existent group."
                | None -> failwith <| "Cannot add entity '" + acstring address + "' to non-existent screen."
            | _ -> failwith <| "Invalid entity address '" + acstring address + "'."

        static member private entityRemover (address : Entity Address) world =
            match address.Names with
            | [screenName; groupName; entityName] ->
                let (game, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (screen, groupMap) ->
                    match Map.tryFind groupName groupMap with
                    | Some (group, entityMap) ->
                        let entityMap = Map.remove entityName entityMap
                        let groupMap = Map.add groupName (group, entityMap) groupMap
                        let screenMap = Map.add screenName (screen, groupMap) screenMap
                        { world with Simulants = (game, screenMap) }
                    | None -> world
                | None -> world
            | _ -> failwith <| "Invalid entity address '" + acstring address + "'."

        /// Query that the world contains an entity at the given address.
        static member containsEntity address world =
            Option.isSome <| World.optEntityFinder address world

        /// Try to get an entity at the given address.
        static member getOptEntity address world =
            World.optEntityFinder address world

        /// Get an entity at the given address (failing with an exception otherwise), then
        /// transform it with the 'by' procudure.
        static member getEntityBy by address world =
            by ^ Option.get ^ World.getOptEntity address world

        /// Get an entity at the given address (failing with an exception otherwise).
        static member getEntity address world =
            World.getEntityBy id address world

        /// Get an entity with the given name in a group with the given address (failing with an
        /// exception if there isn't one).
        static member getEntityInGroup entityName groupAddress world =
            World.getEntity (World.gatoea groupAddress entityName) world

        /// Get an entity's address with the given name in the group with the given address
        /// (failing with an exception if there isn't one).
        static member getEntityAddressInGroup entityName groupAddress world =
            let address = World.gatoea groupAddress entityName
            ignore <| World.getEntity address world // ensure address is valid
            address

        static member private setEntityWithoutEvent entity address world =
            World.entityAdder entity address world

        static member private setOptEntityWithoutEvent optEntity address world =
            match optEntity with 
            | Some entity -> World.entityAdder entity address world
            | None -> World.entityRemover address world

        /// Set an entity at the given address (failing with an exception if one doesn't exist).
        static member setEntity entity address world =
            let oldEntity = Option.get <| World.optEntityFinder address world
            let world = World.entityAdder entity address world
            if entity.PublishChanges
            then World.publish4 { OldSimulant = oldEntity } (World.EntityChangeEventAddress ->>- address) address world
            else world
            
        /// Update an entity at the given address and the world with the given 'updater' procedure.
        static member updateEntityAndW updater address world =
            let entity = World.getEntity address world
            let (entity, world) = updater entity world
            World.setEntity entity address world

        /// Update an entity with the given 'updater' procedure at the given address.
        static member updateEntityW updater address world =
            World.updateEntityAndW (fun entity world -> (updater entity world, world)) address world
            
        /// Update an entity with the given 'updater' procedure at the given address.
        static member updateEntity updater address world =
            World.updateEntityW (fun entity _ -> updater entity) address world

        /// Update the world with the given 'updater' procedure that uses the entity at given
        /// address in its computation.
        static member updateByEntity updater address world : World =
            let entity = World.getEntity address world
            updater entity world

        /// Lens an entity at the given address.
        static member lensEntity address =
            { Get = World.getEntity address
              Set = fun entity -> World.setEntity entity address }

        /// Get all the entities at the given addresses as transformed them with the 'by'
        /// procedure.
        static member getEntitiesBy by addresses world =
            Seq.map (fun address -> by <| World.getEntity address world) addresses
            
        /// Get all the entities at the given addresses.
        static member getEntities addresses world =
            World.getEntitiesBy id addresses world

        /// Get all the entities in the group at the given address as mapped by their names.
        static member getEntityMapInGroup (groupAddress : Group Address) world =
            match groupAddress.Names with
            | [screenName; groupName] ->
                let (_, screenMap) = world.Simulants
                match Map.tryFind screenName screenMap with
                | Some (_, groupMap) ->
                    match Map.tryFind groupName groupMap with
                    | Some (_, entityMap) -> entityMap
                    | None -> Map.empty
                | None -> Map.empty
            | _ -> failwith <| "Invalid group address '" + acstring groupAddress + "'."

        /// Get all the entities in the group at the given address.
        static member getEntitiesInGroup (groupAddress : Group Address) world =
            Map.toValueSeq <| World.getEntityMapInGroup groupAddress world

        /// Get all the entity addresses in the group at the given address.
        static member getEntityAddressesInGroup groupAddress world =
            let entities = World.getEntitiesInGroup groupAddress world
            Seq.map (fun entity -> World.gatoea groupAddress entity.Name) entities

        /// Set the given entities to the respective addresses. Note, each address must already
        /// have an existing entity, otherwise will fail with an exception.
        static member setEntities entities addresses world =
            Seq.fold2 (fun world entity address -> World.setEntity entity address world) world entities addresses

        /// Set the given entities to the addresses as calculated by
        /// (fun entity -> gatoea groupAddress entity.Name) in the group with the given address.
        /// Note, each address must already have an existing entity, otherwise will fail with an
        /// exception.
        static member setEntitiesInGroup entities groupAddress world =
            Seq.fold (fun world (entity : Entity) -> World.setEntity entity (World.gatoea groupAddress entity.Name) world) world entities

        /// Update the entities at the given addresses and the world with the given 'updater' procedure.
        static member updateEntitiesAndW updater addresses world =
            Seq.fold (fun world address -> World.updateEntityAndW updater address world) world addresses

        /// Update the entities at the given addresses with the given 'updater' procedure.
        static member updateEntitiesW updater addresses world =
            Seq.fold (fun world address -> World.updateEntityW updater address world) world addresses

        /// Update the entities at the given addresses with the given 'updater' procedure.
        static member updateEntities updater addresses world =
            Seq.fold (fun world address -> World.updateEntity updater address world) world addresses

        /// Update all entities in the group at the given address and the world using the given 'updater' procedure.
        static member updateEntitiesInGroupAndW updater groupAddress world =
            let addresses = World.getEntityAddressesInGroup groupAddress world
            Seq.fold (fun world address -> World.updateEntityAndW updater address world) world addresses

        /// Update all entities in the group at the given address using the given 'updater'
        /// procedure. Also passes the current world value to the procedure.
        static member updateEntitiesInGroupW updater groupAddress world =
            let addresses = World.getEntityAddressesInGroup groupAddress world
            Seq.fold (fun world address -> World.updateEntityW updater address world) world addresses

        /// Update all entities in the group at the given address using the given 'updater' procedure.
        static member updateEntitiesInGroup updater groupAddress world =
            let addresses = World.getEntityAddressesInGroup groupAddress world
            Seq.fold (fun world address -> World.updateEntity updater address world) world addresses

        /// Lens the entities at the given addresses.
        static member lensEntities addresses =
            { Get = World.getEntities addresses
              Set = fun entities -> World.setEntities entities addresses }

        /// Lens all entities in the group at the given address.
        static member lensEntitiesInGroup groupAddress =
            { Get = World.getEntitiesInGroup groupAddress
              Set = fun entities -> World.setEntitiesInGroup entities groupAddress }

        /// Filter the given entity addresses by applying the 'pred' procedure to each entity at
        /// its respected address. Also passes the current world value to the procedure.
        static member filterEntityAddressesW pred addresses world =
            Seq.filter (fun address -> World.getEntityBy (fun entity -> pred entity world) address world) addresses
            
        /// Filter the given entity addresses by applying the 'pred' procedure to each entity at
        /// its respected address.
        static member filterEntityAddresses pred addresses world =
            World.filterEntityAddressesW (fun entity _ -> pred entity) addresses world

        static member private registerEntity entity address world =
            Entity.register entity address world

        static member private unregisterEntity entity address world =
            Entity.unregister entity address world

        /// Remove an entity from the world immediately. Can be dangerous if existing in-flight
        /// subscriptions depend on the entity's existence. Use with caution.
        static member removeEntityImmediate (address : Entity Address) world =
            let world = World.publish4 () (World.EntityRemovingEventAddress ->>- address) address world
            match World.getOptEntity address world with
            | Some entity ->
                let (entity, world) = World.unregisterEntity entity address world
                let world = World.setOptEntityWithoutEvent None address world
                (Some entity, world)
            | None -> (None, world)

        /// Remove an entity from the world on the next tick. Use this rather than
        /// removeEntityImmediate unless you need the latter's specific behavior.
        static member removeEntity address world =
            let task =
                { ScheduledTime = world.State.TickTime
                  Operation = fun world -> snd <| World.removeEntityImmediate address world }
            World.addTask task world
            
        /// Remove multiple entities from the world immediately. Can be dangerous if existing
        /// in-flight subscriptions depend on any of the entities' existences. Use with caution.
        static member removeEntitiesImmediate addresses world =
            List.foldBack
                (fun address (entities, world) ->
                    let (entity, world) = World.removeEntityImmediate address world
                    (entity :: entities, world))
                (List.ofSeq addresses)
                ([], world)
                
        /// Remove multiple entities from the world. Use this rather than removeEntitiesImmediate
        /// unless you need the latter's specific behavior.
        static member removeEntities addresses world =
            snd <| World.removeEntitiesImmediate addresses world

        /// Add an entity at the given address to the world.
        static member addEntity entity address world =
            if not <| World.containsEntity address world then
                let world = World.setEntityWithoutEvent entity address world
                let (entity, world) = World.registerEntity entity address world
                let world = World.publish4 () (World.EntityAddEventAddress ->>- address) address world
                (entity, world)
            else failwith <| "Adding an entity that the world already contains at address '" + acstring address + "'."

        /// Add multiple entities to the group at the given address.
        static member addEntities entities (groupAddress : Group Address) world =
            Map.fold
                (fun (entities, world) entityName entity ->
                    let entityAddress = World.gatoea groupAddress entityName
                    let (entity, world) = World.addEntity entity entityAddress world
                    (Map.add entityName entity entities, world))
                (Map.empty, world)
                entities

        (* Group *)

        static member private optGroupFinder (address : Group Address) world =
            match address.Names with
            | [screenName; groupName] ->
                let (_, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (_, groupMap) ->
                    match Map.tryFind groupName groupMap with
                    | Some (group, _) -> Some group
                    | None -> None
                | None -> None
            | _ -> failwith <| "Invalid group address '" + acstring address + "'."

        static member private groupAdder (group : Group) (address : Group Address) world =
            match address.Names with
            | [screenName; groupName] ->
                let (game, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (screen, groupMap) ->
                    match Map.tryFind groupName groupMap with
                    | Some (_, entityMap) ->
                        let groupMap = Map.add groupName (group, entityMap) groupMap
                        let screenMap = Map.add screenName (screen, groupMap) screenMap
                        { world with Simulants = (game, screenMap) }
                    | None ->
                        let groupMap = Map.add groupName (group, Map.empty) groupMap
                        let screenMap = Map.add screenName (screen, groupMap) screenMap
                        { world with Simulants = (game, screenMap) }
                | None -> failwith <| "Cannot add group '" + acstring address + "' to non-existent screen."
            | _ -> failwith <| "Invalid group address '" + acstring address + "'."

        static member private groupRemover (address : Group Address) world =
            match address.Names with
            | [screenName; groupName] ->
                let (game, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (screen, groupMap) ->
                    match Map.tryFind groupName groupMap with
                    | Some (_, entityMap) ->
                        if Map.isEmpty entityMap then
                            let groupMap = Map.remove groupName groupMap
                            let screenMap = Map.add screenName (screen, groupMap) screenMap
                            { world with Simulants = (game, screenMap) }
                        else failwith <| "Cannot remove group " + acstring address + ", which still contains entities."
                    | None -> world
                | None -> world
            | _ -> failwith <| "Invalid group address '" + acstring address + "'."

        /// Query that the world contains a group at the given address.
        static member containsGroup address world =
            Option.isSome <| World.optGroupFinder address world

        /// Try to get a group at the given address.
        static member getOptGroup address world =
            World.optGroupFinder address world

        /// Get a group at the given address (failing with an exception otherwise), then
        /// transform it with the 'by' procudure.
        static member getGroupBy by address world =
            by ^ Option.get ^ World.getOptGroup address world

        /// Get a group at the given address (failing with an exception otherwise).
        static member getGroup address world =
            World.getGroupBy id address world

        /// Get a group with the given name in a screen with the given address (failing with an
        /// exception otherwise).
        static member getGroupInScreen groupName screenAddress world =
            World.getGroup (World.satoga screenAddress groupName) world

        /// Get a group's address with the given name in a group with the given address (failing
        /// with an exception otherwise).
        static member getGroupAddressInScreen groupName screenAddress world =
            let address = World.satoga screenAddress groupName
            ignore <| World.getGroup address world // ensure address is valid
            address

        static member private setGroupWithoutEvent group address world =
            World.groupAdder group address world

        static member private setOptGroupWithoutEvent optGroup address world =
            match optGroup with 
            | Some group -> World.groupAdder group address world
            | None -> World.groupRemover address world

        /// Set a group at the given address (failing with an exception if one doesn't exist).
        static member setGroup group address world =
            let oldGroup = Option.get <| World.optGroupFinder address world
            let world = World.groupAdder group address world
            if group.PublishChanges
            then World.publish4 { OldSimulant = oldGroup } (World.GroupChangeEventAddress ->>- address) address world
            else world

        /// Update a group at the given address and the world with the given 'updater' procedure.
        static member updateGroupAndW updater address world =
            let group = World.getGroup address world
            let (group, world) = updater group world
            World.setGroup group address world

        /// Update a group with the given 'updater' procedure at the given address.
        static member updateGroupW updater address world =
            World.updateGroupAndW (fun group world -> (updater group world, world)) address world
        
        /// Update a group with the given 'updater' procedure at the given address.
        static member updateGroup updater address world =
            World.updateGroupW (fun group _ -> updater group) address world
            
        /// Update the world with the given 'updater' procedure that uses the group at given
        /// address in its computation.
        static member updateByGroup updater address world : World =
            let group = World.getGroup address world
            updater group world

        /// Lens a group at the given address.
        static member lensGroup address =
            { Get = World.getGroup address
              Set = fun group -> World.setGroup group address }

        /// Try to get a group hierarchy (that is, a group with a map to all of its entities) at
        /// the given address.
        static member getOptGroupHierarchy address world =
            match World.getOptGroup address world with
            | Some group ->
                let entityMap = World.getEntityMapInGroup address world
                Some (group, entityMap)
            | None -> None

        /// Get a group hierarchy (that is, a group with a map to all of its entities) at the given
        /// address (failing with an exception if there isn't one).
        static member getGroupHierarchy address world =
            Option.get <| World.getOptGroupHierarchy address world

        /// Get the group hierarches at the given addresses.
        static member getGroupHierarchies addresses world =
            Seq.map (fun address -> World.getGroupHierarchy address world) addresses
            
        /// Get the groups at the given addresses as transformed them with the 'by'
        /// procedure.
        static member getGroupsBy by addresses world =
            Seq.map (fst >> by) <| World.getGroupHierarchies addresses world

        /// Get the groups at the given addresses.
        static member getGroups addresses world =
            World.getGroupsBy id addresses world

        /// Get all the groups in the screen at the given address as mapped by their names.
        static member getGroupMapInScreen (screenAddress : Screen Address) world =
            match screenAddress.Names with
            | [screenName] ->
                let (_, screenMap) = world.Simulants
                match Map.tryFind screenName screenMap with
                | Some (_, groupMap) -> groupMap
                | None -> Map.empty
            | _ -> failwith <| "Invalid screen address '" + acstring screenAddress + "'."

        /// Get all the groups in the screen at the given address.
        static member getGroupsInScreen screenAddress world =
            let groupHierarchies = World.getGroupMapInScreen screenAddress world
            Map.toValueSeqBy fst groupHierarchies

        /// Get all the group addresses in the screen at the given address.
        static member getGroupAddressesInScreen screenAddress world =
            let groupHierarchies = World.getGroupMapInScreen screenAddress world
            Map.toValueSeqBy (fun (group : Group, _) -> World.satoga screenAddress group.Name) groupHierarchies

        /// Set the groups at the given addresses.
        static member setGroups groups addresses world =
            Seq.fold2 (fun world group address -> World.setGroup group address world) world groups addresses
        
        /// Set the groups in the screen at the given addresses.
        static member setGroupsInScreen groups screenAddress world =
            Seq.fold (fun world (group : Group) -> World.setGroup group (World.satoga screenAddress group.Name) world) world groups

        /// Update the groups at the given addresses and the world with the given 'updater' procedure.
        static member updateGroupsAndW updater addresses world =
            Seq.fold (fun world address -> World.updateGroupAndW updater address world) world addresses

        /// Update the groups at the given addresses with the given 'updater' procedure.
        static member updateGroupsW updater addresses world =
            Seq.fold (fun world address -> World.updateGroupW updater address world) world addresses
        
        /// Update the groups at the given addresses with the given 'updater' procedure.
        static member updateGroups updater addresses world =
            Seq.fold (fun world address -> World.updateGroup updater address world) world addresses

        /// Update all groups in the screen at the given address and the world using the given 'updater'
        static member updateGroupsInScreenAndW updater screenAddress world =
            let addresses = World.getGroupAddressesInScreen screenAddress world
            Seq.fold (fun world address -> World.updateGroupAndW updater address world) world addresses

        /// Update all groups in the screen at the given address using the given 'updater'
        static member updateGroupsInScreenW updater screenAddress world =
            let addresses = World.getGroupAddressesInScreen screenAddress world
            Seq.fold (fun world address -> World.updateGroupW updater address world) world addresses

        /// Update all groups in the screen at the given address using the given 'updater' procedure.
        static member updateGroupsInScreen updater screenAddress world =
            let addresses = World.getGroupAddressesInScreen screenAddress world
            Seq.fold (fun world address -> World.updateGroup updater address world) world addresses

        /// Lens the groups at the given addresses.
        static member lensGroups addresses =
            { Get = World.getGroups addresses
              Set = fun groups -> World.setGroups groups addresses }

        /// Lens all groups in the screen at the given address.
        static member lensGroupsInScreen screenAddress =
            { Get = World.getGroupsInScreen screenAddress
              Set = fun groups -> World.setGroupsInScreen groups screenAddress }

        /// Filter the given group addresses by applying the 'pred' procedure to each group at
        /// its respected address. Also passes the current world value to the procedure.
        static member filterGroupAddressesW pred addresses world =
            Seq.filter (fun address -> World.getGroupBy (fun group -> pred group world) address world) addresses
            
        /// Filter the given group addresses by applying the 'pred' procedure to each group at
        /// its respected address.
        static member filterGroupAddresses pred addresses world =
            World.filterGroupAddressesW (fun group _ -> pred group) addresses world

        static member private registerGroup group address world =
            Group.register group address world

        static member private unregisterGroup group address world =
            Group.unregister group address world
            
        /// Remove a group from the world immediately. Can be dangerous if existing in-flight
        /// subscriptions depend on the group's existence. Use with caution.
        static member removeGroupImmediate address world =
            let world = World.publish4 () (World.GroupRemovingEventAddress ->>- address) address world
            match World.getOptGroup address world with
            | Some group ->
                let (group, world) = World.unregisterGroup group address world
                let entityAddresses = World.getEntityAddressesInGroup address world
                let world = snd <| World.removeEntitiesImmediate entityAddresses world
                let world = World.setOptGroupWithoutEvent None address world
                (Some group, world)
            | None -> (None, world)
            
        /// Remove a group from the world on the next tick. Use this rather than
        /// removeEntityImmediate unless you need the latter's specific behavior.
        static member removeGroup address world =
            let task =
                { ScheduledTime = world.State.TickTime
                  Operation = fun world -> snd <| World.removeGroupImmediate address world }
            World.addTask task world
            
        /// Remove multiple groups from the world immediately. Can be dangerous if existing
        /// in-flight subscriptions depend on any of the groups' existences. Use with caution.
        static member removeGroupsImmediate addresses world =
            List.foldBack
                (fun address (groups, world) ->
                    let (group, world) = World.removeGroupImmediate address world
                    (group :: groups, world))
                (List.ofSeq addresses)
                ([], world)
                
        /// Remove multiple groups from the world. Use this rather than removeEntitiesImmediate
        /// unless you need the latter's specific behavior.
        static member removeGroups addresses world =
            snd <| World.removeGroupsImmediate addresses world

        /// Add a group at the given address to the world.
        static member addGroup groupHierarchy address world =
            let (group, entities) = groupHierarchy
            if not <| World.containsGroup address world then
                let world = World.setGroupWithoutEvent group address world
                let world = snd <| World.addEntities entities address world
                let (group, world) = World.registerGroup group address world
                let world = World.publish4 () (World.GroupAddEventAddress ->>- address) address world
                (group, world)
            else failwith <| "Adding a group that the world already contains at address '" + acstring address + "'."

        /// Add multiple groups to the screen at the given address.
        static member addGroups groupHierarchies (screenAddress : Screen Address) world =
            Map.fold
                (fun (groups, world) groupName groupHierarchy ->
                    let (group, world) = World.addGroup groupHierarchy (World.satoga screenAddress groupName) world
                    (group :: groups, world))
                ([], world)
                groupHierarchies

        (* Screen *)

        static member private optScreenFinder (address : Screen Address) world =
            match address.Names with
            | [screenName] ->
                let (_, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (screen, _) -> Some screen
                | None -> None
            | _ -> failwith <| "Invalid screen address '" + acstring address + "'."

        static member private screenAdder (screen : Screen) (address : Screen Address) world =
            match address.Names with
            | [screenName] ->
                let (game, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (_, groupMap) ->
                    let screenMap = Map.add screenName (screen, groupMap) screenMap
                    { world with Simulants = (game, screenMap) }
                | None ->
                    let screenMap = Map.add screenName (screen, Map.empty) screenMap
                    { world with Simulants = (game, screenMap) }
            | _ -> failwith <| "Invalid screen address '" + acstring address + "'."

        static member private screenRemover (address : Screen Address) world =
            match address.Names with
            | [screenName] ->
                let (game, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (_, groupMap) ->
                    if Map.isEmpty groupMap then
                        let screenMap = Map.remove screenName screenMap
                        { world with Simulants = (game, screenMap) }
                    else failwith <| "Cannot remove screen " + acstring address + ", which still contains groups."
                | None -> world
            | _ -> failwith <| "Invalid screen address '" + acstring address + "'."

        /// Query that the world contains a group at the given address.
        static member containsScreen address world =
            Option.isSome <| World.optScreenFinder address world

        /// Try to get a group at the given address.
        static member getOptScreen address world =
            World.optScreenFinder address world

        /// Get a group at the given address (failing with an exception otherwise), then
        /// transform it with the 'by' procudure.
        static member getScreenBy by address world =
            by ^ Option.get ^ World.getOptScreen address world

        /// Get a group at the given address (failing with an exception otherwise).
        static member getScreen address world =
            World.getScreenBy id address world

        /// Try to get a screen hierarchy (that is, a screen with a map to all of its group
        /// hierarchies) at the given address.
        static member getOptScreenHierarchy address world =
            match World.getOptScreen address world with
            | Some screen ->
                let groupMap = World.getGroupMapInScreen address world
                Some (screen, groupMap)
            | None -> None
        
        /// Get a screen hierarchy (that is, a screen with a map to all of its group hierarchies)
        /// at the given address (failing with an exception if there isn't one).
        static member getScreenHierarchy address world =
            Option.get <| World.getOptScreenHierarchy address world

        /// Get a screen's address with the given name (failing with an exception if there isn't
        /// one).
        static member getScreenAddress screenName world =
            let address = ntoa<Screen> screenName
            ignore <| World.getScreen address world // ensures address is valid
            address

        static member private setScreenWithoutEvent screen address world =
            World.screenAdder screen address world

        static member private setOptScreenWithoutEvent optScreen address world =
            match optScreen with 
            | Some screen -> World.screenAdder screen address world
            | None -> World.screenRemover address world

        /// Set a screen at the given address (failing with an exception if one doesn't exist).
        static member setScreen screen address world =
            let oldScreen = Option.get <| World.optScreenFinder address world
            let world = World.screenAdder screen address world
            if screen.PublishChanges
            then World.publish4 { OldSimulant = oldScreen } (World.ScreenChangeEventAddress ->>- address) address world
            else world

        /// Update a screen at the given address with the given 'updater' procedure.
        static member updateScreenAndW updater address world =
            let screen = World.getScreen address world
            let (screen, world) = updater screen world
            World.setScreen screen address world

        /// Update a screen with the given 'updater' procedure at the given address.
        static member updateScreenW updater address world =
            World.updateScreenAndW (fun screen world -> (updater screen world, world)) address world

        /// Update a screen with the given 'updater' procedure at the given address.
        static member updateScreen updater address world =
            World.updateScreenW (fun screen _ -> updater screen) address world

        /// Update the world with the given 'updater' procedure that uses the screen at given
        /// address in its computation.
        static member updateByScreen updater address world : World =
            let screen = World.getScreen address world
            updater screen world

        /// Lens a screen at the given address.
        static member lensScreen address =
            { Get = World.getScreen address
              Set = fun screen -> World.setScreen screen address }

        /// Get the screen hierarches at the given addresses.
        static member getScreenHierarchies addresses world =
            Seq.map (fun address -> World.getScreenHierarchy address world) addresses

        /// Get the screens at the given addresses as transformed them with the 'by'
        /// procedure.
        static member getScreensBy by addresses world =
            Seq.map (fst >> by) <| World.getScreenHierarchies addresses world

        /// Get the screens at the given addresses.
        static member getScreens addresses world =
            World.getScreensBy id addresses world

        /// Get all the screens in the game as mapped by their names.
        static member getScreenMap world =
            snd world.Simulants

        /// Get all the screens in the game.
        static member getScreensInGame world =
            Map.toValueSeqBy fst <| World.getScreenMap world
            
        /// Get the addresses of all the world's screens.
        static member getScreenAddresses world =
            Map.fold (fun addresses screenName _ -> ntoa<Screen> screenName :: addresses) [] (World.getScreenMap world)

        /// Set the screens at the given addresses.
        static member setScreens screens addresses world =
            Seq.fold2 (fun world screen address -> World.setScreen screen address world) world screens addresses
        
        /// Set all the screens in the game.
        static member setScreensInGame screens world =
            Seq.fold (fun world (screen : Screen) -> World.setScreen screen (ntoa screen.Name) world) world screens

        /// Update the screens at the given addresses and the world with the given 'updater' procedure.
        static member updateScreensAndW updater addresses world =
            Seq.fold (fun world address -> World.updateScreenAndW updater address world) world addresses

        /// Update the screens at the given addresses with the given 'updater' procedure.
        static member updateScreensW updater addresses world =
            Seq.fold (fun world address -> World.updateScreenW updater address world) world addresses
        
        /// Update the screens at the given addresses with the given 'updater' procedure.
        static member updateScreens updater addresses world =
            Seq.fold (fun world address -> World.updateScreen updater address world) world addresses

        /// Lens the screens at the given addresses.
        static member lensScreens addresses =
            { Get = World.getScreens addresses
              Set = fun screens -> World.setScreens screens addresses }

        /// Lens all screens in the game at the given address.
        static member lensScreensInGame =
            { Get = World.getScreensInGame
              Set = fun screens -> World.setScreensInGame screens }

        /// Filter the given screen addresses by applying the 'pred' procedure to each screen at
        /// its respected address. Also passes the current world value to the procedure.
        static member filterScreenAddressesW pred addresses world =
            Seq.filter (fun address -> World.getScreenBy (fun screen -> pred screen world) address world) addresses

        /// Filter the given screen addresses by applying the 'pred' procedure to each screen at
        /// its respected address.
        static member filterScreenAddresses pred addresses world =
            World.filterScreenAddressesW (fun screen _ -> pred screen) addresses world

        static member private registerScreen screen address world =
            Screen.register screen address world

        static member private unregisterScreen screen address world =
            Screen.unregister screen address world

        /// Remove a screen from the world immediately. Can be dangerous if existing in-flight
        /// subscriptions depend on the screen's existence. Use with caution.
        static member removeScreenImmediate address world =
            let world = World.publish4 () (World.ScreenRemovingEventAddress ->>- address) address world
            match World.getOptScreen address world with
            | Some screen ->
                let (screen, world) = World.unregisterScreen screen address world
                let groupAddresses = World.getGroupAddressesInScreen address world
                let world = snd <| World.removeGroupsImmediate groupAddresses world
                let world = World.setOptScreenWithoutEvent None address world
                (Some screen, world)
            | None -> (None, world)

        /// Remove a screen from the world on the next tick. Use this rather than
        /// removeEntityImmediate unless you need the latter's specific behavior.
        static member removeScreen address world =
            let task =
                { ScheduledTime = world.State.TickTime
                  Operation = fun world -> snd <| World.removeScreenImmediate address world }
            World.addTask task world

        /// Add a screen at the given address to the world.
        static member addScreen screenHierarchy address world =
            let (screen, groupHierarchies) = screenHierarchy
            if not <| World.containsScreen address world then
                let world = World.setScreenWithoutEvent screen address world
                let world = snd <| World.addGroups groupHierarchies address world
                let (screen, world) = World.registerScreen screen address world
                let world = World.publish4 () (World.ScreenAddEventAddress ->>- address) address world
                (screen, world)
            else failwith <| "Adding a screen that the world already contains at address '" + acstring address + "'."

        (* Game *)

        /// Get the game, then transform it with the 'by' procudure.
        static member getGameBy by world =
            by <| fst world.Simulants

        /// Get the game.
        static member getGame world =
            World.getGameBy id world

        /// Get the game hierarchy.
        static member getGameHierarchy world =
            let game = World.getGame world
            let screenMap = World.getScreenMap world
            (game, screenMap)

        /// Set the game.
        static member setGame game world =
            let oldGame = World.getGame world
            let screenMap = World.getScreenMap world
            let world = { world with Simulants = (game, screenMap) }
            if game.PublishChanges
            then World.publish4 { OldSimulant = oldGame } (World.GameChangeEventAddress ->>- World.GameAddress) World.GameAddress world
            else world

        /// Update the game with the given 'updater' procedure.
        static member updateGameAndW updater world =
            let game = World.getGame world
            let (game, world) = updater game world
            World.setGame game world

        /// Update the game with the given 'updater' procedure.
        static member updateGameW updater world =
            World.updateGameAndW (fun game world -> (updater game world, world)) world

        /// Update the game with the given 'updater' procedure.
        static member updateGame updater world =
            World.updateGameW (fun game _ -> updater game) world

        /// Update the world with the given 'updater' procedure that uses the game in its
        /// computation.
        static member updateByGame updater world : World =
            let game = World.getGame world
            updater game world

        /// Lens the game.
        static member lensGame =
            { Get = World.getGame
              Set = World.setGame }

        (* Simulant *)

        /// Try to get a simulant at the given address.
        static member getOptSimulant<'a when 'a :> Simulant> (address : 'a Address) world =
            match address.Names with
            | [] -> World.getGame world :> Simulant :?> 'a |> Some
            | [_] -> World.getOptScreen (World.atosa address) world |> Option.map (fun s -> s :> Simulant :?> 'a)
            | [_; _] -> World.getOptGroup (World.atoga address) world |> Option.map (fun g -> g :> Simulant :?> 'a)
            | [_; _; _] -> World.getOptEntity (World.atoea address) world |> Option.map (fun e -> e :> Simulant :?> 'a)
            | _ -> failwith <| "Invalid simulant address '" + acstring address + "'."

        static member private getOptSimulantForPublishing (address : Simulant Address) world =
            World.getOptSimulant address world

        /// Query that the world contains a simulant at the given address.
        static member containsSimulant address world =
            Option.isSome <| World.getOptSimulant address world

        /// Get a simulant at the given address (failing with an exception otherwise), then
        /// transform it with the 'by' procudure.
        static member getSimulantBy by address world =
            by ^ Option.get ^ World.getOptSimulant address world

        /// Get a simulant at the given address (failing with an exception otherwise).
        static member getSimulant address world =
            World.getSimulantBy id address world

        /// Set a simulant at the given address (failing with an exception if one doesn't exist).
        static member setSimulant<'a when 'a :> Simulant> (simulant : 'a) (address : 'a Address) world =
            match address.Names with
            | [] -> World.setGame (simulant :> obj :?> Game) world
            | [_] -> World.setScreen (simulant :> obj :?> Screen) (Address.changeType<'a, Screen> address) world
            | [_; _] -> World.setGroup (simulant :> obj :?> Group) (Address.changeType<'a, Group> address) world
            | [_; _; _] -> World.setEntity (simulant :> obj :?> Entity) (Address.changeType<'a, Entity> address) world
            | _ -> failwith <| "Invalid simulant address '" + acstring address + "'."

        /// Update a simulant at the given address with the given 'updater' procedure.
        static member updateSimulantAndW updater address world =
            let simulant = World.getSimulant address world
            let (simulant, world) = updater simulant world
            World.setSimulant simulant address world

        /// Update a simulant with the given 'updater' procedure at the given address.
        static member updateSimulantW updater address world =
            World.updateSimulantAndW (fun simulant world -> (updater simulant world, world)) address world

        /// Update a simulant with the given 'updater' procedure at the given address.
        static member updateSimulant updater address world =
            World.updateSimulantW (fun simulant _ -> updater simulant) address world

        /// Update the world with the given 'updater' procedure that uses the simulant at given
        /// address in its computation.
        static member updateBySimulant updater address world : World =
            let simulant = World.getSimulant address world
            updater simulant world

        /// Update a lensed value at the given address and the world with the given 'updater' procedure.
        static member updateLensedAndW expr lens world : World =
            Lens.update expr (lens @-> World.lens) world

        /// Update a lensed value with the given 'updater' procedure at the given address.
        static member updateLensedW expr lens world : World =
            Lens.updateS expr lens world

        /// Update a lensed value with the given 'updater' procedure at the given address.
        static member updateLensed expr lens world : World =
            World.updateLensedW (fun lensed _ -> expr lensed) lens world

        /// Update the world with the given 'updater' procedure that uses the lensed value at given
        /// address in its computation.
        static member updateByLensed expr lens world : World =
            expr (Lens.get world lens) world

    and [<NoEquality; NoComparison>] EntityProxy =
        { EntityAddress : Entity Address }
        member this.GetId world = (World.getEntity this.EntityAddress world).Id
        member this.GetName world = (World.getEntity this.EntityAddress world).Name
        member this.GetCreationTimeNp world = (World.getEntity this.EntityAddress world).CreationTimeNp
        member this.GetDispatcherNp world = (World.getEntity this.EntityAddress world).DispatcherNp
        member this.GetFacetNames world = (World.getEntity this.EntityAddress world).FacetNames
        member this.GetFacetsNp world = (World.getEntity this.EntityAddress world).FacetsNp
        member this.GetPosition world = (World.getEntity this.EntityAddress world).Position
        member this.SetPosition value world = World.updateEntity (fun entity -> { entity with Position = value}) this.EntityAddress world
        member this.GetDepth world = (World.getEntity this.EntityAddress world).Depth
        member this.SetDepth value world = World.updateEntity (fun entity -> { entity with Depth = value}) this.EntityAddress world
        member this.GetSize world = (World.getEntity this.EntityAddress world).Size
        member this.SetSize value world = World.updateEntity (fun entity -> { entity with Size = value}) this.EntityAddress world
        member this.GetRotation world = (World.getEntity this.EntityAddress world).Rotation
        member this.SetRotation value world = World.updateEntity (fun entity -> { entity with Rotation = value}) this.EntityAddress world
        member this.GetVisible world = (World.getEntity this.EntityAddress world).Visible
        member this.SetVisible value world = World.updateEntity (fun entity -> { entity with Visible = value}) this.EntityAddress world
        member this.GetViewType world = (World.getEntity this.EntityAddress world).ViewType
        member this.SetViewType value world = World.updateEntity (fun entity -> { entity with ViewType = value}) this.EntityAddress world
        member this.GetPublishChanges world = (World.getEntity this.EntityAddress world).PublishChanges
        member this.SetPublishChanges value world = World.updateEntity (fun entity -> { entity with PublishChanges = value}) this.EntityAddress world
        member this.GetPersistent world = (World.getEntity this.EntityAddress world).Persistent
        member this.SetPersistent value world = World.updateEntity (fun entity -> { entity with Persistent = value}) this.EntityAddress world
        member this.GetOptOverlayName world = (World.getEntity this.EntityAddress world).OptOverlayName
        member this.SetOptOverlayName value world = World.updateEntity (fun entity -> { entity with OptOverlayName = value}) this.EntityAddress world
        member this.GetXtension world = (World.getEntity this.EntityAddress world).Xtension
        member this.SetXtension xtension world = World.updateEntity (fun entity -> { entity with Xtension = xtension}) this.EntityAddress world
        member this.UpdateXtension updater world =
            let xtension = this.GetXtension world
            let xtension = updater xtension
            this.SetXtension xtension world

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