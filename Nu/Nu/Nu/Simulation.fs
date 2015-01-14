// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2014.

namespace Nu
open System
open System.Collections.Generic
open System.ComponentModel
open System.Reflection
open System.Xml
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