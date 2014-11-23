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

    /// The state of one of a screen's transitions.
    type [<CLIMutable; StructuralEquality; NoComparison>] Transition =
        { TransitionLifetime : int64
          TransitionType : TransitionType
          OptDissolveImage : Image option }

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

    /// The data for an entity change event.
    and [<StructuralEquality; NoComparison>] EntityChangeData =
        { OldEntity : Entity }

    /// An event used by Nu's purely functional event system.
    and [<ReferenceEquality>] 'a Event =
        { Address : 'a Address
          SubscriberAddress : obj Address
          PublisherAddress : obj Address
          Data : 'a }

    /// Describes whether an event has been resolved or should cascade.
    and EventHandling =
        | Resolve
        | Cascade

    /// Describes a game event subscription.
    and 'd Subscription = 'd Event -> World -> EventHandling * World

    /// Describes a game event subscription that can be boxed / unboxed.
    and BoxableSubscription = obj -> World -> EventHandling * World

    /// An entry into the world's subscription map.
    and SubscriptionEntry = Guid * obj Address * obj

    /// A map of event subscriptions.
    and SubscriptionEntries = Map<obj Address, SubscriptionEntry list>

    /// Abstracts over a subscription sorting procedure.
    and SubscriptionSorter = SubscriptionEntry list -> World -> SubscriptionEntry list

    /// A map of subscription keys to unsubscription data.
    and UnsubscriptionEntries = Map<Guid, obj Address * obj Address>

    /// A task to be completed at the given time, with time being represented by the world's tick
    /// field.
    and [<ReferenceEquality>] Task =
        { ScheduledTime : int64
          Operation : World -> World }

    /// The default dispatcher for entities.
    and EntityDispatcher () =

        static member FieldDefinitions =
            [define? Position Vector2.Zero
             define? Depth 0.0f
             define? Size DefaultEntitySize
             define? Rotation 0.0f
             define? Visible true
             define? ViewType Relative
             define? Persistent true]

        abstract member Register : Entity Address * Entity * World -> Entity * World
        default dispatcher.Register (_, entity, world) = (entity, world)

        abstract member Unregister : Entity Address * Entity * World -> Entity * World
        default dispatcher.Unregister (_, entity, world) = (entity, world)

        abstract member PropagatePhysics : Entity Address * Entity * World -> World
        default dispatcher.PropagatePhysics (_, _, world) = world

        abstract member GetRenderDescriptors : Entity * World -> RenderDescriptor list
        default dispatcher.GetRenderDescriptors (_, _) = []

        abstract member GetQuickSize : Entity * World -> Vector2
        default dispatcher.GetQuickSize (_, _) = Vector2.One

        abstract member GetPickingPriority : Entity * World -> single
        default dispatcher.GetPickingPriority (entity, _) = entity.Depth

    /// The default dispatcher for groups.
    and GroupDispatcher () =

        static member FieldDefinitions =
            [define? Persistent true]

        abstract member Register : Group Address * Group * World -> Group * World
        default dispatcher.Register (_, group, world) = (group, world)

        abstract member Unregister : Group Address * Group * World -> Group * World
        default dispatcher.Unregister (_, group, world) = (group, world)

    /// The default dispatcher for screens.
    and ScreenDispatcher () =

        static member FieldDefinitions =
            [define? Persistent true]

        abstract member Register : Screen Address * Screen * World -> Screen * World
        default dispatcher.Register (_, screen, world) = (screen, world)

        abstract member Unregister : Screen Address * Screen * World -> Screen * World
        default dispatcher.Unregister (_, screen, world) = (screen, world)

    /// The default dispatcher for games.
    and GameDispatcher () =

        abstract member Register : Game * World -> Game * World
        default dispatcher.Register (game, world) = (game, world)

    /// Dynamically augments an entity's behavior in a composable way.
    and Facet () =

        abstract member Register : Entity Address * Entity * World -> Entity * World
        default facet.Register (address, entity, world) =
            let world = facet.RegisterPhysics (address, entity, world)
            (entity, world)

        abstract member Unregister : Entity Address * Entity * World -> Entity * World
        default facet.Unregister (address, entity, world) =
            let world = facet.UnregisterPhysics (address, entity, world)
            (entity, world)

        abstract member RegisterPhysics : Entity Address * Entity * World -> World
        default facet.RegisterPhysics (_, _, world) = world

        abstract member UnregisterPhysics : Entity Address * Entity * World -> World
        default facet.UnregisterPhysics (_, _, world) = world

        abstract member PropagatePhysics : Entity Address * Entity * World -> World
        default facet.PropagatePhysics (_, _, world) = world

        abstract member GetRenderDescriptors : Entity * World -> RenderDescriptor list
        default facet.GetRenderDescriptors (_, _) = []

        abstract member GetQuickSize : Entity * World -> Vector2
        default facet.GetQuickSize (_, _) = DefaultEntitySize

    /// The type around which the whole game engine is based! Used in combination with dispatchers
    /// to implement things like buttons, characters, blocks, and things of that sort.
    /// TODO: now that there are field descriptors, consider making their persistence configurable
    /// with data instead of name-suffixing
    and [<CLIMutable; StructuralEquality; NoComparison>] Entity =
        { Id : Guid
          Name : string
          Position : Vector2 // NOTE: will become a Vector3 if Nu gets 3d capabilities
          Depth : single // NOTE: will become part of position if Nu gets 3d capabilities
          Size : Vector2 // NOTE: will become a Vector3 if Nu gets 3d capabilities
          Rotation : single // NOTE: will become a Vector3 if Nu gets 3d capabilities
          Visible : bool
          ViewType : ViewType
          Persistent : bool
          CreationTimeNp : DateTime // just needed for ordering writes to reduce diff volumes
          DispatcherNp : EntityDispatcher
          FacetNames : string list
          FacetsNp : Facet list
          OptOverlayName : string option
          Xtension : Xtension }

        static member (?) (this : Entity, memberName) =
            Xtension.(?) (this.Xtension, memberName)

        static member (?<-) (this : Entity, memberName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
            { this with Xtension = xtension }

    /// Forms logical groups of entities.
    and [<CLIMutable; StructuralEquality; NoComparison>] Group =
        { Id : Guid
          Name : string
          Persistent : bool
          CreationTimeNp : DateTime
          DispatcherNp : GroupDispatcher
          Xtension : Xtension }

        static member (?) (this : Group, memberName) =
            Xtension.(?) (this.Xtension, memberName)

        static member (?<-) (this : Group, memberName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
            { this with Xtension = xtension }

    /// The screen type that allows transitioning to and fro other screens, and also hosts the
    /// currently interactive groups of entities.
    and [<CLIMutable; StructuralEquality; NoComparison>] Screen =
        { Id : Guid
          Name : string
          ScreenStateNp : ScreenState
          TransitionTicksNp : int64
          Incoming : Transition
          Outgoing : Transition
          Persistent : bool
          CreationTimeNp : DateTime
          DispatcherNp : ScreenDispatcher
          Xtension : Xtension }

        static member (?) (this : Screen, memberName) =
            Xtension.(?) (this.Xtension, memberName)

        static member (?<-) (this : Screen, memberName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
            { this with Xtension = xtension }

    /// The game type that hosts the various screens used to navigate through a game.
    and [<CLIMutable; StructuralEquality; NoComparison>] Game =
        { Id : Guid
          Name : string
          OptSelectedScreenAddress : Screen Address option
          CreationTimeNp : DateTime
          DispatcherNp : GameDispatcher
          Xtension : Xtension }

        static member (?) (this : Game, memberName) =
            Xtension.(?) (this.Xtension, memberName)

        static member (?<-) (this : Game, memberName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
            { this with Xtension = xtension }

    /// Abstracts over the simulation types (Game, Screen, Group, Entity).
    and [<StructuralEquality; NoComparison>] Simulant =
        | Game of Game
        | Screen of Screen
        | Group of Group
        | Entity of Entity

    /// The world's components.
    and [<ReferenceEquality>] Components =
        { EntityDispatchers : Map<string, EntityDispatcher>
          GroupDispatchers : Map<string, GroupDispatcher>
          ScreenDispatchers : Map<string, ScreenDispatcher>
          GameDispatchers : Map<string, GameDispatcher>
          Facets : Map<string, Facet> }

    /// The world's subsystems.
    and [<ReferenceEquality>] Subsystems =
        { AudioPlayer : IAudioPlayer
          Renderer : IRenderer
          Integrator : IIntegrator }

    /// The world's message queues.
    and [<ReferenceEquality>] MessageQueues =
        { AudioMessages : AudioMessage rQueue
          RenderMessages : RenderMessage rQueue
          PhysicsMessages : PhysicsMessage rQueue }

    /// The world's higher order facilities.
    and [<ReferenceEquality>] Callbacks =
        { Tasks : Task list
          Subscriptions : SubscriptionEntries
          Unsubscriptions : UnsubscriptionEntries
          CallbackStates : Map<Guid, obj> }

    /// The world's state.
    and [<ReferenceEquality>] State =
        { TickTime : int64
          Liveness : Liveness
          Interactivity : Interactivity
          OptScreenTransitionDestinationAddress : Screen Address option
          AssetMetadataMap : AssetMetadataMap
          AssetGraphFilePath : string
          Overlayer : Overlayer // TODO: maybe move this to subsystems?
          OverlayFilePath : string
          UserState : obj }

    /// The world, in a functional programming sense. Hosts the game object, the dependencies
    /// needed to implement a game, messages to by consumed by the various engine sub-systems,
    /// and general configuration data.
    ///
    /// TODO: attempt to implement with Fsharpx.PersistentHashMap with hash cached in Address type.
    and [<ReferenceEquality>] World =
        { Game : Game
          Screens : Map<string, Screen>
          Groups : Map<string, Map<string, Group>>
          Entities : Map<string, Map<string, Map<string, Entity>>>
          Camera : Camera
          Components : Components
          Subsystems : Subsystems
          MessageQueues : MessageQueues
          Callbacks : Callbacks
          State : State }

    /// Provides a way to make user-defined components.
    and UserComponentFactory () =
        abstract MakeFacets : unit -> Map<string, Facet>
        default this.MakeFacets () = Map.empty
        abstract MakeEntityDispatchers : unit -> Map<string, EntityDispatcher>
        default this.MakeEntityDispatchers () = Map.empty
        abstract MakeGroupDispatchers : unit -> Map<string, GroupDispatcher>
        default this.MakeGroupDispatchers () = Map.empty
        abstract MakeScreenDispatchers : unit -> Map<string, ScreenDispatcher>
        default this.MakeScreenDispatchers () = Map.empty
        abstract MakeOptGameDispatcher : unit -> (string * GameDispatcher) option
        default this.MakeOptGameDispatcher () = None

[<AutoOpen>]
module WorldAddressModule =

    let atoea address =
        Address.changeType<'t, Entity> address

    let atoga address =
        Address.changeType<'t, Group> address

    let atosa address =
        Address.changeType<'t, Screen> address

    let atoma address =
        Address.changeType<'t, Game> address

    let atoua address =
        Address.changeType<'t, Simulant> address

    let gatoea groupAddress entityName =
        Address.changeType<Group, Entity> groupAddress ->- ltoa [entityName]

    let satoga screenAddress groupName =
        Address.changeType<Screen, Group> screenAddress ->- ltoa [groupName]

    let matosa gameAddress screenName =
        Address.changeType<Game, Screen> gameAddress ->- ltoa [screenName]

    let eatoga entityAddress =
        Address.take 2 entityAddress |> Address.changeType<Entity, Group>

    let gatosa groupAddress =
        Address.take 1 groupAddress |> Address.changeType<Group, Screen>

    let satoma screenAddress =
        Address.take 0 screenAddress |> Address.changeType<Screen, Game>

module WorldConstants =

    let GameAddress = Address<Game>.empty
    let AnyEventAddress = stoa<obj> "*"
    let TickEventAddress = stoa<unit> "Tick"
    let AddEventAddress = stoa<unit> "Add"
    let RemovingEventAddress = stoa<unit> "Removing"
    let IncomingStartEventAddress = stoa<unit> "Incoming/Start"
    let OutgoingStartEventAddress = stoa<unit> "Outgoing/Start"
    let IncomingFinishEventAddress = stoa<unit> "Incoming/Finish"
    let OutgoingFinishEventAddress = stoa<unit> "Outgoing/Finish"
    let SelectEventAddress = stoa<unit> "Select"
    let DeselectEventAddress = stoa<unit> "Deselect"
    let DownEventAddress = stoa<unit> "Down"
    let UpEventAddress = stoa<unit> "Up"
    let ClickEventAddress = stoa<unit> "Click"
    let OnEventAddress = stoa<unit> "On"
    let OffEventAddress = stoa<unit> "Off"
    let TouchEventAddress = stoa<Vector2> "Touch"
    let ReleaseEventAddress = stoa<Vector2> "Release"
    let MouseEventAddress = stoa<obj> "Mouse"
    let MouseMoveEventAddress = MouseEventAddress -<- stoa<MouseMoveData> "Move"
    let MouseDragEventAddress = MouseEventAddress -<- stoa<MouseMoveData> "Drag"
    let MouseLeftEventAddress = MouseEventAddress -<- stoa<MouseButtonData> "Left"
    let MouseCenterEventAddress = MouseEventAddress -<- stoa<MouseButtonData> "Center"
    let MouseRightEventAddress = MouseEventAddress -<- stoa<MouseButtonData> "Right"
    let MouseX1EventAddress = MouseEventAddress -<- stoa<MouseButtonData> "X1"
    let MouseX2EventAddress = MouseEventAddress -<- stoa<MouseButtonData> "X2"
    let MouseLeftDownEventAddress = MouseLeftEventAddress -|- stoa "Down"
    let MouseLeftUpEventAddress = MouseLeftEventAddress -|- stoa "Up"
    let MouseLeftChangeEventAddress = MouseLeftEventAddress -|- stoa "Change"
    let MouseCenterDownEventAddress = MouseCenterEventAddress -|- stoa "Down"
    let MouseCenterUpEventAddress = MouseCenterEventAddress -|- stoa "Up"
    let MouseCenterChangeEventAddress = MouseCenterEventAddress -|- stoa "Change"
    let MouseRightDownEventAddress = MouseRightEventAddress -|- stoa "Down"
    let MouseRightUpEventAddress = MouseRightEventAddress -|- stoa "Up"
    let MouseRightChangeEventAddress = MouseRightEventAddress -|- stoa "Change"
    let MouseX1DownEventAddress = MouseX1EventAddress -|- stoa "Down"
    let MouseX1UpEventAddress = MouseX1EventAddress -|- stoa "Up"
    let MouseX1ChangeEventAddress = MouseX1EventAddress -|- stoa "Change"
    let MouseX2DownEventAddress = MouseX2EventAddress -|- stoa "Down"
    let MouseX2UpEventAddress = MouseX2EventAddress -|- stoa "Up"
    let MouseX2ChangeEventAddress = MouseX2EventAddress -|- stoa "Change"
    let KeyboardKeyEventAddress = stoa<obj> "KeyboardKey"
    let KeyboardKeyDownEventAddress = MouseEventAddress -<- stoa<KeyboardKeyData> "Down"
    let KeyboardKeyUpEventAddress = MouseEventAddress -<- stoa<KeyboardKeyData> "Up"
    let KeyboardKeyChangeEventAddress = MouseEventAddress -<- stoa<KeyboardKeyData> "Change"
    let CollisionEventAddress = stoa<CollisionData> "Collision"
    let EntityChangeEventAddress = stoa<EntityChangeData> "EntityChange"
    let DefaultDissolveImage = { ImagePackageName = DefaultPackageName; ImageAssetName = "Image8" }

[<RequireQualifiedAccess>]
module World =

    let private AnyEventAddressesCache = Dictionary<obj Address, obj Address list> HashIdentity.Structural

    // OPTIMIZATION: priority annotated as single to decrease GC pressure.
    let private sortFstDesc (priority : single, _) (priority2 : single, _) =
        if priority > priority2 then -1
        elif priority < priority2 then 1
        else 0
    
    let private boxSubscription<'d> (subscription : 'd Subscription) =
        let boxableSubscription = fun (event : obj) world ->
            try subscription (event :?> 'd Event) world
            with
            | :? InvalidCastException ->
                // NOTE: If you've reached this exception, then you've probably inadvertantly mixed
                // up an event data type parameter for some form of World.publish or subscribe.
                reraise ()
            | _ -> reraise ()
        box boxableSubscription

    /// Make a key used to track an unsubscription with a subscription.
    let makeSubscriptionKey () =
        Guid.NewGuid ()

    /// Make a callback key used to track callback states.
    let makeCallbackKey () =
        Guid.NewGuid ()

    /// Get a simulant at the given address from the world.
    let mutable getSimulant = Unchecked.defaultof<Simulant Address -> World -> Simulant>

    /// Try to get a simulant at the given address from the world.
    let mutable getOptSimulant = Unchecked.defaultof<Simulant Address -> World -> Simulant option>

    let private getSimulantPublishingPriority getEntityPublishingPriority simulant world =
        match simulant with
        | Game _ -> GamePublishingPriority
        | Screen _ -> ScreenPublishingPriority
        | Group _ -> GroupPublishingPriority
        | Entity entity -> getEntityPublishingPriority entity world

    let private getSortableSubscriptions getEntityPublishingPriority (subscriptions : SubscriptionEntry list) world : (single * SubscriptionEntry) list =
        List.fold
            (fun subscriptions (key, address, subscription) ->
                match getOptSimulant (atoua address) world with
                | Some simulant ->
                    let priority = getSimulantPublishingPriority getEntityPublishingPriority simulant world
                    let subscription = (priority, (key, address, subscription))
                    subscription :: subscriptions
                | None -> (0.0f, (key, address, subscription)) :: subscriptions)
            []
            subscriptions

    let sortSubscriptionsBy by (subscriptions : SubscriptionEntry list) world =
        let subscriptions = getSortableSubscriptions by subscriptions world
        let subscriptions = List.sortWith sortFstDesc subscriptions
        List.map snd subscriptions

    let sortSubscriptionsByPickingPriority subscriptions world =
        sortSubscriptionsBy
            (fun (entity : Entity) world -> entity.DispatcherNp.GetPickingPriority (entity, world))
            subscriptions
            world

    let sortSubscriptionsByHierarchy subscriptions world =
        sortSubscriptionsBy
            (fun _ _ -> EntityPublishingPriority)
            subscriptions
            world

    let sortSubscriptionsNone (subscriptions : SubscriptionEntry list) _ =
        subscriptions

    // OPTIMIZATION: uses memoization.
    let private getAnyEventAddresses eventAddress =
        if not <| Address.isEmpty eventAddress then
            let anyEventAddressesKey = Address.allButLast eventAddress
            let refAnyEventAddresses = ref Unchecked.defaultof<obj Address list>
            if not <| AnyEventAddressesCache.TryGetValue (anyEventAddressesKey, refAnyEventAddresses) then
                let eventAddressList = eventAddress.Names
                let anyEventAddressList = WorldConstants.AnyEventAddress.Names
                let anyEventAddresses =
                    [for i in 0 .. List.length eventAddressList - 1 do
                        let subNameList = List.take i eventAddressList @ anyEventAddressList
                        yield Address.make subNameList]
                AnyEventAddressesCache.Add (anyEventAddressesKey, anyEventAddresses)
                anyEventAddresses
            else !refAnyEventAddresses
        else failwith "Event name cannot be empty."

    let private getSubscriptionsSorted (publishSorter : SubscriptionSorter) eventAddress world =
        let anyEventAddresses = getAnyEventAddresses eventAddress
        let optSubLists = List.map (fun anyEventAddress -> Map.tryFind anyEventAddress world.Callbacks.Subscriptions) anyEventAddresses
        let optSubLists = Map.tryFind eventAddress world.Callbacks.Subscriptions :: optSubLists
        let subLists = List.definitize optSubLists
        let subList = List.concat subLists
        publishSorter subList world

    /// Publish an event.
    let publish<'d, 'p> publishSorter (eventData : 'd) (eventAddress : 'd Address) (publisherAddress : 'p Address) world =
        let objEventAddress = atooa eventAddress
        let subscriptions = getSubscriptionsSorted publishSorter objEventAddress world
        let (_, world) =
            List.foldWhile
                (fun (eventHandling, world) (_, subscriberAddress, subscription) ->
                    if  (match eventHandling with Cascade -> true | Resolve -> false) &&
                        (match world.State.Liveness with Running -> true | Exiting -> false) then
                        let event =
                            { Address = eventAddress
                              SubscriberAddress = subscriberAddress
                              PublisherAddress = atooa publisherAddress
                              Data = eventData }
                        let callableSubscription = unbox<BoxableSubscription> subscription
                        let result = callableSubscription event world
                        Some result
                    else None)
                (Cascade, world)
                subscriptions
        world

    /// Publish an event.
    let publish4<'d, 'p> (eventData : 'd) (eventAddress : 'd Address) (publisherAddress : 'p Address) world =
        publish sortSubscriptionsByHierarchy eventData eventAddress publisherAddress world

    /// Subscribe to an event.
    let subscribe<'d, 's> subscriptionKey (subscription : 'd Subscription) (eventAddress : 'd Address) (subscriberAddress : 's Address) world =
        if not <| Address.isEmpty eventAddress then
            let objEventAddress = atooa eventAddress
            let subscriptions =
                let subscriptionEntry = (subscriptionKey, atooa subscriberAddress, boxSubscription subscription)
                match Map.tryFind objEventAddress world.Callbacks.Subscriptions with
                | Some subscriptionEntries -> Map.add objEventAddress (subscriptionEntry :: subscriptionEntries) world.Callbacks.Subscriptions
                | None -> Map.add objEventAddress [subscriptionEntry] world.Callbacks.Subscriptions
            let unsubscriptions = Map.add subscriptionKey (objEventAddress, atooa subscriberAddress) world.Callbacks.Unsubscriptions
            let callbacks = { world.Callbacks with Subscriptions = subscriptions; Unsubscriptions = unsubscriptions }
            { world with Callbacks = callbacks }
        else failwith "Event name cannot be empty."

    /// Subscribe to an event.
    let subscribe4<'d, 's> (subscription : 'd Subscription) (eventAddress : 'd Address) (subscriberAddress : 's Address) world =
        subscribe (makeSubscriptionKey ()) subscription eventAddress subscriberAddress world

    /// Unsubscribe from an event.
    let unsubscribe subscriptionKey world =
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
    let monitor<'d, 's> (subscription : 'd Subscription) (eventAddress : 'd Address) (subscriberAddress : 's Address) world =
        if not <| Address.isEmpty subscriberAddress then
            let monitorKey = makeSubscriptionKey ()
            let removalKey = makeSubscriptionKey ()
            let world = subscribe<'d, 's> monitorKey subscription eventAddress subscriberAddress world
            let subscription' = fun _ world ->
                let world = unsubscribe removalKey world
                let world = unsubscribe monitorKey world
                (Cascade, world)
            let removingEventAddress = WorldConstants.RemovingEventAddress ->- atooa subscriberAddress
            subscribe<unit, 's> removalKey subscription' removingEventAddress subscriberAddress world
        else failwith "Cannot monitor events with an anonymous subscriber."

    /// Set the Camera field of the world.
    let setCamera camera world =
        { world with Camera = camera }

    /// Transform a bunch of simulants in the context of a world.
    let transformSimulants transform patoca parentAddress simulants world =
        Map.fold
            (fun (simulants, world) simulantName simulant ->
                let (simulant, world) = transform (patoca parentAddress simulantName) simulant world
                (Map.add simulantName simulant simulants, world))
            (Map.empty, world)
            simulants

    /// Get all of a world's dispatchers.
    let internal getDispatchers world =
        Map.map Map.objectify world.Components.EntityDispatchers @@
        Map.map Map.objectify world.Components.GroupDispatchers @@
        Map.map Map.objectify world.Components.ScreenDispatchers @@
        Map.map Map.objectify world.Components.GameDispatchers

    /// Set the EntityDispatchers field of the world.
    let internal setEntityDispatchers dispatchers world =
        let components = { world.Components with EntityDispatchers = dispatchers }
        { world with Components = components }

    /// Set the GroupDispatchers field of the world.
    let internal setGroupDispatchers dispatchers world =
        let components = { world.Components with GroupDispatchers = dispatchers }
        { world with Components = components }

    /// Set the ScreenDispatchers field of the world.
    let internal setScreenDispatchers dispatchers world =
        let components = { world.Components with ScreenDispatchers = dispatchers }
        { world with Components = components }

    /// Set the GameDispatchers field of the world.
    let internal setGameDispatchers dispatchers world =
        let components = { world.Components with GameDispatchers = dispatchers }
        { world with Components = components }

    /// Set the Facets field of the world.
    let internal setFacets facets world =
        let components = { world.Components with Facets = facets }
        { world with Components = components }

    /// Set the AudioPlayer field of the world.
    let internal setAudioPlayer audioPlayer world =
        let subsystems = { world.Subsystems with AudioPlayer = audioPlayer }
        { world with Subsystems = subsystems }

    /// Set the Renderer field of the world.
    let internal setRenderer renderer world =
        let subsystems = { world.Subsystems with Renderer = renderer }
        { world with Subsystems = subsystems }

    /// Set the Integrator field of the world.
    let internal setIntegrator integrator world =
        let subsystems = { world.Subsystems with Integrator = integrator }
        { world with Subsystems = subsystems }

    /// Clear the audio messages.
    let internal clearAudioMessages world =
        let messageQueues = { world.MessageQueues with AudioMessages = [] }
        { world with MessageQueues = messageQueues }

    /// Clear the rendering messages.
    let internal clearRenderMessages world =
        let messageQueues = { world.MessageQueues with RenderMessages = [] }
        { world with MessageQueues = messageQueues }

    /// Clear the physics messages.
    let internal clearPhysicsMessages world =
        let messageQueues = { world.MessageQueues with PhysicsMessages = [] }
        { world with MessageQueues = messageQueues }

    /// Add a physics message to the world.
    let addPhysicsMessage message world =
        let messageQueues = { world.MessageQueues with PhysicsMessages = message :: world.MessageQueues.PhysicsMessages }
        { world with MessageQueues = messageQueues }

    /// Add a rendering message to the world.
    let addRenderMessage message world =
        let messageQueues = { world.MessageQueues with RenderMessages = message :: world.MessageQueues.RenderMessages }
        { world with MessageQueues = messageQueues }

    /// Add an audio message to the world.
    let addAudioMessage message world =
        let messageQueues = { world.MessageQueues with AudioMessages = message :: world.MessageQueues.AudioMessages }
        { world with MessageQueues = messageQueues }

    /// Add a task to be executed by the engine at the specified task tick.
    let addTask task world =
        let callbacks = { world.Callbacks with Tasks = task :: world.Callbacks.Tasks }
        { world with Callbacks = callbacks }

    /// Add multiple task to be executed by the engine at the specified task tick.
    let addTasks tasks world =
        let callbacks = { world.Callbacks with Tasks = tasks @ world.Callbacks.Tasks }
        { world with Callbacks = callbacks }

    /// Restore tasks to be executed by the engine at the specified task tick.
    let internal restoreTasks tasks world =
        let callbacks = { world.Callbacks with Tasks = world.Callbacks.Tasks @ tasks }
        { world with Callbacks = callbacks }

    /// Clear all tasks.
    let internal clearTasks world =
        let callbacks = { world.Callbacks with Tasks = [] }
        { world with Callbacks = callbacks }

    /// Add callback state to the world.
    let addCallbackState key state world =
        let callbacks = { world.Callbacks with CallbackStates = Map.add key (state :> obj) world.Callbacks.CallbackStates }
        { world with Callbacks = callbacks }

    /// Remove callback state from the world.
    let removeCallbackState key world =
        let callbacks = { world.Callbacks with CallbackStates = Map.remove key world.Callbacks.CallbackStates }
        { world with Callbacks = callbacks }

    /// Get callback state from the world.
    let getCallbackState<'a> key world =
        let state = Map.find key world.Callbacks.CallbackStates
        state :?> 'a

    /// Set the Overlayer field of the world.
    let internal setOverlayer overlayer world =
        let state = { world.State with Overlayer = overlayer }
        { world with State = state }

    /// Increment the TickTime field of the world.
    let internal incrementTickTime world =
        let state = { world.State with TickTime = world.State.TickTime + 1L }
        { world with State = state }

    /// Set the OptScreenTransitionDestinationAddress field of the world.
    let internal setOptScreenTransitionDestinationAddress address world =
        let state = { world.State with OptScreenTransitionDestinationAddress = address  }
        { world with State = state }

    /// Place the world into a state such that the app will exit at the end of the current frame.
    let exit world =
        let state = { world.State with Liveness = Exiting }
        { world with State = state }

    /// Query that the engine is in game-playing mode.
    let isGamePlaying world = Interactivity.isGamePlaying world.State.Interactivity

    /// Query that the physics system is running.
    let isPhysicsRunning world = Interactivity.isPhysicsRunning world.State.Interactivity

    /// Set the level of the world's interactivity.
    let setInteractivity interactivity world =
        let state = { world.State with Interactivity = interactivity }
        { world with State = state }

    /// Set the AssetMetadataMap field of the world.
    let setAssetMetadataMap assetMetadataMap world =
        let state = { world.State with AssetMetadataMap = assetMetadataMap }
        { world with State = state }

    /// Get the UserState field of the world, casted to 'u.
    let getUserState world : 'u =
        world.State.UserState :?> 'u

    /// Set the UserState field of the world.
    let setUserState (userState : 'u) world =
        let state = { world.State with UserState = userState }
        { world with State = state }

    /// Transform the UserState field of the world.
    let transformUserState (transformer : 'u -> 'v) world =
        let state = getUserState world
        let state = transformer state
        setUserState state world

[<AutoOpen>]
module WorldInputModule =

    type World with

        /// Convert a MouseButton to SDL's representation.
        static member internal toSdlMouseButton mouseButton =
            MouseState.toSdlButton mouseButton

        /// Convert SDL's representation of a mouse button to a MouseButton.
        static member internal toNuMouseButton mouseButton =
            MouseState.toNuButton mouseButton

        /// Query that the given mouse button is down.
        static member isMouseButtonDown mouseButton (_ : World) =
            MouseState.isButtonDown mouseButton

        /// Get the position of the mouse.
        static member getMousePosition (_ : World) =
            MouseState.getPosition ()

        /// Get the position of the mouse in floating-point coordinates.
        static member getMousePositionF (_ : World) =
            MouseState.getPositionF ()

        /// Query that the given keyboard key is down.
        static member isKeyboardKeyDown scanCode (_ : World) =
            KeyboardState.isKeyDown scanCode

        // TODO: implement isKeyboardModifierActive.

[<AutoOpen>]
module WorldPhysicsModule =

    type World with

        /// Does the world contain the body with the given physics id?
        static member bodyExists physicsId world =
            world.Subsystems.Integrator.BodyExists physicsId

        /// Get the contact normals of the body with the given physics id.
        static member getBodyContactNormals physicsId world =
            world.Subsystems.Integrator.GetBodyContactNormals physicsId

        /// Get the linear velocity of the body with the given physics id.
        static member getBodyLinearVelocity physicsId world =
            world.Subsystems.Integrator.GetBodyLinearVelocity physicsId

        /// Get the contact normals where the body with the given physics id is touching the ground.
        static member getBodyGroundContactNormals physicsId world =
            world.Subsystems.Integrator.GetBodyGroundContactNormals physicsId

        /// Try to get a contact normal where the body with the given physics id is touching the ground.
        static member getBodyOptGroundContactNormal physicsId world =
            world.Subsystems.Integrator.GetBodyOptGroundContactNormal physicsId

        /// Try to get a contact tangent where the body with the given physics id is touching the ground.
        static member getBodyOptGroundContactTangent physicsId world =
            world.Subsystems.Integrator.GetBodyOptGroundContactTangent physicsId

        /// Query that the body with the give physics id is on the ground.
        static member isBodyOnGround physicsId world =
            world.Subsystems.Integrator.IsBodyOnGround physicsId

        /// Send a message to the physics system to create a physics body.
        static member createBody (entityAddress : Entity Address) entityId bodyProperties world =
            let createBodyMessage = CreateBodyMessage { SourceAddress = atooa entityAddress; SourceId = entityId; BodyProperties = bodyProperties }
            World.addPhysicsMessage createBodyMessage world

        /// Send a message to the physics system to create several physics bodies.
        static member createBodies (entityAddress : Entity Address) entityId bodyPropertyList world =
            let createBodiesMessage = CreateBodiesMessage { SourceAddress = atooa entityAddress; SourceId = entityId; BodyPropertyList = bodyPropertyList }
            World.addPhysicsMessage createBodiesMessage world

        /// Send a message to the physics system to destroy a physics body.
        static member destroyBody physicsId world =
            let destroyBodyMessage = DestroyBodyMessage { PhysicsId = physicsId }
            World.addPhysicsMessage destroyBodyMessage world

        /// Send a message to the physics system to destroy several physics bodies.
        static member destroyBodies physicsIds world =
            let destroyBodiesMessage = DestroyBodiesMessage { PhysicsIds = physicsIds }
            World.addPhysicsMessage destroyBodiesMessage world

        /// Send a message to the physics system to set the position of a body with the given physics id.
        static member setBodyPosition position physicsId world =
            let setBodyPositionMessage = SetBodyPositionMessage { PhysicsId = physicsId; Position = position }
            World.addPhysicsMessage setBodyPositionMessage world

        /// Send a message to the physics system to set the rotation of a body with the given physics id.
        static member setBodyRotation rotation physicsId world =
            let setBodyRotationMessage = SetBodyRotationMessage { PhysicsId = physicsId; Rotation = rotation }
            World.addPhysicsMessage setBodyRotationMessage world

        /// Send a message to the physics system to set the linear velocity of a body with the given physics id.
        static member setBodyLinearVelocity linearVelocity physicsId world =
            let setBodyLinearVelocityMessage = SetBodyLinearVelocityMessage { PhysicsId = physicsId; LinearVelocity = linearVelocity }
            World.addPhysicsMessage setBodyLinearVelocityMessage world

        /// Send a message to the physics system to apply linear impulse to a body with the given physics id.
        static member applyBodyLinearImpulse linearImpulse physicsId world =
            let applyBodyLinearImpulseMessage = ApplyBodyLinearImpulseMessage { PhysicsId = physicsId; LinearImpulse = linearImpulse }
            World.addPhysicsMessage applyBodyLinearImpulseMessage world

        /// Send a message to the physics system to apply force to a body with the given physics id.
        static member applyBodyForce force physicsId world =
            let applyBodyForceMessage = ApplyBodyForceMessage { PhysicsId = physicsId; Force = force }
            World.addPhysicsMessage applyBodyForceMessage world

[<AutoOpen>]
module WorldRenderModule =

    type World with

        /// Hint that a rendering asset package with the given name should be loaded. Should be
        /// used to avoid loading assets at inconvenient times (such as in the middle of game play!)
        static member hintRenderPackageUse packageName world =
            let hintRenderPackageUseMessage = HintRenderPackageUseMessage { PackageName = packageName }
            World.addRenderMessage hintRenderPackageUseMessage world
            
        /// Hint that a rendering package should be unloaded since its assets will not be used
        /// again (or until specified via World.hintRenderPackageUse).
        static member hintRenderPackageDisuse packageName world =
            let hintRenderPackageDisuseMessage = HintRenderPackageDisuseMessage { PackageName = packageName }
            World.addRenderMessage hintRenderPackageDisuseMessage world
            
        /// Send a message to the renderer to reload its rendering assets.
        static member reloadRenderAssets world =
            let reloadRenderAssetsMessage = ReloadRenderAssetsMessage
            World.addRenderMessage reloadRenderAssetsMessage world

[<AutoOpen>]
module WorldAudioModule =

    type World with

        /// Send a message to the audio system to play a song.
        static member playSong timeToFadeOutSongMs volume song world =
            let playSongMessage = PlaySongMessage { Volume = volume; TimeToFadeOutSongMs = timeToFadeOutSongMs; Song = song }
            World.addAudioMessage playSongMessage world

        /// Send a message to the audio system to play a song.
        static member playSong6 timeToFadeOutSongMs volume songPackageName songAssetName world =
            let song = { SongPackageName = songPackageName; SongAssetName = songAssetName }
            World.playSong timeToFadeOutSongMs volume song world

        /// Send a message to the audio system to play a sound.
        static member playSound volume sound world =
            let playSoundMessage = PlaySoundMessage { Sound = sound; Volume = volume }
            World.addAudioMessage playSoundMessage world

        /// Send a message to the audio system to play a sound.
        static member playSound5 volume soundPackageName soundAssetName world =
            let sound = { SoundPackageName = soundPackageName; SoundAssetName = soundAssetName }
            World.playSound volume sound world

        /// Send a message to the audio system to fade out a song.
        static member fadeOutSong timeToFadeOutSongMs world =
            let fadeOutSongMessage = FadeOutSongMessage timeToFadeOutSongMs
            World.addAudioMessage fadeOutSongMessage world

        /// Send a message to the audio system to stop a song.
        static member stopSong world =
            World.addAudioMessage StopSongMessage world
            
        /// Hint that an audio asset package with the given name should be loaded. Should be used
        /// to avoid loading assets at inconvenient times (such as in the middle of game play!)
        static member hintAudioPackageUse packageName world =
            let hintAudioPackageUseMessage = HintAudioPackageUseMessage { PackageName = packageName }
            World.addAudioMessage hintAudioPackageUseMessage world
            
        /// Hint that an audio package should be unloaded since its assets will not be used again
        /// (or until specified via a HintAudioPackageUseMessage).
        static member hintAudioPackageDisuse packageName world =
            let hintAudioPackageDisuseMessage = HintAudioPackageDisuseMessage { PackageName = packageName }
            World.addAudioMessage hintAudioPackageDisuseMessage world

        /// Send a message to the audio player to reload its audio assets.
        static member reloadAudioAssets world =
            let reloadAudioAssetsMessage = ReloadAudioAssetsMessage
            World.addAudioMessage reloadAudioAssetsMessage world

[<RequireQualifiedAccess>]
module Simulant =

    let getOptChild optChildFinder address parent =
        let optChild = optChildFinder address parent
        match optChild with
        | Some child -> Some child
        | None -> None

    let setOptChild addChild removeChild address parent optChild =
        match optChild with
        | Some child -> addChild address parent child
        | None -> removeChild address parent

    let getChild optChildFinder address parent =
        Option.get <| optChildFinder address parent

    let setChild childAdder childRemover address parent child =
        setOptChild childAdder childRemover address parent (Some child)

    let toEntity simulant =
        match simulant with
        | Entity entity -> entity
        | Group _ | Screen _ | Game _ -> failwith "Invalid conversion of simulant to entity."

    let toGroup simulant =
        match simulant with
        | Group group -> group
        | Entity _ | Screen _ | Game _ -> failwith "Invalid conversion of simulant to group."

    let toScreen simulant =
        match simulant with
        | Screen screen -> screen
        | Entity _ | Group _ | Game _ -> failwith "Invalid conversion of simulant to screen."

    let toGame simulant =
        match simulant with
        | Game game -> game
        | Entity _ | Group _ | Screen _ -> failwith "Invalid conversion of simulant to game."

    let toGeneric<'s> simulant =
        let s = typeof<'s>
        // OPTIMIZATION: Entity type is most common and therefore checked first.
        if s = typeof<Entity> then toEntity simulant :> obj :?> 's
        elif s = typeof<Group> then toGroup simulant :> obj :?> 's
        elif s = typeof<Screen> then toScreen simulant :> obj :?> 's
        elif s = typeof<Game> then toGame simulant :> obj :?> 's
        elif s = typeof<obj> then simulant :> obj :?> 's
        else failwith <| "Invalid simulation type '" + s.Name + "'."

[<AutoOpen>]
module WorldEventModule =

    type World with

        /// Unwrap commonly-useful values of an event.
        /// TODO: these implementations build in the notion that only simulants are addressable.
        /// While this is true for now, it may not be true later. Make this more general!
        static member unwrapASDE<'d, 's> (event : 'd Event) world =
            let subscriber = World.getOptSimulant (atoua event.SubscriberAddress) world |> Option.get |> Simulant.toGeneric<'s>
            (Address.changeType<obj, 's> event.SubscriberAddress, subscriber, event.Data, event)

        /// Unwrap commonly-useful values of an event.
        static member unwrapASD<'d, 's> (event : 'd Event) world =
            let subscriber = World.getOptSimulant (atoua event.SubscriberAddress) world |> Option.get |> Simulant.toGeneric<'s>
            (Address.changeType<obj, 's> event.SubscriberAddress, subscriber, event.Data)

        /// Unwrap commonly-useful values of an event.
        static member unwrapASE<'d, 's> (event : 'd Event) world =
            let subscriber = World.getOptSimulant (atoua event.SubscriberAddress) world |> Option.get |> Simulant.toGeneric<'s>
            (Address.changeType<obj, 's> event.SubscriberAddress, subscriber, event)

        /// Unwrap commonly-useful values of an event.
        static member unwrapADE<'d, 's> (event : 'd Event) (_ : World) =
            (Address.changeType<obj, 's> event.SubscriberAddress, event.Data, event)

        /// Unwrap commonly-useful values of an event.
        static member unwrapSDE<'d, 's> (event : 'd Event) world =
            let subscriber = World.getOptSimulant (atoua event.SubscriberAddress) world |> Option.get |> Simulant.toGeneric<'s>
            (subscriber, event.Data, event)

        /// Unwrap commonly-useful values of an event.
        static member unwrapAS<'d, 's> (event : 'd Event) world =
            let subscriber = World.getOptSimulant (atoua event.SubscriberAddress) world |> Option.get |> Simulant.toGeneric<'s>
            (Address.changeType<obj, 's> event.SubscriberAddress, subscriber)

        /// Unwrap commonly-useful values of an event.
        static member unwrapAD<'d, 's> (event : 'd Event) (_ : World) =
            (Address.changeType<obj, 's> event.SubscriberAddress, event.Data)

        /// Unwrap commonly-useful values of an event.
        static member unwrapAE<'d, 's> (event : 'd Event) (_ : World) =
            (Address.changeType<obj, 's> event.SubscriberAddress, event)

        /// Unwrap commonly-useful values of an event.
        static member unwrapSD<'d, 's> (event : 'd Event) world =
            let subscriber = World.getOptSimulant (atoua event.SubscriberAddress) world |> Option.get |> Simulant.toGeneric<'s>
            (subscriber, event.Data)

        /// Unwrap commonly-useful values of an event.
        static member unwrapSE<'d, 's> (event : 'd Event) world =
            let subscriber = World.getOptSimulant (atoua event.SubscriberAddress) world |> Option.get |> Simulant.toGeneric<'s>
            (subscriber, event)

        /// Unwrap commonly-useful values of an event.
        static member unwrapDE<'d, 's> (event : 'd Event) (_ : World) =
            (event.Data, event)

        /// Unwrap commonly-useful values of an event.
        static member unwrapA<'d, 's> (event : 'd Event) (_ : World) =
            Address.changeType<obj, 's> event.SubscriberAddress

        /// Unwrap commonly-useful values of an event.
        static member unwrapS<'d, 's> (event : 'd Event) world =
            World.getOptSimulant (atoua event.SubscriberAddress) world |> Option.get |> Simulant.toGeneric<'s>

        /// Unwrap commonly-useful values of an event.
        static member unwrapD<'d, 's> (event : 'd Event) (_ : World) =
            event.Data