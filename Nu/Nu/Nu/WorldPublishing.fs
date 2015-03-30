// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open System.Collections.Generic
open FSharpx
open FSharpx.Collections
open OpenTK
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants

[<AutoOpen>]
module WorldPublishingModule =

    let private AnyEventAddressesCache =
        Dictionary<obj Address, obj Address list> HashIdentity.Structural

    type World with

        static member internal sortFstDesc (priority : single, _) (priority2 : single, _) =
            // OPTIMIZATION: priority parameter is annotated as 'single' to decrease GC pressure.
            if priority > priority2 then -1
            elif priority < priority2 then 1
            else 0

        static member private getAnyEventAddresses eventAddress =
            // OPTIMIZATION: uses memoization.
            if not <| Address.isEmpty eventAddress then
                let anyEventAddressesKey = Address.allButLast eventAddress
                match AnyEventAddressesCache.TryGetValue anyEventAddressesKey with
                | (true, anyEventAddresses) -> anyEventAddresses
                | (false, _) ->
                    let eventAddressNameKeys = eventAddress.NameKeys
                    let anyEventAddressNameKeys = AnyEventAddress.NameKeys
                    let anyEventAddresses =
                        [for i in 0 .. List.length eventAddressNameKeys - 1 do
                            let subNameKeys = List.take i eventAddressNameKeys @ anyEventAddressNameKeys
                            yield ktoa subNameKeys]
                    AnyEventAddressesCache.Add (anyEventAddressesKey, anyEventAddresses)
                    anyEventAddresses
            else failwith "Event name cannot be empty."

        static member private getSortableSubscriptions getEntityPublishingPriority (subscriptions : SubscriptionEntry rQueue) world : (single * SubscriptionEntry) list =
            List.foldBack
                (fun (key, simulant : Simulant, subscription) subscriptions ->
                    let priority = simulant.GetPublishingPriority getEntityPublishingPriority world
                    let subscription = (priority, (key, simulant, subscription))
                    subscription :: subscriptions)
                subscriptions
                []

        static member private getSubscriptionsSorted (publishSorter : SubscriptionSorter) eventAddress world =
            let anyEventAddresses = World.getAnyEventAddresses eventAddress
            let optSubLists = List.map (fun anyEventAddress -> Map.tryFind anyEventAddress world.Callbacks.Subscriptions) anyEventAddresses
            let optSubLists = Map.tryFind eventAddress world.Callbacks.Subscriptions :: optSubLists
            let subLists = List.definitize optSubLists
            let subList = List.concat subLists
            publishSorter subList world

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

        static member private publishEvent<'a, 'p, 's when 'p :> Simulant and 's :> Simulant>
            (subscriber : Simulant) (publisher : 'p) (eventAddress : 'a Address) (eventData : 'a) subscription world =
            let event =
                { Subscriber = subscriber :?> 's
                  Publisher = publisher :> Simulant
                  EventAddress = eventAddress
                  Data = eventData }
            let callableSubscription = unbox<BoxableSubscription> subscription
            let result = callableSubscription event world
            Some result

        /// Make a key used to track an unsubscription with a subscription.
        static member makeSubscriptionKey () = Guid.NewGuid ()

        /// Make a callback key used to track callback states.
        static member makeCallbackKey () = Guid.NewGuid ()

        /// Sort subscriptions using categorization via the 'by' procedure.
        static member sortSubscriptionsBy by (subscriptions : SubscriptionEntry list) world =
            let subscriptions = World.getSortableSubscriptions by subscriptions world
            let subscriptions = List.sortWith World.sortFstDesc subscriptions
            List.map snd subscriptions

        /// Sort subscriptions by their place in the world's simulant hierarchy.
        static member sortSubscriptionsByHierarchy subscriptions world =
            World.sortSubscriptionsBy
                (fun _ _ -> EntityPublishingPriority)
                subscriptions
                world

        /// A 'no-op' for subscription sorting - that is, performs no sorting at all.
        static member sortSubscriptionsNone (subscriptions : SubscriptionEntry list) (_ : World) =
            subscriptions

        /// Publish an event, using the given publishSorter procedure to arranging the order to which subscriptions are published.
        static member publish<'a, 'p when 'p :> Simulant> publishSorter (eventData : 'a) (eventAddress : 'a Address) (publisher : 'p) world =
            let objEventAddress = atooa eventAddress
            let subscriptions = World.getSubscriptionsSorted publishSorter objEventAddress world
            let (_, world) =
                List.foldWhile
                    (fun (eventHandling, world) (_, subscriber : Simulant, subscription) ->
                        if  (match eventHandling with Cascade -> true | Resolve -> false) &&
                            (match world.State.Liveness with Running -> true | Exiting -> false) then
                            match subscriber.SimulantAddress.NameKeys with
                            | [] -> World.publishEvent<'a, 'p, Game> subscriber publisher eventAddress eventData subscription world
                            | [_] -> World.publishEvent<'a, 'p, Screen> subscriber publisher eventAddress eventData subscription world
                            | [_; _] -> World.publishEvent<'a, 'p, Group> subscriber publisher eventAddress eventData subscription world
                            | [_; _; _] -> World.publishEvent<'a, 'p, Entity> subscriber publisher eventAddress eventData subscription world
                            | _ -> failwith "Unexpected match failure in 'Nu.World.publish.'"
                        else None)
                    (Cascade, world)
                    subscriptions
            world

        /// Publish an event.
        static member publish4<'a, 'p when 'p :> Simulant>
            (eventData : 'a) (eventAddress : 'a Address) (publisher : 'p) world =
            World.publish World.sortSubscriptionsByHierarchy eventData eventAddress publisher world

        /// Subscribe to an event.
        static member subscribe<'a, 's when 's :> Simulant>
            subscriptionKey (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriber : 's) world =
            if not <| Address.isEmpty eventAddress then
                let objEventAddress = atooa eventAddress
                let subscriptions =
                    let subscriptionEntry = (subscriptionKey, subscriber :> Simulant, World.boxSubscription subscription)
                    match Map.tryFind objEventAddress world.Callbacks.Subscriptions with
                    | Some subscriptionEntries -> Map.add objEventAddress (subscriptionEntry :: subscriptionEntries) world.Callbacks.Subscriptions
                    | None -> Map.add objEventAddress [subscriptionEntry] world.Callbacks.Subscriptions
                let unsubscriptions = Map.add subscriptionKey (objEventAddress, subscriber :> Simulant) world.Callbacks.Unsubscriptions
                let callbacks = { world.Callbacks with Subscriptions = subscriptions; Unsubscriptions = unsubscriptions }
                { world with Callbacks = callbacks }
            else failwith "Event name cannot be empty."

        /// Subscribe to an event.
        static member subscribe4<'a, 's when 's :> Simulant>
            (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriber : 's) world =
            World.subscribe (World.makeSubscriptionKey ()) subscription eventAddress subscriber world

        /// Unsubscribe from an event.
        static member unsubscribe subscriptionKey world =
            match Map.tryFind subscriptionKey world.Callbacks.Unsubscriptions with
            | Some (eventAddress, subscriber) ->
                match Map.tryFind eventAddress world.Callbacks.Subscriptions with
                | Some subscriptionList ->
                    let subscriptionList =
                        List.remove
                            (fun (subscriptionKey', subscriber', _) ->
                                subscriptionKey' = subscriptionKey &&
                                subscriber' = subscriber)
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
            (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriber : 's) world =
            if not <| Address.isEmpty subscriber.SimulantAddress then
                let monitorKey = World.makeSubscriptionKey ()
                let removalKey = World.makeSubscriptionKey ()
                let world = World.subscribe<'a, 's> monitorKey subscription eventAddress subscriber world
                let subscription' = fun _ world ->
                    let world = World.unsubscribe removalKey world
                    let world = World.unsubscribe monitorKey world
                    (Cascade, world)
                let removingEventAddress = stoa<unit> (typeof<'s>.Name + "/" + "Removing") ->>- subscriber.SimulantAddress
                World.subscribe<unit, 's> removalKey subscription' removingEventAddress subscriber world
            else failwith "Cannot monitor events with an anonymous subscriber."