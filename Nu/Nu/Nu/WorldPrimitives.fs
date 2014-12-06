namespace Nu
open System
open System.Collections.Generic
open Prime
open Nu
open Nu.Constants

[<RequireQualifiedAccess>]
module World =

    let private AnyEventAddressesCache =
        Dictionary<obj Address, obj Address list> HashIdentity.Structural

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

    /// Set the Overlayer field of the world.
    let internal setOverlayer overlayer world =
        let subsystems = { world.Subsystems with Overlayer = overlayer }
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
    let isGamePlaying world =
        Interactivity.isGamePlaying world.State.Interactivity

    /// Query that the physics system is running.
    let isPhysicsRunning world =
        Interactivity.isPhysicsRunning world.State.Interactivity

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

    /// Make a key used to track an unsubscription with a subscription.
    let makeSubscriptionKey () =
        Guid.NewGuid ()

    /// Make a callback key used to track callback states.
    let makeCallbackKey () =
        Guid.NewGuid ()

    /// Get a simulant at the given address from the world.
    let mutable getSimulant =
        Unchecked.defaultof<Simulant Address -> World -> Simulant>

    /// Try to get a simulant at the given address from the world.
    let mutable getOptSimulant =
        Unchecked.defaultof<Simulant Address -> World -> Simulant option>

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
            match AnyEventAddressesCache.TryGetValue anyEventAddressesKey with
            | (true, anyEventAddresses) -> anyEventAddresses
            | (false, _) ->
                let eventAddressList = eventAddress.Names
                let anyEventAddressList = WorldConstants.AnyEventAddress.Names
                let anyEventAddresses =
                    [for i in 0 .. List.length eventAddressList - 1 do
                        let subNameList = List.take i eventAddressList @ anyEventAddressList
                        yield Address.make subNameList]
                AnyEventAddressesCache.Add (anyEventAddressesKey, anyEventAddresses)
                anyEventAddresses
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
                            { SubscriberAddress = subscriberAddress
                              PublisherAddress = atooa publisherAddress
                              EventAddress = eventAddress
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
            let playSongMessage = PlaySongMessage { TimeToFadeOutSongMs = timeToFadeOutSongMs; Volume = volume; Song = song }
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

[<AutoOpen>]
module WorldEventModule =

    type World with

        /// Unwrap commonly-useful values of an event.
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
            
        /// Ignore all handled events.
        static member handleAsPass (_ : 'd Event) (world : World) =
            (Cascade, world)

        /// Swallow all handled events.
        static member handleAsSwallow<'d> (_ : 'd Event) (world : World) =
            (Resolve, world)
        
        /// Handle event by exiting app.
        static member handleAsExit<'d> (_ : 'd Event) (world : World) =
            (Resolve, World.exit world)