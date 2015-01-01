namespace Nu
open System
open System.Collections.Generic
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants

[<RequireQualifiedAccess>]
module World =

    let private AnyEventAddressesCache =
        Dictionary<obj Address, obj Address list> HashIdentity.Structural

    /// Transform a bunch of simulants in the context of a world.
    let transformSimulants transform patoca simulants parentAddress world =
        Map.fold
            (fun (simulants, world) simulantName simulant ->
                let childAddress = patoca parentAddress simulantName
                let (simulant, world) = transform simulant childAddress world
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

    /// Make a key used to track an unsubscription with a subscription.
    let makeSubscriptionKey () =
        Guid.NewGuid ()

    /// Make a callback key used to track callback states.
    let makeCallbackKey () =
        Guid.NewGuid ()

    /// Try to get a simulant at the given address from the world.
    let mutable private getOptSimulantForPublishing =
        Unchecked.defaultof<Simulant Address -> World -> Simulant option>

    /// Get a simulant at the given address from the world.
    let private getSimulantForPublishing address world =
        Option.get <| getOptSimulantForPublishing address world

    // OPTIMIZATION: priority annotated as single to decrease GC pressure.
    let private sortFstDesc (priority : single, _) (priority2 : single, _) =
        if priority > priority2 then -1
        elif priority < priority2 then 1
        else 0
    
    let private boxSubscription<'a, 's when 's :> Simulant> (subscription : Subscription<'a, 's>) =
        let boxableSubscription = fun (event : obj) world ->
            try subscription (event :?> Event<'a, 's>) world
            with
            | :? InvalidCastException ->
                // NOTE: If you've reached this exception, then you've probably inadvertantly mixed
                // up an event type parameter for some form of World.publish or subscribe.
                reraise ()
            | _ -> reraise ()
        box boxableSubscription

    let private getSortableSubscriptions getEntityPublishingPriority (subscriptions : SubscriptionEntry list) world : (single * SubscriptionEntry) list =
        List.fold
            (fun subscriptions (key, address, subscription) ->
                match getOptSimulantForPublishing (atoua address) world with
                | Some simulant ->
                    let priority = simulant.GetPublishingPriority getEntityPublishingPriority world
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
                let anyEventAddressList = AnyEventAddress.Names
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
        let subListRev = List.rev subList
        publishSorter subListRev world

    let private publishEvent<'a, 'p, 's when 'p :> Simulant and 's :> Simulant>
        (subscriberAddress : obj Address) (publisherAddress : 'p Address) (eventAddress : 'a Address) (eventData : 'a) subscription world =
        let event =
            { SubscriberAddress = Address.changeType<obj, 's> subscriberAddress
              PublisherAddress = Address.changeType<'p, Simulant> publisherAddress
              EventAddress = eventAddress
              Subscriber = getSimulantForPublishing (Address.changeType<obj, Simulant> subscriberAddress) world :?> 's
              Data = eventData }
        let callableSubscription = unbox<BoxableSubscription> subscription
        let result = callableSubscription event world
        Some result

    /// Publish an event.
    let publish<'a, 'p when 'p :> Simulant> publishSorter (eventData : 'a) (eventAddress : 'a Address) (publisherAddress : 'p Address) world =
        let objEventAddress = atooa eventAddress
        let subscriptions = getSubscriptionsSorted publishSorter objEventAddress world
        let (_, world) =
            List.foldWhile
                (fun (eventHandling, world) (_, subscriberAddress, subscription) ->
                    if  (match eventHandling with Cascade -> true | Resolve -> false) &&
                        (match world.State.Liveness with Running -> true | Exiting -> false) then
                        match subscriberAddress.Names with
                        | [] -> publishEvent<'a, 'p, Game> subscriberAddress publisherAddress eventAddress eventData subscription world
                        | [_] -> publishEvent<'a, 'p, Screen> subscriberAddress publisherAddress eventAddress eventData subscription world
                        | [_; _] -> publishEvent<'a, 'p, Group> subscriberAddress publisherAddress eventAddress eventData subscription world
                        | [_; _; _] -> publishEvent<'a, 'p, Entity> subscriberAddress publisherAddress eventAddress eventData subscription world
                        | _ -> failwith "Unexpected match failure in 'Nu.World.publish.'"
                    else None)
                (Cascade, world)
                subscriptions
        world

    /// Publish an event.
    let publish4<'a, 'p when 'p :> Simulant> (eventData : 'a) (eventAddress : 'a Address) (publisherAddress : 'p Address) world =
        publish sortSubscriptionsByHierarchy eventData eventAddress publisherAddress world

    /// Subscribe to an event.
    let subscribe<'a, 's when 's :> Simulant>
        subscriptionKey (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriberAddress : 's Address) world =
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
    let subscribe4<'a, 's when 's :> Simulant>
        (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriberAddress : 's Address) world =
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
    let monitor<'a, 's when 's :> Simulant>
        (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriberAddress : 's Address) world =
        if not <| Address.isEmpty subscriberAddress then
            let monitorKey = makeSubscriptionKey ()
            let removalKey = makeSubscriptionKey ()
            let world = subscribe<'a, 's> monitorKey subscription eventAddress subscriberAddress world
            let subscription' = fun _ world ->
                let world = unsubscribe removalKey world
                let world = unsubscribe monitorKey world
                (Cascade, world)
            let removingEventAddress = RemovingEventAddress ->- atooa subscriberAddress
            subscribe<unit, 's> removalKey subscription' removingEventAddress subscriberAddress world
        else failwith "Cannot monitor events with an anonymous subscriber."

[<AutoOpen>]
module WorldStateModule =

    type World with

        /// Get the state of the world.
        static member getState world =
            world.State

        /// Set the state of the world.
        static member setState state world =
            let oldState = world.State
            let world = { world with State = state }
            World.publish4 { OldWorldState = oldState } WorldStateChangeEventAddress GameAddress world

        /// Update the state of the world.
        static member updateState updater world =
            World.setState (updater world.State) world

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
            let song = { PackageName = songPackageName; AssetName = songAssetName }
            World.playSong timeToFadeOutSongMs volume song world

        /// Send a message to the audio system to play a sound.
        static member playSound volume sound world =
            let playSoundMessage = PlaySoundMessage { Sound = sound; Volume = volume }
            World.addAudioMessage playSoundMessage world

        /// Send a message to the audio system to play a sound.
        static member playSound5 volume soundPackageName soundAssetName world =
            let sound = { PackageName = soundPackageName; AssetName = soundAssetName }
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

        /// Ignore all handled events.
        static member handleAsPass<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Cascade, world)

        /// Swallow all handled events.
        static member handleAsSwallow<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Resolve, world)
        
        /// Handle event by exiting app.
        static member handleAsExit<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Resolve, World.exit world)