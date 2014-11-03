// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2014.

namespace Nu
open System
open System.IO
open System.Collections.Generic
open System.ComponentModel
open System.Reflection
open System.Xml
open System.Xml.Serialization
open SDL2
open OpenTK
open TiledSharp
open Prime
open Nu
open Nu.Constants

[<AutoOpen>]
module WorldModule =

    type World with

        static member handleAsSwallow _ (world : World) =
            (Resolve, world)

        static member handleAsExit _ (world : World) =
            (Resolve, World.exit world)

    let private ScreenTransitionDownMouseKey = World.makeSubscriptionKey ()
    let private ScreenTransitionUpMouseKey = World.makeSubscriptionKey ()
    let private ScreenTransitionDownKeyboardKeyKey = World.makeSubscriptionKey ()
    let private ScreenTransitionUpKeyboardKeyKey = World.makeSubscriptionKey ()
    let private SplashScreenTickKey = World.makeSubscriptionKey ()
    let private AnyEventAddressesCache = Dictionary<Address, Address list> HashIdentity.Structural
    let private LoadedAssemblies = Dictionary<string, Assembly> ()

    type World with

        static member tryGetIsSelectedScreenIdling world =
            match World.getOptSelectedScreen world with
            | Some selectedScreen -> Some <| Screen.isIdling selectedScreen
            | None -> None

        static member isSelectedScreenIdling world =
            match World.tryGetIsSelectedScreenIdling world with
            | Some answer -> answer
            | None -> failwith <| "Cannot query state of non-existent selected screen."

        static member tryGetIsSelectedScreenTransitioning world =
            Option.map not <| World.tryGetIsSelectedScreenIdling world

        static member isSelectedScreenTransitioning world =
            not <| World.isSelectedScreenIdling world

        static member private setScreenState state address screen world =
            let screen = Screen.setScreenState state screen
            let world =
                match state with
                | IdlingState ->
                    world |>
                        World.unsubscribe ScreenTransitionDownMouseKey |>
                        World.unsubscribe ScreenTransitionUpMouseKey |>
                        World.unsubscribe ScreenTransitionDownKeyboardKeyKey |>
                        World.unsubscribe ScreenTransitionUpKeyboardKeyKey
                | IncomingState | OutgoingState ->
                    world |>
                        World.subscribe ScreenTransitionDownMouseKey (DownMouseEventAddress + AnyEventAddress) address World.handleAsSwallow |>
                        World.subscribe ScreenTransitionUpMouseKey (UpMouseEventAddress + AnyEventAddress) address World.handleAsSwallow |>
                        World.subscribe ScreenTransitionDownKeyboardKeyKey (DownKeyboardKeyEventAddress + AnyEventAddress) address World.handleAsSwallow |>
                        World.subscribe ScreenTransitionUpKeyboardKeyKey (UpKeyboardKeyEventAddress + AnyEventAddress) address World.handleAsSwallow
            let world = World.setScreen address screen world
            (screen, world)

        static member selectScreen address screen world =
            let (screen, world) = World.setScreenState IncomingState address screen world
            let world = World.setOptSelectedScreenAddress (Some address) world
            (screen, world)

        static member tryTransitionScreen destinationAddress destinationScreen world =
            match World.getOptSelectedScreenAddress world with
            | Some selectedScreenAddress ->
                match World.getOptScreen selectedScreenAddress world with
                | Some selectedScreen ->
                    let subscriptionKey = World.makeSubscriptionKey ()
                    let subscription = fun _ world ->
                        match world.State.OptScreenTransitionDestinationAddress with
                        | Some address ->
                            let world = World.unsubscribe subscriptionKey world
                            let world = World.setOptScreenTransitionDestinationAddress None world
                            let world = snd <| World.selectScreen address destinationScreen world
                            (Cascade, world)
                        | None -> failwith "No valid OptScreenTransitionDestinationAddress during screen transition!"
                    let world = World.setOptScreenTransitionDestinationAddress (Some destinationAddress) world
                    let world = snd <| World.setScreenState OutgoingState selectedScreenAddress selectedScreen world
                    let world = World.subscribe subscriptionKey (FinishOutgoingEventAddress + selectedScreenAddress) selectedScreenAddress subscription world
                    Some world
                | None -> None
            | None -> None

        static member handleAsScreenTransitionFromSplash destinationAddress _ world =
            let destinationScreen = World.getScreen destinationAddress world
            let world = snd <| World.selectScreen destinationAddress destinationScreen world
            (Cascade, world)

        static member handleAsScreenTransition destinationAddress _ world =
            let destinationScreen = World.getScreen destinationAddress world
            match World.tryTransitionScreen destinationAddress destinationScreen world with
            | Some world -> (Cascade, world)
            | None ->
                trace <| "Program Error: Invalid screen transition for destination address '" + tcstring destinationAddress + "'."
                (Cascade, world)

        // OPTIMIZATION: priority annotated as single to decrease GC pressure.
        static member private sortFstDesc (priority : single, _) (priority2 : single, _) =
            if priority = priority2 then 0
            elif priority > priority2 then -1
            else 1

        static member getSimulant address world =
            match address.AddrList with
            | [] -> Game <| world.Game
            | [_] -> Screen <| World.getScreen address world
            | [_; _] -> Group <| World.getGroup address world
            | [_; _; _] -> Entity <| World.getEntity address world
            | _ -> failwith <| "Invalid simulant address '" + tcstring address + "'."

        static member getOptSimulant address world =
            match address.AddrList with
            | [] -> Some <| Game world.Game
            | [_] -> Option.map Screen <| World.getOptScreen address world
            | [_; _] -> Option.map Group <| World.getOptGroup address world
            | [_; _; _] -> Option.map Entity <| World.getOptEntity address world
            | _ -> failwith <| "Invalid simulant address '" + tcstring address + "'."

        static member private getPublishingPriority getEntityPublishingPriority simulant world =
            match simulant with
            | Game _ -> GamePublishingPriority
            | Screen _ -> ScreenPublishingPriority
            | Group _ -> GroupPublishingPriority
            | Entity entity -> getEntityPublishingPriority entity world

        static member private getSubscriptions getEntityPublishingPriority subscriptions world =
            List.fold
                (fun subscriptions (key, address, subscription) ->
                    match World.getOptSimulant address world with
                    | Some simulant ->
                        let priority = World.getPublishingPriority getEntityPublishingPriority simulant world
                        let subscription = (priority, (key, address, subscription))
                        subscription :: subscriptions
                    | None -> subscriptions)
                []
                subscriptions

        static member sortSubscriptionsBy getEntityPublishingPriority (subscriptions : SubscriptionEntry list) world =
            let subscriptions = World.getSubscriptions getEntityPublishingPriority subscriptions world
            let subscriptions = List.sortWith World.sortFstDesc subscriptions
            List.map snd subscriptions

        static member sortSubscriptionsByPickingPriority subscriptions world =
            World.sortSubscriptionsBy
                (fun (entity : Entity) world -> Entity.getPickingPriority entity world)
                subscriptions
                world

        static member sortSubscriptionsByHierarchy (subscriptions : SubscriptionEntry list) world =
            World.sortSubscriptionsBy
                (fun _ _ -> EntityPublishingPriority)
                subscriptions
                world

        // OPTIMIZATION: uses memoization.
        static member private getAnyEventAddresses eventAddress =
            if not <| Address.isEmpty eventAddress then
                let anyEventAddressesKey = Address.allButLast eventAddress
                let refAnyEventAddresses = ref Unchecked.defaultof<Address list>
                if not <| AnyEventAddressesCache.TryGetValue (anyEventAddressesKey, refAnyEventAddresses) then
                    let eventAddressList = eventAddress.AddrList
                    let anyEventAddressList = AnyEventAddress.AddrList
                    let anyEventAddresses =
                        [for i in 0 .. List.length eventAddressList - 1 do
                            let subNameList = List.take i eventAddressList @ anyEventAddressList
                            yield Address.make subNameList]
                    AnyEventAddressesCache.Add (anyEventAddressesKey, anyEventAddresses)
                    anyEventAddresses
                else !refAnyEventAddresses
            else failwith "Event name cannot be empty."

        static member private getSubscriptionsSorted publishSorter eventAddress world =
            let anyEventAddresses = World.getAnyEventAddresses eventAddress
            let optSubLists = List.map (fun anyEventAddress -> Map.tryFind anyEventAddress world.Callbacks.Subscriptions) anyEventAddresses
            let optSubLists = Map.tryFind eventAddress world.Callbacks.Subscriptions :: optSubLists
            let subLists = List.definitize optSubLists
            let subList = List.concat subLists
            publishSorter subList world

        /// Publish an event.
        static member private publishDefinition publishSorter eventAddress publisherAddress eventData world =
            let subscriptions = World.getSubscriptionsSorted publishSorter eventAddress world
            let (_, world) =
                List.foldWhile
                    (fun (eventHandling, world) (_, subscriberAddress, subscription) ->
                        match World.getOptSimulant subscriberAddress world with
                        | Some subscriber ->
                            let event =
                                { Address = eventAddress
                                  SubscriberAddress = subscriberAddress
                                  Subscriber = subscriber
                                  PublisherAddress = publisherAddress
                                  OptPublisher = World.getOptSimulant publisherAddress world
                                  Data = eventData }
                            if  (match eventHandling with Cascade -> true | Resolve -> false) &&
                                (match world.State.Liveness with Running -> true | Exiting -> false) then
                                let result = subscription event world
                                Some result
                            else None
                        | None -> Some (Cascade, world))
                    (Cascade, world)
                    subscriptions
            world

        /// Publish an event.
        static member private publish4Definition eventAddress publisherAddress eventData world =
            World.publish World.sortSubscriptionsByHierarchy eventAddress publisherAddress eventData world

        /// Subscribe to an event.
        static member private subscribeDefinition subscriptionKey eventAddress subscriberAddress subscription world =
            if not <| Address.isEmpty eventAddress then
                let subscriptions =
                    let subscriptionEntry = (subscriptionKey, subscriberAddress, subscription)
                    match Map.tryFind eventAddress world.Callbacks.Subscriptions with
                    | Some subscriptionEntries -> Map.add eventAddress (subscriptionEntry :: subscriptionEntries) world.Callbacks.Subscriptions
                    | None -> Map.add eventAddress [subscriptionEntry] world.Callbacks.Subscriptions
                let unsubscriptions = Map.add subscriptionKey (eventAddress, subscriberAddress) world.Callbacks.Unsubscriptions
                let callbacks = { world.Callbacks with Subscriptions = subscriptions; Unsubscriptions = unsubscriptions }
                { world with Callbacks = callbacks }
            else failwith "Event name cannot be empty."

        /// Subscribe to an event.
        static member private subscribe4Definition eventAddress subscriberAddress subscription world =
            World.subscribe (World.makeSubscriptionKey ()) eventAddress subscriberAddress subscription world

        /// Unsubscribe from an event.
        static member private unsubscribeDefinition subscriptionKey world =
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
                | None -> world // TODO: consider failure signal
            | None -> world // TODO: consider failure signal

        /// Keep active a subscription for the duration of a procedure.
        static member private withSubscriptionDefinition eventAddress subscriberAddress subscription procedure world =
            let subscriptionKey = World.makeSubscriptionKey ()
            let world = World.subscribe subscriptionKey eventAddress subscriberAddress subscription world
            let world = procedure world
            World.unsubscribe subscriptionKey world

        /// Keep active a subscription for the lifetime of a simulant.
        static member private monitorDefinition eventAddress subscriberAddress subscription world =
            if not <| Address.isEmpty subscriberAddress then
                let observationKey = World.makeSubscriptionKey ()
                let removalKey = World.makeSubscriptionKey ()
                let world = World.subscribe observationKey eventAddress subscriberAddress subscription world
                let subscription = fun _ world ->
                    let world = World.unsubscribe removalKey world
                    let world = World.unsubscribe observationKey world
                    (Cascade, world)
                World.subscribe removalKey (RemovingEventAddress + subscriberAddress) subscriberAddress subscription world
            else failwith "Cannot monitor events with an anonymous subscriber."

        static member saveGroupToFile group entities filePath world =
            use file = File.Open (filePath, FileMode.Create)
            let writerSettings = XmlWriterSettings ()
            writerSettings.Indent <- true
            // NOTE: XmlWriter can also write to an XmlDocument / XmlNode instance by using
            // XmlWriter.Create <| (document.CreateNavigator ()).AppendChild ()
            use writer = XmlWriter.Create (file, writerSettings)
            writer.WriteStartDocument ()
            writer.WriteStartElement RootNodeName
            World.writeGroup world.State.Overlayer writer group entities
            writer.WriteEndElement ()
            writer.WriteEndDocument ()

        static member loadGroupFromFile filePath world =
            let document = XmlDocument ()
            document.Load (filePath : string)
            let rootNode = document.[RootNodeName]
            let groupNode = rootNode.[GroupNodeName]
            World.readGroup groupNode typeof<GroupDispatcher>.Name typeof<EntityDispatcher>.Name world

        static member private updateTransition1 (transition : Transition) =
            if transition.TransitionTicks = transition.TransitionLifetime then (true, { transition with TransitionTicks = 0L })
            else (false, { transition with TransitionTicks = transition.TransitionTicks + 1L })

        // TODO: split this function up...
        static member private updateTransition handleUpdate world =
            let world =
                match World.getOptSelectedScreenAddress world with
                | Some selectedScreenAddress ->
                    let selectedScreen = World.getScreen selectedScreenAddress world
                    match selectedScreen.ScreenState with
                    | IncomingState ->
                        let world =
                            if selectedScreen.Incoming.TransitionTicks = 0L
                            then World.publish4 (SelectEventAddress + selectedScreenAddress) selectedScreenAddress (NoData ()) world
                            else world
                        match world.State.Liveness with
                        | Running ->
                            let world =
                                if selectedScreen.Incoming.TransitionTicks = 0L
                                then World.publish4 (StartIncomingEventAddress + selectedScreenAddress) selectedScreenAddress (NoData ()) world
                                else world
                            match world.State.Liveness with
                            | Running ->
                                let (finished, incoming) = World.updateTransition1 selectedScreen.Incoming
                                let selectedScreen = Screen.setIncoming incoming selectedScreen
                                let world = World.setScreen selectedScreenAddress selectedScreen world
                                if finished then
                                    let world = snd <| World.setScreenState IdlingState selectedScreenAddress selectedScreen world
                                    World.publish4 (FinishIncomingEventAddress + selectedScreenAddress) selectedScreenAddress (NoData ()) world
                                else world
                            | Exiting -> world
                        | Exiting -> world
                    | OutgoingState ->
                        let world =
                            if selectedScreen.Outgoing.TransitionTicks <> 0L then world
                            else World.publish4 (StartOutgoingEventAddress + selectedScreenAddress) selectedScreenAddress (NoData ()) world
                        match world.State.Liveness with
                        | Running ->
                            let (finished, outgoing) = World.updateTransition1 selectedScreen.Outgoing
                            let selectedScreen = Screen.setOutgoing outgoing selectedScreen
                            let world = World.setScreen selectedScreenAddress selectedScreen world
                            if finished then
                                let world = snd <| World.setScreenState IdlingState selectedScreenAddress selectedScreen world
                                let world = World.publish4 (DeselectEventAddress + selectedScreenAddress) selectedScreenAddress (NoData ()) world
                                match world.State.Liveness with
                                | Running -> World.publish4 (FinishOutgoingEventAddress + selectedScreenAddress) selectedScreenAddress (NoData ()) world
                                | Exiting -> world
                            else world
                        | Exiting -> world
                    | IdlingState -> world
                | None -> world
            match world.State.Liveness with
            | Running -> handleUpdate world
            | Exiting -> world

        static member private handleSplashScreenIdleTick idlingTime ticks event world =
            let world = World.unsubscribe SplashScreenTickKey world
            if ticks < idlingTime then
                let subscription = World.handleSplashScreenIdleTick idlingTime (inc ticks)
                let world = World.subscribe SplashScreenTickKey event.Address event.SubscriberAddress subscription world
                (Cascade, world)
            else
                match World.getOptSelectedScreenAddress world with
                | Some selectedScreenAddress ->
                    match World.getOptScreen selectedScreenAddress world with
                    | Some selectedScreen ->
                        let world = snd <| World.setScreenState OutgoingState selectedScreenAddress selectedScreen world
                        (Cascade, world)
                    | None ->
                        trace "Program Error: Could not handle splash screen tick due to no selected screen."
                        (Resolve, World.exit world)
                | None ->
                    trace "Program Error: Could not handle splash screen tick due to no selected screen."
                    (Resolve, World.exit world)

        static member internal handleSplashScreenIdle idlingTime event world =
            let subscription = World.handleSplashScreenIdleTick idlingTime 0L
            let world = World.subscribe SplashScreenTickKey TickEventAddress event.SubscriberAddress subscription world
            (Resolve, world)

        static member addSplashScreenFromData destination address screenDispatcherName incomingTime idlingTime outgoingTime image world =
            let splashScreen = World.makeDissolveScreen screenDispatcherName (Some <| Address.head address) incomingTime outgoingTime world
            let splashGroup = World.makeGroup typeof<GroupDispatcher>.Name (Some "SplashGroup") world
            let splashLabel = World.makeEntity typeof<LabelDispatcher>.Name (Some "SplashLabel") world
            let splashLabel = Entity.setSize world.Camera.EyeSize splashLabel
            let splashLabel = Entity.setPosition (-world.Camera.EyeSize * 0.5f) splashLabel
            let splashLabel = Entity.setLabelImage image splashLabel
            let splashGroupDescriptors = Map.singleton splashGroup.Name (splashGroup, Map.singleton splashLabel.Name splashLabel)
            let world = snd <| World.addScreen address splashScreen splashGroupDescriptors world
            let world = World.monitor (FinishIncomingEventAddress + address) address (World.handleSplashScreenIdle idlingTime) world
            let world = World.monitor (FinishOutgoingEventAddress + address) address (World.handleAsScreenTransitionFromSplash destination) world
            (splashScreen, world)

        static member addDissolveScreenFromFile screenDispatcherName groupFilePath incomingTime outgoingTime screenAddress world =
            let dissolveScreen = World.makeDissolveScreen screenDispatcherName (Some <| Address.head screenAddress) incomingTime outgoingTime world
            let (group, entities) = World.loadGroupFromFile groupFilePath world
            let dissolveGroupDescriptors = Map.singleton group.Name (group, entities)
            World.addScreen screenAddress dissolveScreen dissolveGroupDescriptors world

        static member private createIntrinsicOverlays dispatchers facets =

            let hasFacetNamesField = fun sourceType ->
                sourceType = typeof<EntityDispatcher>

            let usesFacets = fun sourceType ->
                sourceType = typeof<EntityDispatcher> ||
                sourceType.IsSubclassOf typeof<EntityDispatcher>

            let dispatchers = Map.toValueList dispatchers
            let facets = Map.toValueListBy (fun facet -> facet :> obj) facets
            let sources = facets @ dispatchers
            let sourceTypes = List.map (fun source -> source.GetType ()) sources
            Reflection.createIntrinsicOverlays hasFacetNamesField usesFacets sourceTypes

        static member tryReloadOverlays inputDirectory outputDirectory world =
            
            // try to reload overlay file
            let inputOverlayFilePath = Path.Combine (inputDirectory, world.State.OverlayFilePath)
            let outputOverlayFilePath = Path.Combine (outputDirectory, world.State.OverlayFilePath)
            try File.Copy (inputOverlayFilePath, outputOverlayFilePath, true)

                // cache old overlayer and make new one
                let oldOverlayer = world.State.Overlayer
                let dispatchers = World.getDispatchers world
                let intrinsicOverlays = World.createIntrinsicOverlays dispatchers world.Components.Facets
                let overlayer = Overlayer.make outputOverlayFilePath intrinsicOverlays
                let world = World.setOverlayer overlayer world

                // apply overlays to all entities
                let world =
                    Seq.fold
                        (fun world (address, entity : Entity) ->
                            let entity = { entity with Id = entity.Id } // hacky copy
                            match entity.OptOverlayName with
                            | Some overlayName ->
                                let oldFacetNames = entity.FacetNames
                                Overlayer.applyOverlayToFacetNames entity.OptOverlayName overlayName entity oldOverlayer world.State.Overlayer
                                match World.trySynchronizeFacets oldFacetNames (Some address) entity world with
                                | Right (entity, world) ->
                                    Overlayer.applyOverlay5 entity.OptOverlayName overlayName entity oldOverlayer world.State.Overlayer
                                    World.setEntity address entity world
                                | Left error -> note <| "There was an issue in applying a reloaded overlay: " + error; world
                            | None -> world)
                        world
                        (World.getEntities1 world)

                // right!
                Right world

            // propagate error
            with exn -> Left <| tcstring exn

        static member tryReloadAssets inputDirectory outputDirectory refinementDirectory world =
            
            // try to reload asset graph file
            try File.Copy (
                    Path.Combine (inputDirectory, world.State.AssetGraphFilePath),
                    Path.Combine (outputDirectory, world.State.AssetGraphFilePath), true)

                // reload asset graph
                match Assets.tryBuildAssetGraph inputDirectory outputDirectory refinementDirectory false world.State.AssetGraphFilePath with
                | Right () ->

                    // reload asset metadata
                    match Metadata.tryGenerateAssetMetadataMap world.State.AssetGraphFilePath with
                    | Right assetMetadataMap ->
                    
                        // reload assets
                        let world = World.setAssetMetadataMap assetMetadataMap world
                        let world = World.reloadRenderingAssets world
                        let world = World.reloadAudioAssets world
                        Right world
            
                    // propagate errors
                    | Left errorMsg -> Left errorMsg
                | Left error -> Left error
            with exn -> Left <| tcstring exn

        static member continueHack groupAddress world =
            // NOTE: since messages may be invalid upon continuing a world (especially physics
            // messages), all messages are eliminated. If this poses an issue, the editor will have
            // to instead store past / future worlds only once their current frame has been
            // processed (integrated, advanced, rendered, played, et al).
            let world = World.clearRenderingMessages world
            let world = World.clearAudioMessages world
            let world = World.clearPhysicsMessages world
            let world = World.addPhysicsMessage RebuildPhysicsHackMessage world
            let entities = World.getEntities groupAddress world
            Map.fold
                (fun world _ (entity : Entity) ->
                    let entityAddress = groupAddress @+ [entity.Name]
                    Entity.propagatePhysics entityAddress entity world)
                world
                entities

        static member private play world =
            let audioMessages = world.MessageQueues.AudioMessages
            let world = World.clearAudioMessages world
            let audioPlayer = world.Subsystems.AudioPlayer.Play audioMessages 
            World.setAudioPlayer audioPlayer world

        static member private getGroupRenderDescriptors world entities =
            Map.toValueListBy
                (fun entity -> Entity.getRenderDescriptors entity world)
                entities

        static member private getTransitionRenderDescriptors camera transition =
            match transition.OptDissolveImage with
            | Some dissolveImage ->
                let progress = single transition.TransitionTicks / single transition.TransitionLifetime
                let alpha = match transition.TransitionType with Incoming -> 1.0f - progress | Outgoing -> progress
                let color = Vector4 (Vector3.One, alpha)
                [LayerableDescriptor
                    { Depth = Single.MaxValue
                      LayeredDescriptor =
                        SpriteDescriptor
                            { Position = -camera.EyeSize * 0.5f // negation for right-handedness
                              Size = camera.EyeSize
                              Rotation = 0.0f
                              ViewType = Absolute
                              OptInset = None
                              Image = dissolveImage
                              Color = color }}]
            | None -> []

        static member private getRenderDescriptors world =
            match World.getOptSelectedScreenAddress world with
            | Some selectedScreenAddress ->
                let optGroupMap = Map.tryFind (Address.head selectedScreenAddress) world.Entities
                match optGroupMap with
                | Some groupMap ->
                    let groupValues = Map.toValueList groupMap
                    let entityMaps = List.fold List.flipCons [] groupValues
                    let descriptors = List.map (World.getGroupRenderDescriptors world) entityMaps
                    let descriptors = List.concat <| List.concat descriptors
                    let selectedScreen = World.getScreen selectedScreenAddress world
                    match selectedScreen.ScreenState with
                    | IncomingState -> descriptors @ World.getTransitionRenderDescriptors world.Camera selectedScreen.Incoming
                    | OutgoingState -> descriptors @ World.getTransitionRenderDescriptors world.Camera selectedScreen.Outgoing
                    | IdlingState -> descriptors
                | None -> []
            | None -> []

        static member private render world =
            let renderDescriptors = World.getRenderDescriptors world
            let renderingMessages = world.MessageQueues.RenderingMessages
            let world = World.clearRenderingMessages world
            let renderer = world.Subsystems.Renderer.Render (world.Camera, renderingMessages, renderDescriptors)
            World.setRenderer renderer world

        static member private handleIntegrationMessage world integrationMessage =
            match world.State.Liveness with
            | Running ->
                match integrationMessage with
                | BodyTransformMessage bodyTransformMessage ->
                    match World.getOptEntity bodyTransformMessage.EntityAddress world with
                    | Some entity -> snd <| World.handleBodyTransformMessage bodyTransformMessage bodyTransformMessage.EntityAddress entity world
                    | None -> world
                | BodyCollisionMessage bodyCollisionMessage ->
                    match World.getOptEntity bodyCollisionMessage.EntityAddress world with
                    | Some _ ->
                        let collisionAddress = CollisionEventAddress + bodyCollisionMessage.EntityAddress
                        let collisionData =
                            CollisionData
                                { Normal = bodyCollisionMessage.Normal
                                  Speed = bodyCollisionMessage.Speed
                                  Collidee = bodyCollisionMessage.EntityAddress2 }
                        World.publish4 collisionAddress Address.empty collisionData world
                    | None -> world
            | Exiting -> world

        static member private handleIntegrationMessages integrationMessages world =
            List.fold World.handleIntegrationMessage world integrationMessages

        static member private integrate world =
            if World.isPhysicsRunning world then
                let physicsMessages = world.MessageQueues.PhysicsMessages
                let world = World.clearPhysicsMessages world
                let integrationMessages = world.Subsystems.Integrator.Integrate physicsMessages
                World.handleIntegrationMessages integrationMessages world
            else world

        static member private processTask (tasksNotRun, world) task =
            if task.ScheduledTime < world.State.TickTime then
                debug <| "Task leak found for time '" + tcstring world.State.TickTime + "'."
                (tasksNotRun, world)
            elif task.ScheduledTime = world.State.TickTime then
                let world = task.Operation world
                (tasksNotRun, world)
            else (task :: tasksNotRun, world)

        static member private processTasks world =
            let tasks = List.rev world.Callbacks.Tasks
            let world = World.clearTasks world
            let (tasksNotRun, world) = List.fold World.processTask ([], world) tasks
            let tasksNotRun = List.rev tasksNotRun
            World.restoreTasks tasksNotRun world

        static member processInput (event : SDL.SDL_Event) world =
            let world =
                match event.``type`` with
                | SDL.SDL_EventType.SDL_QUIT ->
                    World.exit world
                | SDL.SDL_EventType.SDL_MOUSEMOTION ->
                    let mousePosition = Vector2 (single event.button.x, single event.button.y)
                    if World.isMouseButtonDown MouseLeft world
                    then World.publish World.sortSubscriptionsByPickingPriority MouseDragEventAddress Address.empty (MouseMoveData { Position = mousePosition }) world
                    else World.publish World.sortSubscriptionsByPickingPriority MouseMoveEventAddress Address.empty (MouseButtonData { Position = mousePosition; Button = MouseLeft }) world
                | SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN ->
                    let mousePosition = World.getMousePositionF world
                    let mouseButton = World.toNuMouseButton <| uint32 event.button.button
                    let mouseEventAddress = DownMouseEventAddress @+ [MouseButton.toEventName mouseButton]
                    let eventData = MouseButtonData { Position = mousePosition; Button = mouseButton }
                    World.publish World.sortSubscriptionsByPickingPriority mouseEventAddress Address.empty eventData world
                | SDL.SDL_EventType.SDL_MOUSEBUTTONUP ->
                    let mousePosition = World.getMousePositionF world
                    let mouseButton = World.toNuMouseButton <| uint32 event.button.button
                    let mouseEventAddress = UpMouseEventAddress @+ [MouseButton.toEventName mouseButton]
                    let eventData = MouseButtonData { Position = mousePosition; Button = mouseButton }
                    World.publish World.sortSubscriptionsByPickingPriority mouseEventAddress Address.empty eventData world
                | SDL.SDL_EventType.SDL_KEYDOWN ->
                    let keyboard = event.key
                    let key = keyboard.keysym
                    let eventData = KeyboardKeyData { ScanCode = int key.scancode; IsRepeat = keyboard.repeat <> byte 0 }
                    World.publish World.sortSubscriptionsByHierarchy DownKeyboardKeyEventAddress Address.empty eventData world
                | SDL.SDL_EventType.SDL_KEYUP ->
                    let keyboard = event.key
                    let key = keyboard.keysym
                    let eventData = KeyboardKeyData { ScanCode = int key.scancode; IsRepeat = keyboard.repeat <> byte 0 }
                    World.publish World.sortSubscriptionsByHierarchy UpKeyboardKeyEventAddress Address.empty eventData world
                | _ -> world
            (world.State.Liveness, world)

        static member processUpdate handleUpdate world =
            let world = World.integrate world
            match world.State.Liveness with
            | Running ->
                let world = World.publish4 TickEventAddress Address.empty (NoData ()) world
                match world.State.Liveness with
                | Running ->
                    let world = World.updateTransition handleUpdate world
                    match world.State.Liveness with
                    | Running ->
                        let world = World.processTasks world
                        (world.State.Liveness, world)
                    | Exiting -> (Exiting, world)
                | Exiting -> (Exiting, world)
            | Exiting -> (Exiting, world)

        static member processRender handleRender world =
            let world = World.render world
            handleRender world

        static member processPlay world =
            let world = World.play world
            World.incrementTickTime world

        static member exitRender world =
            let renderer = world.Subsystems.Renderer.HandleRenderExit () 
            World.setRenderer renderer world

        static member run4 tryMakeWorld handleUpdate handleRender sdlConfig =
            Sdl.run
                tryMakeWorld
                World.processInput
                (World.processUpdate handleUpdate)
                (World.processRender handleRender)
                World.processPlay
                World.exitRender
                sdlConfig

        static member run tryMakeWorld handleUpdate sdlConfig =
            World.run4 tryMakeWorld handleUpdate id sdlConfig

        static member tryMake
            sdlDeps
            (userComponentFactory : UserComponentFactory)
            interactivity
            farseerCautionMode
            userState =

            // attempt to generate asset metadata so the rest of the world can be created
            match Metadata.tryGenerateAssetMetadataMap AssetGraphFilePath with
            | Right assetMetadataMap ->

                // make user components
                let userEntityDispatchers = userComponentFactory.MakeEntityDispatchers ()
                let userGroupDispatchers = userComponentFactory.MakeGroupDispatchers ()
                let userScreenDispatchers = userComponentFactory.MakeScreenDispatchers ()
                let userGameDispatchers = userComponentFactory.MakeGameDispatchers ()
                let userFacets = userComponentFactory.MakeFacets ()

                // infer the active game dispatcher
                let defaultGameDispatcher = GameDispatcher ()
                let activeGameDispatcher =
                    match Map.toValueList userGameDispatchers with
                    | [] -> defaultGameDispatcher
                    | [singlet] -> singlet
                    | head :: _ ->
                        debug <|
                            "Received more than one GameDispatcher from userComponentFactory. " +
                            "Defaulting to '" + Reflection.getTypeName head + "'."
                        head

                // make entity dispatchers
                // TODO: see if we can reflectively generate these
                let defaultEntityDispatchers =
                    Map.ofList
                        [typeof<EntityDispatcher>.Name, EntityDispatcher ()
                         typeof<ButtonDispatcher>.Name, ButtonDispatcher () :> EntityDispatcher
                         typeof<LabelDispatcher>.Name, LabelDispatcher () :> EntityDispatcher
                         typeof<TextDispatcher>.Name, TextDispatcher () :> EntityDispatcher
                         typeof<ToggleDispatcher>.Name, ToggleDispatcher () :> EntityDispatcher
                         typeof<FeelerDispatcher>.Name, FeelerDispatcher () :> EntityDispatcher
                         typeof<FillBarDispatcher>.Name, FillBarDispatcher () :> EntityDispatcher
                         typeof<BlockDispatcher>.Name, BlockDispatcher () :> EntityDispatcher
                         typeof<BoxDispatcher>.Name, BoxDispatcher () :> EntityDispatcher
                         typeof<AvatarDispatcher>.Name, AvatarDispatcher () :> EntityDispatcher
                         typeof<CharacterDispatcher>.Name, CharacterDispatcher () :> EntityDispatcher
                         typeof<TileMapDispatcher>.Name, TileMapDispatcher () :> EntityDispatcher]
                let entityDispatchers = Map.addMany (Map.toSeq userEntityDispatchers) defaultEntityDispatchers

                // make group dispatchers
                let defaultGroupDispatchers = Map.ofList [typeof<GroupDispatcher>.Name, GroupDispatcher ()]
                let groupDispatchers = Map.addMany (Map.toSeq userGroupDispatchers) defaultGroupDispatchers

                // make screen dispatchers
                let defaultScreenDispatchers = Map.ofList [typeof<ScreenDispatcher>.Name, ScreenDispatcher ()]
                let screenDispatchers = Map.addMany (Map.toSeq userScreenDispatchers) defaultScreenDispatchers

                // make game dispatchers
                let defaultGameDispatchers = Map.ofList [typeof<GameDispatcher>.Name, defaultGameDispatcher]
                let gameDispatchers = Map.addMany (Map.toSeq userGameDispatchers) defaultGameDispatchers

                // make facets
                let defaultFacets =
                    Map.ofList
                        [typeof<RigidBodyFacet>.Name, RigidBodyFacet () :> Facet
                         typeof<SpriteFacet>.Name, SpriteFacet () :> Facet
                         typeof<AnimatedSpriteFacet>.Name, AnimatedSpriteFacet () :> Facet
                         typeof<UIFacet>.Name, UIFacet () :> Facet]
                let facets = Map.addMany (Map.toSeq userFacets) defaultFacets

                // make intrinsic overlays
                let dispatchers =
                    Map.map Map.objectify entityDispatchers @@
                    Map.map Map.objectify groupDispatchers @@
                    Map.map Map.objectify screenDispatchers @@
                    Map.map Map.objectify gameDispatchers
                let intrinsicOverlays = World.createIntrinsicOverlays dispatchers facets

                // make the world's components
                let components =
                    { EntityDispatchers = entityDispatchers
                      GroupDispatchers = groupDispatchers
                      ScreenDispatchers = screenDispatchers
                      GameDispatchers = gameDispatchers
                      Facets = facets }

                // make the world's subsystems
                let subsystems =
                    { AudioPlayer = AudioPlayer.make AssetGraphFilePath
                      Renderer = Renderer.make sdlDeps.RenderContext AssetGraphFilePath
                      Integrator = Integrator.make farseerCautionMode Gravity }

                // make the world's message queues
                let messageQueues =
                    { AudioMessages = [HintAudioPackageUseMessage { PackageName = DefaultPackageName }]
                      RenderingMessages = [HintRenderingPackageUseMessage { PackageName = DefaultPackageName }]
                      PhysicsMessages = [] }

                // make the world's callbacks
                let callbacks =
                    { Tasks = []
                      Subscriptions = Map.empty
                      Unsubscriptions = Map.empty
                      CallbackStates = Map.empty }

                // make the world's state
                let state =
                    { TickTime = 0L
                      Liveness = Running
                      Interactivity = interactivity
                      OptScreenTransitionDestinationAddress = None
                      AssetMetadataMap = assetMetadataMap
                      AssetGraphFilePath = AssetGraphFilePath
                      Overlayer = Overlayer.make OverlayFilePath intrinsicOverlays
                      OverlayFilePath = OverlayFilePath
                      UserState = userState }

                // make the world itself
                let world =
                    { Game = Game.make activeGameDispatcher <| Some DefaultGameName
                      Screens = Map.empty
                      Groups = Map.empty
                      Entities = Map.empty
                      Camera = let eyeSize = Vector2 (single sdlDeps.Config.ViewW, single sdlDeps.Config.ViewH) in { EyeCenter = Vector2.Zero; EyeSize = eyeSize }
                      Components = components
                      Subsystems = subsystems
                      MessageQueues = messageQueues
                      Callbacks = callbacks
                      State = state }

                // and finally, register the game
                let world = snd <| Game.register world.Game world
                Right world
            | Left errorMsg -> Left errorMsg

        static member makeEmpty (userState : 'u) =

            // the default dispatchers
            let entityDispatcher = EntityDispatcher ()
            let groupDispatcher = GroupDispatcher ()
            let screenDispatcher = ScreenDispatcher ()
            let gameDispatcher = GameDispatcher ()

            // make the world's components
            let components =
                { EntityDispatchers = Map.singleton (Reflection.getTypeName entityDispatcher) entityDispatcher
                  GroupDispatchers = Map.singleton (Reflection.getTypeName groupDispatcher) groupDispatcher
                  ScreenDispatchers = Map.singleton (Reflection.getTypeName screenDispatcher) screenDispatcher
                  GameDispatchers = Map.singleton (Reflection.getTypeName gameDispatcher) gameDispatcher
                  Facets = Map.empty }

            // make the world's subsystems
            let subsystems =
                { AudioPlayer = { MockAudioPlayer  = () }
                  Renderer = { MockRenderer = () }
                  Integrator = { MockIntegrator = () }}

            // make the world's message queues
            let messageQueues =
                { AudioMessages = []
                  RenderingMessages = []
                  PhysicsMessages = [] }

            // make the world's callbacks
            let callbacks =
                { Tasks = []
                  Subscriptions = Map.empty
                  Unsubscriptions = Map.empty
                  CallbackStates = Map.empty }

            // make the world's state
            let state =
                { TickTime = 0L
                  Liveness = Running
                  Interactivity = UIOnly
                  OptScreenTransitionDestinationAddress = None
                  AssetMetadataMap = Metadata.generateEmptyAssetMetadataMap ()
                  AssetGraphFilePath = String.Empty
                  Overlayer = Overlayer.makeEmpty ()
                  OverlayFilePath = String.Empty
                  UserState = userState }

            // make the world itself
            let world =
                { Game = Game.make gameDispatcher <| Some DefaultGameName
                  Screens = Map.empty
                  Groups = Map.empty
                  Entities = Map.empty
                  Camera = { EyeCenter = Vector2.Zero; EyeSize = Vector2 (single ResolutionXDefault, single ResolutionYDefault) }
                  Components = components
                  Subsystems = subsystems
                  MessageQueues = messageQueues
                  Callbacks = callbacks
                  State = state }

            // and finally, register the game
            snd <| Game.register world.Game world

        static member init () =

            // make types load reflectively from pathed (non-static) assemblies
            AppDomain.CurrentDomain.AssemblyLoad.Add
                (fun args -> LoadedAssemblies.[args.LoadedAssembly.FullName] <- args.LoadedAssembly)
            AppDomain.CurrentDomain.add_AssemblyResolve <| ResolveEventHandler
                (fun _ args -> snd <| LoadedAssemblies.TryGetValue args.Name)

            // ensure the current culture is invariate
            System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo.InvariantCulture

            // init math type converters
            Math.initTypeConverters ()

            // assign functions to the pub / sub refs.
            World.publish <- World.publishDefinition
            World.publish4 <- World.publish4Definition
            World.subscribe <- World.subscribeDefinition
            World.subscribe4 <- World.subscribe4Definition
            World.unsubscribe <- World.unsubscribeDefinition
            World.withSubscription <- World.withSubscriptionDefinition
            World.monitor <- World.monitorDefinition