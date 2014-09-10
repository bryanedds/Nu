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
open Nu.NuConstants

[<AutoOpen>]
module WorldModule =

    type World with

        static member makeSubscriptionKey () =
            Guid.NewGuid ()

        static member handleEventAsSwallow (world : World) =
            (Resolved, world)

        static member handleEventAsExit (world : World) =
            (Resolved, { world with Liveness = Exiting })

    let private ScreenTransitionDownMouseKey = World.makeSubscriptionKey ()
    let private ScreenTransitionUpMouseKey = World.makeSubscriptionKey ()
    let private SplashScreenTickKey = World.makeSubscriptionKey ()
    let private AnyEventNamesCache = Dictionary<Address, Address list> HashIdentity.Structural

    type World with

        static member tryIsSelectedScreenIdling world =
            match World.getOptSelectedScreen world with
            | Some selectedScreen -> Some <| Screen.isIdling selectedScreen
            | None -> None

        static member isSelectedScreenIdling world =
            match World.tryIsSelectedScreenIdling world with
            | Some answer -> answer
            | None -> failwith <| "Cannot query state of non-existent selected screen."
        
        static member private setScreenState state address screen world =
            let screen = Screen.setState state screen
            let world =
                match state with
                | IdlingState ->
                    world |>
                        World.unsubscribe ScreenTransitionDownMouseKey |>
                        World.unsubscribe ScreenTransitionUpMouseKey
                | IncomingState | OutgoingState ->
                    world |>
                        World.subscribe ScreenTransitionDownMouseKey (DownMouseEventName + AnyEventName) address SwallowSub |>
                        World.subscribe ScreenTransitionUpMouseKey (UpMouseEventName + AnyEventName) address SwallowSub
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
                    let sub = CustomSub (fun _ world ->
                        let world = World.unsubscribe subscriptionKey world
                        let world = snd <| World.selectScreen destinationAddress destinationScreen world
                        (Propagate, world))
                    let world = snd <| World.setScreenState OutgoingState selectedScreenAddress selectedScreen world
                    let world = World.subscribe subscriptionKey (FinishOutgoingEventName + selectedScreenAddress) selectedScreenAddress sub world
                    Some world
                | None -> None
            | None -> None

        static member handleEventAsScreenTransitionFromSplash destinationAddress world =
            let destinationScreen = World.getScreen destinationAddress world
            let world = snd <| World.selectScreen destinationAddress destinationScreen world
            (Propagate, world)

        static member handleEventAsScreenTransition destinationAddress world =
            let destinationScreen = World.getScreen destinationAddress world
            match World.tryTransitionScreen destinationAddress destinationScreen world with
            | Some world -> (Propagate, world)
            | None ->
                trace <| "Program Error: Invalid screen transition for destination address '" + string destinationAddress + "'."
                (Propagate, world)

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
            | _ -> failwith <| "Invalid simulant address '" + string address + "'."

        static member getOptSimulant address world =
            match address.AddrList with
            | [] -> Some <| Game world.Game
            | [_] -> Option.map Screen <| World.getOptScreen address world
            | [_; _] -> Option.map Group <| World.getOptGroup address world
            | [_; _; _] -> Option.map Entity <| World.getOptEntity address world
            | _ -> failwith <| "Invalid simulant address '" + string address + "'."

        static member getPublishingPriority getEntityPublishingPriority simulant world =
            match simulant with
            | Game _ -> GamePublishingPriority
            | Screen _ -> ScreenPublishingPriority
            | Group _ -> GroupPublishingPriority
            | Entity entity -> getEntityPublishingPriority entity world

        static member getSubscriptions getEntityPublishingPriority subscriptions world =
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
        static member getAnyEventNames eventName =
            match eventName.AddrList with
            | [] -> failwith "Event name cannot be empty."
            | _ :: _ ->
                let anyEventNamesKey = Address.allButLast eventName
                let refAnyEventNames = ref Unchecked.defaultof<Address list>
                if AnyEventNamesCache.TryGetValue (anyEventNamesKey, refAnyEventNames) then !refAnyEventNames
                else
                    let eventNameList = eventName.AddrList
                    let anyEventNameList = AnyEventName.AddrList
                    let anyEventNames =
                        [for i in 0 .. List.length eventNameList - 1 do
                            let subNameList = List.take i eventNameList @ anyEventNameList
                            yield Address.make subNameList]
                    AnyEventNamesCache.Add (anyEventNamesKey, anyEventNames)
                    anyEventNames

        static member getSubscriptionsSorted publishSorter eventName world =
            let anyEventNames = World.getAnyEventNames eventName
            let optSubLists = List.map (fun anyEventName -> Map.tryFind anyEventName world.Subscriptions) anyEventNames
            let optSubLists = Map.tryFind eventName world.Subscriptions :: optSubLists
            let subLists = List.definitize optSubLists
            let subList = List.concat subLists
            publishSorter subList world

        /// Publish an event.
        static member publishDefinition publishSorter eventName publisher eventData world =
            let subscriptions = World.getSubscriptionsSorted publishSorter eventName world
            let (_, world) =
                List.foldWhile
                    (fun (eventHandling, world) (_, subscriber, subscription) ->
                        let event = { Name = eventName; Publisher = publisher; Subscriber = subscriber; Data = eventData }
                        if  (match eventHandling with Resolved -> true | Propagate -> false) ||
                            (match world.Liveness with Running -> false | Exiting -> true) then
                            None
                        else
                            let result =
                                match subscription with
                                | ExitSub -> World.handleEventAsExit world
                                | SwallowSub -> World.handleEventAsSwallow world
                                | ScreenTransitionSub destination -> World.handleEventAsScreenTransition destination world
                                | ScreenTransitionFromSplashSub destination -> World.handleEventAsScreenTransitionFromSplash destination world
                                | CustomSub fn ->
                                    match World.getOptSimulant event.Subscriber world with
                                    | Some _ -> fn event world
                                    | None -> (Propagate, world)
                            Some result)
                    (Propagate, world)
                    subscriptions
            world

        /// Publish an event.
        static member publish4Definition eventName publisher eventData world =
            World.publish World.sortSubscriptionsByHierarchy eventName publisher eventData world

        /// Subscribe to an event.
        static member subscribeDefinition subscriptionKey eventName subscriber subscription world =
            let subscriptions = 
                match Map.tryFind eventName world.Subscriptions with
                | Some subscriptionList -> Map.add eventName ((subscriptionKey, subscriber, subscription) :: subscriptionList) world.Subscriptions
                | None -> Map.add eventName [(subscriptionKey, subscriber, subscription)] world.Subscriptions
            let unsubscriptions = Map.add subscriptionKey (eventName, subscriber) world.Unsubscriptions
            { world with Subscriptions = subscriptions; Unsubscriptions = unsubscriptions }

        /// Subscribe to an event.
        static member subscribe4Definition eventName subscriber subscription world =
            World.subscribe (World.makeSubscriptionKey ()) eventName subscriber subscription world

        /// Unsubscribe from an event.
        static member unsubscribeDefinition subscriptionKey world =
            match Map.tryFind subscriptionKey world.Unsubscriptions with
            | Some (eventName, subscriber) ->
                match Map.tryFind eventName world.Subscriptions with
                | Some subscriptionList ->
                    let subscriptionList =
                        List.remove
                            (fun (subscriptionKey', subscriber', _) -> subscriptionKey' = subscriptionKey && subscriber' = subscriber)
                            subscriptionList
                    let subscriptions = 
                        match subscriptionList with
                        | [] -> Map.remove eventName world.Subscriptions
                        | _ -> Map.add eventName subscriptionList world.Subscriptions
                    let unsubscriptions = Map.remove subscriptionKey world.Unsubscriptions
                    { world with Subscriptions = subscriptions; Unsubscriptions = unsubscriptions }
                | None -> world // TODO: consider failure signal
            | None -> world // TODO: consider failure signal

        /// Keep active a subscription for the duration of a procedure.
        static member withSubscriptionDefinition eventName subscriber subscription procedure world =
            let subscriptionKey = World.makeSubscriptionKey ()
            let world = World.subscribe subscriptionKey eventName subscriber subscription world
            let world = procedure world
            World.unsubscribe subscriptionKey world

        /// Keep active a subscription for the lifetime of a simulant.
        static member observeDefinition eventName subscriber subscription world =
            if Address.isEmpty subscriber then
                debug "Cannot observe events with an anonymous subscriber."
                world
            else
                let observationKey = World.makeSubscriptionKey ()
                let removalKey = World.makeSubscriptionKey ()
                let world = World.subscribe observationKey eventName subscriber subscription world
                let sub = CustomSub (fun _ world ->
                    let world = World.unsubscribe removalKey world
                    let world = World.unsubscribe observationKey world
                    (Propagate, world))
                World.subscribe removalKey (RemovingEventName + subscriber) subscriber sub world

        static member private updateTransition1 (transition : Transition) =
            if transition.TransitionTicks = transition.TransitionLifetime then (true, { transition with TransitionTicks = 0L })
            else (false, { transition with TransitionTicks = transition.TransitionTicks + 1L })

        // TODO: split this function up...
        static member internal updateTransition handleUpdate world =
            let world =
                match World.getOptSelectedScreenAddress world with
                | Some selectedScreenAddress ->
                    let selectedScreen = World.getScreen selectedScreenAddress world
                    match selectedScreen.State with
                    | IncomingState ->
                        let world =
                            if selectedScreen.Incoming.TransitionTicks = 0L
                            then World.publish4 (SelectEventName + selectedScreenAddress) selectedScreenAddress NoData world
                            else world
                        match world.Liveness with
                        | Running ->
                            let world =
                                if selectedScreen.Incoming.TransitionTicks = 0L
                                then World.publish4 (StartIncomingEventName + selectedScreenAddress) selectedScreenAddress NoData world
                                else world
                            match world.Liveness with
                            | Running ->
                                let (finished, incoming) = World.updateTransition1 selectedScreen.Incoming
                                let selectedScreen = Screen.setIncoming incoming selectedScreen
                                let world = World.setScreen selectedScreenAddress selectedScreen world
                                if finished then
                                    let world = snd <| World.setScreenState IdlingState selectedScreenAddress selectedScreen world
                                    World.publish4 (FinishIncomingEventName + selectedScreenAddress) selectedScreenAddress NoData world
                                else world
                            | Exiting -> world
                        | Exiting -> world
                    | OutgoingState ->
                        let world =
                            if selectedScreen.Outgoing.TransitionTicks <> 0L then world
                            else World.publish4 (StartOutgoingEventName + selectedScreenAddress) selectedScreenAddress NoData world
                        match world.Liveness with
                        | Running ->
                            let (finished, outgoing) = World.updateTransition1 selectedScreen.Outgoing
                            let selectedScreen = Screen.setOutgoing outgoing selectedScreen
                            let world = World.setScreen selectedScreenAddress selectedScreen world
                            if finished then
                                let world = snd <| World.setScreenState IdlingState selectedScreenAddress selectedScreen world
                                let world = World.publish4 (DeselectEventName + selectedScreenAddress) selectedScreenAddress NoData world
                                match world.Liveness with
                                | Running -> World.publish4 (FinishOutgoingEventName + selectedScreenAddress) selectedScreenAddress NoData world
                                | Exiting -> world
                            else world
                        | Exiting -> world
                    | IdlingState -> world
                | None -> world
            match world.Liveness with
            | Running -> handleUpdate world
            | Exiting -> world

        static member private handleSplashScreenIdleTick idlingTime ticks event world =
            let world = World.unsubscribe SplashScreenTickKey world
            if ticks < idlingTime then
                let subscription = CustomSub <| World.handleSplashScreenIdleTick idlingTime (incL ticks)
                let world = World.subscribe SplashScreenTickKey event.Name event.Subscriber subscription world
                (Propagate, world)
            else
                match World.getOptSelectedScreenAddress world with
                | Some selectedScreenAddress ->
                    match World.getOptScreen selectedScreenAddress world with
                    | Some selectedScreen ->
                        let world = snd <| World.setScreenState OutgoingState selectedScreenAddress selectedScreen world
                        (Propagate, world)
                    | None ->
                        trace "Program Error: Could not handle splash screen tick due to no selected screen."
                        (Resolved, { world with Liveness = Exiting })
                | None ->
                    trace "Program Error: Could not handle splash screen tick due to no selected screen."
                    (Resolved, { world with Liveness = Exiting })

        static member internal handleSplashScreenIdle idlingTime event world =
            let subscription = CustomSub <| World.handleSplashScreenIdleTick idlingTime 0L
            let world = World.subscribe SplashScreenTickKey TickEventName event.Subscriber subscription world
            (Resolved, world)

        static member addSplashScreenFromData destination address screenDispatcherName incomingTime idlingTime outgoingTime image world =
            let splashScreen = World.makeDissolveScreen screenDispatcherName (Some <| Address.head address) incomingTime outgoingTime world
            let splashGroup = World.makeGroup typeof<GroupDispatcher>.Name (Some "SplashGroup") world
            let splashLabel = World.makeEntity typeof<LabelDispatcher>.Name (Some "SplashLabel") world
            let splashLabel = Entity.setSize world.Camera.EyeSize splashLabel
            let splashLabel = Entity.setPosition (-world.Camera.EyeSize * 0.5f) splashLabel
            let splashLabel = Entity.setLabelImage image splashLabel
            let splashGroupDescriptors = [(splashGroup.Name, splashGroup, Map.singleton splashLabel.Name splashLabel)]
            let world = snd <| World.addScreen address splashScreen splashGroupDescriptors world
            let world = World.observe (FinishIncomingEventName + address) address (CustomSub <| World.handleSplashScreenIdle idlingTime) world
            let world = World.observe (FinishOutgoingEventName + address) address (ScreenTransitionFromSplashSub destination) world
            (splashScreen, world)

        static member addDissolveScreenFromFile screenDispatcherName groupFileName groupName incomingTime outgoingTime screenAddress world =
            let dissolveScreen = World.makeDissolveScreen screenDispatcherName (Some <| Address.head screenAddress) incomingTime outgoingTime world
            let (group, entities) = World.loadGroupFromFile groupFileName world
            World.addScreen screenAddress dissolveScreen [(groupName, group, entities)] world

        static member activateGameDispatcher assemblyFileName gameDispatcherFullName world =
            let assembly = Assembly.LoadFrom assemblyFileName
            let gameDispatcherType = assembly.GetType gameDispatcherFullName
            let gameDispatcherShortName = gameDispatcherType.Name
            let gameDispatcher = Activator.CreateInstance gameDispatcherType
            let dispatchers = Map.add gameDispatcherShortName gameDispatcher world.Dispatchers
            let world = { world with Dispatchers = dispatchers }
            let world = { world with Game = { world.Game with Xtension = { world.Game.Xtension with OptXDispatcherName = Some gameDispatcherShortName }}}
            world.Game.Register world

        static member createIntrinsicOverlays dispatchers facets =

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

        static member saveGroupToFile group entities fileName world =
            use file = File.Open (fileName, FileMode.Create)
            let writerSettings = XmlWriterSettings ()
            writerSettings.Indent <- true
            // NOTE: XmlWriter can also write to an XmlDocument / XmlNode instance by using
            // XmlWriter.Create <| (document.CreateNavigator ()).AppendChild ()
            use writer = XmlWriter.Create (file, writerSettings)
            writer.WriteStartDocument ()
            writer.WriteStartElement RootNodeName
            World.writeGroup world.Overlayer writer group entities
            writer.WriteEndElement ()
            writer.WriteEndDocument ()

        static member loadGroupFromFile fileName world =
            let document = XmlDocument ()
            document.Load (fileName : string)
            let rootNode = document.[RootNodeName]
            let groupNode = rootNode.[GroupNodeName]
            World.readGroup groupNode typeof<GroupDispatcher>.Name typeof<EntityDispatcher>.Name world

        static member tryReloadOverlays inputDir outputDir world =
            
            // try to reload overlay file
            let inputOverlayFileName = Path.Combine (inputDir, world.OverlayFileName)
            let outputOverlayFileName = Path.Combine (outputDir, world.OverlayFileName)
            try File.Copy (inputOverlayFileName, outputOverlayFileName, true)

                // cache old overlayer and make new one
                let oldOverlayer = world.Overlayer
                let intrinsicOverlays = World.createIntrinsicOverlays world.Dispatchers world.Facets
                let world = { world with Overlayer = Overlayer.make outputOverlayFileName intrinsicOverlays }

                // apply overlays to all entities
                let world =
                    Seq.fold
                        (fun world (address, entity : Entity) ->
                            let entity = { entity with Id = entity.Id } // hacky copy
                            match entity.OptOverlayName with
                            | Some overlayName ->
                                let oldFacetNames = entity.FacetNames
                                Overlayer.applyOverlayToFacetNames entity.OptOverlayName overlayName entity oldOverlayer world.Overlayer
                                match World.trySynchronizeFacets oldFacetNames (Some address) entity world with
                                | Right (entity, world) ->
                                    Overlayer.applyOverlay5 entity.OptOverlayName overlayName entity oldOverlayer world.Overlayer
                                    World.setEntity address entity world
                                | Left _ -> world // TODO: consider if we should debug or note here.. or something?
                            | None -> world)
                        world
                        (World.getEntities1 world)

                // right!
                Right world

            // propagate error
            with exn -> Left <| string exn

        static member tryReloadAssets inputDir outputDir world =
            
            // try to reload asset graph file
            try File.Copy (
                    Path.Combine (inputDir, world.AssetGraphFileName),
                    Path.Combine (outputDir, world.AssetGraphFileName), true)

                // reload asset graph
                match Assets.tryBuildAssetGraph inputDir outputDir world.AssetGraphFileName with
                | Right () ->

                    // reload asset metadata
                    match Metadata.tryGenerateAssetMetadataMap world.AssetGraphFileName with
                    | Right assetMetadataMap ->
                    
                        // reload assets
                        let world = { world with AssetMetadataMap = assetMetadataMap }
                        let world = World.reloadRenderingAssets world
                        let world = World.reloadAudioAssets world
                        Right world
            
                    // propagate errors
                    | Left errorMsg -> Left errorMsg
                | Left error -> Left error
            with exn -> Left <| string exn

        static member continueHack groupAddress world =
            // NOTE: since messages may be invalid upon continuing a world (especially physics
            // messages), all messages are eliminated. If this poses an issue, the editor will have
            // to instead store past / future worlds only once their current frame has been
            // processed (integrated, advanced, rendered, played, et al).
            let world =
                { world with
                    AudioMessages = []
                    RenderMessages = []
                    PhysicsMessages = [RebuildPhysicsHackMessage] }
            let entities = World.getEntities groupAddress world
            Map.fold
                (fun world _ (entity : Entity) ->
                    let entityAddress = addrlist groupAddress [entity.Name]
                    Entity.propagatePhysics entityAddress entity world)
                world
                entities

        static member private play world =
            let audioMessages = world.AudioMessages
            let world = { world with AudioMessages = [] }
            { world with AudioPlayer = Nu.Audio.play audioMessages world.AudioPlayer }

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
                    match selectedScreen.State with
                    | IncomingState -> descriptors @ World.getTransitionRenderDescriptors world.Camera selectedScreen.Incoming
                    | OutgoingState -> descriptors @ World.getTransitionRenderDescriptors world.Camera selectedScreen.Outgoing
                    | IdlingState -> descriptors
                | None -> []
            | None -> []

        static member private render world =
            let renderDescriptors = World.getRenderDescriptors world
            let renderMessages = world.RenderMessages
            let world = { world with RenderMessages = [] }
            let renderer = Nu.Rendering.render world.Camera renderMessages renderDescriptors world.Renderer
            { world with Renderer = renderer }

        static member private handleIntegrationMessage world integrationMessage =
            match world.Liveness with
            | Running ->
                match integrationMessage with
                | BodyTransformMessage bodyTransformMessage ->
                    match World.getOptEntity bodyTransformMessage.EntityAddress world with
                    | Some entity -> snd <| Entity.handleBodyTransformMessage bodyTransformMessage.EntityAddress bodyTransformMessage entity world
                    | None -> world
                | BodyCollisionMessage bodyCollisionMessage ->
                    match World.getOptEntity bodyCollisionMessage.EntityAddress world with
                    | Some _ ->
                        let collisionAddress = CollisionEventName + bodyCollisionMessage.EntityAddress
                        let collisionData =
                            EntityCollisionData
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
                let physicsMessages = world.PhysicsMessages
                let world = { world with PhysicsMessages = [] }
                let integrationMessages = Nu.Physics.integrate physicsMessages world.Integrator
                World.handleIntegrationMessages integrationMessages world
            else world

        static member private runNextTask world =
            let task = List.head world.Tasks
            if task.ScheduledTime = world.TickTime then
                let world = task.Operation world
                { world with Tasks = List.tail world.Tasks }
            else world

        static member private runTasks world =
            List.fold (fun world _ -> World.runNextTask world) world world.Tasks

        // TODO: split this function up
        static member run4 tryMakeWorld handleUpdate handleRender sdlConfig =
            Sdl.run
                (fun sdlDeps -> tryMakeWorld sdlDeps)
                (fun refEvent world ->
                    let event = !refEvent
                    let world =
                        match event.``type`` with
                        | SDL.SDL_EventType.SDL_QUIT ->
                            { world with Liveness = Exiting }
                        | SDL.SDL_EventType.SDL_MOUSEMOTION ->
                            let mousePosition = Vector2 (single event.button.x, single event.button.y)
                            if World.isMouseButtonDown MouseLeft world
                            then World.publish World.sortSubscriptionsByPickingPriority MouseDragEventName Address.empty (MouseMoveData { Position = mousePosition }) world
                            else World.publish World.sortSubscriptionsByPickingPriority MouseMoveEventName Address.empty (MouseButtonData { Position = mousePosition; Button = MouseLeft }) world
                        | SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN ->
                            let mousePosition = World.getMousePositionF world
                            let mouseButton = World.toNuMouseButton <| uint32 event.button.button
                            let mouseEventName = addrlist DownMouseEventName [string mouseButton]
                            let eventData = MouseButtonData { Position = mousePosition; Button = mouseButton }
                            World.publish World.sortSubscriptionsByPickingPriority mouseEventName Address.empty eventData world
                        | SDL.SDL_EventType.SDL_MOUSEBUTTONUP ->
                            let mousePosition = World.getMousePositionF world
                            let mouseButton = World.toNuMouseButton <| uint32 event.button.button
                            let mouseEventName = addrlist UpMouseEventName [string mouseButton]
                            let eventData = MouseButtonData { Position = mousePosition; Button = mouseButton }
                            World.publish World.sortSubscriptionsByPickingPriority mouseEventName Address.empty eventData world
                        | SDL.SDL_EventType.SDL_KEYDOWN ->
                            let key = event.key.keysym
                            let eventData = KeyboardKeyData { ScanCode = uint32 key.scancode }
                            World.publish World.sortSubscriptionsByHierarchy DownKeyboardKeyEventName Address.empty eventData world
                        | SDL.SDL_EventType.SDL_KEYUP ->
                            let key = event.key.keysym
                            let eventData = KeyboardKeyData { ScanCode = uint32 key.scancode }
                            World.publish World.sortSubscriptionsByHierarchy UpKeyboardKeyEventName Address.empty eventData world
                        | _ -> world
                    (world.Liveness, world))
                (fun world ->
                    let world = World.integrate world
                    match world.Liveness with
                    | Running ->
                        let world = World.publish4 TickEventName Address.empty NoData world
                        match world.Liveness with
                        | Running ->
                            let world = World.updateTransition handleUpdate world
                            match world.Liveness with
                            | Running ->
                                let world = World.runTasks world
                                (world.Liveness, world)
                            | Exiting -> (Exiting, world)
                        | Exiting -> (Exiting, world)
                    | Exiting -> (Exiting, world))
                (fun world -> let world = World.render world in handleRender world)
                (fun world -> let world = World.play world in { world with TickTime = world.TickTime + 1L })
                (fun world -> { world with Renderer = Rendering.handleRenderExit world.Renderer })
                sdlConfig

        static member run tryMakeWorld handleUpdate sdlConfig =
            World.run4 tryMakeWorld handleUpdate id sdlConfig

        static member tryMake
            sdlDeps
            (userComponentFactory : IUserComponentFactory)
            interactivity
            farseerCautionMode
            extData =

            // attempt to generate asset metadata so the rest of the world can be created
            match Metadata.tryGenerateAssetMetadataMap AssetGraphFileName with
            | Right assetMetadataMap ->

                // make user dispatchers
                let userDispatchers = userComponentFactory.MakeUserDispatchers ()

                // infer the active game dispatcher
                let defaultGameDispatcher = GameDispatcher () :> obj
                let userDispatcherList = Map.toValueList userDispatchers
                let isGameDispatcher = fun (dispatcher : obj) -> match dispatcher with :? GameDispatcher -> true | _ -> false
                let activeGameDispatcher =
                    match List.tryFind isGameDispatcher userDispatcherList with
                    | Some userGameDispatcher -> userGameDispatcher
                    | None -> defaultGameDispatcher
                let activeGameDispatcherName = Reflection.getTypeName activeGameDispatcher

                // make dispatchers
                // TODO: see if we can reflectively generate this
                let defaultDispatchers =
                    Map.ofList
                        [typeof<EntityDispatcher>.Name, EntityDispatcher () :> obj
                         typeof<ButtonDispatcher>.Name, ButtonDispatcher () :> obj
                         typeof<LabelDispatcher>.Name, LabelDispatcher () :> obj
                         typeof<TextDispatcher>.Name, TextDispatcher () :> obj
                         typeof<ToggleDispatcher>.Name, ToggleDispatcher () :> obj
                         typeof<FeelerDispatcher>.Name, FeelerDispatcher () :> obj
                         typeof<FillBarDispatcher>.Name, FillBarDispatcher () :> obj
                         typeof<BlockDispatcher>.Name, BlockDispatcher () :> obj
                         typeof<AvatarDispatcher>.Name, AvatarDispatcher () :> obj
                         typeof<CharacterDispatcher>.Name, CharacterDispatcher () :> obj
                         typeof<TileMapDispatcher>.Name, TileMapDispatcher () :> obj
                         typeof<GroupDispatcher>.Name, GroupDispatcher () :> obj
                         typeof<ScreenDispatcher>.Name, ScreenDispatcher () :> obj
                         typeof<GameDispatcher>.Name, defaultGameDispatcher]
                let userDispatchers = userComponentFactory.MakeUserDispatchers ()
                let dispatchers = Map.addMany (Map.toSeq userDispatchers) defaultDispatchers

                // make facets
                // TODO: see if we can reflectively generate this
                let defaultFacets =
                    Map.ofList
                        [typeof<RigidBodyFacet>.Name, RigidBodyFacet () :> Facet
                         typeof<SpriteFacet>.Name, SpriteFacet () :> Facet
                         typeof<AnimatedSpriteFacet>.Name, AnimatedSpriteFacet () :> Facet]
                let userFacets = userComponentFactory.MakeUserFacets ()
                let facets = Map.addMany (Map.toSeq userFacets) defaultFacets

                // make intrinsic overlays
                let intrinsicOverlays = World.createIntrinsicOverlays dispatchers facets

                let world =
                    { Game = Game.make activeGameDispatcherName activeGameDispatcher (Some "Game")
                      Screens = Map.empty
                      Groups = Map.empty
                      Entities = Map.empty
                      TickTime = 0L
                      Liveness = Running
                      Interactivity = interactivity
                      Camera = let eyeSize = Vector2 (single sdlDeps.Config.ViewW, single sdlDeps.Config.ViewH) in { EyeCenter = Vector2.Zero; EyeSize = eyeSize }
                      Tasks = []
                      Subscriptions = Map.empty
                      Unsubscriptions = Map.empty
                      AudioPlayer = Audio.makeAudioPlayer AssetGraphFileName
                      Renderer = Rendering.makeRenderer sdlDeps.RenderContext AssetGraphFileName
                      Integrator = Physics.makeIntegrator farseerCautionMode Gravity
                      AssetMetadataMap = assetMetadataMap
                      Overlayer = Overlayer.make OverlayFileName intrinsicOverlays
                      AudioMessages = [HintAudioPackageUseMessage { PackageName = DefaultPackageName }]
                      RenderMessages = [HintRenderingPackageUseMessage { PackageName = DefaultPackageName }]
                      PhysicsMessages = []
                      Dispatchers = dispatchers
                      Facets = facets
                      AssetGraphFileName = AssetGraphFileName
                      OverlayFileName = OverlayFileName
                      ExtData = extData }
                let world = snd <| world.Game.Register world
                Right world
            | Left errorMsg -> Left errorMsg

        static member init () =
            NuMath.initTypeConverters ()
            Audio.initTypeConverters ()
            Rendering.initTypeConverters ()
            World.publish <- World.publishDefinition
            World.publish4 <- World.publish4Definition
            World.subscribe <- World.subscribeDefinition
            World.subscribe4 <- World.subscribe4Definition
            World.unsubscribe <- World.unsubscribeDefinition
            World.withSubscription <- World.withSubscriptionDefinition
            World.observe <- World.observeDefinition