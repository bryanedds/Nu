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
open FSharpx
open FSharpx.Lens.Operators
open SDL2
open OpenTK
open TiledSharp
open Prime
open Nu
open Nu.NuConstants

[<RequireQualifiedAccess>]
module World =

    (* Function forwarding for WorldPrims in lieu of an F# export feature. *)

    // Entity forwarders.
    let getEntity = WorldPrims.getEntity
    let setEntity = WorldPrims.setEntity
    let withEntity = WorldPrims.withEntity
    let withEntityAndWorld = WorldPrims.withEntityAndWorld
    let containsEntity = WorldPrims.containsEntity
    let getOptEntity = WorldPrims.getOptEntity
    let tryWithEntity = WorldPrims.tryWithEntity
    let tryWithEntityAndWorld = WorldPrims.tryWithEntityAndWorld
    let getEntities = WorldPrims.getEntities
    let registerEntity = WorldPrims.registerEntity
    let unregisterEntity = WorldPrims.unregisterEntity
    let removeEntityImmediate = WorldPrims.removeEntityImmediate
    let removeEntity = WorldPrims.removeEntity
    let clearEntitiesImmediate = WorldPrims.clearEntitiesImmediate
    let clearEntities = WorldPrims.clearEntities
    let removeEntitiesImmediate = WorldPrims.removeEntitiesImmediate
    let removeEntities = WorldPrims.removeEntities
    let addEntity = WorldPrims.addEntity
    let addEntities = WorldPrims.addEntities
    
    // Group forwarders.
    let getGroup = WorldPrims.getGroup
    let setGroup = WorldPrims.setGroup
    let withGroup = WorldPrims.withGroup
    let withGroupAndWorld = WorldPrims.withGroupAndWorld
    let containsGroup = WorldPrims.containsGroup
    let getOptGroup = WorldPrims.getOptGroup
    let tryWithGroup = WorldPrims.tryWithGroup
    let tryWithGroupAndWorld = WorldPrims.tryWithGroupAndWorld
    let getGroups = WorldPrims.getGroups
    let registerGroup = WorldPrims.registerGroup
    let unregisterGroup = WorldPrims.unregisterGroup
    let removeGroupImmediate = WorldPrims.removeGroupImmediate
    let removeGroup = WorldPrims.removeGroup
    let clearGroups = WorldPrims.clearGroups
    let clearGroupsImmediate = WorldPrims.clearGroupsImmediate
    let removeGroups = WorldPrims.removeGroups
    let removeGroupsImmediate = WorldPrims.removeGroupsImmediate
    let addGroup = WorldPrims.addGroup
    let addGroups = WorldPrims.addGroups
    
    // Screen forwarders.
    let getScreen = WorldPrims.getScreen
    let setScreen = WorldPrims.setScreen
    let withScreen = WorldPrims.withScreen
    let withScreenAndWorld = WorldPrims.withScreenAndWorld
    let containsScreen = WorldPrims.containsScreen
    let getOptScreen = WorldPrims.getOptScreen
    let tryWithScreen = WorldPrims.tryWithScreen
    let tryWithScreenAndWorld = WorldPrims.tryWithScreenAndWorld
    let getScreens = WorldPrims.getScreens
    let registerScreen = WorldPrims.registerScreen
    let unregisterScreen = WorldPrims.unregisterScreen
    let removeScreenImmediate = WorldPrims.removeScreenImmediate
    let removeScreen = WorldPrims.removeScreen
    let addScreen = WorldPrims.addScreen

    // Game forwarders.
    let getOptSelectedScreenAddress = WorldPrims.getOptSelectedScreenAddress
    let setOptSelectedScreenAddress = WorldPrims.setOptSelectedScreenAddress
    let getOptSelectedScreen = WorldPrims.getOptSelectedScreen
    let setOptSelectedScreen = WorldPrims.setOptSelectedScreen
    
    // Other forwarders.
    let isAddressSelected = WorldPrims.isAddressSelected
    let handleEventAsExit = WorldPrims.handleEventAsExit
    let handleEventAsScreenTransition = WorldPrims.handleEventAsScreenTransition
    let handleEventAsSwallow = WorldPrims.handleEventAsSwallow
    let initTypeConverters = WorldPrims.initTypeConverters
    let selectScreen = WorldPrims.selectScreen
    let transitionScreen = WorldPrims.transitionScreen
    let publish = WorldPrims.publish
    let subscribe = WorldPrims.subscribe
    let observe = WorldPrims.observe
    let unsubscribe = WorldPrims.unsubscribe
    let withSubscription = WorldPrims.withSubscription

    (* Normal functions. *)

    let activateGameDispatcher assemblyFileName gameDispatcherFullName world =
        let assembly = Assembly.LoadFrom assemblyFileName
        let gameDispatcherType = assembly.GetType gameDispatcherFullName
        let gameDispatcherShortName = gameDispatcherType.Name
        let gameDispatcher = Activator.CreateInstance gameDispatcherType
        let dispatchers = Map.add gameDispatcherShortName gameDispatcher world.Dispatchers
        let world = { world with Dispatchers = dispatchers }
        let world = { world with Game = { world.Game with Xtension = { world.Game.Xtension with OptXDispatcherName = Some gameDispatcherShortName }}}
        world.Game.Register world

    let saveGroupToFile group entities fileName (_ : World) =
        use file = File.Open (fileName, FileMode.Create)
        let writerSettings = XmlWriterSettings ()
        writerSettings.Indent <- true
        use writer = XmlWriter.Create (file, writerSettings)
        writer.WriteStartDocument ()
        writer.WriteStartElement "Root"
        Group.writeToXml writer group entities
        writer.WriteEndElement ()
        writer.WriteEndDocument ()

    let loadGroupFromFile fileName world =
        let document = XmlDocument ()
        document.Load (fileName : string)
        let rootNode = document.["Root"]
        let groupNode = rootNode.["Group"]
        Group.readFromXml groupNode typeof<GroupDispatcher>.Name typeof<EntityDispatcher>.Name world

    let private play world =
        let audioMessages = world.AudioMessages
        let world = { world with AudioMessages = [] }
        { world with AudioPlayer = Nu.Audio.play audioMessages world.AudioPlayer }

    let private getGroupRenderDescriptors dispatcherContainer entities =
        let entities = Map.toValueSeq entities
        Seq.map (fun (entity : Entity) -> entity.GetRenderDescriptors dispatcherContainer) entities

    let private getTransitionRenderDescriptors camera transition =
        match transition.OptDissolveSprite with
        | None -> []
        | Some dissolveSprite ->
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
                          Sprite = dissolveSprite
                          Color = color }}]

    let private getRenderDescriptors world =
        match getOptSelectedScreenAddress world with
        | None -> []
        | Some selectedScreenAddress ->
            let optGroupMap = Map.tryFind selectedScreenAddress.[0] world.Entities
            match optGroupMap with
            | None -> []
            | Some groupMap ->
                let entityMaps = List.fold List.flipCons [] <| Map.toValueList groupMap
                let descriptorSeqs = List.map (getGroupRenderDescriptors world) entityMaps
                let descriptorSeq = Seq.concat descriptorSeqs
                let descriptors = List.concat descriptorSeq
                let selectedScreen = getScreen selectedScreenAddress world
                match selectedScreen.State with
                | IncomingState -> descriptors @ getTransitionRenderDescriptors world.Camera selectedScreen.Incoming
                | OutgoingState -> descriptors @ getTransitionRenderDescriptors world.Camera selectedScreen.Outgoing
                | IdlingState -> descriptors

    let private render world =
        let renderMessages = world.RenderMessages
        let renderDescriptors = getRenderDescriptors world
        let renderer = world.Renderer
        let renderer = Nu.Rendering.render world.Camera renderMessages renderDescriptors renderer
        { world with RenderMessages = []; Renderer = renderer }

    let private handleIntegrationMessage world integrationMessage =
        match world.Liveness with
        | Exiting -> world
        | Running ->
            match integrationMessage with
            | BodyTransformMessage bodyTransformMessage ->
                match getOptEntity bodyTransformMessage.EntityAddress world with
                | None -> world
                | Some entity -> entity.HandleBodyTransformMessage (bodyTransformMessage.EntityAddress, bodyTransformMessage, world)
            | BodyCollisionMessage bodyCollisionMessage ->
                match getOptEntity bodyCollisionMessage.EntityAddress world with
                | None -> world
                | Some _ ->
                    let collisionAddress = CollisionEvent @ bodyCollisionMessage.EntityAddress
                    let collisionData = CollisionData (bodyCollisionMessage.Normal, bodyCollisionMessage.Speed, bodyCollisionMessage.EntityAddress2)
                    publish collisionAddress [] collisionData world

    let private handleIntegrationMessages integrationMessages world =
        List.fold handleIntegrationMessage world integrationMessages

    let private integrate world =
        let integrationMessages = Nu.Physics.integrate world.PhysicsMessages world.Integrator
        let world = { world with PhysicsMessages = [] }
        handleIntegrationMessages integrationMessages world

    let private runNextTask world =
        let task = List.head world.Tasks
        if task.Time <> world.Ticks then world
        else
            let world = task.Operation world
            { world with Tasks = List.tail world.Tasks }

    let private runTasks world =
        List.fold (fun world _ -> runNextTask world) world world.Tasks

    let run4 tryMakeWorld handleUpdate handleRender sdlConfig =
        Sdl.run
            (fun sdlDeps -> tryMakeWorld sdlDeps)
            (fun refEvent world ->
                let event = !refEvent
                let world =
                    match event.``type`` with
                    | SDL.SDL_EventType.SDL_QUIT -> { world with Liveness = Exiting }
                    | SDL.SDL_EventType.SDL_MOUSEMOTION ->
                        let mousePosition = Vector2 (single event.button.x, single event.button.y)
                        let world = { world with MouseState = { world.MouseState with MousePosition = mousePosition }}
                        if Set.contains MouseLeft world.MouseState.MouseDowns then publish MouseDragEvent [] (MouseMoveData mousePosition) world
                        else publish MouseMoveEvent [] (MouseButtonData (mousePosition, MouseLeft)) world
                    | SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN ->
                        let mouseButton = Sdl.makeNuMouseButton event.button.button
                        let mouseEvent = addrstr DownMouseEvent <| string mouseButton
                        let world = { world with MouseState = { world.MouseState with MouseDowns = Set.add mouseButton world.MouseState.MouseDowns }}
                        let messageData = MouseButtonData (world.MouseState.MousePosition, mouseButton)
                        publish mouseEvent [] messageData world
                    | SDL.SDL_EventType.SDL_MOUSEBUTTONUP ->
                        let mouseState = world.MouseState
                        let mouseButton = Sdl.makeNuMouseButton event.button.button
                        let mouseEvent = addrstr UpMouseEvent <| string mouseButton
                        if Set.contains mouseButton mouseState.MouseDowns then
                            let world = { world with MouseState = { world.MouseState with MouseDowns = Set.remove mouseButton world.MouseState.MouseDowns }}
                            let messageData = MouseButtonData (world.MouseState.MousePosition, mouseButton)
                            publish mouseEvent [] messageData world
                        else world
                    | _ -> world
                (world.Liveness, world))
            (fun world ->
                let world = integrate world
                match world.Liveness with
                | Exiting -> (Exiting, world)
                | Running ->
                    let world = publish TickEvent [] NoData world
                    match world.Liveness with
                    | Exiting -> (Exiting, world)
                    | Running ->
                        let world = WorldPrims.updateTransition handleUpdate world
                        match world.Liveness with
                        | Exiting -> (Exiting, world)
                        | Running ->
                            let world = runTasks world
                            (world.Liveness, world))
            (fun world -> let world = render world in handleRender world)
            (fun world -> let world = play world in { world with Ticks = world.Ticks + 1L })
            (fun world -> { world with Renderer = Rendering.handleRenderExit world.Renderer })
            sdlConfig

    let run tryMakeWorld handleUpdate sdlConfig =
        run4 tryMakeWorld handleUpdate id sdlConfig

    let addSplashScreenFromData destination address screenDispatcherName incomingTime idlingTime outgoingTime sprite world =
        let splashScreen = Screen.makeDissolve screenDispatcherName incomingTime outgoingTime
        let splashGroup = Group.makeDefault typeof<GroupDispatcher>.Name world
        let splashLabel = Entity.makeDefault typeof<LabelDispatcher>.Name (Some "SplashLabel") world
        let splashLabel = splashLabel.SetSize world.Camera.EyeSize
        let splashLabel = splashLabel.SetPosition <| -world.Camera.EyeSize * 0.5f
        let splashLabel = splashLabel.SetLabelSprite (sprite : Sprite)
        let world = addScreen address splashScreen [("SplashGroup", splashGroup, [splashLabel])] world
        let world = subscribe (FinishIncomingEvent @ address) address (CustomSub <| WorldPrims.handleSplashScreenIdle idlingTime) world
        subscribe (FinishOutgoingEvent @ address) address (ScreenTransitionFromSplashSub destination) world

    let addDissolveScreenFromFile screenDispatcherName groupFileName groupName incomingTime outgoingTime screenAddress world =
        let screen = Screen.makeDissolve screenDispatcherName incomingTime outgoingTime
        let (group, entities) = loadGroupFromFile groupFileName world
        let world = addScreen screenAddress screen [(groupName, group, entities)] world
        world

    let tryMakeEmpty sdlDeps gameDispatcher interactive extData =
        match Metadata.tryGenerateAssetMetadataMap AssetGraphFileName with
        | Left errorMsg -> Left errorMsg
        | Right assetMetadataMap ->
            let gameDispatcherName = (gameDispatcher.GetType ()).Name
            let dispatchers =
                Map.ofList
                    // TODO: see if we can reflectively generate this array
                    [typeof<EntityDispatcher>.Name, EntityDispatcher () :> obj
                     typeof<ButtonDispatcher>.Name, ButtonDispatcher () :> obj
                     typeof<LabelDispatcher>.Name, LabelDispatcher () :> obj
                     typeof<TextBoxDispatcher>.Name, TextBoxDispatcher () :> obj
                     typeof<ToggleDispatcher>.Name, ToggleDispatcher () :> obj
                     typeof<FeelerDispatcher>.Name, FeelerDispatcher () :> obj
                     typeof<FillBarDispatcher>.Name, FillBarDispatcher () :> obj
                     typeof<BlockDispatcher>.Name, BlockDispatcher () :> obj
                     typeof<AvatarDispatcher>.Name, AvatarDispatcher () :> obj
                     typeof<CharacterDispatcher>.Name, CharacterDispatcher () :> obj
                     typeof<TileMapDispatcher>.Name, TileMapDispatcher () :> obj
                     typeof<GroupDispatcher>.Name, GroupDispatcher () :> obj
                     typeof<TransitionDispatcher>.Name, TransitionDispatcher () :> obj
                     typeof<ScreenDispatcher>.Name, ScreenDispatcher () :> obj
                     typeof<GameDispatcher>.Name, GameDispatcher () :> obj
                     gameDispatcherName, gameDispatcher]
            let world =
                { Game = { Id = NuCore.getId (); OptSelectedScreenAddress = None; Xtension = { XFields = Map.empty; OptXDispatcherName = Some gameDispatcherName; CanDefault = true; Sealed = false }}
                  Screens = Map.empty
                  Groups = Map.empty
                  Entities = Map.empty
                  Ticks = 0L
                  Liveness = Running
                  Interactive = interactive
                  Camera = let eyeSize = Vector2 (single sdlDeps.Config.ViewW, single sdlDeps.Config.ViewH) in { EyeCenter = Vector2.Zero; EyeSize = eyeSize }
                  Subscriptions = Map.empty
                  Tasks = []
                  MouseState = { MousePosition = Vector2.Zero; MouseDowns = Set.empty }
                  AudioPlayer = Audio.makeAudioPlayer ()
                  Renderer = Rendering.makeRenderer sdlDeps.RenderContext
                  Integrator = Physics.makeIntegrator Gravity
                  AssetMetadataMap = assetMetadataMap
                  AudioMessages = [HintAudioPackageUse { FileName = AssetGraphFileName; PackageName = DefaultPackageName }]
                  RenderMessages = [HintRenderingPackageUse { FileName = AssetGraphFileName; PackageName = DefaultPackageName }]
                  PhysicsMessages = []
                  Dispatchers = dispatchers
                  ExtData = extData }
            let world = world.Game.Register world
            Right world

    let rebuildPhysicsHack groupAddress world =
        let outstandingMessages = world.PhysicsMessages
        let world = { world with PhysicsMessages = [] }
        let entities = getEntities groupAddress world
        let world =
            Map.fold
                (fun world _ (entity : Entity) -> entity.PropagatePhysics (groupAddress @ [entity.Name], world))
                world
                entities
        { world with PhysicsMessages = outstandingMessages @ world.PhysicsMessages @ [RebuildPhysicsHackMessage]}