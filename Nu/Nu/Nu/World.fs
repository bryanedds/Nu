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
    let worldEntity = WorldPrims.worldEntity
    let worldOptEntity = WorldPrims.worldOptEntity
    let worldEntities = WorldPrims.worldEntities
    let withEntity = WorldPrims.withEntity
    let withOptEntity = WorldPrims.withOptEntity
    let tryWithEntity = WorldPrims.tryWithEntity
    let registerEntity = WorldPrims.registerEntity
    let unregisterEntity = WorldPrims.unregisterEntity
    let removeEntity = WorldPrims.removeEntity
    let removeEntities = WorldPrims.removeEntities
    let addEntity = WorldPrims.addEntity
    let addEntities = WorldPrims.addEntities
    let tryPickEntity = WorldPrims.tryPickEntity
    
    // Group forwarders.
    let worldGroup = WorldPrims.worldGroup
    let worldOptGroup = WorldPrims.worldOptGroup
    let worldGroups = WorldPrims.worldGroups
    let withGroup = WorldPrims.withGroup
    let withOptGroup = WorldPrims.withOptGroup
    let tryWithGroup = WorldPrims.tryWithGroup
    let registerGroup = WorldPrims.registerGroup
    let unregisterGroup = WorldPrims.unregisterGroup
    let removeGroup = WorldPrims.removeGroup
    let removeGroups = WorldPrims.removeGroups
    let addGroup = WorldPrims.addGroup
    let addGroups = WorldPrims.addGroups
    
    // Screen forwarders.
    let worldScreen = WorldPrims.worldScreen
    let worldOptScreen = WorldPrims.worldOptScreen
    let worldScreens = WorldPrims.worldScreens
    let withScreen = WorldPrims.withScreen
    let withOptScreen = WorldPrims.withOptScreen
    let tryWithScreen = WorldPrims.tryWithScreen
    let registerScreen = WorldPrims.registerScreen
    let unregisterScreen = WorldPrims.unregisterScreen
    let removeScreen = WorldPrims.removeScreen
    let addScreen = WorldPrims.addScreen

    // Game forwarders.
    let worldOptSelectedScreenAddress = WorldPrims.worldOptSelectedScreenAddress
    let worldOptSelectedScreen = WorldPrims.worldOptSelectedScreen
    
    // Other forwarders.
    let handleEventAsExit = WorldPrims.handleEventAsExit
    let handleEventAsScreenTransition = WorldPrims.handleEventAsScreenTransition
    let handleEventAsSwallow = WorldPrims.handleEventAsSwallow
    let initTypeConverters = WorldPrims.initTypeConverters
    let publish = WorldPrims.publish
    let subscribe = WorldPrims.subscribe
    let transitionScreen = WorldPrims.transitionScreen
    let unsubscribe = WorldPrims.unsubscribe
    let withSubscription = WorldPrims.withSubscription

    (* Normal functions. *)

    let activateGameDispatcher assemblyFileName gameDispatcherFullName world =
        let assembly = Assembly.LoadFrom assemblyFileName
        let gameDispatcherType = assembly.GetType gameDispatcherFullName
        let gameDispatcherShortName = gameDispatcherType.Name
        let gameDispatcher = Activator.CreateInstance gameDispatcherType
        let dispatchers = Map.add gameDispatcherShortName gameDispatcher world.Dispatchers
        let world' = { world with Dispatchers = dispatchers }
        let world'' = { world' with Game = { world'.Game with Xtension = { world'.Game.Xtension with OptXDispatcherName = Some gameDispatcherShortName }}}
        world''.Game.Register world''

    let saveGroupFile group entities fileName world =
        use file = File.Open (fileName, FileMode.Create)
        let writerSettings = XmlWriterSettings ()
        writerSettings.Indent <- true
        use writer = XmlWriter.Create (file, writerSettings)
        writer.WriteStartDocument ()
        writer.WriteStartElement "Root"
        Group.writeToXml writer group entities
        writer.WriteEndElement ()
        writer.WriteEndDocument ()

    let loadGroupFile (fileName : string) seal world =
        let document = XmlDocument ()
        document.Load fileName
        let rootNode = document.["Root"]
        let groupNode = rootNode.["Group"]
        Group.readFromXml groupNode typeof<GroupDispatcher>.Name typeof<EntityDispatcher>.Name seal world

    let private play world =
        let audioMessages = world.AudioMessages
        let world' = { world with AudioMessages = [] }
        { world' with AudioPlayer = Nu.Audio.play audioMessages world.AudioPlayer }

    let private getGroupRenderDescriptors camera dispatcherContainer entities =
        let entityValues = Map.toValueSeq entities
        let viewAbsolute = Camera.getViewAbsoluteI camera |> Matrix3.getInverseViewMatrix
        let viewRelative = Camera.getViewRelativeI camera |> Matrix3.getInverseViewMatrix
        Seq.map (fun (entity : Entity) -> entity.GetRenderDescriptors (viewAbsolute, viewRelative, dispatcherContainer)) entityValues

    let private getTransitionRenderDescriptors camera dispatcherContainer transition =
        match transition.OptDissolveSprite with
        | None -> []
        | Some dissolveSprite ->
            let progress = single transition.Ticks / single transition.Lifetime
            let alpha = match transition.Type with Incoming -> 1.0f - progress | Outgoing -> progress
            let color = Vector4 (Vector3.One, alpha)
            [LayerableDescriptor <|
                LayeredSpriteDescriptor
                    { Descriptor =
                        { Position = -camera.EyeSize * 0.5f // negation for right-handedness
                          Size = camera.EyeSize
                          Rotation = 0.0f
                          Sprite = dissolveSprite
                          Color = color }
                      Depth = Single.MaxValue }]

    let private getRenderDescriptors world =
        match get world worldOptSelectedScreenAddress with
        | None -> []
        | Some activeScreenAddress ->
            let optGroupMap = Map.tryFind activeScreenAddress.[0] world.Entities
            match optGroupMap with
            | None -> []
            | Some groupMap ->
                let entityMaps = List.fold List.flipCons [] <| Map.toValueList groupMap
                let descriptorSeqs = List.map (getGroupRenderDescriptors world.Camera world) entityMaps
                let descriptorSeq = Seq.concat descriptorSeqs
                let descriptors = List.concat descriptorSeq
                let activeScreen = get world <| worldScreen activeScreenAddress
                match activeScreen.State with
                | IncomingState -> descriptors @ getTransitionRenderDescriptors world.Camera world activeScreen.Incoming
                | OutgoingState -> descriptors @ getTransitionRenderDescriptors world.Camera world activeScreen.Outgoing
                | IdlingState -> descriptors

    let private render world =
        let renderMessages = world.RenderMessages
        let renderDescriptors = getRenderDescriptors world
        let renderer = world.Renderer
        let renderer' = Nu.Rendering.render world.Camera renderMessages renderDescriptors renderer
        { world with RenderMessages = []; Renderer = renderer' }

    let private handleIntegrationMessage (keepRunning, world) integrationMessage : bool * World =
        if not keepRunning then (keepRunning, world)
        else
            match integrationMessage with
            | BodyTransformMessage bodyTransformMessage -> 
                let entityAddress = bodyTransformMessage.EntityAddress
                let entity = get world <| worldEntity entityAddress
                (keepRunning, entity.HandleBodyTransformMessage (bodyTransformMessage, entityAddress, world))
            | BodyCollisionMessage bodyCollisionMessage ->
                let collisionAddress = straddr "Collision" bodyCollisionMessage.EntityAddress
                let collisionData = CollisionData (bodyCollisionMessage.Normal, bodyCollisionMessage.Speed, bodyCollisionMessage.EntityAddress2)
                let collisionMessage = { Handled = false; Data = collisionData }
                publish collisionAddress [] collisionMessage world

    let private handleIntegrationMessages integrationMessages world : bool * World =
        List.fold handleIntegrationMessage (true, world) integrationMessages

    let private integrate world =
        let integrationMessages = Nu.Physics.integrate world.PhysicsMessages world.Integrator
        let world' = { world with PhysicsMessages = [] }
        handleIntegrationMessages integrationMessages world'

    let run4 tryMakeWorld handleUpdate handleRender sdlConfig =
        Sdl.run
            (fun sdlDeps -> tryMakeWorld sdlDeps)
            (fun refEvent world ->
                let event = !refEvent
                match event.``type`` with
                | SDL.SDL_EventType.SDL_QUIT -> (false, world)
                | SDL.SDL_EventType.SDL_MOUSEMOTION ->
                    let mousePosition = Vector2 (single event.button.x, single event.button.y)
                    let world' = { world with MouseState = { world.MouseState with MousePosition = mousePosition }}
                    if Set.contains MouseLeft world'.MouseState.MouseDowns then publish MouseDragEvent [] { Handled = false; Data = MouseMoveData mousePosition } world'
                    else publish MouseMoveEvent [] { Handled = false; Data = MouseButtonData (mousePosition, MouseLeft) } world'
                | SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN ->
                    let mouseButton = Sdl.makeNuMouseButton event.button.button
                    let mouseEvent = addrstr DownMouseEvent <| string mouseButton
                    let world' = { world with MouseState = { world.MouseState with MouseDowns = Set.add mouseButton world.MouseState.MouseDowns }}
                    let messageData = MouseButtonData (world'.MouseState.MousePosition, mouseButton)
                    publish mouseEvent [] { Handled = false; Data = messageData } world'
                | SDL.SDL_EventType.SDL_MOUSEBUTTONUP ->
                    let mouseState = world.MouseState
                    let mouseButton = Sdl.makeNuMouseButton event.button.button
                    let mouseEvent = addrstr UpMouseEvent <| string mouseButton
                    if Set.contains mouseButton mouseState.MouseDowns then
                        let world' = { world with MouseState = { world.MouseState with MouseDowns = Set.remove mouseButton world.MouseState.MouseDowns }}
                        let messageData = MouseButtonData (world'.MouseState.MousePosition, mouseButton)
                        publish mouseEvent [] { Handled = false; Data = messageData } world'
                    else (true, world)
                | _ -> (true, world))
            (fun world ->
                let (keepRunning, world') = integrate world
                if not keepRunning then (false, world')
                else
                    let (keepRunning', world'') = publish TickEvent [] { Handled = false; Data = NoData } world'
                    if not keepRunning' then (false, world'')
                    else WorldPrims.updateTransition handleUpdate world'')
            (fun world -> let world' = render world in handleRender world')
            (fun world -> let world' = play world in { world' with Ticks = world'.Ticks + 1UL })
            (fun world -> { world with Renderer = Rendering.handleRenderExit world.Renderer })
            sdlConfig

    let run tryMakeWorld handleUpdate sdlConfig =
        run4 tryMakeWorld handleUpdate id sdlConfig

    let addSplashScreenFromData handleFinishedOutgoing address screenDispatcherName incomingTime idlingTime outgoingTime sprite seal world =
        let splashScreen = Screen.makeDissolve screenDispatcherName typeof<TransitionDispatcher>.Name incomingTime outgoingTime
        let splashGroup = Group.makeDefault typeof<GroupDispatcher>.Name
        let splashLabel = Entity.makeDefault typeof<LabelDispatcher>.Name (Some "SplashLabel") seal world
        let splashLabel' = splashLabel.SetSize world.Camera.EyeSize
        let splashLabel'' = splashLabel'.SetPosition <| -world.Camera.EyeSize * 0.5f
        let splashLabel'3 = splashLabel''.SetLabelSprite (sprite : Sprite)
        let world' = addScreen address splashScreen [("SplashGroup", splashGroup, [splashLabel'3])] world
        let world'' = subscribe (FinishedIncomingEvent @ address) address (CustomSub <| WorldPrims.handleSplashScreenIdle idlingTime) world'
        subscribe (FinishedOutgoingEvent @ address) address handleFinishedOutgoing world''

    let addDissolveScreenFromFile screenDispatcherName groupFileName groupName incomingTime outgoingTime screenAddress seal world =
        let screen = Screen.makeDissolve screenDispatcherName typeof<TransitionDispatcher>.Name incomingTime outgoingTime
        let (group, entities) = loadGroupFile groupFileName seal world
        addScreen screenAddress screen [(groupName, group, entities)] world

    let tryMakeEmpty sdlDeps gameDispatcher (extData : obj) =
        match Metadata.tryGenerateAssetMetadataMap AssetGraphFileName with
        | Left errorMsg -> Left errorMsg
        | Right assetMetadataMap ->
            let gameDispatcherName = (gameDispatcher.GetType ()).Name
            let dispatchers =
                Map.ofArray
                    // TODO: see if we can reflectively generate this array
                    [|typeof<EntityDispatcher>.Name, EntityDispatcher () :> obj
                      typeof<Entity2dDispatcher>.Name, Entity2dDispatcher () :> obj
                      typeof<ButtonDispatcher>.Name, ButtonDispatcher () :> obj
                      typeof<LabelDispatcher>.Name, LabelDispatcher () :> obj
                      typeof<TextBoxDispatcher>.Name, TextBoxDispatcher () :> obj
                      typeof<ToggleDispatcher>.Name, ToggleDispatcher () :> obj
                      typeof<FeelerDispatcher>.Name, FeelerDispatcher () :> obj
                      typeof<FillBarDispatcher>.Name, FillBarDispatcher () :> obj
                      typeof<BlockDispatcher>.Name, BlockDispatcher () :> obj
                      typeof<AvatarDispatcher>.Name, AvatarDispatcher () :> obj
                      typeof<TileMapDispatcher>.Name, TileMapDispatcher () :> obj
                      typeof<GroupDispatcher>.Name, GroupDispatcher () :> obj
                      typeof<TransitionDispatcher>.Name, TransitionDispatcher () :> obj
                      typeof<ScreenDispatcher>.Name, ScreenDispatcher () :> obj
                      typeof<GameDispatcher>.Name, GameDispatcher () :> obj
                      gameDispatcherName, gameDispatcher|]
            let world =
                { Game = { Id = NuCore.getId (); OptSelectedScreenAddress = None; FacetNamesNs = []; Xtension = { XFields = Map.empty; OptXDispatcherName = Some gameDispatcherName; CanDefault = true; Sealed = false }}
                  Screens = Map.empty
                  Groups = Map.empty
                  Entities = Map.empty
                  Ticks = 0UL
                  Camera = let eyeSize = Vector2 (single sdlDeps.Config.ViewW, single sdlDeps.Config.ViewH) in { EyeCenter = Vector2.Zero; EyeSize = eyeSize }
                  Subscriptions = Map.empty
                  MouseState = { MousePosition = Vector2.Zero; MouseDowns = Set.empty }
                  AudioPlayer = Audio.makeAudioPlayer ()
                  Renderer = Rendering.makeRenderer sdlDeps.RenderContext
                  Integrator = Physics.makeIntegrator Gravity
                  AssetMetadataMap = assetMetadataMap
                  AudioMessages = [HintAudioPackageUse { FileName = AssetGraphFileName; PackageName = DefaultPackageName; HAPU = () }]
                  RenderMessages = [HintRenderingPackageUse { FileName = AssetGraphFileName; PackageName = DefaultPackageName; HRPU = () }]
                  PhysicsMessages = []
                  Dispatchers = dispatchers
                  ExtData = extData }
            let world' = world.Game.Register world
            Right world'

    let reregisterPhysicsHack groupAddress world =
        let entities = get world <| worldEntities groupAddress
        Map.fold (fun world _ (entity : Entity) -> entity.ReregisterPhysicsHack (groupAddress, world)) world entities