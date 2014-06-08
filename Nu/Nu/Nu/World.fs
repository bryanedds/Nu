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
open Nu.NuCore
open Nu.NuConstants
open Nu.NuMath
open Nu.Physics
open Nu.Rendering
open Nu.Metadata
open Nu.Audio
open Nu.Sdl
open Nu.Camera
open Nu.Sim
open Nu.Entity
open Nu.Group
open Nu.Screen
open Nu.Game
open Nu.WorldPrims
module World =

    (* Function forwarding for WorldPrims in lieu of an F# export feature. *)

    // Entity forwarders.
    let worldEntity = worldEntity
    let worldOptEntity = worldOptEntity
    let worldEntities = worldEntities
    let withEntity = withEntity
    let withOptEntity = withOptEntity
    let tryWithEntity = tryWithEntity
    let registerEntity = registerEntity
    let unregisterEntity = unregisterEntity
    let removeEntity = removeEntity
    let removeEntities = removeEntities
    let addEntity = addEntity
    let addEntities = addEntities
    
    // Group forwarders.
    let worldGroup = worldGroup
    let worldOptGroup = worldOptGroup
    let worldGroups = worldGroups
    let withGroup = withGroup
    let withOptGroup = withOptGroup
    let tryWithGroup = tryWithGroup
    let registerGroup = registerGroup
    let unregisterGroup = unregisterGroup
    let removeGroup = removeGroup
    let removeGroups = removeGroups
    let addGroup = addGroup
    let addGroups = addGroups
    
    // Screen forwarders.
    let worldScreen = worldScreen
    let worldOptScreen = worldOptScreen
    let worldScreens = worldScreens
    let withScreen = withScreen
    let withOptScreen = withOptScreen
    let tryWithScreen = tryWithScreen
    let registerScreen = registerScreen
    let unregisterScreen = unregisterScreen
    let removeScreen = removeScreen
    let addScreen = addScreen
    
    // Other forwarders.
    let handleEventAsExit = handleEventAsExit
    let handleEventAsScreenTransition = handleEventAsScreenTransition
    let handleEventAsSwallow = handleEventAsSwallow
    let handleMessage = handleMessage
    let initTypeConverters = initTypeConverters
    let publish = publish
    let subscribe = subscribe
    let transitionScreen = transitionScreen
    let unsubscribe = unsubscribe
    let withSubscription = withSubscription

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
        writeGroupToXml writer group entities
        writer.WriteEndElement ()
        writer.WriteEndDocument ()

    let loadGroupFile (fileName : string) seal world =
        let document = XmlDocument ()
        document.Load fileName
        let rootNode = document.["Root"]
        let groupNode = rootNode.["Group"]
        readGroupFromXml groupNode typeof<GroupDispatcher>.Name typeof<EntityDispatcher>.Name seal world

    let private play world =
        let audioMessages = world.AudioMessages
        let world' = { world with AudioMessages = [] }
        { world' with AudioPlayer = Nu.Audio.play audioMessages world.AudioPlayer }

    let private getGroupRenderDescriptors camera dispatcherContainer entities =
        let entityValues = Map.toValueSeq entities
        let viewAbsolute = getViewAbsoluteI camera |> Matrix3.getInverseViewMatrix
        let viewRelative = getViewRelativeI camera |> Matrix3.getInverseViewMatrix
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

    let run4 tryCreateWorld handleUpdate handleRender sdlConfig =
        runSdl
            (fun sdlDeps -> tryCreateWorld sdlDeps)
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
                    let mouseButton = makeMouseButton event.button.button
                    let mouseEvent = addrstr DownMouseEvent <| string mouseButton
                    let world' = { world with MouseState = { world.MouseState with MouseDowns = Set.add mouseButton world.MouseState.MouseDowns }}
                    let messageData = MouseButtonData (world'.MouseState.MousePosition, mouseButton)
                    publish mouseEvent [] { Handled = false; Data = messageData } world'
                | SDL.SDL_EventType.SDL_MOUSEBUTTONUP ->
                    let mouseState = world.MouseState
                    let mouseButton = makeMouseButton event.button.button
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
                    else updateTransition handleUpdate world'')
            (fun world -> let world' = render world in handleRender world')
            (fun world -> let world' = play world in { world' with Ticks = world'.Ticks + 1UL })
            (fun world -> { world with Renderer = handleRenderExit world.Renderer })
            sdlConfig

    let run tryCreateWorld handleUpdate sdlConfig =
        run4 tryCreateWorld handleUpdate id sdlConfig

    let addSplashScreenFromData handleFinishedOutgoing address screenDispatcherName incomingTime idlingTime outgoingTime sprite seal world =
        let splashScreen = makeDissolveScreen screenDispatcherName typeof<TransitionDispatcher>.Name incomingTime outgoingTime
        let splashGroup = makeDefaultGroup typeof<GroupDispatcher>.Name
        let splashLabel = makeDefaultEntity typeof<LabelDispatcher>.Name (Some "SplashLabel") seal world
        let splashLabel' = splashLabel.SetSize world.Camera.EyeSize
        let splashLabel'' = splashLabel'.SetPosition <| -world.Camera.EyeSize * 0.5f
        let splashLabel'3 = splashLabel''.SetLabelSprite (sprite : Sprite)
        let world' = addScreen address splashScreen [("SplashGroup", splashGroup, [splashLabel'3])] world
        let world'' = subscribe (FinishedIncomingEvent @ address) address (CustomSub <| handleSplashScreenIdle idlingTime) world'
        subscribe (FinishedOutgoingEvent @ address) address handleFinishedOutgoing world''

    let addDissolveScreenFromFile screenDispatcherName groupFileName groupName incomingTime outgoingTime screenAddress seal world =
        let screen = makeDissolveScreen screenDispatcherName typeof<TransitionDispatcher>.Name incomingTime outgoingTime
        let (group, entities) = loadGroupFile groupFileName seal world
        addScreen screenAddress screen [(groupName, group, entities)] world

    let tryCreateEmptyWorld sdlDeps gameDispatcher (extData : obj) =
        match tryGenerateAssetMetadataMap AssetGraphFileName with
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
                { Game = { Id = getNuId (); OptSelectedScreenAddress = None; Xtension = { XFields = Map.empty; OptXDispatcherName = Some gameDispatcherName; CanDefault = true; Sealed = false }}
                  Screens = Map.empty
                  Groups = Map.empty
                  Entities = Map.empty
                  Ticks = 0UL
                  Camera = let eyeSize = Vector2 (single sdlDeps.Config.ViewW, single sdlDeps.Config.ViewH) in { EyeCenter = Vector2.Zero; EyeSize = eyeSize }
                  Subscriptions = Map.empty
                  MouseState = { MousePosition = Vector2.Zero; MouseDowns = Set.empty }
                  AudioPlayer = makeAudioPlayer ()
                  Renderer = makeRenderer sdlDeps.RenderContext
                  Integrator = makeIntegrator Gravity
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