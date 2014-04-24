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
open Nu.DomainModel
open Nu.Camera
open Nu.Entity
open Nu.Group
open Nu.Screen
open Nu.Game
open Nu.WorldPrims
module World =

    (* Function forwarding for WorldPrims in lieu of an F# export feature. *)
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

    let private play world =
        let audioMessages = world.AudioMessages
        let world' = { world with AudioMessages = [] }
        { world' with AudioPlayer = Nu.Audio.play audioMessages world.AudioPlayer }

    let private getGroupRenderDescriptors camera dispatcherContainer entities =
        let view = getInverseView camera
        let entityValues = Map.toValueSeq entities
        Seq.map (fun (entity : Entity) -> entity.GetRenderDescriptors (view, dispatcherContainer)) entityValues

    let private getTransitionRenderDescriptors camera dispatcherContainer transition =
        match transition.OptDissolveSprite with
        | None -> []
        | Some dissolveSprite ->
            let progress = single transition.Ticks / single transition.Lifetime
            let alpha = match transition.Type with Incoming -> 1.0f - progress | Outgoing -> progress
            let color = Vector4 (Vector3.One, alpha)
            [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = Vector2.Zero; Size = camera.EyeSize; Rotation = 0.0f; Sprite = dissolveSprite; Color = color }; Depth = Single.MaxValue })]

    let private getRenderDescriptors world =
        match get world worldOptSelectedScreenAddressLens with
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
                let activeScreen = get world (worldScreenLens activeScreenAddress)
                match activeScreen.State with
                | IncomingState -> descriptors @ getTransitionRenderDescriptors world.Camera world activeScreen.Incoming
                | OutgoingState -> descriptors @ getTransitionRenderDescriptors world.Camera world activeScreen.Outgoing
                | IdlingState -> descriptors

    let private render world =
        let renderMessages = world.RenderMessages
        let renderDescriptors = getRenderDescriptors world
        let renderer = world.Renderer
        let renderer' = Nu.Rendering.render renderMessages renderDescriptors renderer
        { world with RenderMessages = []; Renderer = renderer' }

    let private handleIntegrationMessage (keepRunning, world) integrationMessage : bool * World =
        if not keepRunning then (keepRunning, world)
        else
            match integrationMessage with
            | BodyTransformMessage bodyTransformMessage -> 
                let entityAddress = bodyTransformMessage.EntityAddress
                let entity = get world <| worldEntityLens entityAddress
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
                    let mouseEvent = addrstr DownMouseEvent <| str mouseButton
                    let world' = { world with MouseState = { world.MouseState with MouseDowns = Set.add mouseButton world.MouseState.MouseDowns }}
                    let messageData = MouseButtonData (world'.MouseState.MousePosition, mouseButton)
                    publish mouseEvent [] { Handled = false; Data = messageData } world'
                | SDL.SDL_EventType.SDL_MOUSEBUTTONUP ->
                    let mouseState = world.MouseState
                    let mouseButton = makeMouseButton event.button.button
                    let mouseEvent = addrstr UpMouseEvent <| str mouseButton
                    if Set.contains mouseButton mouseState.MouseDowns then
                        let world' = { world with MouseState = { world.MouseState with MouseDowns = Set.remove mouseButton world.MouseState.MouseDowns }}
                        let messageData = MouseButtonData (world'.MouseState.MousePosition, mouseButton)
                        publish mouseEvent [] { Handled = false; Data = messageData } world'
                    else (true, world)
                | _ -> (true, world))
            (fun world ->
                let (keepRunning, world') = integrate world
                if not keepRunning then (keepRunning, world')
                else
                    let (keepRunning', world'') = publish TickEvent [] { Handled = false; Data = NoData } world'
                    if not keepRunning' then (keepRunning', world'')
                    else updateTransition handleUpdate world'')
            (fun world -> let world' = render world in handleRender world')
            (fun world -> play world)
            (fun world -> { world with Renderer = handleRenderExit world.Renderer })
            sdlConfig

    let run tryCreateWorld handleUpdate sdlConfig =
        run4 tryCreateWorld handleUpdate id sdlConfig

    let addSplashScreenFromData handleFinishedOutgoing address incomingTime idlingTime outgoingTime sprite seal world =
        let splashScreen = makeDissolveScreen incomingTime outgoingTime
        let splashGroup = makeDefaultGroup ()
        let splashLabel = makeDefaultEntity (Lun.make typeof<LabelDispatcher>.Name) (Some "SplashLabel") seal world
        let splashLabel' = splashLabel.SetSize world.Camera.EyeSize
        let splashLabel'' = splashLabel'.SetLabelSprite (sprite : Sprite)
        let world' = addScreen address splashScreen [(Lun.make "SplashGroup", splashGroup, [splashLabel''])] world
        let world'' = subscribe (FinishedIncomingEvent @ address) address (CustomSub <| handleSplashScreenIdle idlingTime) world'
        subscribe (FinishedOutgoingEvent @ address) address handleFinishedOutgoing world''

    let addDissolveScreenFromFile groupFileName groupName incomingTime outgoingTime screenAddress seal world =
        let screen = makeDissolveScreen incomingTime outgoingTime
        let (_, group, entities, world') = loadGroupFile groupFileName world seal false
        addScreen screenAddress screen [(groupName, group, entities)] world'

    let tryCreateEmptyWorld sdlDeps userGameDispatcher (extData : obj) =
        match tryGenerateAssetMetadataMap "AssetGraph.xml" with
        | Left errorMsg -> Left errorMsg
        | Right assetMetadataMap ->
            let userGameDispatcherName = Lun.make (userGameDispatcher.GetType ()).Name
            let dispatchers =
                Map.ofArray
                    // TODO: see if we can reflectively generate this array
                    [|Lun.make typeof<EntityDispatcher>.Name, EntityDispatcher () :> obj
                      Lun.make typeof<Entity2dDispatcher>.Name, Entity2dDispatcher () :> obj
                      Lun.make typeof<ButtonDispatcher>.Name, ButtonDispatcher () :> obj
                      Lun.make typeof<LabelDispatcher>.Name, LabelDispatcher () :> obj
                      Lun.make typeof<TextBoxDispatcher>.Name, TextBoxDispatcher () :> obj
                      Lun.make typeof<ToggleDispatcher>.Name, ToggleDispatcher () :> obj
                      Lun.make typeof<FeelerDispatcher>.Name, FeelerDispatcher () :> obj
                      Lun.make typeof<FillBarDispatcher>.Name, FillBarDispatcher () :> obj
                      Lun.make typeof<BlockDispatcher>.Name, BlockDispatcher () :> obj
                      Lun.make typeof<AvatarDispatcher>.Name, AvatarDispatcher () :> obj
                      Lun.make typeof<TileMapDispatcher>.Name, TileMapDispatcher () :> obj
                      Lun.make typeof<GroupDispatcher>.Name, GroupDispatcher () :> obj
                      Lun.make typeof<TransitionDispatcher>.Name, TransitionDispatcher () :> obj
                      Lun.make typeof<ScreenDispatcher>.Name, ScreenDispatcher () :> obj
                      Lun.make typeof<GameDispatcher>.Name, GameDispatcher () :> obj
                      userGameDispatcherName, userGameDispatcher|]
            let world =
                { Game = { Id = getNuId (); OptSelectedScreenAddress = None; Xtension = { OptXTypeName = Some userGameDispatcherName; XFields = Map.empty; IsSealed = false }}
                  Screens = Map.empty
                  Groups = Map.empty
                  Entities = Map.empty
                  Camera = { EyePosition = Vector2.Zero; EyeSize = Vector2 (single sdlDeps.Config.ViewW, single sdlDeps.Config.ViewH) }
                  Subscriptions = Map.empty
                  MouseState = { MousePosition = Vector2.Zero; MouseDowns = Set.empty }
                  AudioPlayer = makeAudioPlayer ()
                  Renderer = makeRenderer sdlDeps.RenderContext
                  Integrator = makeIntegrator Gravity
                  AssetMetadataMap = assetMetadataMap
                  AudioMessages = [HintAudioPackageUse { FileName = "AssetGraph.xml"; PackageName = "Default"; HAPU = () }]
                  RenderMessages = [HintRenderingPackageUse { FileName = "AssetGraph.xml"; PackageName = "Default"; HRPU = () }]
                  PhysicsMessages = []
                  Dispatchers = dispatchers
                  ExtData = extData }
            let world' = world.Game.Register world
            Right world'

    let reregisterPhysicsHack groupAddress world =
        let entities = get world <| worldEntitiesLens groupAddress
        Map.fold (fun world _ (entity : Entity) -> entity.ReregisterPhysicsHack (groupAddress, world)) world entities