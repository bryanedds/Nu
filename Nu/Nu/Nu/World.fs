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
open Nu
open Nu.Core
open Nu.Constants
open Nu.Math
open Nu.Physics
open Nu.Rendering
open Nu.Metadata
open Nu.Audio
open Nu.Sdl
open Nu.DomainModel
open Nu.CameraModule
open Nu.Entities
open Nu.Groups
open Nu.Screens
open Nu.Games
module WorldModule =

    let TickAddress = addr "tick"
    let MouseDragAddress = addr "mouse/drag"
    let MouseMoveAddress = addr "mouse/move"
    let MouseLeftAddress = addr "mouse/left"
    let MouseCenterAddress = addr "mouse/center"
    let MouseRightAddress = addr "mouse/right"
    let DownMouseLeftAddress = straddr "down" MouseLeftAddress
    let DownMouseCenterAddress = straddr "down" MouseCenterAddress
    let DownMousRightAddress = straddr "down" MouseRightAddress
    let UpMouseLeftAddress = straddr "up" MouseLeftAddress
    let UpMouseCenterAddress = straddr "up" MouseCenterAddress
    let UpMouseRightAddress = straddr "up" MouseRightAddress
    let GameModelPublishingPriority = Single.MaxValue
    let ScreenModelPublishingPriority = GameModelPublishingPriority * 0.5f
    let GroupModelPublishingPriority = ScreenModelPublishingPriority * 0.5f
    let FinishedIncomingAddressPart = addr "finished/incoming"
    let FinishedOutgoingAddressPart = addr "finished/outgoing"
    let FieldFeelerName = Lun.make "feeler"
    let FieldAvatarName = Lun.make "avatar"

    /// Initialize Nu's various type converters.
    /// Must be called for reflection to work in Nu.
    let initTypeConverters () =
        initMathConverters ()
        initAudioConverters ()
        initRenderConverters ()

    /// Mark a message as handled.
    let handle message =
        { Handled = true; Data = message.Data }

    let isAddressSelected address world =
        let optScreenAddress = (get world worldGameLens).OptSelectedScreenModelAddress
        match (address, optScreenAddress) with
        | ([], _) -> true
        | (_, None) -> false
        | (_, Some []) -> false
        | (addressHead :: addressTail, Some (screenAddressHead :: _)) -> addressHead = screenAddressHead

    let sortFstAsc (priority, _) (priority2, _) =
        if priority = priority2 then 0
        elif priority > priority2 then -1
        else 1

    let getPublishingPriority simulant world =
        match simulant with
        | GameModel _ -> GameModelPublishingPriority
        | ScreenModel _ -> ScreenModelPublishingPriority
        | GroupModel _ -> GroupModelPublishingPriority
        | EntityModel entityModel -> getPickingPriority world.XDispatchers entityModel

    let getSimulant address world =
        match address with
        | [] -> GameModel <| get world gameModelLens
        | [_] as screenAddress -> ScreenModel <| get world (worldScreenModelLens screenAddress)
        | [_; _] as groupAddress -> GroupModel <| get world (worldGroupModelLens groupAddress)
        | [_; _; _] as entityAddress -> EntityModel <| get world (worldEntityModelLens entityAddress)
        | _ -> failwith <| "Invalid simulant address '" + str address + "'."

    let getSimulants subscriptions world =
        List.map (fun (address, _) -> getSimulant address world) subscriptions

    let pickingSort entityModels world =
        let priorities = List.map (getPickingPriority world.XDispatchers) entityModels
        let prioritiesAndEntityModels = List.zip priorities entityModels
        let prioritiesAndEntityModelsSorted = List.sortWith sortFstAsc prioritiesAndEntityModels
        List.map snd prioritiesAndEntityModelsSorted

    let tryPick (position : Vector2) entityModels world =
        let entityModelsSorted = pickingSort entityModels world
        List.tryFind
            (fun entityModel ->
                let transform = getEntityModelTransform (Some world.Camera) world.XDispatchers entityModel
                position.X >= transform.Position.X &&
                    position.X < transform.Position.X + transform.Size.X &&
                    position.Y >= transform.Position.Y &&
                    position.Y < transform.Position.Y + transform.Size.Y)
            entityModelsSorted

    let subscriptionSort subscriptions world =
        let simulants = getSimulants subscriptions world
        let priorities = List.map (fun simulant -> getPublishingPriority simulant world) simulants
        let prioritiesAndSubscriptions = List.zip priorities subscriptions
        let prioritiesAndSubscriptionsSorted = List.sortWith sortFstAsc prioritiesAndSubscriptions
        List.map snd prioritiesAndSubscriptionsSorted

    /// Publish a message to the given address.
    let publish address message world : bool * World =
        let optSubList = Map.tryFind address world.Subscriptions
        match optSubList with
        | None -> (true, world)
        | Some subList ->
            let subListSorted = subscriptionSort subList world
            let (_, keepRunning, world'') =
                List.foldWhile
                    (fun (message', keepRunning', world'3) (subscriber, (Subscription subscription)) ->
                        if message'.Handled || not keepRunning' then None
                        elif isAddressSelected subscriber world'3 then Some (subscription address subscriber message' world'3)
                        else Some (message', keepRunning', world'3))
                    (message, true, world)
                    subListSorted
            (keepRunning, world'')

    /// Subscribe to messages at the given address.
    let subscribe address subscriber subscription world =
        let sub = Subscription subscription
        let subs = world.Subscriptions
        let optSubList = Map.tryFind address subs
        { world with
            Subscriptions =
                match optSubList with
                | None -> Map.add address [(subscriber, sub)] subs
                | Some subList -> Map.add address ((subscriber, sub) :: subList) subs }

    /// Unsubscribe to messages at the given address.
    let unsubscribe address subscriber world =
        let subs = world.Subscriptions
        let optSubList = Map.tryFind address subs
        match optSubList with
        | None -> world
        | Some subList ->
            let subList' = List.remove (fun (address, _) -> address = subscriber) subList
            let subscriptions' = Map.add address subList' subs
            { world with Subscriptions = subscriptions' }

    /// Execute a procedure within the context of a given subscription at the given address.
    let withSubscription address subscription subscriber procedure world =
        let world' = subscribe address subscriber subscription world
        let world'' = procedure world'
        unsubscribe address subscriber world''
    
    let handleEventAsSwallow _ _ message world =
        (handle message, true, world)

    let handleEventAsExit _ _ message world =
        (handle message, false, world)

    // TODO: consider turning this into a lens, and removing the screenStateLens
    let getScreenModelState address world =
        let screenModel = get world <| worldScreenModelLens address
        get screenModel screenStateLens

    let setScreenModelState state address world =
        let screenModel = set state (get world <| worldScreenModelLens address) screenStateLens
        let world' = set screenModel world <| worldScreenModelLens address
        match state with
        | IdlingState ->
            world' |>
                unsubscribe DownMouseLeftAddress address |>
                unsubscribe UpMouseLeftAddress address
        | IncomingState | OutgoingState ->
            world' |>
                subscribe DownMouseLeftAddress address handleEventAsSwallow |>
                subscribe UpMouseLeftAddress address handleEventAsSwallow

    let transitionScreen address world =
        let world' = setScreenModelState IncomingState address world
        set (Some address) world' worldOptSelectedScreenModelAddressLens

    let transitionScreenHandler address _ _ message world =
        let world' = transitionScreen address world
        (handle message, true, world')

    let handleFinishedScreenOutgoing screenAddress destScreenAddress address subscriber message world =
        let world' = unsubscribe address subscriber world
        let world'' = transitionScreen destScreenAddress world'
        (handle message, true, world'')

    let handleEventAsScreenTransition screenAddress destScreenAddress address subscriber message world =
        let world' = subscribe (FinishedOutgoingAddressPart @ screenAddress) [] (handleFinishedScreenOutgoing screenAddress destScreenAddress) world
        let optSelectedScreenAddress = get world' worldOptSelectedScreenModelAddressLens
        match optSelectedScreenAddress with
        | None ->
            trace <| "Program Error: Could not handle click as screen transition due to no selected screen model."
            (handle message, true, world)
        | Some selectedScreenAddress ->
            let world'' = setScreenModelState OutgoingState selectedScreenAddress world'
            (handle message, true, world'')

    let updateTransition1 transitionModel =
        let transition = get transitionModel transitionLens
        let (transition', finished) =
            if transition.Ticks = transition.Lifetime then ({ transition with Ticks = 0 }, true)
            else ({ transition with Ticks = transition.Ticks + 1 }, false)
        let transitionModel' = set transition' transitionModel transitionLens
        (transitionModel', finished)

    let updateTransition update world : bool * World =
        let (keepRunning, world') =
            let optSelectedScreenAddress = get world worldOptSelectedScreenModelAddressLens
            match optSelectedScreenAddress with
            | None -> (true, world)
            | Some selectedScreenAddress ->
                let screenModelState = getScreenModelState selectedScreenAddress world
                match screenModelState with
                | IncomingState ->
                    // TODO: remove duplication with below
                    let selectedScreenModel = get world <| worldScreenModelLens selectedScreenAddress
                    let incomingModel = get selectedScreenModel incomingModelLens
                    let (incomingModel', finished) = updateTransition1 incomingModel
                    let selectedScreenModel' = set incomingModel' selectedScreenModel incomingModelLens
                    let world'' = set selectedScreenModel' world <| worldScreenModelLens selectedScreenAddress
                    let world'3 = setScreenModelState (if finished then IdlingState else IncomingState) selectedScreenAddress world''
                    if finished then
                        publish
                            (FinishedIncomingAddressPart @ selectedScreenAddress)
                            { Handled = false; Data = NoData }
                            world'3
                    else (true, world'3)
                | OutgoingState ->
                    let selectedScreenModel = get world <| worldScreenModelLens selectedScreenAddress
                    let outgoingModel = get selectedScreenModel outgoingModelLens
                    let (outgoingModel', finished) = updateTransition1 outgoingModel
                    let selectedScreenModel' = set outgoingModel' selectedScreenModel outgoingModelLens
                    let world'' = set selectedScreenModel' world <| worldScreenModelLens selectedScreenAddress
                    let world'3 = setScreenModelState (if finished then IdlingState else OutgoingState) selectedScreenAddress world''
                    if finished then
                        publish
                            (FinishedOutgoingAddressPart @ selectedScreenAddress)
                            { Handled = false; Data = NoData }
                            world'3
                    else (true, world'3)
                | IdlingState -> (true, world)
        if keepRunning then update world'
        else (keepRunning, world')

    let registerEntityXtension address world =
        match get world <| worldOptEntityModelLens address with
        | None -> world
        | Some entityModel -> invokeEntityModelXtension (fun dispatcher -> dispatcher.Register (address, entityModel, world)) (fun () -> world) world.XDispatchers entityModel

    let unregisterEntityXtension address world =
        match get world <| worldOptEntityModelLens address with
        | None -> world
        | Some entityModel -> invokeEntityModelXtension (fun dispatcher -> dispatcher.Unregister (address, entityModel, world)) (fun () -> world) world.XDispatchers entityModel

    let handleButtonEventDownMouseLeft address subscriber message world =
        match message.Data with
        | MouseButtonData (mousePosition, _) ->
            let button = get world (worldButtonLens subscriber)
            if button.Gui.Entity.Enabled && button.Gui.Entity.Visible then
                if isInBox3 mousePosition button.Gui.Position button.Gui.Size then
                    let button' = { button with IsDown = true }
                    let world' = set button' world (worldButtonLens subscriber)
                    let (keepRunning, world'') = publish (straddr "down" subscriber) { Handled = false; Data = NoData } world'
                    (handle message, keepRunning, world'')
                else (message, true, world)
            else (message, true, world)
        | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")
    
    let handleButtonEventUpMouseLeft address subscriber message world =
        match message.Data with
        | MouseButtonData (mousePosition, _) ->
            let button = get world (worldButtonLens subscriber)
            if button.Gui.Entity.Enabled && button.Gui.Entity.Visible then
                let (keepRunning, world') =
                    let button' = { button with IsDown = false }
                    let world'' = set button' world (worldButtonLens subscriber)
                    publish (straddr "up" subscriber) { Handled = false; Data = NoData } world''
                if keepRunning && isInBox3 mousePosition button.Gui.Position button.Gui.Size && button.IsDown then
                    let (keepRunning', world'') = publish (straddr "click" subscriber) { Handled = false; Data = NoData } world'
                    let sound = PlaySound { Volume = 1.0f; Sound = button.ClickSound }
                    let world'3 = { world'' with AudioMessages = sound :: world''.AudioMessages }
                    (handle message, keepRunning', world'3)
                else (message, keepRunning, world')
            else (message, true, world)
        | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")

    let registerButton address world =
        world |>
            registerEntityXtension address |>
            subscribe DownMouseLeftAddress address handleButtonEventDownMouseLeft |>
            subscribe UpMouseLeftAddress address handleButtonEventUpMouseLeft

    let unregisterButton address world =
        world |>
            unregisterEntityXtension address |>
            unsubscribe DownMouseLeftAddress address |>
            unsubscribe UpMouseLeftAddress address

    let handleToggleEventDownMouseLeft address subscriber message world =
        match message.Data with
        | MouseButtonData (mousePosition, _) ->
            let toggle = get world (worldToggleLens subscriber)
            if toggle.Gui.Entity.Enabled && toggle.Gui.Entity.Visible then
                if isInBox3 mousePosition toggle.Gui.Position toggle.Gui.Size then
                    let toggle' = { toggle with IsPressed = true }
                    let world' = set toggle' world (worldToggleLens subscriber)
                    (handle message, true, world')
                else (message, true, world)
            else (message, true, world)
        | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")
    
    let handleToggleEventUpMouseLeft address subscriber message world =
        match message.Data with
        | MouseButtonData (mousePosition, _) ->
            let toggle = get world (worldToggleLens subscriber)
            if toggle.Gui.Entity.Enabled && toggle.Gui.Entity.Visible && toggle.IsPressed then
                let toggle' = { toggle with IsPressed = false }
                if isInBox3 mousePosition toggle'.Gui.Position toggle'.Gui.Size then
                    let toggle'' = { toggle' with IsOn = not toggle'.IsOn }
                    let world' = set toggle'' world (worldToggleLens subscriber)
                    let messageType = if toggle''.IsOn then "on" else "off"
                    let (keepRunning, world'') = publish (straddr messageType subscriber) { Handled = false; Data = NoData } world'
                    let sound = PlaySound { Volume = 1.0f; Sound = toggle''.ToggleSound }
                    let world'3 = { world'' with AudioMessages = sound :: world''.AudioMessages }
                    (handle message, keepRunning, world'3)
                else
                    let world' = set toggle' world (worldToggleLens subscriber)
                    (message, true, world')
            else (message, true, world)
        | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")

    let unregisterToggle address world =
        world |>
            unregisterEntityXtension address |>
            unsubscribe DownMouseLeftAddress address |>
            unsubscribe UpMouseLeftAddress address

    let registerToggle address world =
        world |>
            registerEntityXtension address |>
            subscribe DownMouseLeftAddress address handleToggleEventDownMouseLeft |>
            subscribe UpMouseLeftAddress address handleToggleEventUpMouseLeft

    let handleFeelerEventDownMouseLeft address subscriber message world =
        match message.Data with
        | MouseButtonData (mousePosition, _) as mouseButtonData ->
            let feeler = get world (worldFeelerLens subscriber)
            if feeler.Gui.Entity.Enabled && feeler.Gui.Entity.Visible then
                if isInBox3 mousePosition feeler.Gui.Position feeler.Gui.Size then
                    let feeler' = { feeler with IsTouched = true }
                    let world' = set feeler' world (worldFeelerLens subscriber)
                    let (keepRunning, world'') = publish (straddr "touch" subscriber) { Handled = false; Data = mouseButtonData } world'
                    (handle message, keepRunning, world'')
                else (message, true, world)
            else (message, true, world)
        | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")
    
    let handleFeelerEventUpMouseLeft address subscriber message world =
        match message.Data with
        | MouseButtonData _ ->
            let feeler = get world (worldFeelerLens subscriber)
            if feeler.Gui.Entity.Enabled && feeler.Gui.Entity.Visible then
                let feeler' = { feeler with IsTouched = false }
                let world' = set feeler' world (worldFeelerLens subscriber)
                let (keepRunning, world'') = publish (straddr "release" subscriber) { Handled = false; Data = NoData } world'
                (handle message, keepRunning, world'')
            else (message, true, world)
        | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")
    
    let registerFeeler address world =
        world |>
            registerEntityXtension address |>
            subscribe DownMouseLeftAddress address handleFeelerEventDownMouseLeft |>
            subscribe UpMouseLeftAddress address handleFeelerEventUpMouseLeft

    let unregisterFeeler address world =
        world |>
            unregisterEntityXtension address |>
            unsubscribe UpMouseLeftAddress address |>
            unsubscribe DownMouseLeftAddress address

    let registerBlockPhysics address (block : Block) world =
        let bodyCreateMessage =
            BodyCreateMessage
                { EntityAddress = address
                  PhysicsId = block.PhysicsId
                  Shape =
                    BoxShape
                        { Extent = block.Actor.Size * 0.5f
                          Properties =
                            { Center = Vector2.Zero
                              Restitution = 0.0f
                              FixedRotation = false
                              LinearDamping = 5.0f
                              AngularDamping = 5.0f }}
                  Position = block.Actor.Position + block.Actor.Size * 0.5f
                  Rotation = block.Actor.Rotation
                  Density = block.Density
                  BodyType = block.BodyType }
        { world with PhysicsMessages = bodyCreateMessage :: world.PhysicsMessages }

    let unregisterBlockPhysics address (block : Block) world =
        let bodyDestroyMessage = BodyDestroyMessage { PhysicsId = block.PhysicsId }
        { world with PhysicsMessages = bodyDestroyMessage :: world.PhysicsMessages }

    let registerAvatarPhysics address avatar world =
        let bodyCreateMessage =
            BodyCreateMessage
                { EntityAddress = address
                  PhysicsId = avatar.PhysicsId
                  Shape =
                    CircleShape
                        { Radius = avatar.Actor.Size.X * 0.5f
                          Properties =
                            { Center = Vector2.Zero
                              Restitution = 0.0f
                              FixedRotation = true
                              LinearDamping = 10.0f
                              AngularDamping = 0.0f }}
                  Position = avatar.Actor.Position + avatar.Actor.Size * 0.5f
                  Rotation = avatar.Actor.Rotation
                  Density = avatar.Density
                  BodyType = BodyType.Dynamic }
        { world with PhysicsMessages = bodyCreateMessage :: world.PhysicsMessages }

    let unregisterAvatarPhysics address avatar world =
        let bodyDestroyMessage = BodyDestroyMessage { PhysicsId = avatar.PhysicsId }
        { world with PhysicsMessages = bodyDestroyMessage :: world.PhysicsMessages }

    let registerTilePhysics tileMap tmd tld address n (world, physicsIds) tile =
        let td = makeTileData tileMap tmd tld n
        match td.OptTileSetTile with
        | None -> (world, physicsIds)
        | Some tileSetTile when not <| tileSetTile.Properties.ContainsKey "c" -> (world, physicsIds)
        | Some tileSetTile ->
            let physicsId = getPhysicsId ()
            let boxShapeProperties =
                { Center = Vector2.Zero
                  Restitution = 0.0f
                  FixedRotation = true
                  LinearDamping = 0.0f
                  AngularDamping = 0.0f }
            let bodyCreateMessage =
                BodyCreateMessage
                    { EntityAddress = address
                      PhysicsId = physicsId
                      Shape = BoxShape { Extent = Vector2 (single <| fst tmd.TileSize, single <| snd tmd.TileSize) * 0.5f; Properties = boxShapeProperties }
                      Position = Vector2 (single <| fst td.TilePosition + fst tmd.TileSize / 2, single <| snd td.TilePosition + snd tmd.TileSize / 2)
                      Rotation = tileMap.Actor.Rotation
                      Density = tileMap.Density
                      BodyType = BodyType.Static }
            let world' = { world with PhysicsMessages = bodyCreateMessage :: world.PhysicsMessages }
            (world', physicsId :: physicsIds)

    let registerTileMapPhysics address tileMap world =
        let collisionLayer = 0 // MAGIC_VALUE: assumption
        let tmd = makeTileMapData tileMap
        let tld = makeTileLayerData tileMap tmd collisionLayer
        let (world', physicsIds) = Seq.foldi (registerTilePhysics tileMap tmd tld address) (world, []) tld.Tiles
        let tileMap' = { tileMap with PhysicsIds = physicsIds }
        (tileMap', world')

    let unregisterTilePhysics world physicsId =
        let bodyDestroyMessage = BodyDestroyMessage { PhysicsId = physicsId }
        { world with PhysicsMessages = bodyDestroyMessage :: world.PhysicsMessages }

    let unregisterTileMapPhysics address tileMap world =
        List.fold unregisterTilePhysics world tileMap.PhysicsIds

    let registerEntityModel address entityModel world =
        match entityModel with
        | CustomEntity _ -> (entityModel, registerEntityXtension address world)
        | CustomGui _ -> (entityModel, registerEntityXtension address world)
        | Button _ -> (entityModel, registerButton address world)
        | Label _ -> (entityModel, registerEntityXtension address world)
        | TextBox _ -> (entityModel, registerEntityXtension address world)
        | Toggle _ -> (entityModel, registerToggle address world)
        | Feeler _ -> (entityModel, registerFeeler address world)
        | CustomActor _ -> (entityModel, registerEntityXtension address world)
        | Block block -> (entityModel, world |> registerEntityXtension address |> registerBlockPhysics address block)
        | Avatar avatar -> (entityModel, world |> registerEntityXtension address |> registerAvatarPhysics address avatar)
        | TileMap tileMap ->
            let (tileMap', world') = world |> registerEntityXtension address |> registerTileMapPhysics address tileMap
            (TileMap tileMap', world')

    let unregisterEntityModel address world =
        let entityModel = get world <| worldEntityModelLens address
        match entityModel with
        | CustomEntity _ -> unregisterEntityXtension address world
        | CustomGui _ -> unregisterEntityXtension address world
        | Button _ -> unregisterButton address world
        | Label _ -> unregisterEntityXtension address world
        | TextBox _ -> unregisterEntityXtension address world
        | Toggle _ -> unregisterToggle address world
        | Feeler _ -> unregisterFeeler address world
        | CustomActor _ -> unregisterEntityXtension address world
        | Block block -> unregisterBlockPhysics address block world
        | Avatar avatar -> unregisterAvatarPhysics address avatar world
        | TileMap tileMap -> unregisterTileMapPhysics address tileMap world

    let removeEntityModel address world =
        let world' = unregisterEntityModel address world
        set None world' <| worldOptEntityModelLens address

    let removeEntityModels address world =
        let entityModels = get world <| worldEntityModelsLens address
        Map.fold
            (fun world' entityModelName _ -> removeEntityModel (address @ [entityModelName]) world')
            world
            entityModels

    let addEntityModel address entityModel world =
        let world' =
            match get world <| worldOptEntityModelLens address with
            | None -> world
            | Some _ -> removeEntityModel address world
        let (entityModel', world'') = registerEntityModel address entityModel world'
        set entityModel' world'' <| worldEntityModelLens address

    let addEntityModels address entityModels world =
        List.fold
            (fun world' entityModel ->
                let entity = get entityModel entityLens
                addEntityModel (addrstr address entity.Name) entityModel world')
            world
            entityModels

    let propagateEntityModelPhysics address entityModel world =
        match entityModel with
        | CustomEntity _
        | CustomGui _
        | Button _
        | Label _
        | TextBox _
        | Toggle _
        | Feeler _ -> world
        | CustomActor _ -> world // TODO: consider if this should invoke an Xtension
        | Block block -> world |> unregisterBlockPhysics address block |> registerBlockPhysics address block
        | Avatar avatar -> world |> unregisterAvatarPhysics address avatar |> registerAvatarPhysics address avatar
        | TileMap tileMap -> snd <| (world |> unregisterTileMapPhysics address tileMap |> registerTileMapPhysics address tileMap)

    let registerGroup address group entityModels world =
        addEntityModels address entityModels world

    let unregisterGroup address world =
        removeEntityModels address world

    let adjustFieldCamera groupAddress world =
        let avatarAddress = groupAddress @ [FieldAvatarName]
        let actor = get world <| worldActorLens avatarAddress
        let camera = { world.Camera with EyePosition = actor.Position + actor.Size * 0.5f }
        { world with Camera = camera }

    let adjustFieldCameraHandler groupAddress _ _ message world =
        (message, true, adjustFieldCamera groupAddress world)

    let moveFieldAvatarHandler groupAddress _ _ message world =
        let feelerAddress = groupAddress @ [FieldFeelerName]
        let feeler = get world (worldFeelerLens feelerAddress)
        if feeler.IsTouched then
            let avatarAddress = groupAddress @ [FieldAvatarName]
            let avatar = get world <| worldAvatarLens avatarAddress
            let camera = world.Camera
            let view = getInverseViewF camera
            let mousePositionWorld = world.MouseState.MousePosition + view
            let actorCenter = avatar.Actor.Position + avatar.Actor.Size * 0.5f
            let impulseVector = (mousePositionWorld - actorCenter) * 5.0f
            let applyImpulseMessage = { PhysicsId = avatar.PhysicsId; Impulse = impulseVector }
            let world' = { world with PhysicsMessages = ApplyImpulseMessage applyImpulseMessage :: world.PhysicsMessages }
            (message, true, world')
        else (message, true, world)

    let registerFieldGroup address (omniFieldGroup : OmniFieldGroup) entityModels world =
        let world_ = subscribe TickAddress [] (moveFieldAvatarHandler address) world
        let world_ = subscribe TickAddress [] (adjustFieldCameraHandler address) world_
        let world_ = { world_ with PhysicsMessages = SetGravityMessage Vector2.Zero :: world_.PhysicsMessages }
        let world_ = addEntityModels address entityModels world_
        adjustFieldCamera address world_

    let unregisterFieldGroup address world =
        let world_ = unsubscribe TickAddress [] world
        let world_ = unsubscribe TickAddress [] world_
        removeEntityModels address world_

    let registerBattleGroup address (omniBattleGroup : OmniBattleGroup) entityModels world =
        let world' = { world with PhysicsMessages = SetGravityMessage Vector2.Zero :: world.PhysicsMessages }
        addEntityModels address entityModels world'

    let unregisterBattleGroup address world =
        removeEntityModels address world

    let registerGroupModel address groupModel entityModels world =
        match groupModel with
        | Group group -> registerGroup address group entityModels world
        | OmniFieldGroup omniFieldGroup -> registerFieldGroup address omniFieldGroup entityModels world
        | OmniBattleGroup omniBattleGroup -> registerBattleGroup address omniBattleGroup entityModels world

    let unregisterGroupModel address world =
        let groupModel = get world <| worldGroupModelLens address
        match groupModel with
        | Group _ -> unregisterGroup address world
        | OmniFieldGroup _ -> unregisterFieldGroup address world
        | OmniBattleGroup _ -> unregisterBattleGroup address world

    let removeGroupModel address world =
        let world' = unregisterGroupModel address world
        set None world' (worldOptGroupModelLens address)

    let removeGroupModels address world =
        let groupModels = get world <| worldGroupModelsLens address
        Map.fold
            (fun world' groupModelName _ -> removeGroupModel (address @ [groupModelName]) world')
            world
            groupModels

    let addGroupModel address (groupModel, entityModels) world =
        let world' =
            match get world <| worldOptGroupModelLens address with
            | None -> world
            | Some _ -> removeGroupModel address world
        let world'' = registerGroupModel address groupModel entityModels world'
        set groupModel world'' (worldGroupModelLens address)

    let addGroupModels address groupDescriptors world =
        List.fold
            (fun world' (groupModelName, groupModel, entityModels) -> addGroupModel (address @ [groupModelName]) (groupModel, entityModels) world')
            world
            groupDescriptors

    let registerScreen address screen groupDescriptors world =
        addGroupModels address groupDescriptors world

    let unregisterScreen address world =
        removeGroupModels address world

    let registerOmniBattleScreen address omniBattleScreen groupDescriptors world =
        addGroupModels address groupDescriptors world

    let unregisterOmniBattleScreen address world =
        removeGroupModels address world

    let registerScreenModel address screenModel groupDescriptor world =
        match screenModel with
        | Screen screen -> registerScreen address screen groupDescriptor world
        | OmniBattleScreen omniBattleScreen -> registerOmniBattleScreen address omniBattleScreen groupDescriptor world

    let unregisterScreenModel address world =
        let screenModel = get world <| worldScreenModelLens address
        match screenModel with
        | Screen screen -> unregisterScreen address world
        | OmniBattleScreen omniBattleScreen -> unregisterOmniBattleScreen address world

    let removeScreenModel address world =
        let world' = unregisterScreenModel address world
        set None world' (worldOptScreenModelLens address)

    let addScreenModel address screenModel groupDescriptors world =
        let world' =
            match get world <| worldOptScreenModelLens address with
            | None -> world
            | Some _ -> removeScreenModel address world
        let world'' = registerScreenModel address screenModel groupDescriptors world'
        set screenModel world'' (worldScreenModelLens address)

    let rec handleSplashScreenIdleTick idlingTime ticks address subscriber message world =
        let world' = unsubscribe address subscriber world
        if ticks < idlingTime then
            let world'' = subscribe address subscriber (handleSplashScreenIdleTick idlingTime <| ticks + 1) world'
            (message, true, world'')
        else
            let optSelectedScreenAddress = get world' worldOptSelectedScreenModelAddressLens
            match optSelectedScreenAddress with
            | None ->
                trace "Program Error: Could not handle splash screen tick due to no selected screen model."
                (message, false, world)
            | Some selectedScreenAddress ->
                let world'' = setScreenModelState OutgoingState selectedScreenAddress world'
                (message, true, world'')

    let handleSplashScreenIdle idlingTime address subscriber message world =
        let world' = subscribe TickAddress subscriber (handleSplashScreenIdleTick idlingTime 0) world
        (handle message, true, world')

    let addSplashScreen handleFinishedOutgoing address incomingTime idlingTime outgoingTime sprite world =
        let splashScreenModel = Screen <| makeDissolveScreen incomingTime outgoingTime
        let splashGroupModel = Group <| makeDefaultGroup ()
        let splashLabel = Label { Gui = { makeDefaultGui (Some "splashLabel") with Size = world.Camera.EyeSize }; LabelSprite = sprite }
        let world' = addScreenModel address splashScreenModel [(Lun.make "splashGroup", splashGroupModel, [splashLabel])] world
        let world'' = subscribe (FinishedIncomingAddressPart @ address) address (handleSplashScreenIdle idlingTime) world'
        subscribe (FinishedOutgoingAddressPart @ address) address handleFinishedOutgoing world''

    let createDissolveScreenFromFile groupModelFileName groupModelName incomingTime outgoingTime screenAddress world =
        let screenModel = Screen <| makeDissolveScreen incomingTime outgoingTime
        let (groupModel, entityModels) = loadGroupModelFile groupModelFileName world
        addScreenModel screenAddress screenModel [(groupModelName, groupModel, entityModels)] world

    let tryCreateEmptyWorld sdlDeps extData =
        match tryGenerateAssetMetadataMap "AssetGraph.xml" with
        | Left errorMsg -> Left errorMsg
        | Right assetMetadataMap ->
            let world =
                { GameModel = Game { Id = getNuId (); OptSelectedScreenModelAddress = None }
                  ScreenModels = Map.empty
                  GroupModels = Map.empty
                  EntityModels = Map.empty
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
                  Components = []
                  XDispatchers = Map.empty
                  ExtData = extData }
            Right world

    let reregisterPhysicsHack4 groupAddress world _ entityModel =
        match entityModel with
        | CustomEntity _
        | CustomGui _
        | Button _
        | Label _
        | TextBox _
        | Toggle _
        | Feeler _ -> world
        | CustomActor _ -> world // TODO: consider if this should invoke an Xtension
        | Block block ->
            let address = addrstr groupAddress block.Actor.Entity.Name
            let world' = unregisterBlockPhysics address block world
            registerBlockPhysics address block world'
        | Avatar avatar ->
            let address = addrstr groupAddress avatar.Actor.Entity.Name
            let world' = unregisterAvatarPhysics address avatar world
            registerAvatarPhysics address avatar world'
        | TileMap tileMap -> 
            let tileMapAddress = addrstr groupAddress tileMap.Actor.Entity.Name
            let world' = unregisterTileMapPhysics tileMapAddress tileMap world
            let (tileMap', world'') = registerTileMapPhysics tileMapAddress tileMap world'
            set tileMap' world'' <| worldTileMapLens tileMapAddress

    let reregisterPhysicsHack groupAddress world =
        let entityModels = get world <| worldEntityModelsLens groupAddress
        Map.fold (reregisterPhysicsHack4 groupAddress) world entityModels

    let getComponentAudioDescriptors world : AudioDescriptor rQueue =
        let descriptorLists = List.fold (fun descs (comp : IWorldComponent) -> comp.GetAudioDescriptors world :: descs) [] world.Components // TODO: get audio descriptors
        List.collect (fun descs -> descs) descriptorLists

    let getAudioDescriptors world : AudioDescriptor rQueue =
        let componentDescriptors = getComponentAudioDescriptors world
        let worldDescriptors = [] // TODO: get audio descriptors when there are some
        componentDescriptors @ worldDescriptors // NOTE: pretty inefficient

    /// Play the world's audio.
    let play world =
        let audioMessages = world.AudioMessages
        let audioDescriptors = getAudioDescriptors world
        let audioPlayer = world.AudioPlayer
        let world' = { world with AudioMessages = [] }
        { world' with AudioPlayer = Nu.Audio.play audioMessages audioDescriptors audioPlayer }

    let getComponentRenderDescriptors world : RenderDescriptor rQueue =
        let descriptorLists = List.fold (fun descs (comp : IWorldComponent) -> comp.GetRenderDescriptors world :: descs) [] world.Components // TODO: get render descriptors
        List.collect (fun descs -> descs) descriptorLists

    let getEntityRenderDescriptors view dispatchers entityModel =
        match entityModel with
        | CustomEntity _
        | CustomGui _
        | CustomActor _ ->
            invokeEntityModelXtension
                (fun dispatcher -> dispatcher.GetRenderDescriptors entityModel)
                (fun () -> [])
                dispatchers
                entityModel
        | Button button ->
            let (_, gui, entity) = buttonSep button
            if not entity.Visible then []
            else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = gui.Position; Size = gui.Size; Rotation = 0.0f; Sprite = (if button.IsDown then button.DownSprite else button.UpSprite); Color = Vector4.One }; Depth = gui.Depth })]
        | Label label ->
            let (_, gui, entity) = labelSep label
            if not label.Gui.Entity.Visible then []
            else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = gui.Position; Size = gui.Size; Rotation = 0.0f; Sprite = label.LabelSprite; Color = Vector4.One }; Depth = gui.Depth })]
        | TextBox textBox ->
            let (_, gui, entity) = textBoxSep textBox
            if not entity.Visible then []
            else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = gui.Position; Size = gui.Size; Rotation = 0.0f; Sprite = textBox.BoxSprite; Color = Vector4.One }; Depth = gui.Depth })
                  LayerableDescriptor (LayeredTextDescriptor { Descriptor = { Text = textBox.Text; Position = gui.Position + textBox.TextOffset; Size = gui.Size - textBox.TextOffset; Font = textBox.TextFont; Color = textBox.TextColor }; Depth = gui.Depth })]
        | Toggle toggle ->
            let (_, gui, entity) = toggleSep toggle
            if not entity.Visible then []
            else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = gui.Position; Size = gui.Size; Rotation = 0.0f; Sprite = (if toggle.IsOn || toggle.IsPressed then toggle.OnSprite else toggle.OffSprite); Color = Vector4.One }; Depth = gui.Depth })]
        | Feeler _ ->
            []
        | Block block ->
            let (_, actor, entity) = blockSep block
            if not entity.Visible then []
            else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = actor.Position - view; Size = actor.Size; Rotation = actor.Rotation; Sprite = block.Sprite; Color = Vector4.One }; Depth = actor.Depth })]
        | Avatar avatar ->
            let (_, actor, entity) = avatarSep avatar
            if not entity.Visible then []
            else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = actor.Position - view; Size = actor.Size; Rotation = actor.Rotation; Sprite = avatar.Sprite; Color = Vector4.One }; Depth = actor.Depth })]
        | TileMap tileMap ->
            let (_, actor, entity) = tileMapSep tileMap
            if not entity.Visible then []
            else
                let map = tileMap.TmxMap
                let layers = List.ofSeq map.Layers
                List.mapi
                    (fun i (layer : TmxLayer) ->
                        let layeredTileLayerDescriptor =
                            LayeredTileLayerDescriptor
                                { Descriptor =
                                    { Position = tileMap.Actor.Position - view
                                      Size = tileMap.Actor.Size
                                      Rotation = actor.Rotation
                                      MapSize = Vector2 (single map.Width, single map.Height)
                                      Tiles = layer.Tiles
                                      TileSize = Vector2 (single map.TileWidth, single map.TileHeight)
                                      TileSet = map.Tilesets.[0] // MAGIC_VALUE: I have no idea how to tell which tile set each tile is from...
                                      TileSetSprite = tileMap.TileMapSprites.[0] } // MAGIC_VALUE: for same reason as above
                                  Depth = actor.Depth + single i * 2.0f } // MAGIC_VALUE: assumption
                        LayerableDescriptor layeredTileLayerDescriptor)
                    layers

    let getGroupModelRenderDescriptors camera dispatchers entityModels =
        let view = getInverseView camera
        let entitModelValues = Map.toValueSeq entityModels
        Seq.map (getEntityRenderDescriptors view dispatchers) entitModelValues

    let getTransitionModelRenderDescriptors camera dispatchers transitionModel =
        match transitionModel with
        | Transition _ -> []
        | Dissolve dissolve ->
            let transition = dissolve.Transition
            let progress = single transition.Ticks / single transition.Lifetime
            let alpha = match transition.Type with Incoming -> 1.0f - progress | Outgoing -> progress
            let color = Vector4 (Vector3.One, alpha)
            [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = Vector2.Zero; Size = camera.EyeSize; Rotation = 0.0f; Sprite = dissolve.Sprite; Color = color }; Depth = Single.MaxValue })]

    let getWorldRenderDescriptors world =
        match get world worldOptSelectedScreenModelAddressLens with
        | None -> []
        | Some activeScreenAddress ->
            let optGroupMap = Map.tryFind activeScreenAddress.[0] world.EntityModels
            match optGroupMap with
            | None -> []
            | Some groupMap ->
                let entityMaps = List.fold List.flipCons [] <| Map.toValueList groupMap
                let descriptorSeqs = List.map (getGroupModelRenderDescriptors world.Camera world.XDispatchers) entityMaps
                let descriptorSeq = Seq.concat descriptorSeqs
                let descriptors = List.concat descriptorSeq
                let activeScreen = get world (worldScreenLens activeScreenAddress)
                match activeScreen.State with
                | IncomingState -> descriptors @ getTransitionModelRenderDescriptors world.Camera world.XDispatchers activeScreen.IncomingModel
                | OutgoingState -> descriptors @ getTransitionModelRenderDescriptors world.Camera world.XDispatchers activeScreen.OutgoingModel
                | IdlingState -> descriptors

    let getRenderDescriptors world : RenderDescriptor rQueue =
        let componentDescriptors = getComponentRenderDescriptors world
        let worldDescriptors = getWorldRenderDescriptors world
        componentDescriptors @ worldDescriptors // NOTE: pretty inefficient

    /// Render the world.
    let render world =
        let renderMessages = world.RenderMessages
        let renderDescriptors = getRenderDescriptors world
        let renderer = world.Renderer
        let renderer' = Nu.Rendering.render renderMessages renderDescriptors renderer
        { world with RenderMessages = []; Renderer = renderer' }

    let handleIntegrationMessage (keepRunning, world) integrationMessage : bool * World =
        if not keepRunning then (keepRunning, world)
        else
            match integrationMessage with
            | BodyTransformMessage bodyTransformMessage ->
                let entityModelAddress = bodyTransformMessage.EntityAddress
                let entityModel = get world <| worldEntityModelLens entityModelAddress
                match entityModel with
                | CustomEntity _
                | CustomGui _
                | CustomActor _ ->
                    let world' =
                        invokeEntityModelXtension
                            (fun dispatcher -> dispatcher.HandleIntegrationMessage (integrationMessage, entityModelAddress, entityModel, world))
                            (fun () -> world)
                            world.XDispatchers
                            entityModel
                    (true, world')
                | Button _
                | Label _
                | TextBox _
                | Toggle _
                | Feeler _ ->
                    debug "Unexpected gui match in Nu.WorldModule.handleIntegrationMessage."
                    (keepRunning, world)
                | Block _
                | Avatar _ ->
                    let actor = get entityModel actorLens
                    let actor' = { actor with Position = bodyTransformMessage.Position - actor.Size * 0.5f // TODO: see if this center-offsetting can be encapsulated within the Physics module!
                                              Rotation = bodyTransformMessage.Rotation }
                    let world' = set actor' world <| worldActorLens bodyTransformMessage.EntityAddress
                    (keepRunning, world')
                | TileMap _ ->
                    // nothing to do here for tile map
                    (keepRunning, world)
            | BodyCollisionMessage bodyCollisionMessage ->
                let collisionAddress = straddr "collision" bodyCollisionMessage.EntityAddress
                let collisionData = CollisionData (bodyCollisionMessage.Normal, bodyCollisionMessage.Speed, bodyCollisionMessage.EntityAddress2)
                let collisionMessage = { Handled = false; Data = collisionData }
                publish collisionAddress collisionMessage world

    /// Handle physics integration messages.
    let handleIntegrationMessages integrationMessages world : bool * World =
        List.fold handleIntegrationMessage (true, world) integrationMessages

    /// Integrate the world.
    let integrate world =
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
                    let world' = set { world.MouseState with MousePosition = mousePosition } world mouseStateLens
                    if Set.contains MouseLeft world'.MouseState.MouseDowns then publish MouseDragAddress { Handled = false; Data = MouseMoveData mousePosition } world'
                    else publish MouseMoveAddress { Handled = false; Data = MouseButtonData (mousePosition, MouseLeft) } world'
                | SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN ->
                    let mouseButton = makeMouseButton event.button.button
                    let world' = set { world.MouseState with MouseDowns = Set.add mouseButton world.MouseState.MouseDowns } world mouseStateLens
                    let messageAddress = addr ("down/mouse" </> str mouseButton)
                    let messageData = MouseButtonData (world'.MouseState.MousePosition, mouseButton)
                    publish messageAddress { Handled = false; Data = messageData } world'
                | SDL.SDL_EventType.SDL_MOUSEBUTTONUP ->
                    let mouseState = world.MouseState
                    let mouseButton = makeMouseButton event.button.button
                    if Set.contains mouseButton mouseState.MouseDowns then
                        let world' = set { world.MouseState with MouseDowns = Set.remove mouseButton world.MouseState.MouseDowns } world mouseStateLens
                        let messageAddress = addr ("up/mouse" </> str mouseButton)
                        let messageData = MouseButtonData (world'.MouseState.MousePosition, mouseButton)
                        publish messageAddress { Handled = false; Data = messageData } world'
                    else (true, world)
                | _ -> (true, world))
            (fun world ->
                let (keepRunning, world') = integrate world
                if not keepRunning then (keepRunning, world')
                else
                    let (keepRunning', world'') = publish TickAddress { Handled = false; Data = NoData } world'
                    if not keepRunning' then (keepRunning', world'')
                    else handleUpdate world'')
            (fun world -> let world' = render world in handleRender world')
            (fun world -> play world)
            (fun world -> { world with Renderer = handleRenderExit world.Renderer })
            sdlConfig

    let run tryCreateWorld handleUpdate sdlConfig =
        run4 tryCreateWorld handleUpdate id sdlConfig