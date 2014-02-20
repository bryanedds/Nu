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
open Xtension
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
open Nu.GroupModule
open Nu.ScreenModule
open Nu.GameModule
module WorldModule =

    let TickAddress = addr "Tick"
    let MouseDragAddress = addr "Mouse/Drag"
    let MouseMoveAddress = addr "Mouse/Move"
    let MouseLeftAddress = addr "Mouse/Left"
    let MouseCenterAddress = addr "Mouse/Center"
    let MouseRightAddress = addr "Mouse/Right"
    let DownMouseLeftAddress = straddr "Down" MouseLeftAddress
    let DownMouseCenterAddress = straddr "Down" MouseCenterAddress
    let DownMousRightAddress = straddr "Down" MouseRightAddress
    let UpMouseLeftAddress = straddr "Up" MouseLeftAddress
    let UpMouseCenterAddress = straddr "Up" MouseCenterAddress
    let UpMouseRightAddress = straddr "Up" MouseRightAddress
    let GamePublishingPriority = Single.MaxValue
    let ScreenPublishingPriority = GamePublishingPriority * 0.5f
    let GroupPublishingPriority = ScreenPublishingPriority * 0.5f
    let FinishedIncomingAddressPart = addr "Finished/Incoming"
    let FinishedOutgoingAddressPart = addr "Finished/Outgoing"
    let FieldFeelerName = Lun.make "Feeler"
    let FieldAvatarName = Lun.make "Avatar"

    /// Initialize Nu's various type converters.
    /// Must be called for reflection to work in Nu.
    let initTypeConverters () =
        initXtensionConverters ()
        initMathConverters ()
        initAudioConverters ()
        initRenderConverters ()
    
    /// Derive an XDispatcherContainer from a world.
    let xdc world =
        { XDispatcherContainer.Dispatchers = world.Dispatchers }

    /// Mark a message as handled.
    let handle message =
        { Handled = true; Data = message.Data }

    let isAddressSelected address world =
        let optScreenAddress = get world worldOptSelectedScreenAddressLens
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
        | Game _ -> GamePublishingPriority
        | Screen _ -> ScreenPublishingPriority
        | Group _ -> GroupPublishingPriority
        | EntityModel entityModel -> getPickingPriority (xdc world) entityModel

    let getSimulant address world =
        match address with
        | [] -> Game <| get world gameLens
        | [_] as screenAddress -> Screen <| get world (worldScreenLens screenAddress)
        | [_; _] as groupAddress -> Group <| get world (worldGroupLens groupAddress)
        | [_; _; _] as entityAddress -> EntityModel <| get world (worldEntityModelLens entityAddress)
        | _ -> failwith <| "Invalid simulant address '" + str address + "'."

    let getSimulants subscriptions world =
        List.map (fun (address, _) -> getSimulant address world) subscriptions

    let pickingSort entityModels world =
        let priorities = List.map (getPickingPriority <| xdc world) entityModels
        let prioritiesAndEntityModels = List.zip priorities entityModels
        let prioritiesAndEntityModelsSorted = List.sortWith sortFstAsc prioritiesAndEntityModels
        List.map snd prioritiesAndEntityModelsSorted

    let tryPick (position : Vector2) entityModels world =
        let entityModelsSorted = pickingSort entityModels world
        List.tryFind
            (fun entityModel ->
                let transform = getEntityModelTransform (Some world.Camera) (xdc world) entityModel
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
    let getScreenState address world =
        let screen = get world <| worldScreenLens address
        get screen screenStateLens

    // TODO: consider turning this into a lens, and removing the screenStateLens
    let setScreenState address state world =
        let screen = set state (get world <| worldScreenLens address) screenStateLens
        let world' = set screen world <| worldScreenLens address
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
        let world' = setScreenState address IncomingState world
        set (Some address) world' worldOptSelectedScreenAddressLens

    let transitionScreenHandler address _ _ message world =
        let world' = transitionScreen address world
        (handle message, true, world')

    let handleFinishedScreenOutgoing screenAddress destScreenAddress address subscriber message world =
        let world' = unsubscribe address subscriber world
        let world'' = transitionScreen destScreenAddress world'
        (handle message, true, world'')

    let handleEventAsScreenTransition screenAddress destScreenAddress address subscriber message world =
        let world' = subscribe (FinishedOutgoingAddressPart @ screenAddress) [] (handleFinishedScreenOutgoing screenAddress destScreenAddress) world
        let optSelectedScreenAddress = get world' worldOptSelectedScreenAddressLens
        match optSelectedScreenAddress with
        | None ->
            trace <| "Program Error: Could not handle click as screen transition due to no selected screen."
            (handle message, true, world)
        | Some selectedScreenAddress ->
            let world'' = setScreenState selectedScreenAddress OutgoingState world'
            (handle message, true, world'')

    let updateTransition1 transition =
        if transition.Ticks = transition.Lifetime then ({ transition with Ticks = 0 }, true)
        else ({ transition with Ticks = transition.Ticks + 1 }, false)

    let updateTransition update world : bool * World =
        let (keepRunning, world') =
            let optSelectedScreenAddress = get world worldOptSelectedScreenAddressLens
            match optSelectedScreenAddress with
            | None -> (true, world)
            | Some selectedScreenAddress ->
                let screenState = getScreenState selectedScreenAddress world
                match screenState with
                | IncomingState ->
                    // TODO: remove duplication with below
                    let selectedScreen = get world <| worldScreenLens selectedScreenAddress
                    let incoming = get selectedScreen incomingLens
                    let (incoming', finished) = updateTransition1 incoming
                    let selectedScreen' = set incoming' selectedScreen incomingLens
                    let world'' = set selectedScreen' world <| worldScreenLens selectedScreenAddress
                    let world'3 = setScreenState selectedScreenAddress (if finished then IdlingState else IncomingState) world''
                    if finished then
                        publish
                            (FinishedIncomingAddressPart @ selectedScreenAddress)
                            { Handled = false; Data = NoData }
                            world'3
                    else (true, world'3)
                | OutgoingState ->
                    let selectedScreen = get world <| worldScreenLens selectedScreenAddress
                    let outgoing = get selectedScreen outgoingLens
                    let (outgoing', finished) = updateTransition1 outgoing
                    let selectedScreen' = set outgoing' selectedScreen outgoingLens
                    let world'' = set selectedScreen' world <| worldScreenLens selectedScreenAddress
                    let world'3 = setScreenState selectedScreenAddress (if finished then IdlingState else OutgoingState) world''
                    if finished then
                        publish
                            (FinishedOutgoingAddressPart @ selectedScreenAddress)
                            { Handled = false; Data = NoData }
                            world'3
                    else (true, world'3)
                | IdlingState -> (true, world)
        if keepRunning then update world'
        else (keepRunning, world')

    type EntityModelDispatcher () =
        class
            abstract member Register : Address * EntityModel * World -> World
            default this.Register (_, _, world) = world

            abstract member Unregister : Address * EntityModel * World -> World
            default this.Unregister (_, _, world) = world

            abstract member HandleIntegrationMessage : IntegrationMessage * Address * EntityModel * World -> World
            default this.HandleIntegrationMessage (_, _, _, world) = world

            abstract member GetRenderDescriptors : EntityModel * IXDispatcherContainer -> RenderDescriptor list
            default this.GetRenderDescriptors (_, _) = []

            abstract member GetQuickSize : EntityModel * IXDispatcherContainer -> Vector2
            default this.GetQuickSize (_, _) = Vector2 DefaultEntitySize
    
            end

    let registerEntityXtension address world =
        match get world <| worldOptEntityModelLens address with
        | None -> world
        | Some entityModel ->
            let entity = get entityModel entityLens
            entity?Register (address, entityModel, world)

    let unregisterEntityXtension address world =
        match get world <| worldOptEntityModelLens address with
        | None -> world
        | Some entityModel ->
            let entity = get entityModel entityLens
            entity?Unregister (address, entityModel, world)

    let handleButtonEventDownMouseLeft address subscriber message world =
        match message.Data with
        | MouseButtonData (mousePosition, _) ->
            let button = get world (worldButtonLens subscriber)
            if button.Entity.Enabled && button.Entity.Visible then
                if isInBox3 mousePosition button.Entity.Position button.Entity.Size then
                    let button' = { button with IsDown = true }
                    let world' = set button' world (worldButtonLens subscriber)
                    let (keepRunning, world'') = publish (straddr "Down" subscriber) { Handled = false; Data = NoData } world'
                    (handle message, keepRunning, world'')
                else (message, true, world)
            else (message, true, world)
        | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")
    
    let handleButtonEventUpMouseLeft address subscriber message world =
        match message.Data with
        | MouseButtonData (mousePosition, _) ->
            let button = get world (worldButtonLens subscriber)
            if button.Entity.Enabled && button.Entity.Visible then
                let (keepRunning, world') =
                    let button' = { button with IsDown = false }
                    let world'' = set button' world (worldButtonLens subscriber)
                    publish (straddr "Up" subscriber) { Handled = false; Data = NoData } world''
                if keepRunning && isInBox3 mousePosition button.Entity.Position button.Entity.Size && button.IsDown then
                    let (keepRunning', world'') = publish (straddr "Click" subscriber) { Handled = false; Data = NoData } world'
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
            if toggle.Entity.Enabled && toggle.Entity.Visible then
                if isInBox3 mousePosition toggle.Entity.Position toggle.Entity.Size then
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
            if toggle.Entity.Enabled && toggle.Entity.Visible && toggle.IsPressed then
                let toggle' = { toggle with IsPressed = false }
                if isInBox3 mousePosition toggle'.Entity.Position toggle'.Entity.Size then
                    let toggle'' = { toggle' with IsOn = not toggle'.IsOn }
                    let world' = set toggle'' world (worldToggleLens subscriber)
                    let messageType = if toggle''.IsOn then "On" else "Off"
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
            if feeler.Entity.Enabled && feeler.Entity.Visible then
                if isInBox3 mousePosition feeler.Entity.Position feeler.Entity.Size then
                    let feeler' = { feeler with IsTouched = true }
                    let world' = set feeler' world (worldFeelerLens subscriber)
                    let (keepRunning, world'') = publish (straddr "Touch" subscriber) { Handled = false; Data = mouseButtonData } world'
                    (handle message, keepRunning, world'')
                else (message, true, world)
            else (message, true, world)
        | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")
    
    let handleFeelerEventUpMouseLeft address subscriber message world =
        match message.Data with
        | MouseButtonData _ ->
            let feeler = get world (worldFeelerLens subscriber)
            if feeler.Entity.Enabled && feeler.Entity.Visible then
                let feeler' = { feeler with IsTouched = false }
                let world' = set feeler' world (worldFeelerLens subscriber)
                let (keepRunning, world'') = publish (straddr "Release" subscriber) { Handled = false; Data = NoData } world'
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
        let block' = { block with PhysicsId = getPhysicsId block.Entity.Id }
        let bodyCreateMessage =
            BodyCreateMessage
                { EntityAddress = address
                  PhysicsId = block'.PhysicsId
                  Shape =
                    BoxShape
                        { Extent = block'.Entity.Size * 0.5f
                          Properties =
                            { Center = Vector2.Zero
                              Restitution = 0.0f
                              FixedRotation = false
                              LinearDamping = 5.0f
                              AngularDamping = 5.0f }}
                  Position = block'.Entity.Position + block'.Entity.Size * 0.5f
                  Rotation = block'.Entity.Rotation
                  Density = block'.Density
                  BodyType = block'.BodyType }
        let world' = { world with PhysicsMessages = bodyCreateMessage :: world.PhysicsMessages }
        (block', world')

    let unregisterBlockPhysics address (block : Block) world =
        let bodyDestroyMessage = BodyDestroyMessage { PhysicsId = block.PhysicsId }
        { world with PhysicsMessages = bodyDestroyMessage :: world.PhysicsMessages }

    let registerAvatarPhysics address (avatar : Avatar) world =
        let avatar' = { avatar with PhysicsId = getPhysicsId avatar.Entity.Id }
        let bodyCreateMessage =
            BodyCreateMessage
                { EntityAddress = address
                  PhysicsId = avatar'.PhysicsId
                  Shape =
                    CircleShape
                        { Radius = avatar'.Entity.Size.X * 0.5f
                          Properties =
                            { Center = Vector2.Zero
                              Restitution = 0.0f
                              FixedRotation = true
                              LinearDamping = 10.0f
                              AngularDamping = 0.0f }}
                  Position = avatar'.Entity.Position + avatar'.Entity.Size * 0.5f
                  Rotation = avatar'.Entity.Rotation
                  Density = avatar'.Density
                  BodyType = BodyType.Dynamic }
        let world' = { world with PhysicsMessages = bodyCreateMessage :: world.PhysicsMessages }
        (avatar', world')

    let unregisterAvatarPhysics address avatar world =
        let bodyDestroyMessage = BodyDestroyMessage { PhysicsId = avatar.PhysicsId }
        { world with PhysicsMessages = bodyDestroyMessage :: world.PhysicsMessages }

    let registerTilePhysics tileMap tmd tld address n (world, physicsIds) tile =
        let td = makeTileData tileMap tmd tld n
        match td.OptTileSetTile with
        | None -> (world, physicsIds)
        | Some tileSetTile when not <| tileSetTile.Properties.ContainsKey "c" -> (world, physicsIds)
        | Some tileSetTile ->
            let physicsId = getPhysicsId tileMap.Entity.Id
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
                      Rotation = tileMap.Entity.Rotation
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
        | Button _ -> (entityModel, registerButton address world)
        | Label _ -> (entityModel, registerEntityXtension address world)
        | TextBox _ -> (entityModel, registerEntityXtension address world)
        | Toggle _ -> (entityModel, registerToggle address world)
        | Feeler _ -> (entityModel, registerFeeler address world)
        | Block block ->
            let (block', world') = world |> registerEntityXtension address |> registerBlockPhysics address block
            (Block block', world')
        | Avatar avatar ->
            let (avatar', world') = world |> registerEntityXtension address |> registerAvatarPhysics address avatar
            (Avatar avatar', world')
        | TileMap tileMap ->
            let (tileMap', world') = world |> registerEntityXtension address |> registerTileMapPhysics address tileMap
            (TileMap tileMap', world')

    let unregisterEntityModel address world =
        let entityModel = get world <| worldEntityModelLens address
        match entityModel with
        | Button _ -> unregisterButton address world
        | Label _ -> unregisterEntityXtension address world
        | TextBox _ -> unregisterEntityXtension address world
        | Toggle _ -> unregisterToggle address world
        | Feeler _ -> unregisterFeeler address world
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
        | Button _
        | Label _
        | TextBox _
        | Toggle _
        | Feeler _ -> world
        | Block block ->
            let (block', world') = world |> unregisterBlockPhysics address block |> registerBlockPhysics address block
            set (Block block') world' <| worldEntityModelLens address
        | Avatar avatar ->
            let (avatar', world') = world |> unregisterAvatarPhysics address avatar |> registerAvatarPhysics address avatar
            set (Avatar avatar') world' <| worldEntityModelLens address
        | TileMap tileMap -> 
            let (tileMap', world') = world |> unregisterTileMapPhysics address tileMap |> registerTileMapPhysics address tileMap
            set (TileMap tileMap') world' <| worldEntityModelLens address

    let adjustFieldCamera groupAddress world =
        let avatarAddress = groupAddress @ [FieldAvatarName]
        let entity = get world <| worldEntityLens avatarAddress
        let camera = { world.Camera with EyePosition = entity.Position + entity.Size * 0.5f }
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
            let avatarCenter = avatar.Entity.Position + avatar.Entity.Size * 0.5f
            let impulseVector = (mousePositionWorld - avatarCenter) * 5.0f
            let applyImpulseMessage = { PhysicsId = avatar.PhysicsId; Impulse = impulseVector }
            let world' = { world with PhysicsMessages = ApplyImpulseMessage applyImpulseMessage :: world.PhysicsMessages }
            (message, true, world')
        else (message, true, world)

    type GroupDispatcher () =
        class
            abstract member Register : Address * Group * EntityModel list * World -> World
            default this.Register (address, _, entityModels, world) =
                addEntityModels address entityModels world

            abstract member Unregister : Address * Group * World -> World
            default this.Unregister (address, _, world) =
                removeEntityModels address world

            end

    type OmniFieldGroupDispatcher () =
        inherit GroupDispatcher () with
            override this.Register (address, omniBattleGroup, entityModels, world) =
                let world_ = subscribe TickAddress [] (moveFieldAvatarHandler address) world
                let world_ = subscribe TickAddress [] (adjustFieldCameraHandler address) world_
                let world_ = { world_ with PhysicsMessages = SetGravityMessage Vector2.Zero :: world_.PhysicsMessages }
                let world_ = base.Register (address, omniBattleGroup, entityModels, world_)
                adjustFieldCamera address world_

            override this.Unregister (address, omniFieldGroup, world) =
                let world_ = unsubscribe TickAddress [] world
                let world_ = unsubscribe TickAddress [] world_
                base.Unregister (address, omniFieldGroup, world_)

            end

    type OmniBattleGroupDispatcher () =
        inherit GroupDispatcher () with
            override this.Register (address, omniBattleGroup, entityModels, world) =
                let world' = { world with PhysicsMessages = SetGravityMessage Vector2.Zero :: world.PhysicsMessages }
                base.Register (address, omniBattleGroup, entityModels, world')

            override this.Unregister (address, omniBattleGroup, world) =
                base.Unregister (address, omniBattleGroup, world)

            end

    let registerGroup address (group : Group) entityModels world =
        group?Register (address, group, entityModels, world)

    let unregisterGroup address world =
        let group = get world <| worldGroupLens address
        group?Unregister (address, group, world)

    let removeGroup address world =
        let world' = unregisterGroup address world
        set None world' (worldOptGroupLens address)

    let removeGroups address world =
        let groups = get world <| worldGroupsLens address
        Map.fold
            (fun world' groupName _ -> removeGroup (address @ [groupName]) world')
            world
            groups

    let addGroup address (group : Group, entityModels) world =
        let world' =
            match get world <| worldOptGroupLens address with
            | None -> world
            | Some _ -> removeGroup address world
        let world'' = registerGroup address group entityModels world'
        set group world'' <| worldGroupLens address

    let addGroups address groupDescriptors world =
        List.fold
            (fun world' (groupName, group, entityModels) -> addGroup (address @ [groupName]) (group, entityModels) world')
            world
            groupDescriptors

    type TransitionDispatcher () =
        class
            end

    type ScreenDispatcher () =
        class
            abstract member Register : Address * Screen * ((Lun * Group * EntityModel list) list) * World -> World
            default this.Register (address, _, groupDescriptors, world) =
                addGroups address groupDescriptors world

            abstract member Unregister : Address * Screen * World -> World
            default this.Unregister (address, _, world) =
                removeGroups address world

            end

    let registerScreen address screen groupDescriptors world =
        screen?Register (address, screen, groupDescriptors, world)

    let unregisterScreen address world =
        let screen = get world <| worldScreenLens address
        screen?Unregister (address, screen, world)

    let removeScreen address world =
        let world' = unregisterScreen address world
        set None world' (worldOptScreenLens address)

    let addScreen address screen groupDescriptors world =
        let world' =
            match get world <| worldOptScreenLens address with
            | None -> world
            | Some _ -> removeScreen address world
        let world'' = registerScreen address screen groupDescriptors world'
        set screen world'' (worldScreenLens address)

    let rec handleSplashScreenIdleTick idlingTime ticks address subscriber message world =
        let world' = unsubscribe address subscriber world
        if ticks < idlingTime then
            let world'' = subscribe address subscriber (handleSplashScreenIdleTick idlingTime <| ticks + 1) world'
            (message, true, world'')
        else
            let optSelectedScreenAddress = get world' worldOptSelectedScreenAddressLens
            match optSelectedScreenAddress with
            | None ->
                trace "Program Error: Could not handle splash screen tick due to no selected screen."
                (message, false, world)
            | Some selectedScreenAddress ->
                let world'' = setScreenState selectedScreenAddress OutgoingState world'
                (message, true, world'')

    let handleSplashScreenIdle idlingTime address subscriber message world =
        let world' = subscribe TickAddress subscriber (handleSplashScreenIdleTick idlingTime 0) world
        (handle message, true, world')

    let addSplashScreen handleFinishedOutgoing address incomingTime idlingTime outgoingTime sprite world =
        let splashScreen = makeDissolveScreen incomingTime outgoingTime
        let splashGroup = makeDefaultGroup ()
        let splashLabel = Label { Entity = { makeDefaultEntity (Some "SplashLabel") with Size = world.Camera.EyeSize }; LabelSprite = sprite }
        let world' = addScreen address splashScreen [(Lun.make "SplashGroup", splashGroup, [splashLabel])] world
        let world'' = subscribe (FinishedIncomingAddressPart @ address) address (handleSplashScreenIdle idlingTime) world'
        subscribe (FinishedOutgoingAddressPart @ address) address handleFinishedOutgoing world''

    let createDissolveScreenFromFile groupFileName groupName incomingTime outgoingTime screenAddress world =
        let screen = makeDissolveScreen incomingTime outgoingTime
        let (group, entityModels) = loadGroupFile groupFileName world
        addScreen screenAddress screen [(groupName, group, entityModels)] world

    type GameDispatcher () =
        class
            abstract member Register : Game * World -> World
            default this.Register (_, world) = world
            end

    type OmniGameDispatcher () =
        inherit GameDispatcher () with
            override this.Register (omniGame, world) =
                let dispatchers =
                    Map.addMany
                        [|Lun.make "OmniBattleGroupDispatcher", OmniBattleGroupDispatcher () :> obj
                          Lun.make "OmniFieldGroupDispatcher", OmniFieldGroupDispatcher () :> obj|]
                        world.Dispatchers
                { world with Dispatchers = dispatchers }
            end

    let tryCreateEmptyWorld sdlDeps extData =
        match tryGenerateAssetMetadataMap "AssetGraph.xml" with
        | Left errorMsg -> Left errorMsg
        | Right assetMetadataMap ->
            let defaultDispatchers =
                Map.ofArray
                    [|Lun.make "EntityModelDispatcher", EntityModelDispatcher () :> obj
                      Lun.make "GroupDispatcher", GroupDispatcher () :> obj
                      Lun.make "TransitionDispatcher", TransitionDispatcher () :> obj
                      Lun.make "ScreenDispatcher", ScreenDispatcher () :> obj
                      Lun.make "GameDispatcher", GameDispatcher () :> obj
                      // TODO: remove these when editor has a way to specify the GameDispatcher
                      Lun.make "OmniBattleGroupDispatcher", OmniBattleGroupDispatcher () :> obj
                      Lun.make "OmniFieldGroupDispatcher", OmniFieldGroupDispatcher () :> obj
                      Lun.make "OmniGameDispatcher", OmniGameDispatcher () :> obj|]
            let world =
                { Game = { Id = getNuId (); OptSelectedScreenAddress = None; Xtension = { OptXTypeName = Some <| Lun.make "GameDispatcher"; XFields = Map.empty }}
                  Screens = Map.empty
                  Groups = Map.empty
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
                  XTypes = Map.empty
                  Dispatchers = defaultDispatchers
                  ExtData = extData }
            Right world

    let reregisterPhysicsHack4 groupAddress world _ entityModel =
        match entityModel with
        | Button _
        | Label _
        | TextBox _
        | Toggle _
        | Feeler _ -> world
        | Block block ->
            let address = addrstr groupAddress block.Entity.Name
            let world' = unregisterBlockPhysics address block world
            let (block', world'') = registerBlockPhysics address block world'
            set block' world'' <| worldBlockLens address
        | Avatar avatar ->
            let address = addrstr groupAddress avatar.Entity.Name
            let world' = unregisterAvatarPhysics address avatar world
            let (avatar', world'') = registerAvatarPhysics address avatar world'
            set avatar' world'' <| worldAvatarLens address
        | TileMap tileMap -> 
            let address = addrstr groupAddress tileMap.Entity.Name
            let world' = unregisterTileMapPhysics address tileMap world
            let (tileMap', world'') = registerTileMapPhysics address tileMap world'
            set tileMap' world'' <| worldTileMapLens address

    let reregisterPhysicsHack groupAddress world =
        let entityModels = get world <| worldEntityModelsLens groupAddress
        Map.fold (reregisterPhysicsHack4 groupAddress) world entityModels

    /// Play the world's audio.
    let play world =
        let audioMessages = world.AudioMessages
        let world' = { world with AudioMessages = [] }
        { world' with AudioPlayer = Nu.Audio.play audioMessages world.AudioPlayer }

    let getEntityRenderDescriptors view dispatcherContainer entityModel =
        match entityModel with
        | Button button ->
            let (_, entity) = buttonSep button
            if not entity.Visible then []
            else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = entity.Position; Size = entity.Size; Rotation = 0.0f; Sprite = (if button.IsDown then button.DownSprite else button.UpSprite); Color = Vector4.One }; Depth = entity.Depth })]
        | Label label ->
            let (_, entity) = labelSep label
            if not label.Entity.Visible then []
            else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = entity.Position; Size = entity.Size; Rotation = 0.0f; Sprite = label.LabelSprite; Color = Vector4.One }; Depth = entity.Depth })]
        | TextBox textBox ->
            let (_, entity) = textBoxSep textBox
            if not entity.Visible then []
            else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = entity.Position; Size = entity.Size; Rotation = 0.0f; Sprite = textBox.BoxSprite; Color = Vector4.One }; Depth = entity.Depth })
                  LayerableDescriptor (LayeredTextDescriptor { Descriptor = { Text = textBox.Text; Position = entity.Position + textBox.TextOffset; Size = entity.Size - textBox.TextOffset; Font = textBox.TextFont; Color = textBox.TextColor }; Depth = entity.Depth })]
        | Toggle toggle ->
            let (_, entity) = toggleSep toggle
            if not entity.Visible then []
            else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = entity.Position; Size = entity.Size; Rotation = 0.0f; Sprite = (if toggle.IsOn || toggle.IsPressed then toggle.OnSprite else toggle.OffSprite); Color = Vector4.One }; Depth = entity.Depth })]
        | Feeler _ -> []
        | Block block ->
            let (_, entity) = blockSep block
            if not entity.Visible then []
            else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = entity.Position - view; Size = entity.Size; Rotation = entity.Rotation; Sprite = block.Sprite; Color = Vector4.One }; Depth = entity.Depth })]
        | Avatar avatar ->
            let (_, entity) = avatarSep avatar
            if not entity.Visible then []
            else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = entity.Position - view; Size = entity.Size; Rotation = entity.Rotation; Sprite = avatar.Sprite; Color = Vector4.One }; Depth = entity.Depth })]
        | TileMap tileMap ->
            let (_, entity) = tileMapSep tileMap
            if not entity.Visible then []
            else
                let map = tileMap.TmxMap
                let layers = List.ofSeq map.Layers
                List.mapi
                    (fun i (layer : TmxLayer) ->
                        let layeredTileLayerDescriptor =
                            LayeredTileLayerDescriptor
                                { Descriptor =
                                    { Position = entity.Position - view
                                      Size = entity.Size
                                      Rotation = entity.Rotation
                                      MapSize = Vector2 (single map.Width, single map.Height)
                                      Tiles = layer.Tiles
                                      TileSize = Vector2 (single map.TileWidth, single map.TileHeight)
                                      TileSet = map.Tilesets.[0] // MAGIC_VALUE: I have no idea how to tell which tile set each tile is from...
                                      TileSetSprite = tileMap.TileMapSprites.[0] } // MAGIC_VALUE: for same reason as above
                                  Depth = entity.Depth + single i * 2.0f } // MAGIC_VALUE: assumption
                        LayerableDescriptor layeredTileLayerDescriptor)
                    layers

    let getGroupRenderDescriptors camera dispatcherContainer entityModels =
        let view = getInverseView camera
        let entitModelValues = Map.toValueSeq entityModels
        Seq.map (getEntityRenderDescriptors view dispatcherContainer) entitModelValues

    let getTransitionModelRenderDescriptors camera dispatcherContainer transition =
        match transition.OptDissolveSprite with
        | None -> []
        | Some dissolveSprite ->
            let progress = single transition.Ticks / single transition.Lifetime
            let alpha = match transition.Type with Incoming -> 1.0f - progress | Outgoing -> progress
            let color = Vector4 (Vector3.One, alpha)
            [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = Vector2.Zero; Size = camera.EyeSize; Rotation = 0.0f; Sprite = dissolveSprite; Color = color }; Depth = Single.MaxValue })]

    let getRenderDescriptors world =
        match get world worldOptSelectedScreenAddressLens with
        | None -> []
        | Some activeScreenAddress ->
            let optGroupMap = Map.tryFind activeScreenAddress.[0] world.EntityModels
            match optGroupMap with
            | None -> []
            | Some groupMap ->
                let entityMaps = List.fold List.flipCons [] <| Map.toValueList groupMap
                let descriptorSeqs = List.map (getGroupRenderDescriptors world.Camera <| xdc world) entityMaps
                let descriptorSeq = Seq.concat descriptorSeqs
                let descriptors = List.concat descriptorSeq
                let activeScreen = get world (worldScreenLens activeScreenAddress)
                match activeScreen.State with
                | IncomingState -> descriptors @ getTransitionModelRenderDescriptors world.Camera (xdc world) activeScreen.Incoming
                | OutgoingState -> descriptors @ getTransitionModelRenderDescriptors world.Camera (xdc world) activeScreen.Outgoing
                | IdlingState -> descriptors

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
                | Button _
                | Label _
                | TextBox _
                | Toggle _
                | Feeler _ ->
                    debug "Unexpected gui match in Nu.WorldModule.handleIntegrationMessage."
                    (keepRunning, world)
                | Block _
                | Avatar _ ->
                    let entity = get entityModel entityLens
                    let entity' = { entity with Position = bodyTransformMessage.Position - entity.Size * 0.5f // TODO: see if this center-offsetting can be encapsulated within the Physics module!
                                                Rotation = bodyTransformMessage.Rotation }
                    let world' = set entity' world <| worldEntityLens bodyTransformMessage.EntityAddress
                    (keepRunning, world')
                | TileMap _ ->
                    // nothing to do here for tile map
                    (keepRunning, world)
            | BodyCollisionMessage bodyCollisionMessage ->
                let collisionAddress = straddr "Collision" bodyCollisionMessage.EntityAddress
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
            (fun sdlDeps ->
                match tryCreateWorld sdlDeps with
                | Left _ as left -> left
                | Right world -> Right <| world.Game?Register (world.Game, world))
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
                    let messageAddress = addr ("Down/Mouse" </> str mouseButton)
                    let messageData = MouseButtonData (world'.MouseState.MousePosition, mouseButton)
                    publish messageAddress { Handled = false; Data = messageData } world'
                | SDL.SDL_EventType.SDL_MOUSEBUTTONUP ->
                    let mouseState = world.MouseState
                    let mouseButton = makeMouseButton event.button.button
                    if Set.contains mouseButton mouseState.MouseDowns then
                        let world' = set { world.MouseState with MouseDowns = Set.remove mouseButton world.MouseState.MouseDowns } world mouseStateLens
                        let messageAddress = addr ("Up/Mouse" </> str mouseButton)
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