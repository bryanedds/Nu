namespace Nu
open System
open FSharpx
open FSharpx.Lens.Operators
open SDL2
open OpenTK
open TiledSharp
open Nu.Core
open Nu.Constants
open Nu.Math
open Nu.Physics
open Nu.Rendering
open Nu.Metadata
open Nu.Audio
open Nu.Sdl
open Nu.Entities
open Nu.Groups
open Nu.Screens
open Nu.Games
open Nu.CameraModule

// WISDOM: On avoiding threads where possible...
//
// Beyond the cases where persistent threads are absolutely required or where transient threads
// implement embarassingly parallel processes, threads should be AVOIDED as a rule.
//
// If it were the case that physics were processed on a separate hardware component and thereby
// ought to be run on a separate persistent thread, then the proper way to approach the problem of
// physics system queries is to copy the relevant portion of the physics state from the PPU to main
// memory every frame. This way, queries against the physics state can be done IMMEDIATELY with no
// need for complex intermediate states (albeit against a physics state that is one frame old).

/// Describes data relevant to specific event messages.
type [<ReferenceEquality>] MessageData =
    | MouseMoveData of Vector2
    | MouseButtonData of Vector2 * MouseButton
    | CollisionData of Vector2 * single * Address
    | OtherData of obj
    | NoData

/// A generic message for the Nu game engine.
/// A reference type.
type [<ReferenceEquality>] Message =
    { Handled : bool
      Data : MessageData }

type [<StructuralEquality; NoComparison>] Simulant =
    | EntityModel of EntityModel
    | GroupModel of GroupModel
    | ScreenModel of ScreenModel
    | GameModel of GameModel

/// Describes a game message subscription.
/// A reference type.
type [<ReferenceEquality>] Subscription =
    Subscription of (Address -> Address -> Message -> World -> (Message * bool * World))

/// A map of game message subscriptions.
/// A reference type due to the reference-typeness of Subscription.
and Subscriptions = Map<Address, (Address * Subscription) list>

/// The world, in a functional programming sense.
/// A reference type with some value semantics.
and [<ReferenceEquality>] World =
    { GameModel : GameModel
      Camera : Camera
      Subscriptions : Subscriptions
      MouseState : MouseState
      AudioPlayer : AudioPlayer
      Renderer : Renderer
      Integrator : Integrator
      AssetMetadataMap : AssetMetadataMap
      AudioMessages : AudioMessage rQueue
      RenderMessages : RenderMessage rQueue
      PhysicsMessages : PhysicsMessage rQueue
      Components : IWorldComponent list
      ExtData : obj }

/// Enables components that open the world for extension.
and IWorldComponent =
    interface
        abstract member GetAudioDescriptors : World -> AudioDescriptor list
        abstract member GetRenderDescriptors : World -> RenderDescriptor list
        // TODO: abstract member GetRenderMessages : World -> RenderMessage rQueue
        // TODO: abstract member GetPhysicsMessages : World -> PhysicsMessage rQueue
        // TODO: abstract member HandleIntegrationMessages : IntegrationMessage rQueue -> World -> World
        end
    
module Sim =

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

    let gameModelLens =
        { Get = fun this -> this.GameModel
          Set = fun gameModel this -> { this with GameModel = gameModel }}
      
    let cameraLens =
        { Get = fun this -> this.Camera
          Set = fun camera this -> { this with Camera = camera }}

    let mouseStateLens =
        { Get = fun this -> this.MouseState
          Set = fun mouseState this -> { this with MouseState = mouseState }}

    let worldGameLens =
        { Get = fun this -> get this.GameModel gameLens
          Set = fun game this -> { this with GameModel = set game this.GameModel gameLens }}

    let worldOptSelectedScreenModelAddressLens = gameModelLens >>| optSelectedScreenModelAddressLens
    let worldOptSelectedScreenModelLens = gameModelLens >>| optSelectedScreenModelLens
    
    let worldScreenModelLens (address : Address) = gameModelLens >>| screenModelLens address
    let worldOptScreenModelLens (address : Address) = gameModelLens >>| optScreenModelLens address
    
    let worldScreenLens (address : Address) = gameModelLens >>| gameModelScreenLens address
    let worldOptScreenLens (address : Address) = gameModelLens >>| gameModelOptScreenLens address
    
    let worldGroupModelLens (address : Address) = gameModelLens >>| gameModelGroupModelLens address
    let worldOptGroupModelLens (address : Address) = gameModelLens >>| gameModelOptGroupModelLens address
    
    let worldGroupLens (address : Address) = gameModelLens >>| gameModelGroupLens address
    let worldOptGroupLens (address : Address) = gameModelLens >>| gameModelOptGroupLens address
    
    let worldEntityModelLens (address : Address) = gameModelLens >>| gameModelEntityModelLens address
    let worldOptEntityModelLens (address : Address) = gameModelLens >>| gameModelOptEntityModelLens address
    
    let worldEntityLens (address : Address) = gameModelLens >>| gameModelEntityLens address
    let worldOptEntityLens (address : Address) = gameModelLens >>| gameModelOptEntityLens address
    
    let worldGuiLens (address : Address) = gameModelLens >>| gameModelGuiLens address
    let worldOptGuiLens (address : Address) = gameModelLens >>| gameModelOptGuiLens address
    
    let worldButtonLens (address : Address) = gameModelLens >>| gameModelButtonLens address
    let worldOptButtonLens (address : Address) = gameModelLens >>| gameModelOptButtonLens address
    
    let worldLabelLens (address : Address) = gameModelLens >>| gameModelLabelLens address
    let worldOptLabelLens (address : Address) = gameModelLens >>| gameModelOptLabelLens address
    
    let worldTextBoxLens (address : Address) = gameModelLens >>| gameModelTextBoxLens address
    let worldOptTextBoxLens (address : Address) = gameModelLens >>| gameModelOptTextBoxLens address
    
    let worldToggleLens (address : Address) = gameModelLens >>| gameModelToggleLens address
    let worldOptToggleLens (address : Address) = gameModelLens >>| gameModelOptToggleLens address
    
    let worldFeelerLens (address : Address) = gameModelLens >>| gameModelFeelerLens address
    let worldOptFeelerLens (address : Address) = gameModelLens >>| gameModelOptFeelerLens address
    
    let worldActorLens (address : Address) = gameModelLens >>| gameModelActorLens address
    let worldOptActorLens (address : Address) = gameModelLens >>| gameModelOptActorLens address
    
    let worldBlockLens (address : Address) = gameModelLens >>| gameModelBlockLens address
    let worldOptBlockLens (address : Address) = gameModelLens >>| gameModelOptBlockLens address
    
    let worldAvatarLens (address : Address) = gameModelLens >>| gameModelAvatarLens address
    let worldOptAvatarLens (address : Address) = gameModelLens >>| gameModelOptAvatarLens address
    
    let worldTileMapLens (address : Address) = gameModelLens >>| gameModelTileMapLens address
    let worldOptTileMapLens (address : Address) = gameModelLens >>| gameModelOptTileMapLens address

    let tryCreateEmptyWorld sdlDeps extData =
        match tryGenerateAssetMetadataMap "AssetGraph.xml" with
        | Left errorMsg -> Left errorMsg
        | Right assetMetadataMap ->
            let world =
                { GameModel = Game { Id = getNuId (); ScreenModels = Map.empty; OptSelectedScreenModelAddress = None }
                  Camera = { EyePosition = Vector2.Zero; EyeSize = Vector2 (single sdlDeps.Config.ViewW, single sdlDeps.Config.ViewH) }
                  Subscriptions = Map.empty
                  MouseState = { MousePosition = Vector2.Zero; MouseDowns = Set.empty }
                  AudioPlayer = makeAudioPlayer ()
                  Renderer = makeRenderer sdlDeps.RenderContext
                  Integrator = makeIntegrator Gravity
                  AssetMetadataMap = assetMetadataMap
                  AudioMessages = []
                  RenderMessages = []
                  PhysicsMessages = []
                  Components = []
                  ExtData = extData }
            Right world

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
        | (addressHead :: addressTail, Some [screenAddressHead]) ->
            if addressHead <> screenAddressHead then false
            else
                match addressTail with
                | [] -> true
                | addressHead' :: addressTail' ->
                    let screenModel = get world <| worldScreenModelLens [addressHead]
                    let screen = get screenModel screenLens
                    match addressTail' with
                    | [] -> screen.GroupModels.ContainsKey addressHead'
                    | addressHead'' :: addressTail'' ->
                        let groupModel = Map.find addressHead' screen.GroupModels
                        let group = get groupModel groupLens
                        match addressTail'' with
                        | [] -> group.EntityModels.ContainsKey addressHead''
                        | _ -> false
        | (_, Some (_ :: _)) -> false

    let sortFstAsc (priority, _) (priority2, _) =
        if priority = priority2 then 0
        elif priority > priority2 then -1
        else 1

    let getPublishingPriority simulant =
        match simulant with
        | GameModel _ -> GameModelPublishingPriority
        | ScreenModel _ -> ScreenModelPublishingPriority
        | GroupModel _ -> GroupModelPublishingPriority
        | EntityModel entityModel -> getPickingPriority entityModel

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
        let priorities = List.map getPickingPriority entityModels
        let prioritiesAndEntityModels = List.zip priorities entityModels
        let prioritiesAndEntityModelsSorted = List.sortWith sortFstAsc prioritiesAndEntityModels
        List.map snd prioritiesAndEntityModelsSorted

    let tryPick (position : Vector2) entityModels world =
        let entityModelsSorted = pickingSort entityModels world
        List.tryFind
            (fun entityModel ->
                let transform = getEntityModelTransform (Some world.Camera) entityModel
                position.X >= transform.Position.X &&
                    position.X < transform.Position.X + transform.Size.X &&
                    position.Y >= transform.Position.Y &&
                    position.Y < transform.Position.Y + transform.Size.Y)
            entityModelsSorted

    let subscriptionSort subscriptions world =
        let simulants = getSimulants subscriptions world
        let priorities = List.map getPublishingPriority simulants
        let prioritiesAndSubscriptions = List.zip priorities subscriptions
        let prioritiesAndSubscriptionsSorted = List.sortWith sortFstAsc prioritiesAndSubscriptions
        List.map snd prioritiesAndSubscriptionsSorted

    /// Publish a message to the given address.
    let publish address message world_ : bool * World =
        let optSubList = Map.tryFind address world_.Subscriptions
        match optSubList with
        | None -> (true, world_)
        | Some subList ->
            let subListSorted = subscriptionSort subList world_
            let (_, keepRunning, world_) =
                List.foldWhile
                    (fun (message', keepRunning', world_) (subscriber, (Subscription subscription)) ->
                        if message'.Handled || not keepRunning' then None
                        elif isAddressSelected subscriber world_ then Some (subscription address subscriber message' world_)
                        else Some (message', keepRunning', world_))
                    (message, true, world_)
                    subListSorted
            (keepRunning, world_)

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

    let updateTransition update world_ : bool * World =
        let (keepRunning, world_) =
            let optSelectedScreenAddress = get world_ worldOptSelectedScreenModelAddressLens
            match optSelectedScreenAddress with
            | None -> (true, world_)
            | Some selectedScreenAddress ->
                let screenModelState = getScreenModelState selectedScreenAddress world_
                match screenModelState with
                | IncomingState ->
                    // TODO: remove duplication with below
                    let selectedScreenModel = get world_ <| worldScreenModelLens selectedScreenAddress
                    let incomingModel = get selectedScreenModel incomingModelLens
                    let (incomingModel', finished) = updateTransition1 incomingModel
                    let selectedScreenModel' = set incomingModel' selectedScreenModel incomingModelLens
                    let world_ = set selectedScreenModel' world_ <| worldScreenModelLens selectedScreenAddress
                    let world_ = setScreenModelState (if finished then IdlingState else IncomingState) selectedScreenAddress world_
                    if finished then
                        publish
                            (FinishedIncomingAddressPart @ selectedScreenAddress)
                            { Handled = false; Data = NoData }
                            world_
                    else (true, world_)
                | OutgoingState ->
                    let selectedScreenModel = get world_ <| worldScreenModelLens selectedScreenAddress
                    let outgoingModel = get selectedScreenModel outgoingModelLens
                    let (outgoingModel', finished) = updateTransition1 outgoingModel
                    let selectedScreenModel' = set outgoingModel' selectedScreenModel outgoingModelLens
                    let world_ = set selectedScreenModel' world_ <| worldScreenModelLens selectedScreenAddress
                    let world_ = setScreenModelState (if finished then IdlingState else OutgoingState) selectedScreenAddress world_
                    if finished then
                        publish
                            (FinishedOutgoingAddressPart @ selectedScreenAddress)
                            { Handled = false; Data = NoData }
                            world_
                    else (true, world_)
                | IdlingState -> (true, world_)
        if keepRunning then update world_
        else (keepRunning, world_)

    let unregisterButton address world =
        world |>
            unsubscribe DownMouseLeftAddress address |>
            unsubscribe UpMouseLeftAddress address

    let handleButtonEventDownMouseLeft address subscriber message world_ =
        match message.Data with
        | MouseButtonData (mousePosition, _) ->
            let button = get world_ (worldButtonLens subscriber)
            if button.Gui.Entity.Enabled && button.Gui.Entity.Visible then
                if isInBox3 mousePosition button.Gui.Position button.Gui.Size then
                    let button' = { button with IsDown = true }
                    let world_ = set button' world_ (worldButtonLens subscriber)
                    let (keepRunning, world_) = publish (straddr "down" subscriber) { Handled = false; Data = NoData } world_
                    (handle message, keepRunning, world_)
                else (message, true, world_)
            else (message, true, world_)
        | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")
    
    let handleButtonEventUpMouseLeft address subscriber message world_ =
        match message.Data with
        | MouseButtonData (mousePosition, _) ->
            let button = get world_ (worldButtonLens subscriber)
            if button.Gui.Entity.Enabled && button.Gui.Entity.Visible then
                let (keepRunning_, world_) =
                    let button' = { button with IsDown = false }
                    let world_ = set button' world_ (worldButtonLens subscriber)
                    publish (straddr "up" subscriber) { Handled = false; Data = NoData } world_
                if keepRunning_ && isInBox3 mousePosition button.Gui.Position button.Gui.Size && button.IsDown then
                    let (keepRunning_, world_) = publish (straddr "click" subscriber) { Handled = false; Data = NoData } world_
                    let sound = PlaySound { Volume = 1.0f; Sound = button.ClickSound }
                    let world_ = { world_ with AudioMessages = sound :: world_.AudioMessages }
                    (handle message, keepRunning_, world_)
                else (message, keepRunning_, world_)
            else (message, true, world_)
        | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")

    let registerButton address world_ =
        let optOldButton = get world_ (worldOptButtonLens address)
        let world_ = if optOldButton.IsSome then unregisterButton address world_ else world_
        let world_ = subscribe DownMouseLeftAddress address handleButtonEventDownMouseLeft world_
        subscribe UpMouseLeftAddress address handleButtonEventUpMouseLeft world_

    let addButton address button world =
        let world' = registerButton address world
        set (Button button) world' (worldEntityModelLens address)

    let removeButton address world =
        let world' = set None world (worldOptEntityModelLens address)
        unregisterButton address world'

    let addLabel address label world =
        set (Label label) world (worldEntityModelLens address)

    let removeLabel address world =
        set None world (worldOptEntityModelLens address)

    let addTextBox address textBox world =
        set (TextBox textBox) world (worldEntityModelLens address)

    let removeTextBox address world =
        set None world (worldOptEntityModelLens address)

    let unregisterToggle address world =
        world |>
            unsubscribe DownMouseLeftAddress address |>
            unsubscribe UpMouseLeftAddress address

    let handleToggleEventDownMouseLeft address subscriber message world_ =
        match message.Data with
        | MouseButtonData (mousePosition, _) ->
            let toggle = get world_ (worldToggleLens subscriber)
            if toggle.Gui.Entity.Enabled && toggle.Gui.Entity.Visible then
                if isInBox3 mousePosition toggle.Gui.Position toggle.Gui.Size then
                    let toggle' = { toggle with IsPressed = true }
                    let world_ = set toggle' world_ (worldToggleLens subscriber)
                    (handle message, true, world_)
                else (message, true, world_)
            else (message, true, world_)
        | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")
    
    let handleToggleEventUpMouseLeft address subscriber message world_ =
        match message.Data with
        | MouseButtonData (mousePosition, _) ->
            let toggle = get world_ (worldToggleLens subscriber)
            if toggle.Gui.Entity.Enabled && toggle.Gui.Entity.Visible && toggle.IsPressed then
                let toggle' = { toggle with IsPressed = false }
                if isInBox3 mousePosition toggle'.Gui.Position toggle'.Gui.Size then
                    let toggle'' = { toggle' with IsOn = not toggle'.IsOn }
                    let world_ = set toggle'' world_ (worldToggleLens subscriber)
                    let messageType = if toggle''.IsOn then "on" else "off"
                    let (keepRunning, world_) = publish (straddr messageType subscriber) { Handled = false; Data = NoData } world_
                    let sound = PlaySound { Volume = 1.0f; Sound = toggle''.ToggleSound }
                    let world_ = { world_ with AudioMessages = sound :: world_.AudioMessages }
                    (handle message, keepRunning, world_)
                else
                    let world_ = set toggle' world_ (worldToggleLens subscriber)
                    (message, true, world_)
            else (message, true, world_)
        | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")

    let registerToggle address world =
        let world' = subscribe DownMouseLeftAddress address handleToggleEventDownMouseLeft world
        subscribe UpMouseLeftAddress address handleToggleEventUpMouseLeft world'

    let addToggle address toggle world =
        let world' = registerToggle address world
        set (Toggle toggle) world' (worldEntityModelLens address)

    let removeToggle address world =
        let world' = set None world (worldOptEntityModelLens address)
        unregisterToggle address world'

    let unregisterFeeler address world =
        world |>
            unsubscribe UpMouseLeftAddress address |>
            unsubscribe DownMouseLeftAddress address

    let handleFeelerEventDownMouseLeft address subscriber message world_ =
        match message.Data with
        | MouseButtonData (mousePosition, _) as mouseButtonData ->
            let feeler = get world_ (worldFeelerLens subscriber)
            if feeler.Gui.Entity.Enabled && feeler.Gui.Entity.Visible then
                if isInBox3 mousePosition feeler.Gui.Position feeler.Gui.Size then
                    let feeler' = { feeler with IsTouched = true }
                    let world_ = set feeler' world_ (worldFeelerLens subscriber)
                    let (keepRunning, world_) = publish (straddr "touch" subscriber) { Handled = false; Data = mouseButtonData } world_
                    (handle message, keepRunning, world_)
                else (message, true, world_)
            else (message, true, world_)
        | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")
    
    let handleFeelerEventUpMouseLeft address subscriber message world_ =
        match message.Data with
        | MouseButtonData _ ->
            let feeler = get world_ (worldFeelerLens subscriber)
            if feeler.Gui.Entity.Enabled && feeler.Gui.Entity.Visible then
                let feeler' = { feeler with IsTouched = false }
                let world_ = set feeler' world_ (worldFeelerLens subscriber)
                let (keepRunning, world_) = publish (straddr "release" subscriber) { Handled = false; Data = NoData } world_
                (handle message, keepRunning, world_)
            else (message, true, world_)
        | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")
    
    let registerFeeler address world =
        let world' = subscribe DownMouseLeftAddress address handleFeelerEventDownMouseLeft world
        subscribe UpMouseLeftAddress address handleFeelerEventUpMouseLeft world'

    let addFeeler address feeler world =
        let world' = registerFeeler address world
        set (Feeler feeler) world' (worldEntityModelLens address)

    let removeFeeler address world =
        let world' = set None world (worldOptEntityModelLens address)
        unregisterFeeler address world'

    let unregisterBlockPhysics address (block : Block) world =
        let bodyDestroyMessage = BodyDestroyMessage { PhysicsId = block.PhysicsId }
        { world with PhysicsMessages = bodyDestroyMessage :: world.PhysicsMessages }

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
                              Restitution = 0.5f
                              FixedRotation = false
                              LinearDamping = 5.0f
                              AngularDamping = 5.0f }}
                  Position = block.Actor.Position + block.Actor.Size * 0.5f
                  Rotation = block.Actor.Rotation
                  Density = block.Density
                  BodyType = block.BodyType }
        { world with PhysicsMessages = bodyCreateMessage :: world.PhysicsMessages }

    let addBlock address block world =
        let world' = registerBlockPhysics address block world
        set (Block block) world' (worldEntityModelLens address)

    let removeBlock address block world =
        let world' = set None world (worldOptEntityModelLens address)
        unregisterBlockPhysics address block world'

    let unregisterAvatarPhysics address avatar world =
        let bodyDestroyMessage = BodyDestroyMessage { PhysicsId = avatar.PhysicsId }
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

    let addAvatar address avatar world =
        let world' = registerAvatarPhysics address avatar world
        set (Avatar avatar) world' (worldEntityModelLens address)

    let removeAvatar address avatar world =
        let world' = set None world (worldOptEntityModelLens address)
        unregisterAvatarPhysics address avatar world'

    let unregisterTileMapPhysics address tileMap world =
        world

    let registerTileMapPhysics address tileMap world =
        (*let bodyCreateMessage =
            BodyCreateMessage
                { EntityAddress = address
                  Shape = BoxShape { Center = Vector2.Zero; Extent = actor.Size * 0.5f }
                  Position = actor.Position + actor.Size * 0.5f
                  Rotation = actor.Rotation
                  Density = tileMap.Density
                  BodyType = BodyType.Static }
        { world with PhysicsMessages = bodyCreateMessage :: world.PhysicsMessages }*)
        world

    let addTileMap address tileMap world =
        let world' = registerTileMapPhysics address tileMap world
        set (TileMap tileMap) world' (worldEntityModelLens address)

    let removeTileMap address tileMap world =
        let world' = set None world (worldOptEntityModelLens address)
        unregisterTileMapPhysics tileMap address world'

    let addEntityModel address entityModel world =
        match entityModel with
        | Button button -> addButton address button world
        | Label label -> addLabel address label world
        | TextBox textBox -> addTextBox address textBox world
        | Toggle toggle -> addToggle address toggle world
        | Feeler feeler -> addFeeler address feeler world
        | Block block -> addBlock address block world
        | Avatar avatar -> addAvatar address avatar world
        | TileMap tileMap -> addTileMap address tileMap world

    let addEntityModels address entityModels world =
        let group = get world (worldGroupLens address)
        List.fold
            (fun world' entityModel ->
                let entity = get entityModel entityLens
                addEntityModel (addrstr address entity.Name) entityModel world')
            world
            entityModels

    let removeEntityModel address world =
        let entityModel = get world <| worldEntityModelLens address
        match entityModel with
        | Button button -> removeButton address world
        | Label label -> removeLabel address world
        | TextBox textBox -> removeTextBox address world
        | Toggle toggle -> removeToggle address world
        | Feeler feeler -> removeFeeler address world
        | Block block -> removeBlock address block world
        | Avatar avatar -> removeAvatar address avatar world
        | TileMap tileMap -> removeTileMap address tileMap world

    let removeEntityModels address world =
        let group = get world (worldGroupLens address)
        Seq.fold
            (fun world' entityAddress -> removeEntityModel (address @ [entityAddress]) world')
            world
            (Map.toKeySeq group.EntityModels)

    let addGroup address group entityModels world =
        traceIf (not <| Map.isEmpty group.EntityModels) "Adding populated groups to the world is not supported."
        let world' = set (Group group) world (worldGroupModelLens address)
        addEntityModels address entityModels world'

    let removeGroup address world =
        let world' = removeEntityModels address world
        set None world' (worldOptGroupModelLens address)

    let propagateBlockPhysics address block world =
        let world' = unregisterBlockPhysics address block world
        registerBlockPhysics address block world'

    let propagateAvatarPhysics address avatar world =
        let world' = unregisterAvatarPhysics address avatar world
        registerAvatarPhysics address avatar world'

    let propagateEntityModelPhysics address entityModel world =
        match entityModel with
        | Button _
        | Label _
        | TextBox _
        | Toggle _
        | Feeler _ -> world
        | Block block -> propagateBlockPhysics address block world
        | Avatar avatar -> propagateAvatarPhysics address avatar world
        | TileMap tileMap -> world // TODO

    // TODO: see if there's a nice way to put this module in another file
    [<RequireQualifiedAccess>]
    module Test =

        let ScreenAddress = addr "testScreenModel"
        let GroupAddress = addrstr ScreenAddress "testGroupModel"
        let FeelerAddress = addrstr GroupAddress "testFeeler"
        let TextBoxAddress = addrstr GroupAddress "testTextBox"
        let ToggleAddress = addrstr GroupAddress "testToggle"
        let LabelAddress = addrstr GroupAddress "testLabel"
        let ButtonAddress = addrstr GroupAddress "testButton"
        let BlockAddress = addrstr GroupAddress "testBlock"
        let TileMapAddress = addrstr GroupAddress "testTileMap"
        let FloorAddress = addrstr GroupAddress "testFloor"
        let AvatarAddress = addrstr GroupAddress "testAvatar"
        let ClickButtonAddress = straddr "click" ButtonAddress

        let addTestGroup address testGroup entityModels world_ =
        
            debugIf (fun () -> not <| Map.isEmpty testGroup.Group.EntityModels) "Adding populated groups to the world is not supported."

            let assetMetadataMap =
                match tryGenerateAssetMetadataMap "AssetGraph.xml" with
                | Left errorMsg -> failwith errorMsg
                | Right assetMetadataMap -> assetMetadataMap

            let createTestBlock assetMetadataMap =
                let id = getNuId ()
                let testBlock =
                    { PhysicsId = getPhysicsId ()
                      Density = NormalDensity
                      BodyType = Dynamic
                      Sprite = { SpriteAssetName = Lun.make "Image3"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
                      Actor =
                      { Position = Vector2 (400.0f, 200.0f)
                        Depth = 0.0f
                        Size = getTextureSizeAsVector2 (Lun.make "Image3") (Lun.make "Default") assetMetadataMap
                        Rotation = 2.0f
                        Entity =
                        { Id = id
                          Name = str id
                          Enabled = true
                          Visible = true }}}
                testBlock

            let adjustCamera1 world_ =
                let actor = get world_ (worldActorLens AvatarAddress)
                let camera = { world_.Camera with EyePosition = actor.Position + actor.Size * 0.5f }
                { world_ with Camera = camera }
                  
            let adjustCamera _ _ message world_ =
                (message, true, adjustCamera1 world_)

            let moveAvatar address _ message world_ =
                let feeler = get world_ (worldFeelerLens FeelerAddress)
                if feeler.IsTouched then
                    let avatar = get world_ (worldAvatarLens AvatarAddress)
                    let camera = world_.Camera
                    let view = getInverseViewF camera
                    let mousePositionWorld = world_.MouseState.MousePosition + view
                    let actorCenter = avatar.Actor.Position + avatar.Actor.Size * 0.5f
                    let impulseVector = (mousePositionWorld - actorCenter) * 5.0f
                    let applyImpulseMessage = { PhysicsId = avatar.PhysicsId; Impulse = impulseVector }
                    let world_ = { world_ with PhysicsMessages = ApplyImpulseMessage applyImpulseMessage :: world_.PhysicsMessages }
                    (message, true, world_)
                else (message, true, world_)

            let addBoxes _ _ message world_ =
                let world_ =
                    List.fold
                        (fun world_ _ ->
                            let block = createTestBlock assetMetadataMap
                            addBlock (addrstr GroupAddress block.Actor.Entity.Name) block world_)
                        world_
                        [0..7]
                (handle message, true, world_)

            let hintRenderingPackageUse = HintRenderingPackageUse { FileName = "AssetGraph.xml"; PackageName = "Default"; HRPU = () }
            let playSong = PlaySong { Song = { SongAssetName = Lun.make "Song"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }; FadeOutCurrentSong = true }
            let world_ = set (Some ScreenAddress) world_ worldOptSelectedScreenModelAddressLens
            let world_ = subscribe TickAddress [] moveAvatar world_
            let world_ = subscribe TickAddress [] adjustCamera world_
            let world_ = subscribe ClickButtonAddress [] addBoxes world_
            let world_ = { world_ with PhysicsMessages = SetGravityMessage Vector2.Zero :: world_.PhysicsMessages }
            let world_ = { world_ with RenderMessages = hintRenderingPackageUse :: world_.RenderMessages }
            let world_ = { world_ with AudioMessages = FadeOutSong :: playSong :: world_.AudioMessages }
            let world_ = set (TestGroup testGroup) world_ (worldGroupModelLens address)
            let world_ = addEntityModels address entityModels world_
            adjustCamera1 world_

        let removeTestGroup address world_ =
            let world_ = unsubscribe TickAddress [] world_
            let world_ = unsubscribe TickAddress [] world_
            let world_ = unsubscribe ClickButtonAddress [] world_
            let world_ = set None world_ worldOptSelectedScreenModelAddressLens
            let world_ = removeEntityModels address world_
            set None world_ (worldOptGroupModelLens address)

    let addGroupModel address groupModel entityModels world =
        match groupModel with
        | Group group -> addGroup address group entityModels world
        | TestGroup testGroup -> Test.addTestGroup address testGroup entityModels world

    let removeGroupModel address world =
        let groupModel = get world <| worldGroupModelLens address
        match groupModel with
        | Group group -> removeGroup address world
        | TestGroup testGroup -> Test.removeTestGroup address world

    let addGroupModels address groupDescriptors world =
        List.fold
            (fun world' (groupName, groupModel, entityModels) ->
                addGroupModel (address @ [groupName]) groupModel entityModels world')
            world
            groupDescriptors

    let removeGroupModels address world =
        let screenModel = get world <| worldScreenModelLens address
        let groupModels = get screenModel screenGroupModelsLens
        Map.fold
            (fun world' groupModelName _ ->
                removeGroupModel (address @ [groupModelName]) world')
            world
            groupModels

    let addScreen address screen groupDescriptors world =
        debugIf (fun () -> not <| Map.isEmpty screen.GroupModels) "Adding populated screens to the world is not supported."
        let world' = set (Screen screen) world (worldScreenModelLens address)
        addGroupModels address groupDescriptors world'

    let removeScreen address world =
        let world' = removeGroupModels address world
        set None world' (worldOptScreenModelLens address)

    let addScreenModel address screenModel groupDescriptor world =
        match screenModel with
        | Screen screen -> addScreen address screen groupDescriptor world

    let removeScreenModel address world =
        let screenModel = get world <| worldScreenModelLens address
        match screenModel with
        | Screen screen -> removeScreen address world

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
        let splashScreenModel = makeDissolveScreen incomingTime outgoingTime
        let splashGroupModel = Group <| makeDefaultGroup ()
        let splashLabel = Label { Gui = { makeDefaultGui (Some "splashLabel") with Size = world.Camera.EyeSize }; LabelSprite = sprite }
        let world' = addScreen address splashScreenModel [(Lun.make "splashGroup", splashGroupModel, [splashLabel])] world
        let world'' = subscribe (FinishedIncomingAddressPart @ address) address (handleSplashScreenIdle idlingTime) world'
        subscribe (FinishedOutgoingAddressPart @ address) address handleFinishedOutgoing world''

    let createDissolveScreenFromFile groupModelFileName groupModelName incomingTime outgoingTime screenAddress world =
        let screenModel = Screen <| makeDissolveScreen incomingTime outgoingTime
        let (groupModel, entityModels) = loadGroupModelFile groupModelFileName world
        addScreenModel screenAddress screenModel [(groupModelName, groupModel, entityModels)] world

    let reregisterPhysicsHack4 groupAddress world _ entityModel =
        match entityModel with
        | Button _
        | Label _
        | TextBox _
        | Toggle _
        | Feeler _ -> world
        | Block block -> registerBlockPhysics (addrstr groupAddress block.Actor.Entity.Name) block world
        | Avatar avatar -> registerAvatarPhysics (addrstr groupAddress avatar.Actor.Entity.Name) avatar world
        | TileMap tileMap -> registerTileMapPhysics (addrstr groupAddress tileMap.Actor.Entity.Name) tileMap world

    let reregisterPhysicsHack groupAddress world =
        let groupModel = get world <| worldGroupModelLens groupAddress
        let entityModels = get groupModel entityModelsLens
        Map.fold (reregisterPhysicsHack4 groupAddress) world entityModels

    let getComponentAudioDescriptors world : AudioDescriptor rQueue =
        let descriptorLists = List.fold (fun descs (comp : IWorldComponent) -> comp.GetAudioDescriptors world :: descs) [] world.Components // TODO: get audio descriptors
        List.collect (fun descs -> descs) descriptorLists

    let getAudioDescriptors world : AudioDescriptor rQueue =
        let componentDescriptors = getComponentAudioDescriptors world
        let worldDescriptors = [] // TODO: get audio descriptors when there are some
        componentDescriptors @ worldDescriptors // NOTE: pretty inefficient

    /// Play the world's audio.
    let play world_ =
        let audioMessages = world_.AudioMessages
        let audioDescriptors = getAudioDescriptors world_
        let audioPlayer = world_.AudioPlayer
        let world_ = { world_ with AudioMessages = [] }
        let world_ = { world_ with AudioPlayer = Nu.Audio.play audioMessages audioDescriptors audioPlayer }
        world_

    let getComponentRenderDescriptors world : RenderDescriptor rQueue =
        let descriptorLists = List.fold (fun descs (comp : IWorldComponent) -> comp.GetRenderDescriptors world :: descs) [] world.Components // TODO: get render descriptors
        List.collect (fun descs -> descs) descriptorLists

    let getEntityRenderDescriptors view entity =
        match entity with
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
                List.map
                    (fun (layer : TmxLayer) ->
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
                                  Depth = actor.Depth }
                        LayerableDescriptor layeredTileLayerDescriptor)
                    layers

    let getGroupModelRenderDescriptors camera groupModel =
        let group = get groupModel groupLens
        let view = getInverseView camera
        let entities = Map.toValueSeq group.EntityModels
        Seq.map (getEntityRenderDescriptors view) entities

    let getTransitionModelRenderDescriptors camera transitionModel =
        match transitionModel with
        | Transition _ -> []
        | Dissolve dissolve ->
            let transition = dissolve.Transition
            let progress = single transition.Ticks / single transition.Lifetime
            let alpha = match transition.Type with Incoming -> 1.0f - progress | Outgoing -> progress
            let color = Vector4 (1.0f, 1.0f, 1.0f, alpha)
            [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = Vector2.Zero; Size = camera.EyeSize; Rotation = 0.0f; Sprite = dissolve.Sprite; Color = color }; Depth = Single.MaxValue })]

    let getWorldRenderDescriptors world =
        match get world worldOptSelectedScreenModelAddressLens with
        | None -> []
        | Some activeScreenAddress ->
            let activeScreen = get world (worldScreenLens activeScreenAddress)
            let groups = Map.toValueSeq activeScreen.GroupModels
            let descriptorSeqLists = Seq.map (getGroupModelRenderDescriptors world.Camera) groups
            let descriptorSeq = Seq.concat descriptorSeqLists
            let descriptors = List.concat descriptorSeq
            match activeScreen.State with
            | IncomingState -> descriptors @ getTransitionModelRenderDescriptors world.Camera activeScreen.IncomingModel
            | OutgoingState -> descriptors @ getTransitionModelRenderDescriptors world.Camera activeScreen.OutgoingModel
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
                let actor = get world (worldActorLens bodyTransformMessage.EntityAddress)
                let actor' = { actor with Position = bodyTransformMessage.Position - actor.Size * 0.5f // TODO: see if this center-offsetting can be encapsulated withing the Physics module!
                                          Rotation = bodyTransformMessage.Rotation }
                (keepRunning, set actor' world <| worldActorLens bodyTransformMessage.EntityAddress)
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