module Nu.Simulation
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
open Nu.AssetMetadata
open Nu.Input
open Nu.Audio
open Nu.Sdl
open Nu.Entity
open Nu.Group
open Nu.Screen
open Nu.Game
open Nu.Camera

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

let TickAddress = [Lun.make "tick"]
let MouseDragAddress = [Lun.make "mouse"; Lun.make "drag"]
let MouseMoveAddress = [Lun.make "mouse"; Lun.make "move"]
let MouseLeftAddress = [Lun.make "mouse"; Lun.make "left"]
let DownMouseLeftAddress = Lun.make "down" :: MouseLeftAddress
let UpMouseLeftAddress = Lun.make "up" :: MouseLeftAddress
let GameModelPublishingPriority = Single.MaxValue
let ScreenModelPublishingPriority = GameModelPublishingPriority * 0.5f
let GroupModelPublishingPriority = ScreenModelPublishingPriority * 0.5f

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
    Subscription of (Address -> Address -> Message -> World -> (Message * World))

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
    
let worldGameModel =
    { Get = fun this -> this.GameModel
      Set = fun gameModel this -> { this with GameModel = gameModel }}

let worldGame =
    { Get = fun this -> get this.GameModel gameModelGame
      Set = fun game this -> { this with GameModel = set game this.GameModel gameModelGame }}
      
let worldCamera =
    { Get = fun this -> this.Camera
      Set = fun camera this -> { this with Camera = camera }}

let worldMouseState =
    { Get = fun this -> this.MouseState
      Set = fun mouseState this -> { this with MouseState = mouseState }}

let worldOptSelectedScreenModelAddress = worldGameModel >>| gameModelOptSelectedScreenModelAddress
    
let worldScreenModel (address : Address) = worldGameModel >>| gameModelScreenModel address
let worldOptScreenModel (address : Address) = worldGameModel >>| gameModelOptScreenModel address
    
let worldScreen (address : Address) = worldGameModel >>| gameModelScreen address
let worldOptScreen (address : Address) = worldGameModel >>| gameModelOptScreen address
    
let worldGroupModel (address : Address) = worldGameModel >>| gameModelGroupModel address
let worldOptGroupModel (address : Address) = worldGameModel >>| gameModelOptGroupModel address
    
let worldGroup (address : Address) = worldGameModel >>| gameModelGroup address
let worldOptGroup (address : Address) = worldGameModel >>| gameModelOptGroup address
    
let worldEntityModel (address : Address) = worldGameModel >>| gameModelEntityModel address
let worldOptEntityModel (address : Address) = worldGameModel >>| gameModelOptEntityModel address
    
let worldEntity (address : Address) = worldGameModel >>| gameModelEntity address
let worldOptEntity (address : Address) = worldGameModel >>| gameModelOptEntity address
    
let worldGui (address : Address) = worldGameModel >>| gameModelGui address
let worldOptGui (address : Address) = worldGameModel >>| gameModelOptGui address
    
let worldButton (address : Address) = worldGameModel >>| gameModelButton address
let worldOptButton (address : Address) = worldGameModel >>| gameModelOptButton address
    
let worldLabel (address : Address) = worldGameModel >>| gameModelLabel address
let worldOptLabel (address : Address) = worldGameModel >>| gameModelOptLabel address
    
let worldTextBox (address : Address) = worldGameModel >>| gameModelTextBox address
let worldOptTextBox (address : Address) = worldGameModel >>| gameModelOptTextBox address
    
let worldToggle (address : Address) = worldGameModel >>| gameModelToggle address
let worldOptToggle (address : Address) = worldGameModel >>| gameModelOptToggle address
    
let worldFeeler (address : Address) = worldGameModel >>| gameModelFeeler address
let worldOptFeeler (address : Address) = worldGameModel >>| gameModelOptFeeler address
    
let worldActor (address : Address) = worldGameModel >>| gameModelActor address
let worldOptActor (address : Address) = worldGameModel >>| gameModelOptActor address
    
let worldBlock (address : Address) = worldGameModel >>| gameModelBlock address
let worldOptBlock (address : Address) = worldGameModel >>| gameModelOptBlock address
    
let worldAvatar (address : Address) = worldGameModel >>| gameModelAvatar address
let worldOptAvatar (address : Address) = worldGameModel >>| gameModelOptAvatar address
    
let worldTileMap (address : Address) = worldGameModel >>| gameModelTileMap address
let worldOptTileMap (address : Address) = worldGameModel >>| gameModelOptTileMap address

let getSimulant address world =
    match address with
    | [] -> GameModel <| get world worldGameModel
    | [_] as screenAddress -> ScreenModel <| get world (worldScreenModel screenAddress)
    | [_; _] as groupAddress -> GroupModel <| get world (worldGroupModel groupAddress)
    | [_; _; _] as entityAddress -> EntityModel <| get world (worldEntityModel entityAddress)
    | _ -> failwith <| "Invalid simulant address '" + str address + "'."

/// Initialize Nu's various type converters.
/// Must be called for reflection to work in Nu.
let initTypeConverters () =
    initMathConverters ()
    initAudioConverters ()
    initRenderConverters ()

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
            let transform = getEntityModelTransform true world.Camera entityModel
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

/// Mark a message as handled.
let handle message =
    { Handled = true; Data = message.Data }

let propagateBlockTransform (block : Block) world =
    let bodyTransformInMessage = { BodyTransformInMessage.PhysicsId = block.PhysicsId; Position = block.Actor.Position + block.Actor.Size * 0.5f; Rotation = block.Actor.Rotation } // TODO: see if this center-offsetting can be encapsulated withing the Physics module!
    { world with PhysicsMessages = BodyTransformInMessage bodyTransformInMessage :: world.PhysicsMessages }

let propagateAvatarTransform (avatar : Avatar) world =
    let bodyTransformInMessage = { BodyTransformInMessage.PhysicsId = avatar.PhysicsId; Position = avatar.Actor.Position + avatar.Actor.Size * 0.5f; Rotation = avatar.Actor.Rotation }// TODO: see if this center-offsetting can be encapsulated withing the Physics module!
    { world with PhysicsMessages = BodyTransformInMessage bodyTransformInMessage :: world.PhysicsMessages }

let propagateEntityModelTransform entityModel world =
    match entityModel with
    | Button _
    | Label _
    | TextBox _
    | Toggle _
    | Feeler _ -> world
    | Block block -> propagateBlockTransform block world
    | Avatar avatar -> propagateAvatarTransform avatar world
    | TileMap tileMap -> world // TODO

let propagateEntityModelProperties entityModel world =
    propagateEntityModelTransform entityModel world

let isAddressSelected address world =
    let optScreenAddress = (get world worldGame).OptSelectedScreenModelAddress
    match (address, optScreenAddress) with
    | ([], _) -> true
    | (_, None) -> false
    | (_, Some []) -> false
    | (addressHead :: addressTail, Some [screenAddressHead]) ->
        match addressTail with
        | [] -> screenAddressHead = addressHead
        | addressHead2 :: addressTail2 ->
            let screenModel = get world <| worldScreenModel [addressHead]
            let screen = get screenModel screenModelscreen
            match addressTail2 with
            | [] -> screen.GroupModels.ContainsKey addressHead2
            | addressHead3 :: addressTail3 ->
                let groupModel = Map.find addressHead2 screen.GroupModels
                let group = get groupModel groupModelGroup
                match addressTail3 with
                | [] -> group.EntityModels.ContainsKey addressHead3
                | _ -> false
    | (_, Some (_ :: _)) -> false

/// Publish a message to the given address.
let publish address message world =
    let optSubList = Map.tryFind address world.Subscriptions
    match optSubList with
    | None -> world
    | Some subList ->
        let subListSorted = subscriptionSort subList world
        let (_, world_) =
            List.foldWhile
                (fun (message_, world_) (subscriber, (Subscription subscription)) ->
                    if message_.Handled then None
                    elif isAddressSelected subscriber world_ then Some (subscription address subscriber message_ world_)
                    else Some (message_, world_))
                (message, world)
                subListSorted
        world_

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
        let subList2 = List.remove (fun (address, _) -> address = subscriber) subList
        let subscriptions2 = Map.add address subList2 subs
        { world with Subscriptions = subscriptions2 }

/// Execute a procedure within the context of a given subscription at the given address.
let withSubscription address subscription subscriber procedure world =
    let world2 = subscribe address subscriber subscription world
    let world3 = procedure world2
    unsubscribe address subscriber world3

let unregisterButton address world =
    world |>
        unsubscribe DownMouseLeftAddress address |>
        unsubscribe UpMouseLeftAddress address

let handleButtonEventDownMouseLeft address subscriber message world_ =
    match message.Data with
    | MouseButtonData (mousePosition, _) ->
        let button = get world_ (worldButton subscriber)
        if button.Gui.Entity.Enabled && button.Gui.Entity.Visible then
            if isInBox3 mousePosition button.Gui.Position button.Gui.Size then
                let button_ = { button with IsDown = true }
                let world_ = set button_ world_ (worldButton subscriber)
                let world_ = publish (Lun.make "down" :: subscriber) { Handled = false; Data = NoData } world_
                (handle message, world_)
            else (message, world_)
        else (message, world_)
    | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")
    
let handleButtonEventUpMouseLeft address subscriber message world_ =
    match message.Data with
    | MouseButtonData (mousePosition, _) ->
        let button = get world_ (worldButton subscriber)
        if button.Gui.Entity.Enabled && button.Gui.Entity.Visible then
            let world_ =
                let button_ = { button with IsDown = false }
                let world_ = set button_ world_ (worldButton subscriber)
                publish (Lun.make "up" :: subscriber) { Handled = false; Data = NoData } world_
            if isInBox3 mousePosition button.Gui.Position button.Gui.Size && button.IsDown then
                let world_ = publish (Lun.make "click" :: subscriber) { Handled = false; Data = NoData } world_
                let sound = PlaySound { Volume = 1.0f; Sound = button.ClickSound }
                let world_ = { world_ with AudioMessages = sound :: world_.AudioMessages }
                (handle message, world_)
            else (message, world_)
        else (message, world_)
    | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")

let registerButton address world_ =
    let optOldButton = get world_ (worldOptButton address)
    let world_ = if optOldButton.IsSome then unregisterButton address world_ else world_
    let world_ = subscribe DownMouseLeftAddress address handleButtonEventDownMouseLeft world_
    subscribe UpMouseLeftAddress address handleButtonEventUpMouseLeft world_

let addButton address button world =
    let world2 = registerButton address world
    set (Button button) world2 (worldEntityModel address)

let removeButton address world =
    let world2 = set None world (worldOptEntityModel address)
    unregisterButton address world2

let addLabel address label world =
    set (Label label) world (worldEntityModel address)

let removeLabel address world =
    set None world (worldOptEntityModel address)

let addTextBox address textBox world =
    set (TextBox textBox) world (worldEntityModel address)

let removeTextBox address world =
    set None world (worldOptEntityModel address)

let unregisterToggle address world =
    world |>
        unsubscribe DownMouseLeftAddress address |>
        unsubscribe UpMouseLeftAddress address

let handleToggleEventDownMouseLeft address subscriber message world_ =
    match message.Data with
    | MouseButtonData (mousePosition, _) ->
        let toggle_ = get world_ (worldToggle subscriber)
        if toggle_.Gui.Entity.Enabled && toggle_.Gui.Entity.Visible then
            if isInBox3 mousePosition toggle_.Gui.Position toggle_.Gui.Size then
                let toggle_ = { toggle_ with IsPressed = true }
                let world_ = set toggle_ world_ (worldToggle subscriber)
                (handle message, world_)
            else (message, world_)
        else (message, world_)
    | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")
    
let handleToggleEventUpMouseLeft address subscriber message world_ =
    match message.Data with
    | MouseButtonData (mousePosition, _) ->
        let toggle = get world_ (worldToggle subscriber)
        if toggle.Gui.Entity.Enabled && toggle.Gui.Entity.Visible && toggle.IsPressed then
            let toggle_ = { toggle with IsPressed = false }
            if isInBox3 mousePosition toggle.Gui.Position toggle.Gui.Size then
                let toggle_ = { toggle_ with IsOn = not toggle_.IsOn }
                let world_ = set toggle_ world_ (worldToggle subscriber)
                let messageType = if toggle.IsOn then "on" else "off"
                let world_ = publish (Lun.make messageType :: subscriber) { Handled = false; Data = NoData } world_
                let sound = PlaySound { Volume = 1.0f; Sound = toggle.ToggleSound }
                let world_ = { world_ with AudioMessages = sound :: world_.AudioMessages }
                (handle message, world_)
            else
                let world_ = set toggle_ world_ (worldToggle subscriber)
                (message, world_)
        else (message, world_)
    | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")

let registerToggle address world =
    let world2 = subscribe DownMouseLeftAddress address handleToggleEventDownMouseLeft world
    subscribe UpMouseLeftAddress address handleToggleEventUpMouseLeft world2

let addToggle address toggle world =
    let world2 = registerToggle address world
    set (Toggle toggle) world2 (worldEntityModel address)

let removeToggle address world =
    let world2 = set None world (worldOptEntityModel address)
    unregisterToggle address world2

let unregisterFeeler address world =
    world |>
        unsubscribe UpMouseLeftAddress address |>
        unsubscribe DownMouseLeftAddress address

let handleFeelerEventDownMouseLeft address subscriber message world_ =
    match message.Data with
    | MouseButtonData (mousePosition, _) as mouseButtonData ->
        let feeler = get world_ (worldFeeler subscriber)
        if feeler.Gui.Entity.Enabled && feeler.Gui.Entity.Visible then
            if isInBox3 mousePosition feeler.Gui.Position feeler.Gui.Size then
                let feeler_ = { feeler with IsTouched = true }
                let world_ = set feeler_ world_ (worldFeeler subscriber)
                let world_ = publish (Lun.make "touch" :: subscriber) { Handled = false; Data = mouseButtonData } world_
                (handle message, world_)
            else (message, world_)
        else (message, world_)
    | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")
    
let handleFeelerEventUpMouseLeft address subscriber message world_ =
    match message.Data with
    | MouseButtonData _ ->
        let feeler_ = get world_ (worldFeeler subscriber)
        if feeler_.Gui.Entity.Enabled && feeler_.Gui.Entity.Visible then
            let feeler_ = { feeler_ with IsTouched = false }
            let world_ = set feeler_ world_ (worldFeeler subscriber)
            let world_ = publish (Lun.make "release" :: subscriber) { Handled = false; Data = NoData } world_
            (handle message, world_)
        else (message, world_)
    | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")
    
let registerFeeler address world =
    let world2 = subscribe DownMouseLeftAddress address handleFeelerEventDownMouseLeft world
    subscribe UpMouseLeftAddress address handleFeelerEventUpMouseLeft world2

let addFeeler address feeler world =
    let world2 = registerFeeler address world
    set (Feeler feeler) world2 (worldEntityModel address)

let removeFeeler address world =
    let world2 = set None world (worldOptEntityModel address)
    unregisterFeeler address world2

let unregisterBlock address (block : Block) world =
    let bodyDestroyMessage = BodyDestroyMessage { PhysicsId = block.PhysicsId }
    { world with PhysicsMessages = bodyDestroyMessage :: world.PhysicsMessages }

let registerBlock address (block : Block) world =
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
    let world2 = registerBlock address block world
    set (Block block) world2 (worldEntityModel address)

let removeBlock address block world =
    let world2 = set None world (worldOptEntityModel address)
    unregisterBlock address block world2

let unregisterAvatar address avatar world =
    let bodyDestroyMessage = BodyDestroyMessage { PhysicsId = avatar.PhysicsId }
    { world with PhysicsMessages = bodyDestroyMessage :: world.PhysicsMessages }

let registerAvatar address avatar world =
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
    let world2 = registerAvatar address avatar world
    set (Avatar avatar) world2 (worldEntityModel address)

let removeAvatar address avatar world =
    let world2 = set None world (worldOptEntityModel address)
    unregisterAvatar address avatar world2

let unregisterTileMap address tileMap world =
    world

let registerTileMap address tileMap world =
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
    let world2 = registerTileMap address tileMap world
    set (TileMap tileMap) world2 (worldEntityModel address)

let removeTileMap address tileMap world =
    let world2 = set None world (worldOptEntityModel address)
    unregisterTileMap tileMap address world2

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

let addEntityModels entityModels address world =
    let group = get world (worldGroup address)
    List.fold
        (fun world_ entityModel ->
            let entity = get entityModel entityModelEntity
            addEntityModel (address @ [Lun.make entity.Name]) entityModel world_)
        world
        entityModels

let removeEntityModel address world =
    let entityModel = get world <| worldEntityModel address
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
    let group = get world (worldGroup address)
    Seq.fold
        (fun world_ entityModelAddress -> removeEntityModel (address @ [entityModelAddress]) world_)
        world
        (Map.toKeySeq group.EntityModels)

let addGroup address group world =
    traceIf (not <| Map.isEmpty group.EntityModels) "Adding populated groups to the world is not supported."
    set (Group group) world (worldGroupModel address)

let removeGroup address world =
    let world2 = removeEntityModels address world
    set None world2 (worldOptGroupModel address)

// TODO: see if there's a nice way to put this module in another file
[<RequireQualifiedAccess>]
module Test =

    let ScreenModelAddress = [Lun.make "testScreenModel"]
    let GroupModelAddress = ScreenModelAddress @ [Lun.make "testGroupModel"]
    let FeelerAddress = GroupModelAddress @ [Lun.make "testFeeler"]
    let TextBoxAddress = GroupModelAddress @ [Lun.make "testTextBox"]
    let ToggleAddress = GroupModelAddress @ [Lun.make "testToggle"]
    let LabelAddress = GroupModelAddress @ [Lun.make "testLabel"]
    let ButtonAddress = GroupModelAddress @ [Lun.make "testButton"]
    let BlockAddress = GroupModelAddress @ [Lun.make "testBlock"]
    let TileMapAddress = GroupModelAddress @ [Lun.make "testTileMap"]
    let FloorAddress = GroupModelAddress @ [Lun.make "testFloor"]
    let AvatarAddress = GroupModelAddress @ [Lun.make "testAvatar"]
    let ClickButtonAddress = Lun.make "click" :: ButtonAddress

    let addTestGroup address testGroup world_ =

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
                  
        let adjustCamera _ _ message world =
            let actor = get world (worldActor AvatarAddress)
            let camera = { world.Camera with EyePosition = actor.Position + actor.Size * 0.5f }
            (message, { world with Camera = camera })

        let moveAvatar address _ message world_ =
            let feeler = get world_ (worldFeeler FeelerAddress)
            if feeler.IsTouched then
                let avatar = get world_ (worldAvatar AvatarAddress)
                let camera = world_.Camera
                let view = inverseViewF camera
                let mousePositionWorld = world_.MouseState.MousePosition + view
                let actorCenter = avatar.Actor.Position + avatar.Actor.Size * 0.5f
                let impulseVector = (mousePositionWorld - actorCenter) * 5.0f
                let applyImpulseMessage = { PhysicsId = avatar.PhysicsId; Impulse = impulseVector }
                let world_ = { world_ with PhysicsMessages = ApplyImpulseMessage applyImpulseMessage :: world_.PhysicsMessages }
                (message, world_)
            else (message, world_)

        let addBoxes _ _ message world_ =
            let world_ =
                List.fold
                    (fun world_ _ ->
                        let block = createTestBlock assetMetadataMap
                        addBlock (GroupModelAddress @ [Lun.make block.Actor.Entity.Name]) block world_)
                    world_
                    [0..7]
            (handle message, world_)

        let hintRenderingPackageUse = HintRenderingPackageUse { FileName = "AssetGraph.xml"; PackageName = "Default"; HRPU = () }
        let playSong = PlaySong { Song = { SongAssetName = Lun.make "Song"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }; FadeOutCurrentSong = true }
        let world_ = set (Some ScreenModelAddress) world_ worldOptSelectedScreenModelAddress
        let world_ = subscribe TickAddress [] moveAvatar world_
        let world_ = subscribe TickAddress [] adjustCamera world_
        let world_ = subscribe ClickButtonAddress [] addBoxes world_
        let world_ = { world_ with PhysicsMessages = SetGravityMessage Vector2.Zero :: world_.PhysicsMessages }
        let world_ = { world_ with RenderMessages = hintRenderingPackageUse :: world_.RenderMessages }
        let world_ = { world_ with AudioMessages = FadeOutSong :: playSong :: world_.AudioMessages }
        traceIf (not <| Map.isEmpty testGroup.Group.EntityModels) "Adding populated groups to the world is not supported."
        set (TestGroup testGroup) world_ (worldGroupModel address)

    let removeTestGroup address world_ =
        let world_ = unsubscribe TickAddress [] world_
        let world_ = unsubscribe TickAddress [] world_
        let world_ = unsubscribe ClickButtonAddress [] world_
        let world_ = set None world_ worldOptSelectedScreenModelAddress
        let world_ = removeEntityModels address world_
        set None world_ (worldOptGroupModel address)

let addGroupModel address groupModel world =
    match groupModel with
    | Group group -> addGroup address group world
    | TestGroup testGroup -> Test.addTestGroup address testGroup world

let removeGroupModel address world =
    let groupModel = get world <| worldGroupModel address
    match groupModel with
    | Group group -> removeGroup address world
    | TestGroup testGroup -> Test.removeTestGroup address world

let addScreen address screen world =
    set (Screen screen) world (worldScreenModel address)

let removeScreen address world =
    set None world (worldOptScreenModel address)

let getComponentAudioDescriptors world : AudioDescriptor rQueue =
    let descriptorLists = List.fold (fun descs (comp : IWorldComponent) -> comp.GetAudioDescriptors world :: descs) [] world.Components // TODO: get audio descriptors
    List.collect (fun descs -> descs) descriptorLists

let getAudioDescriptors world : AudioDescriptor rQueue =
    let componentDescriptors = getComponentAudioDescriptors world
    let worldDescriptors = [] // TODO: get audio descriptors when there are some
    worldDescriptors @ componentDescriptors // NOTE: pretty inefficient

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
        else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = gui.Position; Size = gui.Size; Rotation = 0.0f; Sprite = if button.IsDown then button.DownSprite else button.UpSprite }; Depth = gui.Depth })]
    | Label label ->
        let (_, gui, entity) = labelSep label
        if not label.Gui.Entity.Visible then []
        else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = gui.Position; Size = gui.Size; Rotation = 0.0f; Sprite = label.LabelSprite }; Depth = gui.Depth })]
    | TextBox textBox ->
        let (_, gui, entity) = textBoxSep textBox
        if not entity.Visible then []
        else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = gui.Position; Size = gui.Size; Rotation = 0.0f; Sprite = textBox.BoxSprite }; Depth = gui.Depth })
              LayerableDescriptor (LayeredTextDescriptor { Descriptor = { Text = textBox.Text; Position = gui.Position + textBox.TextOffset; Size = gui.Size - textBox.TextOffset; Font = textBox.TextFont; Color = textBox.TextColor }; Depth = gui.Depth })]
    | Toggle toggle ->
        let (_, gui, entity) = toggleSep toggle
        if not entity.Visible then []
        else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = gui.Position; Size = gui.Size; Rotation = 0.0f; Sprite = if toggle.IsOn || toggle.IsPressed then toggle.OnSprite else toggle.OffSprite }; Depth = gui.Depth })]
    | Feeler _ ->
        []
    | Block block ->
        let (_, actor, entity) = blockSep block
        if not entity.Visible then []
        else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = actor.Position - view; Size = actor.Size; Rotation = actor.Rotation; Sprite = block.Sprite }; Depth = actor.Depth })]
    | Avatar avatar ->
        let (_, actor, entity) = avatarSep avatar
        if not entity.Visible then []
        else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = actor.Position - view; Size = actor.Size; Rotation = actor.Rotation; Sprite = avatar.Sprite }; Depth = actor.Depth })]
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
                                  TileSetSprite = tileMap.TileMapMetadata.[0] } // MAGIC_VALUE: for same reason as above
                              Depth = actor.Depth }
                    LayerableDescriptor layeredTileLayerDescriptor)
                layers

let getGroupModelRenderDescriptors camera groupModel =
    let group = get groupModel groupModelGroup
    let view = inverseView camera
    let entities = Map.toValueSeq group.EntityModels
    Seq.map (getEntityRenderDescriptors view) entities

let getWorldRenderDescriptors world =
    match get world worldOptSelectedScreenModelAddress with
    | None -> []
    | Some activeScreenAddress ->
        let activeScreen = get world (worldScreen activeScreenAddress)
        let groups = Map.toValueSeq activeScreen.GroupModels
        let descriptorSeqLists = Seq.map (getGroupModelRenderDescriptors world.Camera) groups
        let descriptorSeq = Seq.concat descriptorSeqLists
        List.concat descriptorSeq

let getRenderDescriptors world : RenderDescriptor rQueue =
    let componentDescriptors = getComponentRenderDescriptors world
    let worldDescriptors = getWorldRenderDescriptors world
    worldDescriptors @ componentDescriptors // NOTE: pretty inefficient

/// Render the world.
let render world =
    let renderMessages = world.RenderMessages
    let renderDescriptors = getRenderDescriptors world
    let renderer = world.Renderer
    let renderer2 = Nu.Rendering.render renderMessages renderDescriptors renderer
    let world2 = {{ world with RenderMessages = [] } with Renderer = renderer2 }
    world2

let handleIntegrationMessage world integrationMessage =
    match integrationMessage with
    | BodyTransformOutMessage bodyTransformOutMessage ->
        let actor = get world (worldActor bodyTransformOutMessage.EntityAddress)
        let actor2 = {{ actor with Position = bodyTransformOutMessage.Position - actor.Size * 0.5f } // TODO: see if this center-offsetting can be encapsulated withing the Physics module!
                              with Rotation = bodyTransformOutMessage.Rotation }
        set actor2 world (worldActor bodyTransformOutMessage.EntityAddress)
    | BodyCollisionMessage bodyCollisionMessage ->
        let collisionAddress = Lun.make "collision" :: bodyCollisionMessage.EntityAddress
        let collisionData = CollisionData (bodyCollisionMessage.Normal, bodyCollisionMessage.Speed, bodyCollisionMessage.EntityAddress2)
        let collisionMessage = { Handled = false; Data = collisionData }
        publish collisionAddress collisionMessage world

/// Handle physics integration messages.
let handleIntegrationMessages integrationMessages world =
    List.fold handleIntegrationMessage world integrationMessages

/// Integrate the world.
let integrate world =
    let integrationMessages = Nu.Physics.integrate world.PhysicsMessages world.Integrator
    let world2 = { world with PhysicsMessages = [] }
    handleIntegrationMessages integrationMessages world2

let createEmptyWorld sdlDeps extData =
    { GameModel = Game { Id = getNuId (); ScreenModels = Map.empty; OptSelectedScreenModelAddress = None }
      Camera = { EyePosition = Vector2.Zero; EyeSize = Vector2 (single sdlDeps.Config.ViewW, single sdlDeps.Config.ViewH) }
      Subscriptions = Map.empty
      MouseState = { MousePosition = Vector2.Zero; MouseLeftDown = false; MouseRightDown = false; MouseCenterDown = false }
      AudioPlayer = makeAudioPlayer ()
      Renderer = makeRenderer sdlDeps.RenderContext
      Integrator = makeIntegrator Gravity
      AssetMetadataMap = Map.empty
      AudioMessages = []
      RenderMessages = []
      PhysicsMessages = []
      Components = []
      ExtData = extData }

let run4 tryCreateWorld handleUpdate handleRender sdlConfig =
    runSdl
        (fun sdlDeps -> tryCreateWorld sdlDeps)
        (fun refEvent world ->
            let event = !refEvent
            match event.``type`` with
            | SDL.SDL_EventType.SDL_QUIT -> (false, world)
            | SDL.SDL_EventType.SDL_MOUSEMOTION ->
                let mousePosition = Vector2 (single event.button.x, single event.button.y)
                let world2 = set { world.MouseState with MousePosition = mousePosition } world worldMouseState
                let world3 =
                    if world2.MouseState.MouseLeftDown then publish MouseDragAddress { Handled = false; Data = MouseMoveData mousePosition } world2
                    else publish MouseMoveAddress { Handled = false; Data = MouseButtonData (mousePosition, MouseLeft) } world2
                (true, world3)
            | SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN ->
                if event.button.button = byte SDL.SDL_BUTTON_LEFT then
                    let messageData = MouseButtonData (world.MouseState.MousePosition, MouseLeft)
                    let world2 = set { world.MouseState with MouseLeftDown = true } world worldMouseState
                    let world3 = publish DownMouseLeftAddress { Handled = false; Data = messageData } world2
                    (true, world3)
                else (true, world)
            | SDL.SDL_EventType.SDL_MOUSEBUTTONUP ->
                let mouseState = world.MouseState
                if mouseState.MouseLeftDown && event.button.button = byte SDL.SDL_BUTTON_LEFT then
                    let messageData = MouseButtonData (world.MouseState.MousePosition, MouseLeft)
                    let world2 = set { world.MouseState with MouseLeftDown = false } world worldMouseState
                    let world3 = publish UpMouseLeftAddress { Handled = false; Data = messageData } world2
                    (true, world3)
                else (true, world)
            | _ -> (true, world))
        (fun world ->
            let world2 = integrate world
            let world3 = publish TickAddress { Handled = false; Data = NoData } world2
            handleUpdate world3)
        (fun world -> let world2 = render world in handleRender world2)
        (fun world -> play world)
        (fun world -> { world with Renderer = handleRenderExit world.Renderer })
        sdlConfig

let run tryCreateWorld handleUpdate sdlConfig =
    run4 tryCreateWorld handleUpdate id sdlConfig