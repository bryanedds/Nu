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
      Components : IWorldComponent list }
    
    static member gameModel =
        { Get = fun this -> this.GameModel
          Set = fun gameModel this -> { this with GameModel = gameModel }}

    static member game =
        { Get = fun this -> get this.GameModel GameModel.game
          Set = fun game this -> { this with GameModel = set game this.GameModel GameModel.game }}
          
    static member camera =
        { Get = fun this -> this.Camera
          Set = fun camera this -> { this with Camera = camera }}

    static member mouseState =
        { Get = fun this -> this.MouseState
          Set = fun mouseState this -> { this with MouseState = mouseState }}

    static member optActiveScreenAddress = World.gameModel >>| GameModel.optActiveScreenAddress
    
    static member screenModel (address : Address) = World.gameModel >>| GameModel.screenModel address
    static member optScreenModel (address : Address) = World.gameModel >>| GameModel.optScreenModel address
    
    static member screen (address : Address) = World.gameModel >>| GameModel.screen address
    static member optScreen (address : Address) = World.gameModel >>| GameModel.optScreen address
    
    static member groupModel (address : Address) = World.gameModel >>| GameModel.groupModel address
    static member optGroupModel (address : Address) = World.gameModel >>| GameModel.optGroupModel address
    
    static member group (address : Address) = World.gameModel >>| GameModel.group address
    static member optGroup (address : Address) = World.gameModel >>| GameModel.optGroup address
    
    static member entityModel (address : Address) = World.gameModel >>| GameModel.entityModel address
    static member optEntityModel (address : Address) = World.gameModel >>| GameModel.optEntityModel address
    
    static member entity (address : Address) = World.gameModel >>| GameModel.entity address
    static member optEntity (address : Address) = World.gameModel >>| GameModel.optEntity address
    
    static member gui (address : Address) = World.gameModel >>| GameModel.gui address
    static member optGui (address : Address) = World.gameModel >>| GameModel.optGui address
    
    static member button (address : Address) = World.gameModel >>| GameModel.button address
    static member optButton (address : Address) = World.gameModel >>| GameModel.optButton address
    
    static member label (address : Address) = World.gameModel >>| GameModel.label address
    static member optLabel (address : Address) = World.gameModel >>| GameModel.optLabel address
    
    static member textBox (address : Address) = World.gameModel >>| GameModel.textBox address
    static member optTextBox (address : Address) = World.gameModel >>| GameModel.optTextBox address
    
    static member toggle (address : Address) = World.gameModel >>| GameModel.toggle address
    static member optToggle (address : Address) = World.gameModel >>| GameModel.optToggle address
    
    static member feeler (address : Address) = World.gameModel >>| GameModel.feeler address
    static member optFeeler (address : Address) = World.gameModel >>| GameModel.optFeeler address
    
    static member actor (address : Address) = World.gameModel >>| GameModel.actor address
    static member optActor (address : Address) = World.gameModel >>| GameModel.optActor address
    
    static member block (address : Address) = World.gameModel >>| GameModel.block address
    static member optBlock (address : Address) = World.gameModel >>| GameModel.optBlock address
    
    static member avatar (address : Address) = World.gameModel >>| GameModel.avatar address
    static member optAvatar (address : Address) = World.gameModel >>| GameModel.optAvatar address
    
    static member tileMap (address : Address) = World.gameModel >>| GameModel.tileMap address
    static member optTileMap (address : Address) = World.gameModel >>| GameModel.optTileMap address

/// Enables components that open the world for extension.
and IWorldComponent =
    interface
        abstract member GetAudioDescriptors : World -> AudioDescriptor list
        abstract member GetRenderDescriptors : World -> RenderDescriptor list
        // TODO: abstract member GetRenderMessages : World -> RenderMessage rQueue
        // TODO: abstract member GetPhysicsMessages : World -> PhysicsMessage rQueue
        // TODO: abstract member HandleIntegrationMessages : IntegrationMessage rQueue -> World -> World
        end

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
    let optScreenAddress = (get world World.game).OptSelectedScreenAddress
    match (address, optScreenAddress) with
    | ([], _) -> true
    | (_, None) -> false
    | (_, Some screenAddress) ->
        if List.head screenAddress = List.head address then true // TODO: test also to see if group and entity are selected
        else false

/// Publish a message to the given address.
let publish address message world =
    let optSubList = Map.tryFind address world.Subscriptions
    match optSubList with
    | None -> world
    | Some subList ->
        let (_, world_) =
            List.foldWhile
                (fun (message_, world_) (subscriber, (Subscription subscription)) ->
                    if message_.Handled then None
                    elif isAddressSelected subscriber world_ then Some (subscription address subscriber message_ world_)
                    else Some (message_, world_))
                (message, world)
                subList
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
        let subList2 = List.remove (fun (address, _) -> address = subscriber) subList // TODO: consider using List.removeFirst
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

let handleButtonEventDownMouseLeft address subscriber message world =
    match message.Data with
    | MouseButtonData (mousePosition, _) ->
        let button = get world (World.button subscriber)
        if button.Gui.Entity.Enabled && button.Gui.Entity.Visible then
            if isInBox3 mousePosition button.Gui.Position button.Gui.Size then
                let button_ = { button with IsDown = true }
                let world_ = set button_ world (World.button subscriber)
                let world_ = publish (Lun.make "down" :: subscriber) { Handled = false; Data = NoData } world_
                (handle message, world_)
            else (message, world)
        else (message, world)
    | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")
    
let handleButtonEventUpMouseLeft address subscriber message world =
    match message.Data with
    | MouseButtonData (mousePosition, _) ->
        let button = get world (World.button subscriber)
        if button.Gui.Entity.Enabled && button.Gui.Entity.Visible then
            let world_ =
                let button_ = { button with IsDown = false }
                let world_ = set button_ world (World.button subscriber)
                publish (Lun.make "up" :: subscriber) { Handled = false; Data = NoData } world_
            if isInBox3 mousePosition button.Gui.Position button.Gui.Size && button.IsDown then
                let world_ = publish (Lun.make "click" :: subscriber) { Handled = false; Data = NoData } world_
                let sound = PlaySound { Volume = 1.0f; Sound = button.ClickSound }
                let world_ = { world_ with AudioMessages = sound :: world_.AudioMessages }
                (handle message, world_)
            else (message, world_)
        else (message, world)
    | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")

let registerButton address world =
    let optOldButton = get world (World.optButton address)
    let world_ = if optOldButton.IsSome then unregisterButton address world else world
    let world_ = subscribe DownMouseLeftAddress address handleButtonEventDownMouseLeft world_
    subscribe UpMouseLeftAddress address handleButtonEventUpMouseLeft world_

let addButton button address world =
    let world2 = registerButton address world
    set (Button button) world2 (World.entityModel address)

let removeButton address world =
    let world2 = set None world (World.optEntityModel address)
    unregisterButton address world2

let addLabel label address world =
    set (Label label) world (World.entityModel address)

let removeLabel address world =
    set None world (World.optEntityModel address)

let addTextBox textBox address world =
    set (TextBox textBox) world (World.entityModel address)

let removeTextBox address world =
    set None world (World.optEntityModel address)

let unregisterToggle address world =
    world |>
        unsubscribe DownMouseLeftAddress address |>
        unsubscribe UpMouseLeftAddress address

let handleToggleEventDownMouseLeft address subscriber message world =
    match message.Data with
    | MouseButtonData (mousePosition, _) ->
        let toggle = get world (World.toggle subscriber)
        if toggle.Gui.Entity.Enabled && toggle.Gui.Entity.Visible then
            if isInBox3 mousePosition toggle.Gui.Position toggle.Gui.Size then
                let toggle_ = { toggle with IsPressed = true }
                let world_ = set toggle_ world (World.toggle subscriber)
                (handle message, world_)
            else (message, world)
        else (message, world)
    | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")
    
let handleToggleEventUpMouseLeft address subscriber message world =
    match message.Data with
    | MouseButtonData (mousePosition, _) ->
        let toggle = get world (World.toggle subscriber)
        if toggle.Gui.Entity.Enabled && toggle.Gui.Entity.Visible && toggle.IsPressed then
            let toggle_ = { toggle with IsPressed = false }
            if isInBox3 mousePosition toggle.Gui.Position toggle.Gui.Size then
                let toggle_ = { toggle_ with IsOn = not toggle_.IsOn }
                let world_ = set toggle_ world (World.toggle subscriber)
                let messageType = if toggle.IsOn then "on" else "off"
                let world_ = publish (Lun.make messageType :: subscriber) { Handled = false; Data = NoData } world_
                let sound = PlaySound { Volume = 1.0f; Sound = toggle.ToggleSound }
                let world_ = { world_ with AudioMessages = sound :: world_.AudioMessages }
                (handle message, world_)
            else
                let world_ = set toggle_ world (World.toggle subscriber)
                (message, world_)
        else (message, world)
    | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")

let registerToggle address world =
    let optOldToggle = get world (World.optToggle address)
    let world_ = if optOldToggle.IsSome then unregisterToggle address world else world
    let world_ = subscribe DownMouseLeftAddress address handleToggleEventDownMouseLeft world_
    subscribe UpMouseLeftAddress address handleToggleEventUpMouseLeft world_

let addToggle toggle address world =
    let world2 = registerToggle address world
    set (Toggle toggle) world2 (World.entityModel address)

let removeToggle address world =
    set None world (World.optEntityModel address) // TODO: unregister events

let unregisterFeeler address world =
    world |>
        unsubscribe UpMouseLeftAddress address |>
        unsubscribe DownMouseLeftAddress address

let handleFeelerEventDownMouseLeft address subscriber message world =
    match message.Data with
    | MouseButtonData (mousePosition, _) as mouseButtonData ->
        let feeler = get world (World.feeler subscriber)
        if feeler.Gui.Entity.Enabled && feeler.Gui.Entity.Visible then
            if isInBox3 mousePosition feeler.Gui.Position feeler.Gui.Size then
                let feeler_ = { feeler with IsTouched = true }
                let world_ = set feeler_ world (World.feeler subscriber)
                let world_ = publish (Lun.make "touch" :: subscriber) { Handled = false; Data = mouseButtonData } world_
                (handle message, world_)
            else (message, world)
        else (message, world)
    | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")
    
let handleFeelerEventUpMouseLeft address subscriber message world =
    match message.Data with
    | MouseButtonData _ ->
        let feeler = get world (World.feeler subscriber)
        if feeler.Gui.Entity.Enabled && feeler.Gui.Entity.Visible then
            let feeler_ = { feeler with IsTouched = false }
            let world_ = set feeler_ world (World.feeler subscriber)
            let world_ = publish (Lun.make "release" :: subscriber) { Handled = false; Data = NoData } world_
            (handle message, world_)
        else (message, world)
    | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")
    
let registerFeeler address world =
    let optOldFeeler = get world (World.optFeeler address)
    let world_ = if optOldFeeler.IsSome then unregisterFeeler address world else world
    let world_ = subscribe DownMouseLeftAddress address handleFeelerEventDownMouseLeft world_
    subscribe UpMouseLeftAddress address handleFeelerEventUpMouseLeft world_

let addFeeler feeler address world =
    let world2 = registerFeeler address world
    set (Feeler feeler) world2 (World.entityModel address)

let removeFeeler address world =
    set None world (World.optEntityModel address) // TODO: unregister events

let unregisterBlock (block : Block) address world =
    let bodyDestroyMessage = BodyDestroyMessage { PhysicsId = block.PhysicsId }
    { world with PhysicsMessages = bodyDestroyMessage :: world.PhysicsMessages }

let registerBlock (block : Block) address world =
    let optOldBlock = get world (World.optBlock address)
    let world2 =
        match optOldBlock with
        | None -> world
        | Some oldBlock -> unregisterBlock oldBlock address world
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
    { world2 with PhysicsMessages = bodyCreateMessage :: world2.PhysicsMessages }

let addBlock block address world =
    let world2 = registerBlock block address world
    set (Block block) world2 (World.entityModel address)

let removeBlock block address world =
    let world2 = set None world (World.optEntityModel address)
    unregisterBlock block address world2

let unregisterAvatar avatar address world =
    let bodyDestroyMessage = BodyDestroyMessage { PhysicsId = avatar.PhysicsId }
    { world with PhysicsMessages = bodyDestroyMessage :: world.PhysicsMessages }

let registerAvatar avatar address world =
    let optOldAvatar = get world (World.optAvatar address)
    let world2 =
        match optOldAvatar with
        | None -> world
        | Some oldAvatar -> unregisterAvatar oldAvatar address world
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
    { world2 with PhysicsMessages = bodyCreateMessage :: world2.PhysicsMessages }

let addAvatar avatar address world =
    let world2 = registerAvatar avatar address world
    set (Avatar avatar) world2 (World.entityModel address)

let removeAvatar avatar address world =
    let world2 = set None world (World.optEntityModel address)
    unregisterAvatar avatar address world2

let unregisterTileMap tileMap address world =
    world

let registerTileMap tileMap address world =
    let optOldTileMap = get world (World.optTileMap address)
    let world2 =
        match optOldTileMap with
        | None -> world
        | Some oldTileMap -> unregisterTileMap oldTileMap address world
    (*let bodyCreateMessage =
        BodyCreateMessage
            { EntityAddress = address
              Shape = BoxShape { Center = Vector2.Zero; Extent = actor.Size * 0.5f }
              Position = actor.Position + actor.Size * 0.5f
              Rotation = actor.Rotation
              Density = tileMap.Density
              BodyType = BodyType.Static }
    { world2 with PhysicsMessages = bodyCreateMessage :: world2.PhysicsMessages }*)
    world2

let addTileMap tileMap address world =
    let world2 = registerTileMap tileMap address world
    set (TileMap tileMap) world2 (World.entityModel address)

let removeTileMap tileMap address world =
    let world2 = set None world (World.optEntityModel address)
    unregisterTileMap tileMap address world2

let addEntityModel entityModel address world = // TODO: reorder first and second args
    match entityModel with
    | Button button -> addButton button address world
    | Label label -> addLabel label address world
    | TextBox textBox -> addTextBox textBox address world
    | Toggle toggle -> addToggle toggle address world
    | Feeler feeler -> addFeeler feeler address world
    | Block block -> addBlock block address world
    | Avatar avatar -> addAvatar avatar address world
    | TileMap tileMap -> addTileMap tileMap address world

let addEntityModelsToGroup entityModels address world =
    let group = get world (World.group address)
    List.fold
        (fun world_ entityModel ->
            let entity = get entityModel EntityModel.entity
            addEntityModel entityModel (address @ [Lun.make entity.Name]) world_)
        world
        entityModels

let removeEntityModel address world =
    let entityModel = get world <| World.entityModel address
    match entityModel with
    | Button button -> removeButton address world
    | Label label -> removeLabel address world
    | TextBox textBox -> removeTextBox address world
    | Toggle toggle -> removeToggle address world
    | Feeler feeler -> removeFeeler address world
    | Block block -> removeBlock block address world
    | Avatar avatar -> removeAvatar avatar address world
    | TileMap tileMap -> removeTileMap tileMap address world

let removeEntityModelsFromGroup address world =
    let group = get world (World.group address)
    Seq.fold
        (fun world_ entityModelAddress -> removeEntityModel (address @ [entityModelAddress]) world_)
        world
        (Map.toKeySeq group.EntityModels)

let addGroup group address world =
    traceIf (not <| Map.isEmpty group.EntityModels) "Adding populated groups to the world is not supported."
    set (Group group) world (World.groupModel address)

let removeGroup address world =
    let world2 = removeEntityModelsFromGroup address world
    set None world2 (World.optGroupModel address)

// TODO: see if there's a nicer place to put this stuff
let TestScreenAddress = [Lun.make "testScreen"]
let TestGroupAddress = TestScreenAddress @ [Lun.make "testGroup"]
let TestFeelerAddress = TestGroupAddress @ [Lun.make "testFeeler"]
let TestTextBoxAddress = TestGroupAddress @ [Lun.make "testTextBox"]
let TestToggleAddress = TestGroupAddress @ [Lun.make "testToggle"]
let TestLabelAddress = TestGroupAddress @ [Lun.make "testLabel"]
let TestButtonAddress = TestGroupAddress @ [Lun.make "testButton"]
let TestBlockAddress = TestGroupAddress @ [Lun.make "testTestBlock"]
let TestTileMapAddress = TestGroupAddress @ [Lun.make "testTileMap"]
let TestFloorAddress = TestGroupAddress @ [Lun.make "testFloor"]
let TestAvatarAddress = TestGroupAddress @ [Lun.make "testAvatar"]
let ClickTestButtonAddress = Lun.make "click" :: TestButtonAddress

let addGroupModel groupModel address world =

    match groupModel with
    | Group group -> addGroup group address world
    | TestGroup testGroup ->

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
            let actor = get world (World.actor TestAvatarAddress)
            let camera = { world.Camera with EyePosition = actor.Position + actor.Size * 0.5f }
            (message, { world with Camera = camera })

        let moveAvatar address _ message world =
            let feeler = get world (World.feeler TestFeelerAddress)
            if feeler.IsTouched then
                let avatar = get world (World.avatar TestAvatarAddress)
                let camera = world.Camera
                let inverseView = camera.EyePosition - camera.EyeSize * 0.5f
                let mousePositionWorld = world.MouseState.MousePosition + inverseView
                let actorCenter = avatar.Actor.Position + avatar.Actor.Size * 0.5f
                let impulseVector = (mousePositionWorld - actorCenter) * 5.0f
                let applyImpulseMessage = { PhysicsId = avatar.PhysicsId; Impulse = impulseVector }
                let world_ = { world with PhysicsMessages = ApplyImpulseMessage applyImpulseMessage :: world.PhysicsMessages }
                (message, world_)
            else (message, world)

        let addBoxes _ _ message world =
            let world_ =
                List.fold
                    (fun world_ _ ->
                        let block = createTestBlock assetMetadataMap
                        addBlock block (TestGroupAddress @ [Lun.makeN (getNuId ())]) world_)
                    world
                    [0..7]
            (handle message, world_)

        let hintRenderingPackageUse = HintRenderingPackageUse { FileName = "AssetGraph.xml"; PackageName = "Default"; HRPU = () }
        let playSong = PlaySong { Song = { SongAssetName = Lun.make "Song"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }; FadeOutCurrentSong = true }

        // scripting convention
        let w_ = world
        let w_ = subscribe TickAddress [] adjustCamera w_
        let w_ = subscribe TickAddress [] moveAvatar w_
        let w_ = subscribe ClickTestButtonAddress [] addBoxes w_
        let w_ = set (Some TestScreenAddress) w_ World.optActiveScreenAddress
        let w_ = { w_ with PhysicsMessages = SetGravityMessage Vector2.Zero :: w_.PhysicsMessages }
        let w_ = { w_ with RenderMessages = hintRenderingPackageUse :: w_.RenderMessages }
        let w_ = { w_ with AudioMessages = FadeOutSong :: playSong :: w_.AudioMessages }
        addGroup testGroup.Group address w_

let removeGroupModel address world =
    let groupModel = get world <| World.groupModel address
    match groupModel with
    | Group group -> removeGroup address world
    | TestGroup testGroup ->
        let w_ = world
        let w_ = unsubscribe TickAddress [] w_
        let w_ = unsubscribe TickAddress [] w_
        let w_ = unsubscribe ClickTestButtonAddress [] w_
        let w_ = set None w_ World.optActiveScreenAddress
        removeGroup address w_

let addScreen screen address world =
    set (Screen screen) world (World.screenModel address)

let removeScreen address world =
    set None world (World.optScreenModel address)

let getComponentAudioDescriptors world : AudioDescriptor rQueue =
    let descriptorLists = List.fold (fun descs (comp : IWorldComponent) -> comp.GetAudioDescriptors world :: descs) [] world.Components // TODO: get audio descriptors
    List.collect (fun descs -> descs) descriptorLists

let getAudioDescriptors world : AudioDescriptor rQueue =
    let componentDescriptors = getComponentAudioDescriptors world
    let worldDescriptors = [] // TODO: get audio descriptors when there are some
    worldDescriptors @ componentDescriptors // NOTE: pretty inefficient

/// Play the world's audio.
let play world =
    let audioMessages = world.AudioMessages
    let audioDescriptors = getAudioDescriptors world
    let audioPlayer = world.AudioPlayer
    let world_ = { world with AudioMessages = [] }
    let world_ = { world_ with AudioPlayer = Nu.Audio.play audioMessages audioDescriptors audioPlayer }
    world_

let getComponentRenderDescriptors world : RenderDescriptor rQueue =
    let descriptorLists = List.fold (fun descs (comp : IWorldComponent) -> comp.GetRenderDescriptors world :: descs) [] world.Components // TODO: get render descriptors
    List.collect (fun descs -> descs) descriptorLists

let getEntityRenderDescriptors actorView entity =
    match entity with
    | Button button ->
        let (_, gui, entity) = Button.sep button
        if not entity.Visible then []
        else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = gui.Position; Size = gui.Size; Rotation = 0.0f; Sprite = if button.IsDown then button.DownSprite else button.UpSprite }; Depth = gui.Depth })]
    | Label label ->
        let (_, gui, entity) = Label.sep label
        if not label.Gui.Entity.Visible then []
        else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = gui.Position; Size = gui.Size; Rotation = 0.0f; Sprite = label.LabelSprite }; Depth = gui.Depth })]
    | TextBox textBox ->
        let (_, gui, entity) = TextBox.sep textBox
        if not entity.Visible then []
        else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = gui.Position; Size = gui.Size; Rotation = 0.0f; Sprite = textBox.BoxSprite }; Depth = gui.Depth })
              LayerableDescriptor (LayeredTextDescriptor { Descriptor = { Text = textBox.Text; Position = gui.Position + textBox.TextOffset; Size = gui.Size - textBox.TextOffset; Font = textBox.TextFont; Color = textBox.TextColor }; Depth = gui.Depth })]
    | Toggle toggle ->
        let (_, gui, entity) = Toggle.sep toggle
        if not entity.Visible then []
        else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = gui.Position; Size = gui.Size; Rotation = 0.0f; Sprite = if toggle.IsOn || toggle.IsPressed then toggle.OnSprite else toggle.OffSprite }; Depth = gui.Depth })]
    | Feeler _ ->
        []
    | Block block ->
        let (_, actor, entity) = Block.sep block
        if not entity.Visible then []
        else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = actor.Position - actorView; Size = actor.Size; Rotation = actor.Rotation; Sprite = block.Sprite }; Depth = actor.Depth })]
    | Avatar avatar ->
        let (_, actor, entity) = Avatar.sep avatar
        if not entity.Visible then []
        else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = actor.Position - actorView; Size = actor.Size; Rotation = actor.Rotation; Sprite = avatar.Sprite }; Depth = actor.Depth })]
    | TileMap tileMap ->
        let (_, actor, entity) = TileMap.sep tileMap
        if not entity.Visible then []
        else
            let map = tileMap.TmxMap
            let mapWidth = map.Width
            let tileWidth = map.TileWidth
            let tileHeight = map.TileHeight
            let tileSet = map.Tilesets.[0] // MAGIC_VALUE: I have no idea how to tell which tile set each tile is from...
            let tileSetSprite = tileMap.TileMapMetadata.[0] // MAGIC_VALUE: for same reason as above
            let layers = List.ofSeq map.Layers
            let tileLayers =
                List.map
                    (fun (layer : TmxLayer) ->
                        let tiles = List.ofSeq layer.Tiles
                        List.mapi
                            (fun n tile ->
                                let (i, j) = (n % mapWidth, n / mapWidth)
                                let position = Vector2 (actor.Position.X + single (tileWidth * i), actor.Position.Y + single (tileHeight * j)) - actorView
                                let size = Vector2 (single tileWidth, single tileHeight)
                                let gid = layer.Tiles.[n].Gid - tileSet.FirstGid
                                let gidPosition = gid * tileWidth
                                let optTileSetWidth = tileSet.Image.Width
                                let tileSetWidth = optTileSetWidth.Value
                                let tileSetPosition = Vector2 (single <| gidPosition % tileSetWidth, single <| gidPosition / tileSetWidth * tileHeight)
                                LayerableDescriptor (LayeredTileDescriptor { Descriptor = { TileSetPosition = tileSetPosition; Position = position; Size = size; Rotation = actor.Rotation; TileSetSprite = tileSetSprite }; Depth = actor.Depth }))
                            tiles)
                    layers
            List.concat tileLayers

let getGroupModelRenderDescriptors camera groupModel =
    let group = get groupModel GroupModel.group
    let actorView = camera.EyePosition - camera.EyeSize * 0.5f
    let entities = Map.toValueSeq group.EntityModels
    Seq.map (getEntityRenderDescriptors actorView) entities

let getWorldRenderDescriptors world =
    match get world World.optActiveScreenAddress with
    | None -> []
    | Some activeScreenAddress ->
        let activeScreen = get world (World.screen activeScreenAddress)
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
    | BodyTransformMessage bodyTransformMessage ->
        let actor = get world (World.actor bodyTransformMessage.EntityAddress)
        let actor2 = {{ actor with Position = bodyTransformMessage.Position - actor.Size * 0.5f }
                              with Rotation = bodyTransformMessage.Rotation }
        set actor2 world (World.actor bodyTransformMessage.EntityAddress)
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

let createEmptyWorld sdlDeps =
    { GameModel = Game { Id = getNuId (); ScreenModels = Map.empty; OptSelectedScreenAddress = None }
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
      Components = [] }

let run4 tryCreateWorld handleUpdate handleRender sdlConfig =
    runSdl
        (fun sdlDeps -> tryCreateWorld sdlDeps)
        (fun refEvent world ->
            let event = refEvent.Value
            match event.``type`` with
            | SDL.SDL_EventType.SDL_QUIT -> (false, world)
            | SDL.SDL_EventType.SDL_MOUSEMOTION ->
                let mousePosition = Vector2 (single event.button.x, single event.button.y)
                let world2 = set { world.MouseState with MousePosition = mousePosition } world World.mouseState
                let world3 =
                    if world2.MouseState.MouseLeftDown then publish MouseDragAddress { Handled = false; Data = MouseMoveData mousePosition } world2
                    else publish MouseMoveAddress { Handled = false; Data = MouseButtonData (mousePosition, MouseLeft) } world2
                (true, world3)
            | SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN ->
                if event.button.button = byte SDL.SDL_BUTTON_LEFT then
                    let messageData = MouseButtonData (world.MouseState.MousePosition, MouseLeft)
                    let world2 = set { world.MouseState with MouseLeftDown = true } world World.mouseState
                    let world3 = publish DownMouseLeftAddress { Handled = false; Data = messageData } world2
                    (true, world3)
                else (true, world)
            | SDL.SDL_EventType.SDL_MOUSEBUTTONUP ->
                let mouseState = world.MouseState
                if mouseState.MouseLeftDown && event.button.button = byte SDL.SDL_BUTTON_LEFT then
                    let messageData = MouseButtonData (world.MouseState.MousePosition, MouseLeft)
                    let world2 = set { world.MouseState with MouseLeftDown = false } world World.mouseState
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