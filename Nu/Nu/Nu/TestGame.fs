module Nu.TestGame
open System
open SDL2
open OpenTK
open TiledSharp
open Nu.Core
open Nu.Constants
open Nu.Sdl
open Nu.Audio
open Nu.Rendering
open Nu.Physics
open Nu.AssetMetadata
open Nu.Entity
open Nu.Group
open Nu.Screen
open Nu.Game
open Nu.Simulation

let TestScreenAddress = [Lun.make "screen"]
let TestGroupAddress = TestScreenAddress @ [Lun.make "group"]
let TestFeelerAddress = TestGroupAddress @ [Lun.make "feeler"]
let TestTextBoxAddress = TestGroupAddress @ [Lun.make "textBox"]
let TestToggleAddress = TestGroupAddress @ [Lun.make "toggle"]
let TestLabelAddress = TestGroupAddress @ [Lun.make "label"]
let TestButtonAddress = TestGroupAddress @ [Lun.make "button"]
let TestBlockAddress = TestGroupAddress @ [Lun.make "testBlock"]
let TestTileMapAddress = TestGroupAddress @ [Lun.make "tileMap"]
let TestFloorAddress = TestGroupAddress @ [Lun.make "floor"]
let TestAvatarAddress = TestGroupAddress @ [Lun.make "avatar"]
let ClickTestButtonAddress = Lun.make "click" :: TestButtonAddress

let createTestBlock assetMetadataMap =

    let testBlock =
        { PhysicsId = getPhysicsId ()
          Density = NormalDensity
          BodyType = Dynamic
          Sprite = { SpriteAssetName = Lun.make "Image3"; PackageName = Lun.make "Misc"; PackageFileName = "AssetGraph.xml" }}

    let testBlockActor =
        { Position = Vector2 (400.0f, 200.0f)
          Depth = 0.0f
          Size = getTextureSizeAsVector2 (Lun.make "Image3") (Lun.make "Misc") assetMetadataMap
          Rotation = 2.0f
          SubSubtype = Block testBlock }

    let testBlockActorEntity =
        { Id = getNuId ()
          Enabled = true
          Visible = true
          Subtype = Actor testBlockActor }

    (testBlockActorEntity, testBlockActor, testBlock)

let tryCreateTestWorld (sdlDeps : SdlDeps) =

    match tryGenerateAssetMetadataMap "AssetGraph.xml" with
    | Left errorMsg -> Left errorMsg
    | Right assetMetadataMap ->

        let game =
            { Id = getNuId ()
              Screens = Map.empty
              OptSelectedScreenAddress = None
              Subtype = () }
    
        let world =
            { Game = game
              Camera = { EyePosition = Vector2.Zero; EyeSize = Vector2 (single sdlDeps.Config.ViewW, single sdlDeps.Config.ViewH) }
              Subscriptions = Map.empty
              MouseState = { MousePosition = Vector2.Zero; MouseLeftDown = false; MouseRightDown = false; MouseCenterDown = false }
              AudioPlayer = makeAudioPlayer ()
              Renderer = makeRenderer sdlDeps.RenderContext
              Integrator = makeIntegrator Gravity
              AssetMetadataMap = assetMetadataMap
              AudioMessages = []
              RenderMessages = []
              PhysicsMessages = []
              Components = [] }

        let screen =
            { Id = getNuId ()
              Groups = Map.empty
              Subtype = () }

        let group =
            { Id = getNuId ()
              Entities = Map.empty
              Subtype = () }
          
        let feeler =
            { IsTouched = false }

        let feelerGui =
            { Gui.Position = Vector2.Zero
              Depth = -0.1f
              Size = Vector2 (single sdlDeps.Config.ViewW, single sdlDeps.Config.ViewH)
              SubSubtype = Feeler feeler }

        let feelerEntity =
            { Id = getNuId ()
              Enabled = true
              Visible = true
              Subtype = Gui feelerGui }
          
        let textBox =
            { BoxSprite = { SpriteAssetName = Lun.make "Image3"; PackageName = Lun.make "Misc"; PackageFileName = "AssetGraph.xml" }
              Text = "Hi!"
              TextFont = { FontAssetName = Lun.make "Font"; PackageName = Lun.make "Misc"; PackageFileName = "AssetGraph.xml" }
              TextOffset = Vector2 4.0f
              TextColor = Vector4.One }

        let textBoxGui =
            { Gui.Position = Vector2 (120.0f, 50.0f)
              Depth = 0.1f
              Size = getTextureSizeAsVector2 (Lun.make "Image3") (Lun.make "Misc") assetMetadataMap
              SubSubtype = TextBox textBox }

        let textBoxEntity =
            { Id = getNuId ()
              Enabled = true
              Visible = true
              Subtype = Gui textBoxGui }
          
        let toggle =
            { IsOn = false
              IsPressed = false
              OffSprite = { SpriteAssetName = Lun.make "Image3"; PackageName = Lun.make "Misc"; PackageFileName = "AssetGraph.xml" }
              OnSprite = { SpriteAssetName = Lun.make "Image6"; PackageName = Lun.make "Misc"; PackageFileName = "AssetGraph.xml" }
              ToggleSound = { SoundAssetName = Lun.make "Sound"; PackageName = Lun.make "Misc"; PackageFileName = "AssetGraph.xml" }}

        let toggleGui =
            { Gui.Position = Vector2 (720.0f, 50.0f)
              Depth = 0.1f
              Size = getTextureSizeAsVector2 (Lun.make "Image3") (Lun.make "Misc") assetMetadataMap
              SubSubtype = Toggle toggle }

        let toggleEntity =
            { Id = getNuId ()
              Enabled = true
              Visible = true
              Subtype = Gui toggleGui }
          
        let label =
            { LabelSprite = { SpriteAssetName = Lun.make "Image5"; PackageName = Lun.make "Misc"; PackageFileName = "AssetGraph.xml" }}

        let labelGui =
            { Gui.Position = Vector2.Zero
              Depth = -0.1f
              Size = getTextureSizeAsVector2 (Lun.make "Image5") (Lun.make "Misc") assetMetadataMap
              SubSubtype = Label label }

        let labelEntity =
            { Id = getNuId ()
              Enabled = true
              Visible = true
              Subtype = Gui labelGui }
          
        let button =
            { IsDown = false
              UpSprite = { SpriteAssetName = Lun.make "Image"; PackageName = Lun.make "Misc"; PackageFileName = "AssetGraph.xml" }
              DownSprite = { SpriteAssetName = Lun.make "Image2"; PackageName = Lun.make "Misc"; PackageFileName = "AssetGraph.xml" }
              ClickSound = { SoundAssetName = Lun.make "Sound"; PackageName = Lun.make "Misc"; PackageFileName = "AssetGraph.xml" }}

        let buttonGui =
            { Gui.Position = Vector2 (310.0f, 20.0f)
              Depth = 0.1f
              Size = getTextureSizeAsVector2 (Lun.make "Image") (Lun.make "Misc") assetMetadataMap
              SubSubtype = Button button }

        let buttonEntity =
            { Id = getNuId ()
              Enabled = true
              Visible = true
              Subtype = Gui buttonGui }
    
        let tmxMap =
            TmxMap "TileMap.tmx"

        let tileMap =
            { PhysicsIds = [getPhysicsId ()]
              Density = NormalDensity
              TileMapAsset = { TileMapAssetName = Lun.make "TileMap"; PackageName = Lun.make "Misc"; PackageFileName = "AssetGraph.xml" }
              TmxMap = tmxMap
              TileMapMetadata = getTileMapMetadata (Lun.make "TileMap") (Lun.make "Misc") assetMetadataMap }

        let tileMapActor =
            { Position = Vector2.Zero
              Depth = -0.1f
              Size = Vector2 (single (tmxMap.Width * tmxMap.TileWidth), single (tmxMap.Height * tmxMap.TileHeight))
              Rotation = 0.0f
              SubSubtype = TileMap tileMap }

        let tileMapEntity =
            { Id = getNuId ()
              Enabled = true
              Visible = true
              Subtype = Actor tileMapActor }
    
        let floor =
            { PhysicsId = getPhysicsId ()
              Density = NormalDensity
              BodyType = Static
              Sprite = { SpriteAssetName = Lun.make "Image4"; PackageName = Lun.make "Misc"; PackageFileName = "AssetGraph.xml" }}

        let floorActor =
            { Position = Vector2 (120.0f, 520.0f)
              Depth = 0.0f
              Size = getTextureSizeAsVector2 (Lun.make "Image4") (Lun.make "Misc") assetMetadataMap
              Rotation = 0.0f
              SubSubtype = Block floor }

        let floorEntity =
            { Id = getNuId ()
              Enabled = true
              Visible = true
              Subtype = Actor floorActor }
    
        let avatar =
            { PhysicsId = getPhysicsId ()
              Density = NormalDensity
              Sprite = { SpriteAssetName = Lun.make "Image7"; PackageName = Lun.make "Misc"; PackageFileName = "AssetGraph.xml" }}

        let avatarActor =
            { Position = Vector2 (300.0f, 300.0f)
              Depth = 0.0f
              Size = getTextureSizeAsVector2 (Lun.make "Image7") (Lun.make "Misc") assetMetadataMap
              Rotation = 0.0f
              SubSubtype = Avatar avatar }

        let avatarEntity =
            { Id = getNuId ()
              Enabled = true
              Visible = true
              Subtype = Actor avatarActor }

        let moveAvatar address _ message world =
            let (_, _, feeler) = get world (World.entityGuiFeeler TestFeelerAddress)
            if feeler.IsTouched then
                let (_, actor, avatar) = get world (World.entityActorAvatar TestAvatarAddress)
                let camera = world.Camera
                let inverseView = camera.EyePosition - camera.EyeSize * 0.5f
                let mousePositionWorld = world.MouseState.MousePosition + inverseView
                let actorCenter = actor.Position + actor.Size * 0.5f
                let impulseVector = (mousePositionWorld - actorCenter) * 5.0f
                let applyImpulseMessage = { PhysicsId = avatar.PhysicsId; Impulse = impulseVector }
                let world_ = { world with PhysicsMessages = ApplyImpulseMessage applyImpulseMessage :: world.PhysicsMessages }
                (message, world_)
            else (message, world)

        let addBoxes _ _ message world =
            let world_ =
                List.fold
                    (fun world_ _ ->
                        let entityActorBlock = createTestBlock assetMetadataMap
                        addEntityActorBlock entityActorBlock (TestGroupAddress @ [Lun.makeN (getNuId ())]) world_)
                    world
                    [0..7]
            (handle message, world_)

        let hintRenderingPackageUse = HintRenderingPackageUse { FileName = "AssetGraph.xml"; PackageName = "Misc"; HRPU = () }
        let playSong = PlaySong { Song = { SongAssetName = Lun.make "Song"; PackageName = Lun.make "Misc"; PackageFileName = "AssetGraph.xml" }; FadeOutCurrentSong = true }

        // scripting convention
        let w_ = world
        let w_ = subscribe TickAddress [] moveAvatar w_
        let w_ = subscribe ClickTestButtonAddress [] addBoxes w_
        let w_ = addScreen screen TestScreenAddress w_
        let w_ = set (Some TestScreenAddress) w_ World.optActiveScreenAddress
        let w_ = addGroup group TestGroupAddress w_
        let w_ = addEntityGuiFeeler (feelerEntity, feelerGui, feeler) TestFeelerAddress w_
        let w_ = addEntityGuiTextBox (textBoxEntity, textBoxGui, textBox) TestTextBoxAddress w_
        let w_ = addEntityGuiToggle (toggleEntity, toggleGui, toggle) TestToggleAddress w_
        let w_ = addEntityGuiLabel (labelEntity, labelGui, label) TestLabelAddress w_
        let w_ = addEntityGuiButton (buttonEntity, buttonGui, button) TestButtonAddress w_
        let w_ = addEntityActorTileMap (tileMapEntity, tileMapActor, tileMap) TestTileMapAddress w_
        let w_ = addEntityActorBlock (floorEntity, floorActor, floor) TestFloorAddress w_
        let w_ = addEntityActorAvatar (avatarEntity, avatarActor, avatar) TestAvatarAddress w_
        let w_ = { w_ with PhysicsMessages = SetGravityMessage Vector2.Zero :: w_.PhysicsMessages }
        let w_ = { w_ with RenderMessages = hintRenderingPackageUse :: w_.RenderMessages }
        let w_ = { w_ with AudioMessages = FadeOutSong :: playSong :: w_.AudioMessages }
        Right w_

let testHandleUpdate world =
    let (_, actor, _) = get world (World.entityActorAvatar TestAvatarAddress)
    let camera = { world.Camera with EyePosition = actor.Position + actor.Size * 0.5f }
    (true, { world with Camera = camera })