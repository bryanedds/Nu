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
          Sprite = { SpriteAssetName = Lun.make "Image3"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
          Actor =
          { Position = Vector2 (400.0f, 200.0f)
            Depth = 0.0f
            Size = getTextureSizeAsVector2 (Lun.make "Image3") (Lun.make "Default") assetMetadataMap
            Rotation = 2.0f
            Entity =
            { Id = getNuId ()
              Enabled = true
              Visible = true }}}
    testBlock

let tryCreateTestWorld (sdlDeps : SdlDeps) =

    match tryGenerateAssetMetadataMap "AssetGraph.xml" with
    | Left errorMsg -> Left errorMsg
    | Right assetMetadataMap ->

        let game =
            { Id = getNuId ()
              ScreenModels = Map.empty
              OptSelectedScreenAddress = None }
    
        let world =
            { GameModel = Game game
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
              GroupModels = Map.empty }

        let group =
            { Id = getNuId ()
              EntityModels = Map.empty }
          
        let feeler =
            { IsTouched = false
              Gui =
              { Gui.Position = Vector2.Zero
                Depth = -0.1f
                Size = Vector2 (single sdlDeps.Config.ViewW, single sdlDeps.Config.ViewH)
                Entity =
                { Id = getNuId ()
                  Enabled = true
                  Visible = true }}}
          
        let textBox =
            { BoxSprite = { SpriteAssetName = Lun.make "Image3"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
              Text = "Hi!"
              TextFont = { FontAssetName = Lun.make "Font"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
              TextOffset = Vector2 4.0f
              TextColor = Vector4.One
              Gui =
              { Gui.Position = Vector2 (120.0f, 50.0f)
                Depth = 0.1f
                Size = getTextureSizeAsVector2 (Lun.make "Image3") (Lun.make "Default") assetMetadataMap
                Entity =
                { Id = getNuId ()
                  Enabled = true
                  Visible = true }}}
          
        let toggle =
            { IsOn = false
              IsPressed = false
              OffSprite = { SpriteAssetName = Lun.make "Image3"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
              OnSprite = { SpriteAssetName = Lun.make "Image6"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
              ToggleSound = { SoundAssetName = Lun.make "Sound"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
              Gui =
              { Gui.Position = Vector2 (720.0f, 50.0f)
                Depth = 0.1f
                Size = getTextureSizeAsVector2 (Lun.make "Image3") (Lun.make "Default") assetMetadataMap
                Entity =
                { Id = getNuId ()
                  Enabled = true
                  Visible = true }}}
          
        let label =
            { LabelSprite = { SpriteAssetName = Lun.make "Image5"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
              Gui =
              { Gui.Position = Vector2.Zero
                Depth = -0.1f
                Size = getTextureSizeAsVector2 (Lun.make "Image5") (Lun.make "Default") assetMetadataMap
                Entity =
                { Id = getNuId ()
                  Enabled = true
                  Visible = true }}}
          
        let button =
            { IsDown = false
              UpSprite = { SpriteAssetName = Lun.make "Image"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
              DownSprite = { SpriteAssetName = Lun.make "Image2"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
              ClickSound = { SoundAssetName = Lun.make "Sound"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
              Gui =
              { Gui.Position = Vector2 (310.0f, 20.0f)
                Depth = 0.1f
                Size = getTextureSizeAsVector2 (Lun.make "Image") (Lun.make "Default") assetMetadataMap
                Entity =
                { Id = getNuId ()
                  Enabled = true
                  Visible = true }}}
    
        let tmxMap =
            TmxMap "TileMap.tmx"

        let tileMap =
            { PhysicsIds = [getPhysicsId ()]
              Density = NormalDensity
              TileMapAsset = { TileMapAssetName = Lun.make "TileMap"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
              TmxMap = tmxMap
              TileMapMetadata = getTileMapMetadata (Lun.make "TileMap") (Lun.make "Default") assetMetadataMap
              Actor =
              { Position = Vector2.Zero
                Depth = -0.1f
                Size = Vector2 (single (tmxMap.Width * tmxMap.TileWidth), single (tmxMap.Height * tmxMap.TileHeight))
                Rotation = 0.0f
                Entity =
                { Id = getNuId ()
                  Enabled = true
                  Visible = true }}}
    
        let floor =
            { PhysicsId = getPhysicsId ()
              Density = NormalDensity
              BodyType = Static
              Sprite = { SpriteAssetName = Lun.make "Image4"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
              Actor =
              { Position = Vector2 (120.0f, 520.0f)
                Depth = 0.0f
                Size = getTextureSizeAsVector2 (Lun.make "Image4") (Lun.make "Default") assetMetadataMap
                Rotation = 0.0f
                Entity =
                { Id = getNuId ()
                  Enabled = true
                  Visible = true }}}
    
        let avatar =
            { PhysicsId = getPhysicsId ()
              Density = NormalDensity
              Sprite = { SpriteAssetName = Lun.make "Image7"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
              Actor =
              { Position = Vector2 (300.0f, 300.0f)
                Depth = 0.0f
                Size = getTextureSizeAsVector2 (Lun.make "Image7") (Lun.make "Default") assetMetadataMap
                Rotation = 0.0f
                Entity =
                { Id = getNuId ()
                  Enabled = true
                  Visible = true }}}

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
        let w_ = subscribe TickAddress [] moveAvatar w_
        let w_ = subscribe ClickTestButtonAddress [] addBoxes w_
        let w_ = addScreen screen TestScreenAddress w_
        let w_ = set (Some TestScreenAddress) w_ World.optActiveScreenAddress
        let w_ = addGroup group TestGroupAddress w_
        let w_ = addFeeler feeler TestFeelerAddress w_
        let w_ = addTextBox textBox TestTextBoxAddress w_
        let w_ = addToggle toggle TestToggleAddress w_
        let w_ = addLabel label TestLabelAddress w_
        let w_ = addButton button TestButtonAddress w_
        let w_ = addTileMap tileMap TestTileMapAddress w_
        let w_ = addBlock floor TestFloorAddress w_
        let w_ = addAvatar avatar TestAvatarAddress w_
        let w_ = { w_ with PhysicsMessages = SetGravityMessage Vector2.Zero :: w_.PhysicsMessages }
        let w_ = { w_ with RenderMessages = hintRenderingPackageUse :: w_.RenderMessages }
        let w_ = { w_ with AudioMessages = FadeOutSong :: playSong :: w_.AudioMessages }
        Right w_

let testHandleUpdate world =
    let actor = get world (World.actor TestAvatarAddress)
    let camera = { world.Camera with EyePosition = actor.Position + actor.Size * 0.5f }
    (true, { world with Camera = camera })