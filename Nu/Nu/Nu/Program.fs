module Program
open System
open SDL2
open OpenTK
open Nu.Core
open Nu.Constants
open Nu.Sdl
open Nu.Audio
open Nu.Rendering
open Nu.Physics
open Nu.AssetMetadataMap
open Nu.Entity
open Nu.Group
open Nu.Screen
open Nu.Game
open Nu.Simulation

(* WISDOM: Program types and behavior should be closed where possible and open where necessary. *)

(* WISDOM: From benchmarks. it looks like our mobile target will cost us anywhere from a 75% to 90%
decrease in speed as compared to the dev machine. However, this can be mitigated in a few ways with
approximate speed-ups -

2x gain - Run app at 30fps instead of 60
2x gain - put physics in another process
1.5x gain - put rendering in another process, perhaps with physics, and / or render with OpenGL directly
1.3x gain - store loaded assets in a Dictionary<Dictionary, ...>> rather than a Map<Map, ...>>, or...
1.3x gain - alternatively, use short-term memoization with a temporary dictionary to cache asset queries during rendering / playing / etc.
1.2x gain - optimize locality of address usage
?x gain - as a last resort, the immutable Map<Lun, 'a>s used in the Simulants could be changed to mutable
Dictionary<Lun, 'a>s or even Dictionary<Lun, ref 'a>s. *)

let TestScreenAddress = [Lun.make "testScreen"]
let TestGroupAddress = TestScreenAddress @ [Lun.make "testGroup"]
let TestLabelAddress = TestGroupAddress @ [Lun.make "testLabel"]
let TestButtonAddress = TestGroupAddress @ [Lun.make "testButton"]
let TestBlockAddress = TestGroupAddress @ [Lun.make "testBlock"]
let TestFloorAddress = TestGroupAddress @ [Lun.make "testFloor"]
let ClickTestButtonAddress = Lun.make "click" :: TestButtonAddress

let createTestBlock assetMetadataMap =

    let testBlock =
        { PhysicsId = getPhysicsId ()
          Density = NormalDensity
          BodyType = Dynamic
          Sprite = { SpriteAssetName = Lun.make "Image3"; PackageName = Lun.make "Misc"; PackageFileName = "AssetGraph.xml" }
          ContactSound = { SoundAssetName = Lun.make "Sound"; PackageName = Lun.make "Misc"; PackageFileName = "AssetGraph.xml" }}

    let testBlockActor =
        { Position = Vector2 (400.0f, 200.0f)
          Depth = 0.0f
          Size = getTextureSizeAsVector2 (Lun.make "Image3") (Lun.make "Misc") assetMetadataMap
          Rotation = 2.0f
          ActorSemantic = Block testBlock }

    let testBlockActorEntity =
        { Id = getNuId ()
          IsEnabled = true
          IsVisible = true
          EntitySemantic = Actor testBlockActor }

    (testBlockActorEntity, testBlockActor, testBlock)

let createTestWorld (sdlDeps : SdlDeps) =

    let testGame =
        { Id = getNuId ()
          IsEnabled = true
          Screens = Map.empty
          OptActiveScreenAddress = None }

    let assetMetadataMap =
        match tryGenerateAssetMetadataMap "AssetGraph.xml" with
        | Left errorMsg -> failwith errorMsg
        | Right map -> map

    let testWorld =
        { Game = testGame
          Subscriptions = Map.empty
          MouseState = { MouseLeftDown = false; MouseRightDown = false; MouseCenterDown = false }
          AudioPlayer = makeAudioPlayer ()
          Renderer = makeRenderer sdlDeps.RenderContext
          Integrator = makeIntegrator Gravity
          AssetMetadataMap = assetMetadataMap
          AudioMessages = []
          RenderMessages = []
          PhysicsMessages = []
          Components = [] }

    let testScreen =
        { Id = getNuId ()
          IsEnabled = true
          IsVisible = true
          Groups = Map.empty
          ScreenSemantic = Title }

    let testGroup =
        { Id = getNuId ()
          IsEnabled = true
          IsVisible = true
          Entities = Map.empty }
          
    let testLabel =
        { LabelSprite = { SpriteAssetName = Lun.make "Image5"; PackageName = Lun.make "Misc"; PackageFileName = "AssetGraph.xml" }}

    let testLabelGui =
        { Position = Vector2.Zero
          Depth = -0.1f
          Size = getTextureSizeAsVector2 (Lun.make "Image5") (Lun.make "Misc") assetMetadataMap
          GuiSemantic = Label testLabel }

    let testLabelGuiEntity =
        { Id = getNuId ()
          IsEnabled = true
          IsVisible = true
          EntitySemantic = Gui testLabelGui }
          
    let testButton =
        { IsDown = false
          UpSprite = { SpriteAssetName = Lun.make "Image"; PackageName = Lun.make "Misc"; PackageFileName = "AssetGraph.xml" }
          DownSprite = { SpriteAssetName = Lun.make "Image2"; PackageName = Lun.make "Misc"; PackageFileName = "AssetGraph.xml" }
          ClickSound = { SoundAssetName = Lun.make "Sound"; PackageName = Lun.make "Misc"; PackageFileName = "AssetGraph.xml" }}

    let testButtonGui =
        { Position = Vector2 (310.0f, 20.0f)
          Depth = 0.1f
          Size = getTextureSizeAsVector2 (Lun.make "Image") (Lun.make "Misc") assetMetadataMap
          GuiSemantic = Button testButton }

    let testButtonGuiEntity =
        { Id = getNuId ()
          IsEnabled = true
          IsVisible = true
          EntitySemantic = Gui testButtonGui }
    
    let testFloor =
        { PhysicsId = getPhysicsId ()
          Density = NormalDensity
          BodyType = Static
          Sprite = { SpriteAssetName = Lun.make "Image4"; PackageName = Lun.make "Misc"; PackageFileName = "AssetGraph.xml" }
          ContactSound = { SoundAssetName = Lun.make "Sound"; PackageName = Lun.make "Misc"; PackageFileName = "AssetGraph.xml" }}

    let testFloorActor =
        { Position = Vector2 (120.0f, 520.0f)
          Depth = 0.0f
          Size = getTextureSizeAsVector2 (Lun.make "Image4") (Lun.make "Misc") assetMetadataMap
          Rotation = 0.0f
          ActorSemantic = Block testFloor }

    let testFloorActorEntity =
        { Id = getNuId ()
          IsEnabled = true
          IsVisible = true
          EntitySemantic = Actor testFloorActor }

    let addBoxes _ _ _ world =
        List.fold
            (fun world2 _ ->
                let entityActorBlock = createTestBlock assetMetadataMap
                addEntityActorBlock entityActorBlock (TestGroupAddress @ [Lun.makeN (getNuId ())]) world2)
            world
            [0..7]

    let hintRenderingPackageUse = HintRenderingPackageUse { FileName = "AssetGraph.xml"; PackageName = "Misc"; HRPU = () }
    let playSong = PlaySong { Song = { SongAssetName = Lun.make "Song"; PackageName = Lun.make "Misc"; PackageFileName = "AssetGraph.xml" }; FadeOutCurrentSong = true }

    // scripting convention
    let tw_ = testWorld
    let tw_ = subscribe ClickTestButtonAddress [] addBoxes tw_
    let tw_ = addScreen testScreen TestScreenAddress tw_
    let tw_ = setP (Some TestScreenAddress) World.optActiveScreenAddress tw_
    let tw_ = addGroup testGroup TestGroupAddress tw_
    let tw_ = addEntityGuiLabel (testLabelGuiEntity, testLabelGui, testLabel) TestLabelAddress tw_
    let tw_ = addEntityGuiButton (testButtonGuiEntity, testButtonGui, testButton) TestButtonAddress tw_
    let tw_ = addEntityActorBlock (testFloorActorEntity, testFloorActor, testFloor) TestFloorAddress tw_
    let tw_ = { tw_ with RenderMessages = hintRenderingPackageUse :: tw_.RenderMessages }
    { tw_ with AudioMessages = playSong :: playSong :: tw_.AudioMessages }

let [<EntryPoint>] main _ =
    let sdlRendererFlags = enum<SDL.SDL_RendererFlags> (int SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED ||| int SDL.SDL_RendererFlags.SDL_RENDERER_PRESENTVSYNC)
    let sdlConfig = makeSdlConfig "Nu Game Engine" 100 100 900 600 SDL.SDL_WindowFlags.SDL_WINDOW_SHOWN sdlRendererFlags 1024
    run2 createTestWorld sdlConfig

(*module Program
open System
open Propagate

type Data =
  { A : int
    B : byte }

type DataRecording =
    | ARecording of int
    | BRecording of byte

let setA setter =
    ((fun data -> let a2 = setter data.A in { data with A = a2 }),
     (fun data -> ARecording data.A))

let setB setter =
    ((fun data -> let b2 = setter data.B in { data with B = b2 }),
     (fun data -> BRecording data.B))

let [<EntryPoint>] main _ =

    Console.WriteLine (
        propagate 0 >.
        plus 2 >.
        mul 5)

    let propagatedData =
        propagate { A = 0; B = 0uy } >>.
        setA incI >>.
        setB incUy*)

(*module Program
open System
open System.IO
open System.Text  
open System.Threading
open System.Xml
open System.Xml.Serialization
open System.Runtime.Serialization
open System.Reflection
open Microsoft.FSharp.Reflection

let getUnionTypes<'a> () =
    let nestedTypes = typedefof<'a>.GetNestedTypes (BindingFlags.Public ||| BindingFlags.NonPublic) 
    Array.filter FSharpType.IsUnion nestedTypes

type Alpha =
    { X : int * int
      Y : Alpha option }
      
type [<KnownType "GetTypes">] Beta =
    | A of int * Alpha
    | B of Beta option
    | tw_ of Map<int, Beta>
    static member GetTypes () = getUnionTypes<Beta> ()

let reflectionTest () =
    let alpha = { X = (0, 0); Y = Some { X = (1, 1); Y = None }}
    let betaA = A (0, alpha)
    let betaB = B (Some betaA)
    let betaC = tw_ (Map.singleton 0 betaB)
    let sb = StringBuilder ()
    let xmlSerializer = DataContractSerializer typeof<Beta>
    xmlSerializer.WriteObject (XmlTextWriter (StringWriter sb), betaC)
    let sr = str sb
    printfn "%A" sr

let [<EntryPoint>] main _ =
    reflectionTest ()*)