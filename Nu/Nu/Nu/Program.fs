module Program
open System
open SDL2
open OpenTK
open Nu.Constants
open Nu.Sdl
open Nu.Audio
open Nu.Rendering
open Nu.Physics
open Nu.Simulants
open Nu.Simulation

(* WISDOM: Program types and behavior should be closed where possible and open where necessary. *)

let TestScreenAddress = [Lun.make "testScreen"]
let TestGroupAddress = TestScreenAddress @ [Lun.make "testGroup"]
let TestButtonAddress = TestGroupAddress @ [Lun.make "testButton"]
let TestBlockAddress = TestGroupAddress @ [Lun.make "testBlock"]
let TestFloorAddress = TestGroupAddress @ [Lun.make "testFloor"]
let ClickTestButtonAddress = Lun.make "click" :: TestButtonAddress

let createTestBlock () =

    let testBlock =
        { PhysicsId = getPhysicsId ()
          Density = 0.1f // TODO: ensure this is koscher with the physics system
          BodyType = Dynamic
          Sprite = { AssetName = Lun.make "Image3"; PackageName = Lun.make "Misc" }
          ContactSound = { AssetName = Lun.make "Sound"; PackageName = Lun.make "Misc" }}

    let testBlockActor =
        { Position = Vector2 (400.0f, 200.0f)
          Size = Vector2 64.0f // TODO: look this up from bitmap file
          Rotation = 0.0f
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
          Screens = LunTrie.empty
          OptActiveScreenAddress = None }

    let testWorld =
        { Game = testGame
          Subscriptions = Map.empty
          MouseState = { MouseLeftDown = false; MouseRightDown = false; MouseCenterDown = false }
          AudioPlayer = makeAudioPlayer ()
          Renderer = makeRenderer sdlDeps.RenderContext
          Integrator = makeIntegrator Gravity
          AudioMessages = []
          RenderMessages = []
          PhysicsMessages = []
          Components = [] }

    let testScreen =
        { Id = getNuId ()
          IsEnabled = true
          IsVisible = true
          Groups = LunTrie.empty
          ScreenSemantic = Title }

    let testGroup =
        { Id = getNuId ()
          IsEnabled = true
          IsVisible = true
          Entities = LunTrie.empty }
          
    let testButton =
        { IsDown = false
          UpSprite = { AssetName = Lun.make "Image"; PackageName = Lun.make "Misc" }
          DownSprite = { AssetName = Lun.make "Image2"; PackageName = Lun.make "Misc" }
          ClickSound = { AssetName = Lun.make "Sound"; PackageName = Lun.make "Misc" }}

    let testButtonGui =
        { Position = Vector2 100.0f
          Size = Vector2 (256.0f, 64.0f) // TODO: look this up from bitmap file
          GuiSemantic = Button testButton }

    let testButtonGuiEntity =
        { Id = getNuId ()
          IsEnabled = true
          IsVisible = true
          EntitySemantic = Gui testButtonGui }
    
    let testFloor =
        { PhysicsId = getPhysicsId ()
          Density = 0.1f // TODO: ensure this is koscher with the physics system
          BodyType = Static
          Sprite = { AssetName = Lun.make "Image4"; PackageName = Lun.make "Misc" }
          ContactSound = { AssetName = Lun.make "Sound"; PackageName = Lun.make "Misc" }}

    let testFloorActor =
        { Position = Vector2 (250.0f, 650.0f)
          Size = Vector2 (640.0f, 64.0f) // TODO: look this up from bitmap file
          Rotation = 0.0f
          ActorSemantic = Block testFloor }

    let testFloorActorEntity =
        { Id = getNuId ()
          IsEnabled = true
          IsVisible = true
          EntitySemantic = Actor testFloorActor }

    let testWorld_ = subscribe ClickTestButtonAddress [] (fun _ _ _ world -> let entityActorBlock = createTestBlock () in addEntityActorBlock entityActorBlock (TestGroupAddress @ [Lun.make (str (getNuId ()))]) world) testWorld
    let testWorld_ = addScreenX testScreen TestScreenAddress testWorld_
    let testWorld_ = set (Some TestScreenAddress) testWorld_ World.optActiveScreenAddress
    let testWorld_ = addGroup testGroup TestGroupAddress testWorld_
    let testWorld_ = addEntityGuiButton (testButtonGuiEntity, testButtonGui, testButton) TestButtonAddress testWorld_
    let testWorld_ = addEntityActorBlock (testFloorActorEntity, testFloorActor, testFloor) TestFloorAddress testWorld_
    let hintRenderingPackageUse = HintRenderingPackageUse { FileName = "AssetGraph.xml"; PackageName = "Misc"; HRPU = () }
    { testWorld_ with RenderMessages = hintRenderingPackageUse :: testWorld_.RenderMessages }

let [<EntryPoint>] main _ =
    let sdlRendererFlags = enum<SDL.SDL_RendererFlags> (int SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED ||| int SDL.SDL_RendererFlags.SDL_RENDERER_PRESENTVSYNC)
    let sdlConfig = makeSdlConfig "Nu Game Engine" 100 100 1024 768 SDL.SDL_WindowFlags.SDL_WINDOW_SHOWN sdlRendererFlags 1.0f
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
    | C of Map<int, Beta>
    static member GetTypes () = getUnionTypes<Beta> ()

let reflectionTest () =
    let alpha = { X = (0, 0); Y = Some { X = (1, 1); Y = None }}
    let betaA = A (0, alpha)
    let betaB = B (Some betaA)
    let betaC = C (Map.singleton 0 betaB)
    let sb = StringBuilder ()
    let xmlSerializer = DataContractSerializer typeof<Beta>
    xmlSerializer.WriteObject (XmlTextWriter (StringWriter sb), betaC)
    let sr = str sb
    printfn "%A" sr

let [<EntryPoint>] main _ =
    reflectionTest ()*)