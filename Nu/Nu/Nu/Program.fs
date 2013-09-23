module Program
open System
open SDL2
open Nu.Sdl
open Nu.World

(* WISDOM: Program types and behavior should be closed where possible and open where necessary. *)

type Data =
  { A : int
    B : byte }

type DataRecording =
    | ARecording of int
    | BRecording of byte

let setA setter =
    ((fun data -> let newA = setter data.A in { data with A = newA }),
     (fun data -> ARecording data.A))

let setB setter =
    ((fun data -> let newB = setter data.B in { data with B = newB }),
     (fun data -> BRecording data.B))

let [<EntryPoint>] main _ =

    Console.WriteLine (
        propagate 0 >.
        plus 2 >.
        mul 5)

    let propagatedData =
        propagate { A = 0; B = 0uy } >>.
        setA incI >>.
        setB incUy

    let optAssets = Assets.tryLoadAssets "Rendering" "Misc" "AssetGraph.xml"
    let sdlRendererFlags = enum<SDL.SDL_RendererFlags> (int SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED ||| int SDL.SDL_RendererFlags.SDL_RENDERER_PRESENTVSYNC)
    let sdlConfig = makeSdlConfig "Nu Game Engine" 100 100 512 512 SDL.SDL_WindowFlags.SDL_WINDOW_SHOWN sdlRendererFlags
    run sdlConfig

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
    let sb = new StringBuilder()
    let xmlSerializer = DataContractSerializer(typeof<Beta>); 
    xmlSerializer.WriteObject(new XmlTextWriter(new StringWriter(sb)), betaC)
    let sr = sb.ToString()
    printfn "%A" sr

let [<EntryPoint>] main _ =
    reflectionTest ()*)
