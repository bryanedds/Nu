#r "System.Configuration"
#r "../../../Prime/FSharpx.Core/FSharpx.Core.dll"
#r "../../../Prime/FSharpx.Collections/FSharpx.Collections.dll"
#r "../../../Prime/FParsec/FParsecCS.dll" // MUST be referenced BEFORE FParsec.dll!
#r "../../../Prime/FParsec/FParsec.dll"
#r "../../../Prime/xUnit/xunit.dll"
#r "../../../Prime/Prime/Prime/bin/Debug/Prime.exe"
#r "../../../Nu/Farseer/FarseerPhysics.dll"
#r "../../../Nu/Magick.NET/Magick.NET-AnyCPU.dll"
#r "../../../Nu/SDL2#/Debug/SDL2#.dll"
#r "../../../Nu/TiledSharp/Debug/TiledSharp.dll"
#r "../../../SDL2Addendum/SDL2Addendum/SDL2Addendum/bin/Debug/SDL2Addendum.dll"
#r "../../../Nu/Nu/Nu/bin/Debug/Nu.exe"
#r "../../../Nu/Nu/NuEditDesign/bin/Debug/NuEditDesign.exe"
#r "../../../Nu/Nu/NuEdit/bin/Debug/NuEdit.exe"

open System
open System.IO
open FSharpx
open SDL2
open OpenTK
open TiledSharp
open Prime
open Nu
open Nu.Observation
open Nu.Chain
open NuEdit

// set current directly to local for execution in VS F# interactive
Directory.SetCurrentDirectory ^ __SOURCE_DIRECTORY__ + "../bin/Debug"

// initialize NuEdit's dependencies
let form = NuEdit.createForm ()
form.Closing.Add (fun args -> args.Cancel <- true) // disable exiting
let sdlDeps = NuEdit.attemptMakeSdlDeps form |> Either.getRightValue
let (targetDir, plugin) = (".", NuPlugin ()) // alternatively, could pick these with NuEdit.selectTargetDirAndMakeNuPlugin ()

// make world ready for use in NuEdit (could instead use NuEdit.attemptMakeWorld if less flexibility is needed)
let world =
    World.attemptMake false 0L () plugin sdlDeps |> Either.getRightValue |>
    World.createScreen typeof<ScreenDispatcher>.Name None (Some Simulants.EditorScreen.ScreenName) |> snd |>
    World.createGroup typeof<GroupDispatcher>.Name None (Some Simulants.DefaultEditorGroup.GroupName) Simulants.EditorScreen |> snd |>
    World.setOptSelectedScreen (Some Simulants.EditorScreen)

// example of running Nu in NuEdit for 60 frames from repl
NuEdit.runFromRepl (fun world -> World.getTickTime world < 60L) targetDir sdlDeps form world