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
System.IO.Directory.SetCurrentDirectory ^ __SOURCE_DIRECTORY__ + "../bin/Debug"

// initialize NuEdit's dependencies
Nu.init ()
let form = NuEdit.createForm ()
form.Closing.Add (fun args -> args.Cancel <- true) // disable exiting
let (targetDir, plugin) = NuEdit.selectTargetDirAndMakeNuPlugin ()
let sdlDeps = Either.getRightValue ^ NuEdit.attemptMakeSdlDeps form

// make world ready for use in NuEdit (could instead use NuEdit.attemptMakeWorld if less flexibility is needed)
let world =
    World.attemptMake false 0L () plugin sdlDeps |> Either.getRightValue |>
    World.createScreen typeof<ScreenDispatcher>.Name (Some Simulants.EditorScreen.ScreenName) |> snd |>
    World.createGroup typeof<GroupDispatcher>.Name (Some Simulants.DefaultEditorGroup.GroupName) Simulants.EditorScreen |> snd |>
    World.setOptSelectedScreen (Some Simulants.EditorScreen) |>
    NuEdit.attachToWorld targetDir form

// example of running NuEdit for 60 frames
// let world = NuEdit.runFromRepl (fun world -> World.getTickTime world < 60L) sdlDeps form world