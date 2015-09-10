#nowarn "9"
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
World.init ()
let form = NuEdit.createForm ()
let (targetDir, plugin) = NuEdit.selectTargetDirAndMakeNuPlugin ()
let sdlDeps = Either.getRightValue ^ NuEdit.attemptMakeSdlDeps form

// make world for NuEdit
let world = Either.getRightValue ^ NuEdit.tryMakeEditorWorld targetDir sdlDeps form plugin

let () =

    // run for 60 frames
    let world = NuEdit.runFromRepl (fun world -> World.getTickTime world < 60L) sdlDeps form world
    let world = NuEdit.runFromRepl (fun world -> World.getTickTime world < 60L) sdlDeps form world
    ignore world