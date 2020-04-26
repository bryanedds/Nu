// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

#I __SOURCE_DIRECTORY__
#r "../../packages/FParsec.1.0.3/lib/net40-client/FParsecCS.dll" // MUST be referenced BEFORE FParsec.dll!
#r "../../packages/FParsec.1.0.3/lib/net40-client/FParsec.dll"
#r "../../packages/FsCheck.2.11.0/lib/net452/FsCheck.dll"
#r "../../packages/FsCheck.Xunit.2.11.0/lib/net452/FsCheck.Xunit.dll"
#r "../../packages/Prime.5.1.0/lib/net472/Prime.dll"
#r "../../packages/Prime.Scripting.5.1.0/lib/net472/Prime.Scripting.exe"
#r "../../Nu/Nu.Dependencies/FSharpx.Core/FSharpx.Core.dll"
#r "../../Nu/Nu.Dependencies/FSharpx.Collections/FSharpx.Collections.dll"
#r "../../Nu/Nu.Dependencies/Farseer/FarseerPhysics.dll"
#r "../../Nu/Nu.Dependencies/Magick.NET/Magick.NET-AnyCPU.dll"
#r "../../Nu/Nu.Dependencies/Nito.Collections.Deque/Nito.Collections.Deque.dll"
#r "../../Nu/Nu.Dependencies/SDL2-CS/Debug/SDL2-CS.dll"
#r "../../Nu/Nu.Dependencies/TiledSharp/Debug/TiledSharp.dll"
#r "../../Nu/Nu.Math/bin/Debug/Nu.Math.dll"
#r "../../Nu/Nu/bin/Debug/Nu.exe"
#r "../../Nu/Nu.Gaia.Design/bin/Debug/Nu.Gaia.Design.exe"
#r "../../Nu/Nu.Gaia/bin/Debug/Nu.Gaia.exe"

open System
open System.IO
open System.Windows.Forms
open FSharpx
open FSharpx.Collections
open TiledSharp
open Prime
open Nu
open Nu.Declarative
open Nu.Gaia

// set current directly to local for execution in VS F# interactive
Directory.SetCurrentDirectory (__SOURCE_DIRECTORY__ + "../bin/Debug")

// initialize Gaia
Gaia.init NuConfig.defaultConfig

// decide on a target directory and plugin
let (_, targetDir, plugin) = Gaia.selectTargetDirAndMakeNuPlugin ()

// initialize Gaia's form
let form = Gaia.createForm ()
form.Closing.Add (fun args ->
    if not args.Cancel then
        MessageBox.Show ("Cannot close Gaia when running from F# Interactive.", "Cannot close Gaia") |> ignore
        args.Cancel <- true)

// initialize sdl dependencies using the form as its rendering surface
let sdlDeps = Gaia.tryMakeSdlDeps form |> Either.getRightValue

// make world ready for use in Gaia
let world = Gaia.tryMakeWorld false plugin sdlDeps WorldConfig.defaultConfig |> Either.getRightValue

// example of running Nu in Gaia for 60 frames from repl
Gaia.runFromRepl (fun world -> World.getTickTime world < 60L) targetDir sdlDeps form world