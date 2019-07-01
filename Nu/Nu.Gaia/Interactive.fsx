// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

#I __SOURCE_DIRECTORY__
#r "System.Configuration"
#r "../../packages/System.ValueTuple.4.5.0/lib/portable-net40+sl4+win8+wp8/System.ValueTuple.dll"
#r "../../packages/FParsec.1.0.3/lib/net40-client/FParsecCS.dll" // MUST be referenced BEFORE FParsec.dll!
#r "../../packages/FParsec.1.0.3/lib/net40-client/FParsec.dll"
//#r "../../packages/xunit.core.2.3.1/xunit.core.2.3.1.nupkg"
//#r "../../packages/xunit.abstractions.2.0.1/xunit.abstractions.2.0.1.nupkg"
//#r "../../packages/xunit.assert.2.3.1/xunit.assert.2.3.1.nupkg"
#r "../../packages/FsCheck.2.11.0/lib/net452/FsCheck.dll"
#r "../../packages/FsCheck.Xunit.2.11.0/lib/net452/FsCheck.Xunit.dll"
#r "../../packages/Prime.3.5.7/lib/net46/Prime.exe"
#r "../../Nu/Nu.Dependencies/FSharpx.Core/FSharpx.Core.dll"
#r "../../Nu/Nu.Dependencies/FSharpx.Collections/FSharpx.Collections.dll"
#r "../../Nu/Nu.Dependencies/Farseer/FarseerPhysics.dll"
#r "../../Nu/Nu.Dependencies/Magick.NET/Magick.NET-AnyCPU.dll"
#r "../../Nu/Nu.Dependencies/Nito.Collections.Deque/Nito.Collections.Deque.dll"
#r "../../Nu/Nu.Dependencies/SDL2#/Debug/SDL2#.dll"
#r "../../Nu/Nu.Dependencies/TiledSharp/Debug/TiledSharp.dll"
#r "../../Nu/Nu.SDL2/bin/Debug/Nu.SDL2.dll"
#r "../../Nu/Nu/bin/Debug/Nu.exe"
#r "../../Nu/Nu.Gaia.Design/bin/Debug/Nu.Gaia.Design.exe"
#r "../../Nu/Nu.Gaia/bin/Debug/Nu.Gaia.exe"

open System
open System.IO
open System.Windows.Forms
open FSharpx
open FSharpx.Collections
open SDL2
open OpenTK
open TiledSharp
open Prime
open Nu
open Nu.Gaia

// set current directly to local for execution in VS F# interactive
Directory.SetCurrentDirectory (__SOURCE_DIRECTORY__ + "../bin/Debug")

// initialize Nu
Nu.init false

// decide on a target directory and plugin
let (targetDir, plugin) = Gaia.selectTargetDirAndMakeNuPlugin ()

// initialize Gaia's form
let form = Gaia.createForm ()
form.Closing.Add (fun args ->
    if not args.Cancel then
        MessageBox.Show ("Cannot close Gaia when running from F# Interactive.", "Cannot close Gaia") |> ignore
        args.Cancel <- true)

// initialize sdl dependencies using the form as its rendering surface
let sdlDeps = Gaia.tryMakeSdlDeps form |> Either.getRightValue

// make world ready for use in Gaia
let world = Gaia.tryMakeWorld plugin sdlDeps |> Either.getRightValue

// example of running Nu in Gaia for 60 frames from repl
Gaia.runFromRepl (fun world -> World.getTickTime world < 60L) targetDir sdlDeps form world