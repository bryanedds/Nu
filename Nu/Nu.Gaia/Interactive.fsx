// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

#I __SOURCE_DIRECTORY__
#r "System.Configuration"
#r "../../Prime/Prime.Dependencies/FParsec/FParsecCS.dll" // MUST be referenced BEFORE FParsec.dll!
#r "../../Prime/Prime.Dependencies/FParsec/FParsec.dll"
#r "../../Prime/Prime.Dependencies/xunit/xunit.core.dll"
#r "../../Prime/Prime.Dependencies/xunit/xunit.abstractions.dll"
#r "../../Prime/Prime.Dependencies/xunit/xunit.assert.dll"
#r "../../Prime/Prime.Dependencies/xunit/xunit.execution.desktop.dll"
#r "../../Prime/Prime.Dependencies/FsCheck/FsCheck.dll"
#r "../../Prime/Prime.Dependencies/FsCheck.Xunit/FsCheck.Xunit.dll"
#r "../../Prime/Prime/bin/Debug/Prime.exe"
#r "../../Nu/Nu.Dependencies/FSharpx.Core/FSharpx.Core.dll"
#r "../../Nu/Nu.Dependencies/FSharpx.Collections/FSharpx.Collections.dll"
#r "../../Nu/Nu.Dependencies/Farseer/FarseerPhysics.dll"
#r "../../Nu/Nu.Dependencies/Magick.NET/Magick.NET-AnyCPU.dll"
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
open Prime.Stream
open Prime.Chain
open Nu
open Nu.Gaia

// set current directly to local for execution in VS F# interactive
Directory.SetCurrentDirectory ^ __SOURCE_DIRECTORY__ + "../bin/Debug"

// initialize Nu
Nu.init false

// decide on a target directory and plugin
let (targetDir, plugin) = Gaia.selectTargetDirAndMakeNuPlugin ()

// initialize Gaia's form
let form = Gaia.createForm ()
form.Closing.Add (fun args ->
    if not args.Cancel then
        ignore ^ MessageBox.Show ("Cannot close Gaia when running from F# Interactive.", "Cannot close Gaia")
        args.Cancel <- true)

// initialize sdl dependencies using the form as its rendering surface
let sdlDeps = Gaia.attemptMakeSdlDeps form |> Either.getRightValue

// make world ready for use in Gaia
let world = Gaia.attemptMakeWorld plugin sdlDeps |> Either.getRightValue

// example of running Nu in Gaia for 60 frames from repl
Gaia.runFromRepl (fun world -> World.getTickTime world < 60L) targetDir sdlDeps form world