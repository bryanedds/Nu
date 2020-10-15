// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

#I __SOURCE_DIRECTORY__
#r "../../packages/Magick.NET-Q8-x64.7.16.1/lib/net40/Magick.NET-Q8-x64.dll"
#r "../../packages/TiledSharp.1.0.1/lib/netstandard2.0/TiledSharp.dll"
#r "../../packages/FParsec.1.0.3/lib/net40-client/FParsecCS.dll" // MUST be referenced BEFORE FParsec.dll!
#r "../../packages/FParsec.1.0.3/lib/net40-client/FParsec.dll"
#r "../../packages/FsCheck.2.11.0/lib/net452/FsCheck.dll"
#r "../../packages/FsCheck.Xunit.2.11.0/lib/net452/FsCheck.Xunit.dll"
#r "../../packages/Prime.5.25.0/lib/net472/Prime.dll"
#r "../../packages/Prime.Scripting.5.3.0/lib/net472/Prime.Scripting.exe"
#r "../../packages/FSharpx.Core.1.8.32/lib/40/FSharpx.Core.dll"
#r "../../packages/FSharpx.Collections.2.1.3/lib/net45/FSharpx.Collections.dll"
#r "../../packages/FarseerPhysics.3.5.0/lib/NET40/FarseerPhysics.dll"
#r "../../packages/Nito.Collections.Deque.1.1.0/lib/netstandard2.0/Nito.Collections.Deque.dll"
#r "../../packages/SDL2-CS.dll.2.0.0.0/lib/net20/SDL2-CS.dll"
#r "../../Nu/Nu.Math/bin/x64/Debug/Nu.Math.dll"
#r "../../Nu/Nu/bin/Debug/Nu.exe"
#r "../../Nu/Nu.Gaia.Design/bin/x64/Debug/Nu.Gaia.Design.exe"
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
Directory.SetCurrentDirectory (__SOURCE_DIRECTORY__ + "/bin/Debug")

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
let world = Gaia.tryMakeWorld false sdlDeps WorldConfig.defaultConfig plugin |> Either.getRightValue

// stop world from ticking (new variable since you can't shadow in repl for some reason...)
let world' = World.setTickRate 0L world

//run Gaia from repl
Gaia.runFromRepl tautology targetDir sdlDeps form world'