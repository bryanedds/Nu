// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

#I __SOURCE_DIRECTORY__
#r "System.Configuration" // TODO: ensure this reference is actually needed...
#r "../../packages/FParsec.1.0.3/lib/net40-client/FParsecCS.dll" // MUST be referenced BEFORE FParsec.dll!
#r "../../packages/FParsec.1.0.3/lib/net40-client/FParsec.dll"
#r "../../packages/FsCheck.2.11.0/lib/net452/FsCheck.dll"
#r "../../packages/FsCheck.Xunit.2.11.0/lib/net452/FsCheck.Xunit.dll"
#r "../../packages/Prime.5.0.0/lib/net472/Prime.dll"
#r "../../packages/Prime.Scripting.5.0.1/lib/net472/Prime.Scripting.exe"
#r "../../Nu/Nu.Dependencies/FSharpx.Core/FSharpx.Core.dll"
#r "../../Nu/Nu.Dependencies/FSharpx.Collections/FSharpx.Collections.dll"
#r "../../Nu/Nu.Dependencies/Farseer/FarseerPhysics.dll"
#r "../../Nu/Nu.Dependencies/Magick.NET/Magick.NET-AnyCPU.dll"
#r "../../Nu/Nu.Dependencies/Nito.Collections.Deque/Nito.Collections.Deque.dll"
#r "../../Nu/Nu.Dependencies/SDL2-CS/Debug/SDL2-CS.dll"
#r "../../Nu/Nu.Dependencies/TiledSharp/Debug/TiledSharp.dll"
#r "../../Nu/Nu.Math/bin/Debug/Nu.Math.dll"
#r "../../Nu/Nu/bin/Debug/Nu.exe"

open global.System
open global.System.IO
open global.FSharpx
open global.FSharpx.Collections
open global.SDL2
open global.TiledSharp
open global.Prime
open global.Nu
open global.Nu.Declarative

// set current directly to local for execution in VS F# interactive
Directory.SetCurrentDirectory (__SOURCE_DIRECTORY__ + "/bin/Debug")

// initialize Nu
Nu.init NuConfig.defaultConfig