// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

#I __SOURCE_DIRECTORY__
#r "../../packages/Magick.NET-Q8-x64.7.16.1/lib/net40/Magick.NET-Q8-x64.dll"
#r "../../packages/TiledSharp.1.0.1/lib/netstandard2.0/TiledSharp.dll"
#r "../../packages/FParsec.1.0.3/lib/net40-client/FParsecCS.dll" // MUST be referenced BEFORE FParsec.dll!
#r "../../packages/FParsec.1.0.3/lib/net40-client/FParsec.dll"
#r "../../packages/FsCheck.2.11.0/lib/net452/FsCheck.dll"
#r "../../packages/FsCheck.Xunit.2.11.0/lib/net452/FsCheck.Xunit.dll"
#r "../../packages/Prime.6.0.0/lib/net472/Prime.dll"
#r "../../packages/Prime.Scripting.6.0.0/lib/net472/Prime.Scripting.exe"
#r "../../packages/FSharpx.Core.1.8.32/lib/40/FSharpx.Core.dll"
#r "../../packages/FSharpx.Collections.2.1.3/lib/net45/FSharpx.Collections.dll"
#r "../../packages/FarseerPhysics.3.5.0/lib/NET40/FarseerPhysics.dll"
#r "../../packages/Nito.Collections.Deque.1.1.0/lib/netstandard2.0/Nito.Collections.Deque.dll"
#r "../../packages/SDL2-CS.dll.2.0.0.0/lib/net20/SDL2-CS.dll"
#r "../../Nu/Nu.Math/bin/x64/Debug/Nu.Math.dll"
#r "../../Nu/Nu/bin/Debug/Nu.exe"

#load "BlazeAssets.fs"
#load "BlazeSimulants.fs"
#load "BlazeDispatchers.fs"
#load "BlazeDispatcher.fs"
#load "BlazePlugin.fs"

open System.IO
open Prime
open Nu

// ensure project has been built at least once before proceeding
let workingDirPath = __SOURCE_DIRECTORY__ + "/bin/Debug"
if not (Directory.Exists workingDirPath) then failwith "You must build the project at least once before running in interactive."
Directory.SetCurrentDirectory workingDirPath

// copy over required project files
File.Copy ("../../AssetGraph.nuag", "AssetGraph.nuag", true)
File.Copy ("../../Overlayer.nuol", "Overlayer.nuol", true)
File.Copy ("../../Prelude.nuscript", "Prelude.nuscript", true)

// build assets
match AssetGraph.tryMakeFromFile Assets.AssetGraphFilePath with
| Right assetGraph -> AssetGraph.buildAssets "../.." "." "../../refinement" false assetGraph
| Left _ -> ()

// init nu and run game
Nu.init NuConfig.defaultConfig
World.run WorldConfig.defaultConfig (BlazeVector.BlazePlugin ())