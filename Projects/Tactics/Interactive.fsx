// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

#I __SOURCE_DIRECTORY__
#r "../../packages/Magick.NET-Q8-x64.7.5.0.1/lib/net40/Magick.NET-Q8-x64.dll"
#r "../../packages/Csv.1.0.58/lib/net40/Csv.dll"
#r "../../packages/FParsec.1.0.3/lib/net40-client/FParsecCS.dll" // MUST be referenced BEFORE FParsec.dll!
#r "../../packages/FParsec.1.0.3/lib/net40-client/FParsec.dll"
#r "../../packages/Prime.8.0.0/lib/net472/Prime.dll"
#r "../../packages/Prime.Scripting.8.0.0/lib/net472/Prime.Scripting.exe"
#r "../../packages/FSharpx.Core.1.8.32/lib/40/FSharpx.Core.dll"
#r "../../packages/FSharpx.Collections.2.1.3/lib/net45/FSharpx.Collections.dll"
#r "../../packages/Aether.Physics2D.1.5.0/lib/net40/Aether.Physics2D.dll"
#r "../../packages/Nito.Collections.Deque.1.1.0/lib/netstandard2.0/Nito.Collections.Deque.dll"
#r "../../Nu/Nu.Dependencies/SDL2-CS.dll/lib/net20/SDL2-CS.dll"
#r "../../Nu/Nu.Dependencies/TiledSharp.1.0.2/lib/netstandard2.0/TiledSharp.dll"
#r "../../Nu/Nu.Math/bin/x64/Debug/Nu.Math.dll"
#r "../../Nu/Nu/bin/Debug/Nu.exe"

let workingDirPath = __SOURCE_DIRECTORY__ + "/bin/Debug"
if not (System.IO.Directory.Exists workingDirPath) then failwith "You must build the project in Debug mode at least once before running in interactive."
System.IO.Directory.SetCurrentDirectory workingDirPath

#load "Scenery.fs"
#load "SceneryPlugin.fs"

open System
open System.Numerics
open System.IO
open Prime
open Nu

// copy over required project files
File.Copy ("../../AssetGraph.nuag", "AssetGraph.nuag", true)
File.Copy ("../../Overlayer.nuol", "Overlayer.nuol", true)
File.Copy ("../../Prelude.nuscript", "Prelude.nuscript", true)

// build assets
match AssetGraph.tryMakeFromFile Assets.Global.AssetGraphFilePath with
| Right assetGraph -> AssetGraph.buildAssets "../.." "." "../../refinement" false assetGraph
| Left _ -> ()

// init nu and run game
Nu.init NuConfig.defaultConfig
World.run WorldConfig.defaultConfig (Scenery.SceneryPlugin ())