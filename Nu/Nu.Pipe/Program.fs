// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace NuPipe
open System
open Prime
open Nu
module Program =

    let [<EntryPoint; STAThread>] main argv =
        match argv with
        | [||] ->
            let assetGraph = AssetGraph.makeFromFileOpt Assets.Global.AssetGraphFilePath
            AssetGraph.buildAssets "../../.." "." "../../refinement" BcCompression true assetGraph
            Constants.Engine.ExitCodeSuccess
        | [|inputDirectory; outputDirectory; refinementDirectory; blockCompressionStr; fullBuildStr|] ->
            let blockCompression = scvalue blockCompressionStr
            let fullBuild = fullBuildStr = string true
            let assetGraph = AssetGraph.makeFromFileOpt Assets.Global.AssetGraphFilePath
            AssetGraph.buildAssets inputDirectory outputDirectory refinementDirectory blockCompression fullBuild assetGraph
            Constants.Engine.ExitCodeSuccess
        | _ ->
            Console.WriteLine "NuPipe.exe requires five parameters: inputDirectory, outputDirectory, refinementDirectory, blockCompression, and fullBuild."
            Constants.Engine.ExitCodeFailure