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
        | [|inputDirectory; outputDirectory; refinementDirectory; blockCompressionStr; fullBuildStr|] ->
            Constants.Render.TextureCompressionType <- scvalue blockCompressionStr
            let assetGraph = AssetGraph.makeFromFileOpt Assets.Global.AssetGraphFilePath
            let fullBuild = fullBuildStr = string true
            AssetGraph.buildAssets inputDirectory outputDirectory refinementDirectory fullBuild assetGraph
            Constants.Engine.ExitCodeSuccess
        | _ ->
            Console.WriteLine "NuPipe.exe requires five parameters: inputDirectory, outputDirectory, refinementDirectory, blockCompression, and fullBuild."
            Constants.Engine.ExitCodeFailure