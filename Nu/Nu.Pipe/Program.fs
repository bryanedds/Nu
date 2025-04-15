﻿// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace NuPipe
open System
open Prime
open Nu
module Program =

    let [<EntryPoint; STAThread>] main argv =
        match argv with
        | [|inputDirectory; outputDirectory; refinementDirectory; fullBuildStr|] ->
            match AssetGraph.tryMakeFromFile Assets.Global.AssetGraphFilePath with
            | Right assetGraph ->
                let fullBuild = fullBuildStr = string true
                AssetGraph.buildAssets inputDirectory outputDirectory refinementDirectory fullBuild assetGraph
                Constants.Engine.ExitCodeSuccess
            | Left error ->
                Console.WriteLine error
                Constants.Engine.ExitCodeFailure
        | _ ->
            Console.WriteLine "NuPipe.exe requires four parameters: inputDirectory, outputDirectory, refinementDirectory, and a fullBuild flag."
            Constants.Engine.ExitCodeFailure