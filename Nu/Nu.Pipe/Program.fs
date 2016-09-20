namespace NuPipe
open System
open Prime
open Nu
module Program =

    let [<EntryPoint; STAThread>] main argv =
        match argv with
        | [|inputDirectory; outputDirectory; refinementDirectory; fullBuildStr|] ->
            let fullBuild = fullBuildStr = scstring true
            match AssetGraph.tryMakeFromFile Constants.Assets.AssetGraphFilePath with
            | Right assetGraph ->
                AssetGraph.buildAssets inputDirectory outputDirectory refinementDirectory fullBuild assetGraph
                Constants.Engine.SuccessExitCode
            | Left error ->
                Console.WriteLine error
                Constants.Engine.FailureExitCode
        | _ ->
            Console.WriteLine "NuPipe.exe requires four parameters: inputDirectory, outputDirectory, refinementDirectory, and a fullBuild flag."
            Constants.Engine.FailureExitCode