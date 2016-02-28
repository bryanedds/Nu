namespace NuPipe
open System
open System.IO
open Prime
open Nu
module Program =

    let [<EntryPoint; STAThread>] main argv =
        match argv with
        | [|inputDirectory; outputDirectory; refinementDirectory; fullBuildStr|] ->
            let fullBuild = fullBuildStr = scstring true
            match AssetGraph.tryBuildAssetGraph inputDirectory outputDirectory refinementDirectory fullBuild Constants.Assets.AssetGraphFilePath with
            | Right () -> Constants.Engine.SuccessExitCode
            | Left error ->
                Console.WriteLine error
                Constants.Engine.FailureExitCode
        | _ ->
            Console.WriteLine "NuPipe.exe requires four parameters: inputDirectory, outputDirectory, refinementDirectory, and a fullBuild flag."
            Constants.Engine.FailureExitCode