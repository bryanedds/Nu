namespace NuPipe
open System
open System.IO
open Prime
open Nu
open Nu.Constants
module Program =

    let [<EntryPoint>] main argv =
        match argv with
        | [|inputDirectory; tempDirectory; outputDirectory; fullBuildStr|] ->
            let fullBuild = fullBuildStr = string true
            match Assets.tryBuildAssetGraph inputDirectory tempDirectory outputDirectory fullBuild AssetGraphFilePath with
            | Left error -> Console.WriteLine error; FailureExitCode
            | Right () -> SuccessExitCode
        | _ -> Console.WriteLine "NuPipe.exe requires two parameters (input directory and output directory)."; FailureExitCode