namespace NuPipe
open System
open System.IO
open Prime
open Nu
open Nu.Constants
module Program =

    let [<EntryPoint>] main argv =
        World.init ()
        match argv with
        | [|inputDirectory; outputDirectory; refinementDirectory; fullBuildStr|] ->
            let fullBuild = fullBuildStr = acstring true
            match Assets.tryBuildAssetGraph inputDirectory outputDirectory refinementDirectory fullBuild AssetGraphFilePath with
            | Right () -> SuccessExitCode
            | Left error -> Console.WriteLine error; FailureExitCode
        | _ -> Console.WriteLine "NuPipe.exe requires two parameters (input directory and output directory)."; FailureExitCode