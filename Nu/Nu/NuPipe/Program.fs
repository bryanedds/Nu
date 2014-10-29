namespace NuPipe
open System
open System.IO
open Prime
open Nu
open Nu.Constants
module Program =

    let [<EntryPoint>] main argv =
        match argv with
        | [|inputDirectory; outputDirectory|] ->
            match Assets.tryBuildAssetGraph inputDirectory outputDirectory AssetGraphFilePath with
            | Left error ->
                Console.WriteLine error
                FailureReturnCode
            | Right () -> SuccessReturnCode
        | _ ->
            Console.WriteLine "NuPipe.exe requires two parameters (input directory and output directory)."
            FailureReturnCode