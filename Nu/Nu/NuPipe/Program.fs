namespace NuPipe
open System
open System.IO
open Prime
open Nu
open Nu.NuConstants
module Program =

    let SuccessCode = 0
    let FailureCode = -1

    let copyAssetFiles inputDir outputDir assets =
        for asset in assets do
            let assetFilePath = Path.Combine (inputDir, asset.FileName)
            let outputFilePath = Path.Combine (outputDir, asset.FileName)
            let outputDirectory = Path.GetDirectoryName outputFilePath
            ignore <| Directory.CreateDirectory outputDirectory
            if  not <| File.Exists outputFilePath ||
                File.GetLastWriteTimeUtc assetFilePath > File.GetLastWriteTimeUtc outputFilePath then
                File.Copy (assetFilePath, outputFilePath, true)
    
    let [<EntryPoint>] main argv = 
        match argv with
        | [|inputDir; outputDir|] ->
            let eitherAssets = Assets.tryLoadAssets None <| Path.Combine (inputDir, AssetGraphFileName)
            match eitherAssets with
            | Left error ->
                Console.WriteLine error
                FailureCode
            | Right assets ->
                try
                    copyAssetFiles inputDir outputDir assets
                    SuccessCode
                with e ->
                    Console.WriteLine (string e)
                    FailureCode
        | _ ->
            Console.WriteLine "NuPipe.exe requires two parameters (input directory and output directory)."
            FailureCode