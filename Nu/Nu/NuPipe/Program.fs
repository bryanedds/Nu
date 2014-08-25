namespace NuPipe
open System
open System.IO
open Prime
open Nu
open Nu.NuConstants
module Program =
    
    let [<EntryPoint>] main argv = 
        match argv with
        | [|inputDir; outputDir|] ->
            let eitherAssets = Assets.tryLoadAssets None <| Path.Combine (inputDir, AssetGraphFileName)
            match eitherAssets with
            | Left error -> failwith error
            | Right assets ->
                for asset in assets do
                    let assetFilePath = inputDir + "/" + asset.FileName
                    let outputFilePath = outputDir + "/" + asset.FileName
                    let outputDirectory = Path.GetDirectoryName outputFilePath
                    ignore <| Directory.CreateDirectory outputDirectory
                    File.Copy (assetFilePath, outputFilePath)
                0
        | _ -> failwith "NuPipe.exe requires two parameters"