// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open ImageMagick
open ImageMagick.Formats
open Prime

/// A refinement that can be applied to an asset during the build process.
type Refinement =
    | PsdToPng
    | ConvertToDds

    /// Convert a string to a refinement value.
    static member ofString str =
        match str with
        | nameof PsdToPng -> PsdToPng
        | nameof ConvertToDds -> ConvertToDds
        | _ -> failwith ("Invalid refinement '" + str + "'.")

/// Describes a game asset, such as an image, sound, or model in detail.
///
/// All assets must belong to an asset Package, which is a unit of asset loading.
///
/// In order for the renderer to render a single texture, that texture, along with all the other assets in the
/// corresponding package, must be loaded. Also, the only way to unload any of those assets is to send a package unload
/// message to the relevent subsystem.
type Asset =
    abstract AssetTag : AssetTag
    abstract FilePath : string
    abstract Refinements : Refinement list
    abstract Associations : string Set

/// Describes a strongly-typed game asset, such as an image, sound, or model, in detail.
///
/// All assets must belong to an asset Package, which is a unit of asset loading.
///
/// In order for the renderer to render a single texture, that texture, along with all the other assets in the
/// corresponding package, must be loaded. Also, the only way to unload any of those assets is to send a package unload
/// message to the relevent subsystem.
type [<ReferenceEquality>] 'a Asset =
    { AssetTag : 'a AssetTag
      FilePath : string
      Refinements : Refinement list
      Associations : string Set }
    interface Asset with
        member this.AssetTag = this.AssetTag
        member this.FilePath = this.FilePath
        member this.Refinements = this.Refinements
        member this.Associations = this.Associations

/// Asset functions.
[<RequireQualifiedAccess>]
module Asset =

    /// Make an asset value.
    let make<'a> assetTag filePath refinements associations : 'a Asset =
        { AssetTag = assetTag
          FilePath = filePath
          Refinements = refinements
          Associations = associations }

/// Tracks assets as well as their originating file paths.
type [<ReferenceEquality>] Package<'a, 's> =
    { Assets : Dictionary<string, DateTimeOffset * Asset * 'a>
      PackageState : 's }

/// A dictionary of asset packages.
type Packages<'a, 's> = Dictionary<string, Package<'a, 's>>

/// Describes assets and how to process and use them.
type AssetDescriptor =
    | Asset of AssetName : string * FilePath : string * Refinements : Refinement list * Associations : string Set
    | Assets of Directory : string * Extensions : string Set * Refinements : Refinement list * Associations : string Set

/// Describes asset packages.
type PackageDescriptor = AssetDescriptor list

[<RequireQualifiedAccess>]
module AssetGraph =

    /// A graph of all the assets used in a game.
    type AssetGraph =
        private
            { FilePathOpt_ : string option
              PackageDescriptors_ : Map<string, PackageDescriptor> }

        member this.PackageDescriptors =
            this.PackageDescriptors_

    let private AssetGraphStr = """
[[Default
 [[Assets Assets/Default [bmp png psd ttf skel json] [PsdToPng] [Render2d]]
  [Assets Assets/Default [jpg jpeg tga tif tiff dds] [ConvertToDds] [Render3d]]
  [Assets Assets/Default [cbm fbx gltf glb dae obj mtl raw] [] [Render3d]]
  [Assets Assets/Default [wav ogg mp3] [] [Audio]]
  [Assets Assets/Default [cur] [] [Cursor]]
  [Assets Assets/Default [nueffect nuscript csv] [] [Symbol]]
  [Assets Assets/Default [nuentity nugroup tsx tmx atlas nav nbrd glsl bin] [] []]]]]"""

    let private getAssetExtension2 rawAssetExtension refinement =
        match refinement with
        | PsdToPng -> if rawAssetExtension = ".psd" then ".png" else rawAssetExtension
        | ConvertToDds -> ".dds"

    let private getAssetExtension usingRawAssets rawAssetExtension refinements =
        if usingRawAssets
        then List.fold getAssetExtension2 rawAssetExtension refinements
        else rawAssetExtension

    /// Apply a single refinement to an asset.
    let private refineAssetOnce (intermediateFileSubpath : string) intermediateDirectory refinementDirectory refinement =

        // build the intermediate file path
        let intermediateFileExtension = PathF.GetExtensionMixed intermediateFileSubpath
        let intermediateFilePath = intermediateDirectory + "/" + intermediateFileSubpath

        // build the refinement file path
        let refinementFileExtension = getAssetExtension2 intermediateFileExtension refinement
        let refinementFileSubpath = PathF.ChangeExtension (intermediateFileSubpath, refinementFileExtension)
        let refinementFilePath = refinementDirectory + "/" + refinementFileSubpath

        // refine the asset
        Directory.CreateDirectory (PathF.GetDirectoryName refinementFilePath) |> ignore
        match refinement with
        | PsdToPng ->
            if intermediateFileExtension = ".psd" then
                use imageCollection = new MagickImageCollection (intermediateFilePath)
                use image0 = imageCollection.[0] // NOTE: we clear out image0 to fix conversion to png somehow.
                image0.ColorFuzz <- Percentage 100.0
                image0.FloodFill (MagickColors.Transparent, 0, 0)
                use image = imageCollection.Flatten MagickColors.Transparent
                use stream = File.OpenWrite refinementFilePath
                image.Write (stream, MagickFormat.Png32)
            else
                if not (File.Exists refinementFilePath) then
                    File.Copy (intermediateFilePath, refinementFilePath)
                elif File.GetLastWriteTime intermediateFilePath > File.GetLastWriteTime refinementFilePath then
                    File.Copy (intermediateFilePath, refinementFilePath, true)
        | ConvertToDds ->
            use image = new MagickImage (intermediateFilePath)
            use stream = File.OpenWrite refinementFilePath
            let defines = DdsWriteDefines ()
            defines.FastMipmaps <-
#if DEBUG
                true
#else
                false
#endif
            if OpenGL.Texture.BlockCompressable refinementFilePath
            then image.Alpha AlphaOption.Set // implicitly directs use of dxt5 compression - https://github.com/ImageMagick/ImageMagick/pull/4914#issuecomment-1060654324
            else defines.Compression <- DdsCompression.None
            image.Write (stream, defines)

        // return the latest refinement localities
        (refinementFileSubpath, refinementDirectory)

    /// Apply all refinements to an asset.
    let private refineAsset inputFileSubpath inputDirectory refinementDirectory refinements =
        List.fold (fun (intermediateFileSubpath, intermediateDirectory) refinement ->
            refineAssetOnce intermediateFileSubpath intermediateDirectory refinementDirectory refinement)
            (inputFileSubpath, inputDirectory)
            refinements

    /// Build all the assets.
    let private buildAssets5 inputDirectory outputDirectory refinementDirectory fullBuild (assets : Asset list) =

        // build assets
        for asset in assets do

            // build input file path
            let inputFileSubpath = asset.FilePath
            let inputFileExtension = PathF.GetExtensionMixed inputFileSubpath
            let inputFilePath = inputDirectory + "/" + inputFileSubpath

            // build the output file path
            let outputFileExtension = getAssetExtension true inputFileExtension asset.Refinements
            let outputFileSubpath = PathF.ChangeExtension (asset.FilePath, outputFileExtension)
            let outputFilePath = outputDirectory + "/" + outputFileSubpath

            // build the asset if fully building or if it's out of date
            if  fullBuild ||
                not (File.Exists outputFilePath) ||
                File.GetLastWriteTime inputFilePath > File.GetLastWriteTime outputFilePath then

                // refine the asset
                let (intermediateFileSubpath, intermediateDirectory) =
                    if List.isEmpty asset.Refinements then (inputFileSubpath, inputDirectory)
                    else refineAsset inputFileSubpath inputDirectory refinementDirectory asset.Refinements

                // attempt to copy the intermediate asset if output file is out of date
                let intermediateFilePath = intermediateDirectory + "/" + intermediateFileSubpath
                let outputFilePath = outputDirectory + "/" + intermediateFileSubpath
                Directory.CreateDirectory (PathF.GetDirectoryName outputFilePath) |> ignore
                try if File.Exists outputFilePath then File.SetAttributes (outputFilePath, FileAttributes.None)
                    File.Copy (intermediateFilePath, outputFilePath, true)
                    File.SetAttributes (outputFilePath, FileAttributes.ReadOnly) // prevents errors when accidentally altering output .dds files and the like
                with _ -> Log.info ("Resource lock on '" + outputFilePath + "' has prevented build for asset '" + scstring asset.AssetTag + "'.")

    /// Collect the associated assets from package descriptor assets value.
    let private collectAssetsFromPackageDescriptorAssets packageName directory extensions associations refinements : Asset list =
        [if Directory.Exists directory then
            let filePaths =
                [for extension in extensions do
                    for filePath in Directory.GetFiles (directory, "*." + extension, SearchOption.AllDirectories) do
                        PathF.Normalize filePath]
            for filePath in filePaths do
                let assetName = PathF.GetFileNameWithoutExtension filePath
                let tag = AssetTag.make<obj> packageName assetName
                yield Asset.make tag filePath refinements associations
         else Log.info ("Invalid directory '" + directory + "'. when looking for assets.")]

    /// Collect the associated assets from a package descriptor.
    let private collectAssetsFromPackageDescriptor packageName packageDescriptor : Asset list =
        [for assetDescriptor in packageDescriptor do
            match assetDescriptor with
            | Asset (assetName, filePath, refinements, associations) ->
                let tag = AssetTag.make<obj> packageName assetName
                yield Asset.make tag filePath refinements associations
            | Assets (directory, extensions, associations, refinements) ->
                yield! collectAssetsFromPackageDescriptorAssets packageName directory extensions refinements associations]

    /// Attempt to collect all the available assets from a package.
    let tryCollectAssetsFromPackage associationOpt packageName assetGraph =
        let mutable packageDescriptor = Unchecked.defaultof<PackageDescriptor>
        match Map.tryGetValue (packageName, assetGraph.PackageDescriptors_, &packageDescriptor) with
        | true ->
            collectAssetsFromPackageDescriptor packageName packageDescriptor
            |> List.groupBy (fun asset -> asset.FilePath)
            |> List.map snd
            |> List.map (List.reduce (fun asset asset2 ->
                { AssetTag = AssetTag.make asset.AssetTag.PackageName asset.AssetTag.AssetName
                  FilePath = asset.FilePath
                  Refinements = List.append asset.Refinements asset2.Refinements
                  Associations = Set.union asset.Associations asset2.Associations } :> Asset))
            |> List.filter (fun asset -> match associationOpt with Some association -> asset.Associations.Contains association | _ -> true)
            |> Right
        | false -> Left ("Could not find package '" + packageName + "' in asset graph.")

    /// Collect all the available assets from an asset graph document.
    let collectAssets associationOpt assetGraph =
        [for entry in assetGraph.PackageDescriptors_ do
            let packageName = entry.Key
            let packageDescriptor = entry.Value
            yield! collectAssetsFromPackageDescriptor packageName packageDescriptor]
        |> List.groupBy (fun asset -> asset.FilePath)
        |> List.map snd
        |> List.map (List.reduce (fun asset asset2 ->
            { AssetTag = AssetTag.make asset.AssetTag.PackageName asset.AssetTag.AssetName
              FilePath = asset.FilePath
              Refinements = List.append asset.Refinements asset2.Refinements
              Associations = Set.union asset.Associations asset2.Associations } :> Asset))
        |> List.filter (fun asset -> match associationOpt with Some association -> asset.Associations.Contains association | _ -> true)

    /// Build all the available assets found in the given asset graph.
    let buildAssets inputDirectory outputDirectory refinementDirectory fullBuild assetGraph =

        // compute the asset graph's tracker file path
        let outputFilePathOpt =
            Option.map (fun (filePath : string) ->
                outputDirectory + "/" + PathF.ChangeExtension (PathF.GetFileName filePath, ".tracker"))
                assetGraph.FilePathOpt_

        // check if the output assetGraph file is newer than the current
        let fullBuild =
            fullBuild ||
            match (assetGraph.FilePathOpt_, outputFilePathOpt) with
            | (Some filePath, Some outputFilePath) -> File.GetLastWriteTime filePath > File.GetLastWriteTime outputFilePath
            | (None, None) -> false
            | (_, _) -> failwithumf ()

        // collect assets
        let currentDirectory = Directory.GetCurrentDirectory ()
        let assets =
            try Directory.SetCurrentDirectory inputDirectory
                collectAssets None assetGraph
            finally
                Directory.SetCurrentDirectory currentDirectory

        // log when image assets are being shared between 2d and 3d renders
        if List.exists (fun (asset : Asset) ->
            let extension = PathF.GetExtensionLower asset.FilePath
            match extension with
            | ImageExtension _ ->
                asset.Associations.Contains Constants.Associations.Render2d &&
                asset.Associations.Contains Constants.Associations.Render3d
            | _ -> false)
            assets then
            Console.WriteLine "Warning: Due to asset graph limitations, associating image assets with both Render2d and Render3d is not fully supported."

        // build assets
        buildAssets5 inputDirectory outputDirectory refinementDirectory fullBuild assets

        // output the asset graph tracker file
        match outputFilePathOpt with
        | Some outputFilePath -> File.WriteAllText (outputFilePath, "")
        | None -> ()

    /// The empty asset graph.
    let empty =
        { FilePathOpt_ = None
          PackageDescriptors_ = Map.empty }

    /// Make an asset graph.
    let make filePathOpt packageDescriptors =
        { FilePathOpt_ = filePathOpt
          PackageDescriptors_ = packageDescriptors }

    /// Make an asset graph, attempting to use the file at the given file path.
    let makeFromFileOpt filePath =
        let (filePathOpt, packageDescriptors) =
            if File.Exists filePath then
                try File.ReadAllText filePath
                    |> String.unescape
                    |> scvalue<Map<string, PackageDescriptor>>
                    |> fun packageDescriptors -> (Some filePath, packageDescriptors)
                with exn ->
                    Log.warn ("Could not make asset graph from file '" + filePath + "' due to: " + scstring exn)
                    (None, scvalue AssetGraphStr)
            else (None, scvalue AssetGraphStr)
        make filePathOpt packageDescriptors


/// A graph of all the assets used in a game.
type AssetGraph = AssetGraph.AssetGraph