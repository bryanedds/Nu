// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Nu
open System
open System.IO
open ImageMagick
open Prime
open Nu

/// A refinement that can be applied to an asset during the build process.
type Refinement =
    | PsdToPng
    | OldSchool

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Refinement =

    /// Convert a string to a refinement value.
    let fromString str =
        match str with
        | "PsdToPng" -> PsdToPng
        | "OldSchool" -> OldSchool
        | _ -> failwith ^ "Invalid refinement '" + str + "'."

/// Describes a means for looking up an asset.
type AssetTag =
    { PackageName : string
      AssetName : string }

/// Describes a game asset, such as a texture, sound, or model in detail.
type [<StructuralEquality; NoComparison>] Asset =
    { AssetTag : AssetTag
      FilePath : string
      Refinements : Refinement list
      Associations : string Set }

/// All assets must belong to an asset Package, which is a unit of asset loading.
///
/// In order for the renderer to render a single texture, that texture, along with all the other
/// assets in the corresponding package, must be loaded. Also, the only way to unload any of those
/// assets is to send an AssetPackageUnload message to the renderer, which unloads them all. There
/// is an AssetPackageLoad message to load a package when convenient.
///
/// The use of a message system for the renderer should enable streamed loading, optionally with
/// smooth fading-in of late-loaded assets (IE - assets that are already in the view frustum but are
/// still being loaded).
///
/// Finally, the use of AssetPackages could enforce assets to be loaded in order of size and will
/// avoid unnecessary Large Object Heap fragmentation.
type [<StructuralEquality; NoComparison>] Package =
    { Name : string
      AssetNames : string list }

/// A map of asset packages.
type 'a PackageMap = Map<string, Map<string, 'a>>

/// Describes assets and how to process and use them.
type AssetDescriptor =
    | Asset of string * string * string Set * Refinement list
    | Assets of string * string * string Set * Refinement list

/// Describes assets packages.
type PackageDescriptor =
    AssetDescriptor list

[<AutoOpen>]
module AssetGraphModule =

    /// A graph of all the assets used in a game.
    [<Syntax
        ("Asset Assets",
         "nueffect nuscript bmp png ttf tmx wav ogg " +
         "PsdToPng OldSchool Render Audio Symbol",
         "", "",
         Constants.PrettyPrinter.DefaultThresholdMin,
         Constants.PrettyPrinter.DefaultThresholdMax)>]
    type AssetGraph =
        private
            { PackageDescriptors : Map<string, PackageDescriptor> }
    
    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module AssetGraph =
    
        let private getAssetExtension2 rawAssetExtension refinement =
            match refinement with
            | PsdToPng -> "png"
            | OldSchool -> rawAssetExtension
    
        let private getAssetExtension usingRawAssets rawAssetExtension refinements =
            if usingRawAssets then List.fold getAssetExtension2 rawAssetExtension refinements
            else rawAssetExtension
    
        let private writeMagickImageAsPng filePath (image : MagickImage) =
            match Path.GetExtension filePath with
            | ".png" ->
                use stream = File.OpenWrite filePath
                image.Write (stream, MagickFormat.Png32)
            | _ -> Log.info ^ "Invalid image file format for refinement '" + scstring OldSchool + "'; must be of *.png format."
    
        /// Apply a single refinement to an asset.
        let private refineAssetOnce intermediateFileSubpath intermediateDirectory refinementDirectory refinement =
    
            // build the intermediate file path
            let intermediateFileExtension = Path.GetExtension intermediateFileSubpath
            let intermediateFilePath = Path.Combine (intermediateDirectory, intermediateFileSubpath)
    
            // build the refinement file path
            let refinementFileExtension = getAssetExtension2 intermediateFileExtension refinement
            let refinementFileSubpath = Path.ChangeExtension (intermediateFileSubpath, refinementFileExtension)
            let refinementFilePath = Path.Combine (refinementDirectory, refinementFileSubpath)
    
            // refine the asset
            Directory.CreateDirectory ^ Path.GetDirectoryName refinementFilePath |> ignore
            match refinement with
            | PsdToPng ->
                use image = new MagickImage (intermediateFilePath)
                writeMagickImageAsPng refinementFilePath image
            | OldSchool ->
                use image = new MagickImage (intermediateFilePath)
                image.Scale (Percentage 400)
                writeMagickImageAsPng refinementFilePath image
    
            // return the latest refinement localities
            (refinementFileSubpath, refinementDirectory)
    
        /// Apply all refinements to an asset.
        let private refineAsset inputFileSubpath inputDirectory refinementDirectory refinements =
            List.fold
                (fun (intermediateFileSubpath, intermediateDirectory) refinement ->
                    refineAssetOnce intermediateFileSubpath intermediateDirectory refinementDirectory refinement)
                (inputFileSubpath, inputDirectory)
                refinements
    
        /// Build all the assets.
        let private buildAssets5 inputDirectory outputDirectory refinementDirectory fullBuild assets =
    
            // build assets
            for asset in assets do
    
                // build input file path
                let inputFileSubpath = asset.FilePath
                let inputFileExtension = Path.GetExtension inputFileSubpath
                let inputFilePath = Path.Combine (inputDirectory, inputFileSubpath)
    
                // build the output file path
                let outputFileExtension = getAssetExtension true inputFileExtension asset.Refinements
                let outputFileSubpath = Path.ChangeExtension (asset.FilePath, outputFileExtension)
                let outputFilePath = Path.Combine (outputDirectory, outputFileSubpath)
    
                // build the asset if fully building or if it's out of date
                if  fullBuild ||
                    not ^ File.Exists outputFilePath ||
                    File.GetLastWriteTimeUtc inputFilePath > File.GetLastWriteTimeUtc outputFilePath then
    
                    // refine the asset
                    let (intermediateFileSubpath, intermediateDirectory) =
                        if List.isEmpty asset.Refinements then (inputFileSubpath, inputDirectory)
                        else refineAsset inputFileSubpath inputDirectory refinementDirectory asset.Refinements
    
                    // attempt to copy the intermediate asset if output file is out of date
                    let intermediateFilePath = Path.Combine (intermediateDirectory, intermediateFileSubpath)
                    let outputFilePath = Path.Combine (outputDirectory, intermediateFileSubpath)
                    Directory.CreateDirectory ^ Path.GetDirectoryName outputFilePath |> ignore
                    try File.Copy (intermediateFilePath, outputFilePath, true)
                    with _ -> Log.info ^ "Resource lock on '" + outputFilePath + "' has prevented build for asset '" + scstring asset.AssetTag + "'."
    
        /// Load all the assets from a package descriptor.
        let private loadAssetsFromPackageDescriptor4 usingRawAssets associationOpt packageName packageDescriptor =
            let assets =
                List.fold (fun assetsRev assetDescriptor ->
                    match assetDescriptor with
                    | Asset (assetName, filePath, associations, refinements) ->
                        let asset =
                            { AssetTag = { PackageName = packageName; AssetName = assetName }
                              FilePath = filePath
                              Refinements = refinements
                              Associations = associations }
                        asset :: assetsRev
                    | Assets (directory, rawExtension, associations, refinements) ->
                        let extension = getAssetExtension usingRawAssets rawExtension refinements
                        try let filePaths = Directory.GetFiles (directory, "*." + extension, SearchOption.AllDirectories)
                            let assets =
                                Array.map
                                    (fun filePath ->
                                        { AssetTag = { PackageName = packageName; AssetName = Path.GetFileNameWithoutExtension filePath }
                                          FilePath = filePath
                                          Refinements = refinements
                                          Associations = associations })
                                    filePaths |>
                                List.ofArray
                            assets @ assetsRev
                        with exn -> Log.info ^ "Invalid directory '" + directory + "'."; [])
                    [] packageDescriptor |>
                List.rev
            match associationOpt with
            | Some association -> List.filter (fun asset -> Set.contains association asset.Associations) assets
            | None -> assets

        /// Get package descriptors.
        let getPackageDescriptors assetGraph =
            assetGraph.PackageDescriptors

        /// Get package names.
        let getPackageNames assetGraph =
            Map.toKeyList assetGraph.PackageDescriptors

        /// Attempt to load all the available assets from a package.
        let tryLoadAssetsFromPackage usingRawAssets associationOpt packageName assetGraph =
            match Map.tryFind packageName assetGraph.PackageDescriptors with
            | Some packageDescriptor ->
                let assets = loadAssetsFromPackageDescriptor4 usingRawAssets associationOpt packageName packageDescriptor
                Right assets
            | None -> Left ^ "Could not find package '" + packageName + "' in asset graph."

        /// Load all the available assets from an asset graph document.
        let loadAssets usingRawAssets associationOpt assetGraph =
            Map.fold (fun assetListsRev packageName packageDescriptor ->
                let assets = loadAssetsFromPackageDescriptor4 usingRawAssets associationOpt packageName packageDescriptor
                assets :: assetListsRev)
                [] assetGraph.PackageDescriptors |>
            List.rev |>
            List.concat
    
        /// Build all the available assets found in the given asset graph.
        let buildAssets inputDirectory outputDirectory refinementDirectory fullBuild assetGraph =
    
            // load assets
            let currentDirectory = Directory.GetCurrentDirectory ()
            let assets =
                try let () = Directory.SetCurrentDirectory inputDirectory in loadAssets false None assetGraph
                finally Directory.SetCurrentDirectory currentDirectory
    
            // build assets
            buildAssets5 inputDirectory outputDirectory refinementDirectory fullBuild assets

        /// The empty asset graph.
        let empy =
            { PackageDescriptors = Map.empty }

        /// Make an asset graph.
        let make packageDescriptors =
            { PackageDescriptors = packageDescriptors }

        /// Attempt to make an asset graph.
        let tryMakeFromFile filePath =
            try File.ReadAllText filePath |>
                String.unescape |>
                scvalue<Map<string, PackageDescriptor>> |>
                make |>
                Right
            with exn -> Left ^ "Could not make asset graph from file '" + filePath + "' due to: " + scstring exn

/// A graph of all the assets used in a game.
type AssetGraph = AssetGraphModule.AssetGraph