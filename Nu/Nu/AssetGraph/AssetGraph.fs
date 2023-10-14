// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open ImageMagick
open Prime

/// A refinement that can be applied to an asset during the build process.
type Refinement =
    | PsdToPng
    | Retro
    
    /// Convert a string to a refinement value.
    static member ofString str =
        match str with
        | "PsdToPng" -> PsdToPng
        | "Retro" -> Retro
        | _ -> failwith ("Invalid refinement '" + str + "'.")

/// Describes a game asset, such as a texture, sound, or model in detail.
///
/// All assets must belong to an asset Package, which is a unit of asset loading.
///
/// In order for the renderer to render a single texture, that texture, along with all the other
/// assets in the corresponding package, must be loaded. Also, the only way to unload any of those
/// assets is to send an AssetPackageUnload message to the relevent subsystem, which unloads them all.
/// There is an AssetPackageLoad message to load a package when convenient.
///
/// The use of a message system for the subsystem should enable streamed loading, optionally with
/// smooth fading-in of late-loaded assets (IE - render assets that are already in the view frustum
/// but are still being loaded).
///
/// Finally, the use of AssetPackages could enforce assets to be loaded in order of size and will
/// avoid unnecessary Large Object Heap fragmentation.
type [<ReferenceEquality>] 'a Asset =
    { AssetTag : 'a AssetTag
      FilePath : string
      Refinements : Refinement list
      Associations : string Set }

[<RequireQualifiedAccess>]
module Asset =

    /// Make an asset value.
    let make<'a> assetTag filePath refinements associations : 'a Asset =
        { AssetTag = assetTag
          FilePath = filePath
          Refinements = refinements
          Associations = associations }

    /// Convert an asset from one type to another.
    let convert<'a, 'b> (asset : 'a Asset) : 'b Asset =
        make<'b> (AssetTag.convert<'a, 'b> asset.AssetTag) asset.FilePath asset.Refinements asset.Associations

    /// Convert an asset with a specific type to one of obj type.
    let generalize (asset : 'a Asset) : obj Asset =
        convert<'a, obj> asset

    /// Convert an asset with an obj type to one of a specific type.
    let specialize<'a> (asset : obj Asset) : 'a Asset =
        convert<obj, 'a> asset

/// An asset package with additional user-defined state.
type [<ReferenceEquality>] Package<'a, 's> =
    { Assets : Dictionary<string, 'a>
      PackageState : 's }

/// A dictionary of asset packages.
type Packages<'a, 's> = Dictionary<string, Package<'a, 's>>

/// Describes assets and how to process and use them.
type AssetDescriptor =
    | Asset of string * string * string Set * Refinement list
    | Assets of string * string Set * string Set * Refinement list

/// Describes asset packages.
type PackageDescriptor = AssetDescriptor list

[<RequireQualifiedAccess>]
module AssetGraph =

    /// A graph of all the assets used in a game.
    [<Syntax
        ("Asset Assets",
         "nueffect nuscript nugroup psd png bmp ttf tmx wav ogg csv " +
         "PsdToPng Retro " +
         "Render Audio Symbol",
         "", "", "",
         Constants.PrettyPrinter.DefaultThresholdMin,
         Constants.PrettyPrinter.DefaultThresholdMax)>]
    type AssetGraph =
        private
            { FilePathOpt : string option
              PackageDescriptors : Map<string, PackageDescriptor> }

    let private getAssetExtension2 rawAssetExtension refinement =
        match refinement with
        | PsdToPng -> "png"
        | Retro -> rawAssetExtension

    let private getAssetExtension usingRawAssets rawAssetExtension refinements =
        if usingRawAssets then List.fold getAssetExtension2 rawAssetExtension refinements
        else rawAssetExtension

    let private writeMagickImageAsPng psdHack (filePath : string) (image : MagickImage) =
        match Path.GetExtension(filePath).ToLowerInvariant() with
        | ".png" ->
            use stream = File.OpenWrite filePath
            if psdHack then
                // HACK: this is a hack that deals with a more recent image magick bug that causes the transparent
                // pixels of an image to be turned pure white or black. The side-effect of this hack is that your
                // psd images cannot contain full white or black as a color.
                image.ColorFuzz <- Percentage 0.0
                image.Opaque (MagickColor.FromRgba (byte 255, byte 255, byte 255, byte 255), MagickColor.FromRgba (byte 0, byte 0, byte 0, byte 0))
                image.Opaque (MagickColor.FromRgba (byte 0, byte 0, byte 0, byte 255), MagickColor.FromRgba (byte 0, byte 0, byte 0, byte 0))
            image.Write (stream, MagickFormat.Png32)
        | _ -> Log.info ("Invalid image file format for scaling refinement; must be of *.png format.")

    /// Apply a single refinement to an asset.
    let private refineAssetOnce (intermediateFileSubpath : string) intermediateDirectory refinementDirectory refinement =

        // build the intermediate file path
        let intermediateFileExtension = Path.GetExtension intermediateFileSubpath
        let intermediateFilePath = intermediateDirectory + "/" + intermediateFileSubpath

        // build the refinement file path
        let refinementFileExtension = getAssetExtension2 intermediateFileExtension refinement
        let refinementFileSubpath = Path.ChangeExtension (intermediateFileSubpath, refinementFileExtension)
        let refinementFilePath = refinementDirectory + "/" + refinementFileSubpath

        // refine the asset
        Directory.CreateDirectory (Path.GetDirectoryName refinementFilePath) |> ignore
        match refinement with
        | PsdToPng ->
            use image = new MagickImage (intermediateFilePath)
            writeMagickImageAsPng false refinementFilePath image
        | Retro ->
            use image = new MagickImage (intermediateFilePath)
            image.Scale (Percentage 300)
            writeMagickImageAsPng false refinementFilePath image

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
            let inputFilePath = inputDirectory + "/" + inputFileSubpath

            // build the output file path
            let outputFileExtension = getAssetExtension true inputFileExtension asset.Refinements
            let outputFileSubpath = Path.ChangeExtension (asset.FilePath, outputFileExtension)
            let outputFilePath = outputDirectory + "/" + outputFileSubpath

            // build the asset if fully building or if it's out of date
            if  fullBuild ||
                not (File.Exists outputFilePath) ||
                File.GetLastWriteTimeUtc inputFilePath > File.GetLastWriteTimeUtc outputFilePath then

                // refine the asset
                let (intermediateFileSubpath, intermediateDirectory) =
                    if List.isEmpty asset.Refinements then (inputFileSubpath, inputDirectory)
                    else refineAsset inputFileSubpath inputDirectory refinementDirectory asset.Refinements

                // attempt to copy the intermediate asset if output file is out of date
                let intermediateFilePath = intermediateDirectory + "/" + intermediateFileSubpath
                let outputFilePath = outputDirectory + "/" + intermediateFileSubpath
                Directory.CreateDirectory (Path.GetDirectoryName outputFilePath) |> ignore
                try File.Copy (intermediateFilePath, outputFilePath, true)
                with _ -> Log.info ("Resource lock on '" + outputFilePath + "' has prevented build for asset '" + scstring asset.AssetTag + "'.")

    /// Collect the associated assets from package descriptor assets value.
    let private collectAssetsFromPackageDescriptorAssets associationOpt packageName directory extensions associations refinements =
        seq {
            if Directory.Exists directory then
                let filePaths =
                    seq {
                        for extension in extensions do
                            yield! Directory.GetFiles (directory, "*." + extension, SearchOption.AllDirectories) }
                for filePath in filePaths do
                    let filePath = filePath.Replace ("\\", "/") // normalize format
                    let extension = Path.GetExtension(filePath).Replace(".", "").ToLowerInvariant()
                    let assetName = (Path.GetFileNameWithoutExtension filePath)
                    let tag = AssetTag.make<obj> packageName assetName
                    let asset = Asset.make tag filePath refinements associations
                    if Option.isSome associationOpt
                    then if Set.contains extension extensions then yield asset
                    else yield asset
            else Log.info ("Invalid directory '" + directory + "'. when looking for assets.") }

    /// Collect the associated assets from a package descriptor.
    let private collectAssetsFromPackageDescriptor (associationOpt : string option) packageName packageDescriptor =
        seq {
            for assetDescriptor in packageDescriptor do
                match assetDescriptor with
                | Asset (assetName, filePath, associations, refinements) ->
                    let tag = AssetTag.make<obj> packageName assetName
                    let asset = Asset.make tag filePath refinements associations
                    match associationOpt with
                    | Some association -> if Set.contains association associations then yield asset
                    | None -> yield asset
                | Assets (directory, extensions, associations, refinements) ->
                    match associationOpt with
                    | Some association when Set.contains association associations ->
                        yield! collectAssetsFromPackageDescriptorAssets associationOpt packageName directory extensions associations refinements
                    | None ->
                        yield! collectAssetsFromPackageDescriptorAssets associationOpt packageName directory extensions associations refinements
                    | _ -> () } |>
        Seq.toList

    /// Get package descriptors.
    let getPackageDescriptors assetGraph =
        assetGraph.PackageDescriptors

    /// Get package names.
    let getPackageNames assetGraph =
        Map.toKeyList assetGraph.PackageDescriptors

    /// Attempt to collect all the available assets from a package.
    let tryCollectAssetsFromPackage associationOpt packageName assetGraph =
        let mutable packageDescriptor = Unchecked.defaultof<PackageDescriptor>
        match Map.tryGetValue (packageName, assetGraph.PackageDescriptors, &packageDescriptor) with
        | true ->
            collectAssetsFromPackageDescriptor associationOpt packageName packageDescriptor |>
            List.groupBy (fun asset -> asset.FilePath) |>
            List.map (snd >> List.last) |>
            Right
        | false -> Left ("Could not find package '" + packageName + "' in asset graph.")

    /// Collect all the available assets from an asset graph document.
    let collectAssets associationOpt assetGraph =
        seq {
            for entry in assetGraph.PackageDescriptors do
                let packageName = entry.Key
                let packageDescriptor = entry.Value
                yield! collectAssetsFromPackageDescriptor associationOpt packageName packageDescriptor } |>
        Seq.toList |>
        List.groupBy (fun asset -> asset.FilePath) |>
        List.map (snd >> List.last)

    /// Build all the available assets found in the given asset graph.
    let buildAssets inputDirectory outputDirectory refinementDirectory fullBuild assetGraph =

        // compute the asset graph's tracker file path
        let outputFilePathOpt =
            Option.map (fun (filePath : string) ->
                outputDirectory + "/" + Path.ChangeExtension(Path.GetFileName filePath, ".tracker"))
                assetGraph.FilePathOpt

        // check if the output assetGraph file is newer than the current
        let fullBuild =
            fullBuild ||
            match (assetGraph.FilePathOpt, outputFilePathOpt) with
            | (Some filePath, Some outputFilePath) -> File.GetLastWriteTimeUtc filePath > File.GetLastWriteTimeUtc outputFilePath
            | (None, None) -> false
            | (_, _) -> failwithumf ()

        // collect assets
        let currentDirectory = Directory.GetCurrentDirectory ()
        let assets =
            try Directory.SetCurrentDirectory inputDirectory
                collectAssets None assetGraph
            finally
                Directory.SetCurrentDirectory currentDirectory

        // build assets
        buildAssets5 inputDirectory outputDirectory refinementDirectory fullBuild assets

        // output the asset graph tracker file
        match outputFilePathOpt with
        | Some outputFilePath -> File.WriteAllText (outputFilePath, "")
        | None -> ()

    /// The empty asset graph.
    let empty =
        { FilePathOpt = None
          PackageDescriptors = Map.empty }

    /// Make an asset graph.
    let make filePathOpt packageDescriptors =
        { FilePathOpt = filePathOpt
          PackageDescriptors = packageDescriptors }

    /// Attempt to make an asset graph.
    let tryMakeFromFile filePath =
        try File.ReadAllText filePath |>
            String.unescape |>
            scvalue<Map<string, PackageDescriptor>> |>
            make (Some filePath) |>
            Right
        with exn -> Left ("Could not make asset graph from file '" + filePath + "' due to: " + scstring exn)

/// A graph of all the assets used in a game.
type AssetGraph = AssetGraph.AssetGraph