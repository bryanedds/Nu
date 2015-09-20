// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open System.Xml
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
/// TODO: consider if we could / should use a TypeCarrier (phantom type) here.
type AssetTag =
    { PackageName : string
      AssetName : string }

/// Describes a game asset, such as a texture, sound, or model in detail.
type [<StructuralEquality; NoComparison>] Asset =
    { AssetTag : AssetTag
      FilePath : string
      Refinements : Refinement list
      Associations : string list }

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

/// Maps asset (packages + names) to asset values.
type 'a AssetMap = Map<string, Map<string, 'a>>

[<RequireQualifiedAccess>]
module Assets =

    let private tryGetAssetFilePath (node : XmlNode) =
        match node.Attributes.GetNamedItem Constants.Xml.FileAttributeName with
        | null -> None
        | filePath -> Some filePath.InnerText

    let private tryGetAssetDirectory (node : XmlNode) =
        match node.Attributes.GetNamedItem Constants.Xml.DirectoryAttributeName with
        | null -> None
        | directory -> Some directory.InnerText

    let private getAssetAssociations (node : XmlNode) =
        match node.Attributes.GetNamedItem Constants.Xml.AssociationsAttributeName with
        | null -> []
        | associations -> AlgebraicDescriptor.convertFromString associations.InnerText typeof<string list> :?> string list

    let private getAssetExtension2 rawAssetExtension refinement =
        match refinement with
        | PsdToPng -> "png"
        | OldSchool -> rawAssetExtension

    let private getAssetExtension usingRawAssets rawAssetExtension refinements =
        if usingRawAssets then List.fold getAssetExtension2 rawAssetExtension refinements
        else rawAssetExtension

    let private tryGetAssetExtension usingRawAssets refinements (node : XmlNode) =
        match node.Attributes.GetNamedItem Constants.Xml.ExtensionAttributeName with
        | null -> None
        | rawAssetExtension -> 
            let extension = getAssetExtension usingRawAssets rawAssetExtension.InnerText refinements
            Some extension

    let private getAssetSearchOption (node : XmlNode) =
        match node.Attributes.GetNamedItem Constants.Xml.RecursiveAttributeName with
        | null -> SearchOption.TopDirectoryOnly
        | isRecursive -> if isRecursive.InnerText = acstring true then SearchOption.AllDirectories else SearchOption.TopDirectoryOnly

    let private getAssetName filePath (node : XmlNode) =
        match node.Attributes.GetNamedItem Constants.Xml.NameAttributeName with
        | null -> Path.GetFileNameWithoutExtension filePath
        | name -> name.InnerText

    let private getAssetRefinements (node : XmlNode) =
        match node.Attributes.GetNamedItem Constants.Xml.RefinementsAttributeName with
        | null -> []
        | refinements ->
            let refinements = AlgebraicDescriptor.convertFromString refinements.InnerText typeof<string list> :?> string list
            List.map Refinement.fromString refinements

    let private writeMagickImageAsPng filePath (image : MagickImage) =
        match Path.GetExtension filePath with
        | ".png" ->
            use stream = File.OpenWrite filePath
            image.Write (stream, MagickFormat.Png32)
        | _ -> failwith ^ "Invalid image file format for refinement '" + acstring OldSchool + "'; must be of *.png format."

    /// Attempt to load an asset from the given Xml node.
    let tryLoadAssetFromAssetNode (node : XmlNode) =
        match tryGetAssetFilePath node with
        | Some filePath ->
            let name = getAssetName filePath node
            let associations = getAssetAssociations node
            let refinements = getAssetRefinements node
            Some
                { AssetTag = { PackageName = node.ParentNode.Name; AssetName = name }
                  FilePath = filePath
                  Refinements = refinements
                  Associations = associations }
        | None -> None

    /// Attempt to load assets from the given Xml node.
    let tryLoadAssetsFromAssetsNode usingRawAssets (node : XmlNode) =
        match tryGetAssetDirectory node with
        | Some directory ->
            let searchOption = getAssetSearchOption node
            let associations = getAssetAssociations node
            let refinements = getAssetRefinements node
            match tryGetAssetExtension usingRawAssets refinements node with
            | Some extension ->
                try let filePaths = Directory.GetFiles (directory, "*." + extension, searchOption)
                    let assets =
                        Array.map
                            (fun filePath ->
                                { AssetTag = { PackageName = node.ParentNode.Name; AssetName = Path.GetFileNameWithoutExtension filePath }
                                  FilePath = filePath
                                  Associations = associations
                                  Refinements = refinements })
                            filePaths
                    Some ^ List.ofArray assets
                with exn -> debug ^ "Invalid directory '" + directory + "'."; None
            | None -> None
        | None -> None

    /// Attempt to load all the assets from a package Xml node.
    let tryLoadAssetsFromPackageNode usingRawAssets optAssociation (node : XmlNode) =
        let assets =
            enumerable node.ChildNodes |>
            flip Seq.fold [] (fun assetsRev (assetNode : XmlNode) ->
                match assetNode.Name with
                | Constants.Xml.AssetNodeName ->
                    match tryLoadAssetFromAssetNode assetNode with
                    | Some asset -> asset :: assetsRev
                    | None -> debug ^ "Invalid asset node in '" + node.Name + "' in asset graph."; assetsRev
                | Constants.Xml.AssetsNodeName ->
                    match tryLoadAssetsFromAssetsNode usingRawAssets assetNode with
                    | Some loadedAssets -> List.rev loadedAssets @ assetsRev
                    | None -> debug ^ "Invalid assets node in '" + node.Name + "' in asset graph."; assetsRev
                | Constants.Xml.CommentNodeName -> assetsRev
                | invalidNodeType -> debug ^ "Invalid package child node type '" + invalidNodeType + "'."; assetsRev) |>
            List.rev
        match optAssociation with
        | Some association -> List.filter (fun asset -> List.exists ((=) association) asset.Associations) assets
        | None -> assets

    /// Attempt to load all the assets from the document root Xml node.
    let tryLoadAssetsFromRootNode usingRawAssets optAssociation (node : XmlNode) =
        let possiblePackageNodes = List.ofSeq ^ enumerable node.ChildNodes
        let packageNodes =
            possiblePackageNodes |>
            flip List.fold [] (fun packageNodesRev (node : XmlNode) ->
                if node.Name = Constants.Xml.PackageNodeName
                then node :: packageNodesRev
                else packageNodesRev) |>
            List.rev
        let assetLists =
            packageNodes |>
            flip List.fold [] (fun assetListsRev packageNode ->
                let assets = tryLoadAssetsFromPackageNode usingRawAssets optAssociation packageNode
                assets :: assetListsRev) |>
            List.rev
        let assets = List.concat assetLists
        Right assets

    /// Attempt to load all the assets from multiple package Xml nodes.
    /// TODO: test this function!
    let tryLoadAssetsFromPackageNodes usingRawAssets optAssociation nodes =
        let packageNames = List.map (fun (node : XmlNode) -> node.Name) nodes
        match packageNames with
        | [] ->
            let packageListing = String.Join ("; ", packageNames)
            Left ^ "Multiple packages have the same names '" + packageListing + "' which is an error."
        | _ :: _ ->
            let eitherPackageAssetLists =
                List.map
                    (fun (node : XmlNode) ->
                        match tryLoadAssetsFromRootNode usingRawAssets optAssociation node with
                        | Right assets -> Right (node.Name, assets)
                        | Left error -> Left error)
                    nodes
            let (errors, assets) = Either.split eitherPackageAssetLists
            match errors with
            | [] -> Right ^ Map.ofList assets
            | _ :: _ -> Left ^ "Error(s) when loading assets '" + String.Join ("; ", errors) + "'."

    /// Attempt to load all the assets from a package.
    let tryLoadAssetsFromPackage usingRawAssets optAssociation packageName (assetGraphFilePath : string) =
        try let document = XmlDocument ()
            document.Load assetGraphFilePath
            match document.[Constants.Xml.RootNodeName] with
            | null -> Left "Root node is missing from asset graph."
            | rootNode ->
                let possiblePackageNodes = List.ofSeq ^ enumerable rootNode.ChildNodes
                let packageNodes =
                    List.filter
                        (fun (node : XmlNode) ->
                            node.Name = Constants.Xml.PackageNodeName &&
                            (node.Attributes.GetNamedItem Constants.Xml.NameAttributeName).InnerText = packageName)
                        possiblePackageNodes
                match packageNodes with
                | [] -> Left ^ "Package node '" + packageName + "' is missing from asset graph."
                | [packageNode] -> Right ^ tryLoadAssetsFromPackageNode usingRawAssets optAssociation packageNode
                | _ :: _ -> Left ^ "Multiple packages with the same name '" + packageName + "' is an error."
        with exn -> Left ^ acstring exn

    /// Attempt to load all the assets from multiple packages.
    /// TODO: test this function!
    let tryLoadAssetsFromPackages usingRawAssets optAssociation packageNames (assetGraphFilePath : string) =
        try let document = XmlDocument ()
            document.Load assetGraphFilePath
            match document.[Constants.Xml.RootNodeName] with
            | null -> Left "Root node is missing from asset graph."
            | rootNode ->
                let possiblePackageNodes = List.ofSeq ^ enumerable rootNode.ChildNodes
                let packageNameSet = Set.ofList packageNames
                let packageNodes =
                    List.filter
                        (fun (node : XmlNode) ->
                            node.Name = Constants.Xml.PackageNodeName &&
                            Set.contains (node.Attributes.GetNamedItem Constants.Xml.NameAttributeName).InnerText packageNameSet)
                        possiblePackageNodes
                tryLoadAssetsFromPackageNodes usingRawAssets optAssociation packageNodes
        with exn -> Left ^ acstring exn

    /// Try to load all the assets from an asset graph document.
    let tryLoadAssetsFromDocument usingRawAssets optAssociation (assetGraphFilePath : string) =
        try let document = XmlDocument ()
            document.Load assetGraphFilePath
            match document.[Constants.Xml.RootNodeName] with
            | null -> Left "Root node is missing from asset graph."
            | rootNode -> tryLoadAssetsFromRootNode usingRawAssets optAssociation rootNode
        with exn -> Left ^ acstring exn

    /// Apply a single refinement to an asset.
    let refineAssetOnce intermediateFileSubpath intermediateDirectory refinementDirectory refinement =

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
    let refineAsset inputFileSubpath inputDirectory refinementDirectory refinements =
        List.fold
            (fun (intermediateFileSubpath, intermediateDirectory) refinement ->
                refineAssetOnce intermediateFileSubpath intermediateDirectory refinementDirectory refinement)
            (inputFileSubpath, inputDirectory)
            refinements

    /// Build all the assets.
    let buildAssets inputDirectory outputDirectory refinementDirectory fullBuild assets =

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
                with _ -> note ^ "Resource lock on '" + outputFilePath + "' has prevented build for asset '" + acstring asset.AssetTag + "'."

    /// Attempt to build all the assets found in the given asset graph.
    let tryBuildAssetGraph inputDirectory outputDirectory refinementDirectory fullBuild assetGraphFilePath =
        
        // attempt to load assets
        let currentDirectory = Directory.GetCurrentDirectory ()
        let eitherAssets =
            try Directory.SetCurrentDirectory inputDirectory
                tryLoadAssetsFromDocument false None assetGraphFilePath    
            finally Directory.SetCurrentDirectory currentDirectory

        // attempt to build assets
        match eitherAssets with
        | Right assets ->
            try Right ^ buildAssets inputDirectory outputDirectory refinementDirectory fullBuild assets
            with exn -> Left ^ acstring exn
        | Left error -> Left error