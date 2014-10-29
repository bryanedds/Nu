namespace Nu
open System
open System.Xml
open System.IO
open ImageMagick
open Prime
open Nu
open Nu.Constants

[<AutoOpen>]
module AssetsModule =

    /// Describes a game asset, such as a texture, sound, or model in detail.
    type [<StructuralEquality; NoComparison>] Asset =
        { Name : string
          FilePath : string
          Refinements : string list
          Associations : string list
          PackageName : string }

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

    /// Attempt to load an asset from the given Xml node.
    let tryLoadAssetFromAssetNode (node : XmlNode) =
        let attributes = node.Attributes
        match attributes.GetNamedItem FileAttributeName with
        | null -> None
        | filePath ->
            let name =
                match attributes.GetNamedItem NameAttributeName with
                | null -> Path.GetFileNameWithoutExtension filePath.InnerText
                | name -> name.InnerText
            let refinements =
                match attributes.GetNamedItem RefinementsAttributeName with
                | null -> []
                | refinements -> (StringListTypeConverter ()).ConvertFromString refinements.InnerText :?> string list
            match attributes.GetNamedItem AssociationsAttributeName with
            | null -> None
            | associations ->
                let associations = (StringListTypeConverter ()).ConvertFromString associations.InnerText :?> string list
                Some {
                    Name = name
                    FilePath = filePath.InnerText
                    Refinements = refinements
                    Associations = associations
                    PackageName = node.ParentNode.Name }

    /// Attempt to load assets from the given Xml node.
    let tryLoadAssetsFromAssetsNode (node : XmlNode) =
        let attributes = node.Attributes
        match attributes.GetNamedItem DirectoryAttributeName with
        | null -> None
        | directoryPath ->
            let searchOption =
                match attributes.GetNamedItem RecursiveAttributeName with
                | null -> SearchOption.TopDirectoryOnly
                | isRecursive -> if isRecursive.InnerText = string true then SearchOption.AllDirectories else SearchOption.TopDirectoryOnly
            match attributes.GetNamedItem ExtensionAttributeName with
            | null -> None
            | extension ->
                let refinements =
                    match attributes.GetNamedItem RefinementsAttributeName with
                    | null -> []
                    | refinements -> (StringListTypeConverter ()).ConvertFromString refinements.InnerText :?> string list
                match attributes.GetNamedItem AssociationsAttributeName with
                | null -> None
                | associations ->
                    let converter = StringListTypeConverter ()
                    let associations = converter.ConvertFromString associations.InnerText :?> string list
                    try let filePaths = Directory.GetFiles (directoryPath.InnerText, "*." + extension.InnerText, searchOption)
                        let assets =
                            Array.map
                                (fun filePath ->
                                    { Name = Path.GetFileNameWithoutExtension filePath
                                      FilePath = filePath
                                      Associations = associations
                                      Refinements = refinements
                                      PackageName = node.ParentNode.Name })
                                filePaths
                        Some <| List.ofArray assets
                    with exn -> debug <| "Invalid folder path '" + directoryPath.InnerText + "'."; None

    /// Attempt to load all the assets from a package Xml node.
    let tryLoadAssetsFromPackageNode optAssociation (node : XmlNode) =
        let assets =
            List.fold
                (fun assets (assetNode : XmlNode) ->
                    match assetNode.Name with
                    | AssetNodeName ->
                        match tryLoadAssetFromAssetNode assetNode with
                        | Some asset -> asset :: assets
                        | None -> debug <| "Invalid asset node in '" + node.Name + "' in asset graph."; assets
                    | AssetsNodeName ->
                        match tryLoadAssetsFromAssetsNode assetNode with
                        | Some loadedAssets -> loadedAssets @ assets
                        | None -> debug <| "Invalid assets node in '" + node.Name + "' in asset graph."; assets
                    | invalidNodeType -> debug <| "Invalid package child node type '" + invalidNodeType + "'."; assets)
                []
                (List.ofSeq <| enumerable node.ChildNodes)
        let associatedAssets =
            match optAssociation with
            | Some association -> List.filter (fun asset -> List.exists ((=) association) asset.Associations) assets
            | None -> assets
        List.ofSeq associatedAssets

    /// Attempt to load all the assets from the document root Xml node.
    let tryLoadAssetsFromRootNode optAssociation (node : XmlNode) =
        let possiblePackageNodes = List.ofSeq <| enumerable node.ChildNodes
        let packageNodes =
            List.fold
                (fun packageNodes (node : XmlNode) ->
                    if node.Name = PackageNodeName then node :: packageNodes
                    else packageNodes)
                []
                possiblePackageNodes
        let assetLists =
            List.fold
                (fun assetLists packageNode ->
                    let assets = tryLoadAssetsFromPackageNode optAssociation packageNode
                    assets :: assetLists)
                []
                packageNodes
        let assets = List.concat assetLists
        Right assets

    /// Attempt to load all the assets from multiple package Xml nodes.
    /// TODO: test this function!
    let tryLoadAssetsFromPackageNodes optAssociation nodes =
        let packageNames = List.map (fun (node : XmlNode) -> node.Name) nodes
        match packageNames with
        | [] ->
            let packageListing = String.Join ("; ", packageNames)
            Left <| "Multiple packages have the same names '" + packageListing + "' which is an error."
        | _ :: _ ->
            let eitherPackageAssetLists =
                List.map
                    (fun (node : XmlNode) ->
                        match tryLoadAssetsFromRootNode optAssociation node with
                        | Right assets -> Right (node.Name, assets)
                        | Left error -> Left error)
                    nodes
            let (errors, assets) = Either.split eitherPackageAssetLists
            match errors with
            | [] -> Right <| Map.ofList assets
            | _ :: _ -> Left <| "Error(s) when loading assets '" + String.Join ("; ", errors) + "'."

    /// Attempt to load all the assets from a package.
    let tryLoadAssetsFromPackage optAssociation packageName (assetGraphFilePath : string) =
        try let document = XmlDocument ()
            document.Load assetGraphFilePath
            match document.[RootNodeName] with
            | null -> Left "Root node is missing from asset graph."
            | rootNode ->
                let possiblePackageNodes = List.ofSeq <| enumerable rootNode.ChildNodes
                let packageNodes =
                    List.filter
                        (fun (node : XmlNode) ->
                            node.Name = PackageNodeName &&
                            (node.Attributes.GetNamedItem NameAttributeName).InnerText = packageName)
                        possiblePackageNodes
                match packageNodes with
                | [] -> Left <| "Package node '" + packageName + "' is missing from asset graph."
                | [packageNode] -> Right <| tryLoadAssetsFromPackageNode optAssociation packageNode
                | _ :: _ -> Left <| "Multiple packages with the same name '" + packageName + "' is an error."
        with exn -> Left <| string exn

    /// Attempt to load all the assets from multiple packages.
    /// TODO: test this function!
    let tryLoadAssetsFromPackages optAssociation packageNames (assetGraphFilePath : string) =
        try let document = XmlDocument ()
            document.Load assetGraphFilePath
            match document.[RootNodeName] with
            | null -> Left "Root node is missing from asset graph."
            | rootNode ->
                let possiblePackageNodes = List.ofSeq <| enumerable rootNode.ChildNodes
                let packageNameSet = Set.ofList packageNames
                let packageNodes =
                    List.filter
                        (fun (node : XmlNode) ->
                            node.Name = PackageNodeName &&
                            Set.contains (node.Attributes.GetNamedItem NameAttributeName).InnerText packageNameSet)
                        possiblePackageNodes
                tryLoadAssetsFromPackageNodes optAssociation packageNodes
        with exn -> Left <| string exn

    /// Try to load all the assets from an asset graph document.
    let tryLoadAssetsFromDocument optAssociation (assetGraphFilePath : string) =
        try let document = XmlDocument ()
            document.Load assetGraphFilePath
            match document.[RootNodeName] with
            | null -> Left "Root node is missing from asset graph."
            | rootNode -> tryLoadAssetsFromRootNode optAssociation rootNode
        with exn -> Left <| string exn

    /// Build all the assets.
    let buildAssets inputDirectory tempDirectory outputDirectory fullBuild assets =
        
        // right now this function only copies if newer
        for asset in assets do
            
            // find asset file path
            let assetFilePath = Path.Combine (inputDirectory, asset.FilePath)
            
            // set up output directory
            let outputFilePath = Path.Combine (outputDirectory, asset.FilePath)
            let outputDirectoryName = Path.GetDirectoryName outputFilePath
            ignore <| Directory.CreateDirectory outputDirectoryName
            
            // process file if it exists and is newer
            if  fullBuild || 
                not <| File.Exists outputFilePath ||
                File.GetLastWriteTimeUtc assetFilePath > File.GetLastWriteTimeUtc outputFilePath then
                match asset.Refinements with
                | [PsdToPngRefinementName] ->

                    // set up temp directory
                    let tempFilePath = Path.Combine (tempDirectory, PsdToPngRefinementName, asset.FilePath)
                    let tempDirectoryName = Path.GetDirectoryName tempFilePath
                    ignore <| Directory.CreateDirectory tempDirectoryName

                    // convert from pdn to png format
                    let convertedTempFilePath = Path.ChangeExtension (tempFilePath, "png")
                    let convertedOutputFilePath = Path.ChangeExtension (outputFilePath, "png")
                    use image = new MagickImage (assetFilePath)
                    image.Write convertedTempFilePath

                    // copy file to output directory
                    File.Copy (convertedTempFilePath, convertedOutputFilePath, true)

                | _ -> File.Copy (assetFilePath, outputFilePath, true)

    /// Attempt to build all the assets found in the given asset graph.
    let tryBuildAssetGraph inputDirectory tempDirectory outputDirectory fullBuild assetGraphFilePath =
        let currentDirectory = Directory.GetCurrentDirectory ()
        let eitherAssets =
            try Directory.SetCurrentDirectory inputDirectory
                tryLoadAssetsFromDocument None assetGraphFilePath    
            finally Directory.SetCurrentDirectory currentDirectory
        match eitherAssets with
        | Right assets ->
            try Right <| buildAssets inputDirectory tempDirectory outputDirectory fullBuild assets
            with exn -> Left <| string exn
        | Left error -> Left error