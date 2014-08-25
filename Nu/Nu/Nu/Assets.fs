namespace Nu
open System
open System.Xml
open Prime
open Nu.NuConstants

[<AutoOpen>]
module AssetsModule =

    /// Describes a game asset, such as a texture, sound, or model in detail.
    /// A serializable value type.
    type [<StructuralEquality; NoComparison>] Asset =
        { Name : string
          FileName : string
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
    ///
    /// A serializable value type.
    type [<StructuralEquality; NoComparison>] Package =
        { Name : string
          AssetNames : string list }

    type [<StructuralEquality; NoComparison>] AssetKey =
        { Name : string
          PackageName : string }

    type 'a AssetMap = Map<string, Map<string, 'a>>

[<RequireQualifiedAccess>]
module Assets =

    let tryLoadAssetFromNode (node : XmlNode) =
        let attributes = node.Attributes
        let optName = attributes.GetNamedItem NameAttributeName
        match optName with
        | null -> None
        | name ->
            let optFileName = attributes.GetNamedItem FileNameAttributeName
            match optFileName with
            | null -> None
            | fileName ->
                let optAssociations = attributes.GetNamedItem AssociationsAttributeName
                match optAssociations with
                | null -> None
                | associations ->
                    let associationList = List.ofArray (associations.InnerText.Split ',')
                    Some { Name = name.InnerText; FileName = fileName.InnerText; Associations = associationList; PackageName = node.ParentNode.Name }

    let tryLoadPackageAssetsFromNode optAssociation (node : XmlNode) =
        let optAssets =
            List.map
                (fun assetNode -> tryLoadAssetFromNode assetNode)
                (List.ofSeq <| enumerable node.ChildNodes)
        let assets = List.definitize optAssets
        debugIf
            (fun () -> List.length assets <> List.length optAssets)
            ("Invalid asset node in '" + node.InnerText + "' in asset graph.")
        let associatedAssets =
            match optAssociation with
            | None -> assets
            | Some association -> List.filter (fun asset -> List.exists ((=) association) asset.Associations) assets
        List.ofSeq associatedAssets

    let tryLoadPackageAssets optAssociation packageName assetGraphFileName =
        try let document = XmlDocument ()
            document.Load (assetGraphFileName : string)
            let optRootNode = document.[RootNodeName]
            match optRootNode with
            | null -> Left ("Root node is missing from asset graph.")
            | rootNode ->
                let possiblePackageNodes = List.ofSeq <| enumerable rootNode.ChildNodes
                let packageNodes =
                    List.filter
                        (fun (node : XmlNode) ->
                            node.Name = PackageNodeName &&
                            (node.Attributes.GetNamedItem NameAttributeName).InnerText = packageName)
                        possiblePackageNodes
                match packageNodes with
                | [] -> Left ("Package node '" + packageName + "' is missing from asset graph.")
                | packageNode :: _ -> Right <| tryLoadPackageAssetsFromNode optAssociation packageNode
        with exn -> Left (string exn)

    let tryLoadAssets optAssociation assetGraphFileName =
        try let document = XmlDocument ()
            document.Load (assetGraphFileName : string)
            let optRootNode = document.[RootNodeName]
            match optRootNode with
            | null -> Left ("Root node is missing from asset graph.")
            | rootNode ->
                let packageNodes =
                    let possiblePackageNodes = List.ofSeq <| enumerable rootNode.ChildNodes
                    let optPackageNodes =
                        List.map
                            (fun (node : XmlNode) -> if node.Name = PackageNodeName then Some node else None)
                            possiblePackageNodes
                    List.definitize optPackageNodes
                let assetLists =
                    List.fold
                        (fun assetLists packageNode ->
                            let assets = tryLoadPackageAssetsFromNode optAssociation packageNode
                            assets :: assetLists)
                        []
                        packageNodes
                let assets = List.concat assetLists
                Right assets
        with exn -> Left <| string exn