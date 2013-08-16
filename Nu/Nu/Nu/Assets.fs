module Assets
open System
open System.Linq
open System.Xml

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

let tryLoadAsset packageName (xmlNode : XmlNode) =
    let xmlAttributes = xmlNode.Attributes
    let optName = xmlAttributes.GetNamedItem "name"
    match optName with
    | null -> None
    | name ->
        let optFileName = xmlAttributes.GetNamedItem "fileName"
        match optFileName with
        | null -> None
        | fileName ->
            let optAssociations = xmlAttributes.GetNamedItem "associations"
            match optAssociations with
            | null -> None
            | associations ->
                let associationList = List.ofArray (associations.InnerText.Split ',')
                Some { Name = name.InnerText; FileName = fileName.InnerText; Associations = associationList; PackageName = packageName }

let tryLoadAssets association packageName (assetGraphFileName : string) =
    try let document = XmlDocument ()
        document.Load assetGraphFileName
        let optRootNode = document.["root"]
        match optRootNode with
        | null -> Left ("Root node is missing from asset graph file '" + assetGraphFileName + "'.")
        | rootNode ->
            let possiblePackageNodes = rootNode.OfType<XmlNode> ()
            let optPackageNodes = Seq.filter (fun (node : XmlNode) -> (node.Attributes.GetNamedItem "name").InnerText = packageName) possiblePackageNodes
            let optPackageNode = Seq.tryHead optPackageNodes
            match optPackageNode with
            | None -> Left ("Package node '" + packageName + "' is missing from asset graph file '" + assetGraphFileName + "'.")
            | Some packageNode ->
                let optAssets = Seq.map (fun assetNode -> tryLoadAsset packageName assetNode) (packageNode.OfType<XmlNode> ())
                let assets = Seq.definitize optAssets
                if assets.Count () <> optAssets.Count () then Left ("Invalid asset node in '" + packageName + "' in '" + assetGraphFileName + "'.")
                else
                    let associatedAssets = Seq.filter (fun asset -> asset.Associations.Contains association) assets
                    let associatedAssetList = List.ofSeq associatedAssets
                    Right associatedAssetList
    with exn -> Left (str exn)