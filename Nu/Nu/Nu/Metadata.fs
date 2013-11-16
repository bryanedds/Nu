namespace Nu
open System
open System.ComponentModel
open System.Drawing // TODO: see if this dependency can be elegantly removed or replaced with something lighter
open System.IO
open System.Linq
open System.Xml
open OpenTK
open TiledSharp
open Nu.Assets
open Nu.Audio
open Nu.Rendering

type [<StructuralEquality; NoComparison>] AssetMetadata =
    | TextureMetadata of int * int
    | TileMapMetadata of string * Sprite list
    | SoundMetadata
    | SongMetadata
    | OtherMetadata of obj
    | InvalidMetadata of string

type AssetMetadataMap =
    Map<Lun, Map<Lun, AssetMetadata>>

module Metadata =

    let tryGenerateAssetMetadataMap (assetGraphFileName : string) =
        try let document = XmlDocument ()
            document.Load assetGraphFileName
            let optRootNode = document.["root"]
            match optRootNode with
            | null -> Left ("Root node is missing from asset graph file '" + assetGraphFileName + "'.")
            | rootNode ->
                let possiblePackageNodes = List.ofSeq <| rootNode.OfType<XmlNode> ()
                let packageNodes = List.filter (fun (node : XmlNode) -> node.Name = "package") possiblePackageNodes
                let assetMetadataMap =
                    List.fold
                        (fun assetMetadataMap' (packageNode : XmlNode) ->
                            let packageName = (packageNode.Attributes.GetNamedItem "name").InnerText
                            let optAssets = List.map (fun assetNode -> tryLoadAsset packageName assetNode) (List.ofSeq <| packageNode.OfType<XmlNode> ())
                            let assets = List.definitize optAssets
                            debugIf (fun () -> assets.Count () <> optAssets.Count ()) ("Invalid asset node in '" + packageName + "' in '" + assetGraphFileName + "'.")
                            let subMap =
                                Map.ofListBy
                                    (fun (asset : Asset) ->
                                        let extension = Path.GetExtension asset.FileName
                                        let metadata =
                                            match extension with
                                            | ".bmp"
                                            | ".png" -> use bitmap = new Bitmap (asset.FileName) in TextureMetadata (bitmap.Width, bitmap.Height)
                                            | ".tmx" ->
                                                let tmxMap = new TmxMap (asset.FileName)
                                                let tileSets = List.ofSeq tmxMap.Tilesets
                                                let tileSetSprites =
                                                    List.map
                                                        (fun (tileSet : TmxTileset) ->
                                                            let tileSetProperties = tileSet.Properties
                                                            { SpriteAssetName = Lun.make tileSetProperties.["SpriteAssetName"]
                                                              PackageName = Lun.make tileSetProperties.["PackageName"]
                                                              PackageFileName = tileSetProperties.["PackageFileName"] })
                                                        tileSets
                                                TileMapMetadata (asset.FileName, tileSetSprites)
                                            | ".wav" -> SoundMetadata
                                            | ".ogg" -> SongMetadata
                                            | _ -> InvalidMetadata ("Could not load asset metadata '" + str asset + "' due to unknown extension '" + extension + "'.")
                                        (Lun.make asset.Name, metadata))
                                    assets
                            Map.add (Lun.make packageName) subMap assetMetadataMap')
                        Map.empty
                        packageNodes
                Right assetMetadataMap
        with exn -> Left (str exn)
  
    let tryGetMetadata assetName packageName assetMetadataMap =
        let optPackage = Map.tryFind packageName assetMetadataMap
        match optPackage with
        | None -> None
        | Some package ->
            let optAsset = Map.tryFind assetName package
            match optAsset with
            | None -> None
            | Some _ as asset -> asset

    let tryGetTextureMetadata assetName packageName assetMetadataMap =
        let optAsset = tryGetMetadata assetName packageName assetMetadataMap
        match optAsset with
        | None -> None
        | Some (TextureMetadata (width, height)) -> Some (width, height)
        | _ -> None

    let tryGetTextureSizeAsVector2 assetName packageName assetMetadataMap =
        let optMetadata = tryGetTextureMetadata assetName packageName assetMetadataMap
        match optMetadata with
        | None -> None
        | Some metadata -> Some (Vector2 (single <| fst metadata, single <| snd metadata))

    let getTextureSizeAsVector2 assetName packageName assetMetadataMap =
        Option.get <| tryGetTextureSizeAsVector2 assetName packageName assetMetadataMap

    let tryGetTileMapMetadata assetName packageName assetMetadataMap =
        let optAsset = tryGetMetadata assetName packageName assetMetadataMap
        match optAsset with
        | None -> None
        | Some (TileMapMetadata (fileName, sprites)) -> Some (fileName, sprites)
        | _ -> None

    let getTileMapMetadata assetName packageName assetMetadataMap =
        Option.get <| tryGetTileMapMetadata assetName packageName assetMetadataMap