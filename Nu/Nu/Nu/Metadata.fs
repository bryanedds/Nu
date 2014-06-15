namespace Nu
open System
open System.ComponentModel
open System.Collections.Generic
open System.Drawing // TODO: see if this dependency can be elegantly removed or replaced with something lighter
open System.IO
open System.Linq
open System.Xml
open OpenTK
open TiledSharp
open Prime
open Nu

[<AutoOpen>]
module MetadataModule =

    type [<StructuralEquality; NoComparison>] AssetMetadata =
        | TextureMetadata of int * int
        | TileMapMetadata of string * Sprite list * TmxMap
        | SoundMetadata
        | SongMetadata
        | OtherMetadata of obj
        | InvalidMetadata of string

    type AssetMetadataMap =
        Map<string, Map<string, AssetMetadata>>

    exception TileSetPropertyNotFoundException of string

[<RequireQualifiedAccess>]
module Metadata =

    let getTileSetProperties (tileSet : TmxTileset) =
        let properties = tileSet.Properties
        try { SpriteAssetName = properties.["SpriteAssetName"]
              PackageName = properties.["PackageName"]
              PackageFileName = properties.["PackageFileName"] }
        with :? KeyNotFoundException ->
            let errorMessage = "TileSet '" + tileSet.Name + "' missing one or more properties (SpriteAssetName, PackageName, and / or PackageFileName)."
            raise <| TileSetPropertyNotFoundException errorMessage

    let tryGenerateAssetMetadataMap assetGraphFileName =
        try let document = XmlDocument ()
            document.Load (assetGraphFileName : string) 
            let optRootNode = document.["root"]
            match optRootNode with
            | null -> Left <| "Root node is missing from asset graph file '" + assetGraphFileName + "'."
            | rootNode ->
                let possiblePackageNodes = List.ofSeq <| rootNode.OfType<XmlNode> ()
                let packageNodes = List.filter (fun (node : XmlNode) -> node.Name = "package") possiblePackageNodes
                let assetMetadataMap =
                    List.fold
                        (fun assetMetadataMap (packageNode : XmlNode) ->
                            let packageName = (packageNode.Attributes.GetNamedItem "name").InnerText
                            let optAssets =
                                List.map
                                    (fun assetNode -> Assets.tryLoadAsset packageName assetNode)
                                    (List.ofSeq <| packageNode.OfType<XmlNode> ())
                            let assets = List.definitize optAssets
                            debugIf (fun () -> assets.Count () <> optAssets.Count ()) <| "Invalid asset node in '" + packageName + "' in '" + assetGraphFileName + "'."
                            let subMap =
                                Map.ofListBy
                                    (fun (asset : Asset) ->
                                        let extension = Path.GetExtension asset.FileName
                                        let metadata =
                                            match extension with
                                            | ".bmp"
                                            | ".png" ->
                                                if not <| File.Exists asset.FileName then
                                                    let errorMessage = "Failed to load Bitmap due to missing file '" + asset.FileName + "'."
                                                    trace errorMessage
                                                    InvalidMetadata errorMessage
                                                else
                                                    try use bitmap = new Bitmap (asset.FileName) in TextureMetadata (bitmap.Width, bitmap.Height)
                                                    with _ as e ->
                                                        let errorMessage = "Failed to load Bitmap '" + asset.FileName + "' due to '" + string e + "'."
                                                        trace errorMessage
                                                        InvalidMetadata errorMessage
                                            | ".tmx" ->
                                                try let tmxMap = TmxMap asset.FileName
                                                    let tileSets = List.ofSeq tmxMap.Tilesets
                                                    let tileSetSprites = List.map getTileSetProperties tileSets
                                                    TileMapMetadata (asset.FileName, tileSetSprites, tmxMap)
                                                with _ as e ->
                                                    let errorMessage = "Failed to load TmxMap '" + asset.FileName + "' due to '" + string e + "'."
                                                    trace errorMessage
                                                    InvalidMetadata errorMessage
                                            | ".wav" -> SoundMetadata
                                            | ".ogg" -> SongMetadata
                                            | _ -> InvalidMetadata <| "Could not load asset metadata '" + string asset + "' due to unknown extension '" + extension + "'."
                                        (asset.Name, metadata))
                                    assets
                            Map.add packageName subMap assetMetadataMap)
                        Map.empty
                        packageNodes
                Right assetMetadataMap
        with exn -> Left <| string exn
  
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
        | Some metadata -> Some <| Vector2 (single <| fst metadata, single <| snd metadata)

    let getTextureSizeAsVector2 assetName packageName assetMetadataMap =
        Option.get <| tryGetTextureSizeAsVector2 assetName packageName assetMetadataMap

    let tryGetTileMapMetadata assetName packageName assetMetadataMap =
        let optAsset = tryGetMetadata assetName packageName assetMetadataMap
        match optAsset with
        | None -> None
        | Some (TileMapMetadata (fileName, sprites, tmxMap)) -> Some (fileName, sprites, tmxMap)
        | _ -> None

    let getTileMapMetadata assetName packageName assetMetadataMap =
        Option.get <| tryGetTileMapMetadata assetName packageName assetMetadataMap