// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open TiledSharp
open Prime
open Nu

/// An image. Currently just used as a phantom type.
type Image = private { __ : unit }

/// A font. Currently just used as a phantom type.
type Font = private { __ : unit }

/// A tile map. Currently just used as a phantom type.
type TileMap = private { __ : unit }

/// A cube map. Currently just used as a phantom type.
type CubeMap = private { __ : unit }

/// Song. Currently just used as a phantom type.
type Song = private { __ : unit }

/// Sound. Currently just used as a phantom type.
type Sound = private { __ : unit }

/// A static model. Currently just used as a phantom type.
type StaticModel = private { __ : unit }

/// Thrown when a tile set property is not found.
exception TileSetPropertyNotFoundException of string

[<AutoOpen>]
module TmxExtensions =

    // OPTIMIZATION: cache tileset image assets.
    let ImageAssetsMemo = dictPlus<TmxTileset, Image AssetTag> HashIdentity.Structural []

    type TmxTileset with
        member this.GetImageAsset tileMapPackage =
            match ImageAssetsMemo.TryGetValue this with
            | (false, _) ->
                let imageAsset =
                    match this.Properties.TryGetValue "Image" with
                    | (true, imageAssetTagString) ->
                        try scvalue<Image AssetTag> imageAssetTagString
                        with :? KeyNotFoundException ->
                            let errorMessage =
                                "Tileset '" + this.Name + "' missing Image property.\n" +
                                "You must add a Custom Property to the tile set called 'Image' and give it an asset value like '[PackageName AssetName]'.\n" +
                                "This will specify where the engine can find the tile set's associated image asset."
                            raise (TileSetPropertyNotFoundException errorMessage)
                    | (false, _) ->
                        let name = Path.GetFileNameWithoutExtension this.Image.Source
                        asset tileMapPackage name // infer asset tag
                ImageAssetsMemo.Add (this, imageAsset)
                imageAsset
            | (true, imageAssets) -> imageAssets

    type TmxMap with
        member this.GetImageAssets tileMapPackage =
            this.Tilesets |>
            Array.ofSeq |>
            Array.map (fun (tileSet : TmxTileset) -> (tileSet, tileSet.GetImageAsset tileMapPackage))