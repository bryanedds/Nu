// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open TiledSharp
open Prime

/// A raw data asset. Currently just used as a phantom type.
type Raw = private { __ : unit }

/// An image. Currently just used as a phantom type.
type Image = private { __ : unit }

/// A font. Currently just used as a phantom type.
type Font = private { __ : unit }

/// A tile map. Currently just used as a phantom type.
type TileMap = private { __ : unit }

/// A cube map. Currently just used as a phantom type.
type CubeMap = private { __ : unit }

/// Sound. Currently just used as a phantom type.
type Sound = private { __ : unit }

/// Song. Currently just used as a phantom type.
type Song = private { __ : unit }

/// A static model. Currently just used as a phantom type.
type StaticModel = private { __ : unit }

/// An animated model. Currently just used as a phantom type.
type AnimatedModel = private { __ : unit }

/// Thrown when a tile set property is not found.
exception TileSetPropertyNotFoundException of string

[<AutoOpen>]
module AssetPatterns =

    let (|RawExtension|_|) extension = match extension with ".raw" -> Some extension | _ -> None
    let (|ImageExtension|_|) extension = match extension with ".bmp" | ".png" | ".jpg" | ".jpeg" | ".tga" | ".tif" | ".tiff" | ".dds" -> Some extension | _ -> None
    let (|FontExtension|_|) extension = match extension with ".ttf" -> Some extension | _ -> None
    let (|TileMapExtension|_|) extension = match extension with ".tmx" -> Some extension | _ -> None
    let (|CubeMapExtension|_|) extension = match extension with ".cbm" -> Some extension | _ -> None
    let (|SoundExtension|_|) extension = match extension with ".wav" -> Some extension | _ -> None
    let (|SongExtension|_|) extension = match extension with ".ogg" | ".mp3" -> Some extension | _ -> None
    let (|ModelExtension|_|) extension = match extension with ".fbx" | ".dae" | ".obj" -> Some extension | _ -> None
    let (|CsvExtension|_|) extension = match extension with ".csv" -> Some extension | _ -> None

[<AutoOpen>]
module TmxExtensions =

    type TmxTileset with

        /// Get the associated image asset for the given TmxTileset in the specified package.
        /// If the TmxTileset object has a custom property named "Image" with an asset value like "[PackageName AssetName]",
        /// it will be parsed to retrieve the image asset. If the custom property is missing, the image asset will be
        /// inferred from the tile set's image source filename. Throws a TileSetPropertyNotFoundException if the custom
        /// property "Image" is missing and the tile set's image source filename cannot be inferred.
        /// Thread-safe.
        member this.GetImageAsset tileMapPackage =
            let imageAsset =
                match this.Properties.TryGetValue "Image" with
                | (true, imageAssetTagString) ->
                    try scvalueMemo<Image AssetTag> imageAssetTagString
                    with :? KeyNotFoundException ->
                        let errorMessage =
                            "Tileset '" + this.Name + "' missing Image property.\n" +
                            "You must add a Custom Property to the tile set called 'Image' and give it an asset value like '[PackageName AssetName]'.\n" +
                            "This will specify where the engine can find the tile set's associated image asset."
                        raise (TileSetPropertyNotFoundException errorMessage)
                | (false, _) ->
                    let name = PathF.GetFileNameWithoutExtension this.Image.Source
                    asset tileMapPackage name // infer asset tag
            imageAsset

    type TmxMap with

        /// Get each of the map's tilesets paired with their associated image assets (per the given package).
        /// Thread-safe.
        member this.GetImageAssets tileMapPackage =
            this.Tilesets |>
            Array.ofSeq |>
            Array.map (fun (tileSet : TmxTileset) -> struct (tileSet, tileSet.GetImageAsset tileMapPackage))