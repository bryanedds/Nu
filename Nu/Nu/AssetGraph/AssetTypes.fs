// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Generic
open TiledSharp
open Prime

/// A phantom type to denote the type of an asset as a blob of raw data.
type Raw = private { __ : unit }

/// A phantom type to denote the type of an asset as an image.
type Image = private { __ : unit }

/// A phantom type to denote the type of an asset as a font.
type Font = private { __ : unit }

/// A phantom type to denote the type of an asset as a tile map.
type TileMap = private { __ : unit }

/// A phantom type to denote the type of an asset as a spine skeleton.
type SpineSkeleton = private { __ : unit }

/// A phantom type to denote the type of an asset as a cursor.
type Cursor = private { __ : unit }

/// A phantom type to denote the type of an asset as a cube map.
type CubeMap = private { __ : unit }

/// A phantom type to denote the type of an asset as a sound.
type Sound = private { __ : unit }

/// A phantom type to denote the type of an asset as a song.
type Song = private { __ : unit }

/// A phantom type to denote the type of an asset as a static model.
type StaticModel = private { __ : unit }

/// A phantom type to denote the type of an asset as an animated model.
type AnimatedModel = private { __ : unit }

/// Thrown when a tile set property is not found.
exception TileSetPropertyNotFoundException of string

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
            this.Tilesets
            |> Array.ofSeq
            |> Array.map (fun (tileSet : TmxTileset) -> struct (tileSet, tileSet.GetImageAsset tileMapPackage))