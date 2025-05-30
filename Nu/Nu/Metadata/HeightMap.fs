// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Buffers.Binary
open System.IO
open System.Numerics
open Prime

/// The endianness which indicates byte order in a raw asset.
type [<Struct>] Endianness =
    | LittleEndian
    | BigEndian

/// The format of a raw asset.
/// NOTE: [<Struct>] attribute was removed in order to work around an F# regression where calling GetUnionFields on this
/// type's cases can result in an InvalidOperationException "Multiple CompilationMappingAttributes, expected at most
/// one".
/// TODO: apparently, one work-around for this regression is to update to the latest FSharp.Core as of the regression,
/// version 9.0.3 (or higher), so let's do this when it makes sense then restore the Struct attribute.
type RawFormat =
    | RawUInt8
    | RawUInt16 of Endianness : Endianness
    | RawUInt32 of Endianness : Endianness
    | RawSingle of Endianness : Endianness

/// A height map for 3d terrain constructed from a raw asset.
type [<Struct>] RawHeightMap =
    { Resolution : Vector2i
      RawFormat : RawFormat
      RawAsset : Raw AssetTag }

type HeightMapMetadata =
    { Resolution : Vector2i
      HeightsNormalized : single array
      PositionsAndTexCoordses : struct (Vector3 * Vector2) array }

/// A height map for terrain.
type [<StructuralEquality; NoComparison>] HeightMap =
    | ImageHeightMap of Image AssetTag // only supports 8-bit depth on Red channel
    | RawHeightMap of RawHeightMap

    static member private tryGetTextureData tryGetAssetFilePath (assetTag : Image AssetTag) =
        match tryGetAssetFilePath assetTag with
        | Some filePath ->
            match OpenGL.Texture.TryCreateTextureData (false, filePath) with
            | Some textureData ->
                let metadata = textureData.Metadata
                let (blockCompressed, bytes) = textureData.Bytes
                textureData.Dispose ()
                ValueSome (metadata, blockCompressed, bytes)
            | None -> ValueNone
        | None -> ValueNone

    static member private tryGetRawAssetData tryGetAssetFilePath (assetTag : Raw AssetTag) =
        match tryGetAssetFilePath assetTag with
        | Some filePath ->
            try let bytes = File.ReadAllBytes filePath
                ValueSome bytes
            with exn ->
                Log.info ("Could not load texture '" + filePath + "' due to: " + scstring exn)
                ValueNone
        | None -> ValueNone

    static member private tryGetImageHeightMapMetadata tryGetAssetFilePath (bounds : Box3) tiles image =

        // attempt to load texture data
        match HeightMap.tryGetTextureData tryGetAssetFilePath image with
        | ValueSome (metadata, blockCompressed, bytes) ->

            // currently only supporting height data from block-compressed files
            if not blockCompressed then

                // compute normalize heights
                let resolutionX = metadata.TextureWidth
                let resolutionY = metadata.TextureHeight
                let scalar = 1.0f / single Byte.MaxValue
                let heightsNormalized =
                    [|for y in 0 .. dec resolutionY do
                        for x in 0 .. dec resolutionX do
                            let index = (resolutionX * y + x) * 4 + 2 // extract r channel of pixel
                            single bytes[index] * scalar|]

                // compute positions and tex coordses
                let quadSizeX = bounds.Size.X / single (dec resolutionX)
                let quadSizeY = bounds.Size.Z / single (dec resolutionY)
                let terrainHeight = bounds.Size.Y
                let terrainPositionX = bounds.Min.X
                let terrainPositionY = bounds.Min.Y
                let terrainPositionZ = bounds.Min.Z
                let texelWidth = 1.0f / single resolutionX
                let texelHeight = 1.0f / single resolutionY
                let positionsAndTexCoordses =
                    [|for y in 0 .. dec resolutionY do
                        for x in 0 .. dec resolutionX do
                            let normalized = heightsNormalized.[y * resolutionX + x]
                            let position = v3 (single x * quadSizeX + terrainPositionX) (normalized * terrainHeight + terrainPositionY) (single y * quadSizeY + terrainPositionZ)
                            let texCoords = v2 (single x * texelWidth) (single y * texelHeight) * tiles
                            struct (position, texCoords)|]

                // fin
                ValueSome { Resolution = v2i resolutionX resolutionY; HeightsNormalized = heightsNormalized; PositionsAndTexCoordses = positionsAndTexCoordses }
            else Log.info "Block-compressed image files are unsupported for use as height maps."; ValueNone
        | ValueNone -> ValueNone

    static member private tryGetRawHeightMapMetadata tryGetAssetFilePath (bounds : Box3) tiles map =

        // ensure raw asset exists
        match HeightMap.tryGetRawAssetData tryGetAssetFilePath map.RawAsset with
        | ValueSome rawAsset ->

            try // read normalized heights
                let resolutionX = map.Resolution.X
                let resolutionY = map.Resolution.Y
                let quadSizeX = bounds.Size.X / single (dec resolutionX)
                let quadSizeY = bounds.Size.Z / single (dec resolutionY)
                let terrainHeight = bounds.Size.Y
                let terrainPositionX = bounds.Min.X
                let terrainPositionY = bounds.Min.Y
                let terrainPositionZ = bounds.Min.Z
                let texelWidth = 1.0f / single resolutionX
                let texelHeight = 1.0f / single resolutionY
                use rawMemory = new MemoryStream (rawAsset)
                use rawReader = new BinaryReader (rawMemory)
                let heightsNormalized =
                    [|match map.RawFormat with
                      | RawUInt8 ->
                        let scalar = 1.0f / single Byte.MaxValue
                        for _ in 0 .. dec (resolutionY * resolutionX) do
                            single (rawReader.ReadByte ()) * scalar
                      | RawUInt16 endianness ->
                        let scalar = 1.0f / single UInt16.MaxValue
                        for _ in 0 .. dec (resolutionY * resolutionX) do
                            let value =
                                match endianness with
                                | LittleEndian -> BinaryPrimitives.ReadUInt16LittleEndian (rawReader.ReadBytes 2)
                                | BigEndian -> BinaryPrimitives.ReadUInt16BigEndian (rawReader.ReadBytes 2)
                            single value * scalar
                      | RawUInt32 endianness ->
                        let scalar = 1.0f / single UInt32.MaxValue
                        for _ in 0 .. dec (resolutionY * resolutionX) do
                            let value =
                                match endianness with
                                | LittleEndian -> BinaryPrimitives.ReadUInt32LittleEndian (rawReader.ReadBytes 4)
                                | BigEndian -> BinaryPrimitives.ReadUInt32BigEndian (rawReader.ReadBytes 4)
                            single value * scalar
                      | RawSingle endianness ->
                        for _ in 0 .. dec (resolutionY * resolutionX) do
                            let value =
                                match endianness with
                                | LittleEndian -> BinaryPrimitives.ReadSingleLittleEndian (rawReader.ReadBytes 4)
                                | BigEndian -> BinaryPrimitives.ReadSingleBigEndian (rawReader.ReadBytes 4)
                            value|]

                // compute positions and tex coordses
                let positionsAndTexCoordses =
                    [|for y in 0 .. dec resolutionY do
                        for x in 0 .. dec resolutionX do
                            let normalized = heightsNormalized.[y * resolutionX + x]
                            let position = v3 (single x * quadSizeX + terrainPositionX) (normalized * terrainHeight + terrainPositionY) (single y * quadSizeY + terrainPositionZ)
                            let texCoords = v2 (single x * texelWidth) (1.0f - single y * texelHeight) * tiles // NOTE: flip y because textures are loaded 'upside-down'
                            struct (position, texCoords)|]

                // fin
                ValueSome { Resolution = v2i resolutionX resolutionY; HeightsNormalized = heightsNormalized; PositionsAndTexCoordses = positionsAndTexCoordses }
            with exn -> Log.infoOnce ("Attempt to read raw height map failed with the following exception: " + exn.Message); ValueNone
        | ValueNone -> ValueNone

    /// Attempt to compute height map metadata, loading assets as required.
    /// NOTE: if the heightmap pixel represents a quad in the terrain geometry in the exporting program, the geometry
    /// produced here is slightly different, with the border slightly clipped, and the terrain and quad size, slightly
    /// larger. i.e if the original map is 32m^2 and the original quad 1m^2 and the heightmap is 32x32, the quad axes
    /// below will be > 1.0.
    static member tryGetMetadata (tryGetAssetFilePath : AssetTag -> string option) bounds tiles heightMap =
        match heightMap with
        | ImageHeightMap image -> HeightMap.tryGetImageHeightMapMetadata tryGetAssetFilePath bounds tiles image
        | RawHeightMap map -> HeightMap.tryGetRawHeightMapMetadata tryGetAssetFilePath bounds tiles map