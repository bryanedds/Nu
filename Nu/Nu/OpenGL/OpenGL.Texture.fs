// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace OpenGL
open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open System.Threading
open FSharp.NativeInterop
open SDL
open ImageMagick
open BCnEncoder.Shared.ImageFiles
open AstcEncoder
open Pfim
open Prime
open Nu

// NOTE: on Nu's texturing nomenclature -
// Texture | Texture2d =    2d texture
// Texture3d =              3d texture
// (Cube|_)Map =            cube map
[<RequireQualifiedAccess>]
module Texture =

    /// The compression to use for a texture, if any.
    type TextureCompression =
        | Uncompressed
        | ColorCompression
        | NormalCompression

        /// The OpenGL internal format corresponding to this block compression. This can vary based on
        /// Constants.Render.TextureBlockCompression.
        member this.InternalFormat =
            match this with
            | Uncompressed ->
                OpenGL.InternalFormat.Rgba8
            | ColorCompression ->
                match Constants.Render.TextureBlockCompression with
                | BcCompression -> OpenGL.InternalFormat.CompressedRgbaS3tcDxt5Ext
                | AstcCompression -> OpenGL.InternalFormat.CompressedRgbaAstc4x4
            | NormalCompression ->
                match Constants.Render.TextureBlockCompression with
                | BcCompression -> OpenGL.InternalFormat.CompressedRgRgtc2
                | AstcCompression -> OpenGL.InternalFormat.CompressedRgbaAstc4x4

        /// The OpenGL pixel format corresponding to this block compression. This can vary based on
        /// Constants.Render.TextureBlockCompression.
        member this.PixelFormat =
            match Constants.Render.TextureBlockCompression with
            | BcCompression -> OpenGL.PixelFormat.Bgra
            | AstcCompression -> OpenGL.PixelFormat.Rgba

    /// Infer that an asset with the given file path should be filtered in a 2D rendering context.
    let InferFiltered2d (filePath : string) =
        let name = PathF.GetFileNameWithoutExtension filePath
        name.EndsWith "_f" ||
        name.EndsWith "Filtered"

    /// Infer the type of block compression that an asset with the given file path should utilize.
    let InferCompression (filePath : string) =
        let name = PathF.GetFileNameWithoutExtension filePath
        if  name.EndsWith "_f" ||
            name.EndsWith "_hm" ||
            name.EndsWith "_b" ||
            name.EndsWith "_t" ||
            name.EndsWith "_u" ||
            name.EndsWith "Face" ||
            name.EndsWith "HeightMap" ||
            name.EndsWith "Blend" ||
            name.EndsWith "Tint" ||
            name.EndsWith "Uncompressed" then Uncompressed
        elif
            name.EndsWith "_n" ||
            name.EndsWith "_normal" ||
            name.EndsWith "Normal" then NormalCompression
        else ColorCompression

    /// Detect that a dds file uses a compressed representation.
    let DetectCompressionDds (dds : DdsFile) =
        let format = dds.header.ddsPixelFormat.DxgiFormat
        let formatStr = string format
        formatStr.StartsWith "DxgiFormatBc" ||
        formatStr.StartsWith "DxgiFormatAtc"

    /// Detect that a ktx file uses a compressed representation.
    let DetectCompressionKtx (ktx : KtxFile) =
        let format = ktx.header.GlInternalFormat
        let formatStr = string format
        formatStr.StartsWith "GlCompressed"

    /// Write the binary header of a ktx file.
    /// Implementation based on https://registry.khronos.org/KTX/specs/1.0/ktxspec.v1.html
    let WriteKtxHeader (resolution : Vector2i, mipmapLevels, compressed, writer : BinaryWriter) =
        writer.Write                                // ktx identifier
            [|0xABuy; 0x4Buy; 0x54uy; 0x58uy        //
              0x20uy; 0x31uy; 0x31uy; 0xBBuy        //
              0x0Duy; 0x0Auy; 0x1Auy; 0x0Auy|]      //
        writer.Write 0x04030201u                    // endianness
        writer.Write 0u                             // glType
        writer.Write 1u                             // glTypeSize
        writer.Write 0u                             // glFormat
        if compressed                               // glInternalFormat
        then writer.Write 0x93B0u                   // GL_COMPRESSED_RGBA_ASTC_4x4_KHR
        else writer.Write 0x1908u                   // GL_RGBA
        writer.Write 0x1908u                        // glBaseInternalFormat GL_RGBA
        writer.Write (uint32 resolution.X)          // width
        writer.Write (uint32 resolution.Y)          // height
        writer.Write 1u                             // depth
        writer.Write 0u                             // array elements
        writer.Write 1u                             // faces
        writer.Write (uint32 mipmapLevels)          // mip levels
        writer.Write 0u                             // key-value data size

    /// Attempt to generate uncompressed astc bytes an MagickImage to astc bytes.
    let TryGenerateUncompressedImage (image : MagickImage) =
        let pixelBytes = image.GetPixels().ToByteArray(PixelMapping.RGBA)
        let resolution = v2i (int image.Width) (int image.Height)
        Some (resolution, pixelBytes)

    /// Attempt to generate uncompressed astc mipmap bytes from a MagickImage.
    let TryGenerateUncompressedMipmaps (image : MagickImage) =
        let mutable (width, height) = (image.Width, image.Height)
        let mipmapOpts =
            [while width >= 1u && height >= 1u do
                width <- width / 2u
                height <- height / 2u
                let mip = image.Clone () :?> MagickImage
                mip.Resize (width, height)
                TryGenerateUncompressedImage mip]
        match List.definitizePlus mipmapOpts with
        | (true, mipmaps) -> Some mipmaps
        | (false, _) -> None

    /// Attempt to compress a MagickImage to astc bytes.
    let TryCompressImage (image : MagickImage) =

        // attempt to configure astc encoder
        let pixelBytes = image.GetPixels().ToByteArray(PixelMapping.RGBA)
        let blockSize = 4u
        let mutable config = AstcencConfig ()
        let status = Astcenc.AstcencConfigInit (AstcencProfile.AstcencPrfLdr, blockSize, blockSize, 1u, Astcenc.AstcencPreMedium, Unchecked.defaultof<AstcencFlags>, &config)
        if status = AstcencError.AstcencSuccess then

            // attempt to initialize astc encoder
            let mutable context = AstcencContext ()
            let status = Astcenc.AstcencContextAlloc(ref config, 1u, &context)
            if status = AstcencError.AstcencSuccess then

                // attempt to compress astc image
                let mutable astcImage = AstcencImage (dimX = image.Width, dimY = image.Height, dimZ = 1u, dataType = AstcencType.AstcencTypeU8, data = pixelBytes)
                let swizzle = AstcencSwizzle (r = AstcencSwz.AstcencSwzR, g = AstcencSwz.AstcencSwzG, b = AstcencSwz.AstcencSwzB, a = AstcencSwz.AstcencSwzA)
                let blockCountX = (uint image.Width + blockSize - 1u) / blockSize
                let blockCountY = (uint image.Height + blockSize - 1u) / blockSize
                let compressedLength = blockCountX * blockCountY * 16u
                let compressedData = Array.zeroCreate<byte> (int compressedLength)
                let status = Astcenc.AstcencCompressImage (context, &astcImage, swizzle, compressedData.AsSpan (), 0u)
                if status = AstcencError.AstcencSuccess
                then Some (v2i (int image.Width) (int image.Height), compressedData)
                else None

            // failure
            else None

        // failure
        else None

    /// Attempt to compress astc mipmap bytes from a MagickImage.
    let TryCompressMipmaps (image : MagickImage) =
        let mutable (width, height) = (image.Width, image.Height)
        let mipmapOpts =
            [while width >= 8u && height >= 8u do
                width <- width / 2u
                height <- height / 2u
                let mip = image.Clone () :?> MagickImage
                mip.Resize (width, height)
                TryCompressImage mip]
        match List.definitizePlus mipmapOpts with
        | (true, mipmaps) -> Some mipmaps
        | (false, _) -> None

    /// Attempt to format an uncompressed pfim image texture (non-mipmap).
    let TryFormatUncompressedPfimageTexture (format, height, stride, data : byte array) =
        match format with
        | ImageFormat.Rgb24 ->
            let converted =
                [|let mutable y = 0
                  while y < height do
                    let mutable x = 0
                    while x < stride - 2 do
                        let i = x + stride * y
                        data.[i]; data.[i+1]; data.[i+2]; 255uy
                        x <- x + 3
                    y <- inc y|]
            Some converted
        | ImageFormat.Rgba32 ->
            let converted =
                [|let mutable y = 0
                  while y < height do
                    let mutable x = 0
                    while x < stride - 3 do
                        let i = x + stride * y
                        data.[i]; data.[i+1]; data.[i+2]; data.[i+3]
                        x <- x + 4
                    y <- inc y|]
            Some converted
        | _ -> Log.info ("Unsupported image format '" + scstring format + "'."); None
        
    /// Attempt to format an uncompressed pfim image mipmap.
    let FormatUncompressedPfimageMipmap (format, mipmap : MipMapOffset, data : byte array) =
        match format with
        | ImageFormat.Rgb24 ->
            let converted =
                [|let mutable y = 0
                  while y < mipmap.Height do
                    let mutable x = 0
                    while x < mipmap.Stride - 2 do
                        let i = x + mipmap.Stride * y + mipmap.DataOffset
                        data.[i]; data.[i+1]; data.[i+2]; 255uy
                        x <- x + 3
                    y <- inc y|]
            (v2i mipmap.Width mipmap.Height, converted)
        | ImageFormat.Rgba32 ->
            let converted =
                [|let mutable y = 0
                  while y < mipmap.Height do
                    let mutable x = 0
                    while x < mipmap.Stride - 3 do
                        let i = x + mipmap.Stride * y + mipmap.DataOffset
                        data.[i]; data.[i+1]; data.[i+2]; data.[i+3]
                        x <- x + 4
                    y <- inc y|]
            (v2i mipmap.Width mipmap.Height, converted)
        | _ -> failwithumf ()

    /// Attempt to format an uncompressed pfim image.
    let TryFormatUncompressedPfimage (minimal, image : IImage) =
        let minimal = minimal && image.MipMaps.Length >= 1 // NOTE: at least one mipmap is needed for minimal load.
        let data = image.Data // OPTIMIZATION: pulling all values out of image to avoid slow property calls.
        let format = image.Format
        let height = image.Height
        let stride = image.Stride
        let mipmaps = image.MipMaps
        let bytesOpt =
            if not minimal
            then TryFormatUncompressedPfimageTexture (format, height, stride, data)
            else Some [||]
        match bytesOpt with
        | Some bytes ->
            let minimalMipmapIndex =
                if minimal
                then min (dec mipmaps.Length) (dec Constants.Render.TextureMinimalMipmapIndex)
                else 0
            let mipmapBytesArray =
                [|for i in minimalMipmapIndex .. dec mipmaps.Length do
                    FormatUncompressedPfimageMipmap (format, mipmaps.[i], data)|]
            if minimal then
                let (minimalMipmapResolution, minimalMipmapBytes) = mipmapBytesArray.[0]
                let remainingMipmapBytes = if minimalMipmapBytes.Length > 1 then Array.tail mipmapBytesArray else [||]
                Some (minimalMipmapResolution, minimalMipmapBytes, remainingMipmapBytes)
            else Some (v2i image.Width image.Height, bytes, mipmapBytesArray)
        | None -> None

    /// Attempt to format compressed pfim image data.
    let FormatCompressedPfdds (minimal, dds : Dds) =
        let minimal = minimal && dds.Header.MipMapCount >= 3u // NOTE: at least three mipmaps are needed for minimal load since the last 2 are not valid when compressed.
        let mutable dims = v2i dds.Width dds.Height
        let mutable size = ((dims.X + 3) / 4) * ((dims.Y + 3) / 4) * 16
        let mutable index = 0
        let bytes =
            if not minimal
            then dds.Data.AsSpan(index, size).ToArray()
            else [||]
        let minimalMipmapIndex =
            if minimal
            then min dds.Header.MipMapCount (uint Constants.Render.TextureMinimalMipmapIndex)
            else 1u
        let mipmapBytesArray =
            if dds.Header.MipMapCount >= 2u then
                [|for i in 1u .. dds.Header.MipMapCount do
                    dims <- dims / 2
                    index <- index + size
                    size <- size / 4
                    if  i >= minimalMipmapIndex &&
                        size >= 16 then // NOTE: as mentioned above, mipmap with size < 16 can exist but isn't valid when compressed.
                        (dims, dds.Data.AsSpan(index, size).ToArray())|]
            else [||]
        if minimal then
            let (minimalMipmapResolution, minimalMipmapBytes) = mipmapBytesArray.[0]
            let remainingMipmapBytes = if minimalMipmapBytes.Length > 1 then Array.tail mipmapBytesArray else [||]
            (minimalMipmapResolution, minimalMipmapBytes, remainingMipmapBytes)
        else (v2i dds.Width dds.Height, bytes, mipmapBytesArray)

    /// An OpenGL texture's metadata.
    type TextureMetadata =
        { TextureWidth : int
          TextureHeight : int
          TextureTexelWidth : single
          TextureTexelHeight : single }

        /// Make texture metadata.
        static member make width height =
            { TextureWidth = width
              TextureHeight = height
              TextureTexelWidth = 1.0f / single width
              TextureTexelHeight = 1.0f / single height }

        /// Unpopulated texture data.
        static member empty =
            { TextureWidth = 0
              TextureHeight = 0
              TextureTexelWidth = 0.0f
              TextureTexelHeight = 0.0f }

    /// Describes data loaded from a texture.
    type TextureData =
        | TextureDataDotNet of Metadata : TextureMetadata * Bytes : byte array
        | TextureDataMipmap of Metadata : TextureMetadata * BlockCompressed : bool * Bytes : byte array * Mipmaps : (Vector2i * byte array) array
        | TextureDataNative of Metadata : TextureMetadata * TextureDataPtr : nativeint * Disposer : IDisposable

        /// Whether the texture can be loaded lazily.
        member this.LazyLoadable =
            match this with
            | TextureDataDotNet (_, _) -> false
            | TextureDataMipmap (_, _, _, _) -> true
            | TextureDataNative (_, _, _) -> false

        /// The metadata portion of this texture data.
        member this.Metadata =
            match this with
            | TextureDataDotNet (metadata, _) -> metadata
            | TextureDataMipmap (metadata, _, _, _) -> metadata
            | TextureDataNative (metadata, _, _) -> metadata
        
        /// The texture byte data.
        member this.Bytes =
            match this with
            | TextureDataDotNet (_, bytes) -> (false, bytes)
            | TextureDataMipmap (_, compressed, bytes, _) -> (compressed, bytes)
            | TextureDataNative (metadata, textureDataPtr, _) ->
                let bytes = Array.zeroCreate<byte> (metadata.TextureWidth * metadata.TextureHeight * sizeof<uint>)
                Marshal.Copy (textureDataPtr, bytes, 0, bytes.Length)
                (false, bytes)
        
        /// Manual disposal.
        member this.Dispose () =
            match this with
            | TextureDataDotNet (_, _) -> ()
            | TextureDataMipmap (_, _, _, _) -> ()
            | TextureDataNative (_, _, disposer) -> disposer.Dispose ()

    /// Create an opengl texture from existing texture data.
    /// NOTE: this function will dispose textureData.
    let CreateTextureGlFromData (minFilter, magFilter, anisoFilter, mipmaps, compression : TextureCompression, textureData) =

        // upload data to opengl as appropriate
        match textureData with
        | TextureDataDotNet (metadata, bytes) ->

            // upload dotnet texture data
            let bytesPtr = GCHandle.Alloc (bytes, GCHandleType.Pinned)
            try let textureId = Gl.GenTexture ()
                Gl.BindTexture (TextureTarget.Texture2d, textureId)
                Gl.TexImage2D (TextureTarget.Texture2d, 0, compression.InternalFormat, metadata.TextureWidth, metadata.TextureHeight, 0, compression.PixelFormat, PixelType.UnsignedByte, bytesPtr.AddrOfPinnedObject ())
                Hl.Assert ()
                Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int minFilter)
                Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int magFilter)
                Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.Repeat)
                Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.Repeat)
                if anisoFilter && mipmaps then Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMaxAnisotropy, Constants.Render.TextureAnisotropyMax)
                if mipmaps then Gl.GenerateMipmap TextureTarget.Texture2d
                Gl.BindTexture (TextureTarget.Texture2d, 0u)
                Hl.Assert ()
                (metadata, textureId)
            finally bytesPtr.Free ()

        | TextureDataMipmap (metadata, compressed, bytes, mipmapBytesArray) ->

            // upload block-compressed dotnet texture data
            if compressed then
                if compression.IsUncompressed then Log.info "Potential inadvertent block-compression of texture (place a breakpoint here for more detail)."
                let bytesPtr = GCHandle.Alloc (bytes, GCHandleType.Pinned)
                try let textureId = Gl.GenTexture ()
                    Gl.BindTexture (TextureTarget.Texture2d, textureId)
                    let format = compression.InternalFormat
                    Gl.TexStorage2D (TextureTarget.Texture2d, inc mipmapBytesArray.Length, Branchless.reinterpret format, metadata.TextureWidth, metadata.TextureHeight)
                    Hl.Assert ()
                    Gl.CompressedTexSubImage2D (TextureTarget.Texture2d, 0, 0, 0, metadata.TextureWidth, metadata.TextureHeight, format, bytes.Length, bytesPtr.AddrOfPinnedObject ())
                    Hl.Assert ()
                    let mutable mipmapIndex = 0
                    while mipmapIndex < mipmapBytesArray.Length do
                        let (mipmapResolution, mipmapBytes) = mipmapBytesArray.[mipmapIndex]
                        let mipmapBytesPtr = GCHandle.Alloc (mipmapBytes, GCHandleType.Pinned)
                        try Gl.CompressedTexSubImage2D (TextureTarget.Texture2d, inc mipmapIndex, 0, 0, mipmapResolution.X, mipmapResolution.Y, format, mipmapBytes.Length, mipmapBytesPtr.AddrOfPinnedObject ())
                        finally mipmapBytesPtr.Free ()
                        mipmapIndex <- inc mipmapIndex
                    Hl.Assert ()
                    Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int minFilter)
                    Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int magFilter)
                    Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.Repeat)
                    Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.Repeat)
                    if anisoFilter && (mipmaps || mipmapBytesArray.Length > 0) then
                        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMaxAnisotropy, Constants.Render.TextureAnisotropyMax)
                    if mipmaps && mipmapBytesArray.Length = 0 then
                        Gl.GenerateMipmap TextureTarget.Texture2d
                    Gl.BindTexture (TextureTarget.Texture2d, 0u)
                    Hl.Assert ()
                    (metadata, textureId)
                finally bytesPtr.Free ()

            // upload uncompressed dotnet texture data
            else
                let bytesPtr = GCHandle.Alloc (bytes, GCHandleType.Pinned)
                try let textureId = Gl.GenTexture ()
                    Gl.BindTexture (TextureTarget.Texture2d, textureId)
                    Gl.TexImage2D (TextureTarget.Texture2d, 0, Uncompressed.InternalFormat, metadata.TextureWidth, metadata.TextureHeight, 0, Uncompressed.PixelFormat, PixelType.UnsignedByte, bytesPtr.AddrOfPinnedObject ())
                    Hl.Assert ()
                    let mutable mipmapIndex = 0
                    while mipmapIndex < mipmapBytesArray.Length do
                        let (mipmapResolution, mipmapBytes) = mipmapBytesArray.[mipmapIndex]
                        let mipmapBytesPtr = GCHandle.Alloc (mipmapBytes, GCHandleType.Pinned)
                        try Gl.TexImage2D (TextureTarget.Texture2d, inc mipmapIndex, Uncompressed.InternalFormat, mipmapResolution.X, mipmapResolution.Y, 0, Uncompressed.PixelFormat, PixelType.UnsignedByte, mipmapBytesPtr.AddrOfPinnedObject ())
                        finally mipmapBytesPtr.Free ()
                        mipmapIndex <- inc mipmapIndex
                        Hl.Assert ()
                    Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int minFilter)
                    Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int magFilter)
                    Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.Repeat)
                    Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.Repeat)
                    if anisoFilter && (mipmaps || mipmapBytesArray.Length > 0) then
                        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMaxAnisotropy, Constants.Render.TextureAnisotropyMax)                
                    if mipmaps && mipmapBytesArray.Length = 0 then
                        Gl.GenerateMipmap TextureTarget.Texture2d
                    Gl.BindTexture (TextureTarget.Texture2d, 0u)
                    Hl.Assert ()
                    (metadata, textureId)
                finally bytesPtr.Free ()

        | TextureDataNative (metadata, bytesPtr, disposer) ->

            // upload native texture data
            use _ = disposer
            let textureId = Gl.GenTexture ()
            Gl.BindTexture (TextureTarget.Texture2d, textureId)
            Gl.TexImage2D (TextureTarget.Texture2d, 0, compression.InternalFormat, metadata.TextureWidth, metadata.TextureHeight, 0, compression.PixelFormat, PixelType.UnsignedByte, bytesPtr)
            Hl.Assert ()
            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int minFilter)
            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int magFilter)
            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.Repeat)
            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.Repeat)
            if anisoFilter && mipmaps then
                Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMaxAnisotropy, Constants.Render.TextureAnisotropyMax)
            if mipmaps then
                Gl.GenerateMipmap TextureTarget.Texture2d
            Gl.BindTexture (TextureTarget.Texture2d, 0u)
            (metadata, textureId)

    /// Attempt to create uploadable texture data from the given file path.
    /// Don't forget to dispose the last field when finished with the texture data.
    let TryCreateTextureData (minimal, filePath : string) =
        if File.Exists filePath then

            // attempt to load data as dds (compressed or uncompressed)
            let platform = Environment.OSVersion.Platform
            let fileExtension = PathF.GetExtensionLower filePath
            if fileExtension = ".dds" then
                try let config = PfimConfig (decompress = false)
                    use fileStream = File.OpenRead filePath
                    use dds = Dds.Create (fileStream, config)
                    if dds.Compressed then
                        let (resolution, bytes, mipmapBytesArray) = FormatCompressedPfdds (minimal, dds)
                        let metadata = TextureMetadata.make resolution.X resolution.Y
                        Some (TextureDataMipmap (metadata, true, bytes, mipmapBytesArray))
                    else
                        match TryFormatUncompressedPfimage (minimal, dds) with
                        | Some (resolution, bytes, mipmapBytesArray) ->
                            let metadata = TextureMetadata.make resolution.X resolution.Y
                            Some (TextureDataMipmap (metadata, false, bytes, mipmapBytesArray))
                        | None -> None
                with _ -> None

            // attempt to load data as ktx (compressed or uncompressed)
            elif fileExtension = ".ktx" then
                try use fileStream = File.OpenRead filePath
                    let ktx = KtxFile.Load fileStream
                    let compressed = DetectCompressionKtx ktx
                    let bytesArray =
                        ktx.MipMaps
                        |> Array.ofSeq
                        |> Array.map (fun mip ->
                            let resolution = v2i (int mip.Width) (int mip.Height)
                            let bytes = mip.Faces.[0].Data
                            (resolution, bytes))
                    if minimal && bytesArray.Length > Constants.Render.TextureMinimalMipmapIndex then
                        let bytesArray = Array.skip Constants.Render.TextureMinimalMipmapIndex bytesArray
                        let (resolution, bytes) = Array.head bytesArray
                        let metadata = TextureMetadata.make resolution.X resolution.Y
                        Some (TextureDataMipmap (metadata, compressed, bytes, Array.tail bytesArray))
                    else
                        let (resolution, bytes) = Array.head bytesArray
                        let metadata = TextureMetadata.make resolution.X resolution.Y
                        Some (TextureDataMipmap (metadata, compressed, bytes, Array.tail bytesArray))
                with _ -> None

            // attempt to load data as tga
            elif fileExtension = ".tga" then
                try let image = Pfimage.FromFile filePath
                    match TryFormatUncompressedPfimage (false, image) with
                    | Some (resolution, bytes, _) ->
                        let metadata = TextureMetadata.make resolution.X resolution.Y
                        Some (TextureDataDotNet (metadata, bytes))
                    | None -> None
                with _ -> None

            // attempt to load data as any format supported by Drawing.Bitmap on Windows
            elif platform = PlatformID.Win32NT || platform = PlatformID.Win32Windows then
                let extension = PathF.GetExtensionLower filePath
                match extension with
                | ImageExtension _ ->
                    try let bitmap = new Drawing.Bitmap (filePath)
                        let data = bitmap.LockBits (Drawing.Rectangle (0, 0, bitmap.Width, bitmap.Height), Drawing.Imaging.ImageLockMode.ReadOnly, Drawing.Imaging.PixelFormat.Format32bppArgb)
                        let metadata = TextureMetadata.make bitmap.Width bitmap.Height
                        let scan0 = data.Scan0
                        Some (TextureDataNative (metadata, scan0, { new IDisposable with member this.Dispose () = bitmap.UnlockBits data; bitmap.Dispose () })) // NOTE: calling UnlockBits explicitly since I can't figure out if Dispose does.
                    with _ -> None
                | _ -> None
                
            // attempt to load data as any format supported by SDL_image on any device
            else
                let format = SDL_PixelFormat.SDL_PIXELFORMAT_ARGB8888 // seems to be the right format on Ubuntu...
                let unconvertedPtr = SDL3_image.IMG_Load filePath
                if not (NativePtr.isNullPtr unconvertedPtr) then
                    let unconverted = NativePtr.toByRef unconvertedPtr
                    let metadata = TextureMetadata.make unconverted.w unconverted.h
                    if unconverted.format <> format then
                        let convertedPtr = SDL3.SDL_ConvertSurface (unconvertedPtr, format)
                        let converted = NativePtr.toByRef convertedPtr
                        SDL3.SDL_DestroySurface unconvertedPtr // no longer need this
                        Some (TextureDataNative (metadata, converted.pixels, { new IDisposable with member this.Dispose () = SDL3.SDL_DestroySurface convertedPtr }))
                    else Some (TextureDataNative (metadata, unconverted.pixels, { new IDisposable with member this.Dispose () = SDL3.SDL_DestroySurface unconvertedPtr }))
                else None
        else None

    /// Attempt to create an opengl texture from a file.
    let TryCreateTextureGl (minimal, minFilter, magFilter, anisoFilter, mipmaps, compression : TextureCompression, filePath) =
        match TryCreateTextureData (minimal, filePath) with
        | Some textureData ->
            let (metadata, textureId) = CreateTextureGlFromData (minFilter, magFilter, anisoFilter, mipmaps, compression, textureData)
            Right (metadata, textureId)
        | None -> Left ("Missing file or unloadable texture data '" + filePath + "'.")

    /// A texture that's immediately loaded.
    type [<Struct; NoEquality; NoComparison>] EagerTexture =
        { TextureMetadata : TextureMetadata
          TextureId : uint }

        /// Destroy this texture's backing OpenGL texture.
        member this.Destroy () =
            Gl.DeleteTextures [|this.TextureId|]

    /// A texture that can be loaded from another thread.
    type LazyTexture (filePath : string, minimalMetadata : TextureMetadata, minimalId : uint, fullMinFilter : TextureMinFilter, fullMagFilter : TextureMagFilter, fullAnisoFilter) =

        let [<VolatileField>] mutable fullServeAttempted = false
        let [<VolatileField>] mutable fullServeParameterized = false
        let [<VolatileField>] mutable fullMetadataAndIdOpt = ValueNone
        let [<VolatileField>] mutable destroyed = false
        let destructionLock = obj ()

        (* Client API - only the client may call this! *)

        member this.FilePath =
            if destroyed then failwith "Accessing field of destroyed texture."
            filePath

        member this.TextureMetadata =
            if destroyed then failwith "Accessing field of destroyed texture."
            if fullServeAttempted then
                match fullMetadataAndIdOpt with
                | ValueSome (metadata, _) -> metadata
                | ValueNone -> minimalMetadata
            else minimalMetadata

        member this.TextureId =
            if destroyed then failwith "Accessing field of destroyed texture."
            if fullServeAttempted then
                match fullMetadataAndIdOpt with
                | ValueSome (_, textureId) ->
                    if not fullServeParameterized then
                        Gl.BindTexture (TextureTarget.Texture2d, textureId)
                        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int fullMinFilter)
                        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int fullMagFilter)
                        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.Repeat)
                        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.Repeat)
                        if fullAnisoFilter then Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMaxAnisotropy, Constants.Render.TextureAnisotropyMax)
                        Gl.BindTexture (TextureTarget.Texture2d, 0u)
                        Hl.Assert ()
                        fullServeParameterized <- true
                    textureId
                | ValueNone -> minimalId
            else minimalId

        member this.Destroy () =
            lock destructionLock $ fun () ->
                if not destroyed then
                    Gl.DeleteTextures [|minimalId|]
                    if fullServeAttempted then
                        match fullMetadataAndIdOpt with
                        | ValueSome (_, fullId) ->
                            Gl.DeleteTextures [|fullId|]
                            fullMetadataAndIdOpt <- ValueNone
                        | ValueNone -> ()
                        fullServeAttempted <- false
                    destroyed <- true

        (* Server API - only the server may call this! *)

        member internal this.TryServe () =
            lock destructionLock $ fun () ->
                if not destroyed && not fullServeAttempted then
                    match TryCreateTextureGl (false, TextureMinFilter.LinearMipmapLinear, TextureMagFilter.Linear, fullAnisoFilter, false, InferCompression filePath, filePath) with
                    | Right (metadata, textureId) ->
                        Gl.Finish () // NOTE: this is REQUIRED in order to ensure the texture is fully created before potential use.
                        fullMetadataAndIdOpt <- ValueSome (metadata, textureId)
                    | Left error -> Log.info ("Could not serve lazy texture due to:" + error)
                    fullServeAttempted <- true

    /// A 2d texture.
    type [<CustomEquality; NoComparison>] Texture =
        | EmptyTexture
        | EagerTexture of EagerTexture
        | LazyTexture of LazyTexture

        static member hash texture =
            match texture with
            | EmptyTexture -> 0
            | EagerTexture eagerTexture -> hash eagerTexture.TextureId
            | LazyTexture lazyTexture -> lazyTexture.GetHashCode ()

        static member equals this that =
            match this with
            | EmptyTexture ->
                match that with
                | EmptyTexture -> true
                | _ -> false
            | EagerTexture eagerThis ->
                match that with
                | EagerTexture eagerThat -> eagerThis.TextureId = eagerThat.TextureId
                | _ -> false
            | LazyTexture lazyThis ->
                match that with
                | LazyTexture lazyThat -> lazyThis = lazyThat
                | _ -> false

        member this.TextureMetadata =
            match this with
            | EmptyTexture -> TextureMetadata.empty
            | EagerTexture eagerTexture -> eagerTexture.TextureMetadata
            | LazyTexture lazyTexture -> lazyTexture.TextureMetadata

        member this.TextureId =
            match this with
            | EmptyTexture -> 0u
            | EagerTexture eagerTexture -> eagerTexture.TextureId
            | LazyTexture lazyTexture -> lazyTexture.TextureId

        member this.Destroy () =
            match this with
            | EmptyTexture -> ()
            | EagerTexture eagerTexture -> eagerTexture.Destroy ()
            | LazyTexture lazyTexture -> lazyTexture.Destroy ()

        override this.GetHashCode () =
            Texture.hash this

        override this.Equals that =
            match that with
            | :? Texture as texture -> Texture.equals this texture
            | _ -> false

        interface System.IEquatable<Texture> with
            member this.Equals that =
                Texture.equals this that

    /// Memoizes and optionally threads texture loads.
    type TextureClient (lazyTextureQueuesOpt : ConcurrentDictionary<_, _> option) =
        let textures = Dictionary<string, Texture> HashIdentity.Structural
        let lazyTextureQueue = ConcurrentQueue ()
        do  match lazyTextureQueuesOpt with
            | Some lazyTextureQueues -> lazyTextureQueues.TryAdd (lazyTextureQueue, lazyTextureQueue) |> ignore<bool>
            | None -> ()

        /// Memoized textures.
        member this.Textures = textures

        /// Lazy texture queue.
        member this.LazyTextureQueue = lazyTextureQueue

        /// Attempt to create a memoized texture from a file.
        member this.TryCreateTexture (desireLazy, minFilter, magFilter, anisoFilter, mipmaps, compression, filePath : string) =

            // memoize texture
            match textures.TryGetValue filePath with
            | (false, _) ->

                // attempt to create texture
                match TryCreateTextureGl (desireLazy, minFilter, magFilter, anisoFilter, mipmaps, compression, filePath) with
                | Right (metadata, textureId) ->
                    let texture =
                        if  desireLazy &&
                            (PathF.GetExtensionLower filePath = ".dds" || PathF.GetExtensionLower filePath = ".ktx") then
                            let lazyTexture = new LazyTexture (filePath, metadata, textureId, minFilter, magFilter, anisoFilter)
                            lazyTextureQueue.Enqueue lazyTexture
                            LazyTexture lazyTexture
                        else EagerTexture { TextureMetadata = metadata; TextureId = textureId }
                    textures.Add (filePath, texture)
                    Right texture
                | Left error -> Left error

            // already exists
            | (true, texture) -> Right texture

        /// Attempt to create a filtered memoized texture from a file.
        member this.TryCreateTextureFiltered (desireLazy, compression, filePath) =
            this.TryCreateTexture (desireLazy, TextureMinFilter.LinearMipmapLinear, TextureMagFilter.Linear, true, true, compression, filePath)

        /// Attempt to create an unfiltered memoized texture from a file.
        member this.TryCreateTextureUnfiltered (desireLazy, filePath) =
            this.TryCreateTexture (desireLazy, TextureMinFilter.Nearest, TextureMagFilter.Nearest, false, false, Uncompressed, filePath)

    /// Populated the texture ids and handles of lazy textures in a threaded manner.
    /// TODO: abstract this to interface that can represent either inline or threaded implementation.
    type TextureServer (lazyTextureQueues : ConcurrentDictionary<LazyTexture ConcurrentQueue, LazyTexture ConcurrentQueue>, sharedContext, window) =
        let mutable threadOpt = None
        let [<VolatileField>] mutable started = false
        let [<VolatileField>] mutable terminated = false

        member private this.Run () =
            let glContext = OpenGL.Hl.CreateSglContextSharedWithCurrentContext (window, sharedContext)
            OpenGL.Hl.Assert ()
            started <- true
            while not terminated do
                let batchTime = Stopwatch.StartNew () // NOTE: we stop loading after 1/2 frame passed so far.
                let desiredFrameTimeMinimumMs = GameTime.DesiredFrameTimeMinimum * 1000.0
                let frameTimeOutMs = max 4L (int64 (desiredFrameTimeMinimumMs * 0.5))
                let lazyTextureQueueEnr = lazyTextureQueues.GetEnumerator ()
                while not terminated && batchTime.ElapsedMilliseconds < frameTimeOutMs && lazyTextureQueueEnr.MoveNext () do
                    let lazyTextureQueue = lazyTextureQueueEnr.Current.Key
                    let mutable lazyTexture = Unchecked.defaultof<_>
                    while not terminated && batchTime.ElapsedMilliseconds < frameTimeOutMs && lazyTextureQueue.TryDequeue &lazyTexture do
                        lazyTexture.TryServe ()
                Thread.Sleep (max 1 (int desiredFrameTimeMinimumMs - int batchTime.ElapsedMilliseconds + 1))
            OpenGL.Hl.DestroySglContext (glContext, window)

        member this.Start () =
            if not started then
                let thread = Thread (ThreadStart (fun () -> this.Run ()))
                threadOpt <- Some thread
                thread.IsBackground <- true
                thread.Start ()
                while not started do Thread.Yield () |> ignore<bool>

        member this.Terminate () =
            if started && not terminated then
                let thread = Option.get threadOpt
                terminated <- true
                thread.Join ()
                threadOpt <- None