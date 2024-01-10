// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OpenGL
open System
open System.Collections.Generic
open System.IO
open System.Runtime.InteropServices
open SDL2
open Pfim
open Prime
open Nu

// NOTE: on Nu's texturing nomenclature -
// Texture | Texture2d =    2d texture
// Texture3d =              3d texture
// (Cube|_)Map =            cube map
[<RequireQualifiedAccess>]
module Texture =

    /// An OpenGL texture.
    type [<Struct>] Texture =
        { TextureId : uint
          TextureHandle : uint64 }

    /// An OpenGL texture's metadata.
    type TextureMetadata =
        { TextureWidth : int
          TextureHeight : int
          TextureTexelWidth : single
          TextureTexelHeight : single
          TextureInternalFormat : InternalFormat }

        /// Unpopulated texture data.
        static member empty =
            { TextureWidth = 0
              TextureHeight = 0
              TextureTexelWidth = 0.0f
              TextureTexelHeight = 0.0f
              TextureInternalFormat = Unchecked.defaultof<InternalFormat> }

    /// Describes data loaded from a texture.
    type TextureData =
        | TextureDataDotNet of TextureMetadata * byte array
        | TextureDataMipmap of TextureMetadata * byte array * (Vector2i * byte array) array
        | TextureDataNative of TextureMetadata * nativeint * IDisposable
        member this.Metadata =
            match this with
            | TextureDataDotNet (metadata, _) -> metadata
            | TextureDataMipmap (metadata, _, _) -> metadata
            | TextureDataNative (metadata, _, _) -> metadata
        member this.Bytes =
            match this with
            | TextureDataDotNet (_, bytes) -> bytes
            | TextureDataMipmap (_, bytes, _) -> bytes
            | TextureDataNative (metadata, textureDataPtr, _) ->
                let bytes = Array.zeroCreate<byte> (metadata.TextureWidth * metadata.TextureHeight * sizeof<uint>)
                Marshal.Copy (textureDataPtr, bytes, 0, bytes.Length)
                bytes
        member this.Dispose () =
            match this with
            | TextureDataDotNet (_, _) -> ()
            | TextureDataMipmap (_, _, _) -> ()
            | TextureDataNative (_, _, disposer) -> disposer.Dispose ()

    /// Memoizes texture loads.
    type [<ReferenceEquality>] TextureMemo =
        { Textures : Dictionary<string, TextureMetadata * Texture> }

        /// Make a texture memoizer.
        static member make () =
            { Textures = Dictionary HashIdentity.Structural }

    let private TryFormatPfimageData (image : IImage) =
        let data = image.Data
        let bytesOpt =
            match image.Format with
            | ImageFormat.Rgb24 ->
                let converted =
                    [|let mutable y = 0
                      while y < image.Height do
                        let mutable x = 0
                        while x < image.Stride - 2 do
                            let i = x + image.Stride * y
                            data.[i]; data.[i+1]; data.[i+2]; 255uy
                            x <- x + 3
                        y <- inc y|]
                Some converted
            | ImageFormat.Rgba32 ->
                let converted =
                    [|let mutable y = 0
                      while y < image.Height do
                        let mutable x = 0
                        while x < image.Stride - 3 do
                            let i = x + image.Stride * y
                            data.[i]; data.[i+1]; data.[i+2]; data.[i+3]
                            x <- x + 4
                        y <- inc y|]
                Some converted
            | _ ->
                Log.info ("Unsupported image format '" + scstring image.Format + "'.")
                None
        match bytesOpt with
        | Some bytes ->
            let mipmaps =
                [|for mipmap in image.MipMaps do
                    match image.Format with
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
                    | _ ->
                        failwithumf ()|]
            Some (bytes, mipmaps)
        | None -> None

    /// Create OpenGL representation of a texture from an already generated OpenGL texture id.
    let CreateTextureFromId textureId =
        let textureHandle = Gl.GetTextureHandleARB textureId
        Hl.Assert () // defensive assertion
        if textureHandle = 0UL then failwith ("Failed to create handle for texture id '" + string textureId + "'.")
        Gl.MakeTextureHandleResidentARB textureHandle
        { TextureId = textureId
          TextureHandle = textureHandle }

    /// Create a texture from existing texture data.
    /// NOTE: this function will dispose textureData.
    let CreateTextureFromData (minFilter, magFilter, mipmaps, textureData) =

        //
        match textureData with
        | TextureDataDotNet (metadata, bytes) ->

            // upload the texture data to gl
            let bytesPtr = GCHandle.Alloc (bytes, GCHandleType.Pinned)
            try let textureId = Gl.GenTexture ()
                Gl.BindTexture (TextureTarget.Texture2d, textureId)
                Gl.TexImage2D (TextureTarget.Texture2d, 0, metadata.TextureInternalFormat, metadata.TextureWidth, metadata.TextureHeight, 0, PixelFormat.Bgra, PixelType.UnsignedByte, bytesPtr.AddrOfPinnedObject ())
                Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int minFilter)
                Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int magFilter)
                Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.Repeat)
                Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.Repeat)
                if mipmaps then
                    Gl.TexParameter (TextureTarget.Texture2d, LanguagePrimitives.EnumOfValue Gl.TEXTURE_MAX_ANISOTROPY, Constants.Render.TextureAnisotropyMax)
                    Gl.GenerateMipmap TextureTarget.Texture2d
                let texture = CreateTextureFromId textureId
                (metadata, texture)
            finally bytesPtr.Free ()

        | TextureDataMipmap (metadata, bytes, mipmapDataArray) ->

            // upload the texture data to gl
            let bytesPtr = GCHandle.Alloc (bytes, GCHandleType.Pinned)
            try let textureId = Gl.GenTexture ()
                Gl.BindTexture (TextureTarget.Texture2d, textureId)
                Gl.TexImage2D (TextureTarget.Texture2d, 0, metadata.TextureInternalFormat, metadata.TextureWidth, metadata.TextureHeight, 0, PixelFormat.Bgra, PixelType.UnsignedByte, bytesPtr.AddrOfPinnedObject ())
                Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int minFilter)
                Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int magFilter)
                Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.Repeat)
                Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.Repeat)
                if mipmaps || mipmapDataArray.Length > 0 then
                    Gl.TexParameter (TextureTarget.Texture2d, LanguagePrimitives.EnumOfValue Gl.TEXTURE_MAX_ANISOTROPY, Constants.Render.TextureAnisotropyMax)
                if mipmaps && mipmapDataArray.Length = 0 then
                    Gl.GenerateMipmap TextureTarget.Texture2d
                let mutable mipmapIndex = 0
                while mipmapIndex < mipmapDataArray.Length do
                    let (mipmapResolution, mipmapBytes) = mipmapDataArray.[mipmapIndex]
                    let mipmapBytesPtr = GCHandle.Alloc (mipmapBytes, GCHandleType.Pinned)
                    try Gl.TexImage2D (TextureTarget.Texture2d, inc mipmapIndex, metadata.TextureInternalFormat, mipmapResolution.X, mipmapResolution.Y, 0, PixelFormat.Bgra, PixelType.UnsignedByte, mipmapBytesPtr.AddrOfPinnedObject ())
                    finally mipmapBytesPtr.Free ()
                    mipmapIndex <- inc mipmapIndex
                let texture = CreateTextureFromId textureId
                (metadata, texture)
            finally bytesPtr.Free ()

        | TextureDataNative (metadata, bytesPtr, disposer) ->

            // upload the texture data to gl
            use _ = disposer
            let textureId = Gl.GenTexture ()
            Gl.BindTexture (TextureTarget.Texture2d, textureId)
            Gl.TexImage2D (TextureTarget.Texture2d, 0, metadata.TextureInternalFormat, metadata.TextureWidth, metadata.TextureHeight, 0, PixelFormat.Bgra, PixelType.UnsignedByte, bytesPtr)
            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int minFilter)
            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int magFilter)
            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.Repeat)
            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.Repeat)
            if mipmaps then
                Gl.TexParameter (TextureTarget.Texture2d, LanguagePrimitives.EnumOfValue Gl.TEXTURE_MAX_ANISOTROPY, Constants.Render.TextureAnisotropyMax)
                Gl.GenerateMipmap TextureTarget.Texture2d
            let texture = CreateTextureFromId textureId
            (metadata, texture)

    /// Create a filtered texture from existing texture data.
    /// NOTE: this function will dispose textureData.
    let CreateTextureFromDataFiltered textureData =
        CreateTextureFromData (TextureMinFilter.LinearMipmapLinear, TextureMagFilter.Linear, true, textureData)

    /// Create an unfiltered texture from existing texture data.
    /// NOTE: this function will dispose textureData.
    let CreateTextureFromDataUnfiltered textureData =
        CreateTextureFromData (TextureMinFilter.Nearest, TextureMagFilter.Nearest, false, textureData)

    /// Attempt to create uploadable texture data from the given file path.
    /// Don't forget to dispose the last field when finished with the texture data.
    let TryCreateTextureData (filePath : string) =
        if File.Exists filePath then
            let platform = Environment.OSVersion.Platform
            let fileExtension = PathF.GetExtensionLower filePath
            if fileExtension = ".tga" then
                try let image = Pfimage.FromFile filePath
                    image.Decompress ()
                    match TryFormatPfimageData image with
                    | Some (bytes, _) ->
                        let metadata =
                            { TextureWidth = image.Width
                              TextureHeight = image.Height
                              TextureTexelWidth = 1.0f / single image.Width
                              TextureTexelHeight = 1.0f / single image.Height
                              TextureInternalFormat = Constants.OpenGL.UncompressedTextureFormat }
                        Some (TextureDataDotNet (metadata, bytes))
                    | None -> None
                with _ -> None
            elif fileExtension = ".dds" then
                try let image = Pfimage.FromFile filePath
                    image.Decompress () // TODO: utilize dxt5 compression where desirable.
                    match TryFormatPfimageData image with
                    | Some (bytes, mipmapDataArray) ->
                        let metadata =
                            { TextureWidth = image.Width
                              TextureHeight = image.Height
                              TextureTexelWidth = 1.0f / single image.Width
                              TextureTexelHeight = 1.0f / single image.Height
                              TextureInternalFormat = Constants.OpenGL.UncompressedTextureFormat }
                        Some (TextureDataMipmap (metadata, bytes, mipmapDataArray))
                    | None -> None
                with _ -> None
            elif platform = PlatformID.Win32NT || platform = PlatformID.Win32Windows then
                try let bitmap = new Drawing.Bitmap (filePath)
                    let data = bitmap.LockBits (Drawing.Rectangle (0, 0, bitmap.Width, bitmap.Height), Drawing.Imaging.ImageLockMode.ReadOnly, Drawing.Imaging.PixelFormat.Format32bppArgb)
                    let metadata =
                        { TextureWidth = bitmap.Width
                          TextureHeight = bitmap.Height
                          TextureTexelWidth = 1.0f / single bitmap.Width
                          TextureTexelHeight = 1.0f / single bitmap.Height
                          TextureInternalFormat = Constants.OpenGL.UncompressedTextureFormat }
                    let scan0 = data.Scan0
                    Some (TextureDataNative (metadata, scan0, { new IDisposable with member this.Dispose () = bitmap.UnlockBits data; bitmap.Dispose () })) // NOTE: calling UnlockBits explicitly since I can't figure out if Dispose does.
                with _ -> None
            else
                // NOTE: System.Drawing.Bitmap is not, AFAIK, available on non-Windows platforms, so we use a slower path here.
                let format = SDL.SDL_PIXELFORMAT_ARGB8888 // seems to be the right format on Ubuntu...
                let unconvertedPtr = SDL_image.IMG_Load filePath
                if unconvertedPtr <> nativeint 0 then
                    let unconverted = Marshal.PtrToStructure<SDL.SDL_Surface> unconvertedPtr
                    let metadata =
                        { TextureWidth = unconverted.w
                          TextureHeight = unconverted.h
                          TextureTexelWidth = 1.0f / single unconverted.w
                          TextureTexelHeight = 1.0f / single unconverted.h
                          TextureInternalFormat = Constants.OpenGL.UncompressedTextureFormat }
                    let unconvertedFormat = Marshal.PtrToStructure<SDL.SDL_PixelFormat> unconverted.format
                    if unconvertedFormat.format <> format then
                        let convertedPtr = SDL.SDL_ConvertSurfaceFormat (unconvertedPtr, format, 0u)
                        let converted = Marshal.PtrToStructure<SDL.SDL_Surface> convertedPtr
                        SDL.SDL_FreeSurface unconvertedPtr // no longer need this
                        Some (TextureDataNative (metadata, converted.pixels, { new IDisposable with member this.Dispose () = SDL.SDL_FreeSurface convertedPtr }))
                    else Some (TextureDataNative (metadata, unconverted.pixels, { new IDisposable with member this.Dispose () = SDL.SDL_FreeSurface unconvertedPtr }))
                else None
        else None

    /// Attempt to create a texture from a file.
    let TryCreateTexture (minFilter, magFilter, mipmaps, filePath) =

        // ensure we can create texture data
        match TryCreateTextureData filePath with
        | Some textureData ->

            // create opengl texture
            let (metadata, texture) = CreateTextureFromData (minFilter, magFilter, mipmaps, textureData)
            Right (metadata, texture)

        // error
        | None -> Left ("Missing file or unloadable texture data '" + filePath + "'.")

    /// Attempt to create a filtered texture from a file.
    let TryCreateTextureFiltered filePath =
        TryCreateTexture (TextureMinFilter.LinearMipmapLinear, TextureMagFilter.Linear, true, filePath)

    /// Attempt to create an unfiltered texture from a file.
    let TryCreateTextureUnfiltered filePath =
        TryCreateTexture (TextureMinFilter.Nearest, TextureMagFilter.Nearest, false, filePath)

    /// Attempt to create a memoized texture from a file.
    let TryCreateTextureMemoized (minFilter, magFilter, generateMipmaps, filePath : string, textureMemo) =

        // memoize texture
        match textureMemo.Textures.TryGetValue filePath with
        | (false, _) ->

            // attempt to create texture
            match TryCreateTexture (minFilter, magFilter, generateMipmaps, filePath) with
            | Right texture ->
                textureMemo.Textures.Add (filePath, texture)
                Right texture
            | Left error -> Left error

        // already exists
        | (true, texture) -> Right texture

    /// Attempt to create a filtered memoized texture from a file.
    let TryCreateTextureFilteredMemoized (filePath, textureMemo) =
        TryCreateTextureMemoized (TextureMinFilter.LinearMipmapLinear, TextureMagFilter.Linear, true, filePath, textureMemo)

    /// Attempt to create an unfiltered memoized texture from a file.
    let TryCreateTextureUnfilteredMemoized (filePath, textureMemo) =
        TryCreateTextureMemoized (TextureMinFilter.Nearest, TextureMagFilter.Nearest, false, filePath, textureMemo)

    /// Destroy OpenGL representation of a texture.
    let DestroyTexture texture =
        Gl.MakeTextureHandleNonResidentARB texture.TextureHandle
        Gl.DeleteTextures [|texture.TextureId|]