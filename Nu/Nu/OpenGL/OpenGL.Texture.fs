// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OpenGL
open System
open System.Collections.Generic
open System.IO
open System.Runtime.InteropServices
open SDL2
open Prime
open Nu

// NOTE: on Nu's texturing nomenclature -
// Texture | Texture2d =    2d texture
// Texture3d =              3d texture
// (Cube|_)Map =            cube map
[<RequireQualifiedAccess>]
module Texture =

    /// A texture's metadata.
    type TextureMetadata =
        { TextureWidth : int
          TextureHeight : int
          TextureTexelWidth : single
          TextureTexelHeight : single
          TextureInternalFormat : InternalFormat
          TextureGenerateMipmaps : bool }

        /// Unpopulated texture data.
        static member empty =
            { TextureWidth = 0
              TextureHeight = 0
              TextureTexelWidth = 0.0f
              TextureTexelHeight = 0.0f
              TextureInternalFormat = Unchecked.defaultof<InternalFormat>
              TextureGenerateMipmaps = false }

    /// Memoizes texture loads.
    type [<ReferenceEquality>] TextureMemo =
        { Textures : Dictionary<string, TextureMetadata * uint> }

        /// Make a texture memoizer.
        static member make () =
            { Textures = Dictionary HashIdentity.Structural }

    /// Create a texture from existing texture data.
    let CreateTexture (internalFormat, width, height, pixelFormat, pixelType, minFilter : OpenGL.TextureMinFilter, magFilter : OpenGL.TextureMagFilter, generateMipmaps, textureData : nativeint) =
        let texture = OpenGL.Gl.GenTexture ()
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, texture)
        OpenGL.Gl.TexImage2D (OpenGL.TextureTarget.Texture2d, 0, internalFormat, width, height, 0, pixelFormat, pixelType, textureData)
        OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMinFilter, int minFilter)
        OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMagFilter, int magFilter)
        OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureWrapS, int TextureWrapMode.Repeat)
        OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureWrapT, int TextureWrapMode.Repeat)
        if generateMipmaps then Gl.GenerateMipmap TextureTarget.Texture2d
        texture

    /// Create a filtered texture from existing texture data.
    let CreateTextureFiltered (internalFormat, width, height, pixelFormat, pixelType, textureData) =
        CreateTexture (internalFormat, width, height, pixelFormat, pixelType, TextureMinFilter.LinearMipmapLinear, TextureMagFilter.Linear, true, textureData)

    /// Create an unfiltered texture from existing texture data.
    let CreateTextureUnfiltered (internalFormat, width, height, pixelFormat, pixelType, textureData) =
        CreateTexture (internalFormat, width, height, pixelFormat, pixelType, TextureMinFilter.Nearest, TextureMagFilter.Nearest, false, textureData)

    /// Create a texture from existing texture data.
    let CreateTextureFromData (internalFormat, minFilter, magFilter, generateMipmaps, metadata, textureData) =

        // update the texture data to gl, creating mip maps if desired
        let texture = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, texture)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, internalFormat, metadata.TextureWidth, metadata.TextureHeight, 0, PixelFormat.Bgra, PixelType.UnsignedByte, textureData)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int minFilter)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int magFilter)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.Repeat)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.Repeat)
        if generateMipmaps then
            Gl.TexParameter (TextureTarget.Texture2d, LanguagePrimitives.EnumOfValue Gl.TEXTURE_MAX_ANISOTROPY, Constants.Render.TextureAnisotropyMax) // NOTE: tho an extension, this one's considered ubiquitous.
            Gl.GenerateMipmap TextureTarget.Texture2d
        texture

    /// Create a filtered texture from existing texture data.
    let CreateTextureFromDataFiltered (internalFormat, metadata, textureData) =
        CreateTextureFromData (internalFormat, TextureMinFilter.LinearMipmapLinear, TextureMagFilter.Linear, true, metadata, textureData)

    /// Create an unfiltered texture from existing texture data.
    let CreateTextureFromDataUnfiltered (internalFormat, metadata, textureData) =
        CreateTextureFromData (internalFormat, TextureMinFilter.Nearest, TextureMagFilter.Nearest, false, metadata, textureData)

    /// Attempt to create uploadable texture data from the given file path.
    /// Don't forget to dispose the last field when finished with the texture data.
    let TryCreateTextureData (internalFormat, generateMipmaps, filePath : string) =
        if File.Exists filePath then
            let platform = Environment.OSVersion.Platform
            let fileExtension = Pathf.GetExtensionLower filePath
            if  (platform = PlatformID.Win32NT || platform = PlatformID.Win32Windows) &&
                fileExtension <> ".tga" (* NOTE: System.Drawing.Bitmap does not seem to support .tga loading. *) then
                // NOTE: System.Drawing.Bitmap is, AFAIK, only available on Windows platforms, so we use a fast path here.
                try let bitmap = new Drawing.Bitmap (filePath)
                    let data = bitmap.LockBits (Drawing.Rectangle (0, 0, bitmap.Width, bitmap.Height), Drawing.Imaging.ImageLockMode.ReadOnly, Drawing.Imaging.PixelFormat.Format32bppArgb)
                    let metadata =
                        { TextureWidth = bitmap.Width
                          TextureHeight = bitmap.Height
                          TextureTexelWidth = 1.0f / single bitmap.Width
                          TextureTexelHeight = 1.0f / single bitmap.Height
                          TextureInternalFormat = internalFormat
                          TextureGenerateMipmaps = generateMipmaps }
                    let scan0 = data.Scan0
                    Some (metadata, scan0, { new IDisposable with member this.Dispose () = bitmap.UnlockBits data; bitmap.Dispose () }) // NOTE: calling UnlockBits explicitly since I can't fiture out if Dispose does.
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
                          TextureInternalFormat = internalFormat
                          TextureGenerateMipmaps = generateMipmaps }
                    let unconvertedFormat = Marshal.PtrToStructure<SDL.SDL_PixelFormat> unconverted.format
                    if unconvertedFormat.format <> format then
                        let convertedPtr = SDL.SDL_ConvertSurfaceFormat (unconvertedPtr, format, 0u)
                        let converted = Marshal.PtrToStructure<SDL.SDL_Surface> convertedPtr
                        SDL.SDL_FreeSurface unconvertedPtr // no longer need this
                        Some (metadata, converted.pixels, { new IDisposable with member this.Dispose () = SDL.SDL_FreeSurface convertedPtr })
                    else Some (metadata, unconverted.pixels, { new IDisposable with member this.Dispose () = SDL.SDL_FreeSurface unconvertedPtr })
                else None
        else None

    /// Attempt to create a texture from a file.
    let TryCreateTexture (internalFormat, minFilter, magFilter, generateMipmaps, filePath) =
    
        // ensure we can create texture data
        match TryCreateTextureData (internalFormat, generateMipmaps, filePath) with
        | Some (metadata, textureData, disposer) ->

            // upload the texture data to gl
            use _ = disposer
            let texture = Gl.GenTexture ()
            Gl.BindTexture (TextureTarget.Texture2d, texture)
            Gl.TexImage2D (TextureTarget.Texture2d, 0, internalFormat, metadata.TextureWidth, metadata.TextureHeight, 0, PixelFormat.Bgra, PixelType.UnsignedByte, textureData)
            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int minFilter)
            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int magFilter)
            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.Repeat)
            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.Repeat)
            if generateMipmaps then
                Gl.TexParameter (TextureTarget.Texture2d, LanguagePrimitives.EnumOfValue Gl.TEXTURE_MAX_ANISOTROPY, Constants.Render.TextureAnisotropyMax) // NOTE: tho an extension, this one's considered ubiquitous.
                Gl.GenerateMipmap TextureTarget.Texture2d
            Right (metadata, texture)

        // error
        | None -> Left ("Missing file or unloadable texture data '" + filePath + "'.")

    /// Attempt to create a filtered texture from a file.
    let TryCreateTextureFiltered (internalFormat, filePath) =
        TryCreateTexture (internalFormat, TextureMinFilter.LinearMipmapLinear, TextureMagFilter.Linear, true, filePath)

    /// Attempt to create an unfiltered texture from a file.
    let TryCreateTextureUnfiltered (internalFormat, filePath) =
        TryCreateTexture (internalFormat, TextureMinFilter.Nearest, TextureMagFilter.Nearest, false, filePath)

    /// Attempt to create a memoized texture from a file.
    let TryCreateTextureMemoized (internalFormat, minFilter, magFilter, generateMipmaps, filePath : string, textureMemo) =

        // memoize texture
        match textureMemo.Textures.TryGetValue filePath with
        | (false, _) ->

            // attempt to create texture
            match TryCreateTexture (internalFormat, minFilter, magFilter, generateMipmaps, filePath) with
            | Right texture ->
                textureMemo.Textures.Add (filePath, texture)
                Right texture
            | Left error -> Left error

        // already exists
        | (true, texture) -> Right texture

    /// Attempt to create a filtered memoized texture from a file.
    let TryCreateTextureFilteredMemoized (internalFormat, filePath, textureMemo) =
        TryCreateTextureMemoized (internalFormat, TextureMinFilter.LinearMipmapLinear, TextureMagFilter.Linear, true, filePath, textureMemo)

    /// Attempt to create an unfiltered memoized texture from a file.
    let TryCreateTextureUnfilteredMemoized (internalFormat, filePath, textureMemo) =
        TryCreateTextureMemoized (internalFormat, TextureMinFilter.Nearest, TextureMagFilter.Nearest, false, filePath, textureMemo)