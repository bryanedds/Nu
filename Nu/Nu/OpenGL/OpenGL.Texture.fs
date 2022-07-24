// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OpenGL
open System
open System.Collections.Generic
open System.Drawing
open System.Drawing.Imaging
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
          TextureInternalFormat : InternalFormat }

        /// Unpopulated texture data.
        static member empty =
            { TextureWidth = 0
              TextureHeight = 0
              TextureTexelWidth = 0.0f
              TextureTexelHeight = 0.0f
              TextureInternalFormat = Unchecked.defaultof<_> }

    /// Memoizes texture loads.
    type [<NoEquality; NoComparison>] TextureMemo =
        private
            { Textures : Dictionary<TextureMinFilter * TextureMagFilter * bool * string, TextureMetadata * uint> }

        /// Make a texture memoizer.
        static member make () =
            { Textures = Dictionary HashIdentity.Structural }

    /// Vertically flip an SDL surface.
    let FlipSurface (surface : SDL.SDL_Surface inref) =
        let rowTop = Array.zeroCreate<byte> surface.pitch
        let rowBottom = Array.zeroCreate<byte> surface.pitch
        for i in 0 .. dec (surface.h / 2) do
            let offsetTop = i * surface.pitch
            let pixelsTop = surface.pixels + nativeint offsetTop
            let offsetBottom = (dec surface.h - i) * surface.pitch
            let pixelsBottom = surface.pixels + nativeint offsetBottom
            Marshal.Copy (pixelsTop, rowTop, 0, surface.pitch)
            Marshal.Copy (pixelsBottom, rowBottom, 0, surface.pitch)
            Marshal.Copy (rowTop, 0, pixelsBottom, surface.pitch)
            Marshal.Copy (rowBottom, 0, pixelsTop, surface.pitch)

    /// Set the minification filter of a texture.
    let SetMinFilter (minFilter : OpenGL.TextureMinFilter, texture) =
        Gl.BindTexture (TextureTarget.Texture2d, texture)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int minFilter)
        Gl.BindTexture (TextureTarget.Texture2d, 0u)

    /// Set the magnification filter of a texture.
    let SetMagFilter (magFilter : OpenGL.TextureMagFilter, texture) =
        Gl.BindTexture (TextureTarget.Texture2d, texture)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int magFilter)
        Gl.BindTexture (TextureTarget.Texture2d, 0u)

    let private TryCreateImageBitmap (filePath : string) =
        try let bitmap = new Bitmap (filePath)
            let data = bitmap.LockBits (Rectangle (0, 0, bitmap.Width, bitmap.Height), ImageLockMode.ReadOnly, Imaging.PixelFormat.Format32bppRgb)
            let metadata =
                { TextureWidth = bitmap.Width
                  TextureHeight = bitmap.Height
                  TextureTexelWidth = 1.0f / single bitmap.Width
                  TextureTexelHeight = 1.0f / single bitmap.Height
                  TextureInternalFormat = InternalFormat.Rgba8 }
            Some (metadata, data.Scan0, { new IDisposable with member this.Dispose () = bitmap.Dispose () })
        with _ -> None

    let private TryCreateImageSurface filePath =
        let format = SDL.SDL_PIXELFORMAT_ARGB8888 // seems to be the right format on Ubuntu...
        let unconvertedPtr = SDL_image.IMG_Load filePath
        if unconvertedPtr <> nativeint 0 then
            let unconverted = Marshal.PtrToStructure<SDL.SDL_Surface> unconvertedPtr
            let metadata =
                { TextureWidth = unconverted.w
                  TextureHeight = unconverted.h
                  TextureTexelWidth = 1.0f / single unconverted.w
                  TextureTexelHeight = 1.0f / single unconverted.h
                  TextureInternalFormat = InternalFormat.Rgba8 }
            let unconvertedFormat = Marshal.PtrToStructure<SDL.SDL_PixelFormat> unconverted.format
            if unconvertedFormat.format <> format then
                let convertedPtr = SDL.SDL_ConvertSurfaceFormat (unconvertedPtr, format, 0u)
                let converted = Marshal.PtrToStructure<SDL.SDL_Surface> convertedPtr
                SDL.SDL_FreeSurface unconvertedPtr // no longer need this
                Some (metadata, converted.pixels, { new IDisposable with member this.Dispose () = SDL.SDL_FreeSurface convertedPtr })
            else Some (metadata, unconverted.pixels, { new IDisposable with member this.Dispose () = SDL.SDL_FreeSurface unconvertedPtr })
        else None

    /// Attempt to create uploadable image data from the given file path.
    /// Don't forget to dispose the last field when finished with the image data.
    let TryCreateImageData filePath =
        match Environment.OSVersion.Platform with
        | PlatformID.Win32NT
        | PlatformID.Win32Windows -> TryCreateImageBitmap filePath
        | _ -> TryCreateImageSurface filePath

    /// Attempt to create a texture from a file.
    let TryCreateTexture (minFilter, magFilter, generateMipmaps, filePath : string) =

        // attmept to load image
        match TryCreateImageData filePath with
        | Some (metadata, imageData, disposer) ->

            // upload the image data to gl
            use d = disposer
            let texture = Gl.GenTexture ()
            Gl.BindTexture (TextureTarget.Texture2d, texture)
            Gl.TexImage2D (TextureTarget.Texture2d, 0, metadata.TextureInternalFormat, metadata.TextureWidth, metadata.TextureHeight, 0, PixelFormat.Bgra, PixelType.UnsignedByte, imageData)
            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int minFilter)
            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int magFilter)
            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
            Gl.TexParameter (TextureTarget.Texture2d, LanguagePrimitives.EnumOfValue Gl.TEXTURE_MAX_ANISOTROPY, Constants.Render.TextureAnisotropyMax) // NOTE: tho an extension, this one's considered ubiquitous.
            if generateMipmaps then Gl.GenerateMipmap TextureTarget.Texture2d
            Right (metadata, texture)

        // failure
        | None -> Left ("Missing file or unloadable image data '" + filePath + "'.")

    /// Attempt to create an unfiltered texture from a file.
    let TryCreateTextureUnfiltered filePath =
        TryCreateTexture (TextureMinFilter.Nearest, TextureMagFilter.Nearest, false, filePath)

    /// Attempt to create a filtered texture from a file.
    let TryCreateTextureFiltered filePath =
        TryCreateTexture (TextureMinFilter.LinearMipmapLinear, TextureMagFilter.Linear, true, filePath)

    /// Delete a texture.
    let DeleteTexture (texture : uint) =
        Gl.DeleteTextures texture

    /// Attempt to create a memoized texture from a file.
    let TryCreateTextureMemoized (minFilter, magFilter, generateMipmaps, filePath : string, textureMemo) =

        // memoize texture
        let textureKey = (minFilter, magFilter, generateMipmaps, Path.Simplify filePath)
        match textureMemo.Textures.TryGetValue textureKey with
        | (false, _) ->

            // attempt to create texture
            match TryCreateTexture (minFilter, magFilter, generateMipmaps, filePath) with
            | Right texture ->
                textureMemo.Textures.Add (textureKey, texture)
                Right texture
            | Left error -> Left error

        // already exists
        | (true, texture) -> Right texture

    /// Attempt to create a memoized unfiltered texture from a file.
    let TryCreateTextureMemoizedUnfiltered (filePath, textureMemo) =
        TryCreateTextureMemoized (TextureMinFilter.Nearest, TextureMagFilter.Nearest, false, filePath, textureMemo)

    /// Attempt to create a memoized filtered texture from a file.
    let TryCreateTextureMemoizedFiltered (filePath, textureMemo) =
        TryCreateTextureMemoized (TextureMinFilter.LinearMipmapLinear, TextureMagFilter.Linear, true, filePath, textureMemo)

    /// Delete memoized textures.
    let DeleteTexturesMemoized (textureMemo) =
        for entry in textureMemo.Textures do
            DeleteTexture (snd entry.Value)
        textureMemo.Textures.Clear ()