// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OpenGL
open System
open System.Collections.Generic
open System.Drawing
open System.Drawing.Imaging
open System.Runtime.InteropServices
open SDL2
open Prime
open Nu

[<RequireQualifiedAccess>]
module Texture2d =

    /// A 2d texture's metadata.
    type Texture2dMetadata =
        { Texture2dWidth : int
          Texture2dHeight : int
          Texture2dTexelWidth : single
          Texture2dTexelHeight : single
          Texture2dInternalFormat : InternalFormat }

        /// Unpopulated 2d texture data.
        static member empty =
            { Texture2dWidth = 0
              Texture2dHeight = 0
              Texture2dTexelWidth = 0.0f
              Texture2dTexelHeight = 0.0f
              Texture2dInternalFormat = Unchecked.defaultof<_> }

    /// Memoizes 2d texture loads.
    type [<NoEquality; NoComparison>] Texture2dMemo =
        private
            { Texture2ds : Dictionary<TextureMinFilter * TextureMagFilter * bool * string, Texture2dMetadata * uint> }

        /// Make a 2d texture memoizer.
        static member make () =
            { Texture2ds = Dictionary HashIdentity.Structural }

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

    let private TryCreateImageBitmap (filePath : string) =
        try let bitmap = new Bitmap (filePath)
            let data = bitmap.LockBits (Rectangle (0, 0, bitmap.Width, bitmap.Height), ImageLockMode.ReadOnly, Imaging.PixelFormat.Format32bppRgb)
            let metadata =
                { Texture2dWidth = bitmap.Width
                  Texture2dHeight = bitmap.Height
                  Texture2dTexelWidth = 1.0f / single bitmap.Width
                  Texture2dTexelHeight = 1.0f / single bitmap.Height
                  Texture2dInternalFormat = InternalFormat.Rgba8 }
            Some (metadata, data.Scan0, { new IDisposable with member this.Dispose () = bitmap.Dispose () })
        with _ -> None

    let private TryCreateImageSurface filePath =
        let format = SDL.SDL_PIXELFORMAT_ABGR8888 // this is RGBA8888 on little-endian architectures
        let unconvertedPtr = SDL_image.IMG_Load filePath
        if unconvertedPtr <> nativeint 0 then
            let unconverted = Marshal.PtrToStructure<SDL.SDL_Surface> unconvertedPtr
            let metadata =
                { Texture2dWidth = unconverted.w
                  Texture2dHeight = unconverted.h
                  Texture2dTexelWidth = 1.0f / single unconverted.w
                  Texture2dTexelHeight = 1.0f / single unconverted.h
                  Texture2dInternalFormat = InternalFormat.Rgba8 }
            let unconvertedFormat = Marshal.PtrToStructure<SDL.SDL_PixelFormat> unconverted.format
            if unconvertedFormat.format <> format then
                let convertedPtr = SDL.SDL_ConvertSurfaceFormat (unconvertedPtr, format, 0u)
                SDL.SDL_FreeSurface unconvertedPtr
                Some (metadata, convertedPtr, { new IDisposable with member this.Dispose () = SDL.SDL_FreeSurface convertedPtr })
            else Some (metadata, unconvertedPtr, { new IDisposable with member this.Dispose () = SDL.SDL_FreeSurface unconvertedPtr })
        else None

    /// Attempt to create uploadable image data from the given file path.
    /// Don't forget to dispose the last field when finished with the image data.
    let TryCreateImageData filePath =
        match Environment.OSVersion.Platform with
        | PlatformID.Win32NT
        | PlatformID.Win32Windows -> TryCreateImageBitmap filePath
        | _ -> TryCreateImageSurface filePath

    /// Attempt to create a 2d texture from a file.
    let TryCreateTexture2d (minFilter, magFilter, generateMipmaps, filePath : string) =

        // attmept to load image
        match TryCreateImageData filePath with
        | Some (metadata, imageData, disposer) ->

            // upload the image data to gl
            use _ = disposer
            let texture2d = Gl.GenTexture ()
            Gl.BindTexture (TextureTarget.Texture2d, texture2d)
            Gl.TexImage2D (TextureTarget.Texture2d, 0, metadata.Texture2dInternalFormat, metadata.Texture2dWidth, metadata.Texture2dHeight, 0, PixelFormat.Bgra, PixelType.UnsignedByte, imageData)
            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int minFilter)
            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int magFilter)
            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
            Gl.TexParameter (TextureTarget.Texture2d, LanguagePrimitives.EnumOfValue Gl.TEXTURE_MAX_ANISOTROPY, Constants.Render.TextureAnisotropyMax) // NOTE: tho an extension, this one's considered ubiquitous.
            if generateMipmaps then Gl.GenerateMipmap TextureTarget.Texture2d
            Right (metadata, texture2d)

        // failure
        | None -> Left ("Missing file or unloadable image data '" + filePath + "'.")

    /// Attempt to create an unfiltered 2d texture from a file.
    let TryCreateTexture2dUnfiltered filePath =
        TryCreateTexture2d (TextureMinFilter.Nearest, TextureMagFilter.Nearest, false, filePath)

    /// Attempt to create a filtered 2d texture from a file.
    let TryCreateTexture2dFiltered filePath =
        TryCreateTexture2d (TextureMinFilter.LinearMipmapLinear, TextureMagFilter.Linear, true, filePath)

    /// Delete a 2d texture.
    let DeleteTexture2d (texture2d : uint) =
        Gl.DeleteTextures texture2d

    /// Attempt to create a memoized 2d texture from a file.
    let TryCreateTexture2dMemoized (minFilter, magFilter, generateMipmaps, filePath : string, texture2dMemo) =

        // memoize 2d texture
        let textureKey = (minFilter, magFilter, generateMipmaps, filePath)
        match texture2dMemo.Texture2ds.TryGetValue textureKey with
        | (false, _) ->

            // attempt to create 2d texture
            match TryCreateTexture2d (minFilter, magFilter, generateMipmaps, filePath) with
            | Right texture2d ->
                texture2dMemo.Texture2ds.Add (textureKey, texture2d)
                Right texture2d
            | Left error -> Left error

        // already exists
        | (true, texture2d) -> Right texture2d

    /// Attempt to create a memoized unfiltered 2d texture from a file.
    let TryCreateTexture2dMemoizedUnfiltered (filePath, texture2dMemo) =
        TryCreateTexture2dMemoized (TextureMinFilter.Nearest, TextureMagFilter.Nearest, false, filePath, texture2dMemo)

    /// Attempt to create a memoized filtered 2d texture from a file.
    let TryCreateTexture2dMemoizedFiltered (filePath, texture2dMemo) =
        TryCreateTexture2dMemoized (TextureMinFilter.LinearMipmapLinear, TextureMagFilter.Linear, true, filePath, texture2dMemo)

    /// Delete memoized 2d textures.
    let DeleteTexture2dsMemoized (texture2dMemo) =
        for entry in texture2dMemo.Texture2ds do
            DeleteTexture2d (snd entry.Value)
        texture2dMemo.Texture2ds.Clear ()