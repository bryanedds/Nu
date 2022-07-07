// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OpenGL
open System
open System.Runtime.InteropServices
open SDL2
open Prime
open Nu

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

    /// Attempt to create an SDL surface of an image from the give file path, converting its format if needed.
    /// NOTE: caller is reponsible for calling SDL.SDL_FreeSurface on return surface pointer.
    let TryCreateImageSurface filePath =
        let format = SDL.SDL_PIXELFORMAT_ABGR8888 // this is RGBA8888 on little-endian architectures
        let unconvertedPtr = SDL_image.IMG_Load filePath
        if unconvertedPtr <> nativeint 0 then
            let unconverted = Marshal.PtrToStructure<SDL.SDL_Surface> unconvertedPtr
            let unconvertedFormat = Marshal.PtrToStructure<SDL.SDL_PixelFormat> unconverted.format
            if unconvertedFormat.format <> format then
                let convertedPtr = SDL.SDL_ConvertSurfaceFormat (unconvertedPtr, format, 0u)
                SDL.SDL_FreeSurface unconvertedPtr
                let converted = Marshal.PtrToStructure<SDL.SDL_Surface> convertedPtr
                Some (convertedPtr, converted)
            else Some (unconvertedPtr, unconverted)
        else None

    /// Attempt to create a 2d texture from a file.
    let TryCreateTexture2d (minFilter, magFilter, filePath : string) =

        // attempt to create image surface
        match TryCreateImageSurface filePath with
        | Some (surfacePtr, surface) ->

            // upload the texture to gl
            let texture = Gl.GenTexture ()
            let internalFormat = InternalFormat.Rgba8
            Gl.BindTexture (TextureTarget.Texture2d, texture)
            Gl.TexImage2D (TextureTarget.Texture2d, 0, internalFormat, surface.w, surface.h, 0, PixelFormat.Rgba, PixelType.UnsignedByte, surface.pixels)
            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int minFilter)
            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int magFilter)
            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
            Gl.TexParameter (TextureTarget.Texture2d, LanguagePrimitives.EnumOfValue Gl.TEXTURE_MAX_ANISOTROPY, 16.0f) // NOTE: while an extension, this is considered ubiquitous.

            // teardown surface
            SDL.SDL_FreeSurface surfacePtr

            // check for errors
            match Gl.GetError () with
            | ErrorCode.NoError ->
                let metadata =
                    { TextureWidth = surface.w
                      TextureHeight = surface.h
                      TextureTexelWidth = 1.0f / single surface.w
                      TextureTexelHeight = 1.0f / single surface.h
                      TextureInternalFormat = internalFormat }
                Right (metadata, texture)
            | error -> Left (string error)

        // load error
        | None -> Left ("Missing file or unloadable image '" + filePath + "'.")

    /// Delete a texture.
    let DeleteTexture (texture : uint) =
        Gl.DeleteTextures texture

    /// Attempt to create an unfiltered 2d texture from a file.
    let TryCreateTexture2dUnfiltered filePath =
        TryCreateTexture2d (TextureMinFilter.Nearest, TextureMagFilter.Nearest, filePath)

    /// Attempt to create an linearly-filtered 2d texture from a file.
    let TryCreateTexture2dLinear filePath =
        TryCreateTexture2d (TextureMinFilter.Linear, TextureMagFilter.Linear, filePath)
        
    /// Attempt to create a cube map from 6 files.
    let TryCreateCubeMap (faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath) =

        // bind new cube map
        let cubeMap = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.TextureCubeMap, cubeMap)
        Hl.Assert ()

        // load faces into cube map
        let mutable errorOpt = None
        let faceFilePaths = [|faceRightFilePath; faceLeftFilePath; faceTopFilePath; faceBottomFilePath; faceBackFilePath; faceFrontFilePath|]
        for i in 0 .. dec faceFilePaths.Length do
            if Option.isNone errorOpt then
                let faceFilePath = faceFilePaths.[i]
                match TryCreateImageSurface faceFilePath with
                | Some (surfacePtr, surface) ->
                    try Gl.TexImage2D (LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i), 0, InternalFormat.Rgba8, surface.w, surface.h, 0, PixelFormat.Rgba, PixelType.UnsignedByte, surface.pixels)
                    finally SDL.SDL_FreeSurface surfacePtr
                    Hl.Assert ()
                | None -> errorOpt <- Some ("Could not create surface for image from '" + faceFilePath + "'")

        // attempt to finalize cube map
        match errorOpt with
        | None ->
            Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear)
            Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
            Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
            Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
            Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapR, int TextureWrapMode.ClampToEdge)
            Right cubeMap
        | Some error ->
            DeleteTexture cubeMap
            Left error