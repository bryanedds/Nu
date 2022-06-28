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
          TextureInternalFormat : OpenGL.InternalFormat }

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

    /// Attempt to create a 2d texture from a file.
    let TryCreateTexture2d (minFilter, magFilter, filePath : string) =

        // attempt to load the texture into an SDL surface, converting its format if needed
        let format = SDL.SDL_PIXELFORMAT_ABGR8888 // this is RGBA8888 on little-endian architectures
        let surfaceOpt =
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

        // ensure surface was loaded into
        match surfaceOpt with
        | Some (surfacePtr, surface) ->

            // upload the texture to gl
            let texture = OpenGL.Gl.GenTexture ()
            OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture0
            OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, texture)
            OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMinFilter, int minFilter)
            OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMagFilter, int magFilter)
            let internalFormat = OpenGL.InternalFormat.Rgba8
            OpenGL.Gl.TexImage2D (OpenGL.TextureTarget.Texture2d, 0, internalFormat, surface.w, surface.h, 0, OpenGL.PixelFormat.Rgba, OpenGL.PixelType.UnsignedByte, surface.pixels)

            // teardown surface
            SDL.SDL_FreeSurface surfacePtr

            // check for errors
            match OpenGL.Gl.GetError () with
            | OpenGL.ErrorCode.NoError ->
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
        OpenGL.Gl.DeleteTextures texture

    /// Attempt to create an unfiltered 2d texture from a file.
    let TryCreateTexture2dUnfiltered filePath =
        TryCreateTexture2d (OpenGL.TextureMinFilter.Nearest, OpenGL.TextureMagFilter.Nearest, filePath)

    /// Attempt to create an linearly-filtered 2d texture from a file.
    let TryCreateTexture2dLinear filePath =
        TryCreateTexture2d (OpenGL.TextureMinFilter.Linear, OpenGL.TextureMagFilter.Linear, filePath)