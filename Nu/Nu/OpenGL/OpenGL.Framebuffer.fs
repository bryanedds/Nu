// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OpenGL
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Framebuffer =

    /// Attempt to create texture 2d buffers.
    let TryCreateTextureBuffers () =

        // create frame buffer object
        let framebuffer = Gl.GenFramebuffer ()
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // create texture 2d buffer
        let textureId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, textureId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rgba32f, Constants.Render.Resolution.X, Constants.Render.Resolution.Y, 0, PixelFormat.Rgba, PixelType.Float, nativeint 0)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int TextureMinFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int TextureMagFilter.Nearest)
        Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, TextureTarget.Texture2d, textureId, 0)
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // create depth and stencil buffers
        let depthStencilBuffer = Gl.GenRenderbuffer ()
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, depthStencilBuffer)
        Gl.RenderbufferStorage (RenderbufferTarget.Renderbuffer, InternalFormat.Depth24Stencil8, Constants.Render.Resolution.X, Constants.Render.Resolution.Y)
        Gl.FramebufferRenderbuffer (FramebufferTarget.Framebuffer, FramebufferAttachment.DepthStencilAttachment, RenderbufferTarget.Renderbuffer, depthStencilBuffer)
        Hl.Assert ()

        // ensure framebuffer is complete
        if Gl.CheckFramebufferStatus FramebufferTarget.Framebuffer = FramebufferStatus.FramebufferComplete then
            let texture = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = textureId }
            Right (texture, framebuffer)
        else Left ("Could not create complete texture 2d framebuffer.")

    /// Destroy texture buffers.
    let DestroyTextureBuffers (position : Texture.Texture, framebuffer) =
        Gl.DeleteFramebuffers [|framebuffer|]
        position.Destroy ()

    /// Create filter box 1d buffers.
    let TryCreateFilterBox1dBuffers (resolutionX, resolutionY) =

        // create frame buffer object
        let framebuffer = Gl.GenFramebuffer ()
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // create filter box buffer
        let filterBoxId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, filterBoxId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.R32f, resolutionX, resolutionY, 0, PixelFormat.Rgba, PixelType.Float, nativeint 0)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int TextureMinFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int TextureMagFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
        Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, TextureTarget.Texture2d, filterBoxId, 0)
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // associate draw buffers
        Gl.DrawBuffers [|int FramebufferAttachment.ColorAttachment0|]
        Hl.Assert ()

        // create render buffer with depth and stencil
        let renderbuffer = Gl.GenRenderbuffer ()
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, renderbuffer)
        Gl.RenderbufferStorage (RenderbufferTarget.Renderbuffer, InternalFormat.Depth24Stencil8, resolutionX, resolutionY)
        Gl.FramebufferRenderbuffer (FramebufferTarget.Framebuffer, FramebufferAttachment.DepthStencilAttachment, RenderbufferTarget.Renderbuffer, renderbuffer)
        Hl.Assert ()

        // ensure framebuffer is complete
        if Gl.CheckFramebufferStatus FramebufferTarget.Framebuffer = FramebufferStatus.FramebufferComplete then
            let filterBox = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = filterBoxId }
            Right (filterBox, renderbuffer, framebuffer)
        else Left "Could not create complete filter box 1d framebuffer."

    /// Destroy filter box 1d buffers.
    let DestroyFilterBox1dBuffers (filterBoxBlurTexture : Texture.Texture, renderbuffer, framebuffer) =
        Gl.DeleteRenderbuffers [|renderbuffer|]
        Gl.DeleteFramebuffers [|framebuffer|]
        filterBoxBlurTexture.Destroy ()

    /// Create filter gaussian 2d buffers.
    let TryCreateFilterGaussian2dBuffers (resolutionX, resolutionY) =

        // create frame buffer object
        let framebuffer = Gl.GenFramebuffer ()
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // create filter gaussian buffer
        let filterGaussianId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, filterGaussianId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rg32f, resolutionX, resolutionY, 0, PixelFormat.Red, PixelType.Float, nativeint 0)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int TextureMinFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int TextureMagFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
        Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, TextureTarget.Texture2d, filterGaussianId, 0)
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // associate draw buffers
        Gl.DrawBuffers [|int FramebufferAttachment.ColorAttachment0|]
        Hl.Assert ()

        // create render buffer with depth and stencil
        let renderbuffer = Gl.GenRenderbuffer ()
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, renderbuffer)
        Gl.RenderbufferStorage (RenderbufferTarget.Renderbuffer, InternalFormat.Depth24Stencil8, resolutionX, resolutionY)
        Gl.FramebufferRenderbuffer (FramebufferTarget.Framebuffer, FramebufferAttachment.DepthStencilAttachment, RenderbufferTarget.Renderbuffer, renderbuffer)
        Hl.Assert ()

        // ensure framebuffer is complete
        if Gl.CheckFramebufferStatus FramebufferTarget.Framebuffer = FramebufferStatus.FramebufferComplete then
            let filterGaussian = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = filterGaussianId }
            Right (filterGaussian, renderbuffer, framebuffer)
        else Left "Could not create complete filter gaussian 2d framebuffer."

    /// Destroy filter gaussian 2d buffers.
    let DestroyFilterGaussian2dBuffers (filterGaussianTexture : Texture.Texture, renderbuffer, framebuffer) =
        Gl.DeleteRenderbuffers [|renderbuffer|]
        Gl.DeleteFramebuffers [|framebuffer|]
        filterGaussianTexture.Destroy ()

    /// Create filter buffers.
    let TryCreateFilterBuffers (resolutionX, resolutionY) =

        // create frame buffer object
        let framebuffer = Gl.GenFramebuffer ()
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // create filter buffer
        let filterId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, filterId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rgba32f, resolutionX, resolutionY, 0, PixelFormat.Rgba, PixelType.Float, nativeint 0)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int TextureMinFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int TextureMagFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
        Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, TextureTarget.Texture2d, filterId, 0)
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // associate draw buffers
        Gl.DrawBuffers [|int FramebufferAttachment.ColorAttachment0|]
        Hl.Assert ()

        // create render buffer with depth and stencil
        let renderbuffer = Gl.GenRenderbuffer ()
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, renderbuffer)
        Gl.RenderbufferStorage (RenderbufferTarget.Renderbuffer, InternalFormat.Depth24Stencil8, resolutionX, resolutionY)
        Gl.FramebufferRenderbuffer (FramebufferTarget.Framebuffer, FramebufferAttachment.DepthStencilAttachment, RenderbufferTarget.Renderbuffer, renderbuffer)
        Hl.Assert ()

        // ensure framebuffer is complete
        if Gl.CheckFramebufferStatus FramebufferTarget.Framebuffer = FramebufferStatus.FramebufferComplete then
            let filter = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = filterId }
            Right (filter, renderbuffer, framebuffer)
        else Left "Could not create complete filter framebuffer."

    /// Destroy filter buffers.
    let DestroyFilterBuffers (filter : Texture.Texture, renderbuffer, framebuffer) =
        Gl.DeleteRenderbuffers [|renderbuffer|]
        Gl.DeleteFramebuffers [|framebuffer|]
        filter.Destroy ()

    /// Attempt to create hdr buffers.
    let TryCreateHdrBuffers () =

        // create frame buffer object
        let framebuffer = Gl.GenFramebuffer ()
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // create position buffer
        let positionId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, positionId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rgba32f, Constants.Render.Resolution.X, Constants.Render.Resolution.Y, 0, PixelFormat.Rgba, PixelType.Float, nativeint 0)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int TextureMinFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int TextureMagFilter.Nearest)
        Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, TextureTarget.Texture2d, positionId, 0)
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // create depth and stencil buffers
        let depthStencilBuffer = Gl.GenRenderbuffer ()
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, depthStencilBuffer)
        Gl.RenderbufferStorage (RenderbufferTarget.Renderbuffer, InternalFormat.Depth24Stencil8, Constants.Render.Resolution.X, Constants.Render.Resolution.Y)
        Gl.FramebufferRenderbuffer (FramebufferTarget.Framebuffer, FramebufferAttachment.DepthStencilAttachment, RenderbufferTarget.Renderbuffer, depthStencilBuffer)
        Hl.Assert ()

        // ensure framebuffer is complete
        if Gl.CheckFramebufferStatus FramebufferTarget.Framebuffer = FramebufferStatus.FramebufferComplete then
            let position = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = positionId }
            Right (position, framebuffer)
        else Left ("Could not create complete HDR framebuffer.")

    /// Destroy hdr buffers.
    let DestroyHdrFrameBuffers (position : Texture.Texture, framebuffer) =
        Gl.DeleteFramebuffers [|framebuffer|]
        position.Destroy ()

    /// Create shadow buffers.
    let TryCreateShadowBuffers (shadowResolutionX, shadowResolutionY) =

        // create frame buffer object
        let framebuffer = Gl.GenFramebuffer ()
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // create shadow texture
        let shadowTextureId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, shadowTextureId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rg32f, shadowResolutionX, shadowResolutionY, 0, PixelFormat.Rgba, PixelType.Float, nativeint 0)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
        Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, TextureTarget.Texture2d, shadowTextureId, 0)
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // associate draw buffers
        Gl.DrawBuffers [|int FramebufferAttachment.ColorAttachment0|]
        Hl.Assert ()

        // create render buffer with depth and stencil
        let renderbuffer = Gl.GenRenderbuffer ()
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, renderbuffer)
        Gl.RenderbufferStorage (RenderbufferTarget.Renderbuffer, InternalFormat.Depth24Stencil8, shadowResolutionX, shadowResolutionY)
        Gl.FramebufferRenderbuffer (FramebufferTarget.Framebuffer, FramebufferAttachment.DepthStencilAttachment, RenderbufferTarget.Renderbuffer, renderbuffer)
        Hl.Assert ()

        // ensure framebuffer is complete
        if Gl.CheckFramebufferStatus FramebufferTarget.Framebuffer = FramebufferStatus.FramebufferComplete then
            let shadowTexture = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = shadowTextureId }
            Right (shadowTexture, renderbuffer, framebuffer)
        else Left "Could not create complete shadow texture framebuffer."

    /// Destroy shadow buffers.
    let DestroyShadowBuffers (shadowTexture : Texture.Texture, renderbuffer, framebuffer) =
        Gl.DeleteRenderbuffers [|renderbuffer|]
        Gl.DeleteFramebuffers [|framebuffer|]
        shadowTexture.Destroy ()

    /// Create a geometry buffers.
    let TryCreateGeometryBuffers () =

        // create frame buffer object
        let framebuffer = Gl.GenFramebuffer ()
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // create position buffer
        let positionId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, positionId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rgba32f, Constants.Render.Resolution.X, Constants.Render.Resolution.Y, 0, PixelFormat.Rgba, PixelType.Float, nativeint 0)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int TextureMinFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int TextureMagFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
        Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, TextureTarget.Texture2d, positionId, 0)
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // create albedo buffer
        let albedoId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, albedoId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rgba32f, Constants.Render.Resolution.X, Constants.Render.Resolution.Y, 0, PixelFormat.Rgba, PixelType.Float, nativeint 0)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int TextureMinFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int TextureMagFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
        Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment1, TextureTarget.Texture2d, albedoId, 0)
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // create material buffer
        let materialId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, materialId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rgba32f, Constants.Render.Resolution.X, Constants.Render.Resolution.Y, 0, PixelFormat.Rgba, PixelType.Float, nativeint 0)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int TextureMinFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int TextureMagFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
        Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment2, TextureTarget.Texture2d, materialId, 0)
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // create normal plus buffer
        let normalPlusId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, normalPlusId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rgba32f, Constants.Render.Resolution.X, Constants.Render.Resolution.Y, 0, PixelFormat.Rgba, PixelType.Float, nativeint 0)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int TextureMinFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int TextureMagFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
        Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment3, TextureTarget.Texture2d, normalPlusId, 0)
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // associate draw buffers
        Gl.DrawBuffers
            [|int FramebufferAttachment.ColorAttachment0
              int FramebufferAttachment.ColorAttachment1
              int FramebufferAttachment.ColorAttachment2
              int FramebufferAttachment.ColorAttachment3|]
        Hl.Assert ()

        // create render buffer with depth and stencil
        let renderbuffer = Gl.GenRenderbuffer ()
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, renderbuffer)
        Gl.RenderbufferStorage (RenderbufferTarget.Renderbuffer, InternalFormat.Depth24Stencil8, Constants.Render.Resolution.X, Constants.Render.Resolution.Y)
        Gl.FramebufferRenderbuffer (FramebufferTarget.Framebuffer, FramebufferAttachment.DepthStencilAttachment, RenderbufferTarget.Renderbuffer, renderbuffer)
        Hl.Assert ()

        // ensure framebuffer is complete
        if Gl.CheckFramebufferStatus FramebufferTarget.Framebuffer = FramebufferStatus.FramebufferComplete then
            let position = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = positionId }
            let albedo = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = albedoId }
            let material = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = materialId }
            let normalPlus = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = normalPlusId }
            Right (position, albedo, material, normalPlus, renderbuffer, framebuffer)
        else Left "Could not create complete geometry framebuffer."

    /// Destroy geometry buffers.
    let DestroyGeometryBuffers (position : Texture.Texture, albedo : Texture.Texture, material : Texture.Texture, normalPlus : Texture.Texture, renderbuffer, framebuffer) =
        Gl.DeleteRenderbuffers [|renderbuffer|]
        Gl.DeleteFramebuffers [|framebuffer|]
        position.Destroy ()
        albedo.Destroy ()
        material.Destroy ()
        normalPlus.Destroy ()

    /// Create light mapping buffers.
    let TryCreateLightMappingBuffers () =

        // create frame buffer object
        let framebuffer = Gl.GenFramebuffer ()
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // create light mapping buffer
        let lightMappingId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, lightMappingId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rgba32f, Constants.Render.Resolution.X, Constants.Render.Resolution.Y, 0, PixelFormat.Rgba, PixelType.Float, nativeint 0)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int TextureMinFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int TextureMagFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
        Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, TextureTarget.Texture2d, lightMappingId, 0)
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // associate draw buffers
        Gl.DrawBuffers [|int FramebufferAttachment.ColorAttachment0|]
        Hl.Assert ()

        // create render buffer with depth and stencil
        let renderbuffer = Gl.GenRenderbuffer ()
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, renderbuffer)
        Gl.RenderbufferStorage (RenderbufferTarget.Renderbuffer, InternalFormat.Depth24Stencil8, Constants.Render.Resolution.X, Constants.Render.Resolution.Y)
        Gl.FramebufferRenderbuffer (FramebufferTarget.Framebuffer, FramebufferAttachment.DepthStencilAttachment, RenderbufferTarget.Renderbuffer, renderbuffer)
        Hl.Assert ()

        // ensure framebuffer is complete
        if Gl.CheckFramebufferStatus FramebufferTarget.Framebuffer = FramebufferStatus.FramebufferComplete then
            let lightMapping = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = lightMappingId }
            Right (lightMapping, renderbuffer, framebuffer)
        else Left "Could not create complete light mapping framebuffer."

    /// Destroy light mapping buffers.
    let DestroyLightMappingBuffers (lightMapping : Texture.Texture, renderbuffer, framebuffer) =
        Gl.DeleteRenderbuffers [|renderbuffer|]
        Gl.DeleteFramebuffers [|framebuffer|]
        lightMapping.Destroy ()

    /// Create irradiance buffers.
    let TryCreateIrradianceBuffers () =

        // create frame buffer object
        let framebuffer = Gl.GenFramebuffer ()
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // create irradiance buffer
        let irradianceId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, irradianceId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rgba32f, Constants.Render.Resolution.X, Constants.Render.Resolution.Y, 0, PixelFormat.Rgba, PixelType.Float, nativeint 0)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int TextureMinFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int TextureMagFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
        Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, TextureTarget.Texture2d, irradianceId, 0)
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // associate draw buffers
        Gl.DrawBuffers [|int FramebufferAttachment.ColorAttachment0|]
        Hl.Assert ()

        // create render buffer with depth and stencil
        let renderbuffer = Gl.GenRenderbuffer ()
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, renderbuffer)
        Gl.RenderbufferStorage (RenderbufferTarget.Renderbuffer, InternalFormat.Depth24Stencil8, Constants.Render.Resolution.X, Constants.Render.Resolution.Y)
        Gl.FramebufferRenderbuffer (FramebufferTarget.Framebuffer, FramebufferAttachment.DepthStencilAttachment, RenderbufferTarget.Renderbuffer, renderbuffer)
        Hl.Assert ()

        // ensure framebuffer is complete
        if Gl.CheckFramebufferStatus FramebufferTarget.Framebuffer = FramebufferStatus.FramebufferComplete then
            let irradiance = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = irradianceId }
            Right (irradiance, renderbuffer, framebuffer)
        else Left "Could not create complete irradiance framebuffer."

    /// Destroy irradiance buffers.
    let DestroyIrradianceBuffers (irradiance : Texture.Texture, renderbuffer, framebuffer) =
        Gl.DeleteRenderbuffers [|renderbuffer|]
        Gl.DeleteFramebuffers [|framebuffer|]
        irradiance.Destroy ()

    /// Create environment filter buffers.
    let TryCreateEnvironmentFilterBuffers () =

        // create frame buffer object
        let framebuffer = Gl.GenFramebuffer ()
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // create environmentFilter buffer
        let environmentFilterId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, environmentFilterId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rgba32f, Constants.Render.Resolution.X, Constants.Render.Resolution.Y, 0, PixelFormat.Rgba, PixelType.Float, nativeint 0)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int TextureMinFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int TextureMagFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
        Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, TextureTarget.Texture2d, environmentFilterId, 0)
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // associate draw buffers
        Gl.DrawBuffers [|int FramebufferAttachment.ColorAttachment0|]
        Hl.Assert ()

        // create render buffer with depth and stencil
        let renderbuffer = Gl.GenRenderbuffer ()
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, renderbuffer)
        Gl.RenderbufferStorage (RenderbufferTarget.Renderbuffer, InternalFormat.Depth24Stencil8, Constants.Render.Resolution.X, Constants.Render.Resolution.Y)
        Gl.FramebufferRenderbuffer (FramebufferTarget.Framebuffer, FramebufferAttachment.DepthStencilAttachment, RenderbufferTarget.Renderbuffer, renderbuffer)
        Hl.Assert ()

        // ensure framebuffer is complete
        if Gl.CheckFramebufferStatus FramebufferTarget.Framebuffer = FramebufferStatus.FramebufferComplete then
            let environmentFilter = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = environmentFilterId }
            Right (environmentFilter, renderbuffer, framebuffer)
        else Left "Could not create complete environment filter framebuffer."

    /// Destroy environment filter buffers.
    let DestroyEnvironmentFilterBuffers (environmentFilter : Texture.Texture, renderbuffer, framebuffer) =
        Gl.DeleteRenderbuffers [|renderbuffer|]
        Gl.DeleteFramebuffers [|framebuffer|]
        environmentFilter.Destroy ()

    /// Create ssao buffers.
    let TryCreateSsaoBuffers () =

        // create frame buffer object
        let framebuffer = Gl.GenFramebuffer ()
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // create ssao buffer
        let ssaoId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, ssaoId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.R32f, Constants.Render.SsaoResolution.X, Constants.Render.SsaoResolution.Y, 0, PixelFormat.Red, PixelType.Float, nativeint 0)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int TextureMinFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int TextureMagFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
        Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, TextureTarget.Texture2d, ssaoId, 0)
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // associate draw buffers
        Gl.DrawBuffers [|int FramebufferAttachment.ColorAttachment0|]
        Hl.Assert ()

        // create render buffer with depth and stencil
        let renderbuffer = Gl.GenRenderbuffer ()
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, renderbuffer)
        Gl.RenderbufferStorage (RenderbufferTarget.Renderbuffer, InternalFormat.Depth24Stencil8, Constants.Render.SsaoResolution.X, Constants.Render.SsaoResolution.Y)
        Gl.FramebufferRenderbuffer (FramebufferTarget.Framebuffer, FramebufferAttachment.DepthStencilAttachment, RenderbufferTarget.Renderbuffer, renderbuffer)
        Hl.Assert ()

        // ensure framebuffer is complete
        if Gl.CheckFramebufferStatus FramebufferTarget.Framebuffer = FramebufferStatus.FramebufferComplete then
            let ssao = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = ssaoId }
            Right (ssao, renderbuffer, framebuffer)
        else Left "Could not create complete ssao framebuffer."

    /// Destroy ssao buffers.
    let DestroySsaoBuffers (ssao : Texture.Texture, renderbuffer, framebuffer) =
        Gl.DeleteRenderbuffers [|renderbuffer|]
        Gl.DeleteFramebuffers [|framebuffer|]
        ssao.Destroy ()