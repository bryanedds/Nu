// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace OpenGL
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Framebuffer =

    /// Attempt to create texture 2d buffers.
    let TryCreateTextureBuffers (resolutionX, resolutionY) =

        // create frame buffer object
        let framebuffer = Gl.GenFramebuffer ()
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // create texture 2d buffer
        let textureId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, textureId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rgba16f, resolutionX, resolutionY, 0, PixelFormat.Rgba, PixelType.HalfFloat, nativeint 0)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int TextureMinFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int TextureMagFilter.Nearest)
        Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, TextureTarget.Texture2d, textureId, 0)
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // create depth and stencil buffers
        let depthStencilBuffer = Gl.GenRenderbuffer ()
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, depthStencilBuffer)
        Gl.RenderbufferStorage (RenderbufferTarget.Renderbuffer, InternalFormat.Depth24Stencil8, resolutionX, resolutionY)
        Gl.FramebufferRenderbuffer (FramebufferTarget.Framebuffer, FramebufferAttachment.DepthStencilAttachment, RenderbufferTarget.Renderbuffer, depthStencilBuffer)
        Hl.Assert ()

        // ensure framebuffer is complete
        if Gl.CheckFramebufferStatus FramebufferTarget.Framebuffer = FramebufferStatus.FramebufferComplete then
            let texture = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = textureId }
            Right (texture, framebuffer)
        else Left "Could not create complete texture 2d framebuffer."

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
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.R16f, resolutionX, resolutionY, 0, PixelFormat.Red, PixelType.HalfFloat, nativeint 0)
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
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rg16f, resolutionX, resolutionY, 0, PixelFormat.Rg, PixelType.HalfFloat, nativeint 0)
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

    /// Create filter bilateral down-sample buffers.
    let TryCreateFilterBilateralDownSampleBuffers (resolutionX, resolutionY) =

        // create frame buffer object
        let framebuffer = Gl.GenFramebuffer ()
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // create color down-sample buffer
        let colorDownSampleId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, colorDownSampleId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rgba16f, resolutionX, resolutionY, 0, PixelFormat.Rgba, PixelType.HalfFloat, nativeint 0)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
        Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, TextureTarget.Texture2d, colorDownSampleId, 0)
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // create depth down-sample buffer
        let depthDownSampleId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, depthDownSampleId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.R16f, resolutionX, resolutionY, 0, PixelFormat.Red, PixelType.HalfFloat, nativeint 0)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
        Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment1, TextureTarget.Texture2d, depthDownSampleId, 0)
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // associate draw buffers
        Gl.DrawBuffers
            [|int FramebufferAttachment.ColorAttachment0
              int FramebufferAttachment.ColorAttachment1|]
        Hl.Assert ()

        // create render buffer with depth and stencil
        let renderbuffer = Gl.GenRenderbuffer ()
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, renderbuffer)
        Gl.RenderbufferStorage (RenderbufferTarget.Renderbuffer, InternalFormat.Depth24Stencil8, resolutionX, resolutionY)
        Gl.FramebufferRenderbuffer (FramebufferTarget.Framebuffer, FramebufferAttachment.DepthStencilAttachment, RenderbufferTarget.Renderbuffer, renderbuffer)
        Hl.Assert ()

        // ensure framebuffer is complete
        if Gl.CheckFramebufferStatus FramebufferTarget.Framebuffer = FramebufferStatus.FramebufferComplete then
            let colorDownSample = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = colorDownSampleId }
            let depthDownSample = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = depthDownSampleId }
            Right (colorDownSample, depthDownSample, renderbuffer, framebuffer)
        else Left "Could not create complete filter down-sample bilateral framebuffer."

    /// Destroy filter fog accum buffers.
    let DestroyFilterBilateralBuffers (downSample : Texture.Texture, upSample : Texture.Texture, framebuffer, renderbuffer) =
        Gl.DeleteRenderbuffers [|renderbuffer|]
        Gl.DeleteFramebuffers [|framebuffer|]
        downSample.Destroy ()
        upSample.Destroy ()

    /// Create filter buffers.
    let TryCreateFilterBuffers (resolutionX, resolutionY) =

        // create frame buffer object
        let framebuffer = Gl.GenFramebuffer ()
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // create filter buffer
        let filterId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, filterId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rgba16f, resolutionX, resolutionY, 0, PixelFormat.Rgba, PixelType.HalfFloat, nativeint 0)
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

    /// Attempt to create HDR buffers.
    let TryCreateHdrBuffers (resolutionX, resolutionY) =

        // create frame buffer object
        let framebuffer = Gl.GenFramebuffer ()
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // create color buffer
        let colorId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, colorId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rgba16f, resolutionX, resolutionY, 0, PixelFormat.Rgba, PixelType.HalfFloat, nativeint 0)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int TextureMinFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int TextureMagFilter.Nearest)
        Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, TextureTarget.Texture2d, colorId, 0)
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
            let color = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = colorId }
            Right (color, framebuffer, renderbuffer)
        else Left "Could not create complete post-lighting framebuffer."

    /// Destroy HDR buffers.
    let DestroyHdrBuffers (color : Texture.Texture, framebuffer, renderbuffer) =
        Gl.DeleteRenderbuffers [|renderbuffer|]
        Gl.DeleteFramebuffers [|framebuffer|]
        color.Destroy ()

    /// Create shadow texture buffers.
    let TryCreateShadowTextureBuffers (shadowResolutionX, shadowResolutionY) =

        // create frame buffer object
        let framebuffer = Gl.GenFramebuffer ()
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // create shadow texture
        let shadowTextureId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, shadowTextureId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rg32f, shadowResolutionX, shadowResolutionY, 0, PixelFormat.Rg, PixelType.Float, nativeint 0)
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

    /// Destroy shadow texture buffers.
    let DestroyShadowTextureBuffers (shadowTexture : Texture.Texture, renderbuffer, framebuffer) =
        Gl.DeleteRenderbuffers [|renderbuffer|]
        Gl.DeleteFramebuffers [|framebuffer|]
        shadowTexture.Destroy ()

    let TryCreateShadowMapBuffers (shadowResolutionX, shadowResolutionY) =

        // create shadow renderbuffer
        let shadowRenderbuffer = Gl.GenRenderbuffer ()
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, shadowRenderbuffer)
        Gl.RenderbufferStorage (RenderbufferTarget.Renderbuffer, InternalFormat.Depth24Stencil8, shadowResolutionX, shadowResolutionY)
        Hl.Assert ()

        // create shadow framebuffer
        let shadowFramebuffer = Gl.GenFramebuffer ()
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, shadowFramebuffer)
        Gl.FramebufferRenderbuffer (FramebufferTarget.Framebuffer, FramebufferAttachment.DepthStencilAttachment, RenderbufferTarget.Renderbuffer, shadowRenderbuffer)
        Hl.Assert ()

        // create shadow map
        let shadowMapId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.TextureCubeMap, shadowMapId)
        Gl.FramebufferTexture (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, shadowMapId, 0)
        Hl.Assert ()

        // setup shadow map textures
        for i in 0 .. dec 6 do
            let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i)
            Gl.TexImage2D (target, 0, InternalFormat.R32f, shadowResolutionX, shadowResolutionY, 0, PixelFormat.Red, PixelType.Float, nativeint 0)
            Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, target, shadowMapId, 0)
            Hl.Assert ()
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapR, int TextureWrapMode.ClampToEdge)
        Gl.BindTexture (TextureTarget.TextureCubeMap, 0u)
        Hl.Assert ()

        // assert shadow map framebuffer completion
        let result =
            if Gl.CheckFramebufferStatus FramebufferTarget.Framebuffer = FramebufferStatus.FramebufferComplete then
                let shadowMap = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = shadowMapId }
                Right (shadowMap, shadowRenderbuffer, shadowFramebuffer)
            else Left "Shadow map framebuffer is incomplete!"

        // teardown attachments
        for i in 0 .. dec 6 do
            let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i)
            Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, target, 0u, 0)
            Hl.Assert ()

        result

    /// Destroy shadow map buffers.
    let DestroyShadowMapBuffers (shadowMap : Texture.Texture, renderbuffer, framebuffer) =
        Gl.DeleteRenderbuffers [|renderbuffer|]
        Gl.DeleteFramebuffers [|framebuffer|]
        shadowMap.Destroy ()

    /// Create a geometry buffers.
    let TryCreateGeometryBuffers (resolutionX, resolutionY) =

        // create frame buffer object
        let framebuffer = Gl.GenFramebuffer ()
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // create position buffer
        let positionId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, positionId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rgba32f, resolutionX, resolutionY, 0, PixelFormat.Rgba, PixelType.Float, nativeint 0)
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
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rgba16f, resolutionX, resolutionY, 0, PixelFormat.Rgba, PixelType.HalfFloat, nativeint 0)
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
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rgba16f, resolutionX, resolutionY, 0, PixelFormat.Rgba, PixelType.HalfFloat, nativeint 0)
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
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rgba16f, resolutionX, resolutionY, 0, PixelFormat.Rgba, PixelType.HalfFloat, nativeint 0)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int TextureMinFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int TextureMagFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
        Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment3, TextureTarget.Texture2d, normalPlusId, 0)
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // create subdermal plus buffer
        let subdermalPlusId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, subdermalPlusId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rgba16f, resolutionX, resolutionY, 0, PixelFormat.Rgba, PixelType.HalfFloat, nativeint 0)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int TextureMinFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int TextureMagFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
        Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment4, TextureTarget.Texture2d, subdermalPlusId, 0)
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // create scatter plus buffer
        let scatterPlusId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, scatterPlusId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rgba16f, resolutionX, resolutionY, 0, PixelFormat.Rgba, PixelType.HalfFloat, nativeint 0)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int TextureMinFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int TextureMagFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
        Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment5, TextureTarget.Texture2d, scatterPlusId, 0)
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // associate draw buffers
        Gl.DrawBuffers
            [|int FramebufferAttachment.ColorAttachment0
              int FramebufferAttachment.ColorAttachment1
              int FramebufferAttachment.ColorAttachment2
              int FramebufferAttachment.ColorAttachment3
              int FramebufferAttachment.ColorAttachment4
              int FramebufferAttachment.ColorAttachment5|]
        Hl.Assert ()

        // create render buffer with depth and stencil
        let renderbuffer = Gl.GenRenderbuffer ()
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, renderbuffer)
        Gl.RenderbufferStorage (RenderbufferTarget.Renderbuffer, InternalFormat.Depth24Stencil8, resolutionX, resolutionY)
        Gl.FramebufferRenderbuffer (FramebufferTarget.Framebuffer, FramebufferAttachment.DepthStencilAttachment, RenderbufferTarget.Renderbuffer, renderbuffer)
        Hl.Assert ()

        // ensure framebuffer is complete
        if Gl.CheckFramebufferStatus FramebufferTarget.Framebuffer = FramebufferStatus.FramebufferComplete then
            let position = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = positionId }
            let albedo = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = albedoId }
            let material = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = materialId }
            let normalPlus = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = normalPlusId }
            let subdermalPlus = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = subdermalPlusId }
            let scatterPlus = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = scatterPlusId }
            Right (position, albedo, material, normalPlus, subdermalPlus, scatterPlus, renderbuffer, framebuffer)
        else Left "Could not create complete geometry framebuffer."

    /// Destroy geometry buffers.
    let DestroyGeometryBuffers (position : Texture.Texture, albedo : Texture.Texture, material : Texture.Texture, normalPlus : Texture.Texture, subdermalPlus : Texture.Texture, scatterPlus : Texture.Texture, renderbuffer, framebuffer) =
        Gl.DeleteRenderbuffers [|renderbuffer|]
        Gl.DeleteFramebuffers [|framebuffer|]
        position.Destroy ()
        albedo.Destroy ()
        material.Destroy ()
        normalPlus.Destroy ()
        subdermalPlus.Destroy ()
        scatterPlus.Destroy ()

    /// Create light mapping buffers.
    let TryCreateLightMappingBuffers (resolutionX, resolutionY) =

        // create frame buffer object
        let framebuffer = Gl.GenFramebuffer ()
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // create light mapping buffer
        let lightMappingId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, lightMappingId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rgba16f, resolutionX, resolutionY, 0, PixelFormat.Rgba, PixelType.HalfFloat, nativeint 0)
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
        Gl.RenderbufferStorage (RenderbufferTarget.Renderbuffer, InternalFormat.Depth24Stencil8, resolutionX, resolutionY)
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
    let TryCreateIrradianceBuffers (resolutionX, resolutionY) =

        // create frame buffer object
        let framebuffer = Gl.GenFramebuffer ()
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // create irradiance buffer
        let irradianceId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, irradianceId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rgba16f, resolutionX, resolutionY, 0, PixelFormat.Rgba, PixelType.HalfFloat, nativeint 0)
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
        Gl.RenderbufferStorage (RenderbufferTarget.Renderbuffer, InternalFormat.Depth24Stencil8, resolutionX, resolutionY)
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
    let TryCreateEnvironmentFilterBuffers (resolutionX, resolutionY) =

        // create frame buffer object
        let framebuffer = Gl.GenFramebuffer ()
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // create environment filter buffer
        let environmentFilterId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, environmentFilterId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rgba16f, resolutionX, resolutionY, 0, PixelFormat.Rgba, PixelType.HalfFloat, nativeint 0)
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
        Gl.RenderbufferStorage (RenderbufferTarget.Renderbuffer, InternalFormat.Depth24Stencil8, resolutionX, resolutionY)
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
    let TryCreateSsaoBuffers (ssaoResolutionX, ssaoResolutionY) =

        // create frame buffer object
        let framebuffer = Gl.GenFramebuffer ()
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // create ssao buffer
        let ssaoId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, ssaoId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.R16f, ssaoResolutionX, ssaoResolutionY, 0, PixelFormat.Red, PixelType.HalfFloat, nativeint 0)
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
        Gl.RenderbufferStorage (RenderbufferTarget.Renderbuffer, InternalFormat.Depth24Stencil8, ssaoResolutionX, ssaoResolutionY)
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

    /// Attempt to create lighting buffers.
    let TryCreateLightingBuffers (resolutionX, resolutionY) =

        // create frame buffer object
        let framebuffer = Gl.GenFramebuffer ()
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // create color buffer
        let colorId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, colorId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rgba16f, resolutionX, resolutionY, 0, PixelFormat.Rgba, PixelType.HalfFloat, nativeint 0)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int TextureMinFilter.Nearest)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int TextureMagFilter.Nearest)
        Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, TextureTarget.Texture2d, colorId, 0)
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // create fog accum buffer (using linear filtering since it's the source for a down-sampling filter)
        let fogAccumId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, fogAccumId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.Rgba16, resolutionX, resolutionY, 0, PixelFormat.Rgba, PixelType.HalfFloat, nativeint 0)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
        Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment1, TextureTarget.Texture2d, fogAccumId, 0)
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // create depth buffer (using linear filtering since it's the source for a down-sampling filter)
        let depthId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.Texture2d, depthId)
        Gl.TexImage2D (TextureTarget.Texture2d, 0, InternalFormat.R16f, resolutionX, resolutionY, 0, PixelFormat.Red, PixelType.HalfFloat, nativeint 0)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear)
        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
        Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment2, TextureTarget.Texture2d, depthId, 0)
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // associate draw buffers
        Gl.DrawBuffers
            [|int FramebufferAttachment.ColorAttachment0
              int FramebufferAttachment.ColorAttachment1
              int FramebufferAttachment.ColorAttachment2|]
        Hl.Assert ()

        // create render buffer with depth and stencil
        let renderbuffer = Gl.GenRenderbuffer ()
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, renderbuffer)
        Gl.RenderbufferStorage (RenderbufferTarget.Renderbuffer, InternalFormat.Depth24Stencil8, resolutionX, resolutionY)
        Gl.FramebufferRenderbuffer (FramebufferTarget.Framebuffer, FramebufferAttachment.DepthStencilAttachment, RenderbufferTarget.Renderbuffer, renderbuffer)
        Hl.Assert ()

        // ensure framebuffer is complete
        if Gl.CheckFramebufferStatus FramebufferTarget.Framebuffer = FramebufferStatus.FramebufferComplete then
            let color = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = colorId }
            let fogAccum = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = fogAccumId }
            let depth = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = depthId }
            Right (color, fogAccum, depth, framebuffer, renderbuffer)
        else Left "Could not create complete lighting framebuffer."

    /// Destroy lighting buffers.
    let DestroyLightingBuffers (color : Texture.Texture, fogAccum : Texture.Texture, depth : Texture.Texture, framebuffer, renderbuffer) =
        Gl.DeleteRenderbuffers [|renderbuffer|]
        Gl.DeleteFramebuffers [|framebuffer|]
        color.Destroy ()
        fogAccum.Destroy ()
        depth.Destroy ()