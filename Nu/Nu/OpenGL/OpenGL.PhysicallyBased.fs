﻿// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace OpenGL
open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Numerics
open System.Runtime.InteropServices
open FSharp.NativeInterop
open Prime
open Nu

[<RequireQualifiedAccess>]
module PhysicallyBased =

    /// A set of physically-based buffers that support a given viewport.
    type PhysicallyBasedBuffers =
        { ShadowTextureBuffersArray : (OpenGL.Texture.Texture * uint * uint) array
          ShadowTextureBuffers2Array : (OpenGL.Texture.Texture * uint * uint) array
          ShadowMapBuffersArray : (OpenGL.Texture.Texture * uint * uint) array
          GeometryBuffers : OpenGL.Texture.Texture * OpenGL.Texture.Texture * OpenGL.Texture.Texture * OpenGL.Texture.Texture * uint * uint
          LightMappingBuffers : OpenGL.Texture.Texture * uint * uint
          AmbientBuffers : OpenGL.Texture.Texture * uint * uint
          IrradianceBuffers : OpenGL.Texture.Texture * uint * uint
          EnvironmentFilterBuffers : OpenGL.Texture.Texture * uint * uint
          SsaoBuffersUnfiltered : OpenGL.Texture.Texture * uint * uint
          SsaoBuffersFiltered : OpenGL.Texture.Texture * uint * uint
          LightingBuffers : OpenGL.Texture.Texture * OpenGL.Texture.Texture * OpenGL.Texture.Texture * uint * uint
          SpecularScreenDownSampleBuffers : OpenGL.Texture.Texture * OpenGL.Texture.Texture * uint * uint
          SpecularScreenUpSampleBuffers : OpenGL.Texture.Texture * uint * uint
          FogAccumDownSampleBuffers : OpenGL.Texture.Texture * OpenGL.Texture.Texture * uint * uint
          FogAccumUpSampleBuffers : OpenGL.Texture.Texture * uint * uint
          CompositionBuffers : OpenGL.Texture.Texture * uint * uint }

    /// Describes the configurable properties of a physically-based material.
    type PhysicallyBasedMaterialProperties =
        { Albedo : Color
          Roughness : single
          Metallic : single
          AmbientOcclusion : single
          Emission : single
          Height : single
          IgnoreLightMaps : bool
          OpaqueDistance : single }

        /// The empty material properties.
        static member empty =
            { Albedo = Color.Zero
              Roughness = 0.0f
              Metallic = 0.0f
              AmbientOcclusion = 0.0f
              Emission = 0.0f
              Height = 0.0f
              IgnoreLightMaps = false
              OpaqueDistance = 0.0f }

    /// Describes a physically-based material.
    type PhysicallyBasedMaterial =
        { AlbedoTexture : Texture.Texture
          RoughnessTexture : Texture.Texture
          MetallicTexture : Texture.Texture
          AmbientOcclusionTexture : Texture.Texture
          EmissionTexture : Texture.Texture
          NormalTexture : Texture.Texture
          HeightTexture : Texture.Texture
          TwoSided : bool }

        /// The empty material.
        static member empty =
            { AlbedoTexture = Texture.EmptyTexture
              RoughnessTexture = Texture.EmptyTexture
              MetallicTexture = Texture.EmptyTexture
              AmbientOcclusionTexture = Texture.EmptyTexture
              EmissionTexture = Texture.EmptyTexture
              NormalTexture = Texture.EmptyTexture
              HeightTexture = Texture.EmptyTexture
              TwoSided = false }

    /// Describes some physically-based geometry that's loaded into VRAM.
    type PhysicallyBasedGeometry =
        { Bounds : Box3
          PrimitiveType : PrimitiveType
          ElementCount : int
          Vertices : Vector3 array
          Indices : int array
          VertexBuffer : uint
          InstanceBuffer : uint
          IndexBuffer : uint
          PhysicallyBasedVao : uint }

    /// Describes a renderable physically-based surface.
    type [<CustomEquality; NoComparison>] PhysicallyBasedSurface =
        { HashCode : int
          SurfaceNames : string array
          SurfaceMatrixIsIdentity : bool // OPTIMIZATION: avoid matrix multiply when unnecessary.
          SurfaceMatrix : Matrix4x4
          SurfaceBounds : Box3
          SurfaceMaterialProperties : PhysicallyBasedMaterialProperties
          SurfaceMaterial : PhysicallyBasedMaterial
          SurfaceMaterialIndex : int
          SurfaceNode : Assimp.Node
          PhysicallyBasedGeometry : PhysicallyBasedGeometry }

        static member extractPresence presenceDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.PresenceOpt with
            | ValueNone ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    ValueOption.defaultValue presenceDefault material.PresenceOpt
                | Some _ | None -> presenceDefault
            | ValueSome presence -> presence

        static member extractRenderStyle renderStyleDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.RenderStyleOpt with
            | ValueNone ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    ValueOption.defaultValue renderStyleDefault material.RenderStyleOpt
                | Some _ | None -> renderStyleDefault
            | ValueSome renderStyle -> renderStyle

        static member extractIgnoreLightMaps ignoreLightMapsDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.IgnoreLightMapsOpt with
            | ValueNone ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    ValueOption.defaultValue ignoreLightMapsDefault material.IgnoreLightMapsOpt
                | Some _ | None -> ignoreLightMapsDefault
            | ValueSome ignoreLightMaps -> ignoreLightMaps

        static member extractOpaqueDistance opaqueDistanceDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.OpaqueDistanceOpt with
            | ValueNone ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    ValueOption.defaultValue opaqueDistanceDefault material.OpaqueDistanceOpt
                | Some _ | None -> opaqueDistanceDefault
            | ValueSome opaqueDistance -> opaqueDistance

        static member extractNavShape shapeDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.NavShapeOpt with
            | ValueNone ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    ValueOption.defaultValue shapeDefault material.NavShapeOpt
                | Some _ | None -> shapeDefault
            | ValueSome shape -> shape

        static member inline hash surface =
            surface.HashCode

        static member equals left right =
            refEq left right || // OPTIMIZATION: first check ref equality.
            left.HashCode = right.HashCode && // OPTIMIZATION: check hash equality to bail as quickly as possible.
            left.SurfaceMaterial.AlbedoTexture = right.SurfaceMaterial.AlbedoTexture &&
            left.SurfaceMaterial.RoughnessTexture = right.SurfaceMaterial.RoughnessTexture &&
            left.SurfaceMaterial.MetallicTexture = right.SurfaceMaterial.MetallicTexture &&
            left.SurfaceMaterial.AmbientOcclusionTexture = right.SurfaceMaterial.AmbientOcclusionTexture &&
            left.SurfaceMaterial.EmissionTexture = right.SurfaceMaterial.EmissionTexture &&
            left.SurfaceMaterial.NormalTexture = right.SurfaceMaterial.NormalTexture &&
            left.SurfaceMaterial.HeightTexture = right.SurfaceMaterial.HeightTexture &&
            left.SurfaceMaterial.TwoSided = right.SurfaceMaterial.TwoSided &&
            left.PhysicallyBasedGeometry.PrimitiveType = right.PhysicallyBasedGeometry.PrimitiveType &&
            left.PhysicallyBasedGeometry.PhysicallyBasedVao = right.PhysicallyBasedGeometry.PhysicallyBasedVao

        static member make names (surfaceMatrix : Matrix4x4) bounds properties material materialIndex surfaceNode geometry =
            let hashCode =
                (hash material.AlbedoTexture) ^^^
                (hash material.RoughnessTexture <<< 2) ^^^
                (hash material.MetallicTexture <<< 4) ^^^
                (hash material.AmbientOcclusionTexture <<< 6) ^^^
                (hash material.EmissionTexture <<< 8) ^^^
                (hash material.NormalTexture <<< 10) ^^^
                (hash material.HeightTexture <<< 12) ^^^
                (hash material.TwoSided <<< 14) ^^^
                (int geometry.PrimitiveType <<< 16) ^^^
                (int geometry.PhysicallyBasedVao <<< 18)
            { HashCode = hashCode
              SurfaceNames = names
              SurfaceMatrixIsIdentity = surfaceMatrix.IsIdentity
              SurfaceMatrix = surfaceMatrix
              SurfaceBounds = bounds
              SurfaceMaterialProperties = properties
              SurfaceMaterial = material
              SurfaceMaterialIndex = materialIndex
              SurfaceNode = surfaceNode
              PhysicallyBasedGeometry = geometry }

        member this.Equals that =
            PhysicallyBasedSurface.equals this that

        override this.Equals (thatObj : obj) =
            match thatObj with
            | :? PhysicallyBasedSurface as that -> PhysicallyBasedSurface.equals this that
            | _ -> false

        override this.GetHashCode () =
            this.HashCode

    module internal PhysicallyBasedSurfaceFns =
        let extractPresence = PhysicallyBasedSurface.extractPresence
        let extractRenderStyle = PhysicallyBasedSurface.extractRenderStyle
        let extractIgnoreLightMaps = PhysicallyBasedSurface.extractIgnoreLightMaps
        let extractOpaqueDistance = PhysicallyBasedSurface.extractOpaqueDistance
        let extractNavShape = PhysicallyBasedSurface.extractNavShape
        let hash = PhysicallyBasedSurface.hash
        let equals = PhysicallyBasedSurface.equals
        let comparer = HashIdentity.FromFunctions hash equals

    /// A light probe inside a physically-based static model.
    type PhysicallyBasedLightProbe =
        { LightProbeNames : string array
          LightProbeMatrixIsIdentity : bool
          LightProbeMatrix : Matrix4x4
          LightProbeBounds : Box3 }

    /// A light inside a physically-based static model.
    type PhysicallyBasedLight =
        { LightNames : string array
          LightMatrixIsIdentity : bool
          LightMatrix : Matrix4x4
          LightColor : Color
          LightBrightness : single
          LightAttenuationLinear : single
          LightAttenuationQuadratic : single
          LightCutoff : single
          LightType : LightType
          LightDesireShadows : bool }

    /// A part of a physically-based hierarchy.
    type PhysicallyBasedPart =
        | PhysicallyBasedNode of string array
        | PhysicallyBasedLightProbe of PhysicallyBasedLightProbe
        | PhysicallyBasedLight of PhysicallyBasedLight
        | PhysicallyBasedSurface of PhysicallyBasedSurface

    /// A physically-based model.
    type PhysicallyBasedModel =
        { Animated : bool
          Bounds : Box3
          LightProbes : PhysicallyBasedLightProbe array
          Lights : PhysicallyBasedLight array
          Surfaces : PhysicallyBasedSurface array
          SceneOpt : Assimp.Scene option
          PhysicallyBasedHierarchy : PhysicallyBasedPart array TreeNode }

    /// Describes a physically-based shader that's loaded into GPU.
    type PhysicallyBasedShader =
        { ViewUniform : int
          ProjectionUniform : int
          BonesUniforms : int array
          EyeCenterUniform : int
          LightCutoffMarginUniform : int
          LightAmbientColorUniform : int
          LightAmbientBrightnessUniform : int
          LightShadowSamplesUniform : int
          LightShadowBiasUniform : int
          LightShadowSampleScalarUniform : int
          LightShadowExponentUniform : int
          LightShadowDensityUniform : int
          SsvfEnabledUniform : int
          SsvfStepsUniform : int
          SsvfAsymmetryUniform : int
          SsvfIntensityUniform : int
          AlbedoTextureUniform : int
          RoughnessTextureUniform : int
          MetallicTextureUniform : int
          AmbientOcclusionTextureUniform : int
          EmissionTextureUniform : int
          NormalTextureUniform : int
          HeightTextureUniform : int
          BrdfTextureUniform : int
          IrradianceMapUniform : int
          EnvironmentFilterMapUniform : int
          IrradianceMapsUniforms : int array
          EnvironmentFilterMapsUniforms : int array
          ShadowTexturesUniforms : int array
          ShadowMapsUniforms : int array
          LightMapOriginsUniform : int
          LightMapMinsUniform : int
          LightMapSizesUniform : int
          LightMapAmbientColorsUniform : int
          LightMapAmbientBrightnessesUniform : int
          LightMapsCountUniform : int
          LightOriginsUniform : int
          LightDirectionsUniform : int
          LightColorsUniform : int
          LightBrightnessesUniform : int
          LightAttenuationLinearsUniform : int
          LightAttenuationQuadraticsUniform : int
          LightCutoffsUniform : int
          LightTypesUniform : int
          LightConeInnersUniform : int
          LightConeOutersUniform : int
          LightShadowIndicesUniform : int
          LightsCountUniform : int
          ShadowMatricesUniforms : int array
          PhysicallyBasedShader : uint }

    /// Describes a physically-based deferred terrain shader that's loaded into GPU.
    type PhysicallyBasedDeferredTerrainShader =
        { ViewUniform : int
          ProjectionUniform : int
          EyeCenterUniform : int
          LightShadowSamplesUniform : int
          LightShadowBiasUniform : int
          LightShadowSampleScalarUniform : int
          LightShadowExponentUniform : int
          LightShadowDensityUniform : int
          LayersCountUniform : int
          AlbedoTexturesUniforms : int array
          RoughnessTexturesUniforms : int array
          AmbientOcclusionTexturesUniforms : int array
          NormalTexturesUniforms : int array
          HeightTexturesUniforms : int array
          PhysicallyBasedShader : uint }

    /// Describes a light mapping pass of a deferred physically-based shader that's loaded into GPU.
    type PhysicallyBasedDeferredLightMappingShader =
        { PositionTextureUniform : int
          NormalPlusTextureUniform : int
          LightMapOriginsUniform : int
          LightMapMinsUniform : int
          LightMapSizesUniform : int
          LightMapsCountUniform : int
          PhysicallyBasedDeferredLightMappingShader : uint }

    /// Describes an ambient pass of a deferred physically-based shader that's loaded into GPU.
    type PhysicallyBasedDeferredAmbientShader =
        { PositionTextureUniform : int
          LightMappingTextureUniform : int
          LightMapAmbientColorUniform : int
          LightMapAmbientBrightnessUniform : int
          LightMapAmbientColorsUniform : int
          LightMapAmbientBrightnessesUniform : int
          PhysicallyBasedDeferredAmbientShader : uint }

    /// Describes an irradiance pass of a deferred physically-based shader that's loaded into GPU.
    type PhysicallyBasedDeferredIrradianceShader =
        { PositionTextureUniform : int
          NormalPlusTextureUniform : int
          LightMappingTextureUniform : int
          IrradianceMapUniform : int
          IrradianceMapsUniforms : int array
          PhysicallyBasedDeferredIrradianceShader : uint }

    /// Describes an environment filter pass of a deferred physically-based shader that's loaded into GPU.
    type PhysicallyBasedDeferredEnvironmentFilterShader =
        { EyeCenterUniform : int
          PositionTextureUniform : int
          MaterialTextureUniform : int
          NormalPlusTextureUniform : int
          LightMappingTextureUniform : int
          EnvironmentFilterMapUniform : int
          EnvironmentFilterMapsUniforms : int array
          LightMapOriginsUniform : int
          LightMapMinsUniform : int
          LightMapSizesUniform : int
          PhysicallyBasedDeferredEnvironmentFilterShader : uint }

    /// Describes an ssao pass of a deferred physically-based shader that's loaded into GPU.
    type PhysicallyBasedDeferredSsaoShader =
        { ViewUniform : int
          ProjectionUniform : int
          PositionTextureUniform : int
          NormalPlusTextureUniform : int
          SsaoResolution : int
          SsaoIntensity : int
          SsaoBias : int
          SsaoRadius : int
          SsaoDistanceMax : int
          SsaoSampleCount : int
          PhysicallyBasedDeferredSsaoShader : uint }

    /// Describes the lighting pass of a deferred physically-based shader that's loaded into GPU.
    type PhysicallyBasedDeferredLightingShader =
        { EyeCenterUniform : int
          ViewUniform : int
          ProjectionUniform : int
          LightCutoffMarginUniform : int
          LightAmbientColorUniform : int
          LightAmbientBrightnessUniform : int
          LightShadowSamplesUniform : int
          LightShadowBiasUniform : int
          LightShadowSampleScalarUniform : int
          LightShadowExponentUniform : int
          LightShadowDensityUniform : int
          SsvfEnabledUniform : int
          SsvfStepsUniform : int
          SsvfAsymmetryUniform : int
          SsvfIntensityUniform : int
          SsrEnabledUniform : int
          SsrDetailUniform : int
          SsrRefinementsMaxUniform : int
          SsrRayThicknessUniform : int
          SsrTowardEyeCutoffUniform : int
          SsrDepthCutoffUniform : int
          SsrDepthCutoffMarginUniform : int
          SsrDistanceCutoffUniform : int
          SsrDistanceCutoffMarginUniform : int
          SsrRoughnessCutoffUniform : int
          SsrRoughnessCutoffMarginUniform : int
          SsrSlopeCutoffUniform : int
          SsrSlopeCutoffMarginUniform : int
          SsrEdgeHorizontalMarginUniform : int
          SsrEdgeVerticalMarginUniform : int
          SsrLightColorUniform : int
          SsrLightBrightnessUniform : int
          PositionTextureUniform : int
          AlbedoTextureUniform : int
          MaterialTextureUniform : int
          NormalPlusTextureUniform : int
          BrdfTextureUniform : int
          AmbientTextureUniform : int
          IrradianceTextureUniform : int
          EnvironmentFilterTextureUniform : int
          SsaoTextureUniform : int
          ShadowTexturesUniforms : int array
          ShadowMapsUniforms : int array
          LightOriginsUniform : int
          LightDirectionsUniform : int
          LightColorsUniform : int
          LightBrightnessesUniform : int
          LightAttenuationLinearsUniform : int
          LightAttenuationQuadraticsUniform : int
          LightCutoffsUniform : int
          LightTypesUniform : int
          LightConeInnersUniform : int
          LightConeOutersUniform : int
          LightShadowIndicesUniform : int
          LightsCountUniform : int
          ShadowMatricesUniforms : int array
          PhysicallyBasedDeferredLightingShader : uint }

    /// Describes the composition pass of a deferred physically-based shader that's loaded into GPU.
    type PhysicallyBasedDeferredCompositionShader =
        { ColorTextureUniform : int
          FogAccumTextureUniform : int
          PhysicallyBasedDeferredCompositionShader : uint }

    /// Create the buffers required for physically-based rendering.
    let CreatePhysicallyBasedBuffers (geometryViewport : Viewport) =

        // create shadow texture buffers array
        let shadowTextureBuffersArray =
            [|for shadowTextureBufferIndex in 0 .. dec Constants.Render.ShadowTexturesMax do
                let shadowResolution = Viewport.getShadowTextureBufferResolution shadowTextureBufferIndex geometryViewport
                match OpenGL.Framebuffer.TryCreateShadowTextureBuffers (shadowResolution.X, shadowResolution.Y) with
                | Right shadowTextureBuffers -> shadowTextureBuffers
                | Left error -> failwith ("Could not create physically-based buffers due to: " + error + ".")|]

        // create second array of shadow texture buffers
        let shadowTextureBuffers2Array =
            [|for shadoTexturewBufferIndex in 0 .. dec Constants.Render.ShadowTexturesMax do
                let shadowResolution = Viewport.getShadowTextureBufferResolution shadoTexturewBufferIndex geometryViewport
                match OpenGL.Framebuffer.TryCreateShadowTextureBuffers (shadowResolution.X, shadowResolution.Y) with
                | Right shadowTextureBuffers -> shadowTextureBuffers
                | Left error -> failwith ("Could not create physically-based buffers due to: " + error + ".")|]

        // create shadow map buffers array
        let shadowMapBuffersArray =
            [|for _ in 0 .. dec Constants.Render.ShadowMapsMax do
                let shadowResolution = geometryViewport.ShadowResolution
                match OpenGL.Framebuffer.TryCreateShadowMapBuffers (shadowResolution.X, shadowResolution.Y) with
                | Right shadowMapBuffers -> shadowMapBuffers
                | Left error -> failwith ("Could not create physically-based buffers due to: " + error + ".")|]

        // create geometry buffers
        let geometryBuffers =
            match OpenGL.Framebuffer.TryCreateGeometryBuffers (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y) with
            | Right geometryBuffers -> geometryBuffers
            | Left error -> failwith ("Could not create physically-based buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create light mapping buffers
        let lightMappingBuffers =
            match OpenGL.Framebuffer.TryCreateLightMappingBuffers (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y) with
            | Right lightMappingBuffers -> lightMappingBuffers
            | Left error -> failwith ("Could not create physically-based buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create ambient buffers
        let ambientBuffers =
            match OpenGL.Framebuffer.TryCreateHdrBuffers (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y) with
            | Right ambientBuffers -> ambientBuffers
            | Left error -> failwith ("Could not create physically-based buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create irradiance buffers
        let irradianceBuffers =
            match OpenGL.Framebuffer.TryCreateIrradianceBuffers (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y) with
            | Right irradianceBuffers -> irradianceBuffers
            | Left error -> failwith ("Could not create physically-based buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create environment filter buffers
        let environmentFilterBuffers =
            match OpenGL.Framebuffer.TryCreateEnvironmentFilterBuffers (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y) with
            | Right environmentFilterBuffers -> environmentFilterBuffers
            | Left error -> failwith ("Could not create physically-based buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create unfiltered ssao buffers
        let ssaoBuffersUnfiltered =
            match OpenGL.Framebuffer.TryCreateSsaoBuffers (geometryViewport.SsaoResolution.X, geometryViewport.SsaoResolution.Y) with
            | Right ssaoBuffers -> ssaoBuffers
            | Left error -> failwith ("Could not create physically-based buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create filtered ssao buffers
        let ssaoBuffersFiltered =
            match OpenGL.Framebuffer.TryCreateSsaoBuffers (geometryViewport.SsaoResolution.X, geometryViewport.SsaoResolution.Y) with
            | Right ssaoBuffers -> ssaoBuffers
            | Left error -> failwith ("Could not create physically-based buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create lighting buffers
        let lightingBuffers =
            match OpenGL.Framebuffer.TryCreateLightingBuffers (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y) with
            | Right lightingBuffers -> lightingBuffers
            | Left error -> failwith ("Could not create physically-based buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create specular screen down-sample buffers
        let specularScreenDownSampleBuffers =
            match OpenGL.Framebuffer.TryCreateFilterBilateralDownSampleBuffers (geometryViewport.Bounds.Size.X / 2, geometryViewport.Bounds.Size.Y / 2) with
            | Right specularScreenDownSampleBuffers -> specularScreenDownSampleBuffers
            | Left error -> failwith ("Could not create physically-based buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create specular screen up-sample buffers
        let specularScreenUpSampleBuffers =
            match OpenGL.Framebuffer.TryCreateHdrBuffers (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y) with
            | Right specularScreenUpSampleBuffers -> specularScreenUpSampleBuffers
            | Left error -> failwith ("Could not create physically-based buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create fog accum down-sample buffers
        let fogAccumDownSampleBuffers =
            match OpenGL.Framebuffer.TryCreateFilterBilateralDownSampleBuffers (geometryViewport.Bounds.Size.X / 2, geometryViewport.Bounds.Size.Y / 2) with
            | Right fogAccumDownSampleBuffers -> fogAccumDownSampleBuffers
            | Left error -> failwith ("Could not create physically-based buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create fog accum up-sample buffers
        let fogAccumUpSampleBuffers =
            match OpenGL.Framebuffer.TryCreateHdrBuffers (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y) with
            | Right fogAccumUpSampleBuffers -> fogAccumUpSampleBuffers
            | Left error -> failwith ("Could not create physically-based buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create composition buffers
        let compositionBuffers =
            match OpenGL.Framebuffer.TryCreateHdrBuffers (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y) with
            | Right filterFogAccumBuffers -> filterFogAccumBuffers
            | Left error -> failwith ("Could not create physically-based buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // make record
        { ShadowTextureBuffersArray = shadowTextureBuffersArray
          ShadowTextureBuffers2Array = shadowTextureBuffers2Array
          ShadowMapBuffersArray = shadowMapBuffersArray
          GeometryBuffers = geometryBuffers
          LightMappingBuffers = lightMappingBuffers
          IrradianceBuffers = irradianceBuffers
          EnvironmentFilterBuffers = environmentFilterBuffers
          AmbientBuffers = ambientBuffers
          SsaoBuffersUnfiltered = ssaoBuffersUnfiltered
          SsaoBuffersFiltered = ssaoBuffersFiltered
          LightingBuffers = lightingBuffers
          SpecularScreenDownSampleBuffers = specularScreenDownSampleBuffers
          SpecularScreenUpSampleBuffers = specularScreenUpSampleBuffers
          FogAccumDownSampleBuffers = fogAccumDownSampleBuffers
          FogAccumUpSampleBuffers = fogAccumUpSampleBuffers
          CompositionBuffers = compositionBuffers }

    /// Destroy the physically-based buffers.
    let DestroyPhysicallyBasedBuffers buffers =
        OpenGL.Framebuffer.DestroyGeometryBuffers buffers.GeometryBuffers
        OpenGL.Framebuffer.DestroyLightMappingBuffers buffers.LightMappingBuffers
        OpenGL.Framebuffer.DestroyIrradianceBuffers buffers.IrradianceBuffers
        OpenGL.Framebuffer.DestroyEnvironmentFilterBuffers buffers.EnvironmentFilterBuffers
        OpenGL.Framebuffer.DestroyHdrBuffers buffers.AmbientBuffers
        OpenGL.Framebuffer.DestroySsaoBuffers buffers.SsaoBuffersUnfiltered
        OpenGL.Framebuffer.DestroySsaoBuffers buffers.SsaoBuffersFiltered
        OpenGL.Framebuffer.DestroyLightingBuffers buffers.LightingBuffers
        OpenGL.Framebuffer.DestroyFilterBilateralBuffers buffers.SpecularScreenDownSampleBuffers
        OpenGL.Framebuffer.DestroyHdrBuffers buffers.SpecularScreenUpSampleBuffers
        OpenGL.Framebuffer.DestroyFilterBilateralBuffers buffers.FogAccumDownSampleBuffers
        OpenGL.Framebuffer.DestroyHdrBuffers buffers.FogAccumUpSampleBuffers
        OpenGL.Framebuffer.DestroyHdrBuffers buffers.CompositionBuffers
        for shadowTextureBuffers in buffers.ShadowTextureBuffersArray do OpenGL.Framebuffer.DestroyShadowTextureBuffers shadowTextureBuffers
        for shadowTextureBuffers2 in buffers.ShadowTextureBuffers2Array do OpenGL.Framebuffer.DestroyShadowTextureBuffers shadowTextureBuffers2
        for shadowMapBuffers in buffers.ShadowMapBuffersArray do OpenGL.Framebuffer.DestroyShadowMapBuffers shadowMapBuffers

    /// Create physically-based material from an assimp mesh, falling back on defaults in case of missing textures.
    /// Uses file name-based inferences to look for texture files in case the ones that were hard-coded in the model
    /// files can't be located.
    /// Thread-safe if renderable = false.
    let CreatePhysicallyBasedMaterial (renderable, dirPath, defaultMaterial, textureClient : Texture.TextureClient, material : Assimp.Material) =

        // compute the directory string to prefix to a local asset file path
        let dirPrefix = if dirPath <> "" then dirPath + "/" else ""

        // attempt to load albedo info
        let albedo =
            if material.HasColorDiffuse
            then color material.ColorDiffuse.R material.ColorDiffuse.G material.ColorDiffuse.B material.ColorDiffuse.A
            else Constants.Render.AlbedoDefault
        let mutable (_, albedoTextureSlotA) = material.GetMaterialTexture (Assimp.TextureType.BaseColor, 0)
        let mutable (_, albedoTextureSlotB) = material.GetMaterialTexture (Assimp.TextureType.Diffuse, 0)
        let mutable albedoTextureSlotFilePath =
            if isNull albedoTextureSlotA.FilePath then
                if isNull albedoTextureSlotB.FilePath then ""
                else albedoTextureSlotB.FilePath
            else albedoTextureSlotA.FilePath
        if albedoTextureSlotFilePath <> "" then
            albedoTextureSlotFilePath <- PathF.Normalize albedoTextureSlotFilePath
            let individualPaths = albedoTextureSlotFilePath.Split "/"
            let possibleFilePaths =
                [|for i in dec individualPaths.Length .. -1 .. 0 do
                    let possibleFilePath = String.join "/" (Array.skip i individualPaths)
                    possibleFilePath
                    if PathF.GetExtensionLower possibleFilePath = ".psd" then PathF.ChangeExtension (possibleFilePath, ".png")
                    PathF.ChangeExtension (possibleFilePath, ".dds")|]
            let mutable found = false
            let mutable i = 0
            while not found && i < possibleFilePaths.Length do
                let possibleFilePath = possibleFilePaths.[i]
                if File.Exists (dirPrefix + possibleFilePath) then
                    albedoTextureSlotFilePath <- possibleFilePath
                    found <- true
                else i <- inc i
        let albedoTexture =
            if renderable then
                match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + albedoTextureSlotFilePath) with
                | Right texture -> texture
                | Left _ -> defaultMaterial.AlbedoTexture
            else defaultMaterial.AlbedoTexture

        // infer possible substitute texture names
        let albedoTextureDirName =              match albedoTextureSlotFilePath with null -> "" | filePath -> PathF.GetDirectoryName filePath
        let albedoTextureFileName =             PathF.GetFileName albedoTextureSlotFilePath
        let substitutionPrefix =                if albedoTextureDirName <> "" then albedoTextureDirName + "/" else ""
        let has_bc =                            albedoTextureFileName.Contains "_bc"
        let has_d =                             albedoTextureFileName.Contains "_d"
        let hasBaseColor =                      albedoTextureFileName.Contains "BaseColor"
        let hasDiffuse =                        albedoTextureFileName.Contains "Diffuse"
        let hasAlbedo =                         albedoTextureFileName.Contains "Albedo"
        let mTextureFilePath =                  if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_m")                       elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_m")                    else ""
        let g_mTextureFilePath =                if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_g_m")                     elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_g_m")                  else ""
        let g_m_aoTextureFilePath =             if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_g_m_ao")                  elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_g_m_ao")               else ""
        let gTextureFilePath =                  if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_g")                       elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_g")                    else ""
        let sTextureFilePath =                  if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_s")                       elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_s")                    else ""
        let aoTextureFilePath =                 if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_ao")                      elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_ao")                   else ""
        let eTextureFilePath =                  if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_e")                       elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_e")                    else ""
        let nTextureFilePath =                  if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_n")                       elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_n")                    else ""
        let hTextureFilePath =                  if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_h")                       elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_h")                    else ""
        let rmTextureFilePath =                 if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "RM")                 elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "RM")               elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "RM")                else ""
        let rmaTextureFilePath =                if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "RMA")                elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "RMA")              elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "RMA")               else ""
        let roughnessTextureFilePath =          if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Roughness")          elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Roughness")        elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Roughness")         else ""
        let metallicTextureFilePath =           if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Metallic")           elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Metallic")         elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Metallic")          else ""
        let metalnessTextureFilePath =          if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Metalness")          elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Metalness")        elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Metalness")         else ""
        let ambientOcclusionTextureFilePath =   if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "AmbientOcclusion")   elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "AmbientOcclusion") elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "AmbientOcclusion")  else ""
        let occlusionTextureFilePath =          if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Occlusion")          elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Occlusion")        elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Occlusion")         else ""
        let aoTextureFilePath' =                if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "AO")                 elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "AO")               elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "AO")                else ""
        let normalTextureFilePath =             if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Normal")             elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Normal")           elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Normal")            else ""
        let emissionTextureFilePath =           if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Emission")           elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Emission")         elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Emission")          else ""
        let heightTextureFilePath =             if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Height")             elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Height")           elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Height")            else ""

        // attempt to load roughness info
        let roughness = Constants.Render.RoughnessDefault
        let mutable (_, roughnessTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Roughness, 0)
        if isNull roughnessTextureSlot.FilePath then roughnessTextureSlot.FilePath <- "" // ensure not null
        roughnessTextureSlot.FilePath <- roughnessTextureSlot.FilePath // trim
        let roughnessTexture =
            if renderable then
                match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + roughnessTextureSlot.FilePath) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + gTextureFilePath) with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + sTextureFilePath) with
                        | Right texture -> texture
                        | Left _ ->
                            match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + g_mTextureFilePath) with
                            | Right texture -> texture
                            | Left _ ->
                                match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + g_m_aoTextureFilePath) with
                                | Right texture -> texture
                                | Left _ ->
                                    match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + roughnessTextureFilePath) with
                                    | Right texture -> texture
                                    | Left _ ->
                                        match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + rmTextureFilePath) with
                                        | Right texture -> texture
                                        | Left _ ->
                                            match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + rmaTextureFilePath) with
                                            | Right texture -> texture
                                            | Left _ -> defaultMaterial.RoughnessTexture
            else defaultMaterial.RoughnessTexture

        // attempt to load metallic info
        let metallic = Constants.Render.MetallicDefault
        let mutable (_, metallicTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Metalness, 0)
        if isNull metallicTextureSlot.FilePath
        then metallicTextureSlot.FilePath <- "" // ensure not null
        else metallicTextureSlot.FilePath <- PathF.Normalize metallicTextureSlot.FilePath
        let metallicTexture =
            if renderable then
                match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + metallicTextureSlot.FilePath) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + mTextureFilePath) with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + g_mTextureFilePath) with
                        | Right texture -> texture
                        | Left _ ->
                            match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + g_m_aoTextureFilePath) with
                            | Right texture -> texture
                            | Left _ ->
                                match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + metallicTextureFilePath) with
                                | Right texture -> texture
                                | Left _ ->
                                    match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + metalnessTextureFilePath) with
                                    | Right texture -> texture
                                    | Left _ ->
                                        match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + rmTextureFilePath) with
                                        | Right texture -> texture
                                        | Left _ ->
                                            match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + rmaTextureFilePath) with
                                            | Right texture -> texture
                                            | Left _ -> defaultMaterial.MetallicTexture
            else defaultMaterial.MetallicTexture

        // attempt to load ambient occlusion info
        let ambientOcclusion = Constants.Render.AmbientOcclusionDefault
        let mutable (_, ambientOcclusionTextureSlotA) = material.GetMaterialTexture (Assimp.TextureType.Ambient, 0)
        let mutable (_, ambientOcclusionTextureSlotB) = material.GetMaterialTexture (Assimp.TextureType.AmbientOcclusion, 0)
        let ambientOcclusionTextureSlotFilePath =
            if isNull ambientOcclusionTextureSlotA.FilePath then
                if isNull ambientOcclusionTextureSlotB.FilePath then ""
                else ambientOcclusionTextureSlotB.FilePath
            else ambientOcclusionTextureSlotA.FilePath
        let ambientOcclusionTexture =
            if renderable then
                match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + ambientOcclusionTextureSlotFilePath) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + aoTextureFilePath) with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + g_m_aoTextureFilePath) with
                        | Right texture -> texture
                        | Left _ ->
                            match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + ambientOcclusionTextureFilePath) with
                            | Right texture -> texture
                            | Left _ ->
                                match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + occlusionTextureFilePath) with
                                | Right texture -> texture
                                | Left _ ->
                                    match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + aoTextureFilePath') with
                                    | Right texture -> texture
                                    | Left _ ->
                                        match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + rmaTextureFilePath) with
                                        | Right texture -> texture
                                        | Left _ -> defaultMaterial.AmbientOcclusionTexture
            else defaultMaterial.AmbientOcclusionTexture

        // attempt to load emission info
        let emission = Constants.Render.EmissionDefault
        let mutable (_, emissionTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Emissive, 0)
        if isNull emissionTextureSlot.FilePath
        then emissionTextureSlot.FilePath <- "" // ensure not null
        else emissionTextureSlot.FilePath <- PathF.Normalize emissionTextureSlot.FilePath
        let emissionTexture =
            if renderable then
                match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + emissionTextureSlot.FilePath) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + eTextureFilePath) with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + emissionTextureFilePath) with
                        | Right texture -> texture
                        | Left _ -> defaultMaterial.EmissionTexture
            else defaultMaterial.EmissionTexture

        // attempt to load normal info
        let mutable (_, normalTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Normals, 0)
        if isNull normalTextureSlot.FilePath
        then normalTextureSlot.FilePath <- "" // ensure not null
        else normalTextureSlot.FilePath <- PathF.Normalize normalTextureSlot.FilePath
        let normalTexture =
            if renderable then
                match textureClient.TryCreateTextureFiltered (true, false, dirPrefix + normalTextureSlot.FilePath) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, false, dirPrefix + nTextureFilePath) with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered (true, false, dirPrefix + normalTextureFilePath) with
                        | Right texture -> texture
                        | Left _ -> defaultMaterial.NormalTexture
            else defaultMaterial.NormalTexture

        // attempt to load height info
        let height = Constants.Render.HeightDefault
        let mutable (_, heightTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Height, 0)
        if isNull heightTextureSlot.FilePath
        then heightTextureSlot.FilePath <- "" // ensure not null
        else heightTextureSlot.FilePath <- PathF.Normalize heightTextureSlot.FilePath
        let heightTexture =
            if renderable then
                match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + heightTextureSlot.FilePath) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + hTextureFilePath) with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + heightTextureFilePath) with
                        | Right texture -> texture
                        | Left _ -> defaultMaterial.HeightTexture
            else defaultMaterial.HeightTexture

        // compute ignore light maps
        let ignoreLightMaps =
            match material.IgnoreLightMapsOpt with
            | ValueSome ignoreLightMaps -> ignoreLightMaps
            | ValueNone -> Constants.Render.IgnoreLightMapsDefault

        // compute opaque distance
        let opaqueDistance =
            match material.OpaqueDistanceOpt with
            | ValueSome opqaqueDistance -> opqaqueDistance
            | ValueNone -> Constants.Render.OpaqueDistanceDefault

        // compute two-sidedness
        let twoSided =
            match material.TwoSidedOpt with
            | ValueSome twoSided -> twoSided
            | ValueNone -> material.IsTwoSided

        // make properties
        let properties =
            { Albedo = color albedo.R albedo.G albedo.B albedo.A
              Roughness = roughness
              Metallic = metallic
              AmbientOcclusion = ambientOcclusion
              Emission = emission
              Height = height
              IgnoreLightMaps = ignoreLightMaps
              OpaqueDistance = opaqueDistance }

        // make material
        let material =
            { AlbedoTexture = albedoTexture
              RoughnessTexture = roughnessTexture
              MetallicTexture = metallicTexture
              AmbientOcclusionTexture = ambientOcclusionTexture
              EmissionTexture = emissionTexture
              NormalTexture = normalTexture
              HeightTexture = heightTexture
              TwoSided = twoSided }

        // fin
        (properties, material)

    /// Create physically-based static mesh from an assimp mesh.
    let CreatePhysicallyBasedStaticMesh (indexData, mesh : Assimp.Mesh) =

        // populate vertex data and bounds
        let vertexData = Array.zeroCreate<single> (mesh.Vertices.Count * 8)
        let mutable positionMin = v3Zero
        let mutable positionMax = v3Zero
        for i in 0 .. dec mesh.Vertices.Count do
            let v = i * 8
            let position = if i < mesh.VertexCount then mesh.Vertices.[i] else Assimp.Vector3D (0.0f, 0.0f, 0.0f)
            let texCoords = if i < mesh.TextureCoordinateChannels.[0].Capacity then mesh.TextureCoordinateChannels.[0].[i] else Assimp.Vector3D (0.0f, 0.0f, 0.0f)
            let normal = if i < mesh.Normals.Count then mesh.Normals.[i] else Assimp.Vector3D (0.5f, 0.5f, 1.0f)
            vertexData.[v] <- position.X
            vertexData.[v+1] <- position.Y
            vertexData.[v+2] <- position.Z
            vertexData.[v+3] <- texCoords.X
            vertexData.[v+4] <- 1.0f - texCoords.Y
            vertexData.[v+5] <- normal.X
            vertexData.[v+6] <- normal.Y
            vertexData.[v+7] <- normal.Z
            positionMin.X <- min positionMin.X position.X
            positionMin.Y <- min positionMin.Y position.Y
            positionMin.Z <- min positionMin.Z position.Z
            positionMax.X <- max positionMax.X position.X
            positionMax.Y <- max positionMax.Y position.Y
            positionMax.Z <- max positionMax.Z position.Z
        let bounds = box3 positionMin (positionMax - positionMin)

        // fin
        (vertexData, indexData, bounds)

    /// Create physically-based animated mesh from an assimp mesh.
    let CreatePhysicallyBasedAnimatedMesh (indexData, mesh : Assimp.Mesh) =

        // populate vertex data (except bone) and bounds
        let vertexData = Array.zeroCreate<single> (mesh.Vertices.Count * 16)
        let mutable positionMin = v3Zero
        let mutable positionMax = v3Zero
        for i in 0 .. dec mesh.Vertices.Count do
            let v = i * 16
            let position = if i < mesh.VertexCount then mesh.Vertices.[i] else Assimp.Vector3D (0.0f, 0.0f, 0.0f)
            let texCoords = if i < mesh.TextureCoordinateChannels.[0].Capacity then mesh.TextureCoordinateChannels.[0].[i] else Assimp.Vector3D (0.0f, 0.0f, 0.0f)
            let normal = if i < mesh.Normals.Count then mesh.Normals.[i] else Assimp.Vector3D (0.5f, 0.5f, 1.0f)
            vertexData.[v] <- position.X
            vertexData.[v+1] <- position.Y
            vertexData.[v+2] <- position.Z
            vertexData.[v+3] <- texCoords.X
            vertexData.[v+4] <- 1.0f - texCoords.Y
            vertexData.[v+5] <- normal.X
            vertexData.[v+6] <- normal.Y
            vertexData.[v+7] <- normal.Z
            vertexData.[v+8] <- -1.0f
            vertexData.[v+9] <- -1.0f
            vertexData.[v+10] <- -1.0f
            vertexData.[v+11] <- -1.0f
            vertexData.[v+12] <- 0.0f
            vertexData.[v+13] <- 0.0f
            vertexData.[v+14] <- 0.0f
            vertexData.[v+15] <- 0.0f
            positionMin.X <- min positionMin.X position.X
            positionMin.Y <- min positionMin.Y position.Y
            positionMin.Z <- min positionMin.Z position.Z
            positionMax.X <- max positionMax.X position.X
            positionMax.Y <- max positionMax.Y position.Y
            positionMax.Z <- max positionMax.Z position.Z
        let bounds = box3 positionMin (positionMax - positionMin)

        // populate vertex bone data
        for boneIndex in 0 .. dec mesh.Bones.Count do
            let weights = mesh.Bones.[boneIndex].VertexWeights
            let weightsCount = mesh.Bones.[boneIndex].VertexWeights.Count
            for weightIndex in 0 .. dec weightsCount do
                let vertexId = weights.[weightIndex].VertexID
                let vertexOffset = vertexId * 16
                let weight = weights.[weightIndex].Weight
                if weight > 0.0f then

                    // find a free slot to specify the current index and weight (free slots are designated as -1.0f index above)
                    let mutable found = false
                    let mutable i = 0
                    while not found && i < Constants.Render.BonesInfluenceMax do
                        if vertexData.[vertexOffset+8+i] = single boneIndex then // already found
                            found <- true
                        elif vertexData.[vertexOffset+8+i] < 0.0f then // found free slot
                            vertexData.[vertexOffset+8+i] <- single boneIndex
                            vertexData.[vertexOffset+12+i] <- weight
                            found <- true
                        else i <- inc i

                    // when all slots are allocated, replace the index and weight of the lowest-weight entry iff the current weight is higher
                    if not found then
                        let mutable lowestOpt = ValueNone
                        for i in 0 .. dec Constants.Render.BonesInfluenceMax do
                            match lowestOpt with
                            | ValueSome lowest ->
                                if vertexData.[vertexOffset+12+i] < vertexData.[vertexOffset+12+lowest] then
                                    lowestOpt <- ValueSome i
                            | ValueNone -> lowestOpt <- ValueSome i
                        match lowestOpt with
                        | ValueSome lowest ->
                            if vertexData.[vertexOffset+12+lowest] < weight then
                                vertexData.[vertexOffset+8+lowest] <- single boneIndex
                                vertexData.[vertexOffset+12+lowest] <- weight
                        | ValueNone -> failwithumf ()
                                

        // fin
        (vertexData, indexData, bounds)

    /// Create a mesh for a physically-based quad.
    let CreatePhysicallyBasedQuadMesh () =

        // make vertex data
        let vertexData =
            [|
                (*   positions   *)         (* tex coords *)    (*   normals   *)
                -1.0f; -1.0f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // bottom-left
                +1.0f; -1.0f; +0.0f;        1.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // bottom-right
                +1.0f; +1.0f; +0.0f;        1.0f; 1.0f;         0.0f;  0.0f; 1.0f;  // top-right
                +1.0f; +1.0f; +0.0f;        1.0f; 1.0f;         0.0f;  0.0f; 1.0f;  // top-right
                -1.0f; +1.0f; +0.0f;        0.0f; 1.0f;         0.0f;  0.0f; 1.0f;  // top-left
                -1.0f; -1.0f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // bottom-left
            |]

        // make index data trivially
        let indexData = Array.init 6 id

        // make bounds trivially
        let bounds = box3 (v3 -1.0f -1.0f 0.0f) (v3 2.0f 2.0f 0.0f)

        // fin
        (vertexData, indexData, bounds)

    /// Create a mesh for a physically-based particle.
    let CreatePhysicallyBasedParticleMesh () =

        // make vertex data
        let vertexData =
            [|
                (*   positions   *)         (* tex coords *)    (*   normals   *)
                -0.5f; -0.5f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // bottom-left
                +0.5f; -0.5f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // bottom-right
                +0.5f; +0.5f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // top-right
                +0.5f; +0.5f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // top-right
                -0.5f; +0.5f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // top-left
                -0.5f; -0.5f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // bottom-left
            |]

        // make index data trivially
        let indexData = Array.init 6 id

        // make bounds trivially
        let bounds = box3 (v3 -0.5f -0.5f 0.0f) (v3 1.0f 1.0f 0.0f)

        // fin
        (vertexData, indexData, bounds)

    /// Create a mesh for a physically-based billboard.
    let CreatePhysicallyBasedBillboardMesh () =

        // make vertex data
        let vertexData =
            [|
                (*   positions   *)         (* tex coords *)    (*   normals   *)
                -0.5f; -0.5f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // bottom-left
                +0.5f; -0.5f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // bottom-right
                +0.5f; +0.5f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // top-right
                -0.5f; -0.5f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // top-right
                +0.5f; +0.5f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // top-left
                -0.5f; +0.5f; +0.0f;        0.0f; 0.0f;         0.0f;  0.0f; 1.0f;  // bottom-left
            |]

        // make index data trivially
        let indexData = Array.init 6 id

        // make bounds trivially
        let bounds = box3 (v3 -0.5f -0.5f 0.0f) (v3 1.0f 1.0f 0.0f)

        // fin
        (vertexData, indexData, bounds)

    /// Create a mesh for a physically-based cube.
    let CreatePhysicallyBasedCubeMesh () =

        // make vertex data
        let vertexData =
            [|
                (*   positions   *)         (* tex coords *)    (*    normals    *)

                // back face
                -0.5f; -0.5f; -0.5f;        0.0f; 0.0f;          0.0f;  0.0f; -1.0f; // bottom-left
                +0.5f; +0.5f; -0.5f;        1.0f; 1.0f;          0.0f;  0.0f; -1.0f; // top-right
                +0.5f; -0.5f; -0.5f;        1.0f; 0.0f;          0.0f;  0.0f; -1.0f; // bottom-right         
                +0.5f; +0.5f; -0.5f;        1.0f; 1.0f;          0.0f;  0.0f; -1.0f; // top-right
                -0.5f; -0.5f; -0.5f;        0.0f; 0.0f;          0.0f;  0.0f; -1.0f; // bottom-left
                -0.5f; +0.5f; -0.5f;        0.0f; 1.0f;          0.0f;  0.0f; -1.0f; // top-left

                // front face
                -0.5f; -0.5f; +0.5f;        0.0f; 0.0f;          0.0f;  0.0f; +1.0f; // bottom-left
                +0.5f; -0.5f; +0.5f;        1.0f; 0.0f;          0.0f;  0.0f; +1.0f; // bottom-right
                +0.5f; +0.5f; +0.5f;        1.0f; 1.0f;          0.0f;  0.0f; +1.0f; // top-right
                +0.5f; +0.5f; +0.5f;        1.0f; 1.0f;          0.0f;  0.0f; +1.0f; // top-right
                -0.5f; +0.5f; +0.5f;        0.0f; 1.0f;          0.0f;  0.0f; +1.0f; // top-left
                -0.5f; -0.5f; +0.5f;        0.0f; 0.0f;          0.0f;  0.0f; +1.0f; // bottom-left

                // left face
                -0.5f; +0.5f; +0.5f;        1.0f; 0.0f;         -1.0f;  0.0f;  0.0f; // top-right
                -0.5f; +0.5f; -0.5f;        1.0f; 1.0f;         -1.0f;  0.0f;  0.0f; // top-left
                -0.5f; -0.5f; -0.5f;        0.0f; 1.0f;         -1.0f;  0.0f;  0.0f; // bottom-left
                -0.5f; -0.5f; -0.5f;        0.0f; 1.0f;         -1.0f;  0.0f;  0.0f; // bottom-left
                -0.5f; -0.5f; +0.5f;        0.0f; 0.0f;         -1.0f;  0.0f;  0.0f; // bottom-right
                -0.5f; +0.5f; +0.5f;        1.0f; 0.0f;         -1.0f;  0.0f;  0.0f; // top-right

                // right face
                +0.5f; +0.5f; +0.5f;        1.0f; 0.0f;         +1.0f;  0.0f;  0.0f; // top-left
                +0.5f; -0.5f; -0.5f;        0.0f; 1.0f;         +1.0f;  0.0f;  0.0f; // bottom-right
                +0.5f; +0.5f; -0.5f;        1.0f; 1.0f;         +1.0f;  0.0f;  0.0f; // top-right         
                +0.5f; -0.5f; -0.5f;        0.0f; 1.0f;         +1.0f;  0.0f;  0.0f; // bottom-right
                +0.5f; +0.5f; +0.5f;        1.0f; 0.0f;         +1.0f;  0.0f;  0.0f; // top-left
                +0.5f; -0.5f; +0.5f;        0.0f; 0.0f;         +1.0f;  0.0f;  0.0f; // bottom-left     

                // bottom face
                -0.5f; -0.5f; -0.5f;        0.0f; 1.0f;          0.0f; -1.0f;  0.0f; // top-right
                +0.5f; -0.5f; -0.5f;        1.0f; 1.0f;          0.0f; -1.0f;  0.0f; // top-left
                +0.5f; -0.5f; +0.5f;        1.0f; 0.0f;          0.0f; -1.0f;  0.0f; // bottom-left
                +0.5f; -0.5f; +0.5f;        1.0f; 0.0f;          0.0f; -1.0f;  0.0f; // bottom-left
                -0.5f; -0.5f; +0.5f;        0.0f; 0.0f;          0.0f; -1.0f;  0.0f; // bottom-right
                -0.5f; -0.5f; -0.5f;        0.0f; 1.0f;          0.0f; -1.0f;  0.0f; // top-right

                // top face
                -0.5f; +0.5f; -0.5f;        0.0f; 1.0f;          0.0f; +1.0f;  0.0f; // top-left
                +0.5f; +0.5f ;+0.5f;        1.0f; 0.0f;          0.0f; +1.0f;  0.0f; // bottom-right
                +0.5f; +0.5f; -0.5f;        1.0f; 1.0f;          0.0f; +1.0f;  0.0f; // top-right     
                +0.5f; +0.5f; +0.5f;        1.0f; 0.0f;          0.0f; +1.0f;  0.0f; // bottom-right
                -0.5f; +0.5f; -0.5f;        0.0f; 1.0f;          0.0f; +1.0f;  0.0f; // top-left
                -0.5f; +0.5f; +0.5f;        0.0f; 0.0f;          0.0f; +1.0f;  0.0f  // bottom-left     
            |]

        // make index data trivially
        let indexData = Array.init 36 id

        // make bounds trivially
        let bounds = box3 (v3Dup -0.5f) v3One

        // fin
        (vertexData, indexData, bounds)

    /// Create physically-based static geometry from a mesh.
    let CreatePhysicallyBasedStaticGeometry (renderable, primitiveType, vertexData : single Memory, indexData : int Memory, bounds) =

        // make buffers
        let (vertices, indices, vertexBuffer, instanceBuffer, indexBuffer, vao) =

            // make renderable
            if renderable then

                // initialize vao
                let vao = Gl.GenVertexArray ()
                Gl.BindVertexArray vao
                Hl.Assert ()

                // create vertex buffer
                let vertexBuffer = Gl.GenBuffer ()
                let texCoordsOffset =   (3 (*position*)) * sizeof<single>
                let normalOffset =      (3 (*position*) + 2 (*tex coords*)) * sizeof<single>
                let vertexSize =        (3 (*position*) + 2 (*tex coords*) + 3 (*normal*)) * sizeof<single>
                Gl.BindBuffer (BufferTarget.ArrayBuffer, vertexBuffer)
                use vertexDataHnd = vertexData.Pin () in
                    let vertexDataNint = vertexDataHnd.Pointer |> NativePtr.ofVoidPtr<single> |> NativePtr.toNativeInt
                    Gl.BufferData (BufferTarget.ArrayBuffer, uint (vertexData.Length * sizeof<single>), vertexDataNint, BufferUsage.StaticDraw)
                Gl.EnableVertexAttribArray 0u
                Gl.VertexAttribPointer (0u, 3, VertexAttribPointerType.Float, false, vertexSize, nativeint 0)
                Gl.EnableVertexAttribArray 1u
                Gl.VertexAttribPointer (1u, 2, VertexAttribPointerType.Float, false, vertexSize, nativeint texCoordsOffset)
                Gl.EnableVertexAttribArray 2u
                Gl.VertexAttribPointer (2u, 3, VertexAttribPointerType.Float, false, vertexSize, nativeint normalOffset)
                Hl.Assert ()

                // create instance buffer
                let instanceBuffer = Gl.GenBuffer ()
                Gl.BindBuffer (BufferTarget.ArrayBuffer, instanceBuffer)
                let instanceData = Array.zeroCreate Constants.Render.InstanceFieldCount
                m4Identity.ToArray (instanceData, 0)
                let strideSize = instanceData.Length * sizeof<single>
                let instanceDataPtr = GCHandle.Alloc (instanceData, GCHandleType.Pinned)
                try Gl.BufferData (BufferTarget.ArrayBuffer, uint strideSize, instanceDataPtr.AddrOfPinnedObject (), BufferUsage.StreamDraw)
                finally instanceDataPtr.Free ()
                Gl.EnableVertexAttribArray 3u
                Gl.VertexAttribPointer (3u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint 0)
                Gl.VertexAttribDivisor (3u, 1u)
                Gl.EnableVertexAttribArray 4u
                Gl.VertexAttribPointer (4u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (4 * sizeof<single>))
                Gl.VertexAttribDivisor (4u, 1u)
                Gl.EnableVertexAttribArray 5u
                Gl.VertexAttribPointer (5u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (8 * sizeof<single>))
                Gl.VertexAttribDivisor (5u, 1u)
                Gl.EnableVertexAttribArray 6u
                Gl.VertexAttribPointer (6u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (12 * sizeof<single>))
                Gl.VertexAttribDivisor (6u, 1u)
                Gl.EnableVertexAttribArray 7u
                Gl.VertexAttribPointer (7u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (16 * sizeof<single>))
                Gl.VertexAttribDivisor (7u, 1u)
                Gl.EnableVertexAttribArray 8u
                Gl.VertexAttribPointer (8u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (20 * sizeof<single>))
                Gl.VertexAttribDivisor (8u, 1u)
                Gl.EnableVertexAttribArray 9u
                Gl.VertexAttribPointer (9u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (24 * sizeof<single>))
                Gl.VertexAttribDivisor (9u, 1u)
                Gl.EnableVertexAttribArray 10u
                Gl.VertexAttribPointer (10u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (28 * sizeof<single>))
                Gl.VertexAttribDivisor (10u, 1u)
                Hl.Assert ()

                // create index buffer
                let indexBuffer = Gl.GenBuffer ()
                Gl.BindBuffer (BufferTarget.ElementArrayBuffer, indexBuffer)
                let indexDataSize = uint (indexData.Length * sizeof<uint>)
                use indexDataHnd = indexData.Pin () in
                    let indexDataNint = indexDataHnd.Pointer |> NativePtr.ofVoidPtr<uint> |> NativePtr.toNativeInt
                    Gl.BufferData (BufferTarget.ElementArrayBuffer, indexDataSize, indexDataNint, BufferUsage.StaticDraw)
                Hl.Assert ()

                // finalize vao
                Gl.BindVertexArray 0u
                Hl.Assert ()

                // fin
                ([||], [||], vertexBuffer, instanceBuffer, indexBuffer, vao)

            // fake buffers
            else

                // compute vertices
                let vertices = Array.zeroCreate (vertexData.Length / 8)
                let vertexData = vertexData.Span
                for i in 0 .. dec vertices.Length do
                    let j = i * 8
                    let vertex = v3 vertexData.[j] vertexData.[j+1] vertexData.[j+2]
                    vertices.[i] <- vertex

                // create indices
                let indices = indexData.ToArray ()

                // fin
                (vertices, indices, 0u, 0u, 0u, 0u)

        // make physically-based geometry
        let geometry =
            { Bounds = bounds
              PrimitiveType = primitiveType
              ElementCount = indexData.Length
              Vertices = vertices
              Indices = indices
              VertexBuffer = vertexBuffer
              InstanceBuffer = instanceBuffer
              IndexBuffer = indexBuffer
              PhysicallyBasedVao = vao }

        // fin
        geometry

    /// Create physically-based static geometry from an assimp mesh.
    let CreatePhysicallyBasedStaticGeometryFromMesh (renderable, indexData, mesh : Assimp.Mesh) =
        match CreatePhysicallyBasedStaticMesh (indexData, mesh) with
        | (vertexData, indexData, bounds) -> CreatePhysicallyBasedStaticGeometry (renderable, PrimitiveType.Triangles, vertexData.AsMemory (), indexData.AsMemory (), bounds)

    /// Create physically-based animated geometry from a mesh.
    let CreatePhysicallyBasedAnimatedGeometry (renderable, primitiveType, vertexData : single Memory, indexData : int Memory, bounds) =

        // make buffers
        let (vertices, indices, vertexBuffer, instanceBuffer, indexBuffer, vao) =

            // make renderable
            if renderable then

                // initialize vao
                let vao = Gl.GenVertexArray ()
                Gl.BindVertexArray vao
                Hl.Assert ()

                // create vertex buffer
                let vertexBuffer = Gl.GenBuffer ()
                let texCoordsOffset =   (3 (*position*)) * sizeof<single>
                let normalOffset =      (3 (*position*) + 2 (*tex coords*)) * sizeof<single>
                let boneIdsOffset =     (3 (*position*) + 2 (*tex coords*) + 3 (*normal*)) * sizeof<single>
                let weightsOffset =     (3 (*position*) + 2 (*tex coords*) + 3 (*normal*) + 4 (*boneIds*)) * sizeof<single>
                let vertexSize =        (3 (*position*) + 2 (*tex coords*) + 3 (*normal*) + 4 (*boneIds*) + 4 (*weights*)) * sizeof<single>
                Gl.BindBuffer (BufferTarget.ArrayBuffer, vertexBuffer)
                use vertexDataHnd = vertexData.Pin () in
                    let vertexDataNint = vertexDataHnd.Pointer |> NativePtr.ofVoidPtr<single> |> NativePtr.toNativeInt
                    Gl.BufferData (BufferTarget.ArrayBuffer, uint (vertexData.Length * sizeof<single>), vertexDataNint, BufferUsage.StaticDraw)
                Gl.EnableVertexAttribArray 0u
                Gl.VertexAttribPointer (0u, 3, VertexAttribPointerType.Float, false, vertexSize, nativeint 0)
                Gl.EnableVertexAttribArray 1u
                Gl.VertexAttribPointer (1u, 2, VertexAttribPointerType.Float, false, vertexSize, nativeint texCoordsOffset)
                Gl.EnableVertexAttribArray 2u
                Gl.VertexAttribPointer (2u, 3, VertexAttribPointerType.Float, false, vertexSize, nativeint normalOffset)
                Gl.EnableVertexAttribArray 3u
                Gl.VertexAttribPointer (3u, 4, VertexAttribPointerType.Float, false, vertexSize, nativeint boneIdsOffset)
                Gl.EnableVertexAttribArray 4u
                Gl.VertexAttribPointer (4u, 4, VertexAttribPointerType.Float, false, vertexSize, nativeint weightsOffset)
                Hl.Assert ()

                // create instance buffer
                let instanceBuffer = Gl.GenBuffer ()
                Gl.BindBuffer (BufferTarget.ArrayBuffer, instanceBuffer)
                let instanceData = Array.zeroCreate Constants.Render.InstanceFieldCount
                m4Identity.ToArray (instanceData, 0)
                let strideSize = instanceData.Length * sizeof<single>
                let instanceDataPtr = GCHandle.Alloc (instanceData, GCHandleType.Pinned)
                try Gl.BufferData (BufferTarget.ArrayBuffer, uint strideSize, instanceDataPtr.AddrOfPinnedObject (), BufferUsage.StreamDraw)
                finally instanceDataPtr.Free ()
                Gl.EnableVertexAttribArray 5u
                Gl.VertexAttribPointer (5u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint 0)
                Gl.VertexAttribDivisor (5u, 1u)
                Gl.EnableVertexAttribArray 6u
                Gl.VertexAttribPointer (6u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (4 * sizeof<single>))
                Gl.VertexAttribDivisor (6u, 1u)
                Gl.EnableVertexAttribArray 7u
                Gl.VertexAttribPointer (7u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (8 * sizeof<single>))
                Gl.VertexAttribDivisor (7u, 1u)
                Gl.EnableVertexAttribArray 8u
                Gl.VertexAttribPointer (8u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (12 * sizeof<single>))
                Gl.VertexAttribDivisor (8u, 1u)
                Gl.EnableVertexAttribArray 9u
                Gl.VertexAttribPointer (9u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (16 * sizeof<single>))
                Gl.VertexAttribDivisor (9u, 1u)
                Gl.EnableVertexAttribArray 10u
                Gl.VertexAttribPointer (10u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (20 * sizeof<single>))
                Gl.VertexAttribDivisor (10u, 1u)
                Gl.EnableVertexAttribArray 11u
                Gl.VertexAttribPointer (11u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (24 * sizeof<single>))
                Gl.VertexAttribDivisor (11u, 1u)
                Gl.EnableVertexAttribArray 12u
                Gl.VertexAttribPointer (12u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (28 * sizeof<single>))
                Gl.VertexAttribDivisor (12u, 1u)
                Hl.Assert ()

                // create index buffer
                let indexBuffer = Gl.GenBuffer ()
                Gl.BindBuffer (BufferTarget.ElementArrayBuffer, indexBuffer)
                let indexDataSize = uint (indexData.Length * sizeof<uint>)
                use indexDataHnd = indexData.Pin () in
                    let indexDataNint = indexDataHnd.Pointer |> NativePtr.ofVoidPtr<uint> |> NativePtr.toNativeInt
                    Gl.BufferData (BufferTarget.ElementArrayBuffer, indexDataSize, indexDataNint, BufferUsage.StaticDraw)
                Hl.Assert ()

                // finalize vao
                Gl.BindVertexArray 0u
                Hl.Assert ()

                // fin
                ([||], [||], vertexBuffer, instanceBuffer, indexBuffer, vao)

            // fake buffers
            else

                // compute vertices
                let vertices = Array.zeroCreate (vertexData.Length / 16)
                let vertexData = vertexData.Span
                for i in 0 .. dec vertices.Length do
                    let j = i * 16
                    let vertex = v3 vertexData.[j] vertexData.[j+1] vertexData.[j+2]
                    vertices.[i] <- vertex

                // create indices
                let indices = indexData.ToArray ()

                // fin
                (vertices, indices, 0u, 0u, 0u, 0u)

        // make physically-based geometry
        let geometry =
            { Bounds = bounds
              PrimitiveType = primitiveType
              ElementCount = indexData.Length
              Vertices = vertices
              Indices = indices
              VertexBuffer = vertexBuffer
              InstanceBuffer = instanceBuffer
              IndexBuffer = indexBuffer
              PhysicallyBasedVao = vao }

        // fin
        geometry

    /// Create physically-based animated geometry from an assimp mesh.
    let CreatePhysicallyBasedAnimatedGeometryFromMesh (renderable, indexData, mesh : Assimp.Mesh) =
        match CreatePhysicallyBasedAnimatedMesh (indexData, mesh) with
        | (vertexData, indexData, bounds) -> CreatePhysicallyBasedAnimatedGeometry (renderable, PrimitiveType.Triangles, vertexData.AsMemory (), indexData.AsMemory (), bounds)

    /// Create physically-based terrain geometry from a mesh.
    let CreatePhysicallyBasedTerrainGeometry (renderable, primitiveType, vertexData : single Memory, indexData : int Memory, bounds) =

        // make buffers
        let (vertices, indices, vertexBuffer, instanceBuffer, indexBuffer, vao) =

            // make renderable
            if renderable then

                // initialize vao
                let vao = Gl.GenVertexArray ()
                Gl.BindVertexArray vao
                Hl.Assert ()

                // create vertex buffer
                let vertexBuffer = Gl.GenBuffer ()
                let texCoordsOffset =   (3 (*position*)) * sizeof<single>
                let normalOffset =      (3 (*position*) + 2 (*tex coords*)) * sizeof<single>
                let tintOffset =        (3 (*position*) + 2 (*tex coords*) + 3 (*normal*)) * sizeof<single>
                let blendsOffset =      (3 (*position*) + 2 (*tex coords*) + 3 (*normal*) + 3 (*tint*)) * sizeof<single>
                let blends2Offset =     (3 (*position*) + 2 (*tex coords*) + 3 (*normal*) + 3 (*tint*) + 4 (*blends*)) * sizeof<single>
                let vertexSize =        (3 (*position*) + 2 (*tex coords*) + 3 (*normal*) + 3 (*tint*) + 4 (*blends*) + 4 (*blends2*)) * sizeof<single>
                Gl.BindBuffer (BufferTarget.ArrayBuffer, vertexBuffer)
                use vertexDataHnd = vertexData.Pin () in
                    let vertexDataNint = vertexDataHnd.Pointer |> NativePtr.ofVoidPtr<single> |> NativePtr.toNativeInt
                    Gl.BufferData (BufferTarget.ArrayBuffer, uint (vertexData.Length * sizeof<single>), vertexDataNint, BufferUsage.StaticDraw)
                Gl.EnableVertexAttribArray 0u
                Gl.VertexAttribPointer (0u, 3, VertexAttribPointerType.Float, false, vertexSize, nativeint 0)
                Gl.EnableVertexAttribArray 1u
                Gl.VertexAttribPointer (1u, 2, VertexAttribPointerType.Float, false, vertexSize, nativeint texCoordsOffset)
                Gl.EnableVertexAttribArray 2u
                Gl.VertexAttribPointer (2u, 3, VertexAttribPointerType.Float, false, vertexSize, nativeint normalOffset)
                Gl.EnableVertexAttribArray 3u
                Gl.VertexAttribPointer (3u, 3, VertexAttribPointerType.Float, false, vertexSize, nativeint tintOffset)
                Gl.EnableVertexAttribArray 4u
                Gl.VertexAttribPointer (4u, 4, VertexAttribPointerType.Float, false, vertexSize, nativeint blendsOffset)
                Gl.EnableVertexAttribArray 5u
                Gl.VertexAttribPointer (5u, 4, VertexAttribPointerType.Float, false, vertexSize, nativeint blends2Offset)
                Hl.Assert ()

                // create instance buffer
                let instanceBuffer = Gl.GenBuffer ()
                Gl.BindBuffer (BufferTarget.ArrayBuffer, instanceBuffer)
                let instanceData = Array.zeroCreate Constants.Render.InstanceFieldCount
                m4Identity.ToArray (instanceData, 0)
                let strideSize = instanceData.Length * sizeof<single>
                let instanceDataPtr = GCHandle.Alloc (instanceData, GCHandleType.Pinned)
                try Gl.BufferData (BufferTarget.ArrayBuffer, uint strideSize, instanceDataPtr.AddrOfPinnedObject (), BufferUsage.StreamDraw)
                finally instanceDataPtr.Free ()
                Gl.EnableVertexAttribArray 6u
                Gl.VertexAttribPointer (6u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint 0) // model fields
                Gl.VertexAttribDivisor (6u, 1u)
                Gl.EnableVertexAttribArray 7u
                Gl.VertexAttribPointer (7u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (4 * sizeof<single>))
                Gl.VertexAttribDivisor (7u, 1u)
                Gl.EnableVertexAttribArray 8u
                Gl.VertexAttribPointer (8u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (8 * sizeof<single>))
                Gl.VertexAttribDivisor (8u, 1u)
                Gl.EnableVertexAttribArray 9u
                Gl.VertexAttribPointer (9u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (12 * sizeof<single>))
                Gl.VertexAttribDivisor (9u, 1u)
                Gl.EnableVertexAttribArray 10u
                Gl.VertexAttribPointer (10u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (16 * sizeof<single>))
                Gl.VertexAttribDivisor (10u, 1u)
                Gl.EnableVertexAttribArray 11u
                Gl.VertexAttribPointer (11u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (20 * sizeof<single>))
                Gl.VertexAttribDivisor (11u, 1u)
                Gl.EnableVertexAttribArray 12u
                Gl.VertexAttribPointer (12u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (24 * sizeof<single>))
                Gl.VertexAttribDivisor (12u, 1u)
                Gl.EnableVertexAttribArray 13u
                Gl.VertexAttribPointer (13u, 4, VertexAttribPointerType.Float, false, strideSize, nativeint (28 * sizeof<single>))
                Gl.VertexAttribDivisor (13u, 1u)
                Hl.Assert ()

                // create index buffer
                let indexBuffer = Gl.GenBuffer ()
                Gl.BindBuffer (BufferTarget.ElementArrayBuffer, indexBuffer)
                let indexDataSize = uint (indexData.Length * sizeof<uint>)
                use indexDataHnd = indexData.Pin () in
                    let indexDataNint = indexDataHnd.Pointer |> NativePtr.ofVoidPtr<uint> |> NativePtr.toNativeInt
                    Gl.BufferData (BufferTarget.ElementArrayBuffer, indexDataSize, indexDataNint, BufferUsage.StaticDraw)
                Hl.Assert ()

                // finalize vao
                Gl.BindVertexArray 0u
                Hl.Assert ()

                // fin
                ([||], [||], vertexBuffer, instanceBuffer, indexBuffer, vao)

            // fake buffers
            else

                // compute vertices
                let vertices = Array.zeroCreate (vertexData.Length / 19)
                let vertexData = vertexData.Span
                for i in 0 .. dec vertices.Length do
                    let j = i * 19
                    let vertex = v3 vertexData.[j] vertexData.[j+1] vertexData.[j+2]
                    vertices.[i] <- vertex

                // create indices
                let indices = indexData.ToArray ()

                // fin
                (vertices, indices, 0u, 0u, 0u, 0u)

        // make physically-based geometry
        let geometry =
            { Bounds = bounds
              PrimitiveType = primitiveType
              ElementCount = indexData.Length
              Vertices = vertices
              Indices = indices
              VertexBuffer = vertexBuffer
              InstanceBuffer = instanceBuffer
              IndexBuffer = indexBuffer
              PhysicallyBasedVao = vao }

        // fin
        geometry

    /// Create physically-based quad.
    let CreatePhysicallyBasedQuad renderable =
        let (vertexData, indexData, bounds) = CreatePhysicallyBasedQuadMesh ()
        CreatePhysicallyBasedStaticGeometry (renderable, PrimitiveType.Triangles, vertexData.AsMemory (), indexData.AsMemory (), bounds)

    /// Create physically-based billboard.
    let CreatePhysicallyBasedBillboard renderable =
        let (vertexData, indexData, bounds) = CreatePhysicallyBasedBillboardMesh ()
        CreatePhysicallyBasedStaticGeometry (renderable, PrimitiveType.Triangles, vertexData.AsMemory (), indexData.AsMemory (), bounds)

    /// Create physically-based cube.
    let CreatePhysicallyBasedCube renderable =
        let (vertexData, indexData, bounds) = CreatePhysicallyBasedCubeMesh ()
        CreatePhysicallyBasedStaticGeometry (renderable, PrimitiveType.Triangles, vertexData.AsMemory (), indexData.AsMemory (), bounds)

    /// Create a physically-based surface.
    let CreatePhysicallyBasedSurface (surfaceNames, surfaceMetadata, surfaceMatrix, surfaceBounds, properties, material, materialIndex, geometry) =
        PhysicallyBasedSurface.make surfaceNames surfaceMetadata surfaceMatrix surfaceBounds properties material materialIndex geometry

    /// Attempt to create physically-based material from an assimp scene.
    /// Thread-safe if renderable = false.
    let TryCreatePhysicallyBasedMaterials (renderable, dirPath, defaultMaterial, textureClient, scene : Assimp.Scene) =
        let mutable errorOpt = None
        let propertiesAndMaterials = Array.zeroCreate scene.Materials.Count
        for i in 0 .. dec scene.Materials.Count do
            if Option.isNone errorOpt then
                let (properties, material) = CreatePhysicallyBasedMaterial (renderable, dirPath, defaultMaterial, textureClient, scene.Materials.[i])
                propertiesAndMaterials.[i] <- (properties, material)
        match errorOpt with
        | Some error -> Left error
        | None -> Right propertiesAndMaterials

    /// Create physically-based static geometries from an assimp scene.
    /// OPTIMIZATION: duplicate geometry is detected and de-duplicated here, which does have some run-time cost.
    let CreatePhysicallyBasedStaticGeometries (renderable, scene : Assimp.Scene) =
        let meshAndGeometryLists = Dictionary<int * int * Assimp.BoundingBox, (Assimp.Mesh * PhysicallyBasedGeometry) List> HashIdentity.Structural
        let geometries = SList.make ()
        for i in 0 .. dec scene.Meshes.Count do
            let indexDataEntry = scene.Metadata.["IndexData" + string i]
            let indexData = indexDataEntry.Data :?> int array
            let mesh = scene.Meshes.[i]
            let mutable found = false
            let meshAndGeometryListOpt = Dictionary.tryFind (mesh.VertexCount, mesh.FaceCount, mesh.BoundingBox) meshAndGeometryLists
            match meshAndGeometryListOpt with
            | Some (meshAndGeometry : (Assimp.Mesh * PhysicallyBasedGeometry) List) ->
                let mutable enr = meshAndGeometry.GetEnumerator ()
                while not found && enr.MoveNext () do
                    let (meshCached, geometryCached) = enr.Current
                    if  Enumerable.SequenceEqual (meshCached.Vertices, mesh.Vertices) && 
                        Enumerable.SequenceEqual (meshCached.TextureCoordinateChannels.[0], mesh.TextureCoordinateChannels.[0]) && 
                        Enumerable.SequenceEqual (meshCached.Normals, mesh.Normals) then
                        geometries.Add geometryCached
                        found <- true
            | None -> ()
            if not found then
                let geometry = CreatePhysicallyBasedStaticGeometryFromMesh (renderable, indexData, mesh)
                match meshAndGeometryListOpt with
                | Some meshesAndGeometries -> meshesAndGeometries.Add (mesh, geometry)
                | None -> meshAndGeometryLists.[(mesh.VertexCount, mesh.FaceCount, mesh.BoundingBox)] <- List [(mesh, geometry)]
                geometries.Add geometry
        geometries

    /// Create physically-based animated geometries from an assimp scene.
    /// TODO: consider deduplicating geometry like in CreatePhysicallyBasedStaticGeometries?
    let CreatePhysicallyBasedAnimatedGeometries (renderable, scene : Assimp.Scene) =
        let geometries = SList.make ()
        for i in 0 .. dec scene.Meshes.Count do
            let indexDataEntry = scene.Metadata.["IndexData" + string i]
            let indexData = indexDataEntry.Data :?> int array
            let mesh = scene.Meshes.[i]
            let geometry = CreatePhysicallyBasedAnimatedGeometryFromMesh (renderable, indexData, mesh)
            geometries.Add geometry
        geometries

    /// Create a physically-based shader.
    let CreatePhysicallyBasedShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let viewUniform = Gl.GetUniformLocation (shader, "view")
        let projectionUniform = Gl.GetUniformLocation (shader, "projection")
        let bonesUniforms =
            Array.init Constants.Render.BonesMax $ fun i ->
                Gl.GetUniformLocation (shader, "bones[" + string i + "]")
        let eyeCenterUniform = Gl.GetUniformLocation (shader, "eyeCenter")
        let lightCutoffMarginUniform = Gl.GetUniformLocation (shader, "lightCutoffMargin")
        let lightAmbientColorUniform = Gl.GetUniformLocation (shader, "lightAmbientColor")
        let lightAmbientBrightnessUniform = Gl.GetUniformLocation (shader, "lightAmbientBrightness")
        let lightShadowSamplesUniform = Gl.GetUniformLocation (shader, "lightShadowSamples")
        let lightShadowBiasUniform = Gl.GetUniformLocation (shader, "lightShadowBias")
        let lightShadowSampleScalarUniform = Gl.GetUniformLocation (shader, "lightShadowSampleScalar")
        let lightShadowExponentUniform = Gl.GetUniformLocation (shader, "lightShadowExponent")
        let lightShadowDensityUniform = Gl.GetUniformLocation (shader, "lightShadowDensity")
        let ssvfEnabledUniform = Gl.GetUniformLocation (shader, "ssvfEnabled")
        let ssvfStepsUniform = Gl.GetUniformLocation (shader, "ssvfSteps")
        let ssvfAsymmetryUniform = Gl.GetUniformLocation (shader, "ssvfAsymmetry")
        let ssvfIntensityUniform = Gl.GetUniformLocation (shader, "ssvfIntensity")
        let albedoTextureUniform = Gl.GetUniformLocation (shader, "albedoTexture")
        let roughnessTextureUniform = Gl.GetUniformLocation (shader, "roughnessTexture")
        let metallicTextureUniform = Gl.GetUniformLocation (shader, "metallicTexture")
        let ambientOcclusionTextureUniform = Gl.GetUniformLocation (shader, "ambientOcclusionTexture")
        let emissionTextureUniform = Gl.GetUniformLocation (shader, "emissionTexture")
        let normalTextureUniform = Gl.GetUniformLocation (shader, "normalTexture")
        let heightTextureUniform = Gl.GetUniformLocation (shader, "heightTexture")
        let brdfTextureUniform = Gl.GetUniformLocation (shader, "brdfTexture")
        let irradianceMapUniform = Gl.GetUniformLocation (shader, "irradianceMap")
        let environmentFilterMapUniform = Gl.GetUniformLocation (shader, "environmentFilterMap")
        let irradianceMapsUniforms =
            Array.init Constants.Render.LightMapsMaxForward $ fun i ->
                Gl.GetUniformLocation (shader, "irradianceMaps[" + string i + "]")
        let environmentFilterMapsUniforms =
            Array.init Constants.Render.LightMapsMaxForward $ fun i ->
                Gl.GetUniformLocation (shader, "environmentFilterMaps[" + string i + "]")
        let shadowTexturesUniforms =
            Array.init Constants.Render.ShadowTexturesMax $ fun i ->
                Gl.GetUniformLocation (shader, "shadowTextures[" + string i + "]")
        let shadowMapsUniforms =
            Array.init Constants.Render.ShadowMapsMax $ fun i ->
                Gl.GetUniformLocation (shader, "shadowMaps[" + string i + "]")
        let lightMapOriginsUniform = Gl.GetUniformLocation (shader, "lightMapOrigins")
        let lightMapMinsUniform = Gl.GetUniformLocation (shader, "lightMapMins")
        let lightMapSizesUniform = Gl.GetUniformLocation (shader, "lightMapSizes")
        let lightMapAmbientColorsUniform = Gl.GetUniformLocation (shader, "lightMapAmbientColors")
        let lightMapAmbientBrightnessesUniform = Gl.GetUniformLocation (shader, "lightMapAmbientBrightnesses")
        let lightMapsCountUniform = Gl.GetUniformLocation (shader, "lightMapsCount")
        let lightOriginsUniform = Gl.GetUniformLocation (shader, "lightOrigins")
        let lightDirectionsUniform = Gl.GetUniformLocation (shader, "lightDirections")
        let lightColorsUniform = Gl.GetUniformLocation (shader, "lightColors")
        let lightBrightnessesUniform = Gl.GetUniformLocation (shader, "lightBrightnesses")
        let lightAttenuationLinearsUniform = Gl.GetUniformLocation (shader, "lightAttenuationLinears")
        let lightAttenuationQuadraticsUniform = Gl.GetUniformLocation (shader, "lightAttenuationQuadratics")
        let lightCutoffsUniform = Gl.GetUniformLocation (shader, "lightCutoffs")
        let lightTypesUniform = Gl.GetUniformLocation (shader, "lightTypes")
        let lightConeInnersUniform = Gl.GetUniformLocation (shader, "lightConeInners")
        let lightConeOutersUniform = Gl.GetUniformLocation (shader, "lightConeOuters")
        let lightShadowIndicesUniform = Gl.GetUniformLocation (shader, "lightShadowIndices")
        let lightsCountUniform = Gl.GetUniformLocation (shader, "lightsCount")
        let shadowMatricesUniforms =
            Array.init Constants.Render.ShadowTexturesMax $ fun i ->
                Gl.GetUniformLocation (shader, "shadowMatrices[" + string i + "]")

        // make shader record
        { ViewUniform = viewUniform
          ProjectionUniform = projectionUniform
          BonesUniforms = bonesUniforms
          EyeCenterUniform = eyeCenterUniform
          LightCutoffMarginUniform = lightCutoffMarginUniform
          LightAmbientColorUniform = lightAmbientColorUniform
          LightAmbientBrightnessUniform = lightAmbientBrightnessUniform
          LightShadowSamplesUniform = lightShadowSamplesUniform
          LightShadowBiasUniform = lightShadowBiasUniform
          LightShadowSampleScalarUniform = lightShadowSampleScalarUniform
          LightShadowExponentUniform = lightShadowExponentUniform
          LightShadowDensityUniform = lightShadowDensityUniform
          SsvfEnabledUniform = ssvfEnabledUniform
          SsvfStepsUniform = ssvfStepsUniform
          SsvfAsymmetryUniform = ssvfAsymmetryUniform
          SsvfIntensityUniform = ssvfIntensityUniform
          AlbedoTextureUniform = albedoTextureUniform
          RoughnessTextureUniform = roughnessTextureUniform
          MetallicTextureUniform = metallicTextureUniform
          AmbientOcclusionTextureUniform = ambientOcclusionTextureUniform
          EmissionTextureUniform = emissionTextureUniform
          NormalTextureUniform = normalTextureUniform
          HeightTextureUniform = heightTextureUniform
          BrdfTextureUniform = brdfTextureUniform
          IrradianceMapUniform = irradianceMapUniform
          EnvironmentFilterMapUniform = environmentFilterMapUniform
          IrradianceMapsUniforms = irradianceMapsUniforms
          EnvironmentFilterMapsUniforms = environmentFilterMapsUniforms
          ShadowTexturesUniforms = shadowTexturesUniforms
          ShadowMapsUniforms = shadowMapsUniforms
          LightMapOriginsUniform = lightMapOriginsUniform
          LightMapMinsUniform = lightMapMinsUniform
          LightMapSizesUniform = lightMapSizesUniform
          LightMapAmbientColorsUniform = lightMapAmbientColorsUniform
          LightMapAmbientBrightnessesUniform = lightMapAmbientBrightnessesUniform
          LightMapsCountUniform = lightMapsCountUniform
          LightOriginsUniform = lightOriginsUniform
          LightDirectionsUniform = lightDirectionsUniform
          LightColorsUniform = lightColorsUniform
          LightBrightnessesUniform = lightBrightnessesUniform
          LightAttenuationLinearsUniform = lightAttenuationLinearsUniform
          LightAttenuationQuadraticsUniform = lightAttenuationQuadraticsUniform
          LightCutoffsUniform = lightCutoffsUniform
          LightTypesUniform = lightTypesUniform
          LightConeInnersUniform = lightConeInnersUniform
          LightConeOutersUniform = lightConeOutersUniform
          LightShadowIndicesUniform = lightShadowIndicesUniform
          LightsCountUniform = lightsCountUniform
          ShadowMatricesUniforms = shadowMatricesUniforms
          PhysicallyBasedShader = shader }

    /// Create a physically-based terrain shader.
    let CreatePhysicallyBasedTerrainShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let viewUniform = Gl.GetUniformLocation (shader, "view")
        let projectionUniform = Gl.GetUniformLocation (shader, "projection")
        let eyeCenterUniform = Gl.GetUniformLocation (shader, "eyeCenter")
        let lightShadowSamplesUniform = Gl.GetUniformLocation (shader, "lightShadowSamples")
        let lightShadowBiasUniform = Gl.GetUniformLocation (shader, "lightShadowBias")
        let lightShadowSampleScalarUniform = Gl.GetUniformLocation (shader, "lightShadowSampleScalar")
        let lightShadowExponentUniform = Gl.GetUniformLocation (shader, "lightShadowExponent")
        let lightShadowDensityUniform = Gl.GetUniformLocation (shader, "lightShadowDensity")
        let layersCountUniform = Gl.GetUniformLocation (shader, "layersCount")
        let albedoTexturesUniforms =
            Array.init Constants.Render.TerrainLayersMax $ fun i ->
                Gl.GetUniformLocation (shader, "albedoTextures[" + string i + "]")
        let roughnessTexturesUniforms =
            Array.init Constants.Render.TerrainLayersMax $ fun i ->
                Gl.GetUniformLocation (shader, "roughnessTextures[" + string i + "]")
        let ambientOcclusionTexturesUniforms =
            Array.init Constants.Render.TerrainLayersMax $ fun i ->
                Gl.GetUniformLocation (shader, "ambientOcclusionTextures[" + string i + "]")
        let normalTexturesUniforms =
            Array.init Constants.Render.TerrainLayersMax $ fun i ->
                Gl.GetUniformLocation (shader, "normalTextures[" + string i + "]")
        let heightTexturesUniforms =
            Array.init Constants.Render.TerrainLayersMax $ fun i ->
                Gl.GetUniformLocation (shader, "heightTextures[" + string i + "]")

        // make shader record
        { ViewUniform = viewUniform
          ProjectionUniform = projectionUniform
          EyeCenterUniform = eyeCenterUniform
          LightShadowSamplesUniform = lightShadowSamplesUniform
          LightShadowBiasUniform = lightShadowBiasUniform
          LightShadowSampleScalarUniform = lightShadowSampleScalarUniform
          LightShadowExponentUniform = lightShadowExponentUniform
          LightShadowDensityUniform = lightShadowDensityUniform
          LayersCountUniform = layersCountUniform
          AlbedoTexturesUniforms = albedoTexturesUniforms
          RoughnessTexturesUniforms = roughnessTexturesUniforms
          AmbientOcclusionTexturesUniforms = ambientOcclusionTexturesUniforms
          NormalTexturesUniforms = normalTexturesUniforms
          HeightTexturesUniforms = heightTexturesUniforms
          PhysicallyBasedShader = shader } : PhysicallyBasedDeferredTerrainShader

    /// Create a physically-based shader for the light mapping pass of deferred rendering.
    let CreatePhysicallyBasedDeferredLightMappingShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let positionTextureUniform = Gl.GetUniformLocation (shader, "positionTexture")
        let normalPlusTextureUniform = Gl.GetUniformLocation (shader, "normalPlusTexture")
        let lightMapOriginsUniform = Gl.GetUniformLocation (shader, "lightMapOrigins")
        let lightMapMinsUniform = Gl.GetUniformLocation (shader, "lightMapMins")
        let lightMapSizesUniform = Gl.GetUniformLocation (shader, "lightMapSizes")
        let lightMapsCountUniform = Gl.GetUniformLocation (shader, "lightMapsCount")

        // make shader record
        { PositionTextureUniform = positionTextureUniform
          NormalPlusTextureUniform = normalPlusTextureUniform
          LightMapOriginsUniform = lightMapOriginsUniform
          LightMapMinsUniform = lightMapMinsUniform
          LightMapSizesUniform = lightMapSizesUniform
          LightMapsCountUniform = lightMapsCountUniform
          PhysicallyBasedDeferredLightMappingShader = shader }

    /// Create a physically-based shader for the ambient pass of deferred rendering.
    let CreatePhysicallyBasedDeferredAmbientShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let positionTextureUniform = Gl.GetUniformLocation (shader, "positionTexture")
        let lightMappingTextureUniform = Gl.GetUniformLocation (shader, "lightMappingTexture")
        let lightMapAmbientColorUniform = Gl.GetUniformLocation (shader, "lightMapAmbientColor")
        let lightMapAmbientBrightnessUniform = Gl.GetUniformLocation (shader, "lightMapAmbientBrightness")
        let lightMapAmbientColorsUniform = Gl.GetUniformLocation (shader, "lightMapAmbientColors")
        let lightMapAmbientBrightnessesUniform = Gl.GetUniformLocation (shader, "lightMapAmbientBrightnesses")

        // make shader record
        { PositionTextureUniform = positionTextureUniform
          LightMappingTextureUniform = lightMappingTextureUniform
          LightMapAmbientColorUniform = lightMapAmbientColorUniform
          LightMapAmbientBrightnessUniform = lightMapAmbientBrightnessUniform
          LightMapAmbientColorsUniform = lightMapAmbientColorsUniform
          LightMapAmbientBrightnessesUniform = lightMapAmbientBrightnessesUniform
          PhysicallyBasedDeferredAmbientShader = shader }

    /// Create a physically-based shader for the irradiance pass of deferred rendering.
    let CreatePhysicallyBasedDeferredIrradianceShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let positionTextureUniform = Gl.GetUniformLocation (shader, "positionTexture")
        let normalPlusTextureUniform = Gl.GetUniformLocation (shader, "normalPlusTexture")
        let lightMappingTextureUniform = Gl.GetUniformLocation (shader, "lightMappingTexture")
        let irradianceMapUniform = Gl.GetUniformLocation (shader, "irradianceMap")
        let irradianceMapsUniforms =
            Array.init Constants.Render.LightMapsMaxDeferred $ fun i ->
                Gl.GetUniformLocation (shader, "irradianceMaps[" + string i + "]")

        // make shader record
        { PositionTextureUniform = positionTextureUniform
          NormalPlusTextureUniform = normalPlusTextureUniform
          LightMappingTextureUniform = lightMappingTextureUniform
          IrradianceMapUniform = irradianceMapUniform
          IrradianceMapsUniforms = irradianceMapsUniforms
          PhysicallyBasedDeferredIrradianceShader = shader }

    /// Create a physically-based shader for the environment filter pass of deferred rendering.
    let CreatePhysicallyBasedDeferredEnvironmentFilterShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let eyeCenterUniform = Gl.GetUniformLocation (shader, "eyeCenter")
        let positionTextureUniform = Gl.GetUniformLocation (shader, "positionTexture")
        let materialTextureUniform = Gl.GetUniformLocation (shader, "materialTexture")
        let normalPlusTextureUniform = Gl.GetUniformLocation (shader, "normalPlusTexture")
        let lightMappingTextureUniform = Gl.GetUniformLocation (shader, "lightMappingTexture")
        let environmentFilterMapUniform = Gl.GetUniformLocation (shader, "environmentFilterMap")
        let environmentFilterMapsUniforms =
            Array.init Constants.Render.LightMapsMaxDeferred $ fun i ->
                Gl.GetUniformLocation (shader, "environmentFilterMaps[" + string i + "]")
        let lightMapOriginsUniform = Gl.GetUniformLocation (shader, "lightMapOrigins")
        let lightMapMinsUniform = Gl.GetUniformLocation (shader, "lightMapMins")
        let lightMapSizesUniform = Gl.GetUniformLocation (shader, "lightMapSizes")

        // make shader record
        { EyeCenterUniform = eyeCenterUniform
          PositionTextureUniform = positionTextureUniform
          MaterialTextureUniform = materialTextureUniform
          NormalPlusTextureUniform = normalPlusTextureUniform
          LightMappingTextureUniform = lightMappingTextureUniform
          EnvironmentFilterMapUniform = environmentFilterMapUniform
          EnvironmentFilterMapsUniforms = environmentFilterMapsUniforms
          LightMapOriginsUniform = lightMapOriginsUniform
          LightMapMinsUniform = lightMapMinsUniform
          LightMapSizesUniform = lightMapSizesUniform
          PhysicallyBasedDeferredEnvironmentFilterShader = shader }

    /// Create a physically-based shader for the ssao pass of deferred rendering.
    let CreatePhysicallyBasedDeferredSsaoShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let viewUniform = Gl.GetUniformLocation (shader, "view")
        let projectionUniform = Gl.GetUniformLocation (shader, "projection")
        let positionTextureUniform = Gl.GetUniformLocation (shader, "positionTexture")
        let normalPlusTextureUniform = Gl.GetUniformLocation (shader, "normalPlusTexture")
        let ssaoResolution = Gl.GetUniformLocation (shader, "ssaoResolution")
        let ssaoIntensity = Gl.GetUniformLocation (shader, "ssaoIntensity")
        let ssaoBias = Gl.GetUniformLocation (shader, "ssaoBias")
        let ssaoRadius = Gl.GetUniformLocation (shader, "ssaoRadius")
        let ssaoDistanceMax = Gl.GetUniformLocation (shader, "ssaoDistanceMax")
        let ssaoSampleCount = Gl.GetUniformLocation (shader, "ssaoSampleCount")

        // make shader record
        { ViewUniform = viewUniform
          ProjectionUniform = projectionUniform
          PositionTextureUniform = positionTextureUniform
          NormalPlusTextureUniform = normalPlusTextureUniform
          SsaoResolution = ssaoResolution
          SsaoIntensity = ssaoIntensity
          SsaoBias = ssaoBias
          SsaoRadius = ssaoRadius
          SsaoDistanceMax = ssaoDistanceMax
          SsaoSampleCount = ssaoSampleCount
          PhysicallyBasedDeferredSsaoShader = shader }
          
    /// Create a physically-based shader for the lighting pass of deferred rendering.
    let CreatePhysicallyBasedDeferredLightingShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let eyeCenterUniform = Gl.GetUniformLocation (shader, "eyeCenter")
        let viewUniform = Gl.GetUniformLocation (shader, "view")
        let projectionUniform = Gl.GetUniformLocation (shader, "projection")
        let lightCutoffMarginUniform = Gl.GetUniformLocation (shader, "lightCutoffMargin")
        let lightAmbientColorUniform = Gl.GetUniformLocation (shader, "lightAmbientColor")
        let lightAmbientBrightnessUniform = Gl.GetUniformLocation (shader, "lightAmbientBrightness")
        let lightShadowSamplesUniform = Gl.GetUniformLocation (shader, "lightShadowSamples")
        let lightShadowBiasUniform = Gl.GetUniformLocation (shader, "lightShadowBias")
        let lightShadowSampleScalarUniform = Gl.GetUniformLocation (shader, "lightShadowSampleScalar")
        let lightShadowExponentUniform = Gl.GetUniformLocation (shader, "lightShadowExponent")
        let lightShadowDensityUniform = Gl.GetUniformLocation (shader, "lightShadowDensity")
        let ssvfEnabledUniform = Gl.GetUniformLocation (shader, "ssvfEnabled")
        let ssvfStepsUniform = Gl.GetUniformLocation (shader, "ssvfSteps")
        let ssvfAsymmetryUniform = Gl.GetUniformLocation (shader, "ssvfAsymmetry")
        let ssvfIntensityUniform = Gl.GetUniformLocation (shader, "ssvfIntensity")
        let ssrEnabledUniform = Gl.GetUniformLocation (shader, "ssrEnabled")
        let ssrDetailUniform = Gl.GetUniformLocation (shader, "ssrDetail")
        let ssrRefinementsMaxUniform = Gl.GetUniformLocation (shader, "ssrRefinementsMax")
        let ssrRayThicknessUniform = Gl.GetUniformLocation (shader, "ssrRayThickness")
        let ssrTowardEyeCutoffUniform = Gl.GetUniformLocation (shader, "ssrTowardEyeCutoff")
        let ssrDepthCutoffUniform = Gl.GetUniformLocation (shader, "ssrDepthCutoff")
        let ssrDepthCutoffMarginUniform = Gl.GetUniformLocation (shader, "ssrDepthCutoffMargin")
        let ssrDistanceCutoffUniform = Gl.GetUniformLocation (shader, "ssrDistanceCutoff")
        let ssrDistanceCutoffMarginUniform = Gl.GetUniformLocation (shader, "ssrDistanceCutoffMargin")
        let ssrRoughnessCutoffUniform = Gl.GetUniformLocation (shader, "ssrRoughnessCutoff")
        let ssrRoughnessCutoffMarginUniform = Gl.GetUniformLocation (shader, "ssrRoughnessCutoffMargin")
        let ssrSlopeCutoffUniform = Gl.GetUniformLocation (shader, "ssrSlopeCutoff")
        let ssrSlopeCutoffMarginUniform = Gl.GetUniformLocation (shader, "ssrSlopeCutoffMargin")
        let ssrEdgeHorizontalMarginUniform = Gl.GetUniformLocation (shader, "ssrEdgeHorizontalMargin")
        let ssrEdgeVerticalMarginUniform = Gl.GetUniformLocation (shader, "ssrEdgeVerticalMargin")
        let ssrLightColorUniform = Gl.GetUniformLocation (shader, "ssrLightColor")
        let ssrLightBrightnessUniform = Gl.GetUniformLocation (shader, "ssrLightBrightness")
        let positionTextureUniform = Gl.GetUniformLocation (shader, "positionTexture")
        let albedoTextureUniform = Gl.GetUniformLocation (shader, "albedoTexture")
        let materialTextureUniform = Gl.GetUniformLocation (shader, "materialTexture")
        let normalPlusTextureUniform = Gl.GetUniformLocation (shader, "normalPlusTexture")
        let brdfTextureUniform = Gl.GetUniformLocation (shader, "brdfTexture")
        let ambientTextureUniform = Gl.GetUniformLocation (shader, "ambientTexture")
        let irradianceTextureUniform = Gl.GetUniformLocation (shader, "irradianceTexture")
        let environmentFilterTextureUniform = Gl.GetUniformLocation (shader, "environmentFilterTexture")
        let ssaoTextureUniform = Gl.GetUniformLocation (shader, "ssaoTexture")
        let shadowTexturesUniforms =
            Array.init Constants.Render.ShadowTexturesMax $ fun i ->
                Gl.GetUniformLocation (shader, "shadowTextures[" + string i + "]")
        let shadowMapsUniforms =
            Array.init Constants.Render.ShadowMapsMax $ fun i ->
                Gl.GetUniformLocation (shader, "shadowMaps[" + string i + "]")
        let lightOriginsUniform = Gl.GetUniformLocation (shader, "lightOrigins")
        let lightDirectionsUniform = Gl.GetUniformLocation (shader, "lightDirections")
        let lightColorsUniform = Gl.GetUniformLocation (shader, "lightColors")
        let lightBrightnessesUniform = Gl.GetUniformLocation (shader, "lightBrightnesses")
        let lightAttenuationLinearsUniform = Gl.GetUniformLocation (shader, "lightAttenuationLinears")
        let lightAttenuationQuadraticsUniform = Gl.GetUniformLocation (shader, "lightAttenuationQuadratics")
        let lightCutoffsUniform = Gl.GetUniformLocation (shader, "lightCutoffs")
        let lightTypesUniform = Gl.GetUniformLocation (shader, "lightTypes")
        let lightConeInnersUniform = Gl.GetUniformLocation (shader, "lightConeInners")
        let lightConeOutersUniform = Gl.GetUniformLocation (shader, "lightConeOuters")
        let lightShadowIndicesUniform = Gl.GetUniformLocation (shader, "lightShadowIndices")
        let lightsCountUniform = Gl.GetUniformLocation (shader, "lightsCount")
        let shadowMatricesUniforms =
            Array.init Constants.Render.ShadowTexturesMax $ fun i ->
                Gl.GetUniformLocation (shader, "shadowMatrices[" + string i + "]")

        // make shader record
        { EyeCenterUniform = eyeCenterUniform
          ViewUniform = viewUniform
          ProjectionUniform = projectionUniform
          LightCutoffMarginUniform = lightCutoffMarginUniform
          LightAmbientColorUniform = lightAmbientColorUniform
          LightAmbientBrightnessUniform = lightAmbientBrightnessUniform
          LightShadowSamplesUniform = lightShadowSamplesUniform
          LightShadowBiasUniform = lightShadowBiasUniform
          LightShadowSampleScalarUniform = lightShadowSampleScalarUniform
          LightShadowExponentUniform = lightShadowExponentUniform
          LightShadowDensityUniform = lightShadowDensityUniform
          SsvfEnabledUniform = ssvfEnabledUniform
          SsvfStepsUniform = ssvfStepsUniform
          SsvfAsymmetryUniform = ssvfAsymmetryUniform
          SsvfIntensityUniform = ssvfIntensityUniform
          SsrEnabledUniform = ssrEnabledUniform
          SsrDetailUniform = ssrDetailUniform
          SsrRefinementsMaxUniform = ssrRefinementsMaxUniform
          SsrRayThicknessUniform = ssrRayThicknessUniform
          SsrTowardEyeCutoffUniform = ssrTowardEyeCutoffUniform
          SsrDepthCutoffUniform = ssrDepthCutoffUniform
          SsrDepthCutoffMarginUniform = ssrDepthCutoffMarginUniform
          SsrDistanceCutoffUniform = ssrDistanceCutoffUniform
          SsrDistanceCutoffMarginUniform = ssrDistanceCutoffMarginUniform
          SsrRoughnessCutoffUniform = ssrRoughnessCutoffUniform
          SsrRoughnessCutoffMarginUniform = ssrRoughnessCutoffMarginUniform
          SsrSlopeCutoffUniform = ssrSlopeCutoffUniform
          SsrSlopeCutoffMarginUniform = ssrSlopeCutoffMarginUniform
          SsrEdgeHorizontalMarginUniform = ssrEdgeHorizontalMarginUniform
          SsrEdgeVerticalMarginUniform = ssrEdgeVerticalMarginUniform
          SsrLightColorUniform = ssrLightColorUniform
          SsrLightBrightnessUniform = ssrLightBrightnessUniform
          PositionTextureUniform = positionTextureUniform
          AlbedoTextureUniform = albedoTextureUniform
          MaterialTextureUniform = materialTextureUniform
          NormalPlusTextureUniform = normalPlusTextureUniform
          BrdfTextureUniform = brdfTextureUniform
          AmbientTextureUniform = ambientTextureUniform
          IrradianceTextureUniform = irradianceTextureUniform
          EnvironmentFilterTextureUniform = environmentFilterTextureUniform
          SsaoTextureUniform = ssaoTextureUniform
          ShadowTexturesUniforms = shadowTexturesUniforms
          ShadowMapsUniforms = shadowMapsUniforms
          LightOriginsUniform = lightOriginsUniform
          LightDirectionsUniform = lightDirectionsUniform
          LightColorsUniform = lightColorsUniform
          LightBrightnessesUniform = lightBrightnessesUniform
          LightAttenuationLinearsUniform = lightAttenuationLinearsUniform
          LightAttenuationQuadraticsUniform = lightAttenuationQuadraticsUniform
          LightCutoffsUniform = lightCutoffsUniform
          LightTypesUniform = lightTypesUniform
          LightConeInnersUniform = lightConeInnersUniform
          LightConeOutersUniform = lightConeOutersUniform
          LightShadowIndicesUniform = lightShadowIndicesUniform
          LightsCountUniform = lightsCountUniform
          ShadowMatricesUniforms = shadowMatricesUniforms
          PhysicallyBasedDeferredLightingShader = shader }

    /// Create a physically-based shader for the composition pass of deferred rendering.
    let CreatePhysicallyBasedDeferredCompositionShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let colorTextureUniform = Gl.GetUniformLocation (shader, "colorTexture")
        let fogAccumTextureUniform = Gl.GetUniformLocation (shader, "fogAccumTexture")

        // make shader record
        { ColorTextureUniform = colorTextureUniform
          FogAccumTextureUniform = fogAccumTextureUniform
          PhysicallyBasedDeferredCompositionShader = shader }

    /// Create the shaders for physically-based deferred rendering.
    let CreatePhysicallyBasedDeferredShaders
        (shaderStaticFilePath,
         shaderAnimatedFilePath,
         terrainShaderFilePath,
         shaderLightMappingFilePath,
         shaderAmbientFilePath,
         shaderIrradianceFilePath,
         shaderEnvironmentFilterFilePath,
         shaderSsaoFilePath,
         shaderLightingFilePath,
         shaderCompositionFilePath) =
        let shaderStatic = CreatePhysicallyBasedShader shaderStaticFilePath in Hl.Assert ()
        let shaderAnimated = CreatePhysicallyBasedShader shaderAnimatedFilePath in Hl.Assert ()
        let shaderTerrain = CreatePhysicallyBasedTerrainShader terrainShaderFilePath in Hl.Assert ()
        let shaderLightMapping = CreatePhysicallyBasedDeferredLightMappingShader shaderLightMappingFilePath in Hl.Assert ()
        let shaderAmbient = CreatePhysicallyBasedDeferredAmbientShader shaderAmbientFilePath in Hl.Assert ()
        let shaderIrradiance = CreatePhysicallyBasedDeferredIrradianceShader shaderIrradianceFilePath in Hl.Assert ()
        let shaderEnvironmentFilter = CreatePhysicallyBasedDeferredEnvironmentFilterShader shaderEnvironmentFilterFilePath in Hl.Assert ()
        let shaderSsao = CreatePhysicallyBasedDeferredSsaoShader shaderSsaoFilePath in Hl.Assert ()
        let shaderLighting = CreatePhysicallyBasedDeferredLightingShader shaderLightingFilePath
        let shaderComposition = CreatePhysicallyBasedDeferredCompositionShader shaderCompositionFilePath
        (shaderStatic, shaderAnimated, shaderTerrain, shaderLightMapping, shaderAmbient, shaderIrradiance, shaderEnvironmentFilter, shaderSsao, shaderLighting, shaderComposition)

    /// Create the shaders for physically-based shadow rendering.
    let CreatePhysicallyBasedShadowShaders
        (shaderStaticShadowPointFilePath,
         shaderStaticShadowSpotFilePath,
         shaderStaticShadowDirectionalFilePath,
         shaderAnimatedShadowPointFilePath,
         shaderAnimatedShadowSpotFilePath,
         shaderAnimatedShadowDirectionalFilePath,
         shaderTerrainShadowPointFilePath,
         shaderTerrainShadowSpotFilePath,
         shaderTerrainShadowDirectionalFilePath) =
        let shaderStaticShadowPoint = CreatePhysicallyBasedShader shaderStaticShadowPointFilePath in Hl.Assert ()
        let shaderStaticShadowSpot = CreatePhysicallyBasedShader shaderStaticShadowSpotFilePath in Hl.Assert ()
        let shaderStaticShadowDirectional = CreatePhysicallyBasedShader shaderStaticShadowDirectionalFilePath in Hl.Assert ()
        let shaderAnimatedShadowPoint = CreatePhysicallyBasedShader shaderAnimatedShadowPointFilePath in Hl.Assert ()
        let shaderAnimatedShadowSpot = CreatePhysicallyBasedShader shaderAnimatedShadowSpotFilePath in Hl.Assert ()
        let shaderAnimatedShadowDirectional = CreatePhysicallyBasedShader shaderAnimatedShadowDirectionalFilePath in Hl.Assert ()
        let shaderTerrainShadowPoint = CreatePhysicallyBasedTerrainShader shaderTerrainShadowPointFilePath in Hl.Assert ()
        let shaderTerrainShadowSpot = CreatePhysicallyBasedTerrainShader shaderTerrainShadowSpotFilePath in Hl.Assert ()
        let shaderTerrainShadowDirectional = CreatePhysicallyBasedTerrainShader shaderTerrainShadowDirectionalFilePath in Hl.Assert ()
        (shaderStaticShadowPoint,
         shaderStaticShadowSpot,
         shaderStaticShadowDirectional,
         shaderAnimatedShadowPoint,
         shaderAnimatedShadowSpot,
         shaderAnimatedShadowDirectional,
         shaderTerrainShadowPoint,
         shaderTerrainShadowSpot,
         shaderTerrainShadowDirectional)

    /// Draw the filter box pass using a physically-based surface.
    let DrawFilterBoxSurface
        (inputTexture : Texture.Texture,
         geometry : PhysicallyBasedGeometry,
         shader : Filter.FilterBoxShader) =

        // setup shader
        Gl.UseProgram shader.FilterBoxShader
        Gl.Uniform1 (shader.InputTextureUniform, 0)
        Hl.Assert ()

        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, inputTexture.TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u

    /// Draw the filter gaussian pass using a physically-based surface.
    let DrawFilterGaussianSurface
        (scale : Vector2,
         inputTexture : Texture.Texture,
         geometry : PhysicallyBasedGeometry,
         shader : Filter.FilterGaussianShader) =

        // setup shader
        Gl.UseProgram shader.FilterGaussianShader
        Gl.Uniform2 (shader.ScaleUniform, scale.X, scale.Y)
        Gl.Uniform1 (shader.InputTextureUniform, 0)
        Hl.Assert ()

        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, inputTexture.TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown shader
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.UseProgram 0u

    /// Draw the filter bilateral down-sample pass using a physically-based surface.
    let DrawFilterBilateralDownSampleSurface
        (colorTexture : Texture.Texture,
         depthTexture : Texture.Texture,
         geometry : PhysicallyBasedGeometry,
         shader : Filter.FilterBilateralDownSampleShader) =

        // setup shader
        Gl.UseProgram shader.FilterBilateralDownSampleShader
        Gl.Uniform1 (shader.ColorTextureUniform, 0)
        Gl.Uniform1 (shader.DepthTextureUniform, 1)
        Hl.Assert ()

        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, colorTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture1
        Gl.BindTexture (TextureTarget.Texture2d, depthTexture.TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown shader
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.ActiveTexture TextureUnit.Texture1
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.UseProgram 0u

    /// Draw the filter bilateral up-sample pass using a physically-based surface.
    let DrawFilterBilateralUpSampleSurface
        (colorDownSampledTexture : Texture.Texture,
         depthDownSampledTexture : Texture.Texture,
         depthTexture : Texture.Texture,
         geometry : PhysicallyBasedGeometry,
         shader : Filter.FilterBilateralUpSampleShader) =

        // setup shader
        Gl.UseProgram shader.FilterBilateralUpSampleShader
        Gl.Uniform1 (shader.ColorDownSampledTextureUniform, 0)
        Gl.Uniform1 (shader.DepthDownSampledTextureUniform, 1)
        Gl.Uniform1 (shader.DepthTextureUniform, 2)
        Hl.Assert ()

        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, colorDownSampledTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture1
        Gl.BindTexture (TextureTarget.Texture2d, depthDownSampledTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture2
        Gl.BindTexture (TextureTarget.Texture2d, depthTexture.TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown shader
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.ActiveTexture TextureUnit.Texture1
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.ActiveTexture TextureUnit.Texture2
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.UseProgram 0u

    /// Draw the filter fxaa pass using a physically-based surface.
    let DrawFilterFxaaSurface
        (inputTexture : Texture.Texture,
         geometry : PhysicallyBasedGeometry,
         shader : Filter.FilterFxaaShader) =

        // setup shader
        Gl.UseProgram shader.FilterFxaaShader
        Gl.Uniform1 (shader.InputTextureUniform, 0)
        Hl.Assert ()

        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, inputTexture.TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown shader
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.UseProgram 0u

    /// Draw a batch of physically-based depth surfaces.
    let DrawPhysicallyBasedDepthSurfaces
        (batchPhase : BatchPhase,
         eyeCenter : Vector3,
         view : single array,
         projection : single array,
         bones : single array array,
         surfacesCount : int,
         instanceFields : single array,
         lightShadowExponent : single,
         material : PhysicallyBasedMaterial,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedShader) =

        // setup dynamic state
        if not material.TwoSided then Gl.Enable EnableCap.CullFace
        Hl.Assert ()

        // start batch
        if batchPhase.Starting then

            // setup state
            Gl.DepthFunc DepthFunction.Lequal
            Gl.Enable EnableCap.DepthTest
            Hl.Assert ()

            // setup shader
            Gl.UseProgram shader.PhysicallyBasedShader
            Gl.Uniform3 (shader.EyeCenterUniform, eyeCenter.X, eyeCenter.Y, eyeCenter.Z)
            Gl.UniformMatrix4 (shader.ViewUniform, false, view)
            Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
            for i in 0 .. dec (min Constants.Render.BonesMax bones.Length) do
                Gl.UniformMatrix4 (shader.BonesUniforms.[i], false, bones.[i])
            Gl.Uniform1 (shader.LightShadowExponentUniform, lightShadowExponent)
            Hl.Assert ()

        // only set up uniforms when there is a surface to render to avoid potentially utilizing destroyed textures
        if surfacesCount > 0 then

            // update instance buffer
            let instanceFieldsPtr = GCHandle.Alloc (instanceFields, GCHandleType.Pinned)
            try Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.InstanceBuffer)
                Gl.BufferData (BufferTarget.ArrayBuffer, uint (surfacesCount * Constants.Render.InstanceFieldCount * sizeof<single>), instanceFieldsPtr.AddrOfPinnedObject (), BufferUsage.StreamDraw)
                Gl.BindBuffer (BufferTarget.ArrayBuffer, 0u)
                Hl.Assert ()
            finally instanceFieldsPtr.Free ()

            // setup geometry
            Gl.BindVertexArray geometry.PhysicallyBasedVao
            Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
            Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
            Hl.Assert ()

            // draw geometry
            Gl.DrawElementsInstanced (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0, surfacesCount)
            Hl.ReportDrawCall surfacesCount
            Hl.Assert ()

        // stop batch
        if batchPhase.Stopping then

            // teardown geometry
            Gl.BindVertexArray 0u
            Hl.Assert ()

            // teardown shader
            Gl.UseProgram 0u
            Hl.Assert ()

            // teardown state
            Gl.DepthFunc DepthFunction.Less
            Gl.Disable EnableCap.DepthTest
            Hl.Assert ()

        // teardown dynamic state
        if not material.TwoSided then Gl.Disable EnableCap.CullFace

    /// Draw a batch of physically-based deferred surfaces.
    let DrawPhysicallyBasedDeferredSurfaces
        (batchPhase : BatchPhase,
         view : single array,
         projection : single array,
         bones : single array array,
         eyeCenter : Vector3,
         surfacesCount : int,
         instanceFields : single array,
         lightShadowSamples : int,
         lightShadowBias : single,
         lightShadowSampleScalar : single,
         lightShadowExponent : single,
         lightShadowDensity : single,
         material : PhysicallyBasedMaterial,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedShader) =

        // setup dynamic state
        if not material.TwoSided then Gl.Enable EnableCap.CullFace
        Hl.Assert ()

        // start batch
        if batchPhase.Starting then

            // setup static state
            Gl.DepthFunc DepthFunction.Lequal
            Gl.Enable EnableCap.DepthTest
            Hl.Assert ()

            // setup shader
            Gl.UseProgram shader.PhysicallyBasedShader
            Gl.UniformMatrix4 (shader.ViewUniform, false, view)
            Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
            for i in 0 .. dec (min Constants.Render.BonesMax bones.Length) do
                Gl.UniformMatrix4 (shader.BonesUniforms.[i], false, bones.[i])
            Gl.Uniform3 (shader.EyeCenterUniform, eyeCenter.X, eyeCenter.Y, eyeCenter.Z)
            Gl.Uniform1 (shader.LightShadowSamplesUniform, lightShadowSamples)
            Gl.Uniform1 (shader.LightShadowBiasUniform, lightShadowBias)
            Gl.Uniform1 (shader.LightShadowSampleScalarUniform, lightShadowSampleScalar)
            Gl.Uniform1 (shader.LightShadowExponentUniform, lightShadowExponent)
            Gl.Uniform1 (shader.LightShadowDensityUniform, lightShadowDensity)
            Gl.Uniform1 (shader.AlbedoTextureUniform, 0)
            Gl.Uniform1 (shader.RoughnessTextureUniform, 1)
            Gl.Uniform1 (shader.MetallicTextureUniform, 2)
            Gl.Uniform1 (shader.AmbientOcclusionTextureUniform, 3)
            Gl.Uniform1 (shader.EmissionTextureUniform, 4)
            Gl.Uniform1 (shader.NormalTextureUniform, 5)
            Gl.Uniform1 (shader.HeightTextureUniform, 6)
            Hl.Assert ()

        // only set up uniforms when there is a surface to render to avoid potentially utilizing destroyed textures
        if surfacesCount > 0 then

            // setup textures
            Gl.ActiveTexture TextureUnit.Texture0
            Gl.BindTexture (TextureTarget.Texture2d, material.AlbedoTexture.TextureId)
            Gl.ActiveTexture TextureUnit.Texture1
            Gl.BindTexture (TextureTarget.Texture2d, material.RoughnessTexture.TextureId)
            Gl.ActiveTexture TextureUnit.Texture2
            Gl.BindTexture (TextureTarget.Texture2d, material.MetallicTexture.TextureId)
            Gl.ActiveTexture TextureUnit.Texture3
            Gl.BindTexture (TextureTarget.Texture2d, material.AmbientOcclusionTexture.TextureId)
            Gl.ActiveTexture TextureUnit.Texture4
            Gl.BindTexture (TextureTarget.Texture2d, material.EmissionTexture.TextureId)
            Gl.ActiveTexture TextureUnit.Texture5
            Gl.BindTexture (TextureTarget.Texture2d, material.NormalTexture.TextureId)
            Gl.ActiveTexture TextureUnit.Texture6
            Gl.BindTexture (TextureTarget.Texture2d, material.HeightTexture.TextureId)
            Hl.Assert ()

            // update instance buffer
            let instanceFieldsPtr = GCHandle.Alloc (instanceFields, GCHandleType.Pinned)
            try Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.InstanceBuffer)
                Gl.BufferData (BufferTarget.ArrayBuffer, uint (surfacesCount * Constants.Render.InstanceFieldCount * sizeof<single>), instanceFieldsPtr.AddrOfPinnedObject (), BufferUsage.StreamDraw)
                Gl.BindBuffer (BufferTarget.ArrayBuffer, 0u)
                Hl.Assert ()
            finally instanceFieldsPtr.Free ()

            // setup geometry
            Gl.BindVertexArray geometry.PhysicallyBasedVao
            Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
            Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
            Hl.Assert ()

            // draw geometry
            Gl.DrawElementsInstanced (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0, surfacesCount)
            Hl.ReportDrawCall surfacesCount
            Hl.Assert ()

            // teardown textures
            Gl.ActiveTexture TextureUnit.Texture0
            Gl.BindTexture (TextureTarget.Texture2d, 0u)
            Gl.ActiveTexture TextureUnit.Texture1
            Gl.BindTexture (TextureTarget.Texture2d, 0u)
            Gl.ActiveTexture TextureUnit.Texture2
            Gl.BindTexture (TextureTarget.Texture2d, 0u)
            Gl.ActiveTexture TextureUnit.Texture3
            Gl.BindTexture (TextureTarget.Texture2d, 0u)
            Gl.ActiveTexture TextureUnit.Texture4
            Gl.BindTexture (TextureTarget.Texture2d, 0u)
            Gl.ActiveTexture TextureUnit.Texture5
            Gl.BindTexture (TextureTarget.Texture2d, 0u)
            Gl.ActiveTexture TextureUnit.Texture6
            Gl.BindTexture (TextureTarget.Texture2d, 0u)

        // stop batch
        if batchPhase.Stopping then

            // teardown geometry in general
            Gl.BindVertexArray 0u
            Hl.Assert ()

            // teardown shader
            Gl.UseProgram 0u
            Hl.Assert ()

            // teardown static state
            Gl.DepthFunc DepthFunction.Less
            Gl.Disable EnableCap.DepthTest
            Hl.Assert ()

        // teardown dynamic state
        if not material.TwoSided then Gl.Disable EnableCap.CullFace

    /// Draw a batch of physically-based forward surfaces.
    let DrawPhysicallyBasedForwardSurfaces
        (view : single array,
         projection : single array,
         bones : single array array,
         surfacesCount : int,
         instanceFields : single array,
         eyeCenter : Vector3,
         lightCutoffMargin : single,
         lightAmbientColor : single array,
         lightAmbientBrightness : single,
         lightShadowSamples : int,
         lightShadowBias : single,
         lightShadowSampleScalar : single,
         lightShadowExponent : single,
         lightShadowDensity : single,
         ssvfEnabled : int,
         ssvfSteps : int,
         ssvfAsymmetry : single,
         ssvfIntensity : single,
         brdfTexture : Texture.Texture,
         irradianceMap : Texture.Texture,
         environmentFilterMap : Texture.Texture,
         irradianceMaps : Texture.Texture array,
         environmentFilterMaps : Texture.Texture array,
         shadowTextures : Texture.Texture array,
         shadowMaps : Texture.Texture array,
         lightMapOrigins : single array,
         lightMapMins : single array,
         lightMapSizes : single array,
         lightMapAmbientColors : single array,
         lightMapAmbientBrightnesses : single array,
         lightMapsCount : int,
         lightOrigins : single array,
         lightDirections : single array,
         lightColors : single array,
         lightBrightnesses : single array,
         lightAttenuationLinears : single array,
         lightAttenuationQuadratics : single array,
         lightCutoffs : single array,
         lightTypes : int array,
         lightConeInners : single array,
         lightConeOuters : single array,
         lightShadowIndices : int array,
         lightsCount : int,
         shadowMatrices : single array array,
         material : PhysicallyBasedMaterial,
         geometry : PhysicallyBasedGeometry,
         depthTest : DepthTest,
         blending : bool,
         shader : PhysicallyBasedShader) =

        // only set up uniforms when there is a surface to render to avoid potentially utilizing destroyed textures
        if surfacesCount > 0 then

            // setup state
            match depthTest with
            | LessThanTest ->
                Gl.DepthFunc DepthFunction.Less
                Gl.Enable EnableCap.DepthTest
            | LessThanOrEqualTest ->
                Gl.DepthFunc DepthFunction.Lequal
                Gl.Enable EnableCap.DepthTest
            | EqualTest ->
                Gl.DepthFunc DepthFunction.Equal
                Gl.Enable EnableCap.DepthTest
            | GreaterThanOrEqualTest ->
                Gl.DepthFunc DepthFunction.Gequal
                Gl.Enable EnableCap.DepthTest
            | GreaterThanTest ->
                Gl.DepthFunc DepthFunction.Greater
                Gl.Enable EnableCap.DepthTest
            | NeverPassTest ->
                Gl.DepthFunc DepthFunction.Never
                Gl.Enable EnableCap.DepthTest
            | AlwaysPassTest -> ()
            if blending then
                Gl.BlendEquation BlendEquationMode.FuncAdd
                Gl.BlendFunc (BlendingFactor.SrcAlpha, BlendingFactor.OneMinusSrcAlpha)
                Gl.Enable EnableCap.Blend
            if not material.TwoSided then Gl.Enable EnableCap.CullFace
            Hl.Assert ()

            // setup shader
            Gl.UseProgram shader.PhysicallyBasedShader
            Gl.UniformMatrix4 (shader.ViewUniform, false, view)
            Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
            for i in 0 .. dec (min Constants.Render.BonesMax bones.Length) do
                Gl.UniformMatrix4 (shader.BonesUniforms.[i], false, bones.[i])
            Gl.Uniform3 (shader.EyeCenterUniform, eyeCenter.X, eyeCenter.Y, eyeCenter.Z)
            Gl.Uniform1 (shader.LightCutoffMarginUniform, lightCutoffMargin)
            if lightAmbientColor.Length = 3 then
                Gl.Uniform3 (shader.LightAmbientColorUniform, lightAmbientColor)
            Gl.Uniform1 (shader.LightAmbientBrightnessUniform, lightAmbientBrightness)
            Gl.Uniform1 (shader.LightShadowSamplesUniform, lightShadowSamples)
            Gl.Uniform1 (shader.LightShadowBiasUniform, lightShadowBias)
            Gl.Uniform1 (shader.LightShadowSampleScalarUniform, lightShadowSampleScalar)
            Gl.Uniform1 (shader.LightShadowExponentUniform, lightShadowExponent)
            Gl.Uniform1 (shader.LightShadowDensityUniform, lightShadowDensity)
            Gl.Uniform1 (shader.SsvfEnabledUniform, ssvfEnabled)
            Gl.Uniform1 (shader.SsvfStepsUniform, ssvfSteps)
            Gl.Uniform1 (shader.SsvfAsymmetryUniform, ssvfAsymmetry)
            Gl.Uniform1 (shader.SsvfIntensityUniform, ssvfIntensity)
            Gl.Uniform1 (shader.AlbedoTextureUniform, 0)
            Gl.Uniform1 (shader.RoughnessTextureUniform, 1)
            Gl.Uniform1 (shader.MetallicTextureUniform, 2)
            Gl.Uniform1 (shader.AmbientOcclusionTextureUniform, 3)
            Gl.Uniform1 (shader.EmissionTextureUniform, 4)
            Gl.Uniform1 (shader.NormalTextureUniform, 5)
            Gl.Uniform1 (shader.HeightTextureUniform, 6)
            Gl.Uniform1 (shader.BrdfTextureUniform, 7)
            Gl.Uniform1 (shader.IrradianceMapUniform, 8)
            Gl.Uniform1 (shader.EnvironmentFilterMapUniform, 9)
            for i in 0 .. dec Constants.Render.LightMapsMaxForward do
                Gl.Uniform1 (shader.IrradianceMapsUniforms.[i], i + 10)
            for i in 0 .. dec Constants.Render.LightMapsMaxForward do
                Gl.Uniform1 (shader.EnvironmentFilterMapsUniforms.[i], i + 10 + Constants.Render.LightMapsMaxForward)
            for i in 0 .. dec Constants.Render.ShadowTexturesMax do
                Gl.Uniform1 (shader.ShadowTexturesUniforms.[i], i + 10 + Constants.Render.LightMapsMaxForward + Constants.Render.LightMapsMaxForward)
            for i in 0 .. dec Constants.Render.ShadowMapsMax do
                Gl.Uniform1 (shader.ShadowMapsUniforms.[i], i + 10 + Constants.Render.LightMapsMaxForward + Constants.Render.LightMapsMaxForward + Constants.Render.ShadowTexturesMax)
            Gl.Uniform3 (shader.LightMapOriginsUniform, lightMapOrigins)
            Gl.Uniform3 (shader.LightMapMinsUniform, lightMapMins)
            Gl.Uniform3 (shader.LightMapSizesUniform, lightMapSizes)
            Gl.Uniform3 (shader.LightMapAmbientColorsUniform, lightMapAmbientColors)
            Gl.Uniform1 (shader.LightMapAmbientBrightnessesUniform, lightMapAmbientBrightnesses)
            Gl.Uniform1 (shader.LightMapsCountUniform, lightMapsCount)
            Gl.Uniform3 (shader.LightOriginsUniform, lightOrigins)
            Gl.Uniform3 (shader.LightDirectionsUniform, lightDirections)
            Gl.Uniform3 (shader.LightColorsUniform, lightColors)
            Gl.Uniform1 (shader.LightBrightnessesUniform, lightBrightnesses)
            Gl.Uniform1 (shader.LightAttenuationLinearsUniform, lightAttenuationLinears)
            Gl.Uniform1 (shader.LightAttenuationQuadraticsUniform, lightAttenuationQuadratics)
            Gl.Uniform1 (shader.LightCutoffsUniform, lightCutoffs)
            Gl.Uniform1 (shader.LightTypesUniform, lightTypes)
            Gl.Uniform1 (shader.LightConeInnersUniform, lightConeInners)
            Gl.Uniform1 (shader.LightConeOutersUniform, lightConeOuters)
            Gl.Uniform1 (shader.LightShadowIndicesUniform, lightShadowIndices)
            Gl.Uniform1 (shader.LightsCountUniform, lightsCount)
            for i in 0 .. dec (min Constants.Render.ShadowTexturesMax shadowMatrices.Length) do
                Gl.UniformMatrix4 (shader.ShadowMatricesUniforms.[i], false, shadowMatrices.[i])
            Hl.Assert ()

            // setup textures
            Gl.ActiveTexture TextureUnit.Texture0
            Gl.BindTexture (TextureTarget.Texture2d, material.AlbedoTexture.TextureId)
            Gl.ActiveTexture TextureUnit.Texture1
            Gl.BindTexture (TextureTarget.Texture2d, material.RoughnessTexture.TextureId)
            Gl.ActiveTexture TextureUnit.Texture2
            Gl.BindTexture (TextureTarget.Texture2d, material.MetallicTexture.TextureId)
            Gl.ActiveTexture TextureUnit.Texture3
            Gl.BindTexture (TextureTarget.Texture2d, material.AmbientOcclusionTexture.TextureId)
            Gl.ActiveTexture TextureUnit.Texture4
            Gl.BindTexture (TextureTarget.Texture2d, material.EmissionTexture.TextureId)
            Gl.ActiveTexture TextureUnit.Texture5
            Gl.BindTexture (TextureTarget.Texture2d, material.NormalTexture.TextureId)
            Gl.ActiveTexture TextureUnit.Texture6
            Gl.BindTexture (TextureTarget.Texture2d, material.HeightTexture.TextureId)
            Gl.ActiveTexture TextureUnit.Texture7
            Gl.BindTexture (TextureTarget.Texture2d, brdfTexture.TextureId)
            Gl.ActiveTexture TextureUnit.Texture8
            Gl.BindTexture (TextureTarget.TextureCubeMap, irradianceMap.TextureId)
            Gl.ActiveTexture TextureUnit.Texture9
            Gl.BindTexture (TextureTarget.TextureCubeMap, environmentFilterMap.TextureId)
            for i in 0 .. dec (min irradianceMaps.Length Constants.Render.LightMapsMaxForward) do
                Gl.ActiveTexture (int TextureUnit.Texture0 + 10 + i |> Branchless.reinterpret)
                Gl.BindTexture (TextureTarget.TextureCubeMap, irradianceMaps.[i].TextureId)
            for i in 0 .. dec (min environmentFilterMaps.Length Constants.Render.LightMapsMaxForward) do
                Gl.ActiveTexture (int TextureUnit.Texture0 + 10 + i + Constants.Render.LightMapsMaxForward |> Branchless.reinterpret)
                Gl.BindTexture (TextureTarget.TextureCubeMap, environmentFilterMaps.[i].TextureId)
            for i in 0 .. dec (min shadowTextures.Length Constants.Render.ShadowTexturesMax) do
                Gl.ActiveTexture (int TextureUnit.Texture0 + 10 + i + Constants.Render.LightMapsMaxForward + Constants.Render.LightMapsMaxForward |> Branchless.reinterpret)
                Gl.BindTexture (TextureTarget.Texture2d, shadowTextures.[i].TextureId)
            for i in 0 .. dec (min shadowMaps.Length Constants.Render.ShadowMapsMax) do
                Gl.ActiveTexture (int TextureUnit.Texture0 + 10 + i + Constants.Render.LightMapsMaxForward + Constants.Render.LightMapsMaxForward + Constants.Render.ShadowTexturesMax |> Branchless.reinterpret)
                Gl.BindTexture (TextureTarget.TextureCubeMap, shadowMaps.[i].TextureId)
            Hl.Assert ()

            // update instance buffer
            let instanceFieldsPtr = GCHandle.Alloc (instanceFields, GCHandleType.Pinned)
            try Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.InstanceBuffer)
                Gl.BufferData (BufferTarget.ArrayBuffer, uint (surfacesCount * Constants.Render.InstanceFieldCount * sizeof<single>), instanceFieldsPtr.AddrOfPinnedObject (), BufferUsage.StreamDraw)
                Gl.BindBuffer (BufferTarget.ArrayBuffer, 0u)
                Hl.Assert ()
            finally instanceFieldsPtr.Free ()

            // setup geometry
            Gl.BindVertexArray geometry.PhysicallyBasedVao
            Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
            Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
            Hl.Assert ()

            // draw geometry
            Gl.DrawElementsInstanced (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0, surfacesCount)
            Hl.ReportDrawCall surfacesCount
            Hl.Assert ()

            // teardown geometry
            Gl.BindVertexArray 0u
            Hl.Assert ()

            // teardown textures
            Gl.ActiveTexture TextureUnit.Texture0
            Gl.BindTexture (TextureTarget.Texture2d, 0u)
            Gl.ActiveTexture TextureUnit.Texture1
            Gl.BindTexture (TextureTarget.Texture2d, 0u)
            Gl.ActiveTexture TextureUnit.Texture2
            Gl.BindTexture (TextureTarget.Texture2d, 0u)
            Gl.ActiveTexture TextureUnit.Texture3
            Gl.BindTexture (TextureTarget.Texture2d, 0u)
            Gl.ActiveTexture TextureUnit.Texture4
            Gl.BindTexture (TextureTarget.Texture2d, 0u)
            Gl.ActiveTexture TextureUnit.Texture5
            Gl.BindTexture (TextureTarget.Texture2d, 0u)
            Gl.ActiveTexture TextureUnit.Texture6
            Gl.BindTexture (TextureTarget.Texture2d, 0u)
            Gl.ActiveTexture TextureUnit.Texture7
            Gl.BindTexture (TextureTarget.TextureCubeMap, 0u)
            Gl.ActiveTexture TextureUnit.Texture8
            Gl.BindTexture (TextureTarget.TextureCubeMap, 0u)
            Gl.ActiveTexture TextureUnit.Texture9
            Gl.BindTexture (TextureTarget.Texture2d, 0u)
            for i in 0 .. dec (min irradianceMaps.Length Constants.Render.LightMapsMaxForward) do
                Gl.ActiveTexture (int TextureUnit.Texture0 + 10 + i |> Branchless.reinterpret)
                Gl.BindTexture (TextureTarget.TextureCubeMap, 0u)
            for i in 0 .. dec (min environmentFilterMaps.Length Constants.Render.LightMapsMaxForward) do
                Gl.ActiveTexture (int TextureUnit.Texture0 + 10 + i + Constants.Render.LightMapsMaxForward |> Branchless.reinterpret)
                Gl.BindTexture (TextureTarget.TextureCubeMap, 0u)
            for i in 0 .. dec (min shadowTextures.Length Constants.Render.ShadowTexturesMax) do
                Gl.ActiveTexture (int TextureUnit.Texture0 + 10 + i + Constants.Render.LightMapsMaxForward + Constants.Render.LightMapsMaxForward |> Branchless.reinterpret)
                Gl.BindTexture (TextureTarget.Texture2d, 0u)
            for i in 0 .. dec (min shadowMaps.Length Constants.Render.ShadowMapsMax) do
                Gl.ActiveTexture (int TextureUnit.Texture0 + 10 + i + Constants.Render.LightMapsMaxForward + Constants.Render.LightMapsMaxForward + Constants.Render.ShadowTexturesMax |> Branchless.reinterpret)
                Gl.BindTexture (TextureTarget.TextureCubeMap, 0u)
            Hl.Assert ()

            // teardown shader
            Gl.UseProgram 0u
            Hl.Assert ()

            // teardown state
            if not depthTest.IsAlwaysPassTest then
                Gl.DepthFunc DepthFunction.Less
                Gl.Disable EnableCap.DepthTest
            if blending then
                Gl.Disable EnableCap.Blend
                Gl.BlendFunc (BlendingFactor.One, BlendingFactor.Zero)
                Gl.BlendEquation BlendEquationMode.FuncAdd
            if not material.TwoSided then Gl.Disable EnableCap.CullFace

    let DrawPhysicallyBasedTerrain
        (view : single array,
         projection : single array,
         eyeCenter : Vector3,
         instanceFields : single array,
         lightShadowSamples : int,
         lightShadowBias : single,
         lightShadowSampleScalar : single,
         lightShadowExponent : single,
         lightShadowDensity : single,
         elementsCount : int,
         materials : PhysicallyBasedMaterial array,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedDeferredTerrainShader) =

        // setup state
        Gl.DepthFunc DepthFunction.Lequal
        Gl.Enable EnableCap.DepthTest
        Gl.Enable EnableCap.CullFace
        Hl.Assert ()

        // enforce layer limit
        let layersCount = min materials.Length Constants.Render.TerrainLayersMax
        
        // setup shader
        Gl.UseProgram shader.PhysicallyBasedShader
        Gl.UniformMatrix4 (shader.ViewUniform, false, view)
        Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
        Gl.Uniform3 (shader.EyeCenterUniform, eyeCenter.X, eyeCenter.Y, eyeCenter.Z)
        Gl.Uniform1 (shader.LightShadowSamplesUniform, lightShadowSamples)
        Gl.Uniform1 (shader.LightShadowBiasUniform, lightShadowBias)
        Gl.Uniform1 (shader.LightShadowSampleScalarUniform, lightShadowSampleScalar)
        Gl.Uniform1 (shader.LightShadowExponentUniform, lightShadowExponent)
        Gl.Uniform1 (shader.LightShadowDensityUniform, lightShadowDensity)
        Gl.Uniform1 (shader.LayersCountUniform, layersCount)
        for i in 0 .. dec Constants.Render.TerrainLayersMax do
            Gl.Uniform1 (shader.AlbedoTexturesUniforms.[i], i)
        for i in 0 .. dec Constants.Render.TerrainLayersMax do
            Gl.Uniform1 (shader.RoughnessTexturesUniforms.[i], i + Constants.Render.TerrainLayersMax)
        for i in 0 .. dec Constants.Render.TerrainLayersMax do
            Gl.Uniform1 (shader.AmbientOcclusionTexturesUniforms.[i], i + Constants.Render.TerrainLayersMax * 2)
        for i in 0 .. dec Constants.Render.TerrainLayersMax do
            Gl.Uniform1 (shader.NormalTexturesUniforms.[i], i + Constants.Render.TerrainLayersMax * 3)
        for i in 0 .. dec Constants.Render.TerrainLayersMax do
            Gl.Uniform1 (shader.HeightTexturesUniforms.[i], i + Constants.Render.TerrainLayersMax * 4)
        Hl.Assert ()

        // setup textures
        for i in 0 .. dec layersCount do
            Gl.ActiveTexture (int TextureUnit.Texture0 + i |> Branchless.reinterpret)
            Gl.BindTexture (TextureTarget.Texture2d, materials.[i].AlbedoTexture.TextureId)
        for i in 0 .. dec layersCount do
            Gl.ActiveTexture (int TextureUnit.Texture0 + i + Constants.Render.TerrainLayersMax |> Branchless.reinterpret)
            Gl.BindTexture (TextureTarget.Texture2d, materials.[i].RoughnessTexture.TextureId)
        for i in 0 .. dec layersCount do
            Gl.ActiveTexture (int TextureUnit.Texture0 + i + Constants.Render.TerrainLayersMax * 2 |> Branchless.reinterpret)
            Gl.BindTexture (TextureTarget.Texture2d, materials.[i].AmbientOcclusionTexture.TextureId)
        for i in 0 .. dec layersCount do
            Gl.ActiveTexture (int TextureUnit.Texture0 + i + Constants.Render.TerrainLayersMax * 3 |> Branchless.reinterpret)
            Gl.BindTexture (TextureTarget.Texture2d, materials.[i].NormalTexture.TextureId)
        for i in 0 .. dec layersCount do
            Gl.ActiveTexture (int TextureUnit.Texture0 + i + Constants.Render.TerrainLayersMax * 4 |> Branchless.reinterpret)
            Gl.BindTexture (TextureTarget.Texture2d, materials.[i].HeightTexture.TextureId)
        Hl.Assert ()

        // update instance buffer
        let instanceFieldsPtr = GCHandle.Alloc (instanceFields, GCHandleType.Pinned)
        try Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.InstanceBuffer)
            Gl.BufferData (BufferTarget.ArrayBuffer, uint (Constants.Render.InstanceFieldCount * sizeof<single>), instanceFieldsPtr.AddrOfPinnedObject (), BufferUsage.StreamDraw)
            Gl.BindBuffer (BufferTarget.ArrayBuffer, 0u)
            Hl.Assert ()
        finally instanceFieldsPtr.Free ()

        // setup geometry
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, elementsCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown textures
        for i in 0 .. dec layersCount * 5 do
            Gl.ActiveTexture (int TextureUnit.Texture0 + i |> Branchless.reinterpret)
            Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()
        
        // teardown shader
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown state
        Gl.DepthFunc DepthFunction.Less
        Gl.Disable EnableCap.DepthTest
        Gl.Disable EnableCap.CullFace

    /// Draw the light mapping pass of a deferred physically-based surface.
    let DrawPhysicallyBasedDeferredLightMappingSurface
        (positionTexture : Texture.Texture,
         normalPlusTexture : Texture.Texture,
         lightMapOrigins : single array,
         lightMapMins : single array,
         lightMapSizes : single array,
         lightMapsCount : int,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedDeferredLightMappingShader) =

        // setup shader
        Gl.UseProgram shader.PhysicallyBasedDeferredLightMappingShader
        Gl.Uniform1 (shader.PositionTextureUniform, 0)
        Gl.Uniform1 (shader.NormalPlusTextureUniform, 1)
        Gl.Uniform3 (shader.LightMapOriginsUniform, lightMapOrigins)
        Gl.Uniform3 (shader.LightMapMinsUniform, lightMapMins)
        Gl.Uniform3 (shader.LightMapSizesUniform, lightMapSizes)
        Gl.Uniform1 (shader.LightMapsCountUniform, lightMapsCount)
        Hl.Assert ()

        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, positionTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture1
        Gl.BindTexture (TextureTarget.Texture2d, normalPlusTexture.TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.ValidateProgram shader.PhysicallyBasedDeferredLightMappingShader
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.ActiveTexture TextureUnit.Texture1
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u

    /// Draw the ambient pass of a deferred physically-based surface.
    let DrawPhysicallyBasedDeferredAmbientSurface
        (positionTexture : Texture.Texture,
         lightMappingTexture : Texture.Texture,
         lightMapAmbientColor : single array,
         lightMapAmbientBrightness : single,
         lightMapAmbientColors : single array,
         lightMapAmbientBrightnesses : single array,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedDeferredAmbientShader) =

        // setup shader
        Gl.UseProgram shader.PhysicallyBasedDeferredAmbientShader
        Gl.Uniform1 (shader.PositionTextureUniform, 0)
        Gl.Uniform1 (shader.LightMappingTextureUniform, 1)
        Gl.Uniform3 (shader.LightMapAmbientColorUniform, lightMapAmbientColor)
        Gl.Uniform1 (shader.LightMapAmbientBrightnessUniform, lightMapAmbientBrightness)
        Gl.Uniform3 (shader.LightMapAmbientColorsUniform, lightMapAmbientColors)
        Gl.Uniform1 (shader.LightMapAmbientBrightnessesUniform, lightMapAmbientBrightnesses)
        Hl.Assert ()

        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, positionTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture1
        Gl.BindTexture (TextureTarget.Texture2d, lightMappingTexture.TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.ActiveTexture TextureUnit.Texture1
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u

    /// Draw the irradiance pass of a deferred physically-based surface.
    let DrawPhysicallyBasedDeferredIrradianceSurface
        (positionTexture : Texture.Texture,
         normalPlusTexture : Texture.Texture,
         lightMappingTexture : Texture.Texture,
         irradianceMap : Texture.Texture,
         irradianceMaps : Texture.Texture array,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedDeferredIrradianceShader) =

        // setup shader
        Gl.UseProgram shader.PhysicallyBasedDeferredIrradianceShader
        Gl.Uniform1 (shader.PositionTextureUniform, 0)
        Gl.Uniform1 (shader.NormalPlusTextureUniform, 1)
        Gl.Uniform1 (shader.LightMappingTextureUniform, 2)
        Gl.Uniform1 (shader.IrradianceMapUniform, 3)
        for i in 0 .. dec Constants.Render.LightMapsMaxDeferred do
            Gl.Uniform1 (shader.IrradianceMapsUniforms.[i], 4 + i)
        Hl.Assert ()

        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, positionTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture1
        Gl.BindTexture (TextureTarget.Texture2d, normalPlusTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture2
        Gl.BindTexture (TextureTarget.Texture2d, lightMappingTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture3
        Gl.BindTexture (TextureTarget.TextureCubeMap, irradianceMap.TextureId)
        for i in 0 .. dec Constants.Render.LightMapsMaxDeferred do
            Gl.ActiveTexture (int TextureUnit.Texture0 + 4 + i |> Branchless.reinterpret)
            Gl.BindTexture (TextureTarget.TextureCubeMap, irradianceMaps.[i].TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.ActiveTexture TextureUnit.Texture1
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.ActiveTexture TextureUnit.Texture2
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.ActiveTexture TextureUnit.Texture3
        Gl.BindTexture (TextureTarget.TextureCubeMap, 0u)
        for i in 0 .. dec Constants.Render.LightMapsMaxDeferred do
            Gl.ActiveTexture (int TextureUnit.Texture0 + 4 + i |> Branchless.reinterpret)
            Gl.BindTexture (TextureTarget.TextureCubeMap, 0u)
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u

    /// Draw the environment filter pass of a deferred physically-based surface.
    let DrawPhysicallyBasedDeferredEnvironmentFilterSurface
        (eyeCenter : Vector3,
         positionTexture : Texture.Texture,
         materialTexture : Texture.Texture,
         normalPlusTexture : Texture.Texture,
         lightMappingTexture : Texture.Texture,
         environmentFilterMap : Texture.Texture,
         environmentFilterMaps : Texture.Texture array,
         lightMapOrigins : single array,
         lightMapMins : single array,
         lightMapSizes : single array,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedDeferredEnvironmentFilterShader) =

        // setup shader
        Gl.UseProgram shader.PhysicallyBasedDeferredEnvironmentFilterShader
        Gl.Uniform3 (shader.EyeCenterUniform, eyeCenter.X, eyeCenter.Y, eyeCenter.Z)
        Gl.Uniform1 (shader.PositionTextureUniform, 0)
        Gl.Uniform1 (shader.MaterialTextureUniform, 1)
        Gl.Uniform1 (shader.NormalPlusTextureUniform, 2)
        Gl.Uniform1 (shader.LightMappingTextureUniform, 3)
        Gl.Uniform1 (shader.EnvironmentFilterMapUniform, 4)
        for i in 0 .. dec Constants.Render.LightMapsMaxDeferred do
            Gl.Uniform1 (shader.EnvironmentFilterMapsUniforms.[i], 5 + i)
        Gl.Uniform3 (shader.LightMapOriginsUniform, lightMapOrigins)
        Gl.Uniform3 (shader.LightMapMinsUniform, lightMapMins)
        Gl.Uniform3 (shader.LightMapSizesUniform, lightMapSizes)
        Hl.Assert ()

        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, positionTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture1
        Gl.BindTexture (TextureTarget.Texture2d, materialTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture2
        Gl.BindTexture (TextureTarget.Texture2d, normalPlusTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture3
        Gl.BindTexture (TextureTarget.Texture2d, lightMappingTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture4
        Gl.BindTexture (TextureTarget.TextureCubeMap, environmentFilterMap.TextureId)
        for i in 0 .. dec Constants.Render.LightMapsMaxDeferred do
            Gl.ActiveTexture (int TextureUnit.Texture0 + 5 + i |> Branchless.reinterpret)
            Gl.BindTexture (TextureTarget.TextureCubeMap, environmentFilterMaps.[i].TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.ActiveTexture TextureUnit.Texture1
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.ActiveTexture TextureUnit.Texture2
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.ActiveTexture TextureUnit.Texture3
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.ActiveTexture TextureUnit.Texture4
        Gl.BindTexture (TextureTarget.TextureCubeMap, 0u)
        for i in 0 .. dec Constants.Render.LightMapsMaxDeferred do
            Gl.ActiveTexture (int TextureUnit.Texture0 + 5 + i |> Branchless.reinterpret)
            Gl.BindTexture (TextureTarget.TextureCubeMap, 0u)
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u

    /// Draw the ssao pass of a deferred physically-based surface.
    let DrawPhysicallyBasedDeferredSsaoSurface
        (view : single array,
         projection : single array,
         positionTexture : Texture.Texture,
         normalPlusTexture : Texture.Texture,
         ssaoResolution : int array,
         ssaoIntensity : single,
         ssaoBias : single,
         ssaoRadius : single,
         ssaoDistanceMax : single,
         ssaoSampleCount : int,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedDeferredSsaoShader) =

        // setup shader
        Gl.UseProgram shader.PhysicallyBasedDeferredSsaoShader
        Gl.UniformMatrix4 (shader.ViewUniform, false, view)
        Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
        Gl.Uniform1 (shader.PositionTextureUniform, 0)
        Gl.Uniform1 (shader.NormalPlusTextureUniform, 1)
        Gl.Uniform2 (shader.SsaoResolution, ssaoResolution)
        Gl.Uniform1 (shader.SsaoIntensity, ssaoIntensity)
        Gl.Uniform1 (shader.SsaoBias, ssaoBias)
        Gl.Uniform1 (shader.SsaoRadius, ssaoRadius)
        Gl.Uniform1 (shader.SsaoDistanceMax, ssaoDistanceMax)
        Gl.Uniform1 (shader.SsaoSampleCount, ssaoSampleCount)
        Hl.Assert ()

        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, positionTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture1
        Gl.BindTexture (TextureTarget.Texture2d, normalPlusTexture.TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.ActiveTexture TextureUnit.Texture1
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u

    /// Draw the lighting pass of a deferred physically-based surface.
    let DrawPhysicallyBasedDeferredLightingSurface
        (eyeCenter : Vector3,
         view : single array,
         projection : single array,
         lightCutoffMargin : single,
         lightShadowSamples : int,
         lightShadowBias : single,
         lightShadowSampleScalar : single,
         lightShadowExponent : single,
         lightShadowDensity : single,
         ssvfEnabled : int,
         ssvfSteps : int,
         ssvfAsymmetry : single,
         ssvfIntensity : single,
         ssrEnabled : int,
         ssrDetail : single,
         ssrRefinementsMax : int,
         ssrRayThickness : single,
         ssrTowardEyeCutoff : single,
         ssrDepthCutoff : single,
         ssrDepthCutoffMargin : single,
         ssrDistanceCutoff : single,
         ssrDistanceCutoffMargin : single,
         ssrRoughnessCutoff : single,
         ssrRoughnessCutoffMargin : single,
         ssrSlopeCutoff : single,
         ssrSlopeCutoffMargin : single,
         ssrEdgeHorizontalMargin : single,
         ssrEdgeVerticalMargin : single,
         ssrLightColor : single array,
         ssrLightBrightness : single,
         positionTexture : Texture.Texture,
         albedoTexture : Texture.Texture,
         materialTexture : Texture.Texture,
         normalPlusTexture : Texture.Texture,
         brdfTexture : Texture.Texture,
         ambientTexture : Texture.Texture,
         irradianceTexture : Texture.Texture,
         environmentFilterTexture : Texture.Texture,
         ssaoTexture : Texture.Texture,
         shadowTextures : Texture.Texture array,
         shadowMaps : Texture.Texture array,
         lightOrigins : single array,
         lightDirections : single array,
         lightColors : single array,
         lightBrightnesses : single array,
         lightAttenuationLinears : single array,
         lightAttenuationQuadratics : single array,
         lightCutoffs : single array,
         lightTypes : int array,
         lightConeInners : single array,
         lightConeOuters : single array,
         lightShadowIndices : int array,
         lightsCount : int,
         shadowMatrices : single array array,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedDeferredLightingShader) =

        // setup shader
        Gl.UseProgram shader.PhysicallyBasedDeferredLightingShader
        Gl.Uniform3 (shader.EyeCenterUniform, eyeCenter.X, eyeCenter.Y, eyeCenter.Z)
        Gl.UniformMatrix4 (shader.ViewUniform, false, view)
        Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
        Gl.Uniform1 (shader.LightCutoffMarginUniform, lightCutoffMargin)
        Gl.Uniform1 (shader.LightShadowSamplesUniform, lightShadowSamples)
        Gl.Uniform1 (shader.LightShadowBiasUniform, lightShadowBias)
        Gl.Uniform1 (shader.LightShadowSampleScalarUniform, lightShadowSampleScalar)
        Gl.Uniform1 (shader.LightShadowExponentUniform, lightShadowExponent)
        Gl.Uniform1 (shader.LightShadowDensityUniform, lightShadowDensity)
        Gl.Uniform1 (shader.SsvfEnabledUniform, ssvfEnabled)
        Gl.Uniform1 (shader.SsvfStepsUniform, ssvfSteps)
        Gl.Uniform1 (shader.SsvfAsymmetryUniform, ssvfAsymmetry)
        Gl.Uniform1 (shader.SsvfIntensityUniform, ssvfIntensity)
        Gl.Uniform1 (shader.SsrEnabledUniform, ssrEnabled)
        Gl.Uniform1 (shader.SsrDetailUniform, ssrDetail)
        Gl.Uniform1 (shader.SsrRefinementsMaxUniform, ssrRefinementsMax)
        Gl.Uniform1 (shader.SsrRayThicknessUniform, ssrRayThickness)
        Gl.Uniform1 (shader.SsrTowardEyeCutoffUniform, ssrTowardEyeCutoff)
        Gl.Uniform1 (shader.SsrDepthCutoffUniform, ssrDepthCutoff)
        Gl.Uniform1 (shader.SsrDepthCutoffMarginUniform, ssrDepthCutoffMargin)
        Gl.Uniform1 (shader.SsrDistanceCutoffUniform, ssrDistanceCutoff)
        Gl.Uniform1 (shader.SsrDistanceCutoffMarginUniform, ssrDistanceCutoffMargin)
        Gl.Uniform1 (shader.SsrRoughnessCutoffUniform, ssrRoughnessCutoff)
        Gl.Uniform1 (shader.SsrRoughnessCutoffMarginUniform, ssrRoughnessCutoffMargin)
        Gl.Uniform1 (shader.SsrSlopeCutoffUniform, ssrSlopeCutoff)
        Gl.Uniform1 (shader.SsrSlopeCutoffMarginUniform, ssrSlopeCutoffMargin)
        Gl.Uniform1 (shader.SsrEdgeHorizontalMarginUniform, ssrEdgeHorizontalMargin)
        Gl.Uniform1 (shader.SsrEdgeVerticalMarginUniform, ssrEdgeVerticalMargin)
        Gl.Uniform3 (shader.SsrLightColorUniform, ssrLightColor)
        Gl.Uniform1 (shader.SsrLightBrightnessUniform, ssrLightBrightness)
        Gl.Uniform1 (shader.PositionTextureUniform, 0)
        Gl.Uniform1 (shader.AlbedoTextureUniform, 1)
        Gl.Uniform1 (shader.MaterialTextureUniform, 2)
        Gl.Uniform1 (shader.NormalPlusTextureUniform, 3)
        Gl.Uniform1 (shader.BrdfTextureUniform, 4)
        Gl.Uniform1 (shader.AmbientTextureUniform, 5)
        Gl.Uniform1 (shader.IrradianceTextureUniform, 6)
        Gl.Uniform1 (shader.EnvironmentFilterTextureUniform, 7)
        Gl.Uniform1 (shader.SsaoTextureUniform, 8)
        for i in 0 .. dec Constants.Render.ShadowTexturesMax do
            Gl.Uniform1 (shader.ShadowTexturesUniforms.[i], i + 9)
        for i in 0 .. dec Constants.Render.ShadowMapsMax do
            Gl.Uniform1 (shader.ShadowMapsUniforms.[i], i + 9 + Constants.Render.ShadowTexturesMax)
        Gl.Uniform3 (shader.LightOriginsUniform, lightOrigins)
        Gl.Uniform3 (shader.LightDirectionsUniform, lightDirections)
        Gl.Uniform3 (shader.LightColorsUniform, lightColors)
        Gl.Uniform1 (shader.LightBrightnessesUniform, lightBrightnesses)
        Gl.Uniform1 (shader.LightAttenuationLinearsUniform, lightAttenuationLinears)
        Gl.Uniform1 (shader.LightAttenuationQuadraticsUniform, lightAttenuationQuadratics)
        Gl.Uniform1 (shader.LightCutoffsUniform, lightCutoffs)
        Gl.Uniform1 (shader.LightTypesUniform, lightTypes)
        Gl.Uniform1 (shader.LightConeInnersUniform, lightConeInners)
        Gl.Uniform1 (shader.LightConeOutersUniform, lightConeOuters)
        Gl.Uniform1 (shader.LightShadowIndicesUniform, lightShadowIndices)
        Gl.Uniform1 (shader.LightsCountUniform, lightsCount)
        for i in 0 .. dec (min Constants.Render.ShadowTexturesMax shadowMatrices.Length) do
            Gl.UniformMatrix4 (shader.ShadowMatricesUniforms.[i], false, shadowMatrices.[i])
        Hl.Assert ()

        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, positionTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture1
        Gl.BindTexture (TextureTarget.Texture2d, albedoTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture2
        Gl.BindTexture (TextureTarget.Texture2d, materialTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture3
        Gl.BindTexture (TextureTarget.Texture2d, normalPlusTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture4
        Gl.BindTexture (TextureTarget.Texture2d, brdfTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture5
        Gl.BindTexture (TextureTarget.Texture2d, ambientTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture6
        Gl.BindTexture (TextureTarget.Texture2d, irradianceTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture7
        Gl.BindTexture (TextureTarget.Texture2d, environmentFilterTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture8
        Gl.BindTexture (TextureTarget.Texture2d, ssaoTexture.TextureId)
        for i in 0 .. dec (min shadowTextures.Length Constants.Render.ShadowTexturesMax) do
            Gl.ActiveTexture (int TextureUnit.Texture0 + 9 + i |> Branchless.reinterpret)
            Gl.BindTexture (TextureTarget.Texture2d, shadowTextures.[i].TextureId)
        for i in 0 .. dec (min shadowMaps.Length Constants.Render.ShadowMapsMax) do
            Gl.ActiveTexture (int TextureUnit.Texture0 + 9 + i + Constants.Render.ShadowTexturesMax |> Branchless.reinterpret)
            Gl.BindTexture (TextureTarget.TextureCubeMap, shadowMaps.[i].TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.ActiveTexture TextureUnit.Texture1
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.ActiveTexture TextureUnit.Texture2
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.ActiveTexture TextureUnit.Texture3
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.ActiveTexture TextureUnit.Texture4
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.ActiveTexture TextureUnit.Texture5
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.ActiveTexture TextureUnit.Texture6
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.ActiveTexture TextureUnit.Texture7
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.ActiveTexture TextureUnit.Texture8
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        for i in 0 .. dec (min shadowTextures.Length Constants.Render.ShadowTexturesMax) do
            Gl.ActiveTexture (int TextureUnit.Texture0 + 9 + i |> Branchless.reinterpret)
            Gl.BindTexture (TextureTarget.Texture2d, 0u)
        for i in 0 .. dec (min shadowMaps.Length Constants.Render.ShadowMapsMax) do
            Gl.ActiveTexture (int TextureUnit.Texture0 + 9 + i + Constants.Render.ShadowTexturesMax |> Branchless.reinterpret)
            Gl.BindTexture (TextureTarget.TextureCubeMap, 0u)
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u

    /// Draw the bilateral up-sample pass of a deferred physically-based surface.
    let DrawPhysicallyBasedDeferredCompositionSurface
        (colorTexture : Texture.Texture,
         fogAccumTexture : Texture.Texture,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedDeferredCompositionShader) =

        // setup shader
        Gl.UseProgram shader.PhysicallyBasedDeferredCompositionShader
        Gl.Uniform1 (shader.ColorTextureUniform, 0)
        Gl.Uniform1 (shader.FogAccumTextureUniform, 1)
        Hl.Assert ()

        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, colorTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture1
        Gl.BindTexture (TextureTarget.Texture2d, fogAccumTexture.TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.ActiveTexture TextureUnit.Texture1
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u

    /// Destroy physically-based geometry resources.
    let DestroyPhysicallyBasedGeometry geometry =
        Gl.BindVertexArray geometry.PhysicallyBasedVao
        Gl.DeleteBuffers [|geometry.VertexBuffer|]
        Gl.DeleteBuffers [|geometry.InstanceBuffer|]
        Gl.DeleteBuffers [|geometry.IndexBuffer|]
        Gl.BindVertexArray 0u
        Gl.DeleteVertexArrays [|geometry.PhysicallyBasedVao|]

    /// Destroy physically-based model resources.
    let DestroyPhysicallyBasedModel (model : PhysicallyBasedModel) =
        let surfacesUnique = // NOTE: models deduplicate underlying geometry, so we make sure to only release each vao once.
            model.Surfaces |>
            Array.groupBy (fun surface -> surface.PhysicallyBasedGeometry.PhysicallyBasedVao) |>
            Array.map snd |>
            Array.map Array.head
        for surface in surfacesUnique do
            DestroyPhysicallyBasedGeometry surface.PhysicallyBasedGeometry

    /// Memoizes physically-based scene loads.
    type PhysicallyBasedSceneClient () =

        let scenes = Dictionary HashIdentity.Structural

        /// Memoized scenes.
        member this.Scenes = scenes

        /// Attempt to create physically-based model from a model file with assimp.
        /// Thread-safe if renderable = false.
        member this.TryCreatePhysicallyBasedModel (renderable, filePath, defaultMaterial, textureClient) =

            // attempt to memoize scene
            let sceneEir =
                match scenes.TryGetValue filePath with
                | (false, _) ->

                    // attempt to create scene
                    use assimp = new Assimp.AssimpContext ()
                    try let scene = assimp.ImportFile (filePath, Constants.Assimp.PostProcessSteps)
                        scene.IndexDatasToMetadata () // avoid polluting memory with face data
                        scenes.[filePath] <- scene
                        Right scene
                    with exn ->
                        Left ("Could not load assimp scene from '" + filePath + "' due to: " + scstring exn)

                // already exists
                | (true, scene) -> Right scene

            // attempt to import from assimp scene
            match sceneEir with
            | Right scene ->
                let dirPath = PathF.GetDirectoryName filePath
                match TryCreatePhysicallyBasedMaterials (renderable, dirPath, defaultMaterial, textureClient, scene) with
                | Right materials ->
                    let animated = scene.Animations.Count <> 0
                    let geometries =
                        if animated
                        then CreatePhysicallyBasedAnimatedGeometries (renderable, scene)
                        else CreatePhysicallyBasedStaticGeometries (renderable, scene)

                    // collect light nodes
                    let lightNodes =
                        [|for i in 0 .. dec scene.LightCount do
                            let light = scene.Lights.[i]
                            let node = scene.RootNode.FindNode light.Name
                            yield (light, node)|]

                    // construct bounds and hierarchy
                    // TODO: P1: consider sanitizing incoming names. Corrupted or incompatible names cause subtle hierarchy bugs.
                    let lightProbes = SList.make ()
                    let lights = SList.make ()
                    let surfaces = SList.make ()
                    let mutable bounds = box3Zero
                    let hierarchy =
                        scene.RootNode.Map ([||], m4Identity, fun node names transform ->
                            [|// collect node
                              yield PhysicallyBasedNode names

                              // attempt to collect light probe
                              let lastNameLower = Array.last(names).ToLowerInvariant()
                              if lastNameLower.Contains "probe" && not (lastNameLower.Contains "probes") then
                                let names = Array.append names [|"LightProbe"|]
                                let lightProbeOrigin = transform.Translation
                                let lightProbeBounds =
                                    box3
                                        (v3Dup Constants.Render.LightProbeSizeDefault * -0.5f + lightProbeOrigin)
                                        (v3Dup Constants.Render.LightProbeSizeDefault)
                                let lightProbe =
                                    { LightProbeNames = names
                                      LightProbeMatrixIsIdentity = transform.IsIdentity
                                      LightProbeMatrix = transform
                                      LightProbeBounds = lightProbeBounds }
                                lightProbes.Add lightProbe
                                yield PhysicallyBasedLightProbe lightProbe

                              // collect light
                              // NOTE: this is an n^2 algorithm to deal with nodes having no light information
                              for i in 0 .. dec lightNodes.Length do
                                let (light, lightNode) = lightNodes.[i]
                                if lightNode = node then
                                    let names = Array.append names [|"Light" + if i > 0 then string i else ""|]
                                    let lightMatrix = Assimp.ExportMatrix node.TransformWorld
                                    let color = color (min 1.0f light.ColorDiffuse.R) (min 1.0f light.ColorDiffuse.G) (min 1.0f light.ColorDiffuse.B) 1.0f
                                    let lightType =
                                        match light.LightType with
                                        | Assimp.LightSourceType.Spot -> SpotLight (light.AngleInnerCone, light.AngleOuterCone)
                                        | _ -> PointLight // default to point light
                                    let physicallyBasedLight =
                                        { LightNames = names
                                          LightMatrixIsIdentity = lightMatrix.IsIdentity
                                          LightMatrix = lightMatrix
                                          LightColor = color
                                          LightBrightness = Constants.Render.BrightnessDefault // TODO: figure out if we can populate this properly.
                                          LightAttenuationLinear = if light.AttenuationLinear > 0.0f then light.AttenuationLinear else Constants.Render.AttenuationLinearDefault
                                          LightAttenuationQuadratic = if light.AttenuationQuadratic > 0.0f then light.AttenuationQuadratic else Constants.Render.AttenuationQuadraticDefault
                                          LightCutoff = Constants.Render.LightCutoffDefault // TODO: figure out if we can populate this properly.
                                          LightType = lightType
                                          LightDesireShadows = false }
                                    lights.Add physicallyBasedLight
                                    yield PhysicallyBasedLight physicallyBasedLight

                              // collect surfaces
                              for i in 0 .. dec node.MeshIndices.Count do
                                let names = Array.append names [|"Geometry" + if i > 0 then string (inc i) else ""|]
                                let meshIndex = node.MeshIndices.[i]
                                let materialIndex = scene.Meshes.[meshIndex].MaterialIndex
                                let (properties, material) = materials.[materialIndex]
                                let geometry = geometries.[meshIndex]
                                let surface = PhysicallyBasedSurface.make names transform geometry.Bounds properties material materialIndex node geometry
                                bounds <- bounds.Combine (geometry.Bounds.Transform transform)
                                surfaces.Add surface
                                yield PhysicallyBasedSurface surface|] |>
                            TreeNode)

                    // fin
                    Right
                        { Animated = animated
                          Bounds = bounds
                          LightProbes = Array.ofSeq lightProbes
                          Lights = Array.ofSeq lights
                          Surfaces = Array.ofSeq surfaces
                          SceneOpt = Some scene
                          PhysicallyBasedHierarchy = hierarchy }

                // error
                | Left error -> Left ("Could not load materials for static model in file name '" + filePath + "' due to: " + error)
            | Left error -> Left error