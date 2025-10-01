// Nu Game Engine.
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
          ShadowCascadeArrayBuffersArray : (OpenGL.Texture.Texture * uint * uint) array
          ShadowCascadeFilterBuffersArray : (OpenGL.Texture.Texture * uint * uint) array
          GeometryBuffers : OpenGL.Texture.Texture * OpenGL.Texture.Texture * OpenGL.Texture.Texture * OpenGL.Texture.Texture * OpenGL.Texture.Texture * OpenGL.Texture.Texture * uint * uint
          LightMappingBuffers : OpenGL.Texture.Texture * uint * uint
          AmbientBuffers : OpenGL.Texture.Texture * uint * uint
          IrradianceBuffers : OpenGL.Texture.Texture * uint * uint
          EnvironmentFilterBuffers : OpenGL.Texture.Texture * uint * uint
          SsaoBuffersUnfiltered : OpenGL.Texture.Texture * uint * uint
          SsaoBuffersFiltered : OpenGL.Texture.Texture * uint * uint
          LightingBuffers : OpenGL.Texture.Texture * OpenGL.Texture.Texture * uint * uint
          ColoringBuffers : OpenGL.Texture.Texture * OpenGL.Texture.Texture * uint * uint
          SpecularScreenDownSampleBuffers : OpenGL.Texture.Texture * OpenGL.Texture.Texture * uint * uint
          SpecularScreenUpSampleBuffers : OpenGL.Texture.Texture * uint * uint
          FogAccumDownSampleBuffers : OpenGL.Texture.Texture * OpenGL.Texture.Texture * uint * uint
          FogAccumUpSampleBuffers : OpenGL.Texture.Texture * uint * uint
          CompositionBuffers : OpenGL.Texture.Texture * uint * uint
          BloomExtractBuffers : OpenGL.Texture.Texture * uint * uint
          BloomSampleBuffers : OpenGL.Texture.Texture array * uint * uint
          BloomApplyBuffers : OpenGL.Texture.Texture * uint * uint
          Filter0Buffers : OpenGL.Texture.Texture * uint * uint
          Filter1Buffers : OpenGL.Texture.Texture * uint * uint
          Filter2Buffers : OpenGL.Texture.Texture * uint * uint
          PresentationBuffers : OpenGL.Texture.Texture * uint * uint }

    /// Describes the configurable properties of a physically-based material.
    type PhysicallyBasedMaterialProperties =
        { Albedo : Color
          Roughness : single
          Metallic : single
          AmbientOcclusion : single
          Emission : single
          Height : single
          IgnoreLightMaps : bool
          OpaqueDistance : single
          FinenessOffset : single
          ScatterType : ScatterType
          SpecularScalar : single
          RefractiveIndex : single }

        /// The empty material properties.
        static member empty =
            { Albedo = Color.Zero
              Roughness = 0.0f
              Metallic = 0.0f
              AmbientOcclusion = 0.0f
              Emission = 0.0f
              Height = 0.0f
              IgnoreLightMaps = false
              OpaqueDistance = 0.0f
              FinenessOffset = 0.0f
              ScatterType = NoScatter
              SpecularScalar = 0.0f
              RefractiveIndex = 0.0f }

    /// Describes a physically-based material.
    type PhysicallyBasedMaterial =
        { AlbedoTexture : Texture.Texture
          RoughnessTexture : Texture.Texture
          MetallicTexture : Texture.Texture
          AmbientOcclusionTexture : Texture.Texture
          EmissionTexture : Texture.Texture
          NormalTexture : Texture.Texture
          HeightTexture : Texture.Texture
          SubdermalTexture : Texture.Texture
          FinenessTexture : Texture.Texture
          ScatterTexture : Texture.Texture
          TwoSided : bool
          Clipped : bool
          Names : string }

        /// The empty material.
        static member empty =
            { AlbedoTexture = Texture.EmptyTexture
              RoughnessTexture = Texture.EmptyTexture
              MetallicTexture = Texture.EmptyTexture
              AmbientOcclusionTexture = Texture.EmptyTexture
              EmissionTexture = Texture.EmptyTexture
              NormalTexture = Texture.EmptyTexture
              HeightTexture = Texture.EmptyTexture
              SubdermalTexture = Texture.EmptyTexture
              FinenessTexture = Texture.EmptyTexture
              ScatterTexture = Texture.EmptyTexture
              TwoSided = false
              Clipped = false
              Names = "" }

    /// Describes some physically-based geometry that's loaded into VRAM.
    type PhysicallyBasedGeometry =
        { Bounds : Box3
          PrimitiveType : PrimitiveType
          ElementCount : int
          Vertices : Vector3 array
          Indices : int array
          mutable TrianglesCached : Vector3 array option
          VertexBuffer : uint
          InstanceBuffer : uint
          IndexBuffer : uint }

        /// Lazily access triangles, building them from Vertices and Indices if needed.
        member this.Triangles =
            match this.TrianglesCached with
            | None ->
                assert (this.PrimitiveType = PrimitiveType.Triangles) // should hold since we use Assimp.PostProcessSteps.Triangulate
                let triangles =
                    [|for points in Array.chunkBySize 3 this.Indices do
                        this.Vertices.[points.[0]]
                        this.Vertices.[points.[1]]
                        this.Vertices.[points.[2]]|]
                this.TrianglesCached <- Some triangles
                triangles
            | Some triangles -> triangles

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

        static member extractFinenessOffset finenessOffsetDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.FinenessOffsetOpt with
            | ValueNone ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    ValueOption.defaultValue finenessOffsetDefault material.FinenessOffsetOpt
                | Some _ | None -> finenessOffsetDefault
            | ValueSome finenessOffset -> finenessOffset

        static member extractScatterType scatterTypeDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.ScatterTypeOpt with
            | ValueNone ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    ValueOption.defaultValue scatterTypeDefault material.ScatterTypeOpt
                | Some _ | None -> scatterTypeDefault
            | ValueSome scatterType -> scatterType

        static member extractSpecularScalar specularScalarDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.SpecularScalarOpt with
            | ValueNone ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    ValueOption.defaultValue specularScalarDefault material.SpecularScalarOpt
                | Some _ | None -> specularScalarDefault
            | ValueSome specularScalar -> specularScalar

        static member extractRefractiveIndex refractiveIndexDefault (sceneOpt : Assimp.Scene option) surface =
            match surface.SurfaceNode.RefractiveIndexOpt with
            | ValueNone ->
                match sceneOpt with
                | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                    let material = scene.Materials.[surface.SurfaceMaterialIndex]
                    ValueOption.defaultValue refractiveIndexDefault material.RefractiveIndexOpt
                | Some _ | None -> refractiveIndexDefault
            | ValueSome refractiveIndex -> refractiveIndex

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
            left.SurfaceMaterial.SubdermalTexture = right.SurfaceMaterial.SubdermalTexture &&
            left.SurfaceMaterial.FinenessTexture = right.SurfaceMaterial.FinenessTexture &&
            left.SurfaceMaterial.ScatterTexture = right.SurfaceMaterial.ScatterTexture &&
            left.SurfaceMaterial.TwoSided = right.SurfaceMaterial.TwoSided &&
            left.SurfaceMaterial.Clipped = right.SurfaceMaterial.Clipped &&
            left.SurfaceMaterial.Names = right.SurfaceMaterial.Names &&
            refEq left.PhysicallyBasedGeometry right.PhysicallyBasedGeometry

        static member comparer =
            HashIdentity.FromFunctions PhysicallyBasedSurface.hash PhysicallyBasedSurface.equals

        static member make names (surfaceMatrix : Matrix4x4) bounds properties material materialIndex surfaceNode geometry =
            let hashCode =
                (hash material.AlbedoTexture) ^^^
                (hash material.RoughnessTexture <<< 2) ^^^
                (hash material.MetallicTexture <<< 4) ^^^
                (hash material.AmbientOcclusionTexture <<< 6) ^^^
                (hash material.EmissionTexture <<< 8) ^^^
                (hash material.NormalTexture <<< 10) ^^^
                (hash material.HeightTexture <<< 12) ^^^
                (hash material.SubdermalTexture <<< 14) ^^^
                (hash material.FinenessTexture <<< 16) ^^^
                (hash material.ScatterTexture <<< 18) ^^^
                (hash material.TwoSided <<< 20) ^^^
                (hash material.Clipped <<< 22) ^^^
                (hash material.Names <<< 24) ^^^
                Runtime.CompilerServices.RuntimeHelpers.GetHashCode geometry <<< 24
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
        let extractFinenessOffset = PhysicallyBasedSurface.extractFinenessOffset
        let extractScatterType = PhysicallyBasedSurface.extractScatterType
        let extractSpecularScalar = PhysicallyBasedSurface.extractSpecularScalar
        let extractRefractiveIndex = PhysicallyBasedSurface.extractRefractiveIndex
        let extractNavShape = PhysicallyBasedSurface.extractNavShape
        let hash = PhysicallyBasedSurface.hash
        let equals = PhysicallyBasedSurface.equals
        let comparer = PhysicallyBasedSurface.comparer
        let make = PhysicallyBasedSurface.make

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
          ViewProjectionUniform : int
          BonesUniforms : int array
          EyeCenterUniform : int
          ViewInverseUniform : int
          ProjectionInverseUniform : int
          LightCutoffMarginUniform : int
          LightAmbientColorUniform : int
          LightAmbientBrightnessUniform : int
          LightAmbientBoostCutoffUniform : int
          LightAmbientBoostScalarUniform : int
          LightShadowSamplesUniform : int
          LightShadowBiasUniform : int
          LightShadowSampleScalarUniform : int
          LightShadowExponentUniform : int
          LightShadowDensityUniform : int
          FogEnabledUniform : int
          FogTypeUniform : int
          FogStartUniform : int
          FogFinishUniform : int
          FogDensityUniform : int
          FogColorUniform : int
          SsvfEnabledUniform : int
          SsvfStepsUniform : int
          SsvfAsymmetryUniform : int
          SsvfIntensityUniform : int
          SsrrEnabledUniform : int
          SsrrIntensityUniform : int
          SsrrDetailUniform : int
          SsrrRefinementsMaxUniform : int
          SsrrRayThicknessUniform : int
          SsrrDistanceCutoffUniform : int
          SsrrDistanceCutoffMarginUniform : int
          SsrrEdgeHorizontalMarginUniform : int
          SsrrEdgeVerticalMarginUniform : int
          AlbedoTextureUniform : int
          RoughnessTextureUniform : int
          MetallicTextureUniform : int
          AmbientOcclusionTextureUniform : int
          EmissionTextureUniform : int
          NormalTextureUniform : int
          HeightTextureUniform : int
          SubdermalTextureUniform : int
          FinenessTextureUniform : int
          ScatterTextureUniform : int
          DepthTextureUniform : int
          ColorTextureUniform : int
          BrdfTextureUniform : int
          IrradianceMapUniform : int
          EnvironmentFilterMapUniform : int
          IrradianceMapsUniforms : int array
          EnvironmentFilterMapsUniforms : int array
          ShadowTexturesUniforms : int array
          ShadowMapsUniforms : int array
          ShadowCascadesUniforms : int array
          LightMapOriginsUniforms : int array
          LightMapMinsUniforms : int array
          LightMapSizesUniforms : int array
          LightMapAmbientColorsUniforms : int array
          LightMapAmbientBrightnessesUniforms : int array
          LightMapsCountUniform : int
          LightOriginsUniforms : int array
          LightDirectionsUniforms : int array
          LightColorsUniforms : int array
          LightBrightnessesUniforms : int array
          LightAttenuationLinearsUniforms : int array
          LightAttenuationQuadraticsUniforms : int array
          LightCutoffsUniforms : int array
          LightTypesUniforms : int array
          LightConeInnersUniforms : int array
          LightConeOutersUniforms : int array
          LightDesireFogsUniforms : int array
          LightShadowIndicesUniforms : int array
          LightsCountUniform : int
          ShadowNearUniform : int
          ShadowMatricesUniforms : int array
          PhysicallyBasedShader : uint }

    /// Describes a physically-based deferred terrain shader that's loaded into GPU.
    type PhysicallyBasedDeferredTerrainShader =
        { ViewUniform : int
          ProjectionUniform : int
          ViewProjectionUniform : int
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
        { EyeCenterUniform : int
          ViewInverseUniform : int
          ProjectionInverseUniform : int
          DepthTextureUniform : int
          NormalPlusTextureUniform : int
          LightMapOriginsUniforms : int array
          LightMapMinsUniforms : int array
          LightMapSizesUniforms : int array
          LightMapsCountUniform : int
          PhysicallyBasedDeferredLightMappingShader : uint }

    /// Describes an ambient pass of a deferred physically-based shader that's loaded into GPU.
    type PhysicallyBasedDeferredAmbientShader =
        { EyeCenterUniform : int
          ViewInverseUniform : int
          ProjectionInverseUniform : int
          DepthTextureUniform : int
          LightMappingTextureUniform : int
          LightMapAmbientColorUniform : int
          LightMapAmbientBrightnessUniform : int
          LightMapAmbientColorsUniforms : int array
          LightMapAmbientBrightnessesUniforms : int array
          PhysicallyBasedDeferredAmbientShader : uint }

    /// Describes an irradiance pass of a deferred physically-based shader that's loaded into GPU.
    type PhysicallyBasedDeferredIrradianceShader =
        { EyeCenterUniform : int
          ViewInverseUniform : int
          ProjectionInverseUniform : int
          DepthTextureUniform : int
          NormalPlusTextureUniform : int
          LightMappingTextureUniform : int
          IrradianceMapUniform : int
          IrradianceMapsUniforms : int array
          PhysicallyBasedDeferredIrradianceShader : uint }

    /// Describes an environment filter pass of a deferred physically-based shader that's loaded into GPU.
    type PhysicallyBasedDeferredEnvironmentFilterShader =
        { EyeCenterUniform : int
          ViewInverseUniform : int
          ProjectionInverseUniform : int
          DepthTextureUniform : int
          MaterialTextureUniform : int
          NormalPlusTextureUniform : int
          LightMappingTextureUniform : int
          EnvironmentFilterMapUniform : int
          EnvironmentFilterMapsUniforms : int array
          LightMapOriginsUniforms : int array
          LightMapMinsUniforms : int array
          LightMapSizesUniforms : int array
          PhysicallyBasedDeferredEnvironmentFilterShader : uint }

    /// Describes an ssao pass of a deferred physically-based shader that's loaded into GPU.
    type PhysicallyBasedDeferredSsaoShader =
        { EyeCenterUniform : int
          ViewUniform : int
          ViewInverseUniform : int
          ProjectionUniform : int
          ProjectionInverseUniform : int
          ViewProjectionUniform : int
          DepthTextureUniform : int
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
          ViewInverseUniform : int
          ProjectionUniform : int
          ProjectionInverseUniform : int
          LightCutoffMarginUniform : int
          LightShadowSamplesUniform : int
          LightShadowBiasUniform : int
          LightShadowSampleScalarUniform : int
          LightShadowExponentUniform : int
          LightShadowDensityUniform : int
          SssEnabledUniform : int
          SsvfEnabledUniform : int
          SsvfStepsUniform : int
          SsvfAsymmetryUniform : int
          SsvfIntensityUniform : int
          DepthTextureUniform : int
          AlbedoTextureUniform : int
          MaterialTextureUniform : int
          NormalPlusTextureUniform : int
          SubdermalPlusTextureUniform : int
          ScatterPlusTextureUniform : int
          ShadowTexturesUniforms : int array
          ShadowMapsUniforms : int array
          ShadowCascadesUniforms : int array
          LightOriginsUniforms : int array
          LightDirectionsUniforms : int array
          LightColorsUniforms : int array
          LightBrightnessesUniforms : int array
          LightAttenuationLinearsUniforms : int array
          LightAttenuationQuadraticsUniforms : int array
          LightCutoffsUniforms : int array
          LightTypesUniforms : int array
          LightConeInnersUniforms : int array
          LightConeOutersUniforms : int array
          LightDesireFogsUniforms : int array
          LightShadowIndicesUniforms : int array
          LightsCountUniform : int
          ShadowNearUniform : int
          ShadowMatricesUniforms : int array
          PhysicallyBasedDeferredLightingShader : uint }

    /// Describes the coloring pass of a deferred physically-based shader that's loaded into GPU.
    type PhysicallyBasedDeferredColoringShader =
        { EyeCenterUniform : int
          ViewUniform : int
          ViewInverseUniform : int
          ProjectionUniform : int
          ProjectionInverseUniform : int
          LightAmbientBoostCutoffUniform : int
          LightAmbientBoostScalarUniform : int
          SsrlEnabledUniform : int
          SsrlIntensityUniform : int
          SsrlDetailUniform : int
          SsrlRefinementsMaxUniform : int
          SsrlRayThicknessUniform : int
          SsrlTowardEyeCutoffUniform : int
          SsrlDepthCutoffUniform : int
          SsrlDepthCutoffMarginUniform : int
          SsrlDistanceCutoffUniform : int
          SsrlDistanceCutoffMarginUniform : int
          SsrlRoughnessCutoffUniform : int
          SsrlRoughnessCutoffMarginUniform : int
          SsrlSlopeCutoffUniform : int
          SsrlSlopeCutoffMarginUniform : int
          SsrlEdgeHorizontalMarginUniform : int
          SsrlEdgeVerticalMarginUniform : int
          DepthTextureUniform : int
          AlbedoTextureUniform : int
          MaterialTextureUniform : int
          NormalPlusTextureUniform : int
          LightAccumTextureUniform : int
          BrdfTextureUniform : int
          AmbientTextureUniform : int
          IrradianceTextureUniform : int
          EnvironmentFilterTextureUniform : int
          SsaoTextureUniform : int
          PhysicallyBasedDeferredColoringShader : uint }

    /// Describes the composition pass of a deferred physically-based shader that's loaded into GPU.
    type PhysicallyBasedDeferredCompositionShader =
        { EyeCenterUniform : int
          ViewInverseUniform : int
          ProjectionInverseUniform : int
          FogEnabledUniform : int
          FogTypeUniform : int
          FogStartUniform : int
          FogFinishUniform : int
          FogColorUniform : int
          FogDensityUniform : int
          DepthTextureUniform : int
          ColorTextureUniform : int
          FogAccumTextureUniform : int
          PhysicallyBasedDeferredCompositionShader : uint }

    /// Create the buffers required for physically-based rendering.
    let CreatePhysicallyBasedBuffers (geometryViewport : Viewport) =

        // create shadow texture buffers array
        let shadowTextureBuffersArray =
            [|for _ in 0 .. dec Constants.Render.ShadowTexturesMax do
                let shadowResolution = geometryViewport.ShadowTextureResolution
                match OpenGL.Framebuffer.TryCreateShadowTextureBuffers (shadowResolution.X, shadowResolution.Y) with
                | Right shadowTextureBuffers -> shadowTextureBuffers
                | Left error -> failwith ("Could not create buffers due to: " + error + ".")|]

        // create second array of shadow texture buffers
        let shadowTextureBuffers2Array =
            [|for _ in 0 .. dec Constants.Render.ShadowTexturesMax do
                let shadowResolution = geometryViewport.ShadowTextureResolution
                match OpenGL.Framebuffer.TryCreateShadowTextureBuffers (shadowResolution.X, shadowResolution.Y) with
                | Right shadowTextureBuffers -> shadowTextureBuffers
                | Left error -> failwith ("Could not create buffers due to: " + error + ".")|]

        // create shadow map buffers array
        let shadowMapBuffersArray =
            [|for _ in 0 .. dec Constants.Render.ShadowMapsMax do
                let shadowResolution = geometryViewport.ShadowMapResolution
                match OpenGL.Framebuffer.TryCreateShadowMapBuffers (shadowResolution.X, shadowResolution.Y) with
                | Right shadowMapBuffers -> shadowMapBuffers
                | Left error -> failwith ("Could not create buffers due to: " + error + ".")|]

        // create shadow cascade array buffers array
        let shadowCascadeArrayBuffersArray =
            [|for _ in 0 .. dec Constants.Render.ShadowCascadesMax do
                let shadowResolution = geometryViewport.ShadowCascadeResolution
                match OpenGL.Framebuffer.TryCreateShadowCascadeArrayBuffers (shadowResolution.X, shadowResolution.Y, Constants.Render.ShadowCascadeLevels) with
                | Right shadowCascadeArrayBuffers -> shadowCascadeArrayBuffers
                | Left error -> failwith ("Could not create buffers due to: " + error + ".")|]

        // create second array of shadow cascade filter buffers
        let shadowCascadeFilterBuffersArray =
            [|for _ in 0 .. dec Constants.Render.ShadowCascadesMax do
                let shadowCascadeResolution = geometryViewport.ShadowCascadeResolution
                match OpenGL.Framebuffer.TryCreateShadowCascadeFilterBuffers (shadowCascadeResolution.X, shadowCascadeResolution.Y) with
                | Right shadowCascadeFilterBuffers -> shadowCascadeFilterBuffers
                | Left error -> failwith ("Could not create buffers due to: " + error + ".")|]

        // create geometry buffers
        let geometryBuffers =
            match OpenGL.Framebuffer.TryCreateGeometryBuffers (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y) with
            | Right geometryBuffers -> geometryBuffers
            | Left error -> failwith ("Could not create buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create light mapping buffers
        let lightMappingBuffers =
            match OpenGL.Framebuffer.TryCreateLightMappingBuffers (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y) with
            | Right lightMappingBuffers -> lightMappingBuffers
            | Left error -> failwith ("Could not create buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create ambient buffers
        let ambientBuffers =
            match OpenGL.Framebuffer.TryCreateColorBuffers (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y, false, false) with
            | Right ambientBuffers -> ambientBuffers
            | Left error -> failwith ("Could not create buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create irradiance buffers
        let irradianceBuffers =
            match OpenGL.Framebuffer.TryCreateIrradianceBuffers (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y) with
            | Right irradianceBuffers -> irradianceBuffers
            | Left error -> failwith ("Could not create buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create environment filter buffers
        let environmentFilterBuffers =
            match OpenGL.Framebuffer.TryCreateEnvironmentFilterBuffers (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y) with
            | Right environmentFilterBuffers -> environmentFilterBuffers
            | Left error -> failwith ("Could not create buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create unfiltered ssao buffers
        let ssaoBuffersUnfiltered =
            match OpenGL.Framebuffer.TryCreateSsaoBuffers (geometryViewport.SsaoResolution.X, geometryViewport.SsaoResolution.Y) with
            | Right ssaoBuffers -> ssaoBuffers
            | Left error -> failwith ("Could not create buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create filtered ssao buffers
        let ssaoBuffersFiltered =
            match OpenGL.Framebuffer.TryCreateSsaoBuffers (geometryViewport.SsaoResolution.X, geometryViewport.SsaoResolution.Y) with
            | Right ssaoBuffers -> ssaoBuffers
            | Left error -> failwith ("Could not create buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create lighting buffers
        let lightingBuffers =
            match OpenGL.Framebuffer.TryCreateLightingBuffers (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y) with
            | Right lightingBuffers -> lightingBuffers
            | Left error -> failwith ("Could not create buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create coloring buffers
        let coloringBuffers =
            match OpenGL.Framebuffer.TryCreateColoringBuffers (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y) with
            | Right coloringBuffers -> coloringBuffers
            | Left error -> failwith ("Could not create buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create specular screen down-sample buffers
        let specularScreenDownSampleBuffers =
            match OpenGL.Framebuffer.TryCreateFilterBilateralDownSampleBuffers (geometryViewport.Bounds.Size.X / 2, geometryViewport.Bounds.Size.Y / 2) with
            | Right specularScreenDownSampleBuffers -> specularScreenDownSampleBuffers
            | Left error -> failwith ("Could not create buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create specular screen up-sample buffers
        let specularScreenUpSampleBuffers =
            match OpenGL.Framebuffer.TryCreateColorBuffers (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y, false, false) with
            | Right specularScreenUpSampleBuffers -> specularScreenUpSampleBuffers
            | Left error -> failwith ("Could not create buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create fog accum down-sample buffers
        let fogAccumDownSampleBuffers =
            match OpenGL.Framebuffer.TryCreateFilterBilateralDownSampleBuffers (geometryViewport.Bounds.Size.X / 2, geometryViewport.Bounds.Size.Y / 2) with
            | Right fogAccumDownSampleBuffers -> fogAccumDownSampleBuffers
            | Left error -> failwith ("Could not create buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create fog accum up-sample buffers
        let fogAccumUpSampleBuffers =
            match OpenGL.Framebuffer.TryCreateColorBuffers (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y, false, false) with
            | Right fogAccumUpSampleBuffers -> fogAccumUpSampleBuffers
            | Left error -> failwith ("Could not create buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create composition buffers
        let compositionBuffers =
            match OpenGL.Framebuffer.TryCreateColorBuffers (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y, false, true) with
            | Right compositionBuffers -> compositionBuffers
            | Left error -> failwith ("Could not create buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create bloom extract buffers
        let bloomExtractBuffers =
            match OpenGL.Framebuffer.TryCreateColorBuffers (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y, true, false) with
            | Right bloomExtractBuffers -> bloomExtractBuffers
            | Left error -> failwith ("Could not create buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create bloom sample buffers
        let bloomSampleBuffers =
            match OpenGL.Framebuffer.TryCreateFilterBloomSampleBuffers (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y, Constants.Render.BloomSampleLevels) with
            | Right bloomSampleBuffers -> bloomSampleBuffers
            | Left error -> failwith ("Could not create buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        /// create bloom apply buffers
        let bloomApplyBuffers =
            match OpenGL.Framebuffer.TryCreateColorBuffers (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y, false, false) with
            | Right bloomApplyBuffers -> bloomApplyBuffers
            | Left error -> failwith ("Could not create buffers due to: " + error + ".")

        // create filter 0 buffers
        let filter0Buffers =
            match OpenGL.Framebuffer.TryCreateColorBuffers (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y, true, false) with
            | Right filter0Buffers -> filter0Buffers
            | Left error -> failwith ("Could not create buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create filter 1 buffers
        let filter1Buffers =
            match OpenGL.Framebuffer.TryCreateColorBuffers (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y, true, false) with
            | Right filter1Buffers -> filter1Buffers
            | Left error -> failwith ("Could not create buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create filter 2 buffers
        let filter2Buffers =
            match OpenGL.Framebuffer.TryCreateColorBuffers (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y, true, false) with
            | Right filter2Buffers -> filter2Buffers
            | Left error -> failwith ("Could not create buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create presentation buffers
        let presentationBuffers =
            match OpenGL.Framebuffer.TryCreateColorBuffers (geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y, false, false) with
            | Right presentationBuffers -> presentationBuffers
            | Left error -> failwith ("Could not create buffers due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // make record
        { ShadowTextureBuffersArray = shadowTextureBuffersArray
          ShadowTextureBuffers2Array = shadowTextureBuffers2Array
          ShadowMapBuffersArray = shadowMapBuffersArray
          ShadowCascadeArrayBuffersArray = shadowCascadeArrayBuffersArray
          ShadowCascadeFilterBuffersArray = shadowCascadeFilterBuffersArray
          GeometryBuffers = geometryBuffers
          LightMappingBuffers = lightMappingBuffers
          IrradianceBuffers = irradianceBuffers
          EnvironmentFilterBuffers = environmentFilterBuffers
          AmbientBuffers = ambientBuffers
          SsaoBuffersUnfiltered = ssaoBuffersUnfiltered
          SsaoBuffersFiltered = ssaoBuffersFiltered
          LightingBuffers = lightingBuffers
          ColoringBuffers = coloringBuffers
          SpecularScreenDownSampleBuffers = specularScreenDownSampleBuffers
          SpecularScreenUpSampleBuffers = specularScreenUpSampleBuffers
          FogAccumDownSampleBuffers = fogAccumDownSampleBuffers
          FogAccumUpSampleBuffers = fogAccumUpSampleBuffers
          CompositionBuffers = compositionBuffers
          BloomExtractBuffers = bloomExtractBuffers
          BloomSampleBuffers = bloomSampleBuffers
          BloomApplyBuffers = bloomApplyBuffers
          Filter0Buffers = filter0Buffers
          Filter1Buffers = filter1Buffers
          Filter2Buffers = filter2Buffers
          PresentationBuffers = presentationBuffers }

    /// Destroy the physically-based buffers.
    let DestroyPhysicallyBasedBuffers buffers =
        OpenGL.Framebuffer.DestroyGeometryBuffers buffers.GeometryBuffers
        OpenGL.Framebuffer.DestroyLightMappingBuffers buffers.LightMappingBuffers
        OpenGL.Framebuffer.DestroyIrradianceBuffers buffers.IrradianceBuffers
        OpenGL.Framebuffer.DestroyEnvironmentFilterBuffers buffers.EnvironmentFilterBuffers
        OpenGL.Framebuffer.DestroyColorBuffers buffers.AmbientBuffers
        OpenGL.Framebuffer.DestroySsaoBuffers buffers.SsaoBuffersUnfiltered
        OpenGL.Framebuffer.DestroySsaoBuffers buffers.SsaoBuffersFiltered
        OpenGL.Framebuffer.DestroyLightingBuffers buffers.LightingBuffers
        OpenGL.Framebuffer.DestroyColoringBuffers buffers.ColoringBuffers
        OpenGL.Framebuffer.DestroyFilterBilateralDownSampleBuffers buffers.SpecularScreenDownSampleBuffers
        OpenGL.Framebuffer.DestroyColorBuffers buffers.SpecularScreenUpSampleBuffers
        OpenGL.Framebuffer.DestroyFilterBilateralDownSampleBuffers buffers.FogAccumDownSampleBuffers
        OpenGL.Framebuffer.DestroyColorBuffers buffers.FogAccumUpSampleBuffers
        OpenGL.Framebuffer.DestroyColorBuffers buffers.CompositionBuffers
        OpenGL.Framebuffer.DestroyColorBuffers buffers.BloomExtractBuffers
        OpenGL.Framebuffer.DestroyFilterBloomSampleBuffers buffers.BloomSampleBuffers
        OpenGL.Framebuffer.DestroyColorBuffers buffers.BloomApplyBuffers
        OpenGL.Framebuffer.DestroyColorBuffers buffers.Filter0Buffers
        OpenGL.Framebuffer.DestroyColorBuffers buffers.Filter1Buffers
        OpenGL.Framebuffer.DestroyColorBuffers buffers.Filter2Buffers
        OpenGL.Framebuffer.DestroyColorBuffers buffers.PresentationBuffers
        for shadowTextureBuffers in buffers.ShadowTextureBuffersArray do OpenGL.Framebuffer.DestroyShadowTextureBuffers shadowTextureBuffers
        for shadowTextureBuffers2 in buffers.ShadowTextureBuffers2Array do OpenGL.Framebuffer.DestroyShadowTextureBuffers shadowTextureBuffers2
        for shadowMapBuffers in buffers.ShadowMapBuffersArray do OpenGL.Framebuffer.DestroyShadowMapBuffers shadowMapBuffers
        for shadowCascadeArrayBuffers in buffers.ShadowCascadeArrayBuffersArray do OpenGL.Framebuffer.DestroyShadowCascadeArrayBuffers shadowCascadeArrayBuffers
        for shadowCascadeFilterBuffers in buffers.ShadowCascadeFilterBuffersArray do OpenGL.Framebuffer.DestroyShadowCascadeFilterBuffers shadowCascadeFilterBuffers

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
        let subdermalTextureFilePath =          if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_subdermal")               elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_subdermal")            else ""
        let finenessTextureFilePath =           if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_fineness")                elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_fineness")             else ""
        let scatterTextureFilePath =            if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_scatter")                 elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_scatter")              else ""
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
        let subdermalTextureFilePath' =         if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Subdermal")          elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Subdermal")        elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Subdermal")         else ""
        let finenessTextureFilePath' =         if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Fineness")          elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Fineness")        elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Fineness")         else ""
        let scatterTextureFilePath' =           if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Scatter")            elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Scatter")          elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Scatter")           else ""

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

        // attempt to load subdermal info
        let subdermalTexture =
            if renderable then
                match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + subdermalTextureFilePath) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + subdermalTextureFilePath') with
                    | Right texture -> texture
                    | Left _ -> defaultMaterial.SubdermalTexture
            else defaultMaterial.SubdermalTexture

        // attempt to load fineness info
        let finenessOffset = Constants.Render.FinenessOffsetDefault
        let finenessTexture =
            if renderable then
                match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + finenessTextureFilePath) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + finenessTextureFilePath') with
                    | Right texture -> texture
                    | Left _ -> defaultMaterial.FinenessTexture
            else defaultMaterial.FinenessTexture

        // attempt to load scatter info
        let scatterType = Constants.Render.ScatterTypeDefault
        let scatterTexture =
            if renderable then
                match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + scatterTextureFilePath) with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered (true, true, dirPrefix + scatterTextureFilePath') with
                    | Right texture -> texture
                    | Left _ -> defaultMaterial.ScatterTexture
            else defaultMaterial.ScatterTexture

        // attempt to load specular scalar info
        let specularScalar =
            match material.SpecularScalarOpt with
            | ValueSome specularScalar -> specularScalar
            | ValueNone -> Constants.Render.SpecularScalarDefault

        // attempt to load refractive index info
        let refractiveIndex =
            match material.RefractiveIndexOpt with
            | ValueSome refractiveIndex -> refractiveIndex
            | ValueNone -> Constants.Render.RefractiveIndexDefault

        // compute two-sidedness
        let twoSided =
            match material.TwoSidedOpt with
            | ValueSome twoSided -> twoSided
            | ValueNone -> material.IsTwoSided

        // compute clippedness
        let clipped =
            match material.ClippedOpt with
            | ValueSome clipped -> clipped
            | ValueNone -> false

        // compose names when not rendering so that surfaces can be correlated without textures
        let names =
            if not renderable then
                albedoTextureSlotFilePath + "/" +
                roughnessTextureSlot.FilePath + "/" +
                metallicTextureSlot.FilePath + "/" +
                ambientOcclusionTextureSlotA.FilePath + "/" +
                ambientOcclusionTextureSlotB.FilePath + "/" +
                emissionTextureSlot.FilePath + "/" +
                normalTextureSlot.FilePath + "/" +
                heightTextureSlot.FilePath
            else ""

        // make properties
        let properties =
            { Albedo = color albedo.R albedo.G albedo.B albedo.A
              Roughness = roughness
              Metallic = metallic
              AmbientOcclusion = ambientOcclusion
              Emission = emission
              Height = height
              IgnoreLightMaps = ignoreLightMaps
              OpaqueDistance = opaqueDistance
              FinenessOffset = finenessOffset
              ScatterType = scatterType
              SpecularScalar = specularScalar
              RefractiveIndex = refractiveIndex }

        // make material
        let material =
            { AlbedoTexture = albedoTexture
              RoughnessTexture = roughnessTexture
              MetallicTexture = metallicTexture
              AmbientOcclusionTexture = ambientOcclusionTexture
              EmissionTexture = emissionTexture
              NormalTexture = normalTexture
              HeightTexture = heightTexture
              SubdermalTexture = subdermalTexture
              FinenessTexture = finenessTexture
              ScatterTexture = scatterTexture
              TwoSided = twoSided
              Clipped = clipped
              Names = names }

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

    let StaticTexCoordsOffset = (3 (*position*)) * sizeof<single>
    let StaticNormalOffset =    (3 (*position*) + 2 (*tex coords*)) * sizeof<single>
    let StaticVertexSize =      (3 (*position*) + 2 (*tex coords*) + 3 (*normal*)) * sizeof<single>

    let CreatePhysicallyBasedStaticVao () =

        // create vao
        let vao =  [|0u|]
        Gl.CreateVertexArrays vao
        let vao = vao.[0]

        // per vertex
        Gl.VertexArrayAttribFormat (vao, 0u, 3, VertexAttribType.Float, false, uint 0)
        Gl.VertexArrayAttribFormat (vao, 1u, 2, VertexAttribType.Float, false, uint StaticTexCoordsOffset)
        Gl.VertexArrayAttribFormat (vao, 2u, 3, VertexAttribType.Float, false, uint StaticNormalOffset)
        Gl.VertexArrayAttribBinding (vao, 0u, 0u)
        Gl.VertexArrayAttribBinding (vao, 1u, 0u)
        Gl.VertexArrayAttribBinding (vao, 2u, 0u)
        Gl.EnableVertexArrayAttrib (vao, 0u)
        Gl.EnableVertexArrayAttrib (vao, 1u)
        Gl.EnableVertexArrayAttrib (vao, 2u)

        // per instance
        Gl.VertexArrayAttribFormat (vao, 3u, 4, VertexAttribType.Float, false, uint 0)
        Gl.VertexArrayAttribFormat (vao, 4u, 4, VertexAttribType.Float, false, uint (4 * sizeof<single>))
        Gl.VertexArrayAttribFormat (vao, 5u, 4, VertexAttribType.Float, false, uint (8 * sizeof<single>))
        Gl.VertexArrayAttribFormat (vao, 6u, 4, VertexAttribType.Float, false, uint (12 * sizeof<single>))
        Gl.VertexArrayAttribFormat (vao, 7u, 4, VertexAttribType.Float, false, uint (16 * sizeof<single>))
        Gl.VertexArrayAttribFormat (vao, 8u, 4, VertexAttribType.Float, false, uint (20 * sizeof<single>))
        Gl.VertexArrayAttribFormat (vao, 9u, 4, VertexAttribType.Float, false, uint (24 * sizeof<single>))
        Gl.VertexArrayAttribFormat (vao, 10u, 4, VertexAttribType.Float, false, uint (28 * sizeof<single>))
        Gl.VertexArrayAttribFormat (vao, 11u, 4, VertexAttribType.Float, false, uint (32 * sizeof<single>))
        Gl.VertexArrayAttribBinding (vao, 3u, 1u) // NOTE: different index for instance!
        Gl.VertexArrayAttribBinding (vao, 4u, 1u)
        Gl.VertexArrayAttribBinding (vao, 5u, 1u)
        Gl.VertexArrayAttribBinding (vao, 6u, 1u)
        Gl.VertexArrayAttribBinding (vao, 7u, 1u)
        Gl.VertexArrayAttribBinding (vao, 8u, 1u)
        Gl.VertexArrayAttribBinding (vao, 9u, 1u)
        Gl.VertexArrayAttribBinding (vao, 10u, 1u)
        Gl.VertexArrayAttribBinding (vao, 11u, 1u)
        Gl.EnableVertexArrayAttrib (vao, 3u)
        Gl.EnableVertexArrayAttrib (vao, 4u)
        Gl.EnableVertexArrayAttrib (vao, 5u)
        Gl.EnableVertexArrayAttrib (vao, 6u)
        Gl.EnableVertexArrayAttrib (vao, 7u)
        Gl.EnableVertexArrayAttrib (vao, 8u)
        Gl.EnableVertexArrayAttrib (vao, 9u)
        Gl.EnableVertexArrayAttrib (vao, 10u)
        Gl.EnableVertexArrayAttrib (vao, 11u)

        // divisors
        Gl.VertexArrayBindingDivisor (vao, 0u, 0u)
        Gl.VertexArrayBindingDivisor (vao, 1u, 1u)

        // fin
        vao

    /// Create physically-based static geometry from a mesh.
    let CreatePhysicallyBasedStaticGeometry (renderable, primitiveType, vertexData : single Memory, indexData : int Memory, bounds) =

        // make buffers
        let (vertices, indices, vertexBuffer, instanceBuffer, indexBuffer) =

            // make renderable
            if renderable then

                // create vertex buffer
                let vertexBuffer = Gl.GenBuffer ()
                Gl.BindBuffer (BufferTarget.ArrayBuffer, vertexBuffer)
                use vertexDataHnd = vertexData.Pin () in
                    let vertexDataNint = vertexDataHnd.Pointer |> NativePtr.ofVoidPtr<single> |> NativePtr.toNativeInt
                    Gl.BufferData (BufferTarget.ArrayBuffer, uint (vertexData.Length * sizeof<single>), vertexDataNint, BufferUsage.StaticDraw)
                Hl.Assert ()

                // create instance buffer
                let instanceBuffer = Gl.GenBuffer ()
                Gl.BindBuffer (BufferTarget.ArrayBuffer, instanceBuffer)
                let instanceData = Array.zeroCreate Constants.Render.InstanceFieldCount
                m4Identity.ToArray (instanceData, 0)
                let instanceDataPtr = GCHandle.Alloc (instanceData, GCHandleType.Pinned)
                let strideSize = Constants.Render.InstanceFieldCount * sizeof<single>
                try Gl.BufferData (BufferTarget.ArrayBuffer, uint strideSize, instanceDataPtr.AddrOfPinnedObject (), BufferUsage.StreamDraw)
                finally instanceDataPtr.Free ()

                // create index buffer
                let indexBuffer = Gl.GenBuffer ()
                Gl.BindBuffer (BufferTarget.ElementArrayBuffer, indexBuffer)
                let indexDataSize = uint (indexData.Length * sizeof<uint>)
                use indexDataHnd = indexData.Pin () in
                    let indexDataNint = indexDataHnd.Pointer |> NativePtr.ofVoidPtr<uint> |> NativePtr.toNativeInt
                    Gl.BufferData (BufferTarget.ElementArrayBuffer, indexDataSize, indexDataNint, BufferUsage.StaticDraw)
                Hl.Assert ()

                // fin
                ([||], [||], vertexBuffer, instanceBuffer, indexBuffer)

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
                (vertices, indices, 0u, 0u, 0u)

        // make physically-based geometry
        let geometry =
            { Bounds = bounds
              PrimitiveType = primitiveType
              ElementCount = indexData.Length
              Vertices = vertices
              Indices = indices
              TrianglesCached = None
              VertexBuffer = vertexBuffer
              InstanceBuffer = instanceBuffer
              IndexBuffer = indexBuffer }

        // fin
        geometry

    /// Create physically-based static geometry from an assimp mesh.
    let CreatePhysicallyBasedStaticGeometryFromMesh (renderable, indexData, mesh : Assimp.Mesh) =
        match CreatePhysicallyBasedStaticMesh (indexData, mesh) with
        | (vertexData, indexData, bounds) -> CreatePhysicallyBasedStaticGeometry (renderable, PrimitiveType.Triangles, vertexData.AsMemory (), indexData.AsMemory (), bounds)

    let AnimatedTexCoordsOffset =   (3 (*position*)) * sizeof<single>
    let AnimatedNormalOffset =      (3 (*position*) + 2 (*tex coords*)) * sizeof<single>
    let AnimatedBoneIdsOffset =     (3 (*position*) + 2 (*tex coords*) + 3 (*normal*)) * sizeof<single>
    let AnimatedWeightsOffset =     (3 (*position*) + 2 (*tex coords*) + 3 (*normal*) + 4 (*boneIds*)) * sizeof<single>
    let AnimatedVertexSize =        (3 (*position*) + 2 (*tex coords*) + 3 (*normal*) + 4 (*boneIds*) + 4 (*weights*)) * sizeof<single>

    let CreatePhysicallyBasedAnimatedVao () =

        // create vao
        let vao =  [|0u|]
        Gl.CreateVertexArrays vao
        let vao = vao.[0]

        // per vertex
        Gl.VertexArrayAttribFormat (vao, 0u, 3, VertexAttribType.Float, false, uint 0)
        Gl.VertexArrayAttribFormat (vao, 1u, 2, VertexAttribType.Float, false, uint AnimatedTexCoordsOffset)
        Gl.VertexArrayAttribFormat (vao, 2u, 3, VertexAttribType.Float, false, uint AnimatedNormalOffset)
        Gl.VertexArrayAttribFormat (vao, 3u, 4, VertexAttribType.Float, false, uint AnimatedBoneIdsOffset)
        Gl.VertexArrayAttribFormat (vao, 4u, 4, VertexAttribType.Float, false, uint AnimatedWeightsOffset)
        Gl.VertexArrayAttribBinding (vao, 0u, 0u)
        Gl.VertexArrayAttribBinding (vao, 1u, 0u)
        Gl.VertexArrayAttribBinding (vao, 2u, 0u)
        Gl.VertexArrayAttribBinding (vao, 3u, 0u)
        Gl.VertexArrayAttribBinding (vao, 4u, 0u)
        Gl.EnableVertexArrayAttrib (vao, 0u)
        Gl.EnableVertexArrayAttrib (vao, 1u)
        Gl.EnableVertexArrayAttrib (vao, 2u)
        Gl.EnableVertexArrayAttrib (vao, 3u)
        Gl.EnableVertexArrayAttrib (vao, 4u)

        // per instance
        Gl.VertexArrayAttribFormat (vao, 5u, 4, VertexAttribType.Float, false, uint 0)
        Gl.VertexArrayAttribFormat (vao, 6u, 4, VertexAttribType.Float, false, uint (4 * sizeof<single>))
        Gl.VertexArrayAttribFormat (vao, 7u, 4, VertexAttribType.Float, false, uint (8 * sizeof<single>))
        Gl.VertexArrayAttribFormat (vao, 8u, 4, VertexAttribType.Float, false, uint (12 * sizeof<single>))
        Gl.VertexArrayAttribFormat (vao, 9u, 4, VertexAttribType.Float, false, uint (16 * sizeof<single>))
        Gl.VertexArrayAttribFormat (vao, 10u, 4, VertexAttribType.Float, false, uint (20 * sizeof<single>))
        Gl.VertexArrayAttribFormat (vao, 11u, 4, VertexAttribType.Float, false, uint (24 * sizeof<single>))
        Gl.VertexArrayAttribFormat (vao, 12u, 4, VertexAttribType.Float, false, uint (28 * sizeof<single>))
        Gl.VertexArrayAttribFormat (vao, 13u, 4, VertexAttribType.Float, false, uint (32 * sizeof<single>))
        Gl.VertexArrayAttribBinding (vao, 5u, 1u) // NOTE: different index for instance!
        Gl.VertexArrayAttribBinding (vao, 6u, 1u)
        Gl.VertexArrayAttribBinding (vao, 7u, 1u)
        Gl.VertexArrayAttribBinding (vao, 8u, 1u)
        Gl.VertexArrayAttribBinding (vao, 9u, 1u)
        Gl.VertexArrayAttribBinding (vao, 10u, 1u)
        Gl.VertexArrayAttribBinding (vao, 11u, 1u)
        Gl.VertexArrayAttribBinding (vao, 12u, 1u)
        Gl.VertexArrayAttribBinding (vao, 13u, 1u)
        Gl.EnableVertexArrayAttrib (vao, 5u)
        Gl.EnableVertexArrayAttrib (vao, 6u)
        Gl.EnableVertexArrayAttrib (vao, 7u)
        Gl.EnableVertexArrayAttrib (vao, 8u)
        Gl.EnableVertexArrayAttrib (vao, 9u)
        Gl.EnableVertexArrayAttrib (vao, 10u)
        Gl.EnableVertexArrayAttrib (vao, 11u)
        Gl.EnableVertexArrayAttrib (vao, 12u)
        Gl.EnableVertexArrayAttrib (vao, 13u)

        // divisors
        Gl.VertexArrayBindingDivisor (vao, 0u, 0u)
        Gl.VertexArrayBindingDivisor (vao, 1u, 1u)

        // fin
        vao

    /// Create physically-based animated geometry from a mesh.
    let CreatePhysicallyBasedAnimatedGeometry (renderable, primitiveType, vertexData : single Memory, indexData : int Memory, bounds) =

        // make buffers
        let (vertices, indices, vertexBuffer, instanceBuffer, indexBuffer) =

            // make renderable
            if renderable then

                // create vertex buffer
                let vertexBuffer = Gl.GenBuffer ()
                Gl.BindBuffer (BufferTarget.ArrayBuffer, vertexBuffer)
                use vertexDataHnd = vertexData.Pin () in
                    let vertexDataNint = vertexDataHnd.Pointer |> NativePtr.ofVoidPtr<single> |> NativePtr.toNativeInt
                    Gl.BufferData (BufferTarget.ArrayBuffer, uint (vertexData.Length * sizeof<single>), vertexDataNint, BufferUsage.StaticDraw)
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
                Hl.Assert ()

                // create index buffer
                let indexBuffer = Gl.GenBuffer ()
                Gl.BindBuffer (BufferTarget.ElementArrayBuffer, indexBuffer)
                let indexDataSize = uint (indexData.Length * sizeof<uint>)
                use indexDataHnd = indexData.Pin () in
                    let indexDataNint = indexDataHnd.Pointer |> NativePtr.ofVoidPtr<uint> |> NativePtr.toNativeInt
                    Gl.BufferData (BufferTarget.ElementArrayBuffer, indexDataSize, indexDataNint, BufferUsage.StaticDraw)
                Hl.Assert ()

                // fin
                ([||], [||], vertexBuffer, instanceBuffer, indexBuffer)

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
                (vertices, indices, 0u, 0u, 0u)

        // make physically-based geometry
        let geometry =
            { Bounds = bounds
              PrimitiveType = primitiveType
              ElementCount = indexData.Length
              Vertices = vertices
              Indices = indices
              TrianglesCached = None
              VertexBuffer = vertexBuffer
              InstanceBuffer = instanceBuffer
              IndexBuffer = indexBuffer }

        // fin
        geometry

    /// Create physically-based animated geometry from an assimp mesh.
    let CreatePhysicallyBasedAnimatedGeometryFromMesh (renderable, indexData, mesh : Assimp.Mesh) =
        match CreatePhysicallyBasedAnimatedMesh (indexData, mesh) with
        | (vertexData, indexData, bounds) -> CreatePhysicallyBasedAnimatedGeometry (renderable, PrimitiveType.Triangles, vertexData.AsMemory (), indexData.AsMemory (), bounds)

    let TerrainTexCoordsOffset =    (3 (*position*)) * sizeof<single>
    let TerrainNormalOffset =       (3 (*position*) + 2 (*tex coords*)) * sizeof<single>
    let TerrainTintOffset =         (3 (*position*) + 2 (*tex coords*) + 3 (*normal*)) * sizeof<single>
    let TerrainBlendsOffset =       (3 (*position*) + 2 (*tex coords*) + 3 (*normal*) + 3 (*tint*)) * sizeof<single>
    let TerrainBlends2Offset =      (3 (*position*) + 2 (*tex coords*) + 3 (*normal*) + 3 (*tint*) + 4 (*blends*)) * sizeof<single>
    let TerrainVertexSize =         (3 (*position*) + 2 (*tex coords*) + 3 (*normal*) + 3 (*tint*) + 4 (*blends*) + 4 (*blends2*)) * sizeof<single>

    let CreatePhysicallyBasedTerrainVao () =

        // create vao
        let vao =  [|0u|]
        Gl.CreateVertexArrays vao
        let vao = vao.[0]

        // per vertex
        Gl.VertexArrayAttribFormat (vao, 0u, 3, VertexAttribType.Float, false, uint 0)
        Gl.VertexArrayAttribFormat (vao, 1u, 2, VertexAttribType.Float, false, uint TerrainTexCoordsOffset)
        Gl.VertexArrayAttribFormat (vao, 2u, 3, VertexAttribType.Float, false, uint TerrainNormalOffset)
        Gl.VertexArrayAttribFormat (vao, 3u, 3, VertexAttribType.Float, false, uint TerrainTintOffset)
        Gl.VertexArrayAttribFormat (vao, 4u, 4, VertexAttribType.Float, false, uint TerrainBlendsOffset)
        Gl.VertexArrayAttribFormat (vao, 5u, 4, VertexAttribType.Float, false, uint TerrainBlends2Offset)
        Gl.VertexArrayAttribBinding (vao, 0u, 0u)
        Gl.VertexArrayAttribBinding (vao, 1u, 0u)
        Gl.VertexArrayAttribBinding (vao, 2u, 0u)
        Gl.VertexArrayAttribBinding (vao, 3u, 0u)
        Gl.VertexArrayAttribBinding (vao, 4u, 0u)
        Gl.VertexArrayAttribBinding (vao, 5u, 0u)
        Gl.EnableVertexArrayAttrib (vao, 0u)
        Gl.EnableVertexArrayAttrib (vao, 1u)
        Gl.EnableVertexArrayAttrib (vao, 2u)
        Gl.EnableVertexArrayAttrib (vao, 3u)
        Gl.EnableVertexArrayAttrib (vao, 4u)
        Gl.EnableVertexArrayAttrib (vao, 5u)

        // per instance
        Gl.VertexArrayAttribFormat (vao, 6u, 4, VertexAttribType.Float, false, uint 0)
        Gl.VertexArrayAttribFormat (vao, 7u, 4, VertexAttribType.Float, false, uint (4 * sizeof<single>))
        Gl.VertexArrayAttribFormat (vao, 8u, 4, VertexAttribType.Float, false, uint (8 * sizeof<single>))
        Gl.VertexArrayAttribFormat (vao, 9u, 4, VertexAttribType.Float, false, uint (12 * sizeof<single>))
        Gl.VertexArrayAttribFormat (vao, 10u, 4, VertexAttribType.Float, false, uint (16 * sizeof<single>))
        Gl.VertexArrayAttribFormat (vao, 11u, 4, VertexAttribType.Float, false, uint (20 * sizeof<single>))
        Gl.VertexArrayAttribFormat (vao, 12u, 4, VertexAttribType.Float, false, uint (24 * sizeof<single>))
        Gl.VertexArrayAttribFormat (vao, 13u, 4, VertexAttribType.Float, false, uint (28 * sizeof<single>))
        Gl.VertexArrayAttribFormat (vao, 14u, 4, VertexAttribType.Float, false, uint (32 * sizeof<single>))
        Gl.VertexArrayAttribBinding (vao, 6u, 1u) // NOTE: different index for instance!
        Gl.VertexArrayAttribBinding (vao, 7u, 1u)
        Gl.VertexArrayAttribBinding (vao, 8u, 1u)
        Gl.VertexArrayAttribBinding (vao, 9u, 1u)
        Gl.VertexArrayAttribBinding (vao, 10u, 1u)
        Gl.VertexArrayAttribBinding (vao, 11u, 1u)
        Gl.VertexArrayAttribBinding (vao, 12u, 1u)
        Gl.VertexArrayAttribBinding (vao, 13u, 1u)
        Gl.VertexArrayAttribBinding (vao, 14u, 1u)
        Gl.EnableVertexArrayAttrib (vao, 6u)
        Gl.EnableVertexArrayAttrib (vao, 7u)
        Gl.EnableVertexArrayAttrib (vao, 8u)
        Gl.EnableVertexArrayAttrib (vao, 9u)
        Gl.EnableVertexArrayAttrib (vao, 10u)
        Gl.EnableVertexArrayAttrib (vao, 11u)
        Gl.EnableVertexArrayAttrib (vao, 12u)
        Gl.EnableVertexArrayAttrib (vao, 13u)
        Gl.EnableVertexArrayAttrib (vao, 14u)

        // divisors
        Gl.VertexArrayBindingDivisor (vao, 0u, 0u)
        Gl.VertexArrayBindingDivisor (vao, 1u, 1u)

        // fin
        vao

    /// Create physically-based terrain geometry from a mesh.
    let CreatePhysicallyBasedTerrainGeometry (renderable, primitiveType, vertexData : single Memory, indexData : int Memory, bounds) =

        // make buffers
        let (vertices, indices, vertexBuffer, instanceBuffer, indexBuffer) =

            // make renderable
            if renderable then

                // create vertex buffer
                let vertexBuffer = Gl.GenBuffer ()
                Gl.BindBuffer (BufferTarget.ArrayBuffer, vertexBuffer)
                use vertexDataHnd = vertexData.Pin () in
                    let vertexDataNint = vertexDataHnd.Pointer |> NativePtr.ofVoidPtr<single> |> NativePtr.toNativeInt
                    Gl.BufferData (BufferTarget.ArrayBuffer, uint (vertexData.Length * sizeof<single>), vertexDataNint, BufferUsage.StaticDraw)

                // create instance buffer
                let instanceBuffer = Gl.GenBuffer ()
                Gl.BindBuffer (BufferTarget.ArrayBuffer, instanceBuffer)
                let instanceData = Array.zeroCreate Constants.Render.InstanceFieldCount
                m4Identity.ToArray (instanceData, 0)
                let strideSize = instanceData.Length * sizeof<single>
                let instanceDataPtr = GCHandle.Alloc (instanceData, GCHandleType.Pinned)
                try Gl.BufferData (BufferTarget.ArrayBuffer, uint strideSize, instanceDataPtr.AddrOfPinnedObject (), BufferUsage.StreamDraw)
                finally instanceDataPtr.Free ()

                // create index buffer
                let indexBuffer = Gl.GenBuffer ()
                Gl.BindBuffer (BufferTarget.ElementArrayBuffer, indexBuffer)
                let indexDataSize = uint (indexData.Length * sizeof<uint>)
                use indexDataHnd = indexData.Pin () in
                    let indexDataNint = indexDataHnd.Pointer |> NativePtr.ofVoidPtr<uint> |> NativePtr.toNativeInt
                    Gl.BufferData (BufferTarget.ElementArrayBuffer, indexDataSize, indexDataNint, BufferUsage.StaticDraw)
                Hl.Assert ()

                // fin
                ([||], [||], vertexBuffer, instanceBuffer, indexBuffer)

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
                (vertices, indices, 0u, 0u, 0u)

        // make physically-based geometry
        let geometry =
            { Bounds = bounds
              PrimitiveType = primitiveType
              ElementCount = indexData.Length
              Vertices = vertices
              Indices = indices
              TrianglesCached = None
              VertexBuffer = vertexBuffer
              InstanceBuffer = instanceBuffer
              IndexBuffer = indexBuffer }

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
    /// OPTIMIZATION: duplicate geometry is detected and deduplicated here, which does have some run-time cost.
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
    let CreatePhysicallyBasedShader lightMapsMax lightsMax (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let viewUniform = Gl.GetUniformLocation (shader, "view")
        let projectionUniform = Gl.GetUniformLocation (shader, "projection")
        let viewProjectionUniform = Gl.GetUniformLocation (shader, "viewProjection")
        let bonesUniforms =
            Array.init Constants.Render.BonesMax $ fun i ->
                Gl.GetUniformLocation (shader, "bones[" + string i + "]")
        let eyeCenterUniform = Gl.GetUniformLocation (shader, "eyeCenter")
        let viewInverseUniform = Gl.GetUniformLocation (shader, "viewInverse")
        let projectionInverseUniform = Gl.GetUniformLocation (shader, "projectionInverse")
        let lightCutoffMarginUniform = Gl.GetUniformLocation (shader, "lightCutoffMargin")
        let lightAmbientColorUniform = Gl.GetUniformLocation (shader, "lightAmbientColor")
        let lightAmbientBrightnessUniform = Gl.GetUniformLocation (shader, "lightAmbientBrightness")
        let lightAmbientBoostCutoffUniform = Gl.GetUniformLocation (shader, "lightAmbientBoostCutoff")
        let lightAmbientBoostScalarUniform = Gl.GetUniformLocation (shader, "lightAmbientBoostScalar")
        let lightShadowSamplesUniform = Gl.GetUniformLocation (shader, "lightShadowSamples")
        let lightShadowBiasUniform = Gl.GetUniformLocation (shader, "lightShadowBias")
        let lightShadowSampleScalarUniform = Gl.GetUniformLocation (shader, "lightShadowSampleScalar")
        let lightShadowExponentUniform = Gl.GetUniformLocation (shader, "lightShadowExponent")
        let lightShadowDensityUniform = Gl.GetUniformLocation (shader, "lightShadowDensity")
        let fogEnabledUniform = Gl.GetUniformLocation (shader, "fogEnabled")
        let fogTypeUniform = Gl.GetUniformLocation (shader, "fogType")
        let fogStartUniform = Gl.GetUniformLocation (shader, "fogStart")
        let fogFinishUniform = Gl.GetUniformLocation (shader, "fogFinish")
        let fogDensityUniform = Gl.GetUniformLocation (shader, "fogDensity")
        let fogColorUniform = Gl.GetUniformLocation (shader, "fogColor")
        let ssvfEnabledUniform = Gl.GetUniformLocation (shader, "ssvfEnabled")
        let ssvfStepsUniform = Gl.GetUniformLocation (shader, "ssvfSteps")
        let ssvfAsymmetryUniform = Gl.GetUniformLocation (shader, "ssvfAsymmetry")
        let ssvfIntensityUniform = Gl.GetUniformLocation (shader, "ssvfIntensity")
        let ssrrEnabledUniform = Gl.GetUniformLocation (shader, "ssrrEnabled")
        let ssrrIntensityUniform = Gl.GetUniformLocation (shader, "ssrrIntensity")
        let ssrrDetailUniform = Gl.GetUniformLocation (shader, "ssrrDetail")
        let ssrrRefinementsMaxUniform = Gl.GetUniformLocation (shader, "ssrrRefinementsMax")
        let ssrrRayThicknessUniform = Gl.GetUniformLocation (shader, "ssrrRayThickness")
        let ssrrDistanceCutoffUniform = Gl.GetUniformLocation (shader, "ssrrDistanceCutoff")
        let ssrrDistanceCutoffMarginUniform = Gl.GetUniformLocation (shader, "ssrrDistanceCutoffMargin")
        let ssrrEdgeHorizontalMarginUniform = Gl.GetUniformLocation (shader, "ssrrEdgeHorizontalMargin")
        let ssrrEdgeVerticalMarginUniform = Gl.GetUniformLocation (shader, "ssrrEdgeVerticalMargin")
        let albedoTextureUniform = Gl.GetUniformLocation (shader, "albedoTexture")
        let roughnessTextureUniform = Gl.GetUniformLocation (shader, "roughnessTexture")
        let metallicTextureUniform = Gl.GetUniformLocation (shader, "metallicTexture")
        let ambientOcclusionTextureUniform = Gl.GetUniformLocation (shader, "ambientOcclusionTexture")
        let emissionTextureUniform = Gl.GetUniformLocation (shader, "emissionTexture")
        let normalTextureUniform = Gl.GetUniformLocation (shader, "normalTexture")
        let heightTextureUniform = Gl.GetUniformLocation (shader, "heightTexture")
        let subdermalTextureUniform = Gl.GetUniformLocation (shader, "subdermalTexture")
        let finenessTextureUniform = Gl.GetUniformLocation (shader, "finenessTexture")
        let scatterTextureUniform = Gl.GetUniformLocation (shader, "scatterTexture")
        let depthTextureUniform = Gl.GetUniformLocation (shader, "depthTexture")
        let colorTextureUniform = Gl.GetUniformLocation (shader, "colorTexture")
        let brdfTextureUniform = Gl.GetUniformLocation (shader, "brdfTexture")
        let irradianceMapUniform = Gl.GetUniformLocation (shader, "irradianceMap")
        let environmentFilterMapUniform = Gl.GetUniformLocation (shader, "environmentFilterMap")
        let irradianceMapsUniforms =
            Array.init lightMapsMax $ fun i ->
                Gl.GetUniformLocation (shader, "irradianceMaps[" + string i + "]")
        let environmentFilterMapsUniforms =
            Array.init lightMapsMax $ fun i ->
                Gl.GetUniformLocation (shader, "environmentFilterMaps[" + string i + "]")
        let shadowTexturesUniforms =
            Array.init Constants.Render.ShadowTexturesMax $ fun i ->
                Gl.GetUniformLocation (shader, "shadowTextures[" + string i + "]")
        let shadowMapsUniforms =
            Array.init Constants.Render.ShadowMapsMax $ fun i ->
                Gl.GetUniformLocation (shader, "shadowMaps[" + string i + "]")
        let shadowCascadesUniforms =
            Array.init Constants.Render.ShadowCascadesMax $ fun i ->
                Gl.GetUniformLocation (shader, "shadowCascades[" + string i + "]")
        let lightMapOriginsUniforms =
            Array.init lightMapsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightMapOrigins[" + string i + "]")
        let lightMapMinsUniforms =
            Array.init lightMapsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightMapMins[" + string i + "]")
        let lightMapSizesUniforms =
            Array.init lightMapsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightMapSizes[" + string i + "]")
        let lightMapAmbientColorsUniforms =
            Array.init lightMapsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightMapAmbientColors[" + string i + "]")
        let lightMapAmbientBrightnessesUniforms =
            Array.init lightMapsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightMapAmbientBrightnesses[" + string i + "]")
        let lightMapsCountUniform = Gl.GetUniformLocation (shader, "lightMapsCount")
        let lightOriginsUniforms =
            Array.init lightsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightOrigins[" + string i + "]")
        let lightDirectionsUniforms =
            Array.init lightsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightDirections[" + string i + "]")
        let lightColorsUniforms =
            Array.init lightsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightColors[" + string i + "]")
        let lightBrightnessesUniforms =
            Array.init lightsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightBrightnesses[" + string i + "]")
        let lightAttenuationLinearsUniforms =
            Array.init lightsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightAttenuationLinears[" + string i + "]")
        let lightAttenuationQuadraticsUniforms =
            Array.init lightsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightAttenuationQuadratics[" + string i + "]")
        let lightCutoffsUniforms =
            Array.init lightsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightCutoffs[" + string i + "]")
        let lightTypesUniforms =
            Array.init lightsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightTypes[" + string i + "]")
        let lightConeInnersUniforms =
            Array.init lightsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightConeInners[" + string i + "]")
        let lightConeOutersUniforms =
            Array.init lightsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightConeOuters[" + string i + "]")
        let lightDesireFogsUniforms =
            Array.init lightsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightDesireFogs[" + string i + "]")
        let lightShadowIndicesUniforms =
            Array.init lightsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightShadowIndices[" + string i + "]")
        let lightsCountUniform = Gl.GetUniformLocation (shader, "lightsCount")
        let shadowNearUniform = Gl.GetUniformLocation (shader, "shadowNear")
        let shadowMatricesUniforms =
            Array.init (Constants.Render.ShadowTexturesMax + Constants.Render.ShadowCascadesMax * Constants.Render.ShadowCascadeLevels) $ fun i ->
                Gl.GetUniformLocation (shader, "shadowMatrices[" + string i + "]")

        // make shader record
        { ViewUniform = viewUniform
          ProjectionUniform = projectionUniform
          ViewProjectionUniform = viewProjectionUniform
          BonesUniforms = bonesUniforms
          EyeCenterUniform = eyeCenterUniform
          ViewInverseUniform = viewInverseUniform
          ProjectionInverseUniform = projectionInverseUniform
          LightCutoffMarginUniform = lightCutoffMarginUniform
          LightAmbientColorUniform = lightAmbientColorUniform
          LightAmbientBrightnessUniform = lightAmbientBrightnessUniform
          LightAmbientBoostCutoffUniform = lightAmbientBoostCutoffUniform
          LightAmbientBoostScalarUniform = lightAmbientBoostScalarUniform
          LightShadowSamplesUniform = lightShadowSamplesUniform
          LightShadowBiasUniform = lightShadowBiasUniform
          LightShadowSampleScalarUniform = lightShadowSampleScalarUniform
          LightShadowExponentUniform = lightShadowExponentUniform
          LightShadowDensityUniform = lightShadowDensityUniform
          FogEnabledUniform = fogEnabledUniform
          FogTypeUniform = fogTypeUniform
          FogStartUniform = fogStartUniform
          FogFinishUniform = fogFinishUniform
          FogColorUniform = fogColorUniform
          FogDensityUniform = fogDensityUniform
          SsvfEnabledUniform = ssvfEnabledUniform
          SsvfStepsUniform = ssvfStepsUniform
          SsvfAsymmetryUniform = ssvfAsymmetryUniform
          SsvfIntensityUniform = ssvfIntensityUniform
          SsrrEnabledUniform = ssrrEnabledUniform
          SsrrIntensityUniform = ssrrIntensityUniform
          SsrrDetailUniform = ssrrDetailUniform
          SsrrRefinementsMaxUniform = ssrrRefinementsMaxUniform
          SsrrRayThicknessUniform = ssrrRayThicknessUniform
          SsrrDistanceCutoffUniform = ssrrDistanceCutoffUniform
          SsrrDistanceCutoffMarginUniform = ssrrDistanceCutoffMarginUniform
          SsrrEdgeHorizontalMarginUniform = ssrrEdgeHorizontalMarginUniform
          SsrrEdgeVerticalMarginUniform = ssrrEdgeVerticalMarginUniform
          AlbedoTextureUniform = albedoTextureUniform
          RoughnessTextureUniform = roughnessTextureUniform
          MetallicTextureUniform = metallicTextureUniform
          AmbientOcclusionTextureUniform = ambientOcclusionTextureUniform
          EmissionTextureUniform = emissionTextureUniform
          NormalTextureUniform = normalTextureUniform
          HeightTextureUniform = heightTextureUniform
          SubdermalTextureUniform = subdermalTextureUniform
          FinenessTextureUniform = finenessTextureUniform
          ScatterTextureUniform = scatterTextureUniform
          DepthTextureUniform = depthTextureUniform
          ColorTextureUniform = colorTextureUniform
          BrdfTextureUniform = brdfTextureUniform
          IrradianceMapUniform = irradianceMapUniform
          EnvironmentFilterMapUniform = environmentFilterMapUniform
          IrradianceMapsUniforms = irradianceMapsUniforms
          EnvironmentFilterMapsUniforms = environmentFilterMapsUniforms
          ShadowTexturesUniforms = shadowTexturesUniforms
          ShadowMapsUniforms = shadowMapsUniforms
          ShadowCascadesUniforms = shadowCascadesUniforms
          LightMapOriginsUniforms = lightMapOriginsUniforms
          LightMapMinsUniforms = lightMapMinsUniforms
          LightMapSizesUniforms = lightMapSizesUniforms
          LightMapAmbientColorsUniforms = lightMapAmbientColorsUniforms
          LightMapAmbientBrightnessesUniforms = lightMapAmbientBrightnessesUniforms
          LightMapsCountUniform = lightMapsCountUniform
          LightOriginsUniforms = lightOriginsUniforms
          LightDirectionsUniforms = lightDirectionsUniforms
          LightColorsUniforms = lightColorsUniforms
          LightBrightnessesUniforms = lightBrightnessesUniforms
          LightAttenuationLinearsUniforms = lightAttenuationLinearsUniforms
          LightAttenuationQuadraticsUniforms = lightAttenuationQuadraticsUniforms
          LightCutoffsUniforms = lightCutoffsUniforms
          LightTypesUniforms = lightTypesUniforms
          LightConeInnersUniforms = lightConeInnersUniforms
          LightConeOutersUniforms = lightConeOutersUniforms
          LightDesireFogsUniforms = lightDesireFogsUniforms
          LightShadowIndicesUniforms = lightShadowIndicesUniforms
          LightsCountUniform = lightsCountUniform
          ShadowNearUniform = shadowNearUniform
          ShadowMatricesUniforms = shadowMatricesUniforms
          PhysicallyBasedShader = shader }

    /// Create a physically-based terrain shader.
    let CreatePhysicallyBasedTerrainShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let eyeCenterUniform = Gl.GetUniformLocation (shader, "eyeCenter")
        let viewUniform = Gl.GetUniformLocation (shader, "view")
        let projectionUniform = Gl.GetUniformLocation (shader, "projection")
        let viewProjectionUniform = Gl.GetUniformLocation (shader, "viewProjection")
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
        { EyeCenterUniform = eyeCenterUniform
          ViewUniform = viewUniform
          ProjectionUniform = projectionUniform
          ViewProjectionUniform = viewProjectionUniform
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
    let CreatePhysicallyBasedDeferredLightMappingShader lightMapsMax (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let eyeCenterUniform = Gl.GetUniformLocation (shader, "eyeCenter")
        let viewInverseUniform = Gl.GetUniformLocation (shader, "viewInverse")
        let projectionInverseUniform = Gl.GetUniformLocation (shader, "projectionInverse")
        let depthTextureUniform = Gl.GetUniformLocation (shader, "depthTexture")
        let normalPlusTextureUniform = Gl.GetUniformLocation (shader, "normalPlusTexture")
        let lightMapOriginsUniforms =
            Array.init lightMapsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightMapOrigins[" + string i + "]")
        let lightMapMinsUniforms =
            Array.init lightMapsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightMapMins[" + string i + "]")
        let lightMapSizesUniforms =
            Array.init lightMapsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightMapSizes[" + string i + "]")
        let lightMapsCountUniform = Gl.GetUniformLocation (shader, "lightMapsCount")

        // make shader record
        { EyeCenterUniform = eyeCenterUniform
          ViewInverseUniform = viewInverseUniform
          ProjectionInverseUniform = projectionInverseUniform
          DepthTextureUniform = depthTextureUniform
          NormalPlusTextureUniform = normalPlusTextureUniform
          LightMapOriginsUniforms = lightMapOriginsUniforms
          LightMapMinsUniforms = lightMapMinsUniforms
          LightMapSizesUniforms = lightMapSizesUniforms
          LightMapsCountUniform = lightMapsCountUniform
          PhysicallyBasedDeferredLightMappingShader = shader }

    /// Create a physically-based shader for the ambient pass of deferred rendering.
    let CreatePhysicallyBasedDeferredAmbientShader lightMapsMax (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let eyeCenterUniform = Gl.GetUniformLocation (shader, "eyeCenter")
        let viewInverseUniform = Gl.GetUniformLocation (shader, "viewInverse")
        let projectionInverseUniform = Gl.GetUniformLocation (shader, "projectionInverse")
        let depthTextureUniform = Gl.GetUniformLocation (shader, "depthTexture")
        let lightMappingTextureUniform = Gl.GetUniformLocation (shader, "lightMappingTexture")
        let lightMapAmbientColorUniform = Gl.GetUniformLocation (shader, "lightMapAmbientColor")
        let lightMapAmbientBrightnessUniform = Gl.GetUniformLocation (shader, "lightMapAmbientBrightness")
        let lightMapAmbientColorsUniforms =
            Array.init lightMapsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightMapAmbientColors[" + string i + "]")
        let lightMapAmbientBrightnessesUniforms =
            Array.init lightMapsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightMapAmbientBrightnesses[" + string i + "]")

        // make shader record
        { EyeCenterUniform = eyeCenterUniform
          ViewInverseUniform = viewInverseUniform
          ProjectionInverseUniform = projectionInverseUniform
          DepthTextureUniform = depthTextureUniform
          LightMappingTextureUniform = lightMappingTextureUniform
          LightMapAmbientColorUniform = lightMapAmbientColorUniform
          LightMapAmbientBrightnessUniform = lightMapAmbientBrightnessUniform
          LightMapAmbientColorsUniforms = lightMapAmbientColorsUniforms
          LightMapAmbientBrightnessesUniforms = lightMapAmbientBrightnessesUniforms
          PhysicallyBasedDeferredAmbientShader = shader }

    /// Create a physically-based shader for the irradiance pass of deferred rendering.
    let CreatePhysicallyBasedDeferredIrradianceShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let eyeCenterUniform = Gl.GetUniformLocation (shader, "eyeCenter")
        let viewInverseUniform = Gl.GetUniformLocation (shader, "viewInverse")
        let projectionInverseUniform = Gl.GetUniformLocation (shader, "projectionInverse")
        let depthTextureUniform = Gl.GetUniformLocation (shader, "depthTexture")
        let normalPlusTextureUniform = Gl.GetUniformLocation (shader, "normalPlusTexture")
        let lightMappingTextureUniform = Gl.GetUniformLocation (shader, "lightMappingTexture")
        let irradianceMapUniform = Gl.GetUniformLocation (shader, "irradianceMap")
        let irradianceMapsUniforms =
            Array.init Constants.Render.LightMapsMaxDeferred $ fun i ->
                Gl.GetUniformLocation (shader, "irradianceMaps[" + string i + "]")

        // make shader record
        { EyeCenterUniform = eyeCenterUniform
          ViewInverseUniform = viewInverseUniform
          ProjectionInverseUniform = projectionInverseUniform
          DepthTextureUniform = depthTextureUniform
          NormalPlusTextureUniform = normalPlusTextureUniform
          LightMappingTextureUniform = lightMappingTextureUniform
          IrradianceMapUniform = irradianceMapUniform
          IrradianceMapsUniforms = irradianceMapsUniforms
          PhysicallyBasedDeferredIrradianceShader = shader }

    /// Create a physically-based shader for the environment filter pass of deferred rendering.
    let CreatePhysicallyBasedDeferredEnvironmentFilterShader lightMapsMax (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let eyeCenterUniform = Gl.GetUniformLocation (shader, "eyeCenter")
        let viewInverseUniform = Gl.GetUniformLocation (shader, "viewInverse")
        let projectionInverseUniform = Gl.GetUniformLocation (shader, "projectionInverse")
        let depthTextureUniform = Gl.GetUniformLocation (shader, "depthTexture")
        let materialTextureUniform = Gl.GetUniformLocation (shader, "materialTexture")
        let normalPlusTextureUniform = Gl.GetUniformLocation (shader, "normalPlusTexture")
        let lightMappingTextureUniform = Gl.GetUniformLocation (shader, "lightMappingTexture")
        let environmentFilterMapUniform = Gl.GetUniformLocation (shader, "environmentFilterMap")
        let environmentFilterMapsUniforms =
            Array.init Constants.Render.LightMapsMaxDeferred $ fun i ->
                Gl.GetUniformLocation (shader, "environmentFilterMaps[" + string i + "]")
        let lightMapOriginsUniforms =
            Array.init lightMapsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightMapOrigins[" + string i + "]")
        let lightMapMinsUniforms =
            Array.init lightMapsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightMapMins[" + string i + "]")
        let lightMapSizesUniforms =
            Array.init lightMapsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightMapSizes[" + string i + "]")

        // make shader record
        { EyeCenterUniform = eyeCenterUniform
          ViewInverseUniform = viewInverseUniform
          ProjectionInverseUniform = projectionInverseUniform
          DepthTextureUniform = depthTextureUniform
          MaterialTextureUniform = materialTextureUniform
          NormalPlusTextureUniform = normalPlusTextureUniform
          LightMappingTextureUniform = lightMappingTextureUniform
          EnvironmentFilterMapUniform = environmentFilterMapUniform
          EnvironmentFilterMapsUniforms = environmentFilterMapsUniforms
          LightMapOriginsUniforms = lightMapOriginsUniforms
          LightMapMinsUniforms = lightMapMinsUniforms
          LightMapSizesUniforms = lightMapSizesUniforms
          PhysicallyBasedDeferredEnvironmentFilterShader = shader }

    /// Create a physically-based shader for the ssao pass of deferred rendering.
    let CreatePhysicallyBasedDeferredSsaoShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let eyeCenterUniform = Gl.GetUniformLocation (shader, "eyeCenter")
        let viewUniform = Gl.GetUniformLocation (shader, "view")
        let viewInverseUniform = Gl.GetUniformLocation (shader, "viewInverse")
        let projectionUniform = Gl.GetUniformLocation (shader, "projection")
        let projectionInverseUniform = Gl.GetUniformLocation (shader, "projectionInverse")
        let viewProjectionUniform = Gl.GetUniformLocation (shader, "viewProjection")
        let depthTextureUniform = Gl.GetUniformLocation (shader, "depthTexture")
        let normalPlusTextureUniform = Gl.GetUniformLocation (shader, "normalPlusTexture")
        let ssaoResolution = Gl.GetUniformLocation (shader, "ssaoResolution")
        let ssaoIntensity = Gl.GetUniformLocation (shader, "ssaoIntensity")
        let ssaoBias = Gl.GetUniformLocation (shader, "ssaoBias")
        let ssaoRadius = Gl.GetUniformLocation (shader, "ssaoRadius")
        let ssaoDistanceMax = Gl.GetUniformLocation (shader, "ssaoDistanceMax")
        let ssaoSampleCount = Gl.GetUniformLocation (shader, "ssaoSampleCount")

        // make shader record
        { EyeCenterUniform = eyeCenterUniform
          ViewUniform = viewUniform
          ViewInverseUniform = viewInverseUniform
          ProjectionUniform = projectionUniform
          ProjectionInverseUniform = projectionInverseUniform
          ViewProjectionUniform = viewProjectionUniform
          DepthTextureUniform = depthTextureUniform
          NormalPlusTextureUniform = normalPlusTextureUniform
          SsaoResolution = ssaoResolution
          SsaoIntensity = ssaoIntensity
          SsaoBias = ssaoBias
          SsaoRadius = ssaoRadius
          SsaoDistanceMax = ssaoDistanceMax
          SsaoSampleCount = ssaoSampleCount
          PhysicallyBasedDeferredSsaoShader = shader }

    /// Create a physically-based shader for the lighting pass of deferred rendering.
    let CreatePhysicallyBasedDeferredLightingShader lightsMax (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let eyeCenterUniform = Gl.GetUniformLocation (shader, "eyeCenter")
        let viewUniform = Gl.GetUniformLocation (shader, "view")
        let viewInverseUniform = Gl.GetUniformLocation (shader, "viewInverse")
        let projectionUniform = Gl.GetUniformLocation (shader, "projection")
        let projectionInverseUniform = Gl.GetUniformLocation (shader, "projectionInverse")
        let lightCutoffMarginUniform = Gl.GetUniformLocation (shader, "lightCutoffMargin")
        let lightShadowSamplesUniform = Gl.GetUniformLocation (shader, "lightShadowSamples")
        let lightShadowBiasUniform = Gl.GetUniformLocation (shader, "lightShadowBias")
        let lightShadowSampleScalarUniform = Gl.GetUniformLocation (shader, "lightShadowSampleScalar")
        let lightShadowExponentUniform = Gl.GetUniformLocation (shader, "lightShadowExponent")
        let lightShadowDensityUniform = Gl.GetUniformLocation (shader, "lightShadowDensity")
        let sssEnabledUniform = Gl.GetUniformLocation (shader, "sssEnabled")
        let ssvfEnabledUniform = Gl.GetUniformLocation (shader, "ssvfEnabled")
        let ssvfStepsUniform = Gl.GetUniformLocation (shader, "ssvfSteps")
        let ssvfAsymmetryUniform = Gl.GetUniformLocation (shader, "ssvfAsymmetry")
        let ssvfIntensityUniform = Gl.GetUniformLocation (shader, "ssvfIntensity")
        let depthTextureUniform = Gl.GetUniformLocation (shader, "depthTexture")
        let albedoTextureUniform = Gl.GetUniformLocation (shader, "albedoTexture")
        let materialTextureUniform = Gl.GetUniformLocation (shader, "materialTexture")
        let normalPlusTextureUniform = Gl.GetUniformLocation (shader, "normalPlusTexture")
        let subdermalPlusTextureUniform = Gl.GetUniformLocation (shader, "subdermalPlusTexture")
        let scatterPlusTextureUniform = Gl.GetUniformLocation (shader, "scatterPlusTexture")
        let shadowTexturesUniforms =
            Array.init Constants.Render.ShadowTexturesMax $ fun i ->
                Gl.GetUniformLocation (shader, "shadowTextures[" + string i + "]")
        let shadowMapsUniforms =
            Array.init Constants.Render.ShadowMapsMax $ fun i ->
                Gl.GetUniformLocation (shader, "shadowMaps[" + string i + "]")
        let shadowCascadesUniforms =
            Array.init Constants.Render.ShadowCascadesMax $ fun i ->
                Gl.GetUniformLocation (shader, "shadowCascades[" + string i + "]")
        let lightOriginsUniforms =
            Array.init lightsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightOrigins[" + string i + "]")
        let lightDirectionsUniforms =
            Array.init lightsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightDirections[" + string i + "]")
        let lightColorsUniforms =
            Array.init lightsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightColors[" + string i + "]")
        let lightBrightnessesUniforms =
            Array.init lightsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightBrightnesses[" + string i + "]")
        let lightAttenuationLinearsUniforms =
            Array.init lightsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightAttenuationLinears[" + string i + "]")
        let lightAttenuationQuadraticsUniforms =
            Array.init lightsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightAttenuationQuadratics[" + string i + "]")
        let lightCutoffsUniforms =
            Array.init lightsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightCutoffs[" + string i + "]")
        let lightTypesUniforms =
            Array.init lightsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightTypes[" + string i + "]")
        let lightConeInnersUniforms =
            Array.init lightsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightConeInners[" + string i + "]")
        let lightConeOutersUniforms =
            Array.init lightsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightConeOuters[" + string i + "]")
        let lightDesireFogsUniforms =
            Array.init lightsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightDesireFogs[" + string i + "]")
        let lightShadowIndicesUniforms =
            Array.init lightsMax $ fun i ->
                Gl.GetUniformLocation (shader, "lightShadowIndices[" + string i + "]")
        let lightsCountUniform = Gl.GetUniformLocation (shader, "lightsCount")
        let shadowNearUniform = Gl.GetUniformLocation (shader, "shadowNear")
        let shadowMatricesUniforms =
            Array.init (Constants.Render.ShadowTexturesMax + Constants.Render.ShadowCascadesMax * Constants.Render.ShadowCascadeLevels) $ fun i ->
                Gl.GetUniformLocation (shader, "shadowMatrices[" + string i + "]")

        // make shader record
        { EyeCenterUniform = eyeCenterUniform
          ViewUniform = viewUniform
          ViewInverseUniform = viewInverseUniform
          ProjectionUniform = projectionUniform
          ProjectionInverseUniform = projectionInverseUniform
          LightCutoffMarginUniform = lightCutoffMarginUniform
          LightShadowSamplesUniform = lightShadowSamplesUniform
          LightShadowBiasUniform = lightShadowBiasUniform
          LightShadowSampleScalarUniform = lightShadowSampleScalarUniform
          LightShadowExponentUniform = lightShadowExponentUniform
          LightShadowDensityUniform = lightShadowDensityUniform
          SssEnabledUniform = sssEnabledUniform
          SsvfEnabledUniform = ssvfEnabledUniform
          SsvfStepsUniform = ssvfStepsUniform
          SsvfAsymmetryUniform = ssvfAsymmetryUniform
          SsvfIntensityUniform = ssvfIntensityUniform
          DepthTextureUniform = depthTextureUniform
          AlbedoTextureUniform = albedoTextureUniform
          MaterialTextureUniform = materialTextureUniform
          NormalPlusTextureUniform = normalPlusTextureUniform
          SubdermalPlusTextureUniform = subdermalPlusTextureUniform
          ScatterPlusTextureUniform = scatterPlusTextureUniform
          ShadowTexturesUniforms = shadowTexturesUniforms
          ShadowMapsUniforms = shadowMapsUniforms
          ShadowCascadesUniforms = shadowCascadesUniforms
          LightOriginsUniforms = lightOriginsUniforms
          LightDirectionsUniforms = lightDirectionsUniforms
          LightColorsUniforms = lightColorsUniforms
          LightBrightnessesUniforms = lightBrightnessesUniforms
          LightAttenuationLinearsUniforms = lightAttenuationLinearsUniforms
          LightAttenuationQuadraticsUniforms = lightAttenuationQuadraticsUniforms
          LightCutoffsUniforms = lightCutoffsUniforms
          LightTypesUniforms = lightTypesUniforms
          LightConeInnersUniforms = lightConeInnersUniforms
          LightConeOutersUniforms = lightConeOutersUniforms
          LightDesireFogsUniforms = lightDesireFogsUniforms
          LightShadowIndicesUniforms = lightShadowIndicesUniforms
          LightsCountUniform = lightsCountUniform
          ShadowNearUniform = shadowNearUniform
          ShadowMatricesUniforms = shadowMatricesUniforms
          PhysicallyBasedDeferredLightingShader = shader }

    /// Create a physically-based shader for the coloring pass of deferred rendering.
    let CreatePhysicallyBasedDeferredColoringShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let eyeCenterUniform = Gl.GetUniformLocation (shader, "eyeCenter")
        let viewUniform = Gl.GetUniformLocation (shader, "view")
        let viewInverseUniform = Gl.GetUniformLocation (shader, "viewInverse")
        let projectionUniform = Gl.GetUniformLocation (shader, "projection")
        let projectionInverseUniform = Gl.GetUniformLocation (shader, "projectionInverse")
        let lightAmbientBoostCutoffUniform = Gl.GetUniformLocation (shader, "lightAmbientBoostCutoff")
        let lightAmbientBoostScalarUniform = Gl.GetUniformLocation (shader, "lightAmbientBoostScalar")
        let ssrlEnabledUniform = Gl.GetUniformLocation (shader, "ssrlEnabled")
        let ssrlIntensityUniform = Gl.GetUniformLocation (shader, "ssrlIntensity")
        let ssrlDetailUniform = Gl.GetUniformLocation (shader, "ssrlDetail")
        let ssrlRefinementsMaxUniform = Gl.GetUniformLocation (shader, "ssrlRefinementsMax")
        let ssrlRayThicknessUniform = Gl.GetUniformLocation (shader, "ssrlRayThickness")
        let ssrlTowardEyeCutoffUniform = Gl.GetUniformLocation (shader, "ssrlTowardEyeCutoff")
        let ssrlDepthCutoffUniform = Gl.GetUniformLocation (shader, "ssrlDepthCutoff")
        let ssrlDepthCutoffMarginUniform = Gl.GetUniformLocation (shader, "ssrlDepthCutoffMargin")
        let ssrlDistanceCutoffUniform = Gl.GetUniformLocation (shader, "ssrlDistanceCutoff")
        let ssrlDistanceCutoffMarginUniform = Gl.GetUniformLocation (shader, "ssrlDistanceCutoffMargin")
        let ssrlRoughnessCutoffUniform = Gl.GetUniformLocation (shader, "ssrlRoughnessCutoff")
        let ssrlRoughnessCutoffMarginUniform = Gl.GetUniformLocation (shader, "ssrlRoughnessCutoffMargin")
        let ssrlSlopeCutoffUniform = Gl.GetUniformLocation (shader, "ssrlSlopeCutoff")
        let ssrlSlopeCutoffMarginUniform = Gl.GetUniformLocation (shader, "ssrlSlopeCutoffMargin")
        let ssrlEdgeHorizontalMarginUniform = Gl.GetUniformLocation (shader, "ssrlEdgeHorizontalMargin")
        let ssrlEdgeVerticalMarginUniform = Gl.GetUniformLocation (shader, "ssrlEdgeVerticalMargin")
        let depthTextureUniform = Gl.GetUniformLocation (shader, "depthTexture")
        let albedoTextureUniform = Gl.GetUniformLocation (shader, "albedoTexture")
        let materialTextureUniform = Gl.GetUniformLocation (shader, "materialTexture")
        let normalPlusTextureUniform = Gl.GetUniformLocation (shader, "normalPlusTexture")
        let lightAccumTextureUniform = Gl.GetUniformLocation (shader, "lightAccumTexture")
        let brdfTextureUniform = Gl.GetUniformLocation (shader, "brdfTexture")
        let ambientTextureUniform = Gl.GetUniformLocation (shader, "ambientTexture")
        let irradianceTextureUniform = Gl.GetUniformLocation (shader, "irradianceTexture")
        let environmentFilterTextureUniform = Gl.GetUniformLocation (shader, "environmentFilterTexture")
        let ssaoTextureUniform = Gl.GetUniformLocation (shader, "ssaoTexture")

        // make shader record
        { EyeCenterUniform = eyeCenterUniform
          ViewUniform = viewUniform
          ViewInverseUniform = viewInverseUniform
          ProjectionUniform = projectionUniform
          ProjectionInverseUniform = projectionInverseUniform
          LightAmbientBoostCutoffUniform = lightAmbientBoostCutoffUniform
          LightAmbientBoostScalarUniform = lightAmbientBoostScalarUniform
          SsrlEnabledUniform = ssrlEnabledUniform
          SsrlIntensityUniform = ssrlIntensityUniform
          SsrlDetailUniform = ssrlDetailUniform
          SsrlRefinementsMaxUniform = ssrlRefinementsMaxUniform
          SsrlRayThicknessUniform = ssrlRayThicknessUniform
          SsrlTowardEyeCutoffUniform = ssrlTowardEyeCutoffUniform
          SsrlDepthCutoffUniform = ssrlDepthCutoffUniform
          SsrlDepthCutoffMarginUniform = ssrlDepthCutoffMarginUniform
          SsrlDistanceCutoffUniform = ssrlDistanceCutoffUniform
          SsrlDistanceCutoffMarginUniform = ssrlDistanceCutoffMarginUniform
          SsrlRoughnessCutoffUniform = ssrlRoughnessCutoffUniform
          SsrlRoughnessCutoffMarginUniform = ssrlRoughnessCutoffMarginUniform
          SsrlSlopeCutoffUniform = ssrlSlopeCutoffUniform
          SsrlSlopeCutoffMarginUniform = ssrlSlopeCutoffMarginUniform
          SsrlEdgeHorizontalMarginUniform = ssrlEdgeHorizontalMarginUniform
          SsrlEdgeVerticalMarginUniform = ssrlEdgeVerticalMarginUniform
          DepthTextureUniform = depthTextureUniform
          AlbedoTextureUniform = albedoTextureUniform
          MaterialTextureUniform = materialTextureUniform
          NormalPlusTextureUniform = normalPlusTextureUniform
          LightAccumTextureUniform = lightAccumTextureUniform
          BrdfTextureUniform = brdfTextureUniform
          AmbientTextureUniform = ambientTextureUniform
          IrradianceTextureUniform = irradianceTextureUniform
          EnvironmentFilterTextureUniform = environmentFilterTextureUniform
          SsaoTextureUniform = ssaoTextureUniform
          PhysicallyBasedDeferredColoringShader = shader }

    /// Create a physically-based shader for the composition pass of deferred rendering.
    let CreatePhysicallyBasedDeferredCompositionShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let eyeCenterUniform = Gl.GetUniformLocation (shader, "eyeCenter")
        let viewInverseUniform = Gl.GetUniformLocation (shader, "viewInverse")
        let projectionInverseUniform = Gl.GetUniformLocation (shader, "projectionInverse")
        let fogEnabledUniform = Gl.GetUniformLocation (shader, "fogEnabled")
        let fogTypeUniform = Gl.GetUniformLocation (shader, "fogType")
        let fogStartUniform = Gl.GetUniformLocation (shader, "fogStart")
        let fogFinishUniform = Gl.GetUniformLocation (shader, "fogFinish")
        let fogDensityUniform = Gl.GetUniformLocation (shader, "fogDensity")
        let fogColorUniform = Gl.GetUniformLocation (shader, "fogColor")
        let depthTextureUniform = Gl.GetUniformLocation (shader, "depthTexture")
        let colorTextureUniform = Gl.GetUniformLocation (shader, "colorTexture")
        let fogAccumTextureUniform = Gl.GetUniformLocation (shader, "fogAccumTexture")

        // make shader record
        { EyeCenterUniform = eyeCenterUniform
          ViewInverseUniform = viewInverseUniform
          ProjectionInverseUniform = projectionInverseUniform
          FogEnabledUniform = fogEnabledUniform
          FogTypeUniform = fogTypeUniform
          FogStartUniform = fogStartUniform
          FogFinishUniform = fogFinishUniform
          FogDensityUniform = fogDensityUniform
          FogColorUniform = fogColorUniform
          DepthTextureUniform = depthTextureUniform
          ColorTextureUniform = colorTextureUniform
          FogAccumTextureUniform = fogAccumTextureUniform
          PhysicallyBasedDeferredCompositionShader = shader }

    /// Draw the filter box pass using a physically-based surface.
    let DrawFilterBoxSurface
        (inputTexture : Texture.Texture,
         geometry : PhysicallyBasedGeometry,
         shader : Filter.FilterBoxShader,
         vao : uint) =

        // setup vao
        Gl.BindVertexArray vao
        Hl.Assert ()

        // setup shader
        Gl.UseProgram shader.FilterBoxShader
        Gl.Uniform1 (shader.InputTextureUniform, 0)
        Hl.Assert ()

        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, inputTexture.TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, StaticVertexSize)
        Gl.VertexArrayVertexBuffer (vao, 1u, geometry.InstanceBuffer, 0, Constants.Render.InstanceFieldCount * sizeof<single>)
        Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown vao
        Gl.BindVertexArray 0u

    /// Draw the filter gaussian pass using a physically-based surface.
    let DrawFilterGaussianSurface
        (scale : Vector2,
         inputTexture : Texture.Texture,
         geometry : PhysicallyBasedGeometry,
         shader : Filter.FilterGaussianShader,
         vao : uint) =

        // setup vao
        Gl.BindVertexArray vao
        Hl.Assert ()
        
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
        Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, StaticVertexSize)
        Gl.VertexArrayVertexBuffer (vao, 1u, geometry.InstanceBuffer, 0, Constants.Render.InstanceFieldCount * sizeof<single>)
        Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown vao
        Gl.BindVertexArray 0u

    /// Draw the filter gaussian array pass using a physically-based surface.
    let DrawFilterGaussianFilterSurface
        (scale : Vector2,
         inputIndex : int,
         inputTextureArray : Texture.Texture,
         geometry : PhysicallyBasedGeometry,
         shader : Filter.FilterGaussianArrayShader,
         vao : uint) =

        // setup vao
        Gl.BindVertexArray vao
        Hl.Assert ()
        
        // setup shader
        Gl.UseProgram shader.FilterGaussianArrayShader
        Gl.Uniform2 (shader.ScaleUniform, scale.X, scale.Y)
        Gl.Uniform1 (shader.InputIndexUniform, inputIndex)
        Gl.Uniform1 (shader.InputTextureArrayUniform, 0)
        Hl.Assert ()

        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2dArray, inputTextureArray.TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, StaticVertexSize)
        Gl.VertexArrayVertexBuffer (vao, 1u, geometry.InstanceBuffer, 0, Constants.Render.InstanceFieldCount * sizeof<single>)
        Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown vao
        Gl.BindVertexArray 0u

    /// Draw the filter gaussian array pass using a physically-based surface.
    let DrawFilterGaussianArraySurface
        (scale : Vector2,
         inputTextureArray : Texture.Texture,
         geometry : PhysicallyBasedGeometry,
         shader : Filter.FilterGaussianShader,
         vao : uint) =

        // setup vao
        Gl.BindVertexArray vao
        Hl.Assert ()
        
        // setup shader
        Gl.UseProgram shader.FilterGaussianShader
        Gl.Uniform2 (shader.ScaleUniform, scale.X, scale.Y)
        Gl.Uniform1 (shader.InputTextureUniform, 0)
        Hl.Assert ()

        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, inputTextureArray.TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, StaticVertexSize)
        Gl.VertexArrayVertexBuffer (vao, 1u, geometry.InstanceBuffer, 0, Constants.Render.InstanceFieldCount * sizeof<single>)
        Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown vao
        Gl.BindVertexArray 0u

    /// Draw the filter bilateral down-sample pass using a physically-based surface.
    let DrawFilterBilateralDownSampleSurface
        (colorTexture : Texture.Texture,
         depthTexture : Texture.Texture,
         geometry : PhysicallyBasedGeometry,
         shader : Filter.FilterBilateralDownSampleShader,
         vao : uint) =

        // setup vao
        Gl.BindVertexArray vao
        Hl.Assert ()

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
        Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, StaticVertexSize)
        Gl.VertexArrayVertexBuffer (vao, 1u, geometry.InstanceBuffer, 0, Constants.Render.InstanceFieldCount * sizeof<single>)
        Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown vao
        Gl.BindVertexArray 0u

    /// Draw the filter bilateral up-sample pass using a physically-based surface.
    let DrawFilterBilateralUpSampleSurface
        (colorDownSampledTexture : Texture.Texture,
         depthDownSampledTexture : Texture.Texture,
         depthTexture : Texture.Texture,
         geometry : PhysicallyBasedGeometry,
         shader : Filter.FilterBilateralUpSampleShader,
         vao : uint) =

        // setup vao
        Gl.BindVertexArray vao
        Hl.Assert ()

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
        Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, StaticVertexSize)
        Gl.VertexArrayVertexBuffer (vao, 1u, geometry.InstanceBuffer, 0, Constants.Render.InstanceFieldCount * sizeof<single>)
        Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown vao
        Gl.BindVertexArray 0u

    /// Draw the bloom extract pass using a physically-based surface.
    let DrawFilterBloomExtractSurface
        (threshold : single,
         colorTexture : Texture.Texture,
         geometry : PhysicallyBasedGeometry,
         shader : Filter.FilterBloomExtractShader,
         vao : uint) =

        // setup vao
        Gl.BindVertexArray vao
        Hl.Assert ()

        // setup shader
        Gl.UseProgram shader.FilterBloomExtractShader
        Gl.Uniform1 (shader.ThresholdUniform, threshold)
        Gl.Uniform1 (shader.ColorTextureUniform, 0)
        Hl.Assert ()

        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, colorTexture.TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, StaticVertexSize)
        Gl.VertexArrayVertexBuffer (vao, 1u, geometry.InstanceBuffer, 0, Constants.Render.InstanceFieldCount * sizeof<single>)
        Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown vao
        Gl.BindVertexArray 0u

    /// Draw the bloom down-samples using a physically-based surface.
    let DrawBloomDownSamplesSurface
        (resolutionX : int,
         resolutionY : int,
         resolutionZ : int,
         karisAverageEnabled : bool,
         sourceTexture : Texture.Texture,
         targetTextures : Texture.Texture array,
         geometry : PhysicallyBasedGeometry,
         shader : Filter.FilterBloomDownSampleShader,
         vao : uint) =

        // setup vao
        Gl.BindVertexArray vao
        Hl.Assert ()

        // setup shader
        Gl.UseProgram shader.FilterBloomDownSampleShader
        Gl.Uniform1 (shader.SampleLevelUniform, 0)
        Gl.Uniform1 (shader.KarisAverageEnabledUniform, if karisAverageEnabled then 1 else 0)
        Gl.Uniform2 (shader.SourceResolutionUniform, single resolutionX, single resolutionY)
        Gl.Uniform1 (shader.SourceTextureUniform, 0)
        Hl.Assert ()

        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, sourceTexture.TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, StaticVertexSize)
        Gl.VertexArrayVertexBuffer (vao, 1u, geometry.InstanceBuffer, 0, Constants.Render.InstanceFieldCount * sizeof<single>)
        Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
        Hl.Assert ()

        // draw down-sample levels
        for i in 0 .. dec resolutionZ do

            // compute target resolution and texture
            let targetResolutionX = resolutionX >>> i
            let targetResolutionY = resolutionY >>> i
            let targetTexture = targetTextures.[i]

            // set viewport to current target's resolution
            Gl.Viewport (0, 0, targetResolutionX, targetResolutionY)
            Hl.Assert ()

            // attach target texture to framebuffer
            Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, TextureTarget.Texture2d, targetTexture.TextureId, 0)
            Hl.Assert ()

            // draw geometry
            Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
            Hl.ReportDrawCall 1
            Hl.Assert ()

            // update src resolution and texture for next iteration
            Gl.Uniform1 (shader.SampleLevelUniform, inc i)
            Gl.Uniform2 (shader.SourceResolutionUniform, single targetResolutionX, single targetResolutionY)
            Gl.BindTexture (TextureTarget.Texture2d, targetTexture.TextureId)
            Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown vao
        Gl.BindVertexArray 0u

    /// Draw the bloom up-samples using a physically-based surface.
    let DrawBloomUpSamplesSurface
        (resolutionX : int,
         resolutionY : int,
         resolutionZ : int,
         filterRadius : single,
         targetTextures : Texture.Texture array,
         geometry : PhysicallyBasedGeometry,
         shader : Filter.FilterBloomUpSampleShader,
         vao : uint) =

        // setup state
        Gl.Enable EnableCap.Blend
        Gl.BlendFunc (BlendingFactor.One, BlendingFactor.One)
        Hl.Assert ()

        // setup vao
        Gl.BindVertexArray vao
        Hl.Assert ()

        // setup shader
        Gl.UseProgram shader.FilterBloomUpSampleShader
        Gl.Uniform1 (shader.FilterRadiusUniform, filterRadius)
        Gl.Uniform1 (shader.SourceTextureUniform, 0)
        Hl.Assert ()

        // setup geometry
        Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, StaticVertexSize)
        Gl.VertexArrayVertexBuffer (vao, 1u, geometry.InstanceBuffer, 0, Constants.Render.InstanceFieldCount * sizeof<single>)
        Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
        Hl.Assert ()

        // draw down-sample levels
        for i in dec resolutionZ .. -1 .. 1 do

            // compute source resolution and target texture
            let sourceTexture = targetTextures.[i]
            let targetTexture = targetTextures.[dec i]
            let targetResolutionX = resolutionX >>> dec i
            let targetResolutionY = resolutionY >>> dec i

            // set viewport to current target's resolution
            Gl.Viewport (0, 0, targetResolutionX, targetResolutionY)
            Hl.Assert ()

            // update texture
            Gl.BindTexture (TextureTarget.Texture2d, sourceTexture.TextureId)
            Hl.Assert ()

            // attach target texture to framebuffer
            Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, TextureTarget.Texture2d, targetTexture.TextureId, 0)
            Hl.Assert ()

            // draw geometry
            Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
            Hl.ReportDrawCall 1
            Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown vao
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown state
        Gl.Disable EnableCap.Blend
        Gl.BlendFunc (BlendingFactor.One, BlendingFactor.Zero)

    /// Draw the bloom apply pass using a physically-based surface.
    let DrawBloomApplySurface
        (bloomStrength : single,
         bloomFilterTexture : Texture.Texture,
         compositionTexture : Texture.Texture,
         geometry : PhysicallyBasedGeometry,
         shader : Filter.FilterBloomApplyShader,
         vao : uint) =

        // setup vao
        Gl.BindVertexArray vao
        Hl.Assert ()

        // setup shader
        Gl.UseProgram shader.FilterBloomApplyShader
        Gl.Uniform1 (shader.BloomStrengthUniform, bloomStrength)
        Gl.Uniform1 (shader.BloomFilterTextureUniform, 0)
        Gl.Uniform1 (shader.CompositionTextureUniform, 1)
        Hl.Assert ()
        
        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, bloomFilterTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture1
        Gl.BindTexture (TextureTarget.Texture2d, compositionTexture.TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, StaticVertexSize)
        Gl.VertexArrayVertexBuffer (vao, 1u, geometry.InstanceBuffer, 0, Constants.Render.InstanceFieldCount * sizeof<single>)
        Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
        Hl.Assert ()
        
        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()
        
        // teardown shader
        Gl.UseProgram 0u
        Hl.Assert ()
        
        // teardown vao
        Gl.BindVertexArray 0u

    /// Draw the filter fxaa pass using a physically-based surface.
    let DrawFilterFxaaSurface
        (inputTexture : Texture.Texture,
         geometry : PhysicallyBasedGeometry,
         shader : Filter.FilterFxaaShader,
         vao : uint) =

        // setup vao
        Gl.BindVertexArray vao
        Hl.Assert ()

        // setup shader
        Gl.UseProgram shader.FilterFxaaShader
        Gl.Uniform1 (shader.InputTextureUniform, 0)
        Hl.Assert ()

        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, inputTexture.TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, StaticVertexSize)
        Gl.VertexArrayVertexBuffer (vao, 1u, geometry.InstanceBuffer, 0, Constants.Render.InstanceFieldCount * sizeof<single>)
        Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown vao
        Gl.BindVertexArray 0u

    /// Draw the filter presentation pass using a physically-based surface.
    let DrawFilterPresentationSurface
        (lightExposure : single,
         toneMapType : int,
         toneMapSlope : Vector3,
         toneMapOffset : Vector3,
         toneMapPower : Vector3,
         toneMapSaturation : single,
         toneMapWhitePoint : single,
         inputTexture : Texture.Texture,
         geometry : PhysicallyBasedGeometry,
         shader : Filter.FilterPresentationShader,
         vao : uint) =

        // setup vao
        Gl.BindVertexArray vao
        Hl.Assert ()

        // setup shader
        Gl.UseProgram shader.FilterPresentationShader
        Gl.Uniform1 (shader.LightExposureUniform, lightExposure)
        Gl.Uniform1 (shader.ToneMapTypeUniform, toneMapType)
        Gl.Uniform3 (shader.ToneMapSlopeUniform, toneMapSlope.X, toneMapSlope.Y, toneMapSlope.Z)
        Gl.Uniform3 (shader.ToneMapOffsetUniform, toneMapOffset.X, toneMapOffset.Y, toneMapOffset.Z)
        Gl.Uniform3 (shader.ToneMapPowerUniform, toneMapPower.X, toneMapPower.Y, toneMapPower.Z)
        Gl.Uniform1 (shader.ToneMapSaturationUniform, toneMapSaturation)
        Gl.Uniform1 (shader.ToneMapWhitePointUniform, toneMapWhitePoint)
        Gl.Uniform1 (shader.InputTextureUniform, 0)
        Hl.Assert ()

        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, inputTexture.TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, StaticVertexSize)
        Gl.VertexArrayVertexBuffer (vao, 1u, geometry.InstanceBuffer, 0, Constants.Render.InstanceFieldCount * sizeof<single>)
        Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown vao
        Gl.BindVertexArray 0u

    /// Draw a batch of physically-based depth surfaces.
    let DrawPhysicallyBasedDepthSurfaces
        (batchPhase : BatchPhase,
         eyeCenter : Vector3,
         view : single array,
         projection : single array,
         viewProjection : single array,
         bones : single array array,
         surfacesCount : int,
         instanceFields : single array,
         lightShadowExponent : single,
         material : PhysicallyBasedMaterial,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedShader,
         vao : uint,
         vertexSize) =

        // setup dynamic state
        if not material.TwoSided then Gl.Enable EnableCap.CullFace
        Hl.Assert ()

        // start batch
        if batchPhase.Starting then

            // setup state
            Gl.DepthFunc DepthFunction.Lequal
            Gl.Enable EnableCap.DepthTest
            Hl.Assert ()

            // setup vao
            Gl.BindVertexArray vao
            Hl.Assert ()

            // setup shader
            Gl.UseProgram shader.PhysicallyBasedShader
            Gl.Uniform3 (shader.EyeCenterUniform, eyeCenter.X, eyeCenter.Y, eyeCenter.Z)
            Gl.UniformMatrix4 (shader.ViewUniform, false, view)
            Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
            Gl.UniformMatrix4 (shader.ViewProjectionUniform, false, viewProjection)
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
            Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, vertexSize)
            Gl.VertexArrayVertexBuffer (vao, 1u, geometry.InstanceBuffer, 0, Constants.Render.InstanceFieldCount * sizeof<single>)
            Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
            Hl.Assert ()

            // draw geometry
            Gl.DrawElementsInstanced (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0, surfacesCount)
            Hl.ReportDrawCall surfacesCount
            Hl.Assert ()

        // stop batch
        if batchPhase.Stopping then

            // teardown shader
            Gl.UseProgram 0u
            Hl.Assert ()

            // teardown vao
            Gl.BindVertexArray 0u
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
         viewProjection : single array,
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
         shader : PhysicallyBasedShader,
         vao : uint,
         vertexSize : int) =

        // setup dynamic state
        if not material.TwoSided then Gl.Enable EnableCap.CullFace
        Hl.Assert ()

        // start batch
        if batchPhase.Starting then

            // setup static state
            Gl.DepthFunc DepthFunction.Lequal
            Gl.Enable EnableCap.DepthTest
            Hl.Assert ()

            // setup vao
            Gl.BindVertexArray vao
            Hl.Assert ()

            // setup shader
            Gl.UseProgram shader.PhysicallyBasedShader
            Gl.UniformMatrix4 (shader.ViewUniform, false, view)
            Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
            Gl.UniformMatrix4 (shader.ViewProjectionUniform, false, viewProjection)
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
            Gl.Uniform1 (shader.SubdermalTextureUniform, 7)
            Gl.Uniform1 (shader.FinenessTextureUniform, 8)
            Gl.Uniform1 (shader.ScatterTextureUniform, 9)
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
            Gl.ActiveTexture TextureUnit.Texture7
            Gl.BindTexture (TextureTarget.Texture2d, material.SubdermalTexture.TextureId)
            Gl.ActiveTexture TextureUnit.Texture8
            Gl.BindTexture (TextureTarget.Texture2d, material.FinenessTexture.TextureId)
            Gl.ActiveTexture TextureUnit.Texture9
            Gl.BindTexture (TextureTarget.Texture2d, material.ScatterTexture.TextureId)
            Hl.Assert ()

            // update instance buffer
            let instanceFieldsPtr = GCHandle.Alloc (instanceFields, GCHandleType.Pinned)
            try Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.InstanceBuffer)
                Gl.BufferData (BufferTarget.ArrayBuffer, uint (surfacesCount * Constants.Render.InstanceFieldCount * sizeof<single>), instanceFieldsPtr.AddrOfPinnedObject (), BufferUsage.StreamDraw)
                Gl.BindBuffer (BufferTarget.ArrayBuffer, 0u)
                Hl.Assert ()
            finally instanceFieldsPtr.Free ()

            // setup geometry
            Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, vertexSize)
            Gl.VertexArrayVertexBuffer (vao, 1u, geometry.InstanceBuffer, 0, Constants.Render.InstanceFieldCount * sizeof<single>)
            Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
            Hl.Assert ()

            // draw geometry
            Gl.DrawElementsInstanced (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0, surfacesCount)
            Hl.ReportDrawCall surfacesCount
            Hl.Assert ()

        // stop batch
        if batchPhase.Stopping then

            // teardown shader
            Gl.UseProgram 0u
            Hl.Assert ()

            // teardown vao
            Gl.BindVertexArray 0u
            Hl.Assert ()

            // teardown static state
            Gl.DepthFunc DepthFunction.Less
            Gl.Disable EnableCap.DepthTest
            Hl.Assert ()

        // teardown dynamic state
        if not material.TwoSided then Gl.Disable EnableCap.CullFace

    /// Begin the process of drawing with a forward shader.
    let BeginPhysicallyBasedForwardShader
        (view : single array,
         projection : single array,
         viewProjection : single array,
         eyeCenter : Vector3,
         viewInverse : single array,
         projectionInverse : single array,
         lightCutoffMargin : single,
         lightAmbientColor : Color,
         lightAmbientBrightness : single,
         lightAmbientBoostCutoff : single,
         lightAmbientBoostScalar : single,
         lightShadowSamples : int,
         lightShadowBias : single,
         lightShadowSampleScalar : single,
         lightShadowExponent : single,
         lightShadowDensity : single,
         fogEnabled : int,
         fogType : int,
         fogStart : single,
         fogFinish : single,
         fogDensity : single,
         fogColor : Color,
         ssvfEnabled : int,
         ssvfSteps : int,
         ssvfAsymmetry : single,
         ssvfIntensity : single,
         ssrrEnabled : int,
         ssrrIntensity : single,
         ssrrDetail : single,
         ssrrRefinementsMax : int,
         ssrrRayThickness : single,
         ssrrDistanceCutoff : single,
         ssrrDistanceCutoffMargin : single,
         ssrrEdgeHorizontalMargin : single,
         ssrrEdgeVerticalMargin : single,
         depthTexture : Texture.Texture,
         colorTexture : Texture.Texture,
         brdfTexture : Texture.Texture,
         irradianceMap : Texture.Texture,
         environmentFilterMap : Texture.Texture,
         shadowNear : single,
         shader : PhysicallyBasedShader,
         _ : uint) =

        // setup shader
        Gl.UseProgram shader.PhysicallyBasedShader
        Gl.UniformMatrix4 (shader.ViewUniform, false, view)
        Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
        Gl.UniformMatrix4 (shader.ViewProjectionUniform, false, viewProjection)
        Gl.Uniform3 (shader.EyeCenterUniform, eyeCenter.X, eyeCenter.Y, eyeCenter.Z)
        Gl.UniformMatrix4 (shader.ViewInverseUniform, false, viewInverse)
        Gl.UniformMatrix4 (shader.ProjectionInverseUniform, false, projectionInverse)
        Gl.Uniform1 (shader.LightCutoffMarginUniform, lightCutoffMargin)
        Gl.Uniform3 (shader.LightAmbientColorUniform, lightAmbientColor.R, lightAmbientColor.G, lightAmbientColor.B)
        Gl.Uniform1 (shader.LightAmbientBrightnessUniform, lightAmbientBrightness)
        Gl.Uniform1 (shader.LightAmbientBoostCutoffUniform, lightAmbientBoostCutoff)
        Gl.Uniform1 (shader.LightAmbientBoostScalarUniform, lightAmbientBoostScalar)
        Gl.Uniform1 (shader.LightShadowSamplesUniform, lightShadowSamples)
        Gl.Uniform1 (shader.LightShadowBiasUniform, lightShadowBias)
        Gl.Uniform1 (shader.LightShadowSampleScalarUniform, lightShadowSampleScalar)
        Gl.Uniform1 (shader.LightShadowExponentUniform, lightShadowExponent)
        Gl.Uniform1 (shader.LightShadowDensityUniform, lightShadowDensity)
        Gl.Uniform1 (shader.FogEnabledUniform, fogEnabled)
        Gl.Uniform1 (shader.FogTypeUniform, fogType)
        Gl.Uniform1 (shader.FogStartUniform, fogStart)
        Gl.Uniform1 (shader.FogFinishUniform, fogFinish)
        Gl.Uniform1 (shader.FogDensityUniform, fogDensity)
        Gl.Uniform4 (shader.FogColorUniform, fogColor.R, fogColor.G, fogColor.B, fogColor.A)
        Gl.Uniform1 (shader.SsvfEnabledUniform, ssvfEnabled)
        Gl.Uniform1 (shader.SsvfStepsUniform, ssvfSteps)
        Gl.Uniform1 (shader.SsvfAsymmetryUniform, ssvfAsymmetry)
        Gl.Uniform1 (shader.SsvfIntensityUniform, ssvfIntensity)
        Gl.Uniform1 (shader.SsrrEnabledUniform, ssrrEnabled)
        Gl.Uniform1 (shader.SsrrIntensityUniform, ssrrIntensity)
        Gl.Uniform1 (shader.SsrrDetailUniform, ssrrDetail)
        Gl.Uniform1 (shader.SsrrRefinementsMaxUniform, ssrrRefinementsMax)
        Gl.Uniform1 (shader.SsrrRayThicknessUniform, ssrrRayThickness)
        Gl.Uniform1 (shader.SsrrDistanceCutoffUniform, ssrrDistanceCutoff)
        Gl.Uniform1 (shader.SsrrDistanceCutoffMarginUniform, ssrrDistanceCutoffMargin)
        Gl.Uniform1 (shader.SsrrEdgeHorizontalMarginUniform, ssrrEdgeHorizontalMargin)
        Gl.Uniform1 (shader.SsrrEdgeVerticalMarginUniform, ssrrEdgeVerticalMargin)
        Gl.Uniform1 (shader.AlbedoTextureUniform, 0)
        Gl.Uniform1 (shader.RoughnessTextureUniform, 1)
        Gl.Uniform1 (shader.MetallicTextureUniform, 2)
        Gl.Uniform1 (shader.AmbientOcclusionTextureUniform, 3)
        Gl.Uniform1 (shader.EmissionTextureUniform, 4)
        Gl.Uniform1 (shader.NormalTextureUniform, 5)
        Gl.Uniform1 (shader.HeightTextureUniform, 6)
        Gl.Uniform1 (shader.DepthTextureUniform, 7)
        Gl.Uniform1 (shader.ColorTextureUniform, 8)
        Gl.Uniform1 (shader.BrdfTextureUniform, 9)
        Gl.Uniform1 (shader.IrradianceMapUniform, 10)
        Gl.Uniform1 (shader.EnvironmentFilterMapUniform, 11)
        Gl.Uniform1 (shader.ShadowNearUniform, shadowNear)
        Hl.Assert ()

        // setup common textures
        Gl.ActiveTexture TextureUnit.Texture7
        Gl.BindTexture (TextureTarget.Texture2d, depthTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture8
        Gl.BindTexture (TextureTarget.Texture2d, colorTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture9
        Gl.BindTexture (TextureTarget.Texture2d, brdfTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture10
        Gl.BindTexture (TextureTarget.TextureCubeMap, irradianceMap.TextureId)
        Gl.ActiveTexture TextureUnit.Texture11
        Gl.BindTexture (TextureTarget.TextureCubeMap, environmentFilterMap.TextureId)
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u

    /// Draw a batch of physically-based forward surfaces.
    let DrawPhysicallyBasedForwardSurfaces
        (bones : single array array,
         surfacesCount : int,
         instanceFields : single array,
         irradianceMaps : Texture.Texture array,
         environmentFilterMaps : Texture.Texture array,
         shadowTextures : Texture.Texture array,
         shadowMaps : Texture.Texture array,
         shadowCascades : Texture.Texture array,
         lightMapOrigins : Vector3 array,
         lightMapMins : Vector3 array,
         lightMapSizes : Vector3 array,
         lightMapAmbientColors : Color array,
         lightMapAmbientBrightnesses : single array,
         lightMapsCount : int,
         lightOrigins : Vector3 array,
         lightDirections : Vector3 array,
         lightColors : Color array,
         lightBrightnesses : single array,
         lightAttenuationLinears : single array,
         lightAttenuationQuadratics : single array,
         lightCutoffs : single array,
         lightTypes : int array,
         lightConeInners : single array,
         lightConeOuters : single array,
         lightDesireFogs : int array,
         lightShadowIndices : int array,
         lightsCount : int,
         shadowMatrices : single array array,
         material : PhysicallyBasedMaterial,
         geometry : PhysicallyBasedGeometry,
         depthTest : DepthTest,
         blending : bool,
         shader : PhysicallyBasedShader,
         vao : uint,
         vertexSize : int) =

        // only draw when there is a surface to render to avoid potentially utilizing destroyed textures
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

            // setup vao
            Gl.BindVertexArray vao
            Hl.Assert ()

            // setup shader
            Gl.UseProgram shader.PhysicallyBasedShader
            Hl.Assert ()

            // setup position-specific state
            for i in 0 .. dec (min Constants.Render.BonesMax bones.Length) do
                Gl.UniformMatrix4 (shader.BonesUniforms.[i], false, bones.[i])
            for i in 0 .. dec Constants.Render.LightMapsMaxForward do
                Gl.Uniform1 (shader.IrradianceMapsUniforms.[i], i + 12)
            for i in 0 .. dec Constants.Render.LightMapsMaxForward do
                Gl.Uniform1 (shader.EnvironmentFilterMapsUniforms.[i], i + 12 + Constants.Render.LightMapsMaxForward)
            for i in 0 .. dec Constants.Render.ShadowTexturesMax do
                Gl.Uniform1 (shader.ShadowTexturesUniforms.[i], i + 12 + Constants.Render.LightMapsMaxForward + Constants.Render.LightMapsMaxForward)
            for i in 0 .. dec Constants.Render.ShadowMapsMax do
                Gl.Uniform1 (shader.ShadowMapsUniforms.[i], i + 12 + Constants.Render.LightMapsMaxForward + Constants.Render.LightMapsMaxForward + Constants.Render.ShadowTexturesMax)
            for i in 0 .. dec Constants.Render.ShadowCascadesMax do
                Gl.Uniform1 (shader.ShadowCascadesUniforms.[i], i + 12 + Constants.Render.LightMapsMaxForward + Constants.Render.LightMapsMaxForward + Constants.Render.ShadowTexturesMax + Constants.Render.ShadowMapsMax)
            for i in 0 .. dec (min lightMapOrigins.Length Constants.Render.LightMapsMaxForward) do
                Gl.Uniform3 (shader.LightMapOriginsUniforms.[i], lightMapOrigins.[i].X, lightMapOrigins.[i].Y, lightMapOrigins.[i].Z)
            for i in 0 .. dec (min lightMapMins.Length Constants.Render.LightMapsMaxForward) do
                Gl.Uniform3 (shader.LightMapMinsUniforms.[i], lightMapMins.[i].X, lightMapMins.[i].Y, lightMapMins.[i].Z)
            for i in 0 .. dec (min lightMapSizes.Length Constants.Render.LightMapsMaxForward) do
                Gl.Uniform3 (shader.LightMapSizesUniforms.[i], lightMapSizes.[i].X, lightMapSizes.[i].Y, lightMapSizes.[i].Z)
            for i in 0 .. dec (min lightMapAmbientColors.Length Constants.Render.LightMapsMaxForward) do
                Gl.Uniform3 (shader.LightMapAmbientColorsUniforms.[i], lightMapAmbientColors.[i].R, lightMapAmbientColors.[i].G, lightMapAmbientColors.[i].B)
            for i in 0 .. dec (min lightMapAmbientBrightnesses.Length Constants.Render.LightMapsMaxForward) do
                Gl.Uniform1 (shader.LightMapAmbientBrightnessesUniforms.[i], lightMapAmbientBrightnesses.[i])
            Gl.Uniform1 (shader.LightMapsCountUniform, lightMapsCount)
            for i in 0 .. dec (min lightOrigins.Length Constants.Render.LightsMaxForward) do
                Gl.Uniform3 (shader.LightOriginsUniforms.[i], lightOrigins.[i].X, lightOrigins.[i].Y, lightOrigins.[i].Z)
            for i in 0 .. dec (min lightDirections.Length Constants.Render.LightsMaxForward) do
                Gl.Uniform3 (shader.LightDirectionsUniforms.[i], lightDirections.[i].X, lightDirections.[i].Y, lightDirections.[i].Z)
            for i in 0 .. dec (min lightColors.Length Constants.Render.LightsMaxForward) do
                Gl.Uniform3 (shader.LightColorsUniforms.[i], lightColors.[i].R, lightColors.[i].G, lightColors.[i].B)
            for i in 0 .. dec (min lightBrightnesses.Length Constants.Render.LightsMaxForward) do
                Gl.Uniform1 (shader.LightBrightnessesUniforms.[i], lightBrightnesses.[i])
            for i in 0 .. dec (min lightAttenuationLinears.Length Constants.Render.LightsMaxForward) do
                Gl.Uniform1 (shader.LightAttenuationLinearsUniforms.[i], lightAttenuationLinears.[i])
            for i in 0 .. dec (min lightAttenuationQuadratics.Length Constants.Render.LightsMaxForward) do
                Gl.Uniform1 (shader.LightAttenuationQuadraticsUniforms.[i], lightAttenuationQuadratics.[i])
            for i in 0 .. dec (min lightCutoffs.Length Constants.Render.LightsMaxForward) do
                Gl.Uniform1 (shader.LightCutoffsUniforms.[i], lightCutoffs.[i])
            for i in 0 .. dec (min lightTypes.Length Constants.Render.LightsMaxForward) do
                Gl.Uniform1 (shader.LightTypesUniforms.[i], lightTypes.[i])
            for i in 0 .. dec (min lightConeInners.Length Constants.Render.LightsMaxForward) do
                Gl.Uniform1 (shader.LightConeInnersUniforms.[i], lightConeInners.[i])
            for i in 0 .. dec (min lightConeOuters.Length Constants.Render.LightsMaxForward) do
                Gl.Uniform1 (shader.LightConeOutersUniforms.[i], lightConeOuters.[i])
            for i in 0 .. dec (min lightDesireFogs.Length Constants.Render.LightsMaxForward) do
                Gl.Uniform1 (shader.LightDesireFogsUniforms.[i], lightDesireFogs.[i])
            for i in 0 .. dec (min lightShadowIndices.Length Constants.Render.LightsMaxForward) do
                Gl.Uniform1 (shader.LightShadowIndicesUniforms.[i], lightShadowIndices.[i])
            Gl.Uniform1 (shader.LightsCountUniform, lightsCount)
            for i in 0 .. dec (min shadowMatrices.Length (Constants.Render.ShadowTexturesMax + Constants.Render.ShadowCascadesMax * Constants.Render.ShadowCascadeLevels)) do
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
            // NOTE: textures 7 through 9 are configured in begin / end functions.
            for i in 0 .. dec (min irradianceMaps.Length Constants.Render.LightMapsMaxForward) do
                Gl.ActiveTexture (int TextureUnit.Texture0 + 12 + i |> Branchless.reinterpret)
                Gl.BindTexture (TextureTarget.TextureCubeMap, irradianceMaps.[i].TextureId)
            for i in 0 .. dec (min environmentFilterMaps.Length Constants.Render.LightMapsMaxForward) do
                Gl.ActiveTexture (int TextureUnit.Texture0 + 12 + i + Constants.Render.LightMapsMaxForward |> Branchless.reinterpret)
                Gl.BindTexture (TextureTarget.TextureCubeMap, environmentFilterMaps.[i].TextureId)
            for i in 0 .. dec (min shadowTextures.Length Constants.Render.ShadowTexturesMax) do
                Gl.ActiveTexture (int TextureUnit.Texture0 + 12 + i + Constants.Render.LightMapsMaxForward + Constants.Render.LightMapsMaxForward |> Branchless.reinterpret)
                Gl.BindTexture (TextureTarget.Texture2d, shadowTextures.[i].TextureId)
            for i in 0 .. dec (min shadowMaps.Length Constants.Render.ShadowMapsMax) do
                Gl.ActiveTexture (int TextureUnit.Texture0 + 12 + i + Constants.Render.LightMapsMaxForward + Constants.Render.LightMapsMaxForward + Constants.Render.ShadowTexturesMax |> Branchless.reinterpret)
                Gl.BindTexture (TextureTarget.TextureCubeMap, shadowMaps.[i].TextureId)
            for i in 0 .. dec (min shadowCascades.Length Constants.Render.ShadowCascadesMax) do
                Gl.ActiveTexture (int TextureUnit.Texture0 + 12 + i + Constants.Render.LightMapsMaxForward + Constants.Render.LightMapsMaxForward + Constants.Render.ShadowTexturesMax + Constants.Render.ShadowMapsMax |> Branchless.reinterpret)
                Gl.BindTexture (TextureTarget.Texture2dArray, shadowCascades.[i].TextureId)
            Hl.Assert ()

            // update instance buffer
            let instanceFieldsPtr = GCHandle.Alloc (instanceFields, GCHandleType.Pinned)
            try Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.InstanceBuffer)
                Gl.BufferData (BufferTarget.ArrayBuffer, uint (surfacesCount * Constants.Render.InstanceFieldCount * sizeof<single>), instanceFieldsPtr.AddrOfPinnedObject (), BufferUsage.StreamDraw)
                Gl.BindBuffer (BufferTarget.ArrayBuffer, 0u)
                Hl.Assert ()
            finally instanceFieldsPtr.Free ()

            // setup geometry
            Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, vertexSize)
            Gl.VertexArrayVertexBuffer (vao, 1u, geometry.InstanceBuffer, 0, Constants.Render.InstanceFieldCount * sizeof<single>)
            Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
            Hl.Assert ()

            // draw geometry
            Gl.DrawElementsInstanced (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0, surfacesCount)
            Hl.ReportDrawCall surfacesCount
            Hl.Assert ()

            // teardown shader
            Gl.UseProgram 0u
            Hl.Assert ()

            // teardown vao
            Gl.BindVertexArray 0u
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

    /// End the process of drawing with a forward shader.
    let EndPhysicallyBasedForwardShader (_ : PhysicallyBasedShader, _ : uint) =
        () // nothing to do

    let DrawPhysicallyBasedTerrain
        (view : single array,
         projection : single array,
         viewProjection : single array,
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
         shader : PhysicallyBasedDeferredTerrainShader,
         vao : uint) =

        // setup state
        Gl.DepthFunc DepthFunction.Lequal
        Gl.Enable EnableCap.DepthTest
        Gl.Enable EnableCap.CullFace
        Hl.Assert ()

        // enforce layer limit
        let layersCount = min materials.Length Constants.Render.TerrainLayersMax

        // setup vao
        Gl.BindVertexArray vao
        Hl.Assert ()
        
        // setup shader
        Gl.UseProgram shader.PhysicallyBasedShader
        Gl.UniformMatrix4 (shader.ViewUniform, false, view)
        Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
        Gl.UniformMatrix4 (shader.ViewProjectionUniform, false, viewProjection)
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
        Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, TerrainVertexSize)
        Gl.VertexArrayVertexBuffer (vao, 1u, geometry.InstanceBuffer, 0, Constants.Render.InstanceFieldCount * sizeof<single>)
        Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, elementsCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown vao
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown state
        Gl.DepthFunc DepthFunction.Less
        Gl.Disable EnableCap.DepthTest
        Gl.Disable EnableCap.CullFace

    /// Draw the light mapping pass of a deferred physically-based surface.
    let DrawPhysicallyBasedDeferredLightMappingSurface
        (eyeCenter : Vector3,
         viewInverse : single array,
         projectionInverse : single array,
         depthTexture : Texture.Texture,
         normalPlusTexture : Texture.Texture,
         lightMapOrigins : Vector3 array,
         lightMapMins : Vector3 array,
         lightMapSizes : Vector3 array,
         lightMapsCount : int,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedDeferredLightMappingShader,
         vao : uint) =

        // setup vao
        Gl.BindVertexArray vao
        Hl.Assert ()

        // setup shader
        Gl.UseProgram shader.PhysicallyBasedDeferredLightMappingShader
        Gl.Uniform3 (shader.EyeCenterUniform, eyeCenter.X, eyeCenter.Y, eyeCenter.Z)
        Gl.UniformMatrix4 (shader.ViewInverseUniform, false, viewInverse)
        Gl.UniformMatrix4 (shader.ProjectionInverseUniform, false, projectionInverse)
        Gl.Uniform1 (shader.DepthTextureUniform, 0)
        Gl.Uniform1 (shader.NormalPlusTextureUniform, 1)
        for i in 0 .. dec (min lightMapOrigins.Length Constants.Render.LightMapsMaxDeferred) do
            Gl.Uniform3 (shader.LightMapOriginsUniforms.[i], lightMapOrigins.[i].X, lightMapOrigins.[i].Y, lightMapOrigins.[i].Z)
        for i in 0 .. dec (min lightMapMins.Length Constants.Render.LightMapsMaxDeferred) do
            Gl.Uniform3 (shader.LightMapMinsUniforms.[i], lightMapMins.[i].X, lightMapMins.[i].Y, lightMapMins.[i].Z)
        for i in 0 .. dec (min lightMapSizes.Length Constants.Render.LightMapsMaxDeferred) do
            Gl.Uniform3 (shader.LightMapSizesUniforms.[i], lightMapSizes.[i].X, lightMapSizes.[i].Y, lightMapSizes.[i].Z)
        Gl.Uniform1 (shader.LightMapsCountUniform, lightMapsCount)
        Hl.Assert ()

        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, depthTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture1
        Gl.BindTexture (TextureTarget.Texture2d, normalPlusTexture.TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, StaticVertexSize)
        Gl.VertexArrayVertexBuffer (vao, 1u, geometry.InstanceBuffer, 0, Constants.Render.InstanceFieldCount * sizeof<single>)
        Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown vao
        Gl.BindVertexArray 0u

    /// Draw the ambient pass of a deferred physically-based surface.
    let DrawPhysicallyBasedDeferredAmbientSurface
        (eyeCenter : Vector3,
         viewInverse : single array,
         projectionInverse : single array,
         depthTexture : Texture.Texture,
         lightMappingTexture : Texture.Texture,
         lightMapAmbientColor : Color,
         lightMapAmbientBrightness : single,
         lightMapAmbientColors : Color array,
         lightMapAmbientBrightnesses : single array,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedDeferredAmbientShader,
         vao : uint) =

        // setup vao
        Gl.BindVertexArray vao
        Hl.Assert ()

        // setup shader
        Gl.UseProgram shader.PhysicallyBasedDeferredAmbientShader
        Gl.Uniform3 (shader.EyeCenterUniform, eyeCenter.X, eyeCenter.Y, eyeCenter.Z)
        Gl.UniformMatrix4 (shader.ViewInverseUniform, false, viewInverse)
        Gl.UniformMatrix4 (shader.ProjectionInverseUniform, false, projectionInverse)
        Gl.Uniform1 (shader.DepthTextureUniform, 0)
        Gl.Uniform1 (shader.LightMappingTextureUniform, 1)
        Gl.Uniform3 (shader.LightMapAmbientColorUniform, lightMapAmbientColor.R, lightMapAmbientColor.G, lightMapAmbientColor.B)
        Gl.Uniform1 (shader.LightMapAmbientBrightnessUniform, lightMapAmbientBrightness)
        for i in 0 .. dec (min lightMapAmbientColors.Length Constants.Render.LightMapsMaxDeferred) do
            Gl.Uniform3 (shader.LightMapAmbientColorsUniforms.[i], lightMapAmbientColors.[i].R, lightMapAmbientColors.[i].G, lightMapAmbientColors.[i].B)
        for i in 0 .. dec (min lightMapAmbientBrightnesses.Length Constants.Render.LightMapsMaxDeferred) do
            Gl.Uniform1 (shader.LightMapAmbientBrightnessesUniforms.[i], lightMapAmbientBrightnesses.[i])
        Hl.Assert ()

        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, depthTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture1
        Gl.BindTexture (TextureTarget.Texture2d, lightMappingTexture.TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, StaticVertexSize)
        Gl.VertexArrayVertexBuffer (vao, 1u, geometry.InstanceBuffer, 0, Constants.Render.InstanceFieldCount * sizeof<single>)
        Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown vao
        Gl.BindVertexArray 0u

    /// Draw the irradiance pass of a deferred physically-based surface.
    let DrawPhysicallyBasedDeferredIrradianceSurface
        (eyeCenter : Vector3,
         viewInverse : single array,
         projectionInverse : single array,
         depthTexture : Texture.Texture,
         normalPlusTexture : Texture.Texture,
         lightMappingTexture : Texture.Texture,
         irradianceMap : Texture.Texture,
         irradianceMaps : Texture.Texture array,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedDeferredIrradianceShader,
         vao : uint) =

        // setup vao
        Gl.BindVertexArray vao
        Hl.Assert ()

        // setup shader
        Gl.UseProgram shader.PhysicallyBasedDeferredIrradianceShader
        Gl.Uniform3 (shader.EyeCenterUniform, eyeCenter.X, eyeCenter.Y, eyeCenter.Z)
        Gl.UniformMatrix4 (shader.ViewInverseUniform, false, viewInverse)
        Gl.UniformMatrix4 (shader.ProjectionInverseUniform, false, projectionInverse)
        Gl.Uniform1 (shader.DepthTextureUniform, 0)
        Gl.Uniform1 (shader.NormalPlusTextureUniform, 1)
        Gl.Uniform1 (shader.LightMappingTextureUniform, 2)
        Gl.Uniform1 (shader.IrradianceMapUniform, 3)
        for i in 0 .. dec Constants.Render.LightMapsMaxDeferred do
            Gl.Uniform1 (shader.IrradianceMapsUniforms.[i], 4 + i)
        Hl.Assert ()

        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, depthTexture.TextureId)
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
        Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, StaticVertexSize)
        Gl.VertexArrayVertexBuffer (vao, 1u, geometry.InstanceBuffer, 0, Constants.Render.InstanceFieldCount * sizeof<single>)
        Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown vao
        Gl.BindVertexArray 0u

    /// Draw the environment filter pass of a deferred physically-based surface.
    let DrawPhysicallyBasedDeferredEnvironmentFilterSurface
        (eyeCenter : Vector3,
         viewInverse : single array,
         projectionInverse : single array,
         depthTexture : Texture.Texture,
         materialTexture : Texture.Texture,
         normalPlusTexture : Texture.Texture,
         lightMappingTexture : Texture.Texture,
         environmentFilterMap : Texture.Texture,
         environmentFilterMaps : Texture.Texture array,
         lightMapOrigins : Vector3 array,
         lightMapMins : Vector3 array,
         lightMapSizes : Vector3 array,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedDeferredEnvironmentFilterShader,
         vao : uint) =

        // setup vao
        Gl.BindVertexArray vao
        Hl.Assert ()

        // setup shader
        Gl.UseProgram shader.PhysicallyBasedDeferredEnvironmentFilterShader
        Gl.Uniform3 (shader.EyeCenterUniform, eyeCenter.X, eyeCenter.Y, eyeCenter.Z)
        Gl.UniformMatrix4 (shader.ViewInverseUniform, false, viewInverse)
        Gl.UniformMatrix4 (shader.ProjectionInverseUniform, false, projectionInverse)
        Gl.Uniform1 (shader.DepthTextureUniform, 0)
        Gl.Uniform1 (shader.MaterialTextureUniform, 1)
        Gl.Uniform1 (shader.NormalPlusTextureUniform, 2)
        Gl.Uniform1 (shader.LightMappingTextureUniform, 3)
        Gl.Uniform1 (shader.EnvironmentFilterMapUniform, 4)
        for i in 0 .. dec Constants.Render.LightMapsMaxDeferred do
            Gl.Uniform1 (shader.EnvironmentFilterMapsUniforms.[i], 5 + i)
        for i in 0 .. dec (min lightMapOrigins.Length Constants.Render.LightMapsMaxDeferred) do
            Gl.Uniform3 (shader.LightMapOriginsUniforms.[i], lightMapOrigins.[i].X, lightMapOrigins.[i].Y, lightMapOrigins.[i].Z)
        for i in 0 .. dec (min lightMapMins.Length Constants.Render.LightMapsMaxDeferred) do
            Gl.Uniform3 (shader.LightMapMinsUniforms.[i], lightMapMins.[i].X, lightMapMins.[i].Y, lightMapMins.[i].Z)
        for i in 0 .. dec (min lightMapSizes.Length Constants.Render.LightMapsMaxDeferred) do
            Gl.Uniform3 (shader.LightMapSizesUniforms.[i], lightMapSizes.[i].X, lightMapSizes.[i].Y, lightMapSizes.[i].Z)
        Hl.Assert ()

        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, depthTexture.TextureId)
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
        Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, StaticVertexSize)
        Gl.VertexArrayVertexBuffer (vao, 1u, geometry.InstanceBuffer, 0, Constants.Render.InstanceFieldCount * sizeof<single>)
        Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown vao
        Gl.BindVertexArray 0u

    /// Draw the ssao pass of a deferred physically-based surface.
    let DrawPhysicallyBasedDeferredSsaoSurface
        (eyeCenter : Vector3,
         view : single array,
         viewInverse : single array,
         projection : single array,
         projectionInverse : single array,
         viewProjection : single array,
         depthTexture : Texture.Texture,
         normalPlusTexture : Texture.Texture,
         ssaoResolution : int array,
         ssaoIntensity : single,
         ssaoBias : single,
         ssaoRadius : single,
         ssaoDistanceMax : single,
         ssaoSampleCount : int,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedDeferredSsaoShader,
         vao : uint) =

        // setup vao
        Gl.BindVertexArray vao
        Hl.Assert ()

        // setup shader
        Gl.UseProgram shader.PhysicallyBasedDeferredSsaoShader
        Gl.Uniform3 (shader.EyeCenterUniform, eyeCenter.X, eyeCenter.Y, eyeCenter.Z)
        Gl.UniformMatrix4 (shader.ViewUniform, false, view)
        Gl.UniformMatrix4 (shader.ViewInverseUniform, false, viewInverse)
        Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
        Gl.UniformMatrix4 (shader.ProjectionInverseUniform, false, projectionInverse)
        Gl.UniformMatrix4 (shader.ViewProjectionUniform, false, viewProjection)
        Gl.Uniform1 (shader.DepthTextureUniform, 0)
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
        Gl.BindTexture (TextureTarget.Texture2d, depthTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture1
        Gl.BindTexture (TextureTarget.Texture2d, normalPlusTexture.TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, StaticVertexSize)
        Gl.VertexArrayVertexBuffer (vao, 1u, geometry.InstanceBuffer, 0, Constants.Render.InstanceFieldCount * sizeof<single>)
        Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown vao
        Gl.BindVertexArray 0u

    /// Draw the lighting pass of a deferred physically-based surface.
    let DrawPhysicallyBasedDeferredLightingSurface
        (eyeCenter : Vector3,
         view : single array,
         viewInverse : single array,
         projection : single array,
         projectionInverse : single array,
         lightCutoffMargin : single,
         lightShadowSamples : int,
         lightShadowBias : single,
         lightShadowSampleScalar : single,
         lightShadowExponent : single,
         lightShadowDensity : single,
         sssEnabled : int,
         ssvfEnabled : int,
         ssvfSteps : int,
         ssvfAsymmetry : single,
         ssvfIntensity : single,
         depthTexture : Texture.Texture,
         albedoTexture : Texture.Texture,
         materialTexture : Texture.Texture,
         normalPlusTexture : Texture.Texture,
         subdermalPlusTexture : Texture.Texture,
         scatterPlusTexture : Texture.Texture,
         shadowTextures : Texture.Texture array,
         shadowMaps : Texture.Texture array,
         shadowCascades : Texture.Texture array,
         lightOrigins : Vector3 array,
         lightDirections : Vector3 array,
         lightColors : Color array,
         lightBrightnesses : single array,
         lightAttenuationLinears : single array,
         lightAttenuationQuadratics : single array,
         lightCutoffs : single array,
         lightTypes : int array,
         lightConeInners : single array,
         lightConeOuters : single array,
         lightDesireFogs : int array,
         lightShadowIndices : int array,
         lightsCount : int,
         shadowNear : single,
         shadowMatrices : single array array,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedDeferredLightingShader,
         vao : uint) =

        // setup vao
        Gl.BindVertexArray vao
        Hl.Assert ()

        // setup shader
        Gl.UseProgram shader.PhysicallyBasedDeferredLightingShader
        Gl.Uniform3 (shader.EyeCenterUniform, eyeCenter.X, eyeCenter.Y, eyeCenter.Z)
        Gl.UniformMatrix4 (shader.ViewUniform, false, view)
        Gl.UniformMatrix4 (shader.ViewInverseUniform, false, viewInverse)
        Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
        Gl.UniformMatrix4 (shader.ProjectionInverseUniform, false, projectionInverse)
        Gl.Uniform1 (shader.LightCutoffMarginUniform, lightCutoffMargin)
        Gl.Uniform1 (shader.LightShadowSamplesUniform, lightShadowSamples)
        Gl.Uniform1 (shader.LightShadowBiasUniform, lightShadowBias)
        Gl.Uniform1 (shader.LightShadowSampleScalarUniform, lightShadowSampleScalar)
        Gl.Uniform1 (shader.LightShadowExponentUniform, lightShadowExponent)
        Gl.Uniform1 (shader.LightShadowDensityUniform, lightShadowDensity)
        Gl.Uniform1 (shader.SssEnabledUniform, sssEnabled)
        Gl.Uniform1 (shader.SsvfEnabledUniform, ssvfEnabled)
        Gl.Uniform1 (shader.SsvfStepsUniform, ssvfSteps)
        Gl.Uniform1 (shader.SsvfAsymmetryUniform, ssvfAsymmetry)
        Gl.Uniform1 (shader.SsvfIntensityUniform, ssvfIntensity)
        Gl.Uniform1 (shader.DepthTextureUniform, 0)
        Gl.Uniform1 (shader.AlbedoTextureUniform, 1)
        Gl.Uniform1 (shader.MaterialTextureUniform, 2)
        Gl.Uniform1 (shader.NormalPlusTextureUniform, 3)
        Gl.Uniform1 (shader.SubdermalPlusTextureUniform, 4)
        Gl.Uniform1 (shader.ScatterPlusTextureUniform, 5)
        for i in 0 .. dec Constants.Render.ShadowTexturesMax do
            Gl.Uniform1 (shader.ShadowTexturesUniforms.[i], i + 6)
        for i in 0 .. dec Constants.Render.ShadowMapsMax do
            Gl.Uniform1 (shader.ShadowMapsUniforms.[i], i + 6 + Constants.Render.ShadowTexturesMax)
        for i in 0 .. dec Constants.Render.ShadowCascadesMax do
            Gl.Uniform1 (shader.ShadowCascadesUniforms.[i], i + 6 + Constants.Render.ShadowTexturesMax + Constants.Render.ShadowMapsMax)
        for i in 0 .. dec (min lightOrigins.Length Constants.Render.LightsMaxDeferred) do
            Gl.Uniform3 (shader.LightOriginsUniforms.[i], lightOrigins.[i].X, lightOrigins.[i].Y, lightOrigins.[i].Z)
        for i in 0 .. dec (min lightDirections.Length Constants.Render.LightsMaxDeferred) do
            Gl.Uniform3 (shader.LightDirectionsUniforms.[i], lightDirections.[i].X, lightDirections.[i].Y, lightDirections.[i].Z)
        for i in 0 .. dec (min lightColors.Length Constants.Render.LightsMaxDeferred) do
            Gl.Uniform3 (shader.LightColorsUniforms.[i], lightColors.[i].R, lightColors.[i].G, lightColors.[i].B)
        for i in 0 .. dec (min lightBrightnesses.Length Constants.Render.LightsMaxDeferred) do
            Gl.Uniform1 (shader.LightBrightnessesUniforms.[i], lightBrightnesses.[i])
        for i in 0 .. dec (min lightAttenuationLinears.Length Constants.Render.LightsMaxDeferred) do
            Gl.Uniform1 (shader.LightAttenuationLinearsUniforms.[i], lightAttenuationLinears.[i])
        for i in 0 .. dec (min lightAttenuationQuadratics.Length Constants.Render.LightsMaxDeferred) do
            Gl.Uniform1 (shader.LightAttenuationQuadraticsUniforms.[i], lightAttenuationQuadratics.[i])
        for i in 0 .. dec (min lightCutoffs.Length Constants.Render.LightsMaxDeferred) do
            Gl.Uniform1 (shader.LightCutoffsUniforms.[i], lightCutoffs.[i])
        for i in 0 .. dec (min lightTypes.Length Constants.Render.LightsMaxDeferred) do
            Gl.Uniform1 (shader.LightTypesUniforms.[i], lightTypes.[i])
        for i in 0 .. dec (min lightConeInners.Length Constants.Render.LightsMaxDeferred) do
            Gl.Uniform1 (shader.LightConeInnersUniforms.[i], lightConeInners.[i])
        for i in 0 .. dec (min lightConeOuters.Length Constants.Render.LightsMaxDeferred) do
            Gl.Uniform1 (shader.LightConeOutersUniforms.[i], lightConeOuters.[i])
        for i in 0 .. dec (min lightDesireFogs.Length Constants.Render.LightsMaxDeferred) do
            Gl.Uniform1 (shader.LightDesireFogsUniforms.[i], lightDesireFogs.[i])
        for i in 0 .. dec (min lightShadowIndices.Length Constants.Render.LightsMaxDeferred) do
            Gl.Uniform1 (shader.LightShadowIndicesUniforms.[i], lightShadowIndices.[i])
        Gl.Uniform1 (shader.LightsCountUniform, lightsCount)
        Gl.Uniform1 (shader.ShadowNearUniform, shadowNear)
        for i in 0 .. dec (min shadowMatrices.Length (Constants.Render.ShadowTexturesMax + Constants.Render.ShadowCascadesMax * Constants.Render.ShadowCascadeLevels)) do
            Gl.UniformMatrix4 (shader.ShadowMatricesUniforms.[i], false, shadowMatrices.[i])
        Hl.Assert ()

        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, depthTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture1
        Gl.BindTexture (TextureTarget.Texture2d, albedoTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture2
        Gl.BindTexture (TextureTarget.Texture2d, materialTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture3
        Gl.BindTexture (TextureTarget.Texture2d, normalPlusTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture4
        Gl.BindTexture (TextureTarget.Texture2d, subdermalPlusTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture5
        Gl.BindTexture (TextureTarget.Texture2d, scatterPlusTexture.TextureId)
        for i in 0 .. dec (min shadowTextures.Length Constants.Render.ShadowTexturesMax) do
            Gl.ActiveTexture (int TextureUnit.Texture0 + 6 + i |> Branchless.reinterpret)
            Gl.BindTexture (TextureTarget.Texture2d, shadowTextures.[i].TextureId)
        for i in 0 .. dec (min shadowMaps.Length Constants.Render.ShadowMapsMax) do
            Gl.ActiveTexture (int TextureUnit.Texture0 + 6 + i + Constants.Render.ShadowTexturesMax |> Branchless.reinterpret)
            Gl.BindTexture (TextureTarget.TextureCubeMap, shadowMaps.[i].TextureId)
        for i in 0 .. dec (min shadowCascades.Length Constants.Render.ShadowCascadesMax) do
            Gl.ActiveTexture (int TextureUnit.Texture0 + 6 + i + Constants.Render.ShadowTexturesMax + Constants.Render.ShadowMapsMax |> Branchless.reinterpret)
            Gl.BindTexture (TextureTarget.Texture2dArray, shadowCascades.[i].TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, StaticVertexSize)
        Gl.VertexArrayVertexBuffer (vao, 1u, geometry.InstanceBuffer, 0, Constants.Render.InstanceFieldCount * sizeof<single>)
        Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown vao
        Gl.BindVertexArray 0u

    /// Draw the coloring pass of a deferred physically-based surface.
    let DrawPhysicallyBasedDeferredColoringSurface
        (eyeCenter : Vector3,
         view : single array,
         viewInverse : single array,
         projection : single array,
         projectionInverse : single array,
         lightAmbientBoostCutoff : single,
         lightAmbientBoostScalar : single,
         ssrlEnabled : int,
         ssrlIntensity : single,
         ssrlDetail : single,
         ssrlRefinementsMax : int,
         ssrlRayThickness : single,
         ssrlTowardEyeCutoff : single,
         ssrlDepthCutoff : single,
         ssrlDepthCutoffMargin : single,
         ssrlDistanceCutoff : single,
         ssrlDistanceCutoffMargin : single,
         ssrlRoughnessCutoff : single,
         ssrlRoughnessCutoffMargin : single,
         ssrlSlopeCutoff : single,
         ssrlSlopeCutoffMargin : single,
         ssrlEdgeHorizontalMargin : single,
         ssrlEdgeVerticalMargin : single,
         depthTexture : Texture.Texture,
         albedoTexture : Texture.Texture,
         materialTexture : Texture.Texture,
         normalPlusTexture : Texture.Texture,
         lightAccumTexture : Texture.Texture,
         brdfTexture : Texture.Texture,
         ambientTexture : Texture.Texture,
         irradianceTexture : Texture.Texture,
         environmentFilterTexture : Texture.Texture,
         ssaoTexture : Texture.Texture,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedDeferredColoringShader,
         vao : uint) =

        // setup vao
        Gl.BindVertexArray vao
        Hl.Assert ()

        // setup shader
        Gl.UseProgram shader.PhysicallyBasedDeferredColoringShader
        Gl.Uniform3 (shader.EyeCenterUniform, eyeCenter.X, eyeCenter.Y, eyeCenter.Z)
        Gl.UniformMatrix4 (shader.ViewUniform, false, view)
        Gl.UniformMatrix4 (shader.ViewInverseUniform, false, viewInverse)
        Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
        Gl.UniformMatrix4 (shader.ProjectionInverseUniform, false, projectionInverse)
        Gl.Uniform1 (shader.LightAmbientBoostCutoffUniform, lightAmbientBoostCutoff)
        Gl.Uniform1 (shader.LightAmbientBoostScalarUniform, lightAmbientBoostScalar)
        Gl.Uniform1 (shader.SsrlEnabledUniform, ssrlEnabled)
        Gl.Uniform1 (shader.SsrlIntensityUniform, ssrlIntensity)
        Gl.Uniform1 (shader.SsrlDetailUniform, ssrlDetail)
        Gl.Uniform1 (shader.SsrlRefinementsMaxUniform, ssrlRefinementsMax)
        Gl.Uniform1 (shader.SsrlRayThicknessUniform, ssrlRayThickness)
        Gl.Uniform1 (shader.SsrlTowardEyeCutoffUniform, ssrlTowardEyeCutoff)
        Gl.Uniform1 (shader.SsrlDepthCutoffUniform, ssrlDepthCutoff)
        Gl.Uniform1 (shader.SsrlDepthCutoffMarginUniform, ssrlDepthCutoffMargin)
        Gl.Uniform1 (shader.SsrlDistanceCutoffUniform, ssrlDistanceCutoff)
        Gl.Uniform1 (shader.SsrlDistanceCutoffMarginUniform, ssrlDistanceCutoffMargin)
        Gl.Uniform1 (shader.SsrlRoughnessCutoffUniform, ssrlRoughnessCutoff)
        Gl.Uniform1 (shader.SsrlRoughnessCutoffMarginUniform, ssrlRoughnessCutoffMargin)
        Gl.Uniform1 (shader.SsrlSlopeCutoffUniform, ssrlSlopeCutoff)
        Gl.Uniform1 (shader.SsrlSlopeCutoffMarginUniform, ssrlSlopeCutoffMargin)
        Gl.Uniform1 (shader.SsrlEdgeHorizontalMarginUniform, ssrlEdgeHorizontalMargin)
        Gl.Uniform1 (shader.SsrlEdgeVerticalMarginUniform, ssrlEdgeVerticalMargin)
        Gl.Uniform1 (shader.DepthTextureUniform, 0)
        Gl.Uniform1 (shader.AlbedoTextureUniform, 1)
        Gl.Uniform1 (shader.MaterialTextureUniform, 2)
        Gl.Uniform1 (shader.NormalPlusTextureUniform, 3)
        Gl.Uniform1 (shader.LightAccumTextureUniform, 4)
        Gl.Uniform1 (shader.BrdfTextureUniform, 5)
        Gl.Uniform1 (shader.AmbientTextureUniform, 6)
        Gl.Uniform1 (shader.IrradianceTextureUniform, 7)
        Gl.Uniform1 (shader.EnvironmentFilterTextureUniform, 8)
        Gl.Uniform1 (shader.SsaoTextureUniform, 9)
        Hl.Assert ()

        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, depthTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture1
        Gl.BindTexture (TextureTarget.Texture2d, albedoTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture2
        Gl.BindTexture (TextureTarget.Texture2d, materialTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture3
        Gl.BindTexture (TextureTarget.Texture2d, normalPlusTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture4
        Gl.BindTexture (TextureTarget.Texture2d, lightAccumTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture5
        Gl.BindTexture (TextureTarget.Texture2d, brdfTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture6
        Gl.BindTexture (TextureTarget.Texture2d, ambientTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture7
        Gl.BindTexture (TextureTarget.Texture2d, irradianceTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture8
        Gl.BindTexture (TextureTarget.Texture2d, environmentFilterTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture9
        Gl.BindTexture (TextureTarget.Texture2d, ssaoTexture.TextureId)

        // setup geometry
        Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, StaticVertexSize)
        Gl.VertexArrayVertexBuffer (vao, 1u, geometry.InstanceBuffer, 0, Constants.Render.InstanceFieldCount * sizeof<single>)
        Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown vao
        Gl.BindVertexArray 0u

    /// Draw the bilateral up-sample pass of a deferred physically-based surface.
    let DrawPhysicallyBasedDeferredCompositionSurface
        (eyeCenter : Vector3,
         viewInverse : single array,
         projectionInverse : single array,
         fogEnabled : int,
         fogType : int,
         fogStart : single,
         fogFinish : single,
         fogDensity : single,
         fogColor : Color,
         depthTexture : Texture.Texture,
         colorTexture : Texture.Texture,
         fogAccumTexture : Texture.Texture,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedDeferredCompositionShader,
         vao : uint) =

        // setup vao
        Gl.BindVertexArray vao
        Hl.Assert ()

        // setup shader
        Gl.UseProgram shader.PhysicallyBasedDeferredCompositionShader
        Gl.Uniform3 (shader.EyeCenterUniform, eyeCenter.X, eyeCenter.Y, eyeCenter.Z)
        Gl.UniformMatrix4 (shader.ViewInverseUniform, false, viewInverse)
        Gl.UniformMatrix4 (shader.ProjectionInverseUniform, false, projectionInverse)
        Gl.Uniform1 (shader.FogEnabledUniform, fogEnabled)
        Gl.Uniform1 (shader.FogTypeUniform, fogType)
        Gl.Uniform1 (shader.FogStartUniform, fogStart)
        Gl.Uniform1 (shader.FogFinishUniform, fogFinish)
        Gl.Uniform1 (shader.FogDensityUniform, fogDensity)
        Gl.Uniform4 (shader.FogColorUniform, fogColor.R, fogColor.G, fogColor.B, fogColor.A)
        Gl.Uniform1 (shader.DepthTextureUniform, 0)
        Gl.Uniform1 (shader.ColorTextureUniform, 1)
        Gl.Uniform1 (shader.FogAccumTextureUniform, 2)
        Hl.Assert ()

        // setup textures
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, depthTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture1
        Gl.BindTexture (TextureTarget.Texture2d, colorTexture.TextureId)
        Gl.ActiveTexture TextureUnit.Texture2
        Gl.BindTexture (TextureTarget.Texture2d, fogAccumTexture.TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, StaticVertexSize)
        Gl.VertexArrayVertexBuffer (vao, 1u, geometry.InstanceBuffer, 0, Constants.Render.InstanceFieldCount * sizeof<single>)
        Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown vao
        Gl.BindVertexArray 0u

    /// Destroy physically-based geometry resources.
    let DestroyPhysicallyBasedGeometry geometry =
        Gl.DeleteBuffers [|geometry.VertexBuffer|]
        Gl.DeleteBuffers [|geometry.InstanceBuffer|]
        Gl.DeleteBuffers [|geometry.IndexBuffer|]

    /// Destroy physically-based model resources.
    let DestroyPhysicallyBasedModel (model : PhysicallyBasedModel) =
        for surface in model.Surfaces do
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
                        scene.ClearColorData () // avoid polluting memory with unused color data
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
                                yield PhysicallyBasedSurface surface|]
                            |> TreeNode)

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

    /// Physically-based shaders.
    type PhysicallyBasedShaders =
        { ShadowStaticPointShader : PhysicallyBasedShader
          ShadowStaticSpotShader : PhysicallyBasedShader
          ShadowStaticDirectionalShader : PhysicallyBasedShader
          ShadowAnimatedPointShader : PhysicallyBasedShader
          ShadowAnimatedSpotShader : PhysicallyBasedShader
          ShadowAnimatedDirectionalShader : PhysicallyBasedShader
          ShadowTerrainPointShader : PhysicallyBasedDeferredTerrainShader
          ShadowTerrainSpotShader : PhysicallyBasedDeferredTerrainShader
          ShadowTerrainDirectionalShader : PhysicallyBasedDeferredTerrainShader
          DeferredStaticShader : PhysicallyBasedShader
          DeferredStaticClippedShader : PhysicallyBasedShader
          DeferredAnimatedShader : PhysicallyBasedShader
          DeferredTerrainShader : PhysicallyBasedDeferredTerrainShader
          DeferredLightMappingShader : PhysicallyBasedDeferredLightMappingShader
          DeferredAmbientShader : PhysicallyBasedDeferredAmbientShader
          DeferredIrradianceShader : PhysicallyBasedDeferredIrradianceShader
          DeferredEnvironmentFilterShader : PhysicallyBasedDeferredEnvironmentFilterShader
          DeferredSsaoShader : PhysicallyBasedDeferredSsaoShader
          DeferredLightingShader : PhysicallyBasedDeferredLightingShader
          DeferredColoringShader : PhysicallyBasedDeferredColoringShader
          DeferredCompositionShader : PhysicallyBasedDeferredCompositionShader
          ForwardStaticShader : PhysicallyBasedShader
          ForwardAnimatedShader : PhysicallyBasedShader }

    let CreatePhysicallyBasedShaders (lightMapsMax, lightsMax) =

        // create shadow shaders
        let shadowStaticPointShader = CreatePhysicallyBasedShader 0 0 Constants.Paths.PhysicallyBasedShadowStaticPointShaderFilePath in Hl.Assert ()
        let shadowStaticSpotShader = CreatePhysicallyBasedShader 0 0 Constants.Paths.PhysicallyBasedShadowStaticSpotShaderFilePath in Hl.Assert ()
        let shadowStaticDirectionalShader = CreatePhysicallyBasedShader 0 0 Constants.Paths.PhysicallyBasedShadowStaticDirectionalShaderFilePath in Hl.Assert ()
        let shadowAnimatedPointShader = CreatePhysicallyBasedShader 0 0 Constants.Paths.PhysicallyBasedShadowAnimatedPointShaderFilePath in Hl.Assert ()
        let shadowAnimatedSpotShader = CreatePhysicallyBasedShader 0 0 Constants.Paths.PhysicallyBasedShadowAnimatedSpotShaderFilePath in Hl.Assert ()
        let shadowAnimatedDirectionalShader = CreatePhysicallyBasedShader 0 0 Constants.Paths.PhysicallyBasedShadowAnimatedDirectionalShaderFilePath in Hl.Assert ()
        let shadowTerrainPointShader = CreatePhysicallyBasedTerrainShader Constants.Paths.PhysicallyBasedShadowTerrainPointShaderFilePath in Hl.Assert ()
        let shadowTerrainSpotShader = CreatePhysicallyBasedTerrainShader Constants.Paths.PhysicallyBasedShadowTerrainSpotShaderFilePath in Hl.Assert ()
        let shadowTerrainDirectionalShader = CreatePhysicallyBasedTerrainShader Constants.Paths.PhysicallyBasedShadowTerrainDirectionalShaderFilePath in Hl.Assert ()

        // create deferred shaders
        let deferredStaticShader = CreatePhysicallyBasedShader lightMapsMax lightsMax Constants.Paths.PhysicallyBasedDeferredStaticShaderFilePath in Hl.Assert ()
        let deferredStaticClippedShader = CreatePhysicallyBasedShader lightMapsMax lightsMax Constants.Paths.PhysicallyBasedDeferredStaticClippedShaderFilePath in Hl.Assert ()
        let deferredAnimatedShader = CreatePhysicallyBasedShader lightMapsMax lightsMax Constants.Paths.PhysicallyBasedDeferredAnimatedShaderFilePath in Hl.Assert ()
        let deferredTerrainShader = CreatePhysicallyBasedTerrainShader Constants.Paths.PhysicallyBasedDeferredTerrainShaderFilePath in Hl.Assert ()
        let deferredLightMappingShader = CreatePhysicallyBasedDeferredLightMappingShader lightMapsMax Constants.Paths.PhysicallyBasedDeferredLightMappingShaderFilePath in Hl.Assert ()
        let deferredAmbientShader = CreatePhysicallyBasedDeferredAmbientShader lightMapsMax Constants.Paths.PhysicallyBasedDeferredAmbientShaderFilePath in Hl.Assert ()
        let deferredIrradianceShader = CreatePhysicallyBasedDeferredIrradianceShader Constants.Paths.PhysicallyBasedDeferredIrradianceShaderFilePath in Hl.Assert ()
        let deferredEnvironmentFilterShader = CreatePhysicallyBasedDeferredEnvironmentFilterShader lightMapsMax Constants.Paths.PhysicallyBasedDeferredEnvironmentFilterShaderFilePath in Hl.Assert ()
        let deferredSsaoShader = CreatePhysicallyBasedDeferredSsaoShader Constants.Paths.PhysicallyBasedDeferredSsaoShaderFilePath in Hl.Assert ()
        let deferredLightingShader = CreatePhysicallyBasedDeferredLightingShader lightsMax Constants.Paths.PhysicallyBasedDeferredLightingShaderFilePath in Hl.Assert ()
        let deferredColoringShader = CreatePhysicallyBasedDeferredColoringShader Constants.Paths.PhysicallyBasedDeferredColoringShaderFilePath in Hl.Assert ()
        let deferredCompositionShader = CreatePhysicallyBasedDeferredCompositionShader Constants.Paths.PhysicallyBasedDeferredCompositionShaderFilePath in Hl.Assert ()

        // create forward shaders
        let forwardStaticShader = CreatePhysicallyBasedShader Constants.Render.LightMapsMaxForward Constants.Render.LightsMaxForward Constants.Paths.PhysicallyBasedForwardStaticShaderFilePath in Hl.Assert ()
        let forwardAnimatedShader = CreatePhysicallyBasedShader Constants.Render.LightMapsMaxForward Constants.Render.LightsMaxForward Constants.Paths.PhysicallyBasedForwardAnimatedShaderFilePath in Hl.Assert ()

        // fin
        { ShadowStaticPointShader = shadowStaticPointShader
          ShadowStaticSpotShader = shadowStaticSpotShader
          ShadowStaticDirectionalShader = shadowStaticDirectionalShader
          ShadowAnimatedPointShader = shadowAnimatedPointShader
          ShadowAnimatedSpotShader = shadowAnimatedSpotShader
          ShadowAnimatedDirectionalShader = shadowAnimatedDirectionalShader
          ShadowTerrainPointShader = shadowTerrainPointShader
          ShadowTerrainSpotShader = shadowTerrainSpotShader
          ShadowTerrainDirectionalShader = shadowTerrainDirectionalShader
          DeferredStaticShader = deferredStaticShader
          DeferredStaticClippedShader = deferredStaticClippedShader
          DeferredAnimatedShader = deferredAnimatedShader
          DeferredTerrainShader = deferredTerrainShader
          DeferredLightMappingShader = deferredLightMappingShader
          DeferredIrradianceShader = deferredIrradianceShader
          DeferredEnvironmentFilterShader = deferredEnvironmentFilterShader
          DeferredAmbientShader = deferredAmbientShader
          DeferredSsaoShader = deferredSsaoShader
          DeferredLightingShader = deferredLightingShader
          DeferredColoringShader = deferredColoringShader
          DeferredCompositionShader = deferredCompositionShader
          ForwardStaticShader = forwardStaticShader
          ForwardAnimatedShader = forwardAnimatedShader }

    let DestroyPhysicallyBasedShaders shaders =
        Gl.DeleteProgram shaders.ShadowStaticPointShader.PhysicallyBasedShader
        Gl.DeleteProgram shaders.ShadowStaticSpotShader.PhysicallyBasedShader
        Gl.DeleteProgram shaders.ShadowStaticDirectionalShader.PhysicallyBasedShader
        Gl.DeleteProgram shaders.ShadowAnimatedPointShader.PhysicallyBasedShader
        Gl.DeleteProgram shaders.ShadowAnimatedSpotShader.PhysicallyBasedShader
        Gl.DeleteProgram shaders.ShadowAnimatedDirectionalShader.PhysicallyBasedShader
        Gl.DeleteProgram shaders.ShadowTerrainPointShader.PhysicallyBasedShader
        Gl.DeleteProgram shaders.ShadowTerrainSpotShader.PhysicallyBasedShader
        Gl.DeleteProgram shaders.ShadowTerrainDirectionalShader.PhysicallyBasedShader
        Gl.DeleteProgram shaders.DeferredStaticShader.PhysicallyBasedShader
        Gl.DeleteProgram shaders.DeferredStaticClippedShader.PhysicallyBasedShader
        Gl.DeleteProgram shaders.DeferredAnimatedShader.PhysicallyBasedShader
        Gl.DeleteProgram shaders.DeferredTerrainShader.PhysicallyBasedShader
        Gl.DeleteProgram shaders.DeferredLightMappingShader.PhysicallyBasedDeferredLightMappingShader
        Gl.DeleteProgram shaders.DeferredIrradianceShader.PhysicallyBasedDeferredIrradianceShader
        Gl.DeleteProgram shaders.DeferredEnvironmentFilterShader.PhysicallyBasedDeferredEnvironmentFilterShader
        Gl.DeleteProgram shaders.DeferredAmbientShader.PhysicallyBasedDeferredAmbientShader
        Gl.DeleteProgram shaders.DeferredSsaoShader.PhysicallyBasedDeferredSsaoShader
        Gl.DeleteProgram shaders.DeferredLightingShader.PhysicallyBasedDeferredLightingShader
        Gl.DeleteProgram shaders.DeferredColoringShader.PhysicallyBasedDeferredColoringShader
        Gl.DeleteProgram shaders.DeferredCompositionShader.PhysicallyBasedDeferredCompositionShader
        Gl.DeleteProgram shaders.ForwardStaticShader.PhysicallyBasedShader
        Gl.DeleteProgram shaders.ForwardAnimatedShader.PhysicallyBasedShader