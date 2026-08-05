// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Vulkan
open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Numerics
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Vortice.Vulkan
open Prime
open Nu

[<Struct; StructLayout (LayoutKind.Explicit)>]
type GaussianEsmStruct =
    [<FieldOffset(0)>] val mutable scale : Vector2
    [<FieldOffset(8)>] val mutable radius : single

[<Struct; StructLayout (LayoutKind.Explicit)>]
type GaussianDofStruct =
    [<FieldOffset(0)>] val mutable scale : Vector2
    [<FieldOffset(8)>] val mutable radius : single

[<Struct; StructLayout (LayoutKind.Explicit)>]
type DepthOfFieldStruct =
    [<FieldOffset(0)>] val mutable nearDistance : single
    [<FieldOffset(4)>] val mutable farDistance : single
    [<FieldOffset(8)>] val mutable focalType : int
    [<FieldOffset(12)>] val mutable focalDistance : single
    [<FieldOffset(16)>] val mutable focalPoint : Vector2

[<Struct; StructLayout (LayoutKind.Explicit)>]
type ToneMappingStruct =
    [<FieldOffset(0)>] val mutable lightExposure : single
    [<FieldOffset(4)>] val mutable toneMapType : int
    [<FieldOffset(16)>] val mutable toneMapSlope : Vector3
    [<FieldOffset(32)>] val mutable toneMapOffset : Vector3
    [<FieldOffset(48)>] val mutable toneMapPower : Vector3
    [<FieldOffset(60)>] val mutable toneMapSaturation : single
    [<FieldOffset(64)>] val mutable toneMapWhitePoint : single

[<Struct; StructLayout (LayoutKind.Explicit)>]
type ChromaticAberrationStruct =
    [<FieldOffset(0)>] val mutable channelOffsets : Vector3
    [<FieldOffset(16)>] val mutable focalPoint : Vector2

[<Struct; StructLayout (LayoutKind.Explicit)>]
type FxaaStruct =
    [<FieldOffset(0)>] val mutable spanMax : single
    [<FieldOffset(4)>] val mutable reduceMinDivisor : single
    [<FieldOffset(8)>] val mutable reduceMulDivisor : single

[<Struct; StructLayout (LayoutKind.Explicit)>]
type BloomExtractStruct =
    [<FieldOffset(0)>] val mutable threshold : single

[<Struct; StructLayout (LayoutKind.Explicit)>]
type BloomDownSampleStruct =
    [<FieldOffset(0)>] val mutable karisAverageEnabled : int
    [<FieldOffset(4)>] val mutable sampleLevel : int
    [<FieldOffset(8)>] val mutable sourceResolution : Vector2

[<Struct; StructLayout (LayoutKind.Explicit)>]
type BloomUpSampleStruct =
    [<FieldOffset(0)>] val mutable radius : single

[<Struct; StructLayout (LayoutKind.Explicit)>]
type BloomApplyStruct =
    [<FieldOffset(0)>] val mutable strength : single

[<Struct; StructLayout (LayoutKind.Explicit)>]
type ShadowVertStruct =
    [<FieldOffset(0)>] val mutable viewProjection : Matrix4x4

[<Struct; StructLayout (LayoutKind.Explicit)>]
type ShadowFragStruct =
    [<FieldOffset(0)>] val mutable eyeCenter : Vector3
    [<FieldOffset(12)>] val mutable lightShadowExponent : single

[<Struct; StructLayout (LayoutKind.Explicit)>]
type TerrainFragStruct =
    [<FieldOffset(0)>] val mutable layersCount : int
    [<FieldOffset(4)>] val mutable lightShadowSamples : int
    [<FieldOffset(8)>] val mutable lightShadowBias : single
    [<FieldOffset(12)>] val mutable lightShadowSampleScalar : single
    [<FieldOffset(16)>] val mutable lightShadowExponent : single
    [<FieldOffset(20)>] val mutable lightShadowDensity : single

[<Struct; StructLayout (LayoutKind.Explicit)>]
type LightingStruct =
    [<FieldOffset(0)>] val mutable lightCutoffMargin : single
    [<FieldOffset(16)>] val mutable lightAmbientColor : Vector3
    [<FieldOffset(28)>] val mutable lightAmbientBrightness : single
    [<FieldOffset(32)>] val mutable lightAmbientBoostCutoff : single
    [<FieldOffset(36)>] val mutable lightAmbientBoostScalar : single
    [<FieldOffset(40)>] val mutable lightShadowSamples : int
    [<FieldOffset(44)>] val mutable lightShadowBias : single
    [<FieldOffset(48)>] val mutable lightShadowSampleScalar : single
    [<FieldOffset(52)>] val mutable lightShadowExponent : single
    [<FieldOffset(56)>] val mutable lightShadowDensity : single
    [<FieldOffset(60)>] val mutable fogEnabled : int
    [<FieldOffset(64)>] val mutable fogType : int
    [<FieldOffset(68)>] val mutable fogStart : single
    [<FieldOffset(72)>] val mutable fogFinish : single
    [<FieldOffset(76)>] val mutable fogDensity : single
    [<FieldOffset(80)>] val mutable fogColor : Vector4
    [<FieldOffset(96)>] val mutable ssvfEnabled : int
    [<FieldOffset(100)>] val mutable ssvfIntensity : single
    [<FieldOffset(104)>] val mutable ssvfSteps : int
    [<FieldOffset(108)>] val mutable ssvfAsymmetry : single
    [<FieldOffset(112)>] val mutable ssrrEnabled : int
    [<FieldOffset(116)>] val mutable ssrrIntensity : single
    [<FieldOffset(120)>] val mutable ssrrDetail : single
    [<FieldOffset(124)>] val mutable ssrrRefinementsMax : int
    [<FieldOffset(128)>] val mutable ssrrRayThickness : single
    [<FieldOffset(132)>] val mutable ssrrDistanceCutoff : single
    [<FieldOffset(136)>] val mutable ssrrDistanceCutoffMargin : single
    [<FieldOffset(140)>] val mutable ssrrEdgeHorizontalMargin : single
    [<FieldOffset(144)>] val mutable ssrrEdgeVerticalMargin : single
    [<FieldOffset(148)>] val mutable ssrlEnabled : int
    [<FieldOffset(152)>] val mutable ssrlIntensity : single
    [<FieldOffset(156)>] val mutable ssrlDetail : single
    [<FieldOffset(160)>] val mutable ssrlRefinementsMax : int
    [<FieldOffset(164)>] val mutable ssrlRayThickness : single
    [<FieldOffset(168)>] val mutable ssrlTowardEyeCutoff : single
    [<FieldOffset(172)>] val mutable ssrlDepthCutoff : single
    [<FieldOffset(176)>] val mutable ssrlDepthCutoffMargin : single
    [<FieldOffset(180)>] val mutable ssrlDistanceCutoff : single
    [<FieldOffset(184)>] val mutable ssrlDistanceCutoffMargin : single
    [<FieldOffset(188)>] val mutable ssrlRoughnessCutoff : single
    [<FieldOffset(192)>] val mutable ssrlRoughnessCutoffMargin : single
    [<FieldOffset(196)>] val mutable ssrlSlopeCutoff : single
    [<FieldOffset(200)>] val mutable ssrlSlopeCutoffMargin : single
    [<FieldOffset(204)>] val mutable ssrlEdgeHorizontalMargin : single
    [<FieldOffset(208)>] val mutable ssrlEdgeVerticalMargin : single
    [<FieldOffset(212)>] val mutable shadowNear : single

// TODO: P1: see if we can come up with a better alternative name than Lighting2?
[<Struct; StructLayout (LayoutKind.Explicit)>]
type Lighting2Struct =
    [<FieldOffset(0)>] val mutable lightCutoffMargin : single
    [<FieldOffset(4)>] val mutable lightShadowSamples : int
    [<FieldOffset(8)>] val mutable lightShadowBias : single
    [<FieldOffset(12)>] val mutable lightShadowSampleScalar : single
    [<FieldOffset(16)>] val mutable lightShadowExponent : single
    [<FieldOffset(20)>] val mutable lightShadowDensity : single
    [<FieldOffset(24)>] val mutable sssEnabled : int
    [<FieldOffset(28)>] val mutable lightsCount : int
    [<FieldOffset(32)>] val mutable shadowNear : single

[<Struct; StructLayout (LayoutKind.Explicit)>]
type LightMapStruct =
    [<FieldOffset(0)>] val mutable origin : Vector3
    [<FieldOffset(16)>] val mutable min : Vector3
    [<FieldOffset(32)>] val mutable size : Vector3
    [<FieldOffset(48)>] val mutable ambientColor : Vector3
    [<FieldOffset(60)>] val mutable ambientBrightness : single

[<Struct; StructLayout (LayoutKind.Explicit)>]
type LightsGeneralStruct =
    [<FieldOffset(0)>] val mutable lightMapsCount : int
    [<FieldOffset(4)>] val mutable lightMapSingletonBlendMargin : single
    [<FieldOffset(8)>] val mutable lightsCount : int

[<Struct; StructLayout (LayoutKind.Explicit)>]
type LightStruct =
    [<FieldOffset(0)>] val mutable origin : Vector3
    [<FieldOffset(16)>] val mutable direction : Vector3
    [<FieldOffset(32)>] val mutable color : Vector3
    [<FieldOffset(44)>] val mutable brightness : single
    [<FieldOffset(48)>] val mutable attenuationLinear : single
    [<FieldOffset(52)>] val mutable attenuationQuadratic : single
    [<FieldOffset(56)>] val mutable cutoff : single
    [<FieldOffset(60)>] val mutable lightType : int
    [<FieldOffset(64)>] val mutable coneInner : single
    [<FieldOffset(68)>] val mutable coneOuter : single
    [<FieldOffset(72)>] val mutable desireFog : int
    [<FieldOffset(76)>] val mutable shadowIndex : int

[<Struct; StructLayout (LayoutKind.Explicit)>]
type SsaoStruct =
    [<FieldOffset(0)>] val mutable resolution : Vector2i
    [<FieldOffset(8)>] val mutable intensity : single
    [<FieldOffset(12)>] val mutable bias : single
    [<FieldOffset(16)>] val mutable radius : single
    [<FieldOffset(20)>] val mutable distanceMax : single
    [<FieldOffset(24)>] val mutable sampleCount : int

/// A set of physically-based attachments that support a given viewport.
type PhysicallyBasedAttachments =
    { DownSampleColorAttachment : Texture
      DownSampleDepthAttachment : Texture
      UpSampleColorAttachment : Texture
      GaussianEsmAttachment : Texture
      GaussianEsmArrayAttachment : Texture
      ColorFull0Attachment : Texture
      ColorFull1Attachment : Texture
      ColorHalf0Attachment : Texture
      ColorHalf1Attachment : Texture
      BloomExtractAttachment : Texture
      BloomSampleAttachments : Texture array
      BloomApplyAttachment : Texture
      ToneMappingAttachment : Texture
      GammaCorrectionAttachment : Texture
      ShadowTextureArrayAttachments : Texture * Texture
      ShadowMapAttachmentsArray : (Texture * Texture) array
      ShadowCascadeArrayAttachmentsArray : (Texture * Texture) array
      GeometryAttachments : Texture * Texture * Texture * Texture * Texture * Texture * Texture * Texture
      LightingAttachment : Texture
      FoggingAttachment : Texture
      LightMappingAttachment : Texture
      AmbientAttachment : Texture
      IrradianceAttachment : Texture
      EnvironmentFilterAttachment : Texture
      SsaoUnfilteredAttachment : Texture
      SsaoFilteredAttachment : Texture
      ColoringAttachments : Texture * Texture
      CompositionAttachment : Texture }

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
      SubsurfaceCutoff : single
      SubsurfaceCutoffMargin : single
      RefractiveIndex : single
      ClearCoat : single
      ClearCoatRoughness : single }

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
          SubsurfaceCutoff = 0.0f
          SubsurfaceCutoffMargin = 0.0f
          RefractiveIndex = 0.0f
          ClearCoat = 0.0f
          ClearCoatRoughness = 0.0f }

/// Describes a physically-based material.
type [<CustomEquality; NoComparison>] PhysicallyBasedMaterial =
    { AlbedoTexture : Texture
      RoughnessTexture : Texture
      MetallicTexture : Texture
      AmbientOcclusionTexture : Texture
      EmissionTexture : Texture
      NormalTexture : Texture
      HeightTexture : Texture
      SubdermalTexture : Texture
      FinenessTexture : Texture
      ScatterTexture : Texture
      ClearCoatTexture : Texture
      ClearCoatRoughnessTexture : Texture
      ClearCoatNormalTexture : Texture
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
          ClearCoatTexture = Texture.EmptyTexture
          ClearCoatRoughnessTexture = Texture.EmptyTexture
          ClearCoatNormalTexture = Texture.EmptyTexture
          TwoSided = false
          Clipped = false
          Names = "" }

    /// Compute hash.
    static member hash material =
        (hash material.AlbedoTexture <<<            00) ^^^
        (hash material.RoughnessTexture <<<         02) ^^^
        (hash material.MetallicTexture <<<          04) ^^^
        (hash material.AmbientOcclusionTexture <<<  06) ^^^
        (hash material.EmissionTexture <<<          08) ^^^
        (hash material.NormalTexture <<<            10) ^^^
        (hash material.HeightTexture <<<            12) ^^^
        (hash material.SubdermalTexture <<<         14) ^^^
        (hash material.FinenessTexture <<<          16) ^^^
        (hash material.ScatterTexture <<<           18) ^^^
        (hash material.TwoSided <<<                 20) ^^^
        (hash material.Clipped <<<                  22) ^^^
        (hash material.Names <<<                    24)

    /// Determing equality.
    static member equals left right =
        refEq left right || // OPTIMIZATION: first check ref equality.
        left.AlbedoTexture = right.AlbedoTexture &&
        left.RoughnessTexture = right.RoughnessTexture &&
        left.MetallicTexture = right.MetallicTexture &&
        left.AmbientOcclusionTexture = right.AmbientOcclusionTexture &&
        left.EmissionTexture = right.EmissionTexture &&
        left.NormalTexture = right.NormalTexture &&
        left.HeightTexture = right.HeightTexture &&
        left.SubdermalTexture = right.SubdermalTexture &&
        left.FinenessTexture = right.FinenessTexture &&
        left.ScatterTexture = right.ScatterTexture &&
        left.TwoSided = right.TwoSided &&
        left.Clipped = right.Clipped &&
        left.Names = right.Names

    override this.GetHashCode () = 
        PhysicallyBasedMaterial.hash this

    override this.Equals that =
        match that with
        | :? PhysicallyBasedMaterial as that -> PhysicallyBasedMaterial.equals this that
        | _ -> false

/// Describes some physically-based geometry that's loaded into VRAM.
type PhysicallyBasedGeometry =
    { Bounds : Box3
      PrimitiveTopology : VkPrimitiveTopology
      ElementCount : int
      Vertices : Vector3 array
      Indices : int array
      mutable TrianglesCached : Vector3 array option
      VertexBuffer : VulkanBuffer
      InstanceBuffer : VulkanBuffer
      IndexBuffer : VulkanBuffer }

    /// Lazily access triangles, building them from Vertices and Indices if needed.
    member this.Triangles =
        match this.TrianglesCached with
        | None ->
            assert (this.PrimitiveTopology = VkPrimitiveTopology.TriangleList) // should hold since we use Assimp.PostProcessSteps.Triangulate
            let triangles =
                [|for points in Array.chunkBySize 3 this.Indices do
                    this.Vertices[points[0]]
                    this.Vertices[points[1]]
                    this.Vertices[points[2]]|]
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

    static member inline hash surface =
        surface.HashCode

    static member equals left right =
        refEq left right || // OPTIMIZATION: first check ref equality.
        left.HashCode = right.HashCode && // OPTIMIZATION: check hash equality to bail as quickly as possible.
        left.SurfaceMaterial = right.SurfaceMaterial &&
        left.SurfaceMaterialIndex = right.SurfaceMaterialIndex &&
        refEq left.PhysicallyBasedGeometry right.PhysicallyBasedGeometry

    static member comparer =
        HashIdentity.FromFunctions PhysicallyBasedSurface.hash PhysicallyBasedSurface.equals

    static member extractPresence presenceDefault (sceneOpt : Assimp.Scene option) surface =
        match surface.SurfaceNode.PresenceOpt with
        | ValueNone ->
            match sceneOpt with
            | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                let material = scene.Materials[surface.SurfaceMaterialIndex]
                ValueOption.defaultValue presenceDefault material.PresenceOpt
            | Some _ | None -> presenceDefault
        | ValueSome presence -> presence

    static member extractRenderStyle renderStyleDefault (sceneOpt : Assimp.Scene option) surface =
        match surface.SurfaceNode.RenderStyleOpt with
        | ValueNone ->
            match sceneOpt with
            | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                let material = scene.Materials[surface.SurfaceMaterialIndex]
                ValueOption.defaultValue renderStyleDefault material.RenderStyleOpt
            | Some _ | None -> renderStyleDefault
        | ValueSome renderStyle -> renderStyle

    static member extractIgnoreLightMaps ignoreLightMapsDefault (sceneOpt : Assimp.Scene option) surface =
        match surface.SurfaceNode.IgnoreLightMapsOpt with
        | ValueNone ->
            match sceneOpt with
            | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                let material = scene.Materials[surface.SurfaceMaterialIndex]
                ValueOption.defaultValue ignoreLightMapsDefault material.IgnoreLightMapsOpt
            | Some _ | None -> ignoreLightMapsDefault
        | ValueSome ignoreLightMaps -> ignoreLightMaps

    static member extractOpaqueDistance opaqueDistanceDefault (sceneOpt : Assimp.Scene option) surface =
        match surface.SurfaceNode.OpaqueDistanceOpt with
        | ValueNone ->
            match sceneOpt with
            | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                let material = scene.Materials[surface.SurfaceMaterialIndex]
                ValueOption.defaultValue opaqueDistanceDefault material.OpaqueDistanceOpt
            | Some _ | None -> opaqueDistanceDefault
        | ValueSome opaqueDistance -> opaqueDistance

    static member extractFinenessOffset finenessOffsetDefault (sceneOpt : Assimp.Scene option) surface =
        match surface.SurfaceNode.FinenessOffsetOpt with
        | ValueNone ->
            match sceneOpt with
            | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                let material = scene.Materials[surface.SurfaceMaterialIndex]
                ValueOption.defaultValue finenessOffsetDefault material.FinenessOffsetOpt
            | Some _ | None -> finenessOffsetDefault
        | ValueSome finenessOffset -> finenessOffset

    static member extractScatterType scatterTypeDefault (sceneOpt : Assimp.Scene option) surface =
        match surface.SurfaceNode.ScatterTypeOpt with
        | ValueNone ->
            match sceneOpt with
            | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                let material = scene.Materials[surface.SurfaceMaterialIndex]
                ValueOption.defaultValue scatterTypeDefault material.ScatterTypeOpt
            | Some _ | None -> scatterTypeDefault
        | ValueSome scatterType -> scatterType

    static member extractSpecularScalar specularScalarDefault (sceneOpt : Assimp.Scene option) surface =
        match surface.SurfaceNode.SpecularScalarOpt with
        | ValueNone ->
            match sceneOpt with
            | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                let material = scene.Materials[surface.SurfaceMaterialIndex]
                ValueOption.defaultValue specularScalarDefault material.SpecularScalarOpt
            | Some _ | None -> specularScalarDefault
        | ValueSome specularScalar -> specularScalar

    static member extractSubsurfaceCutoff subsurfaceCutoffDefault (sceneOpt : Assimp.Scene option) surface =
        match surface.SurfaceNode.SubsurfaceCutoffOpt with
        | ValueNone ->
            match sceneOpt with
            | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                let material = scene.Materials[surface.SurfaceMaterialIndex]
                ValueOption.defaultValue subsurfaceCutoffDefault material.SubsurfaceCutoffOpt
            | Some _ | None -> subsurfaceCutoffDefault
        | ValueSome subsurfaceCutoff -> subsurfaceCutoff

    static member extractSubsurfaceCutoffMargin subsurfaceCutoffMarginDefault (sceneOpt : Assimp.Scene option) surface =
        match surface.SurfaceNode.SubsurfaceCutoffMarginOpt with
        | ValueNone ->
            match sceneOpt with
            | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                let material = scene.Materials[surface.SurfaceMaterialIndex]
                ValueOption.defaultValue subsurfaceCutoffMarginDefault material.SubsurfaceCutoffMarginOpt
            | Some _ | None -> subsurfaceCutoffMarginDefault
        | ValueSome subsurfaceCutoffMargin -> subsurfaceCutoffMargin

    static member extractRefractiveIndex refractiveIndexDefault (sceneOpt : Assimp.Scene option) surface =
        match surface.SurfaceNode.RefractiveIndexOpt with
        | ValueNone ->
            match sceneOpt with
            | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                let material = scene.Materials[surface.SurfaceMaterialIndex]
                ValueOption.defaultValue refractiveIndexDefault material.RefractiveIndexOpt
            | Some _ | None -> refractiveIndexDefault
        | ValueSome refractiveIndex -> refractiveIndex

    static member extractClearCoat clearCoatDefault (sceneOpt : Assimp.Scene option) surface =
        match surface.SurfaceNode.ClearCoatOpt with
        | ValueNone ->
            match sceneOpt with
            | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                let material = scene.Materials[surface.SurfaceMaterialIndex]
                ValueOption.defaultValue clearCoatDefault material.ClearCoatOpt
            | Some _ | None -> clearCoatDefault
        | ValueSome clearCoat -> clearCoat

    static member extractClearCoatRoughness clearCoatRoughnessDefault (sceneOpt : Assimp.Scene option) surface =
        match surface.SurfaceNode.ClearCoatRoughnessOpt with
        | ValueNone ->
            match sceneOpt with
            | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                let material = scene.Materials[surface.SurfaceMaterialIndex]
                ValueOption.defaultValue clearCoatRoughnessDefault material.ClearCoatRoughnessOpt
            | Some _ | None -> clearCoatRoughnessDefault
        | ValueSome clearCoatRoughness -> clearCoatRoughness

    static member extractNavShape shapeDefault (sceneOpt : Assimp.Scene option) surface =
        match surface.SurfaceNode.NavShapeOpt with
        | ValueNone ->
            match sceneOpt with
            | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                let material = scene.Materials[surface.SurfaceMaterialIndex]
                ValueOption.defaultValue shapeDefault material.NavShapeOpt
            | Some _ | None -> shapeDefault
        | ValueSome shape -> shape

    static member make names (surfaceMatrix : Matrix4x4) bounds properties material materialIndex surfaceNode geometry =
        let hashCode =
            hash material ^^^
            hash materialIndex ^^^
            Runtime.CompilerServices.RuntimeHelpers.GetHashCode geometry
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

[<RequireQualifiedAccess>]
module PhysicallyBasedSurfaceFns =
    let extractPresence = PhysicallyBasedSurface.extractPresence
    let extractRenderStyle = PhysicallyBasedSurface.extractRenderStyle
    let extractIgnoreLightMaps = PhysicallyBasedSurface.extractIgnoreLightMaps
    let extractOpaqueDistance = PhysicallyBasedSurface.extractOpaqueDistance
    let extractFinenessOffset = PhysicallyBasedSurface.extractFinenessOffset
    let extractScatterType = PhysicallyBasedSurface.extractScatterType
    let extractSpecularScalar = PhysicallyBasedSurface.extractSpecularScalar
    let extractSubsurfaceCutoff = PhysicallyBasedSurface.extractSubsurfaceCutoff
    let extractSubsurfaceCutoffMargin = PhysicallyBasedSurface.extractSubsurfaceCutoffMargin
    let extractRefractiveIndex = PhysicallyBasedSurface.extractRefractiveIndex
    let extractClearCoat = PhysicallyBasedSurface.extractClearCoat
    let extractClearCoatRoughness = PhysicallyBasedSurface.extractClearCoatRoughness
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

/// Describes a physically-based depth pipeline that's loaded into GPU.
type PhysicallyBasedShadowPipeline =
    { ShadowVertUniform : VulkanBuffer
      BoneUniform : VulkanBuffer
      ShadowFragUniform : VulkanBuffer
      Pipeline : Pipeline }

/// Describes a physically-based pipeline that's loaded into GPU.
type PhysicallyBasedPipeline =
    { EyeUniform : VulkanBuffer
      LightingUniform : VulkanBuffer
      BoneUniform : VulkanBuffer
      LightMapUniform : VulkanBuffer
      LightsGeneralUniform : VulkanBuffer
      LightUniform : VulkanBuffer
      ShadowMatrixUniform : VulkanBuffer
      Pipeline : Pipeline }

/// Describes a physically-based deferred terrain pipeline that's loaded into GPU.
type PhysicallyBasedDeferredTerrainPipeline =
    { EyeUniform : VulkanBuffer
      Lighting3Uniform : VulkanBuffer
      Pipeline : Pipeline }

/// Describes the lighting pass of a deferred physically-based pipeline that's loaded into GPU.
type PhysicallyBasedDeferredLightingPipeline =
    { EyeUniform : VulkanBuffer
      Lighting2Uniform : VulkanBuffer
      LightUniform : VulkanBuffer
      ShadowMatrixUniform : VulkanBuffer
      Pipeline : Pipeline }

/// Describes the fogging pass of a deferred physically-based pipeline that's loaded into GPU.
type PhysicallyBasedDeferredFoggingPipeline =
    { EyeUniform : VulkanBuffer
      LightingUniform : VulkanBuffer
      LightsGeneralUniform : VulkanBuffer
      LightsUniform : VulkanBuffer
      ShadowMatricesUniform : VulkanBuffer
      Pipeline : Pipeline }

/// Describes the light mapping pass of a deferred physically-based pipeline that's loaded into GPU.
type PhysicallyBasedDeferredLightMappingPipeline =
    { EyeUniform : VulkanBuffer
      LightMapsUniform : VulkanBuffer
      LightsGeneralUniform : VulkanBuffer
      Pipeline : Pipeline }

/// Describes the ambient pass of a deferred physically-based pipeline that's loaded into GPU.
type PhysicallyBasedDeferredAmbientPipeline =
    { EyeUniform : VulkanBuffer
      LightMapUniform : VulkanBuffer
      LightMapsUniform : VulkanBuffer
      Pipeline : Pipeline }

/// Describes the irradiance pass of a deferred physically-based pipeline that's loaded into GPU.
type PhysicallyBasedDeferredIrradiancePipeline =
    { EyeUniform : VulkanBuffer
      Pipeline : Pipeline }

/// Describes the environment filter pass of a deferred physically-based pipeline that's loaded into GPU.
type PhysicallyBasedDeferredEnvironmentFilterPipeline =
    { EyeUniform : VulkanBuffer
      LightMapsUniform : VulkanBuffer
      Pipeline : Pipeline }

/// Describes the ssao pass of a deferred physically-based pipeline that's loaded into GPU.
type PhysicallyBasedDeferredSsaoPipeline =
    { EyeUniform : VulkanBuffer
      SsaoUniform : VulkanBuffer
      Pipeline : Pipeline }

/// Describes the coloring pass of a deferred physically-based pipeline that's loaded into GPU.
type PhysicallyBasedDeferredColoringPipeline =
    { EyeUniform : VulkanBuffer
      LightingUniform : VulkanBuffer
      Pipeline : Pipeline }

/// Describes the composition pass of a deferred physically-based pipeline that's loaded into GPU.
type PhysicallyBasedDeferredCompositionPipeline =
    { EyeUniform : VulkanBuffer
      LightingUniform : VulkanBuffer
      Pipeline : Pipeline }

/// Describes a box filter pipeline that's loaded into GPU.
type FilterBoxPipeline =
    { Pipeline : Pipeline }

/// Describes an esm gaussian filter pipeline that's loaded into GPU.
type FilterGaussianEsmPipeline =
    { GaussianEsmUniform : VulkanBuffer
      Pipeline : Pipeline }

/// Describes a down-sample filter pipeline that's loaded into GPU.
type FilterDownSamplePipeline =
    { Pipeline : Pipeline }

/// Describes an up-sample filter pipeline that's loaded into GPU.
type FilterUpSamplePipeline =
    { Pipeline : Pipeline }

/// Describes a depth-of-field gaussian filter pipeline that's loaded into GPU.
type FilterGaussianDofPipeline =
    { GaussianDofUniform : VulkanBuffer
      Pipeline : Pipeline }

/// Describes a depth-of-field filter pipeline that's loaded into GPU.
type FilterDepthOfFieldPipeline =
    { EyeUniform : VulkanBuffer
      DepthOfFieldUniform : VulkanBuffer
      Pipeline : Pipeline }

/// Describes a bloom extract filter pipeline that's loaded into GPU.
type FilterBloomExtractPipeline =
    { BloomExtractUniform : VulkanBuffer
      Pipeline : Pipeline }

/// Describes a bloom down-sample filter pipeline that's loaded into GPU.
type FilterBloomDownSamplePipeline =
    { BloomDownSampleUniform : VulkanBuffer
      Pipeline : Pipeline }

/// Describes a bloom up-sample filter pipeline that's loaded into GPU.
type FilterBloomUpSamplePipeline =
    { BloomUpSampleUniform : VulkanBuffer
      Pipeline : Pipeline }

/// Describes a bloom apply filter pipeline that's loaded into GPU.
type FilterBloomApplyPipeline =
    { BloomApplyUniform : VulkanBuffer
      Pipeline : Pipeline }

/// Describes a tone-mapping filter pipeline that's loaded into GPU.
type FilterToneMappingPipeline =
    { ToneMappingUniform : VulkanBuffer
      Pipeline : Pipeline }

/// Describes a chromatic aberration filter pipeline that's loaded into GPU.
type FilterChromaticAberrationPipeline =
    { ChromaticAberrationUniform : VulkanBuffer
      Pipeline : Pipeline }

/// Describes an fxaa filter pipeline that's loaded into GPU.
type FilterFxaaPipeline =
    { FxaaUniform : VulkanBuffer
      Pipeline : Pipeline }

/// Describes a gamma-correction filter pipeline that's loaded into GPU.
type FilterGammaCorrectionPipeline =
    { Pipeline : Pipeline }

/// Physically-based pipelines.
type PhysicallyBasedPipelines =
    { FilterBox1dPipeline : FilterBoxPipeline
      FilterBilateralDownSamplePipeline : FilterDownSamplePipeline
      FilterBilateralUpSamplePipeline : FilterUpSamplePipeline
      FilterGaussianEsmPipeline : FilterGaussianEsmPipeline
      FilterGaussianDofPipeline : FilterGaussianDofPipeline
      FilterDepthOfFieldPipeline : FilterDepthOfFieldPipeline
      FilterBloomExtractPipeline : FilterBloomExtractPipeline
      FilterBloomDownSamplePipeline : FilterBloomDownSamplePipeline
      FilterBloomUpSamplePipeline : FilterBloomUpSamplePipeline
      FilterBloomApplyPipeline : FilterBloomApplyPipeline
      FilterToneMappingPipeline : FilterToneMappingPipeline
      FilterChromaticAberrationPipeline : FilterChromaticAberrationPipeline
      FilterFxaaPipeline : FilterFxaaPipeline
      FilterGammaCorrectionPipeline : FilterGammaCorrectionPipeline
      ShadowStaticPointPipeline : PhysicallyBasedShadowPipeline
      ShadowStaticSpotPipeline : PhysicallyBasedShadowPipeline
      ShadowStaticDirectionalPipeline : PhysicallyBasedShadowPipeline
      ShadowAnimatedPointPipeline : PhysicallyBasedShadowPipeline
      ShadowAnimatedSpotPipeline : PhysicallyBasedShadowPipeline
      ShadowAnimatedDirectionalPipeline : PhysicallyBasedShadowPipeline
      ShadowTerrainPointPipeline : PhysicallyBasedDeferredTerrainPipeline
      ShadowTerrainSpotPipeline : PhysicallyBasedDeferredTerrainPipeline
      ShadowTerrainDirectionalPipeline : PhysicallyBasedDeferredTerrainPipeline
      DeferredStaticPipeline : PhysicallyBasedPipeline
      DeferredStaticClippedPipeline : PhysicallyBasedPipeline
      DeferredAnimatedPipeline : PhysicallyBasedPipeline
      DeferredTerrainPipeline : PhysicallyBasedDeferredTerrainPipeline
      DeferredLightingPipeline : PhysicallyBasedDeferredLightingPipeline
      DeferredFoggingPipeline : PhysicallyBasedDeferredFoggingPipeline
      DeferredLightMappingPipeline : PhysicallyBasedDeferredLightMappingPipeline
      DeferredAmbientPipeline : PhysicallyBasedDeferredAmbientPipeline
      DeferredIrradiancePipeline : PhysicallyBasedDeferredIrradiancePipeline
      DeferredEnvironmentFilterPipeline : PhysicallyBasedDeferredEnvironmentFilterPipeline
      DeferredSsaoPipeline : PhysicallyBasedDeferredSsaoPipeline
      DeferredColoringPipeline : PhysicallyBasedDeferredColoringPipeline
      DeferredCompositionPipeline : PhysicallyBasedDeferredCompositionPipeline
      ForwardStaticPipeline : PhysicallyBasedPipeline
      ForwardAnimatedPipeline : PhysicallyBasedPipeline }

[<RequireQualifiedAccess>]
module PhysicallyBased =
    
    // static vertex definition
    let StaticTexCoordsOffset =     (3 (*position*)) * sizeof<single>
    let StaticNormalOffset =        (3 (*position*) + 2 (*tex coords*)) * sizeof<single>
    let StaticVertexSize =          (3 (*position*) + 2 (*tex coords*) + 3 (*normal*)) * sizeof<single>
    let StaticVertices =
        [|Pipeline.vertex 0 StaticVertexSize VkVertexInputRate.Vertex
            [|Pipeline.attribute 0 Single3 0
              Pipeline.attribute 1 Single2 StaticTexCoordsOffset
              Pipeline.attribute 2 Single3 StaticNormalOffset|]
          Pipeline.vertex 1 (Constants.Render.InstanceFieldCount * sizeof<single>) VkVertexInputRate.Instance
            [|Pipeline.attribute 3 Single4 0
              Pipeline.attribute 4 Single4 (4 * sizeof<single>)
              Pipeline.attribute 5 Single4 (8 * sizeof<single>)
              Pipeline.attribute 6 Single4 (12 * sizeof<single>)
              Pipeline.attribute 7 Single4 (16 * sizeof<single>)
              Pipeline.attribute 8 Single4 (20 * sizeof<single>)
              Pipeline.attribute 9 Single4 (24 * sizeof<single>)
              Pipeline.attribute 10 Single4 (28 * sizeof<single>)
              Pipeline.attribute 11 Single4 (32 * sizeof<single>)
              Pipeline.attribute 12 Single4 (36 * sizeof<single>)|]|]

    // animated vertex definition
    let AnimatedTexCoordsOffset =   (3 (*position*)) * sizeof<single>
    let AnimatedNormalOffset =      (3 (*position*) + 2 (*tex coords*)) * sizeof<single>
    let AnimatedBoneIdsOffset =     (3 (*position*) + 2 (*tex coords*) + 3 (*normal*)) * sizeof<single>
    let AnimatedWeightsOffset =     (3 (*position*) + 2 (*tex coords*) + 3 (*normal*) + 4 (*boneIds*)) * sizeof<single>
    let AnimatedVertexSize =        (3 (*position*) + 2 (*tex coords*) + 3 (*normal*) + 4 (*boneIds*) + 4 (*weights*)) * sizeof<single>
    let AnimatedVertices =
        [|Pipeline.vertex 0 AnimatedVertexSize VkVertexInputRate.Vertex
            [|Pipeline.attribute 0 Single3 0
              Pipeline.attribute 1 Single2 AnimatedTexCoordsOffset
              Pipeline.attribute 2 Single3 AnimatedNormalOffset
              Pipeline.attribute 3 Single4 AnimatedBoneIdsOffset
              Pipeline.attribute 4 Single4 AnimatedWeightsOffset|]
          Pipeline.vertex 1 (Constants.Render.InstanceFieldCount * sizeof<single>) VkVertexInputRate.Instance
            [|Pipeline.attribute 5 Single4 0
              Pipeline.attribute 6 Single4 (4 * sizeof<single>)
              Pipeline.attribute 7 Single4 (8 * sizeof<single>)
              Pipeline.attribute 8 Single4 (12 * sizeof<single>)
              Pipeline.attribute 9 Single4 (16 * sizeof<single>)
              Pipeline.attribute 10 Single4 (20 * sizeof<single>)
              Pipeline.attribute 11 Single4 (24 * sizeof<single>)
              Pipeline.attribute 12 Single4 (28 * sizeof<single>)
              Pipeline.attribute 13 Single4 (32 * sizeof<single>)
              Pipeline.attribute 14 Single4 (36 * sizeof<single>)|]|]

    // terrain vertex definition
    let TerrainTexCoordsOffset =    (3 (*position*)) * sizeof<single>
    let TerrainNormalOffset =       (3 (*position*) + 2 (*tex coords*)) * sizeof<single>
    let TerrainTintOffset =         (3 (*position*) + 2 (*tex coords*) + 3 (*normal*)) * sizeof<single>
    let TerrainBlendsOffset =       (3 (*position*) + 2 (*tex coords*) + 3 (*normal*) + 3 (*tint*)) * sizeof<single>
    let TerrainBlends2Offset =      (3 (*position*) + 2 (*tex coords*) + 3 (*normal*) + 3 (*tint*) + 4 (*blends*)) * sizeof<single>
    let TerrainVertexSize =         (3 (*position*) + 2 (*tex coords*) + 3 (*normal*) + 3 (*tint*) + 4 (*blends*) + 4 (*blends2*)) * sizeof<single>
    let TerrainVertices =
        [|Pipeline.vertex 0 TerrainVertexSize VkVertexInputRate.Vertex
            [|Pipeline.attribute 0 Single3 0
              Pipeline.attribute 1 Single2 TerrainTexCoordsOffset
              Pipeline.attribute 2 Single3 TerrainNormalOffset
              Pipeline.attribute 3 Single3 TerrainTintOffset
              Pipeline.attribute 4 Single4 TerrainBlendsOffset
              Pipeline.attribute 5 Single4 TerrainBlends2Offset|]
          Pipeline.vertex 1 (Constants.Render.InstanceFieldCount * sizeof<single>) VkVertexInputRate.Instance
            [|Pipeline.attribute 6 Single4 0
              Pipeline.attribute 7 Single4 (4 * sizeof<single>)
              Pipeline.attribute 8 Single4 (8 * sizeof<single>)
              Pipeline.attribute 9 Single4 (12 * sizeof<single>)
              Pipeline.attribute 10 Single4 (16 * sizeof<single>)
              Pipeline.attribute 11 Single4 (20 * sizeof<single>)
              Pipeline.attribute 12 Single4 (24 * sizeof<single>)
              Pipeline.attribute 13 Single4 (28 * sizeof<single>)
              Pipeline.attribute 14 Single4 (32 * sizeof<single>)|]|]

    /// Create the attachments required for physically-based rendering.
    let createPhysicallyBasedAttachments (geometryViewport : Viewport) context =

        // create down/up-sample attachments
        let allUsageFlags = VkImageUsageFlags.Sampled ||| VkImageUsageFlags.TransferSrc ||| VkImageUsageFlags.TransferDst
        let downSampleColorAttachment = Attachment.createColorAttachment Texture2d allUsageFlags Rgba16f Rgba (geometryViewport.Bounds.Size.X / 2) (geometryViewport.Bounds.Size.Y / 2) context
        let downSampleDepthAttachment = Attachment.createColorAttachment Texture2d allUsageFlags R16f Red (geometryViewport.Bounds.Size.X / 2) (geometryViewport.Bounds.Size.Y / 2) context
        let upSampleColorAttachment = Attachment.createColorAttachment Texture2d allUsageFlags Rgba16f Rgba geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y context

        // create gaussian esm attachment
        let gaussianEsmResolution = geometryViewport.ShadowTextureResolution
        let gaussianEsmAttachment = Attachment.createColorAttachment Texture2d VkImageUsageFlags.Sampled Rg32f Rg gaussianEsmResolution.X gaussianEsmResolution.Y context

        // create gaussian esm array attachments
        let gaussianEsmArrayResolution = geometryViewport.ShadowCascadeResolution
        let gaussianEsmArrayAttachment = Attachment.createColorAttachment (Texture2dArray Constants.Render.ShadowCascadeLevels) VkImageUsageFlags.Sampled Rg32f Rg gaussianEsmArrayResolution.X gaussianEsmArrayResolution.Y context

        // create color full attachments
        let colorFull0Attachment = Attachment.createColorAttachment Texture2d allUsageFlags Rgba16f Rgba geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y context
        let colorFull1Attachment = Attachment.createColorAttachment Texture2d allUsageFlags Rgba16f Rgba geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y context

        // create color half attachments
        let colorHalf0Attachment = Attachment.createColorAttachment Texture2d allUsageFlags Rgba16f Rgba (geometryViewport.Bounds.Size.X / 2) (geometryViewport.Bounds.Size.Y / 2) context
        let colorHalf1Attachment = Attachment.createColorAttachment Texture2d allUsageFlags Rgba16f Rgba (geometryViewport.Bounds.Size.X / 2) (geometryViewport.Bounds.Size.Y / 2) context

        // bloom attachments
        let bloomExtractAttachment = Attachment.createColorAttachment Texture2d VkImageUsageFlags.Sampled Rgba16f Rgba geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y context
        let bloomSampleAttachments = Attachment.createBloomSampleAttachments (geometryViewport.Bounds.Size.X / 2) (geometryViewport.Bounds.Size.Y / 2) context
        let bloomApplyAttachment = Attachment.createColorAttachment Texture2d (VkImageUsageFlags.Sampled ||| VkImageUsageFlags.TransferSrc) Rgba16f Rgba geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y context

        // create tone-mapping attachments
        let toneMappingAttachment = Attachment.createToneMappingAttachments geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y context

        // create gamma-correction attachments
        let gammaCorrectionAttachment = Attachment.createGammaCorrectionAttachments geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y context

        // create shadow texture array attachments
        let shadowTextureArrayAttachments =
            let shadowResolution = geometryViewport.ShadowTextureResolution
            Attachment.createShadowTextureArrayAttachments shadowResolution.X shadowResolution.Y Constants.Render.ShadowTexturesMax context

        // create shadow map attachments array
        let shadowMapAttachmentsArray =
            [|for _ in 0 .. dec Constants.Render.ShadowMapsMax do
                let shadowResolution = geometryViewport.ShadowMapResolution
                Attachment.createShadowMapAttachments shadowResolution.X shadowResolution.Y context|]

        // create shadow cascade array attachments array
        let shadowCascadeArrayAttachmentsArray =
            [|for _ in 0 .. dec Constants.Render.ShadowCascadesMax do
                let shadowResolution = geometryViewport.ShadowCascadeResolution
                Attachment.createShadowCascadeArrayAttachments shadowResolution.X shadowResolution.Y Constants.Render.ShadowCascadeLevels context|]

        // create geometry attachments
        let geometryAttachments = Attachment.createGeometryAttachments geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y context

        // create lighting attachment
        let lightingAttachment = Attachment.createLightingAttachment geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y context

        // create fogging attachment
        let foggingAttachment = Attachment.createFoggingAttachment geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y context

        // create light mapping attachment
        let lightMappingAttachment = Attachment.createLightMappingAttachment geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y context

        // create ambient attachment
        let ambientAttachment = Attachment.createAmbientAttachment geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y context

        // create irradiance attachment
        let irradianceAttachment = Attachment.createIrradianceAttachment geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y context

        // create environment filter attachment
        let environmentfilterAttachment = Attachment.createEnvironmentFilterAttachment geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y context

        // create ssao attachments
        let ssaoUsageFlags = VkImageUsageFlags.Sampled ||| VkImageUsageFlags.TransferSrc ||| VkImageUsageFlags.TransferDst
        let ssaoUnfilteredAttachment = Attachment.createColorAttachment Texture2d ssaoUsageFlags R16f Red geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y context
        let ssaoFilteredAttachment = Attachment.createColorAttachment Texture2d ssaoUsageFlags R16f Red geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y context

        // create coloring attachments
        let coloringAttachments = Attachment.createColoringAttachments geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y context

        // create composition attachments
        let compositionAttachment = Attachment.createCompositionAttachment geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y context

        // make record
        { DownSampleColorAttachment = downSampleColorAttachment
          DownSampleDepthAttachment = downSampleDepthAttachment
          UpSampleColorAttachment = upSampleColorAttachment
          GaussianEsmAttachment = gaussianEsmAttachment
          GaussianEsmArrayAttachment = gaussianEsmArrayAttachment
          ColorFull0Attachment = colorFull0Attachment
          ColorFull1Attachment = colorFull1Attachment
          ColorHalf0Attachment = colorHalf0Attachment
          ColorHalf1Attachment = colorHalf1Attachment
          BloomExtractAttachment = bloomExtractAttachment
          BloomSampleAttachments = bloomSampleAttachments
          BloomApplyAttachment = bloomApplyAttachment
          ToneMappingAttachment = toneMappingAttachment
          GammaCorrectionAttachment = gammaCorrectionAttachment
          ShadowTextureArrayAttachments = shadowTextureArrayAttachments
          ShadowMapAttachmentsArray = shadowMapAttachmentsArray
          ShadowCascadeArrayAttachmentsArray = shadowCascadeArrayAttachmentsArray
          GeometryAttachments = geometryAttachments
          LightingAttachment = lightingAttachment
          FoggingAttachment = foggingAttachment
          LightMappingAttachment = lightMappingAttachment
          AmbientAttachment = ambientAttachment
          IrradianceAttachment = irradianceAttachment
          EnvironmentFilterAttachment = environmentfilterAttachment
          SsaoUnfilteredAttachment = ssaoUnfilteredAttachment
          SsaoFilteredAttachment = ssaoFilteredAttachment
          ColoringAttachments = coloringAttachments
          CompositionAttachment = compositionAttachment }

    /// Update the size of the attachments. Must be used every frame.
    let updatePhysicallyBasedAttachmentsSize (geometryViewport : Viewport) (attachments : PhysicallyBasedAttachments) context =
        Attachment.updateColorAttachmentSize geometryViewport.ShadowTextureResolution.X geometryViewport.ShadowTextureResolution.Y attachments.GaussianEsmAttachment context
        Attachment.updateColorAttachmentSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.ColorFull0Attachment context
        Attachment.updateColorAttachmentSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.ColorFull1Attachment context
        Attachment.updateColorAttachmentSize (geometryViewport.Bounds.Size.X / 2) (geometryViewport.Bounds.Size.Y / 2) attachments.ColorHalf0Attachment context
        Attachment.updateColorAttachmentSize (geometryViewport.Bounds.Size.X / 2) (geometryViewport.Bounds.Size.Y / 2) attachments.ColorHalf1Attachment context
        Attachment.updateColorAttachmentSize (geometryViewport.Bounds.Size.X / 2) (geometryViewport.Bounds.Size.Y / 2) attachments.DownSampleColorAttachment context
        Attachment.updateColorAttachmentSize (geometryViewport.Bounds.Size.X / 2) (geometryViewport.Bounds.Size.Y / 2) attachments.DownSampleDepthAttachment context
        Attachment.updateColorAttachmentSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.UpSampleColorAttachment context
        Attachment.updateColorAttachmentSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.BloomExtractAttachment context
        Attachment.updateBloomSampleAttachmentsSize (geometryViewport.Bounds.Size.X / 2) (geometryViewport.Bounds.Size.Y / 2) attachments.BloomSampleAttachments context
        Attachment.updateColorAttachmentSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.BloomApplyAttachment context
        Attachment.updateToneMappingAttachmentsSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.ToneMappingAttachment context
        Attachment.updateGammaCorrectionAttachmentsSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.GammaCorrectionAttachment context
        Attachment.updateShadowTextureArrayAttachmentsSize geometryViewport.ShadowTextureResolution.X geometryViewport.ShadowTextureResolution.Y attachments.ShadowTextureArrayAttachments context
        for i in 0 .. dec attachments.ShadowMapAttachmentsArray.Length do
            Attachment.updateShadowMapAttachmentsSize geometryViewport.ShadowMapResolution.X geometryViewport.ShadowMapResolution.Y attachments.ShadowMapAttachmentsArray[i] context
        for i in 0 .. dec attachments.ShadowCascadeArrayAttachmentsArray.Length do
            Attachment.updateShadowCascadeArrayAttachmentsSize geometryViewport.ShadowCascadeResolution.X geometryViewport.ShadowCascadeResolution.Y attachments.ShadowCascadeArrayAttachmentsArray[i] context
        Attachment.updateGeometryAttachmentsSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.GeometryAttachments context
        Attachment.updateLightingAttachmentSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.LightingAttachment context
        Attachment.updateFoggingAttachmentSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.FoggingAttachment context
        Attachment.updateLightMappingAttachmentSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.LightMappingAttachment context
        Attachment.updateAmbientAttachmentSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.AmbientAttachment context
        Attachment.updateIrradianceAttachmentSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.IrradianceAttachment context
        Attachment.updateEnvironmentFilterAttachmentSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.EnvironmentFilterAttachment context
        Attachment.updateColorAttachmentSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.SsaoUnfilteredAttachment context
        Attachment.updateColorAttachmentSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.SsaoFilteredAttachment context
        Attachment.updateColoringAttachmentsSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.ColoringAttachments context
        Attachment.updateCompositionAttachmentSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.CompositionAttachment context

    /// Destroy the physically-based attachments.
    let destroyPhysicallyBasedAttachments (attachments : PhysicallyBasedAttachments) context =
        Attachment.destroyColorAttachment attachments.GaussianEsmAttachment context
        Attachment.destroyColorAttachment attachments.ColorFull0Attachment context
        Attachment.destroyColorAttachment attachments.ColorFull1Attachment context
        Attachment.destroyColorAttachment attachments.ColorHalf0Attachment context
        Attachment.destroyColorAttachment attachments.ColorHalf1Attachment context
        Attachment.destroyColorAttachment attachments.DownSampleColorAttachment context
        Attachment.destroyColorAttachment attachments.DownSampleDepthAttachment context
        Attachment.destroyColorAttachment attachments.UpSampleColorAttachment context
        Attachment.destroyColorAttachment attachments.BloomExtractAttachment context
        Attachment.destroyBloomSampleAttachments attachments.BloomSampleAttachments context
        Attachment.destroyColorAttachment attachments.BloomApplyAttachment context
        Attachment.destroyToneMappingAttachments attachments.ToneMappingAttachment context
        Attachment.destroyGammaCorrectionAttachment attachments.GammaCorrectionAttachment context
        Attachment.destroyShadowTextureArrayAttachments attachments.ShadowTextureArrayAttachments context
        for i in 0 .. dec attachments.ShadowMapAttachmentsArray.Length do
            Attachment.destroyShadowMapAttachments attachments.ShadowMapAttachmentsArray[i] context
        for i in 0 .. dec attachments.ShadowCascadeArrayAttachmentsArray.Length do
            Attachment.destroyShadowCascadeArrayAttachments attachments.ShadowCascadeArrayAttachmentsArray[i] context
        Attachment.destroyGeometryAttachments attachments.GeometryAttachments context
        Attachment.destroyLightingAttachment attachments.LightingAttachment context
        Attachment.destroyFoggingAttachment attachments.FoggingAttachment context
        Attachment.destroyLightMappingAttachment attachments.LightMappingAttachment context
        Attachment.destroyAmbientAttachment attachments.AmbientAttachment context
        Attachment.destroyIrradianceAttachment attachments.IrradianceAttachment context
        Attachment.destroyEnvironmentFilterAttachment attachments.EnvironmentFilterAttachment context
        Attachment.destroyColorAttachment attachments.SsaoUnfilteredAttachment context
        Attachment.destroyColorAttachment attachments.SsaoFilteredAttachment context
        Attachment.destroyColoringAttachments attachments.ColoringAttachments context
        Attachment.destroyCompositionAttachment attachments.CompositionAttachment context

    /// Create a mesh for a physically-based quad.
    let createPhysicallyBasedQuadMesh () =

        // make vertex data
        let vertexData =
            [|
                (*   positions   *)         (* tex coords *)    (*    normals    *)
                -1.0f; -1.0f; +0.0f;        0.0f; 0.0f;          0.0f;  0.0f;  1.0f // bottom-left
                +1.0f; -1.0f; +0.0f;        1.0f; 0.0f;          0.0f;  0.0f;  1.0f // bottom-right
                +1.0f; +1.0f; +0.0f;        1.0f; 1.0f;          0.0f;  0.0f;  1.0f // top-right
                +1.0f; +1.0f; +0.0f;        1.0f; 1.0f;          0.0f;  0.0f;  1.0f // top-right
                -1.0f; +1.0f; +0.0f;        0.0f; 1.0f;          0.0f;  0.0f;  1.0f // top-left
                -1.0f; -1.0f; +0.0f;        0.0f; 0.0f;          0.0f;  0.0f;  1.0f // bottom-left
            |]

        // make index data trivially
        let indexData = Array.init 6 id

        // make bounds trivially
        let bounds = box3 (v3 -1.0f -1.0f 0.0f) (v3 2.0f 2.0f 0.0f)

        // fin
        (vertexData, indexData, bounds)

    /// Create a mesh for a physically-based particle.
    let createPhysicallyBasedParticleMesh () =

        // make vertex data
        let vertexData =
            [|
                (*   positions   *)         (* tex coords *)    (*    normals    *)
                -0.5f; -0.5f; +0.0f;        0.0f; 0.0f;          0.0f;  0.0f;  1.0f // bottom-left
                +0.5f; -0.5f; +0.0f;        0.0f; 0.0f;          0.0f;  0.0f;  1.0f // bottom-right
                +0.5f; +0.5f; +0.0f;        0.0f; 0.0f;          0.0f;  0.0f;  1.0f // top-right
                +0.5f; +0.5f; +0.0f;        0.0f; 0.0f;          0.0f;  0.0f;  1.0f // top-right
                -0.5f; +0.5f; +0.0f;        0.0f; 0.0f;          0.0f;  0.0f;  1.0f // top-left
                -0.5f; -0.5f; +0.0f;        0.0f; 0.0f;          0.0f;  0.0f;  1.0f // bottom-left
            |]

        // make index data trivially
        let indexData = Array.init 6 id

        // make bounds trivially
        let bounds = box3 (v3 -0.5f -0.5f 0.0f) (v3 1.0f 1.0f 0.0f)

        // fin
        (vertexData, indexData, bounds)

    /// Create a mesh for a physically-based billboard.
    let createPhysicallyBasedBillboardMesh () =

        // make vertex data
        let vertexData =
            [|
                (*   positions   *)         (* tex coords *)    (*    normals    *)
                -0.5f; -0.5f; +0.0f;        0.0f; 0.0f;          0.0f;  0.0f;  1.0f // bottom-left
                +0.5f; -0.5f; +0.0f;        0.0f; 0.0f;          0.0f;  0.0f;  1.0f // bottom-right
                +0.5f; +0.5f; +0.0f;        0.0f; 0.0f;          0.0f;  0.0f;  1.0f // top-right
                -0.5f; -0.5f; +0.0f;        0.0f; 0.0f;          0.0f;  0.0f;  1.0f // top-right
                +0.5f; +0.5f; +0.0f;        0.0f; 0.0f;          0.0f;  0.0f;  1.0f // top-left
                -0.5f; +0.5f; +0.0f;        0.0f; 0.0f;          0.0f;  0.0f;  1.0f // bottom-left
            |]

        // make index data trivially
        let indexData = Array.init 6 id

        // make bounds trivially
        let bounds = box3 (v3 -0.5f -0.5f 0.0f) (v3 1.0f 1.0f 0.0f)

        // fin
        (vertexData, indexData, bounds)

    /// Create physically-based material from an assimp mesh, falling back on defaults in case of missing textures.
    /// Uses file name-based inferences to look for texture files in case the ones that were hard-coded in the model
    /// files can't be located.
    /// Thread-safe if contextOpt = None.
    let createPhysicallyBasedMaterial dirPath defaultMaterial (textureClient : TextureClient) (material : Assimp.Material) contextOpt =

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
                    match Constants.Render.TextureBlockCompression with
                    | BcCompression -> PathF.ChangeExtension (possibleFilePath, ".dds")
                    | AstcCompression -> PathF.ChangeExtension (possibleFilePath, ".ktx")|]
            let mutable found = false
            let mutable i = 0
            while not found && i < possibleFilePaths.Length do
                let possibleFilePath = possibleFilePaths[i]
                if File.Exists (dirPrefix + possibleFilePath) then
                    albedoTextureSlotFilePath <- possibleFilePath
                    found <- true
                else i <- inc i
        let albedoTexture =
            match contextOpt with
            | Some context ->
                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression albedoTextureSlotFilePath) (dirPrefix + albedoTextureSlotFilePath) RenderThread context with
                | Right texture -> texture
                | Left _ -> defaultMaterial.AlbedoTexture
            | None -> defaultMaterial.AlbedoTexture

        // infer possible substitute texture names
        let albedoTextureDirName =              match albedoTextureSlotFilePath with null -> "" | filePath -> PathF.GetDirectoryName filePath
        let albedoTextureFileName =             PathF.GetFileName albedoTextureSlotFilePath
        let substitutionPrefix =                if albedoTextureDirName <> "" then albedoTextureDirName + "/" else ""
        let has_bc =                            albedoTextureFileName.Contains "_bc"
        let has_d =                             albedoTextureFileName.Contains "_d"
        let hasBaseColor =                      albedoTextureFileName.Contains "BaseColor"
        let hasDiffuse =                        albedoTextureFileName.Contains "Diffuse"
        let hasAlbedo =                         albedoTextureFileName.Contains "Albedo"
        let mTextureFilePath =                  if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_m")                       elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_m")                        else ""
        let g_mTextureFilePath =                if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_g_m")                     elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_g_m")                      else ""
        let g_m_aoTextureFilePath =             if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_g_m_ao")                  elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_g_m_ao")                   else ""
        let gTextureFilePath =                  if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_g")                       elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_g")                        else ""
        let sTextureFilePath =                  if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_s")                       elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_s")                        else ""
        let aoTextureFilePath =                 if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_ao")                      elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_ao")                       else ""
        let eTextureFilePath =                  if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_e")                       elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_e")                        else ""
        let nTextureFilePath =                  if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_n")                       elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_n")                        else ""
        let hTextureFilePath =                  if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_h")                       elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_h")                        else ""
        let subdermalTextureFilePath =          if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_subdermal")               elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_subdermal")                else ""
        let finenessTextureFilePath =           if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_fineness")                elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_fineness")                 else ""
        let scatterTextureFilePath =            if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_scatter")                 elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_scatter")                  else ""
        let clearCoatTextureFilePath =          if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_clear_coat")              elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_clear_coat")               else ""
        let clearCoatRoughnessTextureFilePath = if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_clear_coat_roughness")    elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_clear_coat_roughness")     else ""
        let clearCoatNormalTextureFilePath =    if has_bc       then substitutionPrefix + albedoTextureFileName.Replace ("_bc", "_clear_coat_normal")       elif has_d      then substitutionPrefix + albedoTextureFileName.Replace ("_d", "_clear_coat_normal")        else ""
        let rmTextureFilePath =                 if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "RM")                 elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "RM")                   elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "RM")                    else ""
        let rmaTextureFilePath =                if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "RMA")                elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "RMA")                  elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "RMA")                   else ""
        let roughnessTextureFilePath =          if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Roughness")          elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Roughness")            elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Roughness")             else ""
        let metallicTextureFilePath =           if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Metallic")           elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Metallic")             elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Metallic")              else ""
        let metalnessTextureFilePath =          if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Metalness")          elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Metalness")            elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Metalness")             else ""
        let ambientOcclusionTextureFilePath =   if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "AmbientOcclusion")   elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "AmbientOcclusion")     elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "AmbientOcclusion")      else ""
        let occlusionTextureFilePath =          if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Occlusion")          elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Occlusion")            elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Occlusion")             else ""
        let aoTextureFilePath' =                if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "AO")                 elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "AO")                   elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "AO")                    else ""
        let normalTextureFilePath =             if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Normal")             elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Normal")               elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Normal")                else ""
        let emissiveTextureFilePath =           if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Emissive")           elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Emissive")             elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Emissive")              else ""
        let emissionTextureFilePath =           if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Emission")           elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Emission")             elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Emission")              else ""
        let heightTextureFilePath =             if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Height")             elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Height")               elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Height")                else ""
        let subdermalTextureFilePath' =         if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Subdermal")          elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Subdermal")            elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Subdermal")             else ""
        let finenessTextureFilePath' =          if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Fineness")           elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Fineness")             elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Fineness")              else ""
        let scatterTextureFilePath' =           if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "Scatter")            elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "Scatter")              elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "Scatter")               else ""
        let clearCoatTextureFilePath' =         if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "ClearCoat")          elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "ClearCoat")            elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "ClearCoat")             else ""
        let clearCoatRoughnessTextureFilePath' =if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "ClearCoatRoughness") elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "ClearCoatRoughness")   elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "ClearCoatRoughness")    else ""
        let clearCoatNormalTextureFilePath' =   if hasBaseColor then substitutionPrefix + albedoTextureFileName.Replace ("BaseColor", "ClearCoatNormal")    elif hasDiffuse then substitutionPrefix + albedoTextureFileName.Replace ("Diffuse", "ClearCoatNormal")      elif hasAlbedo  then substitutionPrefix + albedoTextureFileName.Replace ("Albedo", "ClearCoatNormal")       else ""

        // attempt to load roughness info
        let roughness = Constants.Render.RoughnessDefault
        let mutable (_, roughnessTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Roughness, 0)
        if isNull roughnessTextureSlot.FilePath then roughnessTextureSlot.FilePath <- "" // ensure not null
        roughnessTextureSlot.FilePath <- roughnessTextureSlot.FilePath // trim
        let roughnessTexture =
            match contextOpt with
            | Some context ->
                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression roughnessTextureSlot.FilePath) (dirPrefix + roughnessTextureSlot.FilePath) RenderThread context with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression gTextureFilePath) (dirPrefix + gTextureFilePath) RenderThread context with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression sTextureFilePath) (dirPrefix + sTextureFilePath) RenderThread context with
                        | Right texture -> texture
                        | Left _ ->
                            match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression g_mTextureFilePath) (dirPrefix + g_mTextureFilePath) RenderThread context with
                            | Right texture -> texture
                            | Left _ ->
                                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression g_m_aoTextureFilePath) (dirPrefix + g_m_aoTextureFilePath) RenderThread context with
                                | Right texture -> texture
                                | Left _ ->
                                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression roughnessTextureFilePath) (dirPrefix + roughnessTextureFilePath) RenderThread context with
                                    | Right texture -> texture
                                    | Left _ ->
                                        match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression rmTextureFilePath) (dirPrefix + rmTextureFilePath) RenderThread context with
                                        | Right texture -> texture
                                        | Left _ ->
                                            match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression rmaTextureFilePath) (dirPrefix + rmaTextureFilePath) RenderThread context with
                                            | Right texture -> texture
                                            | Left _ -> defaultMaterial.RoughnessTexture
            | None -> defaultMaterial.RoughnessTexture

        // attempt to load metallic info
        let metallic = Constants.Render.MetallicDefault
        let mutable (_, metallicTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Metalness, 0)
        if isNull metallicTextureSlot.FilePath
        then metallicTextureSlot.FilePath <- "" // ensure not null
        else metallicTextureSlot.FilePath <- PathF.Normalize metallicTextureSlot.FilePath
        let metallicTexture =
            match contextOpt with
            | Some context ->
                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression metallicTextureSlot.FilePath) (dirPrefix + metallicTextureSlot.FilePath) RenderThread context with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression mTextureFilePath) (dirPrefix + mTextureFilePath) RenderThread context with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression g_mTextureFilePath) (dirPrefix + g_mTextureFilePath) RenderThread context with
                        | Right texture -> texture
                        | Left _ ->
                            match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression g_m_aoTextureFilePath) (dirPrefix + g_m_aoTextureFilePath) RenderThread context with
                            | Right texture -> texture
                            | Left _ ->
                                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression metallicTextureFilePath) (dirPrefix + metallicTextureFilePath) RenderThread context with
                                | Right texture -> texture
                                | Left _ ->
                                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression metalnessTextureFilePath) (dirPrefix + metalnessTextureFilePath) RenderThread context with
                                    | Right texture -> texture
                                    | Left _ ->
                                        match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression rmTextureFilePath) (dirPrefix + rmTextureFilePath) RenderThread context with
                                        | Right texture -> texture
                                        | Left _ ->
                                            match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression rmaTextureFilePath) (dirPrefix + rmaTextureFilePath) RenderThread context with
                                            | Right texture -> texture
                                            | Left _ -> defaultMaterial.MetallicTexture
            | None -> defaultMaterial.MetallicTexture

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
            match contextOpt with
            | Some context ->
                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression ambientOcclusionTextureSlotFilePath) (dirPrefix + ambientOcclusionTextureSlotFilePath) RenderThread context with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression aoTextureFilePath) (dirPrefix + aoTextureFilePath) RenderThread context with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression g_m_aoTextureFilePath) (dirPrefix + g_m_aoTextureFilePath) RenderThread context with
                        | Right texture -> texture
                        | Left _ ->
                            match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression ambientOcclusionTextureFilePath) (dirPrefix + ambientOcclusionTextureFilePath) RenderThread context with
                            | Right texture -> texture
                            | Left _ ->
                                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression occlusionTextureFilePath) (dirPrefix + occlusionTextureFilePath) RenderThread context with
                                | Right texture -> texture
                                | Left _ ->
                                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression aoTextureFilePath') (dirPrefix + aoTextureFilePath') RenderThread context with
                                    | Right texture -> texture
                                    | Left _ ->
                                        match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression rmaTextureFilePath) (dirPrefix + rmaTextureFilePath) RenderThread context with
                                        | Right texture -> texture
                                        | Left _ -> defaultMaterial.AmbientOcclusionTexture
            | None -> defaultMaterial.AmbientOcclusionTexture

        // attempt to load emission info
        let emission = Constants.Render.EmissionDefault
        let mutable (_, emissionTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Emissive, 0)
        if isNull emissionTextureSlot.FilePath
        then emissionTextureSlot.FilePath <- "" // ensure not null
        else emissionTextureSlot.FilePath <- PathF.Normalize emissionTextureSlot.FilePath
        let emissionTexture =
            match contextOpt with
            | Some context ->
                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression emissionTextureSlot.FilePath) (dirPrefix + emissionTextureSlot.FilePath) RenderThread context with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression eTextureFilePath) (dirPrefix + eTextureFilePath) RenderThread context with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression emissiveTextureFilePath) (dirPrefix + emissiveTextureFilePath) RenderThread context with
                        | Right texture -> texture
                        | Left _ ->
                            match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression emissionTextureFilePath) (dirPrefix + emissionTextureFilePath) RenderThread context with
                            | Right texture -> texture
                            | Left _ -> defaultMaterial.EmissionTexture
            | None -> defaultMaterial.EmissionTexture

        // attempt to load normal info
        let mutable (_, normalTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Normals, 0)
        if isNull normalTextureSlot.FilePath
        then normalTextureSlot.FilePath <- "" // ensure not null
        else normalTextureSlot.FilePath <- PathF.Normalize normalTextureSlot.FilePath
        let normalTexture =
            match contextOpt with
            | Some context ->
                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression normalTextureSlot.FilePath) (dirPrefix + normalTextureSlot.FilePath) RenderThread context with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression nTextureFilePath) (dirPrefix + nTextureFilePath) RenderThread context with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression normalTextureFilePath) (dirPrefix + normalTextureFilePath) RenderThread context with
                        | Right texture -> texture
                        | Left _ -> defaultMaterial.NormalTexture
            | None -> defaultMaterial.NormalTexture

        // attempt to load height info
        let height = Constants.Render.HeightDefault
        let mutable (_, heightTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Height, 0)
        if isNull heightTextureSlot.FilePath
        then heightTextureSlot.FilePath <- "" // ensure not null
        else heightTextureSlot.FilePath <- PathF.Normalize heightTextureSlot.FilePath
        let heightTexture =
            match contextOpt with
            | Some context ->
                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression heightTextureSlot.FilePath) (dirPrefix + heightTextureSlot.FilePath) RenderThread context with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression hTextureFilePath) (dirPrefix + hTextureFilePath) RenderThread context with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression heightTextureFilePath) (dirPrefix + heightTextureFilePath) RenderThread context with
                        | Right texture -> texture
                        | Left _ -> defaultMaterial.HeightTexture
            | None -> defaultMaterial.HeightTexture

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
            match contextOpt with
            | Some context ->
                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression subdermalTextureFilePath) (dirPrefix + subdermalTextureFilePath) RenderThread context with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression subdermalTextureFilePath') (dirPrefix + subdermalTextureFilePath') RenderThread context with
                    | Right texture -> texture
                    | Left _ -> defaultMaterial.SubdermalTexture
            | None -> defaultMaterial.SubdermalTexture

        // attempt to load fineness info
        let finenessOffset = Constants.Render.FinenessOffsetDefault
        let finenessTexture =
            match contextOpt with
            | Some context ->
                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression finenessTextureFilePath) (dirPrefix + finenessTextureFilePath) RenderThread context with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression finenessTextureFilePath') (dirPrefix + finenessTextureFilePath') RenderThread context with
                    | Right texture -> texture
                    | Left _ -> defaultMaterial.FinenessTexture
            | None -> defaultMaterial.FinenessTexture

        // attempt to load scatter info
        let scatterType = Constants.Render.ScatterTypeDefault
        let scatterTexture =
            match contextOpt with
            | Some context ->
                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression scatterTextureFilePath) (dirPrefix + scatterTextureFilePath) RenderThread context with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression scatterTextureFilePath') (dirPrefix + scatterTextureFilePath') RenderThread context with
                    | Right texture -> texture
                    | Left _ -> defaultMaterial.ScatterTexture
            | None -> defaultMaterial.ScatterTexture

        // attempt to load specular scalar info
        let specularScalar =
            match material.SpecularScalarOpt with
            | ValueSome specularScalar -> specularScalar
            | ValueNone -> Constants.Render.SpecularScalarDefault

        // attempt to load subsurface cutoff info
        let subsurfaceCutoff =
            match material.SubsurfaceCutoffOpt with
            | ValueSome subsurfaceCutoff -> subsurfaceCutoff
            | ValueNone -> Constants.Render.SubsurfaceCutoffDefault

        // attempt to load subsurface cutoff margin info
        let subsurfaceCutoffMargin =
            match material.SubsurfaceCutoffMarginOpt with
            | ValueSome subsurfaceCutoffMargin -> subsurfaceCutoffMargin
            | ValueNone -> Constants.Render.SubsurfaceCutoffMarginDefault

        // attempt to load refractive index info
        let refractiveIndex =
            match material.RefractiveIndexOpt with
            | ValueSome refractiveIndex -> refractiveIndex
            | ValueNone -> Constants.Render.RefractiveIndexDefault

        // attempt to load clear coat info
        let clearCoat = Constants.Render.ClearCoatDefault
        let clearCoatTexture =
            match contextOpt with
            | Some context ->
                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression clearCoatTextureFilePath) (dirPrefix + clearCoatTextureFilePath) RenderThread context with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression clearCoatTextureFilePath') (dirPrefix + clearCoatTextureFilePath') RenderThread context with
                    | Right texture -> texture
                    | Left _ -> defaultMaterial.ClearCoatTexture
            | None -> defaultMaterial.ClearCoatTexture

        // attempt to load clear coat roughness info
        let clearCoatRoughness = Constants.Render.ClearCoatRoughnessDefault
        let clearCoatRoughnessTexture =
            match contextOpt with
            | Some context ->
                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression clearCoatRoughnessTextureFilePath) (dirPrefix + clearCoatRoughnessTextureFilePath) RenderThread context with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression clearCoatRoughnessTextureFilePath') (dirPrefix + clearCoatRoughnessTextureFilePath') RenderThread context with
                    | Right texture -> texture
                    | Left _ -> defaultMaterial.ClearCoatRoughnessTexture
            | None -> defaultMaterial.ClearCoatRoughnessTexture

        // attempt to load clear coat normal info
        let clearCoatNormalTexture =
            match contextOpt with
            | Some context ->
                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression clearCoatNormalTextureFilePath) (dirPrefix + clearCoatNormalTextureFilePath) RenderThread context with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression clearCoatNormalTextureFilePath') (dirPrefix + clearCoatNormalTextureFilePath') RenderThread context with
                    | Right texture -> texture
                    | Left _ -> defaultMaterial.ClearCoatNormalTexture
            | None -> defaultMaterial.ClearCoatNormalTexture

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
            match contextOpt with
            | Some _ ->
                albedoTextureSlotFilePath + "/" +
                roughnessTextureSlot.FilePath + "/" +
                metallicTextureSlot.FilePath + "/" +
                ambientOcclusionTextureSlotA.FilePath + "/" +
                ambientOcclusionTextureSlotB.FilePath + "/" +
                emissionTextureSlot.FilePath + "/" +
                normalTextureSlot.FilePath + "/" +
                heightTextureSlot.FilePath
            | None -> ""

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
              SubsurfaceCutoff = subsurfaceCutoff
              SubsurfaceCutoffMargin = subsurfaceCutoffMargin
              SpecularScalar = specularScalar
              RefractiveIndex = refractiveIndex
              ClearCoat = clearCoat
              ClearCoatRoughness = clearCoatRoughness }

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
              ClearCoatTexture = clearCoatTexture
              ClearCoatRoughnessTexture = clearCoatRoughnessTexture
              ClearCoatNormalTexture = clearCoatNormalTexture
              TwoSided = twoSided
              Clipped = clipped
              Names = names }

        // fin
        (properties, material)

    /// Attempt to create physically-based material from an assimp scene.
    /// Thread-safe if contextOpt = None.
    let tryCreatePhysicallyBasedMaterials dirPath defaultMaterial textureClient (scene : Assimp.Scene) contextOpt =
        let mutable errorOpt = None
        let propertiesAndMaterials = Array.zeroCreate scene.Materials.Count
        for i in 0 .. dec scene.Materials.Count do
            if Option.isNone errorOpt then
                let (properties, material) = createPhysicallyBasedMaterial dirPath defaultMaterial textureClient scene.Materials[i] contextOpt
                propertiesAndMaterials[i] <- (properties, material)
        match errorOpt with
        | Some error -> Left error
        | None -> Right propertiesAndMaterials

    /// Create physically-based static mesh from an assimp mesh.
    let createPhysicallyBasedStaticMesh indexData (mesh : Assimp.Mesh) =

        // populate vertex data and bounds
        let vertexData = Array.zeroCreate<single> (mesh.Vertices.Count * 8)
        let mutable positionMin = v3Zero
        let mutable positionMax = v3Zero
        for i in 0 .. dec mesh.Vertices.Count do
            let v = i * 8
            let position = if i < mesh.VertexCount then mesh.Vertices[i] else Assimp.Vector3D (0.0f, 0.0f, 0.0f)
            let texCoords = if i < mesh.TextureCoordinateChannels[0].Capacity then mesh.TextureCoordinateChannels[0][i] else Assimp.Vector3D (0.0f, 0.0f, 0.0f)
            let normal = if i < mesh.Normals.Count then mesh.Normals[i] else Assimp.Vector3D (0.5f, 0.5f, 1.0f)
            vertexData[v] <- position.X
            vertexData[v+1] <- position.Y
            vertexData[v+2] <- position.Z
            vertexData[v+3] <- texCoords.X
            vertexData[v+4] <- 1.0f - texCoords.Y
            vertexData[v+5] <- normal.X
            vertexData[v+6] <- normal.Y
            vertexData[v+7] <- normal.Z
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
    let createPhysicallyBasedAnimatedMesh indexData (mesh : Assimp.Mesh) =

        // populate vertex data (except bone) and bounds
        let vertexData = Array.zeroCreate<single> (mesh.Vertices.Count * 16)
        let mutable positionMin = v3Zero
        let mutable positionMax = v3Zero
        for i in 0 .. dec mesh.Vertices.Count do
            let v = i * 16
            let position = if i < mesh.VertexCount then mesh.Vertices[i] else Assimp.Vector3D (0.0f, 0.0f, 0.0f)
            let texCoords = if i < mesh.TextureCoordinateChannels[0].Capacity then mesh.TextureCoordinateChannels[0][i] else Assimp.Vector3D (0.0f, 0.0f, 0.0f)
            let normal = if i < mesh.Normals.Count then mesh.Normals[i] else Assimp.Vector3D (0.5f, 0.5f, 1.0f)
            vertexData[v] <- position.X
            vertexData[v+1] <- position.Y
            vertexData[v+2] <- position.Z
            vertexData[v+3] <- texCoords.X
            vertexData[v+4] <- 1.0f - texCoords.Y
            vertexData[v+5] <- normal.X
            vertexData[v+6] <- normal.Y
            vertexData[v+7] <- normal.Z
            vertexData[v+8] <- -1.0f
            vertexData[v+9] <- -1.0f
            vertexData[v+10] <- -1.0f
            vertexData[v+11] <- -1.0f
            vertexData[v+12] <- 0.0f
            vertexData[v+13] <- 0.0f
            vertexData[v+14] <- 0.0f
            vertexData[v+15] <- 0.0f
            positionMin.X <- min positionMin.X position.X
            positionMin.Y <- min positionMin.Y position.Y
            positionMin.Z <- min positionMin.Z position.Z
            positionMax.X <- max positionMax.X position.X
            positionMax.Y <- max positionMax.Y position.Y
            positionMax.Z <- max positionMax.Z position.Z
        let bounds = box3 positionMin (positionMax - positionMin)

        // populate vertex bone data
        for boneIndex in 0 .. dec mesh.Bones.Count do
            let weights = mesh.Bones[boneIndex].VertexWeights
            let weightsCount = mesh.Bones[boneIndex].VertexWeights.Count
            for weightIndex in 0 .. dec weightsCount do
                let vertexId = weights[weightIndex].VertexID
                let vertexOffset = vertexId * 16
                let weight = weights[weightIndex].Weight
                if weight > 0.0f then

                    // find a free slot to specify the current index and weight (free slots are designated as -1.0f index above)
                    let mutable found = false
                    let mutable i = 0
                    while not found && i < Constants.Render.BonesInfluenceMax do
                        if vertexData[vertexOffset+8+i] = single boneIndex then // already found
                            found <- true
                        elif vertexData[vertexOffset+8+i] < 0.0f then // found free slot
                            vertexData[vertexOffset+8+i] <- single boneIndex
                            vertexData[vertexOffset+12+i] <- weight
                            found <- true
                        else i <- inc i

                    // when all slots are allocated, replace the index and weight of the lowest-weight entry iff the current weight is higher
                    if not found then
                        let mutable lowestOpt = ValueNone
                        for i in 0 .. dec Constants.Render.BonesInfluenceMax do
                            match lowestOpt with
                            | ValueSome lowest ->
                                if vertexData[vertexOffset+12+i] < vertexData[vertexOffset+12+lowest] then
                                    lowestOpt <- ValueSome i
                            | ValueNone -> lowestOpt <- ValueSome i
                        match lowestOpt with
                        | ValueSome lowest ->
                            if vertexData[vertexOffset+12+lowest] < weight then
                                vertexData[vertexOffset+8+lowest] <- single boneIndex
                                vertexData[vertexOffset+12+lowest] <- weight
                        | ValueNone -> failwithumf ()

        // fin
        (vertexData, indexData, bounds)

    /// Create physically-based static geometry from a mesh.
    let createPhysicallyBasedStaticGeometry primitiveTopology (vertexData : single Memory) (indexData : int Memory) bounds contextOpt =

        // make buffers
        let (vertices, indices, vertexBuffer, instanceBuffer, indexBuffer) =

            // make renderable
            match contextOpt with
            | Some context ->

                // create buffers
                let vertexBuffer = VulkanBuffer.createVertexStagedFromMemory vertexData context
                let instanceBuffer = VulkanBuffer.create Instance (Constants.Render.InstanceFieldCount * sizeof<single>) context
                let indexBuffer = VulkanBuffer.createIndexStagedFromMemory indexData context

                // prepare instance buffer
                let instanceData = Array.zeroCreate Constants.Render.InstanceFieldCount
                m4Identity.ToArray (instanceData, 0)
                VulkanBuffer.uploadArray instanceData instanceBuffer context
                
                // fin
                ([||], [||], vertexBuffer, instanceBuffer, indexBuffer)

            // fake buffers
            | None ->

                // compute vertices
                let vertices = Array.zeroCreate (vertexData.Length / 8)
                let vertexData = vertexData.Span
                for i in 0 .. dec vertices.Length do
                    let j = i * 8
                    let vertex = v3 vertexData[j] vertexData[j+1] vertexData[j+2]
                    vertices[i] <- vertex

                // create indices
                let indices = indexData.ToArray ()

                // fin
                (vertices, indices, Unchecked.defaultof<VulkanBuffer>, Unchecked.defaultof<VulkanBuffer>, Unchecked.defaultof<VulkanBuffer>)

        // make physically-based geometry
        let geometry =
            { Bounds = bounds
              PrimitiveTopology = primitiveTopology
              ElementCount = indexData.Length
              Vertices = vertices
              Indices = indices
              TrianglesCached = None
              VertexBuffer = vertexBuffer
              InstanceBuffer = instanceBuffer
              IndexBuffer = indexBuffer }

        // fin
        geometry

    /// Create physically-based quad geometry.
    let createPhysicallyBasedQuadGeometry contextOpt =
        let (vertexData, indexData, bounds) = createPhysicallyBasedQuadMesh ()
        createPhysicallyBasedStaticGeometry VkPrimitiveTopology.TriangleList (vertexData.AsMemory ()) (indexData.AsMemory ()) bounds contextOpt

    /// Create physically-based particle geometry.
    let createPhysicallyBasedParticleGeometry contextOpt =
        let (vertexData, indexData, bounds) = createPhysicallyBasedParticleMesh ()
        createPhysicallyBasedStaticGeometry VkPrimitiveTopology.TriangleList (vertexData.AsMemory ()) (indexData.AsMemory ()) bounds contextOpt

    /// Create physically-based billboard geometry.
    let createPhysicallyBasedBillboardGeometry contextOpt =
        let (vertexData, indexData, bounds) = createPhysicallyBasedBillboardMesh ()
        createPhysicallyBasedStaticGeometry VkPrimitiveTopology.TriangleList (vertexData.AsMemory ()) (indexData.AsMemory ()) bounds contextOpt

    /// Create physically-based static geometry from an assimp mesh.
    let createPhysicallyBasedStaticGeometryFromMesh indexData (mesh : Assimp.Mesh) contextOpt =
        match createPhysicallyBasedStaticMesh indexData mesh with
        | (vertexData, indexData, bounds) -> createPhysicallyBasedStaticGeometry VkPrimitiveTopology.TriangleList (vertexData.AsMemory ()) (indexData.AsMemory ()) bounds contextOpt

    /// Create physically-based animated geometry from a mesh.
    let createPhysicallyBasedAnimatedGeometry primitiveTopology (vertexData : single Memory) (indexData : int Memory) bounds contextOpt =

        // make buffers
        let (vertices, indices, vertexBuffer, instanceBuffer, indexBuffer) =

            // make renderable
            match contextOpt with
            | Some context ->

                // create buffers
                let vertexBuffer = VulkanBuffer.createVertexStagedFromMemory vertexData context
                let instanceBuffer = VulkanBuffer.create Instance (Constants.Render.InstanceFieldCount * sizeof<single>) context
                let indexBuffer = VulkanBuffer.createIndexStagedFromMemory indexData context

                // prepare instance buffer
                let instanceData = Array.zeroCreate Constants.Render.InstanceFieldCount
                m4Identity.ToArray (instanceData, 0)
                VulkanBuffer.uploadArray instanceData instanceBuffer context
                
                // fin
                ([||], [||], vertexBuffer, instanceBuffer, indexBuffer)

            // fake buffers
            | None ->

                // compute vertices
                let vertices = Array.zeroCreate (vertexData.Length / 16)
                let vertexData = vertexData.Span
                for i in 0 .. dec vertices.Length do
                    let j = i * 16
                    let vertex = v3 vertexData[j] vertexData[j+1] vertexData[j+2]
                    vertices[i] <- vertex

                // create indices
                let indices = indexData.ToArray ()

                // fin
                (vertices, indices, Unchecked.defaultof<VulkanBuffer>, Unchecked.defaultof<VulkanBuffer>, Unchecked.defaultof<VulkanBuffer>)

        // make physically-based geometry
        let geometry =
            { Bounds = bounds
              PrimitiveTopology = primitiveTopology
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
    let createPhysicallyBasedAnimatedGeometryFromMesh indexData (mesh : Assimp.Mesh) contextOpt =
        match createPhysicallyBasedAnimatedMesh indexData mesh with
        | (vertexData, indexData, bounds) -> createPhysicallyBasedAnimatedGeometry VkPrimitiveTopology.TriangleList (vertexData.AsMemory ()) (indexData.AsMemory ()) bounds contextOpt

    /// Create physically-based static geometries from an assimp scene.
    /// OPTIMIZATION: duplicate geometry is detected and deduplicated here, which does have some run-time cost.
    let createPhysicallyBasedStaticGeometries (scene : Assimp.Scene) contextOpt =
        let meshAndGeometryLists = Dictionary<int * int * Assimp.BoundingBox, (Assimp.Mesh * PhysicallyBasedGeometry) List> HashIdentity.Structural
        let geometries = SList.make ()
        for i in 0 .. dec scene.Meshes.Count do
            let indexDataEntry = scene.Metadata["IndexData" + string i]
            let indexData = indexDataEntry.Data :?> int array
            let mesh = scene.Meshes[i]
            let mutable found = false
            let meshAndGeometryListOpt = Dictionary.tryFind (mesh.VertexCount, mesh.FaceCount, mesh.BoundingBox) meshAndGeometryLists
            match meshAndGeometryListOpt with
            | Some (meshAndGeometry : (Assimp.Mesh * PhysicallyBasedGeometry) List) ->
                let mutable enr = meshAndGeometry.GetEnumerator ()
                while not found && enr.MoveNext () do
                    let (meshCached, geometryCached) = enr.Current
                    if  Enumerable.SequenceEqual (meshCached.Vertices, mesh.Vertices) && 
                        Enumerable.SequenceEqual (meshCached.TextureCoordinateChannels[0], mesh.TextureCoordinateChannels[0]) && 
                        Enumerable.SequenceEqual (meshCached.Normals, mesh.Normals) then
                        geometries.Add geometryCached
                        found <- true
            | None -> ()
            if not found then
                let geometry = createPhysicallyBasedStaticGeometryFromMesh indexData mesh contextOpt
                match meshAndGeometryListOpt with
                | Some meshesAndGeometries -> meshesAndGeometries.Add (mesh, geometry)
                | None -> meshAndGeometryLists[(mesh.VertexCount, mesh.FaceCount, mesh.BoundingBox)] <- List [(mesh, geometry)]
                geometries.Add geometry
        geometries

    /// Create physically-based animated geometries from an assimp scene.
    /// TODO: consider deduplicating geometry like in createPhysicallyBasedStaticGeometries?
    let createPhysicallyBasedAnimatedGeometries (scene : Assimp.Scene) contextOpt =
        let geometries = SList.make ()
        for i in 0 .. dec scene.Meshes.Count do
            let indexDataEntry = scene.Metadata["IndexData" + string i]
            let indexData = indexDataEntry.Data :?> int array
            let mesh = scene.Meshes[i]
            let geometry = createPhysicallyBasedAnimatedGeometryFromMesh indexData mesh contextOpt
            geometries.Add geometry
        geometries

    /// Create physically-based terrain geometry from a mesh.
    let createPhysicallyBasedTerrainGeometry primitiveTopology (vertexData : single Memory) (indexData : int Memory) bounds contextOpt =

        // make buffers
        let (vertices, indices, vertexBuffer, instanceBuffer, indexBuffer) =

            // make renderable
            match contextOpt with
            | Some context ->

                // create buffers
                let vertexBuffer = VulkanBuffer.createVertexStagedFromMemory vertexData context
                let instanceBuffer = VulkanBuffer.create Instance (Constants.Render.InstanceFieldCount * sizeof<single>) context
                let indexBuffer = VulkanBuffer.createIndexStagedFromMemory indexData context

                // prepare instance buffer
                let instanceData = Array.zeroCreate Constants.Render.InstanceFieldCount
                m4Identity.ToArray (instanceData, 0)
                VulkanBuffer.uploadArray instanceData instanceBuffer context
                
                // fin
                ([||], [||], vertexBuffer, instanceBuffer, indexBuffer)

            // fake buffers
            | None ->

                // compute vertices
                let vertices = Array.zeroCreate (vertexData.Length / 19)
                let vertexData = vertexData.Span
                for i in 0 .. dec vertices.Length do
                    let j = i * 19
                    let vertex = v3 vertexData[j] vertexData[j+1] vertexData[j+2]
                    vertices[i] <- vertex

                // create indices
                let indices = indexData.ToArray ()

                // fin
                (vertices, indices, Unchecked.defaultof<VulkanBuffer>, Unchecked.defaultof<VulkanBuffer>, Unchecked.defaultof<VulkanBuffer>)

        // make physically-based geometry
        let geometry =
            { Bounds = bounds
              PrimitiveTopology = primitiveTopology
              ElementCount = indexData.Length
              Vertices = vertices
              Indices = indices
              TrianglesCached = None
              VertexBuffer = vertexBuffer
              InstanceBuffer = instanceBuffer
              IndexBuffer = indexBuffer }

        // fin
        geometry

    /// Destroy physically-based geometry resources.
    let destroyPhysicallyBasedGeometry geometry context =
        VulkanBuffer.destroy geometry.VertexBuffer context
        VulkanBuffer.destroy geometry.InstanceBuffer context
        VulkanBuffer.destroy geometry.IndexBuffer context

    /// Destroy physically-based model resources.
    /// NOTE: models are created via a PhysicallyBasedSceneClient instance.
    let destroyPhysicallyBasedModel (model : PhysicallyBasedModel) context =
        for surface in model.Surfaces do
            destroyPhysicallyBasedGeometry surface.PhysicallyBasedGeometry context

    /// Create a box filter pipeline.
    let createFilterBoxPipeline shaderPath colorAttachmentFormat =

        // create pipeline
        let pipeline =
            Pipeline.create
                shaderPath
                [|VulkanUnblended|] [|false|] StaticVertices
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 SampledImage FragmentStage 1|] // inputTexture
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|] // inputSampler
                [||] [|colorAttachmentFormat|] None
                [||]

        // make pipeline
        let filterBoxPipeline =
            { FilterBoxPipeline.Pipeline = pipeline }

        // fin
        filterBoxPipeline

    /// Destroy a box filter pipeline.
    let destroyFilterBoxPipeline (filterBoxPipeline : FilterBoxPipeline) context =
        Pipeline.destroy filterBoxPipeline.Pipeline context

    /// Draw the box filter pass of a physically-based surface.
    let drawFilterBoxSurface
        (inputTexture : Texture)
        (inputSampler : Sampler)
        (colorAttachment : Texture)
        (resolution : Vector2i)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : FilterBoxPipeline)
        (context : VulkanContext) =

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify texture
            let mutable textureDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.Pipeline.DrawIndex pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampledTexture 0 0 inputTexture vkSet

            // specify sampler
            let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 inputSampler vkSet

            // set up render
            let clearValue = VkClearValue (r = Constants.Render.ViewportClearColor.R, g = Constants.Render.ViewportClearColor.G, b = Constants.Render.ViewportClearColor.B, a = Constants.Render.ViewportClearColor.A)
            let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
            let mutable vkViewport = Hl.makeViewport false renderArea
            Hl.withRenderingInfo [|colorAttachment.ImageView|] None renderArea (Some clearValue) $ fun renderingInfo ->
                let mutable renderingInfo = renderingInfo
                DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
            DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
            DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

            // set up pipeline
            DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&textureDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&samplerDescriptorSet, 0u, nullPtr)

            // draw
            DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // tear down render
            DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

            // report drawing
            Hl.reportDrawCall 1 true

            // advance pipeline
            Pipeline.advance pipeline.Pipeline

            // advance rendering command buffer
            VulkanContext.advanceRenderCommandBuffer context

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// Create a down-sampler filter pipeline.
    let createFilterDownSamplePipeline shaderFilePath colorAttachmentFormats =

        // create pipeline
        let pipeline =
            Pipeline.create
                shaderFilePath
                [|VulkanUnblended|] [|false|] StaticVertices
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 SampledImage FragmentStage 1 // colorTexture
                      Pipeline.descriptor 1 SampledImage FragmentStage 1|] // depthTexture
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|] // filteredSampler
                [||] colorAttachmentFormats None
                [||]

        // make pipeline
        let filterDownSamplePipeline =
            { FilterDownSamplePipeline.Pipeline = pipeline }

        // fin
        filterDownSamplePipeline

    /// Destroy a down-sample filter pipeline.
    let destroyFilterDownSamplePipeline (downSamplePipeline : FilterDownSamplePipeline) context =
        Pipeline.destroy downSamplePipeline.Pipeline context

    /// Draw the down-sample filter pass of a physically-based surface.
    let drawFilterDownSampleSurface
        (colorTexture : Texture)
        (depthTexture : Texture)
        (filteredSampler : Sampler)
        (colorAttachment : VkImageView)
        (depthAttachment : VkImageView)
        (resolution : Vector2i)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : FilterDownSamplePipeline)
        (context : VulkanContext) =

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify textures
            let mutable texturesDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.Pipeline.DrawIndex pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampledTexture 0 0 colorTexture vkSet
                Pipeline.writeDescriptorSampledTexture 1 0 depthTexture vkSet

            // specify sampler
            let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 filteredSampler vkSet

            // set up render
            let clearValue = VkClearValue (r = Constants.Render.ViewportClearColor.R, g = Constants.Render.ViewportClearColor.G, b = Constants.Render.ViewportClearColor.B, a = Constants.Render.ViewportClearColor.A)
            let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
            let mutable vkViewport = Hl.makeViewport false renderArea
            Hl.withRenderingInfo [|colorAttachment; depthAttachment|] None renderArea (Some clearValue) $ fun renderingInfo ->
                let mutable renderingInfo = renderingInfo
                DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
            DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
            DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

            // set up pipeline
            DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&texturesDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&samplerDescriptorSet, 0u, nullPtr)

            // draw
            DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // tear down render
            DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

            // report drawing
            Hl.reportDrawCall 1 true

            // advance pipeline
            Pipeline.advance pipeline.Pipeline

            // advance rendering command buffer
            VulkanContext.advanceRenderCommandBuffer context

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// Create an up-sampler filter pipeline.
    let createFilterUpSamplePipeline shaderFilePath colorAttachmentFormat =

        // create pipeline
        let pipeline =
            Pipeline.create
                shaderFilePath
                [|VulkanUnblended|] [|false|] StaticVertices
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 SampledImage FragmentStage 1 // downSampledColorTexture
                      Pipeline.descriptor 1 SampledImage FragmentStage 1 // downSampledDepthTexture
                      Pipeline.descriptor 2 SampledImage FragmentStage 1|] // depthTexture
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|] // filteredSampler
                [||] [|colorAttachmentFormat|] None
                [||]

        // make pipeline
        let filterUpSamplePipeline =
            { FilterUpSamplePipeline.Pipeline = pipeline }

        // fin
        filterUpSamplePipeline

    /// Destroy a up-sample filter pipeline.
    let destroyFilterUpSamplePipeline (upSamplePipeline : FilterUpSamplePipeline) context =
        Pipeline.destroy upSamplePipeline.Pipeline context

    /// Draw the up-sample filter pass of a physically-based surface.
    let drawFilterUpSampleSurface
        (downSampledColorTexture : Texture)
        (downSampledDepthTexture : Texture)
        (depthTexture : Texture)
        (filteredSampler : Sampler)
        (colorAttachment : VkImageView)
        (resolution : Vector2i)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : FilterUpSamplePipeline)
        (context : VulkanContext) =

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify textures
            let mutable texturesDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.Pipeline.DrawIndex pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampledTexture 0 0 downSampledColorTexture vkSet
                Pipeline.writeDescriptorSampledTexture 1 0 downSampledDepthTexture vkSet
                Pipeline.writeDescriptorSampledTexture 2 0 depthTexture vkSet

            // specify sampler
            let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 filteredSampler vkSet

            // set up render
            let clearValue = VkClearValue (r = Constants.Render.ViewportClearColor.R, g = Constants.Render.ViewportClearColor.G, b = Constants.Render.ViewportClearColor.B, a = Constants.Render.ViewportClearColor.A)
            let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
            let mutable vkViewport = Hl.makeViewport false renderArea
            Hl.withRenderingInfo [|colorAttachment|] None renderArea (Some clearValue) $ fun renderingInfo ->
                let mutable renderingInfo = renderingInfo
                DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
            DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
            DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

            // set up pipeline
            DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&texturesDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&samplerDescriptorSet, 0u, nullPtr)

            // draw
            DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // tear down render
            DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

            // report drawing
            Hl.reportDrawCall 1 true

            // advance pipeline
            Pipeline.advance pipeline.Pipeline

            // advance rendering command buffer
            VulkanContext.advanceRenderCommandBuffer context

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// Create an esm guassian filter pipeline.
    let createFilterGaussianEsmPipeline colorAttachmentFormat context =

        // create set 0 uniform buffers
        let gaussianEsmUniform = VulkanBuffer.create Uniform sizeof<GaussianEsmStruct> context

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.FilterGaussianEsmShaderFilePath
                [|VulkanUnblended|] [|false|] StaticVertices
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer FragmentStage 1|] // gaussianEsm
                  Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 SampledImage FragmentStage 1|] // inputTexture
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|] // inputSampler
                [||] [|colorAttachmentFormat|] None
                [|gaussianEsmUniform|]

        // make pipeline
        let filterGaussianEsmPipeline =
            { GaussianEsmUniform = gaussianEsmUniform
              Pipeline = pipeline }

        // fin
        filterGaussianEsmPipeline

    /// Destroy an esm gaussian filter pipeline.
    let destroyFilterGaussianEsmPipeline (gaussianEsmPipeline : FilterGaussianEsmPipeline) context =
        Pipeline.destroy gaussianEsmPipeline.Pipeline context

    /// Draw the esm gaussian filter pass of a physically-based surface.
    let drawFilterGaussianEsmSurface
        (scale : Vector2)
        (radius : single)
        (esmImageView : VkImageView)
        (filteredSampler : Sampler)
        (colorAttachment : VkImageView)
        (resolution : Vector2i)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : FilterGaussianEsmPipeline)
        (context : VulkanContext) =

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify gaussianEsm
            let mutable gaussianEsmDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.Pipeline.DrawIndex pipeline.Pipeline $ fun vkSet ->
                let gaussianEsm = GaussianEsmStruct (scale = scale, radius = radius)
                VulkanBuffer.uploadValue gaussianEsm pipeline.GaussianEsmUniform context
                Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.GaussianEsmUniform vkSet

            // specify image views
            let mutable imageViewsDescriptorSet = Pipeline.specifyDescriptorSet 1 pipeline.Pipeline.DrawIndex pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampledImageView 0 0 esmImageView vkSet

            // specify sampler
            let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 2 Unit pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 filteredSampler vkSet

            // set up render
            let clearValue = VkClearValue (1.0f, Single.MaxValue, 0.0f, 0.0f) // TODO: P1: make derived from constant.
            let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
            let mutable vkViewport = Hl.makeViewport false renderArea
            Hl.withRenderingInfo [|colorAttachment|] None renderArea (Some clearValue) $ fun renderingInfo ->
                let mutable renderingInfo = renderingInfo
                DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
            DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
            DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

            // set up pipeline
            DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&gaussianEsmDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&imageViewsDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 2u, 1u, &&samplerDescriptorSet, 0u, nullPtr)

            // draw
            DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // tear down render
            DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

            // report drawing
            Hl.reportDrawCall 1 true

            // advance pipeline
            Pipeline.advance pipeline.Pipeline

            // advance rendering command buffer
            VulkanContext.advanceRenderCommandBuffer context

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// Create a depth-of-field guassian filter pipeline.
    let createFilterGaussianDofPipeline colorAttachmentFormat context =

        // create set 0 uniform buffers
        let gaussianDofUniform = VulkanBuffer.create Uniform sizeof<GaussianDofStruct> context

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.FilterGaussianDofShaderFilePath
                [|VulkanUnblended|] [|false|] StaticVertices
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer FragmentStage 1|] // gaussianDof
                  Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 SampledImage FragmentStage 1|] // inputTexture
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|] // inputSampler
                [||] [|colorAttachmentFormat|] None
                [|gaussianDofUniform|]

        // make pipeline
        let filterGaussianDofPipeline =
            { GaussianDofUniform = gaussianDofUniform
              Pipeline = pipeline }

        // fin
        filterGaussianDofPipeline

    /// Destroy a depth-of-field gaussian filter pipeline.
    let destroyFilterGaussianDofPipeline (gaussianDofPipeline : FilterGaussianDofPipeline) context =
        Pipeline.destroy gaussianDofPipeline.Pipeline context

    /// Draw the depth-of-field gaussian filter pass of a physically-based surface.
    let drawFilterGaussianDofSurface
        (scale : Vector2)
        (radius : single)
        (dofImageView : VkImageView)
        (filteredSampler : Sampler)
        (colorAttachment : VkImageView)
        (resolution : Vector2i)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : FilterGaussianDofPipeline)
        (context : VulkanContext) =

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify gaussianDof
            let mutable gaussianDofDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.Pipeline.DrawIndex pipeline.Pipeline $ fun vkSet ->
                let gaussianDof = GaussianDofStruct (scale = scale, radius = radius)
                VulkanBuffer.uploadValue gaussianDof pipeline.GaussianDofUniform context
                Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.GaussianDofUniform vkSet

            // specify image views
            let mutable imageViewsDescriptorSet = Pipeline.specifyDescriptorSet 1 pipeline.Pipeline.DrawIndex pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampledImageView 0 0 dofImageView vkSet

            // specify sampler
            let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 2 Unit pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 filteredSampler vkSet

            // set up render
            let clearValue = VkClearValue (1.0f, Single.MaxValue, 0.0f, 0.0f) // TODO: P1: make derived from constant.
            let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
            let mutable vkViewport = Hl.makeViewport false renderArea
            Hl.withRenderingInfo [|colorAttachment|] None renderArea (Some clearValue) $ fun renderingInfo ->
                let mutable renderingInfo = renderingInfo
                DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
            DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
            DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

            // set up pipeline
            DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&gaussianDofDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&imageViewsDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 2u, 1u, &&samplerDescriptorSet, 0u, nullPtr)

            // draw
            DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // tear down render
            DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

            // report drawing
            Hl.reportDrawCall 1 true

            // advance pipeline
            Pipeline.advance pipeline.Pipeline

            // advance rendering command buffer
            VulkanContext.advanceRenderCommandBuffer context

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// Create a depth-of-field filter pipeline.
    let createFilterDepthOfFieldPipeline colorAttachmentFormat context =

        // create set 0 uniform buffers
        let eyeUniform = VulkanBuffer.create Uniform sizeof<EyeStruct> context
        let depthOfFieldUniform = VulkanBuffer.create Uniform sizeof<DepthOfFieldStruct> context

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.FilterDepthOfFieldShaderFilePath
                [|VulkanUnblended|] [|false|] StaticVertices
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer FragmentStage 1 // eye
                      Pipeline.descriptor 1 UniformBuffer FragmentStage 1 // depthOfField
                      Pipeline.descriptor 2 SampledImage FragmentStage 1 // depthTexture
                      Pipeline.descriptor 3 SampledImage FragmentStage 1 // blurredTexture
                      Pipeline.descriptor 4 SampledImage FragmentStage 1|] // unblurredTexture
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|] // unfilteredSampler
                [||] [|colorAttachmentFormat|] None
                [|eyeUniform; depthOfFieldUniform|]

        // make pipeline
        let filterDepthOfFieldPipeline =
            { EyeUniform = eyeUniform
              DepthOfFieldUniform = depthOfFieldUniform
              Pipeline = pipeline }

        // fin
        filterDepthOfFieldPipeline

    /// Destroy a tone-mapping filter pipeline.
    let destroyFilterDepthOfFieldPipeline (depthOfFieldPipeline : FilterDepthOfFieldPipeline) context =
        Pipeline.destroy depthOfFieldPipeline.Pipeline context

    /// Draw the depth-of-field filter pass of a physically-based surface.
    let drawFilterDepthOfFieldSurface
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (projectionUnflipped : Matrix4x4)
        (nearDistance : single)
        (farDistance : single)
        (focalType : int)
        (focalDistance : single)
        (focalPoint : Vector2)
        (depthTexture : Texture)
        (blurredTexture : Texture)
        (unblurredTexture : Texture)
        (unfilteredSampler : Sampler)
        (colorAttachment : Texture)
        (resolution : Vector2i)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : FilterDepthOfFieldPipeline)
        (context : VulkanContext) =

        // compute vulkan-appropriate matrices
        let viewInverse = view.Inverted
        let projection = projectionUnflipped.Flipped
        let projectionInverse = projection.Inverted
        let viewProjection = view * projection

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.Pipeline.DrawIndex pipeline.Pipeline $ fun vkSet ->

                // specify eye
                let eye = EyeStruct (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
                VulkanBuffer.uploadValue eye pipeline.EyeUniform context
                Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.EyeUniform vkSet
                
                // specify depth-of-field
                let depthOfField =
                    DepthOfFieldStruct
                        (nearDistance = nearDistance,
                         farDistance = farDistance,
                         focalType = focalType,
                         focalDistance = focalDistance,
                         focalPoint = focalPoint)
                VulkanBuffer.uploadValue depthOfField pipeline.DepthOfFieldUniform context
                Pipeline.writeDescriptorUniformBuffer 1 0 pipeline.DepthOfFieldUniform vkSet

                // specify textures
                Pipeline.writeDescriptorSampledTexture 2 0 depthTexture vkSet
                Pipeline.writeDescriptorSampledTexture 3 0 blurredTexture vkSet
                Pipeline.writeDescriptorSampledTexture 4 0 unblurredTexture vkSet

            // specify sampler
            let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 unfilteredSampler vkSet

            // set up render
            let clearValue = VkClearValue (r = Constants.Render.ViewportClearColor.R, g = Constants.Render.ViewportClearColor.G, b = Constants.Render.ViewportClearColor.B, a = Constants.Render.ViewportClearColor.A)
            let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
            let mutable vkViewport = Hl.makeViewport false renderArea
            Hl.withRenderingInfo [|colorAttachment.ImageView|] None renderArea (Some clearValue) $ fun renderingInfo ->
                let mutable renderingInfo = renderingInfo
                DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
            DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
            DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

            // set up pipeline
            DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&uniformsDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&samplerDescriptorSet, 0u, nullPtr)

            // draw
            DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // tear down render
            DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

            // report drawing
            Hl.reportDrawCall 1 true

            // advance pipeline
            Pipeline.advance pipeline.Pipeline

            // advance rendering command buffer
            VulkanContext.advanceRenderCommandBuffer context

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// Create a bloom down-sample filter pipeline.
    let createFilterBloomDownSamplePipeline colorAttachmentFormat context =

        // create set 0 uniform buffers
        let bloomDownSampleUniform = VulkanBuffer.create Uniform sizeof<BloomDownSampleStruct> context

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.FilterBloomDownSampleShaderFilePath
                [|VulkanUnblended|] [|false|] StaticVertices
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer FragmentStage 1 // bloomDownSample
                      Pipeline.descriptor 1 SampledImage FragmentStage 1|] // inputTexture
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|] // inputSampler
                [||] [|colorAttachmentFormat|] None
                [|bloomDownSampleUniform|]

        // make pipeline
        let filterBloomDownSamplePipeline =
            { BloomDownSampleUniform = bloomDownSampleUniform
              Pipeline = pipeline }

        // fin
        filterBloomDownSamplePipeline

    /// Destroy a bloom down-sample filter pipeline.
    let destroyFilterBloomDownSamplePipeline (bloomDownSamplePipeline : FilterBloomDownSamplePipeline) context =
        Pipeline.destroy bloomDownSamplePipeline.Pipeline context

    /// Draw the bloom down-sample filter passes of a physically-based surface.
    let drawFilterBloomDownSampleSurfaces
        (karisAverageEnabled : bool)
        (inputResolution : Vector2i)
        (inputTexture : Texture)
        (inputSampler : Sampler)
        (colorAttachments : Texture array)
        (resolution : Vector2i)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : FilterBloomDownSamplePipeline)
        (context : VulkanContext) =

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // draw down-sample levels
            for i in 0 .. dec Constants.Render.BloomSampleLevels do

                // compute source resolution and texture
                let sourceResolution = v2i (inputResolution.X >>> i) (inputResolution.Y >>> i)
                let sourceTexture = if i = 0 then inputTexture else colorAttachments[dec i]

                // compute target resolution and texture
                let targetResolution = v2i (resolution.X >>> i) (resolution.Y >>> i)
                let targetTexture = colorAttachments[i]

                // transition target to write
                Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite targetTexture context.RenderCommandBuffer

                // specify uniforms
                let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.Pipeline.DrawIndex pipeline.Pipeline $ fun vkSet ->

                    // specify bloom down-sample
                    let karisAverageEnabledInt = if karisAverageEnabled then 1 else 0
                    let bloomDownSample = BloomDownSampleStruct (karisAverageEnabled = karisAverageEnabledInt, sampleLevel = i, sourceResolution = sourceResolution.V2)
                    VulkanBuffer.uploadValue bloomDownSample pipeline.BloomDownSampleUniform context
                    Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.BloomDownSampleUniform vkSet

                    // specify input texture
                    Pipeline.writeDescriptorSampledTexture 1 0 sourceTexture vkSet

                // specify sampler
                let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline $ fun vkSet ->
                    Pipeline.writeDescriptorSampler 0 0 inputSampler vkSet

                // set up render
                let mutable renderArea = VkRect2D (0, 0, uint targetResolution.X, uint targetResolution.Y)
                let mutable vkViewport = Hl.makeViewport false renderArea
                Hl.withRenderingInfo [|targetTexture.ImageView|] None renderArea None $ fun renderingInfo ->
                    let mutable renderingInfo = renderingInfo
                    DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
                DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
                DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

                // set up pipeline
                DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

                // bind vertex and index buffers
                let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
                let vertexOffsets = [|0UL; 0UL|]
                use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
                use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
                DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
                DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

                // bind descriptor sets
                DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&uniformsDescriptorSet, 0u, nullPtr)
                DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&samplerDescriptorSet, 0u, nullPtr)

                // draw
                DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

                // tear down render
                DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

                // report drawing
                Hl.reportDrawCall 1 true

                // advance pipeline
                Pipeline.advance pipeline.Pipeline

                // advance rendering command buffer
                VulkanContext.advanceRenderCommandBuffer context
                
                // transition target to read
                Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead targetTexture context.RenderCommandBuffer

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// Create a bloom up-sample filter pipeline.
    let createFilterBloomUpSamplePipeline colorAttachmentFormat context =

        // create set 0 uniform buffers
        let bloomUpSampleUniform = VulkanBuffer.create Uniform sizeof<BloomUpSampleStruct> context

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.FilterBloomUpSampleShaderFilePath
                [|VulkanSummation|] [|false|] StaticVertices
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer FragmentStage 1 // bloomUpSample
                      Pipeline.descriptor 1 SampledImage FragmentStage 1|] // inputTexture
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|] // inputSampler
                [||] [|colorAttachmentFormat|] None
                [|bloomUpSampleUniform|]

        // make pipeline
        let filterBloomUpSamplePipeline =
            { BloomUpSampleUniform = bloomUpSampleUniform
              Pipeline = pipeline }

        // fin
        filterBloomUpSamplePipeline

    /// Destroy a bloom up-sample filter pipeline.
    let destroyFilterBloomUpSamplePipeline (bloomUpSamplePipeline : FilterBloomUpSamplePipeline) context =
        Pipeline.destroy bloomUpSamplePipeline.Pipeline context

    /// Draw the bloom up-sample filter passes of a physically-based surface.
    let drawFilterBloomUpSampleSurfaces
        (radius : single)
        (inputTextures : Texture array)
        (inputSampler : Sampler)
        (resolution : Vector2i)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : FilterBloomUpSamplePipeline)
        (context : VulkanContext) =

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanSummation false pipeline.Pipeline with
        | Some vkPipeline ->

            // draw up-sample levels
            for i in dec Constants.Render.BloomSampleLevels .. -1 .. 1 do

                // compute source texture
                let sourceTexture = inputTextures[i]

                // compute target resolution and texture
                let targetResolution = v2i (resolution.X >>> i) (resolution.Y >>> i)
                let targetTexture = inputTextures[dec i]

                // transition target to write
                Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite targetTexture context.RenderCommandBuffer

                // specify uniforms
                let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.Pipeline.DrawIndex pipeline.Pipeline $ fun vkSet ->

                    // specify bloom up-sample
                    let bloomUpSample = BloomUpSampleStruct (radius = radius)
                    VulkanBuffer.uploadValue bloomUpSample pipeline.BloomUpSampleUniform context
                    Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.BloomUpSampleUniform vkSet

                    // specify input texture
                    Pipeline.writeDescriptorSampledTexture 1 0 sourceTexture vkSet

                // specify sampler
                let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline $ fun vkSet ->
                    Pipeline.writeDescriptorSampler 0 0 inputSampler vkSet

                // set up render
                let mutable renderArea = VkRect2D (0, 0, uint targetResolution.X, uint targetResolution.Y)
                let mutable vkViewport = Hl.makeViewport false renderArea
                Hl.withRenderingInfo [|targetTexture.ImageView|] None renderArea None $ fun renderingInfo ->
                    let mutable renderingInfo = renderingInfo
                    DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
                DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
                DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

                // set up pipeline
                DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

                // bind vertex and index buffers
                let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
                let vertexOffsets = [|0UL; 0UL|]
                use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
                use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
                DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
                DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

                // bind descriptor sets
                DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&uniformsDescriptorSet, 0u, nullPtr)
                DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&samplerDescriptorSet, 0u, nullPtr)

                // draw
                DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

                // tear down render
                DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

                // report drawing
                Hl.reportDrawCall 1 true

                // advance pipeline
                Pipeline.advance pipeline.Pipeline

                // advance rendering command buffer
                VulkanContext.advanceRenderCommandBuffer context
                
                // transition target to read
                Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead targetTexture context.RenderCommandBuffer

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// Create a bloom extract filter pipeline.
    let createFilterBloomExtractPipeline colorAttachmentFormat context =

        // create set 0 uniform buffers
        let bloomExtractUniform = VulkanBuffer.create Uniform sizeof<BloomExtractStruct> context

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.FilterBloomExtractShaderFilePath
                [|VulkanUnblended|] [|false|] StaticVertices
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer FragmentStage 1 // bloomExtract
                      Pipeline.descriptor 1 SampledImage FragmentStage 1|] // inputTexture
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|] // inputSampler
                [||] [|colorAttachmentFormat|] None
                [|bloomExtractUniform|]

        // make pipeline
        let filterBloomExtractPipeline =
            { BloomExtractUniform = bloomExtractUniform
              Pipeline = pipeline }

        // fin
        filterBloomExtractPipeline

    /// Destroy a bloom extract filter pipeline.
    let destroyFilterBloomExtractPipeline (bloomExtractPipeline : FilterBloomExtractPipeline) context =
        Pipeline.destroy bloomExtractPipeline.Pipeline context

    /// Draw the bloom extract filter pass of a physically-based surface.
    let drawFilterBloomExtractSurface
        (threshold : single)
        (inputTexture : Texture)
        (inputSampler : Sampler)
        (colorAttachment : Texture)
        (resolution : Vector2i)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : FilterBloomExtractPipeline)
        (context : VulkanContext) =

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.Pipeline.DrawIndex pipeline.Pipeline $ fun vkSet ->
                
                // specify bloom extract
                let bloomExtract = BloomExtractStruct (threshold = threshold)
                VulkanBuffer.uploadValue bloomExtract pipeline.BloomExtractUniform context
                Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.BloomExtractUniform vkSet

                // specify input texture
                Pipeline.writeDescriptorSampledTexture 1 0 inputTexture vkSet

            // specify sampler
            let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 inputSampler vkSet

            // set up render
            let clearValue = VkClearValue (r = Constants.Render.ViewportClearColor.R, g = Constants.Render.ViewportClearColor.G, b = Constants.Render.ViewportClearColor.B, a = Constants.Render.ViewportClearColor.A)
            let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
            let mutable vkViewport = Hl.makeViewport false renderArea
            Hl.withRenderingInfo [|colorAttachment.ImageView|] None renderArea (Some clearValue) $ fun renderingInfo ->
                let mutable renderingInfo = renderingInfo
                DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
            DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
            DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

            // set up pipeline
            DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&uniformsDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&samplerDescriptorSet, 0u, nullPtr)

            // draw
            DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // tear down render
            DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

            // report drawing
            Hl.reportDrawCall 1 true

            // advance pipeline
            Pipeline.advance pipeline.Pipeline

            // advance rendering command buffer
            VulkanContext.advanceRenderCommandBuffer context

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// Create a bloom apply filter pipeline.
    let createFilterBloomApplyPipeline colorAttachmentFormat context =

        // create set 0 uniform buffers
        let bloomApplyUniform = VulkanBuffer.create Uniform sizeof<BloomApplyStruct> context

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.FilterBloomApplyShaderFilePath
                [|VulkanUnblended|] [|false|] StaticVertices
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer FragmentStage 1 // bloomApply
                      Pipeline.descriptor 1 SampledImage FragmentStage 1 // bloomFilterTexture
                      Pipeline.descriptor 2 SampledImage FragmentStage 1|] // compositionTexture
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|] // inputSampler
                [||] [|colorAttachmentFormat|] None
                [|bloomApplyUniform|]

        // make pipeline
        let filterBloomApplyPipeline =
            { BloomApplyUniform = bloomApplyUniform
              Pipeline = pipeline }

        // fin
        filterBloomApplyPipeline

    /// Destroy a bloom apply filter pipeline.
    let destroyFilterBloomApplyPipeline (bloomApplyPipeline : FilterBloomApplyPipeline) context =
        Pipeline.destroy bloomApplyPipeline.Pipeline context

    /// Draw the bloom apply filter pass of a physically-based surface.
    let drawFilterBloomApplySurface
        (strength : single)
        (bloomApplyTexture : Texture)
        (compositionTexture : Texture)
        (inputSampler : Sampler)
        (colorAttachment : Texture)
        (resolution : Vector2i)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : FilterBloomApplyPipeline)
        (context : VulkanContext) =

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.Pipeline.DrawIndex pipeline.Pipeline $ fun vkSet ->
                
                // specify bloom apply
                let bloomApply = BloomApplyStruct (strength = strength)
                VulkanBuffer.uploadValue bloomApply pipeline.BloomApplyUniform context
                Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.BloomApplyUniform vkSet

                // specify textures
                Pipeline.writeDescriptorSampledTexture 1 0 bloomApplyTexture vkSet
                Pipeline.writeDescriptorSampledTexture 2 0 compositionTexture vkSet

            // specify sampler
            let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 inputSampler vkSet

            // set up render
            let clearValue = VkClearValue (r = Constants.Render.ViewportClearColor.R, g = Constants.Render.ViewportClearColor.G, b = Constants.Render.ViewportClearColor.B, a = Constants.Render.ViewportClearColor.A)
            let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
            let mutable vkViewport = Hl.makeViewport false renderArea
            Hl.withRenderingInfo [|colorAttachment.ImageView|] None renderArea (Some clearValue) $ fun renderingInfo ->
                let mutable renderingInfo = renderingInfo
                DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
            DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
            DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

            // set up pipeline
            DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&uniformsDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&samplerDescriptorSet, 0u, nullPtr)

            // draw
            DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // tear down render
            DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

            // report drawing
            Hl.reportDrawCall 1 true

            // advance pipeline
            Pipeline.advance pipeline.Pipeline

            // advance rendering command buffer
            VulkanContext.advanceRenderCommandBuffer context

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// Create a tone-mapping filter pipeline.
    let createFilterToneMappingPipeline colorAttachmentFormat context =

        // create set 0 uniform buffers
        let toneMappingUniform = VulkanBuffer.create Uniform sizeof<ToneMappingStruct> context

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.FilterToneMappingShaderFilePath
                [|VulkanUnblended|] [|false|] StaticVertices
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer FragmentStage 1 // toneMapping
                      Pipeline.descriptor 1 SampledImage FragmentStage 1|] // inputTexture
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|] // inputSampler
                [||] [|colorAttachmentFormat|] None
                [|toneMappingUniform|]

        // make pipeline
        let filterToneMappingPipeline =
            { ToneMappingUniform = toneMappingUniform
              Pipeline = pipeline }

        // fin
        filterToneMappingPipeline

    /// Destroy a tone-mapping filter pipeline.
    let destroyFilterToneMappingPipeline (toneMappingPipeline : FilterToneMappingPipeline) context =
        Pipeline.destroy toneMappingPipeline.Pipeline context

    /// Draw the tone-mapping filter pass of a physically-based surface.
    let drawFilterToneMappingSurface
        (lightExposure : single)
        (toneMapType : ToneMapType)
        (toneMapSlope : Vector3)
        (toneMapOffset : Vector3)
        (toneMapPower : Vector3)
        (toneMapSaturation : single)
        (toneMapWhitePoint : single)
        (inputTexture : Texture)
        (inputSampler : Sampler)
        (colorAttachment : Texture)
        (resolution : Vector2i)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : FilterToneMappingPipeline)
        (context : VulkanContext) =

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.Pipeline.DrawIndex pipeline.Pipeline $ fun vkSet ->
                
                // specify tone-mapping
                let toneMapping =
                    ToneMappingStruct
                        (lightExposure = lightExposure,
                         toneMapType = toneMapType.Enumerate,
                         toneMapSlope = toneMapSlope,
                         toneMapOffset = toneMapOffset,
                         toneMapPower = toneMapPower,
                         toneMapSaturation = toneMapSaturation,
                         toneMapWhitePoint = toneMapWhitePoint)
                VulkanBuffer.uploadValue toneMapping pipeline.ToneMappingUniform context
                Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.ToneMappingUniform vkSet

                // specify input texture
                Pipeline.writeDescriptorSampledTexture 1 0 inputTexture vkSet

            // specify sampler
            let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 inputSampler vkSet

            // set up render
            let clearValue = VkClearValue (r = Constants.Render.ViewportClearColor.R, g = Constants.Render.ViewportClearColor.G, b = Constants.Render.ViewportClearColor.B, a = Constants.Render.ViewportClearColor.A)
            let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
            let mutable vkViewport = Hl.makeViewport false renderArea
            Hl.withRenderingInfo [|colorAttachment.ImageView|] None renderArea (Some clearValue) $ fun renderingInfo ->
                let mutable renderingInfo = renderingInfo
                DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
            DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
            DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

            // set up pipeline
            DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&uniformsDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&samplerDescriptorSet, 0u, nullPtr)

            // draw
            DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // tear down render
            DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

            // report drawing
            Hl.reportDrawCall 1 true

            // advance pipeline
            Pipeline.advance pipeline.Pipeline

            // advance rendering command buffer
            VulkanContext.advanceRenderCommandBuffer context

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// Create a chromatic aberration filter pipeline.
    let createFilterChromaticAberrationPipeline colorAttachmentFormat context =

        // create set 0 uniform buffers
        let chromaticAberrationUniform = VulkanBuffer.create Uniform sizeof<ChromaticAberrationStruct> context

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.FilterChromaticAberrationShaderFilePath
                [|VulkanUnblended|] [|false|] StaticVertices
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer FragmentStage 1 // chromaticAberration
                      Pipeline.descriptor 1 SampledImage FragmentStage 1|] // inputTexture
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|] // inputSampler
                [||] [|colorAttachmentFormat|] None
                [|chromaticAberrationUniform|]

        // make pipeline
        let filterChromaticAberrationPipeline =
            { ChromaticAberrationUniform = chromaticAberrationUniform
              Pipeline = pipeline }

        // fin
        filterChromaticAberrationPipeline

    /// Destroy a tone-mapping filter pipeline.
    let destroyFilterChromaticAberrationPipeline (chromaticAberrationPipeline : FilterChromaticAberrationPipeline) context =
        Pipeline.destroy chromaticAberrationPipeline.Pipeline context

    /// Draw the tone-mapping filter pass of a physically-based surface.
    let drawFilterChromaticAberrationSurface
        (channelOffsets : Vector3)
        (focalPoint : Vector2)
        (inputTexture : Texture)
        (inputSampler : Sampler)
        (colorAttachment : Texture)
        (resolution : Vector2i)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : FilterChromaticAberrationPipeline)
        (context : VulkanContext) =

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.Pipeline.DrawIndex pipeline.Pipeline $ fun vkSet ->
                
                // specify tone-mapping
                let chromaticAberration = ChromaticAberrationStruct (channelOffsets = channelOffsets, focalPoint = focalPoint)
                VulkanBuffer.uploadValue chromaticAberration pipeline.ChromaticAberrationUniform context
                Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.ChromaticAberrationUniform vkSet

                // specify input texture
                Pipeline.writeDescriptorSampledTexture 1 0 inputTexture vkSet

            // specify sampler
            let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 inputSampler vkSet

            // set up render
            let clearValue = VkClearValue (r = Constants.Render.ViewportClearColor.R, g = Constants.Render.ViewportClearColor.G, b = Constants.Render.ViewportClearColor.B, a = Constants.Render.ViewportClearColor.A)
            let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
            let mutable vkViewport = Hl.makeViewport false renderArea
            Hl.withRenderingInfo [|colorAttachment.ImageView|] None renderArea (Some clearValue) $ fun renderingInfo ->
                let mutable renderingInfo = renderingInfo
                DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
            DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
            DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

            // set up pipeline
            DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&uniformsDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&samplerDescriptorSet, 0u, nullPtr)

            // draw
            DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // tear down render
            DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

            // report drawing
            Hl.reportDrawCall 1 true

            // advance pipeline
            Pipeline.advance pipeline.Pipeline

            // advance rendering command buffer
            VulkanContext.advanceRenderCommandBuffer context

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// Create an fxaa filter pipeline.
    let createFilterFxaaPipeline colorAttachmentFormat context =

        // create set 0 uniform buffers
        let fxaaUniform = VulkanBuffer.create Uniform sizeof<FxaaStruct> context

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.FilterFxaaShaderFilePath
                [|VulkanUnblended|] [|false|] StaticVertices
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer FragmentStage 1 // fxaa
                      Pipeline.descriptor 1 SampledImage FragmentStage 1|] // inputTexture
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|] // inputSampler
                [||] [|colorAttachmentFormat|] None
                [|fxaaUniform|]

        // make pipeline
        let filterFxaaPipeline =
            { FxaaUniform = fxaaUniform
              Pipeline = pipeline }

        // fin
        filterFxaaPipeline

    /// Destroy an fxaa filter pipeline.
    let destroyFilterFxaaPipeline (fxaaPipeline : FilterFxaaPipeline) context =
        Pipeline.destroy fxaaPipeline.Pipeline context

    /// Draw the fxaa filter pass of a physically-based surface.
    let drawFilterFxaaSurface
        (spanMax : single)
        (reduceMinDivisor : single)
        (reduceMulDivisor : single)
        (inputTexture : Texture)
        (inputSampler : Sampler)
        (colorAttachment : Texture)
        (resolution : Vector2i)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : FilterFxaaPipeline)
        (context : VulkanContext) =

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.Pipeline.DrawIndex pipeline.Pipeline $ fun vkSet ->
                
                // specify fxaa
                let fxaa = FxaaStruct (spanMax = spanMax, reduceMinDivisor = reduceMinDivisor, reduceMulDivisor = reduceMulDivisor)
                VulkanBuffer.uploadValue fxaa pipeline.FxaaUniform context
                Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.FxaaUniform vkSet

                // specify input texture
                Pipeline.writeDescriptorSampledTexture 1 0 inputTexture vkSet

            // specify sampler
            let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 inputSampler vkSet

            // set up render
            let clearValue = VkClearValue (r = Constants.Render.ViewportClearColor.R, g = Constants.Render.ViewportClearColor.G, b = Constants.Render.ViewportClearColor.B, a = Constants.Render.ViewportClearColor.A)
            let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
            let mutable vkViewport = Hl.makeViewport false renderArea
            Hl.withRenderingInfo [|colorAttachment.ImageView|] None renderArea (Some clearValue) $ fun renderingInfo ->
                let mutable renderingInfo = renderingInfo
                DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
            DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
            DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

            // set up pipeline
            DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&uniformsDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&samplerDescriptorSet, 0u, nullPtr)

            // draw
            DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // tear down render
            DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

            // report drawing
            Hl.reportDrawCall 1 true

            // advance pipeline
            Pipeline.advance pipeline.Pipeline

            // advance rendering command buffer
            VulkanContext.advanceRenderCommandBuffer context

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// Create a gamma-correction filter pipeline.
    let createFilterGammaCorrectionPipeline colorAttachmentFormat =

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.FilterGammaCorrectionShaderFilePath
                [|VulkanUnblended|] [|false|] StaticVertices
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 SampledImage FragmentStage 1|] // inputTexture
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|] // inputSampler
                [||] [|colorAttachmentFormat|] None [||]

        // make pipeline
        let filterGammaCorrectionPipeline =
            { Pipeline = pipeline }

        // fin
        filterGammaCorrectionPipeline

    /// Destroy a gamma-correction filter pipeline.
    let destroyFilterGammaCorrectionPipeline (filterGammaCorrectionPipeline : FilterGammaCorrectionPipeline) context =
        Pipeline.destroy filterGammaCorrectionPipeline.Pipeline context

    /// Draw the gamma-correction filter pass of a physically-based surface.
    let drawFilterGammaCorrectionSurface
        (inputTexture : Texture)
        (inputSampler : Sampler)
        (colorAttachment : Texture)
        (resolution : Vector2i)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : FilterGammaCorrectionPipeline)
        (context : VulkanContext) =

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.Pipeline.DrawIndex pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampledTexture 0 0 inputTexture vkSet

            // specify sampler
            let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 inputSampler vkSet

            // set up render
            let clearValue = VkClearValue (r = Constants.Render.ViewportClearColor.R, g = Constants.Render.ViewportClearColor.G, b = Constants.Render.ViewportClearColor.B, a = Constants.Render.ViewportClearColor.A)
            let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
            let mutable vkViewport = Hl.makeViewport false renderArea
            Hl.withRenderingInfo [|colorAttachment.ImageView|] None renderArea (Some clearValue) $ fun renderingInfo ->
                let mutable renderingInfo = renderingInfo
                DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
            DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
            DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

            // set up pipeline
            DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&uniformsDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&samplerDescriptorSet, 0u, nullPtr)

            // draw
            DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // tear down render
            DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

            // report drawing
            Hl.reportDrawCall 1 true

            // advance pipeline
            Pipeline.advance pipeline.Pipeline

            // advance rendering command buffer
            VulkanContext.advanceRenderCommandBuffer context

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// Create a physically-based shadow pipeline.
    let createPhysicallyBasedShadowPipeline shaderPath vertexBindings colorAttachmentFormats depthTestFormat context =

        // create set 0 uniform buffers
        let shadowVertUniform = VulkanBuffer.create Uniform sizeof<ShadowVertStruct> context
        let shadowFragUniform = VulkanBuffer.create Uniform sizeof<ShadowFragStruct> context

        // create set 1 uniform buffers
        let boneUniform = VulkanBuffer.create Uniform (Constants.Render.BonesMax * sizeof<Matrix4x4>) context

        // create pipeline
        let pipeline =
            Pipeline.create
                shaderPath
                [|VulkanUnblended|] [|false; true|] vertexBindings
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer VertexStage 1
                      Pipeline.descriptor 1 UniformBuffer FragmentStage 1|]
                  Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer VertexStage 1|]|]
                [||] colorAttachmentFormats (Some depthTestFormat)
                [|shadowVertUniform; boneUniform; shadowFragUniform|]

        // make PhysicallyBasedDepthPipeline
        let physicallyBasedDepthPipeline =
            { ShadowVertUniform = shadowVertUniform
              BoneUniform = boneUniform
              ShadowFragUniform = shadowFragUniform
              Pipeline = pipeline }

        // fin
        physicallyBasedDepthPipeline

    /// Destroy PhysicallyBasedShadowPipeline.
    let destroyPhysicallyBasedShadowPipeline (physicallyBasedShadowPipeline : PhysicallyBasedShadowPipeline) context =
        Pipeline.destroy physicallyBasedShadowPipeline.Pipeline context

    /// Begin drawing a batch of physically-based shadow surfaces.
    let beginPhysicallyBasedShadowSurfaces
        (cubeMapFace : bool)
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (projectionUnflipped : Matrix4x4)
        (lightShadowExponent : single)
        (colorClearValueOpt : VkClearValue option)
        (colorAttachment : VkImageView)
        (depthAttachment : Texture)
        (resolution : Vector2i)
        (renderPassIndex : int)
        (pipeline : PhysicallyBasedShadowPipeline)
        (context : VulkanContext) =
        
        // compute vulkan-appropriate matrices
        // NOTE: we do NOT flip when rendering to a cube map face!
        let projection = if cubeMapFace then projectionUnflipped else projectionUnflipped.Flipped
        let viewProjection = view * projection

        // set up render
        let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
        let mutable vkViewport = Hl.makeViewport false renderArea
        Hl.withRenderingInfo [|colorAttachment|] (Some depthAttachment.ImageView) renderArea colorClearValueOpt $ fun renderingInfo ->
            let mutable renderingInfo = renderingInfo
            DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
        DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
        DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

        // specify uniforms
        let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 renderPassIndex pipeline.Pipeline $ fun vkSet ->

            // specify shadow vert
            let shadowVert = ShadowVertStruct (viewProjection = viewProjection)
            VulkanBuffer.uploadValue shadowVert pipeline.ShadowVertUniform context
            Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.ShadowVertUniform vkSet

            // specify shadow frag
            let shadowFrag = ShadowFragStruct (eyeCenter = eyeCenter, lightShadowExponent = lightShadowExponent)
            VulkanBuffer.uploadValue shadowFrag pipeline.ShadowFragUniform context
            Pipeline.writeDescriptorUniformBuffer 1 0 pipeline.ShadowFragUniform vkSet

        // fin
        uniformsDescriptorSet

    /// Draw a batch of physically-based deferred shadow surfaces.
    let drawPhysicallyBasedShadowSurfaces
        (bones : Matrix4x4 array)
        (surfacesCount : int)
        (instanceFields : single array)
        (material : PhysicallyBasedMaterial)
        (geometry : PhysicallyBasedGeometry)
        (uniformsDescriptorSet : VkDescriptorSet)
        (pipeline : PhysicallyBasedShadowPipeline)
        (context : VulkanContext) =

        // ensure there is a surface to render to avoid potentially utilizing destroyed textures
        if surfacesCount > 0 then

            // only draw when required vkPipeline exists
            match Pipeline.tryGetVkPipeline VulkanUnblended (not material.TwoSided) pipeline.Pipeline with
            | Some vkPipeline ->

                // specify instancing
                use instanceFieldsPin = new ArrayPin<_> (instanceFields)
                VulkanBuffer.uploadData (Constants.Render.InstanceFieldCount * sizeof<single>) surfacesCount instanceFieldsPin.NativeInt geometry.InstanceBuffer context

                // specify dynamic when animated
                let mutable dynamicDescriptorSet =
                    if bones.Length = 0 then
                        Pipeline.specifyDescriptorSet 1 0 pipeline.Pipeline ignore
                    else
                        Pipeline.specifyDescriptorSet 1 pipeline.Pipeline.DrawIndex pipeline.Pipeline $ fun vkSet ->
                            use bonesPin = new ArrayPin<_> (bones)
                            VulkanBuffer.uploadData sizeof<Matrix4x4> (min bones.Length Constants.Render.BonesMax) bonesPin.NativeInt pipeline.BoneUniform context
                            Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.BoneUniform vkSet

                // set up pipeline
                DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)
                DeviceApi.vkCmdSetDepthTestEnable (context.RenderCommandBuffer, true)
                DeviceApi.vkCmdSetDepthCompareOp (context.RenderCommandBuffer, VkCompareOp.Less)

                // bind vertex and index buffers
                let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
                let vertexOffsets = [|0UL; 0UL|]
                use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
                use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
                DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
                DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

                // bind descriptor sets
                let mutable uniformDescriptorSet = uniformsDescriptorSet
                DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&uniformDescriptorSet, 0u, nullPtr)
                DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&dynamicDescriptorSet, 0u, nullPtr)

                // draw
                DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, uint surfacesCount, 0u, 0, 0u)

                // report drawing
                Hl.reportDrawCall surfacesCount false

                // advance instancing
                VulkanBuffer.advance geometry.InstanceBuffer

                // advance pipeline
                Pipeline.advance pipeline.Pipeline

            // abort
            | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// End the process of drawing physically-based shadow surfaces.
    let endPhysicallyBasedShadowSurfaces (_ : PhysicallyBasedShadowPipeline) (context : VulkanContext) =

        // tear down render
        DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

        // report draw scope
        Hl.reportDrawScope ()

        // advance rendering command buffer
        VulkanContext.advanceRenderCommandBuffer context

    /// Create a physically-based pipeline.
    let createPhysicallyBasedPipeline lightMapsMax lightsMax shaderPath blends cullModes vertexBindings colorAttachmentFormats depthTestOpt context =

        // create set 0 uniform buffers
        let eyeUniform = VulkanBuffer.create Uniform sizeof<EyeStruct> context
        let lightingUniform = VulkanBuffer.create Uniform sizeof<LightingStruct> context

        // create set 2 uniform buffers
        let shadowMatrixMax = Constants.Render.ShadowTexturesMax + Constants.Render.ShadowCascadesMax * Constants.Render.ShadowCascadeLevels
        let boneUniform = VulkanBuffer.create Uniform (Constants.Render.BonesMax * sizeof<Matrix4x4>) context
        let lightMapsUniform = VulkanBuffer.create Uniform (lightMapsMax * sizeof<LightMapStruct>) context
        let lightsGeneralUniform = VulkanBuffer.create Uniform sizeof<LightsGeneralStruct> context
        let lightsUniform = VulkanBuffer.create Uniform (lightsMax * sizeof<LightStruct>) context
        let shadowMatrixUniform = VulkanBuffer.create Uniform (shadowMatrixMax * sizeof<Matrix4x4>) context

        // create pipeline
        let pipeline =
            Pipeline.create
                shaderPath
                blends cullModes vertexBindings
                
                // descriptor set 0: per render pass
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer VertexAndFragmentStage 1 // eye
                      Pipeline.descriptor 1 UniformBuffer FragmentStage 1 // lighting
                      Pipeline.descriptor 2 SampledImage FragmentStage 1 // depthTexture
                      Pipeline.descriptor 3 SampledImage FragmentStage 1 // colorTexture
                      Pipeline.descriptor 4 SampledImage FragmentStage 1 // brdfTexture
                      Pipeline.descriptor 5 SampledImage FragmentStage 1 // irradianceMap
                      Pipeline.descriptor 6 SampledImage FragmentStage 1|] // environmentFilterMap

                  // descriptor set 1: per material
                  Pipeline.descriptorSet<PhysicallyBasedMaterial>
                    [|Pipeline.descriptor 0 SampledImage FragmentStage 1 // albedoTexture
                      Pipeline.descriptor 1 SampledImage FragmentStage 1 // roughnessTexture
                      Pipeline.descriptor 2 SampledImage FragmentStage 1 // metallicTexture
                      Pipeline.descriptor 3 SampledImage FragmentStage 1 // ambientOcclusionTexture
                      Pipeline.descriptor 4 SampledImage FragmentStage 1 // emissionTexture
                      Pipeline.descriptor 5 SampledImage FragmentStage 1 // normalTexture
                      Pipeline.descriptor 6 SampledImage FragmentStage 1 // heightTexture
                      Pipeline.descriptor 7 SampledImage FragmentStage 1 // subdermalTexture
                      Pipeline.descriptor 8 SampledImage FragmentStage 1 // finenessTexture
                      Pipeline.descriptor 9 SampledImage FragmentStage 1 // scatterTexture
                      Pipeline.descriptor 10 SampledImage FragmentStage 1 // clearCoatTexture
                      Pipeline.descriptor 11 SampledImage FragmentStage 1 // clearCoatRoughnessTexture
                      Pipeline.descriptor 12 SampledImage FragmentStage 1|] // clearCoatNormalTexture

                  // descriptor set 2: dynamic
                  Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer VertexStage 1 // bone
                      Pipeline.descriptor 1 UniformBuffer FragmentStage 1 // lightMap
                      Pipeline.descriptor 2 UniformBuffer FragmentStage 1 // lightsGeneral
                      Pipeline.descriptor 3 UniformBuffer FragmentStage 1 // lights
                      Pipeline.descriptor 4 UniformBuffer FragmentStage 1 // shadowMatrices
                      Pipeline.descriptor 5 SampledImage FragmentStage lightMapsMax // irradianceMaps
                      Pipeline.descriptor 6 SampledImage FragmentStage lightMapsMax // environmentFilterMaps
                      Pipeline.descriptor 7 SampledImage FragmentStage 1 // shadowTextures
                      Pipeline.descriptor 8 SampledImage FragmentStage Constants.Render.ShadowMapsMax // shadowMaps
                      Pipeline.descriptor 9 SampledImage FragmentStage Constants.Render.ShadowCascadesMax|] // shadowCascades

                  // descriptor set 3: samplers
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1
                      Pipeline.descriptor 1 Sampler FragmentStage 1
                      Pipeline.descriptor 2 Sampler FragmentStage 1
                      Pipeline.descriptor 3 Sampler FragmentStage 1
                      Pipeline.descriptor 4 Sampler FragmentStage 1
                      Pipeline.descriptor 5 Sampler FragmentStage 1|]|]

                [||] colorAttachmentFormats depthTestOpt
                [|eyeUniform
                  lightingUniform
                  boneUniform
                  lightMapsUniform
                  lightsGeneralUniform
                  lightsUniform
                  shadowMatrixUniform|]

        // make PhysicallyBasedPipeline
        let physicallyBasedPipeline =
            { EyeUniform = eyeUniform
              LightingUniform = lightingUniform
              BoneUniform = boneUniform
              LightMapUniform = lightMapsUniform
              LightsGeneralUniform = lightsGeneralUniform
              LightUniform = lightsUniform
              ShadowMatrixUniform = shadowMatrixUniform
              Pipeline = pipeline }

        // fin
        physicallyBasedPipeline
    
    /// Destroy PhysicallyBasedPipeline.
    let destroyPhysicallyBasedPipeline (pipeline : PhysicallyBasedPipeline) context =
        Pipeline.destroy pipeline.Pipeline context

    /// Begin drawing a batch of physically-based deferred surfaces.
    let beginPhysicallyBasedDeferredSurfaces
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (projectionUnflipped : Matrix4x4)
        (materialSampler : Sampler)
        (colorAttachments : VkImageView array)
        (depthAttachment : Texture)
        (resolution : Vector2i)
        (renderPassIndex : int)
        (pipeline : PhysicallyBasedPipeline)
        (context : VulkanContext) =

        // compute vulkan-appropriate matrices
        let viewInverse = view.Inverted
        let projection = projectionUnflipped.Flipped
        let projectionInverse = projection.Inverted
        let viewProjection = view * projection

        // specify eye
        let mutable eyeDescriptorSet = Pipeline.specifyDescriptorSet 0 renderPassIndex pipeline.Pipeline $ fun vkSet ->
            let eye = EyeStruct (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
            VulkanBuffer.uploadValue eye pipeline.EyeUniform context
            Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.EyeUniform vkSet

        // specify samplers
        let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 3 Unit pipeline.Pipeline $ fun vkSet ->
            Pipeline.writeDescriptorSampler 0 0 materialSampler vkSet
            
        // set up render
        let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
        let mutable vkViewport = Hl.makeViewport false renderArea
        Hl.withRenderingInfo colorAttachments (Some depthAttachment.ImageView) renderArea None $ fun renderingInfo ->
            let mutable renderingInfo = renderingInfo
            DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
        DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
        DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

        // fin
        (eyeDescriptorSet, samplerDescriptorSet)

    /// Draw a batch of physically-based deferred surfaces.
    let drawPhysicallyBasedDeferredSurfaces
        (bones : Matrix4x4 array)
        (surfacesCount : int)
        (instanceFields : single array)
        (material : PhysicallyBasedMaterial)
        (geometry : PhysicallyBasedGeometry)
        (eyeDescriptorSet : VkDescriptorSet)
        (samplerDescriptorSet : VkDescriptorSet)
        (pipeline : PhysicallyBasedPipeline)
        (context : VulkanContext) =

        // only set up when there is a surface to render to avoid potentially utilizing destroyed textures
        if surfacesCount > 0 then

            // only draw when required vkPipeline exists
            match Pipeline.tryGetVkPipeline VulkanUnblended (not material.TwoSided) pipeline.Pipeline with
            | Some vkPipeline ->

                // specify instancing
                use instanceFieldsPin = new ArrayPin<_> (instanceFields)
                VulkanBuffer.uploadData (Constants.Render.InstanceFieldCount * sizeof<single>) surfacesCount instanceFieldsPin.NativeInt geometry.InstanceBuffer context

                // specify material
                let mutable materialDescriptorSet = Pipeline.specifyDescriptorSet 1 material pipeline.Pipeline $ fun vkSet ->
                    Pipeline.writeDescriptorSampledTexture 0 0 material.AlbedoTexture vkSet
                    Pipeline.writeDescriptorSampledTexture 1 0 material.RoughnessTexture vkSet
                    Pipeline.writeDescriptorSampledTexture 2 0 material.MetallicTexture vkSet
                    Pipeline.writeDescriptorSampledTexture 3 0 material.AmbientOcclusionTexture vkSet
                    Pipeline.writeDescriptorSampledTexture 4 0 material.EmissionTexture vkSet
                    Pipeline.writeDescriptorSampledTexture 5 0 material.NormalTexture vkSet
                    Pipeline.writeDescriptorSampledTexture 6 0 material.HeightTexture vkSet
                    Pipeline.writeDescriptorSampledTexture 7 0 material.SubdermalTexture vkSet
                    Pipeline.writeDescriptorSampledTexture 8 0 material.FinenessTexture vkSet
                    Pipeline.writeDescriptorSampledTexture 9 0 material.ScatterTexture vkSet
                    Pipeline.writeDescriptorSampledTexture 10 0 material.ClearCoatTexture vkSet
                    Pipeline.writeDescriptorSampledTexture 11 0 material.ClearCoatRoughnessTexture vkSet
                    Pipeline.writeDescriptorSampledTexture 12 0 material.ClearCoatNormalTexture vkSet

                // specify dynamic when animated
                let mutable dynamicDescriptorSet =
                    if bones.Length = 0 then
                        Pipeline.specifyDescriptorSet 2 0 pipeline.Pipeline ignore
                    else
                        Pipeline.specifyDescriptorSet 2 pipeline.Pipeline.DrawIndex pipeline.Pipeline $ fun vkSet ->
                            use bonesPin = new ArrayPin<_> (bones)
                            VulkanBuffer.uploadData sizeof<Matrix4x4> (min bones.Length Constants.Render.BonesMax) bonesPin.NativeInt pipeline.BoneUniform context
                            Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.BoneUniform vkSet

                // set up pipeline
                DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)
                DeviceApi.vkCmdSetDepthTestEnable (context.RenderCommandBuffer, true)
                DeviceApi.vkCmdSetDepthCompareOp (context.RenderCommandBuffer, VkCompareOp.Less)

                // bind vertex and index buffers
                let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
                let vertexOffsets = [|0UL; 0UL|]
                use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
                use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
                DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
                DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

                // bind descriptor sets
                let mutable (eyeDescriptorSet, samplerDescriptorSet) = (eyeDescriptorSet, samplerDescriptorSet)
                DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&eyeDescriptorSet, 0u, nullPtr)
                DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&materialDescriptorSet, 0u, nullPtr)
                DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 2u, 1u, &&dynamicDescriptorSet, 0u, nullPtr)
                DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 3u, 1u, &&samplerDescriptorSet, 0u, nullPtr)

                // draw
                DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, uint surfacesCount, 0u, 0, 0u)

                // report drawing
                Hl.reportDrawCall surfacesCount false
                    
                // advance instancing
                VulkanBuffer.advance geometry.InstanceBuffer

                // advance pipeline
                Pipeline.advance pipeline.Pipeline

            // abort
            | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// End the process of drawing physically-based deferred surfaces.
    let endPhysicallyBasedDeferredSurfaces (_ : PhysicallyBasedPipeline) (context : VulkanContext) =

        // tear down render
        DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

        // report draw scope
        Hl.reportDrawScope ()

        // advance rendering command buffer
        VulkanContext.advanceRenderCommandBuffer context

    let createPhysicallyBasedTerrainPipeline shaderFilePath colorAttachmentFormats depthTest context =

        // create uniform buffers
        let eyeUniform = VulkanBuffer.create Uniform sizeof<EyeStruct> context
        let terrainFragUniform = VulkanBuffer.create Uniform sizeof<TerrainFragStruct> context

        // create pipeline
        let pipeline =
            Pipeline.create
                shaderFilePath
                [|VulkanUnblended|] [|true|] TerrainVertices
                [|Pipeline.descriptorSet<int * int>
                    [|Pipeline.descriptor 0 UniformBuffer VertexAndFragmentStage 1 // eye
                      Pipeline.descriptor 1 UniformBuffer FragmentStage 1|] // terrainFrag
                  Pipeline.descriptorSet<PhysicallyBasedMaterial array>
                    [|Pipeline.descriptor 0 SampledImage FragmentStage Constants.Render.TerrainLayersMax // albedoTextures
                      Pipeline.descriptor 1 SampledImage FragmentStage Constants.Render.TerrainLayersMax // roughnessTextures
                      Pipeline.descriptor 2 SampledImage FragmentStage Constants.Render.TerrainLayersMax // ambientOcclusionTextures
                      Pipeline.descriptor 3 SampledImage FragmentStage Constants.Render.TerrainLayersMax // normalTextures
                      Pipeline.descriptor 4 SampledImage FragmentStage Constants.Render.TerrainLayersMax|] // heightTextures
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|]
                [||] colorAttachmentFormats (Some depthTest)
                [|eyeUniform; terrainFragUniform|]

        // make PhysicallyBasedDeferredLightingPipeline
        let physicallyBasedDeferredTerrainPipeline =
            { EyeUniform = eyeUniform
              Lighting3Uniform = terrainFragUniform
              Pipeline = pipeline }
        
        // fin
        physicallyBasedDeferredTerrainPipeline
    
    /// Destroy PhysicallyBasedTerrainPipeline.
    let destroyPhysicallyBasedDeferredTerrainPipeline (pipeline : PhysicallyBasedDeferredTerrainPipeline) context =
        Pipeline.destroy pipeline.Pipeline context

    let drawPhysicallyBasedTerrain
        (shadowCubeMapFace : bool)
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (projectionUnflipped : Matrix4x4)
        (instanceFields : single array)
        (lightShadowSamples : int)
        (lightShadowBias : single)
        (lightShadowSampleScalar : single)
        (lightShadowExponent : single)
        (lightShadowDensity : single)
        (materials : PhysicallyBasedMaterial array)
        (materialSampler : Sampler)
        (geometry : PhysicallyBasedGeometry)
        (colorAttachments : VkImageView array)
        (depthAttachment : Texture)
        (resolution : Vector2i)
        (renderPassIndex : int)
        (pipeline : PhysicallyBasedDeferredTerrainPipeline)
        (context : VulkanContext) =

        // compute vulkan-appropriate matrices
        // NOTE: we do NOT flip when rendering to a shadow cube map face!
        let viewInverse = view.Inverted
        let projection = if shadowCubeMapFace then projectionUnflipped else projectionUnflipped.Flipped
        let projectionInverse = projection.Inverted
        let viewProjection = view * projection

        // enforce layer limit
        let layersCount = min materials.Length Constants.Render.TerrainLayersMax
            
        // set up render
        let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
        let mutable vkViewport = Hl.makeViewport false renderArea
        Hl.withRenderingInfo colorAttachments (Some depthAttachment.ImageView) renderArea None $ fun renderingInfo ->
            let mutable renderingInfo = renderingInfo
            DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
        DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
        DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended true pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 (layersCount, renderPassIndex) pipeline.Pipeline $ fun vkSet ->

                // specify eye
                let eye = EyeStruct (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
                VulkanBuffer.uploadValue eye pipeline.EyeUniform context
                Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.EyeUniform vkSet

                // specify terrain frag
                let mutable terrainFrag = TerrainFragStruct ()
                terrainFrag.layersCount <- layersCount
                terrainFrag.lightShadowSamples <- lightShadowSamples
                terrainFrag.lightShadowBias <- lightShadowBias
                terrainFrag.lightShadowSampleScalar <- lightShadowSampleScalar
                terrainFrag.lightShadowExponent <- lightShadowExponent
                terrainFrag.lightShadowDensity <- lightShadowDensity
                VulkanBuffer.uploadValue terrainFrag pipeline.Lighting3Uniform context
                Pipeline.writeDescriptorUniformBuffer 1 0 pipeline.Lighting3Uniform vkSet

            // specify materials
            // TODO: P1: maybe receive these arrays pre-formed?
            let mutable materialsDescriptorSet = Pipeline.specifyDescriptorSet 1 materials pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampledTextures 0 0 (materials |> Array.map _.AlbedoTexture) vkSet
                Pipeline.writeDescriptorSampledTextures 1 0 (materials |> Array.map _.RoughnessTexture) vkSet
                Pipeline.writeDescriptorSampledTextures 2 0 (materials |> Array.map _.AmbientOcclusionTexture) vkSet
                Pipeline.writeDescriptorSampledTextures 3 0 (materials |> Array.map _.NormalTexture) vkSet
                Pipeline.writeDescriptorSampledTextures 4 0 (materials |> Array.map _.HeightTexture) vkSet

            // specify samplers
            let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 2 Unit pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 materialSampler vkSet

            // specify instancing
            use instanceFieldsPin = new ArrayPin<_> (instanceFields)
            VulkanBuffer.uploadData (Constants.Render.InstanceFieldCount * sizeof<single>) 1 instanceFieldsPin.NativeInt geometry.InstanceBuffer context

            // set up pipeline
            DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)
            DeviceApi.vkCmdSetDepthTestEnable (context.RenderCommandBuffer, true)
            DeviceApi.vkCmdSetDepthCompareOp (context.RenderCommandBuffer, VkCompareOp.Less)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&uniformsDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&materialsDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 2u, 1u, &&samplerDescriptorSet, 0u, nullPtr)

            // draw
            DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // report drawing
            Hl.reportDrawCall 1 false

            // advance instancing
            VulkanBuffer.advance geometry.InstanceBuffer

            // advance pipeline
            Pipeline.advance pipeline.Pipeline

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

        // tear down render
        DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

        // report draw scope
        Hl.reportDrawScope ()

        // advance rendering command buffer
        VulkanContext.advanceRenderCommandBuffer context

    /// Create a PhysicallyBasedDeferredLightingPipeline.
    let createPhysicallyBasedDeferredLightingPipeline colorAttachmentFormat context =

        // create uniform buffers
        let shadowMatrixMax = Constants.Render.ShadowTexturesMax + Constants.Render.ShadowCascadesMax * Constants.Render.ShadowCascadeLevels
        let eyeUniform = VulkanBuffer.create Uniform sizeof<EyeStruct> context
        let lightingUniform = VulkanBuffer.create Uniform sizeof<Lighting2Struct> context
        let lightUniform = VulkanBuffer.create Uniform (Constants.Render.LightsMaxDeferred * sizeof<LightStruct>) context
        let shadowMatrixUniform = VulkanBuffer.create Uniform (shadowMatrixMax * sizeof<Matrix4x4>) context

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.PhysicallyBasedDeferredLightingShaderFilePath
                [|VulkanUnblended|] [|false|] StaticVertices
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer FragmentStage 1 // eye
                      Pipeline.descriptor 1 UniformBuffer FragmentStage 1 // lighting
                      Pipeline.descriptor 2 UniformBuffer FragmentStage 1 // lights
                      Pipeline.descriptor 3 UniformBuffer FragmentStage 1 // shadowMatrices
                      Pipeline.descriptor 4 SampledImage FragmentStage 1 // depth
                      Pipeline.descriptor 5 SampledImage FragmentStage 1 // albedo
                      Pipeline.descriptor 6 SampledImage FragmentStage 1 // material
                      Pipeline.descriptor 7 SampledImage FragmentStage 1 // normalPlus
                      Pipeline.descriptor 8 SampledImage FragmentStage 1 // subdermalPlus
                      Pipeline.descriptor 9 SampledImage FragmentStage 1 // scatterPlus
                      Pipeline.descriptor 10 SampledImage FragmentStage 1 // clearCoatPlus
                      Pipeline.descriptor 11 SampledImage FragmentStage 1 // shadowTextures
                      Pipeline.descriptor 12 SampledImage FragmentStage Constants.Render.ShadowMapsMax // shadowMaps
                      Pipeline.descriptor 13 SampledImage FragmentStage Constants.Render.ShadowCascadesMax|] // shadowCascades
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1
                      Pipeline.descriptor 1 Sampler FragmentStage 1|]|]
                [||] [|colorAttachmentFormat|] None
                [|eyeUniform; lightingUniform; lightUniform; shadowMatrixUniform|]

        // make PhysicallyBasedDeferredLightingPipeline
        let physicallyBasedDeferredLightingPipeline =
            { EyeUniform = eyeUniform
              Lighting2Uniform = lightingUniform
              LightUniform = lightUniform
              ShadowMatrixUniform = shadowMatrixUniform
              Pipeline = pipeline }
        
        // fin
        physicallyBasedDeferredLightingPipeline
    
    /// Destroy PhysicallyBasedDeferredLightingPipeline.
    let destroyPhysicallyBasedDeferredLightingPipeline (pipeline : PhysicallyBasedDeferredLightingPipeline) context =
        Pipeline.destroy pipeline.Pipeline context

    /// Draw the deferred lighting pass of a physically-based surface.
    let drawPhysicallyBasedDeferredLightingSurface
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (projectionUnflipped : Matrix4x4)
        (lightCutoffMargin : single)
        (lightShadowSamples : int)
        (lightShadowBias : single)
        (lightShadowSampleScalar : single)
        (lightShadowExponent : single)
        (lightShadowDensity : single)
        (sssEnabled : int)
        (depthTexture : Texture)
        (albedoTexture : Texture)
        (materialTexture : Texture)
        (normalPlusTexture : Texture)
        (subdermalPlusTexture : Texture)
        (scatterPlusTexture : Texture)
        (clearCoatPlusTexture : Texture)
        (shadowTextureArray : Texture)
        (shadowMaps : Texture array)
        (shadowCascades : Texture array)
        (lightOrigins : Vector3 array)
        (lightDirections : Vector3 array)
        (lightColors : Color array)
        (lightBrightnesses : single array)
        (lightAttenuationLinears : single array)
        (lightAttenuationQuadratics : single array)
        (lightCutoffs : single array)
        (lightTypes : int array)
        (lightConeInners : single array)
        (lightConeOuters : single array)
        (lightDesireFogs : int array)
        (lightShadowIndices : int array)
        (lightsCount : int)
        (shadowNear : single)
        (shadowMatrices : Matrix4x4 array)
        (unfilteredSampler : Sampler)
        (filteredSampler : Sampler)
        (resolution : Vector2i)
        (renderPassIndex : int)
        (geometry : PhysicallyBasedGeometry)
        (lightAccumAttachment : Texture)
        (pipeline : PhysicallyBasedDeferredLightingPipeline)
        (context : VulkanContext) =

        // compute vulkan-appropriate matrices
        let viewInverse = view.Inverted
        let projection = projectionUnflipped.Flipped
        let projectionInverse = projection.Inverted
        let viewProjection = view * projection

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 renderPassIndex pipeline.Pipeline $ fun vkSet ->

                // specify eye
                let eye = EyeStruct (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
                VulkanBuffer.uploadValue eye pipeline.EyeUniform context
                Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.EyeUniform vkSet

                // specify lighting
                let mutable lighting = Lighting2Struct ()
                lighting.lightCutoffMargin <- lightCutoffMargin
                lighting.lightShadowSamples <- lightShadowSamples
                lighting.lightShadowBias <- lightShadowBias
                lighting.lightShadowSampleScalar <- lightShadowSampleScalar
                lighting.lightShadowExponent <- lightShadowExponent
                lighting.lightShadowDensity <- lightShadowDensity
                lighting.shadowNear <- shadowNear
                lighting.sssEnabled <- sssEnabled
                lighting.lightsCount <- lightsCount
                lighting.shadowNear <- shadowNear
                VulkanBuffer.uploadValue lighting pipeline.Lighting2Uniform context
                Pipeline.writeDescriptorUniformBuffer 1 0 pipeline.Lighting2Uniform vkSet

                // specify lights
                let mutable light = LightStruct ()
                use lightPtr = fixed &light
                for i in 0 .. dec Constants.Render.LightsMaxDeferred do
                    if i < lightOrigins.Length then
                        light.origin <- lightOrigins[i]
                        light.direction <- lightDirections[i]
                        light.color <- lightColors[i].V3
                        light.brightness <- lightBrightnesses[i]
                        light.attenuationLinear <- lightAttenuationLinears[i]
                        light.attenuationQuadratic <- lightAttenuationQuadratics[i]
                        light.cutoff <- lightCutoffs[i]
                        light.lightType <- lightTypes[i]
                        light.coneInner <- lightConeInners[i]
                        light.coneOuter <- lightConeOuters[i]
                        light.desireFog <- lightDesireFogs[i]
                        light.shadowIndex <- lightShadowIndices[i]
                    else light <- Unchecked.defaultof<_>
                    VulkanBuffer.writeSubdata (i * sizeof<LightStruct>) 0 sizeof<LightStruct> 1 (NativePtr.toNativeInt lightPtr) pipeline.LightUniform context
                VulkanBuffer.flushSubdata 0 0 sizeof<LightStruct> Constants.Render.LightsMaxDeferred pipeline.LightUniform context
                Pipeline.writeDescriptorUniformBuffer 2 0 pipeline.LightUniform vkSet

                // specify shadow matrices
                use shadowMatricesPin = new ArrayPin<_> (shadowMatrices)
                let shadowMatricesCount = min shadowMatrices.Length (Constants.Render.ShadowTexturesMax + Constants.Render.ShadowCascadesMax * Constants.Render.ShadowCascadeLevels)
                VulkanBuffer.uploadData sizeof<Matrix4x4> shadowMatricesCount shadowMatricesPin.NativeInt pipeline.ShadowMatrixUniform context
                Pipeline.writeDescriptorUniformBuffer 3 0 pipeline.ShadowMatrixUniform vkSet

                // specify textures
                Pipeline.writeDescriptorSampledTexture 4 0 depthTexture vkSet
                Pipeline.writeDescriptorSampledTexture 5 0 albedoTexture vkSet
                Pipeline.writeDescriptorSampledTexture 6 0 materialTexture vkSet
                Pipeline.writeDescriptorSampledTexture 7 0 normalPlusTexture vkSet
                Pipeline.writeDescriptorSampledTexture 8 0 subdermalPlusTexture vkSet
                Pipeline.writeDescriptorSampledTexture 9 0 scatterPlusTexture vkSet
                Pipeline.writeDescriptorSampledTexture 10 0 clearCoatPlusTexture vkSet
                Pipeline.writeDescriptorSampledTexture 11 0 shadowTextureArray vkSet
                Pipeline.writeDescriptorSampledTextures 12 0 (Array.tryTake Constants.Render.ShadowMapsMax shadowMaps) vkSet
                Pipeline.writeDescriptorSampledTextures 13 0 (Array.tryTake Constants.Render.ShadowCascadesMax shadowCascades) vkSet

            // specify samplers
            let mutable samplersDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 unfilteredSampler vkSet
                Pipeline.writeDescriptorSampler 1 0 filteredSampler vkSet

            // set up render
            let clearValue = VkClearValue (r = Constants.Render.ViewportClearColor.R, g = Constants.Render.ViewportClearColor.G, b = Constants.Render.ViewportClearColor.B, a = Constants.Render.ViewportClearColor.A)
            let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
            let mutable vkViewport = Hl.makeViewport false renderArea
            Hl.withRenderingInfo [|lightAccumAttachment.ImageView|] None renderArea (Some clearValue) $ fun renderingInfo ->
                let mutable renderingInfo = renderingInfo
                DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
            DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
            DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

            // set up pipeline
            DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            let mutable (uniformsDescriptorSet, samplersDescriptorSet) = (uniformsDescriptorSet, samplersDescriptorSet)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&uniformsDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&samplersDescriptorSet, 0u, nullPtr)

            // draw
            DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // tear down render
            DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

            // report drawing
            Hl.reportDrawCall 1 true

            // advance pipeline
            Pipeline.advance pipeline.Pipeline

            // advance rendering command buffer
            VulkanContext.advanceRenderCommandBuffer context

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// Create a PhysicallyBasedDeferredFoggingPipeline.
    let createPhysicallyBasedDeferredFoggingPipeline colorAttachmentFormat context =

        // create uniform buffers
        let shadowMatrixMax = Constants.Render.ShadowTexturesMax + Constants.Render.ShadowCascadesMax * Constants.Render.ShadowCascadeLevels
        let eyeUniform = VulkanBuffer.create Uniform sizeof<EyeStruct> context
        let lightingUniform = VulkanBuffer.create Uniform sizeof<LightingStruct> context
        let lightsGeneralUniform = VulkanBuffer.create Uniform sizeof<LightsGeneralStruct> context
        let lightsUniform = VulkanBuffer.create Uniform (Constants.Render.LightsMaxDeferred * sizeof<LightStruct>) context
        let shadowMatricesUniform = VulkanBuffer.create Uniform (shadowMatrixMax * sizeof<Matrix4x4>) context

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.PhysicallyBasedDeferredFoggingShaderFilePath
                [|VulkanUnblended|] [|false|] StaticVertices
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer FragmentStage 1 // eye
                      Pipeline.descriptor 1 UniformBuffer FragmentStage 1 // lighting
                      Pipeline.descriptor 2 UniformBuffer FragmentStage 1 // lightsGeneral
                      Pipeline.descriptor 3 UniformBuffer FragmentStage 1 // lights
                      Pipeline.descriptor 4 UniformBuffer FragmentStage 1 // shadowMatrices
                      Pipeline.descriptor 5 SampledImage FragmentStage 1 // depth
                      Pipeline.descriptor 6 SampledImage FragmentStage 1 // shadowTextures
                      Pipeline.descriptor 7 SampledImage FragmentStage Constants.Render.ShadowMapsMax // shadowMaps
                      Pipeline.descriptor 8 SampledImage FragmentStage Constants.Render.ShadowCascadesMax|] // shadowCascades
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1
                      Pipeline.descriptor 1 Sampler FragmentStage 1|]|]
                [||] [|colorAttachmentFormat|] None
                [|eyeUniform; lightingUniform; lightsGeneralUniform; lightsUniform; shadowMatricesUniform|]

        // make PhysicallyBasedDeferredFoggingPipeline
        let physicallyBasedDeferredFoggingPipeline =
            { EyeUniform = eyeUniform
              LightingUniform = lightingUniform
              LightsGeneralUniform = lightsGeneralUniform
              LightsUniform = lightsUniform
              ShadowMatricesUniform = shadowMatricesUniform
              Pipeline = pipeline }

        // fin
        physicallyBasedDeferredFoggingPipeline

    /// Destroy PhysicallyBasedDeferredFoggingPipeline.
    let destroyPhysicallyBasedDeferredFoggingPipeline (pipeline : PhysicallyBasedDeferredFoggingPipeline) context =
        Pipeline.destroy pipeline.Pipeline context

    /// Draw the deferred fogging pass of a physically-based surface.
    let drawPhysicallyBasedDeferredFoggingSurface
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (projectionUnflipped : Matrix4x4)
        (lightCutoffMargin : single)
        (ssvfEnabled : int)
        (ssvfIntensity : single)
        (ssvfSteps : int)
        (ssvfAsymmetry : single)
        (depthTexture : Texture)
        (shadowTextureArray : Texture)
        (shadowMaps : Texture array)
        (shadowCascades : Texture array)
        (lightMapsCount : int)
        (lightMapSingletonBlendMargin : single)
        (lightOrigins : Vector3 array)
        (lightDirections : Vector3 array)
        (lightColors : Color array)
        (lightBrightnesses : single array)
        (lightAttenuationLinears : single array)
        (lightAttenuationQuadratics : single array)
        (lightCutoffs : single array)
        (lightTypes : int array)
        (lightConeInners : single array)
        (lightConeOuters : single array)
        (lightDesireFogs : int array)
        (lightShadowIndices : int array)
        (lightsCount : int)
        (shadowMatrices : Matrix4x4 array)
        (unfilteredSampler : Sampler)
        (filteredSampler : Sampler)
        (foggingAttachment : Texture)
        (resolution : Vector2i)
        (renderPassIndex : int)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : PhysicallyBasedDeferredFoggingPipeline)
        (context : VulkanContext) =

        // compute vulkan-appropriate matrices
        let viewInverse = view.Inverted
        let projection = projectionUnflipped.Flipped
        let projectionInverse = projection.Inverted
        let viewProjection = view * projection

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 renderPassIndex pipeline.Pipeline $ fun vkSet ->

                // specify eye
                let eye = EyeStruct (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
                VulkanBuffer.uploadValue eye pipeline.EyeUniform context
                Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.EyeUniform vkSet

                // specify lighting
                let mutable lighting = LightingStruct ()
                lighting.lightCutoffMargin <- lightCutoffMargin
                lighting.ssvfEnabled <- ssvfEnabled
                lighting.ssvfIntensity <- ssvfIntensity
                lighting.ssvfSteps <- ssvfSteps
                lighting.ssvfAsymmetry <- ssvfAsymmetry
                VulkanBuffer.uploadValue lighting pipeline.LightingUniform context
                Pipeline.writeDescriptorUniformBuffer 1 0 pipeline.LightingUniform vkSet

                // specify lights general
                let mutable lightsGeneral = LightsGeneralStruct ()
                lightsGeneral.lightMapsCount <- lightMapsCount
                lightsGeneral.lightMapSingletonBlendMargin <- lightMapSingletonBlendMargin
                lightsGeneral.lightsCount <- lightsCount
                VulkanBuffer.uploadValue lightsGeneral pipeline.LightsGeneralUniform context
                Pipeline.writeDescriptorUniformBuffer 2 0 pipeline.LightsGeneralUniform vkSet

                // specify lights
                let mutable light = LightStruct ()
                use lightPtr = fixed &light
                for i in 0 .. dec Constants.Render.LightsMaxDeferred do
                    if i < lightOrigins.Length then
                        light.origin <- lightOrigins[i]
                        light.direction <- lightDirections[i]
                        light.color <- lightColors[i].V3
                        light.brightness <- lightBrightnesses[i]
                        light.attenuationLinear <- lightAttenuationLinears[i]
                        light.attenuationQuadratic <- lightAttenuationQuadratics[i]
                        light.cutoff <- lightCutoffs[i]
                        light.lightType <- lightTypes[i]
                        light.coneInner <- lightConeInners[i]
                        light.coneOuter <- lightConeOuters[i]
                        light.desireFog <- lightDesireFogs[i]
                        light.shadowIndex <- lightShadowIndices[i]
                    else light <- Unchecked.defaultof<_>
                    VulkanBuffer.writeSubdata (i * sizeof<LightStruct>) 0 sizeof<LightStruct> 1 (NativePtr.toNativeInt lightPtr) pipeline.LightsUniform context
                VulkanBuffer.flushSubdata 0 0 sizeof<LightStruct> Constants.Render.LightsMaxDeferred pipeline.LightsUniform context
                Pipeline.writeDescriptorUniformBuffer 3 0 pipeline.LightsUniform vkSet

                // specify shadow matrices
                use shadowMatricesPin = new ArrayPin<_> (shadowMatrices)
                let shadowMatricesCount = min shadowMatrices.Length (Constants.Render.ShadowTexturesMax + Constants.Render.ShadowCascadesMax * Constants.Render.ShadowCascadeLevels)
                VulkanBuffer.uploadData sizeof<Matrix4x4> shadowMatricesCount shadowMatricesPin.NativeInt pipeline.ShadowMatricesUniform context
                Pipeline.writeDescriptorUniformBuffer 4 0 pipeline.ShadowMatricesUniform vkSet

                // specify textures
                Pipeline.writeDescriptorSampledTexture 5 0 depthTexture vkSet
                Pipeline.writeDescriptorSampledTexture 6 0 shadowTextureArray vkSet
                Pipeline.writeDescriptorSampledTextures 7 0 (Array.tryTake Constants.Render.ShadowMapsMax shadowMaps) vkSet
                Pipeline.writeDescriptorSampledTextures 8 0 (Array.tryTake Constants.Render.ShadowCascadesMax shadowCascades) vkSet

            // specify samplers
            let mutable samplersDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 unfilteredSampler vkSet
                Pipeline.writeDescriptorSampler 1 0 filteredSampler vkSet

            // set up render
            let clearValue = VkClearValue (r = Constants.Render.ViewportClearColor.R, g = Constants.Render.ViewportClearColor.G, b = Constants.Render.ViewportClearColor.B, a = Constants.Render.ViewportClearColor.A)
            let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
            let mutable vkViewport = Hl.makeViewport false renderArea
            Hl.withRenderingInfo [|foggingAttachment.ImageView|] None renderArea (Some clearValue) $ fun renderingInfo ->
                let mutable renderingInfo = renderingInfo
                DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
            DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
            DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

            // set up pipeline
            DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            let mutable (uniformsDescriptorSet, samplersDescriptorSet) = (uniformsDescriptorSet, samplersDescriptorSet)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&uniformsDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&samplersDescriptorSet, 0u, nullPtr)

            // draw
            DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // tear down render
            DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

            // report drawing
            Hl.reportDrawCall 1 true

            // advance pipeline
            Pipeline.advance pipeline.Pipeline

            // advance rendering command buffer
            VulkanContext.advanceRenderCommandBuffer context

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// Create a PhysicallyBasedDeferredLightMappingPipeline.
    let createPhysicallyBasedDeferredLightMappingPipeline colorAttachmentFormat context =

        // create uniform buffers
        let eyeUniform = VulkanBuffer.create Uniform sizeof<EyeStruct> context
        let lightMapsUniform = VulkanBuffer.create Uniform (Constants.Render.LightMapsMaxDeferred * sizeof<LightMapStruct>) context
        let lightsGeneralUniform = VulkanBuffer.create Uniform sizeof<LightsGeneralStruct> context

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.PhysicallyBasedDeferredLightMappingShaderFilePath
                [|VulkanUnblended|] [|false|] StaticVertices
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer FragmentStage 1 // eye
                      Pipeline.descriptor 1 UniformBuffer FragmentStage 1 // lightMaps
                      Pipeline.descriptor 2 UniformBuffer FragmentStage 1 // lightsGeneral
                      Pipeline.descriptor 3 SampledImage FragmentStage 1 // depth
                      Pipeline.descriptor 4 SampledImage FragmentStage 1|] // normalPlus
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|]
                [||] [|colorAttachmentFormat|] None
                [|eyeUniform; lightMapsUniform; lightsGeneralUniform|]

        // make PhysicallyBasedDeferredLightingPipeline
        let physicallyBasedDeferredLightMappingPipeline =
            { EyeUniform = eyeUniform
              LightMapsUniform = lightMapsUniform
              LightsGeneralUniform = lightsGeneralUniform
              Pipeline = pipeline }

        // fin
        physicallyBasedDeferredLightMappingPipeline
    
    /// Destroy PhysicallyBasedDeferredLightMappingPipeline.
    let destroyPhysicallyBasedDeferredLightMappingPipeline (pipeline : PhysicallyBasedDeferredLightMappingPipeline) context =
        Pipeline.destroy pipeline.Pipeline context

    /// Draw the deferred light mapping pass of a physically-based surface.
    let drawPhysicallyBasedDeferredLightMappingSurface
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (projectionUnflipped : Matrix4x4)
        (lightMapOrigins : Vector3 array)
        (lightMapMins : Vector3 array)
        (lightMapSizes : Vector3 array)
        (lightMapAmbientColors : Color array)
        (lightMapAmbientBrightnesses : single array)
        (lightMapsCount : int)
        (lightMapSingletonBlendMargin : single)
        (lightsCount : int)
        (depthTexture : Texture)
        (normalPlusTexture : Texture)
        (unfilteredSampler : Sampler)
        (colorAttachment : Texture)
        (resolution : Vector2i)
        (renderPassIndex : int)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : PhysicallyBasedDeferredLightMappingPipeline)
        (context : VulkanContext) =

        // compute vulkan-appropriate matrices
        let viewInverse = view.Inverted
        let projection = projectionUnflipped.Flipped
        let projectionInverse = projection.Inverted
        let viewProjection = view * projection

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 renderPassIndex pipeline.Pipeline $ fun vkSet ->

                // specify eye
                let eye = EyeStruct (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
                VulkanBuffer.uploadValue eye pipeline.EyeUniform context
                Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.EyeUniform vkSet

                // specify light maps
                let mutable lightMap = LightMapStruct ()
                use lightMapPtr = fixed &lightMap
                for i in 0 .. dec Constants.Render.LightMapsMaxDeferred do
                    if i < lightMapOrigins.Length then
                        lightMap.origin <- lightMapOrigins[i]
                        lightMap.min <- lightMapMins[i]
                        lightMap.size <- lightMapSizes[i]
                        lightMap.ambientColor <- lightMapAmbientColors[i].V3
                        lightMap.ambientBrightness <- lightMapAmbientBrightnesses[i]
                    else lightMap <- Unchecked.defaultof<_>
                    VulkanBuffer.writeSubdata (i * sizeof<LightMapStruct>) 0 sizeof<LightMapStruct> 1 (NativePtr.toNativeInt lightMapPtr) pipeline.LightMapsUniform context
                VulkanBuffer.flushSubdata 0 0 sizeof<LightMapStruct> Constants.Render.LightMapsMaxDeferred pipeline.LightMapsUniform context
                Pipeline.writeDescriptorUniformBuffer 1 0 pipeline.LightMapsUniform vkSet

                // specify lights general
                let mutable lightsGeneral = LightsGeneralStruct ()
                lightsGeneral.lightMapsCount <- lightMapsCount
                lightsGeneral.lightMapSingletonBlendMargin <- lightMapSingletonBlendMargin
                lightsGeneral.lightsCount <- lightsCount
                VulkanBuffer.uploadValue lightsGeneral pipeline.LightsGeneralUniform context
                Pipeline.writeDescriptorUniformBuffer 2 0 pipeline.LightsGeneralUniform vkSet

                // specify static environment textures
                Pipeline.writeDescriptorSampledTexture 3 0 depthTexture vkSet
                Pipeline.writeDescriptorSampledTexture 4 0 normalPlusTexture vkSet

            // specify samplers
            let mutable samplersDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 unfilteredSampler vkSet

            // set up render
            let clearValue = VkClearValue (r = Constants.Render.ViewportClearColor.R, g = Constants.Render.ViewportClearColor.G, b = Constants.Render.ViewportClearColor.B, a = Constants.Render.ViewportClearColor.A)
            let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
            let mutable vkViewport = Hl.makeViewport false renderArea
            Hl.withRenderingInfo [|colorAttachment.ImageView|] None renderArea (Some clearValue) $ fun renderingInfo ->
                let mutable renderingInfo = renderingInfo
                DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
            DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
            DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

            // set up pipeline
            DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&uniformsDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&samplersDescriptorSet, 0u, nullPtr)

            // draw
            DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // tear down render
            DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

            // report drawing
            Hl.reportDrawCall 1 true

            // advance pipeline
            Pipeline.advance pipeline.Pipeline

            // advance rendering command buffer
            VulkanContext.advanceRenderCommandBuffer context

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// Create a PhysicallyBasedDeferredAmbientPipeline.
    let createPhysicallyBasedDeferredAmbientPipeline colorAttachmentFormat context =

        // create uniform buffers
        let eyeUniform = VulkanBuffer.create Uniform sizeof<EyeStruct> context
        let lightMapUniform = VulkanBuffer.create Uniform sizeof<LightMapStruct> context
        let lightMapsUniform = VulkanBuffer.create Uniform (Constants.Render.LightMapsMaxDeferred * sizeof<LightMapStruct>) context

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.PhysicallyBasedDeferredAmbientShaderFilePath
                [|VulkanUnblended|] [|false|] StaticVertices
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer FragmentStage 1 // eye
                      Pipeline.descriptor 1 UniformBuffer FragmentStage 1 // lightMap
                      Pipeline.descriptor 2 UniformBuffer FragmentStage 1 // lightMaps
                      Pipeline.descriptor 3 SampledImage FragmentStage 1 // depth
                      Pipeline.descriptor 4 SampledImage FragmentStage 1|] // lightMapping
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|]
                [||] [|colorAttachmentFormat|] None
                [|eyeUniform; lightMapUniform; lightMapsUniform|]

        // make PhysicallyBasedDeferredAmbientPipeline
        let physicallyBasedDeferredAmbientPipeline =
            { EyeUniform = eyeUniform
              LightMapUniform = lightMapUniform
              LightMapsUniform = lightMapsUniform
              Pipeline = pipeline }
        
        // fin
        physicallyBasedDeferredAmbientPipeline

    /// Destroy PhysicallyBasedDeferredAmbientPipeline.
    let destroyPhysicallyBasedDeferredAmbientPipeline (pipeline : PhysicallyBasedDeferredAmbientPipeline) context =
        Pipeline.destroy pipeline.Pipeline context

    /// Draw the deferred ambient pass of a physically-based surface.
    let drawPhysicallyBasedDeferredAmbientSurface
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (projectionUnflipped : Matrix4x4)
        (lightMapFallbackAmbientColor : Color)
        (lightMapFallbackAmbientBrightness : single)
        (lightMapAmbientColors : Color array)
        (lightMapAmbientBrightnesses : single array)
        (depthTexture : Texture)
        (lightMappingTexture : Texture)
        (unfilteredSampler : Sampler)
        (colorAttachment : Texture)
        (resolution : Vector2i)
        (renderPassIndex : int)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : PhysicallyBasedDeferredAmbientPipeline)
        (context : VulkanContext) =

        // compute vulkan-appropriate matrices
        let viewInverse = view.Inverted
        let projection = projectionUnflipped.Flipped
        let projectionInverse = projection.Inverted
        let viewProjection = view * projection

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 renderPassIndex pipeline.Pipeline $ fun vkSet ->

                // specify eye
                let eye = EyeStruct (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
                VulkanBuffer.uploadValue eye pipeline.EyeUniform context
                Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.EyeUniform vkSet

                // specify fallback light map
                let mutable lightMap = LightMapStruct ()
                lightMap.ambientColor <- lightMapFallbackAmbientColor.V3
                lightMap.ambientBrightness <- lightMapFallbackAmbientBrightness
                VulkanBuffer.uploadValue lightMap pipeline.LightMapUniform context
                Pipeline.writeDescriptorUniformBuffer 1 0 pipeline.LightMapUniform vkSet

                // specify light maps
                use lightMapPtr = fixed &lightMap
                for i in 0 .. dec Constants.Render.LightMapsMaxDeferred do
                    if i < lightMapAmbientColors.Length then
                        lightMap.ambientColor <- lightMapAmbientColors[i].V3
                        lightMap.ambientBrightness <- lightMapAmbientBrightnesses[i]
                    else lightMap <- Unchecked.defaultof<_>
                    VulkanBuffer.writeSubdata (i * sizeof<LightMapStruct>) 0 sizeof<LightMapStruct> 1 (NativePtr.toNativeInt lightMapPtr) pipeline.LightMapsUniform context
                VulkanBuffer.flushSubdata 0 0 sizeof<LightMapStruct> Constants.Render.LightMapsMaxDeferred pipeline.LightMapsUniform context
                Pipeline.writeDescriptorUniformBuffer 2 0 pipeline.LightMapsUniform vkSet

                // specify static environment textures
                Pipeline.writeDescriptorSampledTexture 3 0 depthTexture vkSet
                Pipeline.writeDescriptorSampledTexture 4 0 lightMappingTexture vkSet

            // specify samplers
            let mutable samplersDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 unfilteredSampler vkSet

            // set up render
            let clearValue = VkClearValue (r = Constants.Render.AmbientClearColor.R, g = Constants.Render.AmbientClearColor.G, b = Constants.Render.AmbientClearColor.B, a = Constants.Render.AmbientClearColor.A)
            let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
            let mutable vkViewport = Hl.makeViewport false renderArea
            Hl.withRenderingInfo [|colorAttachment.ImageView|] None renderArea (Some clearValue) $ fun renderingInfo ->
                let mutable renderingInfo = renderingInfo
                DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
            DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
            DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

            // set up pipeline
            DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&uniformsDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&samplersDescriptorSet, 0u, nullPtr)

            // draw
            DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // tear down render
            DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

            // report drawing
            Hl.reportDrawCall 1 true

            // advance pipeline
            Pipeline.advance pipeline.Pipeline

            // advance rendering command buffer
            VulkanContext.advanceRenderCommandBuffer context

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// Create a PhysicallyBasedDeferredIrradiancePipeline.
    let createPhysicallyBasedDeferredIrradiancePipeline colorAttachmentFormat context =

        // create uniform buffers
        let eyeUniform = VulkanBuffer.create Uniform sizeof<EyeStruct> context

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.PhysicallyBasedDeferredIrradianceShaderFilePath
                [|VulkanUnblended|] [|false|] StaticVertices
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer FragmentStage 1 // eye
                      Pipeline.descriptor 1 SampledImage FragmentStage 1 // depth
                      Pipeline.descriptor 2 SampledImage FragmentStage 1 // normalPlus
                      Pipeline.descriptor 3 SampledImage FragmentStage 1 // lightMapping
                      Pipeline.descriptor 4 SampledImage FragmentStage 1 // irradianceMap
                      Pipeline.descriptor 5 SampledImage FragmentStage Constants.Render.LightMapsMaxDeferred|] // iraddianceMaps
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1
                      Pipeline.descriptor 1 Sampler FragmentStage 1|]|]
                [||] [|colorAttachmentFormat|] None
                [|eyeUniform|]

        // make PhysicallyBasedDeferredIrradiancePipeline
        let physicallyBasedDeferredIrradiancePipeline =
            { PhysicallyBasedDeferredIrradiancePipeline.EyeUniform = eyeUniform
              Pipeline = pipeline }

        // fin
        physicallyBasedDeferredIrradiancePipeline

    /// Destroy PhysicallyBasedDeferredIrradiancePipeline.
    let destroyPhysicallyBasedDeferredIrradiancePipeline (pipeline : PhysicallyBasedDeferredIrradiancePipeline) context =
        Pipeline.destroy pipeline.Pipeline context

    /// Draw the deferred irradiance pass of a physically-based surface.
    let drawPhysicallyBasedDeferredIrradianceSurface
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (projectionUnflipped : Matrix4x4)
        (depthTexture : Texture)
        (normalPlusTexture : Texture)
        (lightMappingTexture : Texture)
        (irradianceMap : Texture)
        (irradianceMaps : Texture array)
        (unfilteredSampler : Sampler)
        (filteredSampler : Sampler)
        (colorAttachment : Texture)
        (resolution : Vector2i)
        (renderPassIndex : int)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : PhysicallyBasedDeferredIrradiancePipeline)
        (context : VulkanContext) =

        // compute vulkan-appropriate matrices
        let viewInverse = view.Inverted
        let projection = projectionUnflipped.Flipped
        let projectionInverse = projection.Inverted
        let viewProjection = view * projection

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 renderPassIndex pipeline.Pipeline $ fun vkSet ->

                // specify eye
                let eye = EyeStruct (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
                VulkanBuffer.uploadValue eye pipeline.EyeUniform context
                Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.EyeUniform vkSet

                // specify static environment textures
                Pipeline.writeDescriptorSampledTexture 1 0 depthTexture vkSet
                Pipeline.writeDescriptorSampledTexture 2 0 normalPlusTexture vkSet
                Pipeline.writeDescriptorSampledTexture 3 0 lightMappingTexture vkSet
                Pipeline.writeDescriptorSampledTexture 4 0 irradianceMap vkSet
                Pipeline.writeDescriptorSampledTextures 5 0 (Array.tryTake Constants.Render.LightMapsMaxDeferred irradianceMaps) vkSet

            // specify samplers
            let mutable samplersDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 unfilteredSampler vkSet
                Pipeline.writeDescriptorSampler 1 0 filteredSampler vkSet

            // set up render
            let clearValue = VkClearValue (r = Constants.Render.IrradianceClearColor.R, g = Constants.Render.IrradianceClearColor.G, b = Constants.Render.IrradianceClearColor.B, a = Constants.Render.IrradianceClearColor.A)
            let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
            let mutable vkViewport = Hl.makeViewport false renderArea
            Hl.withRenderingInfo [|colorAttachment.ImageView|] None renderArea (Some clearValue) $ fun renderingInfo ->
                let mutable renderingInfo = renderingInfo
                DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
            DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
            DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

            // set up pipeline
            DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&uniformsDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&samplersDescriptorSet, 0u, nullPtr)

            // draw
            DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // tear down render
            DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

            // report drawing
            Hl.reportDrawCall 1 true

            // advance pipeline
            Pipeline.advance pipeline.Pipeline

            // advance rendering command buffer
            VulkanContext.advanceRenderCommandBuffer context

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// Create a deferred environment filter pipeline of a physically-based surface.
    let createPhysicallyBasedDeferredEnvironmentFilterPipeline colorAttachmentFormat context =

        // create uniform buffers
        let eyeUniform = VulkanBuffer.create Uniform sizeof<EyeStruct> context
        let lightMapsUniform = VulkanBuffer.create Uniform (Constants.Render.LightMapsMaxDeferred * sizeof<LightMapStruct>) context

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.PhysicallyBasedDeferredEnvironmentFilterShaderFilePath
                [|VulkanUnblended|] [|false|] StaticVertices
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer FragmentStage 1 // eye
                      Pipeline.descriptor 1 UniformBuffer FragmentStage 1 // lightMaps
                      Pipeline.descriptor 2 SampledImage FragmentStage 1 // depth
                      Pipeline.descriptor 3 SampledImage FragmentStage 1 // material
                      Pipeline.descriptor 4 SampledImage FragmentStage 1 // normalPlus
                      Pipeline.descriptor 5 SampledImage FragmentStage 1 // clearCoatPlus
                      Pipeline.descriptor 6 SampledImage FragmentStage 1 // lightMapping
                      Pipeline.descriptor 7 SampledImage FragmentStage 1 // environmentFilterMap
                      Pipeline.descriptor 8 SampledImage FragmentStage Constants.Render.LightMapsMaxDeferred|] // environmentFilterMaps
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1
                      Pipeline.descriptor 1 Sampler FragmentStage 1|]|]
                [||] [|colorAttachmentFormat|] None
                [|eyeUniform; lightMapsUniform|]

        // make PhysicallyBasedDeferredEnvironmentFilterPipeline
        let physicallyBasedDeferredEnvironmentFilterPipeline =
            { EyeUniform = eyeUniform
              LightMapsUniform = lightMapsUniform
              Pipeline = pipeline }
        
        // fin
        physicallyBasedDeferredEnvironmentFilterPipeline

    /// Destroy a deferred environment filter pipeline of a physically-based surface.
    let destroyPhysicallyBasedDeferredEnvironmentFilterPipeline (pipeline : PhysicallyBasedDeferredEnvironmentFilterPipeline) context =
        Pipeline.destroy pipeline.Pipeline context

    /// Draw a deferred environment filter pass of a physically-based surface.
    let drawPhysicallyBasedDeferredEnvironmentFilterSurface
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (projectionUnflipped : Matrix4x4)
        (lightMapOrigins : Vector3 array)
        (lightMapMins : Vector3 array)
        (lightMapSizes : Vector3 array)
        (lightMapAmbientColors : Color array)
        (lightMapAmbientBrightnesses : single array)
        (depthTexture : Texture)
        (materialTexture : Texture)
        (normalPlusTexture : Texture)
        (clearCoatPlusTexture : Texture)
        (lightMappingTexture : Texture)
        (environmentFilterMap : Texture)
        (environmentFilterMaps : Texture array)
        (unfilteredSampler : Sampler)
        (filteredSampler : Sampler)
        (colorAttachment : Texture)
        (resolution : Vector2i)
        (renderPassIndex : int)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : PhysicallyBasedDeferredEnvironmentFilterPipeline)
        (context : VulkanContext) =

        // compute vulkan-appropriate matrices
        let viewInverse = view.Inverted
        let projection = projectionUnflipped.Flipped
        let projectionInverse = projection.Inverted
        let viewProjection = view * projection

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 renderPassIndex pipeline.Pipeline $ fun vkSet ->

                // specify eye
                let eye = EyeStruct (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
                VulkanBuffer.uploadValue eye pipeline.EyeUniform context
                Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.EyeUniform vkSet

                // specify light maps
                let mutable lightMap = LightMapStruct ()
                use lightMapPtr = fixed &lightMap
                for i in 0 .. dec Constants.Render.LightMapsMaxDeferred do
                    if i < lightMapOrigins.Length then
                        lightMap.origin <- lightMapOrigins[i]
                        lightMap.min <- lightMapMins[i]
                        lightMap.size <- lightMapSizes[i]
                        lightMap.ambientColor <- lightMapAmbientColors[i].V3
                        lightMap.ambientBrightness <- lightMapAmbientBrightnesses[i]
                    else lightMap <- Unchecked.defaultof<_>
                    VulkanBuffer.writeSubdata (i * sizeof<LightMapStruct>) 0 sizeof<LightMapStruct> 1 (NativePtr.toNativeInt lightMapPtr) pipeline.LightMapsUniform context
                VulkanBuffer.flushSubdata 0 0 sizeof<LightMapStruct> Constants.Render.LightMapsMaxDeferred pipeline.LightMapsUniform context
                Pipeline.writeDescriptorUniformBuffer 1 0 pipeline.LightMapsUniform vkSet

                // specify static environment textures
                Pipeline.writeDescriptorSampledTexture 2 0 depthTexture vkSet
                Pipeline.writeDescriptorSampledTexture 3 0 materialTexture vkSet
                Pipeline.writeDescriptorSampledTexture 4 0 normalPlusTexture vkSet
                Pipeline.writeDescriptorSampledTexture 5 0 clearCoatPlusTexture vkSet
                Pipeline.writeDescriptorSampledTexture 6 0 lightMappingTexture vkSet
                Pipeline.writeDescriptorSampledTexture 7 0 environmentFilterMap vkSet
                Pipeline.writeDescriptorSampledTextures 8 0 (Array.tryTake Constants.Render.LightMapsMaxDeferred environmentFilterMaps) vkSet

            // specify samplers
            let mutable samplersDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 unfilteredSampler vkSet
                Pipeline.writeDescriptorSampler 1 0 filteredSampler vkSet

            // set up render
            let clearValue = VkClearValue (r = Constants.Render.EnvironmentClearColor.R, g = Constants.Render.EnvironmentClearColor.G, b = Constants.Render.EnvironmentClearColor.B, a = Constants.Render.EnvironmentClearColor.A)
            let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
            let mutable vkViewport = Hl.makeViewport false renderArea
            Hl.withRenderingInfo [|colorAttachment.ImageView|] None renderArea (Some clearValue) $ fun renderingInfo ->
                let mutable renderingInfo = renderingInfo
                DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
            DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
            DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

            // set up pipeline
            DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&uniformsDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&samplersDescriptorSet, 0u, nullPtr)

            // draw
            DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // tear down render
            DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

            // report drawing
            Hl.reportDrawCall 1 true

            // advance pipeline
            Pipeline.advance pipeline.Pipeline

            // advance rendering command buffer
            VulkanContext.advanceRenderCommandBuffer context

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// Create a deferred ssao pipeline of a physically-based surface.
    let createPhysicallyBasedDeferredSsaoPipeline colorAttachmentFormat context =

        // create set 0 uniform buffers
        let eyeUniform = VulkanBuffer.create Uniform sizeof<EyeStruct> context
        let ssaoUniform = VulkanBuffer.create Uniform sizeof<SsaoStruct> context

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.PhysicallyBasedDeferredSsaoShaderFilePath
                [|VulkanUnblended|] [|false|] StaticVertices
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer FragmentStage 1 // eye
                      Pipeline.descriptor 1 UniformBuffer FragmentStage 1 // ssao
                      Pipeline.descriptor 2 SampledImage FragmentStage 1 // depthTexture
                      Pipeline.descriptor 3 SampledImage FragmentStage 1|] // normalPlusTexture
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|] // inputSampler
                [||] [|colorAttachmentFormat|] None
                [|eyeUniform; ssaoUniform|]

        // make pipeline
        let physicallyBasedDeferredSsaoPipeline =
            { EyeUniform = eyeUniform
              SsaoUniform = ssaoUniform
              Pipeline = pipeline }

        // fin
        physicallyBasedDeferredSsaoPipeline

    /// Destroy a deferred ssao pipeline of a physically-based surface.
    let destroyPhysicallyBasedDeferredSsaoPipeline (ssaoPipeline : PhysicallyBasedDeferredSsaoPipeline) context =
        Pipeline.destroy ssaoPipeline.Pipeline context

    /// Draw a deferred ssao pass of a physically-based surface.
    let drawPhysicallyBasedDeferredSsaoSurface
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (projectionUnflipped : Matrix4x4)
        (intensity : single)
        (bias : single)
        (radius : single)
        (distanceMax : single)
        (sampleCount : int)
        (depthTexture : Texture)
        (normalPlusTexture : Texture)
        (unfilteredSampler : Sampler)
        (colorAttachment : Texture)
        (resolution : Vector2i)
        (renderPassIndex : int)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : PhysicallyBasedDeferredSsaoPipeline)
        (context : VulkanContext) =

        // compute vulkan-appropriate matrices
        let viewInverse = view.Inverted
        let projection = projectionUnflipped.Flipped
        let projectionInverse = projection.Inverted
        let viewProjection = view * projection

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 renderPassIndex pipeline.Pipeline $ fun vkSet ->

                // specify eye
                let eye = EyeStruct (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
                VulkanBuffer.uploadValue eye pipeline.EyeUniform context
                Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.EyeUniform vkSet

                // specify ssao
                let ssao = SsaoStruct (resolution = resolution, intensity = intensity, bias = bias, radius = radius, distanceMax = distanceMax, sampleCount = sampleCount)
                VulkanBuffer.uploadValue ssao pipeline.SsaoUniform context
                Pipeline.writeDescriptorUniformBuffer 1 0 pipeline.SsaoUniform vkSet

                // specify textures
                Pipeline.writeDescriptorSampledTexture 2 0 depthTexture vkSet
                Pipeline.writeDescriptorSampledTexture 3 0 normalPlusTexture vkSet

            // specify sampler
            let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 unfilteredSampler vkSet

            // set up render
            let clearValue = VkClearValue (r = Constants.Render.SsaoClearColor.R, g = Constants.Render.SsaoClearColor.G, b = Constants.Render.SsaoClearColor.B, a = Constants.Render.SsaoClearColor.A)
            let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
            let mutable vkViewport = Hl.makeViewport false renderArea
            Hl.withRenderingInfo [|colorAttachment.ImageView|] None renderArea (Some clearValue) $ fun renderingInfo ->
                let mutable renderingInfo = renderingInfo
                DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
            DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
            DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

            // set up pipeline
            DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&uniformsDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&samplerDescriptorSet, 0u, nullPtr)

            // draw
            DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // tear down render
            DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

            // report drawing
            Hl.reportDrawCall 1 true

            // advance pipeline
            Pipeline.advance pipeline.Pipeline

            // advance rendering command buffer
            VulkanContext.advanceRenderCommandBuffer context

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// Create a PhysicallyBasedDeferredColoringPipeline.
    let createPhysicallyBasedDeferredColoringPipeline colorAttachmentFormats context =

        // create uniform buffers
        let eyeUniform = VulkanBuffer.create Uniform sizeof<EyeStruct> context
        let lightingUniform = VulkanBuffer.create Uniform sizeof<LightingStruct> context

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.PhysicallyBasedDeferredColoringShaderFilePath
                [|VulkanUnblended|] [|false|] StaticVertices
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer FragmentStage 1 // eye
                      Pipeline.descriptor 1 UniformBuffer FragmentStage 1 // lighting
                      Pipeline.descriptor 2 SampledImage FragmentStage 1 // depthTexture
                      Pipeline.descriptor 3 SampledImage FragmentStage 1 // albedoTexture
                      Pipeline.descriptor 4 SampledImage FragmentStage 1 // materialTexture
                      Pipeline.descriptor 5 SampledImage FragmentStage 1 // normalPlusTexture
                      Pipeline.descriptor 6 SampledImage FragmentStage 1 // clearCoatPlusTexture
                      Pipeline.descriptor 7 SampledImage FragmentStage 1 // lightAccumTexture
                      Pipeline.descriptor 8 SampledImage FragmentStage 1 // brdfTexture
                      Pipeline.descriptor 9 SampledImage FragmentStage 1 // ambientTexture
                      Pipeline.descriptor 10 SampledImage FragmentStage 1 // irradianceTexture
                      Pipeline.descriptor 11 SampledImage FragmentStage 1 // environmentFilterTexture
                      Pipeline.descriptor 12 SampledImage FragmentStage 1|] // ssaoTexture
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1
                      Pipeline.descriptor 1 Sampler FragmentStage 1|]|]
                [||] colorAttachmentFormats None
                [|eyeUniform; lightingUniform|]

        // make PhysicallyBasedDeferredColoringPipeline
        let physicallyBasedDeferredColoringPipeline =
            { PhysicallyBasedDeferredColoringPipeline.EyeUniform = eyeUniform
              LightingUniform = lightingUniform
              Pipeline = pipeline }

        // fin
        physicallyBasedDeferredColoringPipeline

    /// Destroy PhysicallyBasedDeferredColoringPipeline.
    let destroyPhysicallyBasedDeferredColoringPipeline (pipeline : PhysicallyBasedDeferredColoringPipeline) context =
        Pipeline.destroy pipeline.Pipeline context

    /// Draw the deferred coloring pass of a physically-based surface.
    let drawPhysicallyBasedDeferredColoringSurface
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (projectionUnflipped : Matrix4x4)
        (lightAmbientBoostCutoff : single)
        (lightAmbientBoostScalar : single)
        (ssrlEnabled : int)
        (ssrlIntensity : single)
        (ssrlDetail : single)
        (ssrlRefinementsMax : int)
        (ssrlRayThickness : single)
        (ssrlTowardEyeCutoff : single)
        (ssrlDepthCutoff : single)
        (ssrlDepthCutoffMargin : single)
        (ssrlDistanceCutoff : single)
        (ssrlDistanceCutoffMargin : single)
        (ssrlRoughnessCutoff : single)
        (ssrlRoughnessCutoffMargin : single)
        (ssrlSlopeCutoff : single)
        (ssrlSlopeCutoffMargin : single)
        (ssrlEdgeHorizontalMargin : single)
        (ssrlEdgeVerticalMargin : single)
        (depthTexture : Texture)
        (albedoTexture : Texture)
        (materialTexture : Texture)
        (normalPlusTexture : Texture)
        (clearCoatPlusTexture : Texture)
        (lightAccumTexture : Texture)
        (brdfTexture : Texture)
        (ambientTexture : Texture)
        (irradianceTexture : Texture)
        (environmentFilterTexture : Texture)
        (ssaoTexture : Texture)
        (unfilteredSampler : Sampler)
        (filteredSampler : Sampler)
        (coloringAttachment : Texture)
        (depthAttachment : Texture)
        (resolution : Vector2i)
        (renderPassIndex : int)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : PhysicallyBasedDeferredColoringPipeline)
        (context : VulkanContext) =

        // compute vulkan-appropriate matrices
        let viewInverse = view.Inverted
        let projection = projectionUnflipped.Flipped
        let projectionInverse = projection.Inverted
        let viewProjection = view * projection

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 renderPassIndex pipeline.Pipeline $ fun vkSet ->

                // specify eye
                let eye = EyeStruct (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
                VulkanBuffer.uploadValue eye pipeline.EyeUniform context
                Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.EyeUniform vkSet

                // specify lighting
                let mutable lighting = LightingStruct ()
                lighting.lightAmbientBoostCutoff <- lightAmbientBoostCutoff
                lighting.lightAmbientBoostScalar <- lightAmbientBoostScalar
                lighting.ssrlEnabled <- ssrlEnabled
                lighting.ssrlIntensity <- ssrlIntensity
                lighting.ssrlDetail <- ssrlDetail
                lighting.ssrlRefinementsMax <- ssrlRefinementsMax
                lighting.ssrlRayThickness <- ssrlRayThickness
                lighting.ssrlTowardEyeCutoff <- ssrlTowardEyeCutoff
                lighting.ssrlDepthCutoff <- ssrlDepthCutoff
                lighting.ssrlDepthCutoffMargin <- ssrlDepthCutoffMargin
                lighting.ssrlDistanceCutoff <- ssrlDistanceCutoff
                lighting.ssrlDistanceCutoffMargin <- ssrlDistanceCutoffMargin
                lighting.ssrlRoughnessCutoff <- ssrlRoughnessCutoff
                lighting.ssrlRoughnessCutoffMargin <- ssrlRoughnessCutoffMargin
                lighting.ssrlSlopeCutoff <- ssrlSlopeCutoff
                lighting.ssrlSlopeCutoffMargin <- ssrlSlopeCutoffMargin
                lighting.ssrlEdgeHorizontalMargin <- ssrlEdgeHorizontalMargin
                lighting.ssrlEdgeVerticalMargin <- ssrlEdgeVerticalMargin
                VulkanBuffer.uploadValue lighting pipeline.LightingUniform context
                Pipeline.writeDescriptorUniformBuffer 1 0 pipeline.LightingUniform vkSet

                // specify textures
                Pipeline.writeDescriptorSampledTexture 2 0 depthTexture vkSet
                Pipeline.writeDescriptorSampledTexture 3 0 albedoTexture vkSet
                Pipeline.writeDescriptorSampledTexture 4 0 materialTexture vkSet
                Pipeline.writeDescriptorSampledTexture 5 0 normalPlusTexture vkSet
                Pipeline.writeDescriptorSampledTexture 6 0 clearCoatPlusTexture vkSet
                Pipeline.writeDescriptorSampledTexture 7 0 lightAccumTexture vkSet
                Pipeline.writeDescriptorSampledTexture 8 0 brdfTexture vkSet
                Pipeline.writeDescriptorSampledTexture 9 0 ambientTexture vkSet
                Pipeline.writeDescriptorSampledTexture 10 0 irradianceTexture vkSet
                Pipeline.writeDescriptorSampledTexture 11 0 environmentFilterTexture vkSet
                Pipeline.writeDescriptorSampledTexture 12 0 ssaoTexture vkSet

            // specify samplers
            let mutable samplersDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 unfilteredSampler vkSet
                Pipeline.writeDescriptorSampler 1 0 filteredSampler vkSet

            // set up render
            let clearValue = VkClearValue (r = Constants.Render.ViewportClearColor.R, g = Constants.Render.ViewportClearColor.G, b = Constants.Render.ViewportClearColor.B, a = Constants.Render.ViewportClearColor.A)
            let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
            let mutable vkViewport = Hl.makeViewport false renderArea
            Hl.withRenderingInfo [|coloringAttachment.ImageView; depthAttachment.ImageView|] None renderArea (Some clearValue) $ fun renderingInfo ->
                let mutable renderingInfo = renderingInfo
                DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
            DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
            DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

            // set up pipeline
            DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            let mutable (uniformsDescriptorSet, samplersDescriptorSet) = (uniformsDescriptorSet, samplersDescriptorSet)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&uniformsDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&samplersDescriptorSet, 0u, nullPtr)

            // draw
            DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // tear down render
            DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

            // report drawing
            Hl.reportDrawCall 1 true

            // advance pipeline
            Pipeline.advance pipeline.Pipeline

            // advance rendering command buffer
            VulkanContext.advanceRenderCommandBuffer context

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// Create a PhysicallyBasedDeferredCompositionPipeline.
    let createPhysicallyBasedDeferredCompositionPipeline colorAttachmentFormat context =

        // create uniform buffers
        let eyeUniform = VulkanBuffer.create Uniform sizeof<EyeStruct> context
        let lightingUniform = VulkanBuffer.create Uniform sizeof<LightingStruct> context

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.PhysicallyBasedDeferredCompositionShaderFilePath
                [|VulkanUnblended|] [|false|] StaticVertices
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer FragmentStage 1 // eye
                      Pipeline.descriptor 1 UniformBuffer FragmentStage 1 // lighting
                      Pipeline.descriptor 2 SampledImage FragmentStage 1 // depthTexture
                      Pipeline.descriptor 3 SampledImage FragmentStage 1 // colorTexture
                      Pipeline.descriptor 4 SampledImage FragmentStage 1|] // fogAccumTexture
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|]
                [||] [|colorAttachmentFormat|] None
                [|eyeUniform; lightingUniform|]

        // make PhysicallyBasedDeferredCompositionPipeline
        let physicallyBasedDeferredCompositionPipeline =
            { PhysicallyBasedDeferredCompositionPipeline.EyeUniform = eyeUniform
              LightingUniform = lightingUniform
              Pipeline = pipeline }

        // fin
        physicallyBasedDeferredCompositionPipeline

    /// Destroy PhysicallyBasedDeferredCompositionPipeline.
    let destroyPhysicallyBasedDeferredCompositionPipeline (pipeline : PhysicallyBasedDeferredCompositionPipeline) context =
        Pipeline.destroy pipeline.Pipeline context

    /// Draw the deferred composition pass of a physically-based surface.
    let drawPhysicallyBasedDeferredCompositionSurface
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (projectionUnflipped : Matrix4x4)
        (fogEnabled : int)
        (fogType : int)
        (fogStart : single)
        (fogFinish : single)
        (fogDensity : single)
        (fogColor : Color)
        (depthTexture : Texture)
        (colorTexture : Texture)
        (fogAccumTexture : Texture)
        (unfilteredSampler : Sampler)
        (compositionAttachment : Texture)
        (resolution : Vector2i)
        (renderPassIndex : int)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : PhysicallyBasedDeferredCompositionPipeline)
        (context : VulkanContext) =

        // compute vulkan-appropriate matrices
        let viewInverse = view.Inverted
        let projection = projectionUnflipped.Flipped
        let projectionInverse = projection.Inverted
        let viewProjection = view * projection

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 renderPassIndex pipeline.Pipeline $ fun vkSet ->

                // specify eye
                let eye = EyeStruct (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
                VulkanBuffer.uploadValue eye pipeline.EyeUniform context
                Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.EyeUniform vkSet

                // specify lighting
                let mutable lighting = LightingStruct ()
                lighting.fogEnabled <- fogEnabled
                lighting.fogType <- fogType
                lighting.fogStart <- fogStart
                lighting.fogFinish <- fogFinish
                lighting.fogDensity <- fogDensity
                lighting.fogColor <- fogColor.V4
                VulkanBuffer.uploadValue lighting pipeline.LightingUniform context
                Pipeline.writeDescriptorUniformBuffer 1 0 pipeline.LightingUniform vkSet

                // specify textures
                Pipeline.writeDescriptorSampledTexture 2 0 depthTexture vkSet
                Pipeline.writeDescriptorSampledTexture 3 0 colorTexture vkSet
                Pipeline.writeDescriptorSampledTexture 4 0 fogAccumTexture vkSet

            // specify samplers
            let mutable samplersDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 unfilteredSampler vkSet

            // set up render
            let clearValue = VkClearValue (r = Constants.Render.ViewportClearColor.R, g = Constants.Render.ViewportClearColor.G, b = Constants.Render.ViewportClearColor.B, a = Constants.Render.ViewportClearColor.A)
            let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
            let mutable vkViewport = Hl.makeViewport false renderArea
            Hl.withRenderingInfo [|compositionAttachment.ImageView|] None renderArea (Some clearValue) $ fun renderingInfo ->
                let mutable renderingInfo = renderingInfo
                DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
            DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
            DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

            // set up pipeline
            DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            let mutable (uniformsDescriptorSet, samplersDescriptorSet) = (uniformsDescriptorSet, samplersDescriptorSet)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&uniformsDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&samplersDescriptorSet, 0u, nullPtr)

            // draw
            DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // tear down render
            DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

            // report drawing
            Hl.reportDrawCall 1 true

            // advance pipeline
            Pipeline.advance pipeline.Pipeline

            // advance rendering command buffer
            VulkanContext.advanceRenderCommandBuffer context

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// Begin the process of drawing physically-based forward surfaces.
    let beginPhysicallyBasedForwardSurfaces
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (projectionUnflipped : Matrix4x4)
        (lightCutoffMargin : single)
        (lightAmbientColor : Color)
        (lightAmbientBrightness : single)
        (lightAmbientBoostCutoff : single)
        (lightAmbientBoostScalar : single)
        (lightShadowSamples : int)
        (lightShadowBias : single)
        (lightShadowSampleScalar : single)
        (lightShadowExponent : single)
        (lightShadowDensity : single)
        (fogEnabled : int)
        (fogType : int)
        (fogStart : single)
        (fogFinish : single)
        (fogDensity : single)
        (fogColor : Color)
        (ssvfEnabled : int)
        (ssvfIntensity : single)
        (ssvfSteps : int)
        (ssvfAsymmetry : single)
        (ssrrEnabled : int)
        (ssrrIntensity : single)
        (ssrrDetail : single)
        (ssrrRefinementsMax : int)
        (ssrrRayThickness : single)
        (ssrrDistanceCutoff : single)
        (ssrrDistanceCutoffMargin : single)
        (ssrrEdgeHorizontalMargin : single)
        (ssrrEdgeVerticalMargin : single)
        (shadowNear : single)
        (depthTexture : Texture)
        (colorTexture : Texture)
        (brdfTexture : Texture)
        (irradianceMap : Texture)
        (environmentFilterMap : Texture)
        (unfilteredSampler : Sampler)
        (filteredSampler : Sampler)
        (materialSampler : Sampler)
        (colorAttachment : Texture)
        (depthAttachment : Texture)
        (resolution : Vector2i)
        (renderPassIndex : int)
        (pipeline : PhysicallyBasedPipeline)
        (context : VulkanContext) =

        // compute vulkan-appropriate matrices
        let viewInverse = view.Inverted
        let projection = projectionUnflipped.Flipped
        let projectionInverse = projection.Inverted
        let viewProjection = view * projection

        // specify uniforms
        let mutable uniformDescriptorSet = Pipeline.specifyDescriptorSet 0 renderPassIndex pipeline.Pipeline $ fun vkSet ->

            // specify eye
            let eye = EyeStruct (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
            VulkanBuffer.uploadValue eye pipeline.EyeUniform context
            Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.EyeUniform vkSet

            // specify lighting
            let mutable lighting = LightingStruct ()
            lighting.lightCutoffMargin <- lightCutoffMargin
            lighting.lightAmbientColor <- lightAmbientColor.V3
            lighting.lightAmbientBrightness <- lightAmbientBrightness
            lighting.lightAmbientBoostCutoff <- lightAmbientBoostCutoff
            lighting.lightAmbientBoostScalar <- lightAmbientBoostScalar
            lighting.lightShadowSamples <- lightShadowSamples
            lighting.lightShadowBias <- lightShadowBias
            lighting.lightShadowSampleScalar <- lightShadowSampleScalar
            lighting.lightShadowExponent <- lightShadowExponent
            lighting.lightShadowDensity <- lightShadowDensity
            lighting.fogEnabled <- fogEnabled
            lighting.fogType <- fogType
            lighting.fogStart <- fogStart
            lighting.fogFinish <- fogFinish
            lighting.fogDensity <- fogDensity
            lighting.fogColor <- fogColor.V4
            lighting.ssvfEnabled <- ssvfEnabled
            lighting.ssvfIntensity <- ssvfIntensity
            lighting.ssvfSteps <- ssvfSteps
            lighting.ssvfAsymmetry <- ssvfAsymmetry
            lighting.ssrrEnabled <- ssrrEnabled
            lighting.ssrrIntensity <- ssrrIntensity
            lighting.ssrrDetail <- ssrrDetail
            lighting.ssrrRefinementsMax <- ssrrRefinementsMax
            lighting.ssrrRayThickness <- ssrrRayThickness
            lighting.ssrrDistanceCutoff <- ssrrDistanceCutoff
            lighting.ssrrDistanceCutoffMargin <- ssrrDistanceCutoffMargin
            lighting.ssrrEdgeHorizontalMargin <- ssrrEdgeHorizontalMargin
            lighting.ssrrEdgeVerticalMargin <- ssrrEdgeVerticalMargin
            lighting.shadowNear <- shadowNear
            VulkanBuffer.uploadValue lighting pipeline.LightingUniform context
            Pipeline.writeDescriptorUniformBuffer 1 0 pipeline.LightingUniform vkSet

            // specify static environment textures
            Pipeline.writeDescriptorSampledTexture 2 0 depthTexture vkSet
            Pipeline.writeDescriptorSampledTexture 3 0 colorTexture vkSet
            Pipeline.writeDescriptorSampledTexture 4 0 brdfTexture vkSet
            Pipeline.writeDescriptorSampledTexture 5 0 irradianceMap vkSet
            Pipeline.writeDescriptorSampledTexture 6 0 environmentFilterMap vkSet

        // specify samplers
        let mutable samplersDescriptorSet = Pipeline.specifyDescriptorSet 3 Unit pipeline.Pipeline $ fun vkSet ->
            Pipeline.writeDescriptorSampler 0 0 unfilteredSampler vkSet
            Pipeline.writeDescriptorSampler 1 0 filteredSampler vkSet
            Pipeline.writeDescriptorSampler 2 0 materialSampler vkSet

        // set up render
        let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
        let mutable vkViewport = Hl.makeViewport false renderArea
        Hl.withRenderingInfo [|colorAttachment.ImageView|] (Some depthAttachment.ImageView) renderArea None $ fun renderingInfo ->
            let mutable renderingInfo = renderingInfo
            DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
        DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
        DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

        // fin
        (uniformDescriptorSet, samplersDescriptorSet)

    /// Draw a batch of physically-based forward surfaces.
    /// TODO: P1: consider altering the representation of incoming light maps and lights data so that each can be
    /// uploaded with a single driver call.
    let drawPhysicallyBasedForwardSurfaces
        (bones : Matrix4x4 array)
        (surfacesCount : int)
        (instanceFields : single array)
        (irradianceMaps : Texture array)
        (environmentFilterMaps : Texture array)
        (shadowTextureArray : Texture)
        (shadowMaps : Texture array)
        (shadowCascades : Texture array)
        (lightMapOrigins : Vector3 array)
        (lightMapMins : Vector3 array)
        (lightMapSizes : Vector3 array)
        (lightMapAmbientColors : Color array)
        (lightMapAmbientBrightnesses : single array)
        (lightMapsCount : int)
        (lightMapSingletonBlendMargin : single)
        (lightOrigins : Vector3 array)
        (lightDirections : Vector3 array)
        (lightColors : Color array)
        (lightBrightnesses : single array)
        (lightAttenuationLinears : single array)
        (lightAttenuationQuadratics : single array)
        (lightCutoffs : single array)
        (lightTypes : int array)
        (lightConeInners : single array)
        (lightConeOuters : single array)
        (lightDesireFogs : int array)
        (lightShadowIndices : int array)
        (lightsCount : int)
        (shadowMatrices : Matrix4x4 array)
        (material : PhysicallyBasedMaterial)
        (geometry : PhysicallyBasedGeometry)
        (depthTest : DepthTest)
        (blending : bool)
        (uniformsDescriptorSet : VkDescriptorSet)
        (samplersDescriptorSet : VkDescriptorSet)
        (pipeline : PhysicallyBasedPipeline)
        (context : VulkanContext) =

        // only draw when required vkPipeline exists
        let blend = if blending then VulkanTransparent else VulkanUnblended
        match Pipeline.tryGetVkPipeline blend (not material.TwoSided) pipeline.Pipeline with
        | Some vkPipeline ->

            // specify instancing
            use instanceFieldsPin = new ArrayPin<_> (instanceFields)
            VulkanBuffer.uploadData (Constants.Render.InstanceFieldCount * sizeof<single>) surfacesCount instanceFieldsPin.NativeInt geometry.InstanceBuffer context

            // specify material
            let mutable materialDescriptorSet = Pipeline.specifyDescriptorSet 1 material pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampledTexture 0 0 material.AlbedoTexture vkSet
                Pipeline.writeDescriptorSampledTexture 1 0 material.RoughnessTexture vkSet
                Pipeline.writeDescriptorSampledTexture 2 0 material.MetallicTexture vkSet
                Pipeline.writeDescriptorSampledTexture 3 0 material.AmbientOcclusionTexture vkSet
                Pipeline.writeDescriptorSampledTexture 4 0 material.EmissionTexture vkSet
                Pipeline.writeDescriptorSampledTexture 5 0 material.NormalTexture vkSet
                Pipeline.writeDescriptorSampledTexture 6 0 material.HeightTexture vkSet

            // specify dynamic
            // NOTE: we do more work on bones specification even when there aren't bones to specify than in the other
            // draw calls.
            let mutable dynamicDescriptorSet = Pipeline.specifyDescriptorSet 2 pipeline.Pipeline.DrawIndex pipeline.Pipeline $ fun vkSet ->

                // specify bones when animated
                if bones.Length > 0 then
                    use bonesPin = new ArrayPin<_> (bones)
                    let bonesCount = min bones.Length Constants.Render.BonesMax
                    VulkanBuffer.uploadData sizeof<Matrix4x4> bonesCount bonesPin.NativeInt pipeline.BoneUniform context
                    Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.BoneUniform vkSet

                // specify light maps
                let mutable lightMap = LightMapStruct ()
                use lightMapPtr = fixed &lightMap
                for i in 0 .. dec Constants.Render.LightMapsMaxForward do
                    if i < lightMapOrigins.Length then
                        lightMap.origin <- lightMapOrigins[i]
                        lightMap.min <- lightMapMins[i]
                        lightMap.size <- lightMapSizes[i]
                        lightMap.ambientColor <- lightMapAmbientColors[i].V3
                        lightMap.ambientBrightness <- lightMapAmbientBrightnesses[i]
                    else lightMap <- Unchecked.defaultof<_>
                    VulkanBuffer.writeSubdata (i * sizeof<LightMapStruct>) 0 sizeof<LightMapStruct> 1 (NativePtr.toNativeInt lightMapPtr) pipeline.LightMapUniform context
                VulkanBuffer.flushSubdata 0 0 sizeof<LightMapStruct> Constants.Render.LightMapsMaxForward pipeline.LightMapUniform context
                Pipeline.writeDescriptorUniformBuffer 1 0 pipeline.LightMapUniform vkSet

                // specify lights general
                let mutable lightsGeneral = LightsGeneralStruct ()
                lightsGeneral.lightMapsCount <- lightMapsCount
                lightsGeneral.lightMapSingletonBlendMargin <- lightMapSingletonBlendMargin
                lightsGeneral.lightsCount <- lightsCount
                VulkanBuffer.uploadValue lightsGeneral pipeline.LightsGeneralUniform context
                Pipeline.writeDescriptorUniformBuffer 2 0 pipeline.LightsGeneralUniform vkSet

                // specify lights
                let mutable light = LightStruct ()
                use lightPtr = fixed &light
                for i in 0 .. dec Constants.Render.LightsMaxForward do
                    if i < lightOrigins.Length then
                        light.origin <- lightOrigins[i]
                        light.direction <- lightDirections[i]
                        light.color <- lightColors[i].V3
                        light.brightness <- lightBrightnesses[i]
                        light.attenuationLinear <- lightAttenuationLinears[i]
                        light.attenuationQuadratic <- lightAttenuationQuadratics[i]
                        light.cutoff <- lightCutoffs[i]
                        light.lightType <- lightTypes[i]
                        light.coneInner <- lightConeInners[i]
                        light.coneOuter <- lightConeOuters[i]
                        light.desireFog <- lightDesireFogs[i]
                        light.shadowIndex <- lightShadowIndices[i]
                    else light <- Unchecked.defaultof<_>
                    VulkanBuffer.writeSubdata (i * sizeof<LightStruct>) 0 sizeof<LightStruct> 1 (NativePtr.toNativeInt lightPtr) pipeline.LightUniform context
                VulkanBuffer.flushSubdata 0 0 sizeof<LightStruct> Constants.Render.LightsMaxForward pipeline.LightUniform context
                Pipeline.writeDescriptorUniformBuffer 3 0 pipeline.LightUniform vkSet

                // specify shadow matrices
                use shadowMatricesPin = new ArrayPin<_> (shadowMatrices)
                let shadowMatricesCount = min shadowMatrices.Length (Constants.Render.ShadowTexturesMax + Constants.Render.ShadowCascadesMax * Constants.Render.ShadowCascadeLevels)
                VulkanBuffer.uploadData sizeof<Matrix4x4> shadowMatricesCount shadowMatricesPin.NativeInt pipeline.ShadowMatrixUniform context
                Pipeline.writeDescriptorUniformBuffer 4 0 pipeline.ShadowMatrixUniform vkSet

                // specify dynamic environment textures
                Pipeline.writeDescriptorSampledTextures 5 0 (Array.tryTake Constants.Render.LightMapsMaxForward irradianceMaps) vkSet
                Pipeline.writeDescriptorSampledTextures 6 0 (Array.tryTake Constants.Render.LightMapsMaxForward environmentFilterMaps) vkSet
                Pipeline.writeDescriptorSampledTexture 7 0 shadowTextureArray vkSet
                Pipeline.writeDescriptorSampledTextures 8 0 (Array.tryTake Constants.Render.ShadowMapsMax shadowMaps) vkSet
                Pipeline.writeDescriptorSampledTextures 9 0 (Array.tryTake Constants.Render.ShadowCascadesMax shadowCascades) vkSet

            // set up pipeline
            DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)
            DeviceApi.vkCmdSetDepthTestEnable (context.RenderCommandBuffer, not depthTest.IsAlwaysPassTest)
            DeviceApi.vkCmdSetDepthCompareOp (context.RenderCommandBuffer, Pipeline.depthTestToVkCompareOp depthTest)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            let mutable (uniformsDescriptorSet, samplersDescriptorSet) = (uniformsDescriptorSet, samplersDescriptorSet)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&uniformsDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&materialDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 2u, 1u, &&dynamicDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 3u, 1u, &&samplersDescriptorSet, 0u, nullPtr)

            // draw
            DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, uint surfacesCount, 0u, 0, 0u)

            // report drawing
            Hl.reportDrawCall surfacesCount false

            // advance instancing
            VulkanBuffer.advance geometry.InstanceBuffer

            // advance pipeline
            Pipeline.advance pipeline.Pipeline

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// End the process of drawing physically-based forward surfaces.
    let endPhysicallyBasedForwardSurfaces (_ : PhysicallyBasedPipeline) (context : VulkanContext)=

        // tear down render
        DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

        // report draw scope
        Hl.reportDrawScope ()

        // advance rendering command buffer
        VulkanContext.advanceRenderCommandBuffer context

    let createPhysicallyBasedPipelines lightMapsMax lightsMax attachments context =

        // create 1d box filter pipeline
        let filterBox1dPipeline = createFilterBoxPipeline Constants.Paths.FilterBox1dShaderFilePath R16f.VkFormat

        // create bilateral down-sample pipeline
        let filterBilateralDownSamplePipeline =
            createFilterDownSamplePipeline
                Constants.Paths.FilterBilateralDownSample4dShaderFilePath
                [|attachments.DownSampleColorAttachment.VkFormat
                  attachments.DownSampleDepthAttachment.VkFormat|]

        // create bilateral up-sample pipeline
        let filterBilateralUpSamplePipeline =
            createFilterUpSamplePipeline
                Constants.Paths.FilterBilateralUpSample4dShaderFilePath
                attachments.UpSampleColorAttachment.VkFormat

        // create esm gaussian filter pipeline
        let filterGaussianEsmPipeline = createFilterGaussianEsmPipeline Rg32f.VkFormat context

        // create depth-of-field gaussian filter pipeline
        let filterGaussianDofPipeline = createFilterGaussianDofPipeline Rgba16f.VkFormat context

        // create depth-of-field filter pipeline
        let filterDepthOfFieldPipeline = createFilterDepthOfFieldPipeline Rgba16f.VkFormat context

        // create bloom extract filter pipeline
        let filterBloomExtractPipeline = createFilterBloomExtractPipeline attachments.BloomExtractAttachment.VkFormat context

        // create bloom down-sample filter pipeline
        let filterBloomDownSamplePipeline = createFilterBloomDownSamplePipeline attachments.BloomSampleAttachments[0].VkFormat context

        // create bloom up-sample filter pipeline
        let filterBloomUpSamplePipeline = createFilterBloomUpSamplePipeline attachments.BloomSampleAttachments[0].VkFormat context

        // create bloom apply filter pipeline
        let filterBloomApplyPipeline = createFilterBloomApplyPipeline attachments.BloomApplyAttachment.VkFormat context

        // create tone-mapping filter pipeline
        let filterToneMappingPipeline = createFilterToneMappingPipeline attachments.ToneMappingAttachment.VkFormat context

        // create chromatic aberration filter pipeline
        let filterChromaticAberrationPipeline = createFilterChromaticAberrationPipeline Rgba16f.VkFormat context

        // create tone-mapping filter pipeline
        let filterFxaaPipeline = createFilterFxaaPipeline attachments.ColorFull0Attachment.VkFormat context

        // create gamma-correction filter pipeline
        let filterGammaCorrectionPipeline = createFilterGammaCorrectionPipeline attachments.GammaCorrectionAttachment.VkFormat

        // create shadow static point pipeline
        let (shadowMapColorAttachment, shadowMapZAttachment) = attachments.ShadowMapAttachmentsArray[0] // assume all like first
        let shadowStaticPointPipeline =
            createPhysicallyBasedShadowPipeline
                Constants.Paths.PhysicallyBasedShadowStaticPointShaderFilePath
                StaticVertices
                [|shadowMapColorAttachment.VkFormat|]
                shadowMapZAttachment.VkFormat
                context

        // create shadow static spot pipeline
        let (shadowTextureArrayColorAttachment, shadowTextureArrayZAttachment) = attachments.ShadowTextureArrayAttachments
        let shadowStaticSpotPipeline =
            createPhysicallyBasedShadowPipeline
                Constants.Paths.PhysicallyBasedShadowStaticSpotShaderFilePath
                StaticVertices
                [|shadowTextureArrayColorAttachment.VkFormat|]
                shadowTextureArrayZAttachment.VkFormat
                context

        // create shadow static directional pipeline
        let shadowStaticDirectionalPipeline =
            createPhysicallyBasedShadowPipeline
                Constants.Paths.PhysicallyBasedShadowStaticDirectionalShaderFilePath
                StaticVertices
                [|shadowTextureArrayColorAttachment.VkFormat|]
                shadowTextureArrayZAttachment.VkFormat
                context

        // create shadow animated point pipeline
        let shadowAnimatedPointPipeline =
            createPhysicallyBasedShadowPipeline
                Constants.Paths.PhysicallyBasedShadowAnimatedPointShaderFilePath
                AnimatedVertices
                [|shadowMapColorAttachment.VkFormat|]
                shadowMapZAttachment.VkFormat
                context

        // create shadow animated spot pipeline
        let shadowAnimatedSpotPipeline =
            createPhysicallyBasedShadowPipeline
                Constants.Paths.PhysicallyBasedShadowAnimatedSpotShaderFilePath
                AnimatedVertices
                [|shadowTextureArrayColorAttachment.VkFormat|]
                shadowTextureArrayZAttachment.VkFormat
                context

        // create shadow animated directional pipeline
        let shadowAnimatedDirectionalPipeline =
            createPhysicallyBasedShadowPipeline
                Constants.Paths.PhysicallyBasedShadowAnimatedDirectionalShaderFilePath
                AnimatedVertices
                [|shadowTextureArrayColorAttachment.VkFormat|]
                shadowTextureArrayZAttachment.VkFormat
                context

        // create shadow terrain point pipeline
        let shadowTerrainPointPipeline =
            createPhysicallyBasedTerrainPipeline
                Constants.Paths.PhysicallyBasedShadowTerrainPointShaderFilePath
                [|shadowMapColorAttachment.VkFormat|]
                shadowTextureArrayZAttachment.VkFormat
                context

        // create shadow terrain spot pipeline
        let shadowTerrainSpotPipeline =
            createPhysicallyBasedTerrainPipeline
                Constants.Paths.PhysicallyBasedShadowTerrainSpotShaderFilePath
                [|shadowTextureArrayColorAttachment.VkFormat|]
                shadowTextureArrayZAttachment.VkFormat
                context

        // create shadow terrain directional pipeline
        let shadowTerrainDirectionalPipeline =
            createPhysicallyBasedTerrainPipeline
                Constants.Paths.PhysicallyBasedShadowTerrainDirectionalShaderFilePath
                [|shadowTextureArrayColorAttachment.VkFormat|]
                shadowTextureArrayZAttachment.VkFormat
                context

        // create deferred static pipeline
        let (depth, albedo, material, normalPlus, subdermalPlus, scatterPlus, clearCoatPlus, z) =
            attachments.GeometryAttachments
        let deferredColorAttachmentFormats =
            [|depth.VkFormat; albedo.VkFormat; material.VkFormat; normalPlus.VkFormat; subdermalPlus.VkFormat; scatterPlus.VkFormat; clearCoatPlus.VkFormat|]
        let deferredStaticPipeline =
            createPhysicallyBasedPipeline
                lightMapsMax
                lightsMax
                Constants.Paths.PhysicallyBasedDeferredStaticShaderFilePath
                [|VulkanUnblended|]
                [|false; true|]
                StaticVertices
                deferredColorAttachmentFormats
                (Some z.VkFormat)
                context

        // create deferred static clipped pipeline
        let deferredStaticClippedPipeline =
            createPhysicallyBasedPipeline
                lightMapsMax
                lightsMax
                Constants.Paths.PhysicallyBasedDeferredStaticClippedShaderFilePath
                [|VulkanUnblended|]
                [|false; true|]
                StaticVertices
                deferredColorAttachmentFormats
                (Some z.VkFormat)
                context

        // create deferred animated pipeline
        let deferredAnimatedPipeline =
            createPhysicallyBasedPipeline
                lightMapsMax
                lightsMax
                Constants.Paths.PhysicallyBasedDeferredAnimatedShaderFilePath
                [|VulkanUnblended|]
                [|false; true|]
                AnimatedVertices
                deferredColorAttachmentFormats
                (Some z.VkFormat)
                context

        // create deferred terrain pipeline
        let deferredTerrainColorAttachmentFormats =
            [|depth.VkFormat; albedo.VkFormat; material.VkFormat; normalPlus.VkFormat; subdermalPlus.VkFormat; scatterPlus.VkFormat|]
        let deferredTerrainPipeline =
            createPhysicallyBasedTerrainPipeline
                Constants.Paths.PhysicallyBasedDeferredTerrainShaderFilePath
                deferredTerrainColorAttachmentFormats
                z.VkFormat
                context
        
        // create deferred lighting pipelines
        let deferredLightingPipeline = createPhysicallyBasedDeferredLightingPipeline attachments.LightingAttachment.VkFormat context
        let deferredFoggingPipeline = createPhysicallyBasedDeferredFoggingPipeline attachments.FoggingAttachment.VkFormat context
        let deferredLightMappingPipeline = createPhysicallyBasedDeferredLightMappingPipeline attachments.LightMappingAttachment.VkFormat context
        let deferredAmbientPipeline = createPhysicallyBasedDeferredAmbientPipeline attachments.AmbientAttachment.VkFormat context
        let deferredIrradiancePipeline = createPhysicallyBasedDeferredIrradiancePipeline attachments.IrradianceAttachment.VkFormat context
        let deferredEnvironmentFilterPipeline = createPhysicallyBasedDeferredEnvironmentFilterPipeline attachments.EnvironmentFilterAttachment.VkFormat context
        let deferredSsaoPipeline = createPhysicallyBasedDeferredSsaoPipeline attachments.SsaoUnfilteredAttachment.VkFormat context
        let deferredColoringPipeline = createPhysicallyBasedDeferredColoringPipeline [|(fst attachments.ColoringAttachments).VkFormat; (snd attachments.ColoringAttachments).VkFormat|] context
        let deferredCompositionPipeline = createPhysicallyBasedDeferredCompositionPipeline attachments.CompositionAttachment.VkFormat context
        
        // create forward static pipeline
        let composition = attachments.CompositionAttachment
        let forwardStaticPipeline =
            createPhysicallyBasedPipeline
                Constants.Render.LightMapsMaxForward
                Constants.Render.LightsMaxForward
                Constants.Paths.PhysicallyBasedForwardStaticShaderFilePath
                [|VulkanUnblended; VulkanTransparent|]
                [|false; true|]
                StaticVertices
                [|composition.VkFormat|]
                (Some z.VkFormat)
                context

        // create forward animated pipeline
        let forwardAnimatedPipeline =
            createPhysicallyBasedPipeline
                Constants.Render.LightMapsMaxForward
                Constants.Render.LightsMaxForward
                Constants.Paths.PhysicallyBasedForwardAnimatedShaderFilePath
                [|VulkanUnblended; VulkanTransparent|]
                [|false; true|]
                AnimatedVertices
                [|composition.VkFormat|]
                (Some z.VkFormat)
                context
        
        // create PhysicallyBasedPipelines
        let physicallyBasedPipelines =
            { FilterBox1dPipeline = filterBox1dPipeline
              FilterBilateralDownSamplePipeline = filterBilateralDownSamplePipeline
              FilterBilateralUpSamplePipeline = filterBilateralUpSamplePipeline
              FilterGaussianEsmPipeline = filterGaussianEsmPipeline
              FilterGaussianDofPipeline = filterGaussianDofPipeline
              FilterDepthOfFieldPipeline = filterDepthOfFieldPipeline
              FilterBloomExtractPipeline = filterBloomExtractPipeline
              FilterBloomDownSamplePipeline = filterBloomDownSamplePipeline
              FilterBloomUpSamplePipeline = filterBloomUpSamplePipeline
              FilterBloomApplyPipeline = filterBloomApplyPipeline
              FilterToneMappingPipeline = filterToneMappingPipeline
              FilterChromaticAberrationPipeline = filterChromaticAberrationPipeline
              FilterFxaaPipeline = filterFxaaPipeline
              FilterGammaCorrectionPipeline = filterGammaCorrectionPipeline
              ShadowStaticPointPipeline = shadowStaticPointPipeline
              ShadowStaticSpotPipeline = shadowStaticSpotPipeline
              ShadowStaticDirectionalPipeline = shadowStaticDirectionalPipeline
              ShadowAnimatedPointPipeline = shadowAnimatedPointPipeline
              ShadowAnimatedSpotPipeline = shadowAnimatedSpotPipeline
              ShadowAnimatedDirectionalPipeline = shadowAnimatedDirectionalPipeline
              ShadowTerrainPointPipeline = shadowTerrainPointPipeline
              ShadowTerrainSpotPipeline = shadowTerrainSpotPipeline
              ShadowTerrainDirectionalPipeline = shadowTerrainDirectionalPipeline
              DeferredStaticPipeline = deferredStaticPipeline
              DeferredStaticClippedPipeline = deferredStaticClippedPipeline
              DeferredAnimatedPipeline = deferredAnimatedPipeline
              DeferredTerrainPipeline = deferredTerrainPipeline
              DeferredLightingPipeline = deferredLightingPipeline
              DeferredFoggingPipeline = deferredFoggingPipeline
              DeferredLightMappingPipeline = deferredLightMappingPipeline
              DeferredAmbientPipeline = deferredAmbientPipeline
              DeferredIrradiancePipeline = deferredIrradiancePipeline
              DeferredEnvironmentFilterPipeline = deferredEnvironmentFilterPipeline
              DeferredSsaoPipeline = deferredSsaoPipeline
              DeferredColoringPipeline = deferredColoringPipeline
              DeferredCompositionPipeline = deferredCompositionPipeline
              ForwardStaticPipeline = forwardStaticPipeline
              ForwardAnimatedPipeline = forwardAnimatedPipeline }

        // fin
        physicallyBasedPipelines

    let beginPhysicallyBasedPipelines physicallyBasedPipelines =
        Pipeline.beginFrame physicallyBasedPipelines.FilterBox1dPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.FilterBilateralDownSamplePipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.FilterBilateralUpSamplePipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.FilterGaussianEsmPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.FilterGaussianDofPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.FilterDepthOfFieldPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.FilterBloomExtractPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.FilterBloomDownSamplePipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.FilterBloomUpSamplePipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.FilterBloomApplyPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.FilterToneMappingPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.FilterChromaticAberrationPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.FilterFxaaPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.FilterGammaCorrectionPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.ShadowStaticPointPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.ShadowStaticSpotPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.ShadowStaticDirectionalPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.ShadowAnimatedPointPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.ShadowAnimatedSpotPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.ShadowAnimatedDirectionalPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.ShadowTerrainPointPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.ShadowTerrainSpotPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.ShadowTerrainDirectionalPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.DeferredStaticPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.DeferredStaticClippedPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.DeferredAnimatedPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.DeferredTerrainPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.DeferredLightingPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.DeferredFoggingPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.DeferredLightMappingPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.DeferredAmbientPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.DeferredIrradiancePipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.DeferredEnvironmentFilterPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.DeferredSsaoPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.DeferredColoringPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.DeferredCompositionPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.ForwardStaticPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.ForwardAnimatedPipeline.Pipeline

    let destroyPhysicallyBasedPipelines physicallyBasedPipelines context =
        destroyFilterBoxPipeline physicallyBasedPipelines.FilterBox1dPipeline context
        destroyFilterDownSamplePipeline physicallyBasedPipelines.FilterBilateralDownSamplePipeline context
        destroyFilterUpSamplePipeline physicallyBasedPipelines.FilterBilateralUpSamplePipeline context
        destroyFilterGaussianEsmPipeline physicallyBasedPipelines.FilterGaussianEsmPipeline context
        destroyFilterGaussianDofPipeline physicallyBasedPipelines.FilterGaussianDofPipeline context
        destroyFilterDepthOfFieldPipeline physicallyBasedPipelines.FilterDepthOfFieldPipeline context
        destroyFilterBloomExtractPipeline physicallyBasedPipelines.FilterBloomExtractPipeline context
        destroyFilterBloomDownSamplePipeline physicallyBasedPipelines.FilterBloomDownSamplePipeline context
        destroyFilterBloomUpSamplePipeline physicallyBasedPipelines.FilterBloomUpSamplePipeline context
        destroyFilterBloomApplyPipeline physicallyBasedPipelines.FilterBloomApplyPipeline context
        destroyFilterToneMappingPipeline physicallyBasedPipelines.FilterToneMappingPipeline context
        destroyFilterChromaticAberrationPipeline physicallyBasedPipelines.FilterChromaticAberrationPipeline context
        destroyFilterFxaaPipeline physicallyBasedPipelines.FilterFxaaPipeline context
        destroyFilterGammaCorrectionPipeline physicallyBasedPipelines.FilterGammaCorrectionPipeline context
        destroyPhysicallyBasedShadowPipeline physicallyBasedPipelines.ShadowStaticPointPipeline context
        destroyPhysicallyBasedShadowPipeline physicallyBasedPipelines.ShadowStaticSpotPipeline context
        destroyPhysicallyBasedShadowPipeline physicallyBasedPipelines.ShadowStaticDirectionalPipeline context
        destroyPhysicallyBasedShadowPipeline physicallyBasedPipelines.ShadowAnimatedPointPipeline context
        destroyPhysicallyBasedShadowPipeline physicallyBasedPipelines.ShadowAnimatedSpotPipeline context
        destroyPhysicallyBasedShadowPipeline physicallyBasedPipelines.ShadowAnimatedDirectionalPipeline context
        destroyPhysicallyBasedDeferredTerrainPipeline physicallyBasedPipelines.ShadowTerrainPointPipeline context
        destroyPhysicallyBasedDeferredTerrainPipeline physicallyBasedPipelines.ShadowTerrainSpotPipeline context
        destroyPhysicallyBasedDeferredTerrainPipeline physicallyBasedPipelines.ShadowTerrainDirectionalPipeline context
        destroyPhysicallyBasedPipeline physicallyBasedPipelines.DeferredStaticPipeline context
        destroyPhysicallyBasedPipeline physicallyBasedPipelines.DeferredStaticClippedPipeline context
        destroyPhysicallyBasedPipeline physicallyBasedPipelines.DeferredAnimatedPipeline context
        destroyPhysicallyBasedDeferredTerrainPipeline physicallyBasedPipelines.DeferredTerrainPipeline context
        destroyPhysicallyBasedDeferredLightingPipeline physicallyBasedPipelines.DeferredLightingPipeline context
        destroyPhysicallyBasedDeferredFoggingPipeline physicallyBasedPipelines.DeferredFoggingPipeline context
        destroyPhysicallyBasedDeferredLightMappingPipeline physicallyBasedPipelines.DeferredLightMappingPipeline context
        destroyPhysicallyBasedDeferredAmbientPipeline physicallyBasedPipelines.DeferredAmbientPipeline context
        destroyPhysicallyBasedDeferredIrradiancePipeline physicallyBasedPipelines.DeferredIrradiancePipeline context
        destroyPhysicallyBasedDeferredEnvironmentFilterPipeline physicallyBasedPipelines.DeferredEnvironmentFilterPipeline context
        destroyPhysicallyBasedDeferredSsaoPipeline physicallyBasedPipelines.DeferredSsaoPipeline context
        destroyPhysicallyBasedDeferredColoringPipeline physicallyBasedPipelines.DeferredColoringPipeline context
        destroyPhysicallyBasedDeferredCompositionPipeline physicallyBasedPipelines.DeferredCompositionPipeline context
        destroyPhysicallyBasedPipeline physicallyBasedPipelines.ForwardStaticPipeline context
        destroyPhysicallyBasedPipeline physicallyBasedPipelines.ForwardAnimatedPipeline context

    let reloadPhysicallyBasedShaders physicallyBasedPipelines context =
        Pipeline.reloadShaders physicallyBasedPipelines.FilterBox1dPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.FilterBilateralDownSamplePipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.FilterBilateralUpSamplePipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.FilterGaussianEsmPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.FilterGaussianDofPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.FilterDepthOfFieldPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.FilterBloomExtractPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.FilterBloomDownSamplePipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.FilterBloomUpSamplePipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.FilterBloomApplyPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.FilterToneMappingPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.FilterChromaticAberrationPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.FilterFxaaPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.FilterGammaCorrectionPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.FilterFxaaPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.ShadowStaticPointPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.ShadowStaticSpotPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.ShadowStaticDirectionalPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.ShadowAnimatedPointPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.ShadowAnimatedSpotPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.ShadowAnimatedDirectionalPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.ShadowTerrainPointPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.ShadowTerrainSpotPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.ShadowTerrainDirectionalPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.DeferredStaticPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.DeferredStaticClippedPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.DeferredAnimatedPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.DeferredLightingPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.DeferredFoggingPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.DeferredLightMappingPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.DeferredAmbientPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.DeferredIrradiancePipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.DeferredEnvironmentFilterPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.DeferredSsaoPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.DeferredColoringPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.DeferredCompositionPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.ForwardStaticPipeline.Pipeline context
        Pipeline.reloadShaders physicallyBasedPipelines.ForwardAnimatedPipeline.Pipeline context

/// Memoizes physically-based scene loads.
type PhysicallyBasedSceneClient () =

    /// Attempt to create physically-based model from a model file with assimp.
    /// Thread-safe if contextOpt = None.
    member this.TryCreatePhysicallyBasedModel filePath defaultMaterial textureClient contextOpt =

        // attempt to import from assimp scene
        match AssimpContext.TryGetScene filePath with
        | Right scene ->
            let dirPath = PathF.GetDirectoryName filePath
            match PhysicallyBased.tryCreatePhysicallyBasedMaterials dirPath defaultMaterial textureClient scene contextOpt with
            | Right materials ->
                let animated = scene.Animations.Count <> 0
                let geometries =
                    if animated
                    then PhysicallyBased.createPhysicallyBasedAnimatedGeometries scene contextOpt
                    else PhysicallyBased.createPhysicallyBasedStaticGeometries scene contextOpt

                // collect light nodes
                let lightNodes =
                    [|for i in 0 .. dec scene.LightCount do
                        let light = scene.Lights[i]
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
                            let (light, lightNode) = lightNodes[i]
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
                            let meshIndex = node.MeshIndices[i]
                            let materialIndex = scene.Meshes[meshIndex].MaterialIndex
                            let (properties, material) = materials[materialIndex]
                            let geometry = geometries[meshIndex]
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