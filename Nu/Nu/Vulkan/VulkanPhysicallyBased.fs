// Nu Game Engine.
// Copyright (C) Bryan Edds.

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
type ShadowVert =
    [<FieldOffset(0)>] val mutable viewProjection : Matrix4x4

[<Struct; StructLayout (LayoutKind.Explicit)>]
type ShadowFrag =
    [<FieldOffset(0)>] val mutable eyeCenter : Vector3
    [<FieldOffset(4)>] val mutable lightShadowExponent : single

[<Struct; StructLayout (LayoutKind.Explicit)>]
type Lighting =
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
type Lighting2 =
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
type LightMap' =
    [<FieldOffset(0)>] val mutable lightMapOrigins : Vector3
    [<FieldOffset(16)>] val mutable lightMapMins : Vector3
    [<FieldOffset(32)>] val mutable lightMapSizes : Vector3
    [<FieldOffset(48)>] val mutable lightMapAmbientColors : Vector3
    [<FieldOffset(60)>] val mutable lightMapAmbientBrightnesses : single

[<Struct; StructLayout (LayoutKind.Explicit)>]
type LightsGeneral =
    [<FieldOffset(0)>] val mutable lightMapsCount : int
    [<FieldOffset(4)>] val mutable lightMapSingletonBlendMargin : single
    [<FieldOffset(8)>] val mutable lightsCount : int

[<Struct; StructLayout (LayoutKind.Explicit)>]
type Light =
    [<FieldOffset(0)>] val mutable lightOrigins : Vector3
    [<FieldOffset(16)>] val mutable lightDirections : Vector3
    [<FieldOffset(32)>] val mutable lightColors : Vector3
    [<FieldOffset(44)>] val mutable lightBrightnesses : single
    [<FieldOffset(48)>] val mutable lightAttenuationLinears : single
    [<FieldOffset(52)>] val mutable lightAttenuationQuadratics : single
    [<FieldOffset(56)>] val mutable lightCutoffs : single
    [<FieldOffset(60)>] val mutable lightTypes : int
    [<FieldOffset(64)>] val mutable lightConeInners : single
    [<FieldOffset(68)>] val mutable lightConeOuters : single
    [<FieldOffset(72)>] val mutable lightDesireFogs : int
    [<FieldOffset(76)>] val mutable lightShadowIndices : int

/// A set of physically-based attachments that support a given viewport.
type PhysicallyBasedAttachments =
    { ShadowTextureArrayAttachments : Texture * Texture
      ShadowMapAttachmentsArray : (Texture * Texture) array
      ShadowCascadeArrayAttachmentsArray : (Texture * Texture) array
      GeometryAttachments : Texture * Texture * Texture * Texture * Texture * Texture * Texture * Texture
      LightingAttachment : Texture
      LightMappingAttachment : Texture
      AmbientAttachment : Texture
      IrradianceAttachment : Texture
      EnvironmentFilterAttachment : Texture
      FoggingAttachment : Texture
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
        refEq left right && // OPTIMIZATION: first check ref equality.
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
      VertexBuffer : Nu.Vulkan.Buffer
      InstanceBuffer : Nu.Vulkan.Buffer
      IndexBuffer : Nu.Vulkan.Buffer }

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
    { ShadowVertUniform : Nu.Vulkan.Buffer
      BoneUniform : Nu.Vulkan.Buffer
      ShadowFragUniform : Nu.Vulkan.Buffer
      Pipeline : Pipeline }

/// Describes a physically-based pipeline that's loaded into GPU.
type PhysicallyBasedPipeline =
    { EyeUniform : Nu.Vulkan.Buffer
      LightingUniform : Nu.Vulkan.Buffer
      BoneUniform : Nu.Vulkan.Buffer
      LightMapUniform : Nu.Vulkan.Buffer
      LightsGeneralUniform : Nu.Vulkan.Buffer
      LightUniform : Nu.Vulkan.Buffer
      ShadowMatrixUniform : Nu.Vulkan.Buffer
      Pipeline : Pipeline }

/// Describes the lighting pass of a deferred physically-based pipeline that's loaded into GPU.
type PhysicallyBasedDeferredLightingPipeline =
    { EyeUniform : Nu.Vulkan.Buffer
      Lighting2Uniform : Nu.Vulkan.Buffer
      LightUniform : Nu.Vulkan.Buffer
      ShadowMatrixUniform : Nu.Vulkan.Buffer
      Pipeline : Pipeline }

/// Describes the light mapping pass of a deferred physically-based pipeline that's loaded into GPU.
type PhysicallyBasedDeferredLightMappingPipeline =
    { EyeUniform : Nu.Vulkan.Buffer
      LightMapsUniform : Nu.Vulkan.Buffer
      LightsGeneralUniform : Nu.Vulkan.Buffer
      Pipeline : Pipeline }

/// Describes the ambient pass of a deferred physically-based pipeline that's loaded into GPU.
type PhysicallyBasedDeferredAmbientPipeline =
    { EyeUniform : Nu.Vulkan.Buffer
      LightMapUniform : Nu.Vulkan.Buffer
      LightMapsUniform : Nu.Vulkan.Buffer
      Pipeline : Pipeline }

/// Describes the irradiance pass of a deferred physically-based pipeline that's loaded into GPU.
type PhysicallyBasedDeferredIrradiancePipeline =
    { EyeUniform : Nu.Vulkan.Buffer
      Pipeline : Pipeline }

/// Describes the environment filter pass of a deferred physically-based pipeline that's loaded into GPU.
type PhysicallyBasedDeferredEnvironmentFilterPipeline =
    { EyeUniform : Nu.Vulkan.Buffer
      LightMapsUniform : Nu.Vulkan.Buffer
      Pipeline : Pipeline }

/// Describes the fogging pass of a deferred physically-based pipeline that's loaded into GPU.
type PhysicallyBasedDeferredFoggingPipeline =
    { EyeUniform : Nu.Vulkan.Buffer
      LightingUniform : Nu.Vulkan.Buffer
      LightsGeneralUniform : Nu.Vulkan.Buffer
      LightsUniform : Nu.Vulkan.Buffer
      ShadowMatricesUniform : Nu.Vulkan.Buffer
      Pipeline : Pipeline }

/// Describes the coloring pass of a deferred physically-based pipeline that's loaded into GPU.
type PhysicallyBasedDeferredColoringPipeline =
    { EyeUniform : Nu.Vulkan.Buffer
      LightingUniform : Nu.Vulkan.Buffer
      Pipeline : Pipeline }

/// Describes the composition pass of a deferred physically-based pipeline that's loaded into GPU.
type PhysicallyBasedDeferredCompositionPipeline =
    { EyeUniform : Nu.Vulkan.Buffer
      LightingUniform : Nu.Vulkan.Buffer
      Pipeline : Pipeline }

/// Physically-based pipelines.
type PhysicallyBasedPipelines =
    { ShadowStaticPointPipeline : PhysicallyBasedShadowPipeline
      ShadowStaticSpotPipeline : PhysicallyBasedShadowPipeline
      ShadowStaticDirectionalPipeline : PhysicallyBasedShadowPipeline
      ShadowAnimatedPointPipeline : PhysicallyBasedShadowPipeline
      ShadowAnimatedSpotPipeline : PhysicallyBasedShadowPipeline
      ShadowAnimatedDirectionalPipeline : PhysicallyBasedShadowPipeline
      DeferredStaticPipeline : PhysicallyBasedPipeline
      DeferredStaticClippedPipeline : PhysicallyBasedPipeline
      DeferredAnimatedPipeline : PhysicallyBasedPipeline
      DeferredLightingPipeline : PhysicallyBasedDeferredLightingPipeline
      DeferredLightMappingPipeline : PhysicallyBasedDeferredLightMappingPipeline
      DeferredAmbientPipeline : PhysicallyBasedDeferredAmbientPipeline
      DeferredIrradiancePipeline : PhysicallyBasedDeferredIrradiancePipeline
      DeferredEnvironmentFilterPipeline : PhysicallyBasedDeferredEnvironmentFilterPipeline
      DeferredFoggingPipeline : PhysicallyBasedDeferredFoggingPipeline
      DeferredColoringPipeline : PhysicallyBasedDeferredColoringPipeline
      DeferredCompositionPipeline : PhysicallyBasedDeferredCompositionPipeline
      ForwardStaticPipeline : PhysicallyBasedPipeline
      ForwardAnimatedPipeline : PhysicallyBasedPipeline }

[<RequireQualifiedAccess>]
module PhysicallyBased =
    
    let StaticTexCoordsOffset =     (3 (*position*)) * sizeof<single>
    let StaticNormalOffset =        (3 (*position*) + 2 (*tex coords*)) * sizeof<single>
    let StaticVertexSize =          (3 (*position*) + 2 (*tex coords*) + 3 (*normal*)) * sizeof<single>
    
    let AnimatedTexCoordsOffset =   (3 (*position*)) * sizeof<single>
    let AnimatedNormalOffset =      (3 (*position*) + 2 (*tex coords*)) * sizeof<single>
    let AnimatedBoneIdsOffset =     (3 (*position*) + 2 (*tex coords*) + 3 (*normal*)) * sizeof<single>
    let AnimatedWeightsOffset =     (3 (*position*) + 2 (*tex coords*) + 3 (*normal*) + 4 (*boneIds*)) * sizeof<single>
    let AnimatedVertexSize =        (3 (*position*) + 2 (*tex coords*) + 3 (*normal*) + 4 (*boneIds*) + 4 (*weights*)) * sizeof<single>

    /// Create a mesh for a physically-based quad.
    let createPhysicallyBasedQuadMesh () =

        // make vertex data
        let vertexData =
            [|
                (*   positions   *)         (* tex coords *)    (*    normals    *)
                -1.0f; -1.0f; +0.0f;        0.0f; 1.0f;          0.0f;  0.0f;  1.0f // bottom-left
                +1.0f; -1.0f; +0.0f;        1.0f; 1.0f;          0.0f;  0.0f;  1.0f // bottom-right
                +1.0f; +1.0f; +0.0f;        1.0f; 0.0f;          0.0f;  0.0f;  1.0f // top-right
                +1.0f; +1.0f; +0.0f;        1.0f; 0.0f;          0.0f;  0.0f;  1.0f // top-right
                -1.0f; +1.0f; +0.0f;        0.0f; 0.0f;          0.0f;  0.0f;  1.0f // top-left
                -1.0f; -1.0f; +0.0f;        0.0f; 1.0f;          0.0f;  0.0f;  1.0f // bottom-left
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

    /// Create a mesh for a physically-based cube.
    let createPhysicallyBasedCubeMesh () =

        // make vertex data
        let vertexData =
            [|
                (*   positions   *)         (* tex coords *)    (*    normals    *)

                // back face
                -0.5f; -0.5f; -0.5f;        0.0f; 1.0f;          0.0f;  0.0f; -1.0f // bottom-left
                +0.5f; +0.5f; -0.5f;        1.0f; 0.0f;          0.0f;  0.0f; -1.0f // top-right
                +0.5f; -0.5f; -0.5f;        1.0f; 1.0f;          0.0f;  0.0f; -1.0f // bottom-right         
                +0.5f; +0.5f; -0.5f;        1.0f; 0.0f;          0.0f;  0.0f; -1.0f // top-right
                -0.5f; -0.5f; -0.5f;        0.0f; 1.0f;          0.0f;  0.0f; -1.0f // bottom-left
                -0.5f; +0.5f; -0.5f;        0.0f; 0.0f;          0.0f;  0.0f; -1.0f // top-left

                // front face
                -0.5f; -0.5f; +0.5f;        0.0f; 1.0f;          0.0f;  0.0f; +1.0f // bottom-left
                +0.5f; -0.5f; +0.5f;        1.0f; 1.0f;          0.0f;  0.0f; +1.0f // bottom-right
                +0.5f; +0.5f; +0.5f;        1.0f; 0.0f;          0.0f;  0.0f; +1.0f // top-right
                +0.5f; +0.5f; +0.5f;        1.0f; 0.0f;          0.0f;  0.0f; +1.0f // top-right
                -0.5f; +0.5f; +0.5f;        0.0f; 0.0f;          0.0f;  0.0f; +1.0f // top-left
                -0.5f; -0.5f; +0.5f;        0.0f; 1.0f;          0.0f;  0.0f; +1.0f // bottom-left

                // left face
                -0.5f; +0.5f; +0.5f;        1.0f; 1.0f;         -1.0f;  0.0f;  0.0f // top-right
                -0.5f; +0.5f; -0.5f;        1.0f; 0.0f;         -1.0f;  0.0f;  0.0f // top-left
                -0.5f; -0.5f; -0.5f;        0.0f; 0.0f;         -1.0f;  0.0f;  0.0f // bottom-left
                -0.5f; -0.5f; -0.5f;        0.0f; 0.0f;         -1.0f;  0.0f;  0.0f // bottom-left
                -0.5f; -0.5f; +0.5f;        0.0f; 1.0f;         -1.0f;  0.0f;  0.0f // bottom-right
                -0.5f; +0.5f; +0.5f;        1.0f; 1.0f;         -1.0f;  0.0f;  0.0f // top-right

                // right face
                +0.5f; +0.5f; +0.5f;        1.0f; 1.0f;         +1.0f;  0.0f;  0.0f // top-left
                +0.5f; -0.5f; -0.5f;        0.0f; 0.0f;         +1.0f;  0.0f;  0.0f // bottom-right
                +0.5f; +0.5f; -0.5f;        1.0f; 0.0f;         +1.0f;  0.0f;  0.0f // top-right         
                +0.5f; -0.5f; -0.5f;        0.0f; 0.0f;         +1.0f;  0.0f;  0.0f // bottom-right
                +0.5f; +0.5f; +0.5f;        1.0f; 1.0f;         +1.0f;  0.0f;  0.0f // top-left
                +0.5f; -0.5f; +0.5f;        0.0f; 1.0f;         +1.0f;  0.0f;  0.0f // bottom-left

                // bottom face
                -0.5f; -0.5f; -0.5f;        0.0f; 0.0f;          0.0f; -1.0f;  0.0f // top-right
                +0.5f; -0.5f; -0.5f;        1.0f; 0.0f;          0.0f; -1.0f;  0.0f // top-left
                +0.5f; -0.5f; +0.5f;        1.0f; 1.0f;          0.0f; -1.0f;  0.0f // bottom-left
                +0.5f; -0.5f; +0.5f;        1.0f; 1.0f;          0.0f; -1.0f;  0.0f // bottom-left
                -0.5f; -0.5f; +0.5f;        0.0f; 1.0f;          0.0f; -1.0f;  0.0f // bottom-right
                -0.5f; -0.5f; -0.5f;        0.0f; 0.0f;          0.0f; -1.0f;  0.0f // top-right

                // top face
                -0.5f; +0.5f; -0.5f;        0.0f; 0.0f;          0.0f; +1.0f;  0.0f // top-left
                +0.5f; +0.5f; +0.5f;        1.0f; 1.0f;          0.0f; +1.0f;  0.0f // bottom-right
                +0.5f; +0.5f; -0.5f;        1.0f; 0.0f;          0.0f; +1.0f;  0.0f // top-right     
                +0.5f; +0.5f; +0.5f;        1.0f; 1.0f;          0.0f; +1.0f;  0.0f // bottom-right
                -0.5f; +0.5f; -0.5f;        0.0f; 0.0f;          0.0f; +1.0f;  0.0f // top-left
                -0.5f; +0.5f; +0.5f;        0.0f; 1.0f;          0.0f; +1.0f;  0.0f // bottom-left
            |]

        // make index data trivially
        let indexData = Array.init 36 id

        // make bounds trivially
        let bounds = box3 (v3Dup -0.5f) v3One

        // fin
        (vertexData, indexData, bounds)

    /// Create the attachments required for physically-based rendering.
    let createPhysicallyBasedAttachments (geometryViewport : Viewport) vkc =

        // create shadow texture array attachments
        let shadowTextureArrayAttachments =
            let shadowResolution = geometryViewport.ShadowTextureResolution
            Attachment.createShadowTextureArrayAttachments shadowResolution.X shadowResolution.Y Constants.Render.ShadowTexturesMax vkc

        // create shadow map attachments array
        let shadowMapAttachmentsArray =
            [|for _ in 0 .. dec Constants.Render.ShadowMapsMax do
                let shadowResolution = geometryViewport.ShadowMapResolution
                Attachment.createShadowMapAttachments shadowResolution.X shadowResolution.Y vkc|]

        // create shadow cascade array attachments array
        let shadowCascadeArrayAttachmentsArray =
            [|for _ in 0 .. dec Constants.Render.ShadowCascadesMax do
                let shadowResolution = geometryViewport.ShadowCascadeResolution
                Attachment.createShadowCascadeArrayAttachments shadowResolution.X shadowResolution.Y Constants.Render.ShadowCascadeLevels vkc|]

        // create geometry attachments
        let geometryAttachments = Attachment.createGeometryAttachments geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y vkc

        // create lighting attachment
        let lightingAttachment = Attachment.createLightingAttachment geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y vkc

        // create light mapping attachment
        let lightMappingAttachment = Attachment.createLightMappingAttachment geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y vkc

        // create ambient attachment
        let ambientAttachment = Attachment.createAmbientAttachment geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y vkc

        // create irradiance attachment
        let irradianceAttachment = Attachment.createIrradianceAttachment geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y vkc

        // create environment filter attachment
        let environmentfilterAttachment = Attachment.createEnvironmentFilterAttachment geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y vkc

        // create fogging attachment
        let foggingAttachment = Attachment.createFoggingAttachment geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y vkc

        // create coloring attachments
        let coloringAttachments = Attachment.createColoringAttachments geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y vkc

        // create composition attachments
        let compositionAttachment = Attachment.createCompositionAttachments geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y vkc

        // make record
        { ShadowTextureArrayAttachments = shadowTextureArrayAttachments
          ShadowMapAttachmentsArray = shadowMapAttachmentsArray
          ShadowCascadeArrayAttachmentsArray = shadowCascadeArrayAttachmentsArray
          GeometryAttachments = geometryAttachments
          LightingAttachment = lightingAttachment
          LightMappingAttachment = lightMappingAttachment
          AmbientAttachment = ambientAttachment
          IrradianceAttachment = irradianceAttachment
          EnvironmentFilterAttachment = environmentfilterAttachment
          FoggingAttachment = foggingAttachment
          ColoringAttachments = coloringAttachments
          CompositionAttachment = compositionAttachment }

    /// Update the size of the attachments. Must be used every frame.
    let updatePhysicallyBasedAttachmentsSize (geometryViewport : Viewport) (attachments : PhysicallyBasedAttachments) vkc =
        Attachment.updateShadowTextureArrayAttachmentsSize geometryViewport.ShadowTextureResolution.X geometryViewport.ShadowTextureResolution.Y attachments.ShadowTextureArrayAttachments vkc
        for i in 0 .. dec attachments.ShadowMapAttachmentsArray.Length do
            Attachment.updateShadowMapAttachmentsSize geometryViewport.ShadowMapResolution.X geometryViewport.ShadowMapResolution.Y attachments.ShadowMapAttachmentsArray[i] vkc
        for i in 0 .. dec attachments.ShadowCascadeArrayAttachmentsArray.Length do
            Attachment.updateShadowCascadeArrayAttachmentsSize geometryViewport.ShadowCascadeResolution.X geometryViewport.ShadowCascadeResolution.Y attachments.ShadowCascadeArrayAttachmentsArray[i] vkc
        Attachment.updateGeometryAttachmentsSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.GeometryAttachments vkc
        Attachment.updateLightingAttachmentSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.LightingAttachment vkc
        Attachment.updateLightMappingAttachmentSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.LightMappingAttachment vkc
        Attachment.updateAmbientAttachmentSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.AmbientAttachment vkc
        Attachment.updateIrradianceAttachmentSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.IrradianceAttachment vkc
        Attachment.updateEnvironmentFilterAttachmentSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.EnvironmentFilterAttachment vkc
        Attachment.updateFoggingAttachmentSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.FoggingAttachment vkc
        Attachment.updateColoringAttachmentsSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.ColoringAttachments vkc
        Attachment.updateCompositionAttachmentSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.CompositionAttachment vkc

    /// Destroy the physically-based attachments.
    let destroyPhysicallyBasedAttachments (attachments : PhysicallyBasedAttachments) vkc =
        Attachment.destroyShadowTextureArrayAttachments attachments.ShadowTextureArrayAttachments vkc
        for i in 0 .. dec attachments.ShadowMapAttachmentsArray.Length do
            Attachment.destroyShadowMapAttachments attachments.ShadowMapAttachmentsArray[i] vkc
        for i in 0 .. dec attachments.ShadowCascadeArrayAttachmentsArray.Length do
            Attachment.destroyShadowCascadeArrayAttachments attachments.ShadowCascadeArrayAttachmentsArray[i] vkc
        Attachment.destroyGeometryAttachments attachments.GeometryAttachments vkc
        Attachment.destroyLightingAttachment attachments.LightingAttachment vkc
        Attachment.destroyLightMappingAttachment attachments.LightMappingAttachment vkc
        Attachment.destroyAmbientAttachment attachments.AmbientAttachment vkc
        Attachment.destroyIrradianceAttachment attachments.IrradianceAttachment vkc
        Attachment.destroyEnvironmentFilterAttachment attachments.EnvironmentFilterAttachment vkc
        Attachment.destroyFoggingAttachment attachments.FoggingAttachment vkc
        Attachment.destroyColoringAttachments attachments.ColoringAttachments vkc
        Attachment.destroyCompositionAttachment attachments.CompositionAttachment vkc

    /// Create physically-based material from an assimp mesh, falling back on defaults in case of missing textures.
    /// Uses file name-based inferences to look for texture files in case the ones that were hard-coded in the model
    /// files can't be located.
    /// Thread-safe if vkcOpt = None.
    let createPhysicallyBasedMaterial dirPath defaultMaterial (textureClient : TextureClient) (material : Assimp.Material) vkcOpt =

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
            match vkcOpt with
            | Some vkc ->
                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression albedoTextureSlotFilePath) (dirPrefix + albedoTextureSlotFilePath) RenderThread vkc with
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
            match vkcOpt with
            | Some vkc ->
                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression roughnessTextureSlot.FilePath) (dirPrefix + roughnessTextureSlot.FilePath) RenderThread vkc with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression gTextureFilePath) (dirPrefix + gTextureFilePath) RenderThread vkc with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression sTextureFilePath) (dirPrefix + sTextureFilePath) RenderThread vkc with
                        | Right texture -> texture
                        | Left _ ->
                            match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression g_mTextureFilePath) (dirPrefix + g_mTextureFilePath) RenderThread vkc with
                            | Right texture -> texture
                            | Left _ ->
                                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression g_m_aoTextureFilePath) (dirPrefix + g_m_aoTextureFilePath) RenderThread vkc with
                                | Right texture -> texture
                                | Left _ ->
                                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression roughnessTextureFilePath) (dirPrefix + roughnessTextureFilePath) RenderThread vkc with
                                    | Right texture -> texture
                                    | Left _ ->
                                        match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression rmTextureFilePath) (dirPrefix + rmTextureFilePath) RenderThread vkc with
                                        | Right texture -> texture
                                        | Left _ ->
                                            match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression rmaTextureFilePath) (dirPrefix + rmaTextureFilePath) RenderThread vkc with
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
            match vkcOpt with
            | Some vkc ->
                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression metallicTextureSlot.FilePath) (dirPrefix + metallicTextureSlot.FilePath) RenderThread vkc with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression mTextureFilePath) (dirPrefix + mTextureFilePath) RenderThread vkc with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression g_mTextureFilePath) (dirPrefix + g_mTextureFilePath) RenderThread vkc with
                        | Right texture -> texture
                        | Left _ ->
                            match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression g_m_aoTextureFilePath) (dirPrefix + g_m_aoTextureFilePath) RenderThread vkc with
                            | Right texture -> texture
                            | Left _ ->
                                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression metallicTextureFilePath) (dirPrefix + metallicTextureFilePath) RenderThread vkc with
                                | Right texture -> texture
                                | Left _ ->
                                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression metalnessTextureFilePath) (dirPrefix + metalnessTextureFilePath) RenderThread vkc with
                                    | Right texture -> texture
                                    | Left _ ->
                                        match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression rmTextureFilePath) (dirPrefix + rmTextureFilePath) RenderThread vkc with
                                        | Right texture -> texture
                                        | Left _ ->
                                            match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression rmaTextureFilePath) (dirPrefix + rmaTextureFilePath) RenderThread vkc with
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
            match vkcOpt with
            | Some vkc ->
                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression ambientOcclusionTextureSlotFilePath) (dirPrefix + ambientOcclusionTextureSlotFilePath) RenderThread vkc with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression aoTextureFilePath) (dirPrefix + aoTextureFilePath) RenderThread vkc with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression g_m_aoTextureFilePath) (dirPrefix + g_m_aoTextureFilePath) RenderThread vkc with
                        | Right texture -> texture
                        | Left _ ->
                            match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression ambientOcclusionTextureFilePath) (dirPrefix + ambientOcclusionTextureFilePath) RenderThread vkc with
                            | Right texture -> texture
                            | Left _ ->
                                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression occlusionTextureFilePath) (dirPrefix + occlusionTextureFilePath) RenderThread vkc with
                                | Right texture -> texture
                                | Left _ ->
                                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression aoTextureFilePath') (dirPrefix + aoTextureFilePath') RenderThread vkc with
                                    | Right texture -> texture
                                    | Left _ ->
                                        match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression rmaTextureFilePath) (dirPrefix + rmaTextureFilePath) RenderThread vkc with
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
            match vkcOpt with
            | Some vkc ->
                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression emissionTextureSlot.FilePath) (dirPrefix + emissionTextureSlot.FilePath) RenderThread vkc with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression eTextureFilePath) (dirPrefix + eTextureFilePath) RenderThread vkc with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression emissiveTextureFilePath) (dirPrefix + emissiveTextureFilePath) RenderThread vkc with
                        | Right texture -> texture
                        | Left _ ->
                            match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression emissionTextureFilePath) (dirPrefix + emissionTextureFilePath) RenderThread vkc with
                            | Right texture -> texture
                            | Left _ -> defaultMaterial.EmissionTexture
            | None -> defaultMaterial.EmissionTexture

        // attempt to load normal info
        let mutable (_, normalTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Normals, 0)
        if isNull normalTextureSlot.FilePath
        then normalTextureSlot.FilePath <- "" // ensure not null
        else normalTextureSlot.FilePath <- PathF.Normalize normalTextureSlot.FilePath
        let normalTexture =
            match vkcOpt with
            | Some vkc ->
                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression normalTextureSlot.FilePath) (dirPrefix + normalTextureSlot.FilePath) RenderThread vkc with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression nTextureFilePath) (dirPrefix + nTextureFilePath) RenderThread vkc with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression normalTextureFilePath) (dirPrefix + normalTextureFilePath) RenderThread vkc with
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
            match vkcOpt with
            | Some vkc ->
                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression heightTextureSlot.FilePath) (dirPrefix + heightTextureSlot.FilePath) RenderThread vkc with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression hTextureFilePath) (dirPrefix + hTextureFilePath) RenderThread vkc with
                    | Right texture -> texture
                    | Left _ ->
                        match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression heightTextureFilePath) (dirPrefix + heightTextureFilePath) RenderThread vkc with
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
            match vkcOpt with
            | Some vkc ->
                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression subdermalTextureFilePath) (dirPrefix + subdermalTextureFilePath) RenderThread vkc with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression subdermalTextureFilePath') (dirPrefix + subdermalTextureFilePath') RenderThread vkc with
                    | Right texture -> texture
                    | Left _ -> defaultMaterial.SubdermalTexture
            | None -> defaultMaterial.SubdermalTexture

        // attempt to load fineness info
        let finenessOffset = Constants.Render.FinenessOffsetDefault
        let finenessTexture =
            match vkcOpt with
            | Some vkc ->
                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression finenessTextureFilePath) (dirPrefix + finenessTextureFilePath) RenderThread vkc with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression finenessTextureFilePath') (dirPrefix + finenessTextureFilePath') RenderThread vkc with
                    | Right texture -> texture
                    | Left _ -> defaultMaterial.FinenessTexture
            | None -> defaultMaterial.FinenessTexture

        // attempt to load scatter info
        let scatterType = Constants.Render.ScatterTypeDefault
        let scatterTexture =
            match vkcOpt with
            | Some vkc ->
                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression scatterTextureFilePath) (dirPrefix + scatterTextureFilePath) RenderThread vkc with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression scatterTextureFilePath') (dirPrefix + scatterTextureFilePath') RenderThread vkc with
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
            match vkcOpt with
            | Some vkc ->
                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression clearCoatTextureFilePath) (dirPrefix + clearCoatTextureFilePath) RenderThread vkc with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression clearCoatTextureFilePath') (dirPrefix + clearCoatTextureFilePath') RenderThread vkc with
                    | Right texture -> texture
                    | Left _ -> defaultMaterial.ClearCoatTexture
            | None -> defaultMaterial.ClearCoatTexture

        // attempt to load clear coat roughness info
        let clearCoatRoughness = Constants.Render.ClearCoatRoughnessDefault
        let clearCoatRoughnessTexture =
            match vkcOpt with
            | Some vkc ->
                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression clearCoatRoughnessTextureFilePath) (dirPrefix + clearCoatRoughnessTextureFilePath) RenderThread vkc with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression clearCoatRoughnessTextureFilePath') (dirPrefix + clearCoatRoughnessTextureFilePath') RenderThread vkc with
                    | Right texture -> texture
                    | Left _ -> defaultMaterial.ClearCoatRoughnessTexture
            | None -> defaultMaterial.ClearCoatRoughnessTexture

        // attempt to load clear coat normal info
        let clearCoatNormalTexture =
            match vkcOpt with
            | Some vkc ->
                match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression clearCoatNormalTextureFilePath) (dirPrefix + clearCoatNormalTextureFilePath) RenderThread vkc with
                | Right texture -> texture
                | Left _ ->
                    match textureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression clearCoatNormalTextureFilePath') (dirPrefix + clearCoatNormalTextureFilePath') RenderThread vkc with
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
            match vkcOpt with
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
    /// Thread-safe if vkcOpt = None.
    let tryCreatePhysicallyBasedMaterials dirPath defaultMaterial textureClient (scene : Assimp.Scene) vkcOpt =
        let mutable errorOpt = None
        let propertiesAndMaterials = Array.zeroCreate scene.Materials.Count
        for i in 0 .. dec scene.Materials.Count do
            if Option.isNone errorOpt then
                let (properties, material) = createPhysicallyBasedMaterial dirPath defaultMaterial textureClient scene.Materials[i] vkcOpt
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
    let createPhysicallyBasedStaticGeometry primitiveTopology (vertexData : single Memory) (indexData : int Memory) bounds vkcOpt =

        // make buffers
        let (vertices, indices, vertexBuffer, instanceBuffer, indexBuffer) =

            // make renderable
            match vkcOpt with
            | Some vkc ->

                // create buffers
                let vertexBuffer = Buffer.createVertexStagedFromMemory vertexData vkc
                let instanceBuffer = Buffer.create (Constants.Render.InstanceFieldCount * sizeof<single>) (Vertex true) vkc
                let indexBuffer = Buffer.createIndexStagedFromMemory indexData vkc

                // prepare instance buffer
                let instanceData = Array.zeroCreate Constants.Render.InstanceFieldCount
                m4Identity.ToArray (instanceData, 0)
                Buffer.uploadArray instanceData instanceBuffer vkc
                
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
                (vertices, indices, Unchecked.defaultof<Nu.Vulkan.Buffer>, Unchecked.defaultof<Nu.Vulkan.Buffer>, Unchecked.defaultof<Nu.Vulkan.Buffer>)

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
    let createPhysicallyBasedQuadGeometry vkcOpt =
        let (vertexData, indexData, bounds) = createPhysicallyBasedQuadMesh ()
        createPhysicallyBasedStaticGeometry VkPrimitiveTopology.TriangleList (vertexData.AsMemory ()) (indexData.AsMemory ()) bounds vkcOpt

    /// Create physically-based particle geometry.
    let createPhysicallyBasedParticleGeometry vkcOpt =
        let (vertexData, indexData, bounds) = createPhysicallyBasedParticleMesh ()
        createPhysicallyBasedStaticGeometry VkPrimitiveTopology.TriangleList (vertexData.AsMemory ()) (indexData.AsMemory ()) bounds vkcOpt

    /// Create physically-based billboard geometry.
    let createPhysicallyBasedBillboardGeometry vkcOpt =
        let (vertexData, indexData, bounds) = createPhysicallyBasedBillboardMesh ()
        createPhysicallyBasedStaticGeometry VkPrimitiveTopology.TriangleList (vertexData.AsMemory ()) (indexData.AsMemory ()) bounds vkcOpt

    /// Create physically-based cube geometry.
    let createPhysicallyBasedCubeGeometry vkcOpt =
        let (vertexData, indexData, bounds) = createPhysicallyBasedCubeMesh ()
        createPhysicallyBasedStaticGeometry VkPrimitiveTopology.TriangleList (vertexData.AsMemory ()) (indexData.AsMemory ()) bounds vkcOpt

    /// Create physically-based static geometry from an assimp mesh.
    let createPhysicallyBasedStaticGeometryFromMesh indexData (mesh : Assimp.Mesh) vkcOpt =
        match createPhysicallyBasedStaticMesh indexData mesh with
        | (vertexData, indexData, bounds) -> createPhysicallyBasedStaticGeometry VkPrimitiveTopology.TriangleList (vertexData.AsMemory ()) (indexData.AsMemory ()) bounds vkcOpt

    /// Create physically-based animated geometry from a mesh.
    let createPhysicallyBasedAnimatedGeometry primitiveTopology (vertexData : single Memory) (indexData : int Memory) bounds vkcOpt =

        // make buffers
        let (vertices, indices, vertexBuffer, instanceBuffer, indexBuffer) =

            // make renderable
            match vkcOpt with
            | Some vkc ->

                // create buffers
                let vertexBuffer = Buffer.createVertexStagedFromMemory vertexData vkc
                let instanceBuffer = Buffer.create (Constants.Render.InstanceFieldCount * sizeof<single>) (Vertex true) vkc
                let indexBuffer = Buffer.createIndexStagedFromMemory indexData vkc

                // prepare instance buffer
                let instanceData = Array.zeroCreate Constants.Render.InstanceFieldCount
                m4Identity.ToArray (instanceData, 0)
                Buffer.uploadArray instanceData instanceBuffer vkc
                
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
                (vertices, indices, Unchecked.defaultof<Nu.Vulkan.Buffer>, Unchecked.defaultof<Nu.Vulkan.Buffer>, Unchecked.defaultof<Nu.Vulkan.Buffer>)

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
    let createPhysicallyBasedAnimatedGeometryFromMesh indexData (mesh : Assimp.Mesh) vkcOpt =
        match createPhysicallyBasedAnimatedMesh indexData mesh with
        | (vertexData, indexData, bounds) -> createPhysicallyBasedAnimatedGeometry VkPrimitiveTopology.TriangleList (vertexData.AsMemory ()) (indexData.AsMemory ()) bounds vkcOpt

    /// Create physically-based static geometries from an assimp scene.
    /// OPTIMIZATION: duplicate geometry is detected and deduplicated here, which does have some run-time cost.
    let createPhysicallyBasedStaticGeometries (scene : Assimp.Scene) vkcOpt =
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
                let geometry = createPhysicallyBasedStaticGeometryFromMesh indexData mesh vkcOpt
                match meshAndGeometryListOpt with
                | Some meshesAndGeometries -> meshesAndGeometries.Add (mesh, geometry)
                | None -> meshAndGeometryLists[(mesh.VertexCount, mesh.FaceCount, mesh.BoundingBox)] <- List [(mesh, geometry)]
                geometries.Add geometry
        geometries

    /// Create physically-based animated geometries from an assimp scene.
    /// TODO: consider deduplicating geometry like in createPhysicallyBasedStaticGeometries?
    let createPhysicallyBasedAnimatedGeometries (scene : Assimp.Scene) vkcOpt =
        let geometries = SList.make ()
        for i in 0 .. dec scene.Meshes.Count do
            let indexDataEntry = scene.Metadata["IndexData" + string i]
            let indexData = indexDataEntry.Data :?> int array
            let mesh = scene.Meshes[i]
            let geometry = createPhysicallyBasedAnimatedGeometryFromMesh indexData mesh vkcOpt
            geometries.Add geometry
        geometries

    /// Destroy physically-based geometry resources.
    let destroyPhysicallyBasedGeometry geometry vkc =
        Buffer.destroy geometry.VertexBuffer vkc
        Buffer.destroy geometry.InstanceBuffer vkc
        Buffer.destroy geometry.IndexBuffer vkc

    /// Destroy physically-based model resources.
    /// NOTE: models are created via a PhysicallyBasedSceneClient instance.
    let destroyPhysicallyBasedModel (model : PhysicallyBasedModel) vkc =
        for surface in model.Surfaces do
            destroyPhysicallyBasedGeometry surface.PhysicallyBasedGeometry vkc

    /// Create a physically-based shadow pipeline.
    let createPhysicallyBasedShadowPipeline shaderPath vertexBindings colorAttachmentFormats depthTestFormat vkc =

        // create set 0 uniform buffers
        let shadowVertUniform = Buffer.create sizeof<ShadowVert> Storage vkc
        let shadowFragUniform = Buffer.create sizeof<ShadowFrag> Storage vkc

        // create set 1 uniform buffers
        let boneUniform = Buffer.create (Constants.Render.BonesMax * sizeof<Matrix4x4>) Storage vkc

        // create pipeline
        let pipeline =
            Pipeline.create
                shaderPath [|VulkanUnblended|] [|false; true|] vertexBindings
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 StorageBuffer VertexStage 1
                      Pipeline.descriptor 1 StorageBuffer FragmentStage 1|]
                  Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 StorageBuffer VertexStage 1|]|]
                [||] colorAttachmentFormats (Some depthTestFormat)
                [|boneUniform; shadowVertUniform; shadowFragUniform|]
                vkc

        // make PhysicallyBasedDepthPipeline
        let physicallyBasedDepthPipeline =
            { ShadowVertUniform = shadowVertUniform
              BoneUniform = boneUniform
              ShadowFragUniform = shadowFragUniform
              Pipeline = pipeline }

        // fin
        physicallyBasedDepthPipeline

    /// Destroy PhysicallyBasedShadowPipeline.
    let destroyPhysicallyBasedShadowPipeline (physicallyBasedShadowPipeline : PhysicallyBasedShadowPipeline) vkc =
        Pipeline.destroy physicallyBasedShadowPipeline.Pipeline vkc

    /// Create a physically-based pipeline.
    let createPhysicallyBasedPipeline lightMapsMax lightsMax shaderPath blends cullModes vertexBindings colorAttachmentFormats depthTestOpt vkc =

        // create set 0 uniform buffers
        let eyeUniform = Buffer.create sizeof<Eye> Storage vkc
        let lightingUniform = Buffer.create sizeof<Lighting> Storage vkc

        // create set 2 uniform buffers
        let shadowMatrixMax = Constants.Render.ShadowTexturesMax + Constants.Render.ShadowCascadesMax * Constants.Render.ShadowCascadeLevels
        let boneUniform = Buffer.create (Constants.Render.BonesMax * sizeof<Matrix4x4>) Storage vkc
        let lightMapsUniform = Buffer.create (lightMapsMax * sizeof<LightMap>) Storage vkc
        let lightsGeneralUniform = Buffer.create sizeof<LightsGeneral> Storage vkc
        let lightsUniform = Buffer.create (lightsMax * sizeof<Light>) Storage vkc
        let shadowMatrixUniform = Buffer.create (shadowMatrixMax * sizeof<Matrix4x4>) Storage vkc

        // create pipeline
        let pipeline =
            Pipeline.create
                shaderPath blends cullModes vertexBindings
                
                // descriptor set 0: per render pass
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 StorageBuffer VertexFragmentStage 1 // eye
                      Pipeline.descriptor 1 StorageBuffer FragmentStage 1 // lighting
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
                    [|Pipeline.descriptor 0 StorageBuffer VertexStage 1 // bone
                      Pipeline.descriptor 1 StorageBuffer FragmentStage 1 // lightMap
                      Pipeline.descriptor 2 StorageBuffer FragmentStage 1 // lightsGeneral
                      Pipeline.descriptor 3 StorageBuffer FragmentStage 1 // light
                      Pipeline.descriptor 4 StorageBuffer FragmentStage 1 // shadowMatrix
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
                vkc

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
    let destroyPhysicallyBasedPipeline (physicallyBasedPipeline : PhysicallyBasedPipeline) vkc =
        Pipeline.destroy physicallyBasedPipeline.Pipeline vkc

    /// Begin drawing a batch of physically-based shadow surfaces.
    let beginPhysicallyBasedShadowSurfaces
        (eyeCenter : Vector3)
        (viewProjection : Matrix4x4)
        (lightShadowExponent : single)
        (resolution : Vector2i)
        (colorClearValueOpt : VkClearValue option)
        (colorAttachments : VkImageView array)
        (depthAttachment : Texture)
        (renderPassIndex : int)
        (pipeline : PhysicallyBasedShadowPipeline)
        (vkc : VulkanContext) =

        // set up render
        let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
        let mutable vkViewport = Hl.makeViewport true renderArea
        let mutable renderingInfo = Hl.makeRenderingInfo colorAttachments (Some depthAttachment.ImageView) renderArea colorClearValueOpt
        Vulkan.vkCmdBeginRendering (vkc.RenderCommandBuffer, asPointer &renderingInfo)
        Vulkan.vkCmdSetViewport (vkc.RenderCommandBuffer, 0u, 1u, asPointer &vkViewport)
        Vulkan.vkCmdSetScissor (vkc.RenderCommandBuffer, 0u, 1u, asPointer &renderArea)

        // specify uniforms
        let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 renderPassIndex pipeline.Pipeline vkc $ fun vkSet ->

            // specify shadow vert
            let shadowVert = ShadowVert (viewProjection = viewProjection)
            Buffer.uploadValue shadowVert pipeline.ShadowVertUniform vkc
            Pipeline.writeDescriptorStorageBuffer 0 0 pipeline.ShadowVertUniform vkSet vkc

            // specify shadow frag
            let shadowFrag = ShadowFrag (eyeCenter = eyeCenter, lightShadowExponent = lightShadowExponent)
            Buffer.uploadValue shadowFrag pipeline.ShadowFragUniform vkc
            Pipeline.writeDescriptorStorageBuffer 1 0 pipeline.ShadowFragUniform vkSet vkc

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
        (vkc : VulkanContext) =

        // ensure there is a surface to render to avoid potentially utilizing destroyed textures
        if surfacesCount > 0 then

            // only draw if required vkPipeline exists
            match Pipeline.tryGetVkPipeline VulkanUnblended (not material.TwoSided) pipeline.Pipeline with
            | Some vkPipeline ->

                // specify instancing
                use instanceFieldsPin = new ArrayPin<_> (instanceFields)
                Buffer.uploadData (Constants.Render.InstanceFieldCount * sizeof<single>) surfacesCount instanceFieldsPin.NativeInt geometry.InstanceBuffer vkc                    // specify dynamic

                let mutable dynamicDescriptorSet = Pipeline.specifyDescriptorSet 1 pipeline.Pipeline.DrawIndex pipeline.Pipeline vkc $ fun vkSet ->
                    use bonesPin = new ArrayPin<_> (bones)
                    Buffer.uploadData sizeof<Matrix4x4> (min bones.Length Constants.Render.BonesMax) bonesPin.NativeInt pipeline.BoneUniform vkc
                    Pipeline.writeDescriptorStorageBuffer 0 0 pipeline.BoneUniform vkSet vkc

                // set up pipeline
                Vulkan.vkCmdBindPipeline (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)
                Vulkan.vkCmdSetDepthTestEnable (vkc.RenderCommandBuffer, true)
                Vulkan.vkCmdSetDepthCompareOp (vkc.RenderCommandBuffer, VkCompareOp.LessOrEqual)

                // bind vertex and index buffers
                let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
                let vertexOffsets = [|0UL; 0UL|]
                use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
                use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
                Vulkan.vkCmdBindVertexBuffers (vkc.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
                Vulkan.vkCmdBindIndexBuffer (vkc.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

                // bind descriptor sets
                let mutable uniformDescriptorSet = uniformsDescriptorSet
                Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, asPointer &uniformDescriptorSet, 0u, nullPtr)
                Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, asPointer &dynamicDescriptorSet, 0u, nullPtr)

                // draw
                Vulkan.vkCmdDrawIndexed (vkc.RenderCommandBuffer, uint geometry.ElementCount, uint surfacesCount, 0u, 0, 0u)

                // advance instancing
                Buffer.advance geometry.InstanceBuffer

                // advance pipeline
                Pipeline.advance surfacesCount pipeline.Pipeline

            // abort
            | None -> Log.warnOnce "Cannot draw because VkPipeline does not exist."

    /// End the process of drawing physically-based shadow surfaces.
    let endPhysicallyBasedShadowSurfaces (_ : PhysicallyBasedShadowPipeline) (vkc : VulkanContext) =

        // tear down render
        Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer

    /// Begin drawing a batch of physically-based deferred surfaces.
    let beginPhysicallyBasedDeferredSurfaces
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (viewInverse : Matrix4x4)
        (projection : Matrix4x4)
        (projectionInverse : Matrix4x4)
        (viewProjection : Matrix4x4)
        (filteredSampler : Sampler)
        (colorAttachments : VkImageView array)
        (depthAttachment : Texture)
        (viewport : Viewport)
        (renderPassIndex : int)
        (pipeline : PhysicallyBasedPipeline)
        (vkc : VulkanContext) =

        // specify eye
        let mutable eyeDescriptorSet = Pipeline.specifyDescriptorSet 0 renderPassIndex pipeline.Pipeline vkc $ fun vkSet ->
            let eye = Eye (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
            Buffer.uploadValue eye pipeline.EyeUniform vkc
            Pipeline.writeDescriptorStorageBuffer 0 0 pipeline.EyeUniform vkSet vkc

        // specify samplers
        let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 3 Unit pipeline.Pipeline vkc $ fun vkSet ->
            Pipeline.writeDescriptorSampler 0 0 filteredSampler vkSet vkc
            
        // set up render
        let mutable renderArea = VkRect2D (0, 0, uint viewport.Bounds.Size.X, uint viewport.Bounds.Size.Y)
        let mutable vkViewport = Hl.makeViewport true renderArea
        let mutable renderingInfo = Hl.makeRenderingInfo colorAttachments (Some depthAttachment.ImageView) renderArea None
        Vulkan.vkCmdBeginRendering (vkc.RenderCommandBuffer, asPointer &renderingInfo)
        Vulkan.vkCmdSetViewport (vkc.RenderCommandBuffer, 0u, 1u, asPointer &vkViewport)
        Vulkan.vkCmdSetScissor (vkc.RenderCommandBuffer, 0u, 1u, asPointer &renderArea)

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
        (vkc : VulkanContext) =

        // only set up when there is a surface to render to avoid potentially utilizing destroyed textures
        if surfacesCount > 0 then

            // only draw if required vkPipeline exists
            match Pipeline.tryGetVkPipeline VulkanUnblended (not material.TwoSided) pipeline.Pipeline with
            | Some vkPipeline ->

                // specify instancing
                use instanceFieldsPin = new ArrayPin<_> (instanceFields)
                Buffer.uploadData (Constants.Render.InstanceFieldCount * sizeof<single>) surfacesCount instanceFieldsPin.NativeInt geometry.InstanceBuffer vkc

                // specify material
                let mutable materialDescriptorSet = Pipeline.specifyDescriptorSet 1 material pipeline.Pipeline vkc $ fun vkSet ->
                    Pipeline.writeDescriptorSampledImage 0 0 material.AlbedoTexture vkSet vkc
                    Pipeline.writeDescriptorSampledImage 1 0 material.RoughnessTexture vkSet vkc
                    Pipeline.writeDescriptorSampledImage 2 0 material.MetallicTexture vkSet vkc
                    Pipeline.writeDescriptorSampledImage 3 0 material.AmbientOcclusionTexture vkSet vkc
                    Pipeline.writeDescriptorSampledImage 4 0 material.EmissionTexture vkSet vkc
                    Pipeline.writeDescriptorSampledImage 5 0 material.NormalTexture vkSet vkc
                    Pipeline.writeDescriptorSampledImage 6 0 material.HeightTexture vkSet vkc
                    Pipeline.writeDescriptorSampledImage 7 0 material.SubdermalTexture vkSet vkc
                    Pipeline.writeDescriptorSampledImage 8 0 material.FinenessTexture vkSet vkc
                    Pipeline.writeDescriptorSampledImage 9 0 material.ScatterTexture vkSet vkc
                    Pipeline.writeDescriptorSampledImage 10 0 material.ClearCoatTexture vkSet vkc
                    Pipeline.writeDescriptorSampledImage 11 0 material.ClearCoatRoughnessTexture vkSet vkc
                    Pipeline.writeDescriptorSampledImage 12 0 material.ClearCoatNormalTexture vkSet vkc

                // specify dynamic
                let mutable dynamicDescriptorSet = Pipeline.specifyDescriptorSet 2 pipeline.Pipeline.DrawIndex pipeline.Pipeline vkc $ fun vkSet ->
                    use bonesPin = new ArrayPin<_> (bones)
                    Buffer.uploadData sizeof<Matrix4x4> (min bones.Length Constants.Render.BonesMax) bonesPin.NativeInt pipeline.BoneUniform vkc
                    Pipeline.writeDescriptorStorageBuffer 0 0 pipeline.BoneUniform vkSet vkc

                // set up pipeline
                Vulkan.vkCmdBindPipeline (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)
                Vulkan.vkCmdSetDepthTestEnable (vkc.RenderCommandBuffer, true)
                Vulkan.vkCmdSetDepthCompareOp (vkc.RenderCommandBuffer, VkCompareOp.LessOrEqual)

                // bind vertex and index buffers
                let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
                let vertexOffsets = [|0UL; 0UL|]
                use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
                use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
                Vulkan.vkCmdBindVertexBuffers (vkc.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
                Vulkan.vkCmdBindIndexBuffer (vkc.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

                // bind descriptor sets
                let mutable (eyeDescriptorSet, samplerDescriptorSet) = (eyeDescriptorSet, samplerDescriptorSet)
                Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, asPointer &eyeDescriptorSet, 0u, nullPtr)
                Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, asPointer &materialDescriptorSet, 0u, nullPtr)
                Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 2u, 1u, asPointer &dynamicDescriptorSet, 0u, nullPtr)
                Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 3u, 1u, asPointer &samplerDescriptorSet, 0u, nullPtr)

                // draw
                Vulkan.vkCmdDrawIndexed (vkc.RenderCommandBuffer, uint geometry.ElementCount, uint surfacesCount, 0u, 0, 0u)
                    
                // advance instancing
                Buffer.advance geometry.InstanceBuffer

                // advance pipeline
                Pipeline.advance surfacesCount pipeline.Pipeline

            // abort
            | None -> Log.warnOnce "Cannot draw because VkPipeline does not exist."

    /// End the process of drawing physically-based deferred surfaces.
    let endPhysicallyBasedDeferredSurfaces (_ : PhysicallyBasedPipeline) (vkc : VulkanContext) =

        // tear down render
        Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer

    /// Create a PhysicallyBasedDeferredLightingPipeline.
    let createPhysicallyBasedDeferredLightingPipeline colorAttachmentFormat vkc =

        // create uniform buffers
        let shadowMatrixMax = Constants.Render.ShadowTexturesMax + Constants.Render.ShadowCascadesMax * Constants.Render.ShadowCascadeLevels
        let eyeUniform = Buffer.create sizeof<Eye> Storage vkc
        let lightingUniform = Buffer.create sizeof<Lighting2> Storage vkc
        let lightUniform = Buffer.create (Constants.Render.LightsMaxDeferred * sizeof<Light>) Storage vkc
        let shadowMatrixUniform = Buffer.create (shadowMatrixMax * sizeof<Matrix4x4>) Storage vkc

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.PhysicallyBasedDeferredLightingShaderFilePath
                [|VulkanUnblended|] [|false|]
                [|Pipeline.vertex 0 StaticVertexSize VkVertexInputRate.Vertex
                    [|Pipeline.attribute 0 Single3 0
                      Pipeline.attribute 1 Single2 StaticTexCoordsOffset
                      Pipeline.attribute 2 Single3 StaticNormalOffset|]|]
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 StorageBuffer FragmentStage 1 // eye
                      Pipeline.descriptor 1 StorageBuffer FragmentStage 1 // lighting
                      Pipeline.descriptor 2 StorageBuffer FragmentStage 1 // light
                      Pipeline.descriptor 3 StorageBuffer FragmentStage 1 // shadowMatrix
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
                vkc

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
    let destroyPhysicallyBasedDeferredLightingPipeline (pipeline : PhysicallyBasedDeferredLightingPipeline) vkc =
        Pipeline.destroy pipeline.Pipeline vkc

    /// Draw the lighting pass of a deferred physically-based surface.
    let drawPhysicallyBasedDeferredLightingSurface
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (viewInverse : Matrix4x4)
        (projection : Matrix4x4)
        (projectionInverse : Matrix4x4)
        (viewProjection : Matrix4x4)
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
        (geometrySampler : Sampler)
        (shadowSampler : Sampler)
        (viewport : Viewport)
        (renderPassIndex : int)
        (geometry : PhysicallyBasedGeometry)
        (lightAccumAttachment : Texture)
        (pipeline : PhysicallyBasedDeferredLightingPipeline)
        (vkc : VulkanContext) =

        // only draw if required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 renderPassIndex pipeline.Pipeline vkc $ fun vkSet ->

                // specify eye
                let eye = Eye (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
                Buffer.uploadValue eye pipeline.EyeUniform vkc
                Pipeline.writeDescriptorStorageBuffer 0 0 pipeline.EyeUniform vkSet vkc

                // specify lighting
                let mutable lighting = Lighting2 ()
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
                Buffer.uploadValue lighting pipeline.Lighting2Uniform vkc
                Pipeline.writeDescriptorStorageBuffer 1 0 pipeline.Lighting2Uniform vkSet vkc

                // specify lights
                let mutable light = Light ()
                use lightPtr = fixed &light
                for i in 0 .. dec Constants.Render.LightsMaxDeferred do
                    if i < lightOrigins.Length then
                        light.lightOrigins <- lightOrigins[i]
                        light.lightDirections <- lightDirections[i]
                        light.lightColors <- lightColors[i].V3
                        light.lightBrightnesses <- lightBrightnesses[i]
                        light.lightAttenuationLinears <- lightAttenuationLinears[i]
                        light.lightAttenuationQuadratics <- lightAttenuationQuadratics[i]
                        light.lightCutoffs <- lightCutoffs[i]
                        light.lightTypes <- lightTypes[i]
                        light.lightConeInners <- lightConeInners[i]
                        light.lightConeOuters <- lightConeOuters[i]
                        light.lightDesireFogs <- lightDesireFogs[i]
                        light.lightShadowIndices <- lightShadowIndices[i]
                    else light <- Unchecked.defaultof<_>
                    Buffer.writeSubdata (i * sizeof<Light>) 0 sizeof<Light> 1 (NativePtr.toNativeInt lightPtr) pipeline.LightUniform vkc
                Buffer.flushSubdata 0 0 sizeof<Light> Constants.Render.LightsMaxDeferred pipeline.LightUniform vkc
                Pipeline.writeDescriptorStorageBuffer 2 0 pipeline.LightUniform vkSet vkc

                // specify shadow matrices
                use shadowMatricesPin = new ArrayPin<_> (shadowMatrices)
                let shadowMatricesCount = min shadowMatrices.Length (Constants.Render.ShadowTexturesMax + Constants.Render.ShadowCascadesMax * Constants.Render.ShadowCascadeLevels)
                Buffer.uploadData sizeof<Matrix4x4> shadowMatricesCount shadowMatricesPin.NativeInt pipeline.ShadowMatrixUniform vkc
                Pipeline.writeDescriptorStorageBuffer 3 0 pipeline.ShadowMatrixUniform vkSet vkc

                // specify textures
                Pipeline.writeDescriptorSampledImage 4 0 depthTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 5 0 albedoTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 6 0 materialTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 7 0 normalPlusTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 8 0 subdermalPlusTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 9 0 scatterPlusTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 10 0 clearCoatPlusTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 11 0 shadowTextureArray vkSet vkc
                Pipeline.writeDescriptorSampledImages 12 0 (Array.tryTake Constants.Render.ShadowMapsMax shadowMaps) vkSet vkc
                Pipeline.writeDescriptorSampledImages 13 0 (Array.tryTake Constants.Render.ShadowCascadesMax shadowCascades) vkSet vkc

            // specify samplers
            let mutable samplersDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline vkc $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 geometrySampler vkSet vkc
                Pipeline.writeDescriptorSampler 1 0 shadowSampler vkSet vkc

            // set up render
            let mutable renderArea = VkRect2D (0, 0, uint viewport.Bounds.Size.X, uint viewport.Bounds.Size.Y)
            let mutable vkViewport = Hl.makeViewport true renderArea
            let clearValue = VkClearValue (r = Constants.Render.ViewportClearColor.R, g = Constants.Render.ViewportClearColor.G, b = Constants.Render.ViewportClearColor.B, a = Constants.Render.ViewportClearColor.A)
            let mutable renderingInfo = Hl.makeRenderingInfo [|lightAccumAttachment.ImageView|] None renderArea (Some clearValue)
            Vulkan.vkCmdBeginRendering (vkc.RenderCommandBuffer, asPointer &renderingInfo)
            Vulkan.vkCmdSetViewport (vkc.RenderCommandBuffer, 0u, 1u, asPointer &vkViewport)
            Vulkan.vkCmdSetScissor (vkc.RenderCommandBuffer, 0u, 1u, asPointer &renderArea)
                
            // set up pipeline
            Vulkan.vkCmdBindPipeline (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            Vulkan.vkCmdBindVertexBuffers (vkc.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            Vulkan.vkCmdBindIndexBuffer (vkc.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            let mutable (uniformsDescriptorSet, samplersDescriptorSet) = (uniformsDescriptorSet, samplersDescriptorSet)
            Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, asPointer &uniformsDescriptorSet, 0u, nullPtr)
            Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, asPointer &samplersDescriptorSet, 0u, nullPtr)

            // draw
            Vulkan.vkCmdDrawIndexed (vkc.RenderCommandBuffer, uint geometry.ElementCount, uint 1, 0u, 0, 0u)

            // tear down render
            Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer

            // advance pipeline
            Pipeline.advance 1 pipeline.Pipeline

        // abort
        | None -> Log.warnOnce "Cannot draw because VkPipeline does not exist."

    /// Create a PhysicallyBasedDeferredLightMappingPipeline.
    let createPhysicallyBasedDeferredLightMappingPipeline colorAttachmentFormat vkc =

        // create uniform buffers
        let eyeUniform = Buffer.create sizeof<Eye> Storage vkc
        let lightMapsUniform = Buffer.create (Constants.Render.LightMapsMaxDeferred * sizeof<LightMap>) Storage vkc
        let lightsGeneralUniform = Buffer.create sizeof<LightsGeneral> Storage vkc

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.PhysicallyBasedDeferredLightMappingShaderFilePath
                [|VulkanUnblended|] [|false|]
                [|Pipeline.vertex 0 StaticVertexSize VkVertexInputRate.Vertex
                    [|Pipeline.attribute 0 Single3 0
                      Pipeline.attribute 1 Single2 StaticTexCoordsOffset
                      Pipeline.attribute 2 Single3 StaticNormalOffset|]|]
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 StorageBuffer FragmentStage 1 // eye
                      Pipeline.descriptor 1 StorageBuffer FragmentStage 1 // lightMaps
                      Pipeline.descriptor 2 StorageBuffer FragmentStage 1 // lightsGeneral
                      Pipeline.descriptor 3 SampledImage FragmentStage 1 // depth
                      Pipeline.descriptor 4 SampledImage FragmentStage 1|] // normalPlus
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|]
                [||] [|colorAttachmentFormat|] None
                [|eyeUniform; lightMapsUniform; lightsGeneralUniform|]
                vkc

        // make PhysicallyBasedDeferredLightingPipeline
        let physicallyBasedDeferredLightMappingPipeline =
            { EyeUniform = eyeUniform
              LightMapsUniform = lightMapsUniform
              LightsGeneralUniform = lightsGeneralUniform
              Pipeline = pipeline }

        // fin
        physicallyBasedDeferredLightMappingPipeline
    
    /// Destroy PhysicallyBasedDeferredLightMappingPipeline.
    let destroyPhysicallyBasedDeferredLightMappingPipeline (pipeline : PhysicallyBasedDeferredLightMappingPipeline) vkc =
        Pipeline.destroy pipeline.Pipeline vkc

    /// Draw the light mapping pass of a deferred physically-based surface.
    let drawPhysicallyBasedDeferredLightMappingSurface
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (viewInverse : Matrix4x4)
        (projection : Matrix4x4)
        (projectionInverse : Matrix4x4)
        (viewProjection : Matrix4x4)
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
        (colorSampler : Sampler)
        (colorAttachment : Texture)
        (viewport : Viewport)
        (renderPassIndex : int)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : PhysicallyBasedDeferredLightMappingPipeline)
        (vkc : VulkanContext) =

        // only draw if required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 renderPassIndex pipeline.Pipeline vkc $ fun vkSet ->

                // specify eye
                let eye = Eye (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
                Buffer.uploadValue eye pipeline.EyeUniform vkc
                Pipeline.writeDescriptorStorageBuffer 0 0 pipeline.EyeUniform vkSet vkc

                // specify light maps
                let mutable lightMap = LightMap' ()
                use lightMapPtr = fixed &lightMap
                for i in 0 .. dec Constants.Render.LightMapsMaxDeferred do
                    if i < lightMapOrigins.Length then
                        lightMap.lightMapOrigins <- lightMapOrigins[i]
                        lightMap.lightMapMins <- lightMapMins[i]
                        lightMap.lightMapSizes <- lightMapSizes[i]
                        lightMap.lightMapAmbientColors <- lightMapAmbientColors[i].V3
                        lightMap.lightMapAmbientBrightnesses <- lightMapAmbientBrightnesses[i]
                    else lightMap <- Unchecked.defaultof<_>
                    Buffer.writeSubdata (i * sizeof<LightMap>) 0 sizeof<LightMap> 1 (NativePtr.toNativeInt lightMapPtr) pipeline.LightMapsUniform vkc
                Buffer.flushSubdata 0 0 sizeof<LightMap> Constants.Render.LightMapsMaxDeferred pipeline.LightMapsUniform vkc
                Pipeline.writeDescriptorStorageBuffer 1 0 pipeline.LightMapsUniform vkSet vkc

                // specify lights general
                let mutable lightsGeneral = LightsGeneral ()
                lightsGeneral.lightMapsCount <- lightMapsCount
                lightsGeneral.lightMapSingletonBlendMargin <- lightMapSingletonBlendMargin
                lightsGeneral.lightsCount <- lightsCount
                Buffer.uploadValue lightsGeneral pipeline.LightsGeneralUniform vkc
                Pipeline.writeDescriptorStorageBuffer 2 0 pipeline.LightsGeneralUniform vkSet vkc

                // specify static environment textures
                Pipeline.writeDescriptorSampledImage 3 0 depthTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 4 0 normalPlusTexture vkSet vkc

            // specify samplers
            let mutable samplersDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline vkc $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 colorSampler vkSet vkc

            // set up render
            let mutable renderArea = VkRect2D (0, 0, uint viewport.Bounds.Size.X, uint viewport.Bounds.Size.Y)
            let mutable vkViewport = Hl.makeViewport true renderArea
            let clearValue = VkClearValue (r = Constants.Render.ViewportClearColor.R, g = Constants.Render.ViewportClearColor.G, b = Constants.Render.ViewportClearColor.B, a = Constants.Render.ViewportClearColor.A)
            let mutable renderingInfo = Hl.makeRenderingInfo [|colorAttachment.ImageView|] None renderArea (Some clearValue)
            Vulkan.vkCmdBeginRendering (vkc.RenderCommandBuffer, asPointer &renderingInfo)
            Vulkan.vkCmdSetViewport (vkc.RenderCommandBuffer, 0u, 1u, asPointer &vkViewport)
            Vulkan.vkCmdSetScissor (vkc.RenderCommandBuffer, 0u, 1u, asPointer &renderArea)

            // set up pipeline
            Vulkan.vkCmdBindPipeline (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            Vulkan.vkCmdBindVertexBuffers (vkc.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            Vulkan.vkCmdBindIndexBuffer (vkc.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, asPointer &uniformsDescriptorSet, 0u, nullPtr)
            Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, asPointer &samplersDescriptorSet, 0u, nullPtr)

            // draw
            Vulkan.vkCmdDrawIndexed (vkc.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // tear down render
            Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer

            // advance pipeline
            Pipeline.advance 1 pipeline.Pipeline

        // abort
        | None -> Log.warnOnce "Cannot draw because VkPipeline does not exist."

    /// Create a PhysicallyBasedDeferredAmbientPipeline.
    let createPhysicallyBasedDeferredAmbientPipeline colorAttachmentFormat vkc =

        // create uniform buffers
        let eyeUniform = Buffer.create sizeof<Eye> Storage vkc
        let lightMapUniform = Buffer.create sizeof<LightMap> Storage vkc
        let lightMapsUniform = Buffer.create (Constants.Render.LightMapsMaxDeferred * sizeof<LightMap>) Storage vkc

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.PhysicallyBasedDeferredAmbientShaderFilePath
                [|VulkanUnblended|] [|false|]
                [|Pipeline.vertex 0 StaticVertexSize VkVertexInputRate.Vertex
                    [|Pipeline.attribute 0 Single3 0
                      Pipeline.attribute 1 Single2 StaticTexCoordsOffset
                      Pipeline.attribute 2 Single3 StaticNormalOffset|]|]
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 StorageBuffer FragmentStage 1 // eye
                      Pipeline.descriptor 1 StorageBuffer FragmentStage 1 // lightMap
                      Pipeline.descriptor 2 StorageBuffer FragmentStage 1 // lightMaps
                      Pipeline.descriptor 3 SampledImage FragmentStage 1 // depth
                      Pipeline.descriptor 4 SampledImage FragmentStage 1|] // lightMapping
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|]
                [||] [|colorAttachmentFormat|] None
                [|eyeUniform; lightMapUniform; lightMapsUniform|]
                vkc

        // make PhysicallyBasedDeferredAmbientPipeline
        let physicallyBasedDeferredAmbientPipeline =
            { EyeUniform = eyeUniform
              LightMapUniform = lightMapUniform
              LightMapsUniform = lightMapsUniform
              Pipeline = pipeline }
        
        // fin
        physicallyBasedDeferredAmbientPipeline

    /// Destroy PhysicallyBasedDeferredAmbientPipeline.
    let destroyPhysicallyBasedDeferredAmbientPipeline (pipeline : PhysicallyBasedDeferredAmbientPipeline) vkc =
        Pipeline.destroy pipeline.Pipeline vkc

    /// Draw the ambient pass of a deferred physically-based surface.
    let drawPhysicallyBasedDeferredAmbientSurface
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (viewInverse : Matrix4x4)
        (projection : Matrix4x4)
        (projectionInverse : Matrix4x4)
        (viewProjection : Matrix4x4)
        (lightMapAmbientColor : Color)
        (lightMapAmbientBrightness : single)
        (lightMapAmbientColors : Color array)
        (lightMapAmbientBrightnesses : single array)
        (depthTexture : Texture)
        (lightMappingTexture : Texture)
        (colorSampler : Sampler)
        (colorAttachment : Texture)
        (viewport : Viewport)
        (renderPassIndex : int)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : PhysicallyBasedDeferredAmbientPipeline)
        (vkc : VulkanContext) =

        // only draw if required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 renderPassIndex pipeline.Pipeline vkc $ fun vkSet ->

                // specify eye
                let eye = Eye (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
                Buffer.uploadValue eye pipeline.EyeUniform vkc
                Pipeline.writeDescriptorStorageBuffer 0 0 pipeline.EyeUniform vkSet vkc

                // specify light map
                let mutable lightMap = LightMap' ()
                lightMap.lightMapAmbientColors <- lightMapAmbientColor.V3
                lightMap.lightMapAmbientBrightnesses <- lightMapAmbientBrightness
                Buffer.uploadValue lightMap pipeline.LightMapsUniform vkc
                Buffer.flushSubdata 0 0 sizeof<LightMap> Constants.Render.LightMapsMaxDeferred pipeline.LightMapsUniform vkc
                Pipeline.writeDescriptorStorageBuffer 1 0 pipeline.LightMapsUniform vkSet vkc

                // specify light maps
                use lightMapPtr = fixed &lightMap
                for i in 0 .. dec Constants.Render.LightMapsMaxDeferred do
                    if i < lightMapAmbientColors.Length then
                        lightMap.lightMapAmbientColors <- lightMapAmbientColors[i].V3
                        lightMap.lightMapAmbientBrightnesses <- lightMapAmbientBrightnesses[i]
                    else lightMap <- Unchecked.defaultof<_>
                    Buffer.writeSubdata (i * sizeof<LightMap>) 0 sizeof<LightMap> 1 (NativePtr.toNativeInt lightMapPtr) pipeline.LightMapsUniform vkc
                Buffer.flushSubdata 0 0 sizeof<LightMap> Constants.Render.LightMapsMaxDeferred pipeline.LightMapsUniform vkc
                Pipeline.writeDescriptorStorageBuffer 2 0 pipeline.LightMapsUniform vkSet vkc

                // specify static environment textures
                Pipeline.writeDescriptorSampledImage 3 0 depthTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 4 0 lightMappingTexture vkSet vkc

            // specify samplers
            let mutable samplersDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline vkc $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 colorSampler vkSet vkc

            // set up render
            let mutable renderArea = VkRect2D (0, 0, uint viewport.Bounds.Size.X, uint viewport.Bounds.Size.Y)
            let mutable vkViewport = Hl.makeViewport true renderArea
            let clearValue = VkClearValue (r = Constants.Render.ViewportClearColor.R, g = Constants.Render.ViewportClearColor.G, b = Constants.Render.ViewportClearColor.B, a = Constants.Render.ViewportClearColor.A)
            let mutable renderingInfo = Hl.makeRenderingInfo [|colorAttachment.ImageView|] None renderArea (Some clearValue)
            Vulkan.vkCmdBeginRendering (vkc.RenderCommandBuffer, asPointer &renderingInfo)
            Vulkan.vkCmdSetViewport (vkc.RenderCommandBuffer, 0u, 1u, asPointer &vkViewport)
            Vulkan.vkCmdSetScissor (vkc.RenderCommandBuffer, 0u, 1u, asPointer &renderArea)

            // set up pipeline
            Vulkan.vkCmdBindPipeline (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            Vulkan.vkCmdBindVertexBuffers (vkc.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            Vulkan.vkCmdBindIndexBuffer (vkc.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, asPointer &uniformsDescriptorSet, 0u, nullPtr)
            Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, asPointer &samplersDescriptorSet, 0u, nullPtr)

            // draw
            Vulkan.vkCmdDrawIndexed (vkc.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // tear down render
            Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer

            // advance pipeline
            Pipeline.advance 1 pipeline.Pipeline

        // abort
        | None -> Log.warnOnce "Cannot draw because VkPipeline does not exist."

    /// Create a PhysicallyBasedDeferredIrradiancePipeline.
    let createPhysicallyBasedDeferredIrradiancePipeline colorAttachmentFormat vkc =

        // create uniform buffers
        let eyeUniform = Buffer.create sizeof<Eye> Storage vkc

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.PhysicallyBasedDeferredIrradianceShaderFilePath
                [|VulkanUnblended|] [|false|]
                [|Pipeline.vertex 0 StaticVertexSize VkVertexInputRate.Vertex
                    [|Pipeline.attribute 0 Single3 0
                      Pipeline.attribute 1 Single2 StaticTexCoordsOffset
                      Pipeline.attribute 2 Single3 StaticNormalOffset|]|]
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 StorageBuffer FragmentStage 1 // eye
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
                vkc

        // make PhysicallyBasedDeferredIrradiancePipeline
        let physicallyBasedDeferredIrradiancePipeline =
            { PhysicallyBasedDeferredIrradiancePipeline.EyeUniform = eyeUniform
              Pipeline = pipeline }

        // fin
        physicallyBasedDeferredIrradiancePipeline

    /// Destroy PhysicallyBasedDeferredIrradiancePipeline.
    let destroyPhysicallyBasedDeferredIrradiancePipeline (pipeline : PhysicallyBasedDeferredIrradiancePipeline) vkc =
        Pipeline.destroy pipeline.Pipeline vkc

    /// Draw the irradiance pass of a deferred physically-based surface.
    let drawPhysicallyBasedDeferredIrradianceSurface
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (viewInverse : Matrix4x4)
        (projection : Matrix4x4)
        (projectionInverse : Matrix4x4)
        (viewProjection : Matrix4x4)
        (depthTexture : Texture)
        (normalPlusTexture : Texture)
        (lightMappingTexture : Texture)
        (irradianceMap : Texture)
        (irradianceMaps : Texture array)
        (colorSampler : Sampler)
        (irradianceSampler : Sampler)
        (colorAttachment : Texture)
        (viewport : Viewport)
        (renderPassIndex : int)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : PhysicallyBasedDeferredIrradiancePipeline)
        (vkc : VulkanContext) =

        // only draw if required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 renderPassIndex pipeline.Pipeline vkc $ fun vkSet ->

                // specify eye
                let eye = Eye (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
                Buffer.uploadValue eye pipeline.EyeUniform vkc
                Pipeline.writeDescriptorStorageBuffer 0 0 pipeline.EyeUniform vkSet vkc

                // specify static environment textures
                Pipeline.writeDescriptorSampledImage 1 0 depthTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 2 0 normalPlusTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 3 0 lightMappingTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 4 0 irradianceMap vkSet vkc
                Pipeline.writeDescriptorSampledImages 5 0 (Array.tryTake Constants.Render.LightMapsMaxDeferred irradianceMaps) vkSet vkc

            // specify samplers
            let mutable samplersDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline vkc $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 colorSampler vkSet vkc
                Pipeline.writeDescriptorSampler 1 0 irradianceSampler vkSet vkc

            // set up render
            let mutable renderArea = VkRect2D (0, 0, uint viewport.Bounds.Size.X, uint viewport.Bounds.Size.Y)
            let mutable vkViewport = Hl.makeViewport true renderArea
            let clearValue = VkClearValue (r = Constants.Render.ViewportClearColor.R, g = Constants.Render.ViewportClearColor.G, b = Constants.Render.ViewportClearColor.B, a = Constants.Render.ViewportClearColor.A)
            let mutable renderingInfo = Hl.makeRenderingInfo [|colorAttachment.ImageView|] None renderArea (Some clearValue)
            Vulkan.vkCmdBeginRendering (vkc.RenderCommandBuffer, asPointer &renderingInfo)
            Vulkan.vkCmdSetViewport (vkc.RenderCommandBuffer, 0u, 1u, asPointer &vkViewport)
            Vulkan.vkCmdSetScissor (vkc.RenderCommandBuffer, 0u, 1u, asPointer &renderArea)

            // set up pipeline
            Vulkan.vkCmdBindPipeline (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            Vulkan.vkCmdBindVertexBuffers (vkc.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            Vulkan.vkCmdBindIndexBuffer (vkc.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, asPointer &uniformsDescriptorSet, 0u, nullPtr)
            Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, asPointer &samplersDescriptorSet, 0u, nullPtr)

            // draw
            Vulkan.vkCmdDrawIndexed (vkc.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // tear down render
            Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer

            // advance pipeline
            Pipeline.advance 1 pipeline.Pipeline

        // abort
        | None -> Log.warnOnce "Cannot draw because VkPipeline does not exist."

    /// Create a PhysicallyBasedDeferredEnvironmentFilterPipeline.
    let createPhysicallyBasedDeferredEnvironmentFilterPipeline colorAttachmentFormat vkc =

        // create uniform buffers
        let eyeUniform = Buffer.create sizeof<Eye> Storage vkc
        let lightMapsUniform = Buffer.create (Constants.Render.LightMapsMaxDeferred * sizeof<LightMap>) Storage vkc

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.PhysicallyBasedDeferredEnvironmentFilterShaderFilePath
                [|VulkanUnblended|] [|false|]
                [|Pipeline.vertex 0 StaticVertexSize VkVertexInputRate.Vertex
                    [|Pipeline.attribute 0 Single3 0
                      Pipeline.attribute 1 Single2 StaticTexCoordsOffset
                      Pipeline.attribute 2 Single3 StaticNormalOffset|]|]
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 StorageBuffer FragmentStage 1 // eye
                      Pipeline.descriptor 1 StorageBuffer FragmentStage 1 // lightMaps
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
                vkc

        // make PhysicallyBasedDeferredEnvironmentFilterPipeline
        let physicallyBasedDeferredEnvironmentFilterPipeline =
            { EyeUniform = eyeUniform
              LightMapsUniform = lightMapsUniform
              Pipeline = pipeline }
        
        // fin
        physicallyBasedDeferredEnvironmentFilterPipeline

    /// Destroy PhysicallyBasedDeferredEnvironmentFilterPipeline.
    let destroyPhysicallyBasedDeferredEnvironmentFilterPipeline (pipeline : PhysicallyBasedDeferredEnvironmentFilterPipeline) vkc =
        Pipeline.destroy pipeline.Pipeline vkc

    /// Draw the environment filter pass of a deferred physically-based surface.
    let drawPhysicallyBasedDeferredEnvironmentFilterSurface
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (viewInverse : Matrix4x4)
        (projection : Matrix4x4)
        (projectionInverse : Matrix4x4)
        (viewProjection : Matrix4x4)
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
        (colorSampler : Sampler)
        (environmentFilterSampler : Sampler)
        (colorAttachment : Texture)
        (viewport : Viewport)
        (renderPassIndex : int)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : PhysicallyBasedDeferredEnvironmentFilterPipeline)
        (vkc : VulkanContext) =

        // only draw if required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 renderPassIndex pipeline.Pipeline vkc $ fun vkSet ->

                // specify eye
                let eye = Eye (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
                Buffer.uploadValue eye pipeline.EyeUniform vkc
                Pipeline.writeDescriptorStorageBuffer 0 0 pipeline.EyeUniform vkSet vkc

                // specify light maps
                let mutable lightMap = LightMap' ()
                use lightMapPtr = fixed &lightMap
                for i in 0 .. dec Constants.Render.LightMapsMaxDeferred do
                    if i < lightMapOrigins.Length then
                        lightMap.lightMapOrigins <- lightMapOrigins[i]
                        lightMap.lightMapMins <- lightMapMins[i]
                        lightMap.lightMapSizes <- lightMapSizes[i]
                        lightMap.lightMapAmbientColors <- lightMapAmbientColors[i].V3
                        lightMap.lightMapAmbientBrightnesses <- lightMapAmbientBrightnesses[i]
                    else lightMap <- Unchecked.defaultof<_>
                    Buffer.writeSubdata (i * sizeof<LightMap>) 0 sizeof<LightMap> 1 (NativePtr.toNativeInt lightMapPtr) pipeline.LightMapsUniform vkc
                Buffer.flushSubdata 0 0 sizeof<LightMap> Constants.Render.LightMapsMaxDeferred pipeline.LightMapsUniform vkc
                Pipeline.writeDescriptorStorageBuffer 1 0 pipeline.LightMapsUniform vkSet vkc

                // specify static environment textures
                Pipeline.writeDescriptorSampledImage 2 0 depthTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 3 0 materialTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 4 0 normalPlusTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 5 0 clearCoatPlusTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 6 0 lightMappingTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 7 0 environmentFilterMap vkSet vkc
                Pipeline.writeDescriptorSampledImages 8 0 (Array.tryTake Constants.Render.LightMapsMaxDeferred environmentFilterMaps) vkSet vkc

            // specify samplers
            let mutable samplersDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline vkc $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 colorSampler vkSet vkc
                Pipeline.writeDescriptorSampler 1 0 environmentFilterSampler vkSet vkc

            // set up render
            let mutable renderArea = VkRect2D (0, 0, uint viewport.Bounds.Size.X, uint viewport.Bounds.Size.Y)
            let mutable vkViewport = Hl.makeViewport true renderArea
            let clearValue = VkClearValue (r = Constants.Render.ViewportClearColor.R, g = Constants.Render.ViewportClearColor.G, b = Constants.Render.ViewportClearColor.B, a = Constants.Render.ViewportClearColor.A)
            let mutable renderingInfo = Hl.makeRenderingInfo [|colorAttachment.ImageView|] None renderArea (Some clearValue)
            Vulkan.vkCmdBeginRendering (vkc.RenderCommandBuffer, asPointer &renderingInfo)
            Vulkan.vkCmdSetViewport (vkc.RenderCommandBuffer, 0u, 1u, asPointer &vkViewport)
            Vulkan.vkCmdSetScissor (vkc.RenderCommandBuffer, 0u, 1u, asPointer &renderArea)

            // set up pipeline
            Vulkan.vkCmdBindPipeline (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            Vulkan.vkCmdBindVertexBuffers (vkc.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            Vulkan.vkCmdBindIndexBuffer (vkc.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, asPointer &uniformsDescriptorSet, 0u, nullPtr)
            Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, asPointer &samplersDescriptorSet, 0u, nullPtr)

            // draw
            Vulkan.vkCmdDrawIndexed (vkc.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // tear down render
            Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer

            // advance pipeline
            Pipeline.advance 1 pipeline.Pipeline

        // abort
        | None -> Log.warnOnce "Cannot draw because VkPipeline does not exist."

    /// Create a PhysicallyBasedDeferredFoggingPipeline.
    let createPhysicallyBasedDeferredFoggingPipeline colorAttachmentFormat vkc =

        // create uniform buffers
        let shadowMatrixMax = Constants.Render.ShadowTexturesMax + Constants.Render.ShadowCascadesMax * Constants.Render.ShadowCascadeLevels
        let eyeUniform = Buffer.create sizeof<Eye> Storage vkc
        let lightingUniform = Buffer.create sizeof<Lighting> Storage vkc
        let lightsGeneralUniform = Buffer.create sizeof<LightsGeneral> Storage vkc
        let lightsUniform = Buffer.create (Constants.Render.LightsMaxDeferred * sizeof<Light>) Storage vkc
        let shadowMatricesUniform = Buffer.create (shadowMatrixMax * sizeof<Matrix4x4>) Storage vkc

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.PhysicallyBasedDeferredFoggingShaderFilePath
                [|VulkanUnblended|] [|false|]
                [|Pipeline.vertex 0 StaticVertexSize VkVertexInputRate.Vertex
                    [|Pipeline.attribute 0 Single3 0
                      Pipeline.attribute 1 Single2 StaticTexCoordsOffset
                      Pipeline.attribute 2 Single3 StaticNormalOffset|]|]
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 StorageBuffer FragmentStage 1 // eye
                      Pipeline.descriptor 1 StorageBuffer FragmentStage 1 // lighting
                      Pipeline.descriptor 2 StorageBuffer FragmentStage 1 // lightsGeneral
                      Pipeline.descriptor 3 StorageBuffer FragmentStage 1 // lights
                      Pipeline.descriptor 4 StorageBuffer FragmentStage 1 // shadowMatrices
                      Pipeline.descriptor 5 SampledImage FragmentStage 1 // depth
                      Pipeline.descriptor 6 SampledImage FragmentStage 1 // shadowTextures
                      Pipeline.descriptor 7 SampledImage FragmentStage Constants.Render.ShadowMapsMax // shadowMaps
                      Pipeline.descriptor 8 SampledImage FragmentStage Constants.Render.ShadowCascadesMax|] // shadowCascades
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1
                      Pipeline.descriptor 1 Sampler FragmentStage 1|]|]
                [||] [|colorAttachmentFormat|] None
                [|eyeUniform; lightingUniform; lightsGeneralUniform; lightsUniform|]
                vkc

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
    let destroyPhysicallyBasedDeferredFoggingPipeline (pipeline : PhysicallyBasedDeferredFoggingPipeline) vkc =
        Pipeline.destroy pipeline.Pipeline vkc

    /// Draw the fogging pass of a deferred physically-based surface.
    let drawPhysicallyBasedDeferredFoggingSurface
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (viewInverse : Matrix4x4)
        (projection : Matrix4x4)
        (projectionInverse : Matrix4x4)
        (viewProjection : Matrix4x4)
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
        (colorSampler : Sampler)
        (shadowSampler : Sampler)
        (foggingAttachment : Texture)
        (viewport : Viewport)
        (renderPassIndex : int)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : PhysicallyBasedDeferredFoggingPipeline)
        (vkc : VulkanContext) =

        // only draw if required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 renderPassIndex pipeline.Pipeline vkc $ fun vkSet ->

                // specify eye
                let eye = Eye (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
                Buffer.uploadValue eye pipeline.EyeUniform vkc
                Pipeline.writeDescriptorStorageBuffer 0 0 pipeline.EyeUniform vkSet vkc

                // specify lighting
                let mutable lighting = Lighting ()
                lighting.lightCutoffMargin <- lightCutoffMargin
                lighting.ssvfEnabled <- ssvfEnabled
                lighting.ssvfIntensity <- ssvfIntensity
                lighting.ssvfSteps <- ssvfSteps
                lighting.ssvfAsymmetry <- ssvfAsymmetry
                Buffer.uploadValue lighting pipeline.LightingUniform vkc
                Pipeline.writeDescriptorStorageBuffer 1 0 pipeline.LightingUniform vkSet vkc

                // specify lights general
                let mutable lightsGeneral = LightsGeneral ()
                lightsGeneral.lightMapsCount <- lightMapsCount
                lightsGeneral.lightMapSingletonBlendMargin <- lightMapSingletonBlendMargin
                lightsGeneral.lightsCount <- lightsCount
                Buffer.uploadValue lightsGeneral pipeline.LightsGeneralUniform vkc
                Pipeline.writeDescriptorStorageBuffer 2 0 pipeline.LightsGeneralUniform vkSet vkc

                // specify lights
                let mutable light = Light ()
                use lightPtr = fixed &light
                for i in 0 .. dec Constants.Render.LightsMaxDeferred do
                    if i < lightOrigins.Length then
                        light.lightOrigins <- lightOrigins[i]
                        light.lightDirections <- lightDirections[i]
                        light.lightColors <- lightColors[i].V3
                        light.lightBrightnesses <- lightBrightnesses[i]
                        light.lightAttenuationLinears <- lightAttenuationLinears[i]
                        light.lightAttenuationQuadratics <- lightAttenuationQuadratics[i]
                        light.lightCutoffs <- lightCutoffs[i]
                        light.lightTypes <- lightTypes[i]
                        light.lightConeInners <- lightConeInners[i]
                        light.lightConeOuters <- lightConeOuters[i]
                        light.lightDesireFogs <- lightDesireFogs[i]
                        light.lightShadowIndices <- lightShadowIndices[i]
                    else light <- Unchecked.defaultof<_>
                    Buffer.writeSubdata (i * sizeof<Light>) 0 sizeof<Light> 1 (NativePtr.toNativeInt lightPtr) pipeline.LightsUniform vkc
                Buffer.flushSubdata 0 0 sizeof<Light> Constants.Render.LightsMaxDeferred pipeline.LightsUniform vkc
                Pipeline.writeDescriptorStorageBuffer 3 0 pipeline.LightsUniform vkSet vkc

                // specify shadow matrices
                use shadowMatricesPin = new ArrayPin<_> (shadowMatrices)
                let shadowMatricesCount = min shadowMatrices.Length (Constants.Render.ShadowTexturesMax + Constants.Render.ShadowCascadesMax * Constants.Render.ShadowCascadeLevels)
                Buffer.uploadData sizeof<Matrix4x4> shadowMatricesCount shadowMatricesPin.NativeInt pipeline.ShadowMatricesUniform vkc
                Pipeline.writeDescriptorStorageBuffer 4 0 pipeline.ShadowMatricesUniform vkSet vkc

                // specify textures
                Pipeline.writeDescriptorSampledImage 5 0 depthTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 6 0 shadowTextureArray vkSet vkc
                Pipeline.writeDescriptorSampledImages 7 0 (Array.tryTake Constants.Render.ShadowMapsMax shadowMaps) vkSet vkc
                Pipeline.writeDescriptorSampledImages 8 0 (Array.tryTake Constants.Render.ShadowCascadesMax shadowCascades) vkSet vkc

            // specify samplers
            let mutable samplersDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline vkc $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 colorSampler vkSet vkc
                Pipeline.writeDescriptorSampler 1 0 shadowSampler vkSet vkc

            // set up render
            let mutable renderArea = VkRect2D (0, 0, uint viewport.Bounds.Size.X, uint viewport.Bounds.Size.Y)
            let mutable vkViewport = Hl.makeViewport true renderArea
            let clearValue = VkClearValue (r = Constants.Render.ViewportClearColor.R, g = Constants.Render.ViewportClearColor.G, b = Constants.Render.ViewportClearColor.B, a = Constants.Render.ViewportClearColor.A)
            let mutable renderingInfo = Hl.makeRenderingInfo [|foggingAttachment.ImageView|] None renderArea (Some clearValue)
            Vulkan.vkCmdBeginRendering (vkc.RenderCommandBuffer, asPointer &renderingInfo)
            Vulkan.vkCmdSetViewport (vkc.RenderCommandBuffer, 0u, 1u, asPointer &vkViewport)
            Vulkan.vkCmdSetScissor (vkc.RenderCommandBuffer, 0u, 1u, asPointer &renderArea)

            // set up pipeline
            Vulkan.vkCmdBindPipeline (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            Vulkan.vkCmdBindVertexBuffers (vkc.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            Vulkan.vkCmdBindIndexBuffer (vkc.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            let mutable (uniformsDescriptorSet, samplersDescriptorSet) = (uniformsDescriptorSet, samplersDescriptorSet)
            Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, asPointer &uniformsDescriptorSet, 0u, nullPtr)
            Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, asPointer &samplersDescriptorSet, 0u, nullPtr)

            // draw
            Vulkan.vkCmdDrawIndexed (vkc.RenderCommandBuffer, uint geometry.ElementCount, uint 1, 0u, 0, 0u)

            // tear down render
            Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer

            // advance pipeline
            Pipeline.advance 1 pipeline.Pipeline

        // abort
        | None -> Log.warnOnce "Cannot draw because VkPipeline does not exist."

    /// Create a PhysicallyBasedDeferredColoringPipeline.
    let createPhysicallyBasedDeferredColoringPipeline colorAttachmentFormats vkc =

        // create uniform buffers
        let eyeUniform = Buffer.create sizeof<Eye> Storage vkc
        let lightingUniform = Buffer.create sizeof<Lighting> Storage vkc

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.PhysicallyBasedDeferredColoringShaderFilePath
                [|VulkanUnblended|] [|false|]
                [|Pipeline.vertex 0 StaticVertexSize VkVertexInputRate.Vertex
                    [|Pipeline.attribute 0 Single3 0
                      Pipeline.attribute 1 Single2 StaticTexCoordsOffset
                      Pipeline.attribute 2 Single3 StaticNormalOffset|]|]
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 StorageBuffer FragmentStage 1 // eye
                      Pipeline.descriptor 1 StorageBuffer FragmentStage 1 // lighting
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
                vkc

        // make PhysicallyBasedDeferredColoringPipeline
        let physicallyBasedDeferredColoringPipeline =
            { PhysicallyBasedDeferredColoringPipeline.EyeUniform = eyeUniform
              LightingUniform = lightingUniform
              Pipeline = pipeline }

        // fin
        physicallyBasedDeferredColoringPipeline

    /// Destroy PhysicallyBasedDeferredColoringPipeline.
    let destroyPhysicallyBasedDeferredColoringPipeline (pipeline : PhysicallyBasedDeferredColoringPipeline) vkc =
        Pipeline.destroy pipeline.Pipeline vkc

    /// Draw the coloring pass of a deferred physically-based surface.
    let drawPhysicallyBasedDeferredColoringSurface
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (viewInverse : Matrix4x4)
        (projection : Matrix4x4)
        (projectionInverse : Matrix4x4)
        (viewProjection : Matrix4x4)
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
        (colorSampler : Sampler)
        (brdfSampler : Sampler)
        (coloringAttachment : Texture)
        (depthAttachment : Texture)
        (viewport : Viewport)
        (renderPassIndex : int)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : PhysicallyBasedDeferredColoringPipeline)
        (vkc : VulkanContext) =

        // only draw if required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 renderPassIndex pipeline.Pipeline vkc $ fun vkSet ->

                // specify eye
                let eye = Eye (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
                Buffer.uploadValue eye pipeline.EyeUniform vkc
                Pipeline.writeDescriptorStorageBuffer 0 0 pipeline.EyeUniform vkSet vkc

                // specify lighting
                let mutable lighting = Lighting ()
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
                Buffer.uploadValue lighting pipeline.LightingUniform vkc
                Pipeline.writeDescriptorStorageBuffer 1 0 pipeline.LightingUniform vkSet vkc

                // specify textures
                Pipeline.writeDescriptorSampledImage 2 0 depthTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 3 0 albedoTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 4 0 materialTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 5 0 normalPlusTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 6 0 clearCoatPlusTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 7 0 lightAccumTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 8 0 brdfTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 9 0 ambientTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 10 0 irradianceTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 11 0 environmentFilterTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 12 0 ssaoTexture vkSet vkc

            // specify samplers
            let mutable samplersDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline vkc $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 colorSampler vkSet vkc
                Pipeline.writeDescriptorSampler 1 0 brdfSampler vkSet vkc

            // set up render
            let mutable renderArea = VkRect2D (0, 0, uint viewport.Bounds.Size.X, uint viewport.Bounds.Size.Y)
            let mutable vkViewport = Hl.makeViewport true renderArea
            let clearValue = VkClearValue (r = Constants.Render.ViewportClearColor.R, g = Constants.Render.ViewportClearColor.G, b = Constants.Render.ViewportClearColor.B, a = Constants.Render.ViewportClearColor.A)
            let mutable renderingInfo = Hl.makeRenderingInfo [|coloringAttachment.ImageView; depthAttachment.ImageView|] None renderArea (Some clearValue)
            Vulkan.vkCmdBeginRendering (vkc.RenderCommandBuffer, asPointer &renderingInfo)
            Vulkan.vkCmdSetViewport (vkc.RenderCommandBuffer, 0u, 1u, asPointer &vkViewport)
            Vulkan.vkCmdSetScissor (vkc.RenderCommandBuffer, 0u, 1u, asPointer &renderArea)

            // set up pipeline
            Vulkan.vkCmdBindPipeline (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            Vulkan.vkCmdBindVertexBuffers (vkc.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            Vulkan.vkCmdBindIndexBuffer (vkc.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            let mutable (uniformsDescriptorSet, samplersDescriptorSet) = (uniformsDescriptorSet, samplersDescriptorSet)
            Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, asPointer &uniformsDescriptorSet, 0u, nullPtr)
            Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, asPointer &samplersDescriptorSet, 0u, nullPtr)

            // draw
            Vulkan.vkCmdDrawIndexed (vkc.RenderCommandBuffer, uint geometry.ElementCount, uint 1, 0u, 0, 0u)

            // tear down render
            Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer

            // advance pipeline
            Pipeline.advance 1 pipeline.Pipeline

        // abort
        | None -> Log.warnOnce "Cannot draw because VkPipeline does not exist."

    /// Create a PhysicallyBasedDeferredCompositionPipeline.
    let createPhysicallyBasedDeferredCompositionPipeline colorAttachmentFormat vkc =

        // create uniform buffers
        let eyeUniform = Buffer.create sizeof<Eye> Storage vkc
        let lightingUniform = Buffer.create sizeof<Lighting> Storage vkc

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.PhysicallyBasedDeferredCompositionShaderFilePath
                [|VulkanUnblended|] [|false|]
                [|Pipeline.vertex 0 StaticVertexSize VkVertexInputRate.Vertex
                    [|Pipeline.attribute 0 Single3 0
                      Pipeline.attribute 1 Single2 StaticTexCoordsOffset
                      Pipeline.attribute 2 Single3 StaticNormalOffset|]|]
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 StorageBuffer FragmentStage 1 // eye
                      Pipeline.descriptor 1 StorageBuffer FragmentStage 1 // lighting
                      Pipeline.descriptor 2 SampledImage FragmentStage 1 // depthTexture
                      Pipeline.descriptor 3 SampledImage FragmentStage 1 // colorTexture
                      Pipeline.descriptor 4 SampledImage FragmentStage 1|] // fogAccumTexture
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|]
                [||] [|colorAttachmentFormat|] None
                [|eyeUniform; lightingUniform|]
                vkc

        // make PhysicallyBasedDeferredCompositionPipeline
        let physicallyBasedDeferredCompositionPipeline =
            { PhysicallyBasedDeferredCompositionPipeline.EyeUniform = eyeUniform
              LightingUniform = lightingUniform
              Pipeline = pipeline }

        // fin
        physicallyBasedDeferredCompositionPipeline

    /// Destroy PhysicallyBasedDeferredCompositionPipeline.
    let destroyPhysicallyBasedDeferredCompositionPipeline (pipeline : PhysicallyBasedDeferredCompositionPipeline) vkc =
        Pipeline.destroy pipeline.Pipeline vkc

    /// Draw the bilateral up-sample pass of a deferred physically-based surface.
    let drawPhysicallyBasedDeferredCompositionSurface
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (viewInverse : Matrix4x4)
        (projection : Matrix4x4)
        (projectionInverse : Matrix4x4)
        (viewProjection : Matrix4x4)
        (fogEnabled : int)
        (fogType : int)
        (fogStart : single)
        (fogFinish : single)
        (fogDensity : single)
        (fogColor : Color)
        (depthTexture : Texture)
        (colorTexture : Texture)
        (fogAccumTexture : Texture)
        (colorSampler : Sampler)
        (compositionAttachment : Texture)
        (viewport : Viewport)
        (renderPassIndex : int)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : PhysicallyBasedDeferredCompositionPipeline)
        (vkc : VulkanContext) =

        // only draw if required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 renderPassIndex pipeline.Pipeline vkc $ fun vkSet ->

                // specify eye
                let eye = Eye (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
                Buffer.uploadValue eye pipeline.EyeUniform vkc
                Pipeline.writeDescriptorStorageBuffer 0 0 pipeline.EyeUniform vkSet vkc

                // specify lighting
                let mutable lighting = Lighting ()
                lighting.fogEnabled <- fogEnabled
                lighting.fogType <- fogType
                lighting.fogStart <- fogStart
                lighting.fogFinish <- fogFinish
                lighting.fogDensity <- fogDensity
                lighting.fogColor <- fogColor.V4
                Buffer.uploadValue lighting pipeline.LightingUniform vkc
                Pipeline.writeDescriptorStorageBuffer 1 0 pipeline.LightingUniform vkSet vkc

                // specify textures
                Pipeline.writeDescriptorSampledImage 2 0 depthTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 3 0 colorTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 4 0 fogAccumTexture vkSet vkc

            // specify samplers
            let mutable samplersDescriptorSet = Pipeline.specifyDescriptorSet 1 Unit pipeline.Pipeline vkc $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 colorSampler vkSet vkc

            // set up render
            let mutable renderArea = VkRect2D (0, 0, uint viewport.Bounds.Size.X, uint viewport.Bounds.Size.Y)
            let mutable vkViewport = Hl.makeViewport true renderArea
            let clearValue = VkClearValue (r = Constants.Render.ViewportClearColor.R, g = Constants.Render.ViewportClearColor.G, b = Constants.Render.ViewportClearColor.B, a = Constants.Render.ViewportClearColor.A)
            let mutable renderingInfo = Hl.makeRenderingInfo [|compositionAttachment.ImageView|] None renderArea (Some clearValue)
            Vulkan.vkCmdBeginRendering (vkc.RenderCommandBuffer, asPointer &renderingInfo)
            Vulkan.vkCmdSetViewport (vkc.RenderCommandBuffer, 0u, 1u, asPointer &vkViewport)
            Vulkan.vkCmdSetScissor (vkc.RenderCommandBuffer, 0u, 1u, asPointer &renderArea)

            // set up pipeline
            Vulkan.vkCmdBindPipeline (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            Vulkan.vkCmdBindVertexBuffers (vkc.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            Vulkan.vkCmdBindIndexBuffer (vkc.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            let mutable (uniformsDescriptorSet, samplersDescriptorSet) = (uniformsDescriptorSet, samplersDescriptorSet)
            Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, asPointer &uniformsDescriptorSet, 0u, nullPtr)
            Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, asPointer &samplersDescriptorSet, 0u, nullPtr)

            // draw
            Vulkan.vkCmdDrawIndexed (vkc.RenderCommandBuffer, uint geometry.ElementCount, uint 1, 0u, 0, 0u)

            // tear down render
            Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer

            // advance pipeline
            Pipeline.advance 1 pipeline.Pipeline

        // abort
        | None -> Log.warnOnce "Cannot draw because VkPipeline does not exist."

    /// Begin the process of drawing physically-based forward surfaces.
    let beginPhysicallyBasedForwardSurfaces
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (viewInverse : Matrix4x4)
        (projection : Matrix4x4)
        (projectionInverse : Matrix4x4)
        (viewProjection : Matrix4x4)
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
        (filteredSampler : Sampler)
        (cubeMapSampler : Sampler)
        (shadowSampler : Sampler)
        (colorSampler : Sampler)
        (depthSampler : Sampler)
        (brdfSampler : Sampler)
        (colorAttachment : Texture)
        (depthAttachment : Texture)
        (viewport : Viewport)
        (renderPassIndex : int)
        (pipeline : PhysicallyBasedPipeline)
        (vkc : VulkanContext) =

        // specify uniforms
        let mutable uniformDescriptorSet = Pipeline.specifyDescriptorSet 0 renderPassIndex pipeline.Pipeline vkc $ fun vkSet ->

            // specify eye
            let eye = Eye (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
            Buffer.uploadValue eye pipeline.EyeUniform vkc
            Pipeline.writeDescriptorStorageBuffer 0 0 pipeline.EyeUniform vkSet vkc

            // specify lighting
            let mutable lighting = Lighting ()
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
            Buffer.uploadValue lighting pipeline.LightingUniform vkc
            Pipeline.writeDescriptorStorageBuffer 1 0 pipeline.LightingUniform vkSet vkc

            // specify static environment textures
            Pipeline.writeDescriptorSampledImage 2 0 depthTexture vkSet vkc
            Pipeline.writeDescriptorSampledImage 3 0 colorTexture vkSet vkc
            Pipeline.writeDescriptorSampledImage 4 0 brdfTexture vkSet vkc
            Pipeline.writeDescriptorSampledImage 5 0 irradianceMap vkSet vkc
            Pipeline.writeDescriptorSampledImage 6 0 environmentFilterMap vkSet vkc

        // specify samplers
        let mutable samplersDescriptorSet = Pipeline.specifyDescriptorSet 3 Unit pipeline.Pipeline vkc $ fun vkSet ->
            Pipeline.writeDescriptorSampler 0 0 filteredSampler vkSet vkc
            Pipeline.writeDescriptorSampler 1 0 cubeMapSampler vkSet vkc
            Pipeline.writeDescriptorSampler 2 0 shadowSampler vkSet vkc
            Pipeline.writeDescriptorSampler 3 0 colorSampler vkSet vkc
            Pipeline.writeDescriptorSampler 4 0 depthSampler vkSet vkc
            Pipeline.writeDescriptorSampler 5 0 brdfSampler vkSet vkc

        // set up render
        let mutable renderArea = VkRect2D (0, 0, uint viewport.Bounds.Size.X, uint viewport.Bounds.Size.Y)
        let mutable vkViewport = Hl.makeViewport true renderArea
        let mutable renderingInfo = Hl.makeRenderingInfo [|colorAttachment.ImageView|] (Some depthAttachment.ImageView) renderArea None
        Vulkan.vkCmdBeginRendering (vkc.RenderCommandBuffer, asPointer &renderingInfo)
        Vulkan.vkCmdSetViewport (vkc.RenderCommandBuffer, 0u, 1u, asPointer &vkViewport)
        Vulkan.vkCmdSetScissor (vkc.RenderCommandBuffer, 0u, 1u, asPointer &renderArea)

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
        (vkc : VulkanContext) =

        // only draw if required vkPipeline exists
        let blend = if blending then VulkanTransparent else VulkanUnblended
        match Pipeline.tryGetVkPipeline blend (not material.TwoSided) pipeline.Pipeline with
        | Some vkPipeline ->

            // specify instancing
            use instanceFieldsPin = new ArrayPin<_> (instanceFields)
            Buffer.uploadData (Constants.Render.InstanceFieldCount * sizeof<single>) surfacesCount instanceFieldsPin.NativeInt geometry.InstanceBuffer vkc

            // specify material
            let mutable materialDescriptorSet = Pipeline.specifyDescriptorSet 1 material pipeline.Pipeline vkc $ fun vkSet ->
                Pipeline.writeDescriptorSampledImage 0 0 material.AlbedoTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 1 0 material.RoughnessTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 2 0 material.MetallicTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 3 0 material.AmbientOcclusionTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 4 0 material.EmissionTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 5 0 material.NormalTexture vkSet vkc
                Pipeline.writeDescriptorSampledImage 6 0 material.HeightTexture vkSet vkc

            // specify dynamic
            let mutable dynamicDescriptorSet = Pipeline.specifyDescriptorSet 2 pipeline.Pipeline.DrawIndex pipeline.Pipeline vkc $ fun vkSet ->

                // specify bones
                use bonesPin = new ArrayPin<_> (bones)
                let bonesCount = min bones.Length Constants.Render.BonesMax
                Buffer.uploadData sizeof<Matrix4x4> bonesCount bonesPin.NativeInt pipeline.BoneUniform vkc
                Pipeline.writeDescriptorStorageBuffer 0 0 pipeline.BoneUniform vkSet vkc

                // specify light maps
                let mutable lightMap = LightMap' ()
                use lightMapPtr = fixed &lightMap
                for i in 0 .. dec Constants.Render.LightMapsMaxForward do
                    if i < lightMapOrigins.Length then
                        lightMap.lightMapOrigins <- lightMapOrigins[i]
                        lightMap.lightMapMins <- lightMapMins[i]
                        lightMap.lightMapSizes <- lightMapSizes[i]
                        lightMap.lightMapAmbientColors <- lightMapAmbientColors[i].V3
                        lightMap.lightMapAmbientBrightnesses <- lightMapAmbientBrightnesses[i]
                    else lightMap <- Unchecked.defaultof<_>
                    Buffer.writeSubdata (i * sizeof<LightMap>) 0 sizeof<LightMap> 1 (NativePtr.toNativeInt lightMapPtr) pipeline.LightMapUniform vkc
                Buffer.flushSubdata 0 0 sizeof<LightMap> Constants.Render.LightMapsMaxForward pipeline.LightMapUniform vkc
                Pipeline.writeDescriptorStorageBuffer 1 0 pipeline.LightMapUniform vkSet vkc

                // specify lights general
                let mutable lightsGeneral = LightsGeneral ()
                lightsGeneral.lightMapsCount <- lightMapsCount
                lightsGeneral.lightMapSingletonBlendMargin <- lightMapSingletonBlendMargin
                lightsGeneral.lightsCount <- lightsCount
                Buffer.uploadValue lightsGeneral pipeline.LightsGeneralUniform vkc
                Pipeline.writeDescriptorStorageBuffer 2 0 pipeline.LightsGeneralUniform vkSet vkc

                // specify lights
                let mutable light = Light ()
                use lightPtr = fixed &light
                for i in 0 .. dec Constants.Render.LightsMaxForward do
                    if i < lightOrigins.Length then
                        light.lightOrigins <- lightOrigins[i]
                        light.lightDirections <- lightDirections[i]
                        light.lightColors <- lightColors[i].V3
                        light.lightBrightnesses <- lightBrightnesses[i]
                        light.lightAttenuationLinears <- lightAttenuationLinears[i]
                        light.lightAttenuationQuadratics <- lightAttenuationQuadratics[i]
                        light.lightCutoffs <- lightCutoffs[i]
                        light.lightTypes <- lightTypes[i]
                        light.lightConeInners <- lightConeInners[i]
                        light.lightConeOuters <- lightConeOuters[i]
                        light.lightDesireFogs <- lightDesireFogs[i]
                        light.lightShadowIndices <- lightShadowIndices[i]
                    else light <- Unchecked.defaultof<_>
                    Buffer.writeSubdata (i * sizeof<Light>) 0 sizeof<Light> 1 (NativePtr.toNativeInt lightPtr) pipeline.LightUniform vkc
                Buffer.flushSubdata 0 0 sizeof<Light> Constants.Render.LightsMaxForward pipeline.LightUniform vkc
                Pipeline.writeDescriptorStorageBuffer 3 0 pipeline.LightUniform vkSet vkc

                // specify shadow matrices
                use shadowMatricesPin = new ArrayPin<_> (shadowMatrices)
                let shadowMatricesCount = min shadowMatrices.Length (Constants.Render.ShadowTexturesMax + Constants.Render.ShadowCascadesMax * Constants.Render.ShadowCascadeLevels)
                Buffer.uploadData sizeof<Matrix4x4> shadowMatricesCount shadowMatricesPin.NativeInt pipeline.ShadowMatrixUniform vkc
                Pipeline.writeDescriptorStorageBuffer 4 0 pipeline.ShadowMatrixUniform vkSet vkc

                // specify dynamic environment textures
                Pipeline.writeDescriptorSampledImages 5 0 (Array.tryTake Constants.Render.LightMapsMaxForward irradianceMaps) vkSet vkc
                Pipeline.writeDescriptorSampledImages 6 0 (Array.tryTake Constants.Render.LightMapsMaxForward environmentFilterMaps) vkSet vkc
                Pipeline.writeDescriptorSampledImage 7 0 shadowTextureArray vkSet vkc
                Pipeline.writeDescriptorSampledImages 8 0 (Array.tryTake Constants.Render.ShadowMapsMax shadowMaps) vkSet vkc
                Pipeline.writeDescriptorSampledImages 9 0 (Array.tryTake Constants.Render.ShadowCascadesMax shadowCascades) vkSet vkc

            // set up pipeline
            Vulkan.vkCmdBindPipeline (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)
            Vulkan.vkCmdSetDepthTestEnable (vkc.RenderCommandBuffer, not depthTest.IsAlwaysPassTest)
            Vulkan.vkCmdSetDepthCompareOp (vkc.RenderCommandBuffer, Pipeline.depthTestToVkCompareOp depthTest)

            // bind vertex and index buffers
            let vertexBuffers = [|geometry.VertexBuffer.VkBuffer; geometry.InstanceBuffer.VkBuffer|]
            let vertexOffsets = [|0UL; 0UL|]
            use vertexBuffersPin = new ArrayPin<_> (vertexBuffers)
            use vertexOffsetsPin = new ArrayPin<_> (vertexOffsets)
            Vulkan.vkCmdBindVertexBuffers (vkc.RenderCommandBuffer, 0u, 2u, vertexBuffersPin.Pointer, vertexOffsetsPin.Pointer)
            Vulkan.vkCmdBindIndexBuffer (vkc.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            let mutable (uniformsDescriptorSet, samplersDescriptorSet) = (uniformsDescriptorSet, samplersDescriptorSet)
            Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, asPointer &uniformsDescriptorSet, 0u, nullPtr)
            Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, asPointer &materialDescriptorSet, 0u, nullPtr)
            Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 2u, 1u, asPointer &dynamicDescriptorSet, 0u, nullPtr)
            Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 3u, 1u, asPointer &samplersDescriptorSet, 0u, nullPtr)

            // draw
            Vulkan.vkCmdDrawIndexed (vkc.RenderCommandBuffer, uint geometry.ElementCount, uint surfacesCount, 0u, 0, 0u)

            // advance instancing
            Buffer.advance geometry.InstanceBuffer

            // advance pipeline
            Pipeline.advance surfacesCount pipeline.Pipeline

        // abort
        | None -> Log.warnOnce "Cannot draw because VkPipeline does not exist."

    /// End the process of drawing physically-based forward surfaces.
    let endPhysicallyBasedForwardSurfaces (_ : PhysicallyBasedPipeline) (vkc : VulkanContext)=

        // tear down render
        Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer

    let createPhysicallyBasedPipelines lightMapsMax lightsMax attachments vkc =

        // static vertices
        let staticVertices =
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

        // animated vertices
        let animatedVertices =
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

        // create shadow static point pipeline
        let (shadowMapColorAttachment, shadowMapZAttachment) = attachments.ShadowMapAttachmentsArray[0] // assume all like first
        let shadowStaticPointPipeline =
            createPhysicallyBasedShadowPipeline
                Constants.Paths.PhysicallyBasedShadowStaticPointShaderFilePath
                staticVertices
                [|shadowMapColorAttachment.VkFormat|]
                shadowMapZAttachment.VkFormat
                vkc

        // create shadow static spot pipeline
        let (shadowTextureArrayColorAttachment, shadowTextureArrayZAttachment) = attachments.ShadowTextureArrayAttachments
        let shadowStaticSpotPipeline =
            createPhysicallyBasedShadowPipeline
                Constants.Paths.PhysicallyBasedShadowStaticSpotShaderFilePath
                staticVertices
                [|shadowTextureArrayColorAttachment.VkFormat|]
                shadowTextureArrayZAttachment.VkFormat
                vkc

        // create shadow static directional pipeline
        let shadowStaticDirectionalPipeline =
            createPhysicallyBasedShadowPipeline
                Constants.Paths.PhysicallyBasedShadowStaticDirectionalShaderFilePath
                staticVertices
                [|shadowTextureArrayColorAttachment.VkFormat|]
                shadowTextureArrayZAttachment.VkFormat
                vkc

        // create shadow animated point pipeline
        let shadowAnimatedPointPipeline =
            createPhysicallyBasedShadowPipeline
                Constants.Paths.PhysicallyBasedShadowAnimatedPointShaderFilePath
                animatedVertices
                [|shadowMapColorAttachment.VkFormat|]
                shadowMapZAttachment.VkFormat
                vkc

        // create shadow animated spot pipeline
        let shadowAnimatedSpotPipeline =
            createPhysicallyBasedShadowPipeline
                Constants.Paths.PhysicallyBasedShadowAnimatedSpotShaderFilePath
                animatedVertices
                [|shadowTextureArrayColorAttachment.VkFormat|]
                shadowTextureArrayZAttachment.VkFormat
                vkc

        // create shadow animated directional pipeline
        let shadowAnimatedDirectionalPipeline =
            createPhysicallyBasedShadowPipeline
                Constants.Paths.PhysicallyBasedShadowAnimatedDirectionalShaderFilePath
                animatedVertices
                [|shadowTextureArrayColorAttachment.VkFormat|]
                shadowTextureArrayZAttachment.VkFormat
                vkc

        // create deferred static pipeline
        // NOTE: DJL: we use the composition z attachment directly to avoid having to find a depth format supporting copy operations,
        // which is problematic on some mesa drivers.
        let (depth, albedo, material, normalPlus, subdermalPlus, scatterPlus, clearCoatPlus, z) = attachments.GeometryAttachments
        let deferredStaticPipeline =
            createPhysicallyBasedPipeline
                lightMapsMax
                lightsMax
                Constants.Paths.PhysicallyBasedDeferredStaticShaderFilePath
                [|VulkanUnblended|]
                [|false; true|]
                staticVertices
                [|depth.VkFormat
                  albedo.VkFormat
                  material.VkFormat
                  normalPlus.VkFormat
                  subdermalPlus.VkFormat
                  scatterPlus.VkFormat
                  clearCoatPlus.VkFormat|]
                (Some z.VkFormat)
                vkc

        // create deferred static clipped pipeline
        let deferredStaticClippedPipeline =
            createPhysicallyBasedPipeline
                lightMapsMax
                lightsMax
                Constants.Paths.PhysicallyBasedDeferredStaticClippedShaderFilePath
                [|VulkanUnblended|]
                [|false; true|]
                staticVertices
                [|depth.VkFormat
                  albedo.VkFormat
                  material.VkFormat
                  normalPlus.VkFormat
                  subdermalPlus.VkFormat
                  scatterPlus.VkFormat
                  clearCoatPlus.VkFormat|]
                (Some z.VkFormat)
                vkc

        // create deferred animated pipeline
        let deferredAnimatedPipeline =
            createPhysicallyBasedPipeline
                lightMapsMax
                lightsMax
                Constants.Paths.PhysicallyBasedDeferredAnimatedShaderFilePath
                [|VulkanUnblended|]
                [|false; true|]
                animatedVertices
                [|depth.VkFormat
                  albedo.VkFormat
                  material.VkFormat
                  normalPlus.VkFormat
                  subdermalPlus.VkFormat
                  scatterPlus.VkFormat
                  clearCoatPlus.VkFormat|]
                (Some z.VkFormat)
                vkc
        
        // create deferred lighting pipelines
        let deferredLightingPipeline = createPhysicallyBasedDeferredLightingPipeline attachments.LightingAttachment.VkFormat vkc
        let deferredLightMappingPipeline = createPhysicallyBasedDeferredLightMappingPipeline attachments.LightMappingAttachment.VkFormat vkc
        let deferredAmbientPipeline = createPhysicallyBasedDeferredAmbientPipeline attachments.AmbientAttachment.VkFormat vkc
        let deferredIrradiancePipeline = createPhysicallyBasedDeferredIrradiancePipeline attachments.IrradianceAttachment.VkFormat vkc
        let deferredEnvironmentFilterPipeline = createPhysicallyBasedDeferredEnvironmentFilterPipeline attachments.EnvironmentFilterAttachment.VkFormat vkc
        let deferredFoggingPipeline = createPhysicallyBasedDeferredFoggingPipeline attachments.FoggingAttachment.VkFormat vkc
        let deferredColoringPipeline = createPhysicallyBasedDeferredColoringPipeline [|(fst attachments.ColoringAttachments).VkFormat; (snd attachments.ColoringAttachments).VkFormat|] vkc
        let deferredCompositionPipeline = createPhysicallyBasedDeferredCompositionPipeline attachments.CompositionAttachment.VkFormat vkc
        
        // create forward static pipeline
        let composition = attachments.CompositionAttachment
        let forwardStaticPipeline =
            createPhysicallyBasedPipeline
                Constants.Render.LightMapsMaxForward
                Constants.Render.LightsMaxForward
                Constants.Paths.PhysicallyBasedForwardStaticShaderFilePath
                [|VulkanUnblended; VulkanTransparent|]
                [|false; true|]
                staticVertices
                [|composition.VkFormat|]
                (Some z.VkFormat)
                vkc

        // create forward animated pipeline
        let forwardAnimatedPipeline =
            createPhysicallyBasedPipeline
                Constants.Render.LightMapsMaxForward
                Constants.Render.LightsMaxForward
                Constants.Paths.PhysicallyBasedForwardAnimatedShaderFilePath
                [|VulkanUnblended; VulkanTransparent|]
                [|false; true|]
                animatedVertices
                [|composition.VkFormat|]
                (Some z.VkFormat)
                vkc
        
        // create PhysicallyBasedPipelines
        let physicallyBasedPipelines =
            { ShadowStaticPointPipeline = shadowStaticPointPipeline
              ShadowStaticSpotPipeline = shadowStaticSpotPipeline
              ShadowStaticDirectionalPipeline = shadowStaticDirectionalPipeline
              ShadowAnimatedPointPipeline = shadowAnimatedPointPipeline
              ShadowAnimatedSpotPipeline = shadowAnimatedSpotPipeline
              ShadowAnimatedDirectionalPipeline = shadowAnimatedDirectionalPipeline
              DeferredStaticPipeline = deferredStaticPipeline
              DeferredStaticClippedPipeline = deferredStaticClippedPipeline
              DeferredAnimatedPipeline = deferredAnimatedPipeline
              DeferredLightingPipeline = deferredLightingPipeline
              DeferredLightMappingPipeline = deferredLightMappingPipeline
              DeferredAmbientPipeline = deferredAmbientPipeline
              DeferredIrradiancePipeline = deferredIrradiancePipeline
              DeferredEnvironmentFilterPipeline = deferredEnvironmentFilterPipeline
              DeferredFoggingPipeline = deferredFoggingPipeline
              DeferredColoringPipeline = deferredColoringPipeline
              DeferredCompositionPipeline = deferredCompositionPipeline
              ForwardStaticPipeline = forwardStaticPipeline
              ForwardAnimatedPipeline = forwardAnimatedPipeline }

        // fin
        physicallyBasedPipelines

    let beginPhysicallyBasedPipelines physicallyBasedPipelines =
        Pipeline.beginFrame physicallyBasedPipelines.ShadowStaticPointPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.ShadowStaticSpotPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.ShadowStaticDirectionalPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.ShadowAnimatedPointPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.ShadowAnimatedSpotPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.ShadowAnimatedDirectionalPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.DeferredStaticPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.DeferredStaticClippedPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.DeferredAnimatedPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.DeferredLightingPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.DeferredLightMappingPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.DeferredAmbientPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.DeferredIrradiancePipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.DeferredEnvironmentFilterPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.DeferredFoggingPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.DeferredColoringPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.DeferredCompositionPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.ForwardStaticPipeline.Pipeline
        Pipeline.beginFrame physicallyBasedPipelines.ForwardAnimatedPipeline.Pipeline

    let destroyPhysicallyBasedPipelines physicallyBasedPipelines vkc =
        destroyPhysicallyBasedShadowPipeline physicallyBasedPipelines.ShadowStaticPointPipeline vkc
        destroyPhysicallyBasedShadowPipeline physicallyBasedPipelines.ShadowStaticSpotPipeline vkc
        destroyPhysicallyBasedShadowPipeline physicallyBasedPipelines.ShadowStaticDirectionalPipeline vkc
        destroyPhysicallyBasedShadowPipeline physicallyBasedPipelines.ShadowAnimatedPointPipeline vkc
        destroyPhysicallyBasedShadowPipeline physicallyBasedPipelines.ShadowAnimatedSpotPipeline vkc
        destroyPhysicallyBasedShadowPipeline physicallyBasedPipelines.ShadowAnimatedDirectionalPipeline vkc
        destroyPhysicallyBasedPipeline physicallyBasedPipelines.DeferredStaticPipeline vkc
        destroyPhysicallyBasedPipeline physicallyBasedPipelines.DeferredStaticClippedPipeline vkc
        destroyPhysicallyBasedPipeline physicallyBasedPipelines.DeferredAnimatedPipeline vkc
        destroyPhysicallyBasedDeferredLightingPipeline physicallyBasedPipelines.DeferredLightingPipeline vkc
        destroyPhysicallyBasedDeferredLightMappingPipeline physicallyBasedPipelines.DeferredLightMappingPipeline vkc
        destroyPhysicallyBasedDeferredAmbientPipeline physicallyBasedPipelines.DeferredAmbientPipeline vkc
        destroyPhysicallyBasedDeferredIrradiancePipeline physicallyBasedPipelines.DeferredIrradiancePipeline vkc
        destroyPhysicallyBasedDeferredEnvironmentFilterPipeline physicallyBasedPipelines.DeferredEnvironmentFilterPipeline vkc
        destroyPhysicallyBasedDeferredFoggingPipeline physicallyBasedPipelines.DeferredFoggingPipeline vkc
        destroyPhysicallyBasedDeferredColoringPipeline physicallyBasedPipelines.DeferredColoringPipeline vkc
        destroyPhysicallyBasedDeferredCompositionPipeline physicallyBasedPipelines.DeferredCompositionPipeline vkc
        destroyPhysicallyBasedPipeline physicallyBasedPipelines.ForwardStaticPipeline vkc
        destroyPhysicallyBasedPipeline physicallyBasedPipelines.ForwardAnimatedPipeline vkc

    let reloadPhysicallyBasedShaders physicallyBasedPipelines vkc =
        Pipeline.reloadShaders physicallyBasedPipelines.ShadowStaticPointPipeline.Pipeline vkc
        Pipeline.reloadShaders physicallyBasedPipelines.ShadowStaticSpotPipeline.Pipeline vkc
        Pipeline.reloadShaders physicallyBasedPipelines.ShadowStaticDirectionalPipeline.Pipeline vkc
        Pipeline.reloadShaders physicallyBasedPipelines.ShadowAnimatedPointPipeline.Pipeline vkc
        Pipeline.reloadShaders physicallyBasedPipelines.ShadowAnimatedSpotPipeline.Pipeline vkc
        Pipeline.reloadShaders physicallyBasedPipelines.ShadowAnimatedDirectionalPipeline.Pipeline vkc
        Pipeline.reloadShaders physicallyBasedPipelines.DeferredStaticPipeline.Pipeline vkc
        Pipeline.reloadShaders physicallyBasedPipelines.DeferredStaticClippedPipeline.Pipeline vkc
        Pipeline.reloadShaders physicallyBasedPipelines.DeferredAnimatedPipeline.Pipeline vkc
        Pipeline.reloadShaders physicallyBasedPipelines.DeferredLightingPipeline.Pipeline vkc
        Pipeline.reloadShaders physicallyBasedPipelines.DeferredLightMappingPipeline.Pipeline vkc
        Pipeline.reloadShaders physicallyBasedPipelines.DeferredAmbientPipeline.Pipeline vkc
        Pipeline.reloadShaders physicallyBasedPipelines.DeferredIrradiancePipeline.Pipeline vkc
        Pipeline.reloadShaders physicallyBasedPipelines.DeferredEnvironmentFilterPipeline.Pipeline vkc
        Pipeline.reloadShaders physicallyBasedPipelines.DeferredFoggingPipeline.Pipeline vkc
        Pipeline.reloadShaders physicallyBasedPipelines.DeferredColoringPipeline.Pipeline vkc
        Pipeline.reloadShaders physicallyBasedPipelines.DeferredCompositionPipeline.Pipeline vkc
        Pipeline.reloadShaders physicallyBasedPipelines.ForwardStaticPipeline.Pipeline vkc
        Pipeline.reloadShaders physicallyBasedPipelines.ForwardAnimatedPipeline.Pipeline vkc

/// Memoizes physically-based scene loads.
type PhysicallyBasedSceneClient () =

    /// Attempt to create physically-based model from a model file with assimp.
    /// Thread-safe if vkcOpt = None.
    member this.TryCreatePhysicallyBasedModel filePath defaultMaterial textureClient vkcOpt =

        // attempt to import from assimp scene
        match AssimpContext.TryGetScene filePath with
        | Right scene ->
            let dirPath = PathF.GetDirectoryName filePath
            match PhysicallyBased.tryCreatePhysicallyBasedMaterials dirPath defaultMaterial textureClient scene vkcOpt with
            | Right materials ->
                let animated = scene.Animations.Count <> 0
                let geometries =
                    if animated
                    then PhysicallyBased.createPhysicallyBasedAnimatedGeometries scene vkcOpt
                    else PhysicallyBased.createPhysicallyBasedStaticGeometries scene vkcOpt

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