// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Vulkan
open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Numerics
open System.Runtime.InteropServices
open Vortice.Vulkan
open Prime
open Nu

[<Struct; StructLayout(LayoutKind.Explicit)>]
type PhysicallyBasedTransform =
    [<FieldOffset(0)>] val mutable view : Matrix4x4
    [<FieldOffset(64)>] val mutable projection : Matrix4x4
    [<FieldOffset(128)>] val mutable viewProjection : Matrix4x4
    [<FieldOffset(192)>] val mutable viewInverse : Matrix4x4
    [<FieldOffset(256)>] val mutable projectionInverse : Matrix4x4
    [<FieldOffset(320)>] val mutable eyeCenter : Vector3

[<Struct; StructLayout(LayoutKind.Explicit)>]
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
    [<FieldOffset(148)>] val mutable shadowNear : single

[<Struct; StructLayout(LayoutKind.Explicit)>]
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

[<Struct; StructLayout(LayoutKind.Explicit)>]
type Bone =
    [<FieldOffset(0)>] val mutable bone : Matrix4x4

[<Struct; StructLayout(LayoutKind.Explicit)>]
type LightMap =
    [<FieldOffset(0)>] val mutable lightMapOrigins : Vector3
    [<FieldOffset(16)>] val mutable lightMapMins : Vector3
    [<FieldOffset(32)>] val mutable lightMapSizes : Vector3
    [<FieldOffset(48)>] val mutable lightMapAmbientColors : Vector3
    [<FieldOffset(60)>] val mutable lightMapAmbientBrightnesses : single

[<Struct; StructLayout(LayoutKind.Explicit)>]
type LightsGeneral =
    [<FieldOffset(0)>] val mutable lightMapsCount : int
    [<FieldOffset(4)>] val mutable lightMapSingletonBlendMargin : single
    [<FieldOffset(8)>] val mutable lightsCount : int

[<Struct; StructLayout(LayoutKind.Explicit)>]
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
      ColoringAttachments : Texture * Texture
      CompositionAttachments : Texture * Texture }

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

    static member extractSubsurfaceCutoff subsurfaceCutoffDefault (sceneOpt : Assimp.Scene option) surface =
        match surface.SurfaceNode.SubsurfaceCutoffOpt with
        | ValueNone ->
            match sceneOpt with
            | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                let material = scene.Materials.[surface.SurfaceMaterialIndex]
                ValueOption.defaultValue subsurfaceCutoffDefault material.SubsurfaceCutoffOpt
            | Some _ | None -> subsurfaceCutoffDefault
        | ValueSome subsurfaceCutoff -> subsurfaceCutoff

    static member extractSubsurfaceCutoffMargin subsurfaceCutoffMarginDefault (sceneOpt : Assimp.Scene option) surface =
        match surface.SurfaceNode.SubsurfaceCutoffMarginOpt with
        | ValueNone ->
            match sceneOpt with
            | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                let material = scene.Materials.[surface.SurfaceMaterialIndex]
                ValueOption.defaultValue subsurfaceCutoffMarginDefault material.SubsurfaceCutoffMarginOpt
            | Some _ | None -> subsurfaceCutoffMarginDefault
        | ValueSome subsurfaceCutoffMargin -> subsurfaceCutoffMargin

    static member extractRefractiveIndex refractiveIndexDefault (sceneOpt : Assimp.Scene option) surface =
        match surface.SurfaceNode.RefractiveIndexOpt with
        | ValueNone ->
            match sceneOpt with
            | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                let material = scene.Materials.[surface.SurfaceMaterialIndex]
                ValueOption.defaultValue refractiveIndexDefault material.RefractiveIndexOpt
            | Some _ | None -> refractiveIndexDefault
        | ValueSome refractiveIndex -> refractiveIndex

    static member extractClearCoat clearCoatDefault (sceneOpt : Assimp.Scene option) surface =
        match surface.SurfaceNode.ClearCoatOpt with
        | ValueNone ->
            match sceneOpt with
            | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                let material = scene.Materials.[surface.SurfaceMaterialIndex]
                ValueOption.defaultValue clearCoatDefault material.ClearCoatOpt
            | Some _ | None -> clearCoatDefault
        | ValueSome clearCoat -> clearCoat

    static member extractClearCoatRoughness clearCoatRoughnessDefault (sceneOpt : Assimp.Scene option) surface =
        match surface.SurfaceNode.ClearCoatRoughnessOpt with
        | ValueNone ->
            match sceneOpt with
            | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                let material = scene.Materials.[surface.SurfaceMaterialIndex]
                ValueOption.defaultValue clearCoatRoughnessDefault material.ClearCoatRoughnessOpt
            | Some _ | None -> clearCoatRoughnessDefault
        | ValueSome clearCoatRoughness -> clearCoatRoughness

    static member extractNavShape shapeDefault (sceneOpt : Assimp.Scene option) surface =
        match surface.SurfaceNode.NavShapeOpt with
        | ValueNone ->
            match sceneOpt with
            | Some scene when surface.SurfaceMaterialIndex < scene.Materials.Count ->
                let material = scene.Materials.[surface.SurfaceMaterialIndex]
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

/// Describes a physically-based pipeline that's loaded into GPU.
type PhysicallyBasedPipeline =
    { TransformUniform : Nu.Vulkan.Buffer
      LightingUniform : Nu.Vulkan.Buffer
      BoneUniform : Nu.Vulkan.Buffer
      LightMapUniform : Nu.Vulkan.Buffer
      LightsGeneralUniform : Nu.Vulkan.Buffer
      LightUniform : Nu.Vulkan.Buffer
      ShadowMatrixUniform : Nu.Vulkan.Buffer
      Pipeline : Pipeline }

/// Describes the lighting pass of a deferred physically-based pipeline that's loaded into GPU.
type PhysicallyBasedDeferredLightingPipeline =
    { TransformUniform : Nu.Vulkan.Buffer
      LightingUniform : Nu.Vulkan.Buffer
      LightUniform : Nu.Vulkan.Buffer
      ShadowMatrixUniform : Nu.Vulkan.Buffer
      Pipeline : Pipeline }

/// Physically-based pipelines.
type PhysicallyBasedPipelines =
    { DeferredStaticPipeline : PhysicallyBasedPipeline
      DeferredLightingPipeline : PhysicallyBasedDeferredLightingPipeline
      ForwardStaticPipeline : PhysicallyBasedPipeline }

[<RequireQualifiedAccess>]
module PhysicallyBased =
    
    let StaticTexCoordsOffset = (3 (*position*)) * sizeof<single>
    let StaticNormalOffset =    (3 (*position*) + 2 (*tex coords*)) * sizeof<single>
    let StaticVertexSize =      (3 (*position*) + 2 (*tex coords*) + 3 (*normal*)) * sizeof<single>

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
        
        // create coloring attachments
        let coloringAttachments = Attachment.createColoringAttachments geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y vkc

        // create composition attachments
        let compositionAttachments = Attachment.createGeneralAttachments geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y vkc

        // make record
        { ShadowTextureArrayAttachments = shadowTextureArrayAttachments
          ShadowMapAttachmentsArray = shadowMapAttachmentsArray
          ShadowCascadeArrayAttachmentsArray = shadowCascadeArrayAttachmentsArray
          GeometryAttachments = geometryAttachments
          LightingAttachment = lightingAttachment
          ColoringAttachments = coloringAttachments
          CompositionAttachments = compositionAttachments }

    /// Update the size of the attachments. Must be used every frame.
    let updatePhysicallyBasedAttachmentsSize (geometryViewport : Viewport) (attachments : PhysicallyBasedAttachments) vkc =
        Attachment.updateShadowTextureArrayAttachmentsSize geometryViewport.ShadowTextureResolution.X geometryViewport.ShadowTextureResolution.Y attachments.ShadowTextureArrayAttachments vkc
        for i in 0 .. dec attachments.ShadowMapAttachmentsArray.Length do
            Attachment.updateShadowMapAttachmentsSize geometryViewport.ShadowMapResolution.X geometryViewport.ShadowMapResolution.Y attachments.ShadowMapAttachmentsArray[i] vkc
        for i in 0 .. dec attachments.ShadowCascadeArrayAttachmentsArray.Length do
            Attachment.updateShadowCascadeArrayAttachmentsSize geometryViewport.ShadowCascadeResolution.X geometryViewport.ShadowCascadeResolution.Y attachments.ShadowCascadeArrayAttachmentsArray.[i] vkc
        Attachment.updateGeometryAttachmentsSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.GeometryAttachments vkc
        Attachment.updateLightingAttachmentSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.LightingAttachment vkc
        Attachment.updateColoringAttachmentsSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.ColoringAttachments vkc
        Attachment.updateGeneralAttachmentsSize geometryViewport.Bounds.Size.X geometryViewport.Bounds.Size.Y attachments.CompositionAttachments vkc
    
    /// Destroy the physically-based attachments.
    let destroyPhysicallyBasedAttachments (attachments : PhysicallyBasedAttachments) vkc =
        Attachment.destroyShadowTextureArrayAttachments attachments.ShadowTextureArrayAttachments vkc
        for i in 0 .. dec attachments.ShadowMapAttachmentsArray.Length do
            Attachment.destroyShadowMapAttachments attachments.ShadowMapAttachmentsArray.[i] vkc
        for i in 0 .. dec attachments.ShadowCascadeArrayAttachmentsArray.Length do
            Attachment.destroyShadowCascadeArrayAttachments attachments.ShadowCascadeArrayAttachmentsArray.[i] vkc
        Attachment.destroyGeometryAttachments attachments.GeometryAttachments vkc
        Attachment.destroyLightingAttachment attachments.LightingAttachment vkc
        Attachment.destroyColoringAttachments attachments.ColoringAttachments vkc
        Attachment.destroyGeneralAttachments attachments.CompositionAttachments vkc
    
    /// Create physically-based material from an assimp mesh, falling back on defaults in case of missing textures.
    /// Uses file name-based inferences to look for texture files in case the ones that were hard-coded in the model
    /// files can't be located.
    /// Thread-safe if vkcOpt = None.
    let createPhysicallyBasedMaterial vkcOpt dirPath defaultMaterial (textureClient : TextureClient) (material : Assimp.Material) =

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
                let possibleFilePath = possibleFilePaths.[i]
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

    /// Create physically-based static mesh from an assimp mesh.
    let createPhysicallyBasedStaticMesh indexData (mesh : Assimp.Mesh) =

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
    let createPhysicallyBasedAnimatedMesh indexData (mesh : Assimp.Mesh) =

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

    /// Create physically-based static geometry from a mesh.
    let createPhysicallyBasedStaticGeometry vkcOpt primitiveTopology (vertexData : single Memory) (indexData : int Memory) bounds =

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
                    let vertex = v3 vertexData.[j] vertexData.[j+1] vertexData.[j+2]
                    vertices.[i] <- vertex

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

    /// Create physically-based static geometry from an assimp mesh.
    let createPhysicallyBasedStaticGeometryFromMesh vkcOpt indexData (mesh : Assimp.Mesh) =
        match createPhysicallyBasedStaticMesh indexData mesh with
        | (vertexData, indexData, bounds) -> createPhysicallyBasedStaticGeometry vkcOpt VkPrimitiveTopology.TriangleList (vertexData.AsMemory ()) (indexData.AsMemory ()) bounds
    
    /// Create physically-based animated geometry from a mesh.
    let createPhysicallyBasedAnimatedGeometry vkcOpt primitiveTopology (vertexData : single Memory) (indexData : int Memory) bounds =

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
                    let vertex = v3 vertexData.[j] vertexData.[j+1] vertexData.[j+2]
                    vertices.[i] <- vertex

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
    let createPhysicallyBasedAnimatedGeometryFromMesh vkcOpt indexData (mesh : Assimp.Mesh) =
        match createPhysicallyBasedAnimatedMesh indexData mesh with
        | (vertexData, indexData, bounds) -> createPhysicallyBasedAnimatedGeometry vkcOpt VkPrimitiveTopology.TriangleList (vertexData.AsMemory ()) (indexData.AsMemory ()) bounds
    
    /// Attempt to create physically-based material from an assimp scene.
    /// Thread-safe if vkcOpt = None.
    let tryCreatePhysicallyBasedMaterials vkcOpt dirPath defaultMaterial textureClient (scene : Assimp.Scene) =
        let mutable errorOpt = None
        let propertiesAndMaterials = Array.zeroCreate scene.Materials.Count
        for i in 0 .. dec scene.Materials.Count do
            if Option.isNone errorOpt then
                let (properties, material) = createPhysicallyBasedMaterial vkcOpt dirPath defaultMaterial textureClient scene.Materials.[i]
                propertiesAndMaterials.[i] <- (properties, material)
        match errorOpt with
        | Some error -> Left error
        | None -> Right propertiesAndMaterials

    /// Create physically-based static geometries from an assimp scene.
    /// OPTIMIZATION: duplicate geometry is detected and deduplicated here, which does have some run-time cost.
    let createPhysicallyBasedStaticGeometries vkcOpt (scene : Assimp.Scene) =
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
                let geometry = createPhysicallyBasedStaticGeometryFromMesh vkcOpt indexData mesh
                match meshAndGeometryListOpt with
                | Some meshesAndGeometries -> meshesAndGeometries.Add (mesh, geometry)
                | None -> meshAndGeometryLists.[(mesh.VertexCount, mesh.FaceCount, mesh.BoundingBox)] <- List [(mesh, geometry)]
                geometries.Add geometry
        geometries

    /// Create physically-based animated geometries from an assimp scene.
    /// TODO: consider deduplicating geometry like in CreatePhysicallyBasedStaticGeometries?
    let createPhysicallyBasedAnimatedGeometries vkcOpt (scene : Assimp.Scene) =
        let geometries = SList.make ()
        for i in 0 .. dec scene.Meshes.Count do
            let indexDataEntry = scene.Metadata.["IndexData" + string i]
            let indexData = indexDataEntry.Data :?> int array
            let mesh = scene.Meshes.[i]
            let geometry = createPhysicallyBasedAnimatedGeometryFromMesh vkcOpt indexData mesh
            geometries.Add geometry
        geometries

    /// Create a physically-based pipeline.
    let createPhysicallyBasedPipeline lightMapsMax lightsMax shaderPath blends cullModes vertexBindings colorAttachmentFormats depthTestOpt vkc =

        // create set 0 uniform buffers
        let transformUniform = Buffer.create sizeof<Transform> Storage vkc
        let lightingUniform = Buffer.create sizeof<Lighting> Storage vkc
        
        // create set 2 uniform buffers
        let shadowMatrixMax = Constants.Render.ShadowTexturesMax + Constants.Render.ShadowCascadesMax * Constants.Render.ShadowCascadeLevels
        let boneUniform = Buffer.create (Constants.Render.BonesMax * sizeof<Bone>) Storage vkc
        let lightMapUniform = Buffer.create (lightMapsMax * sizeof<LightMap>) Storage vkc
        let lightsGeneralUniform = Buffer.create sizeof<LightsGeneral> Storage vkc
        let lightUniform = Buffer.create (lightsMax * sizeof<Light>) Storage vkc
        let shadowMatrixUniform = Buffer.create (shadowMatrixMax * sizeof<Matrix4x4>) Storage vkc

        // create pipeline
        let pipeline =
            Pipeline.create
                shaderPath blends cullModes vertexBindings
                
                // descriptor set 0: per render pass
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 StorageBuffer VertexFragmentStage 1 // transform
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
                [|transformUniform
                  lightingUniform
                  boneUniform
                  lightMapUniform
                  lightsGeneralUniform
                  lightUniform
                  shadowMatrixUniform|]
                vkc

        // make PhysicallyBasedPipeline
        let physicallyBasedPipeline =
            { TransformUniform = transformUniform
              LightingUniform = lightingUniform
              BoneUniform = boneUniform
              LightMapUniform = lightMapUniform
              LightsGeneralUniform = lightsGeneralUniform
              LightUniform = lightUniform
              ShadowMatrixUniform = shadowMatrixUniform
              Pipeline = pipeline }
        
        // fin
        physicallyBasedPipeline
    
    /// Destroy PhysicallyBasedPipeline.
    let destroyPhysicallyBasedPipeline (physicallyBasedPipeline : PhysicallyBasedPipeline) vkc =
        Pipeline.destroy physicallyBasedPipeline.Pipeline vkc
    
    /// Create a PhysicallyBasedDeferredLightingPipeline.
    let createPhysicallyBasedDeferredLightingPipeline lightsMax colorAttachmentFormat vkc =

        // create uniform buffers
        let shadowMatrixMax = Constants.Render.ShadowTexturesMax + Constants.Render.ShadowCascadesMax * Constants.Render.ShadowCascadeLevels
        let transformUniform = Buffer.create sizeof<Transform> Storage vkc
        let lightingUniform = Buffer.create sizeof<Lighting2> Storage vkc
        let lightUniform = Buffer.create (lightsMax * sizeof<Light>) Storage vkc
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
                    [|Pipeline.descriptor 0 StorageBuffer FragmentStage 1 // transform
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
                [||] colorAttachmentFormat None
                [|transformUniform; lightingUniform; lightUniform; shadowMatrixUniform|]
                vkc

        // make PhysicallyBasedDeferredLightingPipeline
        let physicallyBasedDeferredLightingPipeline =
            { TransformUniform = transformUniform
              LightingUniform = lightingUniform
              LightUniform = lightUniform
              ShadowMatrixUniform = shadowMatrixUniform
              Pipeline = pipeline }
        
        // fin
        physicallyBasedDeferredLightingPipeline
    
    /// Destroy PhysicallyBasedDeferredLightingPipeline.
    let destroyPhysicallyBasedDeferredLightingPipeline (pipeline : PhysicallyBasedDeferredLightingPipeline) vkc =
        Pipeline.destroy pipeline.Pipeline vkc

    /// Draw a batch of physically-based deferred surfaces.
    let beginPhysicallyBasedDeferredPipeline
        (view : Matrix4x4)
        (projection : Matrix4x4)
        (viewProjection : Matrix4x4)
        (eyeCenter : Vector3)
        (filteredSampler : Sampler)
        (renderPassIndex : int)
        (pipeline : PhysicallyBasedPipeline)
        (vkc : VulkanContext) =

        // specify tranform
        let mutable transformDescriptorSet = Pipeline.specifyDescriptorSet 0 renderPassIndex pipeline.Pipeline vkc $ fun vkSet ->
            let mutable transform = PhysicallyBasedTransform ()
            transform.view <- view
            transform.projection <- projection
            transform.viewProjection <- viewProjection
            transform.viewInverse <- view.Inverted
            transform.projectionInverse <- projection.Inverted
            transform.eyeCenter <- eyeCenter
            Buffer.uploadValue transform pipeline.TransformUniform vkc
            Pipeline.writeDescriptorStorageBuffer 0 0 pipeline.TransformUniform vkSet vkc

        // specify samplers
        let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 3 Unit pipeline.Pipeline vkc $ fun vkSet ->
            Pipeline.writeDescriptorSampler 0 0 filteredSampler vkSet vkc

        // fin
        (transformDescriptorSet, samplerDescriptorSet)

    /// Draw a batch of physically-based deferred surfaces.
    let drawPhysicallyBasedDeferredSurfaces
        (bones : Matrix4x4 array)
        (surfacesCount : int)
        (instanceFields : single array)
        (material : PhysicallyBasedMaterial)
        (geometry : PhysicallyBasedGeometry)
        (viewport : Viewport)
        (colorAttachments : VkImageView array)
        (depthAttachment : Texture)
        (transformDescriptorSet : VkDescriptorSet)
        (samplerDescriptorSet : VkDescriptorSet)
        (pipeline : PhysicallyBasedPipeline)
        (vkc : VulkanContext) =

        // only draw if render area is valid
        let mutable renderArea = VkRect2D (0, 0, uint viewport.Bounds.Size.X, uint viewport.Bounds.Size.Y)
        let mutable vkViewport = Hl.makeViewport true renderArea
        if Hl.validateRect renderArea then

            // only set up when there is a surface to render to avoid potentially utilizing destroyed textures
            if surfacesCount > 0 then

                // only draw if required vkPipeline exists
                match Pipeline.tryGetVkPipeline VulkanUnblended (not material.TwoSided) pipeline.Pipeline with
                | Some vkPipeline ->

                    // specify instancing
                    use instanceFieldsPin = new ArrayPin<_> (instanceFields)
                    Buffer.uploadSubdata 0 0 (Constants.Render.InstanceFieldCount * sizeof<single>) surfacesCount instanceFieldsPin.NativeInt geometry.InstanceBuffer vkc

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
                        Buffer.uploadSubdata 0 0 sizeof<Bone> (min bones.Length Constants.Render.BonesMax) bonesPin.NativeInt pipeline.BoneUniform vkc
                        Pipeline.writeDescriptorStorageBuffer 0 0 pipeline.BoneUniform vkSet vkc

                    // set up render
                    let mutable rendering = Hl.makeRenderingInfo colorAttachments (Some depthAttachment.ImageView) renderArea None
                    Vulkan.vkCmdBeginRendering (vkc.RenderCommandBuffer, asPointer &rendering)
                    Vulkan.vkCmdBindPipeline (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)
                    Vulkan.vkCmdSetViewport (vkc.RenderCommandBuffer, 0u, 1u, asPointer &vkViewport)
                    Vulkan.vkCmdSetScissor (vkc.RenderCommandBuffer, 0u, 1u, asPointer &renderArea)
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
                    let mutable (transformDescriptorSet, samplerDescriptorSet) = (transformDescriptorSet, samplerDescriptorSet)
                    Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, asPointer &transformDescriptorSet, 0u, nullPtr)
                    Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, asPointer &materialDescriptorSet, 0u, nullPtr)
                    Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 2u, 1u, asPointer &dynamicDescriptorSet, 0u, nullPtr)
                    Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 3u, 1u, asPointer &samplerDescriptorSet, 0u, nullPtr)

                    // draw
                    Vulkan.vkCmdDrawIndexed (vkc.RenderCommandBuffer, uint geometry.ElementCount, uint surfacesCount, 0u, 0, 0u)

                    // tear down render
                    Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer
                    
                    // advance instancing
                    geometry.InstanceBuffer.Advance ()

                    // advance pipeline
                    pipeline.Pipeline.Advance surfacesCount

                // abort
                | None -> Log.warnOnce "Cannot draw because VkPipeline does not exist."

    /// Begin the process of drawing with a forward pipeline.
    let beginPhysicallyBasedForwardPipeline
        (view : Matrix4x4)
        (projection : Matrix4x4)
        (viewProjection : Matrix4x4)
        (eyeCenter : Vector3)
        (viewInverse : Matrix4x4)
        (projectionInverse : Matrix4x4)
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
        (shadowNear : single)
        (renderPassIndex : int)
        (pipeline : PhysicallyBasedPipeline)
        (vkc : VulkanContext) =

        // specify uniforms
        let mutable uniformsDescriptorSet = Pipeline.specifyDescriptorSet 0 renderPassIndex pipeline.Pipeline vkc $ fun vkSet ->

            // specify transform
            let mutable transform = PhysicallyBasedTransform ()
            transform.view <- view
            transform.projection <- projection
            transform.viewProjection <- viewProjection
            transform.viewInverse <- viewInverse
            transform.projectionInverse <- projectionInverse
            transform.eyeCenter <- eyeCenter
            Buffer.uploadValue transform pipeline.TransformUniform vkc
            Pipeline.writeDescriptorStorageBuffer 0 0 pipeline.TransformUniform vkSet vkc

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

        // fin
        (uniformsDescriptorSet, samplersDescriptorSet)

    /// End the process of drawing with a deferred pipeline.
    let endPhysicallyBasedDeferredPipeline (_ : PhysicallyBasedPipeline) =
        () // nothing to do

    /// Draw a batch of physically-based forward surfaces.
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
        (viewport : Viewport)
        (colorAttachment : Texture)
        (depthAttachment : Texture)
        (uniformsDescriptorSet : VkDescriptorSet)
        (samplersDescriptorSet : VkDescriptorSet)
        (pipeline : PhysicallyBasedPipeline)
        (vkc : VulkanContext) =
        
        // only draw if render area is valid
        let mutable renderArea = VkRect2D (0, 0, uint viewport.Bounds.Size.X, uint viewport.Bounds.Size.Y)
        let mutable vkViewport = Hl.makeViewport true renderArea
        if Hl.validateRect renderArea then

            // only draw if required vkPipeline exists
            let blend = if blending then VulkanTransparent else VulkanUnblended
            match Pipeline.tryGetVkPipeline blend (not material.TwoSided) pipeline.Pipeline with
            | Some vkPipeline ->

                // specify instancing
                use instanceFieldsPin = new ArrayPin<_> (instanceFields)
                Buffer.uploadSubdata 0 0 (Constants.Render.InstanceFieldCount * sizeof<single>) surfacesCount instanceFieldsPin.NativeInt geometry.InstanceBuffer vkc

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
                    Buffer.uploadSubdata 0 0 sizeof<Bone> (min bones.Length Constants.Render.BonesMax) bonesPin.NativeInt pipeline.BoneUniform vkc
                    Pipeline.writeDescriptorStorageBuffer 0 0 pipeline.BoneUniform vkSet vkc

                    // specify light maps
                    for i in 0 .. dec Constants.Render.LightMapsMaxForward do
                        let mutable lightMap = LightMap ()
                        if lightMapOrigins.Length > i then
                            lightMap.lightMapOrigins <- lightMapOrigins.[i]
                            lightMap.lightMapMins <- lightMapMins.[i]
                            lightMap.lightMapSizes <- lightMapSizes.[i]
                            lightMap.lightMapAmbientColors <- lightMapAmbientColors.[i].V3
                            lightMap.lightMapAmbientBrightnesses <- lightMapAmbientBrightnesses.[i]
                        use lightMapPin = new ArrayPin<_> ([|lightMap|])
                        Buffer.uploadSubdata (i * sizeof<LightMap>) 0 sizeof<LightMap> 1 lightMapPin.NativeInt pipeline.LightMapUniform vkc
                    Pipeline.writeDescriptorStorageBuffer 1 0 pipeline.LightMapUniform vkSet vkc

                    // specify lights general
                    let mutable lightsGeneral = LightsGeneral ()
                    lightsGeneral.lightMapsCount <- lightMapsCount
                    lightsGeneral.lightMapSingletonBlendMargin <- lightMapSingletonBlendMargin
                    lightsGeneral.lightsCount <- lightsCount
                    Buffer.uploadValue lightsGeneral pipeline.LightsGeneralUniform vkc
                    Pipeline.writeDescriptorStorageBuffer 2 0 pipeline.LightsGeneralUniform vkSet vkc
                    
                    // specify lights
                    for i in 0 .. dec Constants.Render.LightsMaxForward do
                        let mutable light = Light ()
                        if lightOrigins.Length < i then
                            light.lightOrigins <- lightOrigins.[i]
                            light.lightDirections <- lightDirections.[i]
                            light.lightColors <- lightColors.[i].V3
                            light.lightBrightnesses <- lightBrightnesses.[i]
                            light.lightAttenuationLinears <- lightAttenuationLinears.[i]
                            light.lightAttenuationQuadratics <- lightAttenuationQuadratics.[i]
                            light.lightCutoffs <- lightCutoffs.[i]
                            light.lightTypes <- lightTypes.[i]
                            light.lightConeInners <- lightConeInners.[i]
                            light.lightConeOuters <- lightConeOuters.[i]
                            light.lightDesireFogs <- lightDesireFogs.[i]
                            light.lightShadowIndices <- lightShadowIndices.[i]
                        use lightPin = new ArrayPin<_> ([|light|])
                        Buffer.uploadSubdata (i * sizeof<Light>) 0 sizeof<Light> 1 lightPin.NativeInt pipeline.LightUniform vkc
                    Pipeline.writeDescriptorStorageBuffer 3 0 pipeline.LightUniform vkSet vkc

                    // specify shadow matrices
                    for i in 0 .. dec (Constants.Render.ShadowTexturesMax + Constants.Render.ShadowCascadesMax * Constants.Render.ShadowCascadeLevels) do
                        let mutable shadowMatrix = m4Zero
                        let shadowMatrixPin = new ArrayPin<_> ([|shadowMatrix|])
                        if shadowMatrices.Length < i then shadowMatrix <- shadowMatrices.[i]
                        Buffer.uploadSubdata (i * sizeof<Matrix4x4>) 0 sizeof<Matrix4x4> 1 shadowMatrixPin.NativeInt pipeline.ShadowMatrixUniform vkc
                    Pipeline.writeDescriptorStorageBuffer 4 0 pipeline.ShadowMatrixUniform vkSet vkc

                    // specify dynamics environment textures
                    Pipeline.writeDescriptorSampledImages 5 0 irradianceMaps vkSet vkc
                    Pipeline.writeDescriptorSampledImages 6 0 environmentFilterMaps vkSet vkc
                    Pipeline.writeDescriptorSampledImage 7 0 shadowTextureArray vkSet vkc
                    Pipeline.writeDescriptorSampledImages 8 0 shadowMaps vkSet vkc
                    Pipeline.writeDescriptorSampledImages 9 0 shadowCascades vkSet vkc
                
                // set up render
                let mutable rendering = Hl.makeRenderingInfo [|colorAttachment.ImageView|] (Some depthAttachment.ImageView) renderArea None
                Vulkan.vkCmdBeginRendering (vkc.RenderCommandBuffer, asPointer &rendering)
                Vulkan.vkCmdBindPipeline (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)
                Vulkan.vkCmdSetViewport (vkc.RenderCommandBuffer, 0u, 1u, asPointer &vkViewport)
                Vulkan.vkCmdSetScissor (vkc.RenderCommandBuffer, 0u, 1u, asPointer &renderArea)
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

                // tear down render
                Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer

                // advance instancing
                geometry.InstanceBuffer.Advance ()

                // advance pipeline
                pipeline.Pipeline.Advance surfacesCount

            // abort
            | None -> Log.warnOnce "Cannot draw because VkPipeline does not exist."

    /// End the process of drawing with a forward pipeline.
    let endPhysicallyBasedForwardPipeline (_ : PhysicallyBasedPipeline) =
        () // nothing to do

    /// Draw the lighting pass of a deferred physically-based surface.
    let drawPhysicallyBasedDeferredLightingSurface
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (viewInverse : Matrix4x4)
        (projection : Matrix4x4)
        (projectionInverse : Matrix4x4)
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
        (lightShadowIndices : int array)
        (lightsCount : int)
        (shadowNear : single)
        (shadowMatrices : single array array)
        (renderPassIndex : int)
        (geometry : PhysicallyBasedGeometry)
        (pipeline : PhysicallyBasedDeferredLightingPipeline)
        (vkc : VulkanContext) =

    //    // bind uniforms
    //    let mutable transform = Transform ()
    //    transform.view <- view
    //    transform.projection <- projection
    //    transform.viewInverse <- viewInverse
    //    transform.projectionInverse <- projectionInverse
    //    transform.eyeCenter <- eyeCenter
    //    Buffer.uploadValue renderPassIndex 0 0 transform pipeline.TransformUniform vkc
    //    Pipeline.writeDescriptorStorageBuffer 0 0 renderPassIndex 0 pipeline.TransformUniform.[renderPassIndex] pipeline.Pipeline vkc
    //    let mutable lighting = Lighting2 ()
    //    lighting.lightCutoffMargin <- lightCutoffMargin
    //    lighting.lightShadowSamples <- lightShadowSamples
    //    lighting.lightShadowBias <- lightShadowBias
    //    lighting.lightShadowSampleScalar <- lightShadowSampleScalar
    //    lighting.lightShadowExponent <- lightShadowExponent
    //    lighting.lightShadowDensity <- lightShadowDensity
    //    lighting.sssEnabled <- sssEnabled
    //    lighting.lightsCount <- lightsCount
    //    lighting.shadowNear <- shadowNear
    //    Buffer.uploadValue renderPassIndex 0 0 lighting pipeline.LightingUniform vkc
    //    Pipeline.writeDescriptorStorageBuffer 0 1 renderPassIndex 0 pipeline.LightingUniform.[renderPassIndex] pipeline.Pipeline vkc
    //
    //
    //    Gl.Uniform1 (shader.DepthTextureUniform, 0)
    //    Gl.Uniform1 (shader.AlbedoTextureUniform, 1)
    //    Gl.Uniform1 (shader.MaterialTextureUniform, 2)
    //    Gl.Uniform1 (shader.NormalPlusTextureUniform, 3)
    //    Gl.Uniform1 (shader.SubdermalPlusTextureUniform, 4)
    //    Gl.Uniform1 (shader.ScatterPlusTextureUniform, 5)
    //    Gl.Uniform1 (shader.ClearCoatPlusTextureUniform, 6)
    //    Gl.Uniform1 (shader.ShadowTexturesUniform, 7)
    //    for i in 0 .. dec Constants.Render.ShadowMapsMax do
    //        Gl.Uniform1 (shader.ShadowMapsUniforms[i], i + 8)
    //    for i in 0 .. dec Constants.Render.ShadowCascadesMax do
    //        Gl.Uniform1 (shader.ShadowCascadesUniforms[i], i + 8 + Constants.Render.ShadowMapsMax)
    //    for i in 0 .. dec (min lightOrigins.Length Constants.Render.LightsMaxDeferred) do
    //        Gl.Uniform3 (shader.LightOriginsUniforms[i], lightOrigins[i].X, lightOrigins[i].Y, lightOrigins[i].Z)
    //    for i in 0 .. dec (min lightDirections.Length Constants.Render.LightsMaxDeferred) do
    //        Gl.Uniform3 (shader.LightDirectionsUniforms[i], lightDirections[i].X, lightDirections[i].Y, lightDirections[i].Z)
    //    for i in 0 .. dec (min lightColors.Length Constants.Render.LightsMaxDeferred) do
    //        Gl.Uniform3 (shader.LightColorsUniforms[i], lightColors[i].R, lightColors[i].G, lightColors[i].B)
    //    for i in 0 .. dec (min lightBrightnesses.Length Constants.Render.LightsMaxDeferred) do
    //        Gl.Uniform1 (shader.LightBrightnessesUniforms[i], lightBrightnesses[i])
    //    for i in 0 .. dec (min lightAttenuationLinears.Length Constants.Render.LightsMaxDeferred) do
    //        Gl.Uniform1 (shader.LightAttenuationLinearsUniforms[i], lightAttenuationLinears[i])
    //    for i in 0 .. dec (min lightAttenuationQuadratics.Length Constants.Render.LightsMaxDeferred) do
    //        Gl.Uniform1 (shader.LightAttenuationQuadraticsUniforms[i], lightAttenuationQuadratics[i])
    //    for i in 0 .. dec (min lightCutoffs.Length Constants.Render.LightsMaxDeferred) do
    //        Gl.Uniform1 (shader.LightCutoffsUniforms[i], lightCutoffs[i])
    //    for i in 0 .. dec (min lightTypes.Length Constants.Render.LightsMaxDeferred) do
    //        Gl.Uniform1 (shader.LightTypesUniforms[i], lightTypes[i])
    //    for i in 0 .. dec (min lightConeInners.Length Constants.Render.LightsMaxDeferred) do
    //        Gl.Uniform1 (shader.LightConeInnersUniforms[i], lightConeInners[i])
    //    for i in 0 .. dec (min lightConeOuters.Length Constants.Render.LightsMaxDeferred) do
    //        Gl.Uniform1 (shader.LightConeOutersUniforms[i], lightConeOuters[i])
    //    for i in 0 .. dec (min lightShadowIndices.Length Constants.Render.LightsMaxDeferred) do
    //        Gl.Uniform1 (shader.LightShadowIndicesUniforms[i], lightShadowIndices[i])
    //    for i in 0 .. dec (min shadowMatrices.Length (Constants.Render.ShadowTexturesMax + Constants.Render.ShadowCascadesMax * Constants.Render.ShadowCascadeLevels)) do
    //        Gl.UniformMatrix4 (shader.ShadowMatricesUniforms[i], false, shadowMatrices[i])
    //
    //    // setup textures
    //    Gl.ActiveTexture TextureUnit.Texture0
    //    Gl.BindTexture (TextureTarget.Texture2d, depthTexture.TextureId)
    //    Gl.ActiveTexture TextureUnit.Texture1
    //    Gl.BindTexture (TextureTarget.Texture2d, albedoTexture.TextureId)
    //    Gl.ActiveTexture TextureUnit.Texture2
    //    Gl.BindTexture (TextureTarget.Texture2d, materialTexture.TextureId)
    //    Gl.ActiveTexture TextureUnit.Texture3
    //    Gl.BindTexture (TextureTarget.Texture2d, normalPlusTexture.TextureId)
    //    Gl.ActiveTexture TextureUnit.Texture4
    //    Gl.BindTexture (TextureTarget.Texture2d, subdermalPlusTexture.TextureId)
    //    Gl.ActiveTexture TextureUnit.Texture5
    //    Gl.BindTexture (TextureTarget.Texture2d, scatterPlusTexture.TextureId)
    //    Gl.ActiveTexture TextureUnit.Texture6
    //    Gl.BindTexture (TextureTarget.Texture2d, clearCoatPlusTexture.TextureId)
    //    Gl.ActiveTexture (int TextureUnit.Texture0 + 7 |> Branchless.reinterpret)
    //    Gl.BindTexture (TextureTarget.Texture2dArray, shadowTextureArray.TextureId)
    //    for i in 0 .. dec (min shadowMaps.Length Constants.Render.ShadowMapsMax) do
    //        Gl.ActiveTexture (int TextureUnit.Texture0 + 8 + i |> Branchless.reinterpret)
    //        Gl.BindTexture (TextureTarget.TextureCubeMap, shadowMaps[i].TextureId)
    //    for i in 0 .. dec (min shadowCascades.Length Constants.Render.ShadowCascadesMax) do
    //        Gl.ActiveTexture (int TextureUnit.Texture0 + 8 + i + Constants.Render.ShadowMapsMax |> Branchless.reinterpret)
    //        Gl.BindTexture (TextureTarget.Texture2dArray, shadowCascades[i].TextureId)
    //    Hl.Assert ()
    //
    //    // setup geometry
    //    Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, StaticVertexSize)
    //    Gl.VertexArrayVertexBuffer (vao, 1u, geometry.InstanceBuffer, 0, Constants.Render.InstanceFieldCount * sizeof<single>)
    //    Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
    //    Hl.Assert ()
    //
    //    // draw geometry
    //    Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.reportDrawCall 1

    /// Destroy physically-based geometry resources.
    let destroyPhysicallyBasedGeometry geometry vkc =
        Buffer.destroy geometry.VertexBuffer vkc
        Buffer.destroy geometry.InstanceBuffer vkc
        Buffer.destroy geometry.IndexBuffer vkc

    /// Destroy physically-based model resources.
    let destroyPhysicallyBasedModel (model : PhysicallyBasedModel) vkc =
        for surface in model.Surfaces do
            destroyPhysicallyBasedGeometry surface.PhysicallyBasedGeometry vkc

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
        
        // create deferred static pipeline
        // NOTE: DJL: we use the composition z attachment directly to avoid having to find a depth format supporting copy operations,
        // which is problematic on some mesa drivers.
        let (depth, albedo, material, normalPlus, subdermalPlus, scatterPlus, clearCoatPlus, _) = attachments.GeometryAttachments
        let (composition, compositionZ) = attachments.CompositionAttachments
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
                (Some compositionZ.VkFormat)
                vkc
        
        // create deferred lighting pipeline
        let deferredLightingPipeline = createPhysicallyBasedDeferredLightingPipeline lightsMax [|attachments.LightingAttachment.VkFormat|] vkc
        
        // create forward static pipeline
        let forwardStaticPipeline =
            createPhysicallyBasedPipeline
                Constants.Render.LightMapsMaxForward
                Constants.Render.LightsMaxForward
                Constants.Paths.PhysicallyBasedForwardStaticShaderFilePath
                [|VulkanUnblended; VulkanTransparent|]
                [|false; true|]
                staticVertices
                [|composition.VkFormat|]
                (Some compositionZ.VkFormat)
                vkc
        
        // create PhysicallyBasedPipelines
        let physicallyBasedPipelines =
            { DeferredStaticPipeline = deferredStaticPipeline
              DeferredLightingPipeline = deferredLightingPipeline
              ForwardStaticPipeline = forwardStaticPipeline }

        // fin
        physicallyBasedPipelines
    
    let reloadPhysicallyBasedShaders physicallyBasedPipelines vkc =
        Pipeline.reloadShaders physicallyBasedPipelines.DeferredStaticPipeline.Pipeline vkc
        Pipeline.reloadShaders physicallyBasedPipelines.DeferredLightingPipeline.Pipeline vkc
        Pipeline.reloadShaders physicallyBasedPipelines.ForwardStaticPipeline.Pipeline vkc
    
    let destroyPhysicallyBasedPipelines physicallyBasedPipelines vkc =
        destroyPhysicallyBasedPipeline physicallyBasedPipelines.DeferredStaticPipeline vkc
        destroyPhysicallyBasedDeferredLightingPipeline physicallyBasedPipelines.DeferredLightingPipeline vkc
        destroyPhysicallyBasedPipeline physicallyBasedPipelines.ForwardStaticPipeline vkc

/// Memoizes physically-based scene loads.
type PhysicallyBasedSceneClient () =

    /// Attempt to create physically-based model from a model file with assimp.
    /// Thread-safe if vkcOpt = None.
    member this.TryCreatePhysicallyBasedModel vkcOpt filePath defaultMaterial textureClient =

        // attempt to import from assimp scene
        match AssimpContext.TryGetScene filePath with
        | Right scene ->
            let dirPath = PathF.GetDirectoryName filePath
            match PhysicallyBased.tryCreatePhysicallyBasedMaterials vkcOpt dirPath defaultMaterial textureClient scene with
            | Right materials ->
                let animated = scene.Animations.Count <> 0
                let geometries =
                    if animated
                    then PhysicallyBased.createPhysicallyBasedAnimatedGeometries vkcOpt scene
                    else PhysicallyBased.createPhysicallyBasedStaticGeometries vkcOpt scene

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