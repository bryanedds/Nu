// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open SDL2
open Prime

/// A layer from which a 3d terrain's material is composed.
type TerrainLayer =
    { AlbedoImage : Image AssetTag
      RoughnessImage : Image AssetTag
      AmbientOcclusionImage : Image AssetTag
      NormalImage : Image AssetTag
      HeightImage : Image AssetTag }

/// Blend-weights for a 3d terrain.
type BlendMap =
    | RgbaMap of Image AssetTag
    | RedsMap of Image AssetTag array

/// A material as projected from images to a 3d terrain.
type FlatMaterial =
    { AlbedoImage : Image AssetTag
      RoughnessImage : Image AssetTag
      AmbientOcclusionImage : Image AssetTag
      NormalImage : Image AssetTag
      HeightImage : Image AssetTag }

/// Blend-weighted material for a 3d terrain.
type BlendMaterial =
    { TerrainLayers : TerrainLayer array
      BlendMap : BlendMap }

/// Describes the material of which a 3d terrain is composed.
type TerrainMaterial =
    | FlatMaterial of FlatMaterial
    | BlendMaterial of BlendMaterial

/// Material properties for terrain surfaces.
type [<SymbolicExpansion>] TerrainMaterialProperties =
    { AlbedoOpt : Color option
      RoughnessOpt : single option
      AmbientOcclusionOpt : single option
      HeightOpt : single option
      IgnoreLightMapsOpt : bool option }

/// TerrainMaterialProperties functions.
[<RequireQualifiedAccess>]
module TerrainMaterialProperties =

    let defaultProperties =
        { AlbedoOpt = Some Constants.Render.AlbedoDefault
          RoughnessOpt = Some Constants.Render.RoughnessDefault
          AmbientOcclusionOpt = Some Constants.Render.AmbientOcclusionDefault
          HeightOpt = Some Constants.Render.HeightDefault
          IgnoreLightMapsOpt = Some false }

    let empty =
        { AlbedoOpt = None
          RoughnessOpt = None
          AmbientOcclusionOpt = None
          HeightOpt = None
          IgnoreLightMapsOpt = None }

/// Material properties for surfaces.
/// NOTE: this type has to go after TerrainMaterialProperties lest the latter's field names shadow this one's.
type [<SymbolicExpansion>] MaterialProperties =
    { AlbedoOpt : Color voption
      RoughnessOpt : single voption
      MetallicOpt : single voption
      AmbientOcclusionOpt : single voption
      EmissionOpt : single voption
      HeightOpt : single voption
      IgnoreLightMapsOpt : bool voption
      OpaqueDistanceOpt : single voption
      FinenessOffsetOpt : single voption
      ScatterTypeOpt : ScatterType voption
      SpecularScalarOpt : single voption
      RefractiveIndexOpt : single voption }

    member this.Albedo = ValueOption.defaultValue Constants.Render.AlbedoDefault this.AlbedoOpt
    member this.Roughness = ValueOption.defaultValue Constants.Render.RoughnessDefault this.RoughnessOpt
    member this.Metallic = ValueOption.defaultValue Constants.Render.MetallicDefault this.MetallicOpt
    member this.AmbientOcclusion = ValueOption.defaultValue Constants.Render.AmbientOcclusionDefault this.AmbientOcclusionOpt
    member this.Emission = ValueOption.defaultValue Constants.Render.EmissionDefault this.EmissionOpt
    member this.Height = ValueOption.defaultValue Constants.Render.HeightDefault this.HeightOpt
    member this.IgnoreLightMaps = ValueOption.defaultValue Constants.Render.IgnoreLightMapsDefault this.IgnoreLightMapsOpt
    member this.OpaqueDistance = ValueOption.defaultValue Constants.Render.OpaqueDistanceDefault this.OpaqueDistanceOpt
    member this.FinenessOffset = ValueOption.defaultValue Constants.Render.FinenessOffsetDefault this.FinenessOffsetOpt
    member this.ScatterType = ValueOption.defaultValue Constants.Render.ScatterTypeDefault this.ScatterTypeOpt
    member this.SpecularScalar = ValueOption.defaultValue Constants.Render.SpecularScalarDefault this.SpecularScalarOpt
    member this.RefractiveIndex = ValueOption.defaultValue Constants.Render.RefractiveIndexDefault this.RefractiveIndexOpt

/// MaterialProperties functions.
[<RequireQualifiedAccess>]
module MaterialProperties =

    /// Material properties with populated default properties.
    let defaultProperties =
        { AlbedoOpt = ValueSome Constants.Render.AlbedoDefault
          RoughnessOpt = ValueSome Constants.Render.RoughnessDefault
          MetallicOpt = ValueSome Constants.Render.MetallicDefault
          AmbientOcclusionOpt = ValueSome Constants.Render.AmbientOcclusionDefault
          EmissionOpt = ValueSome Constants.Render.EmissionDefault
          HeightOpt = ValueSome Constants.Render.HeightDefault
          IgnoreLightMapsOpt = ValueSome Constants.Render.IgnoreLightMapsDefault
          OpaqueDistanceOpt = ValueSome Constants.Render.OpaqueDistanceDefault
          FinenessOffsetOpt = ValueSome Constants.Render.FinenessOffsetDefault
          ScatterTypeOpt = ValueSome Constants.Render.ScatterTypeDefault
          SpecularScalarOpt = ValueSome Constants.Render.SpecularScalarDefault
          RefractiveIndexOpt = ValueSome Constants.Render.RefractiveIndexDefault }

    /// Empty material properties.
    let empty =
        { AlbedoOpt = ValueNone
          RoughnessOpt = ValueNone
          MetallicOpt = ValueNone
          AmbientOcclusionOpt = ValueNone
          EmissionOpt = ValueNone
          HeightOpt = ValueNone
          IgnoreLightMapsOpt = ValueNone
          OpaqueDistanceOpt = ValueNone
          FinenessOffsetOpt = ValueNone
          ScatterTypeOpt = ValueNone
          SpecularScalarOpt = ValueNone
          RefractiveIndexOpt = ValueNone }

/// Material description for surfaces.
type [<SymbolicExpansion; CustomEquality; NoComparison>] Material =
    { AlbedoImageOpt : Image AssetTag voption
      RoughnessImageOpt : Image AssetTag voption
      MetallicImageOpt : Image AssetTag voption
      AmbientOcclusionImageOpt : Image AssetTag voption
      EmissionImageOpt : Image AssetTag voption
      NormalImageOpt : Image AssetTag voption
      HeightImageOpt : Image AssetTag voption
      SubdermalImageOpt : Image AssetTag voption
      FinenessImageOpt : Image AssetTag voption
      ScatterImageOpt : Image AssetTag voption
      TwoSidedOpt : bool voption
      ClippedOpt : bool voption }

    member this.AlbedoImage = ValueOption.defaultValue (asset Assets.Default.PackageName Assets.Default.MaterialAlbedoName) this.AlbedoImageOpt
    member this.RoughnessImage = ValueOption.defaultValue (asset Assets.Default.PackageName Assets.Default.MaterialRoughnessName) this.RoughnessImageOpt
    member this.MetallicImage = ValueOption.defaultValue (asset Assets.Default.PackageName Assets.Default.MaterialMetallicName) this.MetallicImageOpt
    member this.AmbientOcclusionImage = ValueOption.defaultValue (asset Assets.Default.PackageName Assets.Default.MaterialAmbientOcclusionName) this.AmbientOcclusionImageOpt
    member this.EmissionImage = ValueOption.defaultValue (asset Assets.Default.PackageName Assets.Default.MaterialEmissionName) this.EmissionImageOpt
    member this.NormalImage = ValueOption.defaultValue (asset Assets.Default.PackageName Assets.Default.MaterialNormalName) this.NormalImageOpt
    member this.HeightImage = ValueOption.defaultValue (asset Assets.Default.PackageName Assets.Default.MaterialHeightName) this.HeightImageOpt
    member this.SubdermalImage = ValueOption.defaultValue (asset Assets.Default.PackageName Assets.Default.MaterialSubdermalName) this.SubdermalImageOpt
    member this.FinenessImage = ValueOption.defaultValue (asset Assets.Default.PackageName Assets.Default.MaterialFinenessName) this.FinenessImageOpt
    member this.ScatterImage = ValueOption.defaultValue (asset Assets.Default.PackageName Assets.Default.MaterialScatterName) this.ScatterImageOpt
    member this.TwoSided = ValueOption.defaultValue false this.TwoSidedOpt
    member this.Clipped = ValueOption.defaultValue false this.ClippedOpt

    /// Get the hash code for this material.
    static member hash material =
        hash material.AlbedoImageOpt ^^^
        hash material.RoughnessImageOpt ^^^
        hash material.MetallicImageOpt ^^^
        hash material.AmbientOcclusionImageOpt ^^^
        hash material.EmissionImageOpt ^^^
        hash material.NormalImageOpt ^^^
        hash material.HeightImageOpt ^^^
        hash material.SubdermalImageOpt ^^^
        hash material.FinenessImageOpt ^^^
        hash material.ScatterImageOpt ^^^
        hash material.TwoSidedOpt ^^^
        hash material.ClippedOpt

    /// Check that two materials are equal.
    static member equals this that =
        refEq this that ||
        this.AlbedoImageOpt = that.AlbedoImageOpt &&
        this.RoughnessImageOpt = that.RoughnessImageOpt &&
        this.MetallicImageOpt = that.MetallicImageOpt &&
        this.AmbientOcclusionImageOpt = that.AmbientOcclusionImageOpt &&
        this.EmissionImageOpt = that.EmissionImageOpt &&
        this.NormalImageOpt = that.NormalImageOpt &&
        this.HeightImageOpt = that.HeightImageOpt &&
        this.SubdermalImageOpt = that.SubdermalImageOpt &&
        this.FinenessImageOpt = that.FinenessImageOpt &&
        this.ScatterImageOpt = that.ScatterImageOpt &&
        this.TwoSidedOpt = that.TwoSidedOpt &&
        this.ClippedOpt = that.ClippedOpt

    override this.GetHashCode () =
        Material.hash this

    override this.Equals (that : obj) =
        match that with
        | :? Material as that -> Material.equals this that
        | _ -> false

    interface IEquatable<Material> with
        member this.Equals that =
            Material.equals this that

/// Material functions.
[<RequireQualifiedAccess>]
module Material =

    /// The material with populated default images.
    let defaultMaterial =
        { AlbedoImageOpt = ValueSome (asset Assets.Default.PackageName Assets.Default.MaterialAlbedoName)
          RoughnessImageOpt = ValueSome (asset Assets.Default.PackageName Assets.Default.MaterialRoughnessName)
          MetallicImageOpt = ValueSome (asset Assets.Default.PackageName Assets.Default.MaterialMetallicName)
          AmbientOcclusionImageOpt = ValueSome (asset Assets.Default.PackageName Assets.Default.MaterialAmbientOcclusionName)
          EmissionImageOpt = ValueSome (asset Assets.Default.PackageName Assets.Default.MaterialEmissionName)
          NormalImageOpt = ValueSome (asset Assets.Default.PackageName Assets.Default.MaterialNormalName)
          HeightImageOpt = ValueSome (asset Assets.Default.PackageName Assets.Default.MaterialHeightName)
          SubdermalImageOpt = ValueSome (asset Assets.Default.PackageName Assets.Default.MaterialSubdermalName)
          FinenessImageOpt = ValueSome (asset Assets.Default.PackageName Assets.Default.MaterialFinenessName)
          ScatterImageOpt = ValueSome (asset Assets.Default.PackageName Assets.Default.MaterialScatterName)
          TwoSidedOpt = ValueSome false
          ClippedOpt = ValueSome false }

    /// The empty material.
    let empty =
        { AlbedoImageOpt = ValueNone
          RoughnessImageOpt = ValueNone
          MetallicImageOpt = ValueNone
          AmbientOcclusionImageOpt = ValueNone
          EmissionImageOpt = ValueNone
          NormalImageOpt = ValueNone
          HeightImageOpt = ValueNone
          SubdermalImageOpt = ValueNone
          FinenessImageOpt = ValueNone
          ScatterImageOpt = ValueNone
          TwoSidedOpt = ValueNone
          ClippedOpt = ValueNone }

/// A mutable 3d light probe value type.
type [<Struct>] LightProbe3dValue =
    { mutable LightProbeId : uint64
      mutable Enabled : bool
      mutable Origin : Vector3
      mutable Bounds : Box3
      mutable Stale : bool }

/// A mutable 3d light value type.
type [<Struct>] Light3dValue =
    { mutable LightId : uint64
      mutable Origin : Vector3
      mutable Rotation : Quaternion
      mutable Direction : Vector3
      mutable Presence : Presence
      mutable Color : Color
      mutable Brightness : single
      mutable AttenuationLinear : single
      mutable AttenuationQuadratic : single
      mutable LightCutoff : single
      mutable LightType : LightType
      mutable DesireShadows : bool
      mutable Bounds : Box3 }

/// A mutable billboard value type.
type [<Struct>] BillboardValue =
    { mutable ModelMatrix : Matrix4x4
      mutable CastShadow : bool
      mutable Presence : Presence
      mutable InsetOpt : Box2 option
      mutable MaterialProperties : MaterialProperties
      mutable Material : Material
      mutable ShadowOffset : single
      mutable DepthTest : DepthTest
      mutable RenderType : RenderType }

/// A mutable static model value type.
type [<Struct>] StaticModelValue =
    { mutable ModelMatrix : Matrix4x4
      mutable CastShadow : bool
      mutable Presence : Presence
      mutable InsetOpt : Box2 option
      mutable MaterialProperties : MaterialProperties
      mutable StaticModel : StaticModel AssetTag
      mutable Clipped : bool
      mutable DepthTest : DepthTest
      mutable RenderType : RenderType }

/// A mutable static model surface value type.
type [<Struct>] StaticModelSurfaceValue =
    { mutable ModelMatrix : Matrix4x4
      mutable CastShadow : bool
      mutable Presence : Presence
      mutable InsetOpt : Box2 option
      mutable MaterialProperties : MaterialProperties
      mutable Material : Material
      mutable StaticModel : StaticModel AssetTag
      mutable SurfaceIndex : int
      mutable DepthTest : DepthTest
      mutable RenderType : RenderType }

/// Describes billboard-based particles.
type BillboardParticlesDescriptor =
    { MaterialProperties : MaterialProperties
      Material : Material
      ShadowOffset : single
      Particles : Particle SArray
      RenderType : RenderType }

/// Describes a terrain patch as a potential subdivision of a larger terrain geometry.
type TerrainPatchDescriptor =
    { PatchId : Vector2i
      PatchResolution : Vector2i
      PatchBounds : Box3
      PatchOffset : Vector2i }

/// Describes a static 3d terrain geometry.
type TerrainGeometryDescriptor =
    { Bounds : Box3
      Material : TerrainMaterial
      TintImageOpt : Image AssetTag option
      NormalImageOpt : Image AssetTag option
      Tiles : Vector2
      HeightMap : HeightMap
      Patches : Vector2i }

/// Describes a static 3d terrain.
type TerrainDescriptor =
    { Bounds : Box3
      CastShadow : bool
      InsetOpt : Box2 option
      MaterialProperties : TerrainMaterialProperties
      Material : TerrainMaterial
      TintImageOpt : Image AssetTag option
      NormalImageOpt : Image AssetTag option
      Tiles : Vector2
      HeightMap : HeightMap
      Patches : Vector2i }

    member this.TerrainGeometryDescriptor =
        { Bounds = this.Bounds
          Material = this.Material
          TintImageOpt = this.TintImageOpt
          NormalImageOpt = this.NormalImageOpt
          Tiles = this.Tiles
          HeightMap = this.HeightMap
          Patches = this.Patches }

/// An internally cached static model used to reduce GC promotion or pressure.
type CachedStaticModelMessage =
    { mutable CachedStaticModelMatrix : Matrix4x4
      mutable CachedStaticModelCastShadow : bool
      mutable CachedStaticModelPresence : Presence
      mutable CachedStaticModelInsetOpt : Box2 voption
      mutable CachedStaticModelMaterialProperties : MaterialProperties
      mutable CachedStaticModel : StaticModel AssetTag
      mutable CachedStaticModelClipped : bool
      mutable CachedStaticModelDepthTest : DepthTest
      mutable CachedStaticModelRenderType : RenderType
      mutable CachedStaticModelRenderPass : RenderPass }

/// An internally cached static model surface used to reduce GC promotion or pressure.
type CachedStaticModelSurfaceMessage =
    { mutable CachedStaticModelSurfaceMatrix : Matrix4x4
      mutable CachedStaticModelSurfaceCastShadow : bool
      mutable CachedStaticModelSurfacePresence : Presence
      mutable CachedStaticModelSurfaceInsetOpt : Box2 voption
      mutable CachedStaticModelSurfaceMaterialProperties : MaterialProperties
      mutable CachedStaticModelSurfaceMaterial : Material
      mutable CachedStaticModelSurfaceModel : StaticModel AssetTag
      mutable CachedStaticModelSurfaceIndex : int
      mutable CachedStaticModelSurfaceDepthTest : DepthTest
      mutable CachedStaticModelSurfaceRenderType : RenderType
      mutable CachedStaticModelSurfaceRenderPass : RenderPass }

/// An internally cached animated model used to reduce GC promotion or pressure.
type CachedAnimatedModelMessage =
    { mutable CachedAnimatedModelMatrix : Matrix4x4
      mutable CachedAnimatedModelCastShadow : bool
      mutable CachedAnimatedModelPresence : Presence
      mutable CachedAnimatedModelInsetOpt : Box2 voption
      mutable CachedAnimatedModelMaterialProperties : MaterialProperties
      mutable CachedAnimatedModelBoneTransforms : Matrix4x4 array
      mutable CachedAnimatedModel : AnimatedModel AssetTag
      mutable CachedAnimatedModelSubsortOffsets : Map<int, single>
      mutable CachedAnimatedModelDualRenderedSurfaceIndices : int Set
      mutable CachedAnimatedModelDepthTest : DepthTest
      mutable CachedAnimatedModelRenderType : RenderType
      mutable CachedAnimatedModelRenderPass : RenderPass }        

/// Describes a static model surface.
type StaticModelSurfaceDescriptor =
    { Positions : Vector3 array
      TexCoordses : Vector2 array
      Normals : Vector3 array
      Indices : int array
      ModelMatrix : Matrix4x4
      Bounds : Box3
      MaterialProperties : OpenGL.PhysicallyBased.PhysicallyBasedMaterialProperties
      IgnoreLightMaps : bool
      AlbedoImage : Image AssetTag
      RoughnessImage : Image AssetTag
      MetallicImage : Image AssetTag
      AmbientOcclusionImage : Image AssetTag
      EmissionImage : Image AssetTag
      NormalImage : Image AssetTag
      HeightImage : Image AssetTag
      SubdermalImage : Image AssetTag
      FinenessImage : Image AssetTag
      ScatterImage : Image AssetTag
      TwoSided : bool
      Clipped : bool }

/// Describes how to create a user-defined static model.
type CreateUserDefinedStaticModel =
    { StaticModelSurfaceDescriptors : StaticModelSurfaceDescriptor array
      Bounds : Box3
      StaticModel : StaticModel AssetTag }

/// Describes how to destroy a user-defined static model.
type DestroyUserDefinedStaticModel =
    { StaticModel : StaticModel AssetTag }

/// Describes how to render a sky box.
type RenderSkyBox =
    { AmbientColor : Color
      AmbientBrightness : single
      CubeMapColor : Color
      CubeMapBrightness : single
      CubeMap : CubeMap AssetTag
      RenderPass : RenderPass }

/// Describes how to render a 3D light probe.
type RenderLightProbe3d =
    { LightProbeId : uint64
      Enabled : bool
      Origin : Vector3
      AmbientColor : Color
      AmbientBrightness : single
      Bounds : Box3
      RenderPass : RenderPass }

/// Describes how to render a 3D light map.
type RenderLightMap3d =
    { LightProbeId : uint64
      RenderPass : RenderPass }

/// Describes how to render a 3D light.
type RenderLight3d =
    { LightId : uint64
      Origin : Vector3
      Rotation : Quaternion
      Direction : Vector3
      Color : Color
      Brightness : single
      AttenuationLinear : single
      AttenuationQuadratic : single
      LightCutoff : single
      LightType : LightType
      DesireShadows : bool
      DesireFog : bool
      Bounds : Box3
      RenderPass : RenderPass }

/// Describes how to render a billboard.
type RenderBillboard =
    { ModelMatrix : Matrix4x4
      CastShadow : bool
      Presence : Presence
      InsetOpt : Box2 option
      OrientUp : bool
      Planar : bool
      MaterialProperties : MaterialProperties
      Material : Material
      ShadowOffset : single
      DepthTest : DepthTest
      RenderType : RenderType
      RenderPass : RenderPass }

/// Describes how to render multiple billboards with shared attributes.
type RenderBillboards =
    { Billboards : (Matrix4x4 * bool * Presence * Box2 option * bool * bool) SList
      MaterialProperties : MaterialProperties
      Material : Material
      ShadowOffset : single
      DepthTest : DepthTest
      RenderType : RenderType
      RenderPass : RenderPass }

/// Describes how to render billboard particles.
type RenderBillboardParticles =
    { CastShadow : bool
      Presence : Presence
      MaterialProperties : MaterialProperties
      Material : Material
      ShadowOffset : single
      Particles : Particle SArray
      DepthTest : DepthTest
      RenderType : RenderType
      RenderPass : RenderPass }

/// Describes how to render a static model surface.
type RenderStaticModelSurface =
    { ModelMatrix : Matrix4x4
      CastShadow : bool
      Presence : Presence
      InsetOpt : Box2 option
      MaterialProperties : MaterialProperties
      Material : Material
      StaticModel : StaticModel AssetTag
      SurfaceIndex : int
      DepthTest : DepthTest
      RenderType : RenderType
      RenderPass : RenderPass }

/// Describes a pre-composed batch of static model surfaces.
type StaticModelSurfacePreBatch =
    { PreBatchId : Guid
      StaticModelSurfaces : (Matrix4x4 * bool * Presence * Box2 * MaterialProperties * Box3) array
      Material : Material
      StaticModel : StaticModel AssetTag
      SurfaceIndex : int
      Clipped : bool
      DepthTest : DepthTest
      RenderType : RenderType }

/// Describes how to render a static model surface pre-batch.
type RenderStaticModelSurfacePreBatch =
    { StaticModelSurfacePreBatch : StaticModelSurfacePreBatch
      RenderPass : RenderPass }

/// Describes how to render multiple static model surface pre-batches with shared attributes.
type RenderStaticModelSurfacePreBatches =
    { StaticModelSurfacePreBatches : StaticModelSurfacePreBatch array
      RenderPass : RenderPass }

/// Describes how to render a static model.
type RenderStaticModel =
    { ModelMatrix : Matrix4x4
      CastShadow : bool
      Presence : Presence
      InsetOpt : Box2 option
      MaterialProperties : MaterialProperties
      StaticModel : StaticModel AssetTag
      Clipped : bool
      DepthTest : DepthTest
      RenderType : RenderType
      RenderPass : RenderPass }

/// Describes how to render multiple static model with shared attributes.
type RenderStaticModels =
    { StaticModels : (Matrix4x4 * bool * Presence * Box2 option * MaterialProperties) SList
      StaticModel : StaticModel AssetTag
      Clipped : bool
      DepthTest : DepthTest
      RenderType : RenderType
      RenderPass : RenderPass }

/// Describes how to render an animated model.
type RenderAnimatedModel =
    { ModelMatrix : Matrix4x4
      CastShadow : bool
      Presence : Presence
      InsetOpt : Box2 option
      MaterialProperties : MaterialProperties
      BoneTransforms : Matrix4x4 array
      AnimatedModel : AnimatedModel AssetTag
      SubsortOffsets : Map<int, single>
      DualRenderedSurfaceIndices : int Set
      DepthTest : DepthTest
      RenderType : RenderType
      RenderPass : RenderPass }

/// Describes how to render multiple animated models with shared attributes.
type RenderAnimatedModels =
    { BoneTransforms : Matrix4x4 array
      AnimatedModels : (Matrix4x4 * bool * Presence * Box2 option * MaterialProperties) SList
      AnimatedModel : AnimatedModel AssetTag
      SubsortOffsets : Map<int, single>
      DualRenderedSurfaceIndices : int Set
      DepthTest : DepthTest
      RenderType : RenderType
      RenderPass : RenderPass }

/// Describes how to render a user-defined static model.
type RenderUserDefinedStaticModel =
    { ModelMatrix : Matrix4x4
      CastShadow : bool
      Clipped : bool
      Presence : Presence
      InsetOpt : Box2 option
      MaterialProperties : MaterialProperties
      StaticModelSurfaceDescriptors : StaticModelSurfaceDescriptor array
      Bounds : Box3
      DepthTest : DepthTest
      RenderType : RenderType
      RenderPass : RenderPass }

/// Describes how to render terrain.
type RenderTerrain =
    { Visible : bool
      TerrainDescriptor : TerrainDescriptor
      RenderPass : RenderPass }

/// Configures 3d lighting and ssao.
type [<SymbolicExpansion>] Lighting3dConfig =
    { LightCutoffMargin : single
      LightAmbientBoostCutoff : single
      LightAmbientBoostScalar : single
      LightShadowSamples : int
      LightShadowBias : single
      LightShadowSampleScalar : single
      LightShadowExponent : single
      LightShadowDensity : single
      LightExposure : single
      ToneMapType : ToneMapType
      ToneMapSlope : Vector3
      ToneMapOffset : Vector3
      ToneMapPower : Vector3
      ToneMapSaturation : single
      ToneMapWhitePoint : single
      FogEnabled : bool
      FogType : FogType
      FogStart : single
      FogFinish : single
      FogDensity : single
      FogColor : Color
      SssEnabled : bool
      SsaoEnabled : bool
      SsaoIntensity : single
      SsaoBias : single
      SsaoRadius : single
      SsaoDistanceMax : single
      SsvfEnabled : bool
      SsvfSteps : int
      SsvfAsymmetry : single
      SsvfIntensity : single
      SsrlEnabled : bool
      SsrlIntensity : single
      SsrlDetail : single
      SsrlRefinementsMax : int
      SsrlRayThickness : single
      SsrlTowardEyeCutoff : single
      SsrlDepthCutoff : single
      SsrlDepthCutoffMargin : single
      SsrlDistanceCutoff : single
      SsrlDistanceCutoffMargin : single
      SsrlRoughnessCutoff : single
      SsrlRoughnessCutoffMargin : single
      SsrlSlopeCutoff : single
      SsrlSlopeCutoffMargin : single
      SsrlEdgeHorizontalMargin : single
      SsrlEdgeVerticalMargin : single
      SsrrEnabled : bool
      SsrrIntensity : single
      SsrrDetail : single
      SsrrRefinementsMax : int
      SsrrRayThickness : single
      SsrrDistanceCutoff : single
      SsrrDistanceCutoffMargin : single
      SsrrEdgeHorizontalMargin : single
      SsrrEdgeVerticalMargin : single
      BloomEnabled : bool
      BloomThreshold : single
      BloomKarisAverageEnabled : bool
      BloomFilterRadius : single
      BloomStrength : single }

    static member defaultConfig =
        { LightCutoffMargin = Constants.Render.LightCutoffMarginDefault
          LightAmbientBoostCutoff = Constants.Render.LightAmbientBoostCutoffDefault
          LightAmbientBoostScalar = Constants.Render.LightAmbientBoostScalarDefault
          LightShadowSamples = Constants.Render.LightShadowSamplesDefault
          LightShadowBias = Constants.Render.LightShadowBiasDefault
          LightShadowSampleScalar = Constants.Render.LightShadowSampleScalarDefault
          LightShadowExponent = Constants.Render.LightShadowExponentDefault
          LightShadowDensity = Constants.Render.LightShadowDensityDefault
          LightExposure = Constants.Render.LightExposureDefault
          ToneMapType = Constants.Render.ToneMapTypeDefault
          ToneMapSlope = Constants.Render.ToneMapSlopeDefault
          ToneMapOffset = Constants.Render.ToneMapOffsetDefault
          ToneMapPower = Constants.Render.ToneMapPowerDefault
          ToneMapSaturation = Constants.Render.ToneMapSaturationDefault
          ToneMapWhitePoint = Constants.Render.ToneMapWhitePointDefault
          FogEnabled = Constants.Render.FogEnabledDefault
          FogType = Constants.Render.FogTypeDefault
          FogStart = Constants.Render.FogStartDefault
          FogFinish = Constants.Render.FogFinishDefault
          FogDensity = Constants.Render.FogDensityDefault
          FogColor = Constants.Render.FogColorDefault
          SssEnabled = Constants.Render.SssEnabledLocalDefault
          SsaoEnabled = Constants.Render.SsaoEnabledLocalDefault
          SsaoIntensity = Constants.Render.SsaoIntensityDefault
          SsaoBias = Constants.Render.SsaoBiasDefault
          SsaoRadius = Constants.Render.SsaoRadiusDefault
          SsaoDistanceMax = Constants.Render.SsaoDistanceMaxDefault
          SsvfEnabled = Constants.Render.SsvfEnabledLocalDefault
          SsvfSteps = Constants.Render.SsvfStepsDefault
          SsvfAsymmetry = Constants.Render.SsvfAsymmetryDefault
          SsvfIntensity = Constants.Render.SsvfIntensityDefault
          SsrlEnabled = Constants.Render.SsrlEnabledLocalDefault
          SsrlIntensity = Constants.Render.SsrlIntensityDefault
          SsrlDetail = Constants.Render.SsrlDetailDefault
          SsrlRefinementsMax = Constants.Render.SsrlRefinementsMaxDefault
          SsrlRayThickness = Constants.Render.SsrlRayThicknessDefault
          SsrlTowardEyeCutoff = Constants.Render.SsrlTowardEyeCutoffDefault
          SsrlDepthCutoff = Constants.Render.SsrlDepthCutoffDefault
          SsrlDepthCutoffMargin = Constants.Render.SsrlDepthCutoffMarginDefault
          SsrlDistanceCutoff = Constants.Render.SsrlDistanceCutoffDefault
          SsrlDistanceCutoffMargin = Constants.Render.SsrlDistanceCutoffMarginDefault
          SsrlRoughnessCutoff = Constants.Render.SsrlRoughnessCutoffDefault
          SsrlRoughnessCutoffMargin = Constants.Render.SsrlRoughnessCutoffMarginDefault
          SsrlSlopeCutoff = Constants.Render.SsrlSlopeCutoffDefault
          SsrlSlopeCutoffMargin = Constants.Render.SsrlSlopeCutoffMarginDefault
          SsrlEdgeHorizontalMargin = Constants.Render.SsrlEdgeHorizontalMarginDefault
          SsrlEdgeVerticalMargin = Constants.Render.SsrlEdgeVerticalMarginDefault
          SsrrEnabled = Constants.Render.SsrrEnabledLocalDefault
          SsrrIntensity = Constants.Render.SsrrIntensityDefault
          SsrrDetail = Constants.Render.SsrrDetailDefault
          SsrrRefinementsMax = Constants.Render.SsrrRefinementsMaxDefault
          SsrrRayThickness = Constants.Render.SsrrRayThicknessDefault
          SsrrDistanceCutoff = Constants.Render.SsrrDistanceCutoffDefault
          SsrrDistanceCutoffMargin = Constants.Render.SsrrDistanceCutoffMarginDefault
          SsrrEdgeHorizontalMargin = Constants.Render.SsrrEdgeHorizontalMarginDefault
          SsrrEdgeVerticalMargin = Constants.Render.SsrrEdgeVerticalMarginDefault
          BloomEnabled = Constants.Render.BloomEnabledLocalDefault
          BloomThreshold = Constants.Render.BloomThresholdDefault
          BloomKarisAverageEnabled = Constants.Render.BloomKarisAverageEnabledDefault
          BloomFilterRadius = Constants.Render.BloomFilterRadiusDefault
          BloomStrength = Constants.Render.BloomStrengthDefault }

/// Configures 3d renderer.
type [<SymbolicExpansion>] Renderer3dConfig =
    { LightMappingEnabled : bool
      LightShadowingEnabled : bool
      SssEnabled : bool
      SsaoEnabled : bool
      SsaoSampleCount : int
      SsvfEnabled : bool
      SsrlEnabled : bool
      SsrrEnabled : bool
      BloomEnabled : bool
      FxaaEnabled : bool }

    static member defaultConfig =
        { LightMappingEnabled = Constants.Render.LightMappingEnabledDefault
          LightShadowingEnabled = Constants.Render.LightShadowingEnabledDefault
          SssEnabled = Constants.Render.SssEnabledGlobalDefault
          SsaoEnabled = Constants.Render.SsaoEnabledGlobalDefault
          SsaoSampleCount = Constants.Render.SsaoSampleCountDefault
          SsvfEnabled = Constants.Render.SsvfEnabledGlobalDefault
          SsrlEnabled = Constants.Render.SsrlEnabledGlobalDefault
          SsrrEnabled = Constants.Render.SsrrEnabledGlobalDefault
          BloomEnabled = Constants.Render.BloomEnabledGlobalDefault
          FxaaEnabled = Constants.Render.FxaaEnabledDefault }

/// A message to the 3d renderer.
type RenderMessage3d =
    | CreateUserDefinedStaticModel of CreateUserDefinedStaticModel
    | DestroyUserDefinedStaticModel of DestroyUserDefinedStaticModel
    | RenderSkyBox of RenderSkyBox
    | RenderLightProbe3d of RenderLightProbe3d
    | RenderLightMap3d of RenderLightMap3d
    | RenderLight3d of RenderLight3d
    | RenderBillboard of RenderBillboard
    | RenderBillboards of RenderBillboards
    | RenderBillboardParticles of RenderBillboardParticles
    | RenderStaticModelSurface of RenderStaticModelSurface
    | RenderStaticModelSurfacePreBatch of RenderStaticModelSurfacePreBatch
    | RenderStaticModelSurfacePreBatches of RenderStaticModelSurfacePreBatches
    | RenderStaticModel of RenderStaticModel
    | RenderStaticModels of RenderStaticModels
    | RenderCachedStaticModel of CachedStaticModelMessage
    | RenderCachedStaticModelSurface of CachedStaticModelSurfaceMessage
    | RenderUserDefinedStaticModel of RenderUserDefinedStaticModel
    | RenderAnimatedModel of RenderAnimatedModel
    | RenderAnimatedModels of RenderAnimatedModels
    | RenderCachedAnimatedModel of CachedAnimatedModelMessage
    | RenderTerrain of RenderTerrain
    | ConfigureLighting3d of Lighting3dConfig
    | ConfigureRenderer3d of Renderer3dConfig
    | LoadRenderPackage3d of string
    | UnloadRenderPackage3d of string
    | ReloadRenderAssets3d

/// A sortable light map.
/// OPTIMIZATION: mutable field for caching distance squared.
type private SortableLightMap =
    { SortableLightMapEnabled : bool
      SortableLightMapOrigin : Vector3
      SortableLightMapAmbientColor : Color
      SortableLightMapAmbientBrightness : single
      SortableLightMapBounds : Box3
      SortableLightMapIrradianceMap : OpenGL.Texture.Texture
      SortableLightMapEnvironmentFilterMap : OpenGL.Texture.Texture
      mutable SortableLightMapDistanceSquared : single }

    /// TODO: maybe put this somewhere general?
    static member private distanceFromBounds (point: Vector3) (bounds : Box3) =
        let x = min bounds.Max.X (max bounds.Min.X point.X)
        let y = min bounds.Max.Y (max bounds.Min.Y point.Y)
        let z = min bounds.Max.Z (max bounds.Min.Z point.Z)
        (point - v3 x y z).MagnitudeSquared

    /// Sort light maps into array for uploading to OpenGL.
    /// TODO: consider getting rid of allocation here.
    static member sortLightMaps lightMapsMax position boundsOpt lightMaps =
        let lightMapOrigins = Array.zeroCreate<Vector3> lightMapsMax
        let lightMapMins = Array.zeroCreate<Vector3> lightMapsMax
        let lightMapSizes = Array.zeroCreate<Vector3> lightMapsMax
        let lightMapAmbientColors = Array.zeroCreate<Color> lightMapsMax
        let lightMapAmbientBrightnesses = Array.zeroCreate<single> lightMapsMax
        let lightMapIrradianceMaps = Array.init<OpenGL.Texture.Texture> lightMapsMax (fun _ -> OpenGL.Texture.EmptyTexture)
        let lightMapEnvironmentFilterMaps = Array.init<OpenGL.Texture.Texture> lightMapsMax (fun _ -> OpenGL.Texture.EmptyTexture)
        let lightMapsFiltered =
            match boundsOpt with
            | Some (bounds : Box3) -> lightMaps |> Array.filter (fun lightMap -> lightMap.SortableLightMapBounds.Intersects bounds)
            | None -> lightMaps
        for lightMap in lightMapsFiltered do
            lightMap.SortableLightMapDistanceSquared <- SortableLightMap.distanceFromBounds position lightMap.SortableLightMapBounds
        let lightMapsSorted =
            lightMapsFiltered |> Array.sortBy (fun lightMap -> lightMap.SortableLightMapDistanceSquared)
        for i in 0 .. dec lightMapsMax do
            if i < lightMapsSorted.Length then
                let lightMap = lightMapsSorted.[i]
                lightMapOrigins.[i] <- lightMap.SortableLightMapOrigin
                lightMapMins.[i] <- lightMap.SortableLightMapBounds.Min
                lightMapSizes.[i] <- lightMap.SortableLightMapBounds.Size
                lightMapAmbientColors.[i] <- lightMap.SortableLightMapAmbientColor
                lightMapAmbientBrightnesses.[i] <- lightMap.SortableLightMapAmbientBrightness
                lightMapIrradianceMaps.[i] <- lightMap.SortableLightMapIrradianceMap
                lightMapEnvironmentFilterMaps.[i] <- lightMap.SortableLightMapEnvironmentFilterMap
        (lightMapOrigins, lightMapMins, lightMapSizes, lightMapAmbientColors, lightMapAmbientBrightnesses, lightMapIrradianceMaps, lightMapEnvironmentFilterMaps)

/// A sortable light.
/// OPTIMIZATION: mutable field for caching distance squared.
type private SortableLight =
    { SortableLightId : uint64
      SortableLightOrigin : Vector3
      SortableLightRotation : Quaternion
      SortableLightDirection : Vector3
      SortableLightColor : Color
      SortableLightBrightness : single
      SortableLightAttenuationLinear : single
      SortableLightAttenuationQuadratic : single
      SortableLightCutoff : single
      SortableLightType : int
      SortableLightConeInner : single
      SortableLightConeOuter : single
      SortableLightDesireShadows : int
      SortableLightDesireFog : int
      SortableLightBounds : Box3
      mutable SortableLightDistance : single }

    static member private project light =
        let directionalWeight = match light.SortableLightType with 2 -> -1 | _ -> 0
        let desiredShadowsWeight = -light.SortableLightDesireShadows
        struct (directionalWeight, light.SortableLightDistance, desiredShadowsWeight)

    /// Sort shadowing point lights.
    /// TODO: see if we can get rid of allocation here.
    static member sortShadowingPointLightsIntoArray lightsMax position lights =
        let lightsArray = Array.zeroCreate<_> lightsMax
        for light in lights do
            light.SortableLightDistance <-
                (light.SortableLightOrigin - position).Magnitude - light.SortableLightCutoff |> max 0.0f
        let lightsSorted =
            lights
            |> Seq.toArray
            |> Array.filter (fun light -> light.SortableLightDesireShadows = 1 && light.SortableLightType = 0)
            |> Array.sortBy SortableLight.project
        for i in 0 .. dec lightsMax do
            if i < lightsSorted.Length then
                let light = lightsSorted.[i]
                lightsArray.[i] <- struct (light.SortableLightId, light.SortableLightOrigin, light.SortableLightCutoff, light.SortableLightConeOuter, light.SortableLightDesireShadows, light.SortableLightBounds)
        lightsArray

    /// Sort shadowing spot and directional lights.
    /// TODO: see if we can get rid of allocation here.
    static member sortShadowingSpotAndDirectionalLightsIntoArray lightsMax position lights =
        let lightsArray = Array.zeroCreate<_> lightsMax
        for light in lights do
            light.SortableLightDistance <-
                (light.SortableLightOrigin - position).Magnitude - light.SortableLightCutoff |> max 0.0f
        let lightsSorted =
            lights
            |> Seq.toArray
            |> Array.filter (fun light -> light.SortableLightDesireShadows = 1 && (light.SortableLightType = 1 || light.SortableLightType = 2))
            |> Array.sortBy SortableLight.project
        for i in 0 .. dec lightsMax do
            if i < lightsSorted.Length then
                let light = lightsSorted.[i]
                lightsArray.[i] <- struct (light.SortableLightId, light.SortableLightOrigin, light.SortableLightCutoff, light.SortableLightConeOuter, light.SortableLightDesireShadows, light.SortableLightBounds)
        lightsArray

    /// Sort shadowing cascaded lights.
    /// TODO: see if we can get rid of allocation here.
    static member sortShadowingCascadedLightsIntoArray lightsMax position lights =
        let lightsArray = Array.zeroCreate<_> lightsMax
        for light in lights do
            light.SortableLightDistance <-
                (light.SortableLightOrigin - position).Magnitude - light.SortableLightCutoff |> max 0.0f
        let lightsSorted =
            lights
            |> Seq.toArray
            |> Array.filter (fun light -> light.SortableLightDesireShadows = 1 && light.SortableLightType = 3)
            |> Array.sortBy SortableLight.project
        for i in 0 .. dec lightsMax do
            if i < lightsSorted.Length then
                let light = lightsSorted.[i]
                lightsArray.[i] <- struct (light.SortableLightId, light.SortableLightOrigin, light.SortableLightCutoff, light.SortableLightConeOuter, light.SortableLightDesireShadows, light.SortableLightBounds)
        lightsArray

    /// Sort lights into float array for uploading to OpenGL.
    /// TODO: see if we can get rid of allocation here.
    static member sortLights lightsMax position lights =
        let lightIds = Array.zeroCreate<uint64> lightsMax
        let lightOrigins = Array.zeroCreate<Vector3> lightsMax
        let lightDirections = Array.zeroCreate<Vector3> lightsMax
        let lightColors = Array.zeroCreate<Color> lightsMax
        let lightBrightnesses = Array.zeroCreate<single> lightsMax
        let lightAttenuationLinears = Array.zeroCreate<single> lightsMax
        let lightAttenuationQuadratics = Array.zeroCreate<single> lightsMax
        let lightCutoffs = Array.zeroCreate<single> lightsMax
        let lightTypes = Array.zeroCreate<int> lightsMax
        let lightConeInners = Array.zeroCreate<single> lightsMax
        let lightConeOuters = Array.zeroCreate<single> lightsMax
        let lightDesireFogs = Array.zeroCreate<int> lightsMax
        for light in lights do
            light.SortableLightDistance <-
                (light.SortableLightOrigin - position).Magnitude - light.SortableLightCutoff |> max 0.0f
        let lightsSorted = lights |> Seq.toArray |> Array.sortBy SortableLight.project
        for i in 0 .. dec lightsMax do
            if i < lightsSorted.Length then
                let light = lightsSorted.[i]
                lightIds.[i] <- light.SortableLightId
                lightOrigins.[i] <- light.SortableLightOrigin
                lightDirections.[i] <- light.SortableLightDirection
                lightColors.[i] <- light.SortableLightColor
                lightBrightnesses.[i] <- light.SortableLightBrightness
                lightAttenuationLinears.[i] <- light.SortableLightAttenuationLinear
                lightAttenuationQuadratics.[i] <- light.SortableLightAttenuationQuadratic
                lightCutoffs.[i] <- light.SortableLightCutoff
                lightTypes.[i] <- light.SortableLightType
                lightConeInners.[i] <- light.SortableLightConeInner
                lightConeOuters.[i] <- light.SortableLightConeOuter
                lightDesireFogs.[i] <- light.SortableLightDesireFog
        (lightIds, lightOrigins, lightDirections, lightColors, lightBrightnesses, lightAttenuationLinears, lightAttenuationQuadratics, lightCutoffs, lightTypes, lightConeInners, lightConeOuters, lightDesireFogs)

    /// Sort light shadow indices.
    static member sortLightShadowIndices (lightShadowIndices : Dictionary<uint64, int>) (lightIds : uint64 array) =
        let indices =
            [|for lightId in lightIds do
                match lightShadowIndices.TryGetValue lightId with
                | (true, index) -> index
                | (false, _) -> -1|]
        let indicesPadded = Array.append indices (Array.create lightIds.Length -1)
        Array.take lightIds.Length indicesPadded

/// Enables efficient comparison of animated model surfaces.
type [<CustomEquality; NoComparison; Struct>] private AnimatedModelSurfaceKey =
    { BoneTransforms : Matrix4x4 array
      AnimatedSurface : OpenGL.PhysicallyBased.PhysicallyBasedSurface }

    static member hash amsKey =
        let mutable hashCode = 0
        for i in 0 .. dec amsKey.BoneTransforms.Length do hashCode <- hashCode ^^^ amsKey.BoneTransforms.[i].GetHashCode ()
        hashCode <- hashCode ^^^ OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.hash amsKey.AnimatedSurface
        hashCode

    static member equals left right =
        if left.BoneTransforms.Length = right.BoneTransforms.Length then
            let mutable equal = true
            let mutable i = 0
            while i < left.BoneTransforms.Length && equal do
                equal <- m4Eq left.BoneTransforms.[i] right.BoneTransforms.[i]
                i <- inc i
            equal && OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.equals left.AnimatedSurface right.AnimatedSurface
        else false

    static member comparer =
        HashIdentity.FromFunctions AnimatedModelSurfaceKey.hash AnimatedModelSurfaceKey.equals

    override this.GetHashCode () =
        AnimatedModelSurfaceKey.hash this

    override this.Equals thatObj =
        match thatObj with
        | :? AnimatedModelSurfaceKey as that -> AnimatedModelSurfaceKey.equals this that
        | _ -> false

/// A collection of tasks in a render pass.
type [<ReferenceEquality>] private RenderTasks =
    { SkyBoxes : (Color * single * Color * single * CubeMap AssetTag) List
      LightProbes : Dictionary<uint64, struct (bool * Vector3 * Color * single * Box3)>
      LightMaps : SortableLightMap List
      LightMapRenders : uint64 HashSet
      Lights : SortableLight List
      DeferredStatic : Dictionary<OpenGL.PhysicallyBased.PhysicallyBasedSurface, struct (Matrix4x4 * bool * Presence * Box2 * MaterialProperties) List>
      DeferredStaticPreBatches : Dictionary<Guid, struct (OpenGL.PhysicallyBased.PhysicallyBasedSurface * (Matrix4x4 * bool * Presence * Box2 * MaterialProperties * Box3) array)>
      DeferredStaticClipped : Dictionary<OpenGL.PhysicallyBased.PhysicallyBasedSurface, struct (Matrix4x4 * bool * Presence * Box2 * MaterialProperties) List>
      DeferredStaticClippedPreBatches : Dictionary<Guid, struct (OpenGL.PhysicallyBased.PhysicallyBasedSurface * (Matrix4x4 * bool * Presence * Box2 * MaterialProperties * Box3) array)>
      DeferredAnimated : Dictionary<AnimatedModelSurfaceKey, struct (Matrix4x4 * bool * Presence * Box2 * MaterialProperties) List>
      DeferredTerrains : struct (TerrainDescriptor * TerrainPatchDescriptor * OpenGL.PhysicallyBased.PhysicallyBasedGeometry) List
      Forward : struct (single * single * Matrix4x4 * bool * Presence * Box2 * MaterialProperties * Matrix4x4 array voption * OpenGL.PhysicallyBased.PhysicallyBasedSurface * DepthTest) List
      ForwardSorted : struct (Matrix4x4 * bool * Presence * Box2 * MaterialProperties * Matrix4x4 array voption * OpenGL.PhysicallyBased.PhysicallyBasedSurface * DepthTest) List
      DeferredStaticRemovals : OpenGL.PhysicallyBased.PhysicallyBasedSurface List
      DeferredStaticClippedRemovals : OpenGL.PhysicallyBased.PhysicallyBasedSurface List
      DeferredAnimatedRemovals : AnimatedModelSurfaceKey List
      mutable ShadowBufferIndexOpt : int option }

    static member make () =
        { SkyBoxes = List ()
          LightProbes = Dictionary HashIdentity.Structural
          LightMapRenders = HashSet HashIdentity.Structural
          LightMaps = List ()
          Lights = List ()
          DeferredStatic = dictPlus OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.comparer []
          DeferredStaticPreBatches = dictPlus HashIdentity.Structural []
          DeferredStaticClipped = dictPlus OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.comparer []
          DeferredStaticClippedPreBatches = dictPlus HashIdentity.Structural []
          DeferredAnimated = dictPlus AnimatedModelSurfaceKey.comparer []
          DeferredTerrains = List ()
          Forward = List ()
          ForwardSorted = List ()
          DeferredStaticRemovals = List ()
          DeferredStaticClippedRemovals = List ()
          DeferredAnimatedRemovals = List ()
          ShadowBufferIndexOpt = None }

    static member clear renderTasks =

        renderTasks.SkyBoxes.Clear ()
        renderTasks.LightProbes.Clear ()
        renderTasks.LightMapRenders.Clear ()
        renderTasks.LightMaps.Clear ()
        renderTasks.Lights.Clear ()

        for entry in renderTasks.DeferredStatic do
            if entry.Value.Count = 0
            then renderTasks.DeferredStaticRemovals.Add entry.Key
            else entry.Value.Clear ()
        for removal in renderTasks.DeferredStaticRemovals do
            renderTasks.DeferredStatic.Remove removal |> ignore<bool>
        renderTasks.DeferredStaticRemovals.Clear ()
        renderTasks.DeferredStaticPreBatches.Clear ()

        for entry in renderTasks.DeferredStaticClipped do
            if entry.Value.Count = 0
            then renderTasks.DeferredStaticClippedRemovals.Add entry.Key
            else entry.Value.Clear ()
        for removal in renderTasks.DeferredStaticClippedRemovals do
            renderTasks.DeferredStaticClipped.Remove removal |> ignore<bool>
        renderTasks.DeferredStaticClippedRemovals.Clear ()
        renderTasks.DeferredStaticClippedPreBatches.Clear ()

        for entry in renderTasks.DeferredAnimated do
            if entry.Value.Count = 0
            then renderTasks.DeferredAnimatedRemovals.Add entry.Key
            else entry.Value.Clear ()
        for removal in renderTasks.DeferredAnimatedRemovals do
            renderTasks.DeferredAnimated.Remove removal |> ignore<bool>
        renderTasks.DeferredAnimatedRemovals.Clear ()

        renderTasks.Forward.Clear ()
        renderTasks.ForwardSorted.Clear ()
        renderTasks.DeferredTerrains.Clear ()

        renderTasks.ShadowBufferIndexOpt <- None

    static member shadowUpToDate lightingConfigChanged renderingConfigChanged renderTasks renderTasksCached =
        if not lightingConfigChanged && not renderingConfigChanged then
            let deferredStaticCached =
                renderTasks.DeferredStatic.Count = renderTasksCached.DeferredStatic.Count &&
                (renderTasks.DeferredStatic, renderTasksCached.DeferredStatic)
                ||> Seq.forall2 (fun static_ staticCached ->
                    OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.equals static_.Key staticCached.Key &&
                    static_.Value.Count = staticCached.Value.Count &&
                    (static_.Value, staticCached.Value)
                    ||> Seq.forall2 (fun struct (m, cs, _, _, _) struct (mCached, csCached, _, _, _) -> m4Eq m mCached && cs = csCached))
            let deferredStaticPreBatchesCached =
                renderTasks.DeferredStaticPreBatches.Count = renderTasksCached.DeferredStaticPreBatches.Count &&
                (renderTasks.DeferredStaticPreBatches, renderTasksCached.DeferredStaticPreBatches)
                ||> Seq.forall2 (fun staticPreBatch staticPreBatchCached -> staticPreBatch.Key = staticPreBatchCached.Key)
            let deferredStaticClippedCached =
                renderTasks.DeferredStaticClipped.Count = renderTasksCached.DeferredStaticClipped.Count &&
                (renderTasks.DeferredStaticClipped, renderTasksCached.DeferredStaticClipped)
                ||> Seq.forall2 (fun static_ staticCached ->
                    OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.equals static_.Key staticCached.Key &&
                    static_.Value.Count = staticCached.Value.Count &&
                    (static_.Value, staticCached.Value)
                    ||> Seq.forall2 (fun struct (m, cs, _, _, _) struct (mCached, csCached, _, _, _) -> m4Eq m mCached && cs = csCached))
            let deferredStaticClippedPreBatchesCached =
                renderTasks.DeferredStaticClippedPreBatches.Count = renderTasksCached.DeferredStaticClippedPreBatches.Count &&
                (renderTasks.DeferredStaticClippedPreBatches, renderTasksCached.DeferredStaticClippedPreBatches)
                ||> Seq.forall2 (fun staticPreBatch staticPreBatchCached -> staticPreBatch.Key = staticPreBatchCached.Key)
            let deferredAnimatedCached =
                renderTasks.DeferredAnimated.Count = renderTasksCached.DeferredAnimated.Count &&
                (renderTasks.DeferredAnimated, renderTasksCached.DeferredAnimated)
                ||> Seq.forall2 (fun animated animatedCached ->
                    AnimatedModelSurfaceKey.equals animated.Key animatedCached.Key &&
                    animated.Value.Count = animatedCached.Value.Count &&
                    (animated.Value, animatedCached.Value)
                    ||> Seq.forall2 (fun struct (m, cs, _, _, _) struct (mCached, csCached, _, _, _) -> m4Eq m mCached && cs = csCached))
            let deferredTerrainsCached =
                renderTasks.DeferredTerrains.Count = renderTasksCached.DeferredTerrains.Count &&
                (renderTasks.DeferredTerrains, renderTasksCached.DeferredTerrains)
                ||> Seq.forall2 (fun struct (terrainDescriptor, patchDescriptor, _) struct (terrainDescriptorCached, patchDescriptorCached, _) ->
                    patchDescriptor = patchDescriptorCached &&
                    box3Eq terrainDescriptor.Bounds terrainDescriptorCached.Bounds &&
                    terrainDescriptor.CastShadow = terrainDescriptorCached.CastShadow &&
                    terrainDescriptor.HeightMap = terrainDescriptorCached.HeightMap)
            let forwardCached =
                renderTasks.Forward.Count = renderTasksCached.Forward.Count &&
                (renderTasks.Forward, renderTasksCached.Forward)
                ||> Seq.forall2 (fun struct (_, _, m, cs, _, _, _, bo, s, _) struct (_, _, mCached, csCached, _, _, _, boCached, sCached, _) ->
                    m4Eq m mCached &&
                    cs = csCached &&
                    bo = boCached && // TODO: P0: optimize?
                    OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.equals s sCached)
            deferredStaticCached &&
            deferredStaticPreBatchesCached &&
            deferredStaticClippedCached &&
            deferredStaticClippedPreBatchesCached &&
            deferredAnimatedCached &&
            deferredTerrainsCached &&
            forwardCached
        else false

/// The 3d renderer. Represents a 3d rendering subsystem in Nu generally.
type Renderer3d =

    /// The current renderer configuration.
    abstract RendererConfig : Renderer3dConfig

    /// Render a frame of the game.
    abstract Render :
        frustumInterior : Frustum ->
        frustumExterior : Frustum ->
        frustumImposter : Frustum ->
        lightBox : Box3 ->
        eyeCenter : Vector3 ->
        eyeRotation : Quaternion ->
        eyeFieldOfView : single ->
        geometryViewport : Viewport ->
        rasterViewport : Viewport ->
        renderMessages : RenderMessage3d List -> unit

    /// Handle render clean up by freeing all loaded render assets.
    abstract CleanUp : unit -> unit

/// The stub implementation of Renderer3d.
type [<ReferenceEquality>] StubRenderer3d =
    private
        { StubRenderer3d : unit }

    interface Renderer3d with
        member renderer.RendererConfig = Renderer3dConfig.defaultConfig
        member renderer.Render _ _ _ _ _ _ _ _ _ _ = ()
        member renderer.CleanUp () = ()

    static member make () =
        { StubRenderer3d = () }

/// The OpenGL implementation of Renderer3d.
type [<ReferenceEquality>] GlRenderer3d =
    private
        { mutable GeometryViewport : Viewport
          mutable RasterViewport : Viewport
          LazyTextureQueues : ConcurrentDictionary<OpenGL.Texture.LazyTexture ConcurrentQueue, OpenGL.Texture.LazyTexture ConcurrentQueue>
          TextureServer : OpenGL.Texture.TextureServer
          CubeMapVao : uint
          mutable SkyBoxShader : OpenGL.SkyBox.SkyBoxShader
          mutable IrradianceShader : OpenGL.CubeMap.CubeMapShader
          mutable EnvironmentFilterShader : OpenGL.LightMap.EnvironmentFilterShader
          mutable FilterShaders : OpenGL.Filter.FilterShaders
          PhysicallyBasedStaticVao : uint
          PhysicallyBasedAnimatedVao : uint
          PhysicallyBasedTerrainVao : uint
          mutable PhysicallyBasedShaders : OpenGL.PhysicallyBased.PhysicallyBasedShaders
          ShadowMatrices : Matrix4x4 array
          LightShadowIndices : Dictionary<uint64, int>
          LightsDesiringShadows : Dictionary<uint64, SortableLight>
          CubeMapGeometry : OpenGL.CubeMap.CubeMapGeometry
          BillboardGeometry : OpenGL.PhysicallyBased.PhysicallyBasedGeometry
          PhysicallyBasedQuad : OpenGL.PhysicallyBased.PhysicallyBasedGeometry
          PhysicallyBasedTerrainGeometries : Dictionary<TerrainGeometryDescriptor, Dictionary<TerrainPatchDescriptor, OpenGL.PhysicallyBased.PhysicallyBasedGeometry>>
          PhysicallyBasedTerrainGeometriesUtilized : TerrainGeometryDescriptor HashSet
          CubeMap : OpenGL.Texture.Texture
          WhiteTexture : OpenGL.Texture.Texture
          BlackTexture : OpenGL.Texture.Texture
          BrdfTexture : OpenGL.Texture.Texture
          ReflectionRenderbuffer : uint
          ReflectionFramebuffer : uint
          IrradianceMapRenderbuffer : uint
          IrradianceMapFramebuffer : uint
          EnvironmentFilterRenderbuffer : uint
          EnvironmentFilterFramebuffer : uint
          IrradianceMap : OpenGL.Texture.Texture
          EnvironmentFilterMap : OpenGL.Texture.Texture
          PhysicallyBasedMaterial : OpenGL.PhysicallyBased.PhysicallyBasedMaterial
          mutable PhysicallyBasedBuffers : OpenGL.PhysicallyBased.PhysicallyBasedBuffers
          LightMaps : Dictionary<uint64, OpenGL.LightMap.LightMap>
          mutable LightingConfig : Lighting3dConfig
          mutable LightingConfigChanged : bool
          mutable RendererConfig : Renderer3dConfig
          mutable RendererConfigChanged : bool
          mutable InstanceFields : single array
          mutable UserDefinedStaticModelFields : single array
          ForwardSurfacesComparer : IComparer<struct (single * single * Matrix4x4 * bool * Presence * Box2 * MaterialProperties * Matrix4x4 array voption * OpenGL.PhysicallyBased.PhysicallyBasedSurface * DepthTest * single * int)>
          ForwardSurfacesSortBuffer : struct (single * single * Matrix4x4 * bool * Presence * Box2 * MaterialProperties * Matrix4x4 array voption * OpenGL.PhysicallyBased.PhysicallyBasedSurface * DepthTest * single * int) List
          RenderPackages : Packages<RenderAsset, AssetClient>
          mutable RenderPasses : Dictionary<RenderPass, RenderTasks>
          mutable RenderPasses2 : Dictionary<RenderPass, RenderTasks>
          mutable RenderPackageCachedOpt : RenderPackageCached
          mutable RenderAssetCached : RenderAssetCached
          mutable ReloadAssetsRequested : bool
          RenderMessages : RenderMessage3d List }

    static member private radicalInverse (bits : uint) =
        let mutable bits = bits
        bits <- (bits <<< 16) ||| (bits >>> 16);
        bits <- ((bits &&& 0x55555555u) <<< 1) ||| ((bits &&& 0xAAAAAAAAu) >>> 1)
        bits <- ((bits &&& 0x33333333u) <<< 2) ||| ((bits &&& 0xCCCCCCCCu) >>> 2)
        bits <- ((bits &&& 0x0F0F0F0Fu) <<< 4) ||| ((bits &&& 0xF0F0F0F0u) >>> 4)
        bits <- ((bits &&& 0x00FF00FFu) <<< 8) ||| ((bits &&& 0xFF00FF00u) >>> 8)
        single bits * 2.3283064365386963e-10f

    static member private hammersley (i : int) (N : int) =
        v2 (single i / single N) (GlRenderer3d.radicalInverse (uint i))

    static member private importanceSampleGGX (xi : Vector2) (roughness : single) (n : Vector3) =

        // compute dependencies
        let a = roughness * roughness
        let phi = MathF.TWO_PI * xi.X
        let cosTheta = sqrt ((1.0f - xi.Y) / (1.0f + (a * a - 1.0f) * xi.Y))
        let sinTheta = sqrt (1.0f - cosTheta * cosTheta)
        
        // from spherical coordinates to cartesian coordinates
        let mutable h = v3Zero
        h.X <- cos phi * sinTheta
        h.Y <- sin phi * sinTheta
        h.Z <- cosTheta

        // from tangent-space vector to world-space sample vector
        let up = if abs n.Z < 0.999f then v3Back else v3Right
        let tangent = (up.Cross n).Normalized
        let bitangent = n.Cross tangent

        // importance sample
        (tangent * h.X + bitangent * h.Y + n * h.Z).Normalized

    static member private geometrySchlickGGX (nDotV : single) (roughness : single) =
        let a = roughness
        let k = a * a / 2.0f
        let nom = nDotV
        let denom = nDotV * (1.0f - k) + k
        nom / denom

    static member private geometrySmith (roughness : single) (nov : single) (nol : single) =
        let ggx2 = GlRenderer3d.geometrySchlickGGX nov roughness
        let ggx1 = GlRenderer3d.geometrySchlickGGX nol roughness
        ggx1 * ggx2
        
    static member private integrateBrdf (nDotV : single) (roughness : single) (samples : int) =
        let mutable v = v3Zero
        v.X <- sqrt (1.0f - nDotV * nDotV)
        v.Y <- 0.0f
        v.Z <- nDotV
        let mutable a = 0.0f
        let mutable b = 0.0f
        let n = v3Back
        for i in 0 .. dec samples do
            let xi = GlRenderer3d.hammersley i samples
            let h = GlRenderer3d.importanceSampleGGX xi roughness n
            let l = (2.0f * v.Dot h * h - v).Normalized
            let nol = max l.Z 0.0f
            let noh = max h.Z 0.0f
            let voh = max (v.Dot h) 0.0f
            let nov = max (n.Dot v) 0.0f
            if nol > 0.0f then
                let g = GlRenderer3d.geometrySmith roughness nov nol
                let gVis = (g * voh) / (noh * nov)
                let fc = pown (1.0f - voh) 5
                a <- a + (1.0f - fc) * gVis
                b <- b + fc * gVis
        v2 (a / single samples) (b / single samples)

    static member private invalidateCaches renderer =
        renderer.RenderPackageCachedOpt <- Unchecked.defaultof<_>
        renderer.RenderAssetCached.CachedAssetTagOpt <- Unchecked.defaultof<_>
        renderer.RenderAssetCached.CachedRenderAsset <- RawAsset

    static member private clearRenderPasses renderer =
        renderer.RenderPasses.Clear ()
        renderer.RenderPasses2.Clear ()

    static member private tryLoadTextureAsset (assetClient : AssetClient) (asset : Asset) renderer =
        GlRenderer3d.invalidateCaches renderer
        match assetClient.TextureClient.TryCreateTextureFiltered (true, OpenGL.Texture.BlockCompressable asset.FilePath, asset.FilePath) with
        | Right texture ->
            Some texture
        | Left error ->
            Log.info ("Could not load texture '" + asset.FilePath + "' due to '" + error + "'.")
            None

    static member private tryLoadCubeMapAsset (assetClient : AssetClient) (asset : Asset) renderer =
        GlRenderer3d.invalidateCaches renderer
        match File.ReadAllLines asset.FilePath |> Array.filter (String.IsNullOrWhiteSpace >> not) with
        | [|faceRightFilePath; faceLeftFilePath; faceTopFilePath; faceBottomFilePath; faceBackFilePath; faceFrontFilePath|] ->
            let dirPath = PathF.GetDirectoryName asset.FilePath
            let faceRightFilePath = dirPath + "/" + faceRightFilePath.Trim ()
            let faceLeftFilePath = dirPath + "/" + faceLeftFilePath.Trim ()
            let faceTopFilePath = dirPath + "/" + faceTopFilePath.Trim ()
            let faceBottomFilePath = dirPath + "/" + faceBottomFilePath.Trim ()
            let faceBackFilePath = dirPath + "/" + faceBackFilePath.Trim ()
            let faceFrontFilePath = dirPath + "/" + faceFrontFilePath.Trim ()
            let cubeMapKey = (faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath)
            match assetClient.CubeMapClient.TryCreateCubeMap cubeMapKey with
            | Right cubeMap -> Some (cubeMapKey, cubeMap, ref None)
            | Left error -> Log.info ("Could not load cube map '" + asset.FilePath + "' due to: " + error); None
        | _ -> Log.info ("Could not load cube map '" + asset.FilePath + "' due to requiring exactly 6 file paths with each file path on its own line."); None

    static member private tryLoadModelAsset (assetClient : AssetClient) (asset : Asset) renderer =
        GlRenderer3d.invalidateCaches renderer
        match assetClient.SceneClient.TryCreatePhysicallyBasedModel (true, asset.FilePath, renderer.PhysicallyBasedMaterial, assetClient.TextureClient) with
        | Right model -> Some model
        | Left error -> Log.info ("Could not load model '" + asset.FilePath + "' due to: " + error); None

    static member private tryLoadRawAsset (asset : Asset) renderer =
        GlRenderer3d.invalidateCaches renderer
        if File.Exists asset.FilePath
        then Some ()
        else None

    static member private tryLoadRenderAsset (assetClient : AssetClient) (asset : Asset) renderer =
        GlRenderer3d.invalidateCaches renderer
        match PathF.GetExtensionLower asset.FilePath with
        | RawExtension _ ->
            match GlRenderer3d.tryLoadRawAsset asset renderer with
            | Some () -> Some RawAsset
            | None -> None
        | ImageExtension _ ->
            match GlRenderer3d.tryLoadTextureAsset assetClient asset renderer with
            | Some texture -> Some (TextureAsset texture)
            | None -> None
        | CubeMapExtension _ ->
            match GlRenderer3d.tryLoadCubeMapAsset assetClient asset renderer with
            | Some (cubeMapKey, cubeMap, opt) -> Some (CubeMapAsset (cubeMapKey, cubeMap, opt))
            | None -> None
        | ModelExtension _ ->
            match GlRenderer3d.tryLoadModelAsset assetClient asset renderer with
            | Some model ->
                if model.Animated
                then Some (AnimatedModelAsset model)
                else Some (StaticModelAsset (false, model))
            | None -> None
        | _ -> None

    static member private freeRenderAsset renderAsset renderer =
        GlRenderer3d.invalidateCaches renderer
        match renderAsset with
        | RawAsset -> () // nothing to do
        | TextureAsset texture -> texture.Destroy ()
        | FontAsset (_, font) -> SDL_ttf.TTF_CloseFont font
        | CubeMapAsset (_, cubeMap, _) -> cubeMap.Destroy ()
        | StaticModelAsset (_, model) -> OpenGL.PhysicallyBased.DestroyPhysicallyBasedModel model
        | AnimatedModelAsset model -> OpenGL.PhysicallyBased.DestroyPhysicallyBasedModel model
        OpenGL.Hl.Assert ()

    static member private tryLoadRenderPackage packageName renderer =

        // make a new asset graph and load its assets
        let assetGraph = AssetGraph.makeFromFileOpt Assets.Global.AssetGraphFilePath
        match AssetGraph.tryCollectAssetsFromPackage (Some Constants.Associations.Render3d) packageName assetGraph with
        | Right assetsCollected ->

            // log when image assets are being shared between 2d and 3d renders
            if List.exists (fun (asset : Asset) ->
                let extension = PathF.GetExtensionLower asset.FilePath
                match extension with
                | ImageExtension _ ->
                    asset.Associations.Contains Constants.Associations.Render2d &&
                    asset.Associations.Contains Constants.Associations.Render3d
                | _ -> false)
                assetsCollected then
                Log.warnOnce "Due to asset graph limitations, associating image assets with both Render2d and Render3d is not fully supported."

            // find or create render package
            let renderPackage =
                match Dictionary.tryFind packageName renderer.RenderPackages with
                | Some renderPackage -> renderPackage
                | None ->
                    let assetClient =
                        AssetClient
                            (OpenGL.Texture.TextureClient (Some renderer.LazyTextureQueues),
                                OpenGL.CubeMap.CubeMapClient (),
                                OpenGL.PhysicallyBased.PhysicallyBasedSceneClient ())
                    let renderPackage = { Assets = dictPlus StringComparer.Ordinal []; PackageState = assetClient }
                    renderer.RenderPackages.[packageName] <- renderPackage
                    renderPackage

            // categorize existing assets based on the required action
            let assetsExisting = renderPackage.Assets
            let assetsToFree = Dictionary ()
            let assetsToKeep = Dictionary ()
            for assetEntry in assetsExisting do
                let assetName = assetEntry.Key
                let (lastWriteTime, asset, renderAsset) = assetEntry.Value
                let lastWriteTime' =
                    try DateTimeOffset (File.GetLastWriteTime asset.FilePath)
                    with exn -> Log.info ("Asset file write time read error due to: " + scstring exn); DateTimeOffset.MinValue.DateTime
                if lastWriteTime < lastWriteTime'
                then assetsToFree.Add (asset.FilePath, renderAsset)
                else assetsToKeep.Add (assetName, (lastWriteTime, asset, renderAsset))

            // free assets, including memo entries
            for assetEntry in assetsToFree do
                let filePath = assetEntry.Key
                let renderAsset = assetEntry.Value
                match renderAsset with
                | RawAsset -> ()
                | TextureAsset _ -> renderPackage.PackageState.TextureClient.Textures.Remove filePath |> ignore<bool>
                | FontAsset _ -> ()
                | CubeMapAsset (cubeMapKey, _, _) -> renderPackage.PackageState.CubeMapClient.CubeMaps.Remove cubeMapKey |> ignore<bool>
                | StaticModelAsset _ | AnimatedModelAsset _ -> renderPackage.PackageState.SceneClient.Scenes.Remove filePath |> ignore<bool>
                GlRenderer3d.freeRenderAsset renderAsset renderer

            // categorize assets to load
            let assetsToLoad = HashSet ()
            for asset in assetsCollected do
                if not (assetsToKeep.ContainsKey asset.AssetTag.AssetName) then
                    assetsToLoad.Add asset |> ignore<bool>

            // preload assets
            renderPackage.PackageState.PreloadAssets (false, assetsToLoad)

            // load assets
            let assetsLoaded = Dictionary ()
            for asset in assetsToLoad do
                match GlRenderer3d.tryLoadRenderAsset renderPackage.PackageState asset renderer with
                | Some renderAsset ->
                    let lastWriteTime =
                        try DateTimeOffset (File.GetLastWriteTime asset.FilePath)
                        with exn -> Log.info ("Asset file write time read error due to: " + scstring exn); DateTimeOffset.MinValue.DateTime
                    assetsLoaded.[asset.AssetTag.AssetName] <- (lastWriteTime, asset, renderAsset)
                | None -> ()

            // update assets to keep
            let assetsUpdated =
                [|for assetEntry in assetsToKeep do
                    let assetName = assetEntry.Key
                    let (lastWriteTime, asset, renderAsset) = assetEntry.Value
                    let dirPath = PathF.GetDirectoryName asset.FilePath
                    let renderAsset =
                        match renderAsset with
                        | RawAsset | TextureAsset _ | FontAsset _ | CubeMapAsset _ ->
                            renderAsset
                        | StaticModelAsset (userDefined, staticModel) ->
                            match staticModel.SceneOpt with
                            | Some scene when not userDefined ->
                                let surfaces =
                                    [|for surface in staticModel.Surfaces do
                                        let material = scene.Materials.[surface.SurfaceMaterialIndex]
                                        let (_, material) = OpenGL.PhysicallyBased.CreatePhysicallyBasedMaterial (true, dirPath, renderer.PhysicallyBasedMaterial, renderPackage.PackageState.TextureClient, material)
                                        { surface with SurfaceMaterial = material }|]
                                StaticModelAsset (userDefined, { staticModel with Surfaces = surfaces })
                            | Some _ | None -> renderAsset
                        | AnimatedModelAsset animatedModel ->
                            match animatedModel.SceneOpt with
                            | Some scene ->
                                let surfaces =
                                    [|for surface in animatedModel.Surfaces do
                                        let material = scene.Materials.[surface.SurfaceMaterialIndex]
                                        let (_, material) = OpenGL.PhysicallyBased.CreatePhysicallyBasedMaterial (true, dirPath, renderer.PhysicallyBasedMaterial, renderPackage.PackageState.TextureClient, material)
                                        { surface with SurfaceMaterial = material }|]
                                AnimatedModelAsset { animatedModel with Surfaces = surfaces }
                            | None -> renderAsset
                    KeyValuePair (assetName, (lastWriteTime, asset, renderAsset))|]

            // insert assets into package
            for assetEntry in Seq.append assetsUpdated assetsLoaded do
                let assetName = assetEntry.Key
                let (lastWriteTime, asset, renderAsset) = assetEntry.Value
                renderPackage.Assets.[assetName] <- (lastWriteTime, asset, renderAsset)

        // handle error cases
        | Left failedAssetNames ->
            Log.info ("Render package load failed due to unloadable assets '" + failedAssetNames + "' for package '" + packageName + "'.")

    static member private tryGetRenderAsset (assetTag : AssetTag) renderer =
        let mutable assetInfo = Unchecked.defaultof<DateTimeOffset * Asset * RenderAsset> // OPTIMIZATION: seems like TryGetValue allocates here if we use the tupling idiom (this may only be the case in Debug builds tho).
        if  renderer.RenderAssetCached.CachedAssetTagOpt :> obj |> notNull &&
            assetEq assetTag renderer.RenderAssetCached.CachedAssetTagOpt then
            renderer.RenderAssetCached.CachedAssetTagOpt <- assetTag // NOTE: this isn't redundant because we want to trigger refEq early-out.
            ValueSome renderer.RenderAssetCached.CachedRenderAsset
        elif
            renderer.RenderPackageCachedOpt :> obj |> notNull &&
            renderer.RenderPackageCachedOpt.CachedPackageName = assetTag.PackageName then
            let assets = renderer.RenderPackageCachedOpt.CachedPackageAssets
            if assets.TryGetValue (assetTag.AssetName, &assetInfo) then
                let asset = Triple.thd assetInfo
                renderer.RenderAssetCached.CachedAssetTagOpt <- assetTag
                renderer.RenderAssetCached.CachedRenderAsset <- asset
                ValueSome asset
            else ValueNone
        else
            match Dictionary.tryFind assetTag.PackageName renderer.RenderPackages with
            | Some package ->
                renderer.RenderPackageCachedOpt <- { CachedPackageName = assetTag.PackageName; CachedPackageAssets = package.Assets }
                if package.Assets.TryGetValue (assetTag.AssetName, &assetInfo) then
                    let asset = Triple.thd assetInfo
                    renderer.RenderAssetCached.CachedAssetTagOpt <- assetTag
                    renderer.RenderAssetCached.CachedRenderAsset <- asset
                    ValueSome asset
                else ValueNone
            | None ->
                Log.info ("Loading Render3d package '" + assetTag.PackageName + "' for asset '" + assetTag.AssetName + "' on the fly.")
                GlRenderer3d.tryLoadRenderPackage assetTag.PackageName renderer
                match renderer.RenderPackages.TryGetValue assetTag.PackageName with
                | (true, package) ->
                    renderer.RenderPackageCachedOpt <- { CachedPackageName = assetTag.PackageName; CachedPackageAssets = package.Assets }
                    if package.Assets.TryGetValue (assetTag.AssetName, &assetInfo) then
                        let asset = Triple.thd assetInfo
                        renderer.RenderAssetCached.CachedAssetTagOpt <- assetTag
                        renderer.RenderAssetCached.CachedRenderAsset <- asset
                        ValueSome asset
                    else ValueNone
                | (false, _) -> ValueNone

    static member private tryGetFilePath (assetTag : AssetTag) renderer =
        match GlRenderer3d.tryGetRenderAsset assetTag renderer with
        | ValueSome _ ->
            match renderer.RenderPackages.TryGetValue assetTag.PackageName with
            | (true, package) ->
                match package.Assets.TryGetValue assetTag.AssetName with
                | (true, (_, asset, _)) -> Some asset.FilePath
                | (false, _) -> None
            | (false, _) -> None
        | ValueNone -> None

    static member private tryGetTextureData minimal (assetTag : Image AssetTag) renderer =
        match GlRenderer3d.tryGetFilePath assetTag renderer with
        | Some filePath ->
            match OpenGL.Texture.TryCreateTextureData (minimal, filePath) with
            | Some textureData ->
                let metadata = textureData.Metadata
                let (blockCompressed, bytes) = textureData.Bytes
                textureData.Dispose ()
                Some (metadata, blockCompressed, bytes)
            | None -> None
        | None -> None

    static member private tryGetHeightMapResolution heightMap renderer =
        match heightMap with
        | ImageHeightMap image ->
            match GlRenderer3d.tryGetRenderAsset image renderer with
            | ValueSome renderAsset ->
                match renderAsset with
                | TextureAsset texture ->
                    let metadata = texture.TextureMetadata
                    Some (metadata.TextureWidth, metadata.TextureHeight)
                | _ -> None
            | ValueNone -> None
        | RawHeightMap map -> Some (map.Resolution.X, map.Resolution.Y)

    static member private tryDestroyUserDefinedStaticModel assetTag renderer =

        // ensure target package is loaded if possible
        if not (renderer.RenderPackages.ContainsKey assetTag.PackageName) then
            GlRenderer3d.tryLoadRenderPackage assetTag.PackageName renderer

        // free any existing user-created static model, also determining if target asset can be user-created
        match renderer.RenderPackages.TryGetValue assetTag.PackageName with
        | (true, package) ->
            match package.Assets.TryGetValue assetTag.AssetName with
            | (true, (_, _, asset)) ->
                match asset with
                | StaticModelAsset (userDefined, _) when userDefined -> GlRenderer3d.freeRenderAsset asset renderer
                | _ -> ()
            | (false, _) -> ()
        | (false, _) -> ()

    static member private tryCreateUserDefinedStaticModel surfaceDescriptors bounds (assetTag : StaticModel AssetTag) renderer =

        // ensure target package is loaded if possible
        if not (renderer.RenderPackages.ContainsKey assetTag.PackageName) then
            GlRenderer3d.tryLoadRenderPackage assetTag.PackageName renderer

        // determine if target asset can be created
        let canCreateUserDefinedStaticModel =
            match renderer.RenderPackages.TryGetValue assetTag.PackageName with
            | (true, package) -> not (package.Assets.ContainsKey assetTag.AssetName)
            | (false, _) -> true

        // ensure the user can create the static model
        if canCreateUserDefinedStaticModel then

            // create surfaces
            let surfaces = List ()
            for (surfaceDescriptor : StaticModelSurfaceDescriptor) in surfaceDescriptors do

                // get albedo metadata and texture
                let albedoTexture =
                    match GlRenderer3d.tryGetRenderAsset surfaceDescriptor.AlbedoImage renderer with
                    | ValueSome (TextureAsset texture) -> texture
                    | _ -> renderer.PhysicallyBasedMaterial.AlbedoTexture

                // make material properties
                let properties : OpenGL.PhysicallyBased.PhysicallyBasedMaterialProperties =
                    { Albedo = surfaceDescriptor.MaterialProperties.Albedo
                      Roughness = surfaceDescriptor.MaterialProperties.Roughness
                      Metallic = surfaceDescriptor.MaterialProperties.Metallic
                      AmbientOcclusion = surfaceDescriptor.MaterialProperties.AmbientOcclusion
                      Emission = surfaceDescriptor.MaterialProperties.Emission
                      Height = surfaceDescriptor.MaterialProperties.Height
                      IgnoreLightMaps = surfaceDescriptor.MaterialProperties.IgnoreLightMaps
                      OpaqueDistance = surfaceDescriptor.MaterialProperties.OpaqueDistance
                      FinenessOffset = surfaceDescriptor.MaterialProperties.FinenessOffset
                      ScatterType = surfaceDescriptor.MaterialProperties.ScatterType
                      SpecularScalar = surfaceDescriptor.MaterialProperties.SpecularScalar
                      RefractiveIndex = surfaceDescriptor.MaterialProperties.RefractiveIndex }

                // make material
                let material : OpenGL.PhysicallyBased.PhysicallyBasedMaterial =
                    { AlbedoTexture = albedoTexture
                      RoughnessTexture = match GlRenderer3d.tryGetRenderAsset surfaceDescriptor.RoughnessImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.RoughnessTexture
                      MetallicTexture = match GlRenderer3d.tryGetRenderAsset surfaceDescriptor.MetallicImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.MetallicTexture
                      AmbientOcclusionTexture = match GlRenderer3d.tryGetRenderAsset surfaceDescriptor.AmbientOcclusionImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.AmbientOcclusionTexture
                      EmissionTexture = match GlRenderer3d.tryGetRenderAsset surfaceDescriptor.EmissionImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.EmissionTexture
                      NormalTexture = match GlRenderer3d.tryGetRenderAsset surfaceDescriptor.NormalImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.NormalTexture
                      HeightTexture = match GlRenderer3d.tryGetRenderAsset surfaceDescriptor.HeightImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.HeightTexture
                      SubdermalTexture = match GlRenderer3d.tryGetRenderAsset surfaceDescriptor.SubdermalImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.SubdermalTexture
                      FinenessTexture = match GlRenderer3d.tryGetRenderAsset surfaceDescriptor.FinenessImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.FinenessTexture
                      ScatterTexture = match GlRenderer3d.tryGetRenderAsset surfaceDescriptor.ScatterImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.ScatterTexture
                      TwoSided = surfaceDescriptor.TwoSided
                      Clipped = surfaceDescriptor.Clipped
                      Names = "" }

                // create vertex data, truncating it when required
                let vertexCount = surfaceDescriptor.Positions.Length
                let elementCount = vertexCount * 8
                if  renderer.UserDefinedStaticModelFields.Length < elementCount then
                    renderer.UserDefinedStaticModelFields <- Array.zeroCreate elementCount // TODO: grow this by power of two.
                let vertexData = renderer.UserDefinedStaticModelFields.AsMemory (0, elementCount)
                let mutable i = 0
                try
                    let vertexData = vertexData.Span
                    while i < vertexCount do
                        let u = i * 8
                        vertexData.[u] <- surfaceDescriptor.Positions.[i].X
                        vertexData.[u+1] <- surfaceDescriptor.Positions.[i].Y
                        vertexData.[u+2] <- surfaceDescriptor.Positions.[i].Z
                        vertexData.[u+3] <- surfaceDescriptor.TexCoordses.[i].X
                        vertexData.[u+4] <- surfaceDescriptor.TexCoordses.[i].Y
                        vertexData.[u+5] <- surfaceDescriptor.Normals.[i].X
                        vertexData.[u+6] <- surfaceDescriptor.Normals.[i].Y
                        vertexData.[u+7] <- surfaceDescriptor.Normals.[i].Z
                        i <- inc i
                with :? IndexOutOfRangeException ->
                    Log.info "Vertex data truncated due to an unequal count among surface descriptor Positions, TexCoordses, and Normals."

                // create index data
                let indexData = surfaceDescriptor.Indices.AsMemory ()

                // create geometry
                let geometry = OpenGL.PhysicallyBased.CreatePhysicallyBasedStaticGeometry (true, OpenGL.PrimitiveType.Triangles, vertexData, indexData, surfaceDescriptor.Bounds) // TODO: consider letting user specify primitive drawing type.

                // create surface
                let surface = OpenGL.PhysicallyBased.CreatePhysicallyBasedSurface (Array.empty, surfaceDescriptor.ModelMatrix, surfaceDescriptor.Bounds, properties, material, -1, Assimp.Node.Empty, geometry)
                surfaces.Add surface

            // create user-defined static model
            let surfaces = Seq.toArray surfaces
            let hierarchy = TreeNode (Array.map OpenGL.PhysicallyBased.PhysicallyBasedSurface surfaces)
            let model : OpenGL.PhysicallyBased.PhysicallyBasedModel =
                { Animated = false
                  Bounds = bounds
                  LightProbes = [||]
                  Lights = [||]
                  Surfaces = surfaces
                  SceneOpt = None
                  PhysicallyBasedHierarchy = hierarchy }

            // assign model as appropriate render package asset
            match renderer.RenderPackages.TryGetValue assetTag.PackageName with
            | (true, package) ->
                let asset = Asset.make assetTag "" [] (Set.singleton Constants.Associations.Render3d)
                package.Assets.[assetTag.AssetName] <- (DateTimeOffset.MinValue.DateTime, asset, StaticModelAsset (true, model))
            | (false, _) ->
                let assetClient =
                    AssetClient
                        (OpenGL.Texture.TextureClient (Some renderer.LazyTextureQueues),
                         OpenGL.CubeMap.CubeMapClient (),
                         OpenGL.PhysicallyBased.PhysicallyBasedSceneClient ())
                let asset = Asset.make assetTag "" [] (Set.singleton Constants.Associations.Render3d)
                let package = { Assets = Dictionary.singleton StringComparer.Ordinal assetTag.AssetName (DateTimeOffset.MinValue.DateTime, asset, StaticModelAsset (true, model)); PackageState = assetClient }
                renderer.RenderPackages.[assetTag.PackageName] <- package

        // attempted to replace a loaded asset
        else Log.info ("Cannot replace a loaded asset '" + scstring assetTag + "' with a user-created static model.")

    static member private getRenderTasks renderPass renderer =
        let mutable renderTasks = Unchecked.defaultof<RenderTasks> // OPTIMIZATION: seems like TryGetValue allocates here if we use the tupling idiom (this may only be the case in Debug builds tho).
        if renderer.RenderPasses.TryGetValue (renderPass, &renderTasks)
        then renderTasks
        else
            let displacedPasses =
                [for entry in renderer.RenderPasses do
                    if RenderPass.displaces renderPass entry.Key then
                        entry]
            for displacedPass in displacedPasses do
                renderer.RenderPasses.Remove displacedPass.Key |> ignore<bool>
            let renderTasks =
                match displacedPasses with
                | head :: _ ->
                    let recycledTasks = head.Value
                    RenderTasks.clear recycledTasks
                    recycledTasks
                | _ -> RenderTasks.make ()
            renderer.RenderPasses.Add (renderPass, renderTasks)
            renderTasks

    static member private getLastSkyBoxOpt renderPass renderer =
        let renderTasks = GlRenderer3d.getRenderTasks renderPass renderer
        match Seq.tryLast renderTasks.SkyBoxes with
        | Some (lightAmbientColor, lightAmbientBrightness, cubeMapColor, cubeMapBrightness, cubeMapAsset) ->
            match GlRenderer3d.tryGetRenderAsset cubeMapAsset renderer with
            | ValueSome asset ->
                match asset with
                | CubeMapAsset (_, cubeMap, cubeMapIrradianceAndEnvironmentMapOptRef) ->
                    let cubeMapOpt = Some (cubeMapColor, cubeMapBrightness, cubeMap, cubeMapIrradianceAndEnvironmentMapOptRef)
                    (lightAmbientColor, lightAmbientBrightness, cubeMapOpt)
                | _ ->
                    Log.info "Could not utilize sky box due to mismatched cube map asset."
                    (lightAmbientColor, lightAmbientBrightness, None)
            | ValueNone ->
                Log.info "Could not utilize sky box due to non-existent cube map asset."
                (lightAmbientColor, lightAmbientBrightness, None)
        | None -> (Color.White, 1.0f, None)
        
    static member private tryComputeTerrainPatches (geometryDescriptor : TerrainGeometryDescriptor) (heightMapMetadata : HeightMapMetadata) =

        // clamp patch count and validate terrain resolution
        let patches = geometryDescriptor.Patches
        let patches = v2i (max 1 patches.X) (max 1 patches.Y)
        let patches = v2i (min 64 patches.X) (min 64 patches.Y)
        let terrainBounds = geometryDescriptor.Bounds
        let terrainResolution = heightMapMetadata.Resolution
        if  dec terrainResolution.X >= 1 &&
            dec terrainResolution.Y >= 1 then

            // compute patch resolution
            let patchResolution =
                if  patches.X >= dec terrainResolution.X ||
                    patches.Y >= dec terrainResolution.Y then
                    v2iDup 2
                else
                    let patchResolutionX = single (dec terrainResolution.X) / single patches.X |> ceil |> int |> inc
                    let patchResolutionY = single (dec terrainResolution.Y) / single patches.Y |> ceil |> int |> inc
                    v2i patchResolutionX patchResolutionY

            // handle single patch case
            if  patchResolution.X > terrainResolution.X ||
                patchResolution.Y > terrainResolution.Y ||
                patchResolution = terrainResolution then
                [|{ PatchId = v2iZero
                    PatchBounds = geometryDescriptor.Bounds
                    PatchOffset = v2iZero
                    PatchResolution = terrainResolution }|] |> Some
            
            // otherwise, handle multi-patch case
            else

                // compute patch cell size
                let cellSize =
                    v3
                        (terrainBounds.Size.X / single terrainResolution.X)
                        terrainBounds.Size.Y
                        (terrainBounds.Size.Z / single terrainResolution.Y)

                // compute full patch size (without underflow taken into account)
                let patchSizeFull =
                    v3
                        (single terrainResolution.X * cellSize.X)
                        terrainBounds.Size.Y
                        (single terrainResolution.Y * cellSize.Z)

                // compute patches
                [|for patchY in 0 .. dec patches.Y do
                    for patchX in 0 .. dec patches.X do

                        // compute patch min
                        let patchOffset =
                            v2i
                                (patchX * dec patchResolution.X)
                                (patchY * dec patchResolution.Y)
                        let patchMin =
                            v3
                                (terrainBounds.Min.X + single patchOffset.X * cellSize.X)
                                terrainBounds.Min.Y
                                (terrainBounds.Min.Z + single patchOffset.Y * cellSize.Z)

                        // compute patch size
                        let patchOverflow =
                            v2i
                                (patchOffset.X + patchResolution.X - terrainResolution.X |> max 0)
                                (patchOffset.Y + patchResolution.Y - terrainResolution.Y |> max 0)
                        let patchResolution =
                            patchResolution - patchOverflow
                        let patchSize =
                            v3
                                (single patchResolution.X * cellSize.X)
                                patchSizeFull.Y
                                (single patchResolution.Y * cellSize.Z)

                        // fin
                        { PatchId = v2i patchX patchY
                          PatchResolution = patchResolution
                          PatchBounds = box3 patchMin patchSize
                          PatchOffset = patchOffset }|] |> Some

        // invalid terrain resolution
        else Log.error "Terrain resolution must be at least 2x2."; None

    static member private createPhysicallyBasedTerrainNormals (resolution : Vector2i) (positionsAndTexCoordses : struct (Vector3 * Vector2) array) =
        [|for y in 0 .. dec resolution.Y do
            for x in 0 .. dec resolution.X do
                if x > 0 && y > 0 && x < dec resolution.X && y < dec resolution.Y then
                    let v  = fst' positionsAndTexCoordses.[resolution.X * y + x]
                    let n  = fst' positionsAndTexCoordses.[resolution.X * dec y + x]
                    let ne = fst' positionsAndTexCoordses.[resolution.X * dec y + inc x]
                    let e  = fst' positionsAndTexCoordses.[resolution.X * y + inc x]
                    let s  = fst' positionsAndTexCoordses.[resolution.X * inc y + x]
                    let sw = fst' positionsAndTexCoordses.[resolution.X * inc y + dec x]
                    let w  = fst' positionsAndTexCoordses.[resolution.X * y + dec x]
                    let normalSum =
                        Vector3.Cross (ne - v, n - v) +
                        Vector3.Cross (e - v,  ne - v) +
                        Vector3.Cross (s - v,  e - v) +
                        Vector3.Cross (sw - v, s - v) +
                        Vector3.Cross (w - v,  sw - v) +
                        Vector3.Cross (n - v,  w - v)
                    let normal = normalSum.Normalized
                    normal
                else v3Up|]

    static member private tryCreatePhysicallyBasedTerrainGeometryData (geometryDescriptor : TerrainGeometryDescriptor) (heightMapMetadata : HeightMapMetadata) renderer =

        // compute normals
        let resolution = heightMapMetadata.Resolution
        let positionsAndTexCoordses = heightMapMetadata.PositionsAndTexCoordses
        let normalsOpt =
            match geometryDescriptor.NormalImageOpt with
            | Some normalImage ->
                match GlRenderer3d.tryGetTextureData false normalImage renderer with
                | Some (metadata, blockCompressed, bytes) ->
                    if metadata.TextureWidth * metadata.TextureHeight = positionsAndTexCoordses.Length then
                        if not blockCompressed then
                            let scalar = 1.0f / single Byte.MaxValue
                            bytes
                            |> Array.map (fun b -> single b * scalar)
                            |> Array.chunkBySize 4
                            |> Array.map (fun b ->
                                let tangent = (v3 b.[2] b.[1] b.[0] * 2.0f - v3One).Normalized
                                let normal = v3 tangent.X tangent.Z -tangent.Y
                                normal)
                            |> Some
                        else Log.info "Block-compressed images not supported for terrain normal images."; None
                    else Log.info "Normal image resolution does not match terrain resolution."; None
                | None -> Some (GlRenderer3d.createPhysicallyBasedTerrainNormals resolution positionsAndTexCoordses)
            | None -> Some (GlRenderer3d.createPhysicallyBasedTerrainNormals resolution positionsAndTexCoordses)

        // compute tint
        let tintOpt =
            match geometryDescriptor.TintImageOpt with
            | Some tintImage ->
                match GlRenderer3d.tryGetTextureData false tintImage renderer with
                | Some (metadata, blockCompressed, bytes) ->
                    if metadata.TextureWidth * metadata.TextureHeight = positionsAndTexCoordses.Length then
                        if not blockCompressed then
                            let scalar = 1.0f / single Byte.MaxValue
                            bytes
                            |> Array.map (fun b -> single b * scalar)
                            |> Array.chunkBySize 4
                            |> Array.map (fun b -> v3 b.[2] b.[1] b.[0])
                            |> Some
                        else Log.info "Block-compressed images not supported for terrain tint images."; None
                    else Log.info "Tint image resolution does not match terrain resolution."; None
                | None -> Some (Array.init positionsAndTexCoordses.Length (fun _ -> v3One))
            | _ -> Some (Array.init positionsAndTexCoordses.Length (fun _ -> v3One))

        // compute blendses, logging if more than the safe number of terrain layers is utilized
        // NOTE: there are 8 blends, each of which we account for regardless of TerrainLayersMax.
        let blendses = Array2D.zeroCreate<single> positionsAndTexCoordses.Length 8
        match geometryDescriptor.Material with
        | BlendMaterial blendMaterial ->
            if blendMaterial.TerrainLayers.Length > Constants.Render.TerrainLayersMax then
                Log.infoOnce
                    ("Terrain has more than " +
                        string Constants.Render.TerrainLayersMax +
                        " layers which references more than the number of supported fragment shader textures.")
            match blendMaterial.BlendMap with
            | RgbaMap rgbaMap ->
                match GlRenderer3d.tryGetTextureData false rgbaMap renderer with
                | Some (metadata, blockCompressed, bytes) ->
                    if metadata.TextureWidth * metadata.TextureHeight = positionsAndTexCoordses.Length then
                        if not blockCompressed then
                            let scalar = 1.0f / single Byte.MaxValue
                            for i in 0 .. dec positionsAndTexCoordses.Length do
                                // ARGB reverse byte order, from Drawing.Bitmap (windows).
                                // TODO: confirm it is the same for SDL (linux).
                                blendses.[i, 0] <- single bytes.[i * 4 + 2] * scalar
                                blendses.[i, 1] <- single bytes.[i * 4 + 1] * scalar
                                blendses.[i, 2] <- single bytes.[i * 4 + 0] * scalar
                                blendses.[i, 3] <- single bytes.[i * 4 + 3] * scalar
                        else Log.info "Block-compressed images not supported for terrain blend iamges."
                    else Log.info "Blend image resolution does not match terrain resolution."
                | None -> Log.info ("Could not locate texture data for blend image '" + scstring rgbaMap + "'.")
            | RedsMap reds ->
                let scalar = 1.0f / single Byte.MaxValue
                for i in 0 .. dec (min reds.Length 8) do
                    let red = reds.[i]
                    match GlRenderer3d.tryGetTextureData false red renderer with
                    | Some (metadata, blockCompressed, bytes) ->
                        if metadata.TextureWidth * metadata.TextureHeight = positionsAndTexCoordses.Length then
                            if not blockCompressed then
                                for j in 0 .. dec positionsAndTexCoordses.Length do
                                    blendses.[j, i] <- single bytes.[j * 4 + 2] * scalar
                            else Log.info "Block-compressed images not supported for terrain blend images."
                        else Log.info "Blend image resolution does not match terrain resolution."
                    | None -> Log.info ("Could not locate texture data for blend image '" + scstring red + "'.")
        | FlatMaterial _ ->
            for i in 0 .. dec positionsAndTexCoordses.Length do
                blendses.[i,0] <- 1.0f

        // ensure we've got usable input data
        match (normalsOpt, tintOpt) with
        | (Some normals, Some tint) ->
                
            // provide data necessary for creating geometry
            Some (positionsAndTexCoordses, normals, blendses, tint)

        // error
        | (_, _) -> None

    static member private makeBillboardMaterial (properties : MaterialProperties inref, material : Material inref, renderer) =
        let albedoTexture =
            match GlRenderer3d.tryGetRenderAsset material.AlbedoImage renderer with
            | ValueSome (TextureAsset texture) -> texture
            | _ -> renderer.PhysicallyBasedMaterial.AlbedoTexture
        let roughnessTexture =
            match GlRenderer3d.tryGetRenderAsset material.RoughnessImage renderer with
            | ValueSome (TextureAsset texture) -> texture
            | _ -> renderer.PhysicallyBasedMaterial.RoughnessTexture
        let metallicTexture =
            match GlRenderer3d.tryGetRenderAsset material.MetallicImage renderer with
            | ValueSome (TextureAsset texture) -> texture
            | _ -> renderer.PhysicallyBasedMaterial.MetallicTexture
        let ambientOcclusionTexture =
            match GlRenderer3d.tryGetRenderAsset material.AmbientOcclusionImage renderer with
            | ValueSome (TextureAsset texture) -> texture
            | _ -> renderer.PhysicallyBasedMaterial.AmbientOcclusionTexture
        let emissionTexture =
            match GlRenderer3d.tryGetRenderAsset material.EmissionImage renderer with
            | ValueSome (TextureAsset texture) -> texture
            | _ -> renderer.PhysicallyBasedMaterial.EmissionTexture
        let normalTexture =
            match GlRenderer3d.tryGetRenderAsset material.NormalImage renderer with
            | ValueSome (TextureAsset texture) -> texture
            | _ -> renderer.PhysicallyBasedMaterial.NormalTexture
        let heightTexture =
            match GlRenderer3d.tryGetRenderAsset material.HeightImage renderer with
            | ValueSome (TextureAsset texture) -> texture
            | _ -> renderer.PhysicallyBasedMaterial.HeightTexture
        let subdermalTexture =
            match GlRenderer3d.tryGetRenderAsset material.SubdermalImage renderer with
            | ValueSome (TextureAsset texture) -> texture
            | _ -> renderer.PhysicallyBasedMaterial.SubdermalTexture
        let finenessTexture =
            match GlRenderer3d.tryGetRenderAsset material.FinenessImage renderer with
            | ValueSome (TextureAsset texture) -> texture
            | _ -> renderer.PhysicallyBasedMaterial.FinenessTexture
        let scatterTexture =
            match GlRenderer3d.tryGetRenderAsset material.ScatterImage renderer with
            | ValueSome (TextureAsset texture) -> texture
            | _ -> renderer.PhysicallyBasedMaterial.ScatterTexture
        let properties : OpenGL.PhysicallyBased.PhysicallyBasedMaterialProperties =
            { Albedo = properties.Albedo
              Roughness = properties.Roughness
              Metallic = properties.Metallic
              AmbientOcclusion = properties.AmbientOcclusion
              Emission = properties.Emission
              Height = properties.Height
              IgnoreLightMaps = properties.IgnoreLightMaps
              OpaqueDistance = properties.OpaqueDistance
              FinenessOffset = properties.FinenessOffset
              ScatterType = properties.ScatterType
              SpecularScalar = properties.SpecularScalar
              RefractiveIndex = properties.RefractiveIndex }
        let material : OpenGL.PhysicallyBased.PhysicallyBasedMaterial =
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
              TwoSided = true
              Clipped = true
              Names = "" }
        struct (properties, material)

    static member private applySurfaceMaterial (material : Material inref, surfaceMaterial : OpenGL.PhysicallyBased.PhysicallyBasedMaterial inref, renderer) =
        let albedoTexture =
            match material.AlbedoImageOpt with
            | ValueSome image ->
                match GlRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.AlbedoTexture
            | ValueNone -> surfaceMaterial.AlbedoTexture
        let roughnessTexture =
            match material.RoughnessImageOpt with
            | ValueSome image ->
                match GlRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.RoughnessTexture
            | ValueNone -> surfaceMaterial.RoughnessTexture
        let metallicTexture =
            match material.MetallicImageOpt with
            | ValueSome image ->
                match GlRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.MetallicTexture
            | ValueNone -> surfaceMaterial.MetallicTexture
        let ambientOcclusionTexture =
            match material.AmbientOcclusionImageOpt with
            | ValueSome image ->
                match GlRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.AmbientOcclusionTexture
            | ValueNone -> surfaceMaterial.AmbientOcclusionTexture
        let emissionTexture =
            match material.EmissionImageOpt with
            | ValueSome image ->
                match GlRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.EmissionTexture
            | ValueNone -> surfaceMaterial.EmissionTexture
        let normalTexture =
            match material.NormalImageOpt with
            | ValueSome image ->
                match GlRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.NormalTexture
            | ValueNone -> surfaceMaterial.NormalTexture
        let heightTexture =
            match material.HeightImageOpt with
            | ValueSome image ->
                match GlRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.HeightTexture
            | ValueNone -> surfaceMaterial.HeightTexture
        let finenessTexture =
            match material.FinenessImageOpt with
            | ValueSome image ->
                match GlRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.FinenessTexture
            | ValueNone -> surfaceMaterial.FinenessTexture
        let subdermalTexture =
            match material.SubdermalImageOpt with
            | ValueSome image ->
                match GlRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.SubdermalTexture
            | ValueNone -> surfaceMaterial.SubdermalTexture
        let scatterTexture =
            match material.ScatterImageOpt with
            | ValueSome image ->
                match GlRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.ScatterTexture
            | ValueNone -> surfaceMaterial.ScatterTexture
        let twoSided =
            match material.TwoSidedOpt with
            | ValueSome twoSided -> twoSided
            | ValueNone -> surfaceMaterial.TwoSided
        let clipped =
            match material.ClippedOpt with
            | ValueSome clipped -> clipped
            | ValueNone -> surfaceMaterial.Clipped
        let surfaceMaterial : OpenGL.PhysicallyBased.PhysicallyBasedMaterial =
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
              Names = "" }
        surfaceMaterial

    static member private handleReloadShaders renderer =
        renderer.SkyBoxShader <- OpenGL.SkyBox.CreateSkyBoxShader Constants.Paths.SkyBoxShaderFilePath
        OpenGL.Hl.Assert ()
        renderer.IrradianceShader <- OpenGL.CubeMap.CreateCubeMapShader Constants.Paths.IrradianceShaderFilePath
        OpenGL.Hl.Assert ()
        renderer.EnvironmentFilterShader <- OpenGL.LightMap.CreateEnvironmentFilterShader Constants.Paths.EnvironmentFilterShaderFilePath
        OpenGL.Hl.Assert ()
        renderer.FilterShaders <- OpenGL.Filter.CreateFilterShaders ()
        OpenGL.Hl.Assert ()
        renderer.PhysicallyBasedShaders <- OpenGL.PhysicallyBased.CreatePhysicallyBasedShaders (Constants.Render.LightMapsMaxDeferred, Constants.Render.LightsMaxDeferred)
        OpenGL.Hl.Assert ()

    static member private handleLoadRenderPackage hintPackageName renderer =
        GlRenderer3d.tryLoadRenderPackage hintPackageName renderer

    static member private handleUnloadRenderPackage hintPackageName renderer =
        GlRenderer3d.invalidateCaches renderer
        match Dictionary.tryFind hintPackageName renderer.RenderPackages with
        | Some package ->
            for (_, _, asset) in package.Assets.Values do GlRenderer3d.freeRenderAsset asset renderer
            let mutable unused = Unchecked.defaultof<_>
            renderer.LazyTextureQueues.Remove (package.PackageState.TextureClient.LazyTextureQueue, &unused) |> ignore<bool>
            renderer.RenderPackages.Remove hintPackageName |> ignore
        | None -> ()

    static member private handleReloadRenderAssets renderer =
        GlRenderer3d.invalidateCaches renderer
        GlRenderer3d.clearRenderPasses renderer // invalidate render task keys that now contain potentially stale data
        GlRenderer3d.handleReloadShaders renderer
        for packageName in renderer.RenderPackages |> Seq.map (fun entry -> entry.Key) |> Array.ofSeq do
            GlRenderer3d.tryLoadRenderPackage packageName renderer

    static member private sortForwardSurfaces
        eyeCenter
        (surfaces : struct (single * single * Matrix4x4 * bool * Presence * Box2 * MaterialProperties * Matrix4x4 array voption * OpenGL.PhysicallyBased.PhysicallyBasedSurface * DepthTest) List)
        (forwardSurfacesComparer : IComparer<struct (single * single * Matrix4x4 * bool * Presence * Box2 * MaterialProperties * Matrix4x4 array voption * OpenGL.PhysicallyBased.PhysicallyBasedSurface * DepthTest * single * int)>)
        (forwardSurfacesSortBuffer : struct (single * single * Matrix4x4 * bool * Presence * Box2 * MaterialProperties * Matrix4x4 array voption * OpenGL.PhysicallyBased.PhysicallyBasedSurface * DepthTest * single * int) List) =
        for i in 0 .. dec surfaces.Count do
            let struct (subsort, sort, model, castShadow, presence, texCoordsOffset, properties, boneTransformsOpt, surface, depthTest) = surfaces.[i]
            forwardSurfacesSortBuffer.Add struct (subsort, sort, model, castShadow, presence, texCoordsOffset, properties, boneTransformsOpt, surface, depthTest, (model.Translation - eyeCenter).MagnitudeSquared, i)
        forwardSurfacesSortBuffer.Sort forwardSurfacesComparer
        forwardSurfacesSortBuffer

    static member private categorizeBillboardSurface
        (eyeCenter : Vector3,
         eyeRotation : Quaternion,
         model : Matrix4x4,
         castShadow : bool,
         presence : Presence,
         insetOpt : Box2 option,
         albedoMetadata : OpenGL.Texture.TextureMetadata,
         properties,
         orientUp,
         planar,
         shadowOffset,
         billboardSurface : OpenGL.PhysicallyBased.PhysicallyBasedSurface,
         depthTest,
         renderType,
         renderPass,
         renderTasks,
         _ : GlRenderer3d) =

        // compute tex coords offset
        let texCoordsOffset =
            match insetOpt with
            | Some inset ->
                let texelWidth = albedoMetadata.TextureTexelWidth
                let texelHeight = albedoMetadata.TextureTexelHeight
                let px = inset.Min.X * texelWidth
                let py = (inset.Min.Y + inset.Size.Y) * texelHeight
                let sx = inset.Size.X * texelWidth
                let sy = -inset.Size.Y * texelHeight
                Box2 (px, py, sx, sy)
            | None ->
                let px = 0.0f
                let py = 1.0f
                let sx = 1.0f
                let sy = -1.0f
                Box2 (px, py, sx, sy)

        // render as appropriate
        match renderPass with
        | ShadowPass (_, _, _, shadowRotation, _) ->

            // compute billboard rotation
            let lookRotation = shadowRotation * Quaternion.CreateFromAxisAngle (v3Right, -MathF.PI_OVER_2)
            let billboardRotation = Matrix4x4.CreateFromQuaternion lookRotation

            // add render task as appropriate
            let mutable affineRotation = model
            affineRotation.Translation <- v3Zero
            let mutable billboardMatrix = model * billboardRotation
            billboardMatrix.Translation <- model.Translation + lookRotation.Forward * shadowOffset
            match renderType with
            | DeferredRenderType ->
                if not billboardSurface.SurfaceMaterial.Clipped then
                    let mutable renderOps = Unchecked.defaultof<_> // OPTIMIZATION: TryGetValue using the auto-pairing syntax of F# allocation when the 'TValue is a struct tuple.
                    if renderTasks.DeferredStatic.TryGetValue (billboardSurface, &renderOps)
                    then renderOps.Add struct (billboardMatrix, castShadow, presence, texCoordsOffset, properties)
                    else renderTasks.DeferredStatic.Add (billboardSurface, List ([struct (billboardMatrix, castShadow, presence, texCoordsOffset, properties)]))
                else
                    let mutable renderOps = Unchecked.defaultof<_> // OPTIMIZATION: TryGetValue using the auto-pairing syntax of F# allocation when the 'TValue is a struct tuple.
                    if renderTasks.DeferredStaticClipped.TryGetValue (billboardSurface, &renderOps)
                    then renderOps.Add struct (billboardMatrix, castShadow, presence, texCoordsOffset, properties)
                    else renderTasks.DeferredStaticClipped.Add (billboardSurface, List ([struct (billboardMatrix, castShadow, presence, texCoordsOffset, properties)]))
            | ForwardRenderType (subsort, sort) ->
                renderTasks.Forward.Add struct (subsort, sort, billboardMatrix, castShadow, presence, texCoordsOffset, properties, ValueNone, billboardSurface, depthTest)

        | _ ->
            
            // compute billboard rotation based on orient up and planarness
            let billboardRotation =
                if orientUp then

                    // oriented up and planar, like a tree billboard
                    if planar then
                        let forwardFlat = eyeRotation.Forward.WithY 0.0f
                        if forwardFlat.MagnitudeSquared > 0.0f then
                            let forward = forwardFlat.Normalized
                            let yaw = MathF.Atan2 (forward.X, forward.Z) - MathF.PI
                            Matrix4x4.CreateRotationY yaw
                        else m4Identity

                    // oriented up and not planar, like a character billboard
                    else
                        let eyeFlat = eyeCenter.WithY 0.0f
                        let positionFlat = model.Translation.WithY 0.0f
                        let eyeToPositionFlat = positionFlat - eyeFlat
                        if eyeToPositionFlat.MagnitudeSquared > 0.0f then
                            let forward = eyeToPositionFlat.Normalized
                            let yaw = MathF.Atan2 (forward.X, forward.Z) - MathF.PI
                            Matrix4x4.CreateRotationY yaw
                        else m4Identity

                // not oriented up and planar, like a simple billboard
                elif planar then
                    Matrix4x4.CreateFromQuaternion eyeRotation 

                // not oriented up and not planar, like a sprite
                else
                    let lookat = Matrix4x4.CreateLookAt (eyeCenter, model.Translation, eyeRotation.Up)
                    lookat.Inverted
                    
            // add render task as appropriate
            let mutable affineRotation = model
            affineRotation.Translation <- v3Zero
            let mutable billboardMatrix = model * billboardRotation
            billboardMatrix.Translation <- model.Translation
            match renderType with
            | DeferredRenderType ->
                if not billboardSurface.SurfaceMaterial.Clipped then
                    let mutable renderOps = Unchecked.defaultof<_> // OPTIMIZATION: TryGetValue using the auto-pairing syntax of F# allocation when the 'TValue is a struct tuple.
                    if renderTasks.DeferredStatic.TryGetValue (billboardSurface, &renderOps)
                    then renderOps.Add struct (billboardMatrix, castShadow, presence, texCoordsOffset, properties)
                    else renderTasks.DeferredStatic.Add (billboardSurface, List ([struct (billboardMatrix, castShadow, presence, texCoordsOffset, properties)]))
                else
                    let mutable renderOps = Unchecked.defaultof<_> // OPTIMIZATION: TryGetValue using the auto-pairing syntax of F# allocation when the 'TValue is a struct tuple.
                    if renderTasks.DeferredStaticClipped.TryGetValue (billboardSurface, &renderOps)
                    then renderOps.Add struct (billboardMatrix, castShadow, presence, texCoordsOffset, properties)
                    else renderTasks.DeferredStaticClipped.Add (billboardSurface, List ([struct (billboardMatrix, castShadow, presence, texCoordsOffset, properties)]))
            | ForwardRenderType (subsort, sort) ->
                renderTasks.Forward.Add struct (subsort, sort, billboardMatrix, castShadow, presence, texCoordsOffset, properties, ValueNone, billboardSurface, depthTest)

    static member private categorizeStaticModelSurface
        (model : Matrix4x4 inref,
         castShadow : bool,
         presence : Presence,
         insetOpt : Box2 voption inref,
         properties : MaterialProperties inref,
         surface : OpenGL.PhysicallyBased.PhysicallyBasedSurface,
         depthTest : DepthTest,
         renderType : RenderType,
         renderPass : RenderPass,
         renderTasksOpt : RenderTasks voption,
         renderer) =

        // compute tex coords offset
        let texCoordsOffset =
            match insetOpt with
            | ValueSome inset ->
                let albedoMetadata = surface.SurfaceMaterial.AlbedoTexture.TextureMetadata
                let texelWidth = albedoMetadata.TextureTexelWidth
                let texelHeight = albedoMetadata.TextureTexelHeight
                let px = inset.Min.X * texelWidth
                let py = (inset.Min.Y + inset.Size.Y) * texelHeight
                let sx = inset.Size.X * texelWidth
                let sy = -inset.Size.Y * texelHeight
                Box2 (px, py, sx, sy)
            | ValueNone -> box2 v2Zero v2Zero

        // decide destination of render tasks
        let renderTasks =
            match renderTasksOpt with
            | ValueSome renderTasks -> renderTasks
            | ValueNone -> GlRenderer3d.getRenderTasks renderPass renderer
        
        // render as appropriate
        match renderType with
        | DeferredRenderType ->
            if not surface.SurfaceMaterial.Clipped then
                let mutable renderOps = Unchecked.defaultof<_> // OPTIMIZATION: TryGetValue using the auto-pairing syntax of F# allocation when the 'TValue is a struct tuple.
                if renderTasks.DeferredStatic.TryGetValue (surface, &renderOps)
                then renderOps.Add struct (model, castShadow, presence, texCoordsOffset, properties)
                else renderTasks.DeferredStatic.Add (surface, List ([struct (model, castShadow, presence, texCoordsOffset, properties)]))
            else
                let mutable renderOps = Unchecked.defaultof<_> // OPTIMIZATION: TryGetValue using the auto-pairing syntax of F# allocation when the 'TValue is a struct tuple.
                if renderTasks.DeferredStaticClipped.TryGetValue (surface, &renderOps)
                then renderOps.Add struct (model, castShadow, presence, texCoordsOffset, properties)
                else renderTasks.DeferredStaticClipped.Add (surface, List ([struct (model, castShadow, presence, texCoordsOffset, properties)]))
        | ForwardRenderType (subsort, sort) ->
            renderTasks.Forward.Add struct (subsort, sort, model, castShadow, presence, texCoordsOffset, properties, ValueNone, surface, depthTest)

    static member private categorizeStaticModelSurfaceByIndex
        (model : Matrix4x4 inref,
         castShadow : bool,
         presence : Presence,
         insetOpt : Box2 voption inref,
         properties : MaterialProperties inref,
         material : Material inref,
         staticModel : StaticModel AssetTag,
         surfaceIndex : int,
         depthTest : DepthTest,
         renderType : RenderType,
         renderPass : RenderPass,
         renderer) =
        match GlRenderer3d.tryGetRenderAsset staticModel renderer with
        | ValueSome renderAsset ->
            match renderAsset with
            | StaticModelAsset (_, modelAsset) ->
                if surfaceIndex > -1 && surfaceIndex < modelAsset.Surfaces.Length then
                    let surface = modelAsset.Surfaces.[surfaceIndex]
                    let surface = // OPTIMIZATION: apply surface material only if effective.
                        if material <> Material.empty then
                            let surfaceMaterial = GlRenderer3d.applySurfaceMaterial (&material, &surface.SurfaceMaterial, renderer)
                            { surface with SurfaceMaterial = surfaceMaterial }
                        else surface
                    GlRenderer3d.categorizeStaticModelSurface (&model, castShadow, presence, &insetOpt, &properties, surface, depthTest, renderType, renderPass, ValueNone, renderer)
            | _ -> Log.infoOnce ("Cannot render static model surface with a non-static model asset for '" + scstring staticModel + "'.")
        | ValueNone -> Log.infoOnce ("Cannot render static model surface due to unloadable asset(s) for '" + scstring staticModel + "'.")

    static member private categorizeStaticModelSurfacePreBatch
        (preBatchId : Guid,
         staticModelSurfaces : _ array,
         material : Material,
         staticModel : StaticModel AssetTag,
         surfaceIndex : int,
         depthTest : DepthTest,
         renderType : RenderType,
         frustumInterior : Frustum,
         frustumExterior : Frustum,
         frustumImposter : Frustum,
         lightBox : Box3,
         renderPass : RenderPass,
         renderTasks : RenderTasks,
         renderer) =
        match GlRenderer3d.tryGetRenderAsset staticModel renderer with
        | ValueSome renderAsset ->
            match renderAsset with
            | StaticModelAsset (_, modelAsset) ->
                if surfaceIndex > -1 && surfaceIndex < modelAsset.Surfaces.Length then
                    let surface = modelAsset.Surfaces.[surfaceIndex]
                    let surface = // OPTIMIZATION: apply surface material only if effective.
                        if material <> Material.empty then
                            let surfaceMaterial = GlRenderer3d.applySurfaceMaterial (&material, &surface.SurfaceMaterial, renderer)
                            { surface with SurfaceMaterial = surfaceMaterial }
                        else surface
                    match renderType with
                    | DeferredRenderType ->
                        let preBatch = struct (surface, staticModelSurfaces)
                        if not surface.SurfaceMaterial.Clipped
                        then renderTasks.DeferredStaticPreBatches.Add (preBatchId, preBatch)
                        else renderTasks.DeferredStaticClippedPreBatches.Add (preBatchId, preBatch)
                    | ForwardRenderType (subsort, sort) ->
                        for (model, castShadow, presence, insetOpt, properties, bounds) in staticModelSurfaces do
                            let unculled =
                                match renderPass with
                                | LightMapPass (_, _) -> true // TODO: see if we have enough context to cull here.
                                | ShadowPass (_, _, shadowLightType, _, shadowFrustum) ->
                                    if castShadow then // TODO: see if we should check for CastShadow when constructing the pre-batch.
                                        let shadowFrustumInteriorOpt = if LightType.shouldShadowInterior shadowLightType then ValueSome shadowFrustum else ValueNone
                                        Presence.intersects3d shadowFrustumInteriorOpt shadowFrustum shadowFrustum ValueNone false false presence bounds
                                    else false
                                | ReflectionPass (_, reflFrustum) -> Presence.intersects3d ValueNone reflFrustum reflFrustum ValueNone false false presence bounds
                                | NormalPass -> Presence.intersects3d (ValueSome frustumInterior) frustumExterior frustumImposter (ValueSome lightBox) false false presence bounds
                            if unculled then
                                renderTasks.Forward.Add struct (subsort, sort, model, castShadow, presence, insetOpt, properties, ValueNone, surface, depthTest)
            | _ -> ()
        | ValueNone -> ()

    static member private categorizeStaticModel
        (frustumInterior : Frustum,
         frustumExterior : Frustum,
         frustumImposter : Frustum,
         lightBox : Box3,
         model : Matrix4x4 inref,
         castShadow : bool,
         presence : Presence,
         insetOpt : Box2 voption inref,
         properties : MaterialProperties inref,
         staticModel : StaticModel AssetTag,
         clipped : bool,
         depthTest : DepthTest,
         renderType : RenderType,
         renderPass : RenderPass,
         renderTasks : RenderTasks,
         renderer) =
        let renderStyle = match renderType with DeferredRenderType -> Deferred | ForwardRenderType (subsort, sort) -> Forward (subsort, sort)
        match GlRenderer3d.tryGetRenderAsset staticModel renderer with
        | ValueSome renderAsset ->
            match renderAsset with
            | StaticModelAsset (_, modelAsset) ->
                for light in modelAsset.Lights do
                    let lightMatrix = light.LightMatrix * model
                    let lightBounds = Box3 (lightMatrix.Translation - v3Dup light.LightCutoff, v3Dup light.LightCutoff * 2.0f)
                    let direction = lightMatrix.Rotation.Down
                    let unculled =
                        match renderPass with
                        | LightMapPass (_, _) -> true // TODO: see if we have enough context to cull here.
                        | NormalPass -> Presence.intersects3d (ValueSome frustumInterior) frustumExterior frustumImposter (ValueSome lightBox) false true presence lightBounds
                        | _ -> false
                    if unculled then
                        let coneOuter = match light.LightType with SpotLight (_, coneOuter) -> min coneOuter MathF.PI_MINUS_EPSILON | _ -> MathF.TWO_PI
                        let coneInner = match light.LightType with SpotLight (coneInner, _) -> min coneInner coneOuter | _ -> MathF.TWO_PI
                        let light =
                            { SortableLightId = 0UL
                              SortableLightOrigin = lightMatrix.Translation
                              SortableLightRotation = lightMatrix.Rotation
                              SortableLightDirection = direction
                              SortableLightColor = light.LightColor
                              SortableLightBrightness = light.LightBrightness
                              SortableLightAttenuationLinear = light.LightAttenuationLinear
                              SortableLightAttenuationQuadratic = light.LightAttenuationQuadratic
                              SortableLightCutoff = light.LightCutoff
                              SortableLightType = light.LightType.Enumerate
                              SortableLightConeInner = coneInner
                              SortableLightConeOuter = coneOuter
                              SortableLightDesireShadows = 0
                              SortableLightDesireFog = 0
                              SortableLightBounds = lightBounds
                              SortableLightDistance = Single.MaxValue }
                        renderTasks.Lights.Add light
                for surface in modelAsset.Surfaces do
                    let surface = // OPTIMIZATION: apply surface material only if effective.
                        if clipped
                        then { surface with SurfaceMaterial = { surface.SurfaceMaterial with Clipped = clipped }}
                        else surface
                    let surfaceMatrix = if surface.SurfaceMatrixIsIdentity then model else surface.SurfaceMatrix * model
                    let surfaceBounds = surface.SurfaceBounds.Transform surfaceMatrix
                    let presence = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractPresence presence modelAsset.SceneOpt surface
                    let renderStyle = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractRenderStyle renderStyle modelAsset.SceneOpt surface
                    let renderType = match renderStyle with Deferred -> DeferredRenderType | Forward (subsort, sort) -> ForwardRenderType (subsort, sort)
                    let ignoreLightMaps = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractIgnoreLightMaps properties.IgnoreLightMaps modelAsset.SceneOpt surface
                    let properties = if ignoreLightMaps <> properties.IgnoreLightMaps then { properties with IgnoreLightMapsOpt = ValueSome ignoreLightMaps } else properties
                    let finenessOffset = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractFinenessOffset properties.FinenessOffset modelAsset.SceneOpt surface
                    let properties = if finenessOffset <> properties.FinenessOffset then { properties with FinenessOffsetOpt = ValueSome finenessOffset } else properties
                    let scatterType = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractScatterType properties.ScatterType modelAsset.SceneOpt surface
                    let properties = if scatterType <> properties.ScatterType then { properties with ScatterTypeOpt = ValueSome scatterType } else properties
                    let unculled =
                        match renderPass with
                        | LightMapPass (_, _) -> true // TODO: see if we have enough context to cull here.
                        | ShadowPass (_, _, shadowLightType, _, shadowFrustum) ->
                            let shadowFrustumInteriorOpt = if LightType.shouldShadowInterior shadowLightType then ValueSome shadowFrustum else ValueNone
                            Presence.intersects3d shadowFrustumInteriorOpt shadowFrustum shadowFrustum ValueNone false false presence surfaceBounds
                        | ReflectionPass (_, reflFrustum) -> Presence.intersects3d ValueNone reflFrustum reflFrustum ValueNone false false presence surfaceBounds
                        | NormalPass -> Presence.intersects3d (ValueSome frustumInterior) frustumExterior frustumImposter (ValueSome lightBox) false false presence surfaceBounds
                    if unculled then
                        GlRenderer3d.categorizeStaticModelSurface (&surfaceMatrix, castShadow, presence, &insetOpt, &properties, surface, depthTest, renderType, renderPass, ValueSome renderTasks, renderer)
            | _ -> Log.infoOnce ("Cannot render static model with a non-static model asset for '" + scstring staticModel + "'.")
        | ValueNone -> Log.infoOnce ("Cannot render static model due to unloadable asset(s) for '" + scstring staticModel + "'.")

    static member private categorizeAnimatedModel
        (model : Matrix4x4 inref,
         castShadow : bool,
         presence : Presence,
         insetOpt : Box2 voption inref,
         properties : MaterialProperties inref,
         boneTransforms : Matrix4x4 array,
         animatedModel : AnimatedModel AssetTag,
         subsortOffsets : Map<int, single>,
         drsIndices : int Set,
         depthTest : DepthTest,
         renderType : RenderType,
         renderTasks : RenderTasks,
         renderer) =

        // ensure we have the required animated model
        match GlRenderer3d.tryGetRenderAsset animatedModel renderer with
        | ValueSome renderAsset ->
            match renderAsset with
            | AnimatedModelAsset modelAsset ->

                // render animated surfaces
                for i in 0 .. dec modelAsset.Surfaces.Length do

                    // compute tex coords offset
                    let surface = modelAsset.Surfaces.[i]
                    let texCoordsOffset =
                        match insetOpt with
                        | ValueSome inset ->
                            let albedoMetadata = surface.SurfaceMaterial.AlbedoTexture.TextureMetadata
                            let texelWidth = albedoMetadata.TextureTexelWidth
                            let texelHeight = albedoMetadata.TextureTexelHeight
                            let px = inset.Min.X * texelWidth
                            let py = (inset.Min.Y + inset.Size.Y) * texelHeight
                            let sx = inset.Size.X * texelWidth
                            let sy = -inset.Size.Y * texelHeight
                            Box2 (px, py, sx, sy)
                        | ValueNone -> box2 v2Zero v2Zero

                    // check if dual rendering needed
                    let dualRendering = drsIndices.Contains i

                    // deferred render animated surface when needed
                    if renderType = DeferredRenderType || dualRendering then
                        let animatedModelSurfaceKey = { BoneTransforms = boneTransforms; AnimatedSurface = surface }
                        match renderTasks.DeferredAnimated.TryGetValue animatedModelSurfaceKey with
                        | (true, renderOps) -> renderOps.Add struct (model, castShadow, presence, texCoordsOffset, properties)
                        | (false, _) -> renderTasks.DeferredAnimated.Add (animatedModelSurfaceKey, List ([struct (model, castShadow, presence, texCoordsOffset, properties)]))

                    // forward render animated surface when needed
                    let subsortOffset =
                        match subsortOffsets.TryGetValue i with
                        | (true, subsortOffset) -> subsortOffset
                        | (false, _) -> 0.0f
                    let sortsOpt =
                        match renderType with
                        | ForwardRenderType (subsort, sort) -> ValueSome struct (subsort + subsortOffset, sort)
                        | _ -> if dualRendering then ValueSome struct (subsortOffset, 0.0f) else ValueNone
                    match sortsOpt with
                    | ValueSome struct (subsort, sort) ->
                        renderTasks.Forward.Add struct (subsort, sort, model, castShadow, presence, texCoordsOffset, properties, ValueSome boneTransforms, surface, depthTest)
                    | ValueNone -> ()

            // unable to render
            | _ -> Log.infoOnce ("Cannot render animated model with a non-animated model asset '" + scstring animatedModel + "'.")
        | ValueNone -> Log.infoOnce ("Cannot render animated model due to unloadable asset(s) for '" + scstring animatedModel + "'.")

    static member private categorizeAnimatedModels
        (animatedModels : (Matrix4x4 * bool * Presence * Box2 option * MaterialProperties) SList,
         boneTransforms : Matrix4x4 array,
         animatedModel : AnimatedModel AssetTag,
         subsortOffsets : Map<int, single>,
         drsIndices : int Set,
         depthTest : DepthTest,
         renderType : RenderType,
         renderTasks : RenderTasks,
         renderer) =

        // ensure we have the required animated model
        match GlRenderer3d.tryGetRenderAsset animatedModel renderer with
        | ValueSome renderAsset ->
            match renderAsset with
            | AnimatedModelAsset modelAsset ->

                // render animated surfaces
                for i in 0 .. dec modelAsset.Surfaces.Length do

                    // render animated surfaces
                    let surface = modelAsset.Surfaces.[i]
                    for (model, castShadow, presence, insetOpt, properties) in animatedModels do // TODO: see if these should a struct tuples.

                        // compute tex coords offset
                        let texCoordsOffset =
                            match insetOpt with
                            | Some inset ->
                                let albedoMetadata = surface.SurfaceMaterial.AlbedoTexture.TextureMetadata
                                let texelWidth = albedoMetadata.TextureTexelWidth
                                let texelHeight = albedoMetadata.TextureTexelHeight
                                let px = inset.Min.X * texelWidth
                                let py = (inset.Min.Y + inset.Size.Y) * texelHeight
                                let sx = inset.Size.X * texelWidth
                                let sy = -inset.Size.Y * texelHeight
                                Box2 (px, py, sx, sy)
                            | None -> box2 v2Zero v2Zero

                        // deferred render animated surface when needed
                        if renderType = DeferredRenderType then
                            let animatedModelSurfaceKey = { BoneTransforms = boneTransforms; AnimatedSurface = surface }
                            match renderTasks.DeferredAnimated.TryGetValue animatedModelSurfaceKey with
                            | (true, renderOps) -> renderOps.Add struct (model, castShadow, presence, texCoordsOffset, properties)
                            | (false, _) -> renderTasks.DeferredAnimated.Add (animatedModelSurfaceKey, List ([struct (model, castShadow, presence, texCoordsOffset, properties)]))

                        // forward render animated surface when needed
                        let subsortOffset =
                            match subsortOffsets.TryGetValue i with
                            | (true, subsortOffset) -> subsortOffset
                            | (false, _) -> 0.0f
                        let sortsOpt =
                            match renderType with
                            | ForwardRenderType (subsort, sort) -> ValueSome struct (subsort + subsortOffset, sort)
                            | _ -> if drsIndices.Contains i then ValueSome struct (subsortOffset, 0.0f) else ValueNone
                        match sortsOpt with
                        | ValueSome struct (subsort, sort) ->
                            renderTasks.Forward.Add struct (subsort, sort, model, castShadow, presence, texCoordsOffset, properties, ValueSome boneTransforms, surface, depthTest)
                        | ValueNone -> ()

            // unable to render
            | _ -> Log.infoOnce ("Cannot render animated model with a non-animated model asset '" + scstring animatedModel + "'.")
        | ValueNone -> Log.infoOnce ("Cannot render animated model due to unloadable asset(s) for '" + scstring animatedModel + "'.")

    static member private categorizeTerrain
        (visible : bool,
         terrainDescriptor : TerrainDescriptor,
         renderTasks : RenderTasks,
         renderer) =

        // attempt to create terrain geometry when not already cached
        let geometryDescriptor = terrainDescriptor.TerrainGeometryDescriptor
        if not (renderer.PhysicallyBasedTerrainGeometries.ContainsKey geometryDescriptor) then

            // attempt to get height map metadata to compute patches
            let heightMapMetadataOpt =
                HeightMap.tryGetMetadata
                    (fun assetTag -> GlRenderer3d.tryGetFilePath assetTag renderer)
                    terrainDescriptor.Bounds
                    terrainDescriptor.Tiles
                    terrainDescriptor.HeightMap

            // attempt to extract height map metadata for geometry creation
            match heightMapMetadataOpt with
            | ValueSome heightMapMetadata ->

                // attempt to create geometry data for the entire terrain
                match GlRenderer3d.tryCreatePhysicallyBasedTerrainGeometryData geometryDescriptor heightMapMetadata renderer with
                | Some (positionsAndTexCoordses, normals, blendses, tint) ->

                    // attempt to compute patches for the terrain
                    match GlRenderer3d.tryComputeTerrainPatches geometryDescriptor heightMapMetadata with
                    | Some patches ->

                        // create geometry for each patch
                        for patchDescriptor in patches do

                            // extract positions and texture coordinates for this patch
                            let patchResolution = patchDescriptor.PatchResolution
                            let patchOffset = patchDescriptor.PatchOffset
                            let patchPositionsAndTexCoordses =
                                [|for y in 0 .. dec patchResolution.Y do
                                    for x in 0 .. dec patchResolution.X do
                                        let i = (patchOffset.Y + y) * heightMapMetadata.Resolution.X + (patchOffset.X + x)
                                        positionsAndTexCoordses.[i]|]

                            // extract normals
                            let patchNormals =
                                [|for y in 0 .. dec patchResolution.Y do
                                    for x in 0 .. dec patchResolution.X do
                                        let i = (patchOffset.Y + y) * heightMapMetadata.Resolution.X + (patchOffset.X + x)
                                        normals.[i]|]

                            // extract blendses
                            let patchBlendses =
                                [|for y in 0 .. dec patchResolution.Y do
                                    for x in 0 .. dec patchResolution.X do
                                        let i = (patchOffset.Y + y) * heightMapMetadata.Resolution.X + (patchOffset.X + x)
                                        [|for j in 0 .. dec 8 do blendses.[i, j]|]|]

                            // extract tint
                            let patchTint =
                                [|for y in 0 .. dec patchResolution.Y do
                                    for x in 0 .. dec patchResolution.X do
                                        let i = (patchOffset.Y + y) * heightMapMetadata.Resolution.X + (patchOffset.X + x)
                                        tint.[i]|]

                            // compute vertices
                            let vertices =
                                [|for i in 0 .. dec patchPositionsAndTexCoordses.Length do
                                    let struct (p, tc) = patchPositionsAndTexCoordses.[i]
                                    let n = patchNormals.[i]
                                    let s = patchBlendses
                                    let t = patchTint.[i]
                                    yield!
                                        [|p.X; p.Y; p.Z
                                          tc.X; tc.Y
                                          n.X; n.Y; n.Z
                                          t.X; t.Y; t.Z
                                          s.[i].[0]; s.[i].[1]; s.[i].[2]; s.[i].[3]; s.[i].[4]; s.[i].[5]; s.[i].[6]; s.[i].[7]|]|]

                            // compute indices, splitting quad along the standard orientation (as used by World Creator, AFAIK).
                            let indices =
                                [|for y in 0 .. dec patchResolution.Y - 1 do
                                    for x in 0 .. dec patchResolution.X - 1 do
                                        yield patchResolution.X * y + x
                                        yield patchResolution.X * inc y + x
                                        yield patchResolution.X * y + inc x
                                        yield patchResolution.X * inc y + x
                                        yield patchResolution.X * inc y + inc x
                                        yield patchResolution.X * y + inc x|]

                            // create the actual geometry
                            let geometry = OpenGL.PhysicallyBased.CreatePhysicallyBasedTerrainGeometry (true, OpenGL.PrimitiveType.Triangles, vertices.AsMemory (), indices.AsMemory (), geometryDescriptor.Bounds)
                            match renderer.PhysicallyBasedTerrainGeometries.TryGetValue geometryDescriptor with
                            | (true, existingGeometry) -> existingGeometry.Add (patchDescriptor, geometry)
                            | (false, _) -> renderer.PhysicallyBasedTerrainGeometries.Add (geometryDescriptor, dictPlus HashIdentity.Structural [(patchDescriptor, geometry)])

                    // could not create geometry
                    | None -> ()

                // could not compute patches
                | None -> ()

            // could not extract height map metadata
            | ValueNone -> ()

        // attempt to add patch to appropriate render list when visible
        if visible then
            match renderer.PhysicallyBasedTerrainGeometries.TryGetValue geometryDescriptor with
            | (true, patchGeometries) ->
                for entry in patchGeometries do
                    let patchDescriptor = entry.Key
                    let patchGeometry = entry.Value
                    renderTasks.DeferredTerrains.Add struct (terrainDescriptor, patchDescriptor, patchGeometry)
            | (false, _) -> ()

        // mark patch geometry as utilized regardless of visibility (to keep it from being destroyed)
        renderer.PhysicallyBasedTerrainGeometriesUtilized.Add geometryDescriptor |> ignore<bool>

    static member private categorize
        frustumInterior
        frustumExterior
        frustumImposter
        lightBox
        eyeCenter
        eyeRotation
        renderMessages
        renderer =
        let userDefinedStaticModelsToDestroy = SList.make ()
        for message in renderMessages do
            match message with
            | CreateUserDefinedStaticModel cudsm ->
                GlRenderer3d.tryCreateUserDefinedStaticModel cudsm.StaticModelSurfaceDescriptors cudsm.Bounds cudsm.StaticModel renderer
            | DestroyUserDefinedStaticModel dudsm ->
                userDefinedStaticModelsToDestroy.Add dudsm.StaticModel 
            | RenderSkyBox rsb ->
                let renderTasks = GlRenderer3d.getRenderTasks rsb.RenderPass renderer
                renderTasks.SkyBoxes.Add (rsb.AmbientColor, rsb.AmbientBrightness, rsb.CubeMapColor, rsb.CubeMapBrightness, rsb.CubeMap)
            | RenderLightProbe3d rlp ->
                let renderTasks = GlRenderer3d.getRenderTasks rlp.RenderPass renderer
                if renderTasks.LightProbes.ContainsKey rlp.LightProbeId then
                    Log.warnOnce ("Multiple light probe messages coming in with the same id of '" + string rlp.LightProbeId + "'.")
                    renderTasks.LightProbes.Remove rlp.LightProbeId |> ignore<bool>
                renderTasks.LightProbes.Add (rlp.LightProbeId, struct (rlp.Enabled, rlp.Origin, rlp.AmbientColor, rlp.AmbientBrightness, rlp.Bounds))
            | RenderLightMap3d rlm ->
                let renderTasks = GlRenderer3d.getRenderTasks rlm.RenderPass renderer
                renderTasks.LightMapRenders.Add rlm.LightProbeId |> ignore<bool>
            | RenderLight3d rl ->
                let direction = rl.Rotation.Down
                let renderTasks = GlRenderer3d.getRenderTasks rl.RenderPass renderer
                let coneOuter = match rl.LightType with SpotLight (_, coneOuter) -> min coneOuter MathF.PI_MINUS_EPSILON | _ -> MathF.TWO_PI
                let coneInner = match rl.LightType with SpotLight (coneInner, _) -> min coneInner coneOuter | _ -> MathF.TWO_PI
                let light =
                    { SortableLightId = rl.LightId
                      SortableLightOrigin = rl.Origin
                      SortableLightRotation = rl.Rotation
                      SortableLightDirection = direction
                      SortableLightColor = rl.Color
                      SortableLightBrightness = rl.Brightness
                      SortableLightAttenuationLinear = rl.AttenuationLinear
                      SortableLightAttenuationQuadratic = rl.AttenuationQuadratic
                      SortableLightCutoff = rl.LightCutoff
                      SortableLightType = rl.LightType.Enumerate
                      SortableLightConeInner = coneInner
                      SortableLightConeOuter = coneOuter
                      SortableLightDesireShadows = if rl.DesireShadows then 1 else 0
                      SortableLightDesireFog = if rl.DesireFog then 1 else 0
                      SortableLightBounds = rl.Bounds
                      SortableLightDistance = Single.MaxValue }
                renderTasks.Lights.Add light
                if rl.DesireShadows then
                    renderer.LightsDesiringShadows.[rl.LightId] <- light
            | RenderBillboard rb ->
                let struct (billboardProperties, billboardMaterial) = GlRenderer3d.makeBillboardMaterial (&rb.MaterialProperties, &rb.Material, renderer)
                let billboardSurface = OpenGL.PhysicallyBased.CreatePhysicallyBasedSurface (Array.empty, m4Identity, box3 (v3 -0.5f 0.5f -0.5f) v3One, billboardProperties, billboardMaterial, -1, Assimp.Node.Empty, renderer.BillboardGeometry)
                let renderTasks = GlRenderer3d.getRenderTasks rb.RenderPass renderer
                GlRenderer3d.categorizeBillboardSurface (eyeCenter, eyeRotation, rb.ModelMatrix, rb.CastShadow, rb.Presence, rb.InsetOpt, billboardMaterial.AlbedoTexture.TextureMetadata, rb.MaterialProperties, rb.OrientUp, rb.Planar, rb.ShadowOffset, billboardSurface, rb.DepthTest, rb.RenderType, rb.RenderPass, renderTasks, renderer)
            | RenderBillboards rbs ->
                let struct (billboardProperties, billboardMaterial) = GlRenderer3d.makeBillboardMaterial (&rbs.MaterialProperties, &rbs.Material, renderer)
                let billboardSurface = OpenGL.PhysicallyBased.CreatePhysicallyBasedSurface (Array.empty, m4Identity, box3 (v3 -0.5f -0.5f -0.5f) v3One, billboardProperties, billboardMaterial, -1, Assimp.Node.Empty, renderer.BillboardGeometry)
                let renderTasks = GlRenderer3d.getRenderTasks rbs.RenderPass renderer
                for (model, castShadow, presence, insetOpt, orientUp, planar) in rbs.Billboards do
                    GlRenderer3d.categorizeBillboardSurface (eyeCenter, eyeRotation, model, castShadow, presence, insetOpt, billboardMaterial.AlbedoTexture.TextureMetadata, rbs.MaterialProperties, orientUp, planar, rbs.ShadowOffset, billboardSurface, rbs.DepthTest, rbs.RenderType, rbs.RenderPass, renderTasks, renderer)
            | RenderBillboardParticles rbps ->
                let struct (billboardProperties, billboardMaterial) = GlRenderer3d.makeBillboardMaterial (&rbps.MaterialProperties, &rbps.Material, renderer)
                let renderTasks = GlRenderer3d.getRenderTasks rbps.RenderPass renderer
                for particle in rbps.Particles do
                    let billboardMatrix =
                        Matrix4x4.CreateAffine
                            (particle.Transform.Position,
                             particle.Transform.Rotation,
                             particle.Transform.Size * particle.Transform.Scale)
                    let billboardProperties = { billboardProperties with Albedo = billboardProperties.Albedo * particle.Color; Emission = particle.Emission.R }
                    let billboardSurface = OpenGL.PhysicallyBased.CreatePhysicallyBasedSurface (Array.empty, m4Identity, box3Zero, billboardProperties, billboardMaterial, -1, Assimp.Node.Empty, renderer.BillboardGeometry)
                    GlRenderer3d.categorizeBillboardSurface (eyeCenter, eyeRotation, billboardMatrix, rbps.CastShadow, rbps.Presence, Option.ofValueOption particle.InsetOpt, billboardMaterial.AlbedoTexture.TextureMetadata, rbps.MaterialProperties, true, false, rbps.ShadowOffset, billboardSurface, rbps.DepthTest, rbps.RenderType, rbps.RenderPass, renderTasks, renderer)
            | RenderStaticModelSurface rsms ->
                let insetOpt = Option.toValueOption rsms.InsetOpt
                GlRenderer3d.categorizeStaticModelSurfaceByIndex (&rsms.ModelMatrix, rsms.CastShadow, rsms.Presence, &insetOpt, &rsms.MaterialProperties, &rsms.Material, rsms.StaticModel, rsms.SurfaceIndex, rsms.DepthTest, rsms.RenderType, rsms.RenderPass, renderer)
            | RenderStaticModelSurfacePreBatch rsmsb ->
                let renderPass = rsmsb.RenderPass
                let renderTasks = GlRenderer3d.getRenderTasks renderPass renderer
                GlRenderer3d.categorizeStaticModelSurfacePreBatch (rsmsb.StaticModelSurfacePreBatch.PreBatchId, rsmsb.StaticModelSurfacePreBatch.StaticModelSurfaces, rsmsb.StaticModelSurfacePreBatch.Material, rsmsb.StaticModelSurfacePreBatch.StaticModel, rsmsb.StaticModelSurfacePreBatch.SurfaceIndex, rsmsb.StaticModelSurfacePreBatch.DepthTest, rsmsb.StaticModelSurfacePreBatch.RenderType, frustumInterior, frustumExterior, frustumImposter, lightBox, renderPass, renderTasks, renderer)
            | RenderStaticModelSurfacePreBatches rsmsbs ->
                let renderPass = rsmsbs.RenderPass
                let renderTasks = GlRenderer3d.getRenderTasks renderPass renderer
                for preBatch in rsmsbs.StaticModelSurfacePreBatches do
                    GlRenderer3d.categorizeStaticModelSurfacePreBatch (preBatch.PreBatchId, preBatch.StaticModelSurfaces, preBatch.Material, preBatch.StaticModel, preBatch.SurfaceIndex, preBatch.DepthTest, preBatch.RenderType, frustumInterior, frustumExterior, frustumImposter, lightBox, renderPass, renderTasks, renderer)
            | RenderStaticModel rsm ->
                let insetOpt = Option.toValueOption rsm.InsetOpt
                let renderTasks = GlRenderer3d.getRenderTasks rsm.RenderPass renderer
                GlRenderer3d.categorizeStaticModel (frustumInterior, frustumExterior, frustumImposter, lightBox, &rsm.ModelMatrix, rsm.CastShadow, rsm.Presence, &insetOpt, &rsm.MaterialProperties, rsm.StaticModel, rsm.Clipped, rsm.DepthTest, rsm.RenderType, rsm.RenderPass, renderTasks, renderer)
            | RenderStaticModels rsms ->
                let renderTasks = GlRenderer3d.getRenderTasks rsms.RenderPass renderer
                for (model, castShadow, presence, insetOpt, properties) in rsms.StaticModels do // TODO: see if these should be struct tuples.
                    let insetOpt = Option.toValueOption insetOpt
                    GlRenderer3d.categorizeStaticModel (frustumInterior, frustumExterior, frustumImposter, lightBox, &model, castShadow, presence, &insetOpt, &properties, rsms.StaticModel, rsms.Clipped, rsms.DepthTest, rsms.RenderType, rsms.RenderPass, renderTasks, renderer)
            | RenderCachedStaticModel csmm ->
                let renderTasks = GlRenderer3d.getRenderTasks csmm.CachedStaticModelRenderPass renderer
                GlRenderer3d.categorizeStaticModel (frustumInterior, frustumExterior, frustumImposter, lightBox, &csmm.CachedStaticModelMatrix, csmm.CachedStaticModelCastShadow, csmm.CachedStaticModelPresence, &csmm.CachedStaticModelInsetOpt, &csmm.CachedStaticModelMaterialProperties, csmm.CachedStaticModel, csmm.CachedStaticModelClipped, csmm.CachedStaticModelDepthTest, csmm.CachedStaticModelRenderType, csmm.CachedStaticModelRenderPass, renderTasks, renderer)
            | RenderCachedStaticModelSurface csmsm ->
                GlRenderer3d.categorizeStaticModelSurfaceByIndex (&csmsm.CachedStaticModelSurfaceMatrix, csmsm.CachedStaticModelSurfaceCastShadow, csmsm.CachedStaticModelSurfacePresence, &csmsm.CachedStaticModelSurfaceInsetOpt, &csmsm.CachedStaticModelSurfaceMaterialProperties, &csmsm.CachedStaticModelSurfaceMaterial, csmsm.CachedStaticModelSurfaceModel, csmsm.CachedStaticModelSurfaceIndex, csmsm.CachedStaticModelSurfaceDepthTest, csmsm.CachedStaticModelSurfaceRenderType, csmsm.CachedStaticModelSurfaceRenderPass, renderer)
            | RenderUserDefinedStaticModel rudsm ->
                let insetOpt = Option.toValueOption rudsm.InsetOpt
                let assetTag = asset Assets.Default.PackageName Gen.name // TODO: see if we should instead use a specialized package for temporary assets like these.
                GlRenderer3d.tryCreateUserDefinedStaticModel rudsm.StaticModelSurfaceDescriptors rudsm.Bounds assetTag renderer
                let renderTasks = GlRenderer3d.getRenderTasks rudsm.RenderPass renderer
                GlRenderer3d.categorizeStaticModel (frustumInterior, frustumExterior, frustumImposter, lightBox, &rudsm.ModelMatrix, rudsm.CastShadow, rudsm.Presence, &insetOpt, &rudsm.MaterialProperties, assetTag, rudsm.Clipped, rudsm.DepthTest, rudsm.RenderType, rudsm.RenderPass, renderTasks, renderer)
                userDefinedStaticModelsToDestroy.Add assetTag
            | RenderAnimatedModel rsm ->
                let insetOpt = Option.toValueOption rsm.InsetOpt
                let renderTasks = GlRenderer3d.getRenderTasks rsm.RenderPass renderer
                GlRenderer3d.categorizeAnimatedModel (&rsm.ModelMatrix, rsm.CastShadow, rsm.Presence, &insetOpt, &rsm.MaterialProperties, rsm.BoneTransforms, rsm.AnimatedModel, rsm.SubsortOffsets, rsm.DualRenderedSurfaceIndices, rsm.DepthTest, rsm.RenderType, renderTasks, renderer)
            | RenderAnimatedModels rams ->
                let renderTasks = GlRenderer3d.getRenderTasks rams.RenderPass renderer
                GlRenderer3d.categorizeAnimatedModels (rams.AnimatedModels, rams.BoneTransforms, rams.AnimatedModel, rams.SubsortOffsets, rams.DualRenderedSurfaceIndices, rams.DepthTest, rams.RenderType, renderTasks, renderer)
            | RenderCachedAnimatedModel camm ->
                let renderTasks = GlRenderer3d.getRenderTasks camm.CachedAnimatedModelRenderPass renderer
                GlRenderer3d.categorizeAnimatedModel (&camm.CachedAnimatedModelMatrix, camm.CachedAnimatedModelCastShadow, camm.CachedAnimatedModelPresence, &camm.CachedAnimatedModelInsetOpt, &camm.CachedAnimatedModelMaterialProperties, camm.CachedAnimatedModelBoneTransforms, camm.CachedAnimatedModel, camm.CachedAnimatedModelSubsortOffsets, camm.CachedAnimatedModelDualRenderedSurfaceIndices, camm.CachedAnimatedModelDepthTest, camm.CachedAnimatedModelRenderType, renderTasks, renderer)
            | RenderTerrain rt ->
                let renderTasks = GlRenderer3d.getRenderTasks rt.RenderPass renderer
                GlRenderer3d.categorizeTerrain (rt.Visible, rt.TerrainDescriptor, renderTasks, renderer)
            | ConfigureLighting3d l3c ->
                if renderer.LightingConfig <> l3c then renderer.LightingConfigChanged <- true
                renderer.LightingConfig <- l3c
            | ConfigureRenderer3d r3c ->
                if renderer.RendererConfig <> r3c then renderer.RendererConfigChanged <- true
                renderer.RendererConfig <- r3c
            | LoadRenderPackage3d packageName ->
                GlRenderer3d.handleLoadRenderPackage packageName renderer
            | UnloadRenderPackage3d packageName ->
                GlRenderer3d.handleUnloadRenderPackage packageName renderer
            | ReloadRenderAssets3d ->
                renderer.ReloadAssetsRequested <- true
        userDefinedStaticModelsToDestroy

    static member private renderPhysicallyBasedDepthSurfaces
        batchPhase eyeCenter viewArray projectionArray viewProjectionArray bonesArray (parameters : struct (Matrix4x4 * bool * Presence * Box2 * MaterialProperties) List)
        (surface : OpenGL.PhysicallyBased.PhysicallyBasedSurface) shader vao vertexSize renderer =

        // ensure we have a large enough instance fields array
        let mutable length = renderer.InstanceFields.Length
        while parameters.Count * Constants.Render.InstanceFieldCount > length do length <- length * 2
        if renderer.InstanceFields.Length < length then
            renderer.InstanceFields <- Array.zeroCreate<single> length

        // blit parameters to instance fields
        for i in 0 .. dec parameters.Count do
            let struct (model, _, _, _, _) = parameters.[i]
            model.ToArray (renderer.InstanceFields, i * Constants.Render.InstanceFieldCount)

        // draw surfaces
        OpenGL.PhysicallyBased.DrawPhysicallyBasedDepthSurfaces
            (batchPhase, eyeCenter, viewArray, projectionArray, viewProjectionArray, bonesArray, parameters.Count,
             renderer.InstanceFields, renderer.LightingConfig.LightShadowExponent, surface.SurfaceMaterial, surface.PhysicallyBasedGeometry, shader, vao, vertexSize)

    static member private renderPhysicallyBasedDepthSurfacePreBatch
        shadowLightType shadowFrustum
        eyeCenter viewArray projectionArray viewProjectionArray bonesArray (parameters : (Matrix4x4 * bool * Presence * Box2 * MaterialProperties * Box3) array)
        (surface : OpenGL.PhysicallyBased.PhysicallyBasedSurface) shader vao vertexSize renderer =

        // ensure we have a large enough instance fields array
        let mutable length = renderer.InstanceFields.Length
        while parameters.Length * Constants.Render.InstanceFieldCount > length do length <- length * 2
        if renderer.InstanceFields.Length < length then
            renderer.InstanceFields <- Array.zeroCreate<single> length

        // blit unculled parameters to instance fields
        let mutable i = 0
        for j in 0 .. dec parameters.Length do
            let (model, castShadow, presence, _, _, bounds) = parameters.[j]
            let unculled =
                castShadow &&
                let shadowFrustumInteriorOpt = if LightType.shouldShadowInterior shadowLightType then ValueSome shadowFrustum else ValueNone
                Presence.intersects3d shadowFrustumInteriorOpt shadowFrustum shadowFrustum ValueNone false false presence bounds
            if unculled then
                model.ToArray (renderer.InstanceFields, i * Constants.Render.InstanceFieldCount)
                i <- inc i

        // draw surfaces
        OpenGL.PhysicallyBased.DrawPhysicallyBasedDepthSurfaces
            (SingletonPhase, eyeCenter, viewArray, projectionArray, viewProjectionArray, bonesArray, i,
             renderer.InstanceFields, renderer.LightingConfig.LightShadowExponent, surface.SurfaceMaterial, surface.PhysicallyBasedGeometry, shader, vao, vertexSize)

    static member private renderPhysicallyBasedDeferredSurfaces
        batchPhase viewArray projectionArray viewProjectionArray bonesArray eyeCenter (parameters : struct (Matrix4x4 * bool * Presence * Box2 * MaterialProperties) List)
        lightShadowSamples lightShadowBias lightShadowSampleScalar lightShadowExponent lightShadowDensity (surface : OpenGL.PhysicallyBased.PhysicallyBasedSurface) shader vao vertexSize renderer =
                                                                      
        // ensure we have a large enough instance fields array
        let mutable length = renderer.InstanceFields.Length
        while parameters.Count * Constants.Render.InstanceFieldCount > length do length <- length * 2
        if renderer.InstanceFields.Length < length then
            renderer.InstanceFields <- Array.zeroCreate<single> length

        // blit parameters to instance fields
        for i in 0 .. dec parameters.Count do
            let struct (model, _, presence, texCoordsOffset, properties) = parameters.[i]
            model.ToArray (renderer.InstanceFields, i * Constants.Render.InstanceFieldCount)
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 16] <- texCoordsOffset.Min.X
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 16 + 1] <- texCoordsOffset.Min.Y
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 16 + 2] <- texCoordsOffset.Min.X + texCoordsOffset.Size.X
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 16 + 3] <- texCoordsOffset.Min.Y + texCoordsOffset.Size.Y
            let albedo = match properties.AlbedoOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Albedo
            let roughness = match properties.RoughnessOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Roughness
            let metallic = match properties.MetallicOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Metallic
            let ambientOcclusion = match properties.AmbientOcclusionOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.AmbientOcclusion
            let emission = match properties.EmissionOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Emission
            let height = match properties.HeightOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Height
            let ignoreLightMaps = match properties.IgnoreLightMapsOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.IgnoreLightMaps
            let opaqueDistance = match properties.OpaqueDistanceOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.OpaqueDistance
            let finenessOffset = match properties.FinenessOffsetOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.FinenessOffset
            let scatterType = match properties.ScatterTypeOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.ScatterType
            let specularScalar = match properties.SpecularScalarOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.SpecularScalar
            let refractiveIndex = match properties.RefractiveIndexOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.RefractiveIndex
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 20] <- albedo.R
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 20 + 1] <- albedo.G
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 20 + 2] <- albedo.B
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 20 + 3] <- albedo.A
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 24] <- roughness
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 24 + 1] <- metallic
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 24 + 2] <- ambientOcclusion
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 24 + 3] <- emission
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 28] <- surface.SurfaceMaterial.AlbedoTexture.TextureMetadata.TextureTexelHeight * height
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 29] <- if ignoreLightMaps then 1.0f else 0.0f
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 30] <- presence.DepthCutoff
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 31] <- opaqueDistance
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 32] <- finenessOffset
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 33] <- scatterType.Enumerate
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 34] <- specularScalar
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 35] <- refractiveIndex

        // draw deferred surfaces
        OpenGL.PhysicallyBased.DrawPhysicallyBasedDeferredSurfaces
            (batchPhase, viewArray, projectionArray, viewProjectionArray, bonesArray, eyeCenter,
             parameters.Count, renderer.InstanceFields, lightShadowSamples, lightShadowBias, lightShadowSampleScalar, lightShadowExponent, lightShadowDensity, surface.SurfaceMaterial, surface.PhysicallyBasedGeometry, shader, vao, vertexSize)

    static member private renderPhysicallyBasedDeferredSurfacePreBatch
        frustumInterior frustumExterior frustumImposter lightBox renderPass
        viewArray projectionArray viewProjectionArray bonesArray eyeCenter (parameters : (Matrix4x4 * bool * Presence * Box2 * MaterialProperties * Box3) array)
        lightShadowSamples lightShadowBias lightShadowSampleScalar lightShadowExponent lightShadowDensity (surface : OpenGL.PhysicallyBased.PhysicallyBasedSurface) shader vao vertexSize renderer =

        // ensure we have a large enough instance fields array
        let mutable length = renderer.InstanceFields.Length
        while parameters.Length * Constants.Render.InstanceFieldCount > length do length <- length * 2
        if renderer.InstanceFields.Length < length then
            renderer.InstanceFields <- Array.zeroCreate<single> length

        // blit parameters to instance fields
        let mutable i = 0
        for j in 0 .. dec parameters.Length do
            let (model, _, presence, texCoordsOffset, properties, bounds) = parameters.[j]
            let unculled =
                match renderPass with
                | LightMapPass (_, _) -> true // TODO: see if we have enough context to cull here.
                | ShadowPass (_, _, shadowLightType, _, shadowFrustum) ->
                    let shadowFrustumInteriorOpt = if LightType.shouldShadowInterior shadowLightType then ValueSome shadowFrustum else ValueNone
                    Presence.intersects3d shadowFrustumInteriorOpt shadowFrustum shadowFrustum ValueNone false false presence bounds
                | ReflectionPass (_, reflFrustum) -> Presence.intersects3d ValueNone reflFrustum reflFrustum ValueNone false false presence bounds
                | NormalPass -> Presence.intersects3d (ValueSome frustumInterior) frustumExterior frustumImposter (ValueSome lightBox) false false presence bounds
            if unculled then
                model.ToArray (renderer.InstanceFields, i * Constants.Render.InstanceFieldCount)
                renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 16] <- texCoordsOffset.Min.X
                renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 16 + 1] <- texCoordsOffset.Min.Y
                renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 16 + 2] <- texCoordsOffset.Min.X + texCoordsOffset.Size.X
                renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 16 + 3] <- texCoordsOffset.Min.Y + texCoordsOffset.Size.Y
                let albedo = match properties.AlbedoOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Albedo
                let roughness = match properties.RoughnessOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Roughness
                let metallic = match properties.MetallicOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Metallic
                let ambientOcclusion = match properties.AmbientOcclusionOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.AmbientOcclusion
                let emission = match properties.EmissionOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Emission
                let height = match properties.HeightOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Height
                let ignoreLightMaps = match properties.IgnoreLightMapsOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.IgnoreLightMaps
                let opaqueDistance = match properties.OpaqueDistanceOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.OpaqueDistance
                let finenessOffset = match properties.FinenessOffsetOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.FinenessOffset
                let scatterType = match properties.ScatterTypeOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.ScatterType
                let specularScalar = match properties.SpecularScalarOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.SpecularScalar
                let refractiveIndex = match properties.RefractiveIndexOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.RefractiveIndex
                renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 20] <- albedo.R
                renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 20 + 1] <- albedo.G
                renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 20 + 2] <- albedo.B
                renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 20 + 3] <- albedo.A
                renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 24] <- roughness
                renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 24 + 1] <- metallic
                renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 24 + 2] <- ambientOcclusion
                renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 24 + 3] <- emission
                renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 28] <- surface.SurfaceMaterial.AlbedoTexture.TextureMetadata.TextureTexelHeight * height
                renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 29] <- if ignoreLightMaps then 1.0f else 0.0f
                renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 30] <- presence.DepthCutoff
                renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 31] <- opaqueDistance
                renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 32] <- finenessOffset
                renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 33] <- scatterType.Enumerate
                renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 34] <- specularScalar
                renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 35] <- refractiveIndex
                i <- inc i

        // draw deferred surfaces
        OpenGL.PhysicallyBased.DrawPhysicallyBasedDeferredSurfaces
            (SingletonPhase, viewArray, projectionArray, viewProjectionArray, bonesArray, eyeCenter,
             i, renderer.InstanceFields, lightShadowSamples, lightShadowBias, lightShadowSampleScalar, lightShadowExponent, lightShadowDensity, surface.SurfaceMaterial, surface.PhysicallyBasedGeometry, shader, vao, vertexSize)

    static member private beginPhysicallyBasedForwardShader
        viewArray projectionArray viewProjectionArray eyeCenter viewInverseArray projectionInverseArray
        lightCutoffMargin lightAmbientColor lightAmbientBrightness lightAmbientBoostCutoff lightAmbientBoostScalar lightShadowSamples lightShadowBias lightShadowSampleScalar lightShadowExponent lightShadowDensity
        fogEnabled fogType fogStart fogFinish fogDensity fogColor ssvfEnabled ssvfSteps ssvfAsymmetry ssvfIntensity ssrlEnabled ssrlIntensity ssrlDetail ssrlRefinementsMax ssrlRayThickness ssrlDistanceCutoff ssrlDistanceCutoffMargin ssrlEdgeHorizontalMargin ssrlEdgeVerticalMargin
        depthTexture colorTexture brdfTexture irradianceMap environmentFilterMap irradianceMaps shader vao =

        // begin shader
        OpenGL.PhysicallyBased.BeginPhysicallyBasedForwardShader
            (viewArray, projectionArray, viewProjectionArray, eyeCenter, viewInverseArray, projectionInverseArray,
             lightCutoffMargin, lightAmbientColor, lightAmbientBrightness, lightAmbientBoostCutoff, lightAmbientBoostScalar, lightShadowSamples, lightShadowBias, lightShadowSampleScalar, lightShadowExponent, lightShadowDensity,
             fogEnabled, fogType, fogStart, fogFinish, fogDensity, fogColor, ssvfEnabled, ssvfSteps, ssvfAsymmetry, ssvfIntensity, ssrlEnabled, ssrlIntensity, ssrlDetail, ssrlRefinementsMax, ssrlRayThickness, ssrlDistanceCutoff, ssrlDistanceCutoffMargin, ssrlEdgeHorizontalMargin, ssrlEdgeVerticalMargin,
             depthTexture, colorTexture, brdfTexture, irradianceMap, environmentFilterMap, irradianceMaps, shader, vao)

    static member private renderPhysicallyBasedForwardSurfaces
        bonesArrays (parameters : struct (Matrix4x4 * Presence * Box2 * MaterialProperties) SList)
        irradianceMaps environmentFilterMaps shadowTextures shadowMaps shadowCascades lightMapOrigins lightMapMins lightMapSizes lightMapAmbientColors lightMapAmbientBrightnesses lightMapsCount
        lightOrigins lightDirections lightColors lightBrightnesses lightAttenuationLinears lightAttenuationQuadratics lightCutoffs lightTypes lightConeInners lightConeOuters lightDesireFogs lightShadowIndices lightsCount shadowMatrices
        (surface : OpenGL.PhysicallyBased.PhysicallyBasedSurface) depthTest blending shader vao vertexSize renderer =

        // ensure we have a large enough instance fields array
        let mutable length = renderer.InstanceFields.Length
        while parameters.Length * Constants.Render.InstanceFieldCount > length do length <- length * 2
        if renderer.InstanceFields.Length < length then
            renderer.InstanceFields <- Array.zeroCreate<single> length

        // blit parameters to instance fields
        for i in 0 .. dec parameters.Length do
            let struct (model, presence, texCoordsOffset, properties) = parameters.[i]
            model.ToArray (renderer.InstanceFields, i * Constants.Render.InstanceFieldCount)
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 16] <- texCoordsOffset.Min.X
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 16 + 1] <- texCoordsOffset.Min.Y
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 16 + 2] <- texCoordsOffset.Min.X + texCoordsOffset.Size.X
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 16 + 3] <- texCoordsOffset.Min.Y + texCoordsOffset.Size.Y
            let albedo = match properties.AlbedoOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Albedo
            let roughness = match properties.RoughnessOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Roughness
            let metallic = match properties.MetallicOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Metallic
            let ambientOcclusion = match properties.AmbientOcclusionOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.AmbientOcclusion
            let emission = match properties.EmissionOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Emission
            let height = match properties.HeightOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Height
            let ignoreLightMaps = match properties.IgnoreLightMapsOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.IgnoreLightMaps
            let opaqueDistance = match properties.OpaqueDistanceOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.OpaqueDistance
            let finenessOffset = match properties.FinenessOffsetOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.FinenessOffset
            let scatterType = match properties.ScatterTypeOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.ScatterType
            let specularScalar = match properties.SpecularScalarOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.SpecularScalar
            let refractiveIndex = match properties.RefractiveIndexOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.RefractiveIndex
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 20] <- albedo.R
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 20 + 1] <- albedo.G
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 20 + 2] <- albedo.B
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 20 + 3] <- albedo.A
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 24] <- roughness
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 24 + 1] <- metallic
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 24 + 2] <- ambientOcclusion
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 24 + 3] <- emission
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 28] <- surface.SurfaceMaterial.AlbedoTexture.TextureMetadata.TextureTexelHeight * height
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 29] <- if ignoreLightMaps then 1.0f else 0.0f
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 30] <- presence.DepthCutoff
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 31] <- opaqueDistance
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 32] <- finenessOffset
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 33] <- scatterType.Enumerate
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 34] <- specularScalar
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 35] <- refractiveIndex

        // draw forward surfaces
        OpenGL.PhysicallyBased.DrawPhysicallyBasedForwardSurfaces
            (bonesArrays, parameters.Length, renderer.InstanceFields,
             irradianceMaps, environmentFilterMaps, shadowTextures, shadowMaps, shadowCascades, lightMapOrigins, lightMapMins, lightMapSizes, lightMapAmbientColors, lightMapAmbientBrightnesses, lightMapsCount,
             lightOrigins, lightDirections, lightColors, lightBrightnesses, lightAttenuationLinears, lightAttenuationQuadratics, lightCutoffs, lightTypes, lightConeInners, lightConeOuters, lightDesireFogs, lightShadowIndices, lightsCount, shadowMatrices,
             surface.SurfaceMaterial, surface.PhysicallyBasedGeometry, depthTest, blending, shader, vao, vertexSize)

    static member private endPhysicallyBasedForwardShader shader vao =

        // end shader
        OpenGL.PhysicallyBased.EndPhysicallyBasedForwardShader (shader, vao)

    // TODO: apply intention blocks to this function.
    static member private renderPhysicallyBasedTerrain
        viewArray projectionArray viewProjectionArray eyeCenter
        lightShadowSamples lightShadowBias lightShadowSampleScalar lightShadowExponent lightShadowDensity
        terrainDescriptor geometry shader vao renderer =
        let (resolutionX, resolutionY) = Option.defaultValue (0, 0) (GlRenderer3d.tryGetHeightMapResolution terrainDescriptor.HeightMap renderer)                
        let elementsCount = dec resolutionX * dec resolutionY * 6
        let terrainMaterialProperties = terrainDescriptor.MaterialProperties
        let materialProperties : OpenGL.PhysicallyBased.PhysicallyBasedMaterialProperties =
            { Albedo = Option.defaultValue Constants.Render.AlbedoDefault terrainMaterialProperties.AlbedoOpt
              Roughness = Option.defaultValue Constants.Render.RoughnessDefault terrainMaterialProperties.RoughnessOpt
              Metallic = Constants.Render.MetallicDefault
              AmbientOcclusion = Option.defaultValue Constants.Render.AmbientOcclusionDefault terrainMaterialProperties.AmbientOcclusionOpt
              Emission = Constants.Render.EmissionDefault
              Height = Option.defaultValue Constants.Render.HeightDefault terrainMaterialProperties.HeightOpt
              IgnoreLightMaps = Option.defaultValue Constants.Render.IgnoreLightMapsDefault terrainMaterialProperties.IgnoreLightMapsOpt
              OpaqueDistance = Constants.Render.OpaqueDistanceDefault
              FinenessOffset = Constants.Render.FinenessOffsetDefault
              ScatterType = Constants.Render.ScatterTypeDefault
              SpecularScalar = Constants.Render.SpecularScalarDefault
              RefractiveIndex = Constants.Render.RefractiveIndexDefault }
        let (texelWidth, texelHeight, materials) =
            match terrainDescriptor.Material with
            | BlendMaterial blendMaterial ->
                let mutable texelWidth = Single.MaxValue
                let mutable texelHeight = Single.MaxValue
                let materials =
                    [|for i in 0 .. dec blendMaterial.TerrainLayers.Length do
                        let layer =
                            blendMaterial.TerrainLayers.[i]
                        let defaultMaterial =
                            renderer.PhysicallyBasedMaterial
                        let albedoTexture =
                            match GlRenderer3d.tryGetRenderAsset layer.AlbedoImage renderer with
                            | ValueSome renderAsset -> match renderAsset with TextureAsset texture -> texture | _ -> defaultMaterial.AlbedoTexture
                            | ValueNone -> defaultMaterial.AlbedoTexture
                        let roughnessTexture =
                            match GlRenderer3d.tryGetRenderAsset layer.RoughnessImage renderer with
                            | ValueSome renderAsset -> match renderAsset with TextureAsset texture -> texture | _ -> defaultMaterial.RoughnessTexture
                            | ValueNone -> defaultMaterial.RoughnessTexture
                        let ambientOcclusionTexture =
                            match GlRenderer3d.tryGetRenderAsset layer.AmbientOcclusionImage renderer with
                            | ValueSome renderAsset -> match renderAsset with TextureAsset texture -> texture | _ -> defaultMaterial.AmbientOcclusionTexture
                            | ValueNone -> defaultMaterial.AmbientOcclusionTexture
                        let normalTexture =
                            match GlRenderer3d.tryGetRenderAsset layer.NormalImage renderer with
                            | ValueSome renderAsset -> match renderAsset with TextureAsset texture -> texture | _ -> defaultMaterial.NormalTexture
                            | ValueNone -> defaultMaterial.NormalTexture
                        let heightTexture =
                            match GlRenderer3d.tryGetRenderAsset layer.HeightImage renderer with
                            | ValueSome renderAsset -> match renderAsset with TextureAsset texture -> texture | _ -> defaultMaterial.HeightTexture
                            | ValueNone -> defaultMaterial.HeightTexture
                        let albedoMetadata = albedoTexture.TextureMetadata
                        texelWidth <- min texelWidth albedoMetadata.TextureTexelWidth
                        texelHeight <- min texelHeight albedoMetadata.TextureTexelHeight
                        { defaultMaterial with
                            AlbedoTexture = albedoTexture
                            RoughnessTexture = roughnessTexture
                            AmbientOcclusionTexture = ambientOcclusionTexture
                            NormalTexture = normalTexture
                            HeightTexture = heightTexture }|]
                (texelWidth, texelHeight, materials)
            | FlatMaterial flatMaterial ->
                let defaultMaterial =
                    renderer.PhysicallyBasedMaterial
                let albedoTexture =
                    match GlRenderer3d.tryGetRenderAsset flatMaterial.AlbedoImage renderer with
                    | ValueSome renderAsset -> match renderAsset with TextureAsset texture -> texture | _ -> defaultMaterial.AlbedoTexture
                    | ValueNone -> defaultMaterial.AlbedoTexture
                let roughnessTexture =
                    match GlRenderer3d.tryGetRenderAsset flatMaterial.RoughnessImage renderer with
                    | ValueSome renderAsset -> match renderAsset with TextureAsset texture -> texture | _ -> defaultMaterial.RoughnessTexture
                    | ValueNone -> defaultMaterial.RoughnessTexture
                let ambientOcclusionTexture =
                    match GlRenderer3d.tryGetRenderAsset flatMaterial.AmbientOcclusionImage renderer with
                    | ValueSome renderAsset -> match renderAsset with TextureAsset texture -> texture | _ -> defaultMaterial.AmbientOcclusionTexture
                    | ValueNone -> defaultMaterial.AmbientOcclusionTexture
                let normalTexture =
                    match GlRenderer3d.tryGetRenderAsset flatMaterial.NormalImage renderer with
                    | ValueSome renderAsset -> match renderAsset with TextureAsset texture -> texture | _ -> defaultMaterial.NormalTexture
                    | ValueNone -> defaultMaterial.NormalTexture
                let heightTexture =
                    match GlRenderer3d.tryGetRenderAsset flatMaterial.HeightImage renderer with
                    | ValueSome renderAsset -> match renderAsset with TextureAsset texture -> texture | _ -> defaultMaterial.HeightTexture
                    | ValueNone -> defaultMaterial.HeightTexture
                let material =
                    { defaultMaterial with
                        AlbedoTexture = albedoTexture
                        RoughnessTexture = roughnessTexture
                        AmbientOcclusionTexture = ambientOcclusionTexture
                        NormalTexture = normalTexture
                        HeightTexture = heightTexture }
                let albedoMetadata = albedoTexture.TextureMetadata
                (albedoMetadata.TextureTexelWidth, albedoMetadata.TextureTexelHeight, [|material|])
        let texCoordsOffset =
            match terrainDescriptor.InsetOpt with
            | Some inset ->
                let texelWidth = texelWidth
                let texelHeight = texelHeight
                let px = inset.Min.X * texelWidth
                let py = (inset.Min.Y + inset.Size.Y) * texelHeight
                let sx = inset.Size.X * texelWidth
                let sy = -inset.Size.Y * texelHeight
                Box2 (px, py, sx, sy)
            | None -> box2 v2Zero v2Zero
        let instanceFields = Array.zeroCreate Constants.Render.InstanceFieldCount
        m4Identity.ToArray (instanceFields, 0)
        instanceFields.[16] <- texCoordsOffset.Min.X
        instanceFields.[17] <- texCoordsOffset.Min.Y
        instanceFields.[18] <- texCoordsOffset.Min.X + texCoordsOffset.Size.X
        instanceFields.[19] <- texCoordsOffset.Min.Y + texCoordsOffset.Size.Y
        instanceFields.[20] <- materialProperties.Albedo.R
        instanceFields.[21] <- materialProperties.Albedo.G
        instanceFields.[22] <- materialProperties.Albedo.B
        instanceFields.[23] <- materialProperties.Albedo.A
        instanceFields.[24] <- materialProperties.Roughness
        instanceFields.[25] <- materialProperties.Metallic
        instanceFields.[26] <- materialProperties.AmbientOcclusion
        instanceFields.[27] <- materialProperties.Emission
        instanceFields.[28] <- texelHeight * materialProperties.Height
        OpenGL.PhysicallyBased.DrawPhysicallyBasedTerrain
            (viewArray, projectionArray, viewProjectionArray, eyeCenter,
             instanceFields, lightShadowSamples, lightShadowBias, lightShadowSampleScalar, lightShadowExponent, lightShadowDensity,
             elementsCount, materials, geometry, shader, vao)

    static member private renderShadow
        lightOrigin
        (lightView : Matrix4x4)
        (lightProjection : Matrix4x4)
        (lightViewProjection : Matrix4x4)
        lightFrustum
        lightType
        renderTasks
        renderer =

        // compute matrix arrays
        let lightViewArray = lightView.ToArray ()
        let lightProjectionArray = lightProjection.ToArray ()
        let lightViewProjectionArray = lightViewProjection.ToArray ()

        // deferred render static surface shadows
        let mutable i = 0
        for entry in renderTasks.DeferredStatic do
            let batchPhase =
                match renderTasks.DeferredStatic.Count with
                | 1 -> SingletonPhase
                | count -> if i = 0 then StartingPhase elif i = dec count then StoppingPhase else ResumingPhase
            let shadowShader =
                match lightType with
                | PointLight -> renderer.PhysicallyBasedShaders.ShadowStaticPointShader
                | SpotLight (_, _) -> renderer.PhysicallyBasedShaders.ShadowStaticSpotShader
                | DirectionalLight | CascadedLight -> renderer.PhysicallyBasedShaders.ShadowStaticDirectionalShader
            GlRenderer3d.renderPhysicallyBasedDepthSurfaces
                batchPhase lightOrigin lightViewArray lightProjectionArray lightViewProjectionArray [||] entry.Value
                entry.Key shadowShader renderer.PhysicallyBasedStaticVao OpenGL.PhysicallyBased.StaticVertexSize renderer
            OpenGL.Hl.Assert ()
            i <- inc i

        // deferred render static surface preBatches shadows
        for entry in renderTasks.DeferredStaticPreBatches do
            let struct (surface, preBatch) = entry.Value
            let shadowShader =
                match lightType with
                | PointLight -> renderer.PhysicallyBasedShaders.ShadowStaticPointShader
                | SpotLight (_, _) -> renderer.PhysicallyBasedShaders.ShadowStaticSpotShader
                | DirectionalLight | CascadedLight -> renderer.PhysicallyBasedShaders.ShadowStaticDirectionalShader
            GlRenderer3d.renderPhysicallyBasedDepthSurfacePreBatch
                lightType lightFrustum lightOrigin lightViewArray lightProjectionArray lightViewProjectionArray [||] preBatch
                surface shadowShader renderer.PhysicallyBasedStaticVao OpenGL.PhysicallyBased.StaticVertexSize renderer
            OpenGL.Hl.Assert ()

        // deferred render static surface clipped shadows (TODO: consider implementing clipped shadow rendering.)
        let mutable i = 0
        for entry in renderTasks.DeferredStaticClipped do
            let batchPhase =
                match renderTasks.DeferredStaticClipped.Count with
                | 1 -> SingletonPhase
                | count -> if i = 0 then StartingPhase elif i = dec count then StoppingPhase else ResumingPhase
            let shadowShader =
                match lightType with
                | PointLight -> renderer.PhysicallyBasedShaders.ShadowStaticPointShader
                | SpotLight (_, _) -> renderer.PhysicallyBasedShaders.ShadowStaticSpotShader
                | DirectionalLight | CascadedLight -> renderer.PhysicallyBasedShaders.ShadowStaticDirectionalShader
            GlRenderer3d.renderPhysicallyBasedDepthSurfaces
                batchPhase lightOrigin lightViewArray lightProjectionArray lightViewProjectionArray [||] entry.Value
                entry.Key shadowShader renderer.PhysicallyBasedStaticVao OpenGL.PhysicallyBased.StaticVertexSize renderer
            OpenGL.Hl.Assert ()
            i <- inc i

        // deferred render static surface pre-batches clipped shadows (TODO: consider implementing clipped shadow rendering.)
        for entry in renderTasks.DeferredStaticClippedPreBatches do
            let struct (surface, preBatch) = entry.Value
            let shadowShader =
                match lightType with
                | PointLight -> renderer.PhysicallyBasedShaders.ShadowStaticPointShader
                | SpotLight (_, _) -> renderer.PhysicallyBasedShaders.ShadowStaticSpotShader
                | DirectionalLight | CascadedLight -> renderer.PhysicallyBasedShaders.ShadowStaticDirectionalShader
            GlRenderer3d.renderPhysicallyBasedDepthSurfacePreBatch
                lightType lightFrustum lightOrigin lightViewArray lightProjectionArray lightViewProjectionArray [||] preBatch
                surface shadowShader renderer.PhysicallyBasedStaticVao OpenGL.PhysicallyBased.StaticVertexSize renderer
            OpenGL.Hl.Assert ()

        // deferred render animated surface shadows
        for entry in renderTasks.DeferredAnimated do
            let surfaceKey = entry.Key
            let parameters = entry.Value
            let boneArrays = List ()
            let bonesArrays = Array.zeroCreate surfaceKey.BoneTransforms.Length
            for i in 0 .. dec surfaceKey.BoneTransforms.Length do
                let boneArray = surfaceKey.BoneTransforms.[i].ToArray ()
                boneArrays.Add boneArray
                bonesArrays.[i] <- boneArray
            let shadowShader =
                match lightType with
                | PointLight -> renderer.PhysicallyBasedShaders.ShadowAnimatedPointShader
                | SpotLight (_, _) -> renderer.PhysicallyBasedShaders.ShadowAnimatedSpotShader
                | DirectionalLight | CascadedLight -> renderer.PhysicallyBasedShaders.ShadowAnimatedDirectionalShader
            GlRenderer3d.renderPhysicallyBasedDepthSurfaces
                SingletonPhase lightOrigin lightViewArray lightProjectionArray lightViewProjectionArray bonesArrays parameters
                surfaceKey.AnimatedSurface shadowShader renderer.PhysicallyBasedAnimatedVao OpenGL.PhysicallyBased.AnimatedVertexSize renderer
            OpenGL.Hl.Assert ()

        // attempt to deferred render terrain shadows
        for struct (descriptor, patchDescriptor, geometry) in renderTasks.DeferredTerrains do
            if lightFrustum.Intersects patchDescriptor.PatchBounds then
                let shadowShader =
                    match lightType with
                    | PointLight -> renderer.PhysicallyBasedShaders.ShadowTerrainPointShader
                    | SpotLight (_, _) -> renderer.PhysicallyBasedShaders.ShadowTerrainSpotShader
                    | DirectionalLight | CascadedLight -> renderer.PhysicallyBasedShaders.ShadowTerrainDirectionalShader
                GlRenderer3d.renderPhysicallyBasedTerrain
                    lightViewArray lightProjectionArray lightViewProjectionArray lightOrigin
                    renderer.LightingConfig.LightShadowSamples renderer.LightingConfig.LightShadowBias renderer.LightingConfig.LightShadowSampleScalar renderer.LightingConfig.LightShadowExponent renderer.LightingConfig.LightShadowDensity
                    descriptor geometry shadowShader renderer.PhysicallyBasedTerrainVao renderer

        // forward render surface shadows
        for struct (model, castShadow, presence, texCoordsOffset, properties, boneTransformsOpt, surface, _) in renderTasks.ForwardSorted do
            if castShadow then
                match boneTransformsOpt with
                | ValueSome boneTransforms ->
                    let bonesArrays = Array.zeroCreate boneTransforms.Length
                    for i in 0 .. dec boneTransforms.Length do
                        let boneArray = boneTransforms.[i].ToArray ()
                        bonesArrays.[i] <- boneArray
                    let shadowShader =
                        match lightType with
                        | PointLight -> renderer.PhysicallyBasedShaders.ShadowAnimatedPointShader
                        | SpotLight (_, _) -> renderer.PhysicallyBasedShaders.ShadowAnimatedSpotShader
                        | DirectionalLight | CascadedLight -> renderer.PhysicallyBasedShaders.ShadowAnimatedDirectionalShader
                    GlRenderer3d.renderPhysicallyBasedDepthSurfaces
                        SingletonPhase lightOrigin lightViewArray lightProjectionArray lightViewProjectionArray bonesArrays (List ([struct (model, castShadow, presence, texCoordsOffset, properties)]))
                        surface shadowShader renderer.PhysicallyBasedAnimatedVao OpenGL.PhysicallyBased.AnimatedVertexSize renderer
                    OpenGL.Hl.Assert ()
                | ValueNone ->
                    let shadowShader =
                        match lightType with
                        | PointLight -> renderer.PhysicallyBasedShaders.ShadowStaticPointShader
                        | SpotLight (_, _) -> renderer.PhysicallyBasedShaders.ShadowStaticSpotShader
                        | DirectionalLight | CascadedLight -> renderer.PhysicallyBasedShaders.ShadowStaticDirectionalShader
                    GlRenderer3d.renderPhysicallyBasedDepthSurfaces
                        SingletonPhase lightOrigin lightViewArray lightProjectionArray lightViewProjectionArray [||] (List ([struct (model, castShadow, presence, texCoordsOffset, properties)]))
                        surface shadowShader renderer.PhysicallyBasedStaticVao OpenGL.PhysicallyBased.StaticVertexSize renderer
                    OpenGL.Hl.Assert ()

    static member private renderShadowTexture
        renderTasks
        renderer
        (lightOrigin : Vector3)
        (lightView : Matrix4x4)
        (lightProjection : Matrix4x4)
        (lightViewProjection : Matrix4x4)
        (lightFrustum : Frustum)
        (lightType : LightType)
        (shadowResolution : Vector2i)
        (renderbuffer : uint)
        (framebuffer : uint) =

        // send forward surfaces directly to sorted buffer since no sorting is needed for shadows
        for struct (_, _, model, castShadow, presence, texCoordsOffset, properties, boneTransformsOpt, surface, depthTest) in renderTasks.Forward do
            renderTasks.ForwardSorted.Add struct (model, castShadow, presence, texCoordsOffset, properties, boneTransformsOpt, surface, depthTest)

        // setup shadow buffer and viewport
        OpenGL.Gl.Viewport (0, 0, shadowResolution.X, shadowResolution.Y)
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, renderbuffer)
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, framebuffer)
        OpenGL.Gl.ClearColor (1.0f, Single.MaxValue, 0.0f, 0.0f)
        OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit)
        OpenGL.Hl.Assert ()

        // actually render shadow
        GlRenderer3d.renderShadow lightOrigin lightView lightProjection lightViewProjection lightFrustum lightType renderTasks renderer

        // unbind shadow mapping frame buffer
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, 0u)

    static member private renderShadowMapFace
        renderTasks
        renderer
        (lightOrigin : Vector3)
        (lightCutoff : single)
        (shadowFace : int)
        (shadowView : Matrix4x4)
        (shadowProjection : Matrix4x4)
        (shadowViewProjection : Matrix4x4)
        (shadowFrustum : Frustum)
        (shadowResolution : Vector2i)
        (shadowMap : OpenGL.Texture.Texture)
        (renderbuffer : uint)
        (framebuffer : uint) =

        // send forward surfaces directly to sorted buffer since no sorting is needed for shadows
        for struct (_, _, model, castShadow, presence, texCoordsOffset, properties, boneTransformsOpt, surface, depthTest) in renderTasks.Forward do
            renderTasks.ForwardSorted.Add struct (model, castShadow, presence, texCoordsOffset, properties, boneTransformsOpt, surface, depthTest)

        // setup shadow frame buffer and viewport
        OpenGL.Gl.Viewport (0, 0, shadowResolution.X, shadowResolution.Y)
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, renderbuffer)
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, framebuffer)
        OpenGL.Hl.Assert ()

        // setup shadow cube map face for rendering
        let target = LanguagePrimitives.EnumOfValue (int OpenGL.TextureTarget.TextureCubeMapPositiveX + shadowFace)
        OpenGL.Gl.FramebufferTexture2D (OpenGL.FramebufferTarget.Framebuffer, OpenGL.FramebufferAttachment.ColorAttachment0, target, shadowMap.TextureId, 0)
        OpenGL.Gl.ClearColor (lightCutoff, 0.0f, 0.0f, 0.0f)
        OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit)
        OpenGL.Hl.Assert ()
        
        // render to shadow cube map face
        GlRenderer3d.renderShadow lightOrigin shadowView shadowProjection shadowViewProjection shadowFrustum PointLight renderTasks renderer
        OpenGL.Hl.Assert ()
        
        // unbind shadow cube map face
        OpenGL.Gl.FramebufferTexture2D (OpenGL.FramebufferTarget.Framebuffer, OpenGL.FramebufferAttachment.ColorAttachment0, target, 0u, 0)
        OpenGL.Hl.Assert ()
        
        // unbind shadow frame buffer
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, 0u)

    static member private renderShadowCascade
        renderTasks
        renderer
        (lightOrigin : Vector3)
        (lightCutoff : single)
        (shadowCascadeLevel : int)
        (shadowView : Matrix4x4)
        (shadowProjection : Matrix4x4)
        (shadowViewProjection : Matrix4x4)
        (shadowFrustum : Frustum)
        (shadowResolution : Vector2i)
        (shadowCascade : OpenGL.Texture.Texture)
        (renderbuffer : uint)
        (framebuffer : uint) =

        // send forward surfaces directly to sorted buffer since no sorting is needed for shadows
        for struct (_, _, model, castShadow, presence, texCoordsOffset, properties, boneTransformsOpt, surface, depthTest) in renderTasks.Forward do
            renderTasks.ForwardSorted.Add struct (model, castShadow, presence, texCoordsOffset, properties, boneTransformsOpt, surface, depthTest)

        // setup shadow frame buffer and viewport
        OpenGL.Gl.Viewport (0, 0, shadowResolution.X, shadowResolution.Y)
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, renderbuffer)
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, framebuffer)
        OpenGL.Hl.Assert ()

        // setup shadow cascade layer for rendering
        OpenGL.Gl.FramebufferTextureLayer (OpenGL.FramebufferTarget.Framebuffer, OpenGL.FramebufferAttachment.ColorAttachment0, shadowCascade.TextureId, 0, shadowCascadeLevel)
        OpenGL.Gl.ClearColor (lightCutoff, 0.0f, 0.0f, 0.0f)
        OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit)
        OpenGL.Hl.Assert ()

        // render to shadow cascade
        GlRenderer3d.renderShadow lightOrigin shadowView shadowProjection shadowViewProjection shadowFrustum CascadedLight renderTasks renderer
        OpenGL.Hl.Assert ()

        // unbind shadow cascade layer
        OpenGL.Gl.FramebufferTextureLayer (OpenGL.FramebufferTarget.Framebuffer, OpenGL.FramebufferAttachment.ColorAttachment0, 0u, 0, shadowCascadeLevel)
        OpenGL.Hl.Assert ()

        // unbind shadow frame buffer
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, 0u)

    static member private renderGeometry
        frustumInterior
        frustumExterior
        frustumImposter
        lightBox
        renderPass
        renderTasks
        renderer
        topLevelRender
        lightAmbientOverride
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (viewSkyBox : Matrix4x4)
        (geometryFrustum : Frustum)
        (geometryProjection : Matrix4x4)
        (geometryViewProjection : Matrix4x4)
        (rasterBounds : Box2i)
        (rasterProjection : Matrix4x4)
        (framebuffer : uint) =

        // compute matrix arrays
        let viewArray = view.ToArray ()
        let viewInverse = view.Inverted
        let viewInverseArray = viewInverse.ToArray ()
        let viewSkyBoxArray = viewSkyBox.ToArray ()
        let geometryProjectionArray = geometryProjection.ToArray ()
        let geometryProjectionInverse = geometryProjection.Inverted
        let geometryProjectionInverseArray = geometryProjectionInverse.ToArray ()
        let geometryViewProjectionArray = geometryViewProjection.ToArray ()
        let rasterProjectionArray = rasterProjection.ToArray ()
        let rasterProjectionInverse = rasterProjection.Inverted
        let rasterProjectionInverseArray = rasterProjectionInverse.ToArray ()
        let rasterViewProjectionSkyBox = viewSkyBox * rasterProjection
        let rasterViewProjectionSkyBoxArray = rasterViewProjectionSkyBox.ToArray ()

        // get ambient lighting, sky box opt, and fallback light map
        let (lightAmbientColor, lightAmbientBrightness, skyBoxOpt) = GlRenderer3d.getLastSkyBoxOpt renderPass renderer
        let (lightAmbientColor, lightAmbientBrightness) = Option.defaultValue (lightAmbientColor, lightAmbientBrightness) lightAmbientOverride
        let lightMapFallback =
            match skyBoxOpt with
            | Some (ambientColor, ambientBrightness, _, (irradianceAndEnvironmentMapsOptRef : (OpenGL.Texture.Texture * OpenGL.Texture.Texture) option ref)) ->
                let (irradianceMap, environmentFilterMap) =
                    match irradianceAndEnvironmentMapsOptRef.Value with
                    | Some irradianceAndEnvironmentMaps -> irradianceAndEnvironmentMaps
                    | None -> (renderer.IrradianceMap, renderer.EnvironmentFilterMap)
                OpenGL.LightMap.CreateLightMap true v3Zero ambientColor ambientBrightness box3Zero irradianceMap environmentFilterMap
            | None -> OpenGL.LightMap.CreateLightMap true v3Zero Color.White 1.0f box3Zero renderer.IrradianceMap renderer.EnvironmentFilterMap

        // destroy cached light maps whose originating probe no longer exists
        for lightMapKvp in renderer.LightMaps do
            if not (renderTasks.LightProbes.ContainsKey lightMapKvp.Key) then
                OpenGL.LightMap.DestroyLightMap lightMapKvp.Value
                renderer.LightMaps.Remove lightMapKvp.Key |> ignore<bool>

        // ensure light maps are synchronized with any light probe changes
        for (lightMapId, lightMap) in renderer.LightMaps.Pairs do
            match renderTasks.LightProbes.TryGetValue lightMapId with
            | (true, (lightProbeEnabled, lightProbeOrigin, lightProbeAmbientColor, lightProbeAmbientBrightness, lightProbeBounds)) ->
                if  lightMap.Enabled <> lightProbeEnabled ||
                    lightMap.Origin <> lightProbeOrigin ||
                    lightMap.AmbientColor <> lightProbeAmbientColor ||
                    lightMap.AmbientBrightness <> lightProbeAmbientBrightness ||
                    lightMap.Bounds <> lightProbeBounds then
                    let lightMap =
                        { lightMap with
                            Enabled = lightProbeEnabled
                            Origin = lightProbeOrigin
                            AmbientColor = lightProbeAmbientColor
                            AmbientBrightness = lightProbeAmbientBrightness
                            Bounds = lightProbeBounds }
                    renderer.LightMaps.[lightMapId] <- lightMap
            | _ -> ()

        // collect light maps from cached light maps and ensure they're up to date with their light probes
        for lightMapKvp in renderer.LightMaps do
            let lightMap =
                { SortableLightMapEnabled = lightMapKvp.Value.Enabled
                  SortableLightMapOrigin = lightMapKvp.Value.Origin
                  SortableLightMapBounds = lightMapKvp.Value.Bounds
                  SortableLightMapAmbientColor = lightMapKvp.Value.AmbientColor
                  SortableLightMapAmbientBrightness = lightMapKvp.Value.AmbientBrightness
                  SortableLightMapIrradianceMap = lightMapKvp.Value.IrradianceMap
                  SortableLightMapEnvironmentFilterMap = lightMapKvp.Value.EnvironmentFilterMap
                  SortableLightMapDistanceSquared = Single.MaxValue }
            let lightMap =
                match renderTasks.LightProbes.TryGetValue lightMapKvp.Key with
                | (true, (lightProbeEnabled, lightProbeOrigin, lightProbeAmbientColor, lightProbeAmbientBrightness, lightProbeBounds)) ->
                    if  lightMap.SortableLightMapEnabled <> lightProbeEnabled ||
                        lightMap.SortableLightMapOrigin <> lightProbeOrigin ||
                        lightMap.SortableLightMapAmbientColor <> lightProbeAmbientColor ||
                        lightMap.SortableLightMapAmbientBrightness <> lightProbeAmbientBrightness ||
                        lightMap.SortableLightMapBounds <> lightProbeBounds then
                        { lightMap with
                            SortableLightMapEnabled = lightProbeEnabled
                            SortableLightMapOrigin = lightProbeOrigin
                            SortableLightMapAmbientColor = lightProbeAmbientColor
                            SortableLightMapAmbientBrightness = lightProbeAmbientBrightness
                            SortableLightMapBounds = lightProbeBounds }
                    else lightMap
                | _ -> lightMap
            renderTasks.LightMaps.Add lightMap

        // filter light maps according to enabledness and intersection with the geometry frustum
        let lightMaps =
            renderTasks.LightMaps
            |> Array.ofSeq
            |> Array.filter (fun lightMap -> lightMap.SortableLightMapEnabled && geometryFrustum.Intersects lightMap.SortableLightMapBounds)

        // sort light maps for deferred rendering relative to eye center
        let (lightMapOrigins, lightMapMins, lightMapSizes, lightMapAmbientColors, lightMapAmbientBrightnesses, lightMapIrradianceMaps, lightMapEnvironmentFilterMaps) =
            if topLevelRender then
                SortableLightMap.sortLightMaps Constants.Render.LightMapsMaxDeferred eyeCenter None lightMaps
            else
                (Array.zeroCreate (Constants.Render.LightMapsMaxDeferred * 4),
                 Array.zeroCreate (Constants.Render.LightMapsMaxDeferred * 4),
                 Array.zeroCreate (Constants.Render.LightMapsMaxDeferred * 4),
                 Array.zeroCreate (Constants.Render.LightMapsMaxDeferred * 4),
                 Array.zeroCreate Constants.Render.LightMapsMaxDeferred,
                 Array.init Constants.Render.LightMapsMaxDeferred (fun _ -> OpenGL.Texture.EmptyTexture),
                 Array.init Constants.Render.LightMapsMaxDeferred (fun _ -> OpenGL.Texture.EmptyTexture))

        // sort lights for deferred rendering relative to eye center
        let (lightIds, lightOrigins, lightDirections, lightColors, lightBrightnesses, lightAttenuationLinears, lightAttenuationQuadratics, lightCutoffs, lightTypes, lightConeInners, lightConeOuters, lightDesireFogs) =
            SortableLight.sortLights Constants.Render.LightsMaxDeferred eyeCenter renderTasks.Lights

        // compute light shadow indices according to sorted lights
        let lightShadowIndices = SortableLight.sortLightShadowIndices renderer.LightShadowIndices lightIds

        // grab shadow textures
        let shadowTextures = Array.map a__ renderer.PhysicallyBasedBuffers.ShadowTextureBuffersArray

        // grab shadow maps
        let shadowMaps = Array.map a__ renderer.PhysicallyBasedBuffers.ShadowMapBuffersArray

        // grab shadow cascades
        let shadowCascades = Array.map a__ renderer.PhysicallyBasedBuffers.ShadowCascadeArrayBuffersArray

        // presume shadow near plane distance as interior near plane distance
        let shadowNear = Constants.Render.NearPlaneDistanceInterior

        // grab shadow matrices
        let shadowMatrices = Array.map (fun (m : Matrix4x4) -> m.ToArray ()) renderer.ShadowMatrices

        // sort forward surfaces from far to near
        let forwardSurfacesSortBuffer = GlRenderer3d.sortForwardSurfaces eyeCenter renderTasks.Forward renderer.ForwardSurfacesComparer renderer.ForwardSurfacesSortBuffer
        for struct (_, _, model, castShadow, presence, texCoordsOffset, properties, boneTransformsOpt, surface, depthTest, _, _) in forwardSurfacesSortBuffer do
            renderTasks.ForwardSorted.Add struct (model, castShadow, presence, texCoordsOffset, properties, boneTransformsOpt, surface, depthTest)
        forwardSurfacesSortBuffer.Clear ()

        // setup geometry buffer and viewport
        let geometryResolution = renderer.GeometryViewport.Bounds.Size
        let (depthTexture, albedoTexture, materialTexture, normalPlusTexture, subdermalPlusTexture, scatterPlusTexture, geometryRenderbuffer, geometryFramebuffer) = renderer.PhysicallyBasedBuffers.GeometryBuffers
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, geometryRenderbuffer)
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, geometryFramebuffer)
        OpenGL.Gl.ClearColor (Constants.Render.ViewportClearColor.R, Constants.Render.ViewportClearColor.G, Constants.Render.ViewportClearColor.B, Constants.Render.ViewportClearColor.A)
        OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
        OpenGL.Gl.Viewport (0, 0, geometryResolution.X, geometryResolution.Y)
        OpenGL.Hl.Assert ()

        // render static surfaces deferred
        let mutable i = 0
        for entry in renderTasks.DeferredStatic do
            let batchPhase =
                match renderTasks.DeferredStatic.Count with
                | 1 -> SingletonPhase
                | count -> if i = 0 then StartingPhase elif i = dec count then StoppingPhase else ResumingPhase
            GlRenderer3d.renderPhysicallyBasedDeferredSurfaces
                batchPhase viewArray geometryProjectionArray geometryViewProjectionArray [||] eyeCenter entry.Value
                renderer.LightingConfig.LightShadowSamples renderer.LightingConfig.LightShadowBias renderer.LightingConfig.LightShadowSampleScalar renderer.LightingConfig.LightShadowExponent renderer.LightingConfig.LightShadowDensity
                entry.Key renderer.PhysicallyBasedShaders.DeferredStaticShader renderer.PhysicallyBasedStaticVao OpenGL.PhysicallyBased.StaticVertexSize renderer
            OpenGL.Hl.Assert ()
            i <- inc i

        // render static surface pre-batches deferred
        for entry in renderTasks.DeferredStaticPreBatches do
            let struct (surface, preBatch) = entry.Value
            GlRenderer3d.renderPhysicallyBasedDeferredSurfacePreBatch
                frustumInterior frustumExterior frustumImposter lightBox renderPass
                viewArray geometryProjectionArray geometryViewProjectionArray [||] eyeCenter preBatch
                renderer.LightingConfig.LightShadowSamples renderer.LightingConfig.LightShadowBias renderer.LightingConfig.LightShadowSampleScalar renderer.LightingConfig.LightShadowExponent renderer.LightingConfig.LightShadowDensity
                surface renderer.PhysicallyBasedShaders.DeferredStaticShader renderer.PhysicallyBasedStaticVao OpenGL.PhysicallyBased.StaticVertexSize renderer
            OpenGL.Hl.Assert ()

        // render static surfaces clipped deferred
        let mutable i = 0
        for entry in renderTasks.DeferredStaticClipped do
            let batchPhase =
                match renderTasks.DeferredStaticClipped.Count with
                | 1 -> SingletonPhase
                | count -> if i = 0 then StartingPhase elif i = dec count then StoppingPhase else ResumingPhase
            GlRenderer3d.renderPhysicallyBasedDeferredSurfaces
                batchPhase viewArray geometryProjectionArray geometryViewProjectionArray [||] eyeCenter entry.Value
                renderer.LightingConfig.LightShadowSamples renderer.LightingConfig.LightShadowBias renderer.LightingConfig.LightShadowSampleScalar renderer.LightingConfig.LightShadowExponent renderer.LightingConfig.LightShadowDensity
                entry.Key renderer.PhysicallyBasedShaders.DeferredStaticClippedShader renderer.PhysicallyBasedStaticVao OpenGL.PhysicallyBased.StaticVertexSize renderer
            OpenGL.Hl.Assert ()
            i <- inc i

        // render static surface pre-batches clipped deferred
        for entry in renderTasks.DeferredStaticClippedPreBatches do
            let struct (surface, preBatch) = entry.Value
            GlRenderer3d.renderPhysicallyBasedDeferredSurfacePreBatch
                frustumInterior frustumExterior frustumImposter lightBox renderPass
                viewArray geometryProjectionArray geometryViewProjectionArray [||] eyeCenter preBatch
                renderer.LightingConfig.LightShadowSamples renderer.LightingConfig.LightShadowBias renderer.LightingConfig.LightShadowSampleScalar renderer.LightingConfig.LightShadowExponent renderer.LightingConfig.LightShadowDensity
                surface renderer.PhysicallyBasedShaders.DeferredStaticClippedShader renderer.PhysicallyBasedStaticVao OpenGL.PhysicallyBased.StaticVertexSize renderer
            OpenGL.Hl.Assert ()

        // render animated surfaces deferred
        for entry in renderTasks.DeferredAnimated do
            let surfaceKey = entry.Key
            let parameters = entry.Value
            let bonesArrays = Array.zeroCreate surfaceKey.BoneTransforms.Length
            for i in 0 .. dec surfaceKey.BoneTransforms.Length do
                let boneArray = surfaceKey.BoneTransforms.[i].ToArray ()
                bonesArrays.[i] <- boneArray
            GlRenderer3d.renderPhysicallyBasedDeferredSurfaces
                SingletonPhase viewArray geometryProjectionArray geometryViewProjectionArray bonesArrays eyeCenter parameters
                renderer.LightingConfig.LightShadowSamples renderer.LightingConfig.LightShadowBias renderer.LightingConfig.LightShadowSampleScalar renderer.LightingConfig.LightShadowExponent renderer.LightingConfig.LightShadowDensity
                surfaceKey.AnimatedSurface renderer.PhysicallyBasedShaders.DeferredAnimatedShader renderer.PhysicallyBasedAnimatedVao OpenGL.PhysicallyBased.AnimatedVertexSize renderer
            OpenGL.Hl.Assert ()

        // render terrains deferred
        for struct (descriptor, _, geometry) in renderTasks.DeferredTerrains do
            GlRenderer3d.renderPhysicallyBasedTerrain
                viewArray geometryProjectionArray geometryViewProjectionArray eyeCenter
                renderer.LightingConfig.LightShadowSamples renderer.LightingConfig.LightShadowBias renderer.LightingConfig.LightShadowSampleScalar renderer.LightingConfig.LightShadowSampleScalar renderer.LightingConfig.LightShadowDensity
                descriptor geometry renderer.PhysicallyBasedShaders.DeferredTerrainShader renderer.PhysicallyBasedTerrainVao renderer

        // run light mapping pass
        let lightMappingTexture =

            // but only when desired
            if renderer.RendererConfig.LightMappingEnabled then

                // setup light mapping buffer and viewport
                let (lightMappingTexture, lightMappingRenderbuffer, lightMappingFramebuffer) = renderer.PhysicallyBasedBuffers.LightMappingBuffers
                OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, lightMappingRenderbuffer)
                OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, lightMappingFramebuffer)
                OpenGL.Gl.ClearColor (Constants.Render.ViewportClearColor.R, Constants.Render.ViewportClearColor.G, Constants.Render.ViewportClearColor.B, Constants.Render.ViewportClearColor.A)
                OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
                OpenGL.Gl.Viewport (0, 0, geometryResolution.X, geometryResolution.Y)
                OpenGL.Hl.Assert ()

                // deferred render light mapping quad
                OpenGL.PhysicallyBased.DrawPhysicallyBasedDeferredLightMappingSurface
                    (eyeCenter, viewInverseArray, rasterProjectionInverseArray,
                     depthTexture, normalPlusTexture,
                     lightMapOrigins, lightMapMins, lightMapSizes, min lightMapEnvironmentFilterMaps.Length renderTasks.LightMaps.Count,
                     renderer.PhysicallyBasedQuad, renderer.PhysicallyBasedShaders.DeferredLightMappingShader, renderer.PhysicallyBasedStaticVao)
                OpenGL.Hl.Assert ()
                lightMappingTexture

            // just use black texture
            else renderer.BlackTexture

        // setup ambient buffer and viewport
        let (ambientTexture, ambientRenderbuffer, ambientFramebuffer) = renderer.PhysicallyBasedBuffers.AmbientBuffers
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, ambientRenderbuffer)
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, ambientFramebuffer)
        OpenGL.Gl.ClearColor (1.0f, 1.0f, 1.0f, 1.0f) // NOTE: white ambient value.
        OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
        OpenGL.Gl.Viewport (0, 0, geometryResolution.X, geometryResolution.Y)
        OpenGL.Hl.Assert ()

        // deferred render ambient quad
        OpenGL.PhysicallyBased.DrawPhysicallyBasedDeferredAmbientSurface
            (eyeCenter, viewInverseArray, rasterProjectionInverseArray,
             depthTexture, lightMappingTexture,
             lightAmbientColor, lightAmbientBrightness, lightMapAmbientColors, lightMapAmbientBrightnesses,
             renderer.PhysicallyBasedQuad, renderer.PhysicallyBasedShaders.DeferredAmbientShader, renderer.PhysicallyBasedStaticVao)
        OpenGL.Hl.Assert ()

        // setup irradiance buffer and viewport
        let (irradianceTexture, irradianceRenderbuffer, irradianceFramebuffer) = renderer.PhysicallyBasedBuffers.IrradianceBuffers
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, irradianceRenderbuffer)
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, irradianceFramebuffer)
        OpenGL.Gl.ClearColor (1.0f, 1.0f, 1.0f, 1.0f) // NOTE: white irradiance value.
        OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
        OpenGL.Gl.Viewport (0, 0, geometryResolution.X, geometryResolution.Y)
        OpenGL.Hl.Assert ()

        // deferred render irradiance quad
        OpenGL.PhysicallyBased.DrawPhysicallyBasedDeferredIrradianceSurface
            (eyeCenter, viewInverseArray, rasterProjectionInverseArray,
             depthTexture, normalPlusTexture, lightMappingTexture,
             lightMapFallback.IrradianceMap, lightMapIrradianceMaps,
             renderer.PhysicallyBasedQuad, renderer.PhysicallyBasedShaders.DeferredIrradianceShader, renderer.PhysicallyBasedStaticVao)
        OpenGL.Hl.Assert ()

        // setup environment filter buffer and viewport
        let (environmentFilterTexture, environmentFilterRenderbuffer, environmentFilterFramebuffer) = renderer.PhysicallyBasedBuffers.EnvironmentFilterBuffers
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, environmentFilterRenderbuffer)
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, environmentFilterFramebuffer)
        OpenGL.Gl.ClearColor (1.0f, 1.0f, 1.0f, 1.0f) // NOTE: white environment filter value.
        OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
        OpenGL.Gl.Viewport (0, 0, geometryResolution.X, geometryResolution.Y)
        OpenGL.Hl.Assert ()

        // deferred render environment filter quad
        OpenGL.PhysicallyBased.DrawPhysicallyBasedDeferredEnvironmentFilterSurface
            (eyeCenter, viewInverseArray, rasterProjectionInverseArray,
             depthTexture, materialTexture, normalPlusTexture, lightMappingTexture,
             lightMapFallback.EnvironmentFilterMap, lightMapEnvironmentFilterMaps, lightMapOrigins, lightMapMins, lightMapSizes,
             renderer.PhysicallyBasedQuad, renderer.PhysicallyBasedShaders.DeferredEnvironmentFilterShader, renderer.PhysicallyBasedStaticVao)
        OpenGL.Hl.Assert ()

        // run ssao pass
        let ssaoTextureFiltered =

            // but only when desired
            if renderer.RendererConfig.SsaoEnabled && renderer.LightingConfig.SsaoEnabled then

                // setup unfiltered ssao buffer and viewport
                let ssaoResolution = renderer.GeometryViewport.SsaoResolution
                let (ssaoTextureUnfiltered, ssaoRenderbuffer, ssaoFramebuffer) = renderer.PhysicallyBasedBuffers.SsaoBuffersUnfiltered
                OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, ssaoRenderbuffer)
                OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, ssaoFramebuffer)
                OpenGL.Gl.ClearColor (1.0f, 1.0f, 1.0f, 1.0f) // NOTE: white ssao value.
                OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
                OpenGL.Gl.Viewport (0, 0, ssaoResolution.X, ssaoResolution.Y)
                OpenGL.Hl.Assert ()

                // deferred render ssao quad
                OpenGL.PhysicallyBased.DrawPhysicallyBasedDeferredSsaoSurface
                    (eyeCenter, viewArray, viewInverseArray, geometryProjectionArray, geometryProjectionInverseArray, geometryViewProjectionArray,
                     depthTexture, normalPlusTexture, [|ssaoResolution.X; ssaoResolution.Y|],
                     renderer.LightingConfig.SsaoIntensity, renderer.LightingConfig.SsaoBias, renderer.LightingConfig.SsaoRadius, renderer.LightingConfig.SsaoDistanceMax, renderer.RendererConfig.SsaoSampleCount,
                     renderer.PhysicallyBasedQuad, renderer.PhysicallyBasedShaders.DeferredSsaoShader, renderer.PhysicallyBasedStaticVao)
                OpenGL.Hl.Assert ()

                // setup filtered ssao buffer and viewport
                let (ssaoTextureFiltered, ssaoFilteredRenderbuffer, ssaoFilteredFramebuffer) = renderer.PhysicallyBasedBuffers.SsaoBuffersFiltered
                OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, ssaoFilteredRenderbuffer)
                OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, ssaoFilteredFramebuffer)
                OpenGL.Gl.ClearColor (Constants.Render.ViewportClearColor.R, Constants.Render.ViewportClearColor.G, Constants.Render.ViewportClearColor.B, Constants.Render.ViewportClearColor.A)
                OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
                OpenGL.Gl.Viewport (0, 0, ssaoResolution.X, ssaoResolution.Y)
                OpenGL.Hl.Assert ()

                // deferred render filtered ssao quad
                OpenGL.PhysicallyBased.DrawFilterBoxSurface (ssaoTextureUnfiltered, renderer.PhysicallyBasedQuad, renderer.FilterShaders.FilterBox1dShader, renderer.PhysicallyBasedStaticVao)
                OpenGL.Hl.Assert ()
                ssaoTextureFiltered

            // just use white texture
            else renderer.WhiteTexture

        // setup lighting buffers and viewport
        let (lightAccumTexture, fogAccumTexture, lightingRenderbuffer, lightingFramebuffer) = renderer.PhysicallyBasedBuffers.LightingBuffers
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, lightingRenderbuffer)
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, lightingFramebuffer)
        OpenGL.Gl.ClearColor (Constants.Render.ViewportClearColor.R, Constants.Render.ViewportClearColor.G, Constants.Render.ViewportClearColor.B, Constants.Render.ViewportClearColor.A)
        OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
        OpenGL.Gl.Viewport (0, 0, geometryResolution.X, geometryResolution.Y)
        OpenGL.Hl.Assert ()

        // deferred render quad to lighting buffers
        let sssEnabled = if renderer.RendererConfig.SssEnabled && renderer.LightingConfig.SssEnabled then 1 else 0
        let ssvfEnabled = if renderer.RendererConfig.SsvfEnabled && renderer.LightingConfig.SsvfEnabled then 1 else 0
        OpenGL.PhysicallyBased.DrawPhysicallyBasedDeferredLightingSurface
            (eyeCenter, viewArray, viewInverseArray, geometryProjectionArray, geometryProjectionInverseArray, renderer.LightingConfig.LightCutoffMargin,
             renderer.LightingConfig.LightShadowSamples, renderer.LightingConfig.LightShadowBias, renderer.LightingConfig.LightShadowSampleScalar, renderer.LightingConfig.LightShadowExponent, renderer.LightingConfig.LightShadowDensity,
             sssEnabled, ssvfEnabled, renderer.LightingConfig.SsvfSteps, renderer.LightingConfig.SsvfAsymmetry, renderer.LightingConfig.SsvfIntensity,
             depthTexture, albedoTexture, materialTexture, normalPlusTexture, subdermalPlusTexture, scatterPlusTexture, shadowTextures, shadowMaps, shadowCascades,
             lightOrigins, lightDirections, lightColors, lightBrightnesses, lightAttenuationLinears, lightAttenuationQuadratics, lightCutoffs, lightTypes, lightConeInners, lightConeOuters, lightDesireFogs, lightShadowIndices, min lightIds.Length renderTasks.Lights.Count, shadowNear, shadowMatrices,
             renderer.PhysicallyBasedQuad, renderer.PhysicallyBasedShaders.DeferredLightingShader, renderer.PhysicallyBasedStaticVao)
        OpenGL.Hl.Assert ()

        // setup coloring buffers and viewport
        let (colorTexture, depthTexture2, coloringRenderbuffer, coloringFramebuffer) = renderer.PhysicallyBasedBuffers.ColoringBuffers
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, coloringRenderbuffer)
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, coloringFramebuffer)
        OpenGL.Gl.ClearColor (Constants.Render.ViewportClearColor.R, Constants.Render.ViewportClearColor.G, Constants.Render.ViewportClearColor.B, Constants.Render.ViewportClearColor.A)
        OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
        OpenGL.Gl.Viewport (0, 0, geometryResolution.X, geometryResolution.Y)
        OpenGL.Hl.Assert ()

        // deferred render quad to coloring buffers
        let ssrlEnabled = if renderer.RendererConfig.SsrlEnabled && renderer.LightingConfig.SsrlEnabled then 1 else 0
        OpenGL.PhysicallyBased.DrawPhysicallyBasedDeferredColoringSurface
            (eyeCenter, viewArray, viewInverseArray, geometryProjectionArray, geometryProjectionInverseArray, renderer.LightingConfig.LightAmbientBoostCutoff, renderer.LightingConfig.LightAmbientBoostScalar,
             ssrlEnabled, renderer.LightingConfig.SsrlIntensity, renderer.LightingConfig.SsrlDetail, renderer.LightingConfig.SsrlRefinementsMax, renderer.LightingConfig.SsrlRayThickness, renderer.LightingConfig.SsrlTowardEyeCutoff,
             renderer.LightingConfig.SsrlDepthCutoff, renderer.LightingConfig.SsrlDepthCutoffMargin, renderer.LightingConfig.SsrlDistanceCutoff, renderer.LightingConfig.SsrlDistanceCutoffMargin, renderer.LightingConfig.SsrlRoughnessCutoff, renderer.LightingConfig.SsrlRoughnessCutoffMargin,
             renderer.LightingConfig.SsrlSlopeCutoff, renderer.LightingConfig.SsrlSlopeCutoffMargin, renderer.LightingConfig.SsrlEdgeHorizontalMargin, renderer.LightingConfig.SsrlEdgeVerticalMargin,
             depthTexture, albedoTexture, materialTexture, normalPlusTexture, lightAccumTexture, renderer.BrdfTexture, ambientTexture, irradianceTexture, environmentFilterTexture, ssaoTextureFiltered,
             renderer.PhysicallyBasedQuad, renderer.PhysicallyBasedShaders.DeferredColoringShader, renderer.PhysicallyBasedStaticVao)
        OpenGL.Hl.Assert ()

        // run fog accum blur pass
        let fogAccumBlurTexture =

            // but only when desired
            if renderer.LightingConfig.SsvfEnabled then

                // setup fog accum down-sample buffers and viewport
                let (fogAccumDownSampleTexture, depthDownSampleTexture, fogAccumDownSampleRenderbuffer, fogAccumDownSampleFramebuffer) = renderer.PhysicallyBasedBuffers.FogAccumDownSampleBuffers
                OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, fogAccumDownSampleRenderbuffer)
                OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, fogAccumDownSampleFramebuffer)
                OpenGL.Gl.ClearColor (Constants.Render.ViewportClearColor.R, Constants.Render.ViewportClearColor.G, Constants.Render.ViewportClearColor.B, Constants.Render.ViewportClearColor.A)
                OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
                OpenGL.Gl.Viewport (0, 0, geometryResolution.X / 2, geometryResolution.Y / 2)
                OpenGL.Hl.Assert ()

                // deferred render fog accum quad to down-sample buffers
                // NOTE: the depthTexture gets rendered redundantly here, but we're ignoring that inefficiency for now.
                // TODO: P1: let's see if we can remember why we're rendering depth here and potentially avoid it for
                // efficiency.
                OpenGL.PhysicallyBased.DrawFilterBilateralDownSampleSurface (fogAccumTexture, depthTexture2, renderer.PhysicallyBasedQuad, renderer.FilterShaders.FilterBilateralDownSample4dShader, renderer.PhysicallyBasedStaticVao)
                OpenGL.Hl.Assert ()

                // setup fog accum up-sample buffers and viewport
                let (fogAccumUpSampleTexture, fogAccumUpSampleRenderbuffer, fogAccumUpSampleFramebuffer) = renderer.PhysicallyBasedBuffers.FogAccumUpSampleBuffers
                OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, fogAccumUpSampleRenderbuffer)
                OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, fogAccumUpSampleFramebuffer)
                OpenGL.Gl.ClearColor (Constants.Render.ViewportClearColor.R, Constants.Render.ViewportClearColor.G, Constants.Render.ViewportClearColor.B, Constants.Render.ViewportClearColor.A)
                OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
                OpenGL.Gl.Viewport (0, 0, geometryResolution.X, geometryResolution.Y)
                OpenGL.Hl.Assert ()

                // deferred render fog accum quad to up-sample buffers
                OpenGL.PhysicallyBased.DrawFilterBilateralUpSampleSurface (fogAccumDownSampleTexture, depthDownSampleTexture, depthTexture2, renderer.PhysicallyBasedQuad, renderer.FilterShaders.FilterBilateralUpSample4dShader, renderer.PhysicallyBasedStaticVao)
                OpenGL.Hl.Assert ()
                fogAccumUpSampleTexture

            // just use black texture
            else renderer.BlackTexture

        // setup composition buffers and viewport
        let (compositionTexture, compositionRenderbuffer, compositionFramebuffer) = renderer.PhysicallyBasedBuffers.CompositionBuffers
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, compositionRenderbuffer)
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, compositionFramebuffer)
        OpenGL.Gl.ClearColor (Constants.Render.ViewportClearColor.R, Constants.Render.ViewportClearColor.G, Constants.Render.ViewportClearColor.B, Constants.Render.ViewportClearColor.A)
        OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
        OpenGL.Gl.Viewport (0, 0, geometryResolution.X, geometryResolution.Y)
        OpenGL.Hl.Assert ()

        // deferred render composition quad to composition buffers
        let fogEnabled = if renderer.LightingConfig.FogEnabled then 1 else 0
        let fogType = renderer.LightingConfig.FogType.Enumerate
        OpenGL.PhysicallyBased.DrawPhysicallyBasedDeferredCompositionSurface
            (eyeCenter, viewInverseArray, rasterProjectionInverseArray, fogEnabled, fogType, renderer.LightingConfig.FogStart, renderer.LightingConfig.FogFinish, renderer.LightingConfig.FogDensity, renderer.LightingConfig.FogColor,
             depthTexture, colorTexture, fogAccumBlurTexture, renderer.PhysicallyBasedQuad, renderer.PhysicallyBasedShaders.DeferredCompositionShader, renderer.PhysicallyBasedStaticVao)

        // copy depths from geometry framebuffer to composition framebuffer
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.ReadFramebuffer, geometryFramebuffer)
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.DrawFramebuffer, compositionFramebuffer)
        OpenGL.Gl.BlitFramebuffer
            (0, 0, geometryResolution.X, geometryResolution.Y,
             0, 0, geometryResolution.X, geometryResolution.Y,
             OpenGL.ClearBufferMask.DepthBufferBit,
             OpenGL.BlitFramebufferFilter.Nearest)
        OpenGL.Hl.Assert ()

        // attempt to render sky box to composition buffer
        match skyBoxOpt with
        | Some (cubeMapColor, cubeMapBrightness, cubeMap, _) ->
            OpenGL.SkyBox.DrawSkyBox (viewSkyBoxArray, rasterProjectionArray, rasterViewProjectionSkyBoxArray, cubeMapColor, cubeMapBrightness, cubeMap, renderer.CubeMapGeometry, renderer.SkyBoxShader, renderer.CubeMapVao)
            OpenGL.Hl.Assert ()
        | None -> ()

        // forward render surfaces to composition buffer
        let ssrrEnabled =
            if renderer.RendererConfig.SsrrEnabled && renderer.LightingConfig.SsrrEnabled then 1 else 0
        let forwardSsvfSteps =
            renderer.LightingConfig.SsvfSteps * 2 // HACK: need an increase in forward-rendered steps since they don't get a blur pass.
        let forwardShaderAndVaos =
            [(renderer.PhysicallyBasedShaders.ForwardStaticShader, renderer.PhysicallyBasedStaticVao)
             (renderer.PhysicallyBasedShaders.ForwardAnimatedShader, renderer.PhysicallyBasedAnimatedVao)]
        for (shader, vao) in forwardShaderAndVaos do
            GlRenderer3d.beginPhysicallyBasedForwardShader
                viewArray geometryProjectionArray geometryViewProjectionArray eyeCenter viewInverseArray rasterProjectionInverseArray renderer.LightingConfig.LightCutoffMargin lightAmbientColor lightAmbientBrightness renderer.LightingConfig.LightAmbientBoostCutoff renderer.LightingConfig.LightAmbientBoostScalar
                renderer.LightingConfig.LightShadowSamples renderer.LightingConfig.LightShadowBias renderer.LightingConfig.LightShadowSampleScalar renderer.LightingConfig.LightShadowExponent renderer.LightingConfig.LightShadowDensity
                fogEnabled fogType renderer.LightingConfig.FogStart renderer.LightingConfig.FogFinish renderer.LightingConfig.FogDensity renderer.LightingConfig.FogColor ssvfEnabled forwardSsvfSteps renderer.LightingConfig.SsvfAsymmetry renderer.LightingConfig.SsvfIntensity
                ssrrEnabled renderer.LightingConfig.SsrrIntensity renderer.LightingConfig.SsrrDetail renderer.LightingConfig.SsrrRefinementsMax renderer.LightingConfig.SsrrRayThickness renderer.LightingConfig.SsrrDistanceCutoff renderer.LightingConfig.SsrrDistanceCutoffMargin renderer.LightingConfig.SsrrEdgeHorizontalMargin renderer.LightingConfig.SsrrEdgeVerticalMargin
                depthTexture2 colorTexture renderer.BrdfTexture lightMapFallback.IrradianceMap lightMapFallback.EnvironmentFilterMap shadowNear shader vao
        for (model, _, presence, texCoordsOffset, properties, boneTransformsOpt, surface, depthTest) in renderTasks.ForwardSorted do
            let (lightMapOrigins, lightMapMins, lightMapSizes, lightMapAmbientColors, lightMapAmbientBrightnesses, lightMapIrradianceMaps, lightMapEnvironmentFilterMaps) =
                let surfaceBounds = surface.SurfaceBounds.Transform model
                SortableLightMap.sortLightMaps Constants.Render.LightMapsMaxForward model.Translation (Some surfaceBounds) lightMaps
            let (lightIds, lightOrigins, lightDirections, lightColors, lightBrightnesses, lightAttenuationLinears, lightAttenuationQuadratics, lightCutoffs, lightTypes, lightConeInners, lightConeOuters, lightDesireFogs) =
                SortableLight.sortLights Constants.Render.LightsMaxForward model.Translation renderTasks.Lights
            let lightShadowIndices =
                SortableLight.sortLightShadowIndices renderer.LightShadowIndices lightIds
            let (bonesArray, shader, vao, vertexSize) =
                match boneTransformsOpt with
                | ValueSome boneTransforms ->
                    let boneArrays = List ()
                    let bonesArrays = Array.zeroCreate boneTransforms.Length
                    for i in 0 .. dec boneTransforms.Length do
                        let boneArray = boneTransforms.[i].ToArray ()
                        boneArrays.Add boneArray
                        bonesArrays.[i] <- boneArray
                    (bonesArrays, renderer.PhysicallyBasedShaders.ForwardAnimatedShader, renderer.PhysicallyBasedAnimatedVao, OpenGL.PhysicallyBased.AnimatedVertexSize)
                | ValueNone -> ([||], renderer.PhysicallyBasedShaders.ForwardStaticShader, renderer.PhysicallyBasedStaticVao, OpenGL.PhysicallyBased.StaticVertexSize)
            GlRenderer3d.renderPhysicallyBasedForwardSurfaces
                bonesArray (SList.singleton (model, presence, texCoordsOffset, properties))
                lightMapIrradianceMaps lightMapEnvironmentFilterMaps shadowTextures shadowMaps shadowCascades lightMapOrigins lightMapMins lightMapSizes lightMapAmbientColors lightMapAmbientBrightnesses (min lightMapEnvironmentFilterMaps.Length renderTasks.LightMaps.Count)
                lightOrigins lightDirections lightColors lightBrightnesses lightAttenuationLinears lightAttenuationQuadratics lightCutoffs lightTypes lightConeInners lightConeOuters lightDesireFogs lightShadowIndices (min lightIds.Length renderTasks.Lights.Count) shadowMatrices
                surface depthTest true shader vao vertexSize renderer
            OpenGL.Hl.Assert ()
        for (shader, vao) in forwardShaderAndVaos do
            GlRenderer3d.endPhysicallyBasedForwardShader shader vao

        // apply bloom filter when desired
        if renderer.RendererConfig.BloomEnabled && renderer.LightingConfig.BloomEnabled then

            // setup bloom extract buffers and viewport
            let (bloomExtractTexture, bloomExtractRenderbuffer, bloomExtractFramebuffer) = renderer.PhysicallyBasedBuffers.BloomExtractBuffers
            OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, bloomExtractRenderbuffer)
            OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, bloomExtractFramebuffer)
            OpenGL.Gl.ClearColor (Constants.Render.ViewportClearColor.R, Constants.Render.ViewportClearColor.G, Constants.Render.ViewportClearColor.B, Constants.Render.ViewportClearColor.A)
            OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
            OpenGL.Gl.Viewport (0, 0, geometryResolution.X, geometryResolution.Y)
            OpenGL.Hl.Assert ()

            // render bloom extract buffers
            OpenGL.PhysicallyBased.DrawFilterBloomExtractSurface (renderer.LightingConfig.BloomThreshold, compositionTexture, renderer.PhysicallyBasedQuad, renderer.FilterShaders.FilterBloomExtractShader, renderer.PhysicallyBasedStaticVao)
            OpenGL.Hl.Assert ()

            // setup bloom sample buffers and viewport (no clearing or viewport config needed)
            let (bloomSampleTextures, bloomSampleRenderbuffer, bloomSampleFramebuffer) = renderer.PhysicallyBasedBuffers.BloomSampleBuffers
            OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, bloomSampleRenderbuffer)
            OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, bloomSampleFramebuffer)
            OpenGL.Hl.Assert ()

            // down-sample bloom buffers
            OpenGL.PhysicallyBased.DrawBloomDownSamplesSurface
                (geometryResolution.X, geometryResolution.Y, Constants.Render.BloomSampleLevels, renderer.LightingConfig.BloomKarisAverageEnabled, bloomExtractTexture, bloomSampleTextures,
                 renderer.PhysicallyBasedQuad, renderer.FilterShaders.FilterBloomDownSampleShader, renderer.PhysicallyBasedStaticVao)
            OpenGL.Hl.Assert ()

            // up-sample bloom buffers
            OpenGL.PhysicallyBased.DrawBloomUpSamplesSurface
                (geometryResolution.X, geometryResolution.Y, Constants.Render.BloomSampleLevels, renderer.LightingConfig.BloomFilterRadius, bloomSampleTextures,
                 renderer.PhysicallyBasedQuad, renderer.FilterShaders.FilterBloomUpSampleShader, renderer.PhysicallyBasedStaticVao)
            OpenGL.Hl.Assert ()

            // setup bloom apply buffer and viewport
            let (_, bloomApplyRenderbuffer, bloomApplyFramebuffer) = renderer.PhysicallyBasedBuffers.BloomApplyBuffers
            OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, bloomApplyRenderbuffer)
            OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, bloomApplyFramebuffer)
            OpenGL.Gl.ClearColor (Constants.Render.ViewportClearColor.R, Constants.Render.ViewportClearColor.G, Constants.Render.ViewportClearColor.B, Constants.Render.ViewportClearColor.A)
            OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
            OpenGL.Gl.Viewport (0, 0, geometryResolution.X, geometryResolution.Y)
            OpenGL.Hl.Assert ()

            // render bloom apply pass
            OpenGL.PhysicallyBased.DrawBloomApplySurface
                (renderer.LightingConfig.BloomStrength, bloomSampleTextures.[0], compositionTexture,
                 renderer.PhysicallyBasedQuad, renderer.FilterShaders.FilterBloomApplyShader, renderer.PhysicallyBasedStaticVao)
            OpenGL.Hl.Assert ()

            // blit bloom apply buffer to composition buffer
            OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.ReadFramebuffer, bloomApplyFramebuffer)
            OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.DrawFramebuffer, compositionFramebuffer)
            OpenGL.Gl.BlitFramebuffer
                (0, 0, geometryResolution.X, geometryResolution.Y,
                 0, 0, geometryResolution.X, geometryResolution.Y,
                 OpenGL.ClearBufferMask.ColorBufferBit,
                 OpenGL.BlitFramebufferFilter.Nearest)
            OpenGL.Hl.Assert ()

        // apply fxaa filter when enabled
        if renderer.RendererConfig.FxaaEnabled then

            // setup filter 0 buffer and viewport
            let (filter0Texture, filter0Renderbuffer, filter0Framebuffer) = renderer.PhysicallyBasedBuffers.Filter0Buffers
            OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, filter0Renderbuffer)
            OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, filter0Framebuffer)
            OpenGL.Gl.Viewport (0, 0, geometryResolution.X, geometryResolution.Y)
            OpenGL.Hl.Assert ()

            // blit composition buffer to filter 0 buffer
            OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.ReadFramebuffer, compositionFramebuffer)
            OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.DrawFramebuffer, filter0Framebuffer)
            OpenGL.Gl.BlitFramebuffer
                (0, 0, geometryResolution.X, geometryResolution.Y,
                 0, 0, geometryResolution.X, geometryResolution.Y,
                 OpenGL.ClearBufferMask.ColorBufferBit,
                 OpenGL.BlitFramebufferFilter.Nearest)
            OpenGL.Hl.Assert ()

            // setup filter 1 buffer and viewport
            let (filter1Texture, filter1Renderbuffer, filter1Framebuffer) = renderer.PhysicallyBasedBuffers.Filter1Buffers
            OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, filter1Renderbuffer)
            OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, filter1Framebuffer)
            OpenGL.Gl.Viewport (0, 0, geometryResolution.X, geometryResolution.Y)
            OpenGL.Hl.Assert ()

            // render filter quad via fxaa
            OpenGL.PhysicallyBased.DrawFilterFxaaSurface (filter0Texture, renderer.PhysicallyBasedQuad, renderer.FilterShaders.FilterFxaaShader, renderer.PhysicallyBasedStaticVao)
            OpenGL.Hl.Assert ()

            // setup filter 2 buffer and viewport
            let (filter2Texture, filter2Renderbuffer, filter2Framebuffer) = renderer.PhysicallyBasedBuffers.Filter2Buffers
            OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, filter2Renderbuffer)
            OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, filter2Framebuffer)
            OpenGL.Gl.Viewport (0, 0, geometryResolution.X, geometryResolution.Y)
            OpenGL.Hl.Assert ()

            // render filter quad via gaussian x
            OpenGL.PhysicallyBased.DrawFilterGaussianSurface (v2 (1.0f / single geometryResolution.X) 0.0f, filter1Texture, renderer.PhysicallyBasedQuad, renderer.FilterShaders.FilterGaussian4dShader, renderer.PhysicallyBasedStaticVao)
            OpenGL.Hl.Assert ()

            // setup composition buffer and viewport
            OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, compositionRenderbuffer)
            OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, compositionFramebuffer)
            OpenGL.Gl.ClearColor (Constants.Render.ViewportClearColor.R, Constants.Render.ViewportClearColor.G, Constants.Render.ViewportClearColor.B, Constants.Render.ViewportClearColor.A)
            OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
            OpenGL.Gl.Viewport (0, 0, geometryResolution.X, geometryResolution.Y)
            OpenGL.Hl.Assert ()

            // render filter quad via gaussian y
            OpenGL.PhysicallyBased.DrawFilterGaussianSurface (v2 0.0f (1.0f / single geometryResolution.Y), filter2Texture, renderer.PhysicallyBasedQuad, renderer.FilterShaders.FilterGaussian4dShader, renderer.PhysicallyBasedStaticVao)
            OpenGL.Hl.Assert ()

        // setup presentation buffer and viewport
        let (_, presentationRenderbuffer, presentationFramebuffer) = renderer.PhysicallyBasedBuffers.PresentationBuffers
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, presentationRenderbuffer)
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, presentationFramebuffer)
        OpenGL.Gl.ClearColor (Constants.Render.ViewportClearColor.R, Constants.Render.ViewportClearColor.G, Constants.Render.ViewportClearColor.B, Constants.Render.ViewportClearColor.A)
        OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
        OpenGL.Gl.Viewport (0, 0, geometryResolution.X, geometryResolution.Y)
        OpenGL.Hl.Assert ()

        // render presentation quad to presentation buffers
        OpenGL.PhysicallyBased.DrawFilterPresentationSurface
            (renderer.LightingConfig.LightExposure, renderer.LightingConfig.ToneMapType.Enumerate, renderer.LightingConfig.ToneMapSlope, renderer.LightingConfig.ToneMapOffset,
             renderer.LightingConfig.ToneMapPower, renderer.LightingConfig.ToneMapSaturation, renderer.LightingConfig.ToneMapWhitePoint, compositionTexture,
             renderer.PhysicallyBasedQuad, renderer.FilterShaders.FilterPresentationShader, renderer.PhysicallyBasedStaticVao)
        OpenGL.Hl.Assert ()

        // blit presentation buffer to raster buffer
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.ReadFramebuffer, presentationFramebuffer)
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.DrawFramebuffer, framebuffer)
        OpenGL.Gl.BlitFramebuffer
            (0, 0, geometryResolution.X, geometryResolution.Y,
             rasterBounds.Min.X, rasterBounds.Min.Y, rasterBounds.Max.X, rasterBounds.Max.Y,
             OpenGL.ClearBufferMask.ColorBufferBit,
             OpenGL.BlitFramebufferFilter.Nearest)
        OpenGL.Hl.Assert ()

    /// Render 3d surfaces.
    static member render
        frustumInterior
        frustumExterior
        frustumImposter
        lightBox
        eyeCenter
        eyeRotation
        eyeFieldOfView
        geometryViewport
        rasterViewport
        framebuffer
        (renderMessages : _ List)
        renderer =

        // updates viewports, recreating buffers as needed
        if renderer.GeometryViewport <> geometryViewport then
            GlRenderer3d.invalidateCaches renderer
            GlRenderer3d.clearRenderPasses renderer // force shadows to rerender
            OpenGL.PhysicallyBased.DestroyPhysicallyBasedBuffers renderer.PhysicallyBasedBuffers
            renderer.PhysicallyBasedBuffers <- OpenGL.PhysicallyBased.CreatePhysicallyBasedBuffers geometryViewport
            renderer.GeometryViewport <- geometryViewport
        renderer.RasterViewport <- rasterViewport

        // categorize messages
        let userDefinedStaticModelsToDestroy =
            GlRenderer3d.categorize frustumInterior frustumExterior frustumImposter lightBox eyeCenter eyeRotation renderMessages renderer

        // light map pre-passes
        for (renderPass, renderTasks) in renderer.RenderPasses.Pairs do

            // fallback light map pre-pass
            match GlRenderer3d.getLastSkyBoxOpt renderPass renderer |> __c with
            | Some (_, _, cubeMap, irradianceAndEnvironmentMapsOptRef : (OpenGL.Texture.Texture * OpenGL.Texture.Texture) option ref) ->

                // render fallback irradiance and env filter maps
                if Option.isNone irradianceAndEnvironmentMapsOptRef.Value then

                    // render fallback irradiance map
                    let irradianceMap =
                        OpenGL.LightMap.CreateIrradianceMap
                            (Constants.Render.IrradianceMapResolution,
                                OpenGL.CubeMap.CubeMapSurface.make cubeMap renderer.CubeMapGeometry,
                                renderer.IrradianceShader,
                                renderer.CubeMapVao,
                                renderer.IrradianceMapRenderbuffer,
                                renderer.IrradianceMapFramebuffer)

                    // render fallback env filter map
                    let environmentFilterMap =
                        OpenGL.LightMap.CreateEnvironmentFilterMap
                            (Constants.Render.EnvironmentFilterResolution,
                                OpenGL.CubeMap.CubeMapSurface.make cubeMap renderer.CubeMapGeometry,
                                renderer.EnvironmentFilterShader,
                                renderer.CubeMapVao,
                                renderer.EnvironmentFilterRenderbuffer,
                                renderer.EnvironmentFilterFramebuffer)

                    // add to cache and create light map
                    irradianceAndEnvironmentMapsOptRef.Value <- Some (irradianceMap, environmentFilterMap)

                // nothing to do
                | None -> ()

            // render light map
            match renderPass with
            | LightMapPass (lightProbeId, _) ->
                if renderTasks.LightMapRenders.Contains lightProbeId then

                    // destroy any existing light map
                    match renderer.LightMaps.TryGetValue lightProbeId with
                    | (true, lightMap) ->
                        OpenGL.LightMap.DestroyLightMap lightMap
                        renderer.LightMaps.Remove lightProbeId |> ignore<bool>
                    | (false, _) -> ()

                    // create new light map
                    match renderTasks.LightProbes.TryGetValue lightProbeId with
                    | (true, struct (lightProbeEnabled, lightProbeOrigin, lightProbeAmbientColor, lightProbeAmbientBrightness, lightProbeBounds)) ->

                        // create reflection map
                        let reflectionMap =
                            OpenGL.LightMap.CreateReflectionMap
                                (GlRenderer3d.renderGeometry frustumInterior frustumExterior frustumImposter lightBox renderPass (GlRenderer3d.getRenderTasks renderPass renderer) renderer,
                                 Constants.Render.ReflectionMapResolution,
                                 lightProbeOrigin,
                                 lightProbeAmbientColor,
                                 lightProbeAmbientBrightness,
                                 renderer.ReflectionRenderbuffer,
                                 renderer.ReflectionFramebuffer)

                        // create irradiance map
                        let irradianceMap =
                            OpenGL.LightMap.CreateIrradianceMap
                                (Constants.Render.IrradianceMapResolution,
                                 OpenGL.CubeMap.CubeMapSurface.make reflectionMap renderer.CubeMapGeometry,
                                 renderer.IrradianceShader,
                                 renderer.CubeMapVao,
                                 renderer.IrradianceMapRenderbuffer,
                                 renderer.IrradianceMapFramebuffer)

                        // create env filter map
                        let environmentFilterMap =
                            OpenGL.LightMap.CreateEnvironmentFilterMap
                                (Constants.Render.EnvironmentFilterResolution,
                                 OpenGL.CubeMap.CubeMapSurface.make reflectionMap renderer.CubeMapGeometry,
                                 renderer.EnvironmentFilterShader,
                                 renderer.CubeMapVao,
                                 renderer.EnvironmentFilterRenderbuffer,
                                 renderer.EnvironmentFilterFramebuffer)

                        // destroy reflection map
                        reflectionMap.Destroy ()

                        // create light map
                        let lightMap = OpenGL.LightMap.CreateLightMap lightProbeEnabled lightProbeOrigin lightProbeAmbientColor lightProbeAmbientBrightness lightProbeBounds irradianceMap environmentFilterMap

                        // add light map to cache
                        renderer.LightMaps.[lightProbeId] <- lightMap

                    | (false, _) -> ()
            | _ -> ()

        // sort spot and directional lights according to how they are utilized by shadows
        let normalPass = NormalPass
        let normalTasks = GlRenderer3d.getRenderTasks normalPass renderer
        let spotAndDirectionalLightsArray = SortableLight.sortShadowingSpotAndDirectionalLightsIntoArray Constants.Render.ShadowTexturesMax eyeCenter normalTasks.Lights

        // sort spot and directional lights so that shadows that have the possibility of cache reuse come to the front
        // NOTE: this approach has O(n^2) complexity altho perhaps it could be optimized.
        let spotAndDirectionalLightsArray =
            Array.sortBy (fun struct (id, _, _, _, _, _) ->
                renderer.RenderPasses2.Pairs
                |> Seq.choose (fun (renderPass, renderTasks) -> match renderPass with ShadowPass (id2, indexInfoOpt, _, _, _) when id2 = id && indexInfoOpt.IsNone -> renderTasks.ShadowBufferIndexOpt | _ -> None)
                |> Seq.headOrDefault Int32.MaxValue)
                spotAndDirectionalLightsArray

        // shadow texture pre-passes
        let mutable shadowTextureBufferIndex = 0
        for struct (lightId, lightOrigin, lightCutoff, lightConeOuter, lightDesireShadows, lightBounds) in spotAndDirectionalLightsArray do
            if renderer.RendererConfig.LightShadowingEnabled && lightDesireShadows = 1 && lightBox.Intersects lightBounds then
                for (renderPass, renderTasks) in renderer.RenderPasses.Pairs do
                    match renderPass with
                    | ShadowPass (shadowLightId, shadowIndexInfoOpt, shadowLightType, shadowRotation, shadowFrustum) when
                        lightId = shadowLightId && shadowIndexInfoOpt.IsNone && shadowTextureBufferIndex < Constants.Render.ShadowTexturesMax ->

                        // attempt to set up shadow texture drawing
                        let (shadowOrigin, shadowView, shadowProjection) =
                            match shadowLightType with
                            | SpotLight (_, _) ->
                                let shadowForward = shadowRotation.Down
                                let shadowUp = shadowForward.OrthonormalUp
                                let shadowView = Matrix4x4.CreateLookAt (lightOrigin, lightOrigin + shadowForward, shadowUp)
                                let shadowFov = max (min lightConeOuter Constants.Render.ShadowFovMax) 0.01f
                                let shadowCutoff = max lightCutoff (Constants.Render.NearPlaneDistanceInterior * 2.0f)
                                let shadowProjection = Matrix4x4.CreatePerspectiveFieldOfView (shadowFov, 1.0f, Constants.Render.NearPlaneDistanceInterior, shadowCutoff)
                                (lightOrigin, shadowView, shadowProjection)
                            | DirectionalLight ->
                                let shadowForward = shadowRotation.Down
                                let shadowUp = shadowForward.OrthonormalUp
                                let shadowView = Matrix4x4.CreateLookAt (lightOrigin, lightOrigin + shadowForward, shadowUp)
                                let shadowCutoff = lightCutoff
                                let shadowProjection = Matrix4x4.CreateOrthographic (shadowCutoff * 2.0f, shadowCutoff * 2.0f, -shadowCutoff, shadowCutoff)
                                (lightOrigin, shadowView, shadowProjection)
                            | PointLight | CascadedLight -> failwithumf ()
                            
                        // draw shadow texture when not cached
                        let shouldDraw =
                            renderer.RendererConfig.LightShadowingEnabled &&
                            match renderer.RenderPasses2.TryGetValue renderPass with
                            | (true, renderTasksCached) ->
                                if Option.contains shadowTextureBufferIndex renderTasksCached.ShadowBufferIndexOpt then
                                    let upToDate = RenderTasks.shadowUpToDate renderer.LightingConfigChanged renderer.RendererConfigChanged renderTasks renderTasksCached
                                    not upToDate
                                else true
                            | (_, _) -> true
                        if shouldDraw then

                            // draw shadow texture
                            let shadowViewProjection = shadowView * shadowProjection
                            let shadowResolution = renderer.GeometryViewport.ShadowTextureResolution
                            let (shadowTexture, shadowRenderbuffer, shadowFramebuffer) = renderer.PhysicallyBasedBuffers.ShadowTextureBuffersArray.[shadowTextureBufferIndex]
                            GlRenderer3d.renderShadowTexture renderTasks renderer shadowOrigin shadowView shadowProjection shadowViewProjection shadowFrustum shadowLightType shadowResolution shadowRenderbuffer shadowFramebuffer

                            // filter shadows on the x (presuming that viewport already configured correctly)
                            let (shadowTexture2, shadowRenderbuffer2, shadowFramebuffer2) = renderer.PhysicallyBasedBuffers.ShadowTextureBuffers2Array.[shadowTextureBufferIndex]
                            OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, shadowRenderbuffer2)
                            OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, shadowFramebuffer2)
                            OpenGL.PhysicallyBased.DrawFilterGaussianSurface (v2 (1.0f / single shadowResolution.X) 0.0f, shadowTexture, renderer.PhysicallyBasedQuad, renderer.FilterShaders.FilterGaussian2dShader, renderer.PhysicallyBasedStaticVao)
                            OpenGL.Hl.Assert ()

                            // filter shadows on the y (presuming that viewport already configured correctly)
                            OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, shadowRenderbuffer)
                            OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, shadowFramebuffer)
                            OpenGL.PhysicallyBased.DrawFilterGaussianSurface (v2 0.0f (1.0f / single shadowResolution.Y), shadowTexture2, renderer.PhysicallyBasedQuad, renderer.FilterShaders.FilterGaussian2dShader, renderer.PhysicallyBasedStaticVao)
                            OpenGL.Hl.Assert ()

                        // remember the utilized index for the next frame
                        renderTasks.ShadowBufferIndexOpt <- Some shadowTextureBufferIndex

                        // update renderer values
                        renderer.ShadowMatrices.[shadowTextureBufferIndex] <- shadowView * shadowProjection
                        renderer.LightShadowIndices.[lightId] <- shadowTextureBufferIndex

                        // next shadow
                        shadowTextureBufferIndex <- inc shadowTextureBufferIndex

                    | _ -> ()

        // sort point lights according to how they are utilized by shadows
        let pointLightsArray = SortableLight.sortShadowingPointLightsIntoArray Constants.Render.ShadowMapsMax eyeCenter normalTasks.Lights

        // sort point lights so that shadows that have the possibility of cache reuse come to the front
        // NOTE: this approach has O(n^2) complexity altho perhaps it could be optimized.
        let pointLightsArray =
            Array.sortBy (fun struct (id, _, _, _, _, _) ->
                renderer.RenderPasses2.Pairs
                |> Seq.choose (fun (renderPass, renderTasks) -> match renderPass with ShadowPass (id2, indexInfoOpt, _, _, _) when id2 = id && indexInfoOpt.IsSome -> renderTasks.ShadowBufferIndexOpt | _ -> None)
                |> Seq.headOrDefault Int32.MaxValue)
                pointLightsArray

        // shadow map pre-passes
        let mutable shadowMapBufferIndex = 0
        for struct (lightId, lightOrigin, lightCutoff, _, lightDesireShadows, lightBounds) in pointLightsArray do
            if renderer.RendererConfig.LightShadowingEnabled && lightDesireShadows = 1 && lightBox.Intersects lightBounds then
                for (renderPass, renderTasks) in renderer.RenderPasses.Pairs do
                    match renderPass with
                    | ShadowPass (shadowLightId, shadowIndexInfoOpt, shadowLightType, _, shadowFrustum) when
                        lightId = shadowLightId && shadowIndexInfoOpt.IsSome && shadowMapBufferIndex < Constants.Render.ShadowMapsMax ->
                        match shadowLightType with
                        | PointLight ->

                            // destructure shadow index info
                            let (shadowFace, shadowView, shadowProjection) = shadowIndexInfoOpt.Value
                            let shadowViewProjection = shadowView * shadowProjection

                            // draw shadow map when not cached
                            // NOTE: it's a tiny bit inefficient that we set up and tear down the same shadow map
                            // once per face render here, but probably nothing worth caring about.
                            let shouldDraw =
                                match renderer.RenderPasses2.TryGetValue renderPass with
                                | (true, renderTasksCached) ->
                                    if Option.contains (shadowMapBufferIndex + Constants.Render.ShadowTexturesMax) renderTasksCached.ShadowBufferIndexOpt then
                                        let upToDate = RenderTasks.shadowUpToDate renderer.LightingConfigChanged renderer.RendererConfigChanged renderTasks renderTasksCached
                                        not upToDate
                                    else true
                                | (_, _) -> true
                            if shouldDraw then
                                let shadowResolution = renderer.GeometryViewport.ShadowMapResolution
                                let (shadowTexture, shadowRenderbuffer, shadowFramebuffer) = renderer.PhysicallyBasedBuffers.ShadowMapBuffersArray.[shadowMapBufferIndex]
                                GlRenderer3d.renderShadowMapFace renderTasks renderer lightOrigin lightCutoff shadowFace shadowView shadowProjection shadowViewProjection shadowFrustum shadowResolution shadowTexture shadowRenderbuffer shadowFramebuffer

                            // remember the utilized index for the next frame
                            renderTasks.ShadowBufferIndexOpt <- Some (shadowMapBufferIndex + Constants.Render.ShadowTexturesMax)

                            // update renderer values or next shadow
                            // NOTE: this behavior completely DEPENDS on shadow index messages for a shadow map being
                            // received and processed in numerical order.
                            if shadowFace = 0 then
                                renderer.LightShadowIndices.[lightId] <- shadowMapBufferIndex + Constants.Render.ShadowTexturesMax
                            elif shadowFace = dec 6 then
                                shadowMapBufferIndex <- inc shadowMapBufferIndex

                        | SpotLight (_, _) | DirectionalLight | CascadedLight -> failwithumf ()
                    | _ -> ()

        // sort cascaded lights according to how they are utilized by shadows
        let cascadedLightsArray = SortableLight.sortShadowingCascadedLightsIntoArray Constants.Render.ShadowCascadesMax eyeCenter normalTasks.Lights

        // sort cascaded lights so that shadows that have the possibility of cache reuse come to the front
        // NOTE: this approach has O(n^2) complexity altho perhaps it could be optimized.
        let cascadedLightsArray =
            Array.sortBy (fun struct (id, _, _, _, _, _) ->
                renderer.RenderPasses2.Pairs
                |> Seq.choose (fun (renderPass, renderTasks) -> match renderPass with ShadowPass (id2, indexInfoOpt, _, _, _) when id2 = id && indexInfoOpt.IsSome -> renderTasks.ShadowBufferIndexOpt | _ -> None)
                |> Seq.headOrDefault Int32.MaxValue)
                cascadedLightsArray

        // shadow cascade pre-passes
        let mutable shadowCascadeBufferIndex = 0
        for struct (lightId, lightOrigin, lightCutoff, _, lightDesireShadows, lightBounds) in cascadedLightsArray do
            if renderer.RendererConfig.LightShadowingEnabled && lightDesireShadows = 1 && lightBox.Intersects lightBounds then
                for (renderPass, renderTasks) in renderer.RenderPasses.Pairs do
                    match renderPass with
                    | ShadowPass (shadowLightId, shadowIndexInfoOpt, shadowLightType, _, shadowFrustum) when
                        lightId = shadowLightId && shadowIndexInfoOpt.IsSome && shadowCascadeBufferIndex < Constants.Render.ShadowCascadesMax ->
                        match shadowLightType with
                        | CascadedLight ->

                            // destructure shadow index info
                            let (shadowCascadeLevel, shadowView, shadowProjection) = shadowIndexInfoOpt.Value
                            let shadowViewProjection = shadowView * shadowProjection
                            
                            // draw shadow cascade when not cached
                            // NOTE: it's a tiny bit inefficient that we set up and tear down the same shadow texture
                            // array once per face render here, but probably nothing worth caring about.
                            let shouldDraw =
                                match renderer.RenderPasses2.TryGetValue renderPass with
                                | (true, renderTasksCached) ->
                                    if Option.contains (shadowCascadeBufferIndex + Constants.Render.ShadowTexturesMax) renderTasksCached.ShadowBufferIndexOpt then
                                        let upToDate = RenderTasks.shadowUpToDate renderer.LightingConfigChanged renderer.RendererConfigChanged renderTasks renderTasksCached
                                        not upToDate
                                    else true
                                | (_, _) -> true
                            if shouldDraw then

                                // draw shadow cascade
                                let shadowResolution = renderer.GeometryViewport.ShadowCascadeResolution
                                let (shadowCascadeArray, shadowRenderbuffer, shadowFramebuffer) = renderer.PhysicallyBasedBuffers.ShadowCascadeArrayBuffersArray.[shadowCascadeBufferIndex]
                                GlRenderer3d.renderShadowCascade renderTasks renderer lightOrigin lightCutoff shadowCascadeLevel shadowView shadowProjection shadowViewProjection shadowFrustum shadowResolution shadowCascadeArray shadowRenderbuffer shadowFramebuffer
                                OpenGL.Hl.Assert ()

                                // filter cascade on the x (presuming that viewport already configured correctly)
                                let (shadowCascadeFilter, shadowRenderbuffer2, shadowFramebuffer2) = renderer.PhysicallyBasedBuffers.ShadowCascadeFilterBuffersArray.[shadowCascadeBufferIndex]
                                OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, shadowRenderbuffer2)
                                OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, shadowFramebuffer2)
                                OpenGL.PhysicallyBased.DrawFilterGaussianFilterSurface (v2 (1.0f / single shadowResolution.X) 0.0f, shadowCascadeLevel, shadowCascadeArray, renderer.PhysicallyBasedQuad, renderer.FilterShaders.FilterGaussianArray2dShader, renderer.PhysicallyBasedStaticVao)
                                OpenGL.Hl.Assert ()

                                // filter cascade on the y (presuming that viewport already configured correctly)
                                OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, shadowRenderbuffer)
                                OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, shadowFramebuffer)
                                OpenGL.Gl.FramebufferTextureLayer (OpenGL.FramebufferTarget.Framebuffer, OpenGL.FramebufferAttachment.ColorAttachment0, shadowCascadeArray.TextureId, 0, shadowCascadeLevel)
                                OpenGL.PhysicallyBased.DrawFilterGaussianArraySurface (v2 0.0f (1.0f / single shadowResolution.Y), shadowCascadeFilter, renderer.PhysicallyBasedQuad, renderer.FilterShaders.FilterGaussian2dShader, renderer.PhysicallyBasedStaticVao)
                                OpenGL.Gl.FramebufferTextureLayer (OpenGL.FramebufferTarget.Framebuffer, OpenGL.FramebufferAttachment.ColorAttachment0, 0u, 0, shadowCascadeLevel)
                                OpenGL.Hl.Assert ()

                            // remember the utilized index for the next frame
                            renderTasks.ShadowBufferIndexOpt <- Some (shadowCascadeBufferIndex + Constants.Render.ShadowTexturesMax)

                            // update renderer values or next shadow
                            // NOTE: this behavior completely DEPENDS on shadow index messages for a shadow cascade
                            // being received and processed in numerical order.
                            renderer.ShadowMatrices.
                                [shadowCascadeBufferIndex * Constants.Render.ShadowCascadeLevels + shadowCascadeLevel + Constants.Render.ShadowTexturesMax] <-
                                shadowView * shadowProjection
                            if shadowCascadeLevel = 0 then
                                renderer.LightShadowIndices.[lightId] <- shadowCascadeBufferIndex + Constants.Render.ShadowTexturesMax
                            elif shadowCascadeLevel = dec Constants.Render.ShadowCascadeLevels then
                                shadowCascadeBufferIndex <- inc shadowCascadeBufferIndex

                        | PointLight | SpotLight (_, _) | DirectionalLight -> failwithumf ()
                    | _ -> ()

        // process top-level geometry pass
        // OPTIMIZATION: we don't process rendering tasks if there are no render messages.
        if renderMessages.Count > 0 then
            let view = Viewport.getView3d eyeCenter eyeRotation
            let viewSkyBox = Matrix4x4.CreateFromQuaternion eyeRotation.Inverted
            let frustum = Viewport.getFrustum eyeCenter eyeRotation eyeFieldOfView geometryViewport
            let geometryProjection = Viewport.getProjection3d eyeFieldOfView geometryViewport
            let geometryViewProjection = view * geometryProjection
            let rasterProjection = Viewport.getProjection3d eyeFieldOfView rasterViewport
            GlRenderer3d.renderGeometry
                frustumInterior frustumExterior frustumImposter lightBox normalPass normalTasks renderer
                true None eyeCenter view viewSkyBox frustum geometryProjection geometryViewProjection rasterViewport.Inset rasterProjection
                framebuffer

        // clear light shadow indices
        renderer.LightShadowIndices.Clear ()

        // clear lights desiring shadows
        renderer.LightsDesiringShadows.Clear ()

        // destroy user-defined static models
        for staticModel in userDefinedStaticModelsToDestroy do
            GlRenderer3d.tryDestroyUserDefinedStaticModel staticModel renderer

        // destroy cached terrain geometries that weren't rendered this frame
        for geometry in renderer.PhysicallyBasedTerrainGeometries do
            if not (renderer.PhysicallyBasedTerrainGeometriesUtilized.Contains geometry.Key) then
                for patchGeometry in geometry.Value do
                    OpenGL.PhysicallyBased.DestroyPhysicallyBasedGeometry patchGeometry.Value
                    renderer.PhysicallyBasedTerrainGeometries.Remove geometry.Key |> ignore<bool>

        // reset terrain geometry book-keeping
        renderer.PhysicallyBasedTerrainGeometriesUtilized.Clear ()

        // clear config dirty flags
        renderer.LightingConfigChanged <- false
        renderer.RendererConfigChanged <- false

        // reload render assets upon request
        if renderer.ReloadAssetsRequested then
            GlRenderer3d.handleReloadRenderAssets renderer
            OpenGL.Hl.Assert ()
            renderer.ReloadAssetsRequested <- false

        // swap render passes
        for renderTasks in renderer.RenderPasses.Values do if renderTasks.ShadowBufferIndexOpt.IsNone then RenderTasks.clear renderTasks
        for renderTasks in renderer.RenderPasses2.Values do RenderTasks.clear renderTasks
        let renderPasses = renderer.RenderPasses
        renderer.RenderPasses <- renderer.RenderPasses2
        renderer.RenderPasses2 <- renderPasses

    /// Make a GlRenderer3d.
    static member make glContext window geometryViewport rasterViewport =

        // start lazy texture server
        let sglWindow = match window with SglWindow sglWindow -> sglWindow.SglWindow
        if SDL.SDL_GL_MakeCurrent (sglWindow, IntPtr.Zero) <> 0 then Log.error "Could not clear OpenGL context current when desired."
        let lazyTextureQueues = ConcurrentDictionary<OpenGL.Texture.LazyTexture ConcurrentQueue, OpenGL.Texture.LazyTexture ConcurrentQueue> HashIdentity.Reference
        let textureServer = OpenGL.Texture.TextureServer (lazyTextureQueues, glContext, sglWindow)
        textureServer.Start ()
        if SDL.SDL_GL_MakeCurrent (sglWindow, glContext) <> 0 then Log.error "Could not make OpenGL context current when required."
        OpenGL.Hl.Assert ()

        // create cube map vao
        let cubeMapVao = OpenGL.CubeMap.CreateCubeMapVao ()

        // create sky box shader
        let skyBoxShader = OpenGL.SkyBox.CreateSkyBoxShader Constants.Paths.SkyBoxShaderFilePath
        OpenGL.Hl.Assert ()

        // create irradiance shader
        let irradianceShader = OpenGL.CubeMap.CreateCubeMapShader Constants.Paths.IrradianceShaderFilePath
        OpenGL.Hl.Assert ()

        // create environment filter shader
        let environmentFilterShader = OpenGL.LightMap.CreateEnvironmentFilterShader Constants.Paths.EnvironmentFilterShaderFilePath
        OpenGL.Hl.Assert ()

        // create filter shaders
        let filterShaders = OpenGL.Filter.CreateFilterShaders ()
        OpenGL.Hl.Assert ()

        // create physically-based static vao
        let physicallyBasedStaticVao = OpenGL.PhysicallyBased.CreatePhysicallyBasedStaticVao ()
        OpenGL.Hl.Assert ()

        // create physically-based animated vao
        let physicallyBasedAnimatedVao = OpenGL.PhysicallyBased.CreatePhysicallyBasedAnimatedVao ()
        OpenGL.Hl.Assert ()

        // create physically-based terrain vao
        let physicallyBasedTerrainVao = OpenGL.PhysicallyBased.CreatePhysicallyBasedTerrainVao ()
        OpenGL.Hl.Assert ()

        // create physically-based shaders
        let physicallyBasedShaders = OpenGL.PhysicallyBased.CreatePhysicallyBasedShaders (Constants.Render.LightMapsMaxDeferred, Constants.Render.LightsMaxDeferred)
        OpenGL.Hl.Assert ()

        // create shadow matrices buffer
        let shadowMatricesCount = Constants.Render.ShadowTexturesMax + Constants.Render.ShadowCascadesMax * Constants.Render.ShadowCascadeLevels
        let shadowMatrices = Array.zeroCreate<Matrix4x4> shadowMatricesCount

        // create white cube map
        let cubeMap =
            let white = "Assets/Default/White.png"
            match OpenGL.CubeMap.TryCreateCubeMap (white, white, white, white, white, white) with
            | Right cubeMap -> cubeMap
            | Left error -> failwith error
        OpenGL.Hl.Assert ()

        // create cube map geometry
        let cubeMapGeometry = OpenGL.CubeMap.CreateCubeMapGeometry true
        OpenGL.Hl.Assert ()

        // create physically-based billboard geometry
        let billboardGeometry = OpenGL.PhysicallyBased.CreatePhysicallyBasedBillboard true
        OpenGL.Hl.Assert ()

        // create physically-based quad
        let physicallyBasedQuad = OpenGL.PhysicallyBased.CreatePhysicallyBasedQuad true
        OpenGL.Hl.Assert ()

        // create cube map surface
        let cubeMapSurface = OpenGL.CubeMap.CubeMapSurface.make cubeMap cubeMapGeometry
        OpenGL.Hl.Assert ()

        // create white texture
        let whiteTexture =
            match OpenGL.Texture.TryCreateTextureGl (false, OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true, true, false, "Assets/Default/White.png") with
            | Right (metadata, textureId) -> OpenGL.Texture.EagerTexture { TextureMetadata = metadata; TextureId = textureId }
            | Left error -> failwith ("Could not load white texture due to: " + error)
        OpenGL.Hl.Assert ()

        // create black texture
        let blackTexture =
            match OpenGL.Texture.TryCreateTextureGl (false, OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true, true, false, "Assets/Default/Black.png") with
            | Right (metadata, textureId) -> OpenGL.Texture.EagerTexture { TextureMetadata = metadata; TextureId = textureId }
            | Left error -> failwith ("Could not load black texture due to: " + error)
        OpenGL.Hl.Assert ()

        // load or create and save brdf texture
        let brdfTexture =
            let brdfBuffer =
                let brdfFilePath = "Assets/Default/Brdf.raw"
                try File.ReadAllBytes brdfFilePath
                with _ ->
                    Log.info "Creating missing Brdf.raw file (this may take a lot of time!)"
                    let brdfBuffer =
                        [|for y in 0 .. dec Constants.Render.BrdfResolution do
                            for x in 0 .. dec Constants.Render.BrdfResolution do
                                let nov = (single y + 0.5f) * (1.0f / single Constants.Render.BrdfResolution)
                                let roughness = (single x + 0.5f) * (1.0f / single Constants.Render.BrdfResolution)
                                GlRenderer3d.integrateBrdf nov roughness Constants.Render.BrdfSamples|]
                        |> Array.map (fun v -> [|BitConverter.GetBytes v.X; BitConverter.GetBytes v.Y|])
                        |> Array.concat
                        |> Array.concat
                    File.WriteAllBytes (brdfFilePath, brdfBuffer)
                    brdfBuffer
            let brdfBufferPtr = GCHandle.Alloc (brdfBuffer, GCHandleType.Pinned)
            try let brdfMetadata = OpenGL.Texture.TextureMetadata.make Constants.Render.BrdfResolution Constants.Render.BrdfResolution
                let brdfTextureId = OpenGL.Gl.GenTexture ()
                OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, brdfTextureId)
                OpenGL.Gl.TexImage2D (OpenGL.TextureTarget.Texture2d, 0, OpenGL.Hl.CheckRenderFormat OpenGL.InternalFormat.Rg32f, Constants.Render.BrdfResolution, Constants.Render.BrdfResolution, 0, OpenGL.PixelFormat.Rg, OpenGL.PixelType.Float, brdfBufferPtr.AddrOfPinnedObject ())
                OpenGL.Hl.Assert ()
                OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMinFilter, int OpenGL.TextureMinFilter.Linear)
                OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMagFilter, int OpenGL.TextureMagFilter.Linear)
                OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureWrapS, int OpenGL.TextureWrapMode.ClampToEdge)
                OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureWrapT, int OpenGL.TextureWrapMode.ClampToEdge)
                OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, 0u)
                OpenGL.Hl.Assert ()
                OpenGL.Texture.EagerTexture { TextureMetadata = brdfMetadata; TextureId = brdfTextureId }
            finally brdfBufferPtr.Free ()
        
        // create reflection renderbuffer
        let reflectionRenderbuffer = OpenGL.Gl.GenRenderbuffer ()
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, reflectionRenderbuffer)
        OpenGL.Gl.RenderbufferStorage (OpenGL.RenderbufferTarget.Renderbuffer, OpenGL.Hl.CheckRenderFormat OpenGL.InternalFormat.Depth24Stencil8, Constants.Render.ReflectionMapResolution, Constants.Render.ReflectionMapResolution)
        OpenGL.Hl.Assert ()

        // create reflection framebuffer
        let reflectionFramebuffer = OpenGL.Gl.GenFramebuffer ()
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, reflectionFramebuffer)
        OpenGL.Gl.FramebufferRenderbuffer (OpenGL.FramebufferTarget.Framebuffer, OpenGL.FramebufferAttachment.DepthStencilAttachment, OpenGL.RenderbufferTarget.Renderbuffer, reflectionRenderbuffer)
        OpenGL.Hl.Assert ()
        
        // create irradiance map renderbuffer
        let irradianceMapRenderbuffer = OpenGL.Gl.GenRenderbuffer ()
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, irradianceMapRenderbuffer)
        OpenGL.Gl.RenderbufferStorage (OpenGL.RenderbufferTarget.Renderbuffer, OpenGL.Hl.CheckRenderFormat OpenGL.InternalFormat.DepthComponent16, Constants.Render.IrradianceMapResolution, Constants.Render.IrradianceMapResolution)
        OpenGL.Hl.Assert ()

        // create irradiance map framebuffer
        let irradianceMapFramebuffer = OpenGL.Gl.GenFramebuffer ()
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, irradianceMapFramebuffer)
        OpenGL.Hl.Assert ()

        // create environment filter renderbuffer
        let environmentFilterRenderbuffer = OpenGL.Gl.GenRenderbuffer ()
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, environmentFilterRenderbuffer)
        OpenGL.Gl.RenderbufferStorage (OpenGL.RenderbufferTarget.Renderbuffer, OpenGL.Hl.CheckRenderFormat OpenGL.InternalFormat.DepthComponent16, Constants.Render.EnvironmentFilterResolution, Constants.Render.EnvironmentFilterResolution)
        OpenGL.Hl.Assert ()

        // create environment filter framebuffer
        let environmentFilterFramebuffer = OpenGL.Gl.GenFramebuffer ()
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, environmentFilterFramebuffer)
        OpenGL.Hl.Assert ()

        // create default irradiance map
        let irradianceMap =
            OpenGL.LightMap.CreateIrradianceMap
                (Constants.Render.IrradianceMapResolution,
                 cubeMapSurface,
                 irradianceShader,
                 cubeMapVao,
                 irradianceMapRenderbuffer,
                 irradianceMapFramebuffer)
        OpenGL.Hl.Assert ()

        // create default environment filter map
        let environmentFilterMap =
            OpenGL.LightMap.CreateEnvironmentFilterMap
                (Constants.Render.EnvironmentFilterResolution,
                 cubeMapSurface,
                 environmentFilterShader,
                 cubeMapVao,
                 environmentFilterRenderbuffer,
                 environmentFilterFramebuffer)
        OpenGL.Hl.Assert ()

        // get albedo metadata and texture
        let albedoTexture =
            match OpenGL.Texture.TryCreateTextureGl (false, OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true, true, true, "Assets/Default/MaterialAlbedo.dds") with
            | Right (metadata, textureId) -> OpenGL.Texture.EagerTexture { TextureMetadata = metadata; TextureId = textureId }
            | Left error -> failwith ("Could not load albedo material texture due to: " + error)
        OpenGL.Hl.Assert ()

        // create default physically-based material
        let physicallyBasedMaterial : OpenGL.PhysicallyBased.PhysicallyBasedMaterial =
            let roughnessTexture =
                match OpenGL.Texture.TryCreateTextureGl (false, OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true, true, true, "Assets/Default/MaterialRoughness.dds") with
                | Right (metadata, textureId) -> OpenGL.Texture.EagerTexture { TextureMetadata = metadata; TextureId = textureId }
                | Left error -> failwith ("Could not load material roughness texture due to: " + error)
            let metallicTexture =
                match OpenGL.Texture.TryCreateTextureGl (false, OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true, true, true, "Assets/Default/MaterialMetallic.dds") with
                | Right (metadata, textureId) -> OpenGL.Texture.EagerTexture { TextureMetadata = metadata; TextureId = textureId }
                | Left error -> failwith ("Could not load material metallic texture due to: " + error)
            let ambientOcclusionTexture =
                match OpenGL.Texture.TryCreateTextureGl (false, OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true, true, true, "Assets/Default/MaterialAmbientOcclusion.dds") with
                | Right (metadata, textureId) -> OpenGL.Texture.EagerTexture { TextureMetadata = metadata; TextureId = textureId }
                | Left error -> failwith ("Could not load material ambient occlusion texture due to: " + error)
            let emissionTexture =
                match OpenGL.Texture.TryCreateTextureGl (false, OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true, true, true, "Assets/Default/MaterialEmission.dds") with
                | Right (metadata, textureId) -> OpenGL.Texture.EagerTexture { TextureMetadata = metadata; TextureId = textureId }
                | Left error -> failwith ("Could not load material emission texture due to: " + error)
            let normalTexture =
                match OpenGL.Texture.TryCreateTextureGl (false, OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true, true, false, "Assets/Default/MaterialNormal.dds") with
                | Right (metadata, textureId) -> OpenGL.Texture.EagerTexture { TextureMetadata = metadata; TextureId = textureId }
                | Left error -> failwith ("Could not load material normal texture due to: " + error)
            let heightTexture =
                match OpenGL.Texture.TryCreateTextureGl (false, OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true, true, true, "Assets/Default/MaterialHeight.dds") with
                | Right (metadata, textureId) -> OpenGL.Texture.EagerTexture { TextureMetadata = metadata; TextureId = textureId }
                | Left error -> failwith ("Could not load material height texture due to: " + error)
            let subdermalTexture =
                match OpenGL.Texture.TryCreateTextureGl (false, OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true, true, true, "Assets/Default/MaterialSubdermal.dds") with
                | Right (metadata, textureId) -> OpenGL.Texture.EagerTexture { TextureMetadata = metadata; TextureId = textureId }
                | Left error -> failwith ("Could not load material subdermal texture due to: " + error)
            let finenessTexture =
                match OpenGL.Texture.TryCreateTextureGl (false, OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true, true, true, "Assets/Default/MaterialFineness.dds") with
                | Right (metadata, textureId) -> OpenGL.Texture.EagerTexture { TextureMetadata = metadata; TextureId = textureId }
                | Left error -> failwith ("Could not load material fineness texture due to: " + error)
            let scatterTexture =
                match OpenGL.Texture.TryCreateTextureGl (false, OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true, true, true, "Assets/Default/MaterialSubdermal.dds") with
                | Right (metadata, textureId) -> OpenGL.Texture.EagerTexture { TextureMetadata = metadata; TextureId = textureId }
                | Left error -> failwith ("Could not load material scatter texture due to: " + error)
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
              TwoSided = false
              Clipped = false
              Names = "" }

        // create physically-based buffers using the display viewport
        let physicallyBasedBuffers = OpenGL.PhysicallyBased.CreatePhysicallyBasedBuffers geometryViewport
        OpenGL.Hl.Assert ()

        // create forward surfaces comparer
        let forwardSurfacesComparer =
            { new IComparer<struct (single * single * Matrix4x4 * bool * Presence * Box2 * MaterialProperties * Matrix4x4 array voption * OpenGL.PhysicallyBased.PhysicallyBasedSurface * DepthTest * single * int)> with
                member this.Compare ((subsort, sort, _, _, _, _, _, _, _, _, distanceSquared, order), (subsort2, sort2, _, _, _, _, _, _, _, _, distanceSquared2, order2)) =
                    let sc = sort.CompareTo sort2
                    if sc <> 0 then sc
                    else
                        let dsc = distanceSquared.CompareTo distanceSquared2
                        if dsc <> 0 then -dsc // negated to draw furthest to nearest
                        else
                            let ssc = subsort.CompareTo subsort2
                            if ssc <> 0 then ssc
                            else -order.CompareTo order2 } // order enabled stable sort

        // make renderer
        let renderer =
            { GeometryViewport = geometryViewport
              RasterViewport = rasterViewport
              LazyTextureQueues = lazyTextureQueues
              TextureServer = textureServer
              CubeMapVao = cubeMapVao
              SkyBoxShader = skyBoxShader
              IrradianceShader = irradianceShader
              EnvironmentFilterShader = environmentFilterShader
              FilterShaders = filterShaders
              PhysicallyBasedStaticVao = physicallyBasedStaticVao
              PhysicallyBasedAnimatedVao = physicallyBasedAnimatedVao
              PhysicallyBasedTerrainVao = physicallyBasedTerrainVao
              PhysicallyBasedShaders = physicallyBasedShaders
              ShadowMatrices = shadowMatrices
              LightShadowIndices = dictPlus HashIdentity.Structural []
              LightsDesiringShadows = dictPlus HashIdentity.Structural []
              CubeMapGeometry = cubeMapGeometry
              BillboardGeometry = billboardGeometry
              PhysicallyBasedQuad = physicallyBasedQuad
              PhysicallyBasedTerrainGeometries = Dictionary HashIdentity.Structural
              PhysicallyBasedTerrainGeometriesUtilized = HashSet HashIdentity.Structural
              CubeMap = cubeMapSurface.CubeMap
              WhiteTexture = whiteTexture
              BlackTexture = blackTexture
              BrdfTexture = brdfTexture
              ReflectionRenderbuffer = reflectionRenderbuffer
              ReflectionFramebuffer = reflectionFramebuffer
              IrradianceMapRenderbuffer = irradianceMapRenderbuffer
              IrradianceMapFramebuffer = irradianceMapFramebuffer
              EnvironmentFilterRenderbuffer = environmentFilterRenderbuffer
              EnvironmentFilterFramebuffer = environmentFilterFramebuffer
              IrradianceMap = irradianceMap
              EnvironmentFilterMap = environmentFilterMap
              PhysicallyBasedMaterial = physicallyBasedMaterial
              PhysicallyBasedBuffers = physicallyBasedBuffers
              LightMaps = dictPlus HashIdentity.Structural []
              LightingConfig = Lighting3dConfig.defaultConfig
              LightingConfigChanged = false
              RendererConfig = Renderer3dConfig.defaultConfig
              RendererConfigChanged = false
              InstanceFields = Array.zeroCreate<single> (Constants.Render.InstanceFieldCount * Constants.Render.InstanceBatchPrealloc)
              UserDefinedStaticModelFields = [||]
              ForwardSurfacesComparer = forwardSurfacesComparer
              ForwardSurfacesSortBuffer = List ()
              RenderPackages = dictPlus StringComparer.Ordinal []
              RenderPasses = dictPlus HashIdentity.Structural [(NormalPass, RenderTasks.make ())]
              RenderPasses2 = dictPlus HashIdentity.Structural [(NormalPass, RenderTasks.make ())]
              RenderPackageCachedOpt = Unchecked.defaultof<_>
              RenderAssetCached = { CachedAssetTagOpt = Unchecked.defaultof<_>; CachedRenderAsset = Unchecked.defaultof<_> }
              ReloadAssetsRequested = false
              RenderMessages = List () }

        // fin
        renderer

    interface Renderer3d with

        member renderer.RendererConfig =
            renderer.RendererConfig

        member renderer.Render frustumInterior frustumExterior frustumImposter lightBox eyeCenter eyeRotation eyeFieldOfView geometryViewport rasterViewport renderMessages =
            GlRenderer3d.render frustumInterior frustumExterior frustumImposter lightBox eyeCenter eyeRotation eyeFieldOfView geometryViewport rasterViewport 0u renderMessages renderer

        member renderer.CleanUp () =

            OpenGL.Gl.DeleteVertexArrays [|renderer.CubeMapVao|]
            OpenGL.Hl.Assert ()

            OpenGL.Gl.DeleteProgram renderer.SkyBoxShader.SkyBoxShader
            OpenGL.Gl.DeleteProgram renderer.IrradianceShader.CubeMapShader
            OpenGL.Hl.Assert ()

            OpenGL.Filter.DestroyFilterShaders renderer.FilterShaders
            OpenGL.Hl.Assert ()

            OpenGL.Gl.DeleteVertexArrays [|renderer.PhysicallyBasedStaticVao|]
            OpenGL.Gl.DeleteVertexArrays [|renderer.PhysicallyBasedAnimatedVao|]
            OpenGL.Gl.DeleteVertexArrays [|renderer.PhysicallyBasedTerrainVao|]
            OpenGL.Hl.Assert ()

            OpenGL.PhysicallyBased.DestroyPhysicallyBasedShaders renderer.PhysicallyBasedShaders
            OpenGL.Hl.Assert ()

            OpenGL.CubeMap.DestroyCubeMapGeometry renderer.CubeMapGeometry
            OpenGL.Hl.Assert ()

            OpenGL.PhysicallyBased.DestroyPhysicallyBasedGeometry renderer.BillboardGeometry
            OpenGL.PhysicallyBased.DestroyPhysicallyBasedGeometry renderer.PhysicallyBasedQuad
            OpenGL.Hl.Assert ()

            renderer.CubeMap.Destroy ()
            renderer.BrdfTexture.Destroy ()
            OpenGL.Hl.Assert ()

            OpenGL.Gl.DeleteRenderbuffers [|renderer.ReflectionRenderbuffer|]
            OpenGL.Gl.DeleteFramebuffers [|renderer.ReflectionFramebuffer|]
            OpenGL.Gl.DeleteRenderbuffers [|renderer.IrradianceMapRenderbuffer|]
            OpenGL.Gl.DeleteFramebuffers [|renderer.IrradianceMapFramebuffer|]
            OpenGL.Gl.DeleteRenderbuffers [|renderer.EnvironmentFilterRenderbuffer|]
            OpenGL.Gl.DeleteFramebuffers [|renderer.EnvironmentFilterFramebuffer|]
            OpenGL.Hl.Assert ()

            renderer.IrradianceMap.Destroy ()
            renderer.EnvironmentFilterMap.Destroy ()
            OpenGL.Hl.Assert ()

            renderer.PhysicallyBasedMaterial.AlbedoTexture.Destroy ()
            renderer.PhysicallyBasedMaterial.RoughnessTexture.Destroy ()
            renderer.PhysicallyBasedMaterial.MetallicTexture.Destroy ()
            renderer.PhysicallyBasedMaterial.AmbientOcclusionTexture.Destroy ()
            renderer.PhysicallyBasedMaterial.EmissionTexture.Destroy ()
            renderer.PhysicallyBasedMaterial.NormalTexture.Destroy ()
            renderer.PhysicallyBasedMaterial.HeightTexture.Destroy ()
            OpenGL.Hl.Assert ()

            OpenGL.PhysicallyBased.DestroyPhysicallyBasedBuffers renderer.PhysicallyBasedBuffers
            OpenGL.Hl.Assert ()

            for lightMap in renderer.LightMaps.Values do OpenGL.LightMap.DestroyLightMap lightMap
            renderer.LightMaps.Clear ()
            OpenGL.Hl.Assert ()

            let renderPackages = renderer.RenderPackages |> Seq.map (fun entry -> entry.Value)
            let renderAssets = renderPackages |> Seq.map (fun package -> package.Assets.Values) |> Seq.concat
            for (_, _, asset) in renderAssets do GlRenderer3d.freeRenderAsset asset renderer
            renderer.RenderPackages.Clear ()
            OpenGL.Hl.Assert ()

            renderer.TextureServer.Terminate ()