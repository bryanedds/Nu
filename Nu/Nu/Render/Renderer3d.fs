// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Numerics
open Vortice.Vulkan
open SDL
open Prime
open Nu.Vulkan

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
      OpaqueDistanceOpt : single voption // forward only
      FinenessOffsetOpt : single voption // deferred only
      ScatterTypeOpt : ScatterType voption // deferred only - TODO: consider moving this and related use above FinenessOffsetOpt.
      SpecularScalarOpt : single voption // forward only
      SubsurfaceCutoffOpt : single voption // forward only
      SubsurfaceCutoffMarginOpt : single voption // forward only
      RefractiveIndexOpt : single voption // forward only
      ClearCoatOpt : single voption // deferred only - TODO: consider implementing for forward surfaces as well.
      ClearCoatRoughnessOpt : single voption } // deferred only - TODO: same as above.

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
    member this.SubsurfaceCutoff = ValueOption.defaultValue Constants.Render.SubsurfaceCutoffDefault this.SubsurfaceCutoffOpt
    member this.SubsurfaceCutoffMargin = ValueOption.defaultValue Constants.Render.SubsurfaceCutoffMarginDefault this.SubsurfaceCutoffMarginOpt
    member this.RefractiveIndex = ValueOption.defaultValue Constants.Render.RefractiveIndexDefault this.RefractiveIndexOpt
    member this.ClearCoat = ValueOption.defaultValue Constants.Render.ClearCoatDefault this.ClearCoatOpt
    member this.ClearCoatRoughness = ValueOption.defaultValue Constants.Render.ClearCoatRoughnessDefault this.ClearCoatRoughnessOpt

    /// Material properties with populated default properties.
    static member val defaultProperties =
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
          SubsurfaceCutoffOpt = ValueSome Constants.Render.SubsurfaceCutoffDefault
          SubsurfaceCutoffMarginOpt = ValueSome Constants.Render.SubsurfaceCutoffMarginDefault
          RefractiveIndexOpt = ValueSome Constants.Render.RefractiveIndexDefault
          ClearCoatOpt = ValueSome Constants.Render.ClearCoatDefault
          ClearCoatRoughnessOpt = ValueSome Constants.Render.ClearCoatRoughnessDefault }

    /// Empty material properties.
    static member val empty =
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
          SubsurfaceCutoffOpt = ValueNone
          SubsurfaceCutoffMarginOpt = ValueNone
          RefractiveIndexOpt = ValueNone
          ClearCoatOpt = ValueNone
          ClearCoatRoughnessOpt = ValueNone }

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
      ClearCoatImageOpt : Image AssetTag voption
      ClearCoatRoughnessImageOpt : Image AssetTag voption
      ClearCoatNormalImageOpt : Image AssetTag voption
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
    member this.ClearCoatImage = ValueOption.defaultValue (asset Assets.Default.PackageName Assets.Default.MaterialClearCoatName) this.ClearCoatImageOpt
    member this.ClearCoatRoughnessImage = ValueOption.defaultValue (asset Assets.Default.PackageName Assets.Default.MaterialClearCoatRoughnessName) this.ClearCoatRoughnessImageOpt
    member this.ClearCoatNormalImage = ValueOption.defaultValue (asset Assets.Default.PackageName Assets.Default.MaterialClearCoatNormalName) this.ClearCoatNormalImageOpt
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
        hash material.ClearCoatImageOpt ^^^
        hash material.ClearCoatRoughnessImageOpt ^^^
        hash material.ClearCoatNormalImageOpt ^^^
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
        this.ClearCoatImageOpt = that.ClearCoatImageOpt &&
        this.ClearCoatRoughnessImageOpt = that.ClearCoatRoughnessImageOpt &&
        this.ClearCoatNormalImageOpt = that.ClearCoatNormalImageOpt &&
        this.TwoSidedOpt = that.TwoSidedOpt &&
        this.ClippedOpt = that.ClippedOpt

    /// The material with populated default images.
    static member val defaultMaterial =
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
          ClearCoatImageOpt = ValueSome (asset Assets.Default.PackageName Assets.Default.MaterialClearCoatName)
          ClearCoatRoughnessImageOpt = ValueSome (asset Assets.Default.PackageName Assets.Default.MaterialClearCoatRoughnessName)
          ClearCoatNormalImageOpt = ValueSome (asset Assets.Default.PackageName Assets.Default.MaterialClearCoatNormalName)
          TwoSidedOpt = ValueSome false
          ClippedOpt = ValueSome false }

    /// The empty material.
    static member val empty =
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
          ClearCoatImageOpt = ValueNone
          ClearCoatRoughnessImageOpt = ValueNone
          ClearCoatNormalImageOpt = ValueNone
          TwoSidedOpt = ValueNone
          ClippedOpt = ValueNone }

    override this.GetHashCode () =
        Material.hash this

    override this.Equals (that : obj) =
        match that with
        | :? Material as that -> Material.equals this that
        | _ -> false

    interface IEquatable<Material> with
        member this.Equals that =
            Material.equals this that

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
      mutable DesireShadows : bool }

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
      MaterialProperties : PhysicallyBasedMaterialProperties
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
      ClearCoatImage : Image AssetTag
      ClearCoatRoughnessImage : Image AssetTag
      ClearCoatNormalImage : Image AssetTag
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
      DynamicShadows : bool
      DesireFog : bool
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

/// Configures 3d lighting.
type [<SymbolicExpansion>] Lighting3dConfig =
    { LightCutoffMargin : single
      LightAmbientBoostCutoff : single
      LightAmbientBoostScalar : single
      LightShadowSamples : int
      LightShadowBias : single
      LightShadowSampleScalar : single
      LightShadowExponent : single
      LightShadowRadius : single
      LightShadowDensity : single
      LightMapSingletonBlendMargin : single
      LightExposure : single
      ToneMapType : ToneMapType
      ToneMapSlope : Vector3
      ToneMapOffset : Vector3
      ToneMapPower : Vector3
      ToneMapSaturation : single
      ToneMapWhitePoint : single
      FogEnabled : bool // TODO: rename to DistanceFogEnabled and so on for others?
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
      SsvfIntensity : single
      SsvfSteps : int
      SsvfAsymmetry : single
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
      BloomStrength : single
      BloomThreshold : single
      BloomKarisAverageEnabled : bool
      BloomFilterRadius : single
      DepthOfFieldEnabled : bool
      DepthOfFieldNearDistance : single
      DepthOfFieldFarDistance : single
      DepthOfFieldRadius : single
      DepthOfFieldFocalType : FocalType
      DepthOfFieldFocalDistance : single
      DepthOfFieldFocalPoint : Vector2
      ChromaticAberrationEnabled : bool
      ChromaticAberrationChannelOffsets : Vector3
      ChromaticAberrationFocalPoint : Vector2 }

    static member val defaultConfig =
        { LightCutoffMargin = Constants.Render.LightCutoffMarginDefault
          LightAmbientBoostCutoff = Constants.Render.LightAmbientBoostCutoffDefault
          LightAmbientBoostScalar = Constants.Render.LightAmbientBoostScalarDefault
          LightShadowSamples = Constants.Render.LightShadowSamplesDefault
          LightShadowBias = Constants.Render.LightShadowBiasDefault
          LightShadowSampleScalar = Constants.Render.LightShadowSampleScalarDefault
          LightShadowExponent = Constants.Render.LightShadowExponentDefault
          LightShadowRadius = Constants.Render.LightShadowRadiusDefault
          LightShadowDensity = Constants.Render.LightShadowDensityDefault
          LightMapSingletonBlendMargin = Constants.Render.LightMapSingletonBlendMarginDefault
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
          SsvfIntensity = Constants.Render.SsvfIntensityDefault
          SsvfSteps = Constants.Render.SsvfStepsDefault
          SsvfAsymmetry = Constants.Render.SsvfAsymmetryDefault
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
          BloomStrength = Constants.Render.BloomStrengthDefault
          BloomThreshold = Constants.Render.BloomThresholdDefault
          BloomKarisAverageEnabled = Constants.Render.BloomKarisAverageEnabledDefault
          BloomFilterRadius = Constants.Render.BloomFilterRadiusDefault
          DepthOfFieldEnabled = Constants.Render.DepthOfFieldEnabledLocalDefault
          DepthOfFieldNearDistance = Constants.Render.DepthOfFieldNearDistanceDefault
          DepthOfFieldFarDistance = Constants.Render.DepthOfFieldFarDistanceDefault
          DepthOfFieldRadius = Constants.Render.DepthOfFieldRadiusDefault
          DepthOfFieldFocalType = Constants.Render.DepthOfFieldFocalTypeDefault
          DepthOfFieldFocalDistance = Constants.Render.DepthOfFieldFocalDistanceDefault
          DepthOfFieldFocalPoint = Constants.Render.DepthOfFieldFocalPointDefault
          ChromaticAberrationEnabled = Constants.Render.ChromaticAberrationEnabledLocalDefault
          ChromaticAberrationChannelOffsets = Constants.Render.ChromaticAberrationChannelOffsetsDefault
          ChromaticAberrationFocalPoint = Constants.Render.ChromaticAberrationFocalPointDefault }

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
      DepthOfFieldEnabled : bool
      ChromaticAberrationEnabled : bool
      FxaaEnabled : bool
      FxaaSpanMax : single
      FxaaReduceMinDivisor : single
      FxaaReduceMulDivisor : single }

    static member val defaultConfig =
        { LightMappingEnabled = Constants.Render.LightMappingEnabledDefault
          LightShadowingEnabled = Constants.Render.LightShadowingEnabledDefault
          SssEnabled = Constants.Render.SssEnabledGlobalDefault
          SsaoEnabled = Constants.Render.SsaoEnabledGlobalDefault
          SsaoSampleCount = Constants.Render.SsaoSampleCountDefault
          SsvfEnabled = Constants.Render.SsvfEnabledGlobalDefault
          SsrlEnabled = Constants.Render.SsrlEnabledGlobalDefault
          SsrrEnabled = Constants.Render.SsrrEnabledGlobalDefault
          BloomEnabled = Constants.Render.BloomEnabledGlobalDefault
          DepthOfFieldEnabled = Constants.Render.DepthOfFieldEnabledGlobalDefault
          ChromaticAberrationEnabled = Constants.Render.ChromaticAberrationEnabledGlobalDefault
          FxaaEnabled = Constants.Render.FxaaEnabledDefault
          FxaaSpanMax = Constants.Render.FxaaSpanMaxDefault
          FxaaReduceMinDivisor = Constants.Render.FxaaReduceMinDivisorDefault
          FxaaReduceMulDivisor = Constants.Render.FxaaReduceMulDivisorDefault }

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
      SortableLightMapIrradianceMap : Texture
      SortableLightMapEnvironmentFilterMap : Texture
      mutable SortableLightMapDistanceSquared : single }

    /// TODO: maybe put this somewhere general?
    static member private distanceFromBounds (point: Vector3) (bounds : Box3) =
        let x = min bounds.Max.X (max bounds.Min.X point.X)
        let y = min bounds.Max.Y (max bounds.Min.Y point.Y)
        let z = min bounds.Max.Z (max bounds.Min.Z point.Z)
        (point - v3 x y z).MagnitudeSquared

    /// Sort light maps into array for uploading to Vulkan.
    /// TODO: consider getting rid of allocation here.
    static member sortLightMaps lightMapsMax position boundsOpt irradianceMapDefault environmentMapDefault lightMaps =
        let lightMapOrigins = Array.zeroCreate<Vector3> lightMapsMax
        let lightMapMins = Array.zeroCreate<Vector3> lightMapsMax
        let lightMapSizes = Array.zeroCreate<Vector3> lightMapsMax
        let lightMapAmbientColors = Array.zeroCreate<Color> lightMapsMax
        let lightMapAmbientBrightnesses = Array.zeroCreate<single> lightMapsMax
        let lightMapIrradianceMaps = Array.replicate<Texture> lightMapsMax irradianceMapDefault
        let lightMapEnvironmentFilterMaps = Array.replicate<Texture> lightMapsMax environmentMapDefault
        let lightMapsFiltered =
            match boundsOpt with
            | Some (bounds : Box3) -> lightMaps |> Array.filter (fun lightMap -> lightMap.SortableLightMapBounds.Intersects bounds)
            | None -> lightMaps
        for lightMap in lightMapsFiltered do
            lightMap.SortableLightMapDistanceSquared <- SortableLightMap.distanceFromBounds position lightMap.SortableLightMapBounds
        let lightMapsSorted =
            lightMapsFiltered
            |> Array.sortWith (fun lightMap lightMap2 -> lightMap.SortableLightMapDistanceSquared.CompareTo lightMap2.SortableLightMapDistanceSquared)
        for i in 0 .. dec lightMapsMax do
            if i < lightMapsSorted.Length then
                let lightMap = lightMapsSorted[i]
                lightMapOrigins[i] <- lightMap.SortableLightMapOrigin
                lightMapMins[i] <- lightMap.SortableLightMapBounds.Min
                lightMapSizes[i] <- lightMap.SortableLightMapBounds.Size
                lightMapAmbientColors[i] <- lightMap.SortableLightMapAmbientColor
                lightMapAmbientBrightnesses[i] <- lightMap.SortableLightMapAmbientBrightness
                lightMapIrradianceMaps[i] <- lightMap.SortableLightMapIrradianceMap
                lightMapEnvironmentFilterMaps[i] <- lightMap.SortableLightMapEnvironmentFilterMap
        (lightMapOrigins, lightMapMins, lightMapSizes, lightMapAmbientColors, lightMapAmbientBrightnesses, lightMapIrradianceMaps, lightMapEnvironmentFilterMaps)

[<CustomComparison; CustomEquality>]
type private SortableLightProjection =
    { DirectionalWeight : int
      LightDistance : single
      DesiredShadowsWeight : int }

    static member equals left right =
        left.DirectionalWeight = right.DirectionalWeight &&
        left.LightDistance = right.LightDistance &&
        left.DesiredShadowsWeight = right.DesiredShadowsWeight

    static member compare left right =
        if left.DirectionalWeight < right.DirectionalWeight then 1
        elif left.DirectionalWeight > right.DirectionalWeight then -1
        elif left.LightDistance < right.LightDistance then -1
        elif left.LightDistance > right.LightDistance then 1
        elif left.DesiredShadowsWeight < right.DesiredShadowsWeight then -1
        elif left.DesiredShadowsWeight > right.DesiredShadowsWeight then 1
        else 0

    static member make directionalWeight lightDistance desiredShadowsWeight =
        { DirectionalWeight = directionalWeight; LightDistance = lightDistance; DesiredShadowsWeight = desiredShadowsWeight }

    override this.Equals that =
        match that with
        | :? SortableLightProjection as that -> SortableLightProjection.equals this that
        | _ -> false

    override x.GetHashCode () =
        HashCode.Combine(x.DirectionalWeight, x.LightDistance, x.DesiredShadowsWeight)

    interface IComparable<SortableLightProjection> with
        member this.CompareTo that =
            compare this.DesiredShadowsWeight that.DesiredShadowsWeight

    interface IComparable with
        member this.CompareTo that =
            match that with
            | :? SortableLightProjection as that -> SortableLightProjection.compare this that
            | _ -> failwithumf ()

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
      mutable SortableLightDistance : single }

    // TODO: P0: put the result in a struct with a comparison definition that doesn't allocate.
    static member private project light =
        let directionalWeight = match light.SortableLightType with 2 -> -1 | _ -> 0
        let desiredShadowsWeight = -light.SortableLightDesireShadows
        SortableLightProjection.make directionalWeight light.SortableLightDistance desiredShadowsWeight

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
                let light = lightsSorted[i]
                lightsArray[i] <- struct (light.SortableLightId, light.SortableLightOrigin, light.SortableLightCutoff, light.SortableLightConeOuter, light.SortableLightDesireShadows)
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
                let light = lightsSorted[i]
                lightsArray[i] <- struct (light.SortableLightId, light.SortableLightOrigin, light.SortableLightCutoff, light.SortableLightConeOuter, light.SortableLightDesireShadows)
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
                let light = lightsSorted[i]
                lightsArray[i] <- struct (light.SortableLightId, light.SortableLightOrigin, light.SortableLightCutoff, light.SortableLightConeOuter, light.SortableLightDesireShadows)
        lightsArray

    /// Sort lights into float array for uploading to Vulkan.
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
                let light = lightsSorted[i]
                lightIds[i] <- light.SortableLightId
                lightOrigins[i] <- light.SortableLightOrigin
                lightDirections[i] <- light.SortableLightDirection
                lightColors[i] <- light.SortableLightColor
                lightBrightnesses[i] <- light.SortableLightBrightness
                lightAttenuationLinears[i] <- light.SortableLightAttenuationLinear
                lightAttenuationQuadratics[i] <- light.SortableLightAttenuationQuadratic
                lightCutoffs[i] <- light.SortableLightCutoff
                lightTypes[i] <- light.SortableLightType
                lightConeInners[i] <- light.SortableLightConeInner
                lightConeOuters[i] <- light.SortableLightConeOuter
                lightDesireFogs[i] <- light.SortableLightDesireFog
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
      AnimatedSurface : PhysicallyBasedSurface }

    static member hash amsKey =
        let mutable hashCode = 0
        for i in 0 .. dec amsKey.BoneTransforms.Length do hashCode <- hashCode ^^^ amsKey.BoneTransforms[i].GetHashCode ()
        hashCode <- hashCode ^^^ PhysicallyBasedSurface.hash amsKey.AnimatedSurface
        hashCode

    static member equals left right =
        if left.BoneTransforms.Length = right.BoneTransforms.Length then
            let mutable equal = true
            let mutable i = 0
            while i < left.BoneTransforms.Length && equal do
                equal <- left.BoneTransforms[i] = right.BoneTransforms[i]
                i <- inc i
            equal && PhysicallyBasedSurface.equals left.AnimatedSurface right.AnimatedSurface
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
      DeferredStatic : Dictionary<PhysicallyBasedSurface, struct (Matrix4x4 * bool * Presence * Box2 * MaterialProperties) List>
      DeferredStaticPreBatches : Dictionary<Guid, struct (PhysicallyBasedSurface * (Matrix4x4 * bool * Presence * Box2 * MaterialProperties * Box3) array)>
      DeferredStaticClipped : Dictionary<PhysicallyBasedSurface, struct (Matrix4x4 * bool * Presence * Box2 * MaterialProperties) List>
      DeferredStaticClippedPreBatches : Dictionary<Guid, struct (PhysicallyBasedSurface * (Matrix4x4 * bool * Presence * Box2 * MaterialProperties * Box3) array)>
      DeferredAnimated : Dictionary<AnimatedModelSurfaceKey, struct (Matrix4x4 * bool * Presence * Box2 * MaterialProperties) List>
      DeferredTerrains : struct (TerrainDescriptor * TerrainPatchDescriptor * PhysicallyBasedGeometry) List
      Forward : struct (single * single * Matrix4x4 * bool * Presence * Box2 * MaterialProperties * Matrix4x4 array voption * PhysicallyBasedSurface * DepthTest) List
      ForwardSorted : struct (Matrix4x4 * bool * Presence * Box2 * MaterialProperties * Matrix4x4 array voption * PhysicallyBasedSurface * DepthTest) List
      DeferredStaticRemovals : PhysicallyBasedSurface List
      DeferredStaticClippedRemovals : PhysicallyBasedSurface List
      DeferredAnimatedRemovals : AnimatedModelSurfaceKey List
      mutable ShadowBufferIndexOpt : int option }

    static member make () =
        { SkyBoxes = List ()
          LightProbes = Dictionary HashIdentity.Structural
          LightMapRenders = HashSet HashIdentity.Structural
          LightMaps = List ()
          Lights = List ()
          DeferredStatic = dictPlus PhysicallyBasedSurfaceFns.comparer []
          DeferredStaticPreBatches = dictPlus HashIdentity.Structural []
          DeferredStaticClipped = dictPlus PhysicallyBasedSurfaceFns.comparer []
          DeferredStaticClippedPreBatches = dictPlus HashIdentity.Structural []
          DeferredAnimated = dictPlus AnimatedModelSurfaceKey.comparer []
          DeferredTerrains = List ()
          Forward = List ()
          ForwardSorted = List ()
          DeferredStaticRemovals = List ()
          DeferredStaticClippedRemovals = List ()
          DeferredAnimatedRemovals = List ()
          ShadowBufferIndexOpt = None }

    static member clear lightProbesIncluded renderTasks =

        renderTasks.SkyBoxes.Clear ()
        if lightProbesIncluded then renderTasks.LightProbes.Clear ()
        renderTasks.LightMapRenders.Clear ()
        renderTasks.LightMaps.Clear ()
        renderTasks.Lights.Clear ()

        for entry in renderTasks.DeferredStatic do entry.Value.Clear ()
        renderTasks.DeferredStaticPreBatches.Clear ()

        for entry in renderTasks.DeferredStaticClipped do entry.Value.Clear ()
        renderTasks.DeferredStaticClippedPreBatches.Clear ()

        for entry in renderTasks.DeferredAnimated do entry.Value.Clear ()
        renderTasks.DeferredAnimatedRemovals.Clear ()

        renderTasks.Forward.Clear ()
        renderTasks.ForwardSorted.Clear ()
        renderTasks.DeferredTerrains.Clear ()

        renderTasks.ShadowBufferIndexOpt <- None

    static member sweep renderTasks =

        for entry in renderTasks.DeferredStatic do
            if entry.Value.Count = 0 then
                renderTasks.DeferredStaticRemovals.Add entry.Key
        for removal in renderTasks.DeferredStaticRemovals do
            renderTasks.DeferredStatic.Remove removal |> ignore<bool>
        renderTasks.DeferredStaticRemovals.Clear ()

        for entry in renderTasks.DeferredStaticClipped do
            if entry.Value.Count = 0 then
                renderTasks.DeferredStaticClippedRemovals.Add entry.Key
        for removal in renderTasks.DeferredStaticClippedRemovals do
            renderTasks.DeferredStaticClipped.Remove removal |> ignore<bool>
        renderTasks.DeferredStaticClippedRemovals.Clear ()

        for entry in renderTasks.DeferredAnimated do
            if entry.Value.Count = 0 then
                renderTasks.DeferredAnimatedRemovals.Add entry.Key
        for removal in renderTasks.DeferredAnimatedRemovals do
            renderTasks.DeferredAnimated.Remove removal |> ignore<bool>
        renderTasks.DeferredAnimatedRemovals.Clear ()

    static member shadowUpToDate lightingConfigChanged renderingConfigChanged renderTasks renderTasksCached =
        if not lightingConfigChanged && not renderingConfigChanged then
            let deferredStaticCached =
                renderTasks.DeferredStatic.Count = renderTasksCached.DeferredStatic.Count &&
                let mutable changed = false
                let mutable enr = renderTasks.DeferredStatic.GetEnumerator ()
                while not changed && enr.MoveNext () do
                    let entry = enr.Current
                    let value = entry.Value
                    match renderTasksCached.DeferredStatic.TryGetValue entry.Key with
                    | (true, valueCached) ->
                        if  value.Count <> valueCached.Count ||
                            Seq.exists2 (fun struct (m, cs, _, _, _) struct (mCached, csCached, _, _, _) -> m <> mCached || cs <> csCached) value valueCached then
                            changed <- true
                    | (false, _) -> changed <- true
                not changed
            let deferredStaticClippedCached =
                renderTasks.DeferredStaticClipped.Count = renderTasksCached.DeferredStaticClipped.Count &&
                let mutable changed = false
                let mutable enr = renderTasks.DeferredStaticClipped.GetEnumerator ()
                while not changed && enr.MoveNext () do
                    let entry = enr.Current
                    let value = entry.Value
                    match renderTasksCached.DeferredStaticClipped.TryGetValue entry.Key with
                    | (true, valueCached) ->
                        if  value.Count <> valueCached.Count ||
                            Seq.exists2 (fun struct (m, cs, _, _, _) struct (mCached, csCached, _, _, _) -> m <> mCached || cs <> csCached) value valueCached then
                            changed <- true
                    | (false, _) -> changed <- true
                not changed
            let deferredStaticPreBatchesCached =
                renderTasks.DeferredStaticPreBatches.Count = renderTasksCached.DeferredStaticPreBatches.Count &&
                renderTasks.DeferredStaticPreBatches |> Seq.forall (fun preBatch -> renderTasksCached.DeferredStaticPreBatches.ContainsKey preBatch.Key)
            let deferredStaticClippedPreBatchesCached =
                renderTasks.DeferredStaticClippedPreBatches.Count = renderTasksCached.DeferredStaticClippedPreBatches.Count &&
                renderTasks.DeferredStaticClippedPreBatches |> Seq.forall (fun preBatch -> renderTasksCached.DeferredStaticClippedPreBatches.ContainsKey preBatch.Key)
            let deferredAnimatedCached =
                renderTasks.DeferredAnimated.Count = renderTasksCached.DeferredAnimated.Count &&
                let mutable changed = false
                let mutable enr = renderTasks.DeferredAnimated.GetEnumerator ()
                while not changed && enr.MoveNext () do
                    let entry = enr.Current
                    let value = entry.Value
                    match renderTasksCached.DeferredAnimated.TryGetValue entry.Key with
                    | (true, valueCached) ->
                        if  value.Count <> valueCached.Count ||
                            Seq.exists2 (fun struct (m, cs, _, _, _) struct (mCached, csCached, _, _, _) -> m <> mCached || cs <> csCached) value valueCached then
                            changed <- true
                    | (false, _) -> changed <- true
                not changed
            let deferredTerrainsCached =
                renderTasks.DeferredTerrains.Count = renderTasksCached.DeferredTerrains.Count &&
                (renderTasks.DeferredTerrains, renderTasksCached.DeferredTerrains)
                ||> Seq.forall2 (fun struct (terrainDescriptor, patchDescriptor, _) struct (terrainDescriptorCached, patchDescriptorCached, _) ->
                    patchDescriptor = patchDescriptorCached &&
                    terrainDescriptor.Bounds = terrainDescriptorCached.Bounds &&
                    terrainDescriptor.CastShadow = terrainDescriptorCached.CastShadow &&
                    terrainDescriptor.HeightMap = terrainDescriptorCached.HeightMap)
            let forwardCached =
                renderTasks.Forward.Count = renderTasksCached.Forward.Count &&
                (renderTasks.Forward, renderTasksCached.Forward)
                ||> Seq.forall2 (fun struct (_, _, m, cs, _, _, _, bo, s, _) struct (_, _, mCached, csCached, _, _, _, boCached, sCached, _) ->
                    m = mCached &&
                    cs = csCached &&
                    bo = boCached && // TODO: P1: optimize?
                    PhysicallyBasedSurfaceFns.equals s sCached)
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

    /// Pre-render a frame of the game.
    abstract PreRender :
        frustumInterior : Frustum ->
        frustumExterior : Frustum ->
        frustumImposter : Frustum ->
        eyeCenter : Vector3 ->
        eyeRotation : Quaternion ->
        renderMessages : RenderMessage3d List -> unit

    /// Render a frame of the game.
    abstract Render :
        frustumInterior : Frustum ->
        frustumExterior : Frustum ->
        frustumImposter : Frustum ->
        eyeCenter : Vector3 ->
        eyeRotation : Quaternion ->
        eyeFieldOfView : single ->
        geometryViewport : Viewport ->
        windowViewport : Viewport ->
        renderGeometry : bool -> unit

    /// Handle render clean up by freeing all loaded render assets.
    abstract CleanUp : unit -> unit

/// The stub implementation of Renderer3d.
type [<ReferenceEquality>] StubRenderer3d =
    private
        { StubRenderer3d : unit }

    /// Make a StubRenderer3d.
    static member make () =
        { StubRenderer3d = () }

    interface Renderer3d with
        member renderer.RendererConfig = Renderer3dConfig.defaultConfig
        member renderer.PreRender _ _ _ _ _ _ = ()
        member renderer.Render _ _ _ _ _ _ _ _ _ = ()
        member renderer.CleanUp () = ()

/// The Vulkan implementation of Renderer3d.
type [<ReferenceEquality>] VulkanRenderer3d =
    private
        { VulkanContext : VulkanContext
          mutable GeometryViewport : Viewport
          mutable WindowViewport : Viewport
          UnfilteredSampler : Sampler
          FilteredSampler : Sampler
          MaterialSampler : Sampler
          mutable RenderPassIndex : int
          GeometryInstanced : PhysicallyBasedGeometry HashSet
          mutable SkyBoxPipeline : SkyBoxPipeline
          mutable IrradiancePipeline : CubeMapPipeline
          mutable EnvironmentFilterPipeline : EnvironmentFilterPipeline
          mutable PhysicallyBasedPipelines : PhysicallyBasedPipelines
          ShadowMatricesFlipped : Matrix4x4 array
          LightShadowIndices : Dictionary<uint64, int>
          LightsDesiringShadows : Dictionary<uint64, SortableLight>
          CubeMapGeometry : CubeMapGeometry
          BillboardGeometry : PhysicallyBasedGeometry
          QuadGeometry : PhysicallyBasedGeometry
          TerrainGeometries : Dictionary<TerrainGeometryDescriptor, Dictionary<TerrainPatchDescriptor, PhysicallyBasedGeometry>>
          TerrainGeometriesUtilized : TerrainGeometryDescriptor HashSet
          CubeMap : Texture
          WhiteTexture : Texture
          BlackTexture : Texture
          BrdfTexture : Texture
          IrradianceMap : Texture
          EnvironmentFilterMap : Texture
          PhysicallyBasedMaterial : PhysicallyBasedMaterial
          mutable PhysicallyBasedAttachments : PhysicallyBasedAttachments
          LightMaps : Dictionary<uint64, LightMap>
          mutable LightingConfig : Lighting3dConfig
          mutable LightingConfigChanged : bool
          mutable RendererConfig : Renderer3dConfig
          mutable RendererConfigChanged : bool
          mutable InstanceFields : single array
          mutable UserDefinedStaticModelFields : single array
          UserDefinedStaticModelsToDestroy : StaticModel AssetTag SList
          ForwardSurfacesComparer : IComparer<struct (single * single * Matrix4x4 * bool * Presence * Box2 * MaterialProperties * Matrix4x4 array voption * PhysicallyBasedSurface * DepthTest * single * int)>
          ForwardSurfacesSortBuffer : struct (single * single * Matrix4x4 * bool * Presence * Box2 * MaterialProperties * Matrix4x4 array voption * PhysicallyBasedSurface * DepthTest * single * int) List
          RenderPackages : Packages<RenderAsset, AssetClient>
          mutable RenderPasses : Dictionary<RenderPass, RenderTasks>
          mutable RenderPasses2 : Dictionary<RenderPass, RenderTasks>
          mutable RenderPackageCachedOpt : RenderPackageCached
          mutable RenderAssetCached : RenderAssetCached
          mutable ReloadAssetsRequested : bool
          LazyTextureQueues : ConcurrentDictionary<LazyTexture ConcurrentQueue, LazyTexture ConcurrentQueue>
          TextureServer : TextureServer
          TextureDumpster : TextureDumpster }

    static member private logRenderAssetUnavailableOnce (assetTag : AssetTag) =
        let message =
            "Render asset " + assetTag.AssetName + " is not available from " + assetTag.PackageName + " package in a " + Constants.Associations.Render3d + " context. " +
            "Note that images from a " + Constants.Associations.Render2d + " context are usually not available in a " + Constants.Associations.Render3d + " context."
        Log.warnOnce message

    static member private radicalInverse (bits : uint) =
        let mutable bits = bits
        bits <- (bits <<< 16) ||| (bits >>> 16);
        bits <- ((bits &&& 0x55555555u) <<< 1) ||| ((bits &&& 0xAAAAAAAAu) >>> 1)
        bits <- ((bits &&& 0x33333333u) <<< 2) ||| ((bits &&& 0xCCCCCCCCu) >>> 2)
        bits <- ((bits &&& 0x0F0F0F0Fu) <<< 4) ||| ((bits &&& 0xF0F0F0F0u) >>> 4)
        bits <- ((bits &&& 0x00FF00FFu) <<< 8) ||| ((bits &&& 0xFF00FF00u) >>> 8)
        single bits * 2.3283064365386963e-10f

    static member private hammersley (i : int) (N : int) =
        v2 (single i / single N) (VulkanRenderer3d.radicalInverse (uint i))

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
        let ggx2 = VulkanRenderer3d.geometrySchlickGGX nov roughness
        let ggx1 = VulkanRenderer3d.geometrySchlickGGX nol roughness
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
            let xi = VulkanRenderer3d.hammersley i samples
            let h = VulkanRenderer3d.importanceSampleGGX xi roughness n
            let l = (2.0f * v.Dot h * h - v).Normalized
            let nol = max l.Z 0.0f
            let noh = max h.Z 0.0f
            let voh = max (v.Dot h) 0.0f
            let nov = max (n.Dot v) 0.0f
            if nol > 0.0f then
                let g = VulkanRenderer3d.geometrySmith roughness nov nol
                let gVis = (g * voh) / (noh * nov)
                let fc = pown (1.0f - voh) 5
                a <- a + (1.0f - fc) * gVis
                b <- b + fc * gVis
        v2 (a / single samples) (b / single samples)

    static member private invalidateCaches renderer =
        renderer.RenderPackageCachedOpt <- Unchecked.defaultof<_>
        renderer.RenderAssetCached.CachedAssetTagOpt <- Unchecked.defaultof<_>
        renderer.RenderAssetCached.CachedRenderAsset <- RawAsset

    static member private clearRenderPasses lightProbesIncluded renderer =

        // clear current render passes with possible exception of light probes to preserve tracking of associated light maps
        let normalTasksOpt = Dictionary.tryFind NormalPass renderer.RenderPasses
        renderer.RenderPasses.Clear ()
        match normalTasksOpt with
        | Some normalTasks ->
            RenderTasks.clear lightProbesIncluded normalTasks
            renderer.RenderPasses.Add (NormalPass, normalTasks)
        | None -> ()

        // clear previous render passes
        renderer.RenderPasses2.Clear ()

    static member private tryLoadTextureAsset (assetClient : AssetClient) (asset : Asset) renderer =
        VulkanRenderer3d.invalidateCaches renderer
        match assetClient.TextureClient.TryCreateTextureFiltered true (Hl.inferTextureCompression asset.FilePath) asset.FilePath RenderThread renderer.VulkanContext with
        | Right texture ->
            Some texture
        | Left error ->
            Log.info ("Could not load texture '" + asset.FilePath + "' due to '" + error + "'.")
            None

    static member private tryLoadCubeMapAsset (assetClient : AssetClient) (asset : Asset) renderer =
        VulkanRenderer3d.invalidateCaches renderer
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
            match assetClient.CubeMapClient.TryCreateCubeMap cubeMapKey RenderThread renderer.VulkanContext with
            | Right cubeMap -> Some (cubeMapKey, cubeMap, ref None)
            | Left error -> Log.info ("Could not load cube map '" + asset.FilePath + "' due to: " + error); None
        | _ -> Log.info ("Could not load cube map '" + asset.FilePath + "' due to requiring exactly 6 file paths with each file path on its own line."); None

    static member private tryLoadModelAsset (assetClient : AssetClient) (asset : Asset) renderer =
        VulkanRenderer3d.invalidateCaches renderer
        match assetClient.SceneClient.TryCreatePhysicallyBasedModel asset.FilePath renderer.PhysicallyBasedMaterial assetClient.TextureClient (Some renderer.VulkanContext) with
        | Right model -> Some model
        | Left error -> Log.info ("Could not load model '" + asset.FilePath + "' due to: " + error); None

    static member private tryLoadRawAsset (asset : Asset) renderer =
        VulkanRenderer3d.invalidateCaches renderer
        if File.Exists asset.FilePath
        then Some ()
        else None

    static member private tryLoadRenderAsset (assetClient : AssetClient) (asset : Asset) renderer =
        VulkanRenderer3d.invalidateCaches renderer
        match PathF.GetExtensionLower asset.FilePath with
        | RawExtension _ ->
            match VulkanRenderer3d.tryLoadRawAsset asset renderer with
            | Some () -> Some RawAsset
            | None -> None
        | ImageExtension _ ->
            match VulkanRenderer3d.tryLoadTextureAsset assetClient asset renderer with
            | Some texture -> Some (TextureAsset texture)
            | None -> None
        | CubeMapExtension _ ->
            match VulkanRenderer3d.tryLoadCubeMapAsset assetClient asset renderer with
            | Some (cubeMapKey, cubeMap, opt) -> Some (CubeMapAsset (cubeMapKey, cubeMap, opt))
            | None -> None
        | ModelExtension _ ->
            match VulkanRenderer3d.tryLoadModelAsset assetClient asset renderer with
            | Some model ->
                if model.Animated
                then Some (AnimatedModelAsset model)
                else Some (StaticModelAsset (false, model))
            | None -> None
        | _ -> None

    static member private freeRenderAsset renderAsset renderer =
        VulkanRenderer3d.invalidateCaches renderer
        match renderAsset with
        | RawAsset -> () // nothing to do
        | TextureAsset texture -> Texture.destroy texture renderer.VulkanContext
        | FontAsset (_, font) -> SDL3_ttf.TTF_CloseFont font
        | CubeMapAsset (_, cubeMap, irradianceAndEnvironmentMapOptRef) ->
            Texture.destroy cubeMap renderer.VulkanContext
            match irradianceAndEnvironmentMapOptRef.Value with
            | Some (irradiance, environment) -> 
                Texture.destroy irradiance renderer.VulkanContext
                Texture.destroy environment renderer.VulkanContext
            | None -> ()
        | StaticModelAsset (_, model) -> PhysicallyBased.destroyPhysicallyBasedModel model renderer.VulkanContext
        | AnimatedModelAsset model -> PhysicallyBased.destroyPhysicallyBasedModel model renderer.VulkanContext

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
                            (TextureClient (Some renderer.LazyTextureQueues),
                             CubeMapClient (),
                             PhysicallyBasedSceneClient ())
                    let renderPackage = { Assets = dictPlus StringComparer.Ordinal []; PackageState = assetClient }
                    renderer.RenderPackages[packageName] <- renderPackage
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
                | StaticModelAsset _ | AnimatedModelAsset _ -> ()
                VulkanRenderer3d.freeRenderAsset renderAsset renderer

            // categorize assets to load
            let assetsToLoad = HashSet ()
            for asset in assetsCollected do
                if not (assetsToKeep.ContainsKey asset.AssetTag.AssetName) then
                    assetsToLoad.Add asset |> ignore<bool>

            // preload assets
            renderPackage.PackageState.PreloadAssets (false, assetsToLoad, renderer.VulkanContext)

            // load assets
            let assetsLoaded = Dictionary ()
            for asset in assetsToLoad do
                match VulkanRenderer3d.tryLoadRenderAsset renderPackage.PackageState asset renderer with
                | Some renderAsset ->
                    let lastWriteTime =
                        try DateTimeOffset (File.GetLastWriteTime asset.FilePath)
                        with exn -> Log.info ("Asset file write time read error due to: " + scstring exn); DateTimeOffset.MinValue.DateTime
                    assetsLoaded[asset.AssetTag.AssetName] <- (lastWriteTime, asset, renderAsset)
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
                                        let material = scene.Materials[surface.SurfaceMaterialIndex]
                                        let (_, material) = PhysicallyBased.createPhysicallyBasedMaterial dirPath renderer.PhysicallyBasedMaterial renderPackage.PackageState.TextureClient material (Some renderer.VulkanContext)
                                        { surface with SurfaceMaterial = material }|]
                                StaticModelAsset (userDefined, { staticModel with Surfaces = surfaces })
                            | Some _ | None -> renderAsset
                        | AnimatedModelAsset animatedModel ->
                            match animatedModel.SceneOpt with
                            | Some scene ->
                                let surfaces =
                                    [|for surface in animatedModel.Surfaces do
                                        let material = scene.Materials[surface.SurfaceMaterialIndex]
                                        let (_, material) = PhysicallyBased.createPhysicallyBasedMaterial dirPath renderer.PhysicallyBasedMaterial renderPackage.PackageState.TextureClient material (Some renderer.VulkanContext)
                                        { surface with SurfaceMaterial = material }|]
                                AnimatedModelAsset { animatedModel with Surfaces = surfaces }
                            | None -> renderAsset
                    KeyValuePair (assetName, (lastWriteTime, asset, renderAsset))|]

            // insert assets into package
            for assetEntry in Seq.append assetsUpdated assetsLoaded do
                let assetName = assetEntry.Key
                let (lastWriteTime, asset, renderAsset) = assetEntry.Value
                renderPackage.Assets[assetName] <- (lastWriteTime, asset, renderAsset)

        // handle error cases
        | Left failedAssetNames ->
            Log.info ("Render package load failed due to unloadable assets '" + failedAssetNames + "' for package '" + packageName + "'.")

    static member private tryGetRenderAsset (assetTag : AssetTag) renderer =
        let mutable assetInfo = Unchecked.defaultof<DateTimeOffset * Asset * RenderAsset> // OPTIMIZATION: seems like TryGetValue allocates here if we use the tupling idiom (this may only be the case in Debug builds tho).
        if  renderer.RenderAssetCached.CachedAssetTagOpt :> obj |> notNull &&
            assetTag = renderer.RenderAssetCached.CachedAssetTagOpt then
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
            else VulkanRenderer3d.logRenderAssetUnavailableOnce assetTag; ValueNone
        else
            match Dictionary.tryFind assetTag.PackageName renderer.RenderPackages with
            | Some package ->
                renderer.RenderPackageCachedOpt <- { CachedPackageName = assetTag.PackageName; CachedPackageAssets = package.Assets }
                if package.Assets.TryGetValue (assetTag.AssetName, &assetInfo) then
                    let asset = Triple.thd assetInfo
                    renderer.RenderAssetCached.CachedAssetTagOpt <- assetTag
                    renderer.RenderAssetCached.CachedRenderAsset <- asset
                    ValueSome asset
                else VulkanRenderer3d.logRenderAssetUnavailableOnce assetTag; ValueNone
            | None ->
                Log.info ("Loading Render3d package '" + assetTag.PackageName + "' for asset '" + assetTag.AssetName + "' on the fly.")
                VulkanRenderer3d.tryLoadRenderPackage assetTag.PackageName renderer
                match renderer.RenderPackages.TryGetValue assetTag.PackageName with
                | (true, package) ->
                    renderer.RenderPackageCachedOpt <- { CachedPackageName = assetTag.PackageName; CachedPackageAssets = package.Assets }
                    if package.Assets.TryGetValue (assetTag.AssetName, &assetInfo) then
                        let asset = Triple.thd assetInfo
                        renderer.RenderAssetCached.CachedAssetTagOpt <- assetTag
                        renderer.RenderAssetCached.CachedRenderAsset <- asset
                        ValueSome asset
                    else VulkanRenderer3d.logRenderAssetUnavailableOnce assetTag; ValueNone
                | (false, _) -> ValueNone

    static member private tryGetFilePath (assetTag : AssetTag) renderer =
        match VulkanRenderer3d.tryGetRenderAsset assetTag renderer with
        | ValueSome _ ->
            match renderer.RenderPackages.TryGetValue assetTag.PackageName with
            | (true, package) ->
                match package.Assets.TryGetValue assetTag.AssetName with
                | (true, (_, asset, _)) -> Some asset.FilePath
                | (false, _) -> None
            | (false, _) -> None
        | ValueNone -> None

    static member private tryGetTextureData minimal (assetTag : Image AssetTag) renderer =
        match VulkanRenderer3d.tryGetFilePath assetTag renderer with
        | Some filePath ->
            match TextureData.tryCreate minimal filePath with
            | Some textureData ->
                let metadata = textureData.Metadata
                let (compressed, bytes) = textureData.Bytes
                TextureData.destroy textureData
                Some (metadata, compressed, bytes)
            | None -> None
        | None -> None

    static member private tryGetHeightMapResolution heightMap renderer =
        match heightMap with
        | ImageHeightMap image ->
            match VulkanRenderer3d.tryGetRenderAsset image renderer with
            | ValueSome renderAsset ->
                match renderAsset with
                | TextureAsset texture ->
                    let metadata = texture.TextureMetadata
                    Some (metadata.TextureWidth, metadata.TextureHeight)
                | _ -> None
            | ValueNone -> None
        | RawHeightMap map -> Some (map.Resolution.X, map.Resolution.Y)

    static member private tryCreateUserDefinedStaticModel surfaceDescriptors bounds (assetTag : StaticModel AssetTag) renderer =

        // ensure target package is loaded if possible
        if not (renderer.RenderPackages.ContainsKey assetTag.PackageName) then
            VulkanRenderer3d.tryLoadRenderPackage assetTag.PackageName renderer

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
                    match VulkanRenderer3d.tryGetRenderAsset surfaceDescriptor.AlbedoImage renderer with
                    | ValueSome (TextureAsset texture) -> texture
                    | _ -> renderer.PhysicallyBasedMaterial.AlbedoTexture

                // make material properties
                let properties =
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
                      SubsurfaceCutoff = surfaceDescriptor.MaterialProperties.SubsurfaceCutoff
                      SubsurfaceCutoffMargin = surfaceDescriptor.MaterialProperties.SubsurfaceCutoffMargin
                      RefractiveIndex = surfaceDescriptor.MaterialProperties.RefractiveIndex
                      ClearCoat = surfaceDescriptor.MaterialProperties.ClearCoat
                      ClearCoatRoughness = surfaceDescriptor.MaterialProperties.ClearCoatRoughness }

                // make material
                let material =
                    { AlbedoTexture = albedoTexture
                      RoughnessTexture = match VulkanRenderer3d.tryGetRenderAsset surfaceDescriptor.RoughnessImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.RoughnessTexture
                      MetallicTexture = match VulkanRenderer3d.tryGetRenderAsset surfaceDescriptor.MetallicImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.MetallicTexture
                      AmbientOcclusionTexture = match VulkanRenderer3d.tryGetRenderAsset surfaceDescriptor.AmbientOcclusionImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.AmbientOcclusionTexture
                      EmissionTexture = match VulkanRenderer3d.tryGetRenderAsset surfaceDescriptor.EmissionImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.EmissionTexture
                      NormalTexture = match VulkanRenderer3d.tryGetRenderAsset surfaceDescriptor.NormalImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.NormalTexture
                      HeightTexture = match VulkanRenderer3d.tryGetRenderAsset surfaceDescriptor.HeightImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.HeightTexture
                      SubdermalTexture = match VulkanRenderer3d.tryGetRenderAsset surfaceDescriptor.SubdermalImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.SubdermalTexture
                      FinenessTexture = match VulkanRenderer3d.tryGetRenderAsset surfaceDescriptor.FinenessImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.FinenessTexture
                      ScatterTexture = match VulkanRenderer3d.tryGetRenderAsset surfaceDescriptor.ScatterImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.ScatterTexture
                      ClearCoatTexture = match VulkanRenderer3d.tryGetRenderAsset surfaceDescriptor.ClearCoatImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.ClearCoatTexture
                      ClearCoatRoughnessTexture = match VulkanRenderer3d.tryGetRenderAsset surfaceDescriptor.ClearCoatRoughnessImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.ClearCoatRoughnessTexture
                      ClearCoatNormalTexture = match VulkanRenderer3d.tryGetRenderAsset surfaceDescriptor.ClearCoatNormalImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.ClearCoatNormalTexture
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
                        vertexData[u] <- surfaceDescriptor.Positions[i].X
                        vertexData[u+1] <- surfaceDescriptor.Positions[i].Y
                        vertexData[u+2] <- surfaceDescriptor.Positions[i].Z
                        vertexData[u+3] <- surfaceDescriptor.TexCoordses[i].X
                        vertexData[u+4] <- surfaceDescriptor.TexCoordses[i].Y
                        vertexData[u+5] <- surfaceDescriptor.Normals[i].X
                        vertexData[u+6] <- surfaceDescriptor.Normals[i].Y
                        vertexData[u+7] <- surfaceDescriptor.Normals[i].Z
                        i <- inc i
                with :? IndexOutOfRangeException ->
                    Log.info "Vertex data truncated due to an unequal count among surface descriptor Positions, TexCoordses, and Normals."

                // create index data
                let indexData = surfaceDescriptor.Indices.AsMemory ()

                // create geometry
                // TODO: consider letting user specify primitive drawing type.
                let geometry = PhysicallyBased.createPhysicallyBasedStaticGeometry VkPrimitiveTopology.TriangleList vertexData indexData surfaceDescriptor.Bounds (Some renderer.VulkanContext)

                // create surface
                let surface = PhysicallyBasedSurface.make Array.empty surfaceDescriptor.ModelMatrix surfaceDescriptor.Bounds properties material -1 Assimp.Node.Empty geometry
                surfaces.Add surface

            // create user-defined static model
            let surfaces = Seq.toArray surfaces
            let hierarchy = TreeNode (Array.map PhysicallyBasedSurface surfaces)
            let model =
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
                package.Assets[assetTag.AssetName] <- (DateTimeOffset.MinValue.DateTime, asset, StaticModelAsset (true, model))
            | (false, _) ->
                let assetClient =
                    AssetClient
                        (TextureClient (Some renderer.LazyTextureQueues),
                         CubeMapClient (),
                         PhysicallyBasedSceneClient ())
                let asset = Asset.make assetTag "" [] (Set.singleton Constants.Associations.Render3d)
                let package = { Assets = Dictionary.singleton StringComparer.Ordinal assetTag.AssetName (DateTimeOffset.MinValue.DateTime, asset, StaticModelAsset (true, model)); PackageState = assetClient }
                renderer.RenderPackages[assetTag.PackageName] <- package

        // attempted to replace a loaded asset
        else Log.info ("Cannot replace a loaded asset '" + scstring assetTag + "' with a user-created static model.")

    static member private tryDestroyUserDefinedStaticModel assetTag renderer =

        // ensure target package is loaded if possible
        if not (renderer.RenderPackages.ContainsKey assetTag.PackageName) then
            VulkanRenderer3d.tryLoadRenderPackage assetTag.PackageName renderer

        // free any existing user-created static model, also determining if target asset can be user-created
        match renderer.RenderPackages.TryGetValue assetTag.PackageName with
        | (true, package) ->
            match package.Assets.TryGetValue assetTag.AssetName with
            | (true, (_, _, asset)) ->
                match asset with
                | StaticModelAsset (userDefined, _) when userDefined -> VulkanRenderer3d.freeRenderAsset asset renderer
                | _ -> ()
            | (false, _) -> ()
        | (false, _) -> ()

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
                    RenderTasks.clear true recycledTasks
                    recycledTasks
                | _ -> RenderTasks.make ()
            renderer.RenderPasses.Add (renderPass, renderTasks)
            renderTasks

    static member private getLastSkyBoxOpt renderPass renderer =
        let renderTasks = VulkanRenderer3d.getRenderTasks renderPass renderer
        match Seq.tryLast renderTasks.SkyBoxes with
        | Some (lightAmbientColor, lightAmbientBrightness, cubeMapColor, cubeMapBrightness, cubeMapAsset) ->
            match VulkanRenderer3d.tryGetRenderAsset cubeMapAsset renderer with
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
                    let v  = fst' positionsAndTexCoordses[resolution.X * y + x]
                    let n  = fst' positionsAndTexCoordses[resolution.X * dec y + x]
                    let ne = fst' positionsAndTexCoordses[resolution.X * dec y + inc x]
                    let e  = fst' positionsAndTexCoordses[resolution.X * y + inc x]
                    let s  = fst' positionsAndTexCoordses[resolution.X * inc y + x]
                    let sw = fst' positionsAndTexCoordses[resolution.X * inc y + dec x]
                    let w  = fst' positionsAndTexCoordses[resolution.X * y + dec x]
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
                match VulkanRenderer3d.tryGetTextureData false normalImage renderer with
                | Some (metadata, compressed, bytes) ->
                    if metadata.TextureWidth * metadata.TextureHeight = positionsAndTexCoordses.Length then
                        if not compressed then
                            let scalar = 1.0f / single Byte.MaxValue
                            bytes
                            |> Array.map (fun b -> single b * scalar)
                            |> Array.chunkBySize 4
                            |> Array.map (fun b ->
                                let tangent = (v3 b[2] b[1] b[0] * 2.0f - v3One).Normalized
                                let normal = v3 tangent.X tangent.Z -tangent.Y
                                normal)
                            |> Some
                        else Log.info "Block-compressed images not supported for terrain normal images."; None
                    else Log.info "Normal image resolution does not match terrain resolution."; None
                | None -> Some (VulkanRenderer3d.createPhysicallyBasedTerrainNormals resolution positionsAndTexCoordses)
            | None -> Some (VulkanRenderer3d.createPhysicallyBasedTerrainNormals resolution positionsAndTexCoordses)

        // compute tint
        let tintOpt =
            match geometryDescriptor.TintImageOpt with
            | Some tintImage ->
                match VulkanRenderer3d.tryGetTextureData false tintImage renderer with
                | Some (metadata, compressed, bytes) ->
                    if metadata.TextureWidth * metadata.TextureHeight = positionsAndTexCoordses.Length then
                        if not compressed then
                            let scalar = 1.0f / single Byte.MaxValue
                            bytes
                            |> Array.map (fun b -> single b * scalar)
                            |> Array.chunkBySize 4
                            |> Array.map (fun b -> v3 b[2] b[1] b[0])
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
                match VulkanRenderer3d.tryGetTextureData false rgbaMap renderer with
                | Some (metadata, compressed, bytes) ->
                    if metadata.TextureWidth * metadata.TextureHeight = positionsAndTexCoordses.Length then
                        if not compressed then
                            let scalar = 1.0f / single Byte.MaxValue
                            for i in 0 .. dec positionsAndTexCoordses.Length do
                                // ARGB reverse byte order, from Drawing.Bitmap (windows).
                                // TODO: confirm it is the same for SDL (linux).
                                blendses[i, 0] <- single bytes[i * 4 + 2] * scalar
                                blendses[i, 1] <- single bytes[i * 4 + 1] * scalar
                                blendses[i, 2] <- single bytes[i * 4 + 0] * scalar
                                blendses[i, 3] <- single bytes[i * 4 + 3] * scalar
                        else Log.info "Block-compressed images not supported for terrain blend iamges."
                    else Log.info "Blend image resolution does not match terrain resolution."
                | None -> Log.info ("Could not locate texture data for blend image '" + scstring rgbaMap + "'.")
            | RedsMap reds ->
                let scalar = 1.0f / single Byte.MaxValue
                for i in 0 .. dec (min reds.Length 8) do
                    let red = reds[i]
                    match VulkanRenderer3d.tryGetTextureData false red renderer with
                    | Some (metadata, compressed, bytes) ->
                        if metadata.TextureWidth * metadata.TextureHeight = positionsAndTexCoordses.Length then
                            if not compressed then
                                for j in 0 .. dec positionsAndTexCoordses.Length do
                                    blendses[j, i] <- single bytes[j * 4 + 2] * scalar
                            else Log.info "Block-compressed images not supported for terrain blend images."
                        else Log.info "Blend image resolution does not match terrain resolution."
                    | None -> Log.info ("Could not locate texture data for blend image '" + scstring red + "'.")
        | FlatMaterial _ ->
            for i in 0 .. dec positionsAndTexCoordses.Length do
                blendses[i,0] <- 1.0f

        // ensure we've got usable input data
        match (normalsOpt, tintOpt) with
        | (Some normals, Some tint) ->
                
            // provide data necessary for creating geometry
            Some (positionsAndTexCoordses, normals, blendses, tint)

        // error
        | (_, _) -> None

    static member private makeBillboardMaterial (properties : MaterialProperties inref, material : Material inref, renderer) =
        let albedoTexture =
            match VulkanRenderer3d.tryGetRenderAsset material.AlbedoImage renderer with
            | ValueSome (TextureAsset texture) -> texture
            | _ -> renderer.PhysicallyBasedMaterial.AlbedoTexture
        let roughnessTexture =
            match VulkanRenderer3d.tryGetRenderAsset material.RoughnessImage renderer with
            | ValueSome (TextureAsset texture) -> texture
            | _ -> renderer.PhysicallyBasedMaterial.RoughnessTexture
        let metallicTexture =
            match VulkanRenderer3d.tryGetRenderAsset material.MetallicImage renderer with
            | ValueSome (TextureAsset texture) -> texture
            | _ -> renderer.PhysicallyBasedMaterial.MetallicTexture
        let ambientOcclusionTexture =
            match VulkanRenderer3d.tryGetRenderAsset material.AmbientOcclusionImage renderer with
            | ValueSome (TextureAsset texture) -> texture
            | _ -> renderer.PhysicallyBasedMaterial.AmbientOcclusionTexture
        let emissionTexture =
            match VulkanRenderer3d.tryGetRenderAsset material.EmissionImage renderer with
            | ValueSome (TextureAsset texture) -> texture
            | _ -> renderer.PhysicallyBasedMaterial.EmissionTexture
        let normalTexture =
            match VulkanRenderer3d.tryGetRenderAsset material.NormalImage renderer with
            | ValueSome (TextureAsset texture) -> texture
            | _ -> renderer.PhysicallyBasedMaterial.NormalTexture
        let heightTexture =
            match VulkanRenderer3d.tryGetRenderAsset material.HeightImage renderer with
            | ValueSome (TextureAsset texture) -> texture
            | _ -> renderer.PhysicallyBasedMaterial.HeightTexture
        let subdermalTexture =
            match VulkanRenderer3d.tryGetRenderAsset material.SubdermalImage renderer with
            | ValueSome (TextureAsset texture) -> texture
            | _ -> renderer.PhysicallyBasedMaterial.SubdermalTexture
        let finenessTexture =
            match VulkanRenderer3d.tryGetRenderAsset material.FinenessImage renderer with
            | ValueSome (TextureAsset texture) -> texture
            | _ -> renderer.PhysicallyBasedMaterial.FinenessTexture
        let scatterTexture =
            match VulkanRenderer3d.tryGetRenderAsset material.ScatterImage renderer with
            | ValueSome (TextureAsset texture) -> texture
            | _ -> renderer.PhysicallyBasedMaterial.ScatterTexture
        let clearCoatTexture =
            match VulkanRenderer3d.tryGetRenderAsset material.ClearCoatImage renderer with
            | ValueSome (TextureAsset texture) -> texture
            | _ -> renderer.PhysicallyBasedMaterial.ClearCoatTexture
        let clearCoatRoughnessTexture =
            match VulkanRenderer3d.tryGetRenderAsset material.ClearCoatRoughnessImage renderer with
            | ValueSome (TextureAsset texture) -> texture
            | _ -> renderer.PhysicallyBasedMaterial.ClearCoatRoughnessTexture
        let clearCoatNormalTexture =
            match VulkanRenderer3d.tryGetRenderAsset material.ClearCoatNormalImage renderer with
            | ValueSome (TextureAsset texture) -> texture
            | _ -> renderer.PhysicallyBasedMaterial.ClearCoatNormalTexture
        let properties : PhysicallyBasedMaterialProperties =
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
              SubsurfaceCutoff = properties.SubsurfaceCutoff
              SubsurfaceCutoffMargin = properties.SubsurfaceCutoffMargin
              RefractiveIndex = properties.RefractiveIndex
              ClearCoat = properties.ClearCoat
              ClearCoatRoughness = properties.ClearCoatRoughness }
        let material : PhysicallyBasedMaterial =
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
              TwoSided = true
              Clipped = true
              Names = "" }
        struct (properties, material)

    static member private applySurfaceMaterial (material : Material inref, surfaceMaterial : PhysicallyBasedMaterial inref, renderer) =
        let albedoTexture =
            match material.AlbedoImageOpt with
            | ValueSome image ->
                match VulkanRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.AlbedoTexture
            | ValueNone -> surfaceMaterial.AlbedoTexture
        let roughnessTexture =
            match material.RoughnessImageOpt with
            | ValueSome image ->
                match VulkanRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.RoughnessTexture
            | ValueNone -> surfaceMaterial.RoughnessTexture
        let metallicTexture =
            match material.MetallicImageOpt with
            | ValueSome image ->
                match VulkanRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.MetallicTexture
            | ValueNone -> surfaceMaterial.MetallicTexture
        let ambientOcclusionTexture =
            match material.AmbientOcclusionImageOpt with
            | ValueSome image ->
                match VulkanRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.AmbientOcclusionTexture
            | ValueNone -> surfaceMaterial.AmbientOcclusionTexture
        let emissionTexture =
            match material.EmissionImageOpt with
            | ValueSome image ->
                match VulkanRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.EmissionTexture
            | ValueNone -> surfaceMaterial.EmissionTexture
        let normalTexture =
            match material.NormalImageOpt with
            | ValueSome image ->
                match VulkanRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.NormalTexture
            | ValueNone -> surfaceMaterial.NormalTexture
        let heightTexture =
            match material.HeightImageOpt with
            | ValueSome image ->
                match VulkanRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.HeightTexture
            | ValueNone -> surfaceMaterial.HeightTexture
        let finenessTexture =
            match material.FinenessImageOpt with
            | ValueSome image ->
                match VulkanRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.FinenessTexture
            | ValueNone -> surfaceMaterial.FinenessTexture
        let subdermalTexture =
            match material.SubdermalImageOpt with
            | ValueSome image ->
                match VulkanRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.SubdermalTexture
            | ValueNone -> surfaceMaterial.SubdermalTexture
        let scatterTexture =
            match material.ScatterImageOpt with
            | ValueSome image ->
                match VulkanRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.ScatterTexture
            | ValueNone -> surfaceMaterial.ScatterTexture
        let clearCoatTexture =
            match material.ClearCoatImageOpt with
            | ValueSome image ->
                match VulkanRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.ClearCoatTexture
            | ValueNone -> surfaceMaterial.ClearCoatTexture
        let clearCoatRoughnessTexture =
            match material.ClearCoatRoughnessImageOpt with
            | ValueSome image ->
                match VulkanRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.ClearCoatRoughnessTexture
            | ValueNone -> surfaceMaterial.ClearCoatRoughnessTexture
        let clearCoatNormalTexture =
            match material.ClearCoatNormalImageOpt with
            | ValueSome image ->
                match VulkanRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.ClearCoatNormalTexture
            | ValueNone -> surfaceMaterial.ClearCoatNormalTexture
        let twoSided =
            match material.TwoSidedOpt with
            | ValueSome twoSided -> twoSided
            | ValueNone -> surfaceMaterial.TwoSided
        let clipped =
            match material.ClippedOpt with
            | ValueSome clipped -> clipped
            | ValueNone -> surfaceMaterial.Clipped
        let surfaceMaterial : PhysicallyBasedMaterial =
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
              Names = "" }
        surfaceMaterial

    static member private handleReloadShaders renderer =
        Pipeline.reloadShaders renderer.SkyBoxPipeline.Pipeline renderer.VulkanContext
        Pipeline.reloadShaders renderer.IrradiancePipeline.Pipeline renderer.VulkanContext
        Pipeline.reloadShaders renderer.EnvironmentFilterPipeline.Pipeline renderer.VulkanContext
        PhysicallyBased.reloadPhysicallyBasedShaders renderer.PhysicallyBasedPipelines renderer.VulkanContext
    
    static member private handleLoadRenderPackage hintPackageName renderer =
        VulkanRenderer3d.tryLoadRenderPackage hintPackageName renderer

    static member private handleUnloadRenderPackage hintPackageName renderer =
        VulkanRenderer3d.invalidateCaches renderer
        match Dictionary.tryFind hintPackageName renderer.RenderPackages with
        | Some package ->
            for (_, _, asset) in package.Assets.Values do VulkanRenderer3d.freeRenderAsset asset renderer
            let mutable unused = Unchecked.defaultof<_>
            renderer.LazyTextureQueues.Remove (package.PackageState.TextureClient.LazyTextureQueue, &unused) |> ignore<bool>
            renderer.RenderPackages.Remove hintPackageName |> ignore
        | None -> ()

    static member private handleReloadRenderAssets renderer =
        VulkanRenderer3d.invalidateCaches renderer
        VulkanRenderer3d.clearRenderPasses false renderer // invalidate render task keys that now contain potentially stale data (but keep light probes for light map tracking)
        VulkanRenderer3d.handleReloadShaders renderer // waits for renders to complete, relevant to all asset reload
        for packageName in renderer.RenderPackages |> Seq.map (fun entry -> entry.Key) |> Array.ofSeq do
            VulkanRenderer3d.tryLoadRenderPackage packageName renderer
    
    static member private sortForwardSurfaces
        eyeCenter
        (surfaces : struct (single * single * Matrix4x4 * bool * Presence * Box2 * MaterialProperties * Matrix4x4 array voption * PhysicallyBasedSurface * DepthTest) List)
        (forwardSurfacesComparer : IComparer<struct (single * single * Matrix4x4 * bool * Presence * Box2 * MaterialProperties * Matrix4x4 array voption * PhysicallyBasedSurface * DepthTest * single * int)>)
        (forwardSurfacesSortBuffer : struct (single * single * Matrix4x4 * bool * Presence * Box2 * MaterialProperties * Matrix4x4 array voption * PhysicallyBasedSurface * DepthTest * single * int) List) =
        for i in 0 .. dec surfaces.Count do
            let struct (subsort, sort, model, castShadow, presence, texCoordsOffset, properties, boneTransformsOpt, surface, depthTest) = surfaces[i]
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
         albedoMetadata : TextureMetadata,
         properties,
         orientUp,
         planar,
         shadowOffset,
         billboardSurface : PhysicallyBasedSurface,
         depthTest,
         renderType,
         renderPass,
         renderTasks,
         _ : VulkanRenderer3d) =

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
        | ShadowPass (_, _, _, _, shadowRotation, _) ->

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
                        let up = Vector3.UnitY
                        let camRight = Vector3.Transform (Vector3.UnitX, eyeRotation)
                        let rightFlat = Vector3 (camRight.X, 0.0f, camRight.Z)
                        let right = if rightFlat.LengthSquared () > 0.000001f then Vector3.Normalize rightFlat else Vector3.UnitX
                        let forwardCandidate = Vector3.Cross (right, up)
                        let toCamera = eyeCenter - model.Translation
                        let below = Vector3.Dot (forwardCandidate, toCamera) >= 0.0f
                        let forward = if below then forwardCandidate else Vector3.Cross (up, right)
                        (right, up, forward)
                        |> Matrix4x4.CreateRotation
                        |> Matrix4x4.Transpose

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
         surface : PhysicallyBasedSurface,
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
            | ValueNone -> VulkanRenderer3d.getRenderTasks renderPass renderer
        
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
        match VulkanRenderer3d.tryGetRenderAsset staticModel renderer with
        | ValueSome renderAsset ->
            match renderAsset with
            | StaticModelAsset (_, modelAsset) ->
                if surfaceIndex > -1 && surfaceIndex < modelAsset.Surfaces.Length then
                    let surface = modelAsset.Surfaces[surfaceIndex]
                    let surface = // OPTIMIZATION: apply surface material only if effective.
                        if material <> Material.empty then
                            let surfaceMaterial = VulkanRenderer3d.applySurfaceMaterial (&material, &surface.SurfaceMaterial, renderer)
                            { surface with SurfaceMaterial = surfaceMaterial }
                        else surface
                    VulkanRenderer3d.categorizeStaticModelSurface (&model, castShadow, presence, &insetOpt, &properties, surface, depthTest, renderType, renderPass, ValueNone, renderer)
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
         renderPass : RenderPass,
         renderTasks : RenderTasks,
         renderer) =
        match VulkanRenderer3d.tryGetRenderAsset staticModel renderer with
        | ValueSome renderAsset ->
            match renderAsset with
            | StaticModelAsset (_, modelAsset) ->
                if surfaceIndex > -1 && surfaceIndex < modelAsset.Surfaces.Length then
                    let surface = modelAsset.Surfaces[surfaceIndex]
                    let surface = // OPTIMIZATION: apply surface material only if effective.
                        if material <> Material.empty then
                            let surfaceMaterial = VulkanRenderer3d.applySurfaceMaterial (&material, &surface.SurfaceMaterial, renderer)
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
                                | ShadowPass (_, _, shadowLightType, _, _, shadowFrustum) ->
                                    if castShadow then // TODO: see if we should check for CastShadow when constructing the pre-batch.
                                        let shadowFrustumInteriorOpt = if LightType.shouldShadowInterior shadowLightType then ValueSome shadowFrustum else ValueNone
                                        Presence.intersects3d shadowFrustumInteriorOpt shadowFrustum shadowFrustum false presence bounds
                                    else false
                                | ReflectionPass (_, reflFrustum) -> Presence.intersects3d ValueNone reflFrustum reflFrustum false presence bounds
                                | NormalPass -> Presence.intersects3d (ValueSome frustumInterior) frustumExterior frustumImposter false presence bounds
                            if unculled then
                                renderTasks.Forward.Add struct (subsort, sort, model, castShadow, presence, insetOpt, properties, ValueNone, surface, depthTest)
            | _ -> ()
        | ValueNone -> ()

    static member private categorizeStaticModel
        (frustumInterior : Frustum,
         frustumExterior : Frustum,
         frustumImposter : Frustum,
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
        match VulkanRenderer3d.tryGetRenderAsset staticModel renderer with
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
                        | NormalPass -> Presence.intersects3d (ValueSome frustumInterior) frustumExterior frustumImposter false presence lightBounds
                        | _ -> false
                    if unculled then
                        let coneOuter = match light.LightType with SpotLight (_, coneOuter) -> min coneOuter MathF.TWO_PI | _ -> MathF.TWO_PI
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
                              SortableLightDistance = Single.MaxValue }
                        renderTasks.Lights.Add light
                for surface in modelAsset.Surfaces do
                    let surface = // OPTIMIZATION: apply surface material only if effective.
                        if clipped
                        then { surface with SurfaceMaterial = { surface.SurfaceMaterial with Clipped = clipped }}
                        else surface
                    let surfaceMatrix = if surface.SurfaceMatrixIsIdentity then model else surface.SurfaceMatrix * model
                    let surfaceBounds = surface.SurfaceBounds.Transform surfaceMatrix
                    let presence = PhysicallyBasedSurface.extractPresence presence modelAsset.SceneOpt surface
                    let renderStyle = PhysicallyBasedSurface.extractRenderStyle renderStyle modelAsset.SceneOpt surface
                    let renderType = match renderStyle with Deferred -> DeferredRenderType | Forward (subsort, sort) -> ForwardRenderType (subsort, sort)
                    let ignoreLightMaps = PhysicallyBasedSurface.extractIgnoreLightMaps properties.IgnoreLightMaps modelAsset.SceneOpt surface
                    let properties = if ignoreLightMaps <> properties.IgnoreLightMaps then { properties with IgnoreLightMapsOpt = ValueSome ignoreLightMaps } else properties
                    let finenessOffset = PhysicallyBasedSurface.extractFinenessOffset properties.FinenessOffset modelAsset.SceneOpt surface
                    let properties = if finenessOffset <> properties.FinenessOffset then { properties with FinenessOffsetOpt = ValueSome finenessOffset } else properties
                    let scatterType = PhysicallyBasedSurface.extractScatterType properties.ScatterType modelAsset.SceneOpt surface
                    let properties = if scatterType <> properties.ScatterType then { properties with ScatterTypeOpt = ValueSome scatterType } else properties
                    let unculled =
                        match renderPass with
                        | LightMapPass (_, _) -> true // TODO: see if we have enough context to cull here.
                        | ShadowPass (_, _, shadowLightType, _, _, shadowFrustum) ->
                            let shadowFrustumInteriorOpt = if LightType.shouldShadowInterior shadowLightType then ValueSome shadowFrustum else ValueNone
                            Presence.intersects3d shadowFrustumInteriorOpt shadowFrustum shadowFrustum false presence surfaceBounds
                        | ReflectionPass (_, reflFrustum) -> Presence.intersects3d ValueNone reflFrustum reflFrustum false presence surfaceBounds
                        | NormalPass -> Presence.intersects3d (ValueSome frustumInterior) frustumExterior frustumImposter false presence surfaceBounds
                    if unculled then
                        VulkanRenderer3d.categorizeStaticModelSurface (&surfaceMatrix, castShadow, presence, &insetOpt, &properties, surface, depthTest, renderType, renderPass, ValueSome renderTasks, renderer)
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
        match VulkanRenderer3d.tryGetRenderAsset animatedModel renderer with
        | ValueSome renderAsset ->
            match renderAsset with
            | AnimatedModelAsset modelAsset ->

                // render animated surfaces
                for i in 0 .. dec modelAsset.Surfaces.Length do

                    // compute tex coords offset
                    let surface = modelAsset.Surfaces[i]
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
        match VulkanRenderer3d.tryGetRenderAsset animatedModel renderer with
        | ValueSome renderAsset ->
            match renderAsset with
            | AnimatedModelAsset modelAsset ->

                // render animated surfaces
                for i in 0 .. dec modelAsset.Surfaces.Length do

                    // render animated surfaces
                    let surface = modelAsset.Surfaces[i]
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
        if not (renderer.TerrainGeometries.ContainsKey geometryDescriptor) then

            // attempt to get height map metadata to compute patches
            let heightMapMetadataOpt =
                HeightMap.tryGetMetadata
                    (fun assetTag -> VulkanRenderer3d.tryGetFilePath assetTag renderer)
                    terrainDescriptor.Bounds
                    terrainDescriptor.Tiles
                    terrainDescriptor.HeightMap

            // attempt to extract height map metadata for geometry creation
            match heightMapMetadataOpt with
            | ValueSome heightMapMetadata ->

                // attempt to create geometry data for the entire terrain
                match VulkanRenderer3d.tryCreatePhysicallyBasedTerrainGeometryData geometryDescriptor heightMapMetadata renderer with
                | Some (positionsAndTexCoordses, normals, blendses, tint) ->

                    // attempt to compute patches for the terrain
                    match VulkanRenderer3d.tryComputeTerrainPatches geometryDescriptor heightMapMetadata with
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
                                        positionsAndTexCoordses[i]|]

                            // extract normals
                            let patchNormals =
                                [|for y in 0 .. dec patchResolution.Y do
                                    for x in 0 .. dec patchResolution.X do
                                        let i = (patchOffset.Y + y) * heightMapMetadata.Resolution.X + (patchOffset.X + x)
                                        normals[i]|]

                            // extract blendses
                            let patchBlendses =
                                [|for y in 0 .. dec patchResolution.Y do
                                    for x in 0 .. dec patchResolution.X do
                                        let i = (patchOffset.Y + y) * heightMapMetadata.Resolution.X + (patchOffset.X + x)
                                        [|for j in 0 .. dec 8 do blendses[i, j]|]|]

                            // extract tint
                            let patchTint =
                                [|for y in 0 .. dec patchResolution.Y do
                                    for x in 0 .. dec patchResolution.X do
                                        let i = (patchOffset.Y + y) * heightMapMetadata.Resolution.X + (patchOffset.X + x)
                                        tint[i]|]

                            // compute vertices
                            let vertices =
                                [|for i in 0 .. dec patchPositionsAndTexCoordses.Length do
                                    let struct (p, tc) = patchPositionsAndTexCoordses[i]
                                    let n = patchNormals[i]
                                    let s = patchBlendses
                                    let t = patchTint[i]
                                    yield!
                                        [|p.X; p.Y; p.Z
                                          tc.X; tc.Y
                                          n.X; n.Y; n.Z
                                          t.X; t.Y; t.Z
                                          s[i][0]; s[i][1]; s[i][2]; s[i][3]; s[i][4]; s[i][5]; s[i][6]; s[i][7]|]|]

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
                            let geometry = PhysicallyBased.createPhysicallyBasedTerrainGeometry VkPrimitiveTopology.TriangleList (vertices.AsMemory ()) (indices.AsMemory ()) geometryDescriptor.Bounds (Some renderer.VulkanContext)
                            match renderer.TerrainGeometries.TryGetValue geometryDescriptor with
                            | (true, existingGeometry) -> existingGeometry.Add (patchDescriptor, geometry)
                            | (false, _) -> renderer.TerrainGeometries.Add (geometryDescriptor, dictPlus HashIdentity.Structural [(patchDescriptor, geometry)])

                    // could not create geometry
                    | None -> ()

                // could not compute patches
                | None -> ()

            // could not extract height map metadata
            | ValueNone -> ()

        // attempt to add patch to appropriate render list when visible
        if visible then
            match renderer.TerrainGeometries.TryGetValue geometryDescriptor with
            | (true, patchGeometries) ->
                for entry in patchGeometries do
                    let patchDescriptor = entry.Key
                    let patchGeometry = entry.Value
                    renderTasks.DeferredTerrains.Add struct (terrainDescriptor, patchDescriptor, patchGeometry)
            | (false, _) -> ()

        // mark patch geometry as utilized regardless of visibility (to keep it from being destroyed)
        renderer.TerrainGeometriesUtilized.Add geometryDescriptor |> ignore<bool>

    static member private categorize
        frustumInterior
        frustumExterior
        frustumImposter
        eyeCenter
        eyeRotation
        renderMessages
        renderer =
        for message in renderMessages do
            match message with
            | CreateUserDefinedStaticModel cudsm ->
                VulkanRenderer3d.tryCreateUserDefinedStaticModel cudsm.StaticModelSurfaceDescriptors cudsm.Bounds cudsm.StaticModel renderer
            | DestroyUserDefinedStaticModel dudsm ->
                renderer.UserDefinedStaticModelsToDestroy.Add dudsm.StaticModel 
            | RenderSkyBox rsb ->
                let renderTasks = VulkanRenderer3d.getRenderTasks rsb.RenderPass renderer
                renderTasks.SkyBoxes.Add (rsb.AmbientColor, rsb.AmbientBrightness, rsb.CubeMapColor, rsb.CubeMapBrightness, rsb.CubeMap)
            | RenderLightProbe3d rlp ->
                let renderTasks = VulkanRenderer3d.getRenderTasks rlp.RenderPass renderer
                if renderTasks.LightProbes.ContainsKey rlp.LightProbeId then
                    Log.warnOnce ("Multiple light probe messages coming in with the same id of '" + string rlp.LightProbeId + "'.")
                    renderTasks.LightProbes.Remove rlp.LightProbeId |> ignore<bool>
                renderTasks.LightProbes.Add (rlp.LightProbeId, struct (rlp.Enabled, rlp.Origin, rlp.AmbientColor, rlp.AmbientBrightness, rlp.Bounds))
            | RenderLightMap3d rlm ->
                let renderTasks = VulkanRenderer3d.getRenderTasks rlm.RenderPass renderer
                renderTasks.LightMapRenders.Add rlm.LightProbeId |> ignore<bool>
            | RenderLight3d rl ->
                let direction = rl.Rotation.Down
                let renderTasks = VulkanRenderer3d.getRenderTasks rl.RenderPass renderer
                let coneOuter = match rl.LightType with SpotLight (_, coneOuter) -> min coneOuter MathF.TWO_PI | _ -> MathF.TWO_PI
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
                      SortableLightDistance = Single.MaxValue }
                renderTasks.Lights.Add light
                if rl.DesireShadows then
                    renderer.LightsDesiringShadows[rl.LightId] <- light
            | RenderBillboard rb ->
                let struct (billboardProperties, billboardMaterial) = VulkanRenderer3d.makeBillboardMaterial (&rb.MaterialProperties, &rb.Material, renderer)
                let billboardSurface = PhysicallyBasedSurface.make Array.empty m4Identity (box3 (v3 -0.5f 0.5f -0.5f) v3One) billboardProperties billboardMaterial -1 Assimp.Node.Empty renderer.BillboardGeometry
                let renderTasks = VulkanRenderer3d.getRenderTasks rb.RenderPass renderer
                VulkanRenderer3d.categorizeBillboardSurface (eyeCenter, eyeRotation, rb.ModelMatrix, rb.CastShadow, rb.Presence, rb.InsetOpt, billboardMaterial.AlbedoTexture.TextureMetadata, rb.MaterialProperties, rb.OrientUp, rb.Planar, rb.ShadowOffset, billboardSurface, rb.DepthTest, rb.RenderType, rb.RenderPass, renderTasks, renderer)
            | RenderBillboards rbs ->
                let struct (billboardProperties, billboardMaterial) = VulkanRenderer3d.makeBillboardMaterial (&rbs.MaterialProperties, &rbs.Material, renderer)
                let billboardSurface = PhysicallyBasedSurface.make Array.empty m4Identity (box3 (v3 -0.5f -0.5f -0.5f) v3One) billboardProperties billboardMaterial -1 Assimp.Node.Empty renderer.BillboardGeometry
                let renderTasks = VulkanRenderer3d.getRenderTasks rbs.RenderPass renderer
                for (model, castShadow, presence, insetOpt, orientUp, planar) in rbs.Billboards do
                    VulkanRenderer3d.categorizeBillboardSurface (eyeCenter, eyeRotation, model, castShadow, presence, insetOpt, billboardMaterial.AlbedoTexture.TextureMetadata, rbs.MaterialProperties, orientUp, planar, rbs.ShadowOffset, billboardSurface, rbs.DepthTest, rbs.RenderType, rbs.RenderPass, renderTasks, renderer)
            | RenderBillboardParticles rbps ->
                let struct (billboardProperties, billboardMaterial) = VulkanRenderer3d.makeBillboardMaterial (&rbps.MaterialProperties, &rbps.Material, renderer)
                let renderTasks = VulkanRenderer3d.getRenderTasks rbps.RenderPass renderer
                for particle in rbps.Particles do
                    let billboardMatrix =
                        Matrix4x4.CreateAffine
                            (particle.Transform.Position,
                             particle.Transform.Rotation,
                             particle.Transform.Size * particle.Transform.Scale)
                    let billboardProperties = { billboardProperties with Albedo = billboardProperties.Albedo * particle.Color; Emission = particle.Emission.R }
                    let billboardSurface = PhysicallyBasedSurface.make Array.empty m4Identity box3Zero billboardProperties billboardMaterial -1 Assimp.Node.Empty renderer.BillboardGeometry
                    VulkanRenderer3d.categorizeBillboardSurface (eyeCenter, eyeRotation, billboardMatrix, rbps.CastShadow, rbps.Presence, Option.ofValueOption particle.InsetOpt, billboardMaterial.AlbedoTexture.TextureMetadata, rbps.MaterialProperties, true, false, rbps.ShadowOffset, billboardSurface, rbps.DepthTest, rbps.RenderType, rbps.RenderPass, renderTasks, renderer)
            | RenderStaticModelSurface rsms ->
                let insetOpt = Option.toValueOption rsms.InsetOpt
                VulkanRenderer3d.categorizeStaticModelSurfaceByIndex (&rsms.ModelMatrix, rsms.CastShadow, rsms.Presence, &insetOpt, &rsms.MaterialProperties, &rsms.Material, rsms.StaticModel, rsms.SurfaceIndex, rsms.DepthTest, rsms.RenderType, rsms.RenderPass, renderer)
            | RenderStaticModelSurfacePreBatch rsmsb ->
                let renderPass = rsmsb.RenderPass
                let renderTasks = VulkanRenderer3d.getRenderTasks renderPass renderer
                VulkanRenderer3d.categorizeStaticModelSurfacePreBatch (rsmsb.StaticModelSurfacePreBatch.PreBatchId, rsmsb.StaticModelSurfacePreBatch.StaticModelSurfaces, rsmsb.StaticModelSurfacePreBatch.Material, rsmsb.StaticModelSurfacePreBatch.StaticModel, rsmsb.StaticModelSurfacePreBatch.SurfaceIndex, rsmsb.StaticModelSurfacePreBatch.DepthTest, rsmsb.StaticModelSurfacePreBatch.RenderType, frustumInterior, frustumExterior, frustumImposter, renderPass, renderTasks, renderer)
            | RenderStaticModelSurfacePreBatches rsmsbs ->
                let renderPass = rsmsbs.RenderPass
                let renderTasks = VulkanRenderer3d.getRenderTasks renderPass renderer
                for preBatch in rsmsbs.StaticModelSurfacePreBatches do
                    VulkanRenderer3d.categorizeStaticModelSurfacePreBatch (preBatch.PreBatchId, preBatch.StaticModelSurfaces, preBatch.Material, preBatch.StaticModel, preBatch.SurfaceIndex, preBatch.DepthTest, preBatch.RenderType, frustumInterior, frustumExterior, frustumImposter, renderPass, renderTasks, renderer)
            | RenderStaticModel rsm ->
                let insetOpt = Option.toValueOption rsm.InsetOpt
                let renderTasks = VulkanRenderer3d.getRenderTasks rsm.RenderPass renderer
                VulkanRenderer3d.categorizeStaticModel (frustumInterior, frustumExterior, frustumImposter, &rsm.ModelMatrix, rsm.CastShadow, rsm.Presence, &insetOpt, &rsm.MaterialProperties, rsm.StaticModel, rsm.Clipped, rsm.DepthTest, rsm.RenderType, rsm.RenderPass, renderTasks, renderer)
            | RenderStaticModels rsms ->
                let renderTasks = VulkanRenderer3d.getRenderTasks rsms.RenderPass renderer
                for (model, castShadow, presence, insetOpt, properties) in rsms.StaticModels do // TODO: see if these should be struct tuples.
                    let insetOpt = Option.toValueOption insetOpt
                    VulkanRenderer3d.categorizeStaticModel (frustumInterior, frustumExterior, frustumImposter, &model, castShadow, presence, &insetOpt, &properties, rsms.StaticModel, rsms.Clipped, rsms.DepthTest, rsms.RenderType, rsms.RenderPass, renderTasks, renderer)
            | RenderCachedStaticModelSurface csmsm ->
                VulkanRenderer3d.categorizeStaticModelSurfaceByIndex (&csmsm.CachedStaticModelSurfaceMatrix, csmsm.CachedStaticModelSurfaceCastShadow, csmsm.CachedStaticModelSurfacePresence, &csmsm.CachedStaticModelSurfaceInsetOpt, &csmsm.CachedStaticModelSurfaceMaterialProperties, &csmsm.CachedStaticModelSurfaceMaterial, csmsm.CachedStaticModelSurfaceModel, csmsm.CachedStaticModelSurfaceIndex, csmsm.CachedStaticModelSurfaceDepthTest, csmsm.CachedStaticModelSurfaceRenderType, csmsm.CachedStaticModelSurfaceRenderPass, renderer)
            | RenderCachedStaticModel csmm ->
                let renderTasks = VulkanRenderer3d.getRenderTasks csmm.CachedStaticModelRenderPass renderer
                VulkanRenderer3d.categorizeStaticModel (frustumInterior, frustumExterior, frustumImposter, &csmm.CachedStaticModelMatrix, csmm.CachedStaticModelCastShadow, csmm.CachedStaticModelPresence, &csmm.CachedStaticModelInsetOpt, &csmm.CachedStaticModelMaterialProperties, csmm.CachedStaticModel, csmm.CachedStaticModelClipped, csmm.CachedStaticModelDepthTest, csmm.CachedStaticModelRenderType, csmm.CachedStaticModelRenderPass, renderTasks, renderer)
            | RenderUserDefinedStaticModel rudsm ->
                let insetOpt = Option.toValueOption rudsm.InsetOpt
                let assetTag = asset Assets.Default.PackageName Gen.name // TODO: see if we should instead use a specialized package for temporary assets like these.
                VulkanRenderer3d.tryCreateUserDefinedStaticModel rudsm.StaticModelSurfaceDescriptors rudsm.Bounds assetTag renderer
                let renderTasks = VulkanRenderer3d.getRenderTasks rudsm.RenderPass renderer
                VulkanRenderer3d.categorizeStaticModel (frustumInterior, frustumExterior, frustumImposter, &rudsm.ModelMatrix, rudsm.CastShadow, rudsm.Presence, &insetOpt, &rudsm.MaterialProperties, assetTag, rudsm.Clipped, rudsm.DepthTest, rudsm.RenderType, rudsm.RenderPass, renderTasks, renderer)
                renderer.UserDefinedStaticModelsToDestroy.Add assetTag
            | RenderAnimatedModel rsm ->
                let insetOpt = Option.toValueOption rsm.InsetOpt
                let renderTasks = VulkanRenderer3d.getRenderTasks rsm.RenderPass renderer
                VulkanRenderer3d.categorizeAnimatedModel (&rsm.ModelMatrix, rsm.CastShadow, rsm.Presence, &insetOpt, &rsm.MaterialProperties, rsm.BoneTransforms, rsm.AnimatedModel, rsm.SubsortOffsets, rsm.DualRenderedSurfaceIndices, rsm.DepthTest, rsm.RenderType, renderTasks, renderer)
            | RenderAnimatedModels rams ->
                let renderTasks = VulkanRenderer3d.getRenderTasks rams.RenderPass renderer
                VulkanRenderer3d.categorizeAnimatedModels (rams.AnimatedModels, rams.BoneTransforms, rams.AnimatedModel, rams.SubsortOffsets, rams.DualRenderedSurfaceIndices, rams.DepthTest, rams.RenderType, renderTasks, renderer)
            | RenderCachedAnimatedModel camm ->
                let renderTasks = VulkanRenderer3d.getRenderTasks camm.CachedAnimatedModelRenderPass renderer
                VulkanRenderer3d.categorizeAnimatedModel (&camm.CachedAnimatedModelMatrix, camm.CachedAnimatedModelCastShadow, camm.CachedAnimatedModelPresence, &camm.CachedAnimatedModelInsetOpt, &camm.CachedAnimatedModelMaterialProperties, camm.CachedAnimatedModelBoneTransforms, camm.CachedAnimatedModel, camm.CachedAnimatedModelSubsortOffsets, camm.CachedAnimatedModelDualRenderedSurfaceIndices, camm.CachedAnimatedModelDepthTest, camm.CachedAnimatedModelRenderType, renderTasks, renderer)
            | RenderTerrain rt ->
                let renderTasks = VulkanRenderer3d.getRenderTasks rt.RenderPass renderer
                VulkanRenderer3d.categorizeTerrain (rt.Visible, rt.TerrainDescriptor, renderTasks, renderer)
            | ConfigureLighting3d l3c ->
                if renderer.LightingConfig <> l3c then renderer.LightingConfigChanged <- true
                renderer.LightingConfig <- l3c
            | ConfigureRenderer3d r3c ->
                if renderer.RendererConfig <> r3c then renderer.RendererConfigChanged <- true
                renderer.RendererConfig <- r3c
            | LoadRenderPackage3d packageName ->
                VulkanRenderer3d.handleLoadRenderPackage packageName renderer
            | UnloadRenderPackage3d packageName ->
                VulkanRenderer3d.handleUnloadRenderPackage packageName renderer
            | ReloadRenderAssets3d ->
                renderer.ReloadAssetsRequested <- true

    static member private beginPhysicallyBasedShadowSurfaces
        cubeMapFace eyeCenter view projection lightShadowExponent colorClearValue colorAttachment depthAttachment resolution renderPassIndex pipeline renderer =
        PhysicallyBased.beginPhysicallyBasedShadowSurfaces
            cubeMapFace eyeCenter view projection lightShadowExponent colorClearValue colorAttachment depthAttachment resolution renderPassIndex pipeline renderer.VulkanContext

    static member private renderPhysicallyBasedShadowSurfaces
        bones (parameters : struct (Matrix4x4 * bool * Presence * Box2 * MaterialProperties) List) (surface : PhysicallyBasedSurface)
        uniformsDescriptorSet pipeline renderer =

        // ensure we have a large enough instance fields array
        let mutable length = renderer.InstanceFields.Length
        while parameters.Count * Constants.Render.InstanceFieldCount > length do length <- length * 2
        if renderer.InstanceFields.Length < length then
            renderer.InstanceFields <- Array.zeroCreate<single> length

        // blit parameters to instance fields
        for i in 0 .. dec parameters.Count do
            let struct (model, _, _, _, _) = parameters[i]
            model.ToArray (renderer.InstanceFields, i * Constants.Render.InstanceFieldCount)

        // draw deferred surfaces
        PhysicallyBased.drawPhysicallyBasedShadowSurfaces
            bones parameters.Count renderer.InstanceFields surface.SurfaceMaterial surface.PhysicallyBasedGeometry
            uniformsDescriptorSet pipeline renderer.VulkanContext

        // track geometry instancing
        renderer.GeometryInstanced.Add surface.PhysicallyBasedGeometry |> ignore<bool>

    static member private renderPhysicallyBasedShadowSurfacePreBatch
        shadowLightType shadowFrustum bones (parameters : (Matrix4x4 * bool * Presence * Box2 * MaterialProperties * Box3) array) (surface : PhysicallyBasedSurface)
        uniformsDescriptorSet pipeline renderer =

        // ensure we have a large enough instance fields array
        let mutable length = renderer.InstanceFields.Length
        while parameters.Length * Constants.Render.InstanceFieldCount > length do length <- length * 2
        if renderer.InstanceFields.Length < length then
            renderer.InstanceFields <- Array.zeroCreate<single> length

        // blit parameters to instance fields
        let mutable i = 0
        for j in 0 .. dec parameters.Length do
            let (model, castShadow, presence, _, _, bounds) = parameters[j]
            let unculled =
                castShadow &&
                let shadowFrustumInteriorOpt = if LightType.shouldShadowInterior shadowLightType then ValueSome shadowFrustum else ValueNone
                Presence.intersects3d shadowFrustumInteriorOpt shadowFrustum shadowFrustum false presence bounds
            if unculled then
                model.ToArray (renderer.InstanceFields, i * Constants.Render.InstanceFieldCount)
                i <- inc i

        // draw shadow surfaces
        PhysicallyBased.drawPhysicallyBasedShadowSurfaces
            bones i renderer.InstanceFields surface.SurfaceMaterial surface.PhysicallyBasedGeometry
            uniformsDescriptorSet pipeline renderer.VulkanContext

        // track geometry instancing
        renderer.GeometryInstanced.Add surface.PhysicallyBasedGeometry |> ignore<bool>

        // fin
        i

    static member private endPhysicallyBasedShadowSurfaces pipeline context =
        PhysicallyBased.endPhysicallyBasedShadowSurfaces pipeline context

    static member private beginPhysicallyBasedDeferredSurfaces
        eyeCenter view projection materialSampler colorAttachments depthAttachment resolution renderPassIndex pipeline renderer =
        PhysicallyBased.beginPhysicallyBasedDeferredSurfaces
            eyeCenter view projection materialSampler colorAttachments depthAttachment resolution renderPassIndex pipeline renderer.VulkanContext

    static member private renderPhysicallyBasedDeferredSurfaces
        bones (parameters : struct (Matrix4x4 * bool * Presence * Box2 * MaterialProperties) List) (surface : PhysicallyBasedSurface)
        eyeDescriptorSet samplerDescriptorSet pipeline renderer =
                                                                      
        // ensure we have a large enough instance fields array
        let mutable length = renderer.InstanceFields.Length
        while parameters.Count * Constants.Render.InstanceFieldCount > length do length <- length * 2
        if renderer.InstanceFields.Length < length then
            renderer.InstanceFields <- Array.zeroCreate<single> length

        // blit parameters to instance fields
        for i in 0 .. dec parameters.Count do
            let struct (model, _, presence, texCoordsOffset, properties) = parameters[i]
            model.ToArray (renderer.InstanceFields, i * Constants.Render.InstanceFieldCount)
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 16] <- texCoordsOffset.Min.X
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 16 + 1] <- texCoordsOffset.Min.Y
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 16 + 2] <- texCoordsOffset.Min.X + texCoordsOffset.Size.X
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 16 + 3] <- texCoordsOffset.Min.Y + texCoordsOffset.Size.Y
            let albedo = match properties.AlbedoOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Albedo
            let roughness = match properties.RoughnessOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Roughness
            let metallic = match properties.MetallicOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Metallic
            let ambientOcclusion = match properties.AmbientOcclusionOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.AmbientOcclusion
            let emission = match properties.EmissionOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Emission
            let height = match properties.HeightOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Height
            let ignoreLightMaps = match properties.IgnoreLightMapsOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.IgnoreLightMaps
            let finenessOffset = match properties.FinenessOffsetOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.FinenessOffset
            let scatterType = match properties.ScatterTypeOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.ScatterType
            let clearCoat = match properties.ClearCoatOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.ClearCoat
            let clearCoatRoughness = match properties.ClearCoatRoughnessOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.ClearCoatRoughness
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 20] <- albedo.R
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 20 + 1] <- albedo.G
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 20 + 2] <- albedo.B
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 20 + 3] <- albedo.A
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 24] <- roughness
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 24 + 1] <- metallic
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 24 + 2] <- ambientOcclusion
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 24 + 3] <- emission
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 28] <- surface.SurfaceMaterial.AlbedoTexture.TextureMetadata.TextureTexelHeight * height
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 29] <- if ignoreLightMaps then 1.0f else 0.0f
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 30] <- presence.DepthCutoff
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 31] <- 0.0f // free
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 32] <- finenessOffset
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 33] <- scatterType.Enumerate
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 34] <- 0.0f // free
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 35] <- 0.0f // free
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 36] <- clearCoat
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 37] <- clearCoatRoughness

        // draw deferred surfaces
        PhysicallyBased.drawPhysicallyBasedDeferredSurfaces
            bones parameters.Count renderer.InstanceFields surface.SurfaceMaterial surface.PhysicallyBasedGeometry
            eyeDescriptorSet samplerDescriptorSet pipeline renderer.VulkanContext

        // track geometry instancing
        renderer.GeometryInstanced.Add surface.PhysicallyBasedGeometry |> ignore<bool>

    static member private renderPhysicallyBasedDeferredSurfacePreBatch
        frustumInterior frustumExterior frustumImposter renderPass bones (parameters : (Matrix4x4 * bool * Presence * Box2 * MaterialProperties * Box3) array) (surface : PhysicallyBasedSurface)
        eyeDescriptorSet samplerDescriptorSet pipeline renderer =

        // ensure we have a large enough instance fields array
        let mutable length = renderer.InstanceFields.Length
        while parameters.Length * Constants.Render.InstanceFieldCount > length do length <- length * 2
        if renderer.InstanceFields.Length < length then
            renderer.InstanceFields <- Array.zeroCreate<single> length

        // blit parameters to instance fields
        let mutable i = 0
        for j in 0 .. dec parameters.Length do
            let (model, _, presence, texCoordsOffset, properties, bounds) = parameters[j]
            let unculled =
                match renderPass with
                | LightMapPass (_, _) -> true // TODO: see if we have enough context to cull here.
                | ShadowPass (_, _, shadowLightType, _, _, shadowFrustum) ->
                    let shadowFrustumInteriorOpt = if LightType.shouldShadowInterior shadowLightType then ValueSome shadowFrustum else ValueNone
                    Presence.intersects3d shadowFrustumInteriorOpt shadowFrustum shadowFrustum false presence bounds
                | ReflectionPass (_, reflFrustum) -> Presence.intersects3d ValueNone reflFrustum reflFrustum false presence bounds
                | NormalPass -> Presence.intersects3d (ValueSome frustumInterior) frustumExterior frustumImposter false presence bounds
            if unculled then
                model.ToArray (renderer.InstanceFields, i * Constants.Render.InstanceFieldCount)
                renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 16] <- texCoordsOffset.Min.X
                renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 16 + 1] <- texCoordsOffset.Min.Y
                renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 16 + 2] <- texCoordsOffset.Min.X + texCoordsOffset.Size.X
                renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 16 + 3] <- texCoordsOffset.Min.Y + texCoordsOffset.Size.Y
                let albedo = match properties.AlbedoOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Albedo
                let roughness = match properties.RoughnessOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Roughness
                let metallic = match properties.MetallicOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Metallic
                let ambientOcclusion = match properties.AmbientOcclusionOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.AmbientOcclusion
                let emission = match properties.EmissionOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Emission
                let height = match properties.HeightOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Height
                let ignoreLightMaps = match properties.IgnoreLightMapsOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.IgnoreLightMaps
                let finenessOffset = match properties.FinenessOffsetOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.FinenessOffset
                let scatterType = match properties.ScatterTypeOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.ScatterType
                let clearCoat = match properties.ClearCoatOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.ClearCoat
                let clearCoatRoughness = match properties.ClearCoatRoughnessOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.ClearCoatRoughness
                renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 20] <- albedo.R
                renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 20 + 1] <- albedo.G
                renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 20 + 2] <- albedo.B
                renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 20 + 3] <- albedo.A
                renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 24] <- roughness
                renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 24 + 1] <- metallic
                renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 24 + 2] <- ambientOcclusion
                renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 24 + 3] <- emission
                renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 28] <- surface.SurfaceMaterial.AlbedoTexture.TextureMetadata.TextureTexelHeight * height
                renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 29] <- if ignoreLightMaps then 1.0f else 0.0f
                renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 30] <- presence.DepthCutoff
                renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 31] <- 0.0f // free
                renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 32] <- finenessOffset
                renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 33] <- scatterType.Enumerate
                renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 34] <- 0.0f // free
                renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 35] <- 0.0f // free
                renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 36] <- clearCoat
                renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 37] <- clearCoatRoughness
                i <- inc i

        // draw deferred surfaces
        PhysicallyBased.drawPhysicallyBasedDeferredSurfaces
            bones i renderer.InstanceFields surface.SurfaceMaterial surface.PhysicallyBasedGeometry
            eyeDescriptorSet samplerDescriptorSet pipeline renderer.VulkanContext

        // track geometry instancing
        renderer.GeometryInstanced.Add surface.PhysicallyBasedGeometry |> ignore<bool>

        // fin
        i

    static member private endPhysicallyBasedDeferredSurfaces pipeline context =
        PhysicallyBased.endPhysicallyBasedDeferredSurfaces pipeline context

    static member private beginPhysicallyBasedForwardSurfaces
        eyeCenter view projection lightCutoffMargin lightAmbientColor lightAmbientBrightness lightAmbientBoostCutoff lightAmbientBoostScalar lightShadowSamples lightShadowBias lightShadowSampleScalar lightShadowExponent lightShadowDensity
        fogEnabled fogType fogStart fogFinish fogDensity fogColor ssvfEnabled ssvfIntensity ssvfSteps ssvfAsymmetry ssrrEnabled ssrrIntensity ssrrDetail ssrrRefinementsMax ssrrRayThickness ssrrDistanceCutoff ssrrDistanceCutoffMargin ssrrEdgeHorizontalMargin ssrrEdgeVerticalMargin shadowNear
        depthTexture colorTexture brdfTexture irradianceMap environmentFilterMap unfilteredSampler filteredSampler materialSampler colorAttachment depthAttachment resolution renderPass pipeline context =
        PhysicallyBased.beginPhysicallyBasedForwardSurfaces
            eyeCenter view projection lightCutoffMargin lightAmbientColor lightAmbientBrightness lightAmbientBoostCutoff lightAmbientBoostScalar lightShadowSamples lightShadowBias lightShadowSampleScalar lightShadowExponent lightShadowDensity
            fogEnabled fogType fogStart fogFinish fogDensity fogColor ssvfEnabled ssvfIntensity ssvfSteps ssvfAsymmetry ssrrEnabled ssrrIntensity ssrrDetail ssrrRefinementsMax ssrrRayThickness ssrrDistanceCutoff ssrrDistanceCutoffMargin ssrrEdgeHorizontalMargin ssrrEdgeVerticalMargin shadowNear
            depthTexture colorTexture brdfTexture irradianceMap environmentFilterMap unfilteredSampler filteredSampler materialSampler colorAttachment depthAttachment resolution renderPass pipeline context

    static member private renderPhysicallyBasedForwardSurfaces
        bonesArrays (parameters : struct (Matrix4x4 * Presence * Box2 * MaterialProperties) SList)
        irradianceMaps environmentFilterMaps shadowTextureArray shadowMaps shadowCascades lightMapOrigins lightMapMins lightMapSizes lightMapAmbientColors lightMapAmbientBrightnesses lightMapsCount lightMapSingletonBlendMargin
        lightOrigins lightDirections lightColors lightBrightnesses lightAttenuationLinears lightAttenuationQuadratics lightCutoffs lightTypes lightConeInners lightConeOuters lightDesireFogs lightShadowIndices lightsCount shadowMatricesFlipped
        (surface : PhysicallyBasedSurface) depthTest blending uniformsDescriptorSet samplersDescriptorSet pipeline renderer =

        // ensure we have a large enough instance fields array
        let mutable length = renderer.InstanceFields.Length
        while parameters.Length * Constants.Render.InstanceFieldCount > length do length <- length * 2
        if renderer.InstanceFields.Length < length then
            renderer.InstanceFields <- Array.zeroCreate<single> length

        // blit parameters to instance fields
        for i in 0 .. dec parameters.Length do
            let struct (model, presence, texCoordsOffset, properties) = parameters[i]
            model.ToArray (renderer.InstanceFields, i * Constants.Render.InstanceFieldCount)
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 16] <- texCoordsOffset.Min.X
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 16 + 1] <- texCoordsOffset.Min.Y
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 16 + 2] <- texCoordsOffset.Min.X + texCoordsOffset.Size.X
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 16 + 3] <- texCoordsOffset.Min.Y + texCoordsOffset.Size.Y
            let albedo = match properties.AlbedoOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Albedo
            let roughness = match properties.RoughnessOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Roughness
            let metallic = match properties.MetallicOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Metallic
            let ambientOcclusion = match properties.AmbientOcclusionOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.AmbientOcclusion
            let emission = match properties.EmissionOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Emission
            let height = match properties.HeightOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.Height
            let ignoreLightMaps = match properties.IgnoreLightMapsOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.IgnoreLightMaps
            let opaqueDistance = match properties.OpaqueDistanceOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.OpaqueDistance
            let subsurfaceCutoff = match properties.SubsurfaceCutoffOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.SubsurfaceCutoff
            let subsurfaceCutoffMargin = match properties.SubsurfaceCutoffMarginOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.SubsurfaceCutoffMargin
            let specularScalar = match properties.SpecularScalarOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.SpecularScalar
            let refractiveIndex = match properties.RefractiveIndexOpt with ValueSome value -> value | ValueNone -> surface.SurfaceMaterialProperties.RefractiveIndex
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 20] <- albedo.R
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 20 + 1] <- albedo.G
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 20 + 2] <- albedo.B
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 20 + 3] <- albedo.A
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 24] <- roughness
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 24 + 1] <- metallic
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 24 + 2] <- ambientOcclusion
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 24 + 3] <- emission
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 28] <- surface.SurfaceMaterial.AlbedoTexture.TextureMetadata.TextureTexelHeight * height
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 29] <- if ignoreLightMaps then 1.0f else 0.0f
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 30] <- presence.DepthCutoff
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 31] <- opaqueDistance
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 32] <- subsurfaceCutoff
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 33] <- subsurfaceCutoffMargin
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 34] <- specularScalar
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 35] <- refractiveIndex
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 36] <- 0.0f // free
            renderer.InstanceFields[i * Constants.Render.InstanceFieldCount + 37] <- 0.0f // free

        // make these bindings mutable for passing by ref
        let mutable (uniformsDescriptorSet, samplersDescriptorSet) =
            (uniformsDescriptorSet, samplersDescriptorSet)

        // draw forward surfaces
        PhysicallyBased.drawPhysicallyBasedForwardSurfaces
            bonesArrays parameters.Length renderer.InstanceFields
            irradianceMaps environmentFilterMaps shadowTextureArray shadowMaps shadowCascades lightMapOrigins lightMapMins lightMapSizes lightMapAmbientColors lightMapAmbientBrightnesses lightMapsCount lightMapSingletonBlendMargin
            lightOrigins lightDirections lightColors lightBrightnesses lightAttenuationLinears lightAttenuationQuadratics lightCutoffs lightTypes lightConeInners lightConeOuters lightDesireFogs lightShadowIndices lightsCount shadowMatricesFlipped
            surface.SurfaceMaterial surface.PhysicallyBasedGeometry depthTest blending uniformsDescriptorSet samplersDescriptorSet pipeline renderer.VulkanContext

        // track geometry instancing
        renderer.GeometryInstanced.Add surface.PhysicallyBasedGeometry |> ignore<bool>

    static member private endPhysicallyBasedForwardSurfaces pipeline context =
        PhysicallyBased.endPhysicallyBasedForwardSurfaces pipeline context

    /// Pre-render 3d surfaces.
    static member preRender
        frustumInterior
        frustumExterior
        frustumImposter
        eyeCenter
        eyeRotation
        (renderMessages : _ List)
        (renderer : VulkanRenderer3d) =

        // begin instance buffer frames as requested on previous frame
        for geometry in renderer.GeometryInstanced do
            VulkanBuffer.beginFrame geometry.InstanceBuffer
        renderer.GeometryInstanced.Clear ()

        // begin pipeline frames
        Pipeline.beginFrame renderer.SkyBoxPipeline.Pipeline
        Pipeline.beginFrame renderer.IrradiancePipeline.Pipeline
        Pipeline.beginFrame renderer.EnvironmentFilterPipeline.Pipeline
        PhysicallyBased.beginPhysicallyBasedPipelines renderer.PhysicallyBasedPipelines

        // categorize messages
        VulkanRenderer3d.categorize frustumInterior frustumExterior frustumImposter eyeCenter eyeRotation renderMessages renderer

    static member private renderLightMaps
        frustumInterior
        frustumExterior
        frustumImposter
        renderer =

        // render light maps as needed
        for (renderPass, renderTasks) in renderer.RenderPasses.Pairs do

            // fallback light map pre-pass
            match VulkanRenderer3d.getLastSkyBoxOpt renderPass renderer |> __c with
            | Some (_, _, cubeMap, irradianceAndEnvironmentMapsOptRef : (Texture * Texture) option ref) ->

                // render fallback irradiance and env filter maps
                if Option.isNone irradianceAndEnvironmentMapsOptRef.Value then

                    // special handling for flipped-ness of fallback
                    let fallbackFlipped = false

                    // render fallback irradiance map
                    let irradianceMap =
                        LightMap.createIrradianceMap
                            Constants.Render.IrradianceMapResolution
                            (CubeMapSurface.make (not fallbackFlipped) cubeMap renderer.CubeMapGeometry)
                            renderer.FilteredSampler
                            renderer.IrradianceMap.InternalFormat
                            renderer.IrradiancePipeline
                            (fun () -> renderer.VulkanContext.RenderCommandBuffer)
                            (fun () -> VulkanContext.advanceRenderCommandBuffer renderer.VulkanContext)
                            renderer.VulkanContext

                    // render fallback env filter map
                    let environmentFilterMap =
                        LightMap.createEnvironmentFilterMap
                            Constants.Render.EnvironmentFilterResolution
                            (CubeMapSurface.make fallbackFlipped cubeMap renderer.CubeMapGeometry)
                            renderer.FilteredSampler
                            renderer.EnvironmentFilterMap.InternalFormat
                            renderer.EnvironmentFilterPipeline
                            (fun () -> renderer.VulkanContext.RenderCommandBuffer)
                            (fun () -> VulkanContext.advanceRenderCommandBuffer renderer.VulkanContext)
                            renderer.VulkanContext

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
                        TextureDumpster.toss lightMap.IrradianceMap renderer.TextureDumpster
                        TextureDumpster.toss lightMap.EnvironmentFilterMap renderer.TextureDumpster
                        renderer.LightMaps.Remove lightProbeId |> ignore<bool>
                    | (false, _) -> ()
                            
                    // create new light map
                    match renderTasks.LightProbes.TryGetValue lightProbeId with
                    | (true, struct (lightProbeEnabled, lightProbeOrigin, lightProbeAmbientColor, lightProbeAmbientBrightness, lightProbeBounds)) ->

                        // create reflection map (this will be flipped)
                        let reflectionMap =
                            LightMap.createReflectionMap
                                (VulkanRenderer3d.renderGeometry frustumInterior frustumExterior frustumImposter renderPass (VulkanRenderer3d.getRenderTasks renderPass renderer) renderer)
                                Constants.Render.ReflectionMapResolution
                                lightProbeOrigin
                                lightProbeAmbientColor
                                lightProbeAmbientBrightness
                                (fun () -> renderer.VulkanContext.RenderCommandBuffer)
                                renderer.VulkanContext

                        // create irradiance map
                        let irradianceMap =
                            LightMap.createIrradianceMap
                                Constants.Render.IrradianceMapResolution
                                (CubeMapSurface.make false reflectionMap renderer.CubeMapGeometry)
                                renderer.FilteredSampler
                                renderer.IrradianceMap.InternalFormat
                                renderer.IrradiancePipeline
                                (fun () -> renderer.VulkanContext.RenderCommandBuffer)
                                (fun () -> VulkanContext.advanceRenderCommandBuffer renderer.VulkanContext)
                                renderer.VulkanContext

                        // create env filter map
                        let environmentFilterMap =
                            LightMap.createEnvironmentFilterMap
                                Constants.Render.EnvironmentFilterResolution
                                (CubeMapSurface.make true reflectionMap renderer.CubeMapGeometry)
                                renderer.FilteredSampler
                                renderer.EnvironmentFilterMap.InternalFormat
                                renderer.EnvironmentFilterPipeline
                                (fun () -> renderer.VulkanContext.RenderCommandBuffer)
                                (fun () -> VulkanContext.advanceRenderCommandBuffer renderer.VulkanContext)
                                renderer.VulkanContext

                        // create light map
                        let lightMap = LightMap.createLightMap lightProbeEnabled lightProbeOrigin lightProbeAmbientColor lightProbeAmbientBrightness lightProbeBounds irradianceMap environmentFilterMap

                        // add light map to cache
                        renderer.LightMaps[lightProbeId] <- lightMap

                        // be rid of reflection map
                        TextureDumpster.toss reflectionMap renderer.TextureDumpster

                    | (false, _) -> ()
                | _ -> ()

    static member private renderShadow
        lightOrigin
        (lightView : Matrix4x4)
        (lightProjection : Matrix4x4)
        lightFrustum
        lightType
        lightCutoff
        resolution
        colorAttachment
        depthAttachment
        renderTasks
        renderer =

        // grab appropriate shaders
        let (shadowStaticPipeline, shadowAnimatedPipeline, shadowTerrainPipeline) =
            match lightType with
            | PointLight ->
                (renderer.PhysicallyBasedPipelines.ShadowStaticPointPipeline,
                 renderer.PhysicallyBasedPipelines.ShadowAnimatedPointPipeline,
                 renderer.PhysicallyBasedPipelines.ShadowTerrainPointPipeline)
            | SpotLight (_, _) ->
                (renderer.PhysicallyBasedPipelines.ShadowStaticSpotPipeline,
                 renderer.PhysicallyBasedPipelines.ShadowAnimatedSpotPipeline,
                 renderer.PhysicallyBasedPipelines.ShadowTerrainSpotPipeline)
            | DirectionalLight _ | CascadedLight ->
                (renderer.PhysicallyBasedPipelines.ShadowStaticDirectionalPipeline,
                 renderer.PhysicallyBasedPipelines.ShadowAnimatedDirectionalPipeline,
                 renderer.PhysicallyBasedPipelines.ShadowTerrainDirectionalPipeline)

        // begin deferred static surface shadow rendering
        let mutable counted = 0
        let mutable committed = 0
        let mutable uniformsDescriptorSet = Unchecked.defaultof<_>
        let beginBatch = fun clear ->
            let clearColorValueOpt =
                if clear then
                    match lightType with
                    | PointLight | CascadedLight ->
                        Some (VkClearValue (lightCutoff, 0.0f, 0.0f, 0.0f)) // TODO: make derived from constant.
                    | SpotLight _ | DirectionalLight _ ->
                        Some (VkClearValue (1.0f, Single.MaxValue, 0.0f, 0.0f)) // TODO: make derived from constant.
                else None
            uniformsDescriptorSet <-
                VulkanRenderer3d.beginPhysicallyBasedShadowSurfaces
                    lightType.ShadowsUseCubeMap lightOrigin lightView lightProjection renderer.LightingConfig.LightShadowExponent
                    clearColorValueOpt colorAttachment depthAttachment resolution renderer.RenderPassIndex shadowStaticPipeline renderer
        let endBatch = fun () -> VulkanRenderer3d.endPhysicallyBasedShadowSurfaces shadowStaticPipeline renderer.VulkanContext
        let advanceBatch = fun instances ->
            counted <- counted + instances
            let delta = counted - committed
            if delta >= Constants.Vulkan.ShadowSurfaceInstanceThreshold then
                endBatch ()
                beginBatch false
                committed <- counted
        beginBatch true

        // deferred render static surface shadows
        for entry in renderTasks.DeferredStatic do
            VulkanRenderer3d.renderPhysicallyBasedShadowSurfaces [||] entry.Value entry.Key uniformsDescriptorSet shadowStaticPipeline renderer
            advanceBatch entry.Value.Count
        
        // deferred render static surface pre-batches shadows
        for entry in renderTasks.DeferredStaticPreBatches do
            let struct (surface, preBatch) = entry.Value
            advanceBatch $ VulkanRenderer3d.renderPhysicallyBasedShadowSurfacePreBatch lightType lightFrustum [||] preBatch surface uniformsDescriptorSet shadowStaticPipeline renderer

        // deferred render static surface clipped shadows (TODO: consider implementing clipped shadow rendering.)
        for entry in renderTasks.DeferredStaticClipped do
            VulkanRenderer3d.renderPhysicallyBasedShadowSurfaces [||] entry.Value entry.Key uniformsDescriptorSet shadowStaticPipeline renderer
            advanceBatch entry.Value.Count
        
        // deferred render static surface pre-batches clipped shadows (TODO: consider implementing clipped shadow rendering.)
        for entry in renderTasks.DeferredStaticClippedPreBatches do
            let struct (surface, preBatch) = entry.Value
            advanceBatch $ VulkanRenderer3d.renderPhysicallyBasedShadowSurfacePreBatch lightType lightFrustum [||] preBatch surface uniformsDescriptorSet shadowStaticPipeline renderer
        
        // end deferred static surface shadow rendering
        endBatch ()

        // begin shadow animated rendering
        let uniformsDescriptorSet =
            VulkanRenderer3d.beginPhysicallyBasedShadowSurfaces
                lightType.ShadowsUseCubeMap lightOrigin lightView lightProjection renderer.LightingConfig.LightShadowExponent
                None colorAttachment depthAttachment resolution renderer.RenderPassIndex shadowAnimatedPipeline renderer

        // deferred render animated surface shadows
        for entry in renderTasks.DeferredAnimated do
            let surfaceKey = entry.Key
            let parameters = entry.Value
            VulkanRenderer3d.renderPhysicallyBasedShadowSurfaces
                surfaceKey.BoneTransforms parameters surfaceKey.AnimatedSurface uniformsDescriptorSet shadowAnimatedPipeline renderer
        
        // end shadow animated pipeline
        VulkanRenderer3d.endPhysicallyBasedShadowSurfaces shadowAnimatedPipeline renderer.VulkanContext

        // attempt to deferred render terrain shadows
        for struct (descriptor, patchDescriptor, geometry) in renderTasks.DeferredTerrains do
            if lightFrustum.Intersects patchDescriptor.PatchBounds then
                VulkanRenderer3d.renderPhysicallyBasedTerrain
                    lightType.ShadowsUseCubeMap lightOrigin lightView lightProjection
                    renderer.LightingConfig.LightShadowSamples renderer.LightingConfig.LightShadowBias renderer.LightingConfig.LightShadowSampleScalar renderer.LightingConfig.LightShadowExponent renderer.LightingConfig.LightShadowDensity
                    descriptor renderer.MaterialSampler geometry [|colorAttachment|] depthAttachment resolution renderer.RenderPassIndex shadowTerrainPipeline renderer

        // render forward (static and animated) surface shadows
        let mutable counted = 0
        let mutable committed = 0
        let mutable forwardPipeline = shadowStaticPipeline // speculatively start with forward static
        let mutable uniformsDescriptorSet = Unchecked.defaultof<_>
        let beginBatch = fun () ->
            uniformsDescriptorSet <-
                VulkanRenderer3d.beginPhysicallyBasedShadowSurfaces
                    lightType.ShadowsUseCubeMap lightOrigin lightView lightProjection renderer.LightingConfig.LightShadowExponent
                    None colorAttachment depthAttachment resolution renderer.RenderPassIndex forwardPipeline renderer
        let endBatch = fun () -> VulkanRenderer3d.endPhysicallyBasedShadowSurfaces forwardPipeline renderer.VulkanContext
        let checkBatch = fun forwardPipeline' ->
            if refNeq forwardPipeline forwardPipeline' then
                endBatch ()
                forwardPipeline <- forwardPipeline'
                beginBatch ()
                committed <- counted
        let advanceBatch = fun instances ->
            counted <- counted + instances
            let delta = counted - committed
            if delta >= Constants.Vulkan.ShadowSurfaceInstanceThreshold then
                endBatch ()
                beginBatch ()
                committed <- counted
        beginBatch ()
        
        // forward render (static and animated) surface shadows
        for struct (model, castShadow, presence, texCoordsOffset, properties, boneTransformsOpt, surface, _) in renderTasks.ForwardSorted do
            if castShadow then
                let (boneTransforms, forwardPipeline) =
                    match boneTransformsOpt with
                    | ValueNone -> ([||], shadowStaticPipeline)
                    | ValueSome boneTransforms -> (boneTransforms, shadowAnimatedPipeline)
                checkBatch forwardPipeline
                VulkanRenderer3d.renderPhysicallyBasedShadowSurfaces
                    boneTransforms (List ([struct (model, castShadow, presence, texCoordsOffset, properties)]))
                    surface uniformsDescriptorSet forwardPipeline renderer
                advanceBatch 1
        
        // end forward (static and animated) surface shadows rendering
        endBatch ()

        // advance render pass index
        renderer.RenderPassIndex <- inc renderer.RenderPassIndex

    static member private renderShadowTexture
        renderTasks
        renderer
        (lightOrigin : Vector3)
        (lightView : Matrix4x4)
        (lightProjection : Matrix4x4)
        (lightFrustum : Frustum)
        (lightType : LightType)
        (lightCutoff : single)
        (shadowResolution : Vector2i)
        (colorAttachment : VkImageView)
        (depthAttachment : Texture) =

        // send forward surfaces directly to sorted buffer since no sorting is needed for shadows
        for struct (_, _, model, castShadow, presence, texCoordsOffset, properties, boneTransformsOpt, surface, depthTest) in renderTasks.Forward do
            renderTasks.ForwardSorted.Add struct (model, castShadow, presence, texCoordsOffset, properties, boneTransformsOpt, surface, depthTest)

        // actually render shadow
        VulkanRenderer3d.renderShadow lightOrigin lightView lightProjection lightFrustum lightType lightCutoff shadowResolution colorAttachment depthAttachment renderTasks renderer

    static member private renderShadowMapFace
        (lightOrigin : Vector3)
        (lightCutoff : single)
        (shadowView : Matrix4x4)
        (shadowProjection : Matrix4x4)
        (shadowFrustum : Frustum)
        (shadowResolution : Vector2i)
        (colorAttachment : VkImageView)
        (depthAttachment : Texture)
        renderTasks
        renderer =

        // send forward surfaces directly to sorted buffer since no sorting is needed for shadows
        for struct (_, _, model, castShadow, presence, texCoordsOffset, properties, boneTransformsOpt, surface, depthTest) in renderTasks.Forward do
            renderTasks.ForwardSorted.Add struct (model, castShadow, presence, texCoordsOffset, properties, boneTransformsOpt, surface, depthTest)

        // actually render to shadow cube map face
        VulkanRenderer3d.renderShadow lightOrigin shadowView shadowProjection shadowFrustum PointLight lightCutoff shadowResolution colorAttachment depthAttachment renderTasks renderer

    static member private renderShadowCascade
        (lightOrigin : Vector3)
        (lightCutoff : single)
        (shadowView : Matrix4x4)
        (shadowProjection : Matrix4x4)
        (shadowFrustum : Frustum)
        (shadowResolution : Vector2i)
        (colorAttachment : VkImageView)
        (depthAttachment : Texture)
        renderTasks
        renderer =

        // send forward surfaces directly to sorted buffer since no sorting is needed for shadows
        for struct (_, _, model, castShadow, presence, texCoordsOffset, properties, boneTransformsOpt, surface, depthTest) in renderTasks.Forward do
            renderTasks.ForwardSorted.Add struct (model, castShadow, presence, texCoordsOffset, properties, boneTransformsOpt, surface, depthTest)

        // actually render to shadow cube map face
        VulkanRenderer3d.renderShadow lightOrigin shadowView shadowProjection shadowFrustum CascadedLight lightCutoff shadowResolution colorAttachment depthAttachment renderTasks renderer

    static member private renderShadows eyeCenter renderer =

        // sort spot and directional lights according to how they are utilized by shadows
        let normalTasks = VulkanRenderer3d.getRenderTasks NormalPass renderer
        let spotAndDirectionalLightsArray = SortableLight.sortShadowingSpotAndDirectionalLightsIntoArray Constants.Render.ShadowTexturesMax eyeCenter normalTasks.Lights

        // sort spot and directional lights so that shadows that have the possibility of cache reuse come to the front
        // NOTE: this approach has O(n^2) complexity altho perhaps it could be optimized.
        let spotAndDirectionalLightsArray =
            Array.sortBy (fun struct (id, _, _, _, _) ->
                renderer.RenderPasses2.Pairs
                |> Seq.choose (fun (renderPass, renderTasks) -> match renderPass with ShadowPass (id2, indexInfoOpt, _, _, _, _) when id2 = id && indexInfoOpt.IsNone -> renderTasks.ShadowBufferIndexOpt | _ -> None)
                |> Seq.headOrDefault Int32.MaxValue
                :> IComparable) // OPTIMIZATION: boxing here to avoid it downstream.
                spotAndDirectionalLightsArray

        // shadow texture pre-passes
        let mutable shadowTextureIndex = 0
        for struct (lightId, lightOrigin, lightCutoff, lightConeOuter, lightDesireShadows) in spotAndDirectionalLightsArray do
            if renderer.RendererConfig.LightShadowingEnabled && lightDesireShadows = 1 then
                for (renderPass, renderTasks) in renderer.RenderPasses.Pairs do
                    match renderPass with
                    | ShadowPass (shadowLightId, shadowIndexInfoOpt, shadowLightType, _, shadowRotation, shadowFrustum) when
                        lightId = shadowLightId && shadowIndexInfoOpt.IsNone && shadowTextureIndex < Constants.Render.ShadowTexturesMax ->

                        // attempt to set up shadow texture drawing
                        let (shadowOrigin, shadowView, shadowProjection, shadowCutoff, shadowColorTexture, shadowDepthTexture) =
                            match shadowLightType with
                            | SpotLight (_, _) ->
                                let shadowForward = shadowRotation.Down
                                let shadowUp = shadowForward.OrthonormalUp
                                let shadowView = Matrix4x4.CreateLookAt (lightOrigin, lightOrigin + shadowForward, shadowUp)
                                let shadowFov = max (min lightConeOuter Constants.Render.ShadowFovMax) 0.01f
                                let shadowCutoff = max lightCutoff (Constants.Render.NearPlaneDistanceInterior * 2.0f)
                                let shadowProjection = Matrix4x4.CreatePerspectiveFieldOfView (shadowFov, 1.0f, Constants.Render.NearPlaneDistanceInterior, shadowCutoff)
                                let (shadowColorArrayAttachment, shadowDepthArrayAttachment) = renderer.PhysicallyBasedAttachments.ShadowTextureArrayAttachments
                                (lightOrigin, shadowView, shadowProjection, shadowCutoff, shadowColorArrayAttachment, shadowDepthArrayAttachment)
                            | DirectionalLight _ ->
                                let shadowForward = shadowRotation.Down
                                let shadowUp = shadowForward.OrthonormalUp
                                let shadowView = Matrix4x4.CreateLookAt (lightOrigin, lightOrigin + shadowForward, shadowUp)
                                let shadowCutoff = lightCutoff
                                let shadowProjection = Matrix4x4.CreateOrthographic (shadowCutoff * 2.0f, shadowCutoff * 2.0f, -shadowCutoff, shadowCutoff)
                                let (shadowColorArrayAttachment, shadowDepthArrayAttachment) = renderer.PhysicallyBasedAttachments.ShadowTextureArrayAttachments
                                (lightOrigin, shadowView, shadowProjection, shadowCutoff, shadowColorArrayAttachment, shadowDepthArrayAttachment)
                            | PointLight | CascadedLight -> failwithumf ()

                        // draw shadow texture when not cached
                        let shouldDraw =
                            renderer.RendererConfig.LightShadowingEnabled &&
                            match renderer.RenderPasses2.TryGetValue renderPass with
                            | (true, renderTasksCached) ->
                                if Option.contains shadowTextureIndex renderTasksCached.ShadowBufferIndexOpt then
                                    let upToDate = RenderTasks.shadowUpToDate renderer.LightingConfigChanged renderer.RendererConfigChanged renderTasks renderTasksCached
                                    not upToDate
                                else true
                            | (_, _) -> true
                        if shouldDraw then

                            // draw shadow texture
                            let shadowResolution = renderer.GeometryViewport.ShadowTextureResolution
                            Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite shadowColorTexture renderer.VulkanContext.RenderCommandBuffer
                            Texture.recordTransitionLayout DepthAttachmentRead DepthAttachmentWrite shadowDepthTexture renderer.VulkanContext.RenderCommandBuffer
                            VulkanRenderer3d.renderShadowTexture
                                renderTasks renderer shadowOrigin shadowView shadowProjection shadowFrustum
                                shadowLightType shadowCutoff shadowResolution shadowColorTexture.LayerViews[shadowTextureIndex] shadowDepthTexture
                            Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead shadowColorTexture renderer.VulkanContext.RenderCommandBuffer
                            Texture.recordTransitionLayout DepthAttachmentWrite DepthAttachmentRead shadowDepthTexture renderer.VulkanContext.RenderCommandBuffer

                            // filter shadows on the x
                            let gaussianEsmTexture = renderer.PhysicallyBasedAttachments.GaussianEsmAttachment
                            Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite gaussianEsmTexture renderer.VulkanContext.RenderCommandBuffer
                            PhysicallyBased.drawFilterGaussianEsmSurface
                                (v2 (1.0f / single shadowResolution.X) 0.0f) renderer.LightingConfig.LightShadowRadius
                                shadowColorTexture.LayerViews[shadowTextureIndex] renderer.FilteredSampler gaussianEsmTexture.ImageView shadowResolution
                                renderer.QuadGeometry renderer.PhysicallyBasedPipelines.FilterGaussianEsmPipeline renderer.VulkanContext
                            Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead gaussianEsmTexture renderer.VulkanContext.RenderCommandBuffer

                            // filter shadows on the y
                            Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite shadowColorTexture renderer.VulkanContext.RenderCommandBuffer
                            PhysicallyBased.drawFilterGaussianEsmSurface
                                (v2 0.0f (1.0f / single shadowResolution.Y)) renderer.LightingConfig.LightShadowRadius
                                gaussianEsmTexture.ImageView renderer.FilteredSampler shadowColorTexture.LayerViews[shadowTextureIndex] shadowResolution
                                renderer.QuadGeometry renderer.PhysicallyBasedPipelines.FilterGaussianEsmPipeline renderer.VulkanContext
                            Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead shadowColorTexture renderer.VulkanContext.RenderCommandBuffer

                        // remember the utilized index for the next frame
                        renderTasks.ShadowBufferIndexOpt <- Some shadowTextureIndex

                        // update renderer values
                        renderer.ShadowMatricesFlipped[shadowTextureIndex] <- shadowView * shadowProjection.Flipped
                        renderer.LightShadowIndices[lightId] <- shadowTextureIndex

                        // next shadow
                        shadowTextureIndex <- inc shadowTextureIndex

                    | _ -> ()

        // sort point lights according to how they are utilized by shadows
        let pointLightsArray = SortableLight.sortShadowingPointLightsIntoArray Constants.Render.ShadowMapsMax eyeCenter normalTasks.Lights

        // sort point lights so that shadows that have the possibility of cache reuse come to the front
        // NOTE: this approach has O(n^2) complexity altho perhaps it could be optimized.
        let pointLightsArray =
            Array.sortBy (fun struct (id, _, _, _, _) ->
                renderer.RenderPasses2.Pairs
                |> Seq.choose (fun (renderPass, renderTasks) -> match renderPass with ShadowPass (id2, indexInfoOpt, _, _, _, _) when id2 = id && indexInfoOpt.IsSome -> renderTasks.ShadowBufferIndexOpt | _ -> None)
                |> Seq.headOrDefault Int32.MaxValue
                :> IComparable) // OPTIMIZATION: boxing here to avoid it downstream.
                pointLightsArray

        // shadow map pre-passes
        let mutable shadowMapBufferIndex = 0
        for struct (lightId, lightOrigin, lightCutoff, _, lightDesireShadows) in pointLightsArray do
            if renderer.RendererConfig.LightShadowingEnabled && lightDesireShadows = 1 then
                for (renderPass, renderTasks) in renderer.RenderPasses.Pairs do
                    match renderPass with
                    | ShadowPass (shadowLightId, shadowIndexInfoOpt, shadowLightType, _, _, shadowFrustum) when
                        lightId = shadowLightId && shadowIndexInfoOpt.IsSome && shadowMapBufferIndex < Constants.Render.ShadowMapsMax ->
                        match shadowLightType with
                        | PointLight ->

                            // destructure shadow index info
                            let (shadowFace, shadowView, shadowProjection) = shadowIndexInfoOpt.Value

                            // draw shadow map when not cached
                            // NOTE: it's a tiny bit inefficient that we set up and tear down the same shadow map once
                            // per face render here, but probably nothing worth caring about.
                            let shouldDraw =
                                match renderer.RenderPasses2.TryGetValue renderPass with
                                | (true, renderTasksCached) ->
                                    if Option.contains (shadowMapBufferIndex + Constants.Render.ShadowTexturesMax) renderTasksCached.ShadowBufferIndexOpt then
                                        let upToDate = RenderTasks.shadowUpToDate renderer.LightingConfigChanged renderer.RendererConfigChanged renderTasks renderTasksCached
                                        not upToDate
                                    else true
                                | (_, _) -> true
                            if shouldDraw then // TODO: P1: see if we really need to potentially transition layout for each face...
                                let shadowResolution = renderer.GeometryViewport.ShadowMapResolution
                                let (shadowColorTexture, shadowDepthTexture) = renderer.PhysicallyBasedAttachments.ShadowMapAttachmentsArray[shadowMapBufferIndex]
                                Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite shadowColorTexture renderer.VulkanContext.RenderCommandBuffer
                                Texture.recordTransitionLayout DepthAttachmentRead DepthAttachmentWrite shadowDepthTexture renderer.VulkanContext.RenderCommandBuffer
                                VulkanRenderer3d.renderShadowMapFace lightOrigin lightCutoff shadowView shadowProjection shadowFrustum shadowResolution shadowColorTexture.LayerViews[shadowFace] shadowDepthTexture renderTasks renderer
                                Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead shadowColorTexture renderer.VulkanContext.RenderCommandBuffer
                                Texture.recordTransitionLayout DepthAttachmentWrite DepthAttachmentRead shadowDepthTexture renderer.VulkanContext.RenderCommandBuffer

                            // remember the utilized index for the next frame
                            renderTasks.ShadowBufferIndexOpt <- Some (shadowMapBufferIndex + Constants.Render.ShadowTexturesMax)

                            // update renderer values or next shadow
                            // NOTE: this behavior completely DEPENDS on shadow index messages for a shadow map being
                            // received and processed in numerical order.
                            if shadowFace = 0 then
                                renderer.LightShadowIndices[lightId] <- shadowMapBufferIndex + Constants.Render.ShadowTexturesMax
                            elif shadowFace = dec 6 then
                                shadowMapBufferIndex <- inc shadowMapBufferIndex

                        | SpotLight (_, _) | DirectionalLight _ | CascadedLight -> failwithumf ()
                    | _ -> ()

        // sort cascaded lights according to how they are utilized by shadows
        let cascadedLightsArray = SortableLight.sortShadowingCascadedLightsIntoArray Constants.Render.ShadowCascadesMax eyeCenter normalTasks.Lights

        // sort cascaded lights so that shadows that have the possibility of cache reuse come to the front
        // NOTE: this approach has O(n^2) complexity altho perhaps it could be optimized.
        let cascadedLightsArray =
            Array.sortBy (fun struct (id, _, _, _, _) ->
                renderer.RenderPasses2.Pairs
                |> Seq.choose (fun (renderPass, renderTasks) -> match renderPass with ShadowPass (id2, indexInfoOpt, _, _, _, _) when id2 = id && indexInfoOpt.IsSome -> renderTasks.ShadowBufferIndexOpt | _ -> None)
                |> Seq.headOrDefault Int32.MaxValue
                :> IComparable) // OPTIMIZATION: boxing here to avoid it downstream.
                cascadedLightsArray

        // shadow cascade pre-passes
        let mutable shadowCascadeBufferIndex = 0
        for struct (lightId, lightOrigin, lightCutoff, _, lightDesireShadows) in cascadedLightsArray do
            if renderer.RendererConfig.LightShadowingEnabled && lightDesireShadows = 1 then
                for (renderPass, renderTasks) in renderer.RenderPasses.Pairs do
                    match renderPass with
                    | ShadowPass (shadowLightId, shadowIndexInfoOpt, shadowLightType, _, _, shadowFrustum) when
                        lightId = shadowLightId && shadowIndexInfoOpt.IsSome && shadowCascadeBufferIndex < Constants.Render.ShadowCascadesMax ->
                        match shadowLightType with
                        | CascadedLight ->

                            // destructure shadow index info
                            let (shadowCascadeLevel, shadowView, shadowProjection) = shadowIndexInfoOpt.Value

                            // draw shadow cascade when not cached
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
                                let (shadowCascadeArray, shadowZTexture) = renderer.PhysicallyBasedAttachments.ShadowCascadeArrayAttachmentsArray[shadowCascadeBufferIndex]
                                VulkanRenderer3d.renderShadowCascade lightOrigin lightCutoff shadowView shadowProjection shadowFrustum shadowResolution shadowCascadeArray.LayerViews[shadowCascadeLevel] shadowZTexture renderTasks renderer

                                // filter shadows on the x
                                let gaussianEsmArrayTexture = renderer.PhysicallyBasedAttachments.GaussianEsmArrayAttachment
                                Hl.recordTransitionLayout true 1 shadowCascadeLevel 1 VkImageAspectFlags.Color ColorAttachmentRead ColorAttachmentWrite gaussianEsmArrayTexture.Image renderer.VulkanContext.RenderCommandBuffer
                                PhysicallyBased.drawFilterGaussianEsmSurface
                                    (v2 (1.0f / single shadowResolution.X) 0.0f) renderer.LightingConfig.LightShadowRadius
                                    shadowCascadeArray.LayerViews[shadowCascadeLevel] renderer.FilteredSampler gaussianEsmArrayTexture.LayerViews[shadowCascadeLevel] shadowResolution
                                    renderer.QuadGeometry renderer.PhysicallyBasedPipelines.FilterGaussianEsmPipeline renderer.VulkanContext
                                Hl.recordTransitionLayout true 1 shadowCascadeLevel 1 VkImageAspectFlags.Color ColorAttachmentWrite ColorAttachmentRead gaussianEsmArrayTexture.Image renderer.VulkanContext.RenderCommandBuffer

                                // filter shadows on the y
                                Hl.recordTransitionLayout true 1 shadowCascadeLevel 1 VkImageAspectFlags.Color ColorAttachmentRead ColorAttachmentWrite shadowCascadeArray.Image renderer.VulkanContext.RenderCommandBuffer
                                PhysicallyBased.drawFilterGaussianEsmSurface
                                    (v2 0.0f (1.0f / single shadowResolution.Y)) renderer.LightingConfig.LightShadowRadius
                                    gaussianEsmArrayTexture.LayerViews[shadowCascadeLevel] renderer.FilteredSampler shadowCascadeArray.LayerViews[shadowCascadeLevel] shadowResolution
                                    renderer.QuadGeometry renderer.PhysicallyBasedPipelines.FilterGaussianEsmPipeline renderer.VulkanContext
                                Hl.recordTransitionLayout true 1 shadowCascadeLevel 1 VkImageAspectFlags.Color ColorAttachmentWrite ColorAttachmentRead shadowCascadeArray.Image renderer.VulkanContext.RenderCommandBuffer

                            // remember the utilized index for the next frame
                            renderTasks.ShadowBufferIndexOpt <- Some (shadowCascadeBufferIndex + Constants.Render.ShadowTexturesMax)

                            // update renderer values or next shadow
                            // NOTE: this behavior completely DEPENDS on shadow index messages for a shadow cascade
                            // being received and processed in numerical order.
                            renderer.ShadowMatricesFlipped
                                [shadowCascadeBufferIndex * Constants.Render.ShadowCascadeLevels + shadowCascadeLevel + Constants.Render.ShadowTexturesMax] <-
                                shadowView * shadowProjection.Flipped
                            if shadowCascadeLevel = 0 then
                                renderer.LightShadowIndices[lightId] <- shadowCascadeBufferIndex + Constants.Render.ShadowTexturesMax
                            elif shadowCascadeLevel = dec Constants.Render.ShadowCascadeLevels then
                                shadowCascadeBufferIndex <- inc shadowCascadeBufferIndex

                        | PointLight | SpotLight (_, _) | DirectionalLight _ -> failwithumf ()
                    | _ -> ()

    // TODO: apply intention blocks to this function.
    static member private renderPhysicallyBasedTerrain
        shadowCubeMapFace eyeCenter view projection
        lightShadowSamples lightShadowBias lightShadowSampleScalar lightShadowExponent lightShadowDensity
        (terrainDescriptor : TerrainDescriptor) materialSampler geometry colorAttachments depthAttachment resolution renderPassIndex pipeline renderer =
        let terrainMaterialProperties = terrainDescriptor.MaterialProperties
        let materialProperties =
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
              SubsurfaceCutoff = Constants.Render.SubsurfaceCutoffDefault
              SubsurfaceCutoffMargin = Constants.Render.SubsurfaceCutoffMarginDefault
              RefractiveIndex = Constants.Render.RefractiveIndexDefault
              ClearCoat = Constants.Render.ClearCoatDefault
              ClearCoatRoughness = Constants.Render.ClearCoatRoughnessDefault }
        let (texelWidth, texelHeight, materials) =
            match terrainDescriptor.Material with
            | BlendMaterial blendMaterial ->
                let mutable texelWidth = Single.MaxValue
                let mutable texelHeight = Single.MaxValue
                let materials =
                    [|for i in 0 .. dec blendMaterial.TerrainLayers.Length do
                        let layer =
                            blendMaterial.TerrainLayers[i]
                        let defaultMaterial =
                            renderer.PhysicallyBasedMaterial
                        let albedoTexture =
                            match VulkanRenderer3d.tryGetRenderAsset layer.AlbedoImage renderer with
                            | ValueSome renderAsset -> match renderAsset with TextureAsset texture -> texture | _ -> defaultMaterial.AlbedoTexture
                            | ValueNone -> defaultMaterial.AlbedoTexture
                        let roughnessTexture =
                            match VulkanRenderer3d.tryGetRenderAsset layer.RoughnessImage renderer with
                            | ValueSome renderAsset -> match renderAsset with TextureAsset texture -> texture | _ -> defaultMaterial.RoughnessTexture
                            | ValueNone -> defaultMaterial.RoughnessTexture
                        let ambientOcclusionTexture =
                            match VulkanRenderer3d.tryGetRenderAsset layer.AmbientOcclusionImage renderer with
                            | ValueSome renderAsset -> match renderAsset with TextureAsset texture -> texture | _ -> defaultMaterial.AmbientOcclusionTexture
                            | ValueNone -> defaultMaterial.AmbientOcclusionTexture
                        let normalTexture =
                            match VulkanRenderer3d.tryGetRenderAsset layer.NormalImage renderer with
                            | ValueSome renderAsset -> match renderAsset with TextureAsset texture -> texture | _ -> defaultMaterial.NormalTexture
                            | ValueNone -> defaultMaterial.NormalTexture
                        let heightTexture =
                            match VulkanRenderer3d.tryGetRenderAsset layer.HeightImage renderer with
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
                    match VulkanRenderer3d.tryGetRenderAsset flatMaterial.AlbedoImage renderer with
                    | ValueSome renderAsset -> match renderAsset with TextureAsset texture -> texture | _ -> defaultMaterial.AlbedoTexture
                    | ValueNone -> defaultMaterial.AlbedoTexture
                let roughnessTexture =
                    match VulkanRenderer3d.tryGetRenderAsset flatMaterial.RoughnessImage renderer with
                    | ValueSome renderAsset -> match renderAsset with TextureAsset texture -> texture | _ -> defaultMaterial.RoughnessTexture
                    | ValueNone -> defaultMaterial.RoughnessTexture
                let ambientOcclusionTexture =
                    match VulkanRenderer3d.tryGetRenderAsset flatMaterial.AmbientOcclusionImage renderer with
                    | ValueSome renderAsset -> match renderAsset with TextureAsset texture -> texture | _ -> defaultMaterial.AmbientOcclusionTexture
                    | ValueNone -> defaultMaterial.AmbientOcclusionTexture
                let normalTexture =
                    match VulkanRenderer3d.tryGetRenderAsset flatMaterial.NormalImage renderer with
                    | ValueSome renderAsset -> match renderAsset with TextureAsset texture -> texture | _ -> defaultMaterial.NormalTexture
                    | ValueNone -> defaultMaterial.NormalTexture
                let heightTexture =
                    match VulkanRenderer3d.tryGetRenderAsset flatMaterial.HeightImage renderer with
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
        let materials =
            Array.create Constants.Render.TerrainLayersMax PhysicallyBasedMaterial.empty
            |> Array.append materials
            |> Array.take Constants.Render.TerrainLayersMax
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
        instanceFields[16] <- texCoordsOffset.Min.X
        instanceFields[17] <- texCoordsOffset.Min.Y
        instanceFields[18] <- texCoordsOffset.Min.X + texCoordsOffset.Size.X
        instanceFields[19] <- texCoordsOffset.Min.Y + texCoordsOffset.Size.Y
        instanceFields[20] <- materialProperties.Albedo.R
        instanceFields[21] <- materialProperties.Albedo.G
        instanceFields[22] <- materialProperties.Albedo.B
        instanceFields[23] <- materialProperties.Albedo.A
        instanceFields[24] <- materialProperties.Roughness
        instanceFields[25] <- materialProperties.Metallic
        instanceFields[26] <- materialProperties.AmbientOcclusion
        instanceFields[27] <- materialProperties.Emission
        instanceFields[28] <- texelHeight * materialProperties.Height
        PhysicallyBased.drawPhysicallyBasedTerrain
            shadowCubeMapFace eyeCenter view projection
            instanceFields lightShadowSamples lightShadowBias lightShadowSampleScalar lightShadowExponent lightShadowDensity
            materials materialSampler geometry colorAttachments depthAttachment resolution renderPassIndex pipeline renderer.VulkanContext

        // track geometry instancing
        renderer.GeometryInstanced.Add geometry |> ignore<bool>

    static member private renderGeometry
        frustumInterior
        frustumExterior
        frustumImposter
        renderPass
        (renderTasks : RenderTasks)
        renderer
        topLevelRender
        lightAmbientOverride
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (viewSkyBox : Matrix4x4)
        (geometryFrustum : Frustum)
        (geometryProjection : Matrix4x4)
        (windowProjection : Matrix4x4)
        targetBounds
        targetLayer
        targetImage =

        // get ambient lighting, sky box opt, and fallback light map
        let (lightAmbientColor, lightAmbientBrightness, skyBoxOpt) = VulkanRenderer3d.getLastSkyBoxOpt renderPass renderer
        let (lightAmbientColor, lightAmbientBrightness) = Option.defaultValue (lightAmbientColor, lightAmbientBrightness) lightAmbientOverride
        let lightMapFallback =
            match skyBoxOpt with
            | Some (_, _, _, (irradianceAndEnvironmentMapsOptRef : (Texture * Texture) option ref)) ->
                let (irradianceMap, environmentFilterMap) =
                    match irradianceAndEnvironmentMapsOptRef.Value with
                    | Some irradianceAndEnvironmentMaps -> irradianceAndEnvironmentMaps
                    | None -> (renderer.IrradianceMap, renderer.EnvironmentFilterMap)
                LightMap.createLightMap true v3Zero lightAmbientColor lightAmbientBrightness box3Zero irradianceMap environmentFilterMap
            | None -> LightMap.createLightMap true v3Zero Color.White 1.0f box3Zero renderer.IrradianceMap renderer.EnvironmentFilterMap

        // destroy cached light maps whose originating probe no longer exists (top-level only)
        if topLevelRender then
            for lightMapKvp in renderer.LightMaps do
                if not (renderTasks.LightProbes.ContainsKey lightMapKvp.Key) then
                    TextureDumpster.toss lightMapKvp.Value.IrradianceMap renderer.TextureDumpster
                    TextureDumpster.toss lightMapKvp.Value.EnvironmentFilterMap renderer.TextureDumpster
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
                    renderer.LightMaps[lightMapId] <- lightMap
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
                SortableLightMap.sortLightMaps Constants.Render.LightMapsMaxDeferred eyeCenter None renderer.IrradianceMap renderer.EnvironmentFilterMap lightMaps
            else
                (Array.zeroCreate Constants.Render.LightMapsMaxDeferred,
                 Array.zeroCreate Constants.Render.LightMapsMaxDeferred,
                 Array.zeroCreate Constants.Render.LightMapsMaxDeferred,
                 Array.zeroCreate Constants.Render.LightMapsMaxDeferred,
                 Array.zeroCreate Constants.Render.LightMapsMaxDeferred,
                 Array.create Constants.Render.LightMapsMaxDeferred renderer.IrradianceMap,
                 Array.create Constants.Render.LightMapsMaxDeferred renderer.EnvironmentFilterMap)

        // sort lights for deferred rendering relative to eye center
        let (lightIds, lightOrigins, lightDirections, lightColors, lightBrightnesses, lightAttenuationLinears, lightAttenuationQuadratics, lightCutoffs, lightTypes, lightConeInners, lightConeOuters, lightDesireFogs) =
            SortableLight.sortLights Constants.Render.LightsMaxDeferred eyeCenter renderTasks.Lights

        // compute light shadow indices according to sorted lights
        let lightShadowIndices =
            if topLevelRender
            then SortableLight.sortLightShadowIndices renderer.LightShadowIndices lightIds
            else Array.init Constants.Render.LightsMaxDeferred (constant -1)

        // clear geometry textures
        let geometryResolution = renderer.GeometryViewport.Bounds.Size
        let renderArea = VkRect2D (0, 0, uint geometryResolution.X, uint geometryResolution.Y)
        let clearColor = VkClearValue (Constants.Render.ViewportClearColor.R, Constants.Render.ViewportClearColor.G, Constants.Render.ViewportClearColor.B, Constants.Render.ViewportClearColor.A)
        let (depthTexture, albedoTexture, materialTexture, normalPlusTexture, subdermalPlusTexture, scatterPlusTexture, clearCoatPlusTexture, zTexture) = renderer.PhysicallyBasedAttachments.GeometryAttachments
        Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite depthTexture renderer.VulkanContext.RenderCommandBuffer
        Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite albedoTexture renderer.VulkanContext.RenderCommandBuffer
        Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite materialTexture renderer.VulkanContext.RenderCommandBuffer
        Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite normalPlusTexture renderer.VulkanContext.RenderCommandBuffer
        Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite subdermalPlusTexture renderer.VulkanContext.RenderCommandBuffer
        Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite scatterPlusTexture renderer.VulkanContext.RenderCommandBuffer
        Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite clearCoatPlusTexture renderer.VulkanContext.RenderCommandBuffer
        Texture.recordTransitionLayout DepthAttachmentRead DepthAttachmentWrite zTexture renderer.VulkanContext.RenderCommandBuffer
        let geometryTextureViews = [|depthTexture.ImageView; albedoTexture.ImageView; materialTexture.ImageView; normalPlusTexture.ImageView; subdermalPlusTexture.ImageView; scatterPlusTexture.ImageView; clearCoatPlusTexture.ImageView|]
        Hl.withRenderingInfo geometryTextureViews (Some zTexture.ImageView) renderArea (Some clearColor) $ fun renderingInfo ->
            let mutable renderingInfo = renderingInfo
            DeviceApi.vkCmdBeginRendering (renderer.VulkanContext.RenderCommandBuffer, &&renderingInfo)
        DeviceApi.vkCmdEndRendering renderer.VulkanContext.RenderCommandBuffer
        Hl.reportDrawScope ()

        // begin deferred static surface rendering
        let mutable counted = 0
        let mutable committed = 0
        let mutable eyeDescriptorSet = Unchecked.defaultof<_>
        let mutable samplerDescriptorSet = Unchecked.defaultof<_>
        let beginBatch = fun () ->
            let (eyeDescriptorSet', samplerDescriptorSet') =
                VulkanRenderer3d.beginPhysicallyBasedDeferredSurfaces
                    eyeCenter view geometryProjection renderer.MaterialSampler geometryTextureViews zTexture
                    geometryResolution renderer.RenderPassIndex renderer.PhysicallyBasedPipelines.DeferredStaticPipeline renderer
            eyeDescriptorSet <- eyeDescriptorSet'
            samplerDescriptorSet <- samplerDescriptorSet'
        let endBatch = fun () -> VulkanRenderer3d.endPhysicallyBasedDeferredSurfaces renderer.PhysicallyBasedPipelines.DeferredStaticPipeline renderer.VulkanContext
        let advanceBatch = fun instances ->
            counted <- counted + instances
            let delta = counted - committed
            if delta >= Constants.Vulkan.DeferredSurfaceInstanceThreshold then
                endBatch ()
                beginBatch ()
                committed <- counted
        beginBatch ()

        // render deferred static surfaces
        for entry in renderTasks.DeferredStatic do
            VulkanRenderer3d.renderPhysicallyBasedDeferredSurfaces [||] entry.Value entry.Key eyeDescriptorSet samplerDescriptorSet renderer.PhysicallyBasedPipelines.DeferredStaticPipeline renderer
            advanceBatch entry.Value.Count
        
        // render deferred static surface pre-batches
        for entry in renderTasks.DeferredStaticPreBatches do
            let struct (surface, preBatch) = entry.Value
            advanceBatch $ VulkanRenderer3d.renderPhysicallyBasedDeferredSurfacePreBatch frustumInterior frustumExterior frustumImposter renderPass [||] preBatch surface eyeDescriptorSet samplerDescriptorSet renderer.PhysicallyBasedPipelines.DeferredStaticPipeline renderer
        
        // end deferred static surface rendering
        endBatch ()
        
        // begin deferred static surface clipped rendering
        let mutable counted = 0
        let mutable committed = 0
        let mutable eyeDescriptorSet = Unchecked.defaultof<_>
        let mutable samplerDescriptorSet = Unchecked.defaultof<_>
        let beginBatch = fun () ->
            let (eyeDescriptorSet', samplerDescriptorSet') =
                VulkanRenderer3d.beginPhysicallyBasedDeferredSurfaces
                    eyeCenter view geometryProjection renderer.MaterialSampler geometryTextureViews zTexture
                    geometryResolution renderer.RenderPassIndex renderer.PhysicallyBasedPipelines.DeferredStaticClippedPipeline renderer
            eyeDescriptorSet <- eyeDescriptorSet'
            samplerDescriptorSet <- samplerDescriptorSet'
        let endBatch = fun () -> VulkanRenderer3d.endPhysicallyBasedDeferredSurfaces renderer.PhysicallyBasedPipelines.DeferredStaticClippedPipeline renderer.VulkanContext
        let advanceBatch = fun instances ->
            counted <- counted + instances
            let delta = counted - committed
            if delta >= Constants.Vulkan.DeferredSurfaceInstanceThreshold then
                endBatch ()
                beginBatch ()
                committed <- counted
        beginBatch ()

        // render deferred static surfaces clipped
        for entry in renderTasks.DeferredStaticClipped do
            VulkanRenderer3d.renderPhysicallyBasedDeferredSurfaces [||] entry.Value entry.Key eyeDescriptorSet samplerDescriptorSet renderer.PhysicallyBasedPipelines.DeferredStaticClippedPipeline renderer
            advanceBatch entry.Value.Count
        
        // render deferred static surface clipped pre-batches
        for entry in renderTasks.DeferredStaticClippedPreBatches do
            let struct (surface, preBatch) = entry.Value
            advanceBatch $ VulkanRenderer3d.renderPhysicallyBasedDeferredSurfacePreBatch frustumInterior frustumExterior frustumImposter renderPass [||] preBatch surface eyeDescriptorSet samplerDescriptorSet renderer.PhysicallyBasedPipelines.DeferredStaticClippedPipeline renderer
        
        // end deferred static surface clipped rendering
        endBatch ()
        
        // begin deferred animated rendering
        let (eyeDescriptorSet, samplerDescriptorSet) =
            VulkanRenderer3d.beginPhysicallyBasedDeferredSurfaces
                eyeCenter view geometryProjection renderer.MaterialSampler geometryTextureViews zTexture
                geometryResolution renderer.RenderPassIndex renderer.PhysicallyBasedPipelines.DeferredAnimatedPipeline renderer
        
        // render animated surfaces deferred
        for entry in renderTasks.DeferredAnimated do
            let surfaceKey = entry.Key
            let parameters = entry.Value
            VulkanRenderer3d.renderPhysicallyBasedDeferredSurfaces
                surfaceKey.BoneTransforms parameters surfaceKey.AnimatedSurface
                eyeDescriptorSet samplerDescriptorSet renderer.PhysicallyBasedPipelines.DeferredAnimatedPipeline renderer

        // end deferred animated rendering
        VulkanRenderer3d.endPhysicallyBasedDeferredSurfaces
            renderer.PhysicallyBasedPipelines.DeferredAnimatedPipeline renderer.VulkanContext

        // render terrains deferred
        let terrainTextureViews = [|depthTexture.ImageView; albedoTexture.ImageView; materialTexture.ImageView; normalPlusTexture.ImageView; subdermalPlusTexture.ImageView; scatterPlusTexture.ImageView|]
        for struct (descriptor, _, geometry) in renderTasks.DeferredTerrains do
            VulkanRenderer3d.renderPhysicallyBasedTerrain
                false eyeCenter view geometryProjection
                renderer.LightingConfig.LightShadowSamples renderer.LightingConfig.LightShadowBias renderer.LightingConfig.LightShadowSampleScalar renderer.LightingConfig.LightShadowExponent renderer.LightingConfig.LightShadowDensity
                descriptor renderer.MaterialSampler geometry terrainTextureViews zTexture
                geometryResolution renderer.RenderPassIndex renderer.PhysicallyBasedPipelines.DeferredTerrainPipeline renderer

        // transition geometry attachments (except zTexture) back to reading
        Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead depthTexture renderer.VulkanContext.RenderCommandBuffer
        Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead albedoTexture renderer.VulkanContext.RenderCommandBuffer
        Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead materialTexture renderer.VulkanContext.RenderCommandBuffer
        Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead normalPlusTexture renderer.VulkanContext.RenderCommandBuffer
        Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead subdermalPlusTexture renderer.VulkanContext.RenderCommandBuffer
        Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead scatterPlusTexture renderer.VulkanContext.RenderCommandBuffer
        Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead clearCoatPlusTexture renderer.VulkanContext.RenderCommandBuffer

        // gather shadow resources
        let shadowTextureArray = fst renderer.PhysicallyBasedAttachments.ShadowTextureArrayAttachments
        let shadowMaps = Array.map fst renderer.PhysicallyBasedAttachments.ShadowMapAttachmentsArray
        let shadowCascades = Array.map fst renderer.PhysicallyBasedAttachments.ShadowCascadeArrayAttachmentsArray
        let shadowNear = Constants.Render.NearPlaneDistanceInterior
        let sssEnabled = if renderer.RendererConfig.SssEnabled && renderer.LightingConfig.SssEnabled then 1 else 0

        // deferred render quad to light accum texture
        let lightAccumTexture = renderer.PhysicallyBasedAttachments.LightingAttachment
        Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite lightAccumTexture renderer.VulkanContext.RenderCommandBuffer
        PhysicallyBased.drawPhysicallyBasedDeferredLightingSurface
            eyeCenter view geometryProjection renderer.LightingConfig.LightCutoffMargin renderer.LightingConfig.LightShadowSamples renderer.LightingConfig.LightShadowBias renderer.LightingConfig.LightShadowSampleScalar renderer.LightingConfig.LightShadowExponent renderer.LightingConfig.LightShadowDensity sssEnabled
            depthTexture albedoTexture materialTexture normalPlusTexture subdermalPlusTexture scatterPlusTexture clearCoatPlusTexture shadowTextureArray shadowMaps shadowCascades
            lightOrigins lightDirections lightColors lightBrightnesses lightAttenuationLinears lightAttenuationQuadratics lightCutoffs lightTypes lightConeInners lightConeOuters lightDesireFogs lightShadowIndices (min lightIds.Length renderTasks.Lights.Count) shadowNear renderer.ShadowMatricesFlipped renderer.UnfilteredSampler renderer.FilteredSampler
            geometryResolution renderer.RenderPassIndex renderer.QuadGeometry lightAccumTexture renderer.PhysicallyBasedPipelines.DeferredLightingPipeline renderer.VulkanContext
        Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead lightAccumTexture renderer.VulkanContext.RenderCommandBuffer

        // setup fogging textures and viewport
        let ssvfEnabled = if renderer.RendererConfig.SsvfEnabled && renderer.LightingConfig.SsvfEnabled then 1 else 0
        let fogAccumTexture =

            // but only when desired
            if topLevelRender && ssvfEnabled = 1 then

                // deferred render quad to fogging textures
                let fogAccumTexture = renderer.PhysicallyBasedAttachments.FoggingAttachment
                Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite fogAccumTexture renderer.VulkanContext.RenderCommandBuffer
                PhysicallyBased.drawPhysicallyBasedDeferredFoggingSurface
                    eyeCenter view geometryProjection renderer.LightingConfig.LightCutoffMargin ssvfEnabled renderer.LightingConfig.SsvfIntensity renderer.LightingConfig.SsvfSteps renderer.LightingConfig.SsvfAsymmetry
                    depthTexture shadowTextureArray shadowMaps shadowCascades lightMaps.Length renderer.LightingConfig.LightMapSingletonBlendMargin
                    lightOrigins lightDirections lightColors lightBrightnesses lightAttenuationLinears lightAttenuationQuadratics lightCutoffs lightTypes lightConeInners lightConeOuters lightDesireFogs lightShadowIndices (min lightIds.Length renderTasks.Lights.Count)
                    renderer.ShadowMatricesFlipped renderer.UnfilteredSampler renderer.FilteredSampler fogAccumTexture
                    geometryResolution renderer.RenderPassIndex renderer.QuadGeometry renderer.PhysicallyBasedPipelines.DeferredFoggingPipeline renderer.VulkanContext
                Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead fogAccumTexture renderer.VulkanContext.RenderCommandBuffer

                // deferred render down-sample filter quad to down-sample texture
                // TODO: P0: verify that down-sampling the depth texture is actually beneficial and the shader is
                // correctly utilizing depth comparison.
                let downSampleColorTexture = renderer.PhysicallyBasedAttachments.DownSampleColorAttachment
                let downSampleDepthTexture = renderer.PhysicallyBasedAttachments.DownSampleDepthAttachment
                Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite downSampleColorTexture renderer.VulkanContext.RenderCommandBuffer
                Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite downSampleDepthTexture renderer.VulkanContext.RenderCommandBuffer
                PhysicallyBased.drawFilterDownSampleSurface
                    fogAccumTexture depthTexture renderer.FilteredSampler downSampleColorTexture.ImageView downSampleDepthTexture.ImageView
                    (geometryResolution / 2) renderer.QuadGeometry renderer.PhysicallyBasedPipelines.FilterBilateralDownSamplePipeline renderer.VulkanContext
                Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead downSampleColorTexture renderer.VulkanContext.RenderCommandBuffer
                Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead downSampleDepthTexture renderer.VulkanContext.RenderCommandBuffer

                // deferred render up-sample filter quad to up-sample texture
                let upSampleColorTexture = renderer.PhysicallyBasedAttachments.UpSampleColorAttachment
                Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite upSampleColorTexture renderer.VulkanContext.RenderCommandBuffer
                PhysicallyBased.drawFilterUpSampleSurface
                    downSampleColorTexture downSampleDepthTexture depthTexture renderer.FilteredSampler upSampleColorTexture.ImageView
                    geometryResolution renderer.QuadGeometry renderer.PhysicallyBasedPipelines.FilterBilateralUpSamplePipeline renderer.VulkanContext
                Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead upSampleColorTexture renderer.VulkanContext.RenderCommandBuffer
                
                // blit up-sample texture to fog accum texture
                Texture.recordTransitionLayout ColorAttachmentRead TransferSrc upSampleColorTexture renderer.VulkanContext.RenderCommandBuffer
                Texture.recordTransitionLayout ColorAttachmentRead TransferDst fogAccumTexture renderer.VulkanContext.RenderCommandBuffer
                let bounds = VkRect2D (0, 0, uint geometryResolution.X, uint geometryResolution.Y)
                let mutable region = Hl.makeBlit 0 0 0 0 bounds bounds
                DeviceApi.vkCmdBlitImage (renderer.VulkanContext.RenderCommandBuffer, upSampleColorTexture.Image, TransferSrc.VkImageLayout, fogAccumTexture.Image, TransferDst.VkImageLayout, 1u, &&region, VkFilter.Nearest)
                Texture.recordTransitionLayout TransferSrc ColorAttachmentRead upSampleColorTexture renderer.VulkanContext.RenderCommandBuffer
                Texture.recordTransitionLayout TransferDst ColorAttachmentRead fogAccumTexture renderer.VulkanContext.RenderCommandBuffer
                fogAccumTexture

            // just use black texture
            else renderer.BlackTexture

        // run light mapping pass
        let lightMappingTexture =

            // but only when desired
            if renderer.RendererConfig.LightMappingEnabled then
                let lightMappingTexture = renderer.PhysicallyBasedAttachments.LightMappingAttachment
                Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite lightMappingTexture renderer.VulkanContext.RenderCommandBuffer
                PhysicallyBased.drawPhysicallyBasedDeferredLightMappingSurface
                    eyeCenter view geometryProjection lightMapOrigins lightMapMins lightMapSizes lightMapAmbientColors lightMapAmbientBrightnesses
                    lightMaps.Length renderer.LightingConfig.LightMapSingletonBlendMargin
                    renderTasks.Lights.Count depthTexture normalPlusTexture renderer.UnfilteredSampler lightMappingTexture
                    geometryResolution renderer.RenderPassIndex renderer.QuadGeometry renderer.PhysicallyBasedPipelines.DeferredLightMappingPipeline renderer.VulkanContext
                Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead lightMappingTexture renderer.VulkanContext.RenderCommandBuffer
                lightMappingTexture

            // just use black texture
            else renderer.BlackTexture

        // run ambient pass
        let ambientTexture = renderer.PhysicallyBasedAttachments.AmbientAttachment
        Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite ambientTexture renderer.VulkanContext.RenderCommandBuffer
        PhysicallyBased.drawPhysicallyBasedDeferredAmbientSurface
            eyeCenter view geometryProjection lightMapFallback.AmbientColor lightMapFallback.AmbientBrightness lightMapAmbientColors lightMapAmbientBrightnesses depthTexture lightMappingTexture renderer.UnfilteredSampler ambientTexture
            geometryResolution renderer.RenderPassIndex renderer.QuadGeometry renderer.PhysicallyBasedPipelines.DeferredAmbientPipeline renderer.VulkanContext
        Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead ambientTexture renderer.VulkanContext.RenderCommandBuffer

        // run irradiance pass
        let irradianceTexture = renderer.PhysicallyBasedAttachments.IrradianceAttachment
        Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite irradianceTexture renderer.VulkanContext.RenderCommandBuffer
        PhysicallyBased.drawPhysicallyBasedDeferredIrradianceSurface
            eyeCenter view geometryProjection depthTexture normalPlusTexture lightMappingTexture lightMapFallback.IrradianceMap lightMapIrradianceMaps renderer.UnfilteredSampler renderer.FilteredSampler irradianceTexture
            geometryResolution renderer.RenderPassIndex renderer.QuadGeometry renderer.PhysicallyBasedPipelines.DeferredIrradiancePipeline renderer.VulkanContext
        Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead irradianceTexture renderer.VulkanContext.RenderCommandBuffer

        // run environment filter pass
        let environmentFilterTexture = renderer.PhysicallyBasedAttachments.EnvironmentFilterAttachment
        Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite environmentFilterTexture renderer.VulkanContext.RenderCommandBuffer
        PhysicallyBased.drawPhysicallyBasedDeferredEnvironmentFilterSurface
            eyeCenter view geometryProjection lightMapOrigins lightMapMins lightMapSizes lightMapAmbientColors lightMapAmbientBrightnesses
            depthTexture materialTexture normalPlusTexture clearCoatPlusTexture lightMappingTexture lightMapFallback.EnvironmentFilterMap lightMapEnvironmentFilterMaps renderer.UnfilteredSampler renderer.FilteredSampler environmentFilterTexture
            geometryResolution renderer.RenderPassIndex renderer.QuadGeometry renderer.PhysicallyBasedPipelines.DeferredEnvironmentFilterPipeline renderer.VulkanContext
        Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead environmentFilterTexture renderer.VulkanContext.RenderCommandBuffer

        // run ssao pass
        let ssaoTextureFiltered =

            // but only when desired
            if renderer.RendererConfig.SsaoEnabled && renderer.LightingConfig.SsaoEnabled then

                // render deferred ssao
                let ssaoUnfilteredResolution = renderer.GeometryViewport.SsaoResolution
                let ssaoUnfilteredTexture = renderer.PhysicallyBasedAttachments.SsaoUnfilteredAttachment
                Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite ssaoUnfilteredTexture renderer.VulkanContext.RenderCommandBuffer
                PhysicallyBased.drawPhysicallyBasedDeferredSsaoSurface
                    eyeCenter view geometryProjection
                    renderer.LightingConfig.SsaoIntensity renderer.LightingConfig.SsaoBias renderer.LightingConfig.SsaoRadius renderer.LightingConfig.SsaoDistanceMax renderer.RendererConfig.SsaoSampleCount
                    depthTexture normalPlusTexture renderer.UnfilteredSampler ssaoUnfilteredTexture
                    ssaoUnfilteredResolution renderer.RenderPassIndex renderer.QuadGeometry renderer.PhysicallyBasedPipelines.DeferredSsaoPipeline renderer.VulkanContext
                Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead ssaoUnfilteredTexture renderer.VulkanContext.RenderCommandBuffer

                // filter deferred ssao
                let ssaoFilteredTexture = renderer.PhysicallyBasedAttachments.SsaoFilteredAttachment
                Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite ssaoFilteredTexture renderer.VulkanContext.RenderCommandBuffer
                PhysicallyBased.drawFilterBoxSurface
                    ssaoUnfilteredTexture renderer.UnfilteredSampler ssaoFilteredTexture ssaoUnfilteredResolution
                    renderer.QuadGeometry renderer.PhysicallyBasedPipelines.FilterBox1dPipeline renderer.VulkanContext
                Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead ssaoFilteredTexture renderer.VulkanContext.RenderCommandBuffer
                ssaoFilteredTexture

            // just use white texture
            else renderer.WhiteTexture

        // setup coloring attachments
        let ssrlEnabled = if renderer.RendererConfig.SsrlEnabled && renderer.LightingConfig.SsrlEnabled then 1 else 0
        let (colorTexture, depthTexture2) = renderer.PhysicallyBasedAttachments.ColoringAttachments
        Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite colorTexture renderer.VulkanContext.RenderCommandBuffer
        Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite depthTexture2 renderer.VulkanContext.RenderCommandBuffer
        PhysicallyBased.drawPhysicallyBasedDeferredColoringSurface
            eyeCenter view geometryProjection renderer.LightingConfig.LightAmbientBoostCutoff renderer.LightingConfig.LightAmbientBoostScalar
            ssrlEnabled renderer.LightingConfig.SsrlIntensity renderer.LightingConfig.SsrlDetail renderer.LightingConfig.SsrlRefinementsMax renderer.LightingConfig.SsrlRayThickness renderer.LightingConfig.SsrlTowardEyeCutoff
            renderer.LightingConfig.SsrlDepthCutoff renderer.LightingConfig.SsrlDepthCutoffMargin renderer.LightingConfig.SsrlDistanceCutoff renderer.LightingConfig.SsrlDistanceCutoffMargin
            renderer.LightingConfig.SsrlRoughnessCutoff renderer.LightingConfig.SsrlRoughnessCutoffMargin renderer.LightingConfig.SsrlSlopeCutoff renderer.LightingConfig.SsrlSlopeCutoffMargin
            renderer.LightingConfig.SsrlEdgeHorizontalMargin renderer.LightingConfig.SsrlEdgeVerticalMargin
            depthTexture albedoTexture materialTexture normalPlusTexture clearCoatPlusTexture lightAccumTexture renderer.BrdfTexture ambientTexture irradianceTexture environmentFilterTexture ssaoTextureFiltered
            renderer.UnfilteredSampler renderer.FilteredSampler colorTexture depthTexture2 geometryResolution renderer.RenderPassIndex renderer.QuadGeometry renderer.PhysicallyBasedPipelines.DeferredColoringPipeline renderer.VulkanContext
        Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead colorTexture renderer.VulkanContext.RenderCommandBuffer
        Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead depthTexture2 renderer.VulkanContext.RenderCommandBuffer

        // begin rendering to composition attachment
        let compositionTexture = renderer.PhysicallyBasedAttachments.CompositionAttachment
        Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite compositionTexture renderer.VulkanContext.RenderCommandBuffer

        // deferred render composition quad to composition attachments
        let fogEnabled = if renderer.LightingConfig.FogEnabled then 1 else 0
        let fogType = renderer.LightingConfig.FogType.Enumerate
        PhysicallyBased.drawPhysicallyBasedDeferredCompositionSurface
            eyeCenter view geometryProjection fogEnabled fogType renderer.LightingConfig.FogStart renderer.LightingConfig.FogFinish renderer.LightingConfig.FogDensity renderer.LightingConfig.FogColor
            depthTexture colorTexture fogAccumTexture renderer.UnfilteredSampler compositionTexture
            geometryResolution renderer.RenderPassIndex renderer.QuadGeometry renderer.PhysicallyBasedPipelines.DeferredCompositionPipeline renderer.VulkanContext

        // attempt to render sky box to composition attachment
        match skyBoxOpt with
        | Some (cubeMapColor, cubeMapBrightness, cubeMap, _) ->
            SkyBox.drawSkyBox
                eyeCenter viewSkyBox windowProjection cubeMapColor cubeMapBrightness cubeMap renderer.CubeMapGeometry renderer.FilteredSampler
                compositionTexture zTexture geometryResolution renderer.SkyBoxPipeline renderer.VulkanContext
        | None -> ()

        // render forward (static and animated) surfaces to composition attachment
        let mutable counted = 0
        let mutable committed = 0
        let mutable forwardPipeline = renderer.PhysicallyBasedPipelines.ForwardStaticPipeline // speculatively start with forward static
        let mutable uniformsDescriptorSet = Unchecked.defaultof<_>
        let mutable samplersDescriptorSet = Unchecked.defaultof<_>
        let ssrrEnabled = if renderer.RendererConfig.SsrrEnabled && renderer.LightingConfig.SsrrEnabled then 1 else 0
        let forwardSsvfSteps = renderer.LightingConfig.SsvfSteps * 2 // HACK: need an increase in forward-rendered steps since they don't get a blur pass.
        let beginBatch = fun () ->
            let (uniformsDescriptorSet', samplersDescriptorSet') =
                VulkanRenderer3d.beginPhysicallyBasedForwardSurfaces
                    eyeCenter view geometryProjection renderer.LightingConfig.LightCutoffMargin lightAmbientColor lightAmbientBrightness renderer.LightingConfig.LightAmbientBoostCutoff renderer.LightingConfig.LightAmbientBoostScalar
                    renderer.LightingConfig.LightShadowSamples renderer.LightingConfig.LightShadowBias renderer.LightingConfig.LightShadowSampleScalar renderer.LightingConfig.LightShadowExponent renderer.LightingConfig.LightShadowDensity
                    fogEnabled fogType renderer.LightingConfig.FogStart renderer.LightingConfig.FogFinish renderer.LightingConfig.FogDensity renderer.LightingConfig.FogColor ssvfEnabled renderer.LightingConfig.SsvfIntensity forwardSsvfSteps renderer.LightingConfig.SsvfAsymmetry
                    ssrrEnabled renderer.LightingConfig.SsrrIntensity renderer.LightingConfig.SsrrDetail renderer.LightingConfig.SsrrRefinementsMax renderer.LightingConfig.SsrrRayThickness renderer.LightingConfig.SsrrDistanceCutoff renderer.LightingConfig.SsrrDistanceCutoffMargin renderer.LightingConfig.SsrrEdgeHorizontalMargin renderer.LightingConfig.SsrrEdgeVerticalMargin shadowNear
                    depthTexture2 colorTexture renderer.BrdfTexture lightMapFallback.IrradianceMap lightMapFallback.EnvironmentFilterMap renderer.UnfilteredSampler renderer.FilteredSampler renderer.MaterialSampler compositionTexture zTexture geometryResolution renderer.RenderPassIndex forwardPipeline renderer.VulkanContext
            uniformsDescriptorSet <- uniformsDescriptorSet'
            samplersDescriptorSet <- samplersDescriptorSet'
        let endBatch = fun () -> VulkanRenderer3d.endPhysicallyBasedForwardSurfaces forwardPipeline renderer.VulkanContext
        let checkBatch = fun forwardPipeline' ->
            if refNeq forwardPipeline forwardPipeline' then
                endBatch ()
                forwardPipeline <- forwardPipeline'
                beginBatch ()
                committed <- counted
        let advanceBatch = fun instances ->
            counted <- counted + instances
            let delta = counted - committed
            if delta >= Constants.Vulkan.ForwardSurfaceInstanceThreshold then
                endBatch ()
                beginBatch ()
                committed <- counted
        beginBatch ()

        // sort forward surfaces from far to near
        let forwardSurfacesSortBuffer = VulkanRenderer3d.sortForwardSurfaces eyeCenter renderTasks.Forward renderer.ForwardSurfacesComparer renderer.ForwardSurfacesSortBuffer
        for struct (_, _, model, castShadow, presence, texCoordsOffset, properties, boneTransformsOpt, surface, depthTest, _, _) in forwardSurfacesSortBuffer do
            renderTasks.ForwardSorted.Add struct (model, castShadow, presence, texCoordsOffset, properties, boneTransformsOpt, surface, depthTest)
        forwardSurfacesSortBuffer.Clear ()
        
        // render forward (static and animated) surfaces to composition attachment
        for (model, _, presence, texCoordsOffset, properties, boneTransformsOpt, surface, depthTest) in renderTasks.ForwardSorted do
            let (lightMapOrigins, lightMapMins, lightMapSizes, lightMapAmbientColors, lightMapAmbientBrightnesses, lightMapIrradianceMaps, lightMapEnvironmentFilterMaps) =
                let surfaceBounds = surface.SurfaceBounds.Transform model
                SortableLightMap.sortLightMaps Constants.Render.LightMapsMaxForward model.Translation (Some surfaceBounds) lightMapFallback.IrradianceMap lightMapFallback.EnvironmentFilterMap lightMaps
            let (lightIds, lightOrigins, lightDirections, lightColors, lightBrightnesses, lightAttenuationLinears, lightAttenuationQuadratics, lightCutoffs, lightTypes, lightConeInners, lightConeOuters, lightDesireFogs) =
                SortableLight.sortLights Constants.Render.LightsMaxForward model.Translation renderTasks.Lights
            let lightShadowIndices =
                SortableLight.sortLightShadowIndices renderer.LightShadowIndices lightIds
            let (bonesArray, forwardPipeline) =
                match boneTransformsOpt with
                | ValueNone -> ([||], renderer.PhysicallyBasedPipelines.ForwardStaticPipeline)
                | ValueSome boneTransforms -> (boneTransforms, renderer.PhysicallyBasedPipelines.ForwardAnimatedPipeline)
            checkBatch forwardPipeline
            VulkanRenderer3d.renderPhysicallyBasedForwardSurfaces
                bonesArray (SList.singleton (model, presence, texCoordsOffset, properties))
                lightMapIrradianceMaps lightMapEnvironmentFilterMaps shadowTextureArray shadowMaps shadowCascades lightMapOrigins lightMapMins lightMapSizes lightMapAmbientColors lightMapAmbientBrightnesses lightMaps.Length renderer.LightingConfig.LightMapSingletonBlendMargin
                lightOrigins lightDirections lightColors lightBrightnesses lightAttenuationLinears lightAttenuationQuadratics lightCutoffs lightTypes lightConeInners lightConeOuters lightDesireFogs lightShadowIndices (min lightIds.Length renderTasks.Lights.Count) renderer.ShadowMatricesFlipped
                surface depthTest true uniformsDescriptorSet samplersDescriptorSet forwardPipeline renderer
            advanceBatch 1
        
        // end forward (static and animated) surface rendering to composition attachment
        endBatch ()

        // end rendering to composition attachment as well as zTexture
        Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead compositionTexture renderer.VulkanContext.RenderCommandBuffer
        Texture.recordTransitionLayout DepthAttachmentWrite DepthAttachmentRead zTexture renderer.VulkanContext.RenderCommandBuffer

        // apply bloom filter when desired
        if topLevelRender && renderer.RendererConfig.BloomEnabled && renderer.LightingConfig.BloomEnabled then

            // render bloom extract texture
            let bloomExtractTexture = renderer.PhysicallyBasedAttachments.BloomExtractAttachment
            Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite bloomExtractTexture renderer.VulkanContext.RenderCommandBuffer
            PhysicallyBased.drawFilterBloomExtractSurface
                renderer.LightingConfig.BloomThreshold compositionTexture renderer.FilteredSampler bloomExtractTexture geometryResolution
                renderer.QuadGeometry renderer.PhysicallyBasedPipelines.FilterBloomExtractPipeline renderer.VulkanContext
            Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead bloomExtractTexture renderer.VulkanContext.RenderCommandBuffer

            // down-sample bloom textures
            let bloomSampleTextures = renderer.PhysicallyBasedAttachments.BloomSampleAttachments
            PhysicallyBased.drawFilterBloomDownSampleSurfaces
                renderer.LightingConfig.BloomKarisAverageEnabled geometryResolution bloomExtractTexture renderer.FilteredSampler bloomSampleTextures (geometryResolution / 2)
                renderer.QuadGeometry renderer.PhysicallyBasedPipelines.FilterBloomDownSamplePipeline renderer.VulkanContext

            // up-sample bloom textures
            PhysicallyBased.drawFilterBloomUpSampleSurfaces
                renderer.LightingConfig.BloomFilterRadius bloomSampleTextures renderer.FilteredSampler geometryResolution
                renderer.QuadGeometry renderer.PhysicallyBasedPipelines.FilterBloomUpSamplePipeline renderer.VulkanContext

            // render bloom apply texture
            let bloomApplyTexture = renderer.PhysicallyBasedAttachments.BloomApplyAttachment
            Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite bloomApplyTexture renderer.VulkanContext.RenderCommandBuffer
            PhysicallyBased.drawFilterBloomApplySurface
                renderer.LightingConfig.BloomStrength bloomSampleTextures[0] compositionTexture renderer.FilteredSampler bloomApplyTexture geometryResolution
                renderer.QuadGeometry renderer.PhysicallyBasedPipelines.FilterBloomApplyPipeline renderer.VulkanContext
            Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead bloomApplyTexture renderer.VulkanContext.RenderCommandBuffer

            // blit bloom apply texture to composition texture
            Texture.recordTransitionLayout ColorAttachmentRead TransferSrc bloomApplyTexture renderer.VulkanContext.RenderCommandBuffer
            Texture.recordTransitionLayout ColorAttachmentRead TransferDst compositionTexture renderer.VulkanContext.RenderCommandBuffer
            let bounds = VkRect2D (0, 0, uint geometryResolution.X, uint geometryResolution.Y)
            let mutable region = Hl.makeBlit 0 0 0 0 bounds bounds
            DeviceApi.vkCmdBlitImage (renderer.VulkanContext.RenderCommandBuffer, bloomApplyTexture.Image, TransferSrc.VkImageLayout, compositionTexture.Image, TransferDst.VkImageLayout, 1u, &&region, VkFilter.Nearest)
            Texture.recordTransitionLayout TransferSrc ColorAttachmentRead bloomApplyTexture renderer.VulkanContext.RenderCommandBuffer
            Texture.recordTransitionLayout TransferDst ColorAttachmentRead compositionTexture renderer.VulkanContext.RenderCommandBuffer

        // apply depth of field when desired
        if topLevelRender && renderer.RendererConfig.DepthOfFieldEnabled && renderer.LightingConfig.DepthOfFieldEnabled then

            // blit composition texture to color half filter 0 texture
            let colorHalf0Texture = renderer.PhysicallyBasedAttachments.ColorHalf0Attachment
            Texture.recordTransitionLayout ColorAttachmentRead TransferSrc compositionTexture renderer.VulkanContext.RenderCommandBuffer
            Texture.recordTransitionLayout ColorAttachmentRead TransferDst colorHalf0Texture renderer.VulkanContext.RenderCommandBuffer
            let boundsSrc = VkRect2D (0, 0, uint geometryResolution.X, uint geometryResolution.Y)
            let boundsDst = VkRect2D (0, 0, uint geometryResolution.X / 2u, uint geometryResolution.Y / 2u)
            let mutable region = Hl.makeBlit 0 0 0 0 boundsSrc boundsDst
            DeviceApi.vkCmdBlitImage (renderer.VulkanContext.RenderCommandBuffer, compositionTexture.Image, TransferSrc.VkImageLayout, colorHalf0Texture.Image, TransferDst.VkImageLayout, 1u, &&region, VkFilter.Linear)
            Texture.recordTransitionLayout TransferSrc ColorAttachmentRead compositionTexture renderer.VulkanContext.RenderCommandBuffer
            Texture.recordTransitionLayout TransferDst ColorAttachmentRead colorHalf0Texture renderer.VulkanContext.RenderCommandBuffer

            // render color half filter 1 on the x
            let colorHalf1Texture = renderer.PhysicallyBasedAttachments.ColorHalf1Attachment
            Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite colorHalf1Texture renderer.VulkanContext.RenderCommandBuffer
            PhysicallyBased.drawFilterGaussianDofSurface
                (v2 (1.0f / single geometryResolution.X / 2.0f) 0.0f) renderer.LightingConfig.DepthOfFieldRadius
                colorHalf0Texture.ImageView renderer.FilteredSampler colorHalf1Texture.ImageView (geometryResolution / 2)
                renderer.QuadGeometry renderer.PhysicallyBasedPipelines.FilterGaussianDofPipeline renderer.VulkanContext
            Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead colorHalf1Texture renderer.VulkanContext.RenderCommandBuffer

            // render color half filter 1 on the y
            Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite colorHalf0Texture renderer.VulkanContext.RenderCommandBuffer
            PhysicallyBased.drawFilterGaussianDofSurface
                (v2 0.0f (1.0f / single geometryResolution.Y / 2.0f)) renderer.LightingConfig.DepthOfFieldRadius
                colorHalf1Texture.ImageView renderer.FilteredSampler colorHalf0Texture.ImageView (geometryResolution / 2)
                renderer.QuadGeometry renderer.PhysicallyBasedPipelines.FilterGaussianDofPipeline renderer.VulkanContext
            Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead colorHalf0Texture renderer.VulkanContext.RenderCommandBuffer

            // render color half filter 0 texture to color full filter 0 texture
            let colorFull0Texture = renderer.PhysicallyBasedAttachments.ColorFull0Attachment
            Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite colorFull0Texture renderer.VulkanContext.RenderCommandBuffer
            PhysicallyBased.drawFilterDepthOfFieldSurface
                eyeCenter view geometryProjection
                renderer.LightingConfig.DepthOfFieldNearDistance renderer.LightingConfig.DepthOfFieldFarDistance renderer.LightingConfig.DepthOfFieldFocalType.Enumerate renderer.LightingConfig.DepthOfFieldFocalDistance renderer.LightingConfig.DepthOfFieldFocalPoint
                depthTexture colorHalf0Texture compositionTexture renderer.UnfilteredSampler colorFull0Texture geometryResolution
                renderer.QuadGeometry renderer.PhysicallyBasedPipelines.FilterDepthOfFieldPipeline renderer.VulkanContext
            Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead colorFull0Texture renderer.VulkanContext.RenderCommandBuffer

            // blit color full filter 0 texture to composition texture
            Texture.recordTransitionLayout ColorAttachmentRead TransferSrc colorFull0Texture renderer.VulkanContext.RenderCommandBuffer
            Texture.recordTransitionLayout ColorAttachmentRead TransferDst compositionTexture renderer.VulkanContext.RenderCommandBuffer
            let bounds = VkRect2D (0, 0, uint geometryResolution.X, uint geometryResolution.Y)
            let mutable region = Hl.makeBlit 0 0 0 0 bounds bounds
            DeviceApi.vkCmdBlitImage (renderer.VulkanContext.RenderCommandBuffer, colorFull0Texture.Image, TransferSrc.VkImageLayout, compositionTexture.Image, TransferDst.VkImageLayout, 1u, &&region, VkFilter.Nearest)
            Texture.recordTransitionLayout TransferSrc ColorAttachmentRead colorFull0Texture renderer.VulkanContext.RenderCommandBuffer
            Texture.recordTransitionLayout TransferDst ColorAttachmentRead compositionTexture renderer.VulkanContext.RenderCommandBuffer

        // run tone-mapping pass when appropriate
        let toneMappingTexture = renderer.PhysicallyBasedAttachments.ToneMappingAttachment
        Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite toneMappingTexture renderer.VulkanContext.RenderCommandBuffer
        PhysicallyBased.drawFilterToneMappingSurface
            renderer.LightingConfig.LightExposure renderer.LightingConfig.ToneMapType renderer.LightingConfig.ToneMapSlope renderer.LightingConfig.ToneMapOffset
            renderer.LightingConfig.ToneMapPower renderer.LightingConfig.ToneMapSaturation renderer.LightingConfig.ToneMapWhitePoint
            compositionTexture renderer.UnfilteredSampler toneMappingTexture geometryResolution
            renderer.QuadGeometry renderer.PhysicallyBasedPipelines.FilterToneMappingPipeline renderer.VulkanContext
        Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead toneMappingTexture renderer.VulkanContext.RenderCommandBuffer

        // apply fxaa filter when desired
        if renderer.RendererConfig.FxaaEnabled then

            // run fxaa pass from tone-mapping texture to color full 0
            let colorFull0Texture = renderer.PhysicallyBasedAttachments.ColorFull0Attachment
            Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite colorFull0Texture renderer.VulkanContext.RenderCommandBuffer
            PhysicallyBased.drawFilterFxaaSurface
                renderer.RendererConfig.FxaaSpanMax renderer.RendererConfig.FxaaReduceMinDivisor renderer.RendererConfig.FxaaReduceMulDivisor
                toneMappingTexture renderer.UnfilteredSampler colorFull0Texture geometryResolution
                renderer.QuadGeometry renderer.PhysicallyBasedPipelines.FilterFxaaPipeline renderer.VulkanContext

            // blit from color full 0 back to tone-mapping texture
            Texture.recordTransitionLayout ColorAttachmentWrite TransferSrc colorFull0Texture renderer.VulkanContext.RenderCommandBuffer
            Texture.recordTransitionLayout ColorAttachmentRead TransferDst toneMappingTexture renderer.VulkanContext.RenderCommandBuffer
            let bounds = VkRect2D (0, 0, uint geometryResolution.X, uint geometryResolution.Y)
            let mutable region = Hl.makeBlit 0 0 0 0 bounds bounds
            DeviceApi.vkCmdBlitImage (renderer.VulkanContext.RenderCommandBuffer, colorFull0Texture.Image, TransferSrc.VkImageLayout, toneMappingTexture.Image, TransferDst.VkImageLayout, 1u, &&region, VkFilter.Nearest)
            Texture.recordTransitionLayout TransferSrc ColorAttachmentRead colorFull0Texture renderer.VulkanContext.RenderCommandBuffer
            Texture.recordTransitionLayout TransferDst ColorAttachmentRead toneMappingTexture renderer.VulkanContext.RenderCommandBuffer

        // apply chromatic aberration texture when desired
        if topLevelRender && renderer.RendererConfig.ChromaticAberrationEnabled && renderer.LightingConfig.ChromaticAberrationEnabled then

            // run fxaa pass from tone-mapping texture to color full 0
            let colorFull0Texture = renderer.PhysicallyBasedAttachments.ColorFull0Attachment
            Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite colorFull0Texture renderer.VulkanContext.RenderCommandBuffer
            PhysicallyBased.drawFilterChromaticAberrationSurface
                renderer.LightingConfig.ChromaticAberrationChannelOffsets renderer.LightingConfig.ChromaticAberrationFocalPoint
                toneMappingTexture renderer.UnfilteredSampler colorFull0Texture geometryResolution
                renderer.QuadGeometry renderer.PhysicallyBasedPipelines.FilterChromaticAberrationPipeline renderer.VulkanContext

            // blit from color full 0 back to tone-mapping texture
            Texture.recordTransitionLayout ColorAttachmentWrite TransferSrc colorFull0Texture renderer.VulkanContext.RenderCommandBuffer
            Texture.recordTransitionLayout ColorAttachmentRead TransferDst toneMappingTexture renderer.VulkanContext.RenderCommandBuffer
            let bounds = VkRect2D (0, 0, uint geometryResolution.X, uint geometryResolution.Y)
            let mutable region = Hl.makeBlit 0 0 0 0 bounds bounds
            DeviceApi.vkCmdBlitImage (renderer.VulkanContext.RenderCommandBuffer, colorFull0Texture.Image, TransferSrc.VkImageLayout, toneMappingTexture.Image, TransferDst.VkImageLayout, 1u, &&region, VkFilter.Nearest)
            Texture.recordTransitionLayout TransferSrc ColorAttachmentRead colorFull0Texture renderer.VulkanContext.RenderCommandBuffer
            Texture.recordTransitionLayout TransferDst ColorAttachmentRead toneMappingTexture renderer.VulkanContext.RenderCommandBuffer

        // run gamma-correction pass when needed
        let intermediateTexture =
            if topLevelRender then
                let gammaCorrectionTexture = renderer.PhysicallyBasedAttachments.GammaCorrectionAttachment
                Texture.recordTransitionLayout ColorAttachmentRead ColorAttachmentWrite gammaCorrectionTexture renderer.VulkanContext.RenderCommandBuffer
                PhysicallyBased.drawFilterGammaCorrectionSurface
                    toneMappingTexture renderer.UnfilteredSampler gammaCorrectionTexture geometryResolution
                    renderer.QuadGeometry renderer.PhysicallyBasedPipelines.FilterGammaCorrectionPipeline renderer.VulkanContext
                Texture.recordTransitionLayout ColorAttachmentWrite ColorAttachmentRead gammaCorrectionTexture renderer.VulkanContext.RenderCommandBuffer
                gammaCorrectionTexture
            else toneMappingTexture

        // blit from intermediate texture to target image without filtering
        Texture.recordTransitionLayout ColorAttachmentRead TransferSrc intermediateTexture renderer.VulkanContext.RenderCommandBuffer
        Hl.recordTransitionLayout true 1 targetLayer 1 VkImageAspectFlags.Color ColorAttachmentWrite TransferDst targetImage renderer.VulkanContext.RenderCommandBuffer
        let mutable region = Hl.makeBlit 0 0 0 targetLayer (VkRect2D (0, 0, uint geometryResolution.X, uint geometryResolution.Y)) targetBounds
        DeviceApi.vkCmdBlitImage (renderer.VulkanContext.RenderCommandBuffer, intermediateTexture.Image, TransferSrc.VkImageLayout, targetImage, TransferDst.VkImageLayout, 1u, &&region, VkFilter.Nearest)
        Hl.recordTransitionLayout true 1 targetLayer 1 VkImageAspectFlags.Color TransferDst ColorAttachmentWrite targetImage renderer.VulkanContext.RenderCommandBuffer
        Texture.recordTransitionLayout TransferSrc ColorAttachmentRead intermediateTexture renderer.VulkanContext.RenderCommandBuffer

        // advance render pass index
        renderer.RenderPassIndex <- inc renderer.RenderPassIndex

    /// Render 3d surfaces.
    static member render
        frustumInterior
        frustumExterior
        frustumImposter
        eyeCenter
        eyeRotation
        eyeFieldOfView
        geometryViewport
        windowViewport
        renderGeometry
        (renderer : VulkanRenderer3d) =

        // update viewports
        if renderer.GeometryViewport <> geometryViewport then
            VulkanRenderer3d.invalidateCaches renderer
            VulkanRenderer3d.clearRenderPasses false renderer // force shadows to rerender, but keep light probes for light map tracking
            renderer.GeometryViewport <- geometryViewport
        renderer.WindowViewport <- windowViewport

        // update attachment sizes
        PhysicallyBased.updatePhysicallyBasedAttachmentsSize
            geometryViewport renderer.PhysicallyBasedAttachments renderer.VulkanContext

        // delete textures as requested on previous frame
        TextureDumpster.dump renderer.TextureDumpster renderer.VulkanContext

        // reload render assets when requested on previous frame
        if renderer.ReloadAssetsRequested then
            VulkanRenderer3d.handleReloadRenderAssets renderer
            renderer.ReloadAssetsRequested <- false

        // render when allowed
        if renderer.VulkanContext.RenderAllowed then

            // render light maps
            VulkanRenderer3d.renderLightMaps frustumInterior frustumExterior frustumImposter renderer

            // render shadows
            VulkanRenderer3d.renderShadows eyeCenter renderer

            // render top-level geometry pass
            if renderGeometry then
                let view = Viewport.getView3d eyeCenter eyeRotation
                let viewSkyBox = Matrix4x4.CreateFromQuaternion eyeRotation.Inverted
                let geometryFrustum = Viewport.getFrustum eyeCenter eyeRotation eyeFieldOfView geometryViewport
                let geometryProjection = Viewport.getProjection3d eyeFieldOfView geometryViewport
                let windowProjection = Viewport.getProjection3d eyeFieldOfView windowViewport
                let targetBounds =
                    VkRect2D
                        (renderer.WindowViewport.Inner.Min.X,
                         renderer.WindowViewport.Outer.Max.Y - renderer.WindowViewport.Inner.Max.Y,
                         uint renderer.WindowViewport.Inner.Size.X,
                         uint renderer.WindowViewport.Inner.Size.Y)
                let normalPass = NormalPass
                let normalTasks = VulkanRenderer3d.getRenderTasks normalPass renderer
                VulkanRenderer3d.renderGeometry
                    frustumInterior frustumExterior frustumImposter normalPass normalTasks renderer true None
                    eyeCenter view viewSkyBox geometryFrustum geometryProjection windowProjection
                    targetBounds 0 renderer.VulkanContext.SwapchainImage
        
        // clear config dirty flags
        renderer.LightingConfigChanged <- false
        renderer.RendererConfigChanged <- false

        // clear light shadow indices
        renderer.LightShadowIndices.Clear ()

        // clear lights desiring shadows
        renderer.LightsDesiringShadows.Clear ()

        // destroy cached terrain geometries that weren't rendered this frame
        for geometry in renderer.TerrainGeometries do
            if not (renderer.TerrainGeometriesUtilized.Contains geometry.Key) then
                for patchGeometry in geometry.Value do
                    PhysicallyBased.destroyPhysicallyBasedGeometry patchGeometry.Value renderer.VulkanContext
                    renderer.TerrainGeometries.Remove geometry.Key |> ignore<bool>
        renderer.TerrainGeometriesUtilized.Clear ()

        // destroy user-defined static models
        for staticModel in renderer.UserDefinedStaticModelsToDestroy do
            VulkanRenderer3d.tryDestroyUserDefinedStaticModel staticModel renderer
        renderer.UserDefinedStaticModelsToDestroy.Clear ()

        // swap render passes
        for renderTasks in renderer.RenderPasses.Values do RenderTasks.sweep renderTasks
        for renderTasks in renderer.RenderPasses2.Values do RenderTasks.clear true renderTasks
        let renderPasses = renderer.RenderPasses
        renderer.RenderPasses <- renderer.RenderPasses2
        renderer.RenderPasses2 <- renderPasses

    /// Make a VulkanRenderer3d.
    static member make geometryViewport windowViewport context =

        // start lazy texture server
        let lazyTextureQueues = ConcurrentDictionary<LazyTexture ConcurrentQueue, LazyTexture ConcurrentQueue> HashIdentity.Reference
        let textureServer = TextureServer (lazyTextureQueues, context)
        textureServer.Start ()

        // create texture dumpster
        let textureDumpster = TextureDumpster.create ()

        // create samplers
        let unfilteredSampler = Sampler.create VkSamplerAddressMode.ClampToEdge VkFilter.Nearest VkFilter.Nearest false context
        let filteredSampler = Sampler.create VkSamplerAddressMode.ClampToEdge VkFilter.Linear VkFilter.Linear false context
        let materialSampler = Sampler.create VkSamplerAddressMode.Repeat VkFilter.Linear VkFilter.Linear true context

        // create physically-based attachments using the geometry viewport
        let physicallyBasedAttachments = PhysicallyBased.createPhysicallyBasedAttachments geometryViewport context

        // create sky box pipeline
        let compositionTexture = physicallyBasedAttachments.CompositionAttachment
        let (_, _, _, _, _, _, _, zTexture) = physicallyBasedAttachments.GeometryAttachments
        let skyBoxPipeline = SkyBox.createSkyBoxPipeline compositionTexture.VkFormat zTexture.VkFormat context

        // create irradiance pipeline
        let irradianceFormat = Rgba16f
        let irradiancePipeline = CubeMap.createCubeMapPipeline Constants.Paths.IrradianceShaderFilePath irradianceFormat.VkFormat context

        // create environment filter pipeline
        let environmentFilterFormat = Rgba16f
        let environmentFilterPipeline = LightMap.createEnvironmentFilterPipeline Constants.Paths.EnvironmentFilterShaderFilePath environmentFilterFormat.VkFormat context

        // create physically-based pipelines
        let physicallyBasedPipelines =
            PhysicallyBased.createPhysicallyBasedPipelines
                Constants.Render.LightMapsMaxDeferred
                Constants.Render.LightsMaxDeferred
                physicallyBasedAttachments
                context

        // create shadow matrices flipped buffer
        let shadowMatricesFlippedCount = Constants.Render.ShadowTexturesMax + Constants.Render.ShadowCascadesMax * Constants.Render.ShadowCascadeLevels
        let shadowMatricesFlipped = Array.zeroCreate<Matrix4x4> shadowMatricesFlippedCount

        // create white cube map
        let cubeMap =
            let white = "Assets/Default/White.png"
            match CubeMap.tryCreateCubeMap white white white white white white RenderThread context with
            | Right cubeMap -> cubeMap
            | Left error -> failwith error

        // create cube map geometry
        let cubeMapGeometry = CubeMap.createCubeMapGeometry true context

        // create physically-based billboard geometry
        let billboardGeometry = PhysicallyBased.createPhysicallyBasedBillboardGeometry (Some context)

        // create physically-based quad
        let quadGeometry = PhysicallyBased.createPhysicallyBasedQuadGeometry (Some context)
        
        // create white texture
        let whiteTexture =
            match TextureInternal.tryCreate false true Uncompressed "Assets/Default/White.png" RenderThread context with
            | Right textureInternal -> EagerTexture textureInternal
            | Left error -> failwith ("Could not load white texture due to: " + error)

        // create black texture
        let blackTexture =
            match TextureInternal.tryCreate false true Uncompressed "Assets/Default/Black.png" RenderThread context with
            | Right textureInternal -> EagerTexture textureInternal
            | Left error -> failwith ("Could not load black texture due to: " + error)
        
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
                                VulkanRenderer3d.integrateBrdf nov roughness Constants.Render.BrdfSamples|]
                        |> Array.map (fun v -> [|BitConverter.GetBytes v.X; BitConverter.GetBytes v.Y|])
                        |> Array.concat
                        |> Array.concat
                    File.WriteAllBytes (brdfFilePath, brdfBuffer)
                    brdfBuffer
            let brdfMetadata = TextureMetadata.make Constants.Render.BrdfResolution Constants.Render.BrdfResolution
            let brdfTextureInternal = TextureInternal.create MipmapNone AttachmentNone Texture2d VkImageUsageFlags.None Rg32f Rg brdfMetadata context
            TextureInternal.uploadArray brdfMetadata 0 0 brdfBuffer RenderThread brdfTextureInternal context
            EagerTexture brdfTextureInternal

        // create default irradiance map and default environment filter map and set up transiently
        let commandBuffer = Hl.createTransientCommandBuffer context.TransientCommandPool
        let irradianceMap =
            LightMap.createIrradianceMap
                Constants.Render.IrradianceMapResolution
                (CubeMapSurface.make false cubeMap cubeMapGeometry)
                filteredSampler
                irradianceFormat
                irradiancePipeline
                (fun () -> commandBuffer)
                ignore
                context
        let environmentFilterMap =
            LightMap.createEnvironmentFilterMap
                Constants.Render.EnvironmentFilterResolution
                (CubeMapSurface.make true cubeMap cubeMapGeometry)
                filteredSampler
                environmentFilterFormat
                environmentFilterPipeline
                (fun () -> commandBuffer)
                ignore
                context
        let fence = Hl.createFence false
        ConcurrentCommandQueue.runTransient commandBuffer context.TransientCommandPool fence context.RenderQueue
        DeviceApi.vkDestroyFence (fence, nullPtr)

        // compute compressed image file extension
        let ext =
            match Constants.Render.TextureBlockCompression with
            | BcCompression -> ".dds"
            | AstcCompression -> ".ktx"
        
        // get albedo metadata and texture
        let albedoTexture =
            match TextureInternal.tryCreate false true ColorCompression ("Assets/Default/MaterialAlbedo" + ext) RenderThread context with
            | Right textureInternal -> EagerTexture textureInternal
            | Left error -> failwith ("Could not load albedo material texture due to: " + error)

        // create default physically-based material
        let physicallyBasedMaterial : PhysicallyBasedMaterial =
            let roughnessTexture =
                match TextureInternal.tryCreate false true ColorCompression ("Assets/Default/MaterialRoughness" + ext) RenderThread context with
                | Right textureInternal -> EagerTexture textureInternal
                | Left error -> failwith ("Could not load material roughness texture due to: " + error)
            let metallicTexture =
                match TextureInternal.tryCreate false true ColorCompression ("Assets/Default/MaterialMetallic" + ext) RenderThread context with
                | Right textureInternal -> EagerTexture textureInternal
                | Left error -> failwith ("Could not load material metallic texture due to: " + error)
            let ambientOcclusionTexture =
                match TextureInternal.tryCreate false true ColorCompression ("Assets/Default/MaterialAmbientOcclusion" + ext) RenderThread context with
                | Right textureInternal -> EagerTexture textureInternal
                | Left error -> failwith ("Could not load material ambient occlusion texture due to: " + error)
            let emissionTexture =
                match TextureInternal.tryCreate false true ColorCompression ("Assets/Default/MaterialEmission" + ext) RenderThread context with
                | Right textureInternal -> EagerTexture textureInternal
                | Left error -> failwith ("Could not load material emission texture due to: " + error)
            let normalTexture =
                match TextureInternal.tryCreate false true NormalCompression ("Assets/Default/MaterialNormal" + ext) RenderThread context with
                | Right textureInternal -> EagerTexture textureInternal
                | Left error -> failwith ("Could not load material normal texture due to: " + error)
            let heightTexture =
                match TextureInternal.tryCreate false true ColorCompression ("Assets/Default/MaterialHeight" + ext) RenderThread context with
                | Right textureInternal -> EagerTexture textureInternal
                | Left error -> failwith ("Could not load material height texture due to: " + error)
            let subdermalTexture =
                match TextureInternal.tryCreate false true ColorCompression ("Assets/Default/MaterialSubdermal" + ext) RenderThread context with
                | Right textureInternal -> EagerTexture textureInternal
                | Left error -> failwith ("Could not load material subdermal texture due to: " + error)
            let finenessTexture =
                match TextureInternal.tryCreate false true ColorCompression ("Assets/Default/MaterialFineness" + ext) RenderThread context with
                | Right textureInternal -> EagerTexture textureInternal
                | Left error -> failwith ("Could not load material fineness texture due to: " + error)
            let scatterTexture =
                match TextureInternal.tryCreate false true ColorCompression ("Assets/Default/MaterialSubdermal" + ext) RenderThread context with
                | Right textureInternal -> EagerTexture textureInternal
                | Left error -> failwith ("Could not load material scatter texture due to: " + error)
            let clearCoatTexture =
                match TextureInternal.tryCreate false true ColorCompression ("Assets/Default/MaterialClearCoat" + ext) RenderThread context with
                | Right textureInternal -> EagerTexture textureInternal
                | Left error -> failwith ("Could not load material clear coat texture due to: " + error)
            let clearCoatRoughnessTexture =
                match TextureInternal.tryCreate false true ColorCompression ("Assets/Default/MaterialClearCoatRoughness" + ext) RenderThread context with
                | Right textureInternal -> EagerTexture textureInternal
                | Left error -> failwith ("Could not load material clear coat roughness texture due to: " + error)
            let clearCoatNormalTexture =
                match TextureInternal.tryCreate false true NormalCompression ("Assets/Default/MaterialClearCoatNormal" + ext) RenderThread context with
                | Right textureInternal -> EagerTexture textureInternal
                | Left error -> failwith ("Could not load material clear coat normal texture due to: " + error)
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
              TwoSided = false
              Clipped = false
              Names = "" }
        
        // create forward surfaces comparer
        let forwardSurfacesComparer =
            { new IComparer<struct (single * single * Matrix4x4 * bool * Presence * Box2 * MaterialProperties * Matrix4x4 array voption * PhysicallyBasedSurface * DepthTest * single * int)> with
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
              WindowViewport = windowViewport
              UnfilteredSampler = unfilteredSampler
              FilteredSampler = filteredSampler
              MaterialSampler = materialSampler
              RenderPassIndex = 0
              GeometryInstanced = hashSetPlus HashIdentity.Reference []
              SkyBoxPipeline = skyBoxPipeline
              IrradiancePipeline = irradiancePipeline
              EnvironmentFilterPipeline = environmentFilterPipeline
              PhysicallyBasedPipelines = physicallyBasedPipelines
              ShadowMatricesFlipped = shadowMatricesFlipped
              LightShadowIndices = dictPlus HashIdentity.Structural []
              LightsDesiringShadows = dictPlus HashIdentity.Structural []
              CubeMapGeometry = cubeMapGeometry
              BillboardGeometry = billboardGeometry
              QuadGeometry = quadGeometry
              TerrainGeometries = Dictionary HashIdentity.Structural
              TerrainGeometriesUtilized = HashSet HashIdentity.Structural
              CubeMap = cubeMap
              WhiteTexture = whiteTexture
              BlackTexture = blackTexture
              BrdfTexture = brdfTexture
              IrradianceMap = irradianceMap
              EnvironmentFilterMap = environmentFilterMap
              PhysicallyBasedMaterial = physicallyBasedMaterial
              PhysicallyBasedAttachments = physicallyBasedAttachments
              LightMaps = dictPlus HashIdentity.Structural []
              LightingConfig = Lighting3dConfig.defaultConfig
              LightingConfigChanged = false
              RendererConfig = Renderer3dConfig.defaultConfig
              RendererConfigChanged = false
              InstanceFields = Array.zeroCreate<single> (Constants.Render.InstanceFieldCount * Constants.Render.InstanceBatchPrealloc)
              UserDefinedStaticModelFields = [||]
              UserDefinedStaticModelsToDestroy = SList.make ()
              ForwardSurfacesComparer = forwardSurfacesComparer
              ForwardSurfacesSortBuffer = List ()
              RenderPackages = dictPlus StringComparer.Ordinal []
              RenderPasses = dictPlus HashIdentity.Structural [(NormalPass, RenderTasks.make ())]
              RenderPasses2 = dictPlus HashIdentity.Structural [(NormalPass, RenderTasks.make ())]
              RenderPackageCachedOpt = Unchecked.defaultof<_>
              RenderAssetCached = { CachedAssetTagOpt = Unchecked.defaultof<_>; CachedRenderAsset = Unchecked.defaultof<_> }
              ReloadAssetsRequested = false
              LazyTextureQueues = lazyTextureQueues
              TextureServer = textureServer
              TextureDumpster = textureDumpster
              VulkanContext = context }

        // fin
        renderer

    interface Renderer3d with

        member renderer.RendererConfig =
            renderer.RendererConfig

        member renderer.PreRender frustumInterior frustumExterior frustumImposter eyeCenter eyeRotation renderMessages =
            VulkanRenderer3d.preRender frustumInterior frustumExterior frustumImposter eyeCenter eyeRotation renderMessages renderer

        member renderer.Render frustumInterior frustumExterior frustumImposter eyeCenter eyeRotation eyeFieldOfView geometryViewport windowViewport renderMessages =
            VulkanRenderer3d.render frustumInterior frustumExterior frustumImposter eyeCenter eyeRotation eyeFieldOfView geometryViewport windowViewport renderMessages renderer

        member renderer.CleanUp () =

            // destroy omnipresent samplers
            Sampler.destroy renderer.FilteredSampler
            Sampler.destroy renderer.UnfilteredSampler
            Sampler.destroy renderer.MaterialSampler

            // destroy omnipresent pipelines
            SkyBox.destroySkyBoxPipeline renderer.SkyBoxPipeline renderer.VulkanContext
            CubeMap.destroyCubeMapPipeline renderer.IrradiancePipeline.Pipeline renderer.VulkanContext
            LightMap.destroyEnvironmentFilterPipeline renderer.EnvironmentFilterPipeline renderer.VulkanContext
            PhysicallyBased.destroyPhysicallyBasedPipelines renderer.PhysicallyBasedPipelines renderer.VulkanContext

            // destroy omnipresent geometry
            CubeMap.destroyCubeMapGeometry renderer.CubeMapGeometry renderer.VulkanContext
            PhysicallyBased.destroyPhysicallyBasedGeometry renderer.BillboardGeometry renderer.VulkanContext
            PhysicallyBased.destroyPhysicallyBasedGeometry renderer.QuadGeometry renderer.VulkanContext

            // destroy terrain geometry
            for geometries in renderer.TerrainGeometries.Values do
                for geometry in geometries.Values do
                    PhysicallyBased.destroyPhysicallyBasedGeometry geometry renderer.VulkanContext
            renderer.TerrainGeometries.Clear ()

            // destroy omnipresent textures
            Texture.destroy renderer.CubeMap renderer.VulkanContext
            Texture.destroy renderer.WhiteTexture renderer.VulkanContext
            Texture.destroy renderer.BlackTexture renderer.VulkanContext
            Texture.destroy renderer.BrdfTexture renderer.VulkanContext
            Texture.destroy renderer.IrradianceMap renderer.VulkanContext
            Texture.destroy renderer.EnvironmentFilterMap renderer.VulkanContext

            // destroy omnipresent material
            Texture.destroy renderer.PhysicallyBasedMaterial.AlbedoTexture renderer.VulkanContext
            Texture.destroy renderer.PhysicallyBasedMaterial.RoughnessTexture renderer.VulkanContext
            Texture.destroy renderer.PhysicallyBasedMaterial.MetallicTexture renderer.VulkanContext
            Texture.destroy renderer.PhysicallyBasedMaterial.AmbientOcclusionTexture renderer.VulkanContext
            Texture.destroy renderer.PhysicallyBasedMaterial.EmissionTexture renderer.VulkanContext
            Texture.destroy renderer.PhysicallyBasedMaterial.NormalTexture renderer.VulkanContext
            Texture.destroy renderer.PhysicallyBasedMaterial.HeightTexture renderer.VulkanContext
            Texture.destroy renderer.PhysicallyBasedMaterial.SubdermalTexture renderer.VulkanContext
            Texture.destroy renderer.PhysicallyBasedMaterial.FinenessTexture renderer.VulkanContext
            Texture.destroy renderer.PhysicallyBasedMaterial.ScatterTexture renderer.VulkanContext
            Texture.destroy renderer.PhysicallyBasedMaterial.ClearCoatTexture renderer.VulkanContext
            Texture.destroy renderer.PhysicallyBasedMaterial.ClearCoatRoughnessTexture renderer.VulkanContext
            Texture.destroy renderer.PhysicallyBasedMaterial.ClearCoatNormalTexture renderer.VulkanContext

            // destroy omnipresent attachments
            PhysicallyBased.destroyPhysicallyBasedAttachments renderer.PhysicallyBasedAttachments renderer.VulkanContext

            // destroy light maps
            for lightMap in renderer.LightMaps.Values do
                LightMap.destroyLightMap lightMap renderer.VulkanContext
            renderer.LightMaps.Clear ()

            // destroy loaded assets
            let renderPackages = renderer.RenderPackages |> Seq.map (fun entry -> entry.Value)
            let renderAssets = renderPackages |> Seq.map (fun package -> package.Assets.Values) |> Seq.concat
            for (_, _, asset) in renderAssets do VulkanRenderer3d.freeRenderAsset asset renderer
            renderer.RenderPackages.Clear ()

            // destroy texture dumpster
            TextureDumpster.destroy renderer.TextureDumpster renderer.VulkanContext

            // terminate lazy texture server
            renderer.TextureServer.Terminate ()