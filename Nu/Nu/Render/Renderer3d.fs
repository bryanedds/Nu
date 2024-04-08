// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Numerics
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
    { AlbedoOpt : Color option
      RoughnessOpt : single option
      MetallicOpt : single option
      AmbientOcclusionOpt : single option
      EmissionOpt : single option
      HeightOpt : single option
      IgnoreLightMapsOpt : bool option
      OpaqueDistanceOpt : single option }

    member this.Albedo = Option.defaultValue Constants.Render.AlbedoDefault this.AlbedoOpt
    member this.Roughness = Option.defaultValue Constants.Render.RoughnessDefault this.RoughnessOpt
    member this.Metallic = Option.defaultValue Constants.Render.MetallicDefault this.MetallicOpt
    member this.AmbientOcclusion = Option.defaultValue Constants.Render.AmbientOcclusionDefault this.AmbientOcclusionOpt
    member this.Emission = Option.defaultValue Constants.Render.EmissionDefault this.EmissionOpt
    member this.Height = Option.defaultValue Constants.Render.HeightDefault this.HeightOpt
    member this.IgnoreLightMaps = Option.defaultValue Constants.Render.IgnoreLightMapsDefault this.IgnoreLightMapsOpt
    member this.OpaqueDistance = Option.defaultValue Constants.Render.OpaqueDistanceDefault this.OpaqueDistanceOpt

[<RequireQualifiedAccess>]
module MaterialProperties =

    /// Material properties with populated default properties.
    let defaultProperties =
        { AlbedoOpt = Some Constants.Render.AlbedoDefault
          RoughnessOpt = Some Constants.Render.RoughnessDefault
          MetallicOpt = Some Constants.Render.MetallicDefault
          AmbientOcclusionOpt = Some Constants.Render.AmbientOcclusionDefault
          EmissionOpt = Some Constants.Render.EmissionDefault
          HeightOpt = Some Constants.Render.HeightDefault
          IgnoreLightMapsOpt = Some Constants.Render.IgnoreLightMapsDefault
          OpaqueDistanceOpt = Some Constants.Render.OpaqueDistanceDefault }

    /// Empty material properties.
    let empty =
        { AlbedoOpt = None
          RoughnessOpt = None
          MetallicOpt = None
          AmbientOcclusionOpt = None
          EmissionOpt = None
          HeightOpt = None
          IgnoreLightMapsOpt = None
          OpaqueDistanceOpt = None }

/// Material description for surfaces.
type [<SymbolicExpansion>] Material =
    { AlbedoImageOpt : Image AssetTag option
      RoughnessImageOpt : Image AssetTag option
      MetallicImageOpt : Image AssetTag option
      AmbientOcclusionImageOpt : Image AssetTag option
      EmissionImageOpt : Image AssetTag option
      NormalImageOpt : Image AssetTag option
      HeightImageOpt : Image AssetTag option
      TwoSidedOpt : bool option }

    member this.AlbedoImage = Option.defaultValue (asset Assets.Default.PackageName Assets.Default.MaterialAlbedoName) this.AlbedoImageOpt
    member this.RoughnessImage = Option.defaultValue (asset Assets.Default.PackageName Assets.Default.MaterialRoughnessName) this.RoughnessImageOpt
    member this.MetallicImage = Option.defaultValue (asset Assets.Default.PackageName Assets.Default.MaterialMetallicName) this.MetallicImageOpt
    member this.AmbientOcclusionImage = Option.defaultValue (asset Assets.Default.PackageName Assets.Default.MaterialAmbientOcclusionName) this.AmbientOcclusionImageOpt
    member this.EmissionImage = Option.defaultValue (asset Assets.Default.PackageName Assets.Default.MaterialEmissionName) this.EmissionImageOpt
    member this.NormalImage = Option.defaultValue (asset Assets.Default.PackageName Assets.Default.MaterialNormalName) this.NormalImageOpt
    member this.HeightImage = Option.defaultValue (asset Assets.Default.PackageName Assets.Default.MaterialHeightName) this.HeightImageOpt
    member this.TwoSided = Option.defaultValue false this.TwoSidedOpt

[<RequireQualifiedAccess>]
module Material =

    /// The material with populated default images.
    let defaultMaterial =
        { AlbedoImageOpt = Some (asset Assets.Default.PackageName Assets.Default.MaterialAlbedoName)
          RoughnessImageOpt = Some (asset Assets.Default.PackageName Assets.Default.MaterialRoughnessName)
          MetallicImageOpt = Some (asset Assets.Default.PackageName Assets.Default.MaterialMetallicName)
          AmbientOcclusionImageOpt = Some (asset Assets.Default.PackageName Assets.Default.MaterialAmbientOcclusionName)
          EmissionImageOpt = Some (asset Assets.Default.PackageName Assets.Default.MaterialEmissionName)
          NormalImageOpt = Some (asset Assets.Default.PackageName Assets.Default.MaterialNormalName)
          HeightImageOpt = Some (asset Assets.Default.PackageName Assets.Default.MaterialHeightName)
          TwoSidedOpt = Some false }

    /// The empty material.
    let empty =
        { AlbedoImageOpt = None
          RoughnessImageOpt = None
          MetallicImageOpt = None
          AmbientOcclusionImageOpt = None
          EmissionImageOpt = None
          NormalImageOpt = None
          HeightImageOpt = None
          TwoSidedOpt = None }

/// A mutable 3d light probe value.
type [<Struct>] LightProbe3dValue =
    { mutable LightProbeId : uint64
      mutable Enabled : bool
      mutable Origin : Vector3
      mutable Bounds : Box3
      mutable Stale : bool }

/// A mutable 3d light value.
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

/// A mutable billboard value.
type [<Struct>] BillboardValue =
    { mutable Absolute : bool
      mutable ModelMatrix : Matrix4x4
      mutable Presence : Presence
      mutable InsetOpt : Box2 option
      mutable MaterialProperties : MaterialProperties
      mutable Material : Material
      mutable RenderType : RenderType }

/// A mutable static model value.
type [<Struct>] StaticModelValue =
    { mutable Absolute : bool
      mutable ModelMatrix : Matrix4x4
      mutable Presence : Presence
      mutable InsetOpt : Box2 option
      mutable MaterialProperties : MaterialProperties
      mutable StaticModel : StaticModel AssetTag
      mutable RenderType : RenderType }

/// A mutable static model surface value.
type [<Struct>] StaticModelSurfaceValue =
    { mutable Absolute : bool
      mutable ModelMatrix : Matrix4x4
      mutable Presence : Presence
      mutable InsetOpt : Box2 option
      mutable MaterialProperties : MaterialProperties
      mutable Material : Material
      mutable StaticModel : StaticModel AssetTag
      mutable SurfaceIndex : int
      mutable RenderType : RenderType }

/// Describes billboard-based particles.
type BillboardParticlesDescriptor =
    { Absolute : bool
      MaterialProperties : MaterialProperties
      Material : Material
      Particles : Particle SArray
      RenderType : RenderType }

/// Describes a static 3d terrain geometry.
type TerrainGeometryDescriptor =
    { Bounds : Box3
      Material : TerrainMaterial
      TintImageOpt : Image AssetTag option
      NormalImageOpt : Image AssetTag option
      Tiles : Vector2
      HeightMap : HeightMap
      Segments : Vector2i }

/// Describes a static 3d terrain.
type TerrainDescriptor =
    { Bounds : Box3
      InsetOpt : Box2 option
      MaterialProperties : TerrainMaterialProperties
      Material : TerrainMaterial
      TintImageOpt : Image AssetTag option
      NormalImageOpt : Image AssetTag option
      Tiles : Vector2
      HeightMap : HeightMap
      Segments : Vector2i }

    member this.TerrainGeometryDescriptor =
        { Bounds = this.Bounds
          Material = this.Material
          TintImageOpt = this.TintImageOpt
          NormalImageOpt = this.NormalImageOpt
          Tiles = this.Tiles
          HeightMap = this.HeightMap
          Segments = this.Segments }

/// An internally cached static model used to reduce GC promotion or pressure.
type CachedStaticModelMessage =
    { mutable CachedStaticModelAbsolute : bool
      mutable CachedStaticModelMatrix : Matrix4x4
      mutable CachedStaticModelPresence : Presence
      mutable CachedStaticModelInsetOpt : Box2 voption
      mutable CachedStaticModelMaterialProperties : MaterialProperties
      mutable CachedStaticModel : StaticModel AssetTag
      mutable CachedStaticModelRenderType : RenderType
      mutable CachedStaticModelRenderPass : RenderPass }

/// An internally cached static model surface used to reduce GC promotion or pressure.
type CachedStaticModelSurfaceMessage =
    { mutable CachedStaticModelSurfaceAbsolute : bool
      mutable CachedStaticModelSurfaceMatrix : Matrix4x4
      mutable CachedStaticModelSurfacePresence : Presence
      mutable CachedStaticModelSurfaceInsetOpt : Box2 voption
      mutable CachedStaticModelSurfaceMaterialProperties : MaterialProperties
      mutable CachedStaticModelSurfaceMaterial : Material
      mutable CachedStaticModelSurfaceModel : StaticModel AssetTag
      mutable CachedStaticModelSurfaceIndex : int
      mutable CachedStaticModelSurfaceRenderType : RenderType
      mutable CachedStaticModelSurfaceRenderPass : RenderPass }

/// An internally cached animated model used to reduce GC promotion or pressure.
type CachedAnimatedModelMessage =
    { mutable CachedAnimatedModelAbsolute : bool
      mutable CachedAnimatedModelMatrix : Matrix4x4
      mutable CachedAnimatedModelPresence : Presence
      mutable CachedAnimatedModelInsetOpt : Box2 voption
      mutable CachedAnimatedModelMaterialProperties : MaterialProperties
      mutable CachedAnimatedModelBoneTransforms : Matrix4x4 array
      mutable CachedAnimatedModel : AnimatedModel AssetTag
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
      TwoSided : bool }

type [<ReferenceEquality>] CreateUserDefinedStaticModel =
    { StaticModelSurfaceDescriptors : StaticModelSurfaceDescriptor array
      Bounds : Box3
      StaticModel : StaticModel AssetTag }

type [<ReferenceEquality>] DestroyUserDefinedStaticModel =
    { StaticModel : StaticModel AssetTag }

type [<ReferenceEquality>] RenderSkyBox =
    { AmbientColor : Color
      AmbientBrightness : single
      CubeMapColor : Color
      CubeMapBrightness : single
      CubeMap : CubeMap AssetTag
      RenderPass : RenderPass }

type [<ReferenceEquality>] RenderLightProbe3d =
    { LightProbeId : uint64
      Enabled : bool
      Origin : Vector3
      Bounds : Box3
      RenderPass : RenderPass }

type [<ReferenceEquality>] RenderLightMap3d =
    { LightProbeId : uint64
      RenderPass : RenderPass }

type [<ReferenceEquality>] RenderLight3d =
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
      RenderPass : RenderPass }

type [<ReferenceEquality>] RenderBillboard =
    { Absolute : bool
      ModelMatrix : Matrix4x4
      Presence : Presence
      InsetOpt : Box2 option
      MaterialProperties : MaterialProperties
      Material : Material
      RenderType : RenderType
      RenderPass : RenderPass }

type [<ReferenceEquality>] RenderBillboards =
    { Absolute : bool
      Billboards : (Matrix4x4 * Presence * Box2 option) SList
      MaterialProperties : MaterialProperties
      Material : Material
      RenderType : RenderType
      RenderPass : RenderPass }

type [<ReferenceEquality>] RenderBillboardParticles =
    { Absolute : bool
      Presence : Presence
      MaterialProperties : MaterialProperties
      Material : Material
      Particles : Particle SArray
      RenderType : RenderType
      RenderPass : RenderPass }

type [<ReferenceEquality>] RenderStaticModelSurface =
    { Absolute : bool
      ModelMatrix : Matrix4x4
      Presence : Presence
      InsetOpt : Box2 option
      MaterialProperties : MaterialProperties
      Material : Material
      StaticModel : StaticModel AssetTag
      SurfaceIndex : int
      RenderType : RenderType
      RenderPass : RenderPass }

type [<ReferenceEquality>] RenderStaticModel =
    { Absolute : bool
      ModelMatrix : Matrix4x4
      Presence : Presence
      InsetOpt : Box2 option
      MaterialProperties : MaterialProperties
      StaticModel : StaticModel AssetTag
      RenderType : RenderType
      RenderPass : RenderPass }

type [<ReferenceEquality>] RenderStaticModels =
    { Absolute : bool
      StaticModels : (Matrix4x4 * Presence * Box2 option * MaterialProperties) SList
      StaticModel : StaticModel AssetTag
      RenderType : RenderType
      RenderPass : RenderPass }

type [<ReferenceEquality>] RenderAnimatedModel =
    { Absolute : bool
      ModelMatrix : Matrix4x4
      Presence : Presence
      InsetOpt : Box2 option
      MaterialProperties : MaterialProperties
      BoneTransforms : Matrix4x4 array
      AnimatedModel : AnimatedModel AssetTag
      RenderPass : RenderPass }

type [<ReferenceEquality>] RenderAnimatedModels =
    { Absolute : bool
      BoneTransforms : Matrix4x4 array
      AnimatedModels : (Matrix4x4 * Presence * Box2 option * MaterialProperties) SList
      AnimatedModel : AnimatedModel AssetTag
      RenderPass : RenderPass }

type [<ReferenceEquality>] RenderUserDefinedStaticModel =
    { Absolute : bool
      ModelMatrix : Matrix4x4
      Presence : Presence
      InsetOpt : Box2 option
      MaterialProperties : MaterialProperties
      StaticModelSurfaceDescriptors : StaticModelSurfaceDescriptor array
      Bounds : Box3
      RenderType : RenderType
      RenderPass : RenderPass }

type [<ReferenceEquality>] RenderTerrain =
    { Absolute : bool
      Visible : bool
      TerrainDescriptor : TerrainDescriptor
      RenderPass : RenderPass }

/// Configures lighting and ssao.
type [<ReferenceEquality; SymbolicExpansion>] LightingConfig =
    { LightCutoffMargin : single
      LightShadowBiasAcne : single
      LightShadowBiasBleed : single
      LightMappingEnabled : bool
      SsaoEnabled : bool
      SsaoIntensity : single
      SsaoBias : single
      SsaoRadius : single
      SsaoDistanceMax : single
      SsaoSampleCount : int }

    static member defaultConfig =
        { LightCutoffMargin = Constants.Render.LightCutoffMarginDefault
          LightShadowBiasAcne = Constants.Render.LightShadowBiasAcneDefault
          LightShadowBiasBleed = Constants.Render.LightShadowBiasBleedDefault
          LightMappingEnabled = Constants.Render.LightMappingEnabledDefault
          SsaoEnabled = Constants.Render.SsaoEnabledDefault
          SsaoIntensity = Constants.Render.SsaoIntensityDefault
          SsaoBias = Constants.Render.SsaoBiasDefault
          SsaoRadius = Constants.Render.SsaoRadiusDefault
          SsaoDistanceMax = Constants.Render.SsaoDistanceMaxDefault
          SsaoSampleCount = Constants.Render.SsaoSampleCountDefault }

/// A message to the 3d renderer.
type [<ReferenceEquality>] RenderMessage3d =
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
    | RenderStaticModel of RenderStaticModel
    | RenderStaticModels of RenderStaticModels
    | RenderCachedStaticModel of CachedStaticModelMessage
    | RenderCachedStaticModelSurface of CachedStaticModelSurfaceMessage
    | RenderUserDefinedStaticModel of RenderUserDefinedStaticModel
    | RenderAnimatedModel of RenderAnimatedModel
    | RenderAnimatedModels of RenderAnimatedModels
    | RenderCachedAnimatedModel of CachedAnimatedModelMessage
    | RenderTerrain of RenderTerrain
    | ConfigureLighting of LightingConfig
    | LoadRenderPackage3d of string
    | UnloadRenderPackage3d of string
    | ReloadRenderAssets3d

/// A sortable light map.
/// OPTIMIZATION: mutable field for caching distance squared.
type [<ReferenceEquality>] private SortableLightMap =
    { SortableLightMapEnabled : bool
      SortableLightMapOrigin : Vector3
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
    static member sortLightMapsIntoArrays lightMapsMax position boundsOpt lightMaps =
        let lightMapOrigins = Array.zeroCreate<single> (lightMapsMax * 3)
        let lightMapMins = Array.zeroCreate<single> (lightMapsMax * 3)
        let lightMapSizes = Array.zeroCreate<single> (lightMapsMax * 3)
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
                let i3 = i * 3
                let lightMap = lightMapsSorted.[i]
                lightMapOrigins.[i3] <- lightMap.SortableLightMapOrigin.X
                lightMapOrigins.[i3+1] <- lightMap.SortableLightMapOrigin.Y
                lightMapOrigins.[i3+2] <- lightMap.SortableLightMapOrigin.Z
                lightMapMins.[i3] <- lightMap.SortableLightMapBounds.Min.X
                lightMapMins.[i3+1] <- lightMap.SortableLightMapBounds.Min.Y
                lightMapMins.[i3+2] <- lightMap.SortableLightMapBounds.Min.Z
                lightMapSizes.[i3] <- lightMap.SortableLightMapBounds.Size.X
                lightMapSizes.[i3+1] <- lightMap.SortableLightMapBounds.Size.Y
                lightMapSizes.[i3+2] <- lightMap.SortableLightMapBounds.Size.Z
                lightMapIrradianceMaps.[i] <- lightMap.SortableLightMapIrradianceMap
                lightMapEnvironmentFilterMaps.[i] <- lightMap.SortableLightMapEnvironmentFilterMap
        (lightMapOrigins, lightMapMins, lightMapSizes, lightMapIrradianceMaps, lightMapEnvironmentFilterMaps, lightMapsSorted.Length)

/// A sortable light.
/// OPTIMIZATION: mutable field for caching distance squared.
type [<ReferenceEquality>] private SortableLight =
    { SortableLightId : uint64
      SortableLightOrigin : Vector3
      SortableLightRotation : Quaternion
      SortableLightDirection : Vector3
      SortableLightColor : Color
      SortableLightBrightness : single
      SortableLightAttenuationLinear : single
      SortableLightAttenuationQuadratic : single
      SortableLightCutoff : single
      SortableLightDirectional : int
      SortableLightConeInner : single
      SortableLightConeOuter : single
      SortableLightDesireShadows : int
      mutable SortableLightDistanceSquared : single }

    /// Sort lights into array for uploading to OpenGL.
    /// TODO: consider getting rid of allocation here.
    static member sortLightsIntoArrays lightsMax position lights =
        let lightIds = Array.zeroCreate<uint64> lightsMax
        let lightOrigins = Array.zeroCreate<single> (lightsMax * 3)
        let lightDirections = Array.zeroCreate<single> (lightsMax * 3)
        let lightColors = Array.zeroCreate<single> (lightsMax * 3)
        let lightBrightnesses = Array.zeroCreate<single> lightsMax
        let lightAttenuationLinears = Array.zeroCreate<single> lightsMax
        let lightAttenuationQuadratics = Array.zeroCreate<single> lightsMax
        let lightCutoffs = Array.zeroCreate<single> lightsMax
        let lightDirectionals = Array.zeroCreate<int> lightsMax
        let lightConeInners = Array.zeroCreate<single> lightsMax
        let lightConeOuters = Array.zeroCreate<single> lightsMax
        let lightDesireShadows = Array.zeroCreate<int> lightsMax
        for light in lights do
            light.SortableLightDistanceSquared <- (light.SortableLightOrigin - position).MagnitudeSquared
        let lightsSorted =
            lights |>
            Seq.toArray |>
            Array.sortBy (fun light -> struct (-light.SortableLightDirectional, light.SortableLightDistanceSquared))
        for i in 0 .. dec lightsMax do
            if i < lightsSorted.Length then
                let i3 = i * 3
                let light = lightsSorted.[i]
                lightIds.[i] <- light.SortableLightId
                lightOrigins.[i3] <- light.SortableLightOrigin.X
                lightOrigins.[i3+1] <- light.SortableLightOrigin.Y
                lightOrigins.[i3+2] <- light.SortableLightOrigin.Z
                lightDirections.[i3] <- light.SortableLightDirection.X
                lightDirections.[i3+1] <- light.SortableLightDirection.Y
                lightDirections.[i3+2] <- light.SortableLightDirection.Z
                lightColors.[i3] <- light.SortableLightColor.R
                lightColors.[i3+1] <- light.SortableLightColor.G
                lightColors.[i3+2] <- light.SortableLightColor.B
                lightBrightnesses.[i] <- light.SortableLightBrightness
                lightAttenuationLinears.[i] <- light.SortableLightAttenuationLinear
                lightAttenuationQuadratics.[i] <- light.SortableLightAttenuationQuadratic
                lightCutoffs.[i] <- light.SortableLightCutoff
                lightDirectionals.[i] <- light.SortableLightDirectional
                lightConeInners.[i] <- light.SortableLightConeInner
                lightConeOuters.[i] <- light.SortableLightConeOuter
                lightDesireShadows.[i] <- light.SortableLightDesireShadows
        (lightIds, lightOrigins, lightDirections, lightColors, lightBrightnesses, lightAttenuationLinears, lightAttenuationQuadratics, lightCutoffs, lightDirectionals, lightConeInners, lightConeOuters, lightDesireShadows)

    /// Sort shadow indices.
    static member sortShadowIndices (shadowIndices : Dictionary<uint64, int>) (lightIds : uint64 array) (lightDesireShadows : int array) lightsCount =
        [|for i in 0 .. dec lightsCount do
            if i < Constants.Render.ShadowsMax && lightDesireShadows.[i] <> 0 then
                match shadowIndices.TryGetValue lightIds.[i] with
                | (true, index) -> index
                | (false, _) -> -1 // TODO: log here?
            else -1|]

/// Enables efficient comparison of animated model surfaces.
type [<CustomEquality; NoComparison; Struct>] private AnimatedModelSurfaceKey =
    { BoneTransforms : Matrix4x4 array
      AnimatedModelSurface : OpenGL.PhysicallyBased.PhysicallyBasedSurface }

    static member hash amsKey =
        let mutable hashCode = 0
        for i in 0 .. dec amsKey.BoneTransforms.Length do hashCode <- hashCode ^^^ amsKey.BoneTransforms.[i].GetHashCode ()
        hashCode <- hashCode ^^^ OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.hash amsKey.AnimatedModelSurface
        hashCode

    static member equals left right =
        if left.BoneTransforms.Length = right.BoneTransforms.Length then
            let mutable equal = true
            let mutable i = 0
            while i < left.BoneTransforms.Length && equal do
                equal <- m4Eq left.BoneTransforms.[i] right.BoneTransforms.[i]
                i <- inc i
            equal && OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.equals left.AnimatedModelSurface right.AnimatedModelSurface
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
      LightProbes : Dictionary<uint64, struct (bool * Vector3 * Box3)>
      LightMaps : SortableLightMap List
      LightMapRenders : uint64 HashSet
      Lights : SortableLight List
      DeferredStaticAbsolute : Dictionary<OpenGL.PhysicallyBased.PhysicallyBasedSurface, struct (Matrix4x4 * Presence * Box2 * MaterialProperties) List>
      DeferredStaticRelative : Dictionary<OpenGL.PhysicallyBased.PhysicallyBasedSurface, struct (Matrix4x4 * Presence * Box2 * MaterialProperties) List>
      DeferredAnimatedAbsolute : Dictionary<AnimatedModelSurfaceKey, struct (Matrix4x4 * Presence * Box2 * MaterialProperties) List>
      DeferredAnimatedRelative : Dictionary<AnimatedModelSurfaceKey, struct (Matrix4x4 * Presence * Box2 * MaterialProperties) List>
      DeferredTerrainsAbsolute : struct (TerrainDescriptor * OpenGL.PhysicallyBased.PhysicallyBasedGeometry) List
      DeferredTerrainsRelative : struct (TerrainDescriptor * OpenGL.PhysicallyBased.PhysicallyBasedGeometry) List
      ForwardStaticAbsolute : struct (single * single * Matrix4x4 * Presence * Box2 * MaterialProperties * OpenGL.PhysicallyBased.PhysicallyBasedSurface) List
      ForwardStaticRelative : struct (single * single * Matrix4x4 * Presence * Box2 * MaterialProperties * OpenGL.PhysicallyBased.PhysicallyBasedSurface) List
      ForwardStaticAbsoluteSorted : struct (Matrix4x4 * Presence * Box2 * MaterialProperties * OpenGL.PhysicallyBased.PhysicallyBasedSurface) List
      ForwardStaticRelativeSorted : struct (Matrix4x4 * Presence * Box2 * MaterialProperties * OpenGL.PhysicallyBased.PhysicallyBasedSurface) List
      DeferredStaticRemovals : OpenGL.PhysicallyBased.PhysicallyBasedSurface List
      DeferredAnimatedRemovals : AnimatedModelSurfaceKey List }

    static member make () =
        { SkyBoxes = List ()
          LightProbes = Dictionary HashIdentity.Structural
          LightMapRenders = HashSet HashIdentity.Structural
          LightMaps = List ()
          Lights = List ()
          DeferredStaticAbsolute = dictPlus OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.comparer []
          DeferredStaticRelative = dictPlus OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.comparer []
          DeferredAnimatedAbsolute = dictPlus AnimatedModelSurfaceKey.comparer []
          DeferredAnimatedRelative = dictPlus AnimatedModelSurfaceKey.comparer []
          DeferredTerrainsAbsolute = List ()
          DeferredTerrainsRelative = List ()
          ForwardStaticAbsolute = List ()
          ForwardStaticRelative = List ()
          ForwardStaticAbsoluteSorted = List ()
          ForwardStaticRelativeSorted = List ()
          DeferredStaticRemovals = List ()
          DeferredAnimatedRemovals = List () }

    static member clear renderTasks =
        renderTasks.SkyBoxes.Clear ()
        renderTasks.LightProbes.Clear ()
        renderTasks.LightMapRenders.Clear ()
        renderTasks.LightMaps.Clear ()
        renderTasks.Lights.Clear ()

        for entry in renderTasks.DeferredStaticAbsolute do
            if entry.Value.Count = 0
            then renderTasks.DeferredStaticRemovals.Add entry.Key
            else entry.Value.Clear ()
        for removal in renderTasks.DeferredStaticRemovals do
            renderTasks.DeferredStaticAbsolute.Remove removal |> ignore<bool>
        renderTasks.DeferredStaticRemovals.Clear ()

        for entry in renderTasks.DeferredStaticRelative do
            if entry.Value.Count = 0
            then renderTasks.DeferredStaticRemovals.Add entry.Key
            else entry.Value.Clear ()
        for removal in renderTasks.DeferredStaticRemovals do
            renderTasks.DeferredStaticRelative.Remove removal |> ignore<bool>
        renderTasks.DeferredStaticRemovals.Clear ()

        for entry in renderTasks.DeferredAnimatedAbsolute do
            if entry.Value.Count = 0
            then renderTasks.DeferredAnimatedRemovals.Add entry.Key
            else entry.Value.Clear ()
        for removal in renderTasks.DeferredAnimatedRemovals do
            renderTasks.DeferredAnimatedAbsolute.Remove removal |> ignore<bool>
        renderTasks.DeferredAnimatedRemovals.Clear ()

        for entry in renderTasks.DeferredAnimatedRelative do
            if entry.Value.Count = 0
            then renderTasks.DeferredAnimatedRemovals.Add entry.Key
            else entry.Value.Clear ()
        for removal in renderTasks.DeferredAnimatedRemovals do
            renderTasks.DeferredAnimatedRelative.Remove removal |> ignore<bool>
        renderTasks.DeferredAnimatedRemovals.Clear ()

        renderTasks.ForwardStaticAbsoluteSorted.Clear ()
        renderTasks.ForwardStaticRelativeSorted.Clear ()
        renderTasks.DeferredTerrainsAbsolute.Clear ()
        renderTasks.DeferredTerrainsRelative.Clear ()

/// The 3d renderer. Represents a 3d rendering subsystem in Nu generally.
type Renderer3d =
    /// Render a frame of the game.
    abstract Render : Frustum -> Frustum -> Frustum -> Box3 -> Vector3 -> Quaternion -> Vector2i -> RenderMessage3d List -> unit
    /// Swap a rendered frame of the game.
    abstract Swap : unit -> unit
    /// Handle render clean up by freeing all loaded render assets.
    abstract CleanUp : unit -> unit

/// The stub implementation of Renderer3d.
type [<ReferenceEquality>] StubRenderer3d =
    private
        { StubRenderer3d : unit }

    interface Renderer3d with
        member renderer.Render _ _ _ _ _ _ _ _ = ()
        member renderer.Swap () = ()
        member renderer.CleanUp () = ()

    static member make () =
        { StubRenderer3d = () }

/// The OpenGL implementation of Renderer3d.
type [<ReferenceEquality>] GlRenderer3d =
    private
        { Window : Window
          LazyTextureQueues : ConcurrentDictionary<OpenGL.Texture.LazyTexture ConcurrentQueue, OpenGL.Texture.LazyTexture ConcurrentQueue>
          LazyTextureServer : OpenGL.Texture.LazyTextureServer
          SkyBoxShader : OpenGL.SkyBox.SkyBoxShader
          IrradianceShader : OpenGL.CubeMap.CubeMapShader
          EnvironmentFilterShader : OpenGL.LightMap.EnvironmentFilterShader
          FilterBox1dShader : OpenGL.Filter.FilterBoxShader
          FilterGaussian2dShader : OpenGL.Filter.FilterGaussianShader
          FilterFxaaShader : OpenGL.Filter.FilterFxaaShader
          PhysicallyBasedShadowStaticShader : OpenGL.PhysicallyBased.PhysicallyBasedShader
          PhysicallyBasedShadowAnimatedShader : OpenGL.PhysicallyBased.PhysicallyBasedShader
          PhysicallyBasedShadowTerrainShader : OpenGL.PhysicallyBased.PhysicallyBasedDeferredTerrainShader
          PhysicallyBasedDeferredStaticShader : OpenGL.PhysicallyBased.PhysicallyBasedShader
          PhysicallyBasedDeferredAnimatedShader : OpenGL.PhysicallyBased.PhysicallyBasedShader
          PhysicallyBasedDeferredTerrainShader : OpenGL.PhysicallyBased.PhysicallyBasedDeferredTerrainShader
          PhysicallyBasedDeferredLightMappingShader : OpenGL.PhysicallyBased.PhysicallyBasedDeferredLightMappingShader
          PhysicallyBasedDeferredIrradianceShader : OpenGL.PhysicallyBased.PhysicallyBasedDeferredIrradianceShader
          PhysicallyBasedDeferredEnvironmentFilterShader : OpenGL.PhysicallyBased.PhysicallyBasedDeferredEnvironmentFilterShader
          PhysicallyBasedDeferredSsaoShader : OpenGL.PhysicallyBased.PhysicallyBasedDeferredSsaoShader
          PhysicallyBasedDeferredLightingShader : OpenGL.PhysicallyBased.PhysicallyBasedDeferredLightingShader
          PhysicallyBasedForwardStaticShader : OpenGL.PhysicallyBased.PhysicallyBasedShader
          ShadowBuffersArray : (OpenGL.Texture.Texture * uint * uint) array
          ShadowBuffers2Array : (OpenGL.Texture.Texture * uint * uint) array
          ShadowMatrices : Matrix4x4 array
          ShadowIndices : Dictionary<uint64, int>
          GeometryBuffers : OpenGL.Texture.Texture * OpenGL.Texture.Texture * OpenGL.Texture.Texture * OpenGL.Texture.Texture * uint * uint
          LightMappingBuffers : OpenGL.Texture.Texture * uint * uint
          IrradianceBuffers : OpenGL.Texture.Texture * uint * uint
          EnvironmentFilterBuffers : OpenGL.Texture.Texture * uint * uint
          SsaoBuffersUnfiltered : OpenGL.Texture.Texture * uint * uint
          SsaoBuffersFiltered : OpenGL.Texture.Texture * uint * uint
          FilterBuffers : OpenGL.Texture.Texture * uint * uint
          CubeMapGeometry : OpenGL.CubeMap.CubeMapGeometry
          BillboardGeometry : OpenGL.PhysicallyBased.PhysicallyBasedGeometry
          PhysicallyBasedQuad : OpenGL.PhysicallyBased.PhysicallyBasedGeometry
          PhysicallyBasedTerrainGeometries : Dictionary<TerrainGeometryDescriptor, OpenGL.PhysicallyBased.PhysicallyBasedGeometry>
          PhysicallyBasedTerrainGeometriesUtilized : TerrainGeometryDescriptor HashSet
          CubeMap : OpenGL.Texture.Texture
          WhiteTexture : OpenGL.Texture.Texture
          BlackTexture : OpenGL.Texture.Texture
          BrdfTexture : OpenGL.Texture.Texture
          IrradianceMap : OpenGL.Texture.Texture
          EnvironmentFilterMap : OpenGL.Texture.Texture
          PhysicallyBasedMaterial : OpenGL.PhysicallyBased.PhysicallyBasedMaterial
          LightMaps : Dictionary<uint64, OpenGL.LightMap.LightMap>
          mutable LightingConfig : LightingConfig
          mutable InstanceFields : single array
          mutable UserDefinedStaticModelFields : single array
          LightsDesiringShadows : Dictionary<uint64, SortableLight>
          ForwardSurfacesComparer : IComparer<struct (single * single * Matrix4x4 * Presence * Box2 * MaterialProperties * OpenGL.PhysicallyBased.PhysicallyBasedSurface * single)>
          ForwardSurfacesSortBuffer : struct (single * single * Matrix4x4 * Presence * Box2 * MaterialProperties * OpenGL.PhysicallyBased.PhysicallyBasedSurface * single) List
          RenderTasksDictionary : Dictionary<RenderPass, RenderTasks>
          RenderPackages : Packages<RenderAsset, AssetClient>
          mutable RenderPackageCachedOpt : RenderPackageCached
          mutable RenderAssetCached : RenderAssetCached
          mutable ReloadAssetsRequested : bool
          RenderMessages : RenderMessage3d List }

    static member private invalidateCaches renderer =
        renderer.RenderPackageCachedOpt <- Unchecked.defaultof<_>
        renderer.RenderAssetCached.CachedAssetTagOpt <- Unchecked.defaultof<_>
        renderer.RenderAssetCached.CachedRenderAsset <- RawAsset

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
        | ".raw" ->
            match GlRenderer3d.tryLoadRawAsset asset renderer with
            | Some () -> Some RawAsset
            | None -> None
        | ".bmp" | ".png" | ".jpg" | ".jpeg" | ".tga" | ".tif" | ".tiff" | ".dds" ->
            match GlRenderer3d.tryLoadTextureAsset assetClient asset renderer with
            | Some texture -> Some (TextureAsset texture)
            | None -> None
        | ".cbm" ->
            match GlRenderer3d.tryLoadCubeMapAsset assetClient asset renderer with
            | Some (cubeMapKey, cubeMap, opt) -> Some (CubeMapAsset (cubeMapKey, cubeMap, opt))
            | None -> None
        | ".fbx" | ".dae" | ".obj" ->
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

        // attempt to make new asset graph and load its assets
        match AssetGraph.tryMakeFromFile Assets.Global.AssetGraphFilePath with
        | Right assetGraph ->
            match AssetGraph.tryCollectAssetsFromPackage (Some Constants.Associations.Render3d) packageName assetGraph with
            | Right assetsCollected ->

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
                    let (lastWriteTime, filePath, renderAsset) = assetEntry.Value
                    let lastWriteTime' =
                        try DateTimeOffset (File.GetLastWriteTime filePath)
                        with exn -> Log.info ("Asset file write time read error due to: " + scstring exn); DateTimeOffset.MinValue.DateTime
                    if lastWriteTime < lastWriteTime'
                    then assetsToFree.Add (filePath, renderAsset)
                    else assetsToKeep.Add (assetName, (lastWriteTime, filePath, renderAsset))

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
                        assetsLoaded.[asset.AssetTag.AssetName] <- (lastWriteTime, asset.FilePath, renderAsset)
                    | None -> ()

                // update assets to keep
                let assetsUpdated =
                    [|for assetEntry in assetsToKeep do
                        let assetName = assetEntry.Key
                        let (lastWriteTime, filePath, renderAsset) = assetEntry.Value
                        let dirPath = PathF.GetDirectoryName filePath
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
                        KeyValuePair (assetName, (lastWriteTime, filePath, renderAsset))|]

                // insert assets into package
                for assetEntry in Seq.append assetsUpdated assetsLoaded do
                    let assetName = assetEntry.Key
                    let (lastWriteTime, filePath, renderAsset) = assetEntry.Value
                    renderPackage.Assets.[assetName] <- (lastWriteTime, filePath, renderAsset)

            // handle error cases
            | Left failedAssetNames ->
                Log.info ("Render package load failed due to unloadable assets '" + failedAssetNames + "' for package '" + packageName + "'.")
        | Left error ->
            Log.info ("Render package load failed due to unloadable asset graph due to: '" + error)

    static member private tryGetRenderAsset (assetTag : AssetTag) renderer =
        let mutable assetInfo = Unchecked.defaultof<DateTimeOffset * string * RenderAsset> // OPTIMIZATION: seems like TryGetValue allocates here if we use the tupling idiom (this may only be the case in Debug builds tho).
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
                | (true, (_, filePath, _)) -> Some filePath
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
                      OpaqueDistance = surfaceDescriptor.MaterialProperties.OpaqueDistance }

                // make material
                let material : OpenGL.PhysicallyBased.PhysicallyBasedMaterial =
                    { AlbedoTexture = albedoTexture
                      RoughnessTexture = match GlRenderer3d.tryGetRenderAsset surfaceDescriptor.RoughnessImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.RoughnessTexture
                      MetallicTexture = match GlRenderer3d.tryGetRenderAsset surfaceDescriptor.MetallicImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.MetallicTexture
                      AmbientOcclusionTexture = match GlRenderer3d.tryGetRenderAsset surfaceDescriptor.AmbientOcclusionImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.AmbientOcclusionTexture
                      EmissionTexture = match GlRenderer3d.tryGetRenderAsset surfaceDescriptor.EmissionImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.EmissionTexture
                      NormalTexture = match GlRenderer3d.tryGetRenderAsset surfaceDescriptor.NormalImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.NormalTexture
                      HeightTexture = match GlRenderer3d.tryGetRenderAsset surfaceDescriptor.HeightImage renderer with ValueSome (TextureAsset texture) -> texture | _ -> renderer.PhysicallyBasedMaterial.HeightTexture
                      TwoSided = surfaceDescriptor.TwoSided }

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
                package.Assets.[assetTag.AssetName] <- (DateTimeOffset.MinValue.DateTime, "", StaticModelAsset (true, model))
            | (false, _) ->
                let assetClient =
                    AssetClient
                        (OpenGL.Texture.TextureClient (Some renderer.LazyTextureQueues),
                         OpenGL.CubeMap.CubeMapClient (),
                         OpenGL.PhysicallyBased.PhysicallyBasedSceneClient ())
                let package = { Assets = Dictionary.singleton StringComparer.Ordinal assetTag.AssetName (DateTimeOffset.MinValue.DateTime, "", StaticModelAsset (true, model)); PackageState = assetClient }
                renderer.RenderPackages.[assetTag.PackageName] <- package

        // attempted to replace a loaded asset
        else Log.info ("Cannot replace a loaded asset '" + scstring assetTag + "' with a user-created static model.")

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
                    let normal = normalSum |> Vector3.Normalize
                    normal
                else v3Up|]

    static member private tryCreatePhysicallyBasedTerrainGeometry (geometryDescriptor : TerrainGeometryDescriptor) renderer =

        // attempt to compute positions and tex coords
        let heightMapMetadataOpt =
            HeightMap.tryGetMetadata
                (fun assetTag -> GlRenderer3d.tryGetFilePath assetTag renderer)
                geometryDescriptor.Bounds
                geometryDescriptor.Tiles
                geometryDescriptor.HeightMap

        // on success, continue terrain geometry generation attempt
        match heightMapMetadataOpt with
        | Some heightMapMetadata ->

            // compute normals
            let resolution = heightMapMetadata.Resolution
            let positionsAndTexCoordses = heightMapMetadata.PositionsAndTexCoordses
            let normalsOpt =
                match geometryDescriptor.NormalImageOpt with
                | Some normalImage ->
                    match GlRenderer3d.tryGetTextureData false normalImage renderer with
                    | Some (metadata, blockCompressed, bytes) when metadata.TextureWidth * metadata.TextureHeight = positionsAndTexCoordses.Length ->
                        if not blockCompressed then
                            let scalar = 1.0f / single Byte.MaxValue
                            bytes |>
                            Array.map (fun b -> single b * scalar) |>
                            Array.chunkBySize 4 |>
                            Array.map (fun b ->
                                let tangent = (v3 b.[2] b.[1] b.[0] * 2.0f - v3One).Normalized
                                let normal = v3 tangent.X tangent.Z -tangent.Y
                                normal) |>
                            Some
                        else Log.info "Block-compressed images not supported for terrain normal images."; None
                    | _ -> Some (GlRenderer3d.createPhysicallyBasedTerrainNormals resolution positionsAndTexCoordses)
                | None -> Some (GlRenderer3d.createPhysicallyBasedTerrainNormals resolution positionsAndTexCoordses)

            // compute tint
            let tintOpt =
                match geometryDescriptor.TintImageOpt with
                | Some tintImage ->
                    match GlRenderer3d.tryGetTextureData false tintImage renderer with
                    | Some (metadata, blockCompressed, bytes) when metadata.TextureWidth * metadata.TextureHeight = positionsAndTexCoordses.Length ->
                        if not blockCompressed then
                            let scalar = 1.0f / single Byte.MaxValue
                            bytes |>
                            Array.map (fun b -> single b * scalar) |>
                            Array.chunkBySize 4 |>
                            Array.map (fun b -> v3 b.[2] b.[1] b.[0]) |>
                            Some
                        else Log.info "Block-compressed images not supported for terrain tint images."; None
                    | _ -> Some (Array.init positionsAndTexCoordses.Length (fun _ -> v3One))
                | _ -> Some (Array.init positionsAndTexCoordses.Length (fun _ -> v3One))

            // compute blendses, logging if more than the safe number of terrain layers is utilized
            let blendses = Array2D.zeroCreate<single> positionsAndTexCoordses.Length Constants.Render.TerrainLayersMax
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
                    | Some (metadata, blockCompressed, bytes) when metadata.TextureWidth * metadata.TextureHeight = positionsAndTexCoordses.Length ->
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
                    | _ -> Log.info ("Could not locate texture data for blend image '" + scstring rgbaMap + "'.")
                | RedsMap reds ->
                    let scalar = 1.0f / single Byte.MaxValue
                    for i in 0 .. dec (min reds.Length Constants.Render.TerrainLayersMax) do
                        let red = reds.[i]
                        match GlRenderer3d.tryGetTextureData false red renderer with
                        | Some (metadata, blockCompressed, bytes) when metadata.TextureWidth * metadata.TextureHeight = positionsAndTexCoordses.Length ->
                            if not blockCompressed then
                                for j in 0 .. dec positionsAndTexCoordses.Length do
                                    blendses.[j, i] <- single bytes.[j * 4 + 2] * scalar
                            else Log.info "Block-compressed images not supported for terrain blend images."
                        | _ -> Log.info ("Could not locate texture data for blend image '" + scstring red + "'.")
            | FlatMaterial _ ->
                for i in 0 .. dec positionsAndTexCoordses.Length do
                    blendses.[i,0] <- 1.0f

            // ensure we've got usable input data
            match (normalsOpt, tintOpt) with
            | (Some normals, Some tint) ->

                // compute vertices
                let vertices =
                    [|for i in 0 .. dec positionsAndTexCoordses.Length do
                        let struct (p, tc) = positionsAndTexCoordses.[i]
                        let n = normals.[i]
                        let s = blendses
                        let t = tint.[i]
                        yield!
                            [|p.X; p.Y; p.Z
                              tc.X; tc.Y
                              n.X; n.Y; n.Z
                              t.X; t.Y; t.Z
                              s.[i,0]; s.[i,1]; s.[i,2]; s.[i,3]; s.[i,4]; s.[i,5]; s.[i,6]; s.[i,7]|]|]

                // compute indices, splitting quad along the standard orientation (as used by World Creator, AFAIK).
                let indices = 
                    [|for y in 0 .. dec resolution.Y - 1 do
                        for x in 0 .. dec resolution.X - 1 do
                            yield resolution.X * y + x
                            yield resolution.X * inc y + x
                            yield resolution.X * y + inc x
                            yield resolution.X * inc y + x
                            yield resolution.X * inc y + inc x
                            yield resolution.X * y + inc x|]

                // create the actual geometry
                let geometry = OpenGL.PhysicallyBased.CreatePhysicallyBasedTerrainGeometry (true, OpenGL.PrimitiveType.Triangles, vertices.AsMemory (), indices.AsMemory (), geometryDescriptor.Bounds)
                Some geometry

            // error
            | (_, _) -> None

        // error
        | None -> None

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
        let packageNames = renderer.RenderPackages |> Seq.map (fun entry -> entry.Key) |> Array.ofSeq
        for packageName in packageNames do
            GlRenderer3d.tryLoadRenderPackage packageName renderer

    static member private getShadowBufferResolution shadowBufferIndex =
        let scalar =
            if shadowBufferIndex < Constants.Render.ShadowDetailedCount
            then Constants.Render.ShadowDetailedResolutionScalar
            else 1
        Constants.Render.ShadowResolution * scalar

    static member private getRenderTasks renderPass renderer =
        let mutable renderTasks = Unchecked.defaultof<RenderTasks> // OPTIMIZATION: seems like TryGetValue allocates here if we use the tupling idiom (this may only be the case in Debug builds tho).
        if renderer.RenderTasksDictionary.TryGetValue (renderPass, &renderTasks)
        then renderTasks
        else
            let renderTasks = RenderTasks.make ()
            renderer.RenderTasksDictionary.Add (renderPass, renderTasks)
            renderTasks

    static member private categorizeBillboardSurface
        (absolute,
         eyeRotation : Quaternion,
         model : Matrix4x4,
         presence : Presence,
         insetOpt : Box2 option,
         albedoMetadata : OpenGL.Texture.TextureMetadata,
         orientUp,
         properties,
         billboardSurface,
         renderType,
         renderPass,
         renderer) =
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
            | None -> box2 v2Zero v2One // shouldn't we still be using borders?
        let billboardRotation =
            if orientUp then
                let eyeForward = (Vector3.Transform (v3Forward, eyeRotation)).WithY 0.0f
                let billboardAngle = if Vector3.Dot (eyeForward, v3Right) >= 0.0f then -eyeForward.AngleBetween v3Forward else eyeForward.AngleBetween v3Forward
                Matrix4x4.CreateFromQuaternion (Quaternion.CreateFromAxisAngle (v3Up, billboardAngle))
            else Matrix4x4.CreateFromQuaternion -eyeRotation
        let mutable affineRotation = model
        affineRotation.Translation <- v3Zero
        let mutable billboardMatrix = model * billboardRotation
        billboardMatrix.Translation <- model.Translation
        let renderTasks = GlRenderer3d.getRenderTasks renderPass renderer
        match renderType with
        | DeferredRenderType ->
            if absolute then
                let mutable renderOps = Unchecked.defaultof<_> // OPTIMIZATION: TryGetValue using the auto-pairing syntax of F# allocation when the 'TValue is a struct tuple.
                if renderTasks.DeferredStaticAbsolute.TryGetValue (billboardSurface, &renderOps)
                then renderOps.Add struct (billboardMatrix, presence, texCoordsOffset, properties)
                else renderTasks.DeferredStaticAbsolute.Add (billboardSurface, List ([struct (billboardMatrix, presence, texCoordsOffset, properties)]))
            else
                let mutable renderOps = Unchecked.defaultof<_> // OPTIMIZATION: TryGetValue using the auto-pairing syntax of F# allocation when the 'TValue is a struct tuple.
                if renderTasks.DeferredStaticRelative.TryGetValue (billboardSurface, &renderOps)
                then renderOps.Add struct (billboardMatrix, presence, texCoordsOffset, properties)
                else renderTasks.DeferredStaticRelative.Add (billboardSurface, List ([struct (billboardMatrix, presence, texCoordsOffset, properties)]))
        | ForwardRenderType (subsort, sort) ->
            if absolute
            then renderTasks.ForwardStaticAbsolute.Add struct (subsort, sort, billboardMatrix, presence, texCoordsOffset, properties, billboardSurface)
            else renderTasks.ForwardStaticRelative.Add struct (subsort, sort, billboardMatrix, presence, texCoordsOffset, properties, billboardSurface)

    static member private categorizeStaticModelSurface
        (absolute,
         model : Matrix4x4 inref,
         presence : Presence,
         insetOpt : Box2 voption inref,
         properties : MaterialProperties inref,
         surface : OpenGL.PhysicallyBased.PhysicallyBasedSurface,
         renderType : RenderType,
         renderPass : RenderPass,
         renderTasksOpt : RenderTasks option,
         renderer) =
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
        let renderTasks =
            match renderTasksOpt with
            | Some renderTasks -> renderTasks
            | None -> GlRenderer3d.getRenderTasks renderPass renderer
        match renderType with
        | DeferredRenderType ->
            if absolute then
                let mutable renderOps = Unchecked.defaultof<_> // OPTIMIZATION: TryGetValue using the auto-pairing syntax of F# allocation when the 'TValue is a struct tuple.
                if renderTasks.DeferredStaticAbsolute.TryGetValue (surface, &renderOps)
                then renderOps.Add struct (model, presence, texCoordsOffset, properties)
                else renderTasks.DeferredStaticAbsolute.Add (surface, List ([struct (model, presence, texCoordsOffset, properties)]))
            else
                let mutable renderOps = Unchecked.defaultof<_> // OPTIMIZATION: TryGetValue using the auto-pairing syntax of F# allocation when the 'TValue is a struct tuple.
                if renderTasks.DeferredStaticRelative.TryGetValue (surface, &renderOps)
                then renderOps.Add struct (model, presence, texCoordsOffset, properties)
                else renderTasks.DeferredStaticRelative.Add (surface, List ([struct (model, presence, texCoordsOffset, properties)]))
        | ForwardRenderType (subsort, sort) ->
            if absolute
            then renderTasks.ForwardStaticAbsolute.Add struct (subsort, sort, model, presence, texCoordsOffset, properties, surface)
            else renderTasks.ForwardStaticRelative.Add struct (subsort, sort, model, presence, texCoordsOffset, properties, surface)

    static member private categorizeStaticModelSurfaceByIndex
        (absolute,
         model : Matrix4x4 inref,
         presence : Presence,
         insetOpt : Box2 voption inref,
         properties : MaterialProperties inref,
         material : Material inref,
         staticModel : StaticModel AssetTag,
         surfaceIndex : int,
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
                    GlRenderer3d.categorizeStaticModelSurface (absolute, &model, presence, &insetOpt, &properties, surface, renderType, renderPass, None, renderer)
            | _ -> Log.infoOnce ("Cannot render static model surface with a non-static model asset for '" + scstring staticModel + "'.")
        | ValueNone -> Log.infoOnce ("Cannot render static model surface due to unloadable asset(s) for '" + scstring staticModel + "'.")

    static member private categorizeStaticModel
        (frustumInterior : Frustum,
         frustumExterior : Frustum,
         frustumImposter : Frustum,
         lightBox : Box3,
         absolute : bool,
         model : Matrix4x4 inref,
         presence : Presence,
         insetOpt : Box2 voption inref,
         properties : MaterialProperties inref,
         staticModel : StaticModel AssetTag,
         renderType : RenderType,
         renderPass : RenderPass,
         renderer) =
        let renderStyle = match renderType with DeferredRenderType -> Deferred | ForwardRenderType (subsort, sort) -> Forward (subsort, sort)
        match GlRenderer3d.tryGetRenderAsset staticModel renderer with
        | ValueSome renderAsset ->
            match renderAsset with
            | StaticModelAsset (_, modelAsset) ->
                let renderTasks = GlRenderer3d.getRenderTasks renderPass renderer
                for light in modelAsset.Lights do
                    let lightMatrix = light.LightMatrix * model
                    let lightBounds = Box3 (lightMatrix.Translation - v3Dup light.LightCutoff, v3Dup light.LightCutoff * 2.0f)
                    let lightDirection = lightMatrix.Rotation.Down
                    let unculled =
                        match renderPass with
                        | NormalPass -> Presence.intersects3d (Some frustumInterior) frustumExterior frustumImposter (Some lightBox) false true presence lightBounds
                        | LightMapPass (_, _) -> true // TODO: P1: see if we have enough context to cull here.
                        | _ -> false
                    if unculled then
                        let coneOuter = match light.LightType with SpotLight (_, coneOuter) -> min coneOuter MathF.PI_MINUS_EPSILON | _ -> MathF.TWO_PI
                        let coneInner = match light.LightType with SpotLight (coneInner, _) -> min coneInner coneOuter | _ -> MathF.TWO_PI
                        let light =
                            { SortableLightId = 0UL
                              SortableLightOrigin = lightMatrix.Translation
                              SortableLightRotation = lightMatrix.Rotation
                              SortableLightDirection = lightDirection
                              SortableLightColor = light.LightColor
                              SortableLightBrightness = light.LightBrightness
                              SortableLightAttenuationLinear = light.LightAttenuationLinear
                              SortableLightAttenuationQuadratic = light.LightAttenuationQuadratic
                              SortableLightCutoff = light.LightCutoff
                              SortableLightDirectional = match light.LightType with DirectionalLight -> 1 | _ -> 0
                              SortableLightConeInner = coneInner
                              SortableLightConeOuter = coneOuter
                              SortableLightDesireShadows = 0
                              SortableLightDistanceSquared = Single.MaxValue }
                        renderTasks.Lights.Add light
                for surface in modelAsset.Surfaces do
                    let surfaceMatrix = if surface.SurfaceMatrixIsIdentity then model else surface.SurfaceMatrix * model
                    let surfaceBounds = surface.SurfaceBounds.Transform surfaceMatrix
                    let presence = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractPresence presence modelAsset.SceneOpt surface
                    let renderStyle = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractRenderStyle renderStyle modelAsset.SceneOpt surface
                    let renderType = match renderStyle with Deferred -> DeferredRenderType | Forward (subsort, sort) -> ForwardRenderType (subsort, sort)
                    let ignoreLightMaps = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractIgnoreLightMaps properties.IgnoreLightMaps modelAsset.SceneOpt surface
                    let properties = if ignoreLightMaps <> properties.IgnoreLightMaps then { properties with IgnoreLightMapsOpt = Some ignoreLightMaps } else properties
                    let unculled =
                        match renderPass with
                        | NormalPass -> Presence.intersects3d (Some frustumInterior) frustumExterior frustumImposter (Some lightBox) false false presence surfaceBounds
                        | LightMapPass (_, _) -> true // TODO: P1: see if we have enough context to cull here.
                        | ShadowPass (_, shadowDirectional, shadowFrustum) -> Presence.intersects3d (if shadowDirectional then None else Some shadowFrustum) shadowFrustum shadowFrustum None false false presence surfaceBounds
                        | ReflectionPass (_, reflFrustum) -> Presence.intersects3d None reflFrustum reflFrustum None false false presence surfaceBounds
                    if unculled then
                        GlRenderer3d.categorizeStaticModelSurface (absolute, &surfaceMatrix, presence, &insetOpt, &properties, surface, renderType, renderPass, Some renderTasks, renderer)
            | _ -> Log.infoOnce ("Cannot render static model with a non-static model asset for '" + scstring staticModel + "'.")
        | ValueNone -> Log.infoOnce ("Cannot render static model due to unloadable asset(s) for '" + scstring staticModel + "'.")

    static member private categorizeAnimatedModel
        (absolute : bool,
         model : Matrix4x4 inref,
         presence : Presence,
         insetOpt : Box2 voption inref,
         properties : MaterialProperties inref,
         boneTransforms : Matrix4x4 array,
         animatedModel : AnimatedModel AssetTag,
         renderPass : RenderPass,
         renderer) =

        // ensure we have the required animated model
        match GlRenderer3d.tryGetRenderAsset animatedModel renderer with
        | ValueSome renderAsset ->
            match renderAsset with
            | AnimatedModelAsset modelAsset ->

                // render animated surfaces
                let renderTasks = GlRenderer3d.getRenderTasks renderPass renderer
                for surface in modelAsset.Surfaces do

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

                    // render animated surface
                    if absolute then
                        let animatedModelSurfaceKey = { BoneTransforms = boneTransforms; AnimatedModelSurface = surface }
                        match renderTasks.DeferredAnimatedAbsolute.TryGetValue animatedModelSurfaceKey with
                        | (true, renderOps) -> renderOps.Add struct (model, presence, texCoordsOffset, properties)
                        | (false, _) -> renderTasks.DeferredAnimatedAbsolute.Add (animatedModelSurfaceKey, List ([struct (model, presence, texCoordsOffset, properties)]))
                    else
                        let animatedModelSurfaceKey = { BoneTransforms = boneTransforms; AnimatedModelSurface = surface }
                        match renderTasks.DeferredAnimatedRelative.TryGetValue animatedModelSurfaceKey with
                        | (true, renderOps) -> renderOps.Add struct (model, presence, texCoordsOffset, properties)
                        | (false, _) -> renderTasks.DeferredAnimatedRelative.Add (animatedModelSurfaceKey, List ([struct (model, presence, texCoordsOffset, properties)]))

            // unable to render
            | _ -> Log.infoOnce ("Cannot render animated model with a non-animated model asset '" + scstring animatedModel + "'.")
        | ValueNone -> Log.infoOnce ("Cannot render animated model due to unloadable asset(s) for '" + scstring animatedModel + "'.")

    static member private categorizeAnimatedModels
        (absolute : bool,
         animatedModels : (Matrix4x4 * Presence * Box2 option * MaterialProperties) SList,
         boneTransforms : Matrix4x4 array,
         animatedModel : AnimatedModel AssetTag,
         renderPass : RenderPass,
         renderer) =

        // ensure we have the required animated model
        match GlRenderer3d.tryGetRenderAsset animatedModel renderer with
        | ValueSome renderAsset ->
            match renderAsset with
            | AnimatedModelAsset modelAsset ->

                // render animated surfaces
                let renderTasks = GlRenderer3d.getRenderTasks renderPass renderer
                for surface in modelAsset.Surfaces do

                    // render animated surfaces
                    for (model, presence, insetOpt, properties) in animatedModels do

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

                        // render animated surface
                        if absolute then
                            let animatedModelSurfaceKey = { BoneTransforms = boneTransforms; AnimatedModelSurface = surface }
                            match renderTasks.DeferredAnimatedAbsolute.TryGetValue animatedModelSurfaceKey with
                            | (true, renderOps) -> renderOps.Add struct (model, presence, texCoordsOffset, properties)
                            | (false, _) -> renderTasks.DeferredAnimatedAbsolute.Add (animatedModelSurfaceKey, List ([struct (model, presence, texCoordsOffset, properties)]))
                        else
                            let animatedModelSurfaceKey = { BoneTransforms = boneTransforms; AnimatedModelSurface = surface }
                            match renderTasks.DeferredAnimatedRelative.TryGetValue animatedModelSurfaceKey with
                            | (true, renderOps) -> renderOps.Add struct (model, presence, texCoordsOffset, properties)
                            | (false, _) -> renderTasks.DeferredAnimatedRelative.Add (animatedModelSurfaceKey, List ([struct (model, presence, texCoordsOffset, properties)]))

            // unable to render
            | _ -> Log.infoOnce ("Cannot render animated model with a non-animated model asset '" + scstring animatedModel + "'.")
        | ValueNone -> Log.infoOnce ("Cannot render animated model due to unloadable asset(s) for '" + scstring animatedModel + "'.")

    static member private categorizeTerrain
        (absolute : bool,
         visible : bool,
         terrainDescriptor : TerrainDescriptor,
         renderPass : RenderPass,
         renderer) =

        // attempt to create terrain geometry
        let geometryDescriptor = terrainDescriptor.TerrainGeometryDescriptor
        match renderer.PhysicallyBasedTerrainGeometries.TryGetValue geometryDescriptor with
        | (true, _) -> ()
        | (false, _) ->
            match GlRenderer3d.tryCreatePhysicallyBasedTerrainGeometry geometryDescriptor renderer with
            | Some geometry -> renderer.PhysicallyBasedTerrainGeometries.Add (geometryDescriptor, geometry)
            | None -> ()

        // attempt to add terrain to appropriate render list when visible
        // TODO: also add found geometry to render list so it doesn't have to be looked up redundantly?
        if visible then
            let renderTasks = GlRenderer3d.getRenderTasks renderPass renderer
            match renderer.PhysicallyBasedTerrainGeometries.TryGetValue geometryDescriptor with
            | (true, terrainGeometry) ->
                if absolute
                then renderTasks.DeferredTerrainsAbsolute.Add struct (terrainDescriptor, terrainGeometry)
                else renderTasks.DeferredTerrainsRelative.Add struct (terrainDescriptor, terrainGeometry)
            | (false, _) -> ()

        // mark terrain geometry as utilized regardless of visibility (to keep it from being destroyed).
        renderer.PhysicallyBasedTerrainGeometriesUtilized.Add geometryDescriptor |> ignore<bool>

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

    static member private sortForwardSurfaces
        eyeCenter
        (surfaces : struct (single * single * Matrix4x4 * Presence * Box2 * MaterialProperties * OpenGL.PhysicallyBased.PhysicallyBasedSurface) List)
        (forwardSurfacesComparer : IComparer<struct (single * single * Matrix4x4 * Presence * Box2 * MaterialProperties * OpenGL.PhysicallyBased.PhysicallyBasedSurface * single)>)
        (forwardSurfacesSortBuffer : struct (single * single * Matrix4x4 * Presence * Box2 * MaterialProperties * OpenGL.PhysicallyBased.PhysicallyBasedSurface * single) List) =
        for struct (subsort, sort, model, presence, texCoordsOffset, properties, surface) in surfaces do
            forwardSurfacesSortBuffer.Add struct (subsort, sort, model, presence, texCoordsOffset, properties, surface, (model.Translation - eyeCenter).MagnitudeSquared)
        forwardSurfacesSortBuffer.Sort forwardSurfacesComparer
        forwardSurfacesSortBuffer

    static member private renderPhysicallyBasedShadowSurfaces
        batchPhase viewArray projectionArray bonesArray (parameters : struct (Matrix4x4 * Presence * Box2 * MaterialProperties) List)
        (surface : OpenGL.PhysicallyBased.PhysicallyBasedSurface) shader renderer =

        // ensure we have a large enough instance fields array
        let mutable length = renderer.InstanceFields.Length
        while parameters.Count * Constants.Render.InstanceFieldCount > length do length <- length * 2
        if renderer.InstanceFields.Length < length then
            renderer.InstanceFields <- Array.zeroCreate<single> length

        // blit parameters to instance fields
        for i in 0 .. dec parameters.Count do
            let struct (model, _, _, _) = parameters.[i]
            model.ToArray (renderer.InstanceFields, i * Constants.Render.InstanceFieldCount)

        // draw surfaces
        OpenGL.PhysicallyBased.DrawPhysicallyBasedShadowSurfaces
            (batchPhase, viewArray, projectionArray, bonesArray, parameters.Count,
             renderer.InstanceFields, surface.PhysicallyBasedGeometry, shader)

    static member private renderPhysicallyBasedDeferredSurfaces
        batchPhase viewArray projectionArray bonesArray eyeCenter (parameters : struct (Matrix4x4 * Presence * Box2 * MaterialProperties) List)
        (surface : OpenGL.PhysicallyBased.PhysicallyBasedSurface) shader renderer =

        // ensure we have a large enough instance fields array
        let mutable length = renderer.InstanceFields.Length
        while parameters.Count * Constants.Render.InstanceFieldCount > length do length <- length * 2
        if renderer.InstanceFields.Length < length then
            renderer.InstanceFields <- Array.zeroCreate<single> length

        // blit parameters to instance fields
        for i in 0 .. dec parameters.Count do
            let struct (model, presence, texCoordsOffset, properties) = parameters.[i]
            model.ToArray (renderer.InstanceFields, i * Constants.Render.InstanceFieldCount)
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 16] <- texCoordsOffset.Min.X
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 16 + 1] <- texCoordsOffset.Min.Y
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 16 + 2] <- texCoordsOffset.Min.X + texCoordsOffset.Size.X
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 16 + 3] <- texCoordsOffset.Min.Y + texCoordsOffset.Size.Y
            let albedo = match properties.AlbedoOpt with Some value -> value | None -> surface.SurfaceMaterialProperties.Albedo
            let roughness = match properties.RoughnessOpt with Some value -> value | None -> surface.SurfaceMaterialProperties.Roughness
            let metallic = match properties.MetallicOpt with Some value -> value | None -> surface.SurfaceMaterialProperties.Metallic
            let ambientOcclusion = match properties.AmbientOcclusionOpt with Some value -> value | None -> surface.SurfaceMaterialProperties.AmbientOcclusion
            let emission = match properties.EmissionOpt with Some value -> value | None -> surface.SurfaceMaterialProperties.Emission
            let height = match properties.HeightOpt with Some value -> value | None -> surface.SurfaceMaterialProperties.Height
            let ignoreLightMaps = match properties.IgnoreLightMapsOpt with Some value -> value | None -> surface.SurfaceMaterialProperties.IgnoreLightMaps
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
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 31] <- surface.SurfaceMaterialProperties.OpaqueDistance

        // draw deferred surfaces
        OpenGL.PhysicallyBased.DrawPhysicallyBasedDeferredSurfaces
            (batchPhase, viewArray, projectionArray, bonesArray, eyeCenter,
             parameters.Count, renderer.InstanceFields, surface.SurfaceMaterial, surface.PhysicallyBasedGeometry, shader)

    static member private renderPhysicallyBasedForwardSurfaces
        blending viewArray projectionArray bonesArray (parameters : struct (Matrix4x4 * Presence * Box2 * MaterialProperties) SList)
        eyeCenter lightCutoffMargin lightAmbientColor lightAmbientBrightness lightShadowBiasAcne lightShadowBiasBleed
        brdfTexture irradianceMap environmentFilterMap irradianceMaps environmentFilterMaps shadowTextures lightMapOrigins lightMapMins lightMapSizes lightMapsCount
        lightOrigins lightDirections lightColors lightBrightnesses lightAttenuationLinears lightAttenuationQuadratics lightCutoffs lightDirectionals lightConeInners lightConeOuters lightShadowIndices lightsCount shadowMatrices
        (surface : OpenGL.PhysicallyBased.PhysicallyBasedSurface) shader renderer =

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
            let albedo = match properties.AlbedoOpt with Some value -> value | None -> surface.SurfaceMaterialProperties.Albedo
            let roughness = match properties.RoughnessOpt with Some value -> value | None -> surface.SurfaceMaterialProperties.Roughness
            let metallic = match properties.MetallicOpt with Some value -> value | None -> surface.SurfaceMaterialProperties.Metallic
            let ambientOcclusion = match properties.AmbientOcclusionOpt with Some value -> value | None -> surface.SurfaceMaterialProperties.AmbientOcclusion
            let emission = match properties.EmissionOpt with Some value -> value | None -> surface.SurfaceMaterialProperties.Emission
            let height = match properties.HeightOpt with Some value -> value | None -> surface.SurfaceMaterialProperties.Height
            let ignoreLightMaps = match properties.IgnoreLightMapsOpt with Some value -> value | None -> surface.SurfaceMaterialProperties.IgnoreLightMaps
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
            renderer.InstanceFields.[i * Constants.Render.InstanceFieldCount + 31] <- surface.SurfaceMaterialProperties.OpaqueDistance

        // draw forward surfaces
        OpenGL.PhysicallyBased.DrawPhysicallyBasedForwardSurfaces
            (blending, viewArray, projectionArray, bonesArray, parameters.Length, renderer.InstanceFields,
             eyeCenter, lightCutoffMargin, lightAmbientColor, lightAmbientBrightness, lightShadowBiasAcne, lightShadowBiasBleed,
             brdfTexture, irradianceMap, environmentFilterMap, irradianceMaps, environmentFilterMaps, shadowTextures, lightMapOrigins, lightMapMins, lightMapSizes, lightMapsCount,
             lightOrigins, lightDirections, lightColors, lightBrightnesses, lightAttenuationLinears, lightAttenuationQuadratics, lightCutoffs, lightDirectionals, lightConeInners, lightConeOuters, lightShadowIndices, lightsCount, shadowMatrices,
             surface.SurfaceMaterial, surface.PhysicallyBasedGeometry, shader)

    static member private renderPhysicallyBasedTerrain viewArray geometryProjectionArray eyeCenter terrainDescriptor geometry shader renderer =
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
              OpaqueDistance = Constants.Render.OpaqueDistanceDefault }
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
        let instanceFields =
            Array.append
                (m4Identity.ToArray ())
                ([|texCoordsOffset.Min.X; texCoordsOffset.Min.Y; texCoordsOffset.Min.X + texCoordsOffset.Size.X; texCoordsOffset.Min.Y + texCoordsOffset.Size.Y
                   materialProperties.Albedo.R; materialProperties.Albedo.G; materialProperties.Albedo.B; materialProperties.Albedo.A
                   materialProperties.Roughness; materialProperties.Metallic; materialProperties.AmbientOcclusion; materialProperties.Emission
                   texelHeight * materialProperties.Height|])
        OpenGL.PhysicallyBased.DrawPhysicallyBasedTerrain
            (viewArray, geometryProjectionArray, eyeCenter,
             instanceFields, elementsCount, materials, geometry, shader)
        OpenGL.Hl.Assert ()

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
        let properties : OpenGL.PhysicallyBased.PhysicallyBasedMaterialProperties =
            { Albedo = properties.Albedo
              Roughness = properties.Roughness
              Metallic = properties.Metallic
              AmbientOcclusion = properties.AmbientOcclusion
              Emission = properties.Emission
              Height = properties.Height
              IgnoreLightMaps = properties.IgnoreLightMaps
              OpaqueDistance = properties.OpaqueDistance }
        let material : OpenGL.PhysicallyBased.PhysicallyBasedMaterial =
            { AlbedoTexture = albedoTexture
              RoughnessTexture = roughnessTexture
              MetallicTexture = metallicTexture
              AmbientOcclusionTexture = ambientOcclusionTexture
              EmissionTexture = emissionTexture
              NormalTexture = normalTexture
              HeightTexture = heightTexture
              TwoSided = true }
        struct (properties, material)

    static member private applySurfaceMaterial (material : Material inref, surfaceMaterial : OpenGL.PhysicallyBased.PhysicallyBasedMaterial inref, renderer) =
        let albedoTexture =
            match material.AlbedoImageOpt with
            | Some image ->
                match GlRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.AlbedoTexture
            | None -> surfaceMaterial.AlbedoTexture
        let roughnessTexture =
            match material.RoughnessImageOpt with
            | Some image ->
                match GlRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.RoughnessTexture
            | None -> surfaceMaterial.RoughnessTexture
        let metallicTexture =
            match material.MetallicImageOpt with
            | Some image ->
                match GlRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.MetallicTexture
            | None -> surfaceMaterial.MetallicTexture
        let ambientOcclusionTexture =
            match material.AmbientOcclusionImageOpt with
            | Some image ->
                match GlRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.AmbientOcclusionTexture
            | None -> surfaceMaterial.AmbientOcclusionTexture
        let emissionTexture =
            match material.EmissionImageOpt with
            | Some image ->
                match GlRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.EmissionTexture
            | None -> surfaceMaterial.EmissionTexture
        let normalTexture =
            match material.NormalImageOpt with
            | Some image ->
                match GlRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.NormalTexture
            | None -> surfaceMaterial.NormalTexture
        let heightTexture =
            match material.HeightImageOpt with
            | Some image ->
                match GlRenderer3d.tryGetRenderAsset image renderer with
                | ValueSome (TextureAsset texture) -> texture
                | _ -> surfaceMaterial.HeightTexture
            | None -> surfaceMaterial.HeightTexture
        let twoSided =
            match material.TwoSidedOpt with
            | Some twoSided -> twoSided
            | None -> surfaceMaterial.TwoSided
        let surfaceMaterial : OpenGL.PhysicallyBased.PhysicallyBasedMaterial =
            { AlbedoTexture = albedoTexture
              RoughnessTexture = roughnessTexture
              MetallicTexture = metallicTexture
              AmbientOcclusionTexture = ambientOcclusionTexture
              EmissionTexture = emissionTexture
              NormalTexture = normalTexture
              HeightTexture = heightTexture
              TwoSided = twoSided }
        surfaceMaterial

    static member private renderShadowTexture
        renderTasks
        renderer
        (topLevelRender : bool)
        (lightOrigin : Vector3)
        (lightViewAbsolute : Matrix4x4)
        (lightViewRelative : Matrix4x4)
        (lightProjection : Matrix4x4)
        (shadowResolution : Vector2i)
        (renderbuffer : uint)
        (framebuffer : uint) =

        // compute matrix arrays
        let lightViewAbsoluteArray = lightViewAbsolute.ToArray ()
        let lightViewRelativeArray = lightViewRelative.ToArray ()
        let lightProjectionArray = lightProjection.ToArray ()

        // send absolute forward surfaces directly to sorted buffer since no sorting is needed for shadows
        for struct (_, _, model, presence, texCoordsOffset, properties, surface) in renderTasks.ForwardStaticAbsolute do
            renderTasks.ForwardStaticAbsoluteSorted.Add struct (model, presence, texCoordsOffset, properties, surface)
        renderTasks.ForwardStaticAbsolute.Clear ()

        // send relative forward surfaces directly to sorted buffer since no sorting is needed for shadows
        for struct (_, _, model, presence, texCoordsOffset, properties, surface) in renderTasks.ForwardStaticRelative do
            renderTasks.ForwardStaticRelativeSorted.Add struct (model, presence, texCoordsOffset, properties, surface)
        renderTasks.ForwardStaticRelative.Clear ()

        // setup shadow buffer and viewport
        OpenGL.Gl.Viewport (0, 0, shadowResolution.X, shadowResolution.Y)
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, renderbuffer)
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, framebuffer)
        OpenGL.Gl.Clear OpenGL.ClearBufferMask.DepthBufferBit
        OpenGL.Hl.Assert ()

        // deferred render static surface shadows w/ absolute transforms if in top level render
        if topLevelRender then
            let mutable enr = renderTasks.DeferredStaticAbsolute.GetEnumerator ()
            let mutable i = 0
            while enr.MoveNext () do
                let entry = enr.Current
                let batchPhase =
                    match renderTasks.DeferredStaticAbsolute.Count with
                    | 1 -> SingletonPhase
                    | count -> if i = 0 then StartingPhase elif i = dec count then StoppingPhase else ResumingPhase
                GlRenderer3d.renderPhysicallyBasedShadowSurfaces
                    batchPhase lightViewAbsoluteArray lightProjectionArray [||] entry.Value
                    entry.Key renderer.PhysicallyBasedShadowStaticShader renderer
                OpenGL.Hl.Assert ()
                i <- inc i

        // deferred render static surface shadows w/ relative transforms
        let mutable enr = renderTasks.DeferredStaticRelative.GetEnumerator ()
        let mutable i = 0
        while enr.MoveNext () do
            let entry = enr.Current
            let batchPhase =
                match renderTasks.DeferredStaticRelative.Count with
                | 1 -> SingletonPhase
                | count -> if i = 0 then StartingPhase elif i = dec count then StoppingPhase else ResumingPhase
            GlRenderer3d.renderPhysicallyBasedShadowSurfaces
                batchPhase lightViewRelativeArray lightProjectionArray [||] entry.Value
                entry.Key renderer.PhysicallyBasedShadowStaticShader renderer
            OpenGL.Hl.Assert ()
            i <- inc i

        // deferred render animated surface shadows w/ absolute transforms if in top level render
        if topLevelRender then
            for entry in renderTasks.DeferredAnimatedAbsolute do
                let surfaceKey = entry.Key
                let parameters = entry.Value
                let bonesArray = Array.map (fun (boneTransform : Matrix4x4) -> boneTransform.ToArray ()) surfaceKey.BoneTransforms
                GlRenderer3d.renderPhysicallyBasedShadowSurfaces
                    SingletonPhase lightViewAbsoluteArray lightProjectionArray bonesArray parameters
                    surfaceKey.AnimatedModelSurface renderer.PhysicallyBasedShadowAnimatedShader renderer
                OpenGL.Hl.Assert ()

        // deferred render animated surface shadows w/ relative transforms
        for entry in renderTasks.DeferredAnimatedRelative do
            let surfaceKey = entry.Key
            let parameters = entry.Value
            let bonesArray = Array.map (fun (boneTransform : Matrix4x4) -> boneTransform.ToArray ()) surfaceKey.BoneTransforms
            GlRenderer3d.renderPhysicallyBasedShadowSurfaces
                SingletonPhase lightViewRelativeArray lightProjectionArray bonesArray parameters
                surfaceKey.AnimatedModelSurface renderer.PhysicallyBasedShadowAnimatedShader renderer
            OpenGL.Hl.Assert ()

        // attempt to deferred render terrain shadows w/ absolute transforms if in top level render
        if topLevelRender then
            for (descriptor, geometry) in renderTasks.DeferredTerrainsAbsolute do
                GlRenderer3d.renderPhysicallyBasedTerrain lightViewAbsoluteArray lightProjectionArray lightOrigin descriptor geometry renderer.PhysicallyBasedShadowTerrainShader renderer

        // attempt to deferred render terrains w/ relative transforms
        for (descriptor, geometry) in renderTasks.DeferredTerrainsRelative do
            GlRenderer3d.renderPhysicallyBasedTerrain lightViewRelativeArray lightProjectionArray lightOrigin descriptor geometry renderer.PhysicallyBasedShadowTerrainShader renderer

        // forward render static surface shadows w/ absolute transforms to filter buffer if in top level render
        if topLevelRender then
            for struct (model, presence, texCoordsOffset, properties, surface) in renderTasks.ForwardStaticAbsoluteSorted do
                GlRenderer3d.renderPhysicallyBasedShadowSurfaces
                    SingletonPhase lightViewAbsoluteArray lightProjectionArray [||] (List ([struct (model, presence, texCoordsOffset, properties)]))
                    surface renderer.PhysicallyBasedShadowStaticShader renderer
                OpenGL.Hl.Assert ()

        // forward render static surface shadows w/ relative transforms to filter buffer
        for struct (model, presence, texCoordsOffset, properties, surface) in renderTasks.ForwardStaticRelativeSorted do
            GlRenderer3d.renderPhysicallyBasedShadowSurfaces
                SingletonPhase lightViewRelativeArray lightProjectionArray [||] (List ([struct (model, presence, texCoordsOffset, properties)]))
                surface renderer.PhysicallyBasedShadowStaticShader renderer
            OpenGL.Hl.Assert ()

        // unbind shadow mapping frame buffer
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, 0u)

    static member private renderGeometry
        renderPass
        renderTasks
        renderer
        (topLevelRender : bool)
        (eyeCenter : Vector3)
        (eyeRotation : Quaternion)
        (viewAbsolute : Matrix4x4)
        (viewRelative : Matrix4x4)
        (viewSkyBox : Matrix4x4)
        (geometryViewport : Viewport)
        (geometryProjection : Matrix4x4)
        (ssaoViewport : Viewport)
        (rasterViewport : Viewport)
        (rasterProjection : Matrix4x4)
        (renderbuffer : uint)
        (framebuffer : uint) =

        // compute geometry frustum
        let geometryFrustum = geometryViewport.Frustum (eyeCenter, eyeRotation)

        // compute matrix arrays
        let viewAbsoluteArray = viewAbsolute.ToArray ()
        let viewRelativeArray = viewRelative.ToArray ()
        let viewSkyBoxArray = viewSkyBox.ToArray ()
        let geometryProjectionArray = geometryProjection.ToArray ()
        let rasterProjectionArray = rasterProjection.ToArray ()

        // get sky box opt and fallback light map
        let (lightAmbientColor, lightAmbientBrightness, skyBoxOpt) = GlRenderer3d.getLastSkyBoxOpt renderPass renderer
        let lightAmbientColor = [|lightAmbientColor.R; lightAmbientColor.G; lightAmbientColor.B|]
        let lightMapFallback =
            match skyBoxOpt with
            | Some (_, _, _, irradianceAndEnvironmentMapsOptRef : (OpenGL.Texture.Texture * OpenGL.Texture.Texture) option ref) ->
                let (irradianceMap, environmentFilterMap) =
                    match irradianceAndEnvironmentMapsOptRef.Value with
                    | Some irradianceAndEnvironmentMaps -> irradianceAndEnvironmentMaps
                    | None -> (renderer.IrradianceMap, renderer.EnvironmentFilterMap)
                OpenGL.LightMap.CreateLightMap true v3Zero box3Zero irradianceMap environmentFilterMap
            | None -> OpenGL.LightMap.CreateLightMap true v3Zero box3Zero renderer.IrradianceMap renderer.EnvironmentFilterMap

        // destroy cached light maps whose originating probe no longer exists
        if topLevelRender then
            for lightMapKvp in renderer.LightMaps do
                if not (renderTasks.LightProbes.ContainsKey lightMapKvp.Key) then
                    OpenGL.LightMap.DestroyLightMap lightMapKvp.Value
                    renderer.LightMaps.Remove lightMapKvp.Key |> ignore<bool>

        // collect light maps from cached light maps
        for lightMapKvp in renderer.LightMaps do
            let lightMap =
                { SortableLightMapEnabled = lightMapKvp.Value.Enabled
                  SortableLightMapOrigin = lightMapKvp.Value.Origin
                  SortableLightMapBounds = lightMapKvp.Value.Bounds
                  SortableLightMapIrradianceMap = lightMapKvp.Value.IrradianceMap
                  SortableLightMapEnvironmentFilterMap = lightMapKvp.Value.EnvironmentFilterMap
                  SortableLightMapDistanceSquared = Single.MaxValue }
            renderTasks.LightMaps.Add lightMap

        // filter light map according to enabledness and intersection with the geometry frustum
        let lightMaps =
            renderTasks.LightMaps |>
            Array.ofSeq |>
            Array.filter (fun lightMap -> lightMap.SortableLightMapEnabled && geometryFrustum.Intersects lightMap.SortableLightMapBounds)

        // compute lights count for shaders
        let lightsCount = min renderTasks.Lights.Count Constants.Render.LightsMaxDeferred

        // sort light maps for deferred rendering relative to eye center
        let (lightMapOrigins, lightMapMins, lightMapSizes, lightMapIrradianceMaps, lightMapEnvironmentFilterMaps, lightMapsCount) =
            if topLevelRender then
                SortableLightMap.sortLightMapsIntoArrays Constants.Render.LightMapsMaxDeferred eyeCenter None lightMaps
            else
                (Array.zeroCreate Constants.Render.LightMapsMaxDeferred,
                 Array.zeroCreate Constants.Render.LightMapsMaxDeferred,
                 Array.zeroCreate Constants.Render.LightMapsMaxDeferred,
                 Array.init Constants.Render.LightMapsMaxDeferred (fun _ -> OpenGL.Texture.EmptyTexture),
                 Array.init Constants.Render.LightMapsMaxDeferred (fun _ -> OpenGL.Texture.EmptyTexture),
                 0)

        // sort lights for deferred rendering relative to eye center
        let (lightIds, lightOrigins, lightDirections, lightColors, lightBrightnesses, lightAttenuationLinears, lightAttenuationQuadratics, lightCutoffs, lightDirectionals, lightConeInners, lightConeOuters, lightDesireShadows) =
            SortableLight.sortLightsIntoArrays Constants.Render.LightsMaxDeferred eyeCenter renderTasks.Lights

        // compute light shadow indices according to sorted lights
        let lightShadowIndices = SortableLight.sortShadowIndices renderer.ShadowIndices lightIds lightDesireShadows lightsCount

        // grab shadow textures
        let shadowTextures = Array.map a__ renderer.ShadowBuffersArray

        // grab shadow matrices
        let shadowMatrices = Array.map (fun (m : Matrix4x4) -> m.ToArray ()) renderer.ShadowMatrices

        // sort absolute forward surfaces from far to near
        let forwardSurfacesSortBuffer = GlRenderer3d.sortForwardSurfaces eyeCenter renderTasks.ForwardStaticAbsolute renderer.ForwardSurfacesComparer renderer.ForwardSurfacesSortBuffer
        for struct (_, _, model, presence, texCoordsOffset, properties, surface, _) in forwardSurfacesSortBuffer do
            renderTasks.ForwardStaticAbsoluteSorted.Add struct (model, presence, texCoordsOffset, properties, surface)
        forwardSurfacesSortBuffer.Clear ()
        renderTasks.ForwardStaticAbsolute.Clear ()

        // sort relative forward surfaces from far to near
        let forwardSurfacesSortBuffer = GlRenderer3d.sortForwardSurfaces eyeCenter renderTasks.ForwardStaticRelative renderer.ForwardSurfacesComparer renderer.ForwardSurfacesSortBuffer
        for struct (_, _, model, presence, texCoordsOffset, properties, surface, _) in forwardSurfacesSortBuffer do
            renderTasks.ForwardStaticRelativeSorted.Add struct (model, presence, texCoordsOffset, properties, surface)
        forwardSurfacesSortBuffer.Clear ()
        renderTasks.ForwardStaticRelative.Clear ()

        // setup geometry buffer and viewport
        let (positionTexture, albedoTexture, materialTexture, normalPlusTexture, geometryRenderbuffer, geometryFramebuffer) = renderer.GeometryBuffers
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, geometryRenderbuffer)
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, geometryFramebuffer)
        OpenGL.Gl.ClearColor (Constants.Render.ViewportClearColor.R, Constants.Render.ViewportClearColor.G, Constants.Render.ViewportClearColor.B, Constants.Render.ViewportClearColor.A)
        OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
        OpenGL.Gl.Viewport (geometryViewport.Bounds.Min.X, geometryViewport.Bounds.Min.Y, geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y)
        OpenGL.Hl.Assert ()

        // deferred render static surfaces w/ absolute transforms if in top level render
        if topLevelRender then
            let mutable enr = renderTasks.DeferredStaticAbsolute.GetEnumerator ()
            let mutable i = 0
            while enr.MoveNext () do
                let entry = enr.Current
                let batchPhase =
                    match renderTasks.DeferredStaticAbsolute.Count with
                    | 1 -> SingletonPhase
                    | count -> if i = 0 then StartingPhase elif i = dec count then StoppingPhase else ResumingPhase
                GlRenderer3d.renderPhysicallyBasedDeferredSurfaces
                    batchPhase viewAbsoluteArray geometryProjectionArray [||] eyeCenter entry.Value
                    entry.Key renderer.PhysicallyBasedDeferredStaticShader renderer
                OpenGL.Hl.Assert ()
                i <- inc i

        // deferred render static surfaces w/ relative transforms
        let mutable enr = renderTasks.DeferredStaticRelative.GetEnumerator ()
        let mutable i = 0
        while enr.MoveNext () do
            let entry = enr.Current
            let batchPhase =
                match renderTasks.DeferredStaticRelative.Count with
                | 1 -> SingletonPhase
                | count -> if i = 0 then StartingPhase elif i = dec count then StoppingPhase else ResumingPhase
            GlRenderer3d.renderPhysicallyBasedDeferredSurfaces
                batchPhase viewRelativeArray geometryProjectionArray [||] eyeCenter entry.Value
                entry.Key renderer.PhysicallyBasedDeferredStaticShader renderer
            OpenGL.Hl.Assert ()
            i <- inc i

        // deferred render animated surfaces w/ absolute transforms if in top level render
        if topLevelRender then
            for entry in renderTasks.DeferredAnimatedAbsolute do
                let surfaceKey = entry.Key
                let parameters = entry.Value
                let bonesArray = Array.map (fun (boneTransform : Matrix4x4) -> boneTransform.ToArray ()) surfaceKey.BoneTransforms
                GlRenderer3d.renderPhysicallyBasedDeferredSurfaces
                    SingletonPhase viewAbsoluteArray geometryProjectionArray bonesArray eyeCenter parameters
                    surfaceKey.AnimatedModelSurface renderer.PhysicallyBasedDeferredAnimatedShader renderer
                OpenGL.Hl.Assert ()

        // deferred render animated surfaces w/ relative transforms
        for entry in renderTasks.DeferredAnimatedRelative do
            let surfaceKey = entry.Key
            let parameters = entry.Value
            let bonesArray = Array.map (fun (boneTransform : Matrix4x4) -> boneTransform.ToArray ()) surfaceKey.BoneTransforms
            GlRenderer3d.renderPhysicallyBasedDeferredSurfaces
                SingletonPhase viewRelativeArray geometryProjectionArray bonesArray eyeCenter parameters
                surfaceKey.AnimatedModelSurface renderer.PhysicallyBasedDeferredAnimatedShader renderer
            OpenGL.Hl.Assert ()

        // attempt to deferred render terrains w/ absolute transforms if in top level render
        if topLevelRender then
            for (descriptor, geometry) in renderTasks.DeferredTerrainsAbsolute do
                GlRenderer3d.renderPhysicallyBasedTerrain viewAbsoluteArray geometryProjectionArray eyeCenter descriptor geometry renderer.PhysicallyBasedDeferredTerrainShader renderer

        // attempt to deferred render terrains w/ relative transforms
        for (descriptor, geometry) in renderTasks.DeferredTerrainsRelative do
            GlRenderer3d.renderPhysicallyBasedTerrain viewRelativeArray geometryProjectionArray eyeCenter descriptor geometry renderer.PhysicallyBasedDeferredTerrainShader renderer

        // run light mapping pass
        let lightMappingTexture =

            // but only if needed
            if renderer.LightingConfig.LightMappingEnabled then

                // setup light mapping buffer and viewport
                let (lightMappingTexture, lightMappingRenderbuffer, lightMappingFramebuffer) = renderer.LightMappingBuffers
                OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, lightMappingRenderbuffer)
                OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, lightMappingFramebuffer)
                OpenGL.Gl.ClearColor (Constants.Render.ViewportClearColor.R, Constants.Render.ViewportClearColor.G, Constants.Render.ViewportClearColor.B, Constants.Render.ViewportClearColor.A)
                OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
                OpenGL.Gl.Viewport (geometryViewport.Bounds.Min.X, geometryViewport.Bounds.Min.Y, geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y)
                OpenGL.Hl.Assert ()

                // deferred render light mapping quad
                OpenGL.PhysicallyBased.DrawPhysicallyBasedDeferredLightMappingSurface
                    (positionTexture, normalPlusTexture,
                     lightMapOrigins, lightMapMins, lightMapSizes, lightMapsCount,
                     renderer.PhysicallyBasedQuad, renderer.PhysicallyBasedDeferredLightMappingShader)
                OpenGL.Hl.Assert ()
                lightMappingTexture

            // just use black texture
            else renderer.BlackTexture

        // setup irradiance buffer and viewport
        let (irradianceTexture, irradianceRenderbuffer, irradianceFramebuffer) = renderer.IrradianceBuffers
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, irradianceRenderbuffer)
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, irradianceFramebuffer)
        OpenGL.Gl.ClearColor (Constants.Render.ViewportClearColor.R, Constants.Render.ViewportClearColor.G, Constants.Render.ViewportClearColor.B, Constants.Render.ViewportClearColor.A)
        OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
        OpenGL.Gl.Viewport (geometryViewport.Bounds.Min.X, geometryViewport.Bounds.Min.Y, geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y)
        OpenGL.Hl.Assert ()

        // deferred render irradiance quad
        OpenGL.PhysicallyBased.DrawPhysicallyBasedDeferredIrradianceSurface
            (positionTexture, normalPlusTexture, lightMappingTexture,
             lightMapFallback.IrradianceMap, lightMapIrradianceMaps,
             lightMapOrigins, lightMapMins, lightMapSizes,
             renderer.PhysicallyBasedQuad, renderer.PhysicallyBasedDeferredIrradianceShader)
        OpenGL.Hl.Assert ()

        // setup environment filter buffer and viewport
        let (environmentFilterTexture, environmentFilterRenderbuffer, environmentFilterFramebuffer) = renderer.EnvironmentFilterBuffers
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, environmentFilterRenderbuffer)
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, environmentFilterFramebuffer)
        OpenGL.Gl.ClearColor (Constants.Render.ViewportClearColor.R, Constants.Render.ViewportClearColor.G, Constants.Render.ViewportClearColor.B, Constants.Render.ViewportClearColor.A)
        OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
        OpenGL.Gl.Viewport (geometryViewport.Bounds.Min.X, geometryViewport.Bounds.Min.Y, geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y)
        OpenGL.Hl.Assert ()

        // deferred render environment filter quad
        OpenGL.PhysicallyBased.DrawPhysicallyBasedDeferredEnvironmentFilterSurface
            (eyeCenter,
             positionTexture, materialTexture, normalPlusTexture, lightMappingTexture,
             lightMapFallback.EnvironmentFilterMap, lightMapEnvironmentFilterMaps,
             lightMapOrigins, lightMapMins, lightMapSizes,
             renderer.PhysicallyBasedQuad, renderer.PhysicallyBasedDeferredEnvironmentFilterShader)
        OpenGL.Hl.Assert ()

        // run ssao pass
        let ssaoTextureFiltered =

            // but only if needed
            if renderer.LightingConfig.SsaoEnabled then

                // setup unfiltered ssao buffer and viewport
                let (ssaoTextureUnfiltered, ssaoRenderbuffer, ssaoFramebuffer) = renderer.SsaoBuffersUnfiltered
                OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, ssaoRenderbuffer)
                OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, ssaoFramebuffer)
                OpenGL.Gl.ClearColor (Constants.Render.ViewportClearColor.R, Constants.Render.ViewportClearColor.G, Constants.Render.ViewportClearColor.B, Constants.Render.ViewportClearColor.A)
                OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
                OpenGL.Gl.Viewport (ssaoViewport.Bounds.Min.X, ssaoViewport.Bounds.Min.Y, ssaoViewport.Bounds.Size.X, ssaoViewport.Bounds.Size.Y)
                OpenGL.Hl.Assert ()

                // deferred render ssao quad
                OpenGL.PhysicallyBased.DrawPhysicallyBasedDeferredSsaoSurface
                    (viewRelativeArray, rasterProjectionArray,
                     positionTexture, normalPlusTexture,
                     [|Constants.Render.SsaoResolution.X; Constants.Render.SsaoResolution.Y|],
                     renderer.LightingConfig.SsaoIntensity, renderer.LightingConfig.SsaoBias, renderer.LightingConfig.SsaoRadius, renderer.LightingConfig.SsaoDistanceMax, renderer.LightingConfig.SsaoSampleCount,
                     renderer.PhysicallyBasedQuad, renderer.PhysicallyBasedDeferredSsaoShader)
                OpenGL.Hl.Assert ()

                // setup filtered ssao buffer and viewport
                let (ssaoTextureFiltered, ssaoFilteredRenderbuffer, ssaoFilteredFramebuffer) = renderer.SsaoBuffersFiltered
                OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, ssaoFilteredRenderbuffer)
                OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, ssaoFilteredFramebuffer)
                OpenGL.Gl.ClearColor (Constants.Render.ViewportClearColor.R, Constants.Render.ViewportClearColor.G, Constants.Render.ViewportClearColor.B, Constants.Render.ViewportClearColor.A)
                OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
                OpenGL.Gl.Viewport (ssaoViewport.Bounds.Min.X, ssaoViewport.Bounds.Min.Y, ssaoViewport.Bounds.Size.X, ssaoViewport.Bounds.Size.Y)
                OpenGL.Hl.Assert ()

                // deferred render filtered ssao quad
                OpenGL.PhysicallyBased.DrawFilterBoxSurface (ssaoTextureUnfiltered, renderer.PhysicallyBasedQuad, renderer.FilterBox1dShader)
                OpenGL.Hl.Assert ()
                ssaoTextureFiltered

            // just use white texture
            else renderer.WhiteTexture

        // setup filter buffer and viewport
        let (filterTexture, filterRenderbuffer, filterFramebuffer) = renderer.FilterBuffers
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, filterRenderbuffer)
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, filterFramebuffer)
        OpenGL.Gl.ClearColor (Constants.Render.ViewportClearColor.R, Constants.Render.ViewportClearColor.G, Constants.Render.ViewportClearColor.B, Constants.Render.ViewportClearColor.A)
        OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
        OpenGL.Gl.Viewport (geometryViewport.Bounds.Min.X, geometryViewport.Bounds.Min.Y, geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y)
        OpenGL.Hl.Assert ()

        // copy depths from geometry framebuffer to filter framebuffer
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.ReadFramebuffer, geometryFramebuffer)
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.DrawFramebuffer, filterFramebuffer)
        OpenGL.Gl.BlitFramebuffer
            (geometryViewport.Bounds.Min.X, geometryViewport.Bounds.Min.Y, geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y,
             geometryViewport.Bounds.Min.X, geometryViewport.Bounds.Min.Y, geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y,
             OpenGL.ClearBufferMask.DepthBufferBit,
             OpenGL.BlitFramebufferFilter.Nearest)
        OpenGL.Hl.Assert ()

        // restore filter buffer
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, filterRenderbuffer)
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, filterFramebuffer)
        OpenGL.Gl.Viewport (geometryViewport.Bounds.Min.X, geometryViewport.Bounds.Min.Y, geometryViewport.Bounds.Size.X, geometryViewport.Bounds.Size.Y)
        OpenGL.Hl.Assert ()

        // deferred render lighting quad to filter buffer
        OpenGL.PhysicallyBased.DrawPhysicallyBasedDeferredLightingSurface
            (eyeCenter, renderer.LightingConfig.LightCutoffMargin, lightAmbientColor, lightAmbientBrightness, renderer.LightingConfig.LightShadowBiasAcne, renderer.LightingConfig.LightShadowBiasBleed,
             positionTexture, albedoTexture, materialTexture, normalPlusTexture, renderer.BrdfTexture, irradianceTexture, environmentFilterTexture, ssaoTextureFiltered, shadowTextures,
             lightOrigins, lightDirections, lightColors, lightBrightnesses, lightAttenuationLinears, lightAttenuationQuadratics, lightCutoffs, lightDirectionals, lightConeInners, lightConeOuters, lightShadowIndices, lightsCount, shadowMatrices,
             renderer.PhysicallyBasedQuad, renderer.PhysicallyBasedDeferredLightingShader)
        OpenGL.Hl.Assert ()

        // attempt to render sky box to filter buffer
        match skyBoxOpt with
        | Some (cubeMapColor, cubeMapBrightness, cubeMap, _) ->
            let cubeMapColor = [|cubeMapColor.R; cubeMapColor.G; cubeMapColor.B|]
            OpenGL.SkyBox.DrawSkyBox (viewSkyBoxArray, rasterProjectionArray, cubeMapColor, cubeMapBrightness, cubeMap, renderer.CubeMapGeometry, renderer.SkyBoxShader)
            OpenGL.Hl.Assert ()
        | None -> ()

        // forward render static surfaces w/ absolute transforms to filter buffer if in top level render
        if topLevelRender then
            for (model, presence, texCoordsOffset, properties, surface) in renderTasks.ForwardStaticAbsoluteSorted do
                let (lightMapOrigins, lightMapMins, lightMapSizes, lightMapIrradianceMaps, lightMapEnvironmentFilterMaps, lightMapsCount) =
                    let surfaceBounds = surface.SurfaceBounds.Transform model
                    SortableLightMap.sortLightMapsIntoArrays Constants.Render.LightMapsMaxForward model.Translation (Some surfaceBounds) lightMaps
                let (lightIds, lightOrigins, lightDirections, lightColors, lightBrightnesses, lightAttenuationLinears, lightAttenuationQuadratics, lightCutoffs, lightDirectionals, lightConeInners, lightConeOuters, lightDesireShadows) =
                    SortableLight.sortLightsIntoArrays Constants.Render.LightsMaxForward model.Translation renderTasks.Lights
                let lightShadowIndices =
                    SortableLight.sortShadowIndices renderer.ShadowIndices lightIds lightDesireShadows lightsCount
                GlRenderer3d.renderPhysicallyBasedForwardSurfaces
                    true viewAbsoluteArray rasterProjectionArray [||] (SList.singleton (model, presence, texCoordsOffset, properties))
                    eyeCenter renderer.LightingConfig.LightCutoffMargin lightAmbientColor lightAmbientBrightness renderer.LightingConfig.LightShadowBiasAcne renderer.LightingConfig.LightShadowBiasBleed
                    renderer.BrdfTexture lightMapFallback.IrradianceMap lightMapFallback.EnvironmentFilterMap lightMapIrradianceMaps lightMapEnvironmentFilterMaps shadowTextures lightMapOrigins lightMapMins lightMapSizes lightMapsCount
                    lightOrigins lightDirections lightColors lightBrightnesses lightAttenuationLinears lightAttenuationQuadratics lightCutoffs lightDirectionals lightConeInners lightConeOuters lightShadowIndices lightsCount shadowMatrices
                    surface renderer.PhysicallyBasedForwardStaticShader renderer
                OpenGL.Hl.Assert ()

        // forward render static surfaces w/ relative transforms to filter buffer
        for (model, presence, texCoordsOffset, properties, surface) in renderTasks.ForwardStaticRelativeSorted do
            let (lightMapOrigins, lightMapMins, lightMapSizes, lightMapIrradianceMaps, lightMapEnvironmentFilterMaps, lightMapsCount) =
                let surfaceBounds = surface.SurfaceBounds.Transform model
                SortableLightMap.sortLightMapsIntoArrays Constants.Render.LightMapsMaxForward model.Translation (Some surfaceBounds) lightMaps
            let (lightIds, lightOrigins, lightDirections, lightColors, lightBrightnesses, lightAttenuationLinears, lightAttenuationQuadratics, lightCutoffs, lightDirectionals, lightConeInners, lightConeOuters, lightDesireShadows) =
                SortableLight.sortLightsIntoArrays Constants.Render.LightsMaxForward model.Translation renderTasks.Lights
            let lightShadowIndices =
                SortableLight.sortShadowIndices renderer.ShadowIndices lightIds lightDesireShadows lightsCount
            GlRenderer3d.renderPhysicallyBasedForwardSurfaces
                true viewRelativeArray rasterProjectionArray [||] (SList.singleton (model, presence, texCoordsOffset, properties))
                eyeCenter renderer.LightingConfig.LightCutoffMargin lightAmbientColor lightAmbientBrightness renderer.LightingConfig.LightShadowBiasAcne renderer.LightingConfig.LightShadowBiasBleed
                renderer.BrdfTexture lightMapFallback.IrradianceMap lightMapFallback.EnvironmentFilterMap lightMapIrradianceMaps lightMapEnvironmentFilterMaps shadowTextures lightMapOrigins lightMapMins lightMapSizes lightMapsCount
                lightOrigins lightDirections lightColors lightBrightnesses lightAttenuationLinears lightAttenuationQuadratics lightCutoffs lightDirectionals lightConeInners lightConeOuters lightShadowIndices lightsCount shadowMatrices
                surface renderer.PhysicallyBasedForwardStaticShader renderer
            OpenGL.Hl.Assert ()

        // setup raster buffer and viewport
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, renderbuffer)
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, framebuffer)
        OpenGL.Gl.Viewport (rasterViewport.Bounds.Min.X, rasterViewport.Bounds.Min.Y, rasterViewport.Bounds.Size.X, rasterViewport.Bounds.Size.Y)
        OpenGL.Hl.Assert ()

        // render filter quad via fxaa
        OpenGL.PhysicallyBased.DrawFilterFxaaSurface (filterTexture, renderer.PhysicallyBasedQuad, renderer.FilterFxaaShader)
        OpenGL.Hl.Assert ()

        // destroy cached geometries that weren't rendered this frame
        if topLevelRender then
            for geometry in renderer.PhysicallyBasedTerrainGeometries do
                if not (renderer.PhysicallyBasedTerrainGeometriesUtilized.Contains geometry.Key) then
                    OpenGL.PhysicallyBased.DestroyPhysicallyBasedGeometry geometry.Value
                    renderer.PhysicallyBasedTerrainGeometries.Remove geometry.Key |> ignore<bool>

    /// Render 3d surfaces.
    static member render frustumInterior frustumExterior frustumImposter lightBox eyeCenter (eyeRotation : Quaternion) windowSize renderbuffer framebuffer renderMessages renderer =

        // categorize messages
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
                    Log.infoOnce ("Multiple light probe messages coming in with the same id of '" + string rlp.LightProbeId + "'.")
                    renderTasks.LightProbes.Remove rlp.LightProbeId |> ignore<bool>
                renderTasks.LightProbes.Add (rlp.LightProbeId, struct (rlp.Enabled, rlp.Origin, rlp.Bounds))
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
                      SortableLightDirectional = match rl.LightType with DirectionalLight -> 1 | _ -> 0
                      SortableLightConeInner = coneInner
                      SortableLightConeOuter = coneOuter
                      SortableLightDesireShadows = if rl.DesireShadows then 1 else 0
                      SortableLightDistanceSquared = Single.MaxValue }
                renderTasks.Lights.Add light
                if rl.DesireShadows then
                    renderer.LightsDesiringShadows.[rl.LightId] <- light
            | RenderBillboard rb ->
                let struct (billboardProperties, billboardMaterial) = GlRenderer3d.makeBillboardMaterial (&rb.MaterialProperties, &rb.Material, renderer)
                let billboardSurface = OpenGL.PhysicallyBased.CreatePhysicallyBasedSurface (Array.empty, m4Identity, box3 (v3 -0.5f 0.5f -0.5f) v3One, billboardProperties, billboardMaterial, -1, Assimp.Node.Empty, renderer.BillboardGeometry)
                GlRenderer3d.categorizeBillboardSurface (rb.Absolute, eyeRotation, rb.ModelMatrix, rb.Presence, rb.InsetOpt, billboardMaterial.AlbedoTexture.TextureMetadata, true, rb.MaterialProperties, billboardSurface, rb.RenderType, rb.RenderPass, renderer)
            | RenderBillboards rbs ->
                let struct (billboardProperties, billboardMaterial) = GlRenderer3d.makeBillboardMaterial (&rbs.MaterialProperties, &rbs.Material, renderer)
                let billboardSurface = OpenGL.PhysicallyBased.CreatePhysicallyBasedSurface (Array.empty, m4Identity, box3 (v3 -0.5f -0.5f -0.5f) v3One, billboardProperties, billboardMaterial, -1, Assimp.Node.Empty, renderer.BillboardGeometry)
                for (model, presence, insetOpt) in rbs.Billboards do
                    GlRenderer3d.categorizeBillboardSurface (rbs.Absolute, eyeRotation, model, presence, insetOpt, billboardMaterial.AlbedoTexture.TextureMetadata, true, rbs.MaterialProperties, billboardSurface, rbs.RenderType, rbs.RenderPass, renderer)
            | RenderBillboardParticles rbps ->
                let struct (billboardProperties, billboardMaterial) = GlRenderer3d.makeBillboardMaterial (&rbps.MaterialProperties, &rbps.Material, renderer)
                for particle in rbps.Particles do
                    let billboardMatrix =
                        Matrix4x4.CreateFromTrs
                            (particle.Transform.Position,
                             particle.Transform.Rotation,
                             particle.Transform.Size * particle.Transform.Scale)
                    let billboardProperties = { billboardProperties with Albedo = billboardProperties.Albedo * particle.Color; Emission = particle.Emission.R }
                    let billboardSurface = OpenGL.PhysicallyBased.CreatePhysicallyBasedSurface (Array.empty, m4Identity, box3Zero, billboardProperties, billboardMaterial, -1, Assimp.Node.Empty, renderer.BillboardGeometry)
                    GlRenderer3d.categorizeBillboardSurface (rbps.Absolute, eyeRotation, billboardMatrix, rbps.Presence, Option.ofValueOption particle.InsetOpt, billboardMaterial.AlbedoTexture.TextureMetadata, false, rbps.MaterialProperties, billboardSurface, rbps.RenderType, rbps.RenderPass, renderer)
            | RenderStaticModelSurface rsms ->
                let insetOpt = Option.toValueOption rsms.InsetOpt
                GlRenderer3d.categorizeStaticModelSurfaceByIndex (rsms.Absolute, &rsms.ModelMatrix, rsms.Presence, &insetOpt, &rsms.MaterialProperties, &rsms.Material, rsms.StaticModel, rsms.SurfaceIndex, rsms.RenderType, rsms.RenderPass, renderer)
            | RenderStaticModel rsm ->
                let insetOpt = Option.toValueOption rsm.InsetOpt
                GlRenderer3d.categorizeStaticModel (frustumInterior, frustumExterior, frustumImposter, lightBox, rsm.Absolute, &rsm.ModelMatrix, rsm.Presence, &insetOpt, &rsm.MaterialProperties, rsm.StaticModel, rsm.RenderType, rsm.RenderPass, renderer)
            | RenderStaticModels rsms ->
                for (model, presence, insetOpt, properties) in rsms.StaticModels do
                    let insetOpt = Option.toValueOption insetOpt
                    GlRenderer3d.categorizeStaticModel (frustumInterior, frustumExterior, frustumImposter, lightBox, rsms.Absolute, &model, presence, &insetOpt, &properties, rsms.StaticModel, rsms.RenderType, rsms.RenderPass, renderer)
            | RenderCachedStaticModel csmm ->
                GlRenderer3d.categorizeStaticModel (frustumInterior, frustumExterior, frustumImposter, lightBox, csmm.CachedStaticModelAbsolute, &csmm.CachedStaticModelMatrix, csmm.CachedStaticModelPresence, &csmm.CachedStaticModelInsetOpt, &csmm.CachedStaticModelMaterialProperties, csmm.CachedStaticModel, csmm.CachedStaticModelRenderType, csmm.CachedStaticModelRenderPass, renderer)
            | RenderCachedStaticModelSurface csmsm ->
                GlRenderer3d.categorizeStaticModelSurfaceByIndex (csmsm.CachedStaticModelSurfaceAbsolute, &csmsm.CachedStaticModelSurfaceMatrix, csmsm.CachedStaticModelSurfacePresence, &csmsm.CachedStaticModelSurfaceInsetOpt, &csmsm.CachedStaticModelSurfaceMaterialProperties, &csmsm.CachedStaticModelSurfaceMaterial, csmsm.CachedStaticModelSurfaceModel, csmsm.CachedStaticModelSurfaceIndex, csmsm.CachedStaticModelSurfaceRenderType, csmsm.CachedStaticModelSurfaceRenderPass, renderer)
            | RenderUserDefinedStaticModel rudsm ->
                let insetOpt = Option.toValueOption rudsm.InsetOpt
                let assetTag = asset Assets.Default.PackageName Gen.name // TODO: see if we should instead use a specialized package for temporary assets like these.
                GlRenderer3d.tryCreateUserDefinedStaticModel rudsm.StaticModelSurfaceDescriptors rudsm.Bounds assetTag renderer
                GlRenderer3d.categorizeStaticModel (frustumInterior, frustumExterior, frustumImposter, lightBox, rudsm.Absolute, &rudsm.ModelMatrix, rudsm.Presence, &insetOpt, &rudsm.MaterialProperties, assetTag, rudsm.RenderType, rudsm.RenderPass, renderer)
                userDefinedStaticModelsToDestroy.Add assetTag
            | RenderAnimatedModel rsm ->
                let insetOpt = Option.toValueOption rsm.InsetOpt
                GlRenderer3d.categorizeAnimatedModel (rsm.Absolute, &rsm.ModelMatrix, rsm.Presence, &insetOpt, &rsm.MaterialProperties, rsm.BoneTransforms, rsm.AnimatedModel, rsm.RenderPass, renderer)
            | RenderAnimatedModels rams ->
                GlRenderer3d.categorizeAnimatedModels (rams.Absolute, rams.AnimatedModels, rams.BoneTransforms, rams.AnimatedModel, rams.RenderPass, renderer)
            | RenderCachedAnimatedModel camm ->
                GlRenderer3d.categorizeAnimatedModel (camm.CachedAnimatedModelAbsolute, &camm.CachedAnimatedModelMatrix, camm.CachedAnimatedModelPresence, &camm.CachedAnimatedModelInsetOpt, &camm.CachedAnimatedModelMaterialProperties, camm.CachedAnimatedModelBoneTransforms, camm.CachedAnimatedModel, camm.CachedAnimatedModelRenderPass, renderer)
            | RenderTerrain rt ->
                GlRenderer3d.categorizeTerrain (rt.Absolute, rt.Visible, rt.TerrainDescriptor, rt.RenderPass, renderer)
            | ConfigureLighting lc ->
                renderer.LightingConfig <- lc
            | LoadRenderPackage3d packageName ->
                GlRenderer3d.handleLoadRenderPackage packageName renderer
            | UnloadRenderPackage3d packageName ->
                GlRenderer3d.handleUnloadRenderPackage packageName renderer
            | ReloadRenderAssets3d ->
                renderer.ReloadAssetsRequested <- true

        // light map and shadow pre-passes
        let mutable shadowBufferIndex = 0
        for (renderPass, renderTasks) in renderer.RenderTasksDictionary.Pairs do

            // fallback light map pre-pass
            match GlRenderer3d.getLastSkyBoxOpt renderPass renderer |> __c with
            | Some (_, _, cubeMap, irradianceAndEnvironmentMapsOptRef : (OpenGL.Texture.Texture * OpenGL.Texture.Texture) option ref) ->

                // render fallback irradiance and env filter maps
                if Option.isNone irradianceAndEnvironmentMapsOptRef.Value then

                    // render fallback irradiance map
                    let irradianceMap =
                        OpenGL.LightMap.CreateIrradianceMap
                            (Constants.Render.IrradianceMapResolution,
                             renderer.IrradianceShader,
                             OpenGL.CubeMap.CubeMapSurface.make cubeMap renderer.CubeMapGeometry)

                    // render fallback env filter map
                    let environmentFilterMap =
                        OpenGL.LightMap.CreateEnvironmentFilterMap
                            (Constants.Render.EnvironmentFilterResolution,
                             renderer.EnvironmentFilterShader,
                             OpenGL.CubeMap.CubeMapSurface.make cubeMap renderer.CubeMapGeometry)

                    // add to cache and create light map
                    irradianceAndEnvironmentMapsOptRef.Value <- Some (irradianceMap, environmentFilterMap)

                // nothing to do
                | None -> ()

            match renderPass with
            | LightMapPass (lightProbeId, _) ->
                if renderTasks.LightMapRenders.Contains lightProbeId then

                    // destroy any existing light map
                    match renderer.LightMaps.TryGetValue lightProbeId with
                    | (true, lightMap) ->
                        OpenGL.LightMap.DestroyLightMap lightMap
                        renderer.LightMaps.Remove lightProbeId |> ignore<bool>
                    | (false, _) -> ()

                    match renderTasks.LightProbes.TryGetValue lightProbeId with
                    | (true, struct (lightProbeEnabled, lightProbeOrigin, lightProbeBounds)) ->

                        // create reflection map
                        let reflectionMap =
                            OpenGL.LightMap.CreateReflectionMap
                                (GlRenderer3d.renderGeometry renderPass (GlRenderer3d.getRenderTasks renderPass renderer) renderer,
                                 Constants.Render.Resolution,
                                 Constants.Render.SsaoResolution,
                                 Constants.Render.ReflectionMapResolution,
                                 lightProbeOrigin)

                        // create irradiance map
                        let irradianceMap =
                            OpenGL.LightMap.CreateIrradianceMap
                                (Constants.Render.IrradianceMapResolution,
                                 renderer.IrradianceShader,
                                 OpenGL.CubeMap.CubeMapSurface.make reflectionMap renderer.CubeMapGeometry)

                        // create env filter map
                        let environmentFilterMap =
                            OpenGL.LightMap.CreateEnvironmentFilterMap
                                (Constants.Render.EnvironmentFilterResolution,
                                 renderer.EnvironmentFilterShader,
                                 OpenGL.CubeMap.CubeMapSurface.make reflectionMap renderer.CubeMapGeometry)

                        // destroy reflection map
                        reflectionMap.Destroy ()

                        // create light map
                        let lightMap = OpenGL.LightMap.CreateLightMap lightProbeEnabled lightProbeOrigin lightProbeBounds irradianceMap environmentFilterMap

                        // add light map to cache
                        renderer.LightMaps.Add (lightProbeId, lightMap)

                    | (false, _) -> ()

            | ShadowPass (lightId, _, _) ->
                match renderer.LightsDesiringShadows.TryGetValue lightId with
                | (true, light) ->
                    if shadowBufferIndex < Constants.Render.ShadowsMax then

                        // draw shadows
                        let (shadowOrigin, shadowView, shadowProjection) =
                            if light.SortableLightDirectional = 0 then
                                let shadowOrigin = light.SortableLightOrigin
                                let mutable shadowView = Matrix4x4.CreateFromYawPitchRoll (0.0f, -MathF.PI_OVER_2, 0.0f) * Matrix4x4.CreateFromQuaternion light.SortableLightRotation
                                shadowView.Translation <- light.SortableLightOrigin
                                shadowView <- shadowView.Inverted
                                let shadowFov = max (min light.SortableLightConeOuter Constants.Render.ShadowFovMax) 0.01f
                                let shadowCutoff = max light.SortableLightCutoff 0.1f
                                let shadowProjection = Matrix4x4.CreatePerspectiveFieldOfView (shadowFov, 1.0f, Constants.Render.NearPlaneDistanceInterior, shadowCutoff)
                                (shadowOrigin, shadowView, shadowProjection)
                            else
                                let shadowOrigin = light.SortableLightOrigin
                                let mutable shadowView = Matrix4x4.CreateFromYawPitchRoll (0.0f, -MathF.PI_OVER_2, 0.0f) * Matrix4x4.CreateFromQuaternion light.SortableLightRotation
                                shadowView.Translation <- light.SortableLightOrigin
                                shadowView <- shadowView.Inverted
                                let shadowCutoff = light.SortableLightCutoff
                                let shadowProjection = Matrix4x4.CreateOrthographic (shadowCutoff * 2.0f, shadowCutoff * 2.0f, -shadowCutoff, shadowCutoff)
                                (shadowOrigin, shadowView, shadowProjection)
                        let shadowResolution = GlRenderer3d.getShadowBufferResolution shadowBufferIndex
                        let (shadowTexture, shadowRenderbuffer, shadowFramebuffer) = renderer.ShadowBuffersArray.[shadowBufferIndex]
                        GlRenderer3d.renderShadowTexture renderTasks renderer false shadowOrigin m4Identity shadowView shadowProjection shadowResolution shadowRenderbuffer shadowFramebuffer
                        renderer.ShadowMatrices.[shadowBufferIndex] <- shadowView * shadowProjection
                        renderer.ShadowIndices.[light.SortableLightId] <- shadowBufferIndex

                        // filter shadows on the x (presuming that viewport already configured correctly)
                        let (shadowTexture2, shadowRenderbuffer2, shadowFramebuffer2) = renderer.ShadowBuffers2Array.[shadowBufferIndex]
                        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, shadowRenderbuffer2)
                        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, shadowFramebuffer2)
                        OpenGL.PhysicallyBased.DrawFilterGaussianSurface (v2 (1.0f / single shadowResolution.X) 0.0f, shadowTexture, renderer.PhysicallyBasedQuad, renderer.FilterGaussian2dShader)
                        OpenGL.Hl.Assert ()

                        // filter shadows on the y (presuming that viewport already configured correctly)
                        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, shadowRenderbuffer)
                        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, shadowFramebuffer)
                        OpenGL.PhysicallyBased.DrawFilterGaussianSurface (v2 0.0f (1.0f / single shadowResolution.Y), shadowTexture2, renderer.PhysicallyBasedQuad, renderer.FilterGaussian2dShader)
                        OpenGL.Hl.Assert ()

                        // next shadow
                        shadowBufferIndex <- inc shadowBufferIndex
                    | (false, _) -> ()
            | _ -> ()

        // compute the viewports for the given window size
        let viewport = Constants.Render.Viewport
        let ssaoViewport = Constants.Render.SsaoViewport
        let viewportOffset = Constants.Render.ViewportOffset windowSize

        // compute view and projection
        let viewAbsolute = viewport.View3d (true, eyeCenter, eyeRotation)
        let viewRelative = viewport.View3d (false, eyeCenter, eyeRotation)
        let viewSkyBox = Matrix4x4.CreateFromQuaternion (Quaternion.Inverse eyeRotation)
        let projection = viewport.Projection3d

        // top-level geometry pass
        let renderPass = NormalPass
        let normalTasks = GlRenderer3d.getRenderTasks renderPass renderer
        GlRenderer3d.renderGeometry renderPass normalTasks renderer true eyeCenter eyeRotation viewAbsolute viewRelative viewSkyBox viewport projection ssaoViewport viewportOffset projection renderbuffer framebuffer

        // reset terrain geometry book-keeping
        renderer.PhysicallyBasedTerrainGeometriesUtilized.Clear ()

        // clear render tasks
        // TODO: P1: find some way to purge unused render tasks (ForgetfulDictionary?)
        for renderTasks in renderer.RenderTasksDictionary.Values do
            RenderTasks.clear renderTasks

        // clear shadow matrices
        renderer.ShadowIndices.Clear ()

        // clear lights desiring shadows
        renderer.LightsDesiringShadows.Clear ()

        // destroy user-defined static models
        for staticModel in userDefinedStaticModelsToDestroy do
            GlRenderer3d.tryDestroyUserDefinedStaticModel staticModel renderer

        // reload render assets upon request
        if renderer.ReloadAssetsRequested then
            GlRenderer3d.handleReloadRenderAssets renderer
            OpenGL.Hl.Assert ()
            renderer.ReloadAssetsRequested <- false

    /// Make a GlRenderer3d.
    static member make glContext window =

        // start lazy texture server
        let sglWindow = match window with SglWindow sglWindow -> sglWindow.SglWindow
        SDL.SDL_GL_MakeCurrent (sglWindow, IntPtr.Zero) |> ignore<int>
        let lazyTextureQueues = ConcurrentDictionary<OpenGL.Texture.LazyTexture ConcurrentQueue, OpenGL.Texture.LazyTexture ConcurrentQueue> HashIdentity.Reference
        let lazyTextureServer = OpenGL.Texture.LazyTextureServer (lazyTextureQueues, glContext, sglWindow)
        lazyTextureServer.Start ()
        SDL.SDL_GL_MakeCurrent (sglWindow, glContext) |> ignore<int>

        // create sky box shader
        let skyBoxShader = OpenGL.SkyBox.CreateSkyBoxShader Constants.Paths.SkyBoxShaderFilePath
        OpenGL.Hl.Assert ()

        // create irradiance shader
        let irradianceShader = OpenGL.CubeMap.CreateCubeMapShader Constants.Paths.IrradianceShaderFilePath
        OpenGL.Hl.Assert ()

        // create environment filter shader
        let environmentFilterShader = OpenGL.LightMap.CreateEnvironmentFilterShader Constants.Paths.EnvironmentFilterShaderFilePath
        OpenGL.Hl.Assert ()

        // create filter box 1d shader
        let filterBox1dShader = OpenGL.Filter.CreateFilterBoxShader Constants.Paths.FilterBox1dShaderFilePath
        OpenGL.Hl.Assert ()

        // create filter gaussian 2d shader
        let filterGaussian2dShader = OpenGL.Filter.CreateFilterGaussianShader Constants.Paths.FilterGaussian2dShaderFilePath
        OpenGL.Hl.Assert ()

        // create filter fxaa shader
        let filterFxaaShader = OpenGL.Filter.CreateFilterFxaaShader Constants.Paths.FilterFxaaShaderFilePath
        OpenGL.Hl.Assert ()

        // create shadow shaders
        let (shadowStaticShader, shadowAnimatedShader, shadowTerrainShader) =
            OpenGL.PhysicallyBased.CreatePhysicallyBasedShadowShaders
                (Constants.Paths.PhysicallyBasedShadowStaticShaderFilePath,
                 Constants.Paths.PhysicallyBasedShadowAnimatedShaderFilePath,
                 Constants.Paths.PhysicallyBasedShadowTerrainShaderFilePath)
        OpenGL.Hl.Assert ()

        // create deferred shaders
        let (deferredStaticShader, deferredAnimatedShader, deferredTerrainShader, deferredLightMappingShader, deferredIrradianceShader, deferredEnvironmentFilterShader, deferredSsaoShader, deferredLightingShader) =
            OpenGL.PhysicallyBased.CreatePhysicallyBasedDeferredShaders
                (Constants.Paths.PhysicallyBasedDeferredStaticShaderFilePath,
                 Constants.Paths.PhysicallyBasedDeferredAnimatedShaderFilePath,
                 Constants.Paths.PhysicallyBasedDeferredTerrainShaderFilePath,
                 Constants.Paths.PhysicallyBasedDeferredLightMappingShaderFilePath,
                 Constants.Paths.PhysicallyBasedDeferredIrradianceShaderFilePath,
                 Constants.Paths.PhysicallyBasedDeferredEnvironmentFilterShaderFilePath,
                 Constants.Paths.PhysicallyBasedDeferredSsaoShaderFilePath,
                 Constants.Paths.PhysicallyBasedDeferredLightingShaderFilePath)
        OpenGL.Hl.Assert ()

        // create forward shader
        let forwardStaticShader = OpenGL.PhysicallyBased.CreatePhysicallyBasedShader Constants.Paths.PhysicallyBasedForwardStaticShaderFilePath
        OpenGL.Hl.Assert ()

        // create shadow buffers array
        let shadowBuffersArray =
            [|for shadowBufferIndex in 0 .. dec Constants.Render.ShadowsMax do
                let shadowResolution = GlRenderer3d.getShadowBufferResolution shadowBufferIndex
                match OpenGL.Framebuffer.TryCreateShadowBuffers (shadowResolution.X, shadowResolution.Y) with
                | Right shadowBuffers -> shadowBuffers
                | Left error -> failwith ("Could not create GlRenderer3d due to: " + error + ".")|]

        // create second array of shadow buffers
        let shadowBuffers2Array =
            [|for shadowBufferIndex in 0 .. dec Constants.Render.ShadowsMax do
                let shadowResolution = GlRenderer3d.getShadowBufferResolution shadowBufferIndex
                match OpenGL.Framebuffer.TryCreateShadowBuffers (shadowResolution.X, shadowResolution.Y) with
                | Right shadowBuffers -> shadowBuffers
                | Left error -> failwith ("Could not create GlRenderer3d due to: " + error + ".")|]

        // create shadow matrices array
        let shadowMatrices = Array.zeroCreate<Matrix4x4> Constants.Render.ShadowsMax

        // create shadow indices
        let shadowIndices = dictPlus HashIdentity.Structural []

        // create geometry buffers
        let geometryBuffers =
            match OpenGL.Framebuffer.TryCreateGeometryBuffers () with
            | Right geometryBuffers -> geometryBuffers
            | Left error -> failwith ("Could not create GlRenderer3d due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create light mapping buffers
        let lightMappingBuffers =
            match OpenGL.Framebuffer.TryCreateLightMappingBuffers () with
            | Right lightMappingBuffers -> lightMappingBuffers
            | Left error -> failwith ("Could not create GlRenderer3d due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create irradiance buffers
        let irradianceBuffers =
            match OpenGL.Framebuffer.TryCreateIrradianceBuffers () with
            | Right irradianceBuffers -> irradianceBuffers
            | Left error -> failwith ("Could not create GlRenderer3d due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create environment filter buffers
        let environmentFilterBuffers =
            match OpenGL.Framebuffer.TryCreateEnvironmentFilterBuffers () with
            | Right environmentFilterBuffers -> environmentFilterBuffers
            | Left error -> failwith ("Could not create GlRenderer3d due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create unfiltered ssao buffers
        let ssaoBuffersUnfiltered =
            match OpenGL.Framebuffer.TryCreateSsaoBuffers () with
            | Right ssaoBuffers -> ssaoBuffers
            | Left error -> failwith ("Could not create GlRenderer3d due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create filtered ssao buffers
        let ssaoBuffersFiltered =
            match OpenGL.Framebuffer.TryCreateSsaoBuffers () with
            | Right ssaoBuffers -> ssaoBuffers
            | Left error -> failwith ("Could not create GlRenderer3d due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create filter buffers
        let filterBuffers =
            match OpenGL.Framebuffer.TryCreateFilterBuffers (Constants.Render.Resolution.X, Constants.Render.Resolution.Y) with
            | Right filterBuffers -> filterBuffers
            | Left error -> failwith ("Could not create GlRenderer3d due to: " + error + ".")
        OpenGL.Hl.Assert ()

        // create white cube map
        let cubeMap =
            let white = "Assets/Default/White.bmp"
            match OpenGL.CubeMap.TryCreateCubeMap (white, white, white, white, white, white) with
            | Right cubeMap -> cubeMap
            | Left error -> failwith error
        OpenGL.Hl.Assert ()

        // create cube map geometry
        let cubeMapGeometry = OpenGL.CubeMap.CreateCubeMapGeometry true
        OpenGL.Hl.Assert ()

        // create billboard geometry
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
            match OpenGL.Texture.TryCreateTextureGl (false, OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true, false, "Assets/Default/White.bmp") with
            | Right (metadata, textureId) ->
                let textureHandle = OpenGL.Texture.CreateTextureHandle textureId
                OpenGL.Texture.EagerTexture { TextureMetadata = metadata; TextureId = textureId; TextureHandle = textureHandle }
            | Left error -> failwith ("Could not load white texture due to: " + error)
        OpenGL.Hl.Assert ()

        // create black texture
        let blackTexture =
            match OpenGL.Texture.TryCreateTextureGl (false, OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true, false, "Assets/Default/Black.bmp") with
            | Right (metadata, textureId) ->
                let textureHandle = OpenGL.Texture.CreateTextureHandle textureId
                OpenGL.Texture.EagerTexture { TextureMetadata = metadata; TextureId = textureId; TextureHandle = textureHandle }
            | Left error -> failwith ("Could not load black texture due to: " + error)
        OpenGL.Hl.Assert ()

        // create brdf texture
        let brdfTexture =
            match OpenGL.Texture.TryCreateTextureGl (false, OpenGL.TextureMinFilter.Nearest, OpenGL.TextureMagFilter.Nearest, false, false, "Assets/Default/Brdf.bmp") with
            | Right (metadata, textureId) ->
                let textureHandle = OpenGL.Texture.CreateTextureHandle textureId
                OpenGL.Texture.EagerTexture { TextureMetadata = metadata; TextureId = textureId; TextureHandle = textureHandle }
            | Left error -> failwith ("Could not load brdf texture due to: " + error)
        OpenGL.Hl.Assert ()

        // create default irradiance map
        let irradianceMap = OpenGL.LightMap.CreateIrradianceMap (Constants.Render.IrradianceMapResolution, irradianceShader, cubeMapSurface)
        OpenGL.Hl.Assert ()

        // create default environment filter map
        let environmentFilterMap = OpenGL.LightMap.CreateEnvironmentFilterMap (Constants.Render.EnvironmentFilterResolution, environmentFilterShader, cubeMapSurface)
        OpenGL.Hl.Assert ()

        // get albedo metadata and texture
        let albedoTexture =
            match OpenGL.Texture.TryCreateTextureGl (false, OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true, true, "Assets/Default/MaterialAlbedo.dds") with
            | Right (metadata, textureId) ->
                let textureHandle = OpenGL.Texture.CreateTextureHandle textureId
                OpenGL.Texture.EagerTexture { TextureMetadata = metadata; TextureId = textureId; TextureHandle = textureHandle }
            | Left error -> failwith ("Could not load albedo material texture due to: " + error)
        OpenGL.Hl.Assert ()

        // create default physically-based material
        let physicallyBasedMaterial : OpenGL.PhysicallyBased.PhysicallyBasedMaterial =
            let roughnessTexture =
                match OpenGL.Texture.TryCreateTextureGl (false, OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true, true, "Assets/Default/MaterialRoughness.dds") with
                | Right (metadata, textureId) ->
                    let textureHandle = OpenGL.Texture.CreateTextureHandle textureId
                    OpenGL.Texture.EagerTexture { TextureMetadata = metadata; TextureId = textureId; TextureHandle = textureHandle }
                | Left error -> failwith ("Could not load material roughness texture due to: " + error)
            let metallicTexture =
                match OpenGL.Texture.TryCreateTextureGl (false, OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true, true, "Assets/Default/MaterialMetallic.dds") with
                | Right (metadata, textureId) ->
                    let textureHandle = OpenGL.Texture.CreateTextureHandle textureId
                    OpenGL.Texture.EagerTexture { TextureMetadata = metadata; TextureId = textureId; TextureHandle = textureHandle }
                | Left error -> failwith ("Could not load material metallic texture due to: " + error)
            let ambientOcclusionTexture =
                match OpenGL.Texture.TryCreateTextureGl (false, OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true, true, "Assets/Default/MaterialAmbientOcclusion.dds") with
                | Right (metadata, textureId) ->
                    let textureHandle = OpenGL.Texture.CreateTextureHandle textureId
                    OpenGL.Texture.EagerTexture { TextureMetadata = metadata; TextureId = textureId; TextureHandle = textureHandle }
                | Left error -> failwith ("Could not load material ambient occlusion texture due to: " + error)
            let emissionTexture =
                match OpenGL.Texture.TryCreateTextureGl (false, OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true, true, "Assets/Default/MaterialEmission.dds") with
                | Right (metadata, textureId) ->
                    let textureHandle = OpenGL.Texture.CreateTextureHandle textureId
                    OpenGL.Texture.EagerTexture { TextureMetadata = metadata; TextureId = textureId; TextureHandle = textureHandle }
                | Left error -> failwith ("Could not load material emission texture due to: " + error)
            let normalTexture =
                match OpenGL.Texture.TryCreateTextureGl (false, OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true, false, "Assets/Default/MaterialNormal.dds") with
                | Right (metadata, textureId) ->
                    let textureHandle = OpenGL.Texture.CreateTextureHandle textureId
                    OpenGL.Texture.EagerTexture { TextureMetadata = metadata; TextureId = textureId; TextureHandle = textureHandle }
                | Left error -> failwith ("Could not load material normal texture due to: " + error)
            let heightTexture =
                match OpenGL.Texture.TryCreateTextureGl (false, OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true, true, "Assets/Default/MaterialHeight.dds") with
                | Right (metadata, textureId) ->
                    let textureHandle = OpenGL.Texture.CreateTextureHandle textureId
                    OpenGL.Texture.EagerTexture { TextureMetadata = metadata; TextureId = textureId; TextureHandle = textureHandle }
                | Left error -> failwith ("Could not load material height texture due to: " + error)
            { AlbedoTexture = albedoTexture
              RoughnessTexture = roughnessTexture
              MetallicTexture = metallicTexture
              AmbientOcclusionTexture = ambientOcclusionTexture
              EmissionTexture = emissionTexture
              NormalTexture = normalTexture
              HeightTexture = heightTexture
              TwoSided = false }

        // create forward surfaces comparer
        let forwardSurfacesComparer =
            { new IComparer<struct (single * single * Matrix4x4 * Presence * Box2 * MaterialProperties * OpenGL.PhysicallyBased.PhysicallyBasedSurface * single)> with
                member this.Compare ((subsort, sort, _, _, _, _, _, distanceSquared), (subsort2, sort2, _, _, _, _, _, distanceSquared2)) =
                    let sc = sort.CompareTo sort2
                    if sc <> 0 then sc
                    else
                        let dsc = distanceSquared.CompareTo distanceSquared2
                        if dsc <> 0 then -dsc // negated to draw furthest to nearest
                        else
                            let ssc = subsort.CompareTo subsort2
                            ssc }

        // create render tasks
        let renderTasksDictionary =
            dictPlus RenderPass.comparer [(NormalPass, RenderTasks.make ())]

        // make renderer
        let renderer =
            { Window = window
              LazyTextureQueues = lazyTextureQueues
              LazyTextureServer = lazyTextureServer
              SkyBoxShader = skyBoxShader
              IrradianceShader = irradianceShader
              EnvironmentFilterShader = environmentFilterShader
              FilterBox1dShader = filterBox1dShader
              FilterGaussian2dShader = filterGaussian2dShader
              FilterFxaaShader = filterFxaaShader
              PhysicallyBasedShadowStaticShader = shadowStaticShader
              PhysicallyBasedShadowAnimatedShader = shadowAnimatedShader
              PhysicallyBasedShadowTerrainShader = shadowTerrainShader
              PhysicallyBasedDeferredStaticShader = deferredStaticShader
              PhysicallyBasedDeferredAnimatedShader = deferredAnimatedShader
              PhysicallyBasedDeferredTerrainShader = deferredTerrainShader
              PhysicallyBasedDeferredLightMappingShader = deferredLightMappingShader
              PhysicallyBasedDeferredIrradianceShader = deferredIrradianceShader
              PhysicallyBasedDeferredEnvironmentFilterShader = deferredEnvironmentFilterShader
              PhysicallyBasedDeferredSsaoShader = deferredSsaoShader
              PhysicallyBasedDeferredLightingShader = deferredLightingShader
              PhysicallyBasedForwardStaticShader = forwardStaticShader
              ShadowBuffersArray = shadowBuffersArray
              ShadowBuffers2Array = shadowBuffers2Array
              ShadowMatrices = shadowMatrices
              ShadowIndices = shadowIndices
              GeometryBuffers = geometryBuffers
              LightMappingBuffers = lightMappingBuffers
              IrradianceBuffers = irradianceBuffers
              EnvironmentFilterBuffers = environmentFilterBuffers
              SsaoBuffersUnfiltered = ssaoBuffersUnfiltered
              SsaoBuffersFiltered = ssaoBuffersFiltered
              FilterBuffers = filterBuffers
              CubeMapGeometry = cubeMapGeometry
              BillboardGeometry = billboardGeometry
              PhysicallyBasedQuad = physicallyBasedQuad
              PhysicallyBasedTerrainGeometries = Dictionary HashIdentity.Structural
              PhysicallyBasedTerrainGeometriesUtilized = HashSet HashIdentity.Structural
              CubeMap = cubeMapSurface.CubeMap
              WhiteTexture = whiteTexture
              BlackTexture = blackTexture
              BrdfTexture = brdfTexture
              IrradianceMap = irradianceMap
              EnvironmentFilterMap = environmentFilterMap
              PhysicallyBasedMaterial = physicallyBasedMaterial
              LightMaps = dictPlus HashIdentity.Structural []
              LightingConfig = LightingConfig.defaultConfig
              InstanceFields = Array.zeroCreate<single> (Constants.Render.InstanceFieldCount * Constants.Render.InstanceBatchPrealloc)
              UserDefinedStaticModelFields = [||]
              LightsDesiringShadows = dictPlus HashIdentity.Structural []
              ForwardSurfacesComparer = forwardSurfacesComparer
              ForwardSurfacesSortBuffer = List ()
              RenderTasksDictionary = renderTasksDictionary
              RenderPackages = dictPlus StringComparer.Ordinal []
              RenderPackageCachedOpt = Unchecked.defaultof<_>
              RenderAssetCached = { CachedAssetTagOpt = Unchecked.defaultof<_>; CachedRenderAsset = Unchecked.defaultof<_> }
              ReloadAssetsRequested = false
              RenderMessages = List () }

        // fin
        renderer

    interface Renderer3d with

        member renderer.Render frustumInterior frustumExterior frustumImposter lightBox eyeCenter eyeRotation windowSize renderMessages =
            OpenGL.Hl.ResetDrawCalls ()
            if renderMessages.Count > 0 then
                GlRenderer3d.render frustumInterior frustumExterior frustumImposter lightBox eyeCenter eyeRotation windowSize 0u 0u renderMessages renderer

        member renderer.Swap () =
            match renderer.Window with
            | SglWindow window -> SDL.SDL_GL_SwapWindow window.SglWindow

        member renderer.CleanUp () =
            OpenGL.Gl.DeleteProgram renderer.SkyBoxShader.SkyBoxShader
            OpenGL.Gl.DeleteProgram renderer.IrradianceShader.CubeMapShader
            OpenGL.Gl.DeleteProgram renderer.EnvironmentFilterShader.EnvironmentFilterShader
            OpenGL.Gl.DeleteProgram renderer.PhysicallyBasedDeferredStaticShader.PhysicallyBasedShader
            OpenGL.Gl.DeleteProgram renderer.PhysicallyBasedDeferredAnimatedShader.PhysicallyBasedShader
            OpenGL.Gl.DeleteProgram renderer.PhysicallyBasedDeferredTerrainShader.PhysicallyBasedShader
            OpenGL.Gl.DeleteProgram renderer.PhysicallyBasedDeferredLightMappingShader.PhysicallyBasedDeferredLightMappingShader
            OpenGL.Gl.DeleteProgram renderer.PhysicallyBasedDeferredIrradianceShader.PhysicallyBasedDeferredIrradianceShader
            OpenGL.Gl.DeleteProgram renderer.PhysicallyBasedDeferredEnvironmentFilterShader.PhysicallyBasedDeferredEnvironmentFilterShader
            OpenGL.Gl.DeleteProgram renderer.PhysicallyBasedDeferredSsaoShader.PhysicallyBasedDeferredSsaoShader
            OpenGL.Gl.DeleteProgram renderer.PhysicallyBasedDeferredLightingShader.PhysicallyBasedDeferredLightingShader
            OpenGL.Gl.DeleteProgram renderer.PhysicallyBasedForwardStaticShader.PhysicallyBasedShader
            OpenGL.Gl.DeleteProgram renderer.FilterBox1dShader.FilterBoxShader
            OpenGL.Gl.DeleteProgram renderer.FilterGaussian2dShader.FilterGaussianShader
            OpenGL.Gl.DeleteProgram renderer.FilterFxaaShader.FilterFxaaShader
            OpenGL.CubeMap.DestroyCubeMapGeometry renderer.CubeMapGeometry
            OpenGL.PhysicallyBased.DestroyPhysicallyBasedGeometry renderer.BillboardGeometry
            OpenGL.PhysicallyBased.DestroyPhysicallyBasedGeometry renderer.PhysicallyBasedQuad
            renderer.CubeMap.Destroy ()
            renderer.BrdfTexture.Destroy ()
            renderer.IrradianceMap.Destroy ()
            renderer.EnvironmentFilterMap.Destroy ()
            renderer.PhysicallyBasedMaterial.AlbedoTexture.Destroy ()
            renderer.PhysicallyBasedMaterial.RoughnessTexture.Destroy ()
            renderer.PhysicallyBasedMaterial.MetallicTexture.Destroy ()
            renderer.PhysicallyBasedMaterial.AmbientOcclusionTexture.Destroy ()
            renderer.PhysicallyBasedMaterial.EmissionTexture.Destroy ()
            renderer.PhysicallyBasedMaterial.NormalTexture.Destroy ()
            renderer.PhysicallyBasedMaterial.HeightTexture.Destroy ()
            for lightMap in renderer.LightMaps.Values do OpenGL.LightMap.DestroyLightMap lightMap
            renderer.LightMaps.Clear ()
            let renderPackages = renderer.RenderPackages |> Seq.map (fun entry -> entry.Value)
            let renderAssets = renderPackages |> Seq.map (fun package -> package.Assets.Values) |> Seq.concat
            for (_, _, asset) in renderAssets do GlRenderer3d.freeRenderAsset asset renderer
            renderer.RenderPackages.Clear ()
            OpenGL.Framebuffer.DestroyGeometryBuffers renderer.GeometryBuffers
            OpenGL.Framebuffer.DestroyLightMappingBuffers renderer.LightMappingBuffers
            OpenGL.Framebuffer.DestroyIrradianceBuffers renderer.IrradianceBuffers
            OpenGL.Framebuffer.DestroyEnvironmentFilterBuffers renderer.EnvironmentFilterBuffers
            OpenGL.Framebuffer.DestroySsaoBuffers renderer.SsaoBuffersUnfiltered
            OpenGL.Framebuffer.DestroySsaoBuffers renderer.SsaoBuffersFiltered
            OpenGL.Framebuffer.DestroyFilterBuffers renderer.FilterBuffers
            for shadowBuffers in renderer.ShadowBuffersArray do OpenGL.Framebuffer.DestroyShadowBuffers shadowBuffers
            for shadowBuffers2 in renderer.ShadowBuffers2Array do OpenGL.Framebuffer.DestroyShadowBuffers shadowBuffers2
            renderer.LazyTextureServer.Terminate ()