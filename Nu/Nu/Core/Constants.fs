﻿// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Constants
open System
open System.Configuration
open System.Diagnostics
open System.Numerics
open Prime
open Nu

module OpenGL =

    let [<Literal>] VersionMajor = 4
    let [<Literal>] VersionMinor = 1
    let [<Uniform>] GlslVersionPragma = "#version " + string VersionMajor + string VersionMinor + "0"
    let [<Literal>] UncompressedTextureFormat = OpenGL.InternalFormat.Rgba8
    let [<Literal>] BlockCompressedTextureFormat = OpenGL.InternalFormat.CompressedRgbaS3tcDxt5Ext
    let [<Uniform>] mutable HlAssert = match ConfigurationManager.AppSettings.["HlAssert"] with null -> false | hlAssert -> scvalue hlAssert

[<RequireQualifiedAccess>]
module Assimp =

    let [<Literal>] PostProcessSteps = Assimp.PostProcessSteps.Triangulate ||| Assimp.PostProcessSteps.GlobalScale
    let [<Literal>] RawPropertyPrefix = "$raw."
    let [<Literal>] RenderStylePropertyName = RawPropertyPrefix + "RenderStyle"
    let [<Literal>] PresencePropertyName = RawPropertyPrefix + "Presence"
    let [<Literal>] IgnoreLightMapsPropertyName = RawPropertyPrefix + "IgnoreLightMaps"
    let [<Literal>] OpaqueDistancePropertyName = RawPropertyPrefix + "OpaqueDistance"
    let [<Literal>] TwoSidedPropertyName = RawPropertyPrefix + "TwoSided"
    let [<Literal>] NavShapePropertyName = RawPropertyPrefix + "NavShape"

[<RequireQualifiedAccess>]
module Engine =

    let [<Literal>] ExitCodeSuccess = 0
    let [<Literal>] ExitCodeFailure = 1
    let [<Uniform>] mutable RunSynchronously = match ConfigurationManager.AppSettings.["RunSynchronously"] with null -> false | size -> scvalue size
    let [<Uniform>] mutable Meter2d = match ConfigurationManager.AppSettings.["Meter2d"] with null -> 32.0f | centered -> scvalue centered
    let [<Literal>] GameSortPriority = Single.MaxValue
    let [<Uniform>] ScreenSortPriority = GameSortPriority - 1.0f
    let [<Uniform>] GroupSortPriority = ScreenSortPriority - 1.0f
    let [<Uniform>] EntitySortPriority = GroupSortPriority - 1.0f
    let [<Literal>] NamePropertyName = "Name"
    let [<Literal>] SurnamesPropertyName = "Surnames"
    let [<Literal>] GameName = "Game"
    let [<Literal>] DispatcherPropertyName = "Dispatcher"
    let [<Literal>] PropertiesPropertyName = "Properties"
    let [<Literal>] RequiredDispatcherPropertyName = "RequiredDispatcher"
    let [<Literal>] FacetsPropertyName = "Facets"
    let [<Literal>] OverlayNameOptPropertyName = "OverlayNameOpt"
    let [<Literal>] DispatcherNamePropertyName = "DispatcherName"
    let [<Literal>] FacetNamesPropertyName = "FacetNames"
    let [<Literal>] MountOptPropertyName = "MountOpt"
    let [<Literal>] PropagationSourceOptPropertyName = "PropagationSourceOpt"
    let [<Literal>] PropagatedDescriptorOptPropertyName = "PropagatedDescriptorOpt"
    let [<Literal>] ModelPropertyName = "Model"
    let [<Literal>] TransformPropertyName = "Transform"
    let [<Literal>] BoundsPropertyName = "Bounds"
    let [<Literal>] XtensionPropertyName = "Xtension"
    let [<Literal>] BodyTypePropertyName = "BodyType"
    let [<Literal>] PhysicsMotionPropertyName = "PhysicsMotion"
    let [<Literal>] EffectNameDefault = "Effect"
    let [<Literal>] RefinementDir = "refinement"
    let [<Uniform>] Entity2dSizeDefault = Vector3 (32.0f, 32.0f, 0.0f)
    let [<Uniform>] mutable Entity2dPerimeterCenteredDefault = match ConfigurationManager.AppSettings.["Entity2dPerimeterCenteredDefault"] with null -> true | centered -> scvalue centered
    let [<Uniform>] EntityGuiSizeDefault = Vector3 (128.0f, 32.0f, 0.0f)
    let [<Uniform>] mutable EntityGuiPerimeterCenteredDefault = match ConfigurationManager.AppSettings.["EntityGuiPerimeterCenteredDefault"] with null -> true | centered -> scvalue centered
    let [<Uniform>] Entity3dSizeDefault = Vector3 (1.0f, 1.0f, 1.0f)
    let [<Uniform>] EntityVuiSizeDefault = Vector3 (4.0f, 1.0f, 1.0f)
    let [<Uniform>] Particle2dSizeDefault = Vector3 (4.0f, 4.0f, 0.0f)
    let [<Uniform>] Particle3dSizeDefault = Vector3 (0.25f, 0.25f, 0.25f)
    let [<Uniform>] BodyJoint2dSizeDefault = Vector3 (16.0f, 16.0f, 0.0f)
    let [<Uniform>] BodyJoint3dSizeDefault = Vector3 (0.25f, 0.25f, 0.25f)
    let [<Literal>] ParticleShadowOffsetDefault = 0.15f
    let [<Literal>] BillboardShadowOffsetDefault = 0.6f
    let [<Uniform>] Eye3dCenterDefault = Vector3 (0.0f, 0.0f, 2.0f)
    let [<Uniform>] Eye3dFieldOfViewDefault = single (Math.PI / 3.0)
    let [<Uniform>] mutable QuadnodeSize = match ConfigurationManager.AppSettings.["QuadnodeSize"] with null -> 512.0f | size -> scvalue size
    let [<Uniform>] mutable QuadtreeDepth = match ConfigurationManager.AppSettings.["QuadtreeDepth"] with null -> 8 | depth -> scvalue depth
    let [<Uniform>] QuadtreeSize = Vector2 (QuadnodeSize * single (pown 2 QuadtreeDepth))
    let [<Uniform>] mutable OctnodeSize = match ConfigurationManager.AppSettings.["OctnodeSize"] with null -> 8.0f | size -> scvalue size
    let [<Uniform>] mutable OctreeDepth = match ConfigurationManager.AppSettings.["OctreeDepth"] with null -> 8 | depth -> scvalue depth
    let [<Uniform>] OctreeSize = Vector3 (OctnodeSize * single (pown 2 OctreeDepth))
    let [<Uniform>] mutable EventTracing = match ConfigurationManager.AppSettings.["EventTracing"] with null -> false | tracing -> scvalue tracing
    let [<Uniform>] mutable EventFilter = match ConfigurationManager.AppSettings.["EventFilter"] with null -> Pass | filter -> scvalue filter
    let [<Uniform>] TickDeltaMax = 1.0 / 10.0 * double Stopwatch.Frequency |> int64

[<RequireQualifiedAccess>]
module Render =

    let [<Uniform>] VendorNamesExceptedFromSwapGlFinishRequirement = ["NVIDIA Corporation"; "AMD"; "ATI Technologies Inc."] // see https://github.com/bryanedds/Nu/wiki/Why-glFinish-for-Some-Drivers-or-Vendors
    let [<Literal>] IgnoreLightMapsName = "IgnoreLightMaps"
    let [<Literal>] OpaqueDistanceName = "OpaqueDistance"
    let [<Literal>] TwoSidedName = "TwoSided"
    let [<Literal>] NavShapeName = "NavShape"
    let [<Uniform>] mutable Vsync = match ConfigurationManager.AppSettings.["Vsync"] with null -> true | vsync -> scvalue vsync
    let [<Uniform>] mutable NearPlaneDistanceInterior = match ConfigurationManager.AppSettings.["NearPlaneDistanceInterior"] with null -> 0.125f | distance -> scvalue distance
    let [<Uniform>] mutable FarPlaneDistanceInterior = match ConfigurationManager.AppSettings.["FarPlaneDistanceInterior"] with null -> 16.0f | scalar -> scvalue scalar
    let [<Uniform>] mutable NearPlaneDistanceExterior = match ConfigurationManager.AppSettings.["NearPlaneDistanceExterior"] with null -> 16.0f | distance -> scvalue distance
    let [<Uniform>] mutable FarPlaneDistanceExterior = match ConfigurationManager.AppSettings.["FarPlaneDistanceExterior"] with null -> 256.0f | distance -> scvalue distance
    let [<Uniform>] mutable NearPlaneDistanceImposter = match ConfigurationManager.AppSettings.["NearPlaneDistanceImposter"] with null -> 256.0f | distance -> scvalue distance
    let [<Uniform>] mutable FarPlaneDistanceImposter = match ConfigurationManager.AppSettings.["FarPlaneDistanceImposter"] with null -> 4096.0f | distance -> scvalue distance
    let [<Uniform>] mutable NearPlaneDistanceOmnipresent = NearPlaneDistanceInterior
    let [<Uniform>] mutable FarPlaneDistanceOmnipresent = FarPlaneDistanceImposter
    let [<Uniform>] mutable DisplayVirtualResolution = match ConfigurationManager.AppSettings.["DisplayVirtualResolution"] with null -> v2i 640 360 | resolution -> scvalue resolution
    let [<Uniform>] mutable SsaoResolutionDivisor = match ConfigurationManager.AppSettings.["SsaoResolutionDivisor"] with null -> 1 | divisor -> scvalue divisor
    let [<Uniform>] Play3dBoxSize = Vector3 64.0f
    let [<Uniform>] Light3dBoxSize = Vector3 64.0f
    let [<Uniform>] WindowClearColor = Color.Zero
    let [<Uniform>] ViewportClearColor = Color.Zero // NOTE: do not change this color as the deferred lighting shader checks if position.w zero to ignore fragment.
    let [<Literal>] TexturePriorityDefault = 0.5f // higher priority than (supposed) default, but not maximum. this value is arrived at through experimenting with a Windows NVidia driver.
    let [<Uniform>] mutable TextureAnisotropyMax = match ConfigurationManager.AppSettings.["TextureAnisotropyMax"] with null -> 16.0f | anisoMax -> scvalue anisoMax
    let [<Uniform>] mutable TextureMinimalMipmapIndex = match ConfigurationManager.AppSettings.["TextureMinimalMipmapIndex"] with null -> 2 | index -> scvalue index
    let [<Literal>] SpriteBatchSize = 192 // NOTE: remember to update SPRITE_BATCH_SIZE in shaders when changing this!
    let [<Literal>] SpriteBorderTexelScalar = 0.005f
    let [<Literal>] SpriteMessagesPrealloc = 256
    let [<Literal>] StaticModelMessagesPrealloc = 256
    let [<Literal>] StaticModelSurfaceMessagesPrealloc = 256
    let [<Literal>] BonesMax = 128
    let [<Literal>] BonesInfluenceMax = 4
    let [<Literal>] AnimatedModelRateScalar = 30.0f // some arbitrary scale that mixamo fbx exported from blender seems to like...
    let [<Literal>] AnimatedModelMessagesPrealloc = 128
    let [<Literal>] InstanceFieldCount = 32
    let [<Literal>] InstanceBatchPrealloc = 1024
    let [<Literal>] TerrainLayersMax = 8
    let [<Literal>] BrdfResolution = 256 // NOTE: half typical resolution because we use 32-bit floats instead of 16-bit.
    let [<Literal>] BrdfSamples = 1024
    let [<Literal>] LightMapsMaxDeferred = 32
    let [<Literal>] LightMapsMaxForward = 2
    let [<Literal>] LightsMaxDeferred = 32
    let [<Literal>] LightsMaxForward = 8
    let [<Uniform>] mutable ShadowVirtualResolution = match ConfigurationManager.AppSettings.["ShadowVirtualResolution"] with null -> 128 | scalar -> scvalue scalar
    let [<Literal>] ShadowTexturesMaxShader = 16 // NOTE: remember to update SHADOW_TEXTURES_MAX in shaders when changing this!
    let [<Literal>] ShadowMapsMaxShader = 8 // NOTE: remember to update SHADOW_TEXTURES_MAX in shaders when changing this!
    let [<Uniform>] mutable ShadowTexturesMax = match ConfigurationManager.AppSettings.["ShadowTexturesMax"] with null -> 16 | shadowTexturesMax -> min (scvalue shadowTexturesMax) ShadowTexturesMaxShader
    let [<Uniform>] mutable ShadowMapsMax = match ConfigurationManager.AppSettings.["ShadowMapsMax"] with null -> 8 | shadowMapsMax -> min (scvalue shadowMapsMax) ShadowMapsMaxShader
    let [<Uniform>] mutable ShadowDetailedResolutionScalar = match ConfigurationManager.AppSettings.["ShadowDetailedResolutionScalar"] with null -> 2 | scalar -> scvalue scalar
    let [<Literal>] ShadowFovMax = 2.1f // NOTE: remember to update SHADOW_FOV_MAX in shaders when changing this!
    let [<Literal>] ReflectionMapResolution = 1024
    let [<Literal>] IrradianceMapResolution = 32
    let [<Literal>] EnvironmentFilterResolution = 512
    let [<Literal>] EnvironmentFilterMips = 7 // NOTE: changing this requires changing the REFLECTION_LOD_MAX constants in shader code.
    let [<Literal>] LightMappingEnabledDefault = true
    let [<Literal>] LightCutoffMarginDefault = 0.333f
    let [<Literal>] LightShadowSamplesDefault = 3
    let [<Literal>] LightShadowBiasDefault = 0.005f
    let [<Literal>] LightShadowSampleScalarDefault = 0.01f
    let [<Literal>] LightShadowExponentDefault = 80.0f
    let [<Literal>] LightShadowDensityDefault = 12.0f
    let [<Literal>] SsaoEnabledDefault = true
    let [<Literal>] SsaoSampleCountDefault = 16
    let [<Literal>] SsaoSampleCountMax = 128
    let [<Literal>] SsaoIntensityDefault = 1.5f
    let [<Literal>] SsaoBiasDefault = 0.025f
    let [<Literal>] SsaoRadiusDefault = 0.125f
    let [<Literal>] SsaoDistanceMaxDefault = 0.125f
    let [<Literal>] SsvfEnabledGlobalDefault = false
    let [<Literal>] SsvfEnabledLocalDefault = true
    let [<Literal>] SsvfStepsDefault = 24
    let [<Literal>] SsvfAsymmetryDefault = 0.5f
    let [<Literal>] SsvfIntensityDefault = 0.5f
    let [<Literal>] SsrEnabledGlobalDefault = false
    let [<Literal>] SsrEnabledLocalDefault = true
    let [<Literal>] SsrDetailDefault = 0.21f
    let [<Literal>] SsrRefinementsMaxDefault = 24
    let [<Literal>] SsrRayThicknessDefault = 0.05f
    let [<Literal>] SsrTowardEyeCutoffDefault = 0.9f
    let [<Literal>] SsrDepthCutoffDefault = 24.0f
    let [<Literal>] SsrDepthCutoffMarginDefault = 0.2f
    let [<Literal>] SsrDistanceCutoffDefault = 16.0f
    let [<Literal>] SsrDistanceCutoffMarginDefault = 0.2f
    let [<Literal>] SsrRoughnessCutoffDefault = 0.3f
    let [<Literal>] SsrRoughnessCutoffMarginDefault = 0.3f
    let [<Literal>] SsrSlopeCutoffDefault = 0.1f
    let [<Literal>] SsrSlopeCutoffMarginDefault = 0.2f
    let [<Literal>] SsrEdgeHorizontalMarginDefault = 0.05f
    let [<Literal>] SsrEdgeVerticalMarginDefault = 0.2f
    let [<Uniform>] SsrLightColorDefault = Color.White
    let [<Uniform>] SsrLightBrightnessDefault = 1.0f
    let [<Literal>] FxaaEnabledDefault = true
    let [<Literal>] LightProbeSizeDefault = 3.0f
    let [<Literal>] BrightnessDefault = 3.0f
    let [<Literal>] LightCutoffDefault = 3.0f
    let [<Uniform>] AttenuationLinearDefault = 1.0f / (BrightnessDefault * LightCutoffDefault)
    let [<Uniform>] AttenuationQuadraticDefault = 1.0f / (BrightnessDefault * pown LightCutoffDefault 2)
    let [<Uniform>] AlbedoDefault = Color.White
    let [<Uniform>] RoughnessDefault = 1.0f
    let [<Literal>] MetallicDefault = 1.0f
    let [<Literal>] AmbientOcclusionDefault = 1.0f
    let [<Literal>] EmissionDefault = 1.0f
    let [<Literal>] HeightDefault = 1.0f
    let [<Literal>] IgnoreLightMapsDefault = false
    let [<Literal>] OpaqueDistanceDefault = 100000.0f
    let [<Literal>] FontSizeDefault = 14

[<RequireQualifiedAccess>]
module Audio =

    let [<Literal>] MasterAudioVolumeDefault = 1.0f
    let [<Literal>] MasterSoundVolumeDefault = 1.0f
    let [<Literal>] MasterSongVolumeDefault = 1.0f
    let [<Literal>] SoundVolumeDefault = 1.0f
    let [<Literal>] SongVolumeDefault = 1.0f
    let [<Uniform>] FadeOutTimeDefault = GameTime.ofSeconds 0.5f
    let [<Uniform>] SongResumptionMax = GameTime.ofSeconds 90.0f // HACK: prevents songs from starting over too often due to hack in SdlAudioPlayer.playSong.
    let [<Literal>] Frequency = 44100
    let [<Literal>] BufferSizeDefault = 1024
    let [<Literal>] FadeInSecondsMin = 0.1f // NOTE: Mix_PlayMusic seems to sometimes cause audio 'popping' when starting a song, so a minimum fade is used instead.

[<RequireQualifiedAccess>]
module Physics =

    let [<Uniform>] GravityDefault = Vector3 (0.0f, -9.80665f, 0.0f)
    let [<Uniform>] AlwaysObserve = match ConfigurationManager.AppSettings.["AlwaysObserve"] with null -> true | alwaysObserve -> scvalue alwaysObserve
    let [<Literal>] BreakingPointDefault = 100000.0f
    let [<Literal>] CollisionWildcard = "*"
    let [<Uniform>] Collision3dBodiesMax = match ConfigurationManager.AppSettings.["Collision3dBodiesMax"] with null -> 10240 | step -> scvalue step
    let [<Uniform>] Collision3dBodyPairsMax = match ConfigurationManager.AppSettings.["Collision3dBodyPairsMax"] with null -> 65536 | step -> scvalue step
    let [<Uniform>] Collision3dContactConstraintsMax = match ConfigurationManager.AppSettings.["Collision3dContactConstraintsMax"] with null -> 10240 | step -> scvalue step
    let [<Uniform>] Collision3dSteps = match ConfigurationManager.AppSettings.["Collision3dSteps"] with null -> 1 | step -> scvalue step
    let [<Uniform>] Collision3dThreads = match ConfigurationManager.AppSettings.["Collision3dThreads"] with null -> max 1 (Environment.ProcessorCount - 2) | threads -> scvalue threads
    let [<Uniform>] Collision3dBarriersMax = match ConfigurationManager.AppSettings.["Collision3dBarriersMax"] with null -> max 1 (Environment.ProcessorCount - 2) | barriers -> scvalue barriers
    let [<Uniform>] Collision3dJobsMax = match ConfigurationManager.AppSettings.["Collision3dJobsMax"] with null -> 128 | jobs -> scvalue jobs
    let [<Uniform>] GroundAngleMax = single (Math.PI * 0.25)
    let [<Uniform>] BroadPhaseLayerNonMoving = byte 0 // NOTE: do not use this outside of the engine code.
    let [<Uniform>] BroadPhaseLayerMoving = byte 1 // NOTE: do not use this outside of the engine code.
    let [<Uniform>] ObjectLayerNonMoving = JoltPhysicsSharp.ObjectLayer 0us // NOTE: do not use this outside of the engine code.
    let [<Uniform>] ObjectLayerMoving = JoltPhysicsSharp.ObjectLayer 1us // NOTE: do not use this outside of the engine code.
    let [<Literal>] InternalIndex = -1 // NOTE: do not use this outside of the engine code.

[<RequireQualifiedAccess>]
module Lens =

    let [<Literal>] ChangeName = "Change"
    let [<Literal>] EventName = "Event"
    let [<Uniform>] ChangeNameHash = hash ChangeName
    let [<Uniform>] EventNameHash = hash EventName

[<RequireQualifiedAccess>]
module Associations =

    let [<Literal>] Symbol = "Symbol"
    let [<Literal>] Render2d = "Render2d"
    let [<Literal>] Render3d = "Render3d"
    let [<Literal>] Audio = "Audio"

[<RequireQualifiedAccess>]
module Gui =

    let [<Uniform>] mutable SliceMarginDefault = match ConfigurationManager.AppSettings.["SliceMarginDefault"] with null -> Vector2 (4.0f, 4.0f) | marginDefault -> scvalue marginDefault
    let [<Uniform>] ColorDisabledDefault = Color (0.75f, 0.75f, 0.75f, 0.75f)

[<RequireQualifiedAccess>]
module TileMap =

    let [<Literal>] AnimationPropertyName = "A"
    let [<Literal>] CollisionPropertyName = "C"
    let [<Literal>] ElevationPropertyName = "E"
    let [<Literal>] InfoPropertyName = "I"

[<RequireQualifiedAccess>]
module Particles =

    let [<Literal>] RestitutionDefault = 0.9f

[<RequireQualifiedAccess>]
module Effects =

    let [<Literal>] EffectHistoryMaxDefault = 60 // 1 second of effect history @ 60 fps

[<RequireQualifiedAccess>]
module Paths =

    let [<Literal>] SpriteShaderFilePath = "Assets/Default/Sprite.glsl"
    let [<Literal>] SpriteBatchShaderFilePath = "Assets/Default/SpriteBatch.glsl"
    let [<Literal>] SkyBoxShaderFilePath = "Assets/Default/SkyBox.glsl"
    let [<Literal>] IrradianceShaderFilePath = "Assets/Default/Irradiance.glsl"
    let [<Literal>] EnvironmentFilterShaderFilePath = "Assets/Default/EnvironmentFilter.glsl"
    let [<Literal>] FilterBox1dShaderFilePath = "Assets/Default/FilterBox1d.glsl"
    let [<Literal>] FilterGaussian2dShaderFilePath = "Assets/Default/FilterGaussian2d.glsl"
    let [<Literal>] FilterBilateralDownSample4dShaderFilePath = "Assets/Default/FilterBilateralDownSample4d.glsl"
    let [<Literal>] FilterBilateralUpSample4dShaderFilePath = "Assets/Default/FilterBilateralUpSample4d.glsl"
    let [<Literal>] FilterFxaaShaderFilePath = "Assets/Default/FilterFxaa.glsl"
    let [<Literal>] PhysicallyBasedShadowStaticPointShaderFilePath = "Assets/Default/PhysicallyBasedShadowStaticPoint.glsl"
    let [<Literal>] PhysicallyBasedShadowStaticSpotShaderFilePath = "Assets/Default/PhysicallyBasedShadowStaticSpot.glsl"
    let [<Literal>] PhysicallyBasedShadowStaticDirectionalShaderFilePath = "Assets/Default/PhysicallyBasedShadowStaticDirectional.glsl"
    let [<Literal>] PhysicallyBasedShadowAnimatedPointShaderFilePath = "Assets/Default/PhysicallyBasedShadowAnimatedPoint.glsl"
    let [<Literal>] PhysicallyBasedShadowAnimatedSpotShaderFilePath = "Assets/Default/PhysicallyBasedShadowAnimatedSpot.glsl"
    let [<Literal>] PhysicallyBasedShadowAnimatedDirectionalShaderFilePath = "Assets/Default/PhysicallyBasedShadowAnimatedDirectional.glsl"
    let [<Literal>] PhysicallyBasedShadowTerrainPointShaderFilePath = "Assets/Default/PhysicallyBasedShadowTerrainPoint.glsl"
    let [<Literal>] PhysicallyBasedShadowTerrainSpotShaderFilePath = "Assets/Default/PhysicallyBasedShadowTerrainSpot.glsl"
    let [<Literal>] PhysicallyBasedShadowTerrainDirectionalShaderFilePath = "Assets/Default/PhysicallyBasedShadowTerrainDirectional.glsl"
    let [<Literal>] PhysicallyBasedDeferredStaticShaderFilePath = "Assets/Default/PhysicallyBasedDeferredStatic.glsl"
    let [<Literal>] PhysicallyBasedDeferredAnimatedShaderFilePath = "Assets/Default/PhysicallyBasedDeferredAnimated.glsl"
    let [<Literal>] PhysicallyBasedDeferredTerrainShaderFilePath = "Assets/Default/PhysicallyBasedDeferredTerrain.glsl"
    let [<Literal>] PhysicallyBasedDeferredLightMappingShaderFilePath = "Assets/Default/PhysicallyBasedDeferredLightMapping.glsl"
    let [<Literal>] PhysicallyBasedDeferredAmbientShaderFilePath = "Assets/Default/PhysicallyBasedDeferredAmbient.glsl"
    let [<Literal>] PhysicallyBasedDeferredIrradianceShaderFilePath = "Assets/Default/PhysicallyBasedDeferredIrradiance.glsl"
    let [<Literal>] PhysicallyBasedDeferredEnvironmentFilterShaderFilePath = "Assets/Default/PhysicallyBasedDeferredEnvironmentFilter.glsl"
    let [<Literal>] PhysicallyBasedDeferredSsaoShaderFilePath = "Assets/Default/PhysicallyBasedDeferredSsao.glsl"
    let [<Literal>] PhysicallyBasedDeferredLightingShaderFilePath = "Assets/Default/PhysicallyBasedDeferredLighting.glsl"
    let [<Literal>] PhysicallyBasedDeferredCompositionShaderFilePath = "Assets/Default/PhysicallyBasedDeferredComposition.glsl"
    let [<Literal>] PhysicallyBasedForwardStaticShaderFilePath = "Assets/Default/PhysicallyBasedForwardStatic.glsl"