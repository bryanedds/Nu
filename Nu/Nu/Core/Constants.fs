// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu.Constants
open System
open System.Diagnostics
open System.Numerics
open System.Configuration
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

[<RequireQualifiedAccess>]
module Engine =

    let [<Literal>] ExitCodeSuccess = 0
    let [<Literal>] ExitCodeFailure = 1
    let [<Uniform>] mutable RunSynchronously = match ConfigurationManager.AppSettings.["RunSynchronously"] with null -> false | size -> scvalue size
    let [<Literal>] Meter2d = 48.0f
    let [<Literal>] Meter3d = 1.0f
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
    let [<Literal>] FacetNamesPropertyName = "FacetNames"
    let [<Literal>] MountOptPropertyName = "MountOpt"
    let [<Literal>] PropagationSourceOptPropertyName = "PropagationSourceOpt"
    let [<Literal>] PropagatedDescriptorOptPropertyName = "PropagatedDescriptorOpt"
    let [<Literal>] ModelPropertyName = "Model"
    let [<Literal>] TransformPropertyName = "Transform"
    let [<Literal>] BoundsPropertyName = "Bounds"
    let [<Literal>] XtensionPropertyName = "Xtension"
    let [<Literal>] BodyTypePropertyName = "BodyType"
    let [<Literal>] EffectNameDefault = "Effect"
    let [<Literal>] RefinementDir = "Refinement"
    let [<Uniform>] Entity2dSizeDefault = Vector3 (Meter2d, Meter2d, 0.0f)
    let [<Uniform>] EntityGuiSizeDefault = Vector3 (Meter2d * 4.0f, Meter2d, 0.0f)
    let [<Uniform>] Entity3dSizeDefault = Vector3 1.0f
    let [<Uniform>] EntityVuiSizeDefault = Vector3 1.0f
    let [<Uniform>] mutable EntityPerimeterCentered2dDefault = match ConfigurationManager.AppSettings.["EntityPerimeterCentered2dDefault"] with null -> true | centered -> scvalue centered
    let [<Uniform>] mutable EntityPerimeterCenteredGuiDefault = match ConfigurationManager.AppSettings.["EntityPerimeterCenteredGuiDefault"] with null -> true | centered -> scvalue centered
    let [<Uniform>] Particle2dSizeDefault = Vector3 (12.0f, 12.0f, 0.0f)
    let [<Uniform>] Particle3dSizeDefault = Vector3 (0.1f, 0.1f, 0.1f)
    let [<Uniform>] Eye3dCenterDefault = Vector3 (0.0f, 1.0f, 4.0f)
    let [<Uniform>] mutable QuadnodeSize = match ConfigurationManager.AppSettings.["QuadnodeSize"] with null -> 512.0f | size -> scvalue size
    let [<Uniform>] mutable QuadtreeDepth = match ConfigurationManager.AppSettings.["QuadtreeDepth"] with null -> 7 | depth -> scvalue depth
    let [<Uniform>] QuadtreeSize = Vector2 (QuadnodeSize * single (pown 2 QuadtreeDepth))
    let [<Uniform>] mutable OctnodeSize = match ConfigurationManager.AppSettings.["OctnodeSize"] with null -> 8.0f | size -> scvalue size
    let [<Uniform>] mutable OctreeDepth = match ConfigurationManager.AppSettings.["OctreeDepth"] with null -> 6 | depth -> scvalue depth
    let [<Uniform>] OctreeSize = Vector3 (OctnodeSize * single (pown 2 OctreeDepth))
    let [<Uniform>] mutable EventTracing = match ConfigurationManager.AppSettings.["EventTracing"] with null -> false | tracing -> scvalue tracing
    let [<Uniform>] mutable EventFilter = match ConfigurationManager.AppSettings.["EventFilter"] with null -> Pass | filter -> scvalue filter
    let [<Uniform>] TickDeltaMax = 1.0 / 10.0 * double Stopwatch.Frequency |> int64

[<RequireQualifiedAccess>]
module Render =

    let [<Literal>] IgnoreLightMapsName = "IgnoreLightMaps"
    let [<Literal>] OpaqueDistanceName = "OpaqueDistance"
    let [<Literal>] TwoSidedName = "TwoSided"
    let [<Literal>] NavShapeName = "NavShape"
    let [<Uniform>] mutable Vsync = match ConfigurationManager.AppSettings.["Vsync"] with null -> true | vsync -> scvalue vsync
    let [<Literal>] VirtualResolutionX = 960
    let [<Literal>] VirtualResolutionY = 540
    let [<Uniform>] VirtualResolution = Vector2i (VirtualResolutionX, VirtualResolutionY)
    let [<Uniform>] VirtualResolutionF = Vector2 (single VirtualResolutionX, single VirtualResolutionY)
    let [<Uniform>] VirtualScalar = match ConfigurationManager.AppSettings.["VirtualScalar"] with null -> 2 | scalar -> scvalue scalar
    let [<Uniform>] VirtualScalarF = single VirtualScalar
    let [<Uniform>] VirtualScalar2i = Vector2i VirtualScalar
    let [<Uniform>] VirtualScalar2 = Vector2 (single VirtualScalar2i.X, single VirtualScalar2i.Y)
    let [<Uniform>] Play3dBoxSize = Vector3 64.0f
    let [<Uniform>] Light3dBoxSize = Vector3 64.0f
    let [<Uniform>] mutable NearPlaneDistanceInterior = match ConfigurationManager.AppSettings.["NearPlaneDistanceInterior"] with null -> 0.0625f | scalar -> scvalue scalar
    let [<Uniform>] mutable FarPlaneDistanceInterior = match ConfigurationManager.AppSettings.["FarPlaneDistanceInterior"] with null -> 16.0f (* NOTE: remember to update OPAQUING_DISTANCE in shader when changing this!*) | scalar -> scvalue scalar
    let [<Uniform>] mutable NearPlaneDistanceExterior = match ConfigurationManager.AppSettings.["NearPlaneDistanceExterior"] with null -> FarPlaneDistanceInterior | scalar -> scvalue scalar
    let [<Uniform>] mutable FarPlaneDistanceExterior = match ConfigurationManager.AppSettings.["FarPlaneDistanceExterior"] with null -> 128.0f | scalar -> scvalue scalar
    let [<Uniform>] mutable NearPlaneDistanceImposter = match ConfigurationManager.AppSettings.["NearPlaneDistanceImposter"] with null -> FarPlaneDistanceExterior | scalar -> scvalue scalar
    let [<Uniform>] mutable FarPlaneDistanceImposter = match ConfigurationManager.AppSettings.["FarPlaneDistanceImposter"] with null -> 4096.0f | scalar -> scvalue scalar
    let [<Uniform>] NearPlaneDistanceOmnipresent = NearPlaneDistanceInterior
    let [<Uniform>] FarPlaneDistanceOmnipresent = FarPlaneDistanceImposter
    let [<Uniform>] ResolutionX = VirtualResolutionX * VirtualScalar
    let [<Uniform>] ResolutionY = VirtualResolutionY * VirtualScalar
    let [<Uniform>] ResolutionF = Vector2 (single ResolutionX, single ResolutionY)
    let [<Uniform>] Resolution = Vector2i (ResolutionX, ResolutionY)
    let [<Uniform>] FieldOfView = single (Math.PI / 3.0) // 60 degrees
    let [<Uniform>] ViewportMargin (windowSize : Vector2i) = let size = Vector2i (ResolutionX, ResolutionY) in Vector2i ((windowSize.X - size.X) / 2, (windowSize.Y - size.Y) / 2)
    let [<Uniform>] ViewportOffset (windowSize : Vector2i) = Viewport (NearPlaneDistanceOmnipresent, FarPlaneDistanceOmnipresent, Box2i(ViewportMargin windowSize, Resolution))
    let [<Uniform>] Viewport = Viewport (NearPlaneDistanceOmnipresent, FarPlaneDistanceOmnipresent, Box2i (v2iZero, Resolution))
    let [<Uniform>] ShadowResolutionX = 512 * VirtualScalar
    let [<Uniform>] ShadowResolutionY = 512 * VirtualScalar
    let [<Uniform>] ShadowResolutionF = Vector2 (single ShadowResolutionX, single ShadowResolutionY)
    let [<Uniform>] ShadowResolution = Vector2i (ShadowResolutionX, ShadowResolutionY)
    let [<Uniform>] mutable SsaoResolutionDivisor = match ConfigurationManager.AppSettings.["SsaoResolutionDivisor"] with null -> 1 | ssaoResolutionDivisor -> scvalue ssaoResolutionDivisor
    let [<Uniform>] SsaoResolutionX = ResolutionX / SsaoResolutionDivisor
    let [<Uniform>] SsaoResolutionY = ResolutionY / SsaoResolutionDivisor
    let [<Uniform>] SsaoResolutionF = Vector2 (single SsaoResolutionX, single SsaoResolutionY)
    let [<Uniform>] SsaoResolution = Vector2i (SsaoResolutionX, SsaoResolutionY)
    let [<Uniform>] SsaoViewport = Nu.Viewport (NearPlaneDistanceOmnipresent, FarPlaneDistanceOmnipresent, Box2i (v2iZero, SsaoResolution))
    let [<Uniform>] WindowClearColor = Color.Zero
    let [<Uniform>] ViewportClearColor = Color.White // NOTE: do not change this color as the deferred lighting shader checks if normal color is equal to [1;1;1] to discard fragment.
    let [<Literal>] TexturePriorityDefault = 0.5f // higher priority than (supposed) default, but not maximum. this value is arrived at through experimenting with a Windows NVidia driver.
    let [<Literal>] TextureAnisotropyMax = 8.0f
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
    let [<Literal>] LightMapsMaxDeferred = 32
    let [<Literal>] LightMapsMaxForward = 2
    let [<Literal>] LightsMaxDeferred = 64
    let [<Literal>] LightsMaxForward = 8
    let [<Uniform>] mutable ShadowDetailedCount = match ConfigurationManager.AppSettings.["ShadowDetailedCount"] with null -> 1 | scalar -> scvalue scalar
    let [<Uniform>] mutable ShadowDetailedResolutionScalar = match ConfigurationManager.AppSettings.["ShadowDetailedResolutionScalar"] with null -> 2 | scalar -> scvalue scalar
    let [<Literal>] ShadowFovMax = 2.1f // NOTE: remember to update SHADOW_FOV_MAX in shaders when changing this!
    let [<Literal>] ShadowsMaxShader = 16 // NOTE: remember to update SHADOWS_MAX in shaders when changing this!
    let [<Uniform>] mutable ShadowsMax = match ConfigurationManager.AppSettings.["ShadowsMax"] with null -> 8 | shadowsMax -> min (scvalue shadowsMax) ShadowsMaxShader
    let [<Literal>] ReflectionMapResolution = 1024
    let [<Literal>] IrradianceMapResolution = 32
    let [<Literal>] EnvironmentFilterResolution = 512
    let [<Literal>] EnvironmentFilterMips = 7 // NOTE: changing this requires changing the REFLECTION_LOD_MAX constants in shader code.
    let [<Literal>] LightCutoffMarginDefault = 0.333f
    let [<Literal>] LightShadowBiasAcneDefault = 0.0000002f
    let [<Literal>] LightShadowBiasBleedDefault = 0.5f
    let [<Literal>] LightMappingEnabledDefault = true
    let [<Literal>] SsaoEnabledDefault = true
    let [<Literal>] SsaoIntensityDefault = 1.35f
    let [<Literal>] SsaoBiasDefault = 0.025f
    let [<Literal>] SsaoRadiusDefault = 0.125f // 4x this for outside scenes
    let [<Literal>] SsaoDistanceMaxDefault = 0.125f // 2x this for outside scenes
    let [<Literal>] SsaoSampleCountDefault = 16 // 2x this for outside scenes
    let [<Literal>] SsaoSampleCountMax = 128
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
    let [<Literal>] FontSizeDefault = 24

[<RequireQualifiedAccess>]
module Audio =

    let [<Literal>] SongVolumeDefault = 0.5f
    let [<Literal>] SoundVolumeDefault = 1.0f
    let [<Uniform>] FadeOutTimeDefault = GameTime.ofSeconds 0.5f
    let [<Uniform>] SongResumptionMax = GameTime.ofSeconds 90.0f // HACK: prevents songs from starting over too often due to hack in SdlAudioPlayer.playSong.
    let [<Literal>] Frequency = 44100
    let [<Literal>] BufferSizeDefault = 1024
    let [<Literal>] FadeInSecondsMin = 0.1f // NOTE: Mix_PlayMusic seems to sometimes cause audio 'popping' when starting a song, so a minimum fade is used instead.

[<RequireQualifiedAccess>]
module Physics =

    let [<Uniform>] Gravity2dDefault = Vector3 (0.0f, -9.80665f, 0.0f) * Engine.Entity2dSizeDefault.Y
    let [<Uniform>] Gravity3dDefault = Vector3 (0.0f, -9.80665f, 0.0f)
    let [<Literal>] SleepingThresholdLinear = 1.0f // NOTE: in the example or bullet source code (can't remember), this defaulted to 0.8f...
    let [<Literal>] SleepingThresholdAngular = 1.0f // NOTE: ...and this defaulted to 1.0f.
    let [<Literal>] CollisionWildcard = "*"
    let [<Literal>] Collision3dMargin = 0.01f
    let [<Literal>] AllowedCcdPenetration3d = 0.01f // NOTE: seems to also change the smoothness at which character slide.
    let [<Uniform>] GroundAngleMax = single (Math.PI * 0.25)
    let [<Literal>] PhysicsToPixelRatio = Engine.Meter2d // 48 pixels = 1 meter
    let [<Uniform>] PixelToPhysicsRatio = 1.0f / Engine.Meter2d
    let [<Uniform>] ThreadCount = max 1 (Environment.ProcessorCount - 2)
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

    let [<Uniform>] DisabledColor = Color (0.75f, 0.75f, 0.75f, 0.75f)

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
    let [<Literal>] FilterFxaaShaderFilePath = "Assets/Default/FilterFxaa.glsl"
    let [<Literal>] PhysicallyBasedShadowStaticShaderFilePath = "Assets/Default/PhysicallyBasedShadowStatic.glsl"
    let [<Literal>] PhysicallyBasedShadowAnimatedShaderFilePath = "Assets/Default/PhysicallyBasedShadowAnimated.glsl"
    let [<Literal>] PhysicallyBasedShadowTerrainShaderFilePath = "Assets/Default/PhysicallyBasedShadowTerrain.glsl"
    let [<Literal>] PhysicallyBasedDeferredStaticShaderFilePath = "Assets/Default/PhysicallyBasedDeferredStatic.glsl"
    let [<Literal>] PhysicallyBasedDeferredAnimatedShaderFilePath = "Assets/Default/PhysicallyBasedDeferredAnimated.glsl"
    let [<Literal>] PhysicallyBasedDeferredTerrainShaderFilePath = "Assets/Default/PhysicallyBasedDeferredTerrain.glsl"
    let [<Literal>] PhysicallyBasedDeferredLightMappingShaderFilePath = "Assets/Default/PhysicallyBasedDeferredLightMapping.glsl"
    let [<Literal>] PhysicallyBasedDeferredIrradianceShaderFilePath = "Assets/Default/PhysicallyBasedDeferredIrradiance.glsl"
    let [<Literal>] PhysicallyBasedDeferredEnvironmentFilterShaderFilePath = "Assets/Default/PhysicallyBasedDeferredEnvironmentFilter.glsl"
    let [<Literal>] PhysicallyBasedDeferredSsaoShaderFilePath = "Assets/Default/PhysicallyBasedDeferredSsao.glsl"
    let [<Literal>] PhysicallyBasedDeferredLightingShaderFilePath = "Assets/Default/PhysicallyBasedDeferredLighting.glsl"
    let [<Literal>] PhysicallyBasedForwardStaticShaderFilePath = "Assets/Default/PhysicallyBasedForwardStatic.glsl"