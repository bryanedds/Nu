// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Constants
open System
open System.Collections.Frozen
open System.Configuration
open System.Diagnostics
open System.Numerics
open SDL2
open Prime
open Nu

[<RequireQualifiedAccess>]
module Runtime =

    let [<Uniform>] mutable GcDebug = match ConfigurationManager.AppSettings.["GcDebug"] with null -> false | value -> scvalue value

[<RequireQualifiedAccess>]
module Assimp =

    let [<Literal>] PostProcessSteps = Assimp.PostProcessSteps.Triangulate ||| Assimp.PostProcessSteps.GlobalScale
    let [<Literal>] RawPropertyPrefix = "$raw."
    let [<Literal>] RenderStylePropertyName = RawPropertyPrefix + "RenderStyle"
    let [<Literal>] PresencePropertyName = RawPropertyPrefix + "Presence"
    let [<Literal>] IgnoreLightMapsPropertyName = RawPropertyPrefix + "IgnoreLightMaps"
    let [<Literal>] OpaqueDistancePropertyName = RawPropertyPrefix + "OpaqueDistance"
    let [<Literal>] FinenessOffsetPropertyName = RawPropertyPrefix + "FinenessOffset"
    let [<Literal>] ScatterTypePropertyName = RawPropertyPrefix + "ScatterType"
    let [<Literal>] SpecularScalarPropertyName = RawPropertyPrefix + "SpecularScalar"
    let [<Literal>] RefractiveIndexPropertyName = RawPropertyPrefix + "RefractiveIndex"
    let [<Literal>] TwoSidedPropertyName = RawPropertyPrefix + "TwoSided"
    let [<Literal>] ClippedPropertyName = RawPropertyPrefix + "Clipped"
    let [<Literal>] NavShapePropertyName = RawPropertyPrefix + "NavShape"

// NOTE: no qualification specifier here as module exists as an extension.
module OpenGL =

    let [<Literal>] VersionMajor = 4
    let [<Literal>] VersionMinor = 6
    let [<Literal>] Profile = SDL.SDL_GLprofile.SDL_GL_CONTEXT_PROFILE_CORE
    let [<Uniform>] GlslVersionPragma = "#version " + string VersionMajor + string VersionMinor + "0" + " core"
    let [<Literal>] UncompressedTextureFormat = OpenGL.InternalFormat.Rgba8
    let [<Literal>] BlockCompressedTextureFormat = OpenGL.InternalFormat.CompressedRgbaS3tcDxt5Ext
    let [<Literal>] TextureImageUnitsRequired = 32
    let [<Uniform>] mutable HlDebug = match ConfigurationManager.AppSettings.["HlDebug"] with null -> false | value -> scvalue value

[<RequireQualifiedAccess>]
module ImGui =

    let [<Uniform>] mutable FontSize = match ConfigurationManager.AppSettings.["ImGuiFontSize"] with null -> 13.0f | value -> scvalue value

[<RequireQualifiedAccess>]
module Engine =

    let [<Literal>] ExitCodeSuccess = 0
    let [<Literal>] ExitCodeFailure = 1
    let [<Uniform>] mutable Meter2d = match ConfigurationManager.AppSettings.["Meter2d"] with null -> 32.0f | value -> scvalue value
    let [<Uniform>] mutable RunSynchronously = match ConfigurationManager.AppSettings.["RunSynchronously"] with null -> false | value -> scvalue value
    let [<Uniform>] mutable TickDeltaAveraging = match ConfigurationManager.AppSettings.["TickDeltaAveraging"] with null -> false | value -> scvalue value
    let [<Uniform>] TickDeltaMax = 1.0 / 10.0 * double Stopwatch.Frequency |> int64
    let [<Uniform>] QuadtreeElementMagnitudeMax = 1000.0f // if volume is too big, will wreck quadtree performance
    let [<Uniform>] OctreeElementMagnitudeMax = 100.0f // if volume is too big, will wreck octree performance
    let [<Literal>] GameSortPriority = Single.MaxValue
    let [<Uniform>] ScreenSortPriority = GameSortPriority * 0.5f
    let [<Uniform>] GroupSortPriority = ScreenSortPriority * 0.5f
    let [<Uniform>] EntitySortPriority = GroupSortPriority * 0.5f
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
    let [<Literal>] ModelPropertyName = "Model"
    let [<Literal>] TransformPropertyName = "Transform"
    let [<Literal>] BoundsPropertyName = "Bounds"
    let [<Literal>] XtensionPropertyName = "Xtension"
    let [<Literal>] BodyTypePropertyName = "BodyType"
    let [<Literal>] PhysicsMotionPropertyName = "PhysicsMotion"
    let [<Literal>] EffectNameDefault = "Effect"
    let [<Literal>] RefinementDir = "refinement"
    let [<Uniform>] Entity2dSizeDefault = Vector3 (32.0f, 32.0f, 0.0f)
    let [<Uniform>] EntityGuiSizeDefault = Vector3 (128.0f, 32.0f, 0.0f)
    let [<Uniform>] Entity3dSizeDefault = Vector3 (1.0f, 1.0f, 1.0f)
    let [<Uniform>] EntityVuiSizeDefault = Vector3 (4.0f, 1.0f, 1.0f)
    let [<Uniform>] Particle2dSizeDefault = Vector3 (4.0f, 4.0f, 0.0f)
    let [<Uniform>] Particle3dSizeDefault = Vector3 (0.25f, 0.25f, 0.25f)
    let [<Uniform>] BodyJoint2dSizeDefault = Vector3 (16.0f, 16.0f, 0.0f)
    let [<Uniform>] BodyJoint3dSizeDefault = Vector3 (0.25f, 0.25f, 0.25f)
    let [<Literal>] ParticleShadowOffsetDefault = 0.15f
    let [<Literal>] BillboardShadowOffsetDefault = 0.6f
    let [<Uniform>] Eye3dCenterDefault = Vector3 (0.0f, 0.0f, 2.0f)
    let [<Uniform>] Eye3dFieldOfViewDefault = MathF.PI_OVER_3
    let [<Uniform>] mutable QuadnodeSize = match ConfigurationManager.AppSettings.["QuadnodeSize"] with null -> 512.0f | value -> scvalue value
    let [<Uniform>] mutable QuadtreeDepth = match ConfigurationManager.AppSettings.["QuadtreeDepth"] with null -> 8 | value -> scvalue value
    let [<Uniform>] QuadtreeSize = Vector2 (QuadnodeSize * single (pown 2 QuadtreeDepth))
    let [<Uniform>] mutable OctnodeSize = match ConfigurationManager.AppSettings.["OctnodeSize"] with null -> 8.0f | value -> scvalue value
    let [<Uniform>] mutable OctreeDepth = match ConfigurationManager.AppSettings.["OctreeDepth"] with null -> 8 | value -> scvalue value
    let [<Uniform>] OctreeSize = Vector3 (OctnodeSize * single (pown 2 OctreeDepth))
    let [<Uniform>] mutable EventTracing = match ConfigurationManager.AppSettings.["EventTracing"] with null -> false | value -> scvalue value
    let [<Uniform>] mutable EventFilter = match ConfigurationManager.AppSettings.["EventFilter"] with null -> Pass | value -> scvalue value
    let [<Uniform>] EnvironmentMagnitudeThreshold = 48.0f // sqrt (32^2 + 32^2 + 16^2) = more likely an environment that a static prop
    let [<Uniform>] NonPersistentPropertyNames =
        FrozenSet.ToFrozenSet
            ([(* Simulant Properties *)
              "Dispatcher"
              "Content"
              "Protected"
              "Id"
              (* Game Properties *)
              "Eye3dFrustumInterior"
              "Eye3dFrustumExterior"
              "Eye3dFrustumImposter"
              (* Screen Properties *)
              "TransitionState"
              "Nav3d"
              (* Entity Properties *)
              "Facets"
              "Surnames"
              "PerimeterCenter"
              "PerimeterBottom"
              "PerimeterBottomLeft"
              "PerimeterMin"
              "PerimeterMax"
              "PerimeterCenterLocal"
              "PerimeterBottomLocal"
              "PerimeterBottomLeftLocal"
              "PerimeterMinLocal"
              "PerimeterMaxLocal"
              "RotationMatrix"
              "Angles"
              "AnglesLocal"
              "Degrees"
              "DegreesLocal"
              "AffineMatrix"
              "PerimeterUnscaled"
              "Perimeter"
              "Bounds"
              "Imperative"
              "PresenceOverride"
              "PublishChangeEvents"
              "PublishPreUpdates"
              "PublishUpdates"
              "PublishPostUpdates"
              "Mounted"
              "Is2d"
              "Is3d"
              "Physical"
              "LightProbe"
              "Light"
              "Optimized"],
             StringComparer.Ordinal)
    let [<Literal>] BuildName =
#if DEBUG
        "Debug"
#else
        "Release"
#endif

[<RequireQualifiedAccess>]
module Render =

    let [<Uniform>] VendorNamesExceptedFromSwapGlFinishRequirement = ["NVIDIA Corporation"; "AMD"; "ATI Technologies Inc."] // see https://github.com/bryanedds/Nu/wiki/Why-glFinish-for-Some-Drivers-or-Vendors
    let [<Literal>] IgnoreLightMapsName = "IgnoreLightMaps"
    let [<Literal>] OpaqueDistanceName = "OpaqueDistance"
    let [<Literal>] FinenessOffsetName = "FinenessOffset"
    let [<Literal>] ScatterTypeName = "ScatterType"
    let [<Literal>] SpecularScalarName = "SpecularScalar"
    let [<Literal>] RefractiveIndexName = "RefractiveIndex"
    let [<Literal>] TwoSidedName = "TwoSided"
    let [<Literal>] ClippedName = "Clipped"
    let [<Literal>] NavShapeName = "NavShape"
    let [<Uniform>] mutable Vsync = match ConfigurationManager.AppSettings.["Vsync"] with null -> true | value -> scvalue value
    let [<Uniform>] mutable NearPlaneDistanceInterior = match ConfigurationManager.AppSettings.["NearPlaneDistanceInterior"] with null -> 0.125f | value -> scvalue value
    let [<Uniform>] mutable FarPlaneDistanceInterior = match ConfigurationManager.AppSettings.["FarPlaneDistanceInterior"] with null -> 20.0f | value -> scvalue value
    let [<Uniform>] mutable NearPlaneDistanceExterior = match ConfigurationManager.AppSettings.["NearPlaneDistanceExterior"] with null -> 20.0f | value -> scvalue value
    let [<Uniform>] mutable FarPlaneDistanceExterior = match ConfigurationManager.AppSettings.["FarPlaneDistanceExterior"] with null -> 512.0f | value -> scvalue value
    let [<Uniform>] mutable NearPlaneDistanceImposter = match ConfigurationManager.AppSettings.["NearPlaneDistanceImposter"] with null -> 512.0f | value -> scvalue value
    let [<Uniform>] mutable FarPlaneDistanceImposter = match ConfigurationManager.AppSettings.["FarPlaneDistanceImposter"] with null -> 4096.0f | value -> scvalue value
    let [<Uniform>] mutable NearPlaneDistanceOmnipresent = NearPlaneDistanceInterior
    let [<Uniform>] mutable FarPlaneDistanceOmnipresent = FarPlaneDistanceImposter
    let [<Uniform>] mutable DisplayVirtualResolution = match ConfigurationManager.AppSettings.["DisplayVirtualResolution"] with null -> v2i 640 360 | value -> scvalue value
    let [<Uniform>] mutable SsaoResolutionDivisor = match ConfigurationManager.AppSettings.["SsaoResolutionDivisor"] with null -> 1 | value -> scvalue value
    let [<Uniform>] Play3dBoxSize = Vector3 64.0f
    let [<Uniform>] Light3dBoxSize = Vector3 32.0f
    let [<Uniform>] WindowClearColor = Color.Zero
    let [<Uniform>] ViewportClearColor = Color.Zero // NOTE: do not change this color as the deferred lighting shader checks if position.w zero to ignore fragment.
    let [<Literal>] TexturePriorityDefault = 0.5f // higher priority than (supposed) default, but not maximum. this value is arrived at through experimenting with a Windows NVidia driver.
    let [<Uniform>] mutable TextureAnisotropyMax = match ConfigurationManager.AppSettings.["TextureAnisotropyMax"] with null -> 16.0f | value -> scvalue value
    let [<Uniform>] mutable TextureMinimalMipmapIndex = match ConfigurationManager.AppSettings.["TextureMinimalMipmapIndex"] with null -> 2 | value -> scvalue value
    let [<Literal>] SpriteBatchSize = 192 // NOTE: remember to update SPRITE_BATCH_SIZE in shaders when changing this!
    let [<Literal>] SpriteBorderTexelScalar = 0.005f
    let [<Literal>] SpriteMessagesPrealloc = 256
    let [<Literal>] StaticModelMessagesPrealloc = 256
    let [<Literal>] StaticModelSurfaceMessagesPrealloc = 256
    let [<Uniform>] mutable SpineSkeletonScalar = match ConfigurationManager.AppSettings.["SpineSkeletonScalar"] with null -> 1.0f / 3.0f | value -> scvalue value
    let [<Literal>] BonesMax = 128 // NOTE: remember to update BONES_MAX in shaders when changing this!
    let [<Literal>] BonesInfluenceMax = 4 // NOTE: remember to update BONES_INFLUENCE_MAX in shaders when changing this!
    let [<Literal>] AnimatedModelRateScalar = 30.0f // some arbitrary scale that mixamo fbx exported from blender seems to like...
    let [<Literal>] AnimatedModelMessagesPrealloc = 128
    let [<Literal>] InstanceFieldCount = 36
    let [<Literal>] InstanceBatchPrealloc = 1024
    let [<Literal>] TerrainLayersMax = 6
    let [<Literal>] BrdfResolution = 256 // NOTE: half typical resolution because we use 32-bit floats instead of 16-bit.
    let [<Literal>] BrdfSamples = 1024
    let [<Literal>] LightMapsMaxDeferred = 27 // NOTE: remember to update LIGHT_MAPS_MAX in deferred shaders when changing this!
    let [<Literal>] LightMapsMaxForward = 2 // NOTE: remember to update LIGHT_MAPS_MAX in forward shaders when changing this!
    let [<Literal>] LightsMaxDeferred = 64 // NOTE: remember to update LIGHTS_MAX in deferred shaders when changing this!
    let [<Literal>] LightsMaxForward = 9 // NOTE: remember to update LIGHTS_MAX in forward shaders when changing this!
    let [<Uniform>] mutable ShadowVirtualResolution = match ConfigurationManager.AppSettings.["ShadowVirtualResolution"] with null -> 256 | value -> scvalue value
    let [<Uniform>] mutable ShadowDisplayScalarMax = match ConfigurationManager.AppSettings.["ShadowDisplayScalarMax"] with null -> 4 | value -> scvalue value
    let [<Literal>] ShadowTexturesMax = 8 // NOTE: remember to update SHADOW_TEXTURES_MAX in shaders when changing this!
    let [<Literal>] ShadowMapsMax = 7 // NOTE: remember to update SHADOW_MAPS_MAX in shaders when changing this!
    let [<Uniform>] mutable ShadowDirectionalMarginRatioCull = match ConfigurationManager.AppSettings.["ShadowDirectionalMarginRatioCull"] with null -> 0.5f | value -> scvalue value
    let [<Literal>] ShadowCascadesMax = 2 // NOTE: remember to update SHADOW_CASCADES_MAX in shaders when changing this!
    let [<Literal>] ShadowCascadeLevels = 3 // NOTE: remember to update SHADOW_CASCADE_LEVELS_SIZE in shaders when changing this!
    let [<Uniform>] mutable ShadowCascadeLimits = match ConfigurationManager.AppSettings.["ShadowCascadeLimits"] with null -> [|0.2f; 0.6f; 1.0f|] | value -> scvalue value
    let [<Uniform>] mutable ShadowCascadeMarginRatio = match ConfigurationManager.AppSettings.["ShadowCascadeMarginRatio"] with null -> 0.1f | value -> scvalue value
    let [<Uniform>] mutable ShadowCascadeMarginRatioCull = match ConfigurationManager.AppSettings.["ShadowCascadeMarginRatioCull"] with null -> 0.5f | value -> scvalue value
    let [<Uniform>] ShadowCascadeMarginSizeMin = 3.0f // NOTE: current CSM implementation seems to require this, perhaps due to it being currently hacky.
    let [<Literal>] ShadowFovMax = 2.1f // NOTE: remember to update SHADOW_FOV_MAX in shaders when changing this!
    let [<Literal>] ReflectionMapResolution = 1024
    let [<Literal>] IrradianceMapResolution = 32
    let [<Literal>] EnvironmentFilterResolution = 512
    let [<Literal>] EnvironmentFilterMips = 7 // NOTE: changing this requires changing the REFLECTION_LOD_MAX constants in shader code.
    let [<Literal>] BloomSampleLevels = 6
    let [<Literal>] LightMappingEnabledDefault = true
    let [<Literal>] LightCutoffMarginDefault = 0.333f
    let [<Literal>] LightAmbientBoostCutoffDefault = 0.3f
    let [<Literal>] LightAmbientBoostScalarDefault = 0.5f
    let [<Literal>] LightShadowingEnabledDefault = true
    let [<Literal>] LightShadowSamplesDefault = 3
    let [<Literal>] LightShadowBiasDefault = 0.02f
    let [<Literal>] LightShadowSampleScalarDefault = 0.02f
    let [<Literal>] LightShadowExponentDefault = 40.0f
    let [<Literal>] LightShadowDensityDefault = 8.0f
    let [<Uniform>] LightExposureDefault = 1.0f
    let [<Uniform>] ToneMapTypeDefault = AgXToneMap
    let [<Uniform>] ToneMapSlopeDefault = v3One
    let [<Uniform>] ToneMapOffsetDefault = v3Zero
    let [<Uniform>] ToneMapPowerDefault = v3One
    let [<Uniform>] ToneMapSaturationDefault = 1.0f
    let [<Uniform>] ToneMapWhitePointDefault = 4.0f
    let [<Literal>] FogEnabledDefault = false
    let [<Uniform>] FogTypeDefault = LinearFog
    let [<Literal>] FogStartDefault = 8.0f
    let [<Literal>] FogFinishDefault = 128.0f
    let [<Literal>] FogDensityDefault = 0.01f
    let [<Uniform>] FogColorDefault = Color.Gray.WithA 0.5f
    let [<Literal>] SssEnabledGlobalDefault = true
    let [<Literal>] SssEnabledLocalDefault = true
    let [<Literal>] SsaoEnabledGlobalDefault = true
    let [<Literal>] SsaoEnabledLocalDefault = true
    let [<Literal>] SsaoSampleCountDefault = 16
    let [<Literal>] SsaoSampleCountMax = 128
    let [<Literal>] SsaoIntensityDefault = 1.5f
    let [<Literal>] SsaoBiasDefault = 0.025f
    let [<Literal>] SsaoRadiusDefault = 0.125f
    let [<Literal>] SsaoDistanceMaxDefault = 0.125f
    let [<Literal>] SsvfEnabledGlobalDefault = true
    let [<Literal>] SsvfEnabledLocalDefault = true
    let [<Literal>] SsvfStepsDefault = 16
    let [<Literal>] SsvfAsymmetryDefault = 0.25f
    let [<Literal>] SsvfIntensityDefault = 1.0f
    let [<Literal>] SsrlEnabledGlobalDefault = true
    let [<Literal>] SsrlEnabledLocalDefault = true
    let [<Literal>] SsrlIntensityDefault = 4.0f
    let [<Literal>] SsrlDetailDefault = 0.21f
    let [<Literal>] SsrlRefinementsMaxDefault = 24
    let [<Literal>] SsrlRayThicknessDefault = 0.025f
    let [<Literal>] SsrlTowardEyeCutoffDefault = 0.9f
    let [<Literal>] SsrlDepthCutoffDefault = 24.0f
    let [<Literal>] SsrlDepthCutoffMarginDefault = 0.2f
    let [<Literal>] SsrlDistanceCutoffDefault = 24.0f
    let [<Literal>] SsrlDistanceCutoffMarginDefault = 0.2f
    let [<Literal>] SsrlRoughnessCutoffDefault = 0.3f
    let [<Literal>] SsrlRoughnessCutoffMarginDefault = 0.3f
    let [<Literal>] SsrlSlopeCutoffDefault = 0.1f
    let [<Literal>] SsrlSlopeCutoffMarginDefault = 0.2f
    let [<Literal>] SsrlEdgeHorizontalMarginDefault = 0.1f
    let [<Literal>] SsrlEdgeVerticalMarginDefault = 0.1f
    let [<Literal>] SsrrEnabledGlobalDefault = true
    let [<Literal>] SsrrEnabledLocalDefault = true
    let [<Literal>] SsrrIntensityDefault = 1.0f
    let [<Literal>] SsrrDetailDefault = 0.3f
    let [<Literal>] SsrrRefinementsMaxDefault = 20
    let [<Literal>] SsrrRayThicknessDefault = 0.25f
    let [<Literal>] SsrrDistanceCutoffDefault = 64.0f
    let [<Literal>] SsrrDistanceCutoffMarginDefault = 0.2f
    let [<Literal>] SsrrEdgeHorizontalMarginDefault = 0.1f
    let [<Literal>] SsrrEdgeVerticalMarginDefault = 0.1f
    let [<Literal>] BloomEnabledGlobalDefault = true
    let [<Literal>] BloomEnabledLocalDefault = true
    let [<Literal>] BloomThresholdDefault = 0.5f
    let [<Literal>] BloomKarisAverageEnabledDefault = true
    let [<Literal>] BloomFilterRadiusDefault = 0.004f
    let [<Literal>] BloomStrengthDefault = 0.03f
    let [<Literal>] FxaaEnabledDefault = false
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
    let [<Literal>] FinenessOffsetDefault = 0.0f
    let [<Uniform>] ScatterTypeDefault = NoScatter
    let [<Literal>] SpecularScalarDefault = 1.0f
    let [<Literal>] RefractiveIndexDefault = 1.0f
    let [<Literal>] FontSizeDefault = 14
    let [<Literal>] Body3dSegmentRenderMagnitudeMax = 48.0f
    let [<Literal>] Body3dSegmentRenderDistanceMax = 40.0f
    let [<Literal>] Body3dRenderDistanceMax = 32.0f

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
    let [<Literal>] FadeInSecondsMin = 0.1f // NOTE: Mix_FadeInMusicPos seems to sometimes cause audio 'popping' when starting a song, so a minimum fade is used instead.

[<RequireQualifiedAccess>]
module Physics =

    let [<Uniform>] GravityDefault = Vector3 (0.0f, -9.80665f, 0.0f)
    let [<Literal>] FrictionDefault = 0.5f
    let [<Literal>] AngularDampingDefault = 0.2f
    let [<Literal>] BreakingPointDefault = 1000000.0f
    let [<Literal>] CollisionWildcard = "*"
    let [<Uniform>] mutable Collision3dBodiesMax = match ConfigurationManager.AppSettings.["Collision3dBodiesMax"] with null -> 65536 | value -> scvalue value
    let [<Uniform>] mutable Collision3dBodyPairsMax = match ConfigurationManager.AppSettings.["Collision3dBodyPairsMax"] with null -> 32768 | value -> scvalue value
    let [<Uniform>] mutable Collision3dContactConstraintsMax = match ConfigurationManager.AppSettings.["Collision3dContactConstraintsMax"] with null -> 16384 | value -> scvalue value
    let [<Uniform>] mutable Collision3dSteps = match ConfigurationManager.AppSettings.["Collision3dSteps"] with null -> 1 | value -> scvalue value
    let [<Uniform>] mutable Collision3dThreads = match ConfigurationManager.AppSettings.["Collision3dThreads"] with null -> max 1 (Environment.ProcessorCount - 2) | value -> scvalue value
    let [<Uniform>] mutable Collision3dBarriersMax = match ConfigurationManager.AppSettings.["Collision3dBarriersMax"] with null -> max 1 (Environment.ProcessorCount - 2) | value -> scvalue value
    let [<Uniform>] mutable Collision3dJobsMax = match ConfigurationManager.AppSettings.["Collision3dJobsMax"] with null -> 128 | value -> scvalue value
    let [<Uniform>] mutable Collision3dBodyUnoptimizedCreationMax = 128 * 3 // NOTE: related to https://github.com/jrouwe/JoltPhysics/issues/1520#issuecomment-2667060129
    let [<Uniform>] mutable GroundAngleMax = match ConfigurationManager.AppSettings.["GroundAngleMax"] with null -> single (Math.PI * 0.25) | value -> scvalue value
    let [<Uniform>] internal BroadPhaseLayerNonMoving = byte 0
    let [<Uniform>] internal BroadPhaseLayerMoving = byte 1
    let [<Uniform>] internal ObjectLayerNonMoving = JoltPhysicsSharp.ObjectLayer 0u
    let [<Uniform>] internal ObjectLayerMoving = JoltPhysicsSharp.ObjectLayer 1u
    let [<Uniform>] internal ObjectLayerDisabled = JoltPhysicsSharp.ObjectLayer 2u
    let [<Literal>] internal InternalIndex = -1
    let [<Uniform>] BodyPropertyAffectingPropertyNames =
        FrozenSet.ToFrozenSet
            (["Scale"
              "Offset"
              "Size"
              "BodyEnabled"
              "BodyFrozen"
              "BodyType"
              "BodyShape"
              "SleepingAllowed"
              "Friction"
              "Restitution"
              "LinearDamping"
              "AngularDamping"
              "AngularFactor"
              "Substance"
              "GravityOverride"
              "CharacterProperties"
              "VehicleProperties"
              "CollisionDetection"
              "CollisionCategories"
              "CollisionMask"
              "Sensor"],
             StringComparer.Ordinal)

[<RequireQualifiedAccess>]
module Nav =
    
    let [<Literal>] Bounds3dMagnitudeMax = 256.0f

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
    let [<Literal>] Cursor = "Cursor"

[<RequireQualifiedAccess>]
module Gui =

    let [<Uniform>] mutable SliceMarginDefault = match ConfigurationManager.AppSettings.["SliceMarginDefault"] with null -> Vector2 (4.0f, 4.0f) | value -> scvalue value
    let [<Uniform>] ColorDisabledDefault = Color (0.75f, 0.75f, 0.75f, 0.75f)
    let [<Literal>] TextShiftDefault = 0.5f

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

    let [<Literal>] LogFilePath = "Log.txt"
    let [<Literal>] SpriteShaderFilePath = "Assets/Default/Sprite.glsl"
    let [<Literal>] SpriteBatchShaderFilePath = "Assets/Default/SpriteBatch.glsl"
    let [<Literal>] SkyBoxShaderFilePath = "Assets/Default/SkyBox.glsl"
    let [<Literal>] IrradianceShaderFilePath = "Assets/Default/Irradiance.glsl"
    let [<Literal>] EnvironmentFilterShaderFilePath = "Assets/Default/EnvironmentFilter.glsl"
    let [<Literal>] FilterBox1dShaderFilePath = "Assets/Default/FilterBox1d.glsl"
    let [<Literal>] FilterGaussian2dShaderFilePath = "Assets/Default/FilterGaussian2d.glsl"
    let [<Literal>] FilterGaussianArray2dShaderFilePath = "Assets/Default/FilterGaussianArray2d.glsl"
    let [<Literal>] FilterBilateralDownSample4dShaderFilePath = "Assets/Default/FilterBilateralDownSample4d.glsl"
    let [<Literal>] FilterBilateralUpSample4dShaderFilePath = "Assets/Default/FilterBilateralUpSample4d.glsl"
    let [<Literal>] FilterBloomExtractShaderFilePath = "Assets/Default/FilterBloomExtract.glsl"
    let [<Literal>] FilterBloomDownSampleShaderFilePath = "Assets/Default/FilterBloomDownSample.glsl"
    let [<Literal>] FilterBloomUpSampleShaderFilePath = "Assets/Default/FilterBloomUpSample.glsl"
    let [<Literal>] FilterBloomApplyShaderFilePath = "Assets/Default/FilterBloomApply.glsl"
    let [<Literal>] FilterFxaaShaderFilePath = "Assets/Default/FilterFxaa.glsl"
    let [<Literal>] FilterGaussian4dShaderFilePath = "Assets/Default/FilterGaussian4d.glsl"
    let [<Literal>] FilterPresentationShaderFilePath = "Assets/Default/FilterPresentation.glsl"
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
    let [<Literal>] PhysicallyBasedDeferredStaticClippedShaderFilePath = "Assets/Default/PhysicallyBasedDeferredStaticClipped.glsl"
    let [<Literal>] PhysicallyBasedDeferredAnimatedShaderFilePath = "Assets/Default/PhysicallyBasedDeferredAnimated.glsl"
    let [<Literal>] PhysicallyBasedDeferredTerrainShaderFilePath = "Assets/Default/PhysicallyBasedDeferredTerrain.glsl"
    let [<Literal>] PhysicallyBasedDeferredLightMappingShaderFilePath = "Assets/Default/PhysicallyBasedDeferredLightMapping.glsl"
    let [<Literal>] PhysicallyBasedDeferredAmbientShaderFilePath = "Assets/Default/PhysicallyBasedDeferredAmbient.glsl"
    let [<Literal>] PhysicallyBasedDeferredIrradianceShaderFilePath = "Assets/Default/PhysicallyBasedDeferredIrradiance.glsl"
    let [<Literal>] PhysicallyBasedDeferredEnvironmentFilterShaderFilePath = "Assets/Default/PhysicallyBasedDeferredEnvironmentFilter.glsl"
    let [<Literal>] PhysicallyBasedDeferredSsaoShaderFilePath = "Assets/Default/PhysicallyBasedDeferredSsao.glsl"
    let [<Literal>] PhysicallyBasedDeferredLightingShaderFilePath = "Assets/Default/PhysicallyBasedDeferredLighting.glsl"
    let [<Literal>] PhysicallyBasedDeferredColoringShaderFilePath = "Assets/Default/PhysicallyBasedDeferredColoring.glsl"
    let [<Literal>] PhysicallyBasedDeferredCompositionShaderFilePath = "Assets/Default/PhysicallyBasedDeferredComposition.glsl"
    let [<Literal>] PhysicallyBasedForwardStaticShaderFilePath = "Assets/Default/PhysicallyBasedForwardStatic.glsl"
    let [<Literal>] PhysicallyBasedForwardAnimatedShaderFilePath = "Assets/Default/PhysicallyBasedForwardAnimated.glsl"