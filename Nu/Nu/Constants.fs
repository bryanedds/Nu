// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Constants
open System
open System.Numerics
open System.Configuration
open SDL2
open Nu
open Prime

[<RequireQualifiedAccess>]
module Engine =

    let [<Literal>] DesiredFps = 60
    let [<Literal>] SuccessExitCode = 0
    let [<Literal>] FailureExitCode = 1
    let [<Literal>] NamePropertyName = "Name"
    let [<Literal>] SurnamesPropertyName = "Surnames"
    let [<Literal>] EffectNameDefault = "Effect"
    let [<Literal>] RefinementDir = "refinement"
    let [<Literal>] GameSortPriority = Single.MaxValue
    let [<Literal>] LohSize = 85000
    let (*Literal*) LohArraySlop = sizeof<int array> * sizeof<int array>
    let (*Literal*) LohSizeMinusArraySlop = LohSize - LohArraySlop
    let (*Literal*) ScreenSortPriority = GameSortPriority - 1.0f
    let (*Literal*) GroupSortPriority = ScreenSortPriority - 1.0f
    let (*Literal*) EntitySortPriority = GroupSortPriority - 1.0f
    let (*Literal*) EntitySize2dDefault = Vector3 (48.0f, 48.0f, 0.0f)
    let (*Literal*) EntitySizeGuiDefault = Vector3 (192.0f, 48.0f, 0.0f)
    let (*Literal*) EntitySize3dDefault = Vector3 1.0f
    let (*Literal*) ParticleSize2dDefault = Vector3 (12.0f, 12.0f, 0.0f)
    let (*Literal*) ParticleSize3dDefault = Vector3 (0.01f, 0.01f, 0.01f)
    let (*Literal*) QuadtreeGranularity = 4
    let (*Literal*) QuadtreeDepth = 4
    let (*Literal*) QuadtreeSize = Vector2 (single (Math.Pow (2.0, 18.0)))
    let (*Literal*) QuadtreeBounds = Box2 (-QuadtreeSize * 0.5f, QuadtreeSize)
    let (*Literal*) OctreeGranularity = 4
    let (*Literal*) OctreeDepth = 4
    let (*Literal*) OctreeSize = Vector3 (single (Math.Pow (2.0, 10.0)))
    let (*Literal*) OctreeBounds = Box3 (-OctreeSize * 0.5f, OctreeSize)
    let (*Literal*) InvalidId = Guid.Empty
    let (*Literal*) GravityDefault = Vector3 (0.0f, -9.80665f, 0.0f)
    let (*Literal*) EventTracing = match ConfigurationManager.AppSettings.["EventTracing"] with null -> false | eventTracing -> scvalue<bool> eventTracing
    let (*Literal*) EventFilter = match ConfigurationManager.AppSettings.["EventFilter"] with null -> EventFilter.Empty | eventFilter -> scvalue<EventFilter.Filter> eventFilter

[<RequireQualifiedAccess>]
module Associations =

    let [<Literal>] Symbol = "Symbol"
    let [<Literal>] Render = "Render"
    let [<Literal>] Audio = "Audio"

[<RequireQualifiedAccess>]
module Render =

    let [<Literal>] VirtualResolutionX = 960
    let [<Literal>] VirtualResolutionY = 540
    let (*Literal*) VirtualResolution = Vector2i (VirtualResolutionX, VirtualResolutionY)
    let (*Literal*) VirtualResolutionF = Vector2 (single VirtualResolutionX, single VirtualResolutionY)
    let (*Literal*) VirtualScalar = match ConfigurationManager.AppSettings.["VirtualScalar"] with null -> 2 | resolution -> scvalue<int> resolution
    let (*Literal*) VirtualScalarF = single VirtualScalar
    let (*Literal*) VirtualScalar2i = Vector2i VirtualScalar
    let (*Literal*) VirtualScalar2 = Vector2 (single VirtualScalar2i.X, single VirtualScalar2i.Y)
    let (*Literal*) ResolutionX = VirtualResolutionX * VirtualScalar
    let (*Literal*) ResolutionY = VirtualResolutionY * VirtualScalar
    let (*Literal*) ResolutionF = Vector2 (single ResolutionX, single ResolutionY)
    let (*Literal*) Resolution = Vector2i (ResolutionX, ResolutionY)
    let (*Literal*) ViewportFull windowSize = Box2i (Vector2i.Zero, windowSize)
    let (*Literal*) ViewportMargin (windowSize : Vector2i) = let size = Vector2i (ResolutionX, ResolutionY) in Vector2i ((windowSize.X - size.X) / 2, (windowSize.Y - size.Y) / 2)
    let (*Literal*) ViewportOffset windowSize = Box2i (ViewportMargin windowSize, Resolution)
    let (*Literal*) Viewport = Box2i (Vector2i.Zero, Vector2i (ResolutionX, ResolutionY))
    let (*Literal*) FieldOfView = single (Math.PI / 3.0) // 60 degrees
    let (*Literal*) AspectRatio = ResolutionF.X / ResolutionF.Y
    let [<Literal>] NearPlaneDistance = 0.01f
    let (*Literal*) FarPlaneDistanceEnclosed = 200.0f
    let (*Literal*) FarPlaneDistanceUnenclosed = 500.0f
    let (*Literal*) FarPlaneDistanceAfatecs = 3200.0f // ~2 miles
    let (*Literal*) PlayBoxSize3d = Vector3 200.0f
    let (*Literal*) LightBoxSize3d = Vector3 200.0f
    let (*Literal*) EyePosition3dDefault = Vector3 (0.0f, 2.0f, 4.0f)
    let (*Literal*) WindowClearColor = Color.Black // NOTE: since we're using deferred rendering, black is the only color that will work.
    let [<Literal>] SkyBoxIrradianceMapResolutionX = 32
    let [<Literal>] SkyBoxIrradianceMapResolutionY = 32
    let [<Literal>] OpenGlVersionMajor = 4
    let [<Literal>] OpenGlVersionMinor = 1
    let [<Literal>] OpenGlCore = true
    let (*Literal*) GlslVersionPragma = "#version " + string OpenGlVersionMajor + string OpenGlVersionMinor + "0 " + if OpenGlCore then "core" else ""
    let [<Literal>] SpriteBatchSize = 192
    let [<Literal>] SpriteBorderTexelScalar = 0.01f
    let [<Literal>] SpriteMessagesPrealloc = 256
    let [<Literal>] StaticModelMessagesPrealloc = 256
    let [<Literal>] ShaderLightsMax = 32
    let [<Literal>] EnvironmentMip0Resolution = 64
    let [<Literal>] EnvironmentMipLevels = 5
    let (*Literal*) EnvironmentResolution = EnvironmentMip0Resolution * EnvironmentMipLevels

[<RequireQualifiedAccess>]
module Audio =

    let [<Literal>] Frequency = 44100
    let [<Literal>] SongVolumeDefault = 0.5f
    let [<Literal>] SoundVolumeDefault = 1.0f
    let [<Literal>] BufferSizeDefault = 1024
    let [<Literal>] FadeInMinimum = 100 // NOTE: Mix_PlayMusic seems to sometimes cause audio 'popping' when starting a song, so a minimum fade is used instead.
    let [<Literal>] FadeOutMsDefault = 500
    let [<Literal>] SongResumptionMaximum = 90000L // HACK: prevents songs from starting over too often due to hack in SdlAudioPlayer.playSong.

[<RequireQualifiedAccess>]
module Physics =

    let (*Literal*) PhysicsStepRate = 1.0f / single Engine.DesiredFps
    let [<Literal>] PhysicsToPixelRatio = 48.0f // 48 pixels = 1 meter
    let (*Literal*) PixelToPhysicsRatio = 1.0f / PhysicsToPixelRatio
    let [<Literal>] DensityDefault = 1.0f
    let (*Literal*) GravityDefault = Engine.GravityDefault * PhysicsToPixelRatio

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

    let [<Literal>] EffectHistoryMaxDefault = Engine.DesiredFps // 1 second of effect history

[<RequireQualifiedAccess>]
module Ecs =

    let [<Literal>] ArrayReserve = 256 // just large enough to amortize cache misses
    let [<Literal>] ParallelTaskSizeMinimum = 1024
    let [<Literal>] PreallocateAmount = ArrayReserve
    let [<Literal>] IntraComponentPrefix = "@"
    let [<Literal>] UnscheduledEventSuffix = "!U"
    let [<Literal>] ScheduledEventSuffix = "!S"

[<RequireQualifiedAccess>]
module Paths =

    let [<Literal>] SkyBoxShaderFilePath = "Assets/Default/SkyBox.glsl"
    let [<Literal>] IrradianceShaderFilePath = "Assets/Default/Irradiance.glsl"
    let [<Literal>] EnvironmentShaderFilePath = "Assets/Default/Environment.glsl"
    let [<Literal>] PhysicallyBasedDeferredShaderFilePath = "Assets/Default/PhysicallyBasedDeferred.glsl"
    let [<Literal>] PhysicallyBasedDeferred2ShaderFilePath = "Assets/Default/PhysicallyBasedDeferred2.glsl"
    let [<Literal>] PhysicallyBasedForwardShaderFilePath = "Assets/Default/PhysicallyBasedForward.glsl"
