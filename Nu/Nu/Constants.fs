// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Constants
open System
open System.Numerics
open SDL2
open Nu
open Prime

[<RequireQualifiedAccess>]
module Engine =

    let [<Literal>] DesiredFps = 60
    let [<Literal>] SuccessExitCode = 0
    let [<Literal>] FailureExitCode = 1
    let [<Literal>] NamePropertyName = "Name"
    let [<Literal>] RendererSubsystemName = "RendererSubsystem"
    let [<Literal>] AudioPlayerSubsystemName = "AudioPlayerSubsystem"
    let [<Literal>] PhysicsEngineSubsystemName = "PhysicsEngineSubsystem"
    let [<Literal>] EffectNameDefault = "Effect"
    let [<Literal>] GameSortPriority = Single.MaxValue
    let [<Literal>] RefinementDir = "refinement"
    let (*Literal*) ScreenSortPriority = GameSortPriority - 1.0f
    let (*Literal*) GroupSortPriority = ScreenSortPriority - 1.0f
    let (*Literal*) EntitySortPriority = GroupSortPriority - 1.0f
    let (*Literal*) EntitySizeDefault = Vector2 48.0f
    let (*Literal*) EntityTreeGranularity = 4
    let (*Literal*) EntityTreeDepth = 3
    let (*Literal*) EntityTreeSize = Vector2 (single (Math.Pow (2.0, 16.0)))
    let (*Literal*) EntityTreeBounds = Vector4 (EntityTreeSize.X * -0.5f, EntityTreeSize.Y * -0.5f, EntityTreeSize.X, EntityTreeSize.Y)
    let (*Literal*) ParticleSizeDefault = Vector2 12.0f
    let (*Literal*) InvalidId = Guid.Empty
    let (*Literal*) GravityDefault = Vector2 (0.0f, -9.80665f)

[<RequireQualifiedAccess>]
module Associations =

    let [<Literal>] Symbol = "Symbol"
    let [<Literal>] Render = "Render"
    let [<Literal>] Audio = "Audio"

[<RequireQualifiedAccess>]
module Render =

    let [<Literal>] VirtualResolutionX = 960
    let [<Literal>] VirtualResolutionY = 540
    let (*Literal*) VirtualScalar = Core.getVirtualScalarOrDefault 2
    let (*Literal*) ResolutionX = VirtualResolutionX * VirtualScalar
    let (*Literal*) ResolutionY = VirtualResolutionY * VirtualScalar
    let (*Literal*) GpuInitFlagsDefault = SDL.GPU_InitFlagEnum.GPU_INIT_ENABLE_VSYNC
    let (*Literal*) ScreenClearing = ColorClear (Color (255uy, 255uy, 255uy, 255uy)) // TODO: move this to ViewConfig or WorldConfig?

[<RequireQualifiedAccess>]
module Audio =

    let [<Literal>] Frequency = 44100
    let [<Literal>] SongVolumeDefault = 0.25f
    let [<Literal>] SoundVolumeDefault = 0.5f
    let [<Literal>] BufferSizeDefault = 1024
    let [<Literal>] FadeOutMsDefault = 500

[<RequireQualifiedAccess>]
module Physics =

    let (*Literal*) PhysicsStepRate = 1.0f / single Engine.DesiredFps
    let [<Literal>] PhysicsToPixelRatio = 64.0f
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
module Effects =

    let [<Literal>] EffectHistoryMaxDefault = Engine.DesiredFps // 1 second of effect history

[<RequireQualifiedAccess>]
module Math =

    let [<Literal>] RadiansToDegrees = 57.2957795
    let (*Literal*) DegreesToRadians = 1.0 / RadiansToDegrees
    let (*Literal*) RadiansToDegreesF = single RadiansToDegrees
    let (*Literal*) DegreesToRadiansF = single DegreesToRadians

module Ecs =

    let [<Literal>] ArrayReserve = 256
    let [<Literal>] ArrayGrowth = 2