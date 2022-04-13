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
    let (*Literal*) ScreenSortPriority = GameSortPriority - 1.0f
    let (*Literal*) GroupSortPriority = ScreenSortPriority - 1.0f
    let (*Literal*) EntitySortPriority = GroupSortPriority - 1.0f
    let (*Literal*) EntitySizeDefault2d = Vector3 (48.0f, 48.0f, 0.0f)
    let (*Literal*) EntitySizeDefault3d = Vector3 1.0f
    let (*Literal*) EntityTreeGranularity = 4
    let (*Literal*) EntityTreeDepth = 3
    let (*Literal*) EntityTreeSize = Vector3 (single (Math.Pow (2.0, 16.0)))
    let (*Literal*) EntityTreeBounds = Box3 (-EntityTreeSize * 0.5f, EntityTreeSize * 0.5f)
    let (*Literal*) ParticleSizeDefault = Vector3 12.0f
    let (*Literal*) InvalidId = Guid.Empty
    let (*Literal*) GravityDefault = Vector3 (0.0f, -9.80665f, 0.0f)
    let (*Literal*) EventTracing =
        match ConfigurationManager.AppSettings.["EventTracing"] with
        | null -> false
        | eventTracing -> scvalue<bool> eventTracing
    let (*Literal*) EventFilter =
        match ConfigurationManager.AppSettings.["EventFilter"] with
        | null -> EventFilter.Empty
        | eventFilter -> scvalue<EventFilter.Filter> eventFilter

[<RequireQualifiedAccess>]
module Associations =

    let [<Literal>] Symbol = "Symbol"
    let [<Literal>] Render = "Render"
    let [<Literal>] Audio = "Audio"

[<RequireQualifiedAccess>]
module Render =

    let [<Literal>] VirtualResolutionX = 960
    let [<Literal>] VirtualResolutionY = 540
    let (*Literal*) VirtualResolutionF = Vector2 (single VirtualResolutionX, single VirtualResolutionY)
    let (*Literal*) VirtualResolution = Vector2i (VirtualResolutionX, VirtualResolutionY)
    let (*Literal*) VirtualScalar =
        match ConfigurationManager.AppSettings.["VirtualScalar"] with
        | null -> 2
        | resolution -> scvalue<int> resolution
    let (*Literal*) ResolutionX = VirtualResolutionX * VirtualScalar
    let (*Literal*) ResolutionY = VirtualResolutionY * VirtualScalar
    let (*Literal*) ResolutionF = Vector2 (single ResolutionX, single ResolutionY)
    let (*Literal*) Resolution = Vector2i (ResolutionX, ResolutionY)
    let (*Literal*) RendererFlagsDefault =
        SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED |||
        SDL.SDL_RendererFlags.SDL_RENDERER_PRESENTVSYNC
    let (*Literal*) ScreenClearing = ColorClear (255uy, 255uy, 255uy) // TODO: move this to ViewConfig or WorldConfig?

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

module Ecs =

    let [<Literal>] ArrayReserve = 256 // just large enough to amortize cache misses
    let [<Literal>] ArrayGrowth = 2
    let [<Literal>] PreallocateAmount = ArrayReserve