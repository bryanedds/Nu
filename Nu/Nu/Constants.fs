// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Constants
open System
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
    let [<Literal>] DefaultScreenName = "Screen"
    let [<Literal>] DefaultLayerName = "Layer"
    let [<Literal>] DefaultEntityName = "Entity"
    let [<Literal>] DefaultEffectName = "Effect"
    let [<Literal>] GameSortPriority = Single.MaxValue
    let (*Literal*) ScreenSortPriority = GameSortPriority - 1.0f
    let (*Literal*) LayerSortPriority = ScreenSortPriority - 1.0f
    let (*Literal*) EntitySortPriority = LayerSortPriority - 1.0f
    let (*Literal*) DefaultEntitySize = Vector2 64.0f
    let (*Literal*) EntityTreeGranularity = 4
    let (*Literal*) EntityTreeDepth = 3
    let (*Literal*) EntityTreeSize = Vector2 (single (Math.Pow (2.0, 16.0)))
    let (*Literal*) EntityTreeBounds = Vector4 (EntityTreeSize.X * -0.5f, EntityTreeSize.Y * -0.5f, EntityTreeSize.X * 0.5f, EntityTreeSize.Y * 0.5f)
    let (*Literal*) InvalidId = Guid.Empty
    let (*Literal*) TaskletListConfig = Functional
    let (*Literal*) SimulantMapConfig = Functional
    let (*Literal*) KeyValueMapConfig = Functional

[<RequireQualifiedAccess>]
module Associations =

    let [<Literal>] Symbol = "Symbol"
    let [<Literal>] Render = "Render"
    let [<Literal>] Audio = "Audio"

[<RequireQualifiedAccess>]
module SymbolStore =

    let (*Literal*) SymbolMapConfig = Functional

[<RequireQualifiedAccess>]
module Render =

    let [<Literal>] DefaultResolutionX = 960
    let [<Literal>] DefaultResolutionY = 544
    let (*Literal*) DefaultRendererFlags =
        SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED |||
        SDL.SDL_RendererFlags.SDL_RENDERER_PRESENTVSYNC
    let (*Literal*) ResolutionX = Core.getResolutionOrDefault true DefaultResolutionX
    let (*Literal*) ResolutionY = Core.getResolutionOrDefault false DefaultResolutionY
    let (*Literal*) ScreenClearing = ColorClear (255uy, 255uy, 255uy)

[<RequireQualifiedAccess>]
module Audio =

    let [<Literal>] Frequency = 44100
    let [<Literal>] DefaultSongVolume = 0.25f
    let [<Literal>] DefaultSoundVolume = 0.5f
    let [<Literal>] DefaultBufferSize = 1024
    let [<Literal>] DefaultFadeOutMs = 500
    let (*Literal*) AssetMapConfig = Functional

[<RequireQualifiedAccess>]
module Physics =

    let (*Literal*) PhysicsStepRate = 1.0f / single Engine.DesiredFps
    let [<Literal>] PhysicsToPixelRatio = 64.0f
    let (*Literal*) PixelToPhysicsRatio = 1.0f / PhysicsToPixelRatio
    let [<Literal>] NormalDensity = 1.0f
    let (*Literal*) Gravity = Vector2 (0.0f, -9.80665f) * PhysicsToPixelRatio
    let (*Literal*) MessageListConfig = Functional

[<RequireQualifiedAccess>]
module TileMap =

    let [<Literal>] CollisionPropertyName = "C"
    let [<Literal>] DepthPropertyName = "D"
    let [<Literal>] InfoPropertyName = "I"

[<RequireQualifiedAccess>]
module Metadata =

    let (*Literal*) MetadatMapConfig = Functional

[<RequireQualifiedAccess>]
module Effects =

    let [<Literal>] DefaultEffectHistoryMax = 300 // 5 seconds

[<RequireQualifiedAccess>]
module Math =

    let [<Literal>] RadiansToDegrees = 57.2957795
    let (*Literal*) DegreesToRadians = 1.0 / RadiansToDegrees
    let (*Literal*) RadiansToDegreesF = single RadiansToDegrees
    let (*Literal*) DegreesToRadiansF = single DegreesToRadians

module Ecs =

    let [<Literal>] ArrayReserve = 256
    let [<Literal>] ArrayGrowth = 2