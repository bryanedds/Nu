// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open OpenTK
open Nu

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module Engine =

        let [<Literal>] DesiredFps = 60
        let [<Literal>] SuccessExitCode = 0
        let [<Literal>] FailureExitCode = 1
        let [<Literal>] DefaultSubsystemOrder = 1.0f
        let [<Literal>] NamePropertyName = "Name"
        let [<Literal>] RendererSubsystemName = "RendererSubsystem"
        let [<Literal>] AudioPlayerSubsystemName = "AudioPlayerSubsystem"
        let [<Literal>] PhysicsEngineSubsystemName = "PhysicsEngineSubsystem"
        let [<Literal>] DefaultScreenName = "Screen"
        let [<Literal>] DefaultLayerName = "Layer"
        let [<Literal>] DefaultEntityName = "Entity"
        let [<Literal>] DefaultEffectName = "Effect"
        let [<Literal>] GameSortPriority = Single.MaxValue
        let (*Literal*) ScreenSortPriority = GameSortPriority * 0.5f
        let (*Literal*) LayerSortPriority = ScreenSortPriority * 0.5f
        let (*Literal*) EntitySortPriority = LayerSortPriority * 0.5f
        let (*Literal*) DefaultEntitySize = Vector2 64.0f
        let (*Literal*) EntityTreeGranularity = 2
        let (*Literal*) EntityTreeDepth = 7
        let (*Literal*) EntityTreeSize = Vector2 (single ^ Math.Pow (2.0, 16.0))
        let (*Literal*) EntityTreeBounds = Vector4 (EntityTreeSize.X * -0.5f, EntityTreeSize.Y * -0.5f, EntityTreeSize.X * 0.5f, EntityTreeSize.Y * 0.5f)
        let (*Literal*) VanillaSpecialization = "Vanilla"
        let (*Literal*) InvalidId = Guid.Empty

    [<RequireQualifiedAccess>]
    module Associations =
    
        let [<Literal>] Symbol = "Symbol"
        let [<Literal>] Render = "Render"
        let [<Literal>] Audio = "Audio"

    [<RequireQualifiedAccess>]
    module Render =

        let [<Literal>] ResolutionXDefault = 960
        let [<Literal>] ResolutionYDefault = 544
        let (*Literal*) ResolutionX = Core.getResolutionOrDefault true ResolutionXDefault
        let (*Literal*) ResolutionY = Core.getResolutionOrDefault false ResolutionYDefault
        let (*Literal*) ScreenClearing = ColorClear (255uy, 255uy, 255uy)

    [<RequireQualifiedAccess>]
    module Audio =

        let [<Literal>] Frequency = 44100
        let [<Literal>] DefaultBufferSize = 1024
        let [<Literal>] DefaultTimeToFadeOutSongMs = 500

    [<RequireQualifiedAccess>]
    module Physics =

        let (*Literal*) PhysicsStepRate = 1.0f / single Engine.DesiredFps
        let [<Literal>] PhysicsToPixelRatio = 64.0f
        let (*Literal*) PixelToPhysicsRatio = 1.0f / PhysicsToPixelRatio
        let [<Literal>] NormalDensity = 10.0f // NOTE: this seems to be a stable density for Farseer
        let (*Literal*) Gravity = Vector2 (0.0f, -9.80665f) * PhysicsToPixelRatio
        let [<Literal>] CollisionProperty = "C"

    [<RequireQualifiedAccess>]
    module Effects =

        let [<Literal>] DefaultEffectHistoryMax = 300 // 5 seconds

    [<RequireQualifiedAccess>]
    module Math =

        let [<Literal>] RadiansToDegrees = 57.2957795
        let (*Literal*) DegreesToRadians = 1.0 / RadiansToDegrees
        let (*Literal*) RadiansToDegreesF = single RadiansToDegrees
        let (*Literal*) DegreesToRadiansF = single DegreesToRadians

    [<RequireQualifiedAccess>]
    module PrettyPrint =
    
        let [<Literal>] SimpleThreshold = 1
        let [<Literal>] DetailedThreshold = 2