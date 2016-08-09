// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open OpenTK
open Prime
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
        let [<Literal>] GameName = "Game"
        let [<Literal>] DefaultScreenName = "Screen"
        let [<Literal>] DefaultGroupName = "Group"
        let [<Literal>] DefaultEntityName = "Entity"
        let [<Literal>] GamePublishingPriority = Single.MaxValue
        let ScreenPublishingPriority = GamePublishingPriority * 0.5f
        let GroupPublishingPriority = ScreenPublishingPriority * 0.5f
        let EntityPublishingPriority = GroupPublishingPriority * 0.5f
        let DefaultEntitySize = Vector2 64.0f
        let EntityTreeDepth = 9
        let EntityTreeSize = Vector2 (single ^ Math.Pow (2.0, 18.0))
        let EntityTreeBounds = Vector4 (EntityTreeSize.X * -0.5f, EntityTreeSize.Y * -0.5f, EntityTreeSize.X * 0.5f, EntityTreeSize.Y * 0.5f)
        let VanillaSpecialization = !!"Vanilla"
        let InvalidId = Guid.Empty

    [<RequireQualifiedAccess>]
    module Associations =
    
        let [<Literal>] Symbol = "Symbol"
        let [<Literal>] Render = "Render"
        let [<Literal>] Audio = "Audio"

    [<RequireQualifiedAccess>]
    module Assets =

        let [<Literal>] AssetGraphFilePath = "AssetGraph.nuag"
        let [<Literal>] OverlayerFilePath = "Overlayer.nuol"
        let [<Literal>] DefaultPackageName = "Default"
        let [<Literal>] DefaultImageValue = "[Default Image]"
        let [<Literal>] DefaultTileMapAssetValue = "[Default TileMap]"
        let [<Literal>] DefaultFontValue = "[Default Font]"
        let [<Literal>] DefaultSoundValue = "[Default Sound]"
        let [<Literal>] DefaultSongValue = "[Default Song]"

    [<RequireQualifiedAccess>]
    module Render =

        let [<Literal>] ResolutionXDefault = 960
        let [<Literal>] ResolutionYDefault = 544
        let ResolutionX = Core.getResolutionOrDefault true ResolutionXDefault
        let ResolutionY = Core.getResolutionOrDefault false ResolutionYDefault
        let ScreenClearing = ColorClear (255uy, 255uy, 255uy)

    [<RequireQualifiedAccess>]
    module Audio =

        let [<Literal>] Frequency = 44100
        let [<Literal>] DefaultBufferSize = 1024
        let [<Literal>] DefaultTimeToFadeOutSongMs = 500

    [<RequireQualifiedAccess>]
    module Physics =

        let PhysicsStepRate = 1.0f / single Engine.DesiredFps
        let [<Literal>] PhysicsToPixelRatio = 64.0f
        let PixelToPhysicsRatio = 1.0f / PhysicsToPixelRatio
        let [<Literal>] NormalDensity = 10.0f // NOTE: this seems to be a stable density for Farseer
        let Gravity = Vector2 (0.0f, -9.80665f) * PhysicsToPixelRatio
        let [<Literal>] CollisionProperty = "C"

    [<RequireQualifiedAccess>]
    module Effects =

        let [<Literal>] DefaultEffectHistoryMax = 300 // 5 seconds

    [<RequireQualifiedAccess>]
    module Math =

        let [<Literal>] RadiansToDegrees = 57.2957795
        let DegreesToRadians = 1.0 / RadiansToDegrees
        let RadiansToDegreesF = single RadiansToDegrees
        let DegreesToRadiansF = single DegreesToRadians