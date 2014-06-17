// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2014.

namespace Nu
open System
open OpenTK
open Nu

module NuConstants =

    let [<Literal>] AssetGraphFileName = "AssetGraph.xml"
    let [<Literal>] DefaultPackageName = "Default"
    let [<Literal>] DefaultSpriteValue = "Image;Default;AssetGraph.xml"
    let [<Literal>] DefaultTileMapAssetValue = "TileMap;Default;AssetGraph.xml"
    let [<Literal>] DefaultFontValue = "Font;Default;AssetGraph.xml"
    let [<Literal>] DefaultSoundValue = "Sound;Default;AssetGraph.xml"
    let [<Literal>] DefaultSongValue = "Song;Default;AssetGraph.xml"
    let DesiredFps = 60
    let ScreenClearing = ColorClear (255uy, 255uy, 255uy)
    let PhysicsStepRate = 1.0f / single DesiredFps
    let PhysicsToPixelRatio = 64.0f
    let PixelToPhysicsRatio = 1.0f / PhysicsToPixelRatio
    let AudioFrequency = 44100
    let AudioBufferSizeDefault = 1024
    let NormalDensity = 10.0f // NOTE: this seems to be a stable density for Farseer
    let Gravity = Vector2 (0.0f, -9.80665f) * PhysicsToPixelRatio
    let CollisionProperty = "C"
    let TimeToFadeOutSongMs = 5000
    let RadiansToDegrees = 57.2957795
    let DegreesToRadians = 1.0 / RadiansToDegrees
    let RadiansToDegreesF = single RadiansToDegrees
    let DegreesToRadiansF = single DegreesToRadians
    let DefaultEntitySize = Vector2 64.0f
    let DefaultEntityRotation = 0.0f
    let TickEvent = addr "Tick"
    let MouseDragEvent = addr "Mouse/Drag"
    let MouseMoveEvent = addr "Mouse/Move"
    let MouseLeftEvent = addr "Mouse/Left"
    let MouseCenterEvent = addr "Mouse/Center"
    let MouseRightAddress = addr "Mouse/Right"
    let DownMouseLeftEvent = straddr "Down" MouseLeftEvent
    let DownMouseCenterEvent = straddr "Down" MouseCenterEvent
    let DownMouseRightEvent = straddr "Down" MouseRightAddress
    let UpMouseLeftEvent = straddr "Up" MouseLeftEvent
    let UpMouseCenterEvent = straddr "Up" MouseCenterEvent
    let UpMouseRightEvent = straddr "Up" MouseRightAddress
    let DownMouseEvent = addr "Down/Mouse"
    let UpMouseEvent = addr "Up/Mouse"
    let FinishedIncomingEvent = addr "Finished/Incoming"
    let FinishedOutgoingEvent = addr "Finished/Outgoing"
    let CollisionEvent = addr "Collision"
    let GamePublishingPriority = Single.MaxValue
    let ScreenPublishingPriority = GamePublishingPriority * 0.5f
    let GroupPublishingPriority = ScreenPublishingPriority * 0.5f
    let ResolutionX = NuCore.getResolutionOrDefault true 960
    let ResolutionY = NuCore.getResolutionOrDefault false 544