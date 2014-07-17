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
    let DefaultTimeToFadeOutSongMs = 500
    let RadiansToDegrees = 57.2957795
    let DegreesToRadians = 1.0 / RadiansToDegrees
    let RadiansToDegreesF = single RadiansToDegrees
    let DegreesToRadiansF = single DegreesToRadians
    let DefaultEntitySize = Vector2 64.0f
    let TickEventName = addr "Tick"
    let MouseDragEventName = addr "Mouse/Drag"
    let MouseMoveEventName = addr "Mouse/Move"
    let MouseLeftEventName = addr "Mouse/Left"
    let MouseCenterEventName = addr "Mouse/Center"
    let MouseRightEventName = addr "Mouse/Right"
    let DownMouseLeftEventName = straddr "Down" MouseLeftEventName
    let DownMouseCenterEventName = straddr "Down" MouseCenterEventName
    let DownMouseRightEventName = straddr "Down" MouseRightEventName
    let UpMouseLeftEventName = straddr "Up" MouseLeftEventName
    let UpMouseCenterEventName = straddr "Up" MouseCenterEventName
    let UpMouseRightEventName = straddr "Up" MouseRightEventName
    let DownMouseEventName = addr "Down/Mouse"
    let UpMouseEventName = addr "Up/Mouse"
    let AnyEventName = addr "*"
    let StartIncomingEventName = addr "Start/Incoming"
    let StartOutgoingEventName = addr "Start/Outgoing"
    let FinishIncomingEventName = addr "Finish/Incoming"
    let FinishOutgoingEventName = addr "Finish/Outgoing"
    let SelectEventName = addr "Select"
    let DeselectEventName = addr "Deselect"
    let CollisionEventName = addr "Collision"
    let AddedEventName = addr "Added"
    let RemovingEventName = addr "Removing"
    let GamePublishingPriority = Single.MaxValue
    let ScreenPublishingPriority = GamePublishingPriority * 0.5f
    let GroupPublishingPriority = ScreenPublishingPriority * 0.5f
    let EntityPublishingPriority = GroupPublishingPriority * 0.5f
    let ResolutionX = NuCore.getResolutionOrDefault true 960
    let ResolutionY = NuCore.getResolutionOrDefault false 544