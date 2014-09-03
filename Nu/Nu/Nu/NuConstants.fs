// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2014.

namespace Nu
open System
open OpenTK
open Prime.PrimeConstants
open Nu

module NuConstants =

    let [<Literal>] AssetGraphFileName = "AssetGraph.xml"
    let [<Literal>] OverlayFileName = "Overlay.xml"
    let [<Literal>] DefaultPackageName = "Default"
    let [<Literal>] IncludeAttributeName = "include"
    let [<Literal>] DefaultImageValue = "Image;Default;AssetGraph.xml"
    let [<Literal>] DefaultTileMapAssetValue = "TileMap;Default;AssetGraph.xml"
    let [<Literal>] DefaultFontValue = "Font;Default;AssetGraph.xml"
    let [<Literal>] DefaultSoundValue = "Sound;Default;AssetGraph.xml"
    let [<Literal>] DefaultSongValue = "Song;Default;AssetGraph.xml"
    let [<Literal>] RootNodeName = RootNodeName
    let [<Literal>] PackageNodeName = "Package"
    let [<Literal>] AssetNodeName = "Asset"
    let [<Literal>] GroupNodeName = "Group"
    let [<Literal>] EntityNodeName = "Entity"
    let [<Literal>] NameAttributeName = "name"
    let [<Literal>] FileNameAttributeName = "fileName"
    let [<Literal>] AssociationsAttributeName = "associations"
    let [<Literal>] RenderingAssociation = "Rendering"
    let [<Literal>] AudioAssociation = "Audio"
    let [<Literal>] SuccessReturnCode = 0
    let [<Literal>] FailureReturnCode = 1
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
    let DownEventName = addr "Down"
    let UpEventName = addr "Up"
    let ClickEventName = addr "Click"
    let OnEventName = addr "On"
    let OffEventName = addr "Off"
    let TouchEventName = addr "Touch"
    let ReleaseEventName = addr "Release"
    let MouseDragEventName = addr "Mouse/Drag"
    let MouseMoveEventName = addr "Mouse/Move"
    let MouseLeftEventName = addr "Mouse/Left"
    let MouseCenterEventName = addr "Mouse/Center"
    let MouseRightEventName = addr "Mouse/Right"
    let DownMouseLeftEventName = listaddr ["Down"] MouseLeftEventName
    let DownMouseCenterEventName = listaddr ["Down"] MouseCenterEventName
    let DownMouseRightEventName = listaddr ["Down"] MouseRightEventName
    let UpMouseLeftEventName = listaddr ["Up"] MouseLeftEventName
    let UpMouseCenterEventName = listaddr ["Up"] MouseCenterEventName
    let UpMouseRightEventName = listaddr ["Up"] MouseRightEventName
    let DownMouseEventName = addr "Down/Mouse"
    let UpMouseEventName = addr "Up/Mouse"
    let DownKeyboardKeyEventName = addr "Down/KeyboardKey"
    let UpKeyboardKeyEventName = addr "Up/KeyboardKey"
    let AnyEventName = addr "*"
    let StartIncomingEventName = addr "Start/Incoming"
    let StartOutgoingEventName = addr "Start/Outgoing"
    let FinishIncomingEventName = addr "Finish/Incoming"
    let FinishOutgoingEventName = addr "Finish/Outgoing"
    let SelectEventName = addr "Select"
    let DeselectEventName = addr "Deselect"
    let CollisionEventName = addr "Collision"
    let AddEventName = addr "Add"
    let RemovingEventName = addr "Removing"
    let ChangeEventName = addr "Change"
    let GamePublishingPriority = Single.MaxValue
    let ScreenPublishingPriority = GamePublishingPriority * 0.5f
    let GroupPublishingPriority = ScreenPublishingPriority * 0.5f
    let EntityPublishingPriority = GroupPublishingPriority * 0.5f
    let ResolutionX = NuCore.getResolutionOrDefault true 960
    let ResolutionY = NuCore.getResolutionOrDefault false 544