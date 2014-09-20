// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2014.

namespace Nu
open System
open OpenTK
open Nu

module Constants =

    let [<Literal>] NameFieldName = "Name"
    let [<Literal>] RootNodeName = "Root"
    let [<Literal>] TypeAttributeName = "type"
    let [<Literal>] DispatcherNameAttributeName = "dispatcherName"
    let [<Literal>] AssetGraphFileName = "AssetGraph.xml"
    let [<Literal>] OverlayFileName = "Overlay.xml"
    let [<Literal>] DefaultPackageName = "Default"
    let [<Literal>] IncludeAttributeName = "include"
    let [<Literal>] DefaultImageValue = "Image;Default;AssetGraph.xml"
    let [<Literal>] DefaultTileMapAssetValue = "TileMap;Default;AssetGraph.xml"
    let [<Literal>] DefaultFontValue = "Font;Default;AssetGraph.xml"
    let [<Literal>] DefaultSoundValue = "Sound;Default;AssetGraph.xml"
    let [<Literal>] DefaultSongValue = "Song;Default;AssetGraph.xml"
    let [<Literal>] PackageNodeName = "Package"
    let [<Literal>] AssetNodeName = "Asset"
    let [<Literal>] GroupNodeName = "Group"
    let [<Literal>] EntitiesNodeName = "Entities"
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
    let TickEventName = !* "Tick"
    let DownEventName = !* "Down"
    let UpEventName = !* "Up"
    let ClickEventName = !* "Click"
    let OnEventName = !* "On"
    let OffEventName = !* "Off"
    let TouchEventName = !* "Touch"
    let ReleaseEventName = !* "Release"
    let MouseDragEventName = !* "Mouse/Drag"
    let MouseMoveEventName = !* "Mouse/Move"
    let MouseLeftEventName = !* "Mouse/Left"
    let MouseCenterEventName = !* "Mouse/Center"
    let MouseRightEventName = !* "Mouse/Right"
    let DownMouseLeftEventName = ["Down"] +@ MouseLeftEventName
    let DownMouseCenterEventName = ["Down"] +@ MouseCenterEventName
    let DownMouseRightEventName = ["Down"] +@ MouseRightEventName
    let UpMouseLeftEventName = ["Up"] +@ MouseLeftEventName
    let UpMouseCenterEventName = ["Up"] +@ MouseCenterEventName
    let UpMouseRightEventName = ["Up"] +@ MouseRightEventName
    let DownMouseEventName = !* "Down/Mouse"
    let UpMouseEventName = !* "Up/Mouse"
    let DownKeyboardKeyEventName = !* "Down/KeyboardKey"
    let UpKeyboardKeyEventName = !* "Up/KeyboardKey"
    let AnyEventName = !* "*"
    let StartIncomingEventName = !* "Start/Incoming"
    let StartOutgoingEventName = !* "Start/Outgoing"
    let FinishIncomingEventName = !* "Finish/Incoming"
    let FinishOutgoingEventName = !* "Finish/Outgoing"
    let SelectEventName = !* "Select"
    let DeselectEventName = !* "Deselect"
    let CollisionEventName = !* "Collision"
    let AddEventName = !* "Add"
    let RemovingEventName = !* "Removing"
    let ChangeEventName = !* "Change"
    let GamePublishingPriority = Single.MaxValue
    let ScreenPublishingPriority = GamePublishingPriority * 0.5f
    let GroupPublishingPriority = ScreenPublishingPriority * 0.5f
    let EntityPublishingPriority = GroupPublishingPriority * 0.5f
    let ResolutionX = Core.getResolutionOrDefault true 960
    let ResolutionY = Core.getResolutionOrDefault false 544