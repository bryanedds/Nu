// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open OpenTK
open Nu
module Constants =

    (* Engine Literals *)

    let [<Literal>] DesiredFps = 60
    let [<Literal>] SuccessExitCode = 0
    let [<Literal>] FailureExitCode = 1
    let [<Literal>] DefaultSubsystemOrder = 1.0f
    let [<Literal>] NameFieldName = "Name"
    let [<Literal>] RendererSubsystemName = "RendererSubsystem"
    let [<Literal>] AudioPlayerSubsystemName = "AudioPlayerSubsystem"
    let [<Literal>] IntegratorSubsystemName = "IntegratorSubsystem"
    let [<Literal>] GameNodeName = "Game"
    let [<Literal>] DefaultScreenName = "Screen"
    let [<Literal>] DefaultGroupName = "Group"
    let [<Literal>] DefaultEntityName = "Entity"
    let [<Literal>] GamePublishingPriority = Single.MaxValue
    let (*Literal*) ScreenPublishingPriority = GamePublishingPriority * 0.5f
    let (*Literal*) GroupPublishingPriority = ScreenPublishingPriority * 0.5f
    let (*Literal*) EntityPublishingPriority = GroupPublishingPriority * 0.5f
    let (*Literal*) DefaultEntitySize = Vector2 64.0f
    let (*Literal*) InvalidId = Guid.Empty

    (* Xml Literals *)

    let [<Literal>] RootNodeName = "Root"
    let [<Literal>] DispatcherNameAttributeName = "dispatcherName"
    let [<Literal>] IncludesAttributeName = "includes"
    let [<Literal>] PackageNodeName = "Package"
    let [<Literal>] CommentNodeName = "#comment"
    let [<Literal>] NameAttributeName = "name"
    let [<Literal>] FileAttributeName = "file"
    let [<Literal>] DirectoryAttributeName = "directory"
    let [<Literal>] RecursiveAttributeName = "recursive"
    let [<Literal>] ExtensionAttributeName = "extension"
    let [<Literal>] RefinementsAttributeName = "refinements"
    let [<Literal>] AssociationsAttributeName = "associations"
    let [<Literal>] AssetNodeName = "Asset"
    let [<Literal>] AssetsNodeName = "Assets"
    let [<Literal>] RenderAssociation = "Render"
    let [<Literal>] AudioAssociation = "Audio"
    let [<Literal>] ScreenNodeName = DefaultScreenName
    let [<Literal>] ScreensNodeName = DefaultScreenName + "s"
    let [<Literal>] GroupNodeName = DefaultGroupName
    let [<Literal>] GroupsNodeName = DefaultGroupName + "s"
    let [<Literal>] EntityNodeName = DefaultEntityName
    let [<Literal>] EntitiesNodeName = "Entities"

    (* Asset Literals *)

    let [<Literal>] AssetGraphFilePath = "AssetGraph.xml"
    let [<Literal>] OverlayFilePath = "Overlay.xml"
    let [<Literal>] DefaultPackageName = "Default"
    let [<Literal>] DefaultImageValue = "[Default | Image]"
    let [<Literal>] DefaultTileMapAssetValue = "[Default | TileMap]"
    let [<Literal>] DefaultFontValue = "[Default | Font]"
    let [<Literal>] DefaultSoundValue = "[Default | Sound]"
    let [<Literal>] DefaultSongValue = "[Default | Song]"

    (* Render Literals *)

    let [<Literal>] ResolutionXDefault = 960
    let [<Literal>] ResolutionYDefault = 544
    let (*Literal*) ResolutionX = Core.getResolutionOrDefault true ResolutionXDefault
    let (*Literal*) ResolutionY = Core.getResolutionOrDefault false ResolutionYDefault
    let (*Literal*) ScreenClearing = ColorClear (255uy, 255uy, 255uy)

    (* Audio Literals *)

    let [<Literal>] AudioFrequency = 44100
    let [<Literal>] AudioBufferSizeDefault = 1024
    let [<Literal>] DefaultTimeToFadeOutSongMs = 500

    (* Physics Literals *)

    let (*Literal*) PhysicsStepRate = 1.0f / single DesiredFps
    let [<Literal>] PhysicsToPixelRatio = 64.0f
    let (*Literal*) PixelToPhysicsRatio = 1.0f / PhysicsToPixelRatio
    let [<Literal>] NormalDensity = 10.0f // NOTE: this seems to be a stable density for Farseer
    let (*Literal*) Gravity = Vector2 (0.0f, -9.80665f) * PhysicsToPixelRatio
    let [<Literal>] CollisionProperty = "C"

    (* Math Literals *)

    let [<Literal>] RadiansToDegrees = 57.2957795
    let (*Literal*) DegreesToRadians = 1.0 / RadiansToDegrees
    let (*Literal*) RadiansToDegreesF = single RadiansToDegrees
    let (*Literal*) DegreesToRadiansF = single DegreesToRadians