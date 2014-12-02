// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2014.

namespace Nu
open System
open OpenTK
open Nu

module Constants =

    let [<Literal>] DefaultScreenName = "Screen"
    let [<Literal>] DefaultGroupName = "Group"
    let [<Literal>] DefaultEntityName = "Entity"
    let [<Literal>] NameFieldName = "Name"
    let [<Literal>] RootNodeName = "Root"
    let [<Literal>] TypeAttributeName = "type"
    let [<Literal>] DispatcherNameAttributeName = "dispatcherName"
    let [<Literal>] AssetGraphFilePath = "AssetGraph.xml"
    let [<Literal>] OverlayFilePath = "Overlay.xml"
    let [<Literal>] DefaultPackageName = "Default"
    let [<Literal>] IncludesAttributeName = "includes"
    let [<Literal>] DefaultImageValue = "[Image | Default | AssetGraph.xml]"
    let [<Literal>] DefaultTileMapAssetValue = "[TileMap | Default | AssetGraph.xml]"
    let [<Literal>] DefaultFontValue = "[Font | Default | AssetGraph.xml]"
    let [<Literal>] DefaultSoundValue = "[Sound | Default | AssetGraph.xml]"
    let [<Literal>] DefaultSongValue = "[Song | Default | AssetGraph.xml]"
    let [<Literal>] PackageNodeName = "Package"
    let [<Literal>] AssetNodeName = "Asset"
    let [<Literal>] AssetsNodeName = "Assets"
    let [<Literal>] GameNodeName = "Game"
    let [<Literal>] ScreenNodeName = DefaultScreenName
    let [<Literal>] ScreensNodeName = DefaultScreenName + "s"
    let [<Literal>] GroupNodeName = DefaultGroupName
    let [<Literal>] GroupsNodeName = DefaultGroupName + "s"
    let [<Literal>] EntityNodeName = DefaultEntityName
    let [<Literal>] EntitiesNodeName = "Entities"
    let [<Literal>] NameAttributeName = "name"
    let [<Literal>] FileAttributeName = "file"
    let [<Literal>] DirectoryAttributeName = "directory"
    let [<Literal>] RecursiveAttributeName = "recursive"
    let [<Literal>] ExtensionAttributeName = "extension"
    let [<Literal>] RefinementsAttributeName = "refinements"
    let [<Literal>] AssociationsAttributeName = "associations"
    let [<Literal>] RenderAssociation = "Render"
    let [<Literal>] AudioAssociation = "Audio"
    let [<Literal>] SuccessExitCode = 0
    let [<Literal>] FailureExitCode = 1
    let InvalidId = Guid.Empty
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
    let GamePublishingPriority = Single.MaxValue
    let ScreenPublishingPriority = GamePublishingPriority * 0.5f
    let GroupPublishingPriority = ScreenPublishingPriority * 0.5f
    let EntityPublishingPriority = GroupPublishingPriority * 0.5f
    let ResolutionXDefault = 960
    let ResolutionYDefault = 544
    let ResolutionX = Core.getResolutionOrDefault true ResolutionXDefault
    let ResolutionY = Core.getResolutionOrDefault false ResolutionYDefault