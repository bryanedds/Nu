// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Nu
open System
open System.Collections.Generic
open OpenTK
open Prime
open Prime.Scripting
open global.Nu

[<RequireQualifiedAccess>]
module WorldScriptingBindings =

    let tryGetIsSelectedScreenIdling world =
        let _ = World.tryGetIsSelectedScreenIdling world
        world

    let tryGetIsSelectedScreenTransitioning world =
        let _ = World.tryGetIsSelectedScreenTransitioning world
        world

    let isSelectedScreenIdling world =
        let _ = World.isSelectedScreenIdling world
        world

    let isSelectedScreenTransitioning world =
        let _ = World.isSelectedScreenTransitioning world
        world

    let selectScreen screen world =
        let _ = World.selectScreen screen world
        world

    let tryTransitionScreen destination world =
        let _ = World.tryTransitionScreen destination world
        world

    let transitionScreen destinationAddress world =
        let _ = World.transitionScreen destinationAddress world
        world

    let createDissolveScreenFromLayerFile6 dispatcherName nameOpt dissolveData layerFilePath world =
        let dispatcherName = ScriptingWorld.tryExport (dispatcherName.GetType ()) dispatcherName world |> Option.get :?> String
        let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get :?> FSharpOption<String>
        let dissolveData = ScriptingWorld.tryExport (dissolveData.GetType ()) dissolveData world |> Option.get :?> DissolveData
        let layerFilePath = ScriptingWorld.tryExport (layerFilePath.GetType ()) layerFilePath world |> Option.get :?> String
        let _ = World.createDissolveScreenFromLayerFile6 dispatcherName nameOpt dissolveData layerFilePath world
        world

    let createDissolveScreenFromLayerFile nameOpt dissolveData layerFilePath world =
        let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get :?> FSharpOption<String>
        let dissolveData = ScriptingWorld.tryExport (dissolveData.GetType ()) dissolveData world |> Option.get :?> DissolveData
        let layerFilePath = ScriptingWorld.tryExport (layerFilePath.GetType ()) layerFilePath world |> Option.get :?> String
        let _ = World.createDissolveScreenFromLayerFile nameOpt dissolveData layerFilePath world
        world

    let createSplashScreen6 dispatcherName nameOpt splashData destination world =
        let dispatcherName = ScriptingWorld.tryExport (dispatcherName.GetType ()) dispatcherName world |> Option.get :?> String
        let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get :?> FSharpOption<String>
        let splashData = ScriptingWorld.tryExport (splashData.GetType ()) splashData world |> Option.get :?> SplashData
        let _ = World.createSplashScreen6 dispatcherName nameOpt splashData destination world
        world

    let createSplashScreen nameOpt splashData destination world =
        let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get :?> FSharpOption<String>
        let splashData = ScriptingWorld.tryExport (splashData.GetType ()) splashData world |> Option.get :?> SplashData
        let _ = World.createSplashScreen nameOpt splashData destination world
        world

    let tryReloadOverlays inputDirectory outputDirectory world =
        let inputDirectory = ScriptingWorld.tryExport (inputDirectory.GetType ()) inputDirectory world |> Option.get :?> String
        let outputDirectory = ScriptingWorld.tryExport (outputDirectory.GetType ()) outputDirectory world |> Option.get :?> String
        let _ = World.tryReloadOverlays inputDirectory outputDirectory world
        world

    let tryReloadPrelude inputDirectory outputDirectory world =
        let inputDirectory = ScriptingWorld.tryExport (inputDirectory.GetType ()) inputDirectory world |> Option.get :?> String
        let outputDirectory = ScriptingWorld.tryExport (outputDirectory.GetType ()) outputDirectory world |> Option.get :?> String
        let _ = World.tryReloadPrelude inputDirectory outputDirectory world
        world

    let tryReloadAssetGraph inputDirectory outputDirectory refinementDirectory world =
        let inputDirectory = ScriptingWorld.tryExport (inputDirectory.GetType ()) inputDirectory world |> Option.get :?> String
        let outputDirectory = ScriptingWorld.tryExport (outputDirectory.GetType ()) outputDirectory world |> Option.get :?> String
        let refinementDirectory = ScriptingWorld.tryExport (refinementDirectory.GetType ()) refinementDirectory world |> Option.get :?> String
        let _ = World.tryReloadAssetGraph inputDirectory outputDirectory refinementDirectory world
        world

    let getEntitiesInView selectedScreen world =
        let _ = World.getEntitiesInView selectedScreen world
        world

    let getEntitiesInBounds bounds selectedScreen world =
        let bounds = ScriptingWorld.tryExport (bounds.GetType ()) bounds world |> Option.get :?> Vector4
        let _ = World.getEntitiesInBounds bounds selectedScreen world
        world

    let getEntitiesAtPoint point selectedScreen world =
        let point = ScriptingWorld.tryExport (point.GetType ()) point world |> Option.get :?> Vector2
        let _ = World.getEntitiesAtPoint point selectedScreen world
        world

    let playSong timeToFadeOutSongMs volume song world =
        let timeToFadeOutSongMs = ScriptingWorld.tryExport (timeToFadeOutSongMs.GetType ()) timeToFadeOutSongMs world |> Option.get :?> Int32
        let volume = ScriptingWorld.tryExport (volume.GetType ()) volume world |> Option.get :?> Single
        let song = ScriptingWorld.tryExport (song.GetType ()) song world |> Option.get :?> AssetTag
        let _ = World.playSong timeToFadeOutSongMs volume song world
        world

    let playSong5 timeToFadeOutSongMs volume songPackageName songAssetName world =
        let timeToFadeOutSongMs = ScriptingWorld.tryExport (timeToFadeOutSongMs.GetType ()) timeToFadeOutSongMs world |> Option.get :?> Int32
        let volume = ScriptingWorld.tryExport (volume.GetType ()) volume world |> Option.get :?> Single
        let songPackageName = ScriptingWorld.tryExport (songPackageName.GetType ()) songPackageName world |> Option.get :?> String
        let songAssetName = ScriptingWorld.tryExport (songAssetName.GetType ()) songAssetName world |> Option.get :?> String
        let _ = World.playSong5 timeToFadeOutSongMs volume songPackageName songAssetName world
        world

    let playSound volume sound world =
        let volume = ScriptingWorld.tryExport (volume.GetType ()) volume world |> Option.get :?> Single
        let sound = ScriptingWorld.tryExport (sound.GetType ()) sound world |> Option.get :?> AssetTag
        let _ = World.playSound volume sound world
        world

    let playSound4 volume soundPackageName soundAssetName world =
        let volume = ScriptingWorld.tryExport (volume.GetType ()) volume world |> Option.get :?> Single
        let soundPackageName = ScriptingWorld.tryExport (soundPackageName.GetType ()) soundPackageName world |> Option.get :?> String
        let soundAssetName = ScriptingWorld.tryExport (soundAssetName.GetType ()) soundAssetName world |> Option.get :?> String
        let _ = World.playSound4 volume soundPackageName soundAssetName world
        world

    let fadeOutSong timeToFadeOutSongMs world =
        let timeToFadeOutSongMs = ScriptingWorld.tryExport (timeToFadeOutSongMs.GetType ()) timeToFadeOutSongMs world |> Option.get :?> Int32
        let _ = World.fadeOutSong timeToFadeOutSongMs world
        world

    let stopSong world =
        let _ = World.stopSong world
        world

    let hintAudioPackageUse packageName world =
        let packageName = ScriptingWorld.tryExport (packageName.GetType ()) packageName world |> Option.get :?> String
        let _ = World.hintAudioPackageUse packageName world
        world

    let hintAudioPackageDisuse packageName world =
        let packageName = ScriptingWorld.tryExport (packageName.GetType ()) packageName world |> Option.get :?> String
        let _ = World.hintAudioPackageDisuse packageName world
        world

    let reloadAudioAssets world =
        let _ = World.reloadAudioAssets world
        world

    let hintRenderPackageUse packageName world =
        let packageName = ScriptingWorld.tryExport (packageName.GetType ()) packageName world |> Option.get :?> String
        let _ = World.hintRenderPackageUse packageName world
        world

    let hintRenderPackageDisuse packageName world =
        let packageName = ScriptingWorld.tryExport (packageName.GetType ()) packageName world |> Option.get :?> String
        let _ = World.hintRenderPackageDisuse packageName world
        world

    let reloadRenderAssets world =
        let _ = World.reloadRenderAssets world
        world

    let bodyExists physicsId world =
        let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
        let _ = World.bodyExists physicsId world
        world

    let getBodyContactNormals physicsId world =
        let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
        let _ = World.getBodyContactNormals physicsId world
        world

    let getBodyLinearVelocity physicsId world =
        let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
        let _ = World.getBodyLinearVelocity physicsId world
        world

    let getBodyToGroundContactNormals physicsId world =
        let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
        let _ = World.getBodyToGroundContactNormals physicsId world
        world

    let getBodyToGroundContactNormalOpt physicsId world =
        let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
        let _ = World.getBodyToGroundContactNormalOpt physicsId world
        world

    let getBodyToGroundContactTangentOpt physicsId world =
        let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
        let _ = World.getBodyToGroundContactTangentOpt physicsId world
        world

    let isBodyOnGround physicsId world =
        let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
        let _ = World.isBodyOnGround physicsId world
        world

    let createBody entity entityId bodyProperties world =
        let entityId = ScriptingWorld.tryExport (entityId.GetType ()) entityId world |> Option.get :?> Guid
        let bodyProperties = ScriptingWorld.tryExport (bodyProperties.GetType ()) bodyProperties world |> Option.get :?> BodyProperties
        let _ = World.createBody entity entityId bodyProperties world
        world

    let createBodies entity entityId bodiesProperties world =
        let entityId = ScriptingWorld.tryExport (entityId.GetType ()) entityId world |> Option.get :?> Guid
        let bodiesProperties = ScriptingWorld.tryExport (bodiesProperties.GetType ()) bodiesProperties world |> Option.get :?> BodyProperties[]
        let _ = World.createBodies entity entityId bodiesProperties world
        world

    let destroyBody physicsId world =
        let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
        let _ = World.destroyBody physicsId world
        world

    let destroyBodies physicsIds world =
        let physicsIds = ScriptingWorld.tryExport (physicsIds.GetType ()) physicsIds world |> Option.get :?> PhysicsId[]
        let _ = World.destroyBodies physicsIds world
        world

    let setBodyPosition position physicsId world =
        let position = ScriptingWorld.tryExport (position.GetType ()) position world |> Option.get :?> Vector2
        let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
        let _ = World.setBodyPosition position physicsId world
        world

    let setBodyRotation rotation physicsId world =
        let rotation = ScriptingWorld.tryExport (rotation.GetType ()) rotation world |> Option.get :?> Single
        let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
        let _ = World.setBodyRotation rotation physicsId world
        world

    let setBodyAngularVelocity angularVelocity physicsId world =
        let angularVelocity = ScriptingWorld.tryExport (angularVelocity.GetType ()) angularVelocity world |> Option.get :?> Single
        let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
        let _ = World.setBodyAngularVelocity angularVelocity physicsId world
        world

    let setBodyLinearVelocity linearVelocity physicsId world =
        let linearVelocity = ScriptingWorld.tryExport (linearVelocity.GetType ()) linearVelocity world |> Option.get :?> Vector2
        let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
        let _ = World.setBodyLinearVelocity linearVelocity physicsId world
        world

    let applyBodyAngularImpulse angularImpulse physicsId world =
        let angularImpulse = ScriptingWorld.tryExport (angularImpulse.GetType ()) angularImpulse world |> Option.get :?> Single
        let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
        let _ = World.applyBodyAngularImpulse angularImpulse physicsId world
        world

    let applyBodyLinearImpulse linearImpulse physicsId world =
        let linearImpulse = ScriptingWorld.tryExport (linearImpulse.GetType ()) linearImpulse world |> Option.get :?> Vector2
        let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
        let _ = World.applyBodyLinearImpulse linearImpulse physicsId world
        world

    let applyBodyForce force physicsId world =
        let force = ScriptingWorld.tryExport (force.GetType ()) force world |> Option.get :?> Vector2
        let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
        let _ = World.applyBodyForce force physicsId world
        world

    let isMouseButtonDown mouseButton world =
        let mouseButton = ScriptingWorld.tryExport (mouseButton.GetType ()) mouseButton world |> Option.get :?> MouseButton
        let _ = World.isMouseButtonDown mouseButton world
        world

    let getMousePosition world =
        let _ = World.getMousePosition world
        world

    let getMousePositionF world =
        let _ = World.getMousePositionF world
        world

    let isKeyboardKeyDown scanCode world =
        let scanCode = ScriptingWorld.tryExport (scanCode.GetType ()) scanCode world |> Option.get :?> Int32
        let _ = World.isKeyboardKeyDown scanCode world
        world

    let getSimulantSelected simulant world =
        let _ = World.getSimulantSelected simulant world
        world

    let tryGetSimulantParent simulant world =
        let _ = World.tryGetSimulantParent simulant world
        world

    let getSimulantChildren simulant world =
        let _ = World.getSimulantChildren simulant world
        world

    let getSimulantExists simulant world =
        let _ = World.getSimulantExists simulant world
        world

    let getEntities1 world =
        let _ = World.getEntities1 world
        world

    let getLayers1 world =
        let _ = World.getLayers1 world
        world

    let isSimulantSelected simulant world =
        let _ = World.isSimulantSelected simulant world
        world

    let writeGameToFile filePath world =
        let filePath = ScriptingWorld.tryExport (filePath.GetType ()) filePath world |> Option.get :?> String
        let _ = World.writeGameToFile filePath world
        world

    let readGameFromFile filePath world =
        let filePath = ScriptingWorld.tryExport (filePath.GetType ()) filePath world |> Option.get :?> String
        let _ = World.readGameFromFile filePath world
        world

    let getScreens world =
        let _ = World.getScreens world
        world

    let destroyScreen screen world =
        let _ = World.destroyScreen screen world
        world

    let createScreen3 dispatcherName nameOpt world =
        let dispatcherName = ScriptingWorld.tryExport (dispatcherName.GetType ()) dispatcherName world |> Option.get :?> String
        let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get :?> FSharpOption<String>
        let _ = World.createScreen3 dispatcherName nameOpt world
        world

    let createDissolveScreen5 dispatcherName nameOpt dissolveData world =
        let dispatcherName = ScriptingWorld.tryExport (dispatcherName.GetType ()) dispatcherName world |> Option.get :?> String
        let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get :?> FSharpOption<String>
        let dissolveData = ScriptingWorld.tryExport (dissolveData.GetType ()) dissolveData world |> Option.get :?> DissolveData
        let _ = World.createDissolveScreen5 dispatcherName nameOpt dissolveData world
        world

    let writeScreenToFile filePath screen world =
        let filePath = ScriptingWorld.tryExport (filePath.GetType ()) filePath world |> Option.get :?> String
        let _ = World.writeScreenToFile filePath screen world
        world

    let readScreenFromFile filePath nameOpt world =
        let filePath = ScriptingWorld.tryExport (filePath.GetType ()) filePath world |> Option.get :?> String
        let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get :?> FSharpOption<String>
        let _ = World.readScreenFromFile filePath nameOpt world
        world

    let getLayers screen world =
        let _ = World.getLayers screen world
        world

    let destroyLayer layer world =
        let _ = World.destroyLayer layer world
        world

    let destroyLayers layers world =
        let layers = ScriptingWorld.tryExport (layers.GetType ()) layers world |> Option.get :?> IEnumerable<Layer>
        let _ = World.destroyLayers layers world
        world

    let writeLayerToFile filePath layer world =
        let filePath = ScriptingWorld.tryExport (filePath.GetType ()) filePath world |> Option.get :?> String
        let _ = World.writeLayerToFile filePath layer world
        world

    let readLayerFromFile filePath nameOpt screen world =
        let filePath = ScriptingWorld.tryExport (filePath.GetType ()) filePath world |> Option.get :?> String
        let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get :?> FSharpOption<String>
        let _ = World.readLayerFromFile filePath nameOpt screen world
        world

    let getEntities layer world =
        let _ = World.getEntities layer world
        world

    let destroyEntity entity world =
        let _ = World.destroyEntity entity world
        world

    let destroyEntities entities world =
        let entities = ScriptingWorld.tryExport (entities.GetType ()) entities world |> Option.get :?> IEnumerable<Entity>
        let _ = World.destroyEntities entities world
        world

    let tryPickEntity position entities world =
        let position = ScriptingWorld.tryExport (position.GetType ()) position world |> Option.get :?> Vector2
        let entities = ScriptingWorld.tryExport (entities.GetType ()) entities world |> Option.get :?> IEnumerable<Entity>
        let _ = World.tryPickEntity position entities world
        world

    let createEntity5 dispatcherName nameOpt overlayNameDescriptor layer world =
        let dispatcherName = ScriptingWorld.tryExport (dispatcherName.GetType ()) dispatcherName world |> Option.get :?> String
        let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get :?> FSharpOption<String>
        let overlayNameDescriptor = ScriptingWorld.tryExport (overlayNameDescriptor.GetType ()) overlayNameDescriptor world |> Option.get :?> OverlayNameDescriptor
        let _ = World.createEntity5 dispatcherName nameOpt overlayNameDescriptor layer world
        world

    let reassignEntity entity nameOpt layer world =
        let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get :?> FSharpOption<String>
        let _ = World.reassignEntity entity nameOpt layer world
        world

    let trySetEntityOverlayNameOpt overlayNameOpt entity world =
        let overlayNameOpt = ScriptingWorld.tryExport (overlayNameOpt.GetType ()) overlayNameOpt world |> Option.get :?> FSharpOption<String>
        let _ = World.trySetEntityOverlayNameOpt overlayNameOpt entity world
        world

    let trySetEntityFacetNames facetNames entity world =
        let facetNames = ScriptingWorld.tryExport (facetNames.GetType ()) facetNames world |> Option.get :?> FSharpSet<String>
        let _ = World.trySetEntityFacetNames facetNames entity world
        world

    let createLayer4 dispatcherName nameOpt screen world =
        let dispatcherName = ScriptingWorld.tryExport (dispatcherName.GetType ()) dispatcherName world |> Option.get :?> String
        let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get :?> FSharpOption<String>
        let _ = World.createLayer4 dispatcherName nameOpt screen world
        world

    let getEyeCenter world =
        let _ = World.getEyeCenter world
        world

    let setEyeCenter value world =
        let value = ScriptingWorld.tryExport (value.GetType ()) value world |> Option.get :?> Vector2
        let _ = World.setEyeCenter value world
        world

    let getEyeSize world =
        let _ = World.getEyeSize world
        world

    let setEyeSize value world =
        let value = ScriptingWorld.tryExport (value.GetType ()) value world |> Option.get :?> Vector2
        let _ = World.setEyeSize value world
        world

    let getSelectedScreenOpt world =
        let _ = World.getSelectedScreenOpt world
        world

    let setSelectedScreenOpt value world =
        let value = ScriptingWorld.tryExport (value.GetType ()) value world |> Option.get :?> FSharpOption<Screen>
        let _ = World.setSelectedScreenOpt value world
        world

    let getSelectedScreen world =
        let _ = World.getSelectedScreen world
        world

    let setSelectedScreen screen world =
        let _ = World.setSelectedScreen screen world
        world

    let getScreenTransitionDestinationOpt world =
        let _ = World.getScreenTransitionDestinationOpt world
        world

    let getViewAbsolute world =
        let _ = World.getViewAbsolute world
        world

    let getViewAbsoluteI world =
        let _ = World.getViewAbsoluteI world
        world

    let getViewRelative world =
        let _ = World.getViewRelative world
        world

    let getViewRelativeI world =
        let _ = World.getViewRelativeI world
        world

    let getViewBoundsRelative world =
        let _ = World.getViewBoundsRelative world
        world

    let getViewBoundsAbsolute world =
        let _ = World.getViewBoundsAbsolute world
        world

    let getViewBounds viewType world =
        let viewType = ScriptingWorld.tryExport (viewType.GetType ()) viewType world |> Option.get :?> ViewType
        let _ = World.getViewBounds viewType world
        world

    let isBoundsInView viewType bounds world =
        let viewType = ScriptingWorld.tryExport (viewType.GetType ()) viewType world |> Option.get :?> ViewType
        let bounds = ScriptingWorld.tryExport (bounds.GetType ()) bounds world |> Option.get :?> Vector4
        let _ = World.isBoundsInView viewType bounds world
        world

    let mouseToScreen mousePosition world =
        let mousePosition = ScriptingWorld.tryExport (mousePosition.GetType ()) mousePosition world |> Option.get :?> Vector2
        let _ = World.mouseToScreen mousePosition world
        world

    let mouseToWorld viewType mousePosition world =
        let viewType = ScriptingWorld.tryExport (viewType.GetType ()) viewType world |> Option.get :?> ViewType
        let mousePosition = ScriptingWorld.tryExport (mousePosition.GetType ()) mousePosition world |> Option.get :?> Vector2
        let _ = World.mouseToWorld viewType mousePosition world
        world

    let mouseToEntity viewType entityPosition mousePosition world =
        let viewType = ScriptingWorld.tryExport (viewType.GetType ()) viewType world |> Option.get :?> ViewType
        let entityPosition = ScriptingWorld.tryExport (entityPosition.GetType ()) entityPosition world |> Option.get :?> Vector2
        let mousePosition = ScriptingWorld.tryExport (mousePosition.GetType ()) mousePosition world |> Option.get :?> Vector2
        let _ = World.mouseToEntity viewType entityPosition mousePosition world
        world

    let getTickRate world =
        let _ = World.getTickRate world
        world

    let getTickRateF world =
        let _ = World.getTickRateF world
        world

    let setTickRate tickRate world =
        let tickRate = ScriptingWorld.tryExport (tickRate.GetType ()) tickRate world |> Option.get :?> Int64
        let _ = World.setTickRate tickRate world
        world

    let resetTickTime world =
        let _ = World.resetTickTime world
        world

    let getTickTime world =
        let _ = World.getTickTime world
        world

    let isTicking world =
        let _ = World.isTicking world
        world

    let getUpdateCount world =
        let _ = World.getUpdateCount world
        world

    let getLiveness world =
        let _ = World.getLiveness world
        world

    let exit world =
        let _ = World.exit world
        world

    let tryGetTextureSize assetTag world =
        let assetTag = ScriptingWorld.tryExport (assetTag.GetType ()) assetTag world |> Option.get :?> AssetTag
        let _ = World.tryGetTextureSize assetTag world
        world

    let getTextureSize assetTag world =
        let assetTag = ScriptingWorld.tryExport (assetTag.GetType ()) assetTag world |> Option.get :?> AssetTag
        let _ = World.getTextureSize assetTag world
        world

    let tryGetTextureSizeAsVector2 assetTag world =
        let assetTag = ScriptingWorld.tryExport (assetTag.GetType ()) assetTag world |> Option.get :?> AssetTag
        let _ = World.tryGetTextureSizeAsVector2 assetTag world
        world

    let getTextureSizeAsVector2 assetTag world =
        let assetTag = ScriptingWorld.tryExport (assetTag.GetType ()) assetTag world |> Option.get :?> AssetTag
        let _ = World.getTextureSizeAsVector2 assetTag world
        world

    let tryGetTileMapMetadata assetTag world =
        let assetTag = ScriptingWorld.tryExport (assetTag.GetType ()) assetTag world |> Option.get :?> AssetTag
        let _ = World.tryGetTileMapMetadata assetTag world
        world

    let getTileMapMetadata assetTag world =
        let assetTag = ScriptingWorld.tryExport (assetTag.GetType ()) assetTag world |> Option.get :?> AssetTag
        let _ = World.getTileMapMetadata assetTag world
        world
