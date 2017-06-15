// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Nu
open System
open System.Collections.Generic
open OpenTK
open Prime
open Prime.Scripting
open global.Nu
#nowarn "1182"

module WorldScriptingBindings =

   let tryGetIsSelectedScreenIdling world =
       world

   let tryGetIsSelectedScreenTransitioning world =
       world

   let isSelectedScreenIdling world =
       world

   let isSelectedScreenTransitioning world =
       world

   let selectScreen screen world =
       world

   let tryTransitionScreen destination world =
       world

   let transitionScreen destinationAddress world =
       world

   let createDissolveScreenFromLayerFile6 dispatcherName nameOpt dissolveData layerFilePath world =
       let dispatcherName = ScriptingWorld.tryExport (dispatcherName.GetType ()) dispatcherName world |> Option.get
       let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get
       let dissolveData = ScriptingWorld.tryExport (dissolveData.GetType ()) dissolveData world |> Option.get
       let layerFilePath = ScriptingWorld.tryExport (layerFilePath.GetType ()) layerFilePath world |> Option.get
       world

   let createDissolveScreenFromLayerFile nameOpt dissolveData layerFilePath world =
       let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get
       let dissolveData = ScriptingWorld.tryExport (dissolveData.GetType ()) dissolveData world |> Option.get
       let layerFilePath = ScriptingWorld.tryExport (layerFilePath.GetType ()) layerFilePath world |> Option.get
       world

   let createSplashScreen6 dispatcherName nameOpt splashData destination world =
       let dispatcherName = ScriptingWorld.tryExport (dispatcherName.GetType ()) dispatcherName world |> Option.get
       let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get
       let splashData = ScriptingWorld.tryExport (splashData.GetType ()) splashData world |> Option.get
       world

   let createSplashScreen nameOpt splashData destination world =
       let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get
       let splashData = ScriptingWorld.tryExport (splashData.GetType ()) splashData world |> Option.get
       world

   let tryReloadOverlays inputDirectory outputDirectory world =
       let inputDirectory = ScriptingWorld.tryExport (inputDirectory.GetType ()) inputDirectory world |> Option.get
       let outputDirectory = ScriptingWorld.tryExport (outputDirectory.GetType ()) outputDirectory world |> Option.get
       world

   let tryReloadPrelude inputDirectory outputDirectory world =
       let inputDirectory = ScriptingWorld.tryExport (inputDirectory.GetType ()) inputDirectory world |> Option.get
       let outputDirectory = ScriptingWorld.tryExport (outputDirectory.GetType ()) outputDirectory world |> Option.get
       world

   let tryReloadAssetGraph inputDirectory outputDirectory refinementDirectory world =
       let inputDirectory = ScriptingWorld.tryExport (inputDirectory.GetType ()) inputDirectory world |> Option.get
       let outputDirectory = ScriptingWorld.tryExport (outputDirectory.GetType ()) outputDirectory world |> Option.get
       let refinementDirectory = ScriptingWorld.tryExport (refinementDirectory.GetType ()) refinementDirectory world |> Option.get
       world

   let getEntitiesInView selectedScreen world =
       world

   let getEntitiesInBounds bounds selectedScreen world =
       let bounds = ScriptingWorld.tryExport (bounds.GetType ()) bounds world |> Option.get
       world

   let getEntitiesAtPoint point selectedScreen world =
       let point = ScriptingWorld.tryExport (point.GetType ()) point world |> Option.get
       world

   let playSong timeToFadeOutSongMs volume song world =
       let timeToFadeOutSongMs = ScriptingWorld.tryExport (timeToFadeOutSongMs.GetType ()) timeToFadeOutSongMs world |> Option.get
       let volume = ScriptingWorld.tryExport (volume.GetType ()) volume world |> Option.get
       let song = ScriptingWorld.tryExport (song.GetType ()) song world |> Option.get
       world

   let playSong5 timeToFadeOutSongMs volume songPackageName songAssetName world =
       let timeToFadeOutSongMs = ScriptingWorld.tryExport (timeToFadeOutSongMs.GetType ()) timeToFadeOutSongMs world |> Option.get
       let volume = ScriptingWorld.tryExport (volume.GetType ()) volume world |> Option.get
       let songPackageName = ScriptingWorld.tryExport (songPackageName.GetType ()) songPackageName world |> Option.get
       let songAssetName = ScriptingWorld.tryExport (songAssetName.GetType ()) songAssetName world |> Option.get
       world

   let playSound volume sound world =
       let volume = ScriptingWorld.tryExport (volume.GetType ()) volume world |> Option.get
       let sound = ScriptingWorld.tryExport (sound.GetType ()) sound world |> Option.get
       world

   let playSound4 volume soundPackageName soundAssetName world =
       let volume = ScriptingWorld.tryExport (volume.GetType ()) volume world |> Option.get
       let soundPackageName = ScriptingWorld.tryExport (soundPackageName.GetType ()) soundPackageName world |> Option.get
       let soundAssetName = ScriptingWorld.tryExport (soundAssetName.GetType ()) soundAssetName world |> Option.get
       world

   let fadeOutSong timeToFadeOutSongMs world =
       let timeToFadeOutSongMs = ScriptingWorld.tryExport (timeToFadeOutSongMs.GetType ()) timeToFadeOutSongMs world |> Option.get
       world

   let stopSong world =
       world

   let hintAudioPackageUse packageName world =
       let packageName = ScriptingWorld.tryExport (packageName.GetType ()) packageName world |> Option.get
       world

   let hintAudioPackageDisuse packageName world =
       let packageName = ScriptingWorld.tryExport (packageName.GetType ()) packageName world |> Option.get
       world

   let reloadAudioAssets world =
       world

   let hintRenderPackageUse packageName world =
       let packageName = ScriptingWorld.tryExport (packageName.GetType ()) packageName world |> Option.get
       world

   let hintRenderPackageDisuse packageName world =
       let packageName = ScriptingWorld.tryExport (packageName.GetType ()) packageName world |> Option.get
       world

   let reloadRenderAssets world =
       world

   let bodyExists physicsId world =
       let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get
       world

   let getBodyContactNormals physicsId world =
       let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get
       world

   let getBodyLinearVelocity physicsId world =
       let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get
       world

   let getBodyToGroundContactNormals physicsId world =
       let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get
       world

   let getBodyToGroundContactNormalOpt physicsId world =
       let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get
       world

   let getBodyToGroundContactTangentOpt physicsId world =
       let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get
       world

   let isBodyOnGround physicsId world =
       let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get
       world

   let createBody entity entityId bodyProperties world =
       let entityId = ScriptingWorld.tryExport (entityId.GetType ()) entityId world |> Option.get
       let bodyProperties = ScriptingWorld.tryExport (bodyProperties.GetType ()) bodyProperties world |> Option.get
       world

   let createBodies entity entityId bodiesProperties world =
       let entityId = ScriptingWorld.tryExport (entityId.GetType ()) entityId world |> Option.get
       let bodiesProperties = ScriptingWorld.tryExport (bodiesProperties.GetType ()) bodiesProperties world |> Option.get
       world

   let destroyBody physicsId world =
       let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get
       world

   let destroyBodies physicsIds world =
       let physicsIds = ScriptingWorld.tryExport (physicsIds.GetType ()) physicsIds world |> Option.get
       world

   let setBodyPosition position physicsId world =
       let position = ScriptingWorld.tryExport (position.GetType ()) position world |> Option.get
       let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get
       world

   let setBodyRotation rotation physicsId world =
       let rotation = ScriptingWorld.tryExport (rotation.GetType ()) rotation world |> Option.get
       let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get
       world

   let setBodyAngularVelocity angularVelocity physicsId world =
       let angularVelocity = ScriptingWorld.tryExport (angularVelocity.GetType ()) angularVelocity world |> Option.get
       let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get
       world

   let setBodyLinearVelocity linearVelocity physicsId world =
       let linearVelocity = ScriptingWorld.tryExport (linearVelocity.GetType ()) linearVelocity world |> Option.get
       let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get
       world

   let applyBodyAngularImpulse angularImpulse physicsId world =
       let angularImpulse = ScriptingWorld.tryExport (angularImpulse.GetType ()) angularImpulse world |> Option.get
       let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get
       world

   let applyBodyLinearImpulse linearImpulse physicsId world =
       let linearImpulse = ScriptingWorld.tryExport (linearImpulse.GetType ()) linearImpulse world |> Option.get
       let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get
       world

   let applyBodyForce force physicsId world =
       let force = ScriptingWorld.tryExport (force.GetType ()) force world |> Option.get
       let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get
       world

   let isMouseButtonDown mouseButton world =
       let mouseButton = ScriptingWorld.tryExport (mouseButton.GetType ()) mouseButton world |> Option.get
       world

   let getMousePosition world =
       world

   let getMousePositionF world =
       world

   let isKeyboardKeyDown scanCode world =
       let scanCode = ScriptingWorld.tryExport (scanCode.GetType ()) scanCode world |> Option.get
       world

   let getSimulantSelected simulant world =
       world

   let tryGetSimulantParent simulant world =
       world

   let getSimulantChildren simulant world =
       world

   let getSimulantExists simulant world =
       world

   let getEntities1 world =
       world

   let getLayers1 world =
       world

   let isSimulantSelected simulant world =
       world

   let writeGameToFile filePath world =
       let filePath = ScriptingWorld.tryExport (filePath.GetType ()) filePath world |> Option.get
       world

   let readGameFromFile filePath world =
       let filePath = ScriptingWorld.tryExport (filePath.GetType ()) filePath world |> Option.get
       world

   let getScreens world =
       world

   let destroyScreen screen world =
       world

   let createScreen3 dispatcherName nameOpt world =
       let dispatcherName = ScriptingWorld.tryExport (dispatcherName.GetType ()) dispatcherName world |> Option.get
       let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get
       world

   let createDissolveScreen5 dispatcherName nameOpt dissolveData world =
       let dispatcherName = ScriptingWorld.tryExport (dispatcherName.GetType ()) dispatcherName world |> Option.get
       let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get
       let dissolveData = ScriptingWorld.tryExport (dissolveData.GetType ()) dissolveData world |> Option.get
       world

   let writeScreenToFile filePath screen world =
       let filePath = ScriptingWorld.tryExport (filePath.GetType ()) filePath world |> Option.get
       world

   let readScreenFromFile filePath nameOpt world =
       let filePath = ScriptingWorld.tryExport (filePath.GetType ()) filePath world |> Option.get
       let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get
       world

   let getLayers screen world =
       world

   let destroyLayer layer world =
       world

   let destroyLayers layers world =
       let layers = ScriptingWorld.tryExport (layers.GetType ()) layers world |> Option.get
       world

   let writeLayerToFile filePath layer world =
       let filePath = ScriptingWorld.tryExport (filePath.GetType ()) filePath world |> Option.get
       world

   let readLayerFromFile filePath nameOpt screen world =
       let filePath = ScriptingWorld.tryExport (filePath.GetType ()) filePath world |> Option.get
       let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get
       world

   let getEntities layer world =
       world

   let destroyEntity entity world =
       world

   let destroyEntities entities world =
       let entities = ScriptingWorld.tryExport (entities.GetType ()) entities world |> Option.get
       world

   let tryPickEntity position entities world =
       let position = ScriptingWorld.tryExport (position.GetType ()) position world |> Option.get
       let entities = ScriptingWorld.tryExport (entities.GetType ()) entities world |> Option.get
       world

   let createEntity5 dispatcherName nameOpt overlayNameDescriptor layer world =
       let dispatcherName = ScriptingWorld.tryExport (dispatcherName.GetType ()) dispatcherName world |> Option.get
       let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get
       let overlayNameDescriptor = ScriptingWorld.tryExport (overlayNameDescriptor.GetType ()) overlayNameDescriptor world |> Option.get
       world

   let reassignEntity entity nameOpt layer world =
       let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get
       world

   let trySetEntityOverlayNameOpt overlayNameOpt entity world =
       let overlayNameOpt = ScriptingWorld.tryExport (overlayNameOpt.GetType ()) overlayNameOpt world |> Option.get
       world

   let trySetEntityFacetNames facetNames entity world =
       let facetNames = ScriptingWorld.tryExport (facetNames.GetType ()) facetNames world |> Option.get
       world

   let createLayer4 dispatcherName nameOpt screen world =
       let dispatcherName = ScriptingWorld.tryExport (dispatcherName.GetType ()) dispatcherName world |> Option.get
       let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get
       world

   let getEyeCenter world =
       world

   let setEyeCenter value world =
       let value = ScriptingWorld.tryExport (value.GetType ()) value world |> Option.get
       world

   let getEyeSize world =
       world

   let setEyeSize value world =
       let value = ScriptingWorld.tryExport (value.GetType ()) value world |> Option.get
       world

   let getSelectedScreenOpt world =
       world

   let setSelectedScreenOpt value world =
       let value = ScriptingWorld.tryExport (value.GetType ()) value world |> Option.get
       world

   let getSelectedScreen world =
       world

   let setSelectedScreen screen world =
       world

   let getScreenTransitionDestinationOpt world =
       world

   let getViewAbsolute world =
       world

   let getViewAbsoluteI world =
       world

   let getViewRelative world =
       world

   let getViewRelativeI world =
       world

   let getViewBoundsRelative world =
       world

   let getViewBoundsAbsolute world =
       world

   let getViewBounds viewType world =
       let viewType = ScriptingWorld.tryExport (viewType.GetType ()) viewType world |> Option.get
       world

   let isBoundsInView viewType bounds world =
       let viewType = ScriptingWorld.tryExport (viewType.GetType ()) viewType world |> Option.get
       let bounds = ScriptingWorld.tryExport (bounds.GetType ()) bounds world |> Option.get
       world

   let mouseToScreen mousePosition world =
       let mousePosition = ScriptingWorld.tryExport (mousePosition.GetType ()) mousePosition world |> Option.get
       world

   let mouseToStatic viewType mousePosition world =
       let viewType = ScriptingWorld.tryExport (viewType.GetType ()) viewType world |> Option.get
       let mousePosition = ScriptingWorld.tryExport (mousePosition.GetType ()) mousePosition world |> Option.get
       world

   let mouseToEntity viewType entityPosition mousePosition world =
       let viewType = ScriptingWorld.tryExport (viewType.GetType ()) viewType world |> Option.get
       let entityPosition = ScriptingWorld.tryExport (entityPosition.GetType ()) entityPosition world |> Option.get
       let mousePosition = ScriptingWorld.tryExport (mousePosition.GetType ()) mousePosition world |> Option.get
       world

   let getTickRate world =
       world

   let getTickRateF world =
       world

   let setTickRate tickRate world =
       let tickRate = ScriptingWorld.tryExport (tickRate.GetType ()) tickRate world |> Option.get
       world

   let resetTickTime world =
       world

   let getTickTime world =
       world

   let isTicking world =
       world

   let getUpdateCount world =
       world

   let getLiveness world =
       world

   let exit world =
       world

   let tryGetTextureSize assetTag world =
       let assetTag = ScriptingWorld.tryExport (assetTag.GetType ()) assetTag world |> Option.get
       world

   let getTextureSize assetTag world =
       let assetTag = ScriptingWorld.tryExport (assetTag.GetType ()) assetTag world |> Option.get
       world

   let tryGetTextureSizeAsVector2 assetTag world =
       let assetTag = ScriptingWorld.tryExport (assetTag.GetType ()) assetTag world |> Option.get
       world

   let getTextureSizeAsVector2 assetTag world =
       let assetTag = ScriptingWorld.tryExport (assetTag.GetType ()) assetTag world |> Option.get
       world

   let tryGetTileMapMetadata assetTag world =
       let assetTag = ScriptingWorld.tryExport (assetTag.GetType ()) assetTag world |> Option.get
       world

   let getTileMapMetadata assetTag world =
       let assetTag = ScriptingWorld.tryExport (assetTag.GetType ()) assetTag world |> Option.get
       world
