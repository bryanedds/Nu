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
       world

   let createDissolveScreenFromLayerFile nameOpt dissolveData layerFilePath world =
       world

   let createSplashScreen6 dispatcherName nameOpt splashData destination world =
       world

   let createSplashScreen nameOpt splashData destination world =
       world

   let tryReloadOverlays inputDirectory outputDirectory world =
       world

   let tryReloadPrelude inputDirectory outputDirectory world =
       world

   let tryReloadAssetGraph inputDirectory outputDirectory refinementDirectory world =
       world

   let getEntitiesInView selectedScreen world =
       world

   let getEntitiesInBounds bounds selectedScreen world =
       world

   let getEntitiesAtPoint point selectedScreen world =
       world

   let playSong timeToFadeOutSongMs volume song world =
       world

   let playSong5 timeToFadeOutSongMs volume songPackageName songAssetName world =
       world

   let playSound volume sound world =
       world

   let playSound4 volume soundPackageName soundAssetName world =
       world

   let fadeOutSong timeToFadeOutSongMs world =
       world

   let stopSong world =
       world

   let hintAudioPackageUse packageName world =
       world

   let hintAudioPackageDisuse packageName world =
       world

   let reloadAudioAssets world =
       world

   let hintRenderPackageUse packageName world =
       world

   let hintRenderPackageDisuse packageName world =
       world

   let reloadRenderAssets world =
       world

   let bodyExists physicsId world =
       world

   let getBodyContactNormals physicsId world =
       world

   let getBodyLinearVelocity physicsId world =
       world

   let getBodyToGroundContactNormals physicsId world =
       world

   let getBodyToGroundContactNormalOpt physicsId world =
       world

   let getBodyToGroundContactTangentOpt physicsId world =
       world

   let isBodyOnGround physicsId world =
       world

   let createBody entity entityId bodyProperties world =
       world

   let createBodies entity entityId bodiesProperties world =
       world

   let destroyBody physicsId world =
       world

   let destroyBodies physicsIds world =
       world

   let setBodyPosition position physicsId world =
       world

   let setBodyRotation rotation physicsId world =
       world

   let setBodyAngularVelocity angularVelocity physicsId world =
       world

   let setBodyLinearVelocity linearVelocity physicsId world =
       world

   let applyBodyAngularImpulse angularImpulse physicsId world =
       world

   let applyBodyLinearImpulse linearImpulse physicsId world =
       world

   let applyBodyForce force physicsId world =
       world

   let isMouseButtonDown mouseButton world =
       world

   let getMousePosition world =
       world

   let getMousePositionF world =
       world

   let isKeyboardKeyDown scanCode world =
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
       world

   let readGameFromFile filePath world =
       world

   let getScreens world =
       world

   let destroyScreen screen world =
       world

   let createScreen3 dispatcherName nameOpt world =
       world

   let createDissolveScreen5 dispatcherName nameOpt dissolveData world =
       world

   let writeScreenToFile filePath screen world =
       world

   let readScreenFromFile filePath nameOpt world =
       world

   let getLayers screen world =
       world

   let destroyLayer layer world =
       world

   let destroyLayers layers world =
       world

   let writeLayerToFile filePath layer world =
       world

   let readLayerFromFile filePath nameOpt screen world =
       world

   let getEntities layer world =
       world

   let destroyEntity entity world =
       world

   let destroyEntities entities world =
       world

   let tryPickEntity position entities world =
       world

   let createEntity5 dispatcherName nameOpt overlayNameDescriptor layer world =
       world

   let reassignEntity entity nameOpt layer world =
       world

   let trySetEntityOverlayNameOpt overlayNameOpt entity world =
       world

   let trySetEntityFacetNames facetNames entity world =
       world

   let createLayer4 dispatcherName nameOpt screen world =
       world

   let getEyeCenter world =
       world

   let setEyeCenter value world =
       world

   let getEyeSize world =
       world

   let setEyeSize value world =
       world

   let getSelectedScreenOpt world =
       world

   let setSelectedScreenOpt value world =
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
       world

   let isBoundsInView viewType bounds world =
       world

   let mouseToScreen mousePosition world =
       world

   let mouseToStatic viewType mousePosition world =
       world

   let mouseToEntity viewType entityPosition mousePosition world =
       world

   let getTickRate world =
       world

   let getTickRateF world =
       world

   let setTickRate tickRate world =
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

   let addTasklet tasklet world =
       world

   let addTasklets tasklets world =
       world

   let schedule fn time world =
       world

   let schedule2 fn world =
       world

   let tryGetTextureSize assetTag world =
       world

   let getTextureSize assetTag world =
       world

   let tryGetTextureSizeAsVector2 assetTag world =
       world

   let getTextureSizeAsVector2 assetTag world =
       world

   let tryGetTileMapMetadata assetTag world =
       world

   let getTileMapMetadata assetTag world =
       world
