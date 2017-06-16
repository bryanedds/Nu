// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Nu
open System
open System.Collections.Generic
open OpenTK
open Prime
open global.Nu

[<RequireQualifiedAccess>]
module WorldScriptingBindings =

    let tryGetIsSelectedScreenIdling world =
        let oldWorld = world
        try
            let result = World.tryGetIsSelectedScreenIdling world
            let value = result
            let value = ScriptingWorld.tryImport typeof<FSharpOption<Boolean>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'tryGetIsSelectedScreenIdling' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let tryGetIsSelectedScreenTransitioning world =
        let oldWorld = world
        try
            let result = World.tryGetIsSelectedScreenTransitioning world
            let value = result
            let value = ScriptingWorld.tryImport typeof<FSharpOption<Boolean>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'tryGetIsSelectedScreenTransitioning' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let isSelectedScreenIdling world =
        let oldWorld = world
        try
            let result = World.isSelectedScreenIdling world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Boolean> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'isSelectedScreenIdling' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let isSelectedScreenTransitioning world =
        let oldWorld = world
        try
            let result = World.isSelectedScreenTransitioning world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Boolean> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'isSelectedScreenTransitioning' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let selectScreen screen world =
        let oldWorld = world
        try
            let struct (screen, world) =
                let context = World.getScriptContext world
                match World.evalInternal screen world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (Screen address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.selectScreen screen world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'selectScreen' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let tryTransitionScreen destination world =
        let oldWorld = world
        try
            let struct (destination, world) =
                let context = World.getScriptContext world
                match World.evalInternal destination world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (Screen address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.tryTransitionScreen destination world
            let value = result
            let value = ScriptingWorld.tryImport typeof<FSharpOption<World>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'tryTransitionScreen' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let transitionScreen destination world =
        let oldWorld = world
        try
            let struct (destination, world) =
                let context = World.getScriptContext world
                match World.evalInternal destination world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (Screen address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.transitionScreen destination world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'transitionScreen' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let createDissolveScreenFromLayerFile6 dispatcherName nameOpt dissolveData layerFilePath world =
        let oldWorld = world
        try
            let dispatcherName = ScriptingWorld.tryExport (dispatcherName.GetType ()) dispatcherName world |> Option.get :?> String
            let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get :?> FSharpOption<String>
            let dissolveData = ScriptingWorld.tryExport (dissolveData.GetType ()) dissolveData world |> Option.get :?> DissolveData
            let layerFilePath = ScriptingWorld.tryExport (layerFilePath.GetType ()) layerFilePath world |> Option.get :?> String
            let result = World.createDissolveScreenFromLayerFile6 dispatcherName nameOpt dissolveData layerFilePath world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Tuple<Screen, World>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'createDissolveScreenFromLayerFile6' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let createDissolveScreenFromLayerFile nameOpt dissolveData layerFilePath world =
        let oldWorld = world
        try
            let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get :?> FSharpOption<String>
            let dissolveData = ScriptingWorld.tryExport (dissolveData.GetType ()) dissolveData world |> Option.get :?> DissolveData
            let layerFilePath = ScriptingWorld.tryExport (layerFilePath.GetType ()) layerFilePath world |> Option.get :?> String
            let result = World.createDissolveScreenFromLayerFile nameOpt dissolveData layerFilePath world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Tuple<Screen, World>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'createDissolveScreenFromLayerFile' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let createSplashScreen6 dispatcherName nameOpt splashData destination world =
        let oldWorld = world
        try
            let dispatcherName = ScriptingWorld.tryExport (dispatcherName.GetType ()) dispatcherName world |> Option.get :?> String
            let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get :?> FSharpOption<String>
            let splashData = ScriptingWorld.tryExport (splashData.GetType ()) splashData world |> Option.get :?> SplashData
            let struct (destination, world) =
                let context = World.getScriptContext world
                match World.evalInternal destination world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (Screen address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.createSplashScreen6 dispatcherName nameOpt splashData destination world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Tuple<Screen, World>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'createSplashScreen6' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let createSplashScreen nameOpt splashData destination world =
        let oldWorld = world
        try
            let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get :?> FSharpOption<String>
            let splashData = ScriptingWorld.tryExport (splashData.GetType ()) splashData world |> Option.get :?> SplashData
            let struct (destination, world) =
                let context = World.getScriptContext world
                match World.evalInternal destination world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (Screen address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.createSplashScreen nameOpt splashData destination world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Tuple<Screen, World>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'createSplashScreen' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let tryReloadOverlays inputDirectory outputDirectory world =
        let oldWorld = world
        try
            let inputDirectory = ScriptingWorld.tryExport (inputDirectory.GetType ()) inputDirectory world |> Option.get :?> String
            let outputDirectory = ScriptingWorld.tryExport (outputDirectory.GetType ()) outputDirectory world |> Option.get :?> String
            let result = World.tryReloadOverlays inputDirectory outputDirectory world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Either<Tuple<String, World>, Tuple<Overlayer, World>>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'tryReloadOverlays' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let tryReloadPrelude inputDirectory outputDirectory world =
        let oldWorld = world
        try
            let inputDirectory = ScriptingWorld.tryExport (inputDirectory.GetType ()) inputDirectory world |> Option.get :?> String
            let outputDirectory = ScriptingWorld.tryExport (outputDirectory.GetType ()) outputDirectory world |> Option.get :?> String
            let result = World.tryReloadPrelude inputDirectory outputDirectory world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Either<Tuple<String, World>, Tuple<String, World>>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'tryReloadPrelude' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let tryReloadAssetGraph inputDirectory outputDirectory refinementDirectory world =
        let oldWorld = world
        try
            let inputDirectory = ScriptingWorld.tryExport (inputDirectory.GetType ()) inputDirectory world |> Option.get :?> String
            let outputDirectory = ScriptingWorld.tryExport (outputDirectory.GetType ()) outputDirectory world |> Option.get :?> String
            let refinementDirectory = ScriptingWorld.tryExport (refinementDirectory.GetType ()) refinementDirectory world |> Option.get :?> String
            let result = World.tryReloadAssetGraph inputDirectory outputDirectory refinementDirectory world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Either<Tuple<String, World>, Tuple<AssetGraph, World>>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'tryReloadAssetGraph' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getEntitiesInView selectedScreen world =
        let oldWorld = world
        try
            let struct (selectedScreen, world) =
                let context = World.getScriptContext world
                match World.evalInternal selectedScreen world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (Screen address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.getEntitiesInView selectedScreen world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Tuple<HashSet<Entity>, World>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getEntitiesInView' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getEntitiesInBounds bounds selectedScreen world =
        let oldWorld = world
        try
            let bounds = ScriptingWorld.tryExport (bounds.GetType ()) bounds world |> Option.get :?> Vector4
            let struct (selectedScreen, world) =
                let context = World.getScriptContext world
                match World.evalInternal selectedScreen world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (Screen address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.getEntitiesInBounds bounds selectedScreen world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Tuple<HashSet<Entity>, World>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getEntitiesInBounds' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getEntitiesAtPoint point selectedScreen world =
        let oldWorld = world
        try
            let point = ScriptingWorld.tryExport (point.GetType ()) point world |> Option.get :?> Vector2
            let struct (selectedScreen, world) =
                let context = World.getScriptContext world
                match World.evalInternal selectedScreen world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (Screen address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.getEntitiesAtPoint point selectedScreen world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Tuple<HashSet<Entity>, World>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getEntitiesAtPoint' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let playSong timeToFadeOutSongMs volume song world =
        let oldWorld = world
        try
            let timeToFadeOutSongMs = ScriptingWorld.tryExport (timeToFadeOutSongMs.GetType ()) timeToFadeOutSongMs world |> Option.get :?> Int32
            let volume = ScriptingWorld.tryExport (volume.GetType ()) volume world |> Option.get :?> Single
            let song = ScriptingWorld.tryExport (song.GetType ()) song world |> Option.get :?> AssetTag
            let result = World.playSong timeToFadeOutSongMs volume song world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'playSong' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let playSong5 timeToFadeOutSongMs volume songPackageName songAssetName world =
        let oldWorld = world
        try
            let timeToFadeOutSongMs = ScriptingWorld.tryExport (timeToFadeOutSongMs.GetType ()) timeToFadeOutSongMs world |> Option.get :?> Int32
            let volume = ScriptingWorld.tryExport (volume.GetType ()) volume world |> Option.get :?> Single
            let songPackageName = ScriptingWorld.tryExport (songPackageName.GetType ()) songPackageName world |> Option.get :?> String
            let songAssetName = ScriptingWorld.tryExport (songAssetName.GetType ()) songAssetName world |> Option.get :?> String
            let result = World.playSong5 timeToFadeOutSongMs volume songPackageName songAssetName world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'playSong5' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let playSound volume sound world =
        let oldWorld = world
        try
            let volume = ScriptingWorld.tryExport (volume.GetType ()) volume world |> Option.get :?> Single
            let sound = ScriptingWorld.tryExport (sound.GetType ()) sound world |> Option.get :?> AssetTag
            let result = World.playSound volume sound world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'playSound' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let playSound4 volume soundPackageName soundAssetName world =
        let oldWorld = world
        try
            let volume = ScriptingWorld.tryExport (volume.GetType ()) volume world |> Option.get :?> Single
            let soundPackageName = ScriptingWorld.tryExport (soundPackageName.GetType ()) soundPackageName world |> Option.get :?> String
            let soundAssetName = ScriptingWorld.tryExport (soundAssetName.GetType ()) soundAssetName world |> Option.get :?> String
            let result = World.playSound4 volume soundPackageName soundAssetName world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'playSound4' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let fadeOutSong timeToFadeOutSongMs world =
        let oldWorld = world
        try
            let timeToFadeOutSongMs = ScriptingWorld.tryExport (timeToFadeOutSongMs.GetType ()) timeToFadeOutSongMs world |> Option.get :?> Int32
            let result = World.fadeOutSong timeToFadeOutSongMs world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'fadeOutSong' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let stopSong world =
        let oldWorld = world
        try
            let result = World.stopSong world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'stopSong' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let hintAudioPackageUse packageName world =
        let oldWorld = world
        try
            let packageName = ScriptingWorld.tryExport (packageName.GetType ()) packageName world |> Option.get :?> String
            let result = World.hintAudioPackageUse packageName world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'hintAudioPackageUse' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let hintAudioPackageDisuse packageName world =
        let oldWorld = world
        try
            let packageName = ScriptingWorld.tryExport (packageName.GetType ()) packageName world |> Option.get :?> String
            let result = World.hintAudioPackageDisuse packageName world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'hintAudioPackageDisuse' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let reloadAudioAssets world =
        let oldWorld = world
        try
            let result = World.reloadAudioAssets world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'reloadAudioAssets' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let hintRenderPackageUse packageName world =
        let oldWorld = world
        try
            let packageName = ScriptingWorld.tryExport (packageName.GetType ()) packageName world |> Option.get :?> String
            let result = World.hintRenderPackageUse packageName world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'hintRenderPackageUse' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let hintRenderPackageDisuse packageName world =
        let oldWorld = world
        try
            let packageName = ScriptingWorld.tryExport (packageName.GetType ()) packageName world |> Option.get :?> String
            let result = World.hintRenderPackageDisuse packageName world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'hintRenderPackageDisuse' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let reloadRenderAssets world =
        let oldWorld = world
        try
            let result = World.reloadRenderAssets world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'reloadRenderAssets' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let bodyExists physicsId world =
        let oldWorld = world
        try
            let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
            let result = World.bodyExists physicsId world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Boolean> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'bodyExists' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getBodyContactNormals physicsId world =
        let oldWorld = world
        try
            let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
            let result = World.getBodyContactNormals physicsId world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Vector2[]> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getBodyContactNormals' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getBodyLinearVelocity physicsId world =
        let oldWorld = world
        try
            let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
            let result = World.getBodyLinearVelocity physicsId world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Vector2> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getBodyLinearVelocity' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getBodyToGroundContactNormals physicsId world =
        let oldWorld = world
        try
            let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
            let result = World.getBodyToGroundContactNormals physicsId world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Vector2[]> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getBodyToGroundContactNormals' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getBodyToGroundContactNormalOpt physicsId world =
        let oldWorld = world
        try
            let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
            let result = World.getBodyToGroundContactNormalOpt physicsId world
            let value = result
            let value = ScriptingWorld.tryImport typeof<FSharpOption<Vector2>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getBodyToGroundContactNormalOpt' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getBodyToGroundContactTangentOpt physicsId world =
        let oldWorld = world
        try
            let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
            let result = World.getBodyToGroundContactTangentOpt physicsId world
            let value = result
            let value = ScriptingWorld.tryImport typeof<FSharpOption<Vector2>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getBodyToGroundContactTangentOpt' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let isBodyOnGround physicsId world =
        let oldWorld = world
        try
            let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
            let result = World.isBodyOnGround physicsId world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Boolean> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'isBodyOnGround' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let createBody entity entityId bodyProperties world =
        let oldWorld = world
        try
            let struct (entity, world) =
                let context = World.getScriptContext world
                match World.evalInternal entity world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (Entity address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let entityId = ScriptingWorld.tryExport (entityId.GetType ()) entityId world |> Option.get :?> Guid
            let bodyProperties = ScriptingWorld.tryExport (bodyProperties.GetType ()) bodyProperties world |> Option.get :?> BodyProperties
            let result = World.createBody entity entityId bodyProperties world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'createBody' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let createBodies entity entityId bodiesProperties world =
        let oldWorld = world
        try
            let struct (entity, world) =
                let context = World.getScriptContext world
                match World.evalInternal entity world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (Entity address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let entityId = ScriptingWorld.tryExport (entityId.GetType ()) entityId world |> Option.get :?> Guid
            let bodiesProperties = ScriptingWorld.tryExport (bodiesProperties.GetType ()) bodiesProperties world |> Option.get :?> BodyProperties[]
            let result = World.createBodies entity entityId bodiesProperties world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'createBodies' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let destroyBody physicsId world =
        let oldWorld = world
        try
            let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
            let result = World.destroyBody physicsId world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'destroyBody' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let destroyBodies physicsIds world =
        let oldWorld = world
        try
            let physicsIds = ScriptingWorld.tryExport (physicsIds.GetType ()) physicsIds world |> Option.get :?> PhysicsId[]
            let result = World.destroyBodies physicsIds world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'destroyBodies' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let setBodyPosition position physicsId world =
        let oldWorld = world
        try
            let position = ScriptingWorld.tryExport (position.GetType ()) position world |> Option.get :?> Vector2
            let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
            let result = World.setBodyPosition position physicsId world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'setBodyPosition' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let setBodyRotation rotation physicsId world =
        let oldWorld = world
        try
            let rotation = ScriptingWorld.tryExport (rotation.GetType ()) rotation world |> Option.get :?> Single
            let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
            let result = World.setBodyRotation rotation physicsId world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'setBodyRotation' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let setBodyAngularVelocity angularVelocity physicsId world =
        let oldWorld = world
        try
            let angularVelocity = ScriptingWorld.tryExport (angularVelocity.GetType ()) angularVelocity world |> Option.get :?> Single
            let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
            let result = World.setBodyAngularVelocity angularVelocity physicsId world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'setBodyAngularVelocity' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let setBodyLinearVelocity linearVelocity physicsId world =
        let oldWorld = world
        try
            let linearVelocity = ScriptingWorld.tryExport (linearVelocity.GetType ()) linearVelocity world |> Option.get :?> Vector2
            let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
            let result = World.setBodyLinearVelocity linearVelocity physicsId world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'setBodyLinearVelocity' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let applyBodyAngularImpulse angularImpulse physicsId world =
        let oldWorld = world
        try
            let angularImpulse = ScriptingWorld.tryExport (angularImpulse.GetType ()) angularImpulse world |> Option.get :?> Single
            let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
            let result = World.applyBodyAngularImpulse angularImpulse physicsId world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'applyBodyAngularImpulse' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let applyBodyLinearImpulse linearImpulse physicsId world =
        let oldWorld = world
        try
            let linearImpulse = ScriptingWorld.tryExport (linearImpulse.GetType ()) linearImpulse world |> Option.get :?> Vector2
            let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
            let result = World.applyBodyLinearImpulse linearImpulse physicsId world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'applyBodyLinearImpulse' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let applyBodyForce force physicsId world =
        let oldWorld = world
        try
            let force = ScriptingWorld.tryExport (force.GetType ()) force world |> Option.get :?> Vector2
            let physicsId = ScriptingWorld.tryExport (physicsId.GetType ()) physicsId world |> Option.get :?> PhysicsId
            let result = World.applyBodyForce force physicsId world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'applyBodyForce' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let isMouseButtonDown mouseButton world =
        let oldWorld = world
        try
            let mouseButton = ScriptingWorld.tryExport (mouseButton.GetType ()) mouseButton world |> Option.get :?> MouseButton
            let result = World.isMouseButtonDown mouseButton world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Boolean> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'isMouseButtonDown' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getMousePosition world =
        let oldWorld = world
        try
            let result = World.getMousePosition world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Vector2i> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getMousePosition' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getMousePositionF world =
        let oldWorld = world
        try
            let result = World.getMousePositionF world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Vector2> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getMousePositionF' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let isKeyboardKeyDown scanCode world =
        let oldWorld = world
        try
            let scanCode = ScriptingWorld.tryExport (scanCode.GetType ()) scanCode world |> Option.get :?> Int32
            let result = World.isKeyboardKeyDown scanCode world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Boolean> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'isKeyboardKeyDown' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getSimulantSelected simulant world =
        let oldWorld = world
        try
            let struct (simulant, world) =
                let context = World.getScriptContext world
                match World.evalInternal simulant world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (World.deriveSimulant address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.getSimulantSelected simulant world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Boolean> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getSimulantSelected' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let tryGetSimulantParent simulant world =
        let oldWorld = world
        try
            let struct (simulant, world) =
                let context = World.getScriptContext world
                match World.evalInternal simulant world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (World.deriveSimulant address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.tryGetSimulantParent simulant world
            let value = result
            let value = ScriptingWorld.tryImport typeof<FSharpOption<Simulant>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'tryGetSimulantParent' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getSimulantChildren simulant world =
        let oldWorld = world
        try
            let struct (simulant, world) =
                let context = World.getScriptContext world
                match World.evalInternal simulant world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (World.deriveSimulant address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.getSimulantChildren simulant world
            let value = result
            let value = ScriptingWorld.tryImport typeof<IEnumerable<Simulant>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getSimulantChildren' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getSimulantExists simulant world =
        let oldWorld = world
        try
            let struct (simulant, world) =
                let context = World.getScriptContext world
                match World.evalInternal simulant world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (World.deriveSimulant address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.getSimulantExists simulant world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Boolean> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getSimulantExists' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getEntities1 world =
        let oldWorld = world
        try
            let result = World.getEntities1 world
            let value = result
            let value = ScriptingWorld.tryImport typeof<IEnumerable<Entity>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getEntities1' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getLayers1 world =
        let oldWorld = world
        try
            let result = World.getLayers1 world
            let value = result
            let value = ScriptingWorld.tryImport typeof<IEnumerable<Layer>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getLayers1' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let isSimulantSelected simulant world =
        let oldWorld = world
        try
            let struct (simulant, world) =
                let context = World.getScriptContext world
                match World.evalInternal simulant world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (World.deriveSimulant address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.isSimulantSelected simulant world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Boolean> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'isSimulantSelected' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let writeGameToFile filePath world =
        let oldWorld = world
        try
            let filePath = ScriptingWorld.tryExport (filePath.GetType ()) filePath world |> Option.get :?> String
            let result = World.writeGameToFile filePath world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Void> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'writeGameToFile' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let readGameFromFile filePath world =
        let oldWorld = world
        try
            let filePath = ScriptingWorld.tryExport (filePath.GetType ()) filePath world |> Option.get :?> String
            let result = World.readGameFromFile filePath world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'readGameFromFile' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getScreens world =
        let oldWorld = world
        try
            let result = World.getScreens world
            let value = result
            let value = ScriptingWorld.tryImport typeof<IEnumerable<Screen>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getScreens' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let destroyScreen screen world =
        let oldWorld = world
        try
            let struct (screen, world) =
                let context = World.getScriptContext world
                match World.evalInternal screen world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (Screen address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.destroyScreen screen world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'destroyScreen' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let createScreen3 dispatcherName nameOpt world =
        let oldWorld = world
        try
            let dispatcherName = ScriptingWorld.tryExport (dispatcherName.GetType ()) dispatcherName world |> Option.get :?> String
            let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get :?> FSharpOption<String>
            let result = World.createScreen3 dispatcherName nameOpt world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Tuple<Screen, World>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'createScreen3' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let createDissolveScreen5 dispatcherName nameOpt dissolveData world =
        let oldWorld = world
        try
            let dispatcherName = ScriptingWorld.tryExport (dispatcherName.GetType ()) dispatcherName world |> Option.get :?> String
            let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get :?> FSharpOption<String>
            let dissolveData = ScriptingWorld.tryExport (dissolveData.GetType ()) dissolveData world |> Option.get :?> DissolveData
            let result = World.createDissolveScreen5 dispatcherName nameOpt dissolveData world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Tuple<Screen, World>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'createDissolveScreen5' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let writeScreenToFile filePath screen world =
        let oldWorld = world
        try
            let filePath = ScriptingWorld.tryExport (filePath.GetType ()) filePath world |> Option.get :?> String
            let struct (screen, world) =
                let context = World.getScriptContext world
                match World.evalInternal screen world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (Screen address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.writeScreenToFile filePath screen world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Void> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'writeScreenToFile' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let readScreenFromFile filePath nameOpt world =
        let oldWorld = world
        try
            let filePath = ScriptingWorld.tryExport (filePath.GetType ()) filePath world |> Option.get :?> String
            let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get :?> FSharpOption<String>
            let result = World.readScreenFromFile filePath nameOpt world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Tuple<Screen, World>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'readScreenFromFile' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getLayers screen world =
        let oldWorld = world
        try
            let struct (screen, world) =
                let context = World.getScriptContext world
                match World.evalInternal screen world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (Screen address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.getLayers screen world
            let value = result
            let value = ScriptingWorld.tryImport typeof<IEnumerable<Layer>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getLayers' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let destroyLayer layer world =
        let oldWorld = world
        try
            let struct (layer, world) =
                let context = World.getScriptContext world
                match World.evalInternal layer world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (Layer address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.destroyLayer layer world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'destroyLayer' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let destroyLayers layers world =
        let oldWorld = world
        try
            let layers = ScriptingWorld.tryExport (layers.GetType ()) layers world |> Option.get :?> IEnumerable<Layer>
            let result = World.destroyLayers layers world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'destroyLayers' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let writeLayerToFile filePath layer world =
        let oldWorld = world
        try
            let filePath = ScriptingWorld.tryExport (filePath.GetType ()) filePath world |> Option.get :?> String
            let struct (layer, world) =
                let context = World.getScriptContext world
                match World.evalInternal layer world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (Layer address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.writeLayerToFile filePath layer world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Void> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'writeLayerToFile' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let readLayerFromFile filePath nameOpt screen world =
        let oldWorld = world
        try
            let filePath = ScriptingWorld.tryExport (filePath.GetType ()) filePath world |> Option.get :?> String
            let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get :?> FSharpOption<String>
            let struct (screen, world) =
                let context = World.getScriptContext world
                match World.evalInternal screen world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (Screen address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.readLayerFromFile filePath nameOpt screen world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Tuple<Layer, World>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'readLayerFromFile' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getEntities layer world =
        let oldWorld = world
        try
            let struct (layer, world) =
                let context = World.getScriptContext world
                match World.evalInternal layer world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (Layer address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.getEntities layer world
            let value = result
            let value = ScriptingWorld.tryImport typeof<IEnumerable<Entity>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getEntities' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let destroyEntity entity world =
        let oldWorld = world
        try
            let struct (entity, world) =
                let context = World.getScriptContext world
                match World.evalInternal entity world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (Entity address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.destroyEntity entity world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'destroyEntity' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let destroyEntities entities world =
        let oldWorld = world
        try
            let entities = ScriptingWorld.tryExport (entities.GetType ()) entities world |> Option.get :?> IEnumerable<Entity>
            let result = World.destroyEntities entities world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'destroyEntities' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let tryPickEntity position entities world =
        let oldWorld = world
        try
            let position = ScriptingWorld.tryExport (position.GetType ()) position world |> Option.get :?> Vector2
            let entities = ScriptingWorld.tryExport (entities.GetType ()) entities world |> Option.get :?> IEnumerable<Entity>
            let result = World.tryPickEntity position entities world
            let value = result
            let value = ScriptingWorld.tryImport typeof<FSharpOption<Entity>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'tryPickEntity' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let createEntity5 dispatcherName nameOpt overlayNameDescriptor layer world =
        let oldWorld = world
        try
            let dispatcherName = ScriptingWorld.tryExport (dispatcherName.GetType ()) dispatcherName world |> Option.get :?> String
            let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get :?> FSharpOption<String>
            let overlayNameDescriptor = ScriptingWorld.tryExport (overlayNameDescriptor.GetType ()) overlayNameDescriptor world |> Option.get :?> OverlayNameDescriptor
            let struct (layer, world) =
                let context = World.getScriptContext world
                match World.evalInternal layer world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (Layer address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.createEntity5 dispatcherName nameOpt overlayNameDescriptor layer world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Tuple<Entity, World>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'createEntity5' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let reassignEntity entity nameOpt layer world =
        let oldWorld = world
        try
            let struct (entity, world) =
                let context = World.getScriptContext world
                match World.evalInternal entity world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (Entity address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get :?> FSharpOption<String>
            let struct (layer, world) =
                let context = World.getScriptContext world
                match World.evalInternal layer world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (Layer address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.reassignEntity entity nameOpt layer world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'reassignEntity' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let trySetEntityOverlayNameOpt overlayNameOpt entity world =
        let oldWorld = world
        try
            let overlayNameOpt = ScriptingWorld.tryExport (overlayNameOpt.GetType ()) overlayNameOpt world |> Option.get :?> FSharpOption<String>
            let struct (entity, world) =
                let context = World.getScriptContext world
                match World.evalInternal entity world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (Entity address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.trySetEntityOverlayNameOpt overlayNameOpt entity world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Either<String, World>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'trySetEntityOverlayNameOpt' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let trySetEntityFacetNames facetNames entity world =
        let oldWorld = world
        try
            let facetNames = ScriptingWorld.tryExport (facetNames.GetType ()) facetNames world |> Option.get :?> FSharpSet<String>
            let struct (entity, world) =
                let context = World.getScriptContext world
                match World.evalInternal entity world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (Entity address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.trySetEntityFacetNames facetNames entity world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Either<String, World>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'trySetEntityFacetNames' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let createLayer4 dispatcherName nameOpt screen world =
        let oldWorld = world
        try
            let dispatcherName = ScriptingWorld.tryExport (dispatcherName.GetType ()) dispatcherName world |> Option.get :?> String
            let nameOpt = ScriptingWorld.tryExport (nameOpt.GetType ()) nameOpt world |> Option.get :?> FSharpOption<String>
            let struct (screen, world) =
                let context = World.getScriptContext world
                match World.evalInternal screen world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (Screen address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.createLayer4 dispatcherName nameOpt screen world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Tuple<Layer, World>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'createLayer4' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getEyeCenter world =
        let oldWorld = world
        try
            let result = World.getEyeCenter world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Vector2> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getEyeCenter' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let setEyeCenter value world =
        let oldWorld = world
        try
            let value = ScriptingWorld.tryExport (value.GetType ()) value world |> Option.get :?> Vector2
            let result = World.setEyeCenter value world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'setEyeCenter' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getEyeSize world =
        let oldWorld = world
        try
            let result = World.getEyeSize world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Vector2> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getEyeSize' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let setEyeSize value world =
        let oldWorld = world
        try
            let value = ScriptingWorld.tryExport (value.GetType ()) value world |> Option.get :?> Vector2
            let result = World.setEyeSize value world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'setEyeSize' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getSelectedScreenOpt world =
        let oldWorld = world
        try
            let result = World.getSelectedScreenOpt world
            let value = result
            let value = ScriptingWorld.tryImport typeof<FSharpOption<Screen>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getSelectedScreenOpt' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let setSelectedScreenOpt value world =
        let oldWorld = world
        try
            let value = ScriptingWorld.tryExport (value.GetType ()) value world |> Option.get :?> FSharpOption<Screen>
            let result = World.setSelectedScreenOpt value world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'setSelectedScreenOpt' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getSelectedScreen world =
        let oldWorld = world
        try
            let result = World.getSelectedScreen world
            let value = result
            let value = Scripting.String (scstring value)
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getSelectedScreen' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let setSelectedScreen screen world =
        let oldWorld = world
        try
            let struct (screen, world) =
                let context = World.getScriptContext world
                match World.evalInternal screen world with
                | struct (Scripting.String str, world)
                | struct (Scripting.Keyword str, world) ->
                    let relation = Relation.makeFromString str
                    let address = Relation.resolve context.SimulantAddress relation
                    struct (Screen address, world)
                | struct (Scripting.Violation (_, error, _), _) -> failwith error
                | struct (_, _) -> failwith "Relation must be either a String or Keyword."
            let result = World.setSelectedScreen screen world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'setSelectedScreen' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getScreenTransitionDestinationOpt world =
        let oldWorld = world
        try
            let result = World.getScreenTransitionDestinationOpt world
            let value = result
            let value = ScriptingWorld.tryImport typeof<FSharpOption<Screen>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getScreenTransitionDestinationOpt' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getViewAbsolute world =
        let oldWorld = world
        try
            let result = World.getViewAbsolute world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Matrix3> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getViewAbsolute' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getViewAbsoluteI world =
        let oldWorld = world
        try
            let result = World.getViewAbsoluteI world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Matrix3> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getViewAbsoluteI' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getViewRelative world =
        let oldWorld = world
        try
            let result = World.getViewRelative world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Matrix3> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getViewRelative' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getViewRelativeI world =
        let oldWorld = world
        try
            let result = World.getViewRelativeI world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Matrix3> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getViewRelativeI' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getViewBoundsRelative world =
        let oldWorld = world
        try
            let result = World.getViewBoundsRelative world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Vector4> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getViewBoundsRelative' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getViewBoundsAbsolute world =
        let oldWorld = world
        try
            let result = World.getViewBoundsAbsolute world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Vector4> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getViewBoundsAbsolute' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getViewBounds viewType world =
        let oldWorld = world
        try
            let viewType = ScriptingWorld.tryExport (viewType.GetType ()) viewType world |> Option.get :?> ViewType
            let result = World.getViewBounds viewType world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Vector4> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getViewBounds' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let isBoundsInView viewType bounds world =
        let oldWorld = world
        try
            let viewType = ScriptingWorld.tryExport (viewType.GetType ()) viewType world |> Option.get :?> ViewType
            let bounds = ScriptingWorld.tryExport (bounds.GetType ()) bounds world |> Option.get :?> Vector4
            let result = World.isBoundsInView viewType bounds world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Boolean> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'isBoundsInView' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let mouseToScreen mousePosition world =
        let oldWorld = world
        try
            let mousePosition = ScriptingWorld.tryExport (mousePosition.GetType ()) mousePosition world |> Option.get :?> Vector2
            let result = World.mouseToScreen mousePosition world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Vector2> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'mouseToScreen' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let mouseToWorld viewType mousePosition world =
        let oldWorld = world
        try
            let viewType = ScriptingWorld.tryExport (viewType.GetType ()) viewType world |> Option.get :?> ViewType
            let mousePosition = ScriptingWorld.tryExport (mousePosition.GetType ()) mousePosition world |> Option.get :?> Vector2
            let result = World.mouseToWorld viewType mousePosition world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Vector2> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'mouseToWorld' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let mouseToEntity viewType entityPosition mousePosition world =
        let oldWorld = world
        try
            let viewType = ScriptingWorld.tryExport (viewType.GetType ()) viewType world |> Option.get :?> ViewType
            let entityPosition = ScriptingWorld.tryExport (entityPosition.GetType ()) entityPosition world |> Option.get :?> Vector2
            let mousePosition = ScriptingWorld.tryExport (mousePosition.GetType ()) mousePosition world |> Option.get :?> Vector2
            let result = World.mouseToEntity viewType entityPosition mousePosition world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Vector2> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'mouseToEntity' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getTickRate world =
        let oldWorld = world
        try
            let result = World.getTickRate world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Int64> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getTickRate' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getTickRateF world =
        let oldWorld = world
        try
            let result = World.getTickRateF world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Single> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getTickRateF' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let setTickRate tickRate world =
        let oldWorld = world
        try
            let tickRate = ScriptingWorld.tryExport (tickRate.GetType ()) tickRate world |> Option.get :?> Int64
            let result = World.setTickRate tickRate world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'setTickRate' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let resetTickTime world =
        let oldWorld = world
        try
            let result = World.resetTickTime world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'resetTickTime' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getTickTime world =
        let oldWorld = world
        try
            let result = World.getTickTime world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Int64> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getTickTime' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let isTicking world =
        let oldWorld = world
        try
            let result = World.isTicking world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Boolean> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'isTicking' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getUpdateCount world =
        let oldWorld = world
        try
            let result = World.getUpdateCount world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Int64> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getUpdateCount' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getLiveness world =
        let oldWorld = world
        try
            let result = World.getLiveness world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Liveness> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getLiveness' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let exit world =
        let oldWorld = world
        try
            let result = World.exit world
            struct (Scripting.Unit, result)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'exit' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let tryGetTextureSize assetTag world =
        let oldWorld = world
        try
            let assetTag = ScriptingWorld.tryExport (assetTag.GetType ()) assetTag world |> Option.get :?> AssetTag
            let result = World.tryGetTextureSize assetTag world
            let value = result
            let value = ScriptingWorld.tryImport typeof<FSharpOption<Vector2i>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'tryGetTextureSize' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getTextureSize assetTag world =
        let oldWorld = world
        try
            let assetTag = ScriptingWorld.tryExport (assetTag.GetType ()) assetTag world |> Option.get :?> AssetTag
            let result = World.getTextureSize assetTag world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Vector2i> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getTextureSize' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let tryGetTextureSizeAsVector2 assetTag world =
        let oldWorld = world
        try
            let assetTag = ScriptingWorld.tryExport (assetTag.GetType ()) assetTag world |> Option.get :?> AssetTag
            let result = World.tryGetTextureSizeAsVector2 assetTag world
            let value = result
            let value = ScriptingWorld.tryImport typeof<FSharpOption<Vector2>> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'tryGetTextureSizeAsVector2' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let getTextureSizeAsVector2 assetTag world =
        let oldWorld = world
        try
            let assetTag = ScriptingWorld.tryExport (assetTag.GetType ()) assetTag world |> Option.get :?> AssetTag
            let result = World.getTextureSizeAsVector2 assetTag world
            let value = result
            let value = ScriptingWorld.tryImport typeof<Vector2> value world |> Option.get
            struct (value, world)
        with exn ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "Could not invoke binding 'getTextureSizeAsVector2' due to: " + scstring exn, None)
            struct (violation, World.choose oldWorld)

    let bindings fnName exprs originOpt world =
        match fnName with
        | "tryGetIsSelectedScreenIdling" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                tryGetIsSelectedScreenIdling  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'tryGetIsSelectedScreenIdling' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "tryGetIsSelectedScreenTransitioning" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                tryGetIsSelectedScreenTransitioning  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'tryGetIsSelectedScreenTransitioning' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "isSelectedScreenIdling" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                isSelectedScreenIdling  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'isSelectedScreenIdling' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "isSelectedScreenTransitioning" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                isSelectedScreenTransitioning  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'isSelectedScreenTransitioning' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "selectScreen" ->
            match World.evalManyInternal exprs world with
            | struct ([|screen|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                selectScreen screen world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'selectScreen' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "tryTransitionScreen" ->
            match World.evalManyInternal exprs world with
            | struct ([|destination|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                tryTransitionScreen destination world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'tryTransitionScreen' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "transitionScreen" ->
            match World.evalManyInternal exprs world with
            | struct ([|destination|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                transitionScreen destination world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'transitionScreen' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "createDissolveScreenFromLayerFile6" ->
            match World.evalManyInternal exprs world with
            | struct ([|dispatcherName; nameOpt; dissolveData; layerFilePath|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                createDissolveScreenFromLayerFile6 dispatcherName nameOpt dissolveData layerFilePath world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'createDissolveScreenFromLayerFile6' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "createDissolveScreenFromLayerFile" ->
            match World.evalManyInternal exprs world with
            | struct ([|nameOpt; dissolveData; layerFilePath|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                createDissolveScreenFromLayerFile nameOpt dissolveData layerFilePath world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'createDissolveScreenFromLayerFile' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "createSplashScreen6" ->
            match World.evalManyInternal exprs world with
            | struct ([|dispatcherName; nameOpt; splashData; destination|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                createSplashScreen6 dispatcherName nameOpt splashData destination world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'createSplashScreen6' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "createSplashScreen" ->
            match World.evalManyInternal exprs world with
            | struct ([|nameOpt; splashData; destination|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                createSplashScreen nameOpt splashData destination world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'createSplashScreen' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "tryReloadOverlays" ->
            match World.evalManyInternal exprs world with
            | struct ([|inputDirectory; outputDirectory|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                tryReloadOverlays inputDirectory outputDirectory world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'tryReloadOverlays' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "tryReloadPrelude" ->
            match World.evalManyInternal exprs world with
            | struct ([|inputDirectory; outputDirectory|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                tryReloadPrelude inputDirectory outputDirectory world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'tryReloadPrelude' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "tryReloadAssetGraph" ->
            match World.evalManyInternal exprs world with
            | struct ([|inputDirectory; outputDirectory; refinementDirectory|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                tryReloadAssetGraph inputDirectory outputDirectory refinementDirectory world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'tryReloadAssetGraph' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getEntitiesInView" ->
            match World.evalManyInternal exprs world with
            | struct ([|selectedScreen|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getEntitiesInView selectedScreen world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getEntitiesInView' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getEntitiesInBounds" ->
            match World.evalManyInternal exprs world with
            | struct ([|bounds; selectedScreen|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getEntitiesInBounds bounds selectedScreen world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getEntitiesInBounds' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getEntitiesAtPoint" ->
            match World.evalManyInternal exprs world with
            | struct ([|point; selectedScreen|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getEntitiesAtPoint point selectedScreen world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getEntitiesAtPoint' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "playSong" ->
            match World.evalManyInternal exprs world with
            | struct ([|timeToFadeOutSongMs; volume; song|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                playSong timeToFadeOutSongMs volume song world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'playSong' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "playSong5" ->
            match World.evalManyInternal exprs world with
            | struct ([|timeToFadeOutSongMs; volume; songPackageName; songAssetName|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                playSong5 timeToFadeOutSongMs volume songPackageName songAssetName world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'playSong5' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "playSound" ->
            match World.evalManyInternal exprs world with
            | struct ([|volume; sound|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                playSound volume sound world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'playSound' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "playSound4" ->
            match World.evalManyInternal exprs world with
            | struct ([|volume; soundPackageName; soundAssetName|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                playSound4 volume soundPackageName soundAssetName world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'playSound4' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "fadeOutSong" ->
            match World.evalManyInternal exprs world with
            | struct ([|timeToFadeOutSongMs|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                fadeOutSong timeToFadeOutSongMs world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'fadeOutSong' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "stopSong" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                stopSong  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'stopSong' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "hintAudioPackageUse" ->
            match World.evalManyInternal exprs world with
            | struct ([|packageName|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                hintAudioPackageUse packageName world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'hintAudioPackageUse' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "hintAudioPackageDisuse" ->
            match World.evalManyInternal exprs world with
            | struct ([|packageName|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                hintAudioPackageDisuse packageName world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'hintAudioPackageDisuse' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "reloadAudioAssets" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                reloadAudioAssets  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'reloadAudioAssets' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "hintRenderPackageUse" ->
            match World.evalManyInternal exprs world with
            | struct ([|packageName|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                hintRenderPackageUse packageName world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'hintRenderPackageUse' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "hintRenderPackageDisuse" ->
            match World.evalManyInternal exprs world with
            | struct ([|packageName|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                hintRenderPackageDisuse packageName world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'hintRenderPackageDisuse' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "reloadRenderAssets" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                reloadRenderAssets  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'reloadRenderAssets' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "bodyExists" ->
            match World.evalManyInternal exprs world with
            | struct ([|physicsId|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                bodyExists physicsId world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'bodyExists' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getBodyContactNormals" ->
            match World.evalManyInternal exprs world with
            | struct ([|physicsId|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getBodyContactNormals physicsId world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getBodyContactNormals' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getBodyLinearVelocity" ->
            match World.evalManyInternal exprs world with
            | struct ([|physicsId|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getBodyLinearVelocity physicsId world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getBodyLinearVelocity' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getBodyToGroundContactNormals" ->
            match World.evalManyInternal exprs world with
            | struct ([|physicsId|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getBodyToGroundContactNormals physicsId world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getBodyToGroundContactNormals' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getBodyToGroundContactNormalOpt" ->
            match World.evalManyInternal exprs world with
            | struct ([|physicsId|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getBodyToGroundContactNormalOpt physicsId world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getBodyToGroundContactNormalOpt' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getBodyToGroundContactTangentOpt" ->
            match World.evalManyInternal exprs world with
            | struct ([|physicsId|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getBodyToGroundContactTangentOpt physicsId world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getBodyToGroundContactTangentOpt' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "isBodyOnGround" ->
            match World.evalManyInternal exprs world with
            | struct ([|physicsId|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                isBodyOnGround physicsId world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'isBodyOnGround' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "createBody" ->
            match World.evalManyInternal exprs world with
            | struct ([|entity; entityId; bodyProperties|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                createBody entity entityId bodyProperties world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'createBody' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "createBodies" ->
            match World.evalManyInternal exprs world with
            | struct ([|entity; entityId; bodiesProperties|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                createBodies entity entityId bodiesProperties world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'createBodies' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "destroyBody" ->
            match World.evalManyInternal exprs world with
            | struct ([|physicsId|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                destroyBody physicsId world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'destroyBody' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "destroyBodies" ->
            match World.evalManyInternal exprs world with
            | struct ([|physicsIds|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                destroyBodies physicsIds world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'destroyBodies' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "setBodyPosition" ->
            match World.evalManyInternal exprs world with
            | struct ([|position; physicsId|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                setBodyPosition position physicsId world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'setBodyPosition' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "setBodyRotation" ->
            match World.evalManyInternal exprs world with
            | struct ([|rotation; physicsId|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                setBodyRotation rotation physicsId world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'setBodyRotation' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "setBodyAngularVelocity" ->
            match World.evalManyInternal exprs world with
            | struct ([|angularVelocity; physicsId|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                setBodyAngularVelocity angularVelocity physicsId world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'setBodyAngularVelocity' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "setBodyLinearVelocity" ->
            match World.evalManyInternal exprs world with
            | struct ([|linearVelocity; physicsId|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                setBodyLinearVelocity linearVelocity physicsId world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'setBodyLinearVelocity' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "applyBodyAngularImpulse" ->
            match World.evalManyInternal exprs world with
            | struct ([|angularImpulse; physicsId|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                applyBodyAngularImpulse angularImpulse physicsId world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'applyBodyAngularImpulse' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "applyBodyLinearImpulse" ->
            match World.evalManyInternal exprs world with
            | struct ([|linearImpulse; physicsId|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                applyBodyLinearImpulse linearImpulse physicsId world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'applyBodyLinearImpulse' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "applyBodyForce" ->
            match World.evalManyInternal exprs world with
            | struct ([|force; physicsId|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                applyBodyForce force physicsId world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'applyBodyForce' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "isMouseButtonDown" ->
            match World.evalManyInternal exprs world with
            | struct ([|mouseButton|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                isMouseButtonDown mouseButton world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'isMouseButtonDown' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getMousePosition" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getMousePosition  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getMousePosition' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getMousePositionF" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getMousePositionF  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getMousePositionF' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "isKeyboardKeyDown" ->
            match World.evalManyInternal exprs world with
            | struct ([|scanCode|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                isKeyboardKeyDown scanCode world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'isKeyboardKeyDown' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getSimulantSelected" ->
            match World.evalManyInternal exprs world with
            | struct ([|simulant|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getSimulantSelected simulant world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getSimulantSelected' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "tryGetSimulantParent" ->
            match World.evalManyInternal exprs world with
            | struct ([|simulant|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                tryGetSimulantParent simulant world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'tryGetSimulantParent' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getSimulantChildren" ->
            match World.evalManyInternal exprs world with
            | struct ([|simulant|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getSimulantChildren simulant world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getSimulantChildren' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getSimulantExists" ->
            match World.evalManyInternal exprs world with
            | struct ([|simulant|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getSimulantExists simulant world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getSimulantExists' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getEntities1" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getEntities1  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getEntities1' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getLayers1" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getLayers1  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getLayers1' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "isSimulantSelected" ->
            match World.evalManyInternal exprs world with
            | struct ([|simulant|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                isSimulantSelected simulant world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'isSimulantSelected' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "writeGameToFile" ->
            match World.evalManyInternal exprs world with
            | struct ([|filePath|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                writeGameToFile filePath world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'writeGameToFile' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "readGameFromFile" ->
            match World.evalManyInternal exprs world with
            | struct ([|filePath|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                readGameFromFile filePath world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'readGameFromFile' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getScreens" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getScreens  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getScreens' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "destroyScreen" ->
            match World.evalManyInternal exprs world with
            | struct ([|screen|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                destroyScreen screen world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'destroyScreen' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "createScreen3" ->
            match World.evalManyInternal exprs world with
            | struct ([|dispatcherName; nameOpt|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                createScreen3 dispatcherName nameOpt world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'createScreen3' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "createDissolveScreen5" ->
            match World.evalManyInternal exprs world with
            | struct ([|dispatcherName; nameOpt; dissolveData|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                createDissolveScreen5 dispatcherName nameOpt dissolveData world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'createDissolveScreen5' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "writeScreenToFile" ->
            match World.evalManyInternal exprs world with
            | struct ([|filePath; screen|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                writeScreenToFile filePath screen world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'writeScreenToFile' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "readScreenFromFile" ->
            match World.evalManyInternal exprs world with
            | struct ([|filePath; nameOpt|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                readScreenFromFile filePath nameOpt world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'readScreenFromFile' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getLayers" ->
            match World.evalManyInternal exprs world with
            | struct ([|screen|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getLayers screen world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getLayers' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "destroyLayer" ->
            match World.evalManyInternal exprs world with
            | struct ([|layer|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                destroyLayer layer world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'destroyLayer' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "destroyLayers" ->
            match World.evalManyInternal exprs world with
            | struct ([|layers|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                destroyLayers layers world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'destroyLayers' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "writeLayerToFile" ->
            match World.evalManyInternal exprs world with
            | struct ([|filePath; layer|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                writeLayerToFile filePath layer world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'writeLayerToFile' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "readLayerFromFile" ->
            match World.evalManyInternal exprs world with
            | struct ([|filePath; nameOpt; screen|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                readLayerFromFile filePath nameOpt screen world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'readLayerFromFile' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getEntities" ->
            match World.evalManyInternal exprs world with
            | struct ([|layer|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getEntities layer world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getEntities' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "destroyEntity" ->
            match World.evalManyInternal exprs world with
            | struct ([|entity|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                destroyEntity entity world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'destroyEntity' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "destroyEntities" ->
            match World.evalManyInternal exprs world with
            | struct ([|entities|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                destroyEntities entities world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'destroyEntities' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "tryPickEntity" ->
            match World.evalManyInternal exprs world with
            | struct ([|position; entities|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                tryPickEntity position entities world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'tryPickEntity' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "createEntity5" ->
            match World.evalManyInternal exprs world with
            | struct ([|dispatcherName; nameOpt; overlayNameDescriptor; layer|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                createEntity5 dispatcherName nameOpt overlayNameDescriptor layer world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'createEntity5' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "reassignEntity" ->
            match World.evalManyInternal exprs world with
            | struct ([|entity; nameOpt; layer|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                reassignEntity entity nameOpt layer world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'reassignEntity' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "trySetEntityOverlayNameOpt" ->
            match World.evalManyInternal exprs world with
            | struct ([|overlayNameOpt; entity|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                trySetEntityOverlayNameOpt overlayNameOpt entity world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'trySetEntityOverlayNameOpt' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "trySetEntityFacetNames" ->
            match World.evalManyInternal exprs world with
            | struct ([|facetNames; entity|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                trySetEntityFacetNames facetNames entity world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'trySetEntityFacetNames' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "createLayer4" ->
            match World.evalManyInternal exprs world with
            | struct ([|dispatcherName; nameOpt; screen|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                createLayer4 dispatcherName nameOpt screen world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'createLayer4' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getEyeCenter" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getEyeCenter  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getEyeCenter' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "setEyeCenter" ->
            match World.evalManyInternal exprs world with
            | struct ([|value|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                setEyeCenter value world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'setEyeCenter' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getEyeSize" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getEyeSize  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getEyeSize' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "setEyeSize" ->
            match World.evalManyInternal exprs world with
            | struct ([|value|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                setEyeSize value world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'setEyeSize' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getSelectedScreenOpt" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getSelectedScreenOpt  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getSelectedScreenOpt' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "setSelectedScreenOpt" ->
            match World.evalManyInternal exprs world with
            | struct ([|value|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                setSelectedScreenOpt value world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'setSelectedScreenOpt' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getSelectedScreen" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getSelectedScreen  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getSelectedScreen' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "setSelectedScreen" ->
            match World.evalManyInternal exprs world with
            | struct ([|screen|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                setSelectedScreen screen world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'setSelectedScreen' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getScreenTransitionDestinationOpt" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getScreenTransitionDestinationOpt  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getScreenTransitionDestinationOpt' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getViewAbsolute" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getViewAbsolute  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getViewAbsolute' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getViewAbsoluteI" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getViewAbsoluteI  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getViewAbsoluteI' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getViewRelative" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getViewRelative  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getViewRelative' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getViewRelativeI" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getViewRelativeI  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getViewRelativeI' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getViewBoundsRelative" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getViewBoundsRelative  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getViewBoundsRelative' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getViewBoundsAbsolute" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getViewBoundsAbsolute  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getViewBoundsAbsolute' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getViewBounds" ->
            match World.evalManyInternal exprs world with
            | struct ([|viewType|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getViewBounds viewType world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getViewBounds' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "isBoundsInView" ->
            match World.evalManyInternal exprs world with
            | struct ([|viewType; bounds|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                isBoundsInView viewType bounds world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'isBoundsInView' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "mouseToScreen" ->
            match World.evalManyInternal exprs world with
            | struct ([|mousePosition|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                mouseToScreen mousePosition world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'mouseToScreen' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "mouseToWorld" ->
            match World.evalManyInternal exprs world with
            | struct ([|viewType; mousePosition|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                mouseToWorld viewType mousePosition world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'mouseToWorld' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "mouseToEntity" ->
            match World.evalManyInternal exprs world with
            | struct ([|viewType; entityPosition; mousePosition|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                mouseToEntity viewType entityPosition mousePosition world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'mouseToEntity' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getTickRate" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getTickRate  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getTickRate' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getTickRateF" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getTickRateF  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getTickRateF' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "setTickRate" ->
            match World.evalManyInternal exprs world with
            | struct ([|tickRate|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                setTickRate tickRate world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'setTickRate' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "resetTickTime" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                resetTickTime  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'resetTickTime' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getTickTime" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getTickTime  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getTickTime' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "isTicking" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                isTicking  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'isTicking' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getUpdateCount" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getUpdateCount  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getUpdateCount' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getLiveness" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getLiveness  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getLiveness' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "exit" ->
            match World.evalManyInternal exprs world with
            | struct ([||] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                exit  world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'exit' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "tryGetTextureSize" ->
            match World.evalManyInternal exprs world with
            | struct ([|assetTag|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                tryGetTextureSize assetTag world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'tryGetTextureSize' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getTextureSize" ->
            match World.evalManyInternal exprs world with
            | struct ([|assetTag|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getTextureSize assetTag world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getTextureSize' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "tryGetTextureSizeAsVector2" ->
            match World.evalManyInternal exprs world with
            | struct ([|assetTag|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                tryGetTextureSizeAsVector2 assetTag world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'tryGetTextureSizeAsVector2' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | "getTextureSizeAsVector2" ->
            match World.evalManyInternal exprs world with
            | struct ([|assetTag|] as args, world) when Array.notExists (function Scripting.Violation _ -> true | _ -> false) args ->
                getTextureSizeAsVector2 assetTag world
            | _ ->
                let violation = Scripting.Violation (["InvalidBindingInvocation"], "Incorrect number of arguments for binding 'getTextureSizeAsVector2' at:\n" + SymbolOrigin.tryPrint originOpt, None)
                struct (violation, world)
        | _ ->
            let violation = Scripting.Violation (["InvalidBindingInvocation"], "No binding exists for '" + fnName + "' at:\n" + SymbolOrigin.tryPrint originOpt, None)
            struct (violation, world)
