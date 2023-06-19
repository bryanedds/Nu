// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open Prime
open Nu

[<AutoOpen>]
module WorldView =

    type World with

        static member internal renderView view world =
            match view with
            | Render2d (elevation, horizon, assetTag, operation) -> World.enqueueLayeredOperation2d { Elevation = elevation; Horizon = horizon; AssetTag = AssetTag.generalize assetTag; RenderOperation2d = operation } world
            | Render3d renderMessage -> World.enqueueRenderMessage3d renderMessage world
            | PlaySound (volume, assetTag) -> World.playSound volume assetTag world
            | PlaySong (fadeIn, fadeOut, start, volume, assetTag) -> World.playSong fadeIn fadeOut start volume assetTag world
            | FadeOutSong fade -> World.fadeOutSong fade world
            | StopSong -> World.stopSong world
            | SpawnEmitter (_, _) -> world
            | Tag _ -> world
            | Views views -> Array.fold (fun world view -> World.renderView view world) world views
            | SegmentedViews views -> SArray.fold (fun world view -> World.renderView view world) world views