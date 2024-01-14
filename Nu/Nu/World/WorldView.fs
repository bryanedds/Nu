// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open Prime

[<AutoOpen>]
module WorldView =

    type World with

        static member internal renderView renderPass view world =
            match view with

            // render sprite
            | SpriteView (elevation, horizon, assetTag, sprite) ->
                World.renderLayeredSpriteFast (elevation, horizon, assetTag, &sprite.Transform, &sprite.InsetOpt, sprite.Image, &sprite.Color, sprite.Blend, &sprite.Emission, sprite.Flip, world)

            // render text
            | TextView (elevation, horizon, assetTag, text) ->
                let renderText =
                    { Transform = text.Transform
                      Text = text.Text
                      Font = text.Font
                      Color = text.Color
                      Justification = text.Justification }
                World.enqueueLayeredOperation2d { Elevation = elevation; Horizon = horizon; AssetTag = assetTag; RenderOperation2d = RenderText renderText } world

            // render 3d light
            | Light3dView light ->
                let renderLight =
                    { LightId = light.LightId
                      Origin = light.Origin
                      Rotation = light.Rotation
                      Direction = light.Direction
                      Color = light.Color
                      Brightness = light.Brightness
                      AttenuationLinear = light.AttenuationLinear
                      AttenuationQuadratic = light.AttenuationQuadratic
                      LightCutoff = light.LightCutoff
                      LightType = light.LightType
                      DesireShadows = false
                      RenderPass = renderPass }
                World.enqueueRenderMessage3d (RenderLight3d renderLight) world

            // render billboard
            | BillboardView billboard ->
                let renderBillboard =
                    { Absolute = billboard.Absolute
                      ModelMatrix = billboard.ModelMatrix
                      InsetOpt = billboard.InsetOpt
                      MaterialProperties = billboard.MaterialProperties
                      Material = billboard.Material
                      RenderType = billboard.RenderType
                      RenderPass = renderPass }
                World.enqueueRenderMessage3d (RenderBillboard renderBillboard) world

            // render static model
            | StaticModelView staticModel ->
                let renderStaticModel =
                    { Absolute = staticModel.Absolute
                      ModelMatrix = staticModel.ModelMatrix
                      Presence = staticModel.Presence
                      InsetOpt = staticModel.InsetOpt
                      MaterialProperties = staticModel.MaterialProperties
                      StaticModel = staticModel.StaticModel
                      RenderType = staticModel.RenderType
                      RenderPass = renderPass }
                World.enqueueRenderMessage3d (RenderStaticModel renderStaticModel) world

            // render static model surface
            | StaticModelSurfaceView staticModelSurface ->
                let renderStaticModelSurface =
                    { Absolute = staticModelSurface.Absolute
                      ModelMatrix = staticModelSurface.ModelMatrix
                      InsetOpt = staticModelSurface.InsetOpt
                      MaterialProperties = staticModelSurface.MaterialProperties
                      Material = staticModelSurface.Material
                      StaticModel = staticModelSurface.StaticModel
                      SurfaceIndex = staticModelSurface.SurfaceIndex
                      RenderType = staticModelSurface.RenderType
                      RenderPass = renderPass }
                World.enqueueRenderMessage3d (RenderStaticModelSurface renderStaticModelSurface) world

            // nothing to do
            | SpawnEmitter (_, _) -> ()

            // nothing to do
            | Tag (_, _) -> ()

            // recur
            | Views views ->
                for view in views do
                    World.renderView renderPass view world