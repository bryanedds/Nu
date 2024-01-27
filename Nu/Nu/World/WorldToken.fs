// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open Prime

[<AutoOpen>]
module WorldToken =

    type World with

        static member internal renderToken renderPass token world =
            match token with

            // render sprite
            | SpriteToken (elevation, horizon, assetTag, sprite) ->
                World.renderLayeredSpriteFast (elevation, horizon, assetTag, &sprite.Transform, &sprite.InsetOpt, sprite.Image, &sprite.Color, sprite.Blend, &sprite.Emission, sprite.Flip, world)

            // render text
            | TextToken (elevation, horizon, assetTag, text) ->
                let renderText =
                    { Transform = text.Transform
                      Text = text.Text
                      Font = text.Font
                      FontSize = text.FontSize
                      FontStyle = text.FontStyle
                      Color = text.Color
                      Justification = text.Justification }
                World.enqueueLayeredOperation2d { Elevation = elevation; Horizon = horizon; AssetTag = assetTag; RenderOperation2d = RenderText renderText } world

            // render 3d light
            | Light3dToken light ->
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
            | BillboardToken billboard ->
                let renderBillboard =
                    { Absolute = billboard.Absolute
                      Presence = billboard.Presence
                      ModelMatrix = billboard.ModelMatrix
                      InsetOpt = billboard.InsetOpt
                      MaterialProperties = billboard.MaterialProperties
                      Material = billboard.Material
                      RenderType = billboard.RenderType
                      RenderPass = renderPass }
                World.enqueueRenderMessage3d (RenderBillboard renderBillboard) world

            // render static model
            | StaticModelToken staticModel ->
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
            | StaticModelSurfaceToken staticModelSurface ->
                let renderStaticModelSurface =
                    { Absolute = staticModelSurface.Absolute
                      ModelMatrix = staticModelSurface.ModelMatrix
                      Presence = staticModelSurface.Presence
                      InsetOpt = staticModelSurface.InsetOpt
                      MaterialProperties = staticModelSurface.MaterialProperties
                      Material = staticModelSurface.Material
                      StaticModel = staticModelSurface.StaticModel
                      SurfaceIndex = staticModelSurface.SurfaceIndex
                      RenderType = staticModelSurface.RenderType
                      RenderPass = renderPass }
                World.enqueueRenderMessage3d (RenderStaticModelSurface renderStaticModelSurface) world

            // nothing to do
            | EffectToken (_, _, _) -> ()

            // nothing to do
            | EmitterToken (_, _) -> ()

            // nothing to do
            | TagToken (_, _) -> ()

            // recur
            | Tokens tokens ->
                for token in tokens do
                    World.renderToken renderPass token world