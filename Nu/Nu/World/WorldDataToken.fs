// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open Prime

/// Data token processing functions for the world.
[<AutoOpen>]
module WorldDataToken =

    type World with

        static member internal renderDataToken renderPass dataToken world =

            match dataToken with
            | SpriteToken (elevation, horizon, assetTag, sprite) ->
                World.renderLayeredSpriteFast
                    (elevation, horizon, assetTag, &sprite.Transform, &sprite.InsetOpt, &sprite.ClipOpt,
                     sprite.Image, sprite.UserDefinedImage0, sprite.UserDefinedImage1, sprite.UserDefinedImage2, sprite.PipelineName,
                     &sprite.Color, sprite.Blend, &sprite.Emission, sprite.Flip,
                     sprite.UserDefinedSetting0, sprite.UserDefinedSetting1, sprite.UserDefinedSetting2, sprite.UserDefinedSetting3, sprite.UserDefinedSetting4, sprite.UserDefinedSetting5, sprite.UserDefinedSetting6, sprite.UserDefinedSetting7,
                     world)

            | TextToken (elevation, horizon, assetTag, text) ->
                let renderText =
                    { Transform = text.Transform
                      ClipOpt = text.ClipOpt
                      Text = text.Text
                      Font = text.Font
                      FontSizing = text.FontSizing
                      FontStyling = text.FontStyling
                      Color = text.Color
                      Justification = text.Justification
                      CaretOpt = None }
                World.enqueueLayeredOperation2d { Elevation = elevation; Horizon = horizon; AssetTag = assetTag; RenderOperation2d = RenderText renderText } world

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
                      Specularity = light.Specularity
                      LightCutoff = light.LightCutoff
                      LightType = light.LightType
                      ShadowingType = NoShadowing
                      DesireFog = false
                      RenderPass = renderPass }
                World.enqueueRenderMessage3d (RenderLight3d renderLight) world

            | BillboardToken billboard ->
                let renderBillboard =
                    { ModelMatrix = billboard.ModelMatrix
                      CastShadow = billboard.CastShadow
                      Presence = billboard.Presence
                      InsetOpt = billboard.InsetOpt
                      Material = billboard.Material
                      MaterialProperties = billboard.MaterialProperties
                      ShadowOffset = billboard.ShadowOffset
                      OrientUp = true
                      Planar = true
                      RenderType = billboard.RenderType
                      RenderPass = renderPass }
                World.enqueueRenderMessage3d (RenderBillboard renderBillboard) world

            | StaticModelToken staticModel ->
                let renderStaticModel =
                    { ModelMatrix = staticModel.ModelMatrix
                      CastShadow = staticModel.CastShadow
                      Presence = staticModel.Presence
                      InsetOpt = staticModel.InsetOpt
                      Material = staticModel.Material
                      MaterialProperties = staticModel.MaterialProperties
                      StaticModel = staticModel.StaticModel
                      RenderType = staticModel.RenderType
                      RenderPass = renderPass }
                World.enqueueRenderMessage3d (RenderStaticModel renderStaticModel) world

            | StaticModelSurfaceToken staticModelSurface ->
                let renderStaticModelSurface =
                    { ModelMatrix = staticModelSurface.ModelMatrix
                      CastShadow = staticModelSurface.CastShadow
                      Presence = staticModelSurface.Presence
                      InsetOpt = staticModelSurface.InsetOpt
                      Material = staticModelSurface.Material
                      MaterialProperties = staticModelSurface.MaterialProperties
                      StaticModel = staticModelSurface.StaticModel
                      SurfaceIndex = staticModelSurface.SurfaceIndex
                      RenderType = staticModelSurface.RenderType
                      RenderPass = renderPass }
                World.enqueueRenderMessage3d (RenderStaticModelSurface renderStaticModelSurface) world

            | EffectToken (_, _, _) ->
                () // nothing to do

            | EmitterToken (_, _) ->
                () // nothing to do

            | TagToken (_, _) ->
                () // nothing to do

            | DataTokens dataTokens ->
                for dataToken in dataTokens do
                    World.renderDataToken renderPass dataToken world