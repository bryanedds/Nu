// Nu Game Engine.
// Copyright (C) Bryan Edds.

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
                World.renderLayeredSpriteFast (elevation, horizon, assetTag, &sprite.Transform, &sprite.InsetOpt, &sprite.ClipOpt, sprite.Image, &sprite.Color, sprite.Blend, &sprite.Emission, sprite.Flip, world)

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
                      LightCutoff = light.LightCutoff
                      LightType = light.LightType
                      DesireShadows = false
                      DesireFog = false
                      Bounds = light.Bounds
                      RenderPass = renderPass }
                World.enqueueRenderMessage3d (RenderLight3d renderLight) world

            | BillboardToken billboard ->
                let renderBillboard =
                    { ModelMatrix = billboard.ModelMatrix
                      CastShadow = billboard.CastShadow
                      Presence = billboard.Presence
                      InsetOpt = billboard.InsetOpt
                      MaterialProperties = billboard.MaterialProperties
                      Material = billboard.Material
                      ShadowOffset = billboard.ShadowOffset
                      DepthTest = LessThanOrEqualTest
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
                      MaterialProperties = staticModel.MaterialProperties
                      StaticModel = staticModel.StaticModel
                      Clipped = staticModel.Clipped
                      DepthTest = LessThanOrEqualTest
                      RenderType = staticModel.RenderType
                      RenderPass = renderPass }
                World.enqueueRenderMessage3d (RenderStaticModel renderStaticModel) world

            | StaticModelSurfaceToken staticModelSurface ->
                let renderStaticModelSurface =
                    { ModelMatrix = staticModelSurface.ModelMatrix
                      CastShadow = staticModelSurface.CastShadow
                      Presence = staticModelSurface.Presence
                      InsetOpt = staticModelSurface.InsetOpt
                      MaterialProperties = staticModelSurface.MaterialProperties
                      Material = staticModelSurface.Material
                      StaticModel = staticModelSurface.StaticModel
                      SurfaceIndex = staticModelSurface.SurfaceIndex
                      DepthTest = LessThanOrEqualTest
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