// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// A shape made ready to draw - its model matrix, material properties and model worked out once, so it can be drawn in
/// every render pass of a frame without working them out again.
type Drawing =
    { DrawingMatrix : Matrix4x4
      DrawingProperties : MaterialProperties
      DrawingModel : StaticModel AssetTag
      DrawingCastShadow : bool }

/// Immediate-mode drawing for visuals that have no simulation identity of their own - puppet parts, things in hand,
/// pickups, glints and muzzle flashes. Each function draws for one render pass only, so call them every frame from a
/// Render override rather than declaring entities for them.
[<RequireQualifiedAccess>]
module Draw =

    /// Whether a render pass draws dynamic visuals at all, and whether it draws ones that don't cast shadows. Light maps
    /// and reflections capture only the static scene.
    let passes (renderPass : RenderPass) castShadow (world : World) =
        match renderPass with
        | NormalPass -> true
        | ShadowPass _ -> castShadow && (World.getRenderer3dConfig world).LightShadowingEnabled
        | LightMapPass _ | ReflectionPass _ -> false

    /// Make a shape with a model matrix and material properties ready to draw.
    let prepareProperties partShape (matrix : Matrix4x4) (properties : MaterialProperties) castShadow =
        { DrawingMatrix = matrix
          DrawingProperties = properties
          DrawingModel = PartShapeType.getModel partShape
          DrawingCastShadow = castShadow }

    /// Make a shape with a model matrix ready to draw.
    let prepareMatrix surface partShape (matrix : Matrix4x4) (color : Color) emission castShadow =
        prepareProperties partShape matrix (SurfaceData.makeProperties Data.Surfaces[surface] color emission) castShadow

    /// Draw a shape made ready, if the render pass draws it.
    let drawing renderPass (drawing : Drawing) world =
        if passes renderPass drawing.DrawingCastShadow world then
            let mutable matrix = drawing.DrawingMatrix
            World.renderStaticModelFast
                (&matrix, drawing.DrawingCastShadow, Exterior, ValueNone, SurfaceType.BasicMaterial, drawing.DrawingProperties,
                 drawing.DrawingModel, DeferredRenderType, renderPass, world)

    /// Draw a shape with a model matrix.
    let shapeMatrix renderPass surface partShape (matrix : Matrix4x4) (color : Color) emission castShadow world =
        if passes renderPass castShadow world then
            drawing renderPass (prepareMatrix surface partShape matrix color emission castShadow) world

    /// Draw a shape placed, turned and sized. A vanishing size draws nothing.
    let shape renderPass surface partShape (position : Vector3) (rotation : Quaternion) (scale : Vector3) color emission castShadow world =
        if scale.X > 0.0005f && scale.Y > 0.0005f && scale.Z > 0.0005f then
            shapeMatrix renderPass surface partShape (Matrix4x4.CreateAffine (position, rotation, scale)) color emission castShadow world

    /// Draw a muzzle flash's light, lighting the area for an instant - in the normal pass only, so the light probes don't
    /// capture a passing flash.
    let flash (renderPass : RenderPass) (position : Vector3) world =
        if renderPass.IsNormalPass then
            World.enqueueRenderMessage3d
                (RenderLight3d
                    { LightId = 0x5A47_464C_4153_4800UL // stable for as long as the flash shines
                      Origin = position
                      Rotation = quatIdentity
                      Direction = v3Down
                      Color = Color (1.0f, 0.8f, 0.5f, 1.0f)
                      Brightness = 6.0f
                      AttenuationLinear = 1.0f / (6.0f * 8.0f) // attenuated as Nu's lights auto-attenuate
                      AttenuationQuadratic = 1.0f / (6.0f * 8.0f * 8.0f)
                      Specularity = 1.0f
                      LightCutoff = 8.0f
                      LightType = PointLight (0.0f, None)
                      ShadowingType = NoShadowing
                      DesireFog = false
                      RenderPass = renderPass })
                world
