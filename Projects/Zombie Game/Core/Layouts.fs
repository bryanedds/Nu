// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// The layouts a scene stands on.
[<RequireQualifiedAccess>]
module Layouts =

    /// Where a point along a wall of a chamber of a half width and depth is, and the way it faces into the chamber.
    let private getWallFace halfWidth halfDepth wall (along : single) =
        match wall with
        | North -> (v3 along 0.0f -halfDepth, 0.0f)
        | South -> (v3 along 0.0f halfDepth, MathF.PI)
        | East -> (v3 halfWidth 0.0f along, -MathF.PI_OVER_2)
        | West -> (v3 -halfWidth 0.0f along, MathF.PI_OVER_2)

    /// Build a chamber - a floor, walled all round to the ceiling, a band of darker paint along the walls' feet, broken
    /// where each doorway - its wall, where along it, and how wide - goes through, and the ceiling over it, unless it's
    /// outdoors - a one-way quad facing down, so a fixed eye above it sees through it, drawn forward so screen-space
    /// reflections leave it out. Its origin is the middle of its floor.
    let buildChamber (chamber : ChamberData) (doorways : (WallType * single * single) list) (builder : Builder) =
        let floorSurface = defaultArg chamber.FloorSurface Linoleum
        let wallSurface = defaultArg chamber.WallSurface Paint
        let floorColor = builder.Color chamber.Floor
        let wallColor = builder.Color chamber.Walls
        let dadoColor = builder.Color chamber.Dado
        let width = chamber.Size.X
        let depth = chamber.Size.Y
        let halfWidth = width * 0.5f
        let halfDepth = depth * 0.5f

        // the floor, its top level with the ground walkers stand on, reaching under the walls
        builder.Part floorSurface PartBox (v3 0.0f -0.05f 0.0f) quatIdentity (v3 (width + Constants.Scenery.WallThickness * 2.0f) 0.1f (depth + Constants.Scenery.WallThickness * 2.0f)) floorColor

        // the walls, all to the ceiling, each solid to walkers, with the dado's band along its inner face, broken where a
        // doorway goes through
        let wallSpans =
            [(North, v3 0.0f 0.0f (-halfDepth - Constants.Scenery.WallThickness * 0.5f), v3 (width + Constants.Scenery.WallThickness * 2.0f) 0.0f Constants.Scenery.WallThickness, v3 0.0f 0.0f 1.0f)
             (South, v3 0.0f 0.0f (halfDepth + Constants.Scenery.WallThickness * 0.5f), v3 (width + Constants.Scenery.WallThickness * 2.0f) 0.0f Constants.Scenery.WallThickness, v3 0.0f 0.0f -1.0f)
             (West, v3 (-halfWidth - Constants.Scenery.WallThickness * 0.5f) 0.0f 0.0f, v3 Constants.Scenery.WallThickness 0.0f depth, v3 1.0f 0.0f 0.0f)
             (East, v3 (halfWidth + Constants.Scenery.WallThickness * 0.5f) 0.0f 0.0f, v3 Constants.Scenery.WallThickness 0.0f depth, v3 -1.0f 0.0f 0.0f)]
        for (wall, center, size, inward) in wallSpans do
            builder.Part wallSurface PartBox (center.WithY (Constants.Scenery.WallHeight * 0.5f)) quatIdentity (size.WithY Constants.Scenery.WallHeight) wallColor
            builder.Box (v2 center.X center.Z) (v2 (size.X * 0.5f) (size.Z * 0.5f))
            let face = center + inward * (Constants.Scenery.WallThickness * 0.5f)
            let alongZ = abs inward.X > 0.5f
            let halfLength = if alongZ then halfDepth else halfWidth
            let breaks =
                doorways |>
                List.choose (fun (doorwayWall, along, doorwayWidth) ->
                    if doorwayWall = wall
                    then Some (along - doorwayWidth * 0.5f - Constants.Scenery.DoorwayDadoTuck, along + doorwayWidth * 0.5f + Constants.Scenery.DoorwayDadoTuck)
                    else None) |>
                List.sortBy fst
            let (runs, start) =
                breaks |>
                List.fold (fun (runs, start) (low, high) -> ((if low > start then (start, low) :: runs else runs), max start high)) ([], -halfLength)
            let runs = if start < halfLength then (start, halfLength) :: runs else runs
            for (low, high) in runs do
                let middle = (low + high) * 0.5f
                let offset = if alongZ then v3 0.0f 0.0f middle else v3 middle 0.0f 0.0f
                let band = if alongZ then v3 0.02f Constants.Scenery.DadoHeight (high - low) else v3 (high - low) Constants.Scenery.DadoHeight 0.02f
                builder.Part wallSurface PartBox (face + offset + inward * 0.01f + v3 0.0f (Constants.Scenery.DadoHeight * 0.5f) 0.0f) quatIdentity band dadoColor

        // the ceiling, facing down onto the floor
        if not chamber.Outdoors then
            let facingDown = Quaternion.CreateFromAxisAngle (v3Right, MathF.PI_OVER_2)
            builder.Unreflected wallSurface PartQuad (v3 0.0f Constants.Scenery.WallHeight 0.0f) facingDown (v3 width depth 1.0f) wallColor

        { IsOpen = fun point -> abs point.X < halfWidth && abs point.Y < halfDepth
          WallFace = getWallFace halfWidth halfDepth
          WalkMin = v2 (-halfWidth + 0.3f) (-halfDepth + 0.3f)
          WalkMax = v2 (halfWidth - 0.3f) (halfDepth - 0.3f) }
