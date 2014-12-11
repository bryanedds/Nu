namespace InfinityRpg
open System
open OpenTK
open Prime
open Nu
open Nu.Constants
open InfinityRpg
open InfinityRpg.Constants

[<AutoOpen>]
module DirectionModule =

    // TODO: rename to Up, Right, Down, Left
    type Direction =
        | North
        | East
        | South
        | West

[<AutoOpen>]
module AnimationModule =

    type AnimationData =
        { FrameCount : int
          FrameStutter : int }

module Math =

    let arePositionMsAdjacent positionM positionM2 =
        positionM = positionM2 + Vector2i.Up ||
        positionM = positionM2 + Vector2i.Right ||
        positionM = positionM2 + Vector2i.Down ||
        positionM = positionM2 + Vector2i.Left

    let arePositionsAdjacent position position2 =
        position = position2 + Vector2 (0.0f, TileSize.Y) ||
        position = position2 + Vector2 (TileSize.X, 0.0f) ||
        position = position2 - Vector2 (0.0f, TileSize.Y) ||
        position = position2 - Vector2 (TileSize.X, 0.0f)