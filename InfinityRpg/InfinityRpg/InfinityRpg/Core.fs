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

    type Direction =
        | Upward
        | Rightward
        | Downward
        | Leftward

        static member fromInt n =
            match n with
            | 0 -> Upward
            | 1 -> Rightward
            | 2 -> Downward
            | 3 -> Leftward
            | _ -> failwith <| "Invalid conversion to Direction from int '" + acstring n + "'."

[<AutoOpen>]
module AnimationModule =

    type AnimationData =
        { FrameCount : int
          FrameStutter : int }

[<AutoOpen>]
module Math =

    let vmtovi vm =
        Vector2i.Multiply (vm, TileSizeI)

    let vitovm vi =
        Vector2i.Divide (vi, TileSizeI)

    let vitovf (vi : Vector2i) =
        vi.Vector2

    let vftovi (vf : Vector2) =
        Vector2i vf

    let vmtovf vm =
        vm |> vmtovi |> vitovf

    let vftovm vf =
        vf |> vftovi |> vitovm

    let dtovm d =
        match d with
        | Upward -> Vector2i.Up
        | Rightward -> Vector2i.Right
        | Downward -> Vector2i.Down
        | Leftward -> Vector2i.Left

    let dtovi d =
        dtovm d |> vmtovi

    let dtovf d =
        d |> dtovi |> vitovf

    let vftod v =
        if v <> Vector2.Zero then
            let atan2 = Math.Atan2 (float v.Y, float v.X)
            let angle = if atan2 < 0.0 then atan2 + Math.PI * 2.0 else atan2
            if angle < Math.PI * 0.75 && angle >= Math.PI * 0.25 then Upward
            elif angle < Math.PI * 0.25 || angle >= Math.PI * 1.75 then Rightward
            elif angle < Math.PI * 1.75 && angle >= Math.PI * 1.25 then Downward
            else Leftward
        else failwith "Direction cannot be derived from Vector2.Zero."

    let vitod v =
        v |> vitovf |> vftod

    let vmtod v =
        v |> vmtovf |> vftod

    let arePositionMsAdjacent positionM positionM2 =
        positionM = positionM2 + Vector2i.Up ||
        positionM = positionM2 + Vector2i.Right ||
        positionM = positionM2 + Vector2i.Down ||
        positionM = positionM2 + Vector2i.Left

    let arePositionIsAdjacent positionI positionI2 =
        arePositionMsAdjacent (vitovm positionI) (vitovm positionI2)

    let arePositionsAdjacent position position2 =
        arePositionIsAdjacent (vftovi position) (vftovi position2)

[<AutoOpen>]
module MapBoundsModule =

    type MapBounds =
        { CornerNegative : Vector2i
          CornerPositive : Vector2i }

        static member isPointInBounds (point : Vector2i) bounds =
            not
                (point.X < bounds.CornerNegative.X ||
                 point.X > bounds.CornerPositive.X ||
                 point.Y < bounds.CornerNegative.Y ||
                 point.Y > bounds.CornerPositive.Y)