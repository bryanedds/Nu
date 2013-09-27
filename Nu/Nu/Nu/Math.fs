module Nu.Math
open OpenTK

let isInBox3 (point : Vector2) (boxPos : Vector2) (boxSize : Vector2) =
    point.X >= boxPos.X &&
    point.X < boxPos.X + boxSize.X &&
    point.Y >= boxPos.Y &&
    point.Y < boxPos.Y + boxSize.Y

let isInBox (point : Vector2) (box : Box2) =
    isInBox3
        point
        (Vector2 (box.Left, box.Top))
        (Vector2 (box.Right, box.Bottom))