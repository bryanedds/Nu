module Nu.Camera
open OpenTK

type [<StructuralEquality; NoComparison>] Camera =
    { EyePosition : Vector2
      EyeSize : Vector2 }