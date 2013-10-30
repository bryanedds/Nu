module Nu.Camera
open OpenTK

/// The camera used to dictate what is rendered on the screen.
type [<StructuralEquality; NoComparison>] Camera =
    { EyePosition : Vector2
      EyeSize : Vector2 }