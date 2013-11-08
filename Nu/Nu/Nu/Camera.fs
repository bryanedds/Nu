module Nu.Camera
open OpenTK

/// The camera used to dictate what is rendered on the screen.
type [<StructuralEquality; NoComparison>] Camera =
    { EyePosition : Vector2
      EyeSize : Vector2 }

/// The position of the center of the camera's eye.
let getEyeCenter camera =
    camera.EyePosition + camera.EyeSize * 0.5f

/// The inverse view of the camera, with floating-point accuracy (not good for rendering).
let inverseViewF camera =
    camera.EyePosition - camera.EyeSize * 0.5f

/// The inverse view of the camera, with pixel-level accuracy (good for rendering).
let inverseView camera =
    let inverseViewF = inverseViewF camera
    Vector2 (single <| int inverseViewF.X, single <| int inverseViewF.Y)