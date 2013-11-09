namespace Nu
open OpenTK

/// The camera used to dictate what is rendered on the screen.
type [<StructuralEquality; NoComparison>] Camera =
    { EyePosition : Vector2
      EyeSize : Vector2 }

module CameraModule =

    /// The position of the center of the camera's eye.
    let getEyeCenter camera =
        camera.EyePosition + camera.EyeSize * 0.5f

    /// The inverse view of the camera, with floating-point accuracy (not good for rendering).
    let getInverseViewF camera =
        camera.EyePosition - camera.EyeSize * 0.5f

    /// The inverse view of the camera, with pixel-level accuracy (good for rendering).
    let getInverseView camera =
        let getInverseViewF = getInverseViewF camera
        Vector2 (single <| int getInverseViewF.X, single <| int getInverseViewF.Y)