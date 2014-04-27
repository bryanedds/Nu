namespace Nu
open OpenTK
open Nu

[<AutoOpen>]
module CameraModule =

    /// The camera used to dictate what is rendered on the screen.
    type [<StructuralEquality; NoComparison>] Camera =
        { EyePosition : Vector2
          EyeSize : Vector2
          EyeZoom : single }

module Camera =

    /// The position of the center of the camera's eye.
    let getEyeCenter camera =
        camera.EyePosition + camera.EyeSize * 0.5f

    /// The view of the camera with original float values. Due to the problems with SDL_RenderCopyEx as described in
    /// NuMath.fs, using this function to decide on sprite coordinates is very, very bad for rendering.
    let getViewF camera =
        let translation = camera.EyePosition - camera.EyeSize * 0.5f
        let scale = -camera.EyeZoom + 2.0f
        Matrix3.makeFromTranslationAndScale translation scale

    /// The view of the camera with translation sliced on integers. Good for rendering.
    let getViewI camera =
        let translation = camera.EyePosition - camera.EyeSize * 0.5f
        let translationI = Vector2 (single <| int translation.X, single <| int translation.Y)
        let scale = -camera.EyeZoom + 2.0f
        Matrix3.makeFromTranslationAndScale translationI scale