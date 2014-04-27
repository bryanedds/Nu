namespace Nu
open OpenTK
open Nu

[<AutoOpen>]
module CameraModule =

    /// The camera used to dictate what is rendered on the screen.
    type [<StructuralEquality; NoComparison>] Camera =
        { EyeCenter : Vector2
          EyeSize : Vector2
          EyeZoom : single }

module Camera =

    /// The view of the camera with original float values. Due to the problems with SDL_RenderCopyEx as described in
    /// NuMath.fs, using this function to decide on sprite coordinates is very, very bad for rendering.
    let getViewF camera =
        Matrix3.makeFromTranslationAndScale camera.EyeCenter camera.EyeZoom

    /// The view of the camera with translation sliced on integers. Good for rendering.
    let getViewI camera =
        let translation = camera.EyeCenter
        let translationI = Vector2 (single <| int translation.X, single <| int translation.Y)
        Matrix3.makeFromTranslationAndScale translationI camera.EyeZoom