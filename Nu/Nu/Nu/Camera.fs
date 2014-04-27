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
    let getViewAbsoluteF camera =
        let translation = -camera.EyeSize * 0.5f
        Matrix3.makeFromTranslation translation
        
    /// The view of the camera with translation sliced on integers. Good for rendering.
    let getViewAbsoluteI camera =
        let translation = -camera.EyeSize * 0.5f
        let translationI = Vector2 (single <| int translation.X, single <| int translation.Y)
        Matrix3.makeFromTranslation translationI

    let getViewRelativeF camera =
        let translation = camera.EyeCenter - camera.EyeSize * 0.5f
        Matrix3.makeFromTranslationAndScale translation camera.EyeZoom

    let getViewRelativeI camera =
        let translation = camera.EyeCenter - camera.EyeSize * 0.5f
        let translationI = Vector2 (single <| int translation.X, single <| int translation.Y)
        Matrix3.makeFromTranslationAndScale translationI camera.EyeZoom