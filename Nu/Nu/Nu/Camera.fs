namespace Nu
open OpenTK
open Nu

[<AutoOpen>]
module CameraModule =

    /// The camera used to dictate what is rendered on the screen.
    ///
    /// Due to the complexity of implementing view scaling using the SDL drawing primitives, Nu has
    /// opted to be a pixel-perfect game engine without scaling. Once Nu's renderer is replaced
    /// with direct calls to OpenGL, scaling will likely be implemented.
    ///
    /// If 3D is implemented in Nu, Camera will have to be changed to be 3D instead of 2D.
    type [<StructuralEquality; NoComparison>] Camera =
        { EyeCenter : Vector2
          EyeSize : Vector2 }

[<RequireQualifiedAccess>]
module Camera =

    let getViewAbsoluteF (_ : Camera) =
        Matrix3.identity
        
    let getViewAbsoluteI (_ : Camera) =
        Matrix3.identity

    /// The relative view of the camera with original float values. Due to the problems with
    /// SDL_RenderCopyEx as described in NuMath.fs, using this function to decide on sprite
    /// coordinates is very, very bad for rendering.
    let getViewRelativeF camera =
        let translation = camera.EyeCenter
        Matrix3.makeFromTranslation translation

    /// The relative view of the camera with translation sliced on integers. Good for rendering.
    let getViewRelativeI camera =
        let translation = camera.EyeCenter
        let translationI = Vector2 (single <| int translation.X, single <| int translation.Y)
        Matrix3.makeFromTranslation translationI

    let mouseToScreen (position : Vector2) camera =
        let positionScreen =
            Vector2 (
                position.X - camera.EyeSize.X * 0.5f,
                -(position.Y - camera.EyeSize.Y * 0.5f)) // negation for right-handedness
        positionScreen