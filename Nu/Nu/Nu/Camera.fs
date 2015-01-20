// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open OpenTK
open Nu

[<AutoOpen>]
module CameraModule =

    /// The camera used to dictate what is rendered on the screen.
    ///
    /// Due to the complexity of implementing view scaling using the SDL drawing primitives, Nu has
    /// opted to be a pixel-perfect game engine without zooming. Once Nu's renderer is replaced
    /// with direct calls to OpenGL, zooming will likely be implemented.
    ///
    /// If 3D is implemented in Nu, Camera will have to be changed to be 3D instead of 2D.
    type [<StructuralEquality; NoComparison>] Camera =
        { EyeCenter : Vector2
          EyeSize : Vector2 }

[<RequireQualifiedAccess>]
module Camera =

    /// Get the view of the camera in absolute terms (world space).
    let getViewAbsolute (_ : Camera) =
        Matrix3.Identity
        
    /// Get the view of the camera in absolute terms (world space) with translation sliced on
    /// integers.
    let getViewAbsoluteI (_ : Camera) =
        Matrix3.Identity

    /// The relative view of the camera with original single values. Due to the problems with
    /// SDL_RenderCopyEx as described in Math.fs, using this function to decide on sprite
    /// coordinates is very, very bad for rendering.
    let getViewRelative camera =
        let translation = camera.EyeCenter
        Matrix3.CreateFromTranslation translation

    /// The relative view of the camera with translation sliced on integers. Good for rendering.
    let getViewRelativeI camera =
        let translation = camera.EyeCenter
        let translationI = Vector2 (single <| int translation.X, single <| int translation.Y)
        Matrix3.CreateFromTranslation translationI

    /// Get the bounds of the camera's sight relative to its position.
    let getViewBoundsRelative camera =
        Vector4 (
            camera.EyeCenter.X - camera.EyeSize.X * 0.5f,
            camera.EyeCenter.Y - camera.EyeSize.Y * 0.5f,
            camera.EyeCenter.X + camera.EyeSize.X * 0.5f,
            camera.EyeCenter.Y + camera.EyeSize.Y * 0.5f)

    /// Get the bounds of the camera's sight not relative to its position.
    let getViewBoundsAbsolute camera =
        Vector4 (
            camera.EyeSize.X * -0.5f,
            camera.EyeSize.Y * -0.5f,
            camera.EyeSize.X * 0.5f,
            camera.EyeSize.Y * 0.5f)

    /// Get the bounds of the camera's sight.
    let getViewBounds viewType camera =
        match viewType with
        | Relative -> getViewBoundsRelative camera
        | Absolute -> getViewBoundsAbsolute camera

    /// Query that the given bounds is within the camera's sight.
    let inView viewType (bounds : Vector4) camera =
        let viewBounds = getViewBounds viewType camera
        Math.isBoundsInBounds bounds viewBounds

    /// Query that the given bounds is within the camera's sight.
    let inView3 viewType (position : Vector2) (size : Vector2) camera =
        let viewBounds = getViewBounds viewType camera
        Math.isBoundsInBounds3 position size viewBounds

    /// Transform the given mouse position to screen space.
    let mouseToScreen (mousePosition : Vector2) camera =
        let positionScreen =
            Vector2 (
                mousePosition.X - camera.EyeSize.X * 0.5f,
                -(mousePosition.Y - camera.EyeSize.Y * 0.5f)) // negation for right-handedness
        positionScreen

    /// Transform the given mouse position to world space.
    let mouseToWorld viewType mousePosition camera =
        let positionScreen = mouseToScreen mousePosition camera
        let getView =
            match viewType with
            | Relative -> getViewRelative
            | Absolute -> getViewAbsolute
        let view = getView camera
        let positionEntity = positionScreen * view
        positionEntity

    /// Transform the given mouse position to local space.
    let mouseToLocal viewType localPosition mousePosition camera =
        let positionWorld = mouseToWorld viewType mousePosition camera
        localPosition - positionWorld