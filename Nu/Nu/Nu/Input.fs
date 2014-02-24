namespace Nu
open OpenTK
open Nu
open Nu.NuCore

[<AutoOpen>]
module InputModule =

    /// Describes a mouse button.
    /// A serializable value type.
    type [<StructuralEquality; StructuralComparison>] MouseButton =
        | MouseLeft
        | MouseCenter
        | MouseRight
        override this.ToString () =
            match this with
            | MouseLeft -> "Left"
            | MouseCenter -> "Center"
            | MouseRight -> "Right"

    /// Describes a mouse event.
    /// A serializable value type.
    type [<StructuralEquality; NoComparison>] MouseEvent =
        { Button : MouseButton
          Position : Vector2 }

    type [<StructuralEquality; NoComparison>] MouseState =
        { MousePosition : Vector2
          MouseDowns : MouseButton Set }