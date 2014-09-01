namespace Nu
open OpenTK
open Nu

[<AutoOpen>]
module InputModule =

    // TODO: implement keyboard input

    /// Describes a mouse button.
    type [<StructuralEquality; StructuralComparison>] MouseButton =
        | MouseLeft
        | MouseCenter
        | MouseRight
        override this.ToString () =
            match this with
            | MouseLeft -> "Left"
            | MouseCenter -> "Center"
            | MouseRight -> "Right"

    /// The current state of the mouse.
    type [<StructuralEquality; NoComparison>] MouseState =
        { MousePosition : Vector2
          MouseDowns : MouseButton Set }