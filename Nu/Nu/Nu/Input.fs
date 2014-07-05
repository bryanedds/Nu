namespace Nu
open OpenTK
open Nu

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

    type [<StructuralEquality; NoComparison>] MouseState =
        { MousePosition : Vector2
          MouseDowns : MouseButton Set }