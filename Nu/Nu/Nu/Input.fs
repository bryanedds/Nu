namespace Nu
open OpenTK
open Nu.Core

/// Describes a mouse button.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] MouseButton =
    | MouseLeft
    | MouseRight
    | MouseCenter

/// Describes a mouse event.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] MouseEvent =
    { Button : MouseButton
      Position : Vector2 }

type [<StructuralEquality; NoComparison>] MouseState =
    { MousePosition : Vector2
      MouseLeftDown : bool
      MouseRightDown : bool
      MouseCenterDown : bool }