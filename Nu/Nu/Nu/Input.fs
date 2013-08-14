module Nu.Input
open Nu.Core

/// Describes human input.
/// A serializable value type.
type [<StructuralEquality; StructuralComparison>] HumanInput =
    | PadInput // of ...
    | KeyboardInput // of ...
    | MouseInput // of ...

/// Describes a mouse button.
/// A serializable value type.
type [<StructuralEquality; StructuralComparison>] MouseButton =
    | MouseLeft
    | MouseRight
    | MouseCenter

/// Describes a mouse event.
/// A serializable value type.
type [<StructuralEquality; StructuralComparison>] MouseEvent =
    { Button : MouseButton
      Position : Vector2 }

