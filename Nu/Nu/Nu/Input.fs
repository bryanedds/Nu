namespace Nu
open OpenTK
open Nu.Core
open System

/// Describes a mouse button.
/// A serializable value type.
type [<CustomEquality; CustomComparison>] MouseButton =
    | MouseLeft
    | MouseCenter
    | MouseRight

    interface IComparable with
        member this.CompareTo thatObj =
            match thatObj with
            | :? MouseButton as that ->
                let thisInt = this.Int ()
                let thatInt = that.Int ()
                thisInt.CompareTo thatInt
            | _ -> failwith "Invalid MouseButton comparison."

    override this.Equals thatObj =
        match thatObj with
        | :? MouseButton as that ->
            let thisInt = this.Int ()
            let thatInt = that.Int ()
            thisInt = thatInt
        | _ -> false

    override this.GetHashCode () =
        this.Int ()

    member this.Int () =
        match this with
        | MouseLeft -> 0
        | MouseCenter -> 1
        | MouseRight -> 2

    member this.Address () =
        match this with
        | MouseLeft -> "left"
        | MouseCenter -> "center"
        | MouseRight -> "right"

/// Describes a mouse event.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] MouseEvent =
    { Button : MouseButton
      Position : Vector2 }

type [<StructuralEquality; NoComparison>] MouseState =
    { MousePosition : Vector2
      MouseDowns : MouseButton Set }