namespace Nu
open OpenTK
open SDL2
open Nu

[<AutoOpen>]
module MouseButtonModule =

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

module MouseButton =

    let toSdl mouseButton =
        match mouseButton with
        | MouseLeft -> SDL.SDL_BUTTON_LEFT
        | MouseCenter -> SDL.SDL_BUTTON_MIDDLE
        | MouseRight -> SDL.SDL_BUTTON_RIGHT

    let toNu mouseButton =
        match mouseButton with
        | SDL2.SDL.SDL_BUTTON_LEFT -> MouseLeft
        | SDL2.SDL.SDL_BUTTON_MIDDLE -> MouseCenter
        | SDL2.SDL.SDL_BUTTON_RIGHT -> MouseRight
        | _ -> failwith "Invalid SDL mouse button."

