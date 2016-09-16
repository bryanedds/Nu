// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.ComponentModel
open OpenTK
open SDL2
open Prime
open Nu

/// Describes a mouse button.
type MouseButton =
    | MouseLeft
    | MouseCenter
    | MouseRight
    | MouseX1
    | MouseX2
    override this.ToString () = scstring this
    static member toEventName this = ((scstring this).Substring "Mouse".Length)

[<RequireQualifiedAccess>]
module MouseState =

    /// Convert a MouseButton to SDL's representation.
    let toSdlButton mouseButton =
        match mouseButton with
        | MouseLeft -> SDL.SDL_BUTTON_LEFT
        | MouseCenter -> SDL.SDL_BUTTON_MIDDLE
        | MouseRight -> SDL.SDL_BUTTON_RIGHT
        | MouseX1 -> SDL.SDL_BUTTON_X1
        | MouseX2 -> SDL.SDL_BUTTON_X2

    /// Convert SDL's representation of a mouse button to a MouseButton.
    let toNuButton mouseButton =
        match mouseButton with
        | SDL2.SDL.SDL_BUTTON_LEFT -> MouseLeft
        | SDL2.SDL.SDL_BUTTON_MIDDLE -> MouseCenter
        | SDL2.SDL.SDL_BUTTON_RIGHT -> MouseRight
        | SDL2.SDL.SDL_BUTTON_X1 -> MouseX1
        | SDL2.SDL.SDL_BUTTON_X2 -> MouseX2
        | _ -> failwith "Invalid SDL mouse button."

    /// Check that the given mouse button is down.
    let isButtonDown mouseButton =
        let sdlMouseButton = toSdlButton mouseButton
        SDL.SDL_BUTTON sdlMouseButton = 1u

    /// Get the position of the mouse.
    let getPosition () =
        let (_, x, y) = SDL.SDL_GetMouseState ()
        Vector2i (x, y)

    /// Get the position of the mouse in floating-point coordinates.
    let getPositionF world =
        let mousePosition = getPosition world
        Vector2 (single mousePosition.X, single mousePosition.Y)

[<RequireQualifiedAccess>]
module KeyboardState =

    /// Check that the given keyboard key is down.
    let isKeyDown scanCode =
        let keyboardStatePtr = fst ^ SDL.SDL_GetKeyboardState ()
        let keyboardStatePtr = NativeInterop.NativePtr.ofNativeInt keyboardStatePtr
        let state = NativeInterop.NativePtr.get<byte> keyboardStatePtr scanCode
        state = byte 1