// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open OpenTK
open SDL2
open Prime
open Nu

/// Describes a mouse button.
type [<Struct>] MouseButton =
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
        | SDL.SDL_BUTTON_LEFT -> MouseLeft
        | SDL.SDL_BUTTON_MIDDLE -> MouseCenter
        | SDL.SDL_BUTTON_RIGHT -> MouseRight
        | SDL.SDL_BUTTON_X1 -> MouseX1
        | SDL.SDL_BUTTON_X2 -> MouseX2
        | _ -> failwith "Invalid SDL mouse button."

    /// Check that the given mouse button is down.
    let isButtonDown mouseButton =
        let sdlMouseButton = toSdlButton mouseButton
        let sdlMouseButtonMask = SDL.SDL_BUTTON sdlMouseButton
        let (sdlMouseButtonState, _, _) = SDL.SDL_GetMouseState ()
        sdlMouseButtonState &&& sdlMouseButtonMask <> 0u

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
        let keyboardStatePtr = fst (SDL.SDL_GetKeyboardState ())
        let keyboardStatePtr = NativeInterop.NativePtr.ofNativeInt keyboardStatePtr
        let state = NativeInterop.NativePtr.get<byte> keyboardStatePtr scanCode
        state = byte 1

    /// Check that either ctrl key is down.
    let isCtrlDown () =
        isKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_LCTRL) ||
        isKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_RCTRL)

    /// Check that either alt key is down.
    let isAltDown () =
        isKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_LALT) ||
        isKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_RALT)

    /// Check that either shift key is down.
    let isShiftDown () =
        isKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_LSHIFT) ||
        isKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_RSHIFT)

/// Describes a gamepad button.
type [<Struct>] GamepadButton =
    | ButtonUp
    | ButtonLeft
    | ButtonDown
    | ButtonRight
    | ButtonStart
    | ButtonSelect
    | ButtonL
    | ButtonR
    | ButtonA
    | ButtonB
    | ButtonX
    | ButtonY
    override this.ToString () = scstring this
    static member toEventName this = ((scstring this).Substring "Gamepad".Length)

[<RequireQualifiedAccess>]        
module GamepadState =

    let mutable private Joysticks = [||]

    let SupportedButtonCount = 15

    /// Initialize gamepad state.
    let init () =
        let indices = SDL.SDL_NumJoysticks ()
        Joysticks <-
            Array.map (fun joystick ->
                // NOTE: we don't have a match call to SDL.SDL_JoystickClose, but it may not be necessary
                SDL.SDL_JoystickOpen joystick)
                [|0 .. indices|]

    /// Get the number of open gamepad.
    let getGamepadCount () =
        Array.length Joysticks

    /// Convert a GamepadButton to SDL's representation.
    let toSdlButton gamepadButton =
        match gamepadButton with
        | ButtonUp -> 0
        | ButtonLeft -> 2
        | ButtonDown -> 4
        | ButtonRight -> 6
        | ButtonStart -> 8
        | ButtonSelect -> 9
        | ButtonL -> 10
        | ButtonR -> 11
        | ButtonA -> 12
        | ButtonB -> 13
        | ButtonX -> 14
        | ButtonY -> 15

    /// Convert SDL's representation of a mouse button to a GamepadButton.
    let toNuButton gamepadButton =
        match gamepadButton with
        | 0 -> ButtonUp
        | 2 -> ButtonLeft
        | 4 -> ButtonDown
        | 6 -> ButtonRight
        | 8 -> ButtonStart
        | 9 -> ButtonSelect
        | 10 -> ButtonL
        | 11 -> ButtonR
        | 12 -> ButtonA
        | 13 -> ButtonB
        | 14 -> ButtonX
        | 15 -> ButtonY
        | _ -> failwith "Invalid SDL joystick button."

    /// Check that the given gamepad key is down.
    let isButtonDown index button =
        let sdlButton = toSdlButton button
        match Array.tryItem index Joysticks with
        | Some joystick ->
            match sdlButton with
            | 0 -> SDL.SDL_JoystickGetButton (joystick, 7) = byte 1 || SDL.SDL_JoystickGetButton (joystick, 0) = byte 1 || SDL.SDL_JoystickGetButton (joystick, 1) = byte 1
            | 2 -> SDL.SDL_JoystickGetButton (joystick, 1) = byte 1 || SDL.SDL_JoystickGetButton (joystick, 2) = byte 1 || SDL.SDL_JoystickGetButton (joystick, 3) = byte 1
            | 4 -> SDL.SDL_JoystickGetButton (joystick, 3) = byte 1 || SDL.SDL_JoystickGetButton (joystick, 4) = byte 1 || SDL.SDL_JoystickGetButton (joystick, 5) = byte 1
            | 6 -> SDL.SDL_JoystickGetButton (joystick, 5) = byte 1 || SDL.SDL_JoystickGetButton (joystick, 6) = byte 1 || SDL.SDL_JoystickGetButton (joystick, 7) = byte 1
            | _ -> SDL.SDL_JoystickGetButton (joystick, sdlButton) = byte 1
        | None -> false