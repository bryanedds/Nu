// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open SDL2
open Prime

// for NativeInterop with KeyboardState
#nowarn "9"

/// Describes a mouse button.
type MouseButton =
    | MouseLeft
    | MouseMiddle
    | MouseRight
    | MouseX1
    | MouseX2
    static member toEventName this = this.ToString().Substring "Mouse".Length
    static member ofEventName eventName = scvalue<MouseButton> ("Mouse" + eventName)

/// Describes a gamepad direction.
type GamepadDirection =
    | DirectionUp
    | DirectionUpLeft
    | DirectionLeft
    | DirectionDownLeft
    | DirectionDown
    | DirectionDownRight
    | DirectionRight
    | DirectionUpRight
    | DirectionCentered

/// Describes a gamepad button.
type GamepadButton =
    | ButtonA
    | ButtonB
    | ButtonX
    | ButtonY
    | ButtonL
    | ButtonR
    | ButtonSelect
    | ButtonStart

[<RequireQualifiedAccess>]
module internal MouseState =

    /// Convert a MouseButton to SDL's representation.
    let internal toSdlButton mouseButton =
        match mouseButton with
        | MouseLeft -> SDL.SDL_BUTTON_LEFT
        | MouseMiddle -> SDL.SDL_BUTTON_MIDDLE
        | MouseRight -> SDL.SDL_BUTTON_RIGHT
        | MouseX1 -> SDL.SDL_BUTTON_X1
        | MouseX2 -> SDL.SDL_BUTTON_X2

    /// Convert SDL's representation of a mouse button to a MouseButton.
    let internal toNuButton mouseButton =
        match mouseButton with
        | SDL.SDL_BUTTON_LEFT -> MouseLeft
        | SDL.SDL_BUTTON_MIDDLE -> MouseMiddle
        | SDL.SDL_BUTTON_RIGHT -> MouseRight
        | SDL.SDL_BUTTON_X1 -> MouseX1
        | SDL.SDL_BUTTON_X2 -> MouseX2
        | _ -> failwith "Invalid SDL mouse button."

    /// Get the position of the mouse.
    let internal getPosition () =
        let (_, x, y) = SDL.SDL_GetMouseState ()
        v2 (single x) (single y)

    /// Check that the given mouse button is down.
    let internal isButtonDown mouseButton =
        let sdlMouseButton = toSdlButton mouseButton
        let sdlMouseButtonMask = SDL.SDL_BUTTON sdlMouseButton
        let (sdlMouseButtonState, _, _) = SDL.SDL_GetMouseState ()
        sdlMouseButtonState &&& sdlMouseButtonMask <> 0u

    /// Check that the given mouse button is up.
    let internal isButtonUp mouseButton =
        not (isButtonDown mouseButton)

[<RequireQualifiedAccess>]
module internal KeyboardState =

    /// Check that the given keyboard key is down.
    let internal isKeyDown (key : KeyboardKey) =
        let mutable unused = 0
        let keyboardStatePtr = SDL.SDL_GetKeyboardState &unused
        let keyboardStatePtr = NativeInterop.NativePtr.ofNativeInt keyboardStatePtr
        let state = NativeInterop.NativePtr.get<byte> keyboardStatePtr (int key)
        state = byte 1

    /// Check that the given keyboard key is up.
    let internal isKeyUp (key : KeyboardKey) =
        not (isKeyDown key)

    /// Check that either enter key is down.
    let internal isEnterDown () =
        isKeyDown KeyboardKey.KpEnter ||
        isKeyDown KeyboardKey.Enter

    /// Check that either enter key is up.
    let internal isEnterUp () =
        isKeyUp KeyboardKey.KpEnter ||
        isKeyUp KeyboardKey.Enter

    /// Check that either ctrl key is down.
    let internal isCtrlDown () =
        int (SDL.SDL_GetModState ()) &&& int SDL.SDL_Keymod.KMOD_CTRL <> 0

    /// Check that both ctrl keys are up.
    let internal isCtrlUp () =
        not (isCtrlDown ())

    /// Check that either alt key is down.
    let internal isAltDown () =
        int (SDL.SDL_GetModState ()) &&& int SDL.SDL_Keymod.KMOD_ALT <> 0

    /// Check that both alt keys are up.
    let internal isAltUp () =
        not (isAltDown ())

    /// Check that either shift key is down.
    let internal isShiftDown () =
        int (SDL.SDL_GetModState ()) &&& int SDL.SDL_Keymod.KMOD_SHIFT <> 0

    /// Check that both shift keys are up.
    let internal isShiftUp () =
        not (isShiftDown ())

[<RequireQualifiedAccess>]        
module GamepadState =

    let mutable private Joysticks = [||]

    /// Initialize gamepad state.
    let internal init () =
        let indices = SDL.SDL_NumJoysticks ()
        Joysticks <-
            Array.map (fun joystick ->
                // NOTE: we don't have a matching call to SDL.SDL_JoystickClose, but it may not be necessary
                SDL.SDL_JoystickOpen joystick)
                [|0 .. indices|]

    /// Check that an SDL gamepad button is supported.
    let internal isSdlButtonSupported button =
        button < 8

    /// Get the number of open gamepad.
    let internal getGamepadCount () =
        Array.length Joysticks

    /// Convert a GamepadButton to SDL's representation.
    let internal toSdlButton gamepadButton =
        match gamepadButton with
        | ButtonA -> 0
        | ButtonB -> 1
        | ButtonX -> 2
        | ButtonY -> 3
        | ButtonL -> 4
        | ButtonR -> 5
        | ButtonSelect -> 6
        | ButtonStart -> 7

    /// Convert SDL's representation of a joystick button to a GamepadButton.
    let internal toNuButton gamepadButton =
        match gamepadButton with
        | 0 -> ButtonA
        | 1 -> ButtonB
        | 2 -> ButtonX
        | 3 -> ButtonY
        | 4 -> ButtonL
        | 5 -> ButtonR
        | 6 -> ButtonSelect
        | 7 -> ButtonStart
        | _ -> failwith "Invalid SDL joystick button."

    /// Convert a GamepadDirection to SDL's representation.
    let internal toSdlDirection gamepadDirection =
        match gamepadDirection with
        | DirectionUp -> SDL.SDL_HAT_UP
        | DirectionUpLeft -> SDL.SDL_HAT_LEFTUP
        | DirectionLeft -> SDL.SDL_HAT_LEFT
        | DirectionDownLeft -> SDL.SDL_HAT_LEFTDOWN
        | DirectionDown -> SDL.SDL_HAT_DOWN
        | DirectionDownRight -> SDL.SDL_HAT_RIGHTDOWN
        | DirectionRight -> SDL.SDL_HAT_RIGHT
        | DirectionUpRight -> SDL.SDL_HAT_RIGHTUP
        | DirectionCentered -> SDL.SDL_HAT_CENTERED

    /// Convert SDL's representation of a hat direction to a GamepadDirection.
    let internal toNuDirection gamepadDirection =
        match gamepadDirection with
        | SDL.SDL_HAT_UP -> DirectionUp
        | SDL.SDL_HAT_LEFTUP -> DirectionUpLeft
        | SDL.SDL_HAT_LEFT -> DirectionLeft
        | SDL.SDL_HAT_LEFTDOWN -> DirectionDownLeft
        | SDL.SDL_HAT_DOWN -> DirectionDown
        | SDL.SDL_HAT_RIGHTDOWN -> DirectionDownRight
        | SDL.SDL_HAT_RIGHT -> DirectionRight
        | SDL.SDL_HAT_RIGHTUP -> DirectionUpRight
        | SDL.SDL_HAT_CENTERED -> DirectionCentered
        | _ -> failwith "Invalid SDL hat direction."

    /// Get the given gamepad's current direction.
    let internal getDirection index =
        match Array.tryItem index Joysticks with
        | Some joystick ->
            let hat = SDL.SDL_JoystickGetHat (joystick, 0)
            toNuDirection hat
        | None -> DirectionCentered

    /// Check that the given gamepad's button is down.
    let internal isButtonDown index button =
        let sdlButton = toSdlButton button
        match Array.tryItem index Joysticks with
        | Some joystick -> SDL.SDL_JoystickGetButton (joystick, sdlButton) = byte 1
        | None -> false