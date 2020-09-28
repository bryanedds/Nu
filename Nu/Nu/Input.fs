// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open SDL2
open Prime
open Nu

/// Describes a mouse button.
type [<StructuralEquality; StructuralComparison>] MouseButton =
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
        v2 (single mousePosition.X) (single mousePosition.Y)

[<RequireQualifiedAccess>]
module KeyboardState =

    /// Check that the given keyboard key is down.
    let isKeyDown (key : KeyboardKey) =
        let keyboardStatePtr = fst (SDL.SDL_GetKeyboardState ())
        let keyboardStatePtr = NativeInterop.NativePtr.ofNativeInt keyboardStatePtr
        let state = NativeInterop.NativePtr.get<byte> keyboardStatePtr (int key)
        state = byte 1

    /// Check that either ctrl key is down.
    let isCtrlDown () =
        isKeyDown KeyboardKey.Lctrl ||
        isKeyDown KeyboardKey.Rctrl

    /// Check that either alt key is down.
    let isAltDown () =
        isKeyDown KeyboardKey.Lalt ||
        isKeyDown KeyboardKey.Ralt

    /// Check that either shift key is down.
    let isShiftDown () =
        isKeyDown KeyboardKey.Lshift ||
        isKeyDown KeyboardKey.Rshift

/// Describes a gamepad direction.
type [<StructuralEquality; StructuralComparison>] GamepadDirection =
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
type [<StructuralEquality; StructuralComparison>] GamepadButton =
    | ButtonA
    | ButtonB
    | ButtonX
    | ButtonY
    | ButtonL
    | ButtonR
    | ButtonSelect
    | ButtonStart

[<RequireQualifiedAccess>]        
module GamepadState =

    let mutable private Joysticks = [||]

    let isSdlButtonSupported button =
        button < 8

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
        | ButtonA -> 0
        | ButtonB -> 1
        | ButtonX -> 2
        | ButtonY -> 3
        | ButtonL -> 4
        | ButtonR -> 5
        | ButtonSelect -> 6
        | ButtonStart -> 7

    /// Convert SDL's representation of a joystick button to a GamepadButton.
    let toNuButton gamepadButton =
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
    let toSdlDirection gamepadDirection =
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
    let toNuDirection gamepadDirection =
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

    /// Check that the given gamepad button is down.
    let getDirection index =
        match Array.tryItem index Joysticks with
        | Some joystick ->
            let hat = SDL.SDL_JoystickGetHat (joystick, 0)
            toNuDirection hat
        | None -> DirectionCentered

    /// Check that the given gamepad button is down.
    let isButtonDown index button =
        let sdlButton = toSdlButton button
        match Array.tryItem index Joysticks with
        | Some joystick -> SDL.SDL_JoystickGetButton (joystick, sdlButton) = byte 1
        | None -> false