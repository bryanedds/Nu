// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open System.Numerics
open FSharp.NativeInterop
open SDL
open Prime

/// Describes a mouse button.
type MouseButton =
    | MouseLeft
    | MouseMiddle
    | MouseRight
    | MouseX1
    | MouseX2

    /// Extract the respective event name identifying the given mouse button.
    static member toEventName this =
        match this with
        | MouseLeft -> "Left"
        | MouseMiddle -> "Middle"
        | MouseRight -> "Right"
        | MouseX1 -> "X1"
        | MouseX2 -> "X2"

    /// Extract the respective mouse button identified by the given event name.
    static member ofEventName eventName =
        match eventName with
        | "Left" -> MouseLeft
        | "Middle" -> MouseMiddle
        | "Right" -> MouseRight
        | "X1" -> MouseX1
        | "X2" -> MouseX2
        | _ -> failwithumf ()

/// Describes a source of axis-based input from a gamepad.
type GamepadAxis =
    | StickLeftX
    | StickLeftY
    | StickRightX
    | StickRightY
    | TriggerLeft
    | TriggerRight

    /// Extract the respective name identifying the given gamepad axis.
    static member toEventName this =
        match this with
        | StickLeftX -> "StickLeftX"
        | StickLeftY -> "StickLeftY"
        | StickRightX -> "StickRightX"
        | StickRightY -> "StickRightY"
        | TriggerLeft -> "TriggerLeft"
        | TriggerRight -> "TriggerRight"

    /// Extract the respective gamepad axis identified by the given name.
    static member ofEventName eventName =
        match eventName with
        | "StickLeftX" -> StickLeftX
        | "StickLeftY" -> StickLeftY
        | "StickRightX" -> StickRightX
        | "StickRightY" -> StickRightY
        | "TriggerLeft" -> TriggerLeft
        | "TriggerRight" -> TriggerRight
        | _ -> failwithumf ()

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
// TODO: The actual button and the button name should be decoupled! See https://wiki.libsdl.org/SDL3/SDL_GamepadButton#remarks
type GamepadButton =
    | ButtonA
    | ButtonB
    | ButtonX
    | ButtonY
    | ButtonL
    | ButtonR
    | ButtonSelect
    | ButtonStart

/// Exposes the ongoing state of the mouse.
[<RequireQualifiedAccess>]
module internal MouseState =

    let mutable private MouseButtonStatePrevious : SDL_MouseButtonFlags = LanguagePrimitives.EnumOfValue 0u
    let mutable private MouseButtonStateCurrent : SDL_MouseButtonFlags = LanguagePrimitives.EnumOfValue 0u
    let mutable internal MouseScrollStatePrevious = 0.0f
    let mutable internal MouseScrollStateCurrent = 0.0f

    /// Convert a MouseButton to SDL's representation.
    let internal toSdlButton mouseButton =
        match mouseButton with
        | MouseLeft -> SDLButton.SDL_BUTTON_LEFT
        | MouseMiddle -> SDLButton.SDL_BUTTON_MIDDLE
        | MouseRight -> SDLButton.SDL_BUTTON_RIGHT
        | MouseX1 -> SDLButton.SDL_BUTTON_X1
        | MouseX2 -> SDLButton.SDL_BUTTON_X2

    /// Convert SDL's representation of a mouse button to a MouseButton.
    let internal toNuButton mouseButton =
        match mouseButton with
        | SDLButton.SDL_BUTTON_LEFT -> MouseLeft
        | SDLButton.SDL_BUTTON_MIDDLE -> MouseMiddle
        | SDLButton.SDL_BUTTON_RIGHT -> MouseRight
        | SDLButton.SDL_BUTTON_X1 -> MouseX1
        | SDLButton.SDL_BUTTON_X2 -> MouseX2
        | _ -> failwith "Invalid SDL mouse button."

    /// Convert a MouseButton to SDL's representation.
    let private toSdlButtonFlags mouseButton =
        match mouseButton with
        | MouseLeft -> SDL_MouseButtonFlags.SDL_BUTTON_LMASK
        | MouseMiddle -> SDL_MouseButtonFlags.SDL_BUTTON_MMASK
        | MouseRight -> SDL_MouseButtonFlags.SDL_BUTTON_RMASK
        | MouseX1 -> SDL_MouseButtonFlags.SDL_BUTTON_X1MASK
        | MouseX2 -> SDL_MouseButtonFlags.SDL_BUTTON_X2MASK

    /// Update the current mouse state from SDL.
    let internal update () =
        
        // update button state
        MouseButtonStatePrevious <- MouseButtonStateCurrent
        let sdlMouseButtonState = SDL3.SDL_GetMouseState (NativePtr.nullPtr, NativePtr.nullPtr)
        MouseButtonStateCurrent <- sdlMouseButtonState

        // update scroll state
        MouseScrollStatePrevious <- MouseScrollStateCurrent

    /// Get the position of the mouse.
    let internal getPosition () =
        let mutable x, y = 0.0f, 0.0f
        SDL3.SDL_GetMouseState (&&x, &&y) |> ignore<SDL_MouseButtonFlags>
        v2 x y

    /// Get the scroll of the mouse.
    let internal getScroll () =
        MouseScrollStateCurrent

    /// Check that the given mouse button is down.
    let internal isButtonDown mouseButton =
        let sdlMouseButton = toSdlButtonFlags mouseButton
        MouseButtonStateCurrent &&& sdlMouseButton <> LanguagePrimitives.EnumOfValue 0u

    /// Check that the given mouse button is up.
    let internal isButtonUp mouseButton =
        not (isButtonDown mouseButton)

    /// Check that the given mouse button was just pressed.
    let internal isButtonPressed mouseButton =
        let sdlMouseButton = toSdlButtonFlags mouseButton
        (MouseButtonStatePrevious &&& sdlMouseButton = LanguagePrimitives.EnumOfValue 0u) &&
        (MouseButtonStateCurrent &&& sdlMouseButton <> LanguagePrimitives.EnumOfValue 0u)

    /// Check that the given mouse button was just released.
    let internal isButtonReleased mouseButton =
        let sdlMouseButton = toSdlButtonFlags mouseButton
        (MouseButtonStatePrevious &&& sdlMouseButton <> LanguagePrimitives.EnumOfValue 0u) &&
        (MouseButtonStateCurrent &&& sdlMouseButton = LanguagePrimitives.EnumOfValue 0u)

    /// Get how much the mouse has just scrolled.
    let internal getScrolled () =
        MouseScrollStateCurrent - MouseScrollStatePrevious

    /// Check that the mouse has just scrolled up.
    let internal isScrolledUp () =
        MouseScrollStateCurrent - MouseScrollStatePrevious > 0.0f

    /// Check that the mouse has just scrolled down.
    let internal isScrolledDown () =
        MouseScrollStateCurrent - MouseScrollStatePrevious < 0.0f

/// Exposes the ongoing state of the keyboard.
[<RequireQualifiedAccess>]
module internal KeyboardState =
    
    let mutable private keysCount = 0
    let mutable private KeyboardStatePrevious = Array.empty
    let mutable private KeyboardStateCurrent = Array.empty

    /// Update the current keyboard state from SDL.
    let internal update () =
        // move keyboard state current to previous
        if KeyboardStatePrevious.Length <> KeyboardStateCurrent.Length
        then KeyboardStatePrevious <- Array.copy KeyboardStateCurrent
        else KeyboardStateCurrent.CopyTo (KeyboardStatePrevious.AsSpan ())

        // get current keyboard state
        let oldKeysCount = keysCount
        let keyboardStatePtr = SDL3.SDL_GetKeyboardState &&keysCount
        if oldKeysCount <> keysCount then KeyboardStateCurrent <- Array.zeroCreate keysCount
        Span(NativePtr.toVoidPtr keyboardStatePtr, keysCount).CopyTo (KeyboardStateCurrent.AsSpan ())

    /// Check that the given keyboard key is down.
    let internal isKeyDown (key : KeyboardKey) =
        match KeyboardStateCurrent with
        | [||] -> false
        | keyboardState -> keyboardState.[int key]

    /// Check that the given keyboard key is up.
    let internal isKeyUp (key : KeyboardKey) =
        match KeyboardStateCurrent with
        | [||] -> false
        | keyboardState -> not keyboardState.[int key]

    /// Check that the given keyboard key was just pressed.
    let internal isKeyPressed key =
        match KeyboardStateCurrent with
        | [||] -> false
        | keyboardState ->
            keyboardState.[int key] &&
            match KeyboardStatePrevious with
            | [||] -> false
            | keyboardState -> not keyboardState.[int key]

    /// Check that the given keyboard key was just released.
    let internal isKeyReleased key =
        match KeyboardStateCurrent with
        | [||] -> false
        | keyboardState ->
            not keyboardState.[int key] &&
            match KeyboardStatePrevious with
            | [||] -> false
            | keyboardState -> keyboardState.[int key]

    /// Check that either enter key is down.
    let internal isEnterDown () =
        isKeyDown KeyboardKey.KpEnter ||
        isKeyDown KeyboardKey.Enter

    /// Check that either enter key is up.
    let internal isEnterUp () =
        isKeyUp KeyboardKey.KpEnter ||
        isKeyUp KeyboardKey.Enter

    /// Check that either enter key was just pressed.
    let internal isEnterPressed () =
        isKeyPressed KeyboardKey.KpEnter ||
        isKeyPressed KeyboardKey.Enter

    /// Check that either enter key was just released.
    let internal isEnterReleased () =
        isKeyReleased KeyboardKey.KpEnter ||
        isKeyReleased KeyboardKey.Enter

    /// Check that either ctrl key is down.
    let internal isCtrlDown () =
        SDL3.SDL_GetModState () &&& SDL_Keymod.SDL_KMOD_CTRL <> SDL_Keymod.SDL_KMOD_NONE

    /// Check that both ctrl keys are up.
    let internal isCtrlUp () =
        not (isCtrlDown ())

    /// Check that either alt key is down.
    let internal isAltDown () =
        SDL3.SDL_GetModState () &&& SDL_Keymod.SDL_KMOD_ALT <> SDL_Keymod.SDL_KMOD_NONE

    /// Check that both alt keys are up.
    let internal isAltUp () =
        not (isAltDown ())

    /// Check that either shift key is down.
    let internal isShiftDown () =
        SDL3.SDL_GetModState () &&& SDL_Keymod.SDL_KMOD_SHIFT <> SDL_Keymod.SDL_KMOD_NONE

    /// Check that both shift keys are up.
    let internal isShiftUp () =
        not (isShiftDown ())

/// Exposes the ongoing state of gamepads.
[<RequireQualifiedAccess>]        
module GamepadState =

    let mutable private Joysticks = [||]

    /// Initialize gamepad state.
    let internal init () =
        use joysticks = SDL3.SDL_GetJoysticks ()
        // NOTE: we don't have a matching call to SDL3.SDL_CloseJoystick, but it may not be necessary
        Joysticks <- Array.init joysticks.Count (fun i -> SDL3.SDL_OpenJoystick joysticks.[i])

    /// Get the number of open gamepad.
    let internal getGamepadCount () =
        Array.length Joysticks

    /// Convert an SDL joystick axis to a GamepadAxis.
    let internal toNuAxis axis =
        match axis with
        | SDL_GamepadAxis.SDL_GAMEPAD_AXIS_LEFTX -> StickLeftX
        | SDL_GamepadAxis.SDL_GAMEPAD_AXIS_LEFTY -> StickLeftY
        | SDL_GamepadAxis.SDL_GAMEPAD_AXIS_RIGHTX -> StickRightX
        | SDL_GamepadAxis.SDL_GAMEPAD_AXIS_RIGHTY -> StickRightY
        | SDL_GamepadAxis.SDL_GAMEPAD_AXIS_LEFT_TRIGGER -> TriggerLeft
        | SDL_GamepadAxis.SDL_GAMEPAD_AXIS_RIGHT_TRIGGER -> TriggerRight
        | _ -> failwith "Invalid SDL joystick axis."

    /// Convert a GamepadAxis to SDL's representation.
    let internal toSdlAxis axis =
        match axis with
        | StickLeftX -> SDL_GamepadAxis.SDL_GAMEPAD_AXIS_LEFTX
        | StickLeftY -> SDL_GamepadAxis.SDL_GAMEPAD_AXIS_LEFTY
        | StickRightX -> SDL_GamepadAxis.SDL_GAMEPAD_AXIS_RIGHTX
        | StickRightY -> SDL_GamepadAxis.SDL_GAMEPAD_AXIS_RIGHTY
        | TriggerLeft -> SDL_GamepadAxis.SDL_GAMEPAD_AXIS_LEFT_TRIGGER
        | TriggerRight -> SDL_GamepadAxis.SDL_GAMEPAD_AXIS_RIGHT_TRIGGER

    /// Convert a GamepadButton to SDL's representation.
    let internal toSdlButton gamepadButton =
        match gamepadButton with
        | ButtonA -> SDL_GamepadButton.SDL_GAMEPAD_BUTTON_SOUTH
        | ButtonB -> SDL_GamepadButton.SDL_GAMEPAD_BUTTON_EAST
        | ButtonX -> SDL_GamepadButton.SDL_GAMEPAD_BUTTON_WEST
        | ButtonY -> SDL_GamepadButton.SDL_GAMEPAD_BUTTON_NORTH
        | ButtonL -> SDL_GamepadButton.SDL_GAMEPAD_BUTTON_LEFT_SHOULDER
        | ButtonR -> SDL_GamepadButton.SDL_GAMEPAD_BUTTON_RIGHT_SHOULDER
        | ButtonSelect -> SDL_GamepadButton.SDL_GAMEPAD_BUTTON_BACK
        | ButtonStart -> SDL_GamepadButton.SDL_GAMEPAD_BUTTON_START

    /// Try to convert SDL's representation of a joystick button to a GamepadButton.
    let internal tryToNuButton gamepadButton =
        match gamepadButton with
        | SDL_GamepadButton.SDL_GAMEPAD_BUTTON_SOUTH -> Some ButtonA
        | SDL_GamepadButton.SDL_GAMEPAD_BUTTON_EAST -> Some ButtonB
        | SDL_GamepadButton.SDL_GAMEPAD_BUTTON_WEST -> Some ButtonX
        | SDL_GamepadButton.SDL_GAMEPAD_BUTTON_NORTH -> Some ButtonY
        | SDL_GamepadButton.SDL_GAMEPAD_BUTTON_LEFT_SHOULDER -> Some ButtonL
        | SDL_GamepadButton.SDL_GAMEPAD_BUTTON_RIGHT_SHOULDER -> Some ButtonR
        | SDL_GamepadButton.SDL_GAMEPAD_BUTTON_BACK -> Some ButtonSelect
        | SDL_GamepadButton.SDL_GAMEPAD_BUTTON_START -> Some ButtonStart
        | _ -> None

    /// Convert a GamepadDirection to SDL's representation.
    let internal toSdlDirection gamepadDirection =
        match gamepadDirection with
        | DirectionUp -> SDL3.SDL_HAT_UP
        | DirectionUpLeft -> SDL3.SDL_HAT_LEFTUP
        | DirectionLeft -> SDL3.SDL_HAT_LEFT
        | DirectionDownLeft -> SDL3.SDL_HAT_LEFTDOWN
        | DirectionDown -> SDL3.SDL_HAT_DOWN
        | DirectionDownRight -> SDL3.SDL_HAT_RIGHTDOWN
        | DirectionRight -> SDL3.SDL_HAT_RIGHT
        | DirectionUpRight -> SDL3.SDL_HAT_RIGHTUP
        | DirectionCentered -> SDL3.SDL_HAT_CENTERED

    /// Convert SDL's representation of a hat direction to a GamepadDirection.
    let internal toNuDirection gamepadDirection =
        match uint32 gamepadDirection with
        | SDL3.SDL_HAT_UP -> DirectionUp
        | SDL3.SDL_HAT_LEFTUP -> DirectionUpLeft
        | SDL3.SDL_HAT_LEFT -> DirectionLeft
        | SDL3.SDL_HAT_LEFTDOWN -> DirectionDownLeft
        | SDL3.SDL_HAT_DOWN -> DirectionDown
        | SDL3.SDL_HAT_RIGHTDOWN -> DirectionDownRight
        | SDL3.SDL_HAT_RIGHT -> DirectionRight
        | SDL3.SDL_HAT_RIGHTUP -> DirectionUpRight
        | SDL3.SDL_HAT_CENTERED -> DirectionCentered
        | _ -> failwith "Invalid SDL hat direction."

    /// Convert an SDL joystick axis value to a float in the range -1.0f to 1.0f.
    let internal toNuAxisValue (axisValue : int16) =
        if axisValue >= 0s
        then single axisValue / single Int16.MaxValue
        else single axisValue / single Int16.MinValue |> negate

    /// Get the given gamepad's left joystick axes.
    let internal getStickLeft index =
        match Array.tryItem index Joysticks with
        | Some joystick ->
            let x = SDL3.SDL_GetJoystickAxis (joystick, int SDL_GamepadAxis.SDL_GAMEPAD_AXIS_LEFTX)
            let y = SDL3.SDL_GetJoystickAxis (joystick, int SDL_GamepadAxis.SDL_GAMEPAD_AXIS_LEFTY)
            v2 (toNuAxisValue x) (toNuAxisValue y)
        | None -> v2Zero

    /// Get the given gamepad's right joystick axes.
    let internal getStickRight index =
        match Array.tryItem index Joysticks with
        | Some joystick ->
            let x = SDL3.SDL_GetJoystickAxis (joystick, int SDL_GamepadAxis.SDL_GAMEPAD_AXIS_RIGHTX)
            let y = SDL3.SDL_GetJoystickAxis (joystick, int SDL_GamepadAxis.SDL_GAMEPAD_AXIS_RIGHTY)
            v2 (toNuAxisValue x) (toNuAxisValue y)
        | None -> v2Zero

    /// Get the given gamepad's left trigger axis.
    let internal getTriggerLeft index =
        match Array.tryItem index Joysticks with
        | Some joystick ->
            let value = SDL3.SDL_GetJoystickAxis (joystick, int SDL_GamepadAxis.SDL_GAMEPAD_AXIS_LEFT_TRIGGER)
            toNuAxisValue value
        | None -> 0.0f

    /// Get the given gamepad's right trigger axis.
    let internal getTriggerRight index =
        match Array.tryItem index Joysticks with
        | Some joystick ->
            let value = SDL3.SDL_GetJoystickAxis (joystick, int SDL_GamepadAxis.SDL_GAMEPAD_AXIS_RIGHT_TRIGGER)
            toNuAxisValue value
        | None -> 0.0f

    /// Get the given gamepad's current direction.
    let internal getDirection index =
        match Array.tryItem index Joysticks with
        | Some gamepad ->
            let hat = SDL3.SDL_GetJoystickHat (gamepad, 0)
            toNuDirection hat
        | None -> DirectionCentered

    /// Check that the given gamepad's button is down.
    let internal isButtonDown index button =
        let sdlButton = toSdlButton button
        match Array.tryItem index Joysticks with
        | Some joystick -> SDL3.SDL_GetJoystickButton (joystick, int sdlButton)
        | None -> false