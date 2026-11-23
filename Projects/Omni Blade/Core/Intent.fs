// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

/// What the player means to do this frame - read from the keyboard and gamepads, or given by a test in its place. The
/// presses are true only on the frame the key or button goes down. It remembers from frame to frame which gamepad
/// buttons were down, so a press can be told from a hold, and which way a step is held, for repeating it. Each screen
/// keeps its own, starting anew.
type Intent =
    { Move : Vector3 // held - each axis -1, 0 or 1, as from the arrow keys, WASD, the d-pad or the left stick
      Step : Direction option // pressed, repeating while held - a step of focus through a menu
      Confirm : bool // pressed - interact, or press the focused button
      Cancel : bool // pressed - back out, close, decline or skip
      Menu : bool // pressed - open or close the field menu
      Party : bool // pressed - open the party menu at a save point
      PageUp : bool // pressed - the previous page of a list
      PageDown : bool // pressed - the next page of a list
      GamepadButtonsDown : GamepadButton Set // the gamepad buttons down this frame
      StepDirectionOpt : Direction option // the way a step's held
      StepTime : int64 } // when the step began to be held

    /// An intent of nothing, as the player does nothing.
    static member empty =
        { Move = v3Zero
          Step = None
          Confirm = false
          Cancel = false
          Menu = false
          Party = false
          PageUp = false
          PageDown = false
          GamepadButtonsDown = Set.empty
          StepDirectionOpt = None
          StepTime = 0L }

[<AutoOpen>]
module IntentExtensions =
    type Game with

        /// What a test gives in place of what the player means, read as the keyboard and gamepads would be.
        member this.GetIntentOverrideOpt world : Intent option = this.Get (nameof this.IntentOverrideOpt) world
        member this.SetIntentOverrideOpt (value : Intent option) world = this.Set (nameof this.IntentOverrideOpt) value world
        member this.IntentOverrideOpt = lens (nameof this.IntentOverrideOpt) this this.GetIntentOverrideOpt this.SetIntentOverrideOpt

[<RequireQualifiedAccess>]
module Intent =


    let private MouseButtons =
        [MouseLeft; MouseMiddle; MouseRight; MouseX1; MouseX2]

    let private GamepadButtons =
        [ButtonUp; ButtonLeft; ButtonDown; ButtonRight; ButtonA; ButtonB; ButtonX; ButtonY; ButtonL; ButtonR; ButtonSelect; ButtonStart]

    let private getAxis positive negative =
        (if positive then 1.0f else 0.0f) - (if negative then 1.0f else 0.0f)

    let private getStickAxis value =
        if value >= Constants.Input.StickDeadZone then 1.0f elif value <= -Constants.Input.StickDeadZone then -1.0f else 0.0f

    /// The direction a movement leans - horizontal first, as the field has always faced the avatar.
    let tryGetDirection (move : Vector3) =
        if move.X > 0.0f then Some Rightward
        elif move.X < 0.0f then Some Leftward
        elif move.Y > 0.0f then Some Upward
        elif move.Y < 0.0f then Some Downward
        else None

    /// Read whether the pointer's shown now - a mouse press hiding it until the keyboard or a gamepad is used in a menu
    /// again - along with what the player means from the keyboard and every open gamepad, or what a test gives in its
    /// place, given the intent last frame.
    let read (pointerShown : bool) (intentLast : Intent) (world : World) =

        // keyboard
        let keyDown key = World.isKeyboardKeyDown key world
        let keyPressed key = World.isKeyboardKeyPressed key world
        let altDown = World.isKeyboardAltDown world
        let keyMove =
            v3
                (getAxis (keyDown KeyboardKey.Right || keyDown KeyboardKey.D) (keyDown KeyboardKey.Left || keyDown KeyboardKey.A))
                (getAxis (keyDown KeyboardKey.Up || keyDown KeyboardKey.W) (keyDown KeyboardKey.Down || keyDown KeyboardKey.S))
                0.0f
        // alt+enter toggles full screen, so it doesn't confirm
        let keyConfirm = not altDown && (keyPressed KeyboardKey.Enter || keyPressed KeyboardKey.Space)
        let keyCancel = keyPressed KeyboardKey.Escape || keyPressed KeyboardKey.Backspace
        // shift+tab goes back as page up does, such as to the previous ally in battle, rather than to the menu
        let shiftDown = World.isKeyboardShiftDown world
        let keyMenu = not shiftDown && keyPressed KeyboardKey.Tab
        let keyParty = keyPressed KeyboardKey.P
        let keyPageUp = keyPressed KeyboardKey.Q || keyPressed KeyboardKey.PageUp || (shiftDown && keyPressed KeyboardKey.Tab)
        let keyPageDown = keyPressed KeyboardKey.E || keyPressed KeyboardKey.PageDown

        // gamepads
        let gamepadCount = World.getGamepadCount world
        let buttonsDown =
            set
                [for index in 0 .. dec gamepadCount do
                    for button in GamepadButtons do
                        if SDL.SDLBool.op_Implicit (World.isButtonDown index button world) then button]
        let buttonDown button = buttonsDown.Contains button
        let buttonPressed button = buttonsDown.Contains button && not (intentLast.GamepadButtonsDown.Contains button)
        let stick =
            [for index in 0 .. dec gamepadCount do World.getStickLeft index world] |>
            List.fold (fun (sum : Vector2) stick -> sum + stick) v2Zero
        let gamepadMove =
            v3
                (getAxis (buttonDown ButtonRight) (buttonDown ButtonLeft) + getStickAxis stick.X)
                (getAxis (buttonDown ButtonUp) (buttonDown ButtonDown) - getStickAxis stick.Y) // stick y leans down
                0.0f

        // combined
        let move = keyMove + gamepadMove
        let move = v3 (Math.Clamp (move.X, -1.0f, 1.0f)) (Math.Clamp (move.Y, -1.0f, 1.0f)) 0.0f
        let time = world.UpdateTime
        let directionOpt = tryGetDirection move
        let (step, stepTime) =
            match directionOpt with
            | Some direction when intentLast.StepDirectionOpt = Some direction ->
                let held = time - intentLast.StepTime
                let delay = Constants.Input.StepRepeatDelay
                let repeat = held >= delay && (held - delay) % Constants.Input.StepRepeatPeriod = 0L
                ((if repeat then Some direction else None), intentLast.StepTime)
            | Some direction -> (Some direction, time)
            | None -> (None, time)
        let intentRead =
            { Move = move
              Step = step
              Confirm = keyConfirm || buttonPressed ButtonA
              Cancel = keyCancel || buttonPressed ButtonB
              Menu = keyMenu || buttonPressed ButtonStart
              Party = keyParty || buttonPressed ButtonY
              PageUp = keyPageUp || buttonPressed ButtonL
              PageDown = keyPageDown || buttonPressed ButtonR
              GamepadButtonsDown = buttonsDown
              StepDirectionOpt = directionOpt
              StepTime = stepTime }
        let intent =
            match Game.GetIntentOverrideOpt world with
            | Some intent -> { intent with GamepadButtonsDown = buttonsDown; StepDirectionOpt = directionOpt; StepTime = stepTime }
            | None -> intentRead
        let mousePressed = MouseButtons |> List.exists (fun button -> World.isMouseButtonPressed button world)
        let menuUsed =
            intent.Step.IsSome || intent.Confirm || intent.Cancel || intent.Menu || intent.Party || intent.PageUp || intent.PageDown
        let pointerShown = if mousePressed then false elif menuUsed then true else pointerShown
        (pointerShown, intent)

/// Moving a selection through a menu's rows and columns.
[<RequireQualifiedAccess>]
module Selection =

    /// Step an index through a count, wrapping at the ends.
    let wrap delta count index =
        if count <= 0 then 0 else ((index + delta) % count + count) % count

    /// The vertical step a direction makes, down being forward through a list.
    let rowDelta direction =
        match direction with
        | Upward -> -1
        | Downward -> 1
        | Leftward | Rightward -> 0

    /// The horizontal step a direction makes.
    let columnDelta direction =
        match direction with
        | Leftward -> -1
        | Rightward -> 1
        | Upward | Downward -> 0

    /// Play the sound of choosing something.
    let playAffirm world =
        World.playSound 0.0f 0.0f Constants.Audio.SoundVolumeDefault Assets.Gui.AffirmSound world

    /// Play the sound of choosing something that can't be chosen.
    let playMistake world =
        World.playSound 0.0f 0.0f Constants.Audio.SoundVolumeDefault Assets.Gui.MistakeSound world