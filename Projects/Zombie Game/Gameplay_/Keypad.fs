// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// What a keypad shows - the digits entered and the key highlighted, beside how many digits its code has and the intent
/// it's given - and the code entered at it, raised for the gameplay screen to try.
[<AutoOpen>]
module KeypadDispatcherExtensions =
    type Entity with
        member this.GetKeypad world : Keypad = this.Get (nameof this.Keypad) world
        member this.SetKeypad (value : Keypad) world = this.Set (nameof this.Keypad) value world
        member this.Keypad = lens (nameof this.Keypad) this this.GetKeypad this.SetKeypad
        member this.GetDigits world : int = this.Get (nameof this.Digits) world
        member this.SetDigits (value : int) world = this.Set (nameof this.Digits) value world
        member this.Digits = lens (nameof this.Digits) this this.GetDigits this.SetDigits
        member this.CodeEvent = Events.CodeEvent --> this

/// A keypad - its display and its twelve keys.
type KeypadDispatcher () =
    inherit GuiDispatcherImSim ()

    /// Answer the player's intent - closing, moving between the keys, and pressing one, raising the code entered on
    /// OK.
    static let answer (intent : Intent) (entity : Entity) world =
        let keypad = entity.GetKeypad world
        if intent.Back then
            World.playSound 0.0f 0.0f 0.2f Assets.Gameplay.MenuBackSound world
            World.publish () entity.CloseEvent entity world
        elif intent.Move <> v2iZero then
            let column = (keypad.Key % 3 + intent.Move.X + 3) % 3
            let row = (keypad.Key / 3 + intent.Move.Y + 4) % 4
            entity.SetKeypad { keypad with Key = row * 3 + column } world
            World.playSound 0.0f 0.0f 0.35f Assets.Gameplay.MenuMoveSound world
        elif intent.Act then
            match Constants.Gui.KeypadKeys[keypad.Key] with
            | "C" ->
                entity.SetKeypad { keypad with Entered = "" } world
                World.playSound 0.0f 0.0f 0.12f Assets.Gameplay.KeypadSound world
            | "OK" ->
                entity.SetKeypad { keypad with Entered = "" } world
                World.publish keypad.Entered entity.CodeEvent entity world
            | digit ->
                if keypad.Entered.Length < entity.GetDigits world then entity.SetKeypad { keypad with Entered = keypad.Entered + digit } world
                World.playSound 0.0f 0.0f 0.12f Assets.Gameplay.KeypadSound world

    static member Properties =
        [define Entity.Keypad { Entered = ""; Key = 0 }
         define Entity.Digits 4
         define Entity.Intent Intent.empty]

    override this.Process (entity, world) =

        // answer the player's intent
        if world.TimeAdvancing then answer (entity.GetIntent world) entity world

        // the digits entered and the keys
        let keypad = entity.GetKeypad world
        World.doText "KeypadPanel"
            [Entity.Position .= v3 0.0f 0.0f 0.0f
             Entity.Size .= v3 220.0f 260.0f 0.0f
             Entity.Text .= ""
             Entity.TextColor .= Color.White
             Entity.BackdropImageOpt .= Some Assets.Default.Black
             Entity.Color .= Color.White.WithA 0.9f] world
        World.doText "KeypadDisplay"
            [Entity.Position .= v3 0.0f 96.0f 0.0f
             Entity.Size .= v3 160.0f 28.0f 0.0f
             Entity.Text @= keypad.Entered.PadRight (entity.GetDigits world, '_')
             Entity.TextColor .= Color (0.4f, 1.0f, 0.5f, 1.0f)
             Entity.FontSizing .= Some 14.0f
             Entity.BackdropImageOpt .= Some Assets.Default.Black
             Entity.Color .= Color.White.WithA 0.6f] world
        for (i, key) in Array.indexed Constants.Gui.KeypadKeys do
            let column = i % 3
            let row = i / 3
            World.doText ("Key+" + string i)
                [Entity.Position .= v3 (-56.0f + single column * 56.0f) (50.0f - single row * 40.0f) 0.0f
                 Entity.Size .= v3 46.0f 32.0f 0.0f
                 Entity.Text .= key
                 Entity.TextColor @= (if i = keypad.Key then Constants.Gui.BrightColor else Constants.Gui.DimColor)
                 Entity.BackdropImageOpt .= Some Assets.Default.Black
                 Entity.Color @= Color.White.WithA (if i = keypad.Key then 0.5f else 0.25f)] world
