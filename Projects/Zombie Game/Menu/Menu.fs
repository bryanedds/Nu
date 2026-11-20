// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// What a menu shows - its heading in its color, the line under it, its choices and the one highlighted - and the
/// choice confirmed in it, raised for the game to do.
[<AutoOpen>]
module MenuDispatcherExtensions =
    type Entity with
        member this.GetHeading world : string = this.Get (nameof this.Heading) world
        member this.SetHeading (value : string) world = this.Set (nameof this.Heading) value world
        member this.Heading = lens (nameof this.Heading) this this.GetHeading this.SetHeading
        member this.GetHeadingColor world : Color = this.Get (nameof this.HeadingColor) world
        member this.SetHeadingColor (value : Color) world = this.Set (nameof this.HeadingColor) value world
        member this.HeadingColor = lens (nameof this.HeadingColor) this this.GetHeadingColor this.SetHeadingColor
        member this.GetSubtitle world : string = this.Get (nameof this.Subtitle) world
        member this.SetSubtitle (value : string) world = this.Set (nameof this.Subtitle) value world
        member this.Subtitle = lens (nameof this.Subtitle) this this.GetSubtitle this.SetSubtitle
        member this.GetChoices world : (string * MenuChoice) list = this.Get (nameof this.Choices) world
        member this.SetChoices (value : (string * MenuChoice) list) world = this.Set (nameof this.Choices) value world
        member this.Choices = lens (nameof this.Choices) this this.GetChoices this.SetChoices
        member this.GetCursor world : int = this.Get (nameof this.Cursor) world
        member this.SetCursor (value : int) world = this.Set (nameof this.Cursor) value world
        member this.Cursor = lens (nameof this.Cursor) this this.GetCursor this.SetCursor
        member this.ChooseEvent = Events.ChooseEvent --> this

/// A menu of the title, game over and ending screens - a heading, a line under it, and choices with one highlighted,
/// moved through with up and down, the one highlighted raised on confirming.
type MenuDispatcher () =
    inherit GuiDispatcherImSim ()

    /// Answer the keyboard - moving the highlight up or down, or raising the choice highlighted.
    static let answer (entity : Entity) world =
        let choices = entity.GetChoices world
        let count = choices.Length
        if count > 0 then
            let cursor = min (entity.GetCursor world) (dec count)
            if World.isKeyboardKeyPressed KeyboardKey.Up world || World.isKeyboardKeyPressed KeyboardKey.W world then
                entity.SetCursor ((cursor + count - 1) % count) world
                World.playSound 0.0f 0.0f 0.35f Assets.Gameplay.MenuMoveSound world
            elif World.isKeyboardKeyPressed KeyboardKey.Down world || World.isKeyboardKeyPressed KeyboardKey.S world then
                entity.SetCursor ((cursor + 1) % count) world
                World.playSound 0.0f 0.0f 0.35f Assets.Gameplay.MenuMoveSound world
            elif World.isKeyboardKeyPressed KeyboardKey.Space world || World.isKeyboardKeyPressed KeyboardKey.Enter world then
                World.playSound 0.0f 0.0f 0.2f Assets.Gameplay.MenuConfirmSound world
                World.publish (snd choices[cursor]) entity.ChooseEvent entity world

    static member Properties =
        [define Entity.Heading ""
         define Entity.HeadingColor Color.White
         define Entity.Subtitle ""
         define Entity.Choices []
         define Entity.Cursor 0]

    override this.Process (entity, world) =

        // answer the keyboard
        if world.TimeAdvancing then answer entity world

        // the heading, the line under it, the choices and the hint
        let cursor = entity.GetCursor world
        World.doText "Heading"
            [Entity.Position .= v3 0.0f 70.0f 0.0f
             Entity.Size .= v3 600.0f 60.0f 0.0f
             Entity.Text @= entity.GetHeading world
             Entity.TextColor @= entity.GetHeadingColor world
             Entity.FontSizing .= Some 28.0f] world
        World.doText "Subtitle"
            [Entity.Position .= v3 0.0f 30.0f 0.0f
             Entity.Size .= v3 600.0f 20.0f 0.0f
             Entity.Text @= entity.GetSubtitle world
             Entity.TextColor .= Color (0.6f, 0.58f, 0.55f, 1.0f)] world
        for (i, (label, _)) in List.indexed (entity.GetChoices world) do
            World.doText ("Choice+" + string i)
                [Entity.Position .= v3 0.0f (-20.0f - single i * 22.0f) 0.0f
                 Entity.Size .= v3 600.0f 18.0f 0.0f
                 Entity.Text @= label
                 Entity.TextColor @= (if i = cursor then Color (1.0f, 0.85f, 0.45f, 1.0f) else Color (0.7f, 0.68f, 0.64f, 1.0f))
                 Entity.FontSizing .= Some 10.0f] world
        World.doText "Hint"
            [Entity.Position .= v3 0.0f -150.0f 0.0f
             Entity.Size .= v3 600.0f 12.0f 0.0f
             Entity.Text .= "W/S - Choose    Enter - Confirm"
             Entity.TextColor .= Color (0.5f, 0.48f, 0.45f, 1.0f)
             Entity.FontSizing .= Constants.Gui.SmallFontSizing] world
