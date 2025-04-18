﻿namespace Twenty48
open System
open System.Numerics
open Prime
open Nu
open Twenty48

// this is our MMCC model type representing gameplay.
type GameplayMessage =
    | StartPlaying
    | FinishQuitting
    | TimeUpdate
    | TryShift of Direction
    | Nil
    interface Message
    
// this is our gameplay MMCC command type.
type GameplayCommand =
    | StartQuitting
    interface Command
    
// this extends the Screen API to expose the Gameplay model as well as the Quit event.
[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()
        member this.QuitEvent = Events.QuitEvent --> this
        
// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type GameplayDispatcher () =
    inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> (Gameplay.empty)

    // here we define the screen's fallback model depending on whether screen is selected
    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world
        then Gameplay.initial
        else Gameplay.empty

    // here we define the screen's property values and event handling
    override this.Definitions (_, _) =
        [Screen.SelectEvent => StartPlaying
         Screen.DeselectingEvent => FinishQuitting
         Screen.TimeUpdateEvent => TimeUpdate
         Game.KeyboardKeyDownEvent =|> fun evt ->
            if not evt.Data.Repeated then
                match evt.Data.KeyboardKey with
                | KeyboardKey.Up -> TryShift Upward
                | KeyboardKey.Down -> TryShift Downward
                | KeyboardKey.Left -> TryShift Leftward
                | KeyboardKey.Right -> TryShift Rightward
                | _ -> Nil
            else Nil]

    // here we handle the above messages
    override this.Message (gameplay, message, _, world) =

        match message with
        | StartPlaying ->
            just Gameplay.initial

        | FinishQuitting ->
            just Gameplay.empty

        | TimeUpdate ->
            just { gameplay with GameplayTime = inc gameplay.GameplayTime }

        | TryShift direction ->
            if world.Advancing && gameplay.GameplayState = Playing false then
                let gameplay = Gameplay.shift direction gameplay
                just gameplay
            else just gameplay

        | Nil ->
            just gameplay

    // here we handle the above commands
    override this.Command (_, command, screen, world) =

        match command with
        | StartQuitting ->
            let world = World.publish () screen.QuitEvent screen world
            just world

    // here we describe the content of the game including the level, the hud, and the player
    override this.Content (gameplay, _) =

        [// the scene group while playing
         match gameplay.GameplayState with
         | Playing gameOver ->
            Content.group Simulants.GameplayScene.Name []

               [// board
                let gutter = v3 4.0f 4.0f 0.0f
                let tileSize = v3 32.0f 32.0f 0.0f
                let tileOffset = (gameplay.BoardSize.V3 * tileSize + gutter * (gameplay.BoardSize - v2iOne).V3) * -0.5f
                Content.panel Simulants.GameplayBoard.Name
                   [Entity.Size == v3 148.0f 148.0f 0.0f
                    Entity.Elevation == 1.0f
                    Entity.BackdropImageOpt == Some Assets.Gameplay.BoardImage]
                   [for tile in gameplay.Tiles do
                       Content.text ("Tile+" + string tile.TileId)
                           [Entity.PositionLocal := tile.Position.V3 * (tileSize + gutter) + tileSize * 0.5f + tileOffset
                            Entity.Size == tileSize
                            Entity.ElevationLocal == 1.0f
                            Entity.Text := string tile.Value
                            Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                            Entity.Font == Assets.Gui.ClearSansFont
                            Entity.FontSizing := if tile.Value < 16384 then Some 12 else Some 8
                            Entity.TextColor == Color.GhostWhite
                            Entity.BackdropImageOpt := Some (Assets.Gameplay.TileImage tile.Value)]]

                // score
                Content.text "Score"
                   [Entity.Position == v3 232.0f 155.0f 0.0f
                    Entity.Elevation == 10.0f
                    Entity.Text := "Score: " + string gameplay.Score]

                // game over
                if gameOver then
                   Content.text "GameOver"
                       [Entity.Position == v3 0.0f 155.0f 0.0f
                        Entity.Elevation == 10.0f
                        Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                        Entity.Text == "Game Over!"]]

         // nothing while quit
         | Quit -> ()

         // the gui group
         Content.group Simulants.GameplayGui.Name []
            [Content.button Simulants.GameplayQuit.Name
                [Entity.Position == v3 232.0f -144.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Text == "Quit"
                 Entity.ClickEvent => StartQuitting]]]