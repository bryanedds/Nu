// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.IO
open Prime
open Nu

[<AutoOpen>]
module OmniBlade =

    type Gui =
        | Splash
        | Title
        | Credits
        | Pick
        | Intro of SaveSlot

    type Model =
        | Gui of Gui
        | Field of Field

    type Message =
        | ShowTitle
        | ShowCredits
        | ShowPick
        | ShowIntro of SaveSlot
        | TryLoad of SaveSlot
        | UpdateMessage
        interface Nu.Message

    type Command =
        | Exit
        | UpdateCommand
        interface Nu.Command

    type Game with
        member this.GetModel world = this.GetModelGeneric<Model> world
        member this.SetModel value world = this.SetModelGeneric<Model> value world
        member this.Model = this.ModelGeneric<Model> ()

    type OmniBladeDispatcher () =
        inherit GameDispatcher<Model, Message, Command> (Gui Splash)

        override this.Register (game, world) =
            // HACK: since I incorrectly assumed that master song volume was 0.5f while mixing songs in the editor
            // (it's 1.0f, not 0.5f...), I have to override the default master song volume here...
            let world = World.setMasterSongVolume 0.5f world
            base.Register (game, world)

        override this.Definitions (model, _) =
            [Game.DesiredScreen :=
                match model with
                | Gui gui ->
                    match gui with
                    | Splash -> Desire Simulants.Splash
                    | Title -> Desire Simulants.Title
                    | Credits -> Desire Simulants.Credits
                    | Pick -> Desire Simulants.Pick
                    | Intro _ -> Desire Simulants.Intro
                | Field field ->
                    match field.FieldState with
                    | Play ->
                        match field.BattleOpt with
                        | Some battle ->
                            match battle.BattleState with
                            | BattleQuitting (_, _, _) | BattleQuit -> Desire Simulants.Field
                            | _ -> Desire Simulants.Battle
                        | None -> Desire Simulants.Field
                    | Quitting | Quit -> Desire Simulants.Title
             match model with
             | Gui _ -> ()
             | Field field ->
                 match field.BattleOpt with
                 | None -> Simulants.Field.Field := field
                 | Some battle -> Simulants.Battle.Battle := battle
             Game.UpdateEvent => UpdateMessage
             Game.UpdateEvent => UpdateCommand
             Simulants.Splash.DeselectingEvent => ShowTitle
             Simulants.TitlePlay.ClickEvent => ShowPick
             Simulants.TitleCredits.ClickEvent => ShowCredits
             Simulants.TitleExit.ClickEvent => Exit
             Simulants.PickNewGame1.ClickEvent => ShowIntro Slot1
             Simulants.PickNewGame2.ClickEvent => ShowIntro Slot2
             Simulants.PickNewGame3.ClickEvent => ShowIntro Slot3
             Simulants.PickLoadGame1.ClickEvent => TryLoad Slot1
             Simulants.PickLoadGame2.ClickEvent => TryLoad Slot2
             Simulants.PickLoadGame3.ClickEvent => TryLoad Slot3
             Simulants.PickBack.ClickEvent => ShowTitle
             Simulants.CreditsBack.ClickEvent => ShowTitle]

        override this.Message (model, message, _, world) =

            match message with
            | ShowTitle ->
                just (Gui Title)

            | ShowCredits ->
                just (Gui Credits)

            | ShowPick ->
                just (Gui Pick)

            | ShowIntro slot ->
                just (Gui (Intro slot))

            | TryLoad saveSlot ->
                match Field.tryLoad world.UpdateTime saveSlot with
                | Some loaded -> just (Field loaded)
                | None -> just model

            | UpdateMessage ->

                // pull model state from field and battle screens
                let model =
                    match model with
                    | Gui _ -> model
                    | Field field ->
                        match field.BattleOpt with
                        | None ->
                            let field' = Simulants.Field.GetField world
                            if field =/= field' then Field field' else model
                        | Some battle ->
                            let battle' = Simulants.Battle.GetBattle world
                            if battle =/= battle' then Field (Field.updateBattleOpt (constant (Some battle')) field) else model

                // update model
                let model =
                    match model with
                    | Gui gui ->
                        match gui with
                        | Intro slot ->
                            match Simulants.Intro5.GetTransitionState world with
                            | OutgoingState _ -> Field (Field.initial world.UpdateTime (World.getViewBounds2dAbsolute world) slot )
                            | _ -> model
                        | _ -> model
                    | Field field ->
                        match field.BattleOpt with
                        | Some battle ->
                            match battle.BattleState with
                            | BattleQuitting (_, outcome, consequents) ->
                                if outcome then
                                    let field = Field.exitBattle consequents battle field
                                    Field (Field.updateBattleOpt (constant None) field)
                                else Field (Field.updateFieldState (constant Quitting) field)
                            | _ -> model
                        | None -> model

                // fin
                just model

        override this.Command (_, command, _, world) =

            match command with
            | Exit ->

                // exit when not in editor
                if world.Unaccompanied
                then just (World.exit world)
                else just world

            | UpdateCommand ->

                // update picks
                let world =
                    if Simulants.Pick.Selected world then
                        let world = Simulants.PickNewGame1.SetVisible (not (File.Exists Assets.Global.SaveFilePath1)) world
                        let world = Simulants.PickNewGame2.SetVisible (not (File.Exists Assets.Global.SaveFilePath2)) world
                        let world = Simulants.PickNewGame3.SetVisible (not (File.Exists Assets.Global.SaveFilePath3)) world
                        let world = Simulants.PickLoadGame1.SetVisible (File.Exists Assets.Global.SaveFilePath1) world
                        let world = Simulants.PickLoadGame2.SetVisible (File.Exists Assets.Global.SaveFilePath2) world
                        let world = Simulants.PickLoadGame3.SetVisible (File.Exists Assets.Global.SaveFilePath3) world
                        world
                    else world

                // update full screen toggle
                let world =
                    if World.isKeyboardAltDown world && World.isKeyboardKeyDown KeyboardKey.Enter world then
                        match World.tryGetWindowFullScreen world with
                        | Some fullScreen -> World.trySetWindowFullScreen (not fullScreen) world
                        | None -> world
                    else world

                // fin
                just world

        override this.Content (_, _) =
            [Content.screen<ScreenDispatcher> Simulants.Splash.Name (Slide (Constants.Gui.Dissolve, Constants.Gui.Splash, None, Simulants.Title)) [] []
             Content.screenWithGroupFromFile Simulants.Title.Name (Dissolve (Constants.Gui.Dissolve, Some Assets.Gui.TitleSong)) Assets.Gui.TitleGroupFilePath [] []
             Content.screenWithGroupFromFile Simulants.Credits.Name (Dissolve (Constants.Gui.Dissolve, Some Assets.Gui.TitleSong)) Assets.Gui.CreditsGroupFilePath [] []
             Content.screenWithGroupFromFile Simulants.Pick.Name (Dissolve ({ Constants.Gui.Dissolve with OutgoingTime = 90L }, Some Assets.Gui.TitleSong)) Assets.Gui.PickGroupFilePath [] []
             Content.screen<FieldDispatcher> Simulants.Field.Name (Dissolve (Constants.Gui.Dissolve, None)) [] []
             Content.screen<BattleDispatcher> Simulants.Battle.Name (Dissolve (Constants.Gui.Dissolve, None)) [] []
             Content.screenWithGroupFromFile Simulants.Intro.Name (Slide (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro2)) Assets.Gui.IntroGroupFilePath [] []
             Content.screenWithGroupFromFile Simulants.Intro2.Name (Slide (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro3)) Assets.Gui.Intro2GroupFilePath [] []
             Content.screenWithGroupFromFile Simulants.Intro3.Name (Slide (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro4)) Assets.Gui.Intro3GroupFilePath [] []
             Content.screenWithGroupFromFile Simulants.Intro4.Name (Slide (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro5)) Assets.Gui.Intro4GroupFilePath [] []
             Content.screenWithGroupFromFile Simulants.Intro5.Name (Slide (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Field)) Assets.Gui.Intro5GroupFilePath [] []]

        override this.TruncateModel model =
            match model with
            | Field field -> Field (Field.truncate field)
            | _ -> model

        override this.UntruncateModel (current, incoming) =
            match (current, incoming) with
            | (Field fieldCurrent, Field fieldIncoming) -> Field (Field.untruncate fieldCurrent fieldIncoming)
            | _ -> current