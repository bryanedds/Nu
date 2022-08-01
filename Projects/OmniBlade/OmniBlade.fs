// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System.IO
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module OmniBlade =

    type Gui =
        | Splash
        | Title
        | Credits
        | Pick
        | Intro of SaveSlot

    type [<NoComparison>] Model =
        | Gui of Gui
        | Field of Field

    type Message =
        | ShowTitle
        | ShowCredits
        | ShowPick
        | ShowIntro of SaveSlot
        | TryLoad of SaveSlot
        | UpdateMessage

    type Command =
        | UpdateCommand
        | Exit

    type Game with
        member this.GetModel world = this.GetModelGeneric<Model> world
        member this.SetModel value world = this.SetModelGeneric<Model> value world
        member this.Model = this.ModelGeneric<Model> ()
        member this.Field =
            this.Model |>
            Lens.narrow (fun world -> match this.GetModel world with Field _ -> true | _ -> false) |>
            Lens.bimap (function Field field -> field | _ -> failwithumf ()) Field
        member this.Battle =
            this.Model |>
            Lens.narrow (fun world -> match this.GetModel world with Field field -> Option.isSome field.BattleOpt | _ -> false) |>
            Lens.bimapWorld
                (fun model _ -> match model with Field field when Option.isSome field.BattleOpt -> Option.get field.BattleOpt | _ -> failwithumf ())
                (fun battle world -> match this.GetModel world with Field field -> Field (Field.updateBattleOpt (constant (Some battle)) field) | _ -> failwithumf ())

    type OmniBladeDispatcher () =
        inherit GameDispatcher<Model, Message, Command> (Gui Splash)

        override this.Register (game, world) =
#if DEV
            let world = World.setMasterSongVolume 0.0f world
#endif
            base.Register (game, world)

        override this.Initializers (omni, game) =
            [game.Field <=> Simulants.Field.Screen.Field
             game.Battle <=> Simulants.Battle.Screen.Battle
             game.DesiredScreenOpt <== omni --> fun omni ->
                match omni with
                | Gui gui ->
                    match gui with
                    | Splash -> None
                    | Title -> Some Simulants.Title.Screen
                    | Credits -> Some Simulants.Credits.Screen
                    | Pick -> Some Simulants.Pick.Screen
                    | Intro _ -> Some Simulants.Intro.Screen
                | Field field ->
                    match field.FieldState with
                    | Playing ->
                        match field.BattleOpt with
                        | Some battle ->
                            match battle.BattleState with
                            | BattleQuitting (_, _, _) -> Some Simulants.Field.Screen
                            | _ -> Some Simulants.Battle.Screen
                        | None -> Some Simulants.Field.Screen
                    | Quitting -> Some Simulants.Title.Screen]

        override this.Channel (_, _) =
            [Simulants.Game.UpdateEvent => msg UpdateMessage
             Simulants.Game.UpdateEvent => cmd UpdateCommand
             Simulants.Title.Gui.Play.ClickEvent =|> fun _ -> msg ShowPick
             Simulants.Title.Gui.Credits.ClickEvent => msg ShowCredits
             Simulants.Pick.Gui.NewGame1.ClickEvent => msg (ShowIntro Slot1)
             Simulants.Pick.Gui.NewGame2.ClickEvent => msg (ShowIntro Slot2)
             Simulants.Pick.Gui.NewGame3.ClickEvent => msg (ShowIntro Slot3)
             Simulants.Pick.Gui.LoadGame1.ClickEvent => msg (TryLoad Slot1)
             Simulants.Pick.Gui.LoadGame2.ClickEvent => msg (TryLoad Slot2)
             Simulants.Pick.Gui.LoadGame3.ClickEvent => msg (TryLoad Slot3)
             Simulants.Pick.Gui.Back.ClickEvent => msg ShowTitle
             Simulants.Credits.Gui.Back.ClickEvent => msg ShowTitle
             Simulants.Title.Gui.Exit.ClickEvent => cmd Exit]

        override this.Message (omni, message, _, world) =

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
                match Field.tryLoad saveSlot world with
                | Some loaded -> just (Field loaded)
                | None -> just omni

            | UpdateMessage ->
                match omni with
                | Gui gui ->
                    match gui with
                    | Intro slot ->
                        match Simulants.Intro5.Screen.GetTransitionState world with
                        | OutgoingState -> just (Field (Field.initial slot (max 1UL Gen.randomul) world))
                        | _ -> just omni
                    | _ -> just omni
                | Field field ->
                    match field.BattleOpt with
                    | Some battle ->
                        match battle.BattleState with
                        | BattleQuitting (_, outcome, consequents) ->
                            if outcome then
                                let field = Field.synchronizeFromBattle consequents battle field
                                just (Field (Field.updateBattleOpt (constant None) field))
                            else just (Field (Field.updateFieldState (constant Quitting) field))
                        | _ -> just omni
                    | None -> just omni

        override this.Command (_, command, _, world) =

            match command with
            | UpdateCommand ->

                // update picks
                let world =
                    if Simulants.Pick.Screen.IsSelected world then
                        let world = Simulants.Pick.Gui.NewGame1.SetVisible (not (File.Exists Assets.Global.SaveFilePath1)) world
                        let world = Simulants.Pick.Gui.NewGame2.SetVisible (not (File.Exists Assets.Global.SaveFilePath2)) world
                        let world = Simulants.Pick.Gui.NewGame3.SetVisible (not (File.Exists Assets.Global.SaveFilePath3)) world
                        let world = Simulants.Pick.Gui.LoadGame1.SetVisible (File.Exists Assets.Global.SaveFilePath1) world
                        let world = Simulants.Pick.Gui.LoadGame2.SetVisible (File.Exists Assets.Global.SaveFilePath2) world
                        let world = Simulants.Pick.Gui.LoadGame3.SetVisible (File.Exists Assets.Global.SaveFilePath3) world
                        world
                    else world

                // update full screen toggle
                let world =
                    if KeyboardState.isAltDown () && KeyboardState.isKeyDown KeyboardKey.Return then
                        match World.tryGetWindowFullScreen world with
                        | Some fullScreen -> World.trySetWindowFullScreen (not fullScreen) world
                        | None -> world
                    else world

                // fin
                just world

            | Exit -> just (World.exit world)

        override this.Content (_, _) =

            [// splash
             Content.screen Simulants.Splash.Screen.Name (Nu.Splash (Constants.Gui.Dissolve, Constants.Gui.Splash, None, Simulants.Title.Screen)) [] []

             // title
             Content.screenFromGroupFile Simulants.Title.Screen.Name (Dissolve (Constants.Gui.Dissolve, Some Assets.Gui.TitleSong)) Assets.Gui.TitleGroupFilePath

             // credits
             Content.screenFromGroupFile Simulants.Credits.Screen.Name (Dissolve (Constants.Gui.Dissolve, Some Assets.Gui.TitleSong)) Assets.Gui.CreditsGroupFilePath

             // pick
             Content.screenFromGroupFile Simulants.Pick.Screen.Name (Dissolve ({ Constants.Gui.Dissolve with OutgoingTime = 90L }, Some Assets.Gui.TitleSong)) Assets.Gui.PickGroupFilePath

             // field
             Content.screen<FieldDispatcher> Simulants.Field.Screen.Name (Dissolve (Constants.Gui.Dissolve, None)) [] []

             // battle
             Content.screen<BattleDispatcher> Simulants.Battle.Screen.Name (Dissolve (Constants.Gui.Dissolve, None)) [] []

             // intros
             Content.screenFromGroupFile Simulants.Intro.Screen.Name (Nu.Splash (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro2.Screen)) Assets.Gui.IntroGroupFilePath
             Content.screenFromGroupFile Simulants.Intro2.Screen.Name (Nu.Splash (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro3.Screen)) Assets.Gui.Intro2GroupFilePath
             Content.screenFromGroupFile Simulants.Intro3.Screen.Name (Nu.Splash (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro4.Screen)) Assets.Gui.Intro3GroupFilePath
             Content.screenFromGroupFile Simulants.Intro4.Screen.Name (Nu.Splash (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro5.Screen)) Assets.Gui.Intro4GroupFilePath
             Content.screenFromGroupFile Simulants.Intro5.Screen.Name (Nu.Splash (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Field.Screen)) Assets.Gui.Intro5GroupFilePath]