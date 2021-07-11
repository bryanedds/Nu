// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module GameDispatcher =

    type Gui =
        | Splashing
        | Title
        | Credits

    type [<StructuralEquality; NoComparison>] Omni =
        | Gui of Gui
        | Field of Field

    type [<NoEquality; NoComparison>] OmniMessage =
        | Change of Omni
        | ChangeField of Field
        | ChangeBattle of Battle
        | Update
        | Intro

    type [<NoEquality; NoComparison>] OmniCommand =
        | Show of Screen
        | Exit

    type Game with
        member this.GetOmni = this.GetModel<Omni>
        member this.SetOmni = this.SetModel<Omni>
        member this.Omni = this.Model<Omni> ()

    type OmniDispatcher () =
        inherit GameDispatcher<Omni, OmniMessage, OmniCommand> (Gui Splashing)

        override this.Register (game, world) =
            let world = World.hintRenderPackageUse Assets.Gui.PackageName world
            let world = World.hintAudioPackageUse Assets.Gui.PackageName world
#if DEV
            let world = World.setMasterSongVolume 0.0f world
#endif
            base.Register (game, world)

        override this.Channel (_, _) =
            [Simulants.Field.Screen.Field.ChangeEvent =|> fun evt -> msg (ChangeField (evt.Data.Value :?> Field))
             Simulants.Battle.Screen.Battle.ChangeEvent =|> fun evt -> msg (ChangeBattle (evt.Data.Value :?> Battle))
             Simulants.Game.UpdateEvent => msg Update
#if DEV
             Simulants.Title.Gui.New.ClickEvent => msg (Change (Field (Field.initial (uint64 Gen.random))))
#else
             Simulants.Title.Gui.New.ClickEvent => msg Intro
#endif
             Simulants.Title.Gui.Load.ClickEvent => msg (Change (Field (Field.loadOrInitial (uint64 Gen.random))))
             Simulants.Title.Gui.Credits.ClickEvent => msg (Change (Gui Credits))
             Simulants.Title.Gui.Exit.ClickEvent => cmd Exit
             Simulants.Credits.Gui.Back.ClickEvent => msg (Change (Gui Title))
             Simulants.Intro5.Screen.DeselectEvent => msg (Change (Field (Field.initial (uint64 Gen.random))))]

        override this.Message (omni, message, _, world) =

            match message with
            | Change omni ->
                just omni

            | ChangeField field ->
                match omni with
                | Gui _ -> just omni
                | Field _ -> just (Field field)

            | ChangeBattle battle ->
                match omni with
                | Gui _ -> just omni
                | Field field ->
                    match field.BattleOpt with
                    | None -> just omni
                    | Some _ -> just (Field (Field.updateBattleOpt (constant (Some battle)) field))

            | Update ->
                match omni with
                | Gui gui ->
                    match gui with
                    | Splashing -> just omni
                    | Title -> withCmd (Show Simulants.Title.Screen) omni
                    | Credits -> withCmd (Show Simulants.Credits.Screen) omni
                | Field field ->
                    match field.BattleOpt with
                    | Some battle ->
                        match battle.BattleState with
                        | BattleCease (result, consequents, time) ->
                            if World.getTickTime world - time = 60L then
                                if result
                                then withCmd (Show Simulants.Field.Screen) (Field (Field.synchronizeFromBattle consequents battle field))
                                else withCmd (Show Simulants.Title.Screen) (Gui Title)
                            else withCmd (Show Simulants.Battle.Screen) omni
                        | _ -> withCmd (Show Simulants.Battle.Screen) omni
                    | None -> withCmd (Show Simulants.Field.Screen) omni

            | Intro ->
                let splashing = msg (Change (Gui Splashing))
                let intro = cmd (Show Simulants.Intro.Screen)
                withSigs [splashing; intro] omni

        override this.Command (_, command, _, world) =
            match command with
            | Show screen -> World.transitionScreen screen world |> just
            | Exit -> World.exit world |> just

        override this.Content (omni, _) =

            [// splash
             Content.screen Simulants.Splash.Screen.Name (Splash (Constants.Gui.Dissolve, Constants.Gui.Splash, None, Simulants.Title.Screen)) [] []

             // title
             Content.screenFromGroupFile Simulants.Title.Screen.Name (Dissolve (Constants.Gui.Dissolve, Some Assets.Gui.TitleSong)) Assets.Gui.TitleGroupFilePath

             // credits
             Content.screenFromGroupFile Simulants.Credits.Screen.Name (Dissolve (Constants.Gui.Dissolve, Some Assets.Gui.TitleSong)) Assets.Gui.CreditsGroupFilePath

             // intros
             Content.screenFromGroupFile Simulants.Intro.Screen.Name (Splash (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro2.Screen)) Assets.Gui.IntroGroupFilePath
             Content.screenFromGroupFile Simulants.Intro2.Screen.Name (Splash (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro3.Screen)) Assets.Gui.Intro2GroupFilePath
             Content.screenFromGroupFile Simulants.Intro3.Screen.Name (Splash (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro4.Screen)) Assets.Gui.Intro3GroupFilePath
             Content.screenFromGroupFile Simulants.Intro4.Screen.Name (Splash (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro5.Screen)) Assets.Gui.Intro4GroupFilePath
             Content.screenFromGroupFile Simulants.Intro5.Screen.Name (Splash (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Field.Screen)) Assets.Gui.Intro5GroupFilePath

             // field
             Content.screen<FieldDispatcher> Simulants.Field.Screen.Name (Dissolve (Constants.Gui.Dissolve, None))
                [Screen.Field <== omni --> fun omni ->
                    match omni with
                    | Gui _ -> Field.empty
                    | Field field -> field] []

             // battle
             Content.screen<BattleDispatcher> Simulants.Battle.Screen.Name (Dissolve (Constants.Gui.Dissolve, None))
                [Screen.Battle <== omni --> fun omni ->
                    match omni with
                    | Gui _ -> Battle.empty
                    | Field field -> Option.getOrDefault Battle.empty field.BattleOpt] []]