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
            [Simulants.Field.Field.ChangeEvent =|> fun evt -> msg (ChangeField (evt.Data.Value :?> Field))
             Simulants.Battle.Battle.ChangeEvent =|> fun evt -> msg (ChangeBattle (evt.Data.Value :?> Battle))
             Simulants.Game.UpdateEvent => msg Update
#if DEV
             Simulants.TitleNew.ClickEvent => msg (Change (Field (Field.initial (uint64 Gen.random))))
#else
             Simulants.TitleNew.ClickEvent => msg Intro
#endif
             Simulants.TitleLoad.ClickEvent =|> fun _ -> msg (Change (Field (Field.loadOrInitial (uint64 Gen.random))))
             Simulants.TitleCredits.ClickEvent => msg (Change (Gui Credits))
             Simulants.TitleExit.ClickEvent => cmd Exit
             Simulants.CreditsBack.ClickEvent => msg (Change (Gui Title))
             Simulants.FieldBack.ClickEvent => msg (Change (Gui Title))
             Simulants.Intro5.DeselectEvent => msg (Change (Field (Field.initial (uint64 Gen.random))))]

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
                    | Title -> withCmd (Show Simulants.Title) omni
                    | Credits -> withCmd (Show Simulants.Credits) omni
                | Field field ->
                    match field.BattleOpt with
                    | Some battle ->
                        match battle.BattleState with
                        | BattleCease (result, consequents, time) ->
                            if World.getTickTime world - time = 60L then
                                if result
                                then withCmd (Show Simulants.Field) (Field (Field.synchronizeFromBattle consequents battle field))
                                else withCmd (Show Simulants.Title) (Gui Title)
                            else withCmd (Show Simulants.Battle) omni
                        | _ -> withCmd (Show Simulants.Battle) omni
                    | None -> withCmd (Show Simulants.Field) omni

            | Intro ->
                let splashing = msg (Change (Gui Splashing))
                let intro = cmd (Show Simulants.Intro)
                withSigs [splashing; intro] omni

        override this.Command (_, command, _, world) =
            match command with
            | Show screen -> World.transitionScreen screen world |> just
            | Exit -> World.exit world |> just

        override this.Content (omni, _) =

            [// splash
             Content.screen Simulants.Splash.Name (Splash (Constants.Gui.Dissolve, Constants.Gui.Splash, None, Some Simulants.Title)) [] []

             // title
             Content.screenFromLayerFile Simulants.Title.Name (Dissolve (Constants.Gui.Dissolve, Some Assets.Gui.TitleSong)) Assets.Gui.TitleLayerFilePath

             // credits
             Content.screenFromLayerFile Simulants.Credits.Name (Dissolve (Constants.Gui.Dissolve, Some Assets.Gui.TitleSong)) Assets.Gui.CreditsLayerFilePath

             // intros
             Content.screenFromLayerFile Simulants.Intro.Name (Splash (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.WindSong, Some Simulants.Intro2)) Assets.Gui.IntroLayerFilePath
             Content.screenFromLayerFile Simulants.Intro2.Name (Splash (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.WindSong, Some Simulants.Intro3)) Assets.Gui.Intro2LayerFilePath
             Content.screenFromLayerFile Simulants.Intro3.Name (Splash (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.WindSong, Some Simulants.Intro4)) Assets.Gui.Intro3LayerFilePath
             Content.screenFromLayerFile Simulants.Intro4.Name (Splash (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.WindSong, Some Simulants.Intro5)) Assets.Gui.Intro4LayerFilePath
             Content.screenFromLayerFile Simulants.Intro5.Name (Splash (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.WindSong, None)) Assets.Gui.Intro5LayerFilePath

             // field
             Content.screen<FieldDispatcher> Simulants.Field.Name (Dissolve (Constants.Gui.Dissolve, None))
                [Screen.Field <== omni --> fun omni ->
                    match omni with
                    | Gui _ -> Field.empty
                    | Field field -> field] []

             // battle
             Content.screen<BattleDispatcher> Simulants.Battle.Name (Dissolve (Constants.Gui.Dissolve, Some Assets.Battle.BattleSong))
                [Screen.Battle <== omni --> fun omni ->
                    match omni with
                    | Gui _ -> Battle.empty
                    | Field field -> Option.getOrDefault Battle.empty field.BattleOpt] []]