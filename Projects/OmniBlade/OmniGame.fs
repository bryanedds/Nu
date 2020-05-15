namespace OmniBlade
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module OmniGame =

    type GuiModel =
        | Splashing
        | Title
        | Credits

    type [<NoComparison>] OmniModel =
        | Gui of GuiModel
        | Field of FieldModel

    type [<NoComparison>] OmniMessage =
        | Update
        | UpdateModel of OmniModel
        | UpdateFieldModel of FieldModel
        | UpdateBattleModel of BattleModel

    type [<NoComparison>] OmniCommand =
        | Show of Screen
        | Exit

    type Game with

        member this.GetOmniModel = this.GetModel<OmniModel>
        member this.SetOmniModel = this.SetModel<OmniModel>
        member this.OmniModel = this.Model<OmniModel> ()

    type OmniDispatcher () =
        inherit GameDispatcher<OmniModel, OmniMessage, OmniCommand> (Gui Splashing)

        override this.Register (game, world) =
            let world = World.hintRenderPackageUse Assets.GuiPackageName world
            let world = World.hintAudioPackageUse Assets.GuiPackageName world
            base.Register (game, world)

        override this.Channel (_, _) =
            [Simulants.Game.UpdateEvent => [msg Update]
             Simulants.TitleCredits.ClickEvent => [msg (UpdateModel (Gui Credits))]
             Simulants.CreditsBack.ClickEvent => [msg (UpdateModel (Gui Title))]
             Simulants.FieldBack.ClickEvent => [msg (UpdateModel (Gui Title))]
             Simulants.TitlePlay.ClickEvent => [msg (UpdateModel (Field FieldModel.empty))]
             Simulants.Field.FieldModel.ChangeEvent =|> fun evt -> [msg (UpdateFieldModel (evt.Data.Value :?> FieldModel))]
             Simulants.Battle.BattleModel.ChangeEvent =|> fun evt -> [msg (UpdateBattleModel (evt.Data.Value :?> BattleModel))]
             Simulants.TitleExit.ClickEvent => [cmd Exit]]

        override this.Message (model, message, _, world) =

            match message with
            | UpdateModel model ->
                ignore ()
                just model

            | UpdateFieldModel field ->
                match model with
                | Gui _ -> just model
                | Field _ -> just (Field field)

            | UpdateBattleModel battle ->
                match model with
                | Gui _ -> just model
                | Field field ->
                    match field.BattleOpt with
                    | None -> just model
                    | Some _ -> just (Field (FieldModel.updateBattleOpt (constant (Some battle)) field))

            | Update ->
                match model with
                | Gui gui ->
                    match gui with
                    | Splashing -> just model
                    | Title -> withCmd model (Show Simulants.Title)
                    | Credits -> withCmd model (Show Simulants.Credits)
                | Field field ->
                    match field.BattleOpt with
                    | Some battle ->
                        match battle.BattleState with
                        | BattleCease (_, time) ->
                            if World.getTickTime world - time < 120L
                            then withCmd model (Show Simulants.Battle)
                            else withCmd model (Show Simulants.Field)
                        | _ -> withCmd model (Show Simulants.Battle)
                    | None -> withCmd model (Show Simulants.Field)

        override this.Command (_, command, _, world) =
            match command with
            | Show screen -> World.transitionScreen screen world |> just
            | Exit -> World.exit world |> just

        override this.Content (model, _) =

            [// splash
             Content.screen Simulants.Splash.Name (Splash (Constants.Dissolve.Default, Constants.Splash.Default, Simulants.Title)) [] []

             // title
             Content.screenFromLayerFile Simulants.Title.Name (Dissolve (Constants.Dissolve.Default, (Some Assets.TitleSong))) Assets.TitleLayerFilePath

             // credits
             Content.screenFromLayerFile Simulants.Credits.Name (Dissolve (Constants.Dissolve.Default, (Some Assets.TitleSong))) Assets.CreditsLayerFilePath

             // field
             Content.screen<FieldDispatcher> Simulants.Field.Name (Dissolve (Constants.Dissolve.Default, (Some Assets.FieldSong)))
                [Screen.FieldModel <== model --> fun model ->
                    match model with
                    | Gui _ -> FieldModel.empty
                    | Field field -> field] []

             // battle
             Content.screen<BattleDispatcher> Simulants.Battle.Name (Dissolve (Constants.Dissolve.Default, (Some Assets.BattleSong)))
                [Screen.BattleModel <== model --> fun model ->
                    match model with
                    | Gui _ -> BattleModel.empty
                    | Field field -> Option.getOrDefault BattleModel.empty field.BattleOpt] []]