// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
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

    type [<NoComparison>] Omni =
        | Gui of Gui
        | Field of Field

    type [<NoEquality; NoComparison>] OmniMessage =
        | Update
        | UpdateOmni of Omni
        | UpdateField of Field
        | UpdateBattle of Battle

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
            let world = World.hintRenderPackageUse Assets.GuiPackageName world
            let world = World.hintAudioPackageUse Assets.GuiPackageName world
            base.Register (game, world)

        override this.Channel (_, _) =
            [Simulants.Game.UpdateEvent => msg Update
             Simulants.TitleCredits.ClickEvent => msg (UpdateOmni (Gui Credits))
             Simulants.CreditsBack.ClickEvent => msg (UpdateOmni (Gui Title))
             Simulants.FieldBack.ClickEvent => msg (UpdateOmni (Gui Title))
             Simulants.TitlePlay.ClickEvent => msg (UpdateOmni (Field Field.initial))
             Simulants.Field.Field.ChangeEvent =|> fun evt -> msg (UpdateField (evt.Data.Value :?> Field))
             Simulants.Battle.Battle.ChangeEvent =|> fun evt -> msg (UpdateBattle (evt.Data.Value :?> Battle))
             Simulants.TitleExit.ClickEvent => cmd Exit]

        override this.Message (omni, message, _, world) =

            match message with
            | UpdateOmni omni ->
                just omni

            | UpdateField field ->
                match omni with
                | Gui _ -> just omni
                | Field _ -> just (Field field)

            | UpdateBattle battle ->
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
                        | BattleCease (result, time) ->
                            if World.getTickTime world - time = 120L then
                                if result then
                                    let allies = Battle.getAllies battle
                                    let field =
                                        List.foldi (fun i field (ally : Character) ->
                                            Field.updateLegion (fun legion ->
                                                match Map.tryFind i legion with
                                                | Some legionnaire ->
                                                    let legionnaire = { legionnaire with HitPoints = ally.HitPoints; ExpPoints = ally.ExpPoints }
                                                    Map.add i legionnaire legion
                                                | None -> legion)
                                                field)
                                            field allies
                                    let field = Field.updateInventory (constant battle.Inventory) field
                                    let field = Field.updateBattleOpt (constant None) field
                                    let omni = Field field
                                    withCmd (Show Simulants.Field) omni
                                else withCmd (Show Simulants.Title) (Gui Title)
                            else withCmd (Show Simulants.Battle) omni
                        | _ -> withCmd (Show Simulants.Battle) omni
                    | None -> withCmd (Show Simulants.Field) omni

        override this.Command (_, command, _, world) =
            match command with
            | Show screen -> World.transitionScreen screen world |> just
            | Exit -> World.exit world |> just

        override this.Content (omni, _) =

            [// splash
             Content.screen Simulants.Splash.Name (Splash (Constants.Dissolve.Default, Constants.Splash.Default, Simulants.Title)) [] []

             // title
             Content.screenFromLayerFile Simulants.Title.Name (Dissolve (Constants.Dissolve.Default, (Some Assets.TitleSong))) Assets.TitleLayerFilePath

             // credits
             Content.screenFromLayerFile Simulants.Credits.Name (Dissolve (Constants.Dissolve.Default, (Some Assets.TitleSong))) Assets.CreditsLayerFilePath

             // field
             Content.screen<FieldDispatcher> Simulants.Field.Name (Dissolve (Constants.Dissolve.Default, (Some Assets.FieldSong)))
                [Screen.Field <== omni --> fun omni ->
                    match omni with
                    | Gui _ -> Field.empty
                    | Field field -> field] []

             // battle
             Content.screen<BattleDispatcher> Simulants.Battle.Name (Dissolve (Constants.Dissolve.Default, (Some Assets.BattleSong)))
                [Screen.Battle <== omni --> fun omni ->
                    match omni with
                    | Gui _ -> Battle.empty
                    | Field field -> Option.getOrDefault Battle.empty field.BattleOpt] []]