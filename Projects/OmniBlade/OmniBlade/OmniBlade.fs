// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.IO
open Prime
open Nu

type OmniBlade =
    | Splash
    | Title
    | Credits
    | Pick
    | Intro of SaveSlot
    | Field
    | Battle

type OmniBladeMessage =
    | ShowTitle
    | ShowCredits
    | ShowPick
    | ShowIntro of SaveSlot
    | ShowFieldInitial
    | TryLoad of SaveSlot
    | CommencingBattle
    | CommenceBattle of BattleData * PrizePool
    | ConcludingBattle of bool
    | ConcludeBattle
    interface Message

type OmniBladeCommand =
    | SetField of Field
    | FromFieldToBattle of BattleData * PrizePool
    | FromBattleToField of PrizePool
    | Update
    | Exit
    interface Command

[<AutoOpen>]
module OmniBladeExtensions =
    type Game with
        member this.GetOmniBlade world = this.GetModelGeneric<OmniBlade> world
        member this.SetOmniBlade value world = this.SetModelGeneric<OmniBlade> value world
        member this.OmniBlade = this.ModelGeneric<OmniBlade> ()

type OmniBladeDispatcher () =
    inherit GameDispatcher<OmniBlade, OmniBladeMessage, OmniBladeCommand> (Splash)

    override this.Register (game, world) =
        // HACK: since I incorrectly assumed that master song volume was 0.5f while mixing songs in the editor
        // (it's 1.0f, not 0.5f...), I have to override the default master song volume here...
        World.setMasterSongVolume 0.5f world
        base.Register (game, world)

    override this.Definitions (omniBlade, _) =
        [Game.DesiredScreen :=
            match omniBlade with
            | Splash -> Desire Simulants.Splash
            | Title -> Desire Simulants.Title
            | Credits -> Desire Simulants.Credits
            | Pick -> Desire Simulants.Pick
            | Intro _ -> Desire Simulants.Intro
            | Field -> Desire Simulants.Field
            | Battle -> Desire Simulants.Battle
         Game.UpdateEvent => Update
         if omniBlade = Splash then Simulants.Splash.DeselectingEvent => ShowTitle
         Simulants.TitlePlay.ClickEvent => ShowPick
         Simulants.TitleCredits.ClickEvent => ShowCredits
         Simulants.TitleExit.ClickEvent => Exit
         Simulants.CreditsBack.ClickEvent => ShowTitle
         Simulants.PickNewGame1.ClickEvent => ShowIntro Slot1
         Simulants.PickNewGame2.ClickEvent => ShowIntro Slot2
         Simulants.PickNewGame3.ClickEvent => ShowIntro Slot3
         Simulants.PickLoadGame1.ClickEvent => TryLoad Slot1
         Simulants.PickLoadGame2.ClickEvent => TryLoad Slot2
         Simulants.PickLoadGame3.ClickEvent => TryLoad Slot3
         Simulants.PickBack.ClickEvent => ShowTitle
         Simulants.Intro5.DeselectingEvent => ShowFieldInitial
         Simulants.Field.QuitFieldEvent => ShowTitle
         Simulants.Field.CommencingBattleEvent => CommencingBattle
         Simulants.Field.CommenceBattleEvent =|> fun evt -> CommenceBattle evt.Data
         Simulants.Battle.ConcludingBattleEvent =|> fun evt -> ConcludingBattle evt.Data
         Simulants.Battle.ConcludeBattleEvent => ConcludeBattle]

    override this.Message (omniBlade, message, _, world) =

        match message with
        | ShowTitle ->
            just Title

        | ShowCredits ->
            just Credits

        | ShowPick ->
            just Pick

        | ShowIntro slot ->
            just (Intro slot)

        | ShowFieldInitial ->
            let slot = match omniBlade with Intro slot -> slot | _ -> Slot1
            let field = Field.initial world.UpdateTime (World.getViewBounds2dAbsolute world) slot
            withSignal (SetField field) Field

        | TryLoad saveSlot ->
            match Field.tryLoad world.UpdateTime saveSlot with
            | Some loaded -> withSignal (SetField loaded) Field
            | None -> just omniBlade

        | CommencingBattle ->
            just Battle

        | CommenceBattle (battleData, prizePool) ->
            withSignal (FromFieldToBattle (battleData, prizePool)) Battle

        | ConcludingBattle outcome ->
            if outcome then just Field else just Title

        | ConcludeBattle ->
            let battle = Simulants.Battle.GetBattle world
            match battle.BattleState with
            | BattleConcluding (_, outcome) ->
                if outcome
                then withSignal (FromBattleToField battle.PrizePool) Field
                else just Title
            | _ -> just omniBlade

    override this.Command (_, command, _, world) =

        match command with
        | SetField field ->
            let world = Simulants.Field.SetField field world
            just world

        | FromFieldToBattle (battleData, prizePool) ->
            let field = Simulants.Field.GetField world
            let playTime = Option.defaultValue field.FieldTime field.FieldSongTimeOpt
            let songTime = field.FieldTime - playTime
            let (battle, field) = Field.commenceBattle songTime battleData prizePool field
            let world = Simulants.Battle.SetBattle battle world
            let world = Simulants.Field.SetField field world
            just world

        | FromBattleToField prizePool ->
            let battle = Simulants.Battle.GetBattle world
            let field = Simulants.Field.GetField world
            let field = Field.concludeBattle prizePool.Consequents battle field
            let world = Simulants.Battle.SetBattle Battle.empty world
            let world = Simulants.Field.SetField field world
            just world

        | Update ->

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
                if World.isKeyboardAltDown world && World.isKeyboardKeyDown KeyboardKey.Enter world && world.Unaccompanied then
                    match World.tryGetWindowFullScreen world with
                    | Some fullScreen -> World.trySetWindowFullScreen (not fullScreen) world
                    | None -> world
                else world

            // fin
            just world

        | Exit ->
            if world.Unaccompanied
            then just (World.exit world)
            else just world

    override this.Content (_, _) =
        [Content.screen<ScreenDispatcher> Simulants.Splash.Name (Slide (Constants.Gui.Dissolve, Constants.Gui.Splash, None, Simulants.Title)) [] []
         Content.screenWithGroupFromFile Simulants.Title.Name (Dissolve (Constants.Gui.Dissolve, Some Assets.Gui.TitleSong)) Assets.Gui.TitleGroupFilePath [] []
         Content.screenWithGroupFromFile Simulants.Credits.Name (Dissolve (Constants.Gui.Dissolve, Some Assets.Gui.TitleSong)) Assets.Gui.CreditsGroupFilePath [] []
         Content.screenWithGroupFromFile Simulants.Pick.Name (Dissolve ({ Constants.Gui.Dissolve with OutgoingTime = 90L }, Some Assets.Gui.TitleSong)) Assets.Gui.PickGroupFilePath [] []
         Content.screenWithGroupFromFile Simulants.Intro.Name (Slide (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro2)) Assets.Gui.IntroGroupFilePath [] []
         Content.screenWithGroupFromFile Simulants.Intro2.Name (Slide (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro3)) Assets.Gui.Intro2GroupFilePath [] []
         Content.screenWithGroupFromFile Simulants.Intro3.Name (Slide (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro4)) Assets.Gui.Intro3GroupFilePath [] []
         Content.screenWithGroupFromFile Simulants.Intro4.Name (Slide (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro5)) Assets.Gui.Intro4GroupFilePath [] []
         Content.screenWithGroupFromFile Simulants.Intro5.Name (Slide (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Field)) Assets.Gui.Intro5GroupFilePath [] []
         Content.screen<FieldDispatcher> Simulants.Field.Name (Dissolve (Constants.Gui.Dissolve, None)) [] []
         Content.screen<BattleDispatcher> Simulants.Battle.Name (Dissolve (Constants.Gui.Dissolve, None)) [] []]