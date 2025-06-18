﻿// Nu Game Engine.
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
    | UpdateFullScreen of KeyboardKeyData
    | UpdatePicks
    | SetField of Field
    | FromFieldToBattle of BattleData * PrizePool
    | FromBattleToField of PrizePool
    | ConcludeCredits
    | SynchronizeSongVolume
    | ResetSongVolume
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
        base.Register (game, world)
        World.setMasterSongVolume Constants.Gameplay.SongVolumeDefault world

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
         Game.KeyboardKeyDownEvent =|> fun evt -> UpdateFullScreen evt.Data
         if omniBlade = Splash then Simulants.Splash.DeselectingEvent => ShowTitle
         Simulants.Pick.UpdateEvent => UpdatePicks
         Simulants.TitlePlay.ClickEvent => ShowPick
         Simulants.TitleCredits.ClickEvent => ShowCredits
         Simulants.TitleExit.ClickEvent => Exit
         Simulants.Credits.QuitCreditsEvent => ShowTitle
         Simulants.PickNewGame1.ClickEvent => ShowIntro Slot1
         Simulants.PickNewGame2.ClickEvent => ShowIntro Slot2
         Simulants.PickNewGame3.ClickEvent => ShowIntro Slot3
         Simulants.PickLoadGame1.ClickEvent => TryLoad Slot1
         Simulants.PickLoadGame2.ClickEvent => TryLoad Slot2
         Simulants.PickLoadGame3.ClickEvent => TryLoad Slot3
         Simulants.PickBack.ClickEvent => ShowTitle
         Simulants.Intro5.DeselectingEvent => ShowFieldInitial
         Simulants.Credits.DeselectingEvent => ConcludeCredits
         Simulants.Field.QuitFieldEvent => ShowTitle
         Simulants.Field.CommencingBattleEvent => CommencingBattle
         Simulants.Field.CommenceBattleEvent =|> fun evt -> CommenceBattle evt.Data
         Simulants.Field.SelectEvent => SynchronizeSongVolume
         Simulants.Title.SelectEvent => ResetSongVolume
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
            let field = Field.initial world.UpdateTime slot
            withSignals [SetField field; SynchronizeSongVolume] Field

        | TryLoad saveSlot ->
            match Field.tryLoad world.UpdateTime saveSlot with
            | Some field -> withSignals [SetField field; SynchronizeSongVolume] Field
            | None -> just omniBlade

        | CommencingBattle ->
            just Battle

        | CommenceBattle (battleData, prizePool) ->
            withSignal (FromFieldToBattle (battleData, prizePool)) Battle    

        | ConcludingBattle outcome ->
            if outcome
            then just Field
            else just Title

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
        | UpdateFullScreen keyboardKeyData ->
            if keyboardKeyData.KeyboardKey = KeyboardKey.Enter && World.isKeyboardAltDown world && world.Unaccompanied then
                match World.tryGetWindowFullScreen world with
                | Some fullScreen -> World.trySetWindowFullScreen (not fullScreen) world
                | None -> ()

        | UpdatePicks ->
            if Simulants.Pick.GetSelected world then
                Simulants.PickNewGame1.SetVisible (not (File.Exists Assets.User.SaveFilePath1)) world
                Simulants.PickNewGame2.SetVisible (not (File.Exists Assets.User.SaveFilePath2)) world
                Simulants.PickNewGame3.SetVisible (not (File.Exists Assets.User.SaveFilePath3)) world
                Simulants.PickLoadGame1.SetVisible (File.Exists Assets.User.SaveFilePath1) world
                Simulants.PickLoadGame2.SetVisible (File.Exists Assets.User.SaveFilePath2) world
                Simulants.PickLoadGame3.SetVisible (File.Exists Assets.User.SaveFilePath3) world

        | SetField field ->
            Simulants.Field.SetField field world

        | FromFieldToBattle (battleData, prizePool) ->
            let field = Simulants.Field.GetField world
            let playTime = Option.defaultValue field.FieldTime field.FieldSongTimeOpt
            let songTime = field.FieldTime - playTime
            let (battle, field) = Field.commenceBattle songTime battleData prizePool field
            Simulants.Battle.SetBattle battle world
            Simulants.Field.SetField field world

        | FromBattleToField prizePool ->
            let battle = Simulants.Battle.GetBattle world
            let field = Simulants.Field.GetField world
            let field = Field.concludeBattle prizePool.Consequents battle field
            Simulants.Battle.SetBattle Battle.empty world
            Simulants.Field.SetField field world

        | ConcludeCredits ->
            Simulants.Credits.SetCredits (Credits.make true) world

        | SynchronizeSongVolume ->
            let field = Simulants.Field.GetField world
            World.setMasterSongVolume field.Options.SongVolume world

        | ResetSongVolume ->
            World.setMasterSongVolume Constants.Gameplay.SongVolumeDefault world

        | Exit ->
            if world.Unaccompanied then
                World.exit world

    override this.Content (_, _) =
        [Content.screen<ScreenDispatcher> Simulants.Splash.Name (Slide (Constants.Gui.Dissolve, Constants.Gui.Splash, None, Simulants.Title)) [] []
         Content.screenWithGroupFromFile<TitleDispatcher> Simulants.Title.Name (Dissolve (Constants.Gui.Dissolve, Some Assets.Gui.TitleSong)) Assets.Gui.TitleGroupFilePath [] []
         Content.screen<CreditsDispatcher> Simulants.Credits.Name (Dissolve (Constants.Gui.Dissolve, None)) [] []
         Content.screenWithGroupFromFile Simulants.Pick.Name (Dissolve ({ Constants.Gui.Dissolve with OutgoingTime = 90L }, Some Assets.Gui.TitleSong)) Assets.Gui.PickGroupFilePath [] []
         Content.screenWithGroupFromFile Simulants.Intro.Name (Slide (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro2)) Assets.Gui.IntroGroupFilePath [] []
         Content.screenWithGroupFromFile Simulants.Intro2.Name (Slide (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro3)) Assets.Gui.Intro2GroupFilePath [] []
         Content.screenWithGroupFromFile Simulants.Intro3.Name (Slide (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro4)) Assets.Gui.Intro3GroupFilePath [] []
         Content.screenWithGroupFromFile Simulants.Intro4.Name (Slide (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro5)) Assets.Gui.Intro4GroupFilePath [] []
         Content.screenWithGroupFromFile Simulants.Intro5.Name (Slide (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Field)) Assets.Gui.Intro5GroupFilePath [] []
         Content.screen<FieldDispatcher> Simulants.Field.Name (Dissolve (Constants.Gui.Dissolve, None)) [] []
         Content.screen<BattleDispatcher> Simulants.Battle.Name (Dissolve (Constants.Gui.Dissolve, None)) [] []]