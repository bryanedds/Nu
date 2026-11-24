// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.IO
open Prime
open Nu

/// What the game shows. The screens without models of their own - the splash, the pick and the intro - keep the
/// whether the pointer's shown and the player's intent here, the others in their own models.
type OmniBlade =
    | Splash of bool * Intent
    | Title
    | Credits
    | Pick of int * SaveSlot Set * bool * Intent
    | Intro of SaveSlot * bool * Intent
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
    | ConcludingBattle of BattleOutcome
    | ConcludeBattle
    | ProcessInput
    interface Message

type OmniBladeCommand =
    | UpdateFullScreen of KeyboardKeyData
    | UpdatePicks
    | SetField of Field
    | SetIntent of Screen * bool * Intent
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
    inherit GameDispatcher<OmniBlade, OmniBladeMessage, OmniBladeCommand> (Splash (true, Intent.empty))

    static let IntroScreens =
        [Simulants.Intro; Simulants.Intro2; Simulants.Intro3; Simulants.Intro4; Simulants.Intro5]

    /// Whether the pointer's shown as whatever holds it for the game's model keeps it - the case itself, or the screen
    /// with a model of its own.
    static let getPointerShown omniBlade world =
        match omniBlade with
        | Splash (pointerShown, _) | Pick (_, _, pointerShown, _) | Intro (_, pointerShown, _) -> pointerShown
        | Title -> (Simulants.Title.GetTitle world).TitlePointerShown
        | Credits -> (Simulants.Credits.GetCredits world).CreditsPointerShown
        | Field -> (Simulants.Field.GetField world).FieldPointerShown
        | Battle -> (Simulants.Battle.GetBattle world).BattlePointerShown

    /// Hand whether the pointer's shown over to whatever holds it for the game's model, its intent starting anew - the
    /// case itself, or a command to give it to the screen with a model of its own.
    static let handover omniBlade pointerShown : OmniBlade * Signal list =
        match omniBlade with
        | Splash _ -> (Splash (pointerShown, Intent.empty), [])
        | Pick (selection, saves, _, _) -> (Pick (selection, saves, pointerShown, Intent.empty), [])
        | Intro (slot, _, _) -> (Intro (slot, pointerShown, Intent.empty), [])
        | Title -> (omniBlade, [SetIntent (Simulants.Title, pointerShown, Intent.empty) :> Signal])
        | Credits -> (omniBlade, [SetIntent (Simulants.Credits, pointerShown, Intent.empty) :> Signal])
        | Field -> (omniBlade, [SetIntent (Simulants.Field, pointerShown, Intent.empty) :> Signal])
        | Battle -> (omniBlade, [SetIntent (Simulants.Battle, pointerShown, Intent.empty) :> Signal])

    static member Properties =
        [nonPersistent Game.IntentOverrideOpt None]

    override this.Register (game, world) =
        base.Register (game, world)
        World.setMasterSongVolume Constants.Gameplay.SongVolumeDefault world

    override this.Definitions (omniBlade, _) =
        [Game.KeyboardKeyDownEvent =|> fun evt -> UpdateFullScreen evt.Data
         Game.UpdateEvent => ProcessInput
         Simulants.Title.TitlePlayEvent => ShowPick
         Simulants.Title.TitleCreditsEvent => ShowCredits
         Simulants.Title.TitleExitEvent => Exit
         if omniBlade.IsSplash then Simulants.Splash.DeselectingEvent => ShowTitle
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
        
        // handle message
        let (signals, omniBlade') =
            match message with
            | ShowTitle ->
                just Title

            | ShowCredits ->
                just Credits

            | ShowPick ->
                just (Pick (0, Algorithms.getPickSaves (), getPointerShown omniBlade world, Intent.empty))

            | ShowIntro slot ->
                just (Intro (slot, getPointerShown omniBlade world, Intent.empty))

            | ShowFieldInitial ->
                let slot = match omniBlade with Intro (slot, _, _) -> slot | _ -> Slot1
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
                match outcome with
                | WinBattle -> just Field
                | LoseBattle -> just Title
                | RetryBattle (battleData, prizePool) -> withSignal (FromFieldToBattle (battleData, prizePool)) Battle

            | ConcludeBattle ->
                let battle = Simulants.Battle.GetBattle world
                match battle.BattleState with
                | BattleConcluding (_, outcome) ->
                    match outcome with
                    | WinBattle -> withSignal (FromBattleToField battle.PrizePool) Field
                    | LoseBattle -> just Title
                    | RetryBattle (_, _) -> just Battle
                | _ -> just omniBlade

            | ProcessInput ->
                match (World.getSelectedScreenOpt world, omniBlade) with
                | (Some screen, Splash (pointerShown, intent)) when screen = Simulants.Splash ->
                    just (Splash (Intent.read pointerShown intent world))
                | (Some screen, Intro (slot, pointerShown, intent)) when List.contains screen IntroScreens ->
                    let (pointerShown, intent) = Intent.read pointerShown intent world
                    just (Intro (slot, pointerShown, intent))
                | (Some screen, Pick (selection, saves, pointerShown, intent)) when screen = Simulants.Pick ->
                    let (pointerShown, intent) = Intent.read pointerShown intent world
                    let omniBlade = Pick (selection, saves, pointerShown, intent)
                    if screen.GetIdling world then
                        let selection =
                            match intent.Step with
                            | Some direction -> Selection.wrap (Selection.rowDelta direction) 4 selection
                            | None -> selection
                        let omniBlade = Pick (selection, saves, pointerShown, intent)
                        if intent.Confirm then
                            match selection with
                            | 0 | 1 | 2 ->
                                let slot = match selection with 0 -> Slot1 | 1 -> Slot2 | _ -> Slot3
                                World.playSound 0.0f 0.0f Constants.Audio.SoundVolumeDefault Assets.Gui.SlotSound world
                                withSignal (if saves.Contains slot then TryLoad slot else ShowIntro slot) omniBlade
                            | _ -> Selection.playAffirm world; withSignal ShowTitle omniBlade
                        elif intent.Cancel then
                            Selection.playAffirm world
                            withSignal ShowTitle omniBlade
                        else just omniBlade
                    else just omniBlade
                | (_, _) -> just omniBlade

        // handle handovers
        if getCaseTag omniBlade <> getCaseTag omniBlade' then
            let (omniBlade', handing) = handover omniBlade' (getPointerShown omniBlade world)
            (signals @ handing, omniBlade')
        else (signals, omniBlade')

    override this.Command (omniBlade, command, _, world) =

        match command with
        | UpdateFullScreen keyboardKeyData ->
            if keyboardKeyData.KeyboardKey = KeyboardKey.Enter && World.isKeyboardAltDown world && world.Unaccompanied then
                match World.tryGetWindowFullScreen world with
                | Some fullScreen -> World.trySetWindowFullScreen (not fullScreen) world
                | None -> ()

        | UpdatePicks ->
            match omniBlade with
            | Pick (_, saves, _, _) when Simulants.Pick.GetSelected world ->
                Simulants.PickNewGame1.SetVisible (not (saves.Contains Slot1)) world
                Simulants.PickNewGame2.SetVisible (not (saves.Contains Slot2)) world
                Simulants.PickNewGame3.SetVisible (not (saves.Contains Slot3)) world
                Simulants.PickLoadGame1.SetVisible (saves.Contains Slot1) world
                Simulants.PickLoadGame2.SetVisible (saves.Contains Slot2) world
                Simulants.PickLoadGame3.SetVisible (saves.Contains Slot3) world
            | _ -> ()

        | SetField field ->
            Simulants.Field.SetField field world

        | SetIntent (screen, pointerShown, intent) ->
            if screen = Simulants.Title then
                let title = Simulants.Title.GetTitle world
                Simulants.Title.SetTitle { title with TitlePointerShown = pointerShown; TitleIntent = intent } world
            elif screen = Simulants.Credits then
                let credits = Simulants.Credits.GetCredits world
                Simulants.Credits.SetCredits { credits with CreditsPointerShown = pointerShown; CreditsIntent = intent } world
            elif screen = Simulants.Field then
                let field = Simulants.Field.GetField world
                Simulants.Field.SetField (field |> Field.setFieldPointerShown pointerShown |> Field.setFieldIntent intent) world
            elif screen = Simulants.Battle then
                let battle = Simulants.Battle.GetBattle world
                Simulants.Battle.SetBattle (battle |> Battle.setBattlePointerShown pointerShown |> Battle.setBattleIntent intent) world

        | FromFieldToBattle (battleData, prizePool) ->
            let battle = Simulants.Battle.GetBattle world
            let field = Simulants.Field.GetField world
            let playTime = Option.defaultValue field.FieldTime field.FieldSongTimeOpt
            let songTime = field.FieldTime - playTime
            let (battle', field) = Field.commenceBattle songTime battleData prizePool field
            let battle' = battle' |> Battle.setBattlePointerShown battle.BattlePointerShown |> Battle.setBattleIntent battle.BattleIntent
            Simulants.Battle.SetBattle battle' world
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

    override this.Render (omniBlade, renderPass, _, world) =

        // point at the pick screen's selection
        match (omniBlade, renderPass) with
        | (Pick (selection, saves, pointerShown, _), NormalPass) when Simulants.Pick.GetSelected world && Simulants.Pick.GetIdling world ->
            let entity =
                match selection with
                | 0 -> if saves.Contains Slot1 then Simulants.PickLoadGame1 else Simulants.PickNewGame1
                | 1 -> if saves.Contains Slot2 then Simulants.PickLoadGame2 else Simulants.PickNewGame2
                | 2 -> if saves.Contains Slot3 then Simulants.PickLoadGame3 else Simulants.PickNewGame3
                | _ -> Simulants.PickBack
            Pointer.render pointerShown entity world.UpdateTime world
        | _ -> ()

    override this.Content (omniBlade, _) =
        [Content.screen<ScreenDispatcher> Simulants.Splash.Name omniBlade.IsSplash (Slide (Constants.Gui.Dissolve, Constants.Gui.Splash, None, Simulants.Title)) [] []
         Content.screenWithGroupFromFile<TitleDispatcher> Simulants.Title.Name omniBlade.IsTitle (Dissolve (Constants.Gui.Dissolve, Some Assets.Gui.TitleSong)) Assets.Gui.TitleGroupFilePath [] []
         Content.screen<CreditsDispatcher> Simulants.Credits.Name omniBlade.IsCredits (Dissolve (Constants.Gui.Dissolve, None)) [] []
         Content.screenWithGroupFromFile Simulants.Pick.Name omniBlade.IsPick (Dissolve ({ Constants.Gui.Dissolve with OutgoingTime = 90L }, Some Assets.Gui.TitleSong)) Assets.Gui.PickGroupFilePath [] []
         Content.screenWithGroupFromFile Simulants.Intro.Name omniBlade.IsIntro (Slide (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro2)) Assets.Gui.IntroGroupFilePath [] []
         Content.screenWithGroupFromFile Simulants.Intro2.Name false (Slide (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro3)) Assets.Gui.Intro2GroupFilePath [] []
         Content.screenWithGroupFromFile Simulants.Intro3.Name false (Slide (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro4)) Assets.Gui.Intro3GroupFilePath [] []
         Content.screenWithGroupFromFile Simulants.Intro4.Name false (Slide (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Intro5)) Assets.Gui.Intro4GroupFilePath [] []
         Content.screenWithGroupFromFile Simulants.Intro5.Name false (Slide (Constants.Intro.Dissolve, Constants.Intro.Splash, Some Assets.Gui.IntroSong, Simulants.Field)) Assets.Gui.Intro5GroupFilePath [] []
         Content.screen<FieldDispatcher> Simulants.Field.Name omniBlade.IsField (Dissolve (Constants.Gui.Dissolve, None)) [] []
         Content.screen<BattleDispatcher> Simulants.Battle.Name omniBlade.IsBattle (Dissolve (Constants.Gui.Dissolve, None)) [] []]