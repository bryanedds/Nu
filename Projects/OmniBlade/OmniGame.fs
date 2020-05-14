namespace OmniBlade
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module OmniGame =

    type [<NoComparison>] OmniModel =
        | Title
        | Credits
        | Gameplay of FieldModel * BattleModel option

    type [<NoComparison>] OmniMessage =
        | UpdateModel
        | SetModel of OmniModel

    type [<NoComparison>] OmniCommand =
        | Show of Screen
        | Exit

    type Game with

        member this.GetOmniModel = this.GetModel<OmniModel>
        member this.SetOmniModel = this.SetModel<OmniModel>
        member this.OmniModel = this.Model<OmniModel> ()

    type OmniDispatcher () =
        inherit GameDispatcher<OmniModel, OmniMessage, OmniCommand> (Title)

        override this.Register (game, world) =
            let world = World.hintRenderPackageUse Assets.GuiPackageName world
            let world = World.hintAudioPackageUse Assets.GuiPackageName world
            base.Register (game, world)

        override this.Channel (_, _) =
            [Simulants.Game.OmniModel.ChangeEvent => [msg UpdateModel]
             Simulants.TitlePlay.ClickEvent => [msg (SetModel (Gameplay (FieldModel.empty, None)))]
             Simulants.TitleCredits.ClickEvent => [msg (SetModel Credits)]
             Simulants.CreditsBack.ClickEvent => [msg (SetModel Title)]
             Simulants.FieldBack.ClickEvent => [msg (SetModel Title)]
             Simulants.TitleExit.ClickEvent => [cmd Exit]]

        override this.Message (model, message, _, _) =
            match message with
            | UpdateModel ->
                match model with
                | Title -> withCmd model (Show Simulants.Title)
                | Credits -> withCmd model (Show Simulants.Credits)
                | Gameplay (_, battleModelOpt) ->
                    if Option.isSome battleModelOpt then withCmd model (Show Simulants.Battle)
                    elif Option.isNone battleModelOpt then withCmd model (Show Simulants.Field)
                    else just model
            | SetModel model -> just model

        override this.Command (_, command, _, world) =
            let world =
                match command with
                | Show screen -> World.transitionScreen screen world
                | Exit -> World.exit world
            just world

        override this.Content (model, _) =
            let playTitleSong = { Volume = Constants.Audio.DefaultSongVolume; FadeOutMs = Constants.Audio.DefaultFadeOutMs; Song = Assets.TitleSong }
            let playBattleSong = { Volume = Constants.Audio.DefaultSongVolume; FadeOutMs = Constants.Audio.DefaultFadeOutMs; Song = Assets.BattleSong }
            [Content.screen Simulants.Splash.Name (Splash (Constants.OmniBlade.DissolveData, Constants.OmniBlade.SplashData, Simulants.Title)) [] []
             Content.screenFromLayerFile Simulants.Title.Name (Dissolve (Constants.OmniBlade.DissolveData, (Some playTitleSong))) Assets.TitleLayerFilePath
             Content.screenFromLayerFile Simulants.Credits.Name (Dissolve (Constants.OmniBlade.DissolveData, (Some playTitleSong))) Assets.CreditsLayerFilePath
             Content.screen<FieldDispatcher> Simulants.Field.Name (Dissolve (Constants.OmniBlade.DissolveData, None))
                [Screen.FieldModel <== model --> fun model ->
                    match model with
                    | Title | Credits -> FieldModel.empty
                    | Gameplay (fieldModel, _) -> fieldModel] []
             Content.screen<BattleDispatcher> Simulants.Battle.Name (Dissolve (Constants.OmniBlade.DissolveData, (Some playBattleSong)))
                [Screen.BattleModel <== model --> fun model ->
                    match model with
                    | Title | Credits -> BattleModel.empty
                    | Gameplay (_, battleModelOpt) -> Option.getOrDefault BattleModel.empty battleModelOpt] []]