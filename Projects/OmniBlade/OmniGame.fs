namespace OmniBlade
open Prime
open Nu
open OmniBlade

[<AutoOpen>]
module OmniGame =

    type OmniCommand =
        | PlaySplashSound
        | PlayTitleSong
        | FadeSong
        | ShowTitle
        | ShowCredits
        | ShowField
        | ExitGame

    type StandAloneDispatcher () =
        inherit GameDispatcher<unit, unit, OmniCommand> (())

        override this.Register (game, world) =
            let world = World.hintRenderPackageUse Assets.GuiPackageName world
            let world = World.hintAudioPackageUse Assets.GuiPackageName world
            base.Register (game, world)

        override this.Channel (_, _, _) =
            [Simulants.Splash.RegisterEvent => [cmd PlaySplashSound]
             Simulants.Title.IncomingStartEvent => [cmd PlayTitleSong]
             Simulants.Title.OutgoingStartEvent => [cmd FadeSong]
             Simulants.TitleCredits.ClickEvent => [cmd ShowCredits]
             Simulants.TitlePlay.ClickEvent => [cmd ShowField]
             Simulants.TitleExit.ClickEvent => [cmd ExitGame]
             Simulants.CreditsBack.ClickEvent => [cmd ShowTitle]
             Simulants.Field.OutgoingStartEvent => [cmd FadeSong]
             Simulants.FieldBack.ClickEvent => [cmd ShowTitle]
             Simulants.Battle.OutgoingStartEvent => [cmd FadeSong]
             Simulants.BattleBack.ClickEvent => [cmd ShowTitle]]

        override this.Command (_, command, _, world) =
            let world =
                match command with
                | PlayTitleSong -> World.playSong 0 (Constants.Audio.DefaultSongVolume) Assets.TitleSong world
                | PlaySplashSound -> World.playSound Constants.Audio.DefaultSoundVolume Assets.NuSplashSound world
                | FadeSong -> World.fadeOutSong Constants.Audio.DefaultTimeToFadeOutSongMs world
                | ShowTitle -> World.transitionScreen Simulants.Title world
                | ShowCredits -> World.transitionScreen Simulants.Credits world
                | ShowField -> World.transitionScreen Simulants.Field world
                | ExitGame -> World.exit world
            just world

        override this.Content (_, _, _) =
            [Content.screen Simulants.Splash.Name (Splash (Constants.OmniBlade.DissolveData, Constants.OmniBlade.SplashData, Simulants.Title)) [] []
             Content.screenFromLayerFile Simulants.Title.Name (Dissolve Constants.OmniBlade.DissolveData) Assets.TitleLayerFilePath
             Content.screenFromLayerFile Simulants.Credits.Name (Dissolve Constants.OmniBlade.DissolveData) Assets.CreditsLayerFilePath
             Content.screenFromLayerFile<FieldDispatcher> Simulants.Field.Name (Dissolve Constants.OmniBlade.DissolveData) Assets.FieldHudLayerFilePath
             Content.screen<BattleDispatcher> Simulants.Battle.Name (Dissolve Constants.OmniBlade.DissolveData) [] []]

    type EditorDispatcher () =
        inherit GameDispatcher<unit, unit, OmniCommand> (())