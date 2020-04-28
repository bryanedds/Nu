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
        | ShowBattle
        | ExitGame

    type Game with
    
        member this.GetRom = this.GetModel<Rom>
        member this.SetRom = this.SetModel<Rom>
        member this.Rom = this.Model<Rom>

    type StandAloneDispatcher () =
        inherit GameDispatcher<Rom, unit, OmniCommand> (Rom.readFromFiles ())

        override this.Register (game, world) =
            let world = World.hintRenderPackageUse Assets.GuiPackage world
            let world = World.hintAudioPackageUse Assets.GuiPackage world
            base.Register (game, world)

        override this.Bindings (_, _, _) =
            [Simulants.Splash.RegisterEvent => cmd PlaySplashSound
             Simulants.Title.IncomingStartEvent => cmd PlayTitleSong
             Simulants.Title.OutgoingStartEvent => cmd FadeSong
             Simulants.TitleCredits.ClickEvent => cmd ShowCredits
             Simulants.TitlePlay.ClickEvent => cmd ShowBattle
             Simulants.TitleExit.ClickEvent => cmd ExitGame
             Simulants.CreditsBack.ClickEvent => cmd ShowTitle
             Simulants.Battle.OutgoingStartEvent => cmd FadeSong
             Simulants.BattleBack.ClickEvent => cmd ShowTitle]

        override this.Command (_, command, _, world) =
            let world =
                match command with
                | PlayTitleSong -> World.playSong 0 (1.0f * Constants.Audio.MasterSongVolume) Assets.TitleSong world
                | PlaySplashSound -> World.playSound 1.0f Assets.NuSplashSound world
                | FadeSong -> World.fadeOutSong Constants.Audio.DefaultTimeToFadeOutSongMs world
                | ShowTitle -> World.transitionScreen Simulants.Title world
                | ShowCredits -> World.transitionScreen Simulants.Credits world
                | ShowBattle -> World.transitionScreen Simulants.Battle world
                | ExitGame -> World.exit world
            just world

        override this.Content (_, _, _) =
            [Content.screenFromLayerFile<BattleDispatcher> Simulants.Battle.Name (Dissolve Constants.OmniBlade.DissolveData) Assets.BattleHudLayerFilePath
             Content.screen Simulants.Splash.Name (Splash (Constants.OmniBlade.DissolveData, Constants.OmniBlade.SplashData, Simulants.Title)) [] []
             Content.screenFromLayerFile Simulants.Title.Name (Dissolve Constants.OmniBlade.DissolveData) Assets.TitleLayerFilePath
             Content.screenFromLayerFile Simulants.Credits.Name (Dissolve Constants.OmniBlade.DissolveData) Assets.CreditsLayerFilePath]

    type EditorDispatcher () =
        inherit GameDispatcher<Rom, unit, OmniCommand> (Rom.readFromFiles ())