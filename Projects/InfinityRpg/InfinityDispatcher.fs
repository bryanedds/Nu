namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module InfinityDispatcherModule =

    type InfinityCommand =
        | PlayTitleSong
        | FadeSong
        | ShowTitle
        | ShowCredits
        | ShowGameplay of bool
        | ExitGame

    type InfinityDispatcher () =
        inherit GameDispatcher<unit, unit, InfinityCommand> (())

        override this.Register (game, world) =

            // just pre-load all assets in the application for simplicity
            let world = World.hintRenderPackageUse Assets.GuiPackage world
            let world = World.hintAudioPackageUse Assets.GuiPackage world
            let world = World.hintRenderPackageUse Assets.GameplayPackage world
            let world = World.hintAudioPackageUse Assets.GameplayPackage world

            // get based
            let world = base.Register (game, world)

            // do not persist the hud when saving gameplay
            Simulants.Hud.SetPersistent false world

        override this.Bindings (_, _, _) =
            [Simulants.Title.IncomingStartEvent =>! PlayTitleSong
             Simulants.Title.OutgoingStartEvent =>! FadeSong
             Simulants.TitleCredits.ClickEvent =>! ShowCredits
             Simulants.TitleNewGame.ClickEvent =>! ShowGameplay false
             Simulants.TitleLoadGame.ClickEvent =>! ShowGameplay true
             Simulants.TitleExit.ClickEvent =>! ExitGame
             Simulants.CreditsBack.ClickEvent =>! ShowTitle
             Simulants.Gameplay.OutgoingStartEvent =>! FadeSong
             Simulants.HudBack.ClickEvent =>! ShowTitle]

        override this.Command (command, _, _, world) =
            match command with
            | PlayTitleSong -> World.playSong 0 1.0f Assets.ButterflyGirlSong world
            | FadeSong -> World.fadeOutSong Constants.Audio.DefaultTimeToFadeOutSongMs world
            | ShowTitle -> World.transitionScreen Simulants.Title world
            | ShowCredits -> World.transitionScreen Simulants.Credits world
            | ShowGameplay load -> world |> Simulants.Gameplay.SetShallLoadGame load |> World.transitionScreen Simulants.Gameplay
            | ExitGame -> World.exit world

        override this.Content (_, _, _) =
            [Content.screen Simulants.Splash.ScreenName (Nu.Splash (Constants.InfinityRpg.DissolveData, Constants.InfinityRpg.SplashData, Simulants.Title)) [] []
             Content.screenFromLayerFile Simulants.Title.ScreenName (Dissolve Constants.InfinityRpg.DissolveData) Assets.TitleLayerFilePath
             Content.screenFromLayerFile Simulants.Credits.ScreenName (Dissolve Constants.InfinityRpg.DissolveData) Assets.CreditsLayerFilePath
             Content.screenFromLayerFile<GameplayDispatcher> Simulants.Gameplay.ScreenName (Dissolve Constants.InfinityRpg.DissolveData) Assets.HudLayerFilePath]