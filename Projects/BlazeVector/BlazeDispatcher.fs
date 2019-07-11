namespace BlazeVector
open System
open Prime
open Nu
open Nu.Declarative
open BlazeVector

[<AutoOpen>]
module BlazeDispatcherModule =

    // this is our Elm-style command type. To learn about the Elm-style, read this article here -
    // https://medium.com/@bryanedds/a-game-engine-that-allows-you-to-program-in-the-elm-style-31d806fbe27f
    type BlazeCommand =
        | PlaySplashSound
        | PlayTitleSong
        | FadeSong
        | ShowTitle
        | ShowCredits
        | ShowGameplay
        | ExitGame

    // this is our main game type implemented in the Elm-style. 
    type BlazeDispatcher () =
        inherit GameDispatcher<unit, unit, BlazeCommand> (())

        // here we define the Bindings used to connect events to their desired commands
        override this.Bindings (_, _, _) =
            [Simulants.Splash.SelectEvent =>! PlaySplashSound
             Simulants.Title.IncomingStartEvent =>! PlayTitleSong
             Simulants.Title.OutgoingStartEvent =>! FadeSong
             Simulants.TitleCredits.ClickEvent =>! ShowCredits
             Simulants.TitlePlay.ClickEvent =>! ShowGameplay
             Simulants.TitleExit.ClickEvent =>! ExitGame
             Simulants.CreditsBack.ClickEvent =>! ShowTitle
             Simulants.Gameplay.OutgoingStartEvent =>! FadeSong
             Simulants.GameplayBack.ClickEvent =>! ShowTitle]

        // here we handle the above commands
        override this.Command (command, _, _, world) =
            match command with
            | PlaySplashSound -> World.playSound 1.0f Assets.NuSplashSound world
            | PlayTitleSong -> World.playSong 0 1.0f Assets.MachinerySong world
            | FadeSong -> World.fadeOutSong Constants.Audio.DefaultTimeToFadeOutSongMs world
            | ShowTitle -> World.transitionScreen Simulants.Title world
            | ShowCredits -> World.transitionScreen Simulants.Credits world
            | ShowGameplay -> World.transitionScreen Simulants.Gameplay world
            | ExitGame -> World.exit world

        // here we describe the content of the game including all of its screens.
        override this.Content (_, _, _) =
            [Content.screen Simulants.Splash (Nu.Splash (Constants.BlazeVector.DissolveData, Constants.BlazeVector.SplashData, Simulants.Title)) [] []
             Content.screenFromLayerFile Simulants.Title (Dissolve Constants.BlazeVector.DissolveData) Assets.TitleLayerFilePath
             Content.screenFromLayerFile Simulants.Credits (Dissolve Constants.BlazeVector.DissolveData) Assets.CreditsLayerFilePath
             Content.screenFromLayerFile<GameplayDispatcher> Simulants.Gameplay (Dissolve Constants.BlazeVector.DissolveData) Assets.GameplayLayerFilePath]

        // here we hint to the renderer and audio system that the 'Gui' package should be loaded
        override dispatcher.Register (game, world) =
            let world = World.hintRenderPackageUse Assets.GuiPackage world
            let world = World.hintAudioPackageUse Assets.GuiPackage world
            base.Register (game, world)