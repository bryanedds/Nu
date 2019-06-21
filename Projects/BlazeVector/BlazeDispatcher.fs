namespace BlazeVector
open System
open Prime
open Nu
open BlazeVector

[<AutoOpen>]
module BlazeDispatcherModule =

    /// The custom type for BlazeVector's game dispatcher.
    type BlazeDispatcher () =
        inherit GameDispatcher ()

        // this function handles the selection of the title screen by playing the song "Machinery"
        static let handleSelectTitleScreen _ world =
            World.playSong 0 1.0f Assets.MachinerySong world

        // this function handles the clicking of the play button on the title screen by playing
        // the game
        static let handleClickTitlePlay _ world =
            let world = World.fadeOutSong Constants.Audio.DefaultTimeToFadeOutSongMs world
            World.transitionScreen Gameplay world

        // this function creates the BlazeVector title screen
        static let createTitleScreen game world =

            // this creates a dissolve screen from the specified file with the given parameters
            let world = World.createDissolveScreenFromLayerFile (Some Title.ScreenName) Constants.BlazeVector.DissolveData Assets.TitleLayerFilePath world |> snd

            // this subscribes to the event that is raised when the Title screen is selected for
            // display and interaction, and handles the event by playing the song "Machinery".
            //
            // You will need to familiarize yourself with the calling conventions of the various
            // World.monitor functions as well as the event address operators '-->' and its ilk
            // by studying their types and documentation comments.
            let world = World.monitor handleSelectTitleScreen Title.SelectEvent game world

            // subscribes to the event that is raised when the Title screen's Play button is
            // clicked, and handles the event by transitioning to the Gameplay screen
            let world = World.monitor handleClickTitlePlay TitlePlay.ClickEvent game world

            // subscribes to the event that is raised when the Title screen's Credits button is
            // clicked, and handles the event by transitioning to the Credits screen
            let world = World.monitor (World.handleAsScreenTransition Credits) TitleCredits.ClickEvent game world

            // subscribes to the event that is raised when the Title screen's Exit button is clicked,
            // and handles the event by exiting the game
            World.monitorPlus World.handleAsExit TitleExit.ClickEvent game world |> snd

        // pretty much the same as above, but for the Credits screen
        static let createCreditsScreen game world =
            let world = World.createDissolveScreenFromLayerFile (Some Credits.ScreenName) Constants.BlazeVector.DissolveData Assets.CreditsLayerFilePath world |> snd
            World.monitor (World.handleAsScreenTransition Title) CreditsBack.ClickEvent game world

        // and so on.
        static let createGameplayScreen game world =
            let world = World.createDissolveScreenFromLayerFile<GameplayScreenDispatcher> (Some Gameplay.ScreenName) Constants.BlazeVector.DissolveData Assets.GameplayLayerFilePath world |> snd
            World.monitor (World.handleAsScreenTransition Title) GameplayBack.ClickEvent game world

        // game registration is where the game's high-level logic is set up!
        override dispatcher.Register (game, world) =

            // hint to the renderer and audio system that the 'Gui' package should be loaded up front
            let world = World.hintRenderPackageUse Assets.GuiPackage world
            let world = World.hintAudioPackageUse Assets.GuiPackage world

            // create our screens
            let world = createTitleScreen game world
            let world = createCreditsScreen game world
            let world = createGameplayScreen game world

            // create a splash screen that automatically transitions to the Title screen
            let (splash, world) = World.createSplashScreen (Some Splash.ScreenName) Constants.BlazeVector.SplashData Title world

            // play a neat sound effect, select the splash screen, and we're off!
            let world = World.playSound 1.0f Assets.NuSplashSound world
            World.selectScreen splash world