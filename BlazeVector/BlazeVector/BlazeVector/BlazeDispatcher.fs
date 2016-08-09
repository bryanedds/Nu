namespace BlazeVector
open OpenTK
open SDL2
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
            let world = World.playSong 0 1.0f Constants.Assets.MachinerySong world
            (Cascade, world)

        // this function handles the clicking of the play button on the title screen by playing
        // the game
        static let handleClickTitlePlay _ world =
            let world = World.fadeOutSong Constants.Audio.DefaultTimeToFadeOutSongMs world
            let world = World.transitionScreen Simulants.Gameplay world
            (Cascade, world)

        // this function creates the BlazeVector title screen to the world
        static let createTitleScreen world =

            // this creates a dissolve screen from the specified file with the given parameters
            let world = World.createDissolveScreenFromGroupFile false Constants.BlazeVector.DissolveData Constants.FilePaths.TitleGroup typeof<ScreenDispatcher>.Name None (Some Simulants.Title.ScreenName) world |> snd

            // this subscribes to the event that is raised when the Title screen is selected for
            // display and interaction, and handles the event by playing the song "Machinery".
            //
            // You will need to familiarize yourself with the calling conventions of the various
            // World.subscribe functions as well as the event address operators '->-' and its ilk
            // by studying their types and documentation comments.
            let world = World.subscribe handleSelectTitleScreen (Events.Select ->- Simulants.Title) Simulants.Game world

            // subscribes to the event that is raised when the Title screen's Play button is
            // clicked, and handles the event by transitioning to the Gameplay screen
            let world = World.subscribe handleClickTitlePlay (Events.Click ->- Simulants.TitlePlay) Simulants.Game world

            // subscribes to the event that is raised when the Title screen's Credits button is
            // clicked, and handles the event by transitioning to the Credits screen
            let world = World.subscribe (World.handleAsScreenTransition Simulants.Credits) (Events.Click ->- Simulants.TitleCredits) Simulants.Game world

            // subscribes to the event that is raised when the Title screen's Exit button is clicked,
            // and handles the event by exiting the game
            World.subscribe World.handleAsExit (Events.Click ->- Simulants.TitleExit) Simulants.Game world

        // pretty much the same as above, but for the Credits screen
        static let createCreditsScreen world =
            let world = World.createDissolveScreenFromGroupFile false Constants.BlazeVector.DissolveData Constants.FilePaths.CreditsGroup typeof<ScreenDispatcher>.Name None (Some Simulants.Credits.ScreenName) world |> snd
            World.subscribe (World.handleAsScreenTransition Simulants.Title) (Events.Click ->- Simulants.CreditsBack) Simulants.Game world

        // and so on.
        static let createGameplayScreen world =
            let world = World.createDissolveScreenFromGroupFile false Constants.BlazeVector.DissolveData Constants.FilePaths.GameplayGroup typeof<GameplayScreenDispatcher>.Name None (Some Simulants.Gameplay.ScreenName) world |> snd
            World.subscribe (World.handleAsScreenTransition Simulants.Title) (Events.Click ->- Simulants.GameplayBack) Simulants.Game world

        // game registration is where the game's high-level logic is set up!
        override dispatcher.Register (_, world) =

            // hint to the renderer and audio system that the 'Gui' package should be loaded up front
            let world = World.hintRenderPackageUse Constants.Assets.GuiPackageName world
            let world = World.hintAudioPackageUse Constants.Assets.GuiPackageName world

            // create our screens
            let world = createTitleScreen world
            let world = createCreditsScreen world
            let world = createGameplayScreen world

            // create a splash screen that automatically transitions to the Title screen
            let (splash, world) = World.createSplashScreen false Constants.BlazeVector.SplashData Simulants.Title typeof<ScreenDispatcher>.Name None (Some Simulants.Splash.ScreenName) world

            // play a neat sound effect, select the splash screen, and we're off!
            let world = World.playSound 1.0f Constants.Assets.NuSplashSound world
            World.selectScreen splash world