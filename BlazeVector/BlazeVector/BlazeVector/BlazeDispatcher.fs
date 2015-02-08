namespace BlazeVector
open OpenTK
open SDL2
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open BlazeVector
open BlazeVector.BlazeConstants

[<AutoOpen>]
module BlazeDispatcherModule =

    /// The custom type for BlazeVector's game dispatcher.
    type BlazeDispatcher () =
        inherit GameDispatcher ()

        // this function handles the selection of the title screen by playing the song "Machinery"
        static let handleSelectTitleScreen _ world =
            let world = World.playSong 0 1.0f MachinerySong world
            (Cascade, world)

        // this function handles the clicking of the play button on the title screen by playing
        // the game
        static let handleClickTitlePlay _ world =
            let world = World.fadeOutSong DefaultTimeToFadeOutSongMs world
            let world = World.transitionScreen Gameplay world
            (Cascade, world)

        // this function creates the BlazeVector title screen to the world
        static let createTitleScreen world =

            // this creates a dissolve screen from the specified file with the given parameters
            let world = snd <| World.createDissolveScreenFromGroupFile false DissolveData typeof<ScreenDispatcher>.Name TitleGroupFilePath (Some TitleName) world

            // this subscribes to the event that is raised when the Title screen is selected for
            // display and interaction, and handles the event by playing the song "Machinery".
            //
            // You will need to familiarize yourself with the calling conventions of the various
            // World.subscribe functions as well as the event address operators '->>-' and its ilk
            // by studying their types and documentation comments.
            let world = World.subscribe4 handleSelectTitleScreen (SelectEventAddress ->>- Title.ScreenAddress) Game world

            // subscribes to the event that is raised when the Title screen's Play button is
            // clicked, and handles the event by transitioning to the Gameplay screen
            let world = World.subscribe4 handleClickTitlePlay (ClickEventAddress ->>- TitlePlay.EntityAddress) Game world

            // subscribes to the event that is raised when the Title screen's Credits button is
            // clicked, and handles the event by transitioning to the Credits screen
            let world = World.subscribe4 (World.handleAsScreenTransition Credits) (ClickEventAddress ->>- TitleCredits.EntityAddress) Game world

            // subscribes to the event that is raised when the Title screen's Exit button is clicked,
            // and handles the event by exiting the game
            World.subscribe4 World.handleAsExit (ClickEventAddress ->>- TitleExit.EntityAddress) Game world

        // pretty much the same as above, but for the Credits screen
        static let createCreditsScreen world =
            let world = snd <| World.createDissolveScreenFromGroupFile false DissolveData typeof<ScreenDispatcher>.Name CreditsGroupFilePath (Some CreditsName) world
            World.subscribe4 (World.handleAsScreenTransition Title) (ClickEventAddress ->>- CreditsBack.EntityAddress) Game world

        // and so on.
        static let createGameplayScreen world =
            let world = snd <| World.createDissolveScreenFromGroupFile false DissolveData typeof<GameplayScreenDispatcher>.Name HudGroupFilePath (Some GameplayName) world
            World.subscribe4 (World.handleAsScreenTransition Title) (ClickEventAddress ->>- GameplayBack.EntityAddress) Game world

        // game registration is where the game's high-level logic is set up!
        override dispatcher.Register _ world =

            // hint to the renderer that the 'Gui' package should be loaded up front
            let world = World.hintRenderPackageUse GuiPackageName world

            // create our screens
            let world = createTitleScreen world
            let world = createCreditsScreen world
            let world = createGameplayScreen world

            // create a splash screen that automatically transitions to the Title screen
            let (splash, world) = World.createSplashScreen false SplashData typeof<ScreenDispatcher>.Name Title (Some SplashName) world

            // play a neat sound effect, select the splash screen, and we're off!
            let world = World.playSound 1.0f NuSplashSound world
            World.selectScreen splash world