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
module BlazeVectorModule =

    /// The custom type for BlazeVector's game dispatcher.
    type BlazeDispatcher () =
        inherit GameDispatcher ()

        // this function handles playing the song "Machinery"
        static let handlePlaySongMachinery _ world =
            let world = World.playSong 0 1.0f MachinerySong world
            (Cascade, world)
    
        // this function handles playing the stage
        static let handlePlayStage _ world =
            let world = World.fadeOutSong DefaultTimeToFadeOutSongMs world
            let world = World.transitionScreen StageAddress world
            (Cascade, world)
    
        // this function adds the BlazeVector title screen to the world
        static let addTitleScreen world =
    
            // this adds a dissolve screen from the specified file with the given parameters
            let world = snd <| World.addDissolveScreenFromGroupFile false DissolveData typeof<ScreenDispatcher>.Name TitleGroupFilePath TitleAddress world
    
            // this subscribes to the event that is raised when the Title screen is selected for
            // display and interaction, and handles the event by playing the song "Machinery"
            let world = World.subscribe4 handlePlaySongMachinery SelectTitleEventAddress GameAddress world
    
            // subscribes to the event that is raised when the Title screen's Play button is
            // clicked, and handles the event by transitioning to the Stage screen
            let world = World.subscribe4 handlePlayStage ClickTitlePlayEventAddress GameAddress world
    
            // subscribes to the event that is raised when the Title screen's Credits button is
            // clicked, and handles the event by transitioning to the Credits screen
            let world = World.subscribe4 (World.handleAsScreenTransition CreditsAddress) ClickTitleCreditsEventAddress GameAddress world
    
            // subscribes to the event that is raised when the Title screen's Exit button is clicked,
            // and handles the event by exiting the game
            World.subscribe4 World.handleAsExit ClickTitleExitEventAddress GameAddress world
    
        // pretty much the same as above, but for the Credits screen
        static let addCreditsScreen world =
            let world = snd <| World.addDissolveScreenFromGroupFile false DissolveData typeof<ScreenDispatcher>.Name CreditsGroupFilePath CreditsAddress world
            World.subscribe4 (World.handleAsScreenTransition TitleAddress) ClickCreditsBackEventAddress GameAddress world
    
        // and so on.
        static let addStageScreen world =
            let world = snd <| World.addDissolveScreenFromGroupFile false DissolveData typeof<StageScreenDispatcher>.Name StageGroupFilePath StageAddress world
            World.subscribe4 (World.handleAsScreenTransition TitleAddress) ClickStageBackEventAddress GameAddress world

        // game registration is where the game's high-level logic is set up!
        override dispatcher.Register (game, world) =

            // hint to the renderer that the Gui package should be loaded up front
            let world = World.hintRenderPackageUse GuiPackageName world
            
            // add our Gui screens to the world
            let world = addTitleScreen world
            let world = addCreditsScreen world
            let world = addStageScreen world

            // add to the world a splash screen that automatically transitions to the Title screen
            let (splashScreen, world) = World.addSplashScreen false SplashData typeof<ScreenDispatcher>.Name TitleAddress SplashAddress world

            // play a neat sound effect, select the splash screen, and we're off!
            let world = World.playSound 1.0f NuSplashSound world
            let world = snd <| World.selectScreen splashScreen SplashAddress world
            (game, world)