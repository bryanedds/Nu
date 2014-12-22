namespace BlazeVector
open System
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open BlazeVector
open BlazeVector.BlazeConstants
module BlazeProgression =
    
    /// Creates BlazeVector-specific values (here dispatchers and facets).
    /// Allows BlazeVector simulation types to be created in the game as well as in NuEdit.
    type BlazePlugin () =
        inherit NuPlugin ()

        // make our game-specific entity dispatchers...
        override this.MakeEntityDispatchers () =
            [BulletDispatcher () :> EntityDispatcher
             PlayerDispatcher () :> EntityDispatcher
             EnemyDispatcher () :> EntityDispatcher]

        // make our game-specific group dispatchers...
        override this.MakeGroupDispatchers () =
            [StagePlayDispatcher () :> GroupDispatcher]

        // make our game-specific screen dispatchers...
        override this.MakeScreenDispatchers () =
            [StageScreenDispatcher () :> ScreenDispatcher]

        // make our game-specific game dispatcher...
        override this.MakeOptGameDispatcher () =
            Some (BlazeVectorDispatcher () :> GameDispatcher)

    // this function handles playing the song "Machinery"
    let handlePlaySongMachinery _ world =
        let world = World.playSong 0 1.0f MachinerySong world
        (Cascade, world)

    // this function handles playing the stage
    let handlePlayStage _ world =
        let world = World.fadeOutSong DefaultTimeToFadeOutSongMs world
        let world = World.transitionScreen StageAddress world
        (Cascade, world)

    // this function adds the BlazeVector title screen to the world
    let addTitleScreen world =

        // this adds a dissolve screen from the specified file with the given parameters
        let world = snd <| World.addDissolveScreenFromGroupFile false DissolveData typeof<ScreenDispatcher>.Name TitleAddress TitleGroupFilePath world

        // this subscribes to the event that is raised when the Title screen is selected for
        // display and interaction, and handles the event by playing the song "Machinery"
        let world = World.subscribe4 SelectTitleEventAddress GameAddress handlePlaySongMachinery world

        // subscribes to the event that is raised when the Title screen's Play button is
        // clicked, and handles the event by transitioning to the Stage screen
        let world = World.subscribe4 ClickTitlePlayEventAddress GameAddress handlePlayStage world

        // subscribes to the event that is raised when the Title screen's Credits button is
        // clicked, and handles the event by transitioning to the Credits screen
        let world = World.subscribe4 ClickTitleCreditsEventAddress GameAddress (World.handleAsScreenTransition CreditsAddress) world

        // subscribes to the event that is raised when the Title screen's Exit button is clicked,
        // and handles the event by exiting the game
        World.subscribe4 ClickTitleExitEventAddress GameAddress World.handleAsExit world

    // pretty much the same as above, but for the Credits screen
    let addCreditsScreen world =
        let world = snd <| World.addDissolveScreenFromGroupFile false DissolveData typeof<ScreenDispatcher>.Name CreditsAddress CreditsGroupFilePath world
        World.subscribe4 ClickCreditsBackEventAddress GameAddress (World.handleAsScreenTransition TitleAddress) world

    // and so on.
    let addStageScreen world =
        let world = snd <| World.addDissolveScreenFromGroupFile false DissolveData typeof<StageScreenDispatcher>.Name StageAddress StageGroupFilePath world
        World.subscribe4 ClickStageBackEventAddress GameAddress (World.handleAsScreenTransition TitleAddress) world

    // here we make the BlazeVector world in a callback from the World.run function.
    let tryMakeBlazeVectorWorld userState sdlDeps =

        // create our game's plugin
        let blazePlugin = BlazePlugin ()

        // we use World.tryMake to create an empty world that we will transform to create the
        // BlazeVector world
        let eitherWorld = World.tryMake false true GuiAndPhysicsAndGamePlay userState blazePlugin sdlDeps 
        match eitherWorld with
        | Right world ->

            // hint to the renderer that the Gui package should be loaded up front
            let world = World.hintRenderPackageUse GuiPackageName world
            
            // add our Gui screens to the world
            let world = addTitleScreen world
            let world = addCreditsScreen world
            let world = addStageScreen world

            // add to the world a splash screen that automatically transitions to the Title screen
            let (splashScreen, world) = World.addSplashScreen false SplashData typeof<ScreenDispatcher>.Name SplashAddress TitleAddress world

            // play a neat sound effect, select the splash screen, and we're off!
            let world = World.playSound 1.0f NuSplashSound world
            let world = snd <| World.selectScreen SplashAddress splashScreen world
            Right world

        // propagate error
        | Left _ as left -> left