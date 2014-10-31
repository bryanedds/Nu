namespace BlazeVector
open System
open Prime
open Nu
open Nu.Constants
open BlazeVector
open BlazeVector.BlazeConstants
module BlazeProgression =
    
    /// Creates BlazeVector-specific components (dispatchers and facets).
    /// Allows BlazeVector simulation types to be created in the game as well as in NuEdit.
    type BlazeComponentFactory () =
        inherit UserComponentFactory ()

        // make our game-specific entity dispatchers...
        override this.MakeEntityDispatchers () =
            Map.ofList
                [typeof<BulletDispatcher>.Name, BulletDispatcher () :> EntityDispatcher
                 typeof<PlayerDispatcher>.Name, PlayerDispatcher () :> EntityDispatcher
                 typeof<EnemyDispatcher>.Name, EnemyDispatcher () :> EntityDispatcher]

        // make our game-specific group dispatchers...
        override this.MakeGroupDispatchers () =
            Map.ofList
                [typeof<StagePlayDispatcher>.Name, StagePlayDispatcher () :> GroupDispatcher]

        // make our game-specific screen dispatchers...
        override this.MakeScreenDispatchers () =
            Map.ofList
                [typeof<StageScreenDispatcher>.Name, StageScreenDispatcher () :> ScreenDispatcher]

        // make our game-specific game dispatchers...
        override this.MakeGameDispatchers () =
            Map.ofList
                [typeof<BlazeVectorDispatcher>.Name, BlazeVectorDispatcher () :> GameDispatcher]

        // currently we have no game-specific facets to create, so no need to override MakeFacets.

    // this function handles playing the song "Machinery"
    let handlePlaySongMachinery _ world =
        let world = World.playSong MachinerySong 1.0f 0 world
        (Cascade, world)

    // this function handles playing the stage
    let handlePlayStage _ world =
        let oldWorld = world
        let world = World.fadeOutSong DefaultTimeToFadeOutSongMs world
        let stageScreen = World.getScreen StageAddress world
        match World.tryTransitionScreen StageAddress stageScreen world with
        | Some world -> (Cascade, world)
        | None -> (Cascade, oldWorld)

    // this function adds the BlazeVector title screen to the world
    let addTitleScreen world =

        // this adds a dissolve screen from the specified file with the given parameters
        let world = snd <| World.addDissolveScreenFromFile typeof<ScreenDispatcher>.Name TitleGroupFilePath IncomingTime OutgoingTime TitleAddress world

        // this subscribes to the event that is raised when the Title screen is selected for
        // display and interaction, and handles the event by playing the song "Machinery"
        let world = World.subscribe4 SelectTitleEventAddress Address.empty handlePlaySongMachinery world

        // subscribes to the event that is raised when the Title screen's Play button is
        // clicked, and handles the event by transitioning to the Stage screen
        let world = World.subscribe4 ClickTitlePlayEventAddress Address.empty handlePlayStage world

        // subscribes to the event that is raised when the Title screen's Credits button is
        // clicked, and handles the event by transitioning to the Credits screen
        let world = World.subscribe4 ClickTitleCreditsEventAddress Address.empty (World.handleAsScreenTransition CreditsAddress) world

        // subscribes to the event that is raised when the Title screen's Exit button is clicked,
        // and handles the event by exiting the game
        World.subscribe4 ClickTitleExitEventAddress Address.empty World.handleAsExit world

    // pretty much the same as above, but for the Credits screen
    let addCreditsScreen world =
        let world = snd <| World.addDissolveScreenFromFile typeof<ScreenDispatcher>.Name CreditsGroupFilePath IncomingTime OutgoingTime CreditsAddress world
        World.subscribe4 ClickCreditsBackEventAddress Address.empty (World.handleAsScreenTransition TitleAddress) world

    // and so on.
    let addStageScreen world =
        let world = snd <| World.addDissolveScreenFromFile typeof<StageScreenDispatcher>.Name StageGroupFilePath IncomingTime StageOutgoingTime StageAddress world
        World.subscribe4 ClickStageBackEventAddress Address.empty (World.handleAsScreenTransition TitleAddress) world

    // here we make the BlazeVector world in a callback from the World.run function.
    let tryMakeBlazeVectorWorld sdlDeps userState =

        // create our game's component factory
        let blazeComponentFactory = BlazeComponentFactory ()

        // we use World.tryMake to create an empty world that we will transform to create the
        // BlazeVector world
        let optWorld = World.tryMake sdlDeps blazeComponentFactory UIAndPhysicsAndGamePlay false userState
        match optWorld with
        | Right world ->

            // hint to the renderer that the UI package should be loaded up front
            let world = World.hintRenderingPackageUse UIPackageName world
            
            // add our UI screens to the world
            let world = addTitleScreen world
            let world = addCreditsScreen world
            let world = addStageScreen world

            // add to the world a splash screen that automatically transitions to the Title screen
            let splashScreenImage = { ImageAssetName = "Image5"; PackageName = DefaultPackageName }
            let (splashScreen, world) = World.addSplashScreenFromData TitleAddress SplashAddress typeof<ScreenDispatcher>.Name SplashIncomingTime SplashIdlingTime SplashOutgoingTime splashScreenImage world

            // play a neat sound effect, select the splash screen, and we're off!
            let world = World.playSound NuSplashSound 1.0f world
            let world = snd <| World.selectScreen SplashAddress splashScreen world
            Right world

        // propagate error
        | Left _ as left -> left