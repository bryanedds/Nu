namespace BlazeVector
open System
open Prime
open Nu
open Nu.NuConstants
open BlazeVector
open BlazeVector.BlazeConstants
module BlazeFlow =
    
    /// Creates BlazeVector-specific components (dispatchers and facets).
    /// Allows BlazeVector simulation types to be created in the editor.
    type BlazeComponentFactory () =
        interface IUserComponentFactory with

            member dispatcher.MakeUserDispatchers () =
                // make our game's specific dispatchers
                Map.ofList
                    [typeof<BulletDispatcher>.Name, BulletDispatcher () :> obj
                     typeof<PlayerDispatcher>.Name, PlayerDispatcher () :> obj
                     typeof<EnemyDispatcher>.Name, EnemyDispatcher () :> obj
                     typeof<StagePlayDispatcher>.Name, StagePlayDispatcher () :> obj
                     typeof<StageScreenDispatcher>.Name, StageScreenDispatcher () :> obj
                     typeof<BlazeVectorDispatcher>.Name, BlazeVectorDispatcher () :> obj]

            member dispatcher.MakeUserFacets () =
                // currently we have no game-specific facets to create...
                Map.empty

    // this function handles playing the song "Machinery"
    let handlePlaySongMachinery _ world =
        let world = World.playSong MachinerySong 1.0f 0 world
        (Propagate, world)

    // this function handles playing the stage
    let handlePlayStage _ world =
        let oldWorld = world
        let world = World.fadeOutSong DefaultTimeToFadeOutSongMs world
        let stageScreen = World.getScreen StageAddress world
        match World.tryTransitionScreen StageAddress stageScreen world with
        | Some world -> (Propagate, world)
        | None -> (Propagate, oldWorld)

    // this function adds the BlazeVector title screen to the world
    let addTitleScreen world =

        // this adds a dissolve screen from the specified file with the given parameters
        let world = snd <| World.addDissolveScreenFromFile typeof<ScreenDispatcher>.Name TitleGroupFileName (Address.last TitleGroupAddress) IncomingTime OutgoingTime TitleAddress world

        // this subscribes to the event that is raised when the Title screen is selected for
        // display and interaction, and handles the event by playing the song "Machinery"
        let world = World.subscribe4 SelectTitleEventName Address.empty (CustomSub handlePlaySongMachinery) world

        // subscribes to the event that is raised when the Title screen's Play button is
        // clicked, and handles the event by transitioning to the Stage screen
        let world = World.subscribe4 ClickTitlePlayEventName Address.empty (CustomSub handlePlayStage) world

        // subscribes to the event that is raised when the Title screen's Credits button is
        // clicked, and handles the event by transitioning to the Credits screen
        let world = World.subscribe4 ClickTitleCreditsEventName Address.empty (ScreenTransitionSub CreditsAddress) world

        // subscribe4s to the event that is raised when the Title screen's Exit button is clicked,
        // and handles the event by exiting the game
        World.subscribe4 ClickTitleExitEventName Address.empty ExitSub world

    // pretty much the same as above, but for the Credits screen
    let addCreditsScreen world =
        let world = snd <| World.addDissolveScreenFromFile typeof<ScreenDispatcher>.Name CreditsGroupFileName (Address.last CreditsGroupAddress) IncomingTime OutgoingTime CreditsAddress world
        World.subscribe4 ClickCreditsBackEventName Address.empty (ScreenTransitionSub TitleAddress) world

    // and so on.
    let addStageScreen world =
        let world = snd <| World.addDissolveScreenFromFile typeof<StageScreenDispatcher>.Name StageGroupFileName (Address.last StageGroupAddress) IncomingTime StageOutgoingTime StageAddress world
        World.subscribe4 ClickStageBackEventName Address.empty (ScreenTransitionSub TitleAddress) world

    // here we make the BlazeVector world in a callback from the World.run function.
    let tryMakeBlazeVectorWorld sdlDeps extData =

        // create our game's component factory
        let blazeComponentFactory = BlazeComponentFactory ()

        // we use World.tryMake to create an empty world that we will transform to create the
        // BlazeVector world
        let optWorld = World.tryMake sdlDeps blazeComponentFactory GuiAndPhysicsAndGamePlay false extData
        match optWorld with
        | Right world ->

            // hint to the renderer that the Gui package should be loaded up front
            let world = World.hintRenderingPackageUse GuiPackageName world
            
            // add our UI screens to the world
            let world = addTitleScreen world
            let world = addCreditsScreen world
            let world = addStageScreen world

            // add to the world a splash screen that automatically transitions to the Title screen
            let splashScreenImage = { ImageAssetName = "Image5"; PackageName = DefaultPackageName }
            let (splashScreen, world) = World.addSplashScreenFromData TitleAddress SplashAddress typeof<ScreenDispatcher>.Name IncomingTimeSplash IdlingTime OutgoingTimeSplash splashScreenImage world

            // play a neat sound effect, select the splash screen, and we're off!
            let world = World.playSound NuSplashSound 1.0f world
            let world = snd <| World.selectScreen SplashAddress splashScreen world
            Right world

        // propagate error
        | Left _ as left -> left