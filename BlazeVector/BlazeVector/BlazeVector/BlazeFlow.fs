namespace BlazeVector
open System
open Prime
open Nu
open BlazeVector
open BlazeVector.BlazeConstants
module BlazeFlow =

    // this function adds the BlazeVector title screen to the world
    let addTitleScreen world =
        
        // this adds a dissolve screen from the specified file with the given parameters. Note that
        // the 'seal' parameter is set to true as no XFields will be added to the title screen
        let world_ = World.addDissolveScreenFromFile TitleGroupFileName (List.last TitleGroupAddress) IncomingTime OutgoingTime TitleAddress true world
        
        // this subscribes to the event that is raised when the Title screen's Play button is
        // clicked, and handles the event by transitioning to the Stage screen
        let world_ = World.subscribe ClickTitlePlayEvent [] (ScreenTransitionSub StageAddress) world_
        
        // subscribes to the event that is raised when the Title screen's Credits button is
        // clicked, and handles the event by transitioning to the Credits screen
        let world_ = World.subscribe ClickTitleCreditsEvent [] (ScreenTransitionSub CreditsAddress) world_
        
        // subscribes to the event that is raised when the Title screen's Exit button is clicked,
        // and handles the event by exiting the game
        World.subscribe ClickTitleExitEvent [] ExitSub world_

    // pretty much the same as above, but for the Credits screen
    let addCreditsScreen world =
        let world' = World.addDissolveScreenFromFile CreditsGroupFileName (List.last CreditsGroupAddress) IncomingTime OutgoingTime CreditsAddress true world
        World.subscribe ClickCreditsBackEvent [] (ScreenTransitionSub TitleAddress) world'

    // and so on.
    let addStageScreen world =
        let world' = World.addDissolveScreenFromFile StageGroupFileName (List.last StageGroupAddress) IncomingTime OutgoingTime StageAddress true world
        World.subscribe ClickStageBackEvent [] (ScreenTransitionSub TitleAddress) world'

    // here we create the BlazeVector world in a callback from the World.run function.
    let tryCreateBlazeVectorWorld sdlDeps extData =

        // our custom game dispatcher here is OmniGameDispatcher
        let gameDispatcher = BlazeGameDispatcher () :> obj

        // we use the World.tryCreateEmptyWorld as a convenience function to create an empty world
        // that we will transform to create the BlazeVector world.
        let optWorld = World.tryCreateEmptyWorld sdlDeps gameDispatcher extData
        match optWorld with
        | Left _ as left -> left
        | Right world ->

            // hint to the renderer that the BlazeGui package should be loaded up front
            let hintRenderPackageUse = HintRenderingPackageUse { FileName = NuConstants.AssetGraphFileName; PackageName = BlazeConstants.BlazeGuiPackageName; HRPU = () } 
            let world_ = { world with RenderMessages = hintRenderPackageUse :: world.RenderMessages }
            
            // specify a song to play for the duration of the game via the audio message system
            let gameSong = { SongAssetName = "Song"; PackageName = NuConstants.DefaultPackageName; PackageFileName = NuConstants.AssetGraphFileName }
            let playSongMessage = PlaySong { Song = gameSong; FadeOutCurrentSong = true }
            let world_ = { world_ with AudioMessages = playSongMessage :: world_.AudioMessages }

            // add to the world a splash screen that automatically transitions to the Title screen
            let splashScreenSprite = { SpriteAssetName = "Image5"; PackageName = NuConstants.DefaultPackageName; PackageFileName = NuConstants.AssetGraphFileName }
            let world_ = World.addSplashScreenFromData (ScreenTransitionSub TitleAddress) SplashAddress IncomingTimeSplash IdlingTime OutgoingTimeSplash splashScreenSprite true world_

            // add our UI screens to the world
            let world_ = addTitleScreen world_
            let world_ = addCreditsScreen world_
            let world_ = addStageScreen world_
            
            // transition the world to splash screen
            let world_ = World.transitionScreen SplashAddress world_

            // return our world within the expected Either type, and we're off!
            Right world_