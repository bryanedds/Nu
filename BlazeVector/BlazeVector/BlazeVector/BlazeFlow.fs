namespace BlazeVector
open System
open SDL2
open OpenTK
open TiledSharp
open Prime
open Nu
module BlazeFlow =

    // transition constants. These, and the following constants, will be explained in depth later.
    // Just scan over them for now, or look at them in the debugger on your own.
    let IncomingTimeSplash = 60
    let IncomingTime = 20
    let IdlingTime = 60
    let OutgoingTimeSplash = 40
    let OutgoingTime = 20

    // splash constants
    let SplashAddress = NuCore.addr "Splash"

    // title constants
    let TitleAddress = NuCore.addr "Title"
    let TitleGroupName = "Group"
    let TitleGroupAddress = TitleAddress @ [TitleGroupName]
    let TitleGroupFileName = "Assets/BlazeVector/Groups/Title.nugroup"
    let ClickTitleNewGameEvent = NuCore.straddrstr "Click" TitleGroupAddress "NewGame"
    let ClickTitleLoadGameEvent = NuCore.straddrstr "Click" TitleGroupAddress "LoadGame"
    let ClickTitleCreditsEvent = NuCore.straddrstr "Click" TitleGroupAddress "Credits"
    let ClickTitleExitEvent = NuCore.straddrstr "Click" TitleGroupAddress "Exit"

    // load game constants
    let LoadGameAddress = NuCore.addr "LoadGame"
    let LoadGameGroupName = "Group"
    let LoadGameGroupAddress = LoadGameAddress @ [LoadGameGroupName]
    let LoadGameGroupFileName = "Assets/BlazeVector/Groups/LoadGame.nugroup"
    let ClickLoadGameBackEvent = NuCore.straddrstr "Click" LoadGameGroupAddress "Back"

    // credits constants
    let CreditsAddress = NuCore.addr "Credits"
    let CreditsGroupName = "Group"
    let CreditsGroupAddress = CreditsAddress @ [CreditsGroupName]
    let CreditsGroupFileName = "Assets/BlazeVector/Groups/Credits.nugroup"
    let ClickCreditsBackEvent = NuCore.straddrstr "Click" CreditsGroupAddress "Back"

    // stage constants
    let StageAddress = NuCore.addr "Stage"
    let StageGroupName = "Group"
    let StageGroupAddress = StageAddress @ [StageGroupName]
    let StageGroupFileName = "Assets/BlazeVector/Groups/Stage.nugroup"
    let ClickStageBackEvent = NuCore.straddrstr "Click" StageGroupAddress "Back"

    // now we have something worth explaining. This function adds the BlazeVector title screen to
    // the world.
    let addTitleScreen world =
        
        // this adds a dissolve screen from the specified file with the given parameters. Note that
        // the 'seal' parameter is set to true as no XFields will be added to the title screen.
        let world_ = World.addDissolveScreenFromFile TitleGroupFileName TitleGroupName IncomingTime OutgoingTime TitleAddress true world
        
        // this subscribes to the event that is raised when the Title screen's NewGame button is
        // clicked, and handles the event by transitioning to the Stage screen
        let world_ = World.subscribe ClickTitleNewGameEvent [] (ScreenTransitionSub StageAddress) world_
        
        // subscribes to the event that is raised when the Title screen's LoadGame button is
        // clicked, and handles the event by transitioning to the LoadGame screen
        let world_ = World.subscribe ClickTitleLoadGameEvent [] (ScreenTransitionSub LoadGameAddress) world_
        
        // subscribes to the event that is raised when the Title screen's Credits button is
        // clicked, and handles the event by transitioning to the Credits screen
        let world_ = World.subscribe ClickTitleCreditsEvent [] (ScreenTransitionSub CreditsAddress) world_
        
        // subscribes to the event that is raised when the Title screen's Exit button is clicked,
        // and handles the event by exiting the game
        World.subscribe ClickTitleExitEvent [] ExitSub world_

    // pretty much the same as above, but for the LoadGame screen
    let addLoadGameScreen world =
        let world' = World.addDissolveScreenFromFile LoadGameGroupFileName LoadGameGroupName IncomingTime OutgoingTime LoadGameAddress true world
        World.subscribe ClickLoadGameBackEvent [] (ScreenTransitionSub TitleAddress) world'

    // and so on...
    let addCreditsScreen world =
        let world' = World.addDissolveScreenFromFile CreditsGroupFileName CreditsGroupName IncomingTime OutgoingTime CreditsAddress true world
        World.subscribe ClickCreditsBackEvent [] (ScreenTransitionSub TitleAddress) world'

    // and so on.
    let addStageScreen world =
        let world' = World.addDissolveScreenFromFile StageGroupFileName StageGroupName IncomingTime OutgoingTime StageAddress true world
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
            let hintRenderPackageUse = HintRenderingPackageUse { FileName = "AssetGraph.xml"; PackageName = "BlazeGui"; HRPU = () } 
            let world_ = { world with RenderMessages = hintRenderPackageUse :: world.RenderMessages }
            
            // specify a song to play for the duration of the game via the audio message system
            let gameSong = { SongAssetName = "Song"; PackageName = "Default"; PackageFileName = "AssetGraph.xml" }
            let playSongMessage = PlaySong { Song = gameSong; FadeOutCurrentSong = true }
            let world_ = { world_ with AudioMessages = playSongMessage :: world_.AudioMessages }

            // add to the world a splash screen that automatically transitions to the Title screen
            let splashScreenSprite = { SpriteAssetName = "Image5"; PackageName = "Default"; PackageFileName = "AssetGraph.xml" }
            let world_ = World.addSplashScreenFromData (ScreenTransitionSub TitleAddress) SplashAddress IncomingTimeSplash IdlingTime OutgoingTimeSplash splashScreenSprite true world_

            // add our UI screens to the world
            let world_ = addTitleScreen world_
            let world_ = addLoadGameScreen world_
            let world_ = addCreditsScreen world_
            let world_ = addStageScreen world_
            
            // transition the world to splash screen
            let world_ = World.transitionScreen SplashAddress world_

            // return our world within the expected Either type, and we're off!
            Right world_