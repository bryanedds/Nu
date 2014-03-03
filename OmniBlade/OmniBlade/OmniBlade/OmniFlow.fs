namespace OmniBlade
open System
open SDL2
open OpenTK
open TiledSharp
open Prime
open Nu
module OmniFlow =

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
    let TitleGroupName = Lun.make "Group"
    let TitleGroupAddress = TitleAddress @ [TitleGroupName]
    let TitleGroupFileName = "Assets/OmniBlade/Groups/Title.nugroup"
    let ClickTitleNewGameAddress = NuCore.straddrstr "Click" TitleGroupAddress "NewGame"
    let ClickTitleLoadGameAddress = NuCore.straddrstr "Click" TitleGroupAddress "LoadGame"
    let ClickTitleCreditsAddress = NuCore.straddrstr "Click" TitleGroupAddress "Credits"
    let ClickTitleExitAddress = NuCore.straddrstr "Click" TitleGroupAddress "Exit"

    // load game constants
    let LoadGameAddress = NuCore.addr "LoadGame"
    let LoadGameGroupName = Lun.make "Group"
    let LoadGameGroupAddress = LoadGameAddress @ [LoadGameGroupName]
    let LoadGameGroupFileName = "Assets/OmniBlade/Groups/LoadGame.nugroup"
    let ClickLoadGameBackAddress = NuCore.straddrstr "Click" LoadGameGroupAddress "Back"

    // credits constants
    let CreditsAddress = NuCore.addr "Credits"
    let CreditsGroupName = Lun.make "Group"
    let CreditsGroupAddress = CreditsAddress @ [CreditsGroupName]
    let CreditsGroupFileName = "Assets/OmniBlade/Groups/Credits.nugroup"
    let ClickCreditsBackAddress = NuCore.straddrstr "Click" CreditsGroupAddress "Back"

    // field constants
    let FieldAddress = NuCore.addr "Field"
    let FieldGroupName = Lun.make "Group"
    let FieldGroupAddress = FieldAddress @ [FieldGroupName]
    let FieldGroupFileName = "Assets/OmniBlade/Groups/Field.nugroup"
    let ClickFieldBackAddress = NuCore.straddrstr "Click" FieldGroupAddress "Back"

    // time constants
    let TimeAddress = NuCore.addr "Time"

    // now we have something worth explaining. This function adds the OmniBlade title screen to
    // the world.
    let addTitleScreen world =
        
        // this adds a dissolve screen from the specified file with the given parameters
        let world_ =
            World.addDissolveScreenFromFile
                TitleGroupFileName
                TitleGroupName
                IncomingTime
                OutgoingTime
                TitleAddress
                world
        
        // this subscribes to the event that is raised when the Title screen's NewGame button is
        // clicked, and handles the event by transitioning to the Field screen
        let world_ =
            World.subscribe
                ClickTitleNewGameAddress
                []
                (World.handleEventAsScreenTransition TitleAddress FieldAddress)
                world_
        
        // subscribes to the event that is raised when the Title screen's LoadGame button is
        // clicked, and handles the event by transitioning to the LoadGame screen
        let world_ =
            World.subscribe
                ClickTitleLoadGameAddress
                []
                (World.handleEventAsScreenTransition TitleAddress LoadGameAddress)
                world_
        
        // subscribes to the event that is raised when the Title screen's Credits button is
        // clicked, and handles the event by transitioning to the Credits screen
        let world_ =
            World.subscribe
                ClickTitleCreditsAddress
                []
                (World.handleEventAsScreenTransition TitleAddress CreditsAddress)
                world_
        
        // subscribes to the event that is raised when the Title screen's Exit button is clicked,
        // and handles the event by exiting the game
        World.subscribe ClickTitleExitAddress [] World.handleEventAsExit world_

    // pretty much the same as above, but for the LoadGame screen
    let addLoadGameScreen world =
        
        let world' =
            World.addDissolveScreenFromFile
                LoadGameGroupFileName
                LoadGameGroupName
                IncomingTime
                OutgoingTime
                LoadGameAddress
                world
        
        World.subscribe
            ClickLoadGameBackAddress
            []
            (World.handleEventAsScreenTransition LoadGameAddress TitleAddress)
            world'

    // and so on...
    let addCreditsScreen world =
        
        let world' =
            World.addDissolveScreenFromFile
                CreditsGroupFileName
                CreditsGroupName
                IncomingTime
                OutgoingTime
                CreditsAddress
                world
        
        World.subscribe
            ClickCreditsBackAddress
            []
            (World.handleEventAsScreenTransition CreditsAddress TitleAddress) world'

    // and so on.
    let addFieldScreen world =
        
        let world' =
            World.addDissolveScreenFromFile
                FieldGroupFileName
                FieldGroupName
                IncomingTime
                OutgoingTime
                FieldAddress
                world
        
        World.subscribe
            ClickFieldBackAddress
            []
            (World.handleEventAsScreenTransition FieldAddress TitleAddress)
            world'

    // here we create the OmniBlade world in a callback from the World.run function.
    let tryCreateOmniBladeWorld sdlDeps extData =

        // our custom game dispatcher here is OmniGameDispatcher
        let gameDispatcher = OmniGameDispatcher () :> obj

        // we use the World.tryCreateEmptyWorld as a convenience function to create an empty world
        // that we will transform to create the OmniBlade world.
        let optWorld = World.tryCreateEmptyWorld sdlDeps gameDispatcher extData
        match optWorld with
        | Left _ as left -> left
        | Right world ->
            
            // specify how to create the sprite used for the game's splash screen
            let splashScreenSprite =
                { SpriteAssetName = Lun.make "Image5"
                  PackageName = Lun.make "Default"
                  PackageFileName = "AssetGraph.xml" }

            // add to the world a splash screen that automatically transitions to the Title screen
            // when finished
            let world_ =
                World.addSplashScreenFromData
                    (World.transitionScreenHandler TitleAddress)
                    SplashAddress
                    IncomingTimeSplash
                    IdlingTime
                    OutgoingTimeSplash
                    splashScreenSprite
                    world
            
            // specify the song to play for the duration of the game
            let gameSong =
                { SongAssetName = Lun.make "Song"
                  PackageName = Lun.make "Default"
                  PackageFileName = "AssetGraph.xml" }
            
            // create a message to the audio system to play said song
            let playGameSong =
                PlaySong
                    { Song = gameSong
                      FadeOutCurrentSong = true }
            
            // add the message to the audio message queue
            let world_ = { world_ with AudioMessages = playGameSong :: world.AudioMessages }

            // add our UI screens to the world
            let world_ = addTitleScreen world_
            let world_ = addLoadGameScreen world_
            let world_ = addCreditsScreen world_
            let world_ = addFieldScreen world_
            
            // transition the world to splash screen
            let world_ = World.transitionScreen SplashAddress world_

            // return our world within the expected Either type, and we're off!
            Right world_