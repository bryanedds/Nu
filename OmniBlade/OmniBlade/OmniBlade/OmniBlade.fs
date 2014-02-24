namespace OmniBlade
open System
open SDL2
open OpenTK
open TiledSharp
open Nu
module OmniBladeModule =

    // transition literals
    let IncomingTimeSplash = 60
    let IncomingTime = 20
    let IdlingTime = 60
    let OutgoingTimeSplash = 40
    let OutgoingTime = 20

    // splash literals
    let SplashAddress = Core.addr "Splash"

    // title literals
    let TitleAddress = Core.addr "Title"
    let TitleGroupName = Lun.make "Group"
    let TitleGroupAddress = TitleAddress @ [TitleGroupName]
    let TitleGroupFileName = "Assets/OmniBlade/Groups/Title.nugroup"
    let ClickTitleGroupNewAddress = Core.straddrstr "Click" TitleGroupAddress "New"
    let ClickTitleGroupLoadAddress = Core.straddrstr "Click" TitleGroupAddress "Load"
    let ClickTitleGroupCreditsAddress = Core.straddrstr "Click" TitleGroupAddress "Credits"
    let ClickTitleGroupExitAddress = Core.straddrstr "Click" TitleGroupAddress "Exit"

    // load literals
    let LoadAddress = Core.addr "Load"
    let LoadGroupName = Lun.make "Group"
    let LoadGroupAddress = LoadAddress @ [LoadGroupName]
    let LoadGroupFileName = "Assets/OmniBlade/Groups/Load.nugroup"
    let ClickLoadGroupBackAddress = Core.straddrstr "Click" LoadGroupAddress "Back"

    // credits literals
    let CreditsAddress = Core.addr "Credits"
    let CreditsGroupName = Lun.make "Group"
    let CreditsGroupAddress = CreditsAddress @ [CreditsGroupName]
    let CreditsGroupFileName = "Assets/OmniBlade/Groups/Credits.nugroup"
    let ClickCreditsGroupBackAddress = Core.straddrstr "Click" CreditsGroupAddress "Back"

    // field literals
    let FieldAddress = Core.addr "Field"
    let FieldGroupName = Lun.make "Group"
    let FieldGroupAddress = FieldAddress @ [FieldGroupName]
    let FieldGroupFileName = "Assets/OmniBlade/Groups/Field.nugroup"
    let ClickFieldGroupBackAddress = Core.straddrstr "Click" FieldGroupAddress "Back"

    // time literals
    let TimeAddress = Core.addr "Time"

    let createTitleScreen world =
        let world_ = WorldModule.createDissolveScreenFromFile TitleGroupFileName TitleGroupName IncomingTime OutgoingTime TitleAddress world
        let world_ = WorldModule.subscribe ClickTitleGroupNewAddress [] (WorldModule.handleEventAsScreenTransition TitleAddress FieldAddress) world_
        let world_ = WorldModule.subscribe ClickTitleGroupLoadAddress [] (WorldModule.handleEventAsScreenTransition TitleAddress LoadAddress) world_
        let world_ = WorldModule.subscribe ClickTitleGroupCreditsAddress [] (WorldModule.handleEventAsScreenTransition TitleAddress CreditsAddress) world_
        WorldModule.subscribe ClickTitleGroupExitAddress [] WorldModule.handleEventAsExit world_

    let createLoadScreen world =
        let world' = WorldModule.createDissolveScreenFromFile LoadGroupFileName LoadGroupName IncomingTime OutgoingTime LoadAddress world
        WorldModule.subscribe ClickLoadGroupBackAddress [] (WorldModule.handleEventAsScreenTransition LoadAddress TitleAddress) world'

    let createCreditsScreen world =
        let world' = WorldModule.createDissolveScreenFromFile CreditsGroupFileName CreditsGroupName IncomingTime OutgoingTime CreditsAddress world
        WorldModule.subscribe ClickCreditsGroupBackAddress [] (WorldModule.handleEventAsScreenTransition CreditsAddress TitleAddress) world'

    let createFieldScreen world =
        let world' = WorldModule.createDissolveScreenFromFile FieldGroupFileName FieldGroupName IncomingTime OutgoingTime FieldAddress world
        WorldModule.subscribe ClickFieldGroupBackAddress [] (WorldModule.handleEventAsScreenTransition FieldAddress TitleAddress) world'

    let tryCreateOmniBladeWorld sdlDeps extData =
        let gameDispatcher = OmniGameDispatcher () :> obj
        let optWorld = WorldModule.tryCreateEmptyWorld sdlDeps gameDispatcher extData
        match optWorld with
        | Left _ as left -> left
        | Right world ->
            let playSong = PlaySong { Song = { SongAssetName = Lun.make "Song"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }; FadeOutCurrentSong = true }
            let splashScreenSprite = { SpriteAssetName = Lun.make "Image5"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
            let world_ = { world with AudioMessages = playSong :: world.AudioMessages }
            let world_ = WorldModule.addSplashScreen (WorldModule.transitionScreenHandler TitleAddress) SplashAddress IncomingTimeSplash IdlingTime OutgoingTimeSplash splashScreenSprite world_
            let world_ = createTitleScreen world_
            let world_ = createLoadScreen world_
            let world_ = createCreditsScreen world_
            let world_ = createFieldScreen world_
            let world_ = WorldModule.transitionScreen SplashAddress world_
            Right world_