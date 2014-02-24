namespace OmniBlade
open System
open SDL2
open OpenTK
open TiledSharp
open Nu
module OmniFlow =

    // transition literals
    let IncomingTimeSplash = 60
    let IncomingTime = 20
    let IdlingTime = 60
    let OutgoingTimeSplash = 40
    let OutgoingTime = 20

    // splash literals
    let SplashAddress = NuCore.addr "Splash"

    // title literals
    let TitleAddress = NuCore.addr "Title"
    let TitleGroupName = Lun.make "Group"
    let TitleGroupAddress = TitleAddress @ [TitleGroupName]
    let TitleGroupFileName = "Assets/OmniBlade/Groups/Title.nugroup"
    let ClickTitleGroupNewAddress = NuCore.straddrstr "Click" TitleGroupAddress "New"
    let ClickTitleGroupLoadAddress = NuCore.straddrstr "Click" TitleGroupAddress "Load"
    let ClickTitleGroupCreditsAddress = NuCore.straddrstr "Click" TitleGroupAddress "Credits"
    let ClickTitleGroupExitAddress = NuCore.straddrstr "Click" TitleGroupAddress "Exit"

    // load literals
    let LoadAddress = NuCore.addr "Load"
    let LoadGroupName = Lun.make "Group"
    let LoadGroupAddress = LoadAddress @ [LoadGroupName]
    let LoadGroupFileName = "Assets/OmniBlade/Groups/Load.nugroup"
    let ClickLoadGroupBackAddress = NuCore.straddrstr "Click" LoadGroupAddress "Back"

    // credits literals
    let CreditsAddress = NuCore.addr "Credits"
    let CreditsGroupName = Lun.make "Group"
    let CreditsGroupAddress = CreditsAddress @ [CreditsGroupName]
    let CreditsGroupFileName = "Assets/OmniBlade/Groups/Credits.nugroup"
    let ClickCreditsGroupBackAddress = NuCore.straddrstr "Click" CreditsGroupAddress "Back"

    // field literals
    let FieldAddress = NuCore.addr "Field"
    let FieldGroupName = Lun.make "Group"
    let FieldGroupAddress = FieldAddress @ [FieldGroupName]
    let FieldGroupFileName = "Assets/OmniBlade/Groups/Field.nugroup"
    let ClickFieldGroupBackAddress = NuCore.straddrstr "Click" FieldGroupAddress "Back"

    // time literals
    let TimeAddress = NuCore.addr "Time"

    let addTitleScreen world =
        let world_ = World.addDissolveScreenFromFile TitleGroupFileName TitleGroupName IncomingTime OutgoingTime TitleAddress world
        let world_ = World.subscribe ClickTitleGroupNewAddress [] (World.handleEventAsScreenTransition TitleAddress FieldAddress) world_
        let world_ = World.subscribe ClickTitleGroupLoadAddress [] (World.handleEventAsScreenTransition TitleAddress LoadAddress) world_
        let world_ = World.subscribe ClickTitleGroupCreditsAddress [] (World.handleEventAsScreenTransition TitleAddress CreditsAddress) world_
        World.subscribe ClickTitleGroupExitAddress [] World.handleEventAsExit world_

    let addLoadScreen world =
        let world' = World.addDissolveScreenFromFile LoadGroupFileName LoadGroupName IncomingTime OutgoingTime LoadAddress world
        World.subscribe ClickLoadGroupBackAddress [] (World.handleEventAsScreenTransition LoadAddress TitleAddress) world'

    let addCreditsScreen world =
        let world' = World.addDissolveScreenFromFile CreditsGroupFileName CreditsGroupName IncomingTime OutgoingTime CreditsAddress world
        World.subscribe ClickCreditsGroupBackAddress [] (World.handleEventAsScreenTransition CreditsAddress TitleAddress) world'

    let addFieldScreen world =
        let world' = World.addDissolveScreenFromFile FieldGroupFileName FieldGroupName IncomingTime OutgoingTime FieldAddress world
        World.subscribe ClickFieldGroupBackAddress [] (World.handleEventAsScreenTransition FieldAddress TitleAddress) world'

    let tryCreateOmniBladeWorld sdlDeps extData =
        let gameDispatcher = OmniGameDispatcher () :> obj
        let optWorld = World.tryCreateEmptyWorld sdlDeps gameDispatcher extData
        match optWorld with
        | Left _ as left -> left
        | Right world ->
            let playSong = PlaySong { Song = { SongAssetName = Lun.make "Song"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }; FadeOutCurrentSong = true }
            let splashScreenSprite = { SpriteAssetName = Lun.make "Image5"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
            let world_ = { world with AudioMessages = playSong :: world.AudioMessages }
            let world_ = World.addSplashScreenFromData (World.transitionScreenHandler TitleAddress) SplashAddress IncomingTimeSplash IdlingTime OutgoingTimeSplash splashScreenSprite world_
            let world_ = addTitleScreen world_
            let world_ = addLoadScreen world_
            let world_ = addCreditsScreen world_
            let world_ = addFieldScreen world_
            let world_ = World.transitionScreen SplashAddress world_
            Right world_