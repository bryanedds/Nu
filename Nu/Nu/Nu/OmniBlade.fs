namespace Nu
open System
open SDL2
open OpenTK
open TiledSharp
open Nu
open Nu.Core
open Nu.Constants
open Nu.Sdl
open Nu.Audio
open Nu.Rendering
open Nu.Physics
open Nu.Metadata
open Nu.EntityModule
open Nu.GroupModule
open Nu.ScreenModule
open Nu.GameModule
open Nu.WorldModule
module OmniBlade =

    // transition literals
    let IncomingTimeSplash = 60
    let IncomingTime = 20
    let IdlingTime = 60
    let OutgoingTimeSplash = 40
    let OutgoingTime = 20

    // splash literals
    let SplashAddress = addr "Splash"

    // title literals
    let TitleAddress = addr "Title"
    let TitleGroupName = Lun.make "Group"
    let TitleGroupAddress = TitleAddress @ [TitleGroupName]
    let TitleGroupFileName = "Assets/OmniBlade/Groups/Title.nugroup"
    let ClickTitleGroupNewAddress = straddrstr "Click" TitleGroupAddress "New"
    let ClickTitleGroupLoadAddress = straddrstr "Click" TitleGroupAddress "Load"
    let ClickTitleGroupCreditsAddress = straddrstr "Click" TitleGroupAddress "Credits"
    let ClickTitleGroupExitAddress = straddrstr "Click" TitleGroupAddress "Exit"

    // load literals
    let LoadAddress = addr "Load"
    let LoadGroupName = Lun.make "Group"
    let LoadGroupAddress = LoadAddress @ [LoadGroupName]
    let LoadGroupFileName = "Assets/OmniBlade/Groups/Load.nugroup"
    let ClickLoadGroupBackAddress = straddrstr "Click" LoadGroupAddress "Back"

    // credits literals
    let CreditsAddress = addr "Credits"
    let CreditsGroupName = Lun.make "Group"
    let CreditsGroupAddress = CreditsAddress @ [CreditsGroupName]
    let CreditsGroupFileName = "Assets/OmniBlade/Groups/Credits.nugroup"
    let ClickCreditsGroupBackAddress = straddrstr "Click" CreditsGroupAddress "Back"

    // field literals
    let FieldAddress = addr "Field"
    let FieldGroupName = Lun.make "Group"
    let FieldGroupAddress = FieldAddress @ [FieldGroupName]
    let FieldGroupFileName = "Assets/OmniBlade/Groups/Field.nugroup"
    let ClickFieldGroupBackAddress = straddrstr "Click" FieldGroupAddress "Back"

    // time literals
    let TimeAddress = addr "Time"

    let createTitleScreen world =
        let world_ = createDissolveScreenFromFile TitleGroupFileName TitleGroupName IncomingTime OutgoingTime TitleAddress world
        let world_ = subscribe ClickTitleGroupNewAddress [] (handleEventAsScreenTransition TitleAddress FieldAddress) world_
        let world_ = subscribe ClickTitleGroupLoadAddress [] (handleEventAsScreenTransition TitleAddress LoadAddress) world_
        let world_ = subscribe ClickTitleGroupCreditsAddress [] (handleEventAsScreenTransition TitleAddress CreditsAddress) world_
        subscribe ClickTitleGroupExitAddress [] handleEventAsExit world_

    let createLoadScreen world =
        let world' = createDissolveScreenFromFile LoadGroupFileName LoadGroupName IncomingTime OutgoingTime LoadAddress world
        subscribe ClickLoadGroupBackAddress [] (handleEventAsScreenTransition LoadAddress TitleAddress) world'

    let createCreditsScreen world =
        let world' = createDissolveScreenFromFile CreditsGroupFileName CreditsGroupName IncomingTime OutgoingTime CreditsAddress world
        subscribe ClickCreditsGroupBackAddress [] (handleEventAsScreenTransition CreditsAddress TitleAddress) world'

    let createFieldScreen world =
        let world' = createDissolveScreenFromFile FieldGroupFileName FieldGroupName IncomingTime OutgoingTime FieldAddress world
        subscribe ClickFieldGroupBackAddress [] (handleEventAsScreenTransition FieldAddress TitleAddress) world'

    let tryCreateOmniBladeWorld sdlDeps extData =
        let optWorld = tryCreateEmptyWorld sdlDeps extData
        match optWorld with
        | Left _ as left -> left
        | Right world ->
            let playSong = PlaySong { Song = { SongAssetName = Lun.make "Song"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }; FadeOutCurrentSong = true }
            let splashScreenSprite = { SpriteAssetName = Lun.make "Image5"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
            let world_ = { world with AudioMessages = playSong :: world.AudioMessages }
            let world_ = addSplashScreen (transitionScreenHandler TitleAddress) SplashAddress IncomingTimeSplash IdlingTime OutgoingTimeSplash splashScreenSprite world_
            let world_ = createTitleScreen world_
            let world_ = createLoadScreen world_
            let world_ = createCreditsScreen world_
            let world_ = createFieldScreen world_
            let world_ = transitionScreen SplashAddress world_
            Right world_