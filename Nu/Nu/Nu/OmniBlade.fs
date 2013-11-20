namespace Nu
open System
open SDL2
open OpenTK
open TiledSharp
open Nu.Core
open Nu.Constants
open Nu.Sdl
open Nu.Audio
open Nu.Rendering
open Nu.Physics
open Nu.Metadata
open Nu.Entities
open Nu.Groups
open Nu.Screens
open Nu.Games
open Nu.Sim
module OmniBlade =

    let IncomingTime = 45
    let IdlingTime = 60
    let OutgoingTime = 30

    // splash literals
    let SplashAddress = addr "splash"

    // title literals
    let TitleAddress = addr "title"
    let TitleGroupName = Lun.make "group"
    let TitleGroupAddress = TitleAddress @ [TitleGroupName]
    let TitleGroupFileName = "Title.nugroup"
    let ClickTitleGroupLoadAddress = straddrstr "click" TitleGroupAddress "load"
    let ClickTitleGroupCreditsAddress = straddrstr "click" TitleGroupAddress "credits"
    let ClickTitleGroupExitAddress = straddrstr "click" TitleGroupAddress "exit"

    // load literals
    let LoadAddress = addr "load"
    let LoadGroupName = Lun.make "group"
    let LoadGroupAddress = LoadAddress @ [LoadGroupName]
    let LoadGroupFileName = "Load.nugroup"
    let ClickLoadGroupBackAddress = straddrstr "click" LoadGroupAddress "back"

    // credits literals
    let CreditsAddress = addr "credits"
    let CreditsGroupName = Lun.make "group"
    let CreditsGroupAddress = CreditsAddress @ [CreditsGroupName]
    let CreditsGroupFileName = "Credits.nugroup"
    let ClickCreditsGroupBackAddress = straddrstr "click" CreditsGroupAddress "back"

    // omni literals
    let OmniAddress = addr "omni"

    let createTitleScreen world =
        let world' = createDissolveScreenFromFile TitleGroupFileName TitleGroupName IncomingTime OutgoingTime TitleAddress world
        let world'' = subscribe ClickTitleGroupLoadAddress [] (handleEventAsScreenTransition TitleAddress LoadAddress) world'
        let world''' = subscribe ClickTitleGroupCreditsAddress [] (handleEventAsScreenTransition TitleAddress CreditsAddress) world''
        subscribe ClickTitleGroupExitAddress [] handleEventAsExit world'''

    let createLoadScreen world =
        let world' = createDissolveScreenFromFile LoadGroupFileName LoadGroupName IncomingTime OutgoingTime LoadAddress world
        subscribe ClickLoadGroupBackAddress [] (handleEventAsScreenTransition LoadAddress TitleAddress) world'

    let createCreditsScreen world =
        let world' = createDissolveScreenFromFile CreditsGroupFileName CreditsGroupName IncomingTime OutgoingTime CreditsAddress world
        subscribe ClickCreditsGroupBackAddress [] (handleEventAsScreenTransition CreditsAddress TitleAddress) world'

    let tryCreateOmniBladeWorld sdlDeps extData =
        let optWorld = tryCreateEmptyWorld sdlDeps extData
        match optWorld with
        | Left _ as left -> left
        | Right world_ ->
            let splashScreenSprite = { SpriteAssetName = Lun.make "Image5"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
            let world_ = addSplashScreen (transitionScreenHandler TitleAddress) SplashAddress IncomingTime IdlingTime OutgoingTime splashScreenSprite world_
            let world_ = createTitleScreen world_
            let world_ = createLoadScreen world_
            let world_ = createCreditsScreen world_
            let world_ = transitionScreen SplashAddress world_
            Right world_