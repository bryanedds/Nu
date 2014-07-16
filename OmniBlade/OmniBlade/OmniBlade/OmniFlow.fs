namespace OmniBlade
open System
open Prime
open Nu
open Nu.NuConstants
open OmniBlade
open OmniBlade.OmniConstants
module OmniFlow =

    let addTitleScreen world =
        let world = World.addDissolveScreenFromFile typeof<ScreenDispatcher>.Name TitleGroupFileName (List.last TitleGroupAddress) IncomingTime OutgoingTime TitleAddress world
        let world = World.subscribe4 ClickTitleNewGameEvent [] (ScreenTransitionSub FieldAddress) world
        let world = World.subscribe4 ClickTitleLoadGameEvent [] (ScreenTransitionSub LoadGameAddress) world
        let world = World.subscribe4 ClickTitleCreditsEvent [] (ScreenTransitionSub CreditsAddress) world
        World.subscribe4 ClickTitleExitEvent [] ExitSub world

    let addLoadGameScreen world =
        let world = World.addDissolveScreenFromFile typeof<ScreenDispatcher>.Name LoadGameGroupFileName (List.last LoadGameGroupAddress) IncomingTime OutgoingTime LoadGameAddress world
        World.subscribe4 ClickLoadGameBackEvent [] (ScreenTransitionSub TitleAddress) world

    let addCreditsScreen world =
        let world = World.addDissolveScreenFromFile typeof<ScreenDispatcher>.Name CreditsGroupFileName (List.last CreditsGroupAddress) IncomingTime OutgoingTime CreditsAddress world
        World.subscribe4 ClickCreditsBackEvent [] (ScreenTransitionSub TitleAddress) world

    let addFieldScreen world =
        let world = World.addDissolveScreenFromFile typeof<ScreenDispatcher>.Name FieldGroupFileName (List.last FieldGroupAddress) IncomingTime OutgoingTime FieldAddress world
        World.subscribe4 ClickFieldBackEvent [] (ScreenTransitionSub TitleAddress) world

    let tryMakeOmniBladeWorld sdlDeps extData =
        let gameDispatcher = OmniBladeDispatcher () :> obj
        let optWorld = World.tryMakeEmpty sdlDeps gameDispatcher GuiAndPhysics extData
        match optWorld with
        | Left _ as left -> left
        | Right world ->
            let world = World.hintRenderingPackageUse AssetGraphFileName GuiPackageName world
            let world = World.playSong GameSong 1.0f DefaultTimeToFadeOutSongMs world
            let splashScreenSprite = { SpriteAssetName = "Image5"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName }
            let world = World.addSplashScreenFromData TitleAddress SplashAddress typeof<ScreenDispatcher>.Name IncomingTimeSplash IdlingTime OutgoingTimeSplash splashScreenSprite world
            let world = addTitleScreen world
            let world = addLoadGameScreen world
            let world = addCreditsScreen world
            let world = addFieldScreen world
            let world = World.selectScreen SplashAddress world
            Right world