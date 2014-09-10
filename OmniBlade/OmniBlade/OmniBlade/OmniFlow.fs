namespace OmniBlade
open System
open Prime
open Nu
open Nu.NuConstants
open OmniBlade
open OmniBlade.OmniConstants
module OmniFlow =

    type OmniComponentFactory () =

        interface IUserComponentFactory with

            member dispatcher.MakeUserDispatchers () =
                Map.ofList
                    [typeof<BattleGroupDispatcher>.Name, BattleGroupDispatcher () :> obj
                     typeof<FieldGroupDispatcher>.Name, FieldGroupDispatcher () :> obj
                     typeof<OmniBladeDispatcher>.Name, OmniBladeDispatcher () :> obj]

            member dispatcher.MakeUserFacets () =
                Map.empty

    let addTitleScreen world =
        let world = snd <| World.addDissolveScreenFromFile typeof<ScreenDispatcher>.Name TitleGroupFileName (Address.last TitleGroupAddress) IncomingTime OutgoingTime TitleAddress world
        let world = World.subscribe4 ClickTitleNewGameEvent Address.empty (ScreenTransitionSub FieldAddress) world
        let world = World.subscribe4 ClickTitleLoadGameEvent Address.empty (ScreenTransitionSub LoadGameAddress) world
        let world = World.subscribe4 ClickTitleCreditsEvent Address.empty (ScreenTransitionSub CreditsAddress) world
        World.subscribe4 ClickTitleExitEvent Address.empty ExitSub world

    let addLoadGameScreen world =
        let world = snd <| World.addDissolveScreenFromFile typeof<ScreenDispatcher>.Name LoadGameGroupFileName (Address.last LoadGameGroupAddress) IncomingTime OutgoingTime LoadGameAddress world
        World.subscribe4 ClickLoadGameBackEvent Address.empty (ScreenTransitionSub TitleAddress) world

    let addCreditsScreen world =
        let world = snd <| World.addDissolveScreenFromFile typeof<ScreenDispatcher>.Name CreditsGroupFileName (Address.last CreditsGroupAddress) IncomingTime OutgoingTime CreditsAddress world
        World.subscribe4 ClickCreditsBackEvent Address.empty (ScreenTransitionSub TitleAddress) world

    let addFieldScreen world =
        let world = snd <| World.addDissolveScreenFromFile typeof<ScreenDispatcher>.Name FieldGroupFileName (Address.last FieldGroupAddress) IncomingTime OutgoingTime FieldAddress world
        World.subscribe4 ClickFieldBackEvent Address.empty (ScreenTransitionSub TitleAddress) world

    let tryMakeOmniBladeWorld sdlDeps extData =
        let omniComponentFactory = OmniComponentFactory ()
        let optWorld = World.tryMake sdlDeps omniComponentFactory GuiAndPhysics false extData
        match optWorld with
        | Right world ->
            let world = World.hintRenderingPackageUse GuiPackageName world
            let world = World.playSong GameSong 1.0f DefaultTimeToFadeOutSongMs world
            let splashScreenImage = { ImageAssetName = "Image5"; PackageName = DefaultPackageName }
            let (splashScreen, world) = World.addSplashScreenFromData TitleAddress SplashAddress typeof<ScreenDispatcher>.Name IncomingTimeSplash IdlingTime OutgoingTimeSplash splashScreenImage world
            let world = addTitleScreen world
            let world = addLoadGameScreen world
            let world = addCreditsScreen world
            let world = addFieldScreen world
            let world = snd <| World.selectScreen SplashAddress splashScreen world
            Right world
        | Left _ as left -> left