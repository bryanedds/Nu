namespace InfinityRpg
open Prime
open Nu
open Nu.Constants
open InfinityRpg
open InfinityRpg.Constants

[<RequireQualifiedAccess>]
module Progression =

    let private addTitleScreen world =
        let world = snd <| World.addDissolveScreenFromFile typeof<ScreenDispatcher>.Name TitleGroupFilePath IncomingTime OutgoingTime TitleAddress world
        let world = World.subscribe4 ClickTitleCreditsEventAddress Address.empty (World.handleAsScreenTransition CreditsAddress) world
        World.subscribe4 ClickTitleExitEventAddress Address.empty World.handleAsExit world

    let private addCreditsScreen world =
        let world = snd <| World.addDissolveScreenFromFile typeof<ScreenDispatcher>.Name CreditsGroupFilePath IncomingTime OutgoingTime CreditsAddress world
        World.subscribe4 ClickCreditsBackEventAddress Address.empty (World.handleAsScreenTransition TitleAddress) world

    let tryMakeInfinityRpgWorld sdlDeps userState =
        let componentFactory = InfinityRpgComponentFactory ()
        let optWorld = World.tryMake sdlDeps componentFactory UIAndPhysicsAndGamePlay false userState
        match optWorld with
        | Right world ->
            let world = World.hintRenderingPackageUse UIPackageName world
            let world = addTitleScreen world
            let world = addCreditsScreen world
            let splashScreenImage = { ImageAssetName = "Image5"; PackageName = DefaultPackageName }
            let (splashScreen, world) = World.addSplashScreenFromData TitleAddress SplashAddress typeof<ScreenDispatcher>.Name SplashIncomingTime SplashIdlingTime SplashOutgoingTime splashScreenImage world
            let world = snd <| World.selectScreen SplashAddress splashScreen world
            Right world
        | Left _ as left -> left