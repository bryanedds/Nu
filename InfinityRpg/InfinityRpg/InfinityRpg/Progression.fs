namespace InfinityRpg
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open InfinityRpg
open InfinityRpg.Constants

[<RequireQualifiedAccess>]
module Progression =

    let private addTitleScreen world =
        let world = snd <| World.addDissolveScreenFromFile false typeof<ScreenDispatcher>.Name TitleAddress TitleGroupFilePath DissolveData world
        let world = World.subscribe4 GameAddress ClickTitleCreditsEventAddress (World.handleAsScreenTransition CreditsAddress) world
        World.subscribe4 GameAddress ClickTitleExitEventAddress World.handleAsExit world

    let private addCreditsScreen world =
        let world = snd <| World.addDissolveScreenFromFile false typeof<ScreenDispatcher>.Name CreditsAddress CreditsGroupFilePath DissolveData world
        World.subscribe4 GameAddress ClickCreditsBackEventAddress (World.handleAsScreenTransition TitleAddress) world

    let tryMakeInfinityRpgWorld sdlDeps userState =
        let componentFactory = InfinityRpgComponentFactory ()
        let optWorld = World.tryMake sdlDeps componentFactory GuiAndPhysicsAndGamePlay false userState
        match optWorld with
        | Right world ->
            let world = World.hintRenderingPackageUse GuiPackageName world
            let world = addTitleScreen world
            let world = addCreditsScreen world
            let (splashScreen, world) = World.addSplashScreenFromData false typeof<ScreenDispatcher>.Name NuSplashAddress TitleAddress NuSplashData world
            let world = snd <| World.selectScreen NuSplashAddress splashScreen world
            Right world
        | Left _ as left -> left