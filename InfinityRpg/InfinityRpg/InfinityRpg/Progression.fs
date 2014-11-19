namespace InfinityRpg
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open InfinityRpg
open InfinityRpg.Constants

[<RequireQualifiedAccess>]
module Progression =

    let private handleClickSaveGame _ world =
        let gameHierarchy = World.getGame' world
        World.writeGameToFile SaveFilePath gameHierarchy world
        (Cascade, world)

    let private addTitleScreen world =
        let world = snd <| World.addDissolveScreenFromGroupFile false DissolveData typeof<ScreenDispatcher>.Name TitleAddress TitleGroupFilePath world
        let world = World.subscribe4 GameAddress ClickTitleCreditsEventAddress (World.handleAsScreenTransition CreditsAddress) world
        let world = World.subscribe4 GameAddress ClickTitleNewGameEventAddress (World.handleAsScreenTransition GameplayAddress) world
        World.subscribe4 GameAddress ClickTitleExitEventAddress World.handleAsExit world

    let private addCreditsScreen world =
        let world = snd <| World.addDissolveScreenFromGroupFile false DissolveData typeof<ScreenDispatcher>.Name CreditsAddress CreditsGroupFilePath world
        World.subscribe4 GameAddress ClickCreditsBackEventAddress (World.handleAsScreenTransition TitleAddress) world

    let private addGameplayScreen world =
        let world = snd <| World.addDissolveScreenFromGroupFile true DissolveData typeof<GameplayDispatcher>.Name GameplayAddress HudFilePath world
        let world = World.subscribe4 GameAddress ClickHudBackEventAddress (World.handleAsScreenTransition TitleAddress) world
        World.subscribe4 GameAddress ClickHudSaveGameEventAddress handleClickSaveGame world

    let tryMakeInfinityRpgWorld sdlDeps userState =
        let componentFactory = InfinityRpgComponentFactory ()
        let optWorld = World.tryMake sdlDeps componentFactory GuiAndPhysicsAndGamePlay false userState
        match optWorld with
        | Right world ->
            let world = World.hintRenderingPackageUse GuiPackageName world
            let world = addTitleScreen world
            let world = addCreditsScreen world
            let world = addGameplayScreen world
            let (splashScreen, world) = World.addSplashScreen false NuSplashData typeof<ScreenDispatcher>.Name NuSplashAddress TitleAddress world
            let world = snd <| World.selectScreen NuSplashAddress splashScreen world
            Right world
        | Left _ as left -> left