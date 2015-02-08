namespace InfinityRpg
open System
open System.IO
open OpenTK
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open Nu.Observation
open InfinityRpg
open InfinityRpg.Constants

[<AutoOpen>]
module InfinityRpgModule =

    type InfinityDispatcher () =
        inherit GameDispatcher ()

        static let handleAsScreenTransitionToGameplay shallLoadGame event world =
            let world = Gameplay.SetShallLoadGame shallLoadGame world
            World.handleAsScreenTransition Gameplay event world

        static let createTitle world =
            let world = snd <| World.createDissolveScreenFromGroupFile false DissolveData typeof<ScreenDispatcher>.Name TitleGroupFilePath (Some TitleName) world
            let world = World.subscribe4 (World.handleAsScreenTransition Credits) (ClickEventAddress ->>- TitleCredits.EntityAddress) Game world
            let world = World.subscribe4 (handleAsScreenTransitionToGameplay false) (ClickEventAddress ->>- TitleNewGame.EntityAddress) Game world
            let world = World.subscribe4 (handleAsScreenTransitionToGameplay true) (ClickEventAddress ->>- TitleLoadGame.EntityAddress) Game world
            World.subscribe4 World.handleAsExit (ClickEventAddress ->>- TitleExit.EntityAddress) Game world

        static let createCredits world =
            let world = snd <| World.createDissolveScreenFromGroupFile false DissolveData typeof<ScreenDispatcher>.Name CreditsGroupFilePath (Some CreditsName) world
            World.subscribe4 (World.handleAsScreenTransition Title) (ClickEventAddress ->>- CreditsBack.EntityAddress) Game world

        static let createGameplay world =
            let world = snd <| World.createDissolveScreenFromGroupFile true DissolveData typeof<GameplayDispatcher>.Name HudGroupFilePath (Some GameplayName) world
            let world = Hud.SetPersistent false world // do not persist the Hud
            World.subscribe4 (World.handleAsScreenTransition Title) (ClickEventAddress ->>- HudBack.EntityAddress) Game world

        override dispatcher.Register _ world =
            let world = World.hintRenderPackageUse GuiPackageName world
            let world = World.hintRenderPackageUse GameplayPackageName world
            let world = createTitle world
            let world = createCredits world
            let world = createGameplay world
            let (splash, world) = World.createSplashScreen false NuSplashData typeof<ScreenDispatcher>.Name Title (Some NuSplashName) world
            World.selectScreen splash world