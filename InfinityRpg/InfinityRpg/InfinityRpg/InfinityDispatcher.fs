namespace InfinityRpg
open System
open System.IO
open OpenTK
open Prime
open Nu
open Nu.Observation
open InfinityRpg

type InfinityDispatcher () =
    inherit GameDispatcher ()

    static let handleAsScreenTransitionToGameplay shallLoadGame event world =
        let world = Proxies.Gameplay.SetShallLoadGame shallLoadGame world
        World.handleAsScreenTransition Proxies.Gameplay event world

    static let createTitle world =
        let world = snd <| World.createDissolveScreenFromGroupFile false Constants.InfinityRpg.DissolveData typeof<ScreenDispatcher>.Name Constants.FilePaths.TitleGroupFilePath (Some Proxies.TitleName) world
        let world = World.subscribe4 (World.handleAsScreenTransition Proxies.Credits) (EventAddresses.Click ->>- Proxies.TitleCredits.EntityAddress) Proxies.Game world
        let world = World.subscribe4 (handleAsScreenTransitionToGameplay false) (EventAddresses.Click ->>- Proxies.TitleNewGame.EntityAddress) Proxies.Game world
        let world = World.subscribe4 (handleAsScreenTransitionToGameplay true) (EventAddresses.Click ->>- Proxies.TitleLoadGame.EntityAddress) Proxies.Game world
        World.subscribe4 World.handleAsExit (EventAddresses.Click ->>- Proxies.TitleExit.EntityAddress) Proxies.Game world

    static let createCredits world =
        let world = snd <| World.createDissolveScreenFromGroupFile false Constants.InfinityRpg.DissolveData typeof<ScreenDispatcher>.Name Constants.FilePaths.CreditsGroupFilePath (Some Proxies.CreditsName) world
        World.subscribe4 (World.handleAsScreenTransition Proxies.Title) (EventAddresses.Click ->>- Proxies.CreditsBack.EntityAddress) Proxies.Game world

    static let createGameplay world =
        let world = snd <| World.createDissolveScreenFromGroupFile true Constants.InfinityRpg.DissolveData typeof<GameplayDispatcher>.Name Constants.FilePaths.HudGroupFilePath (Some Proxies.GameplayName) world
        let world = Proxies.Hud.SetPersistent false world // do not persist the Hud
        World.subscribe4 (World.handleAsScreenTransition Proxies.Title) (EventAddresses.Click ->>- Proxies.HudBack.EntityAddress) Proxies.Game world

    override dispatcher.Register _ world =
        let world = World.hintRenderPackageUse Constants.Assets.GuiPackageName world
        let world = World.hintRenderPackageUse Constants.Assets.GameplayPackageName world
        let world = createTitle world
        let world = createCredits world
        let world = createGameplay world
        let (splash, world) = World.createSplashScreen false Constants.InfinityRpg.NuSplashData typeof<ScreenDispatcher>.Name Proxies.Title (Some Proxies.NuSplashName) world
        World.selectScreen splash world