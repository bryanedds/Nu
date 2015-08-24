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
        let world = Simulants.Gameplay.SetShallLoadGame shallLoadGame world
        World.handleAsScreenTransition Simulants.Gameplay event world

    static let createTitle world =
        let world = snd <| World.createDissolveScreenFromGroupFile false Constants.InfinityRpg.DissolveData typeof<ScreenDispatcher>.Name Constants.FilePaths.TitleGroup (Some Simulants.TitleName) world
        let world = World.subscribe4 (World.handleAsScreenTransition Simulants.Credits) (Events.Click ->>- Simulants.TitleCredits) Simulants.Game world
        let world = World.subscribe4 (handleAsScreenTransitionToGameplay false) (Events.Click ->>- Simulants.TitleNewGame) Simulants.Game world
        let world = World.subscribe4 (handleAsScreenTransitionToGameplay true) (Events.Click ->>- Simulants.TitleLoadGame) Simulants.Game world
        World.subscribe4 World.handleAsExit (Events.Click ->>- Simulants.TitleExit) Simulants.Game world

    static let createCredits world =
        let world = snd <| World.createDissolveScreenFromGroupFile false Constants.InfinityRpg.DissolveData typeof<ScreenDispatcher>.Name Constants.FilePaths.CreditsGroup (Some Simulants.CreditsName) world
        World.subscribe4 (World.handleAsScreenTransition Simulants.Title) (Events.Click ->>- Simulants.CreditsBack) Simulants.Game world

    static let createGameplay world =
        let world = snd <| World.createDissolveScreenFromGroupFile true Constants.InfinityRpg.DissolveData typeof<GameplayDispatcher>.Name Constants.FilePaths.HudGroup (Some Simulants.GameplayName) world
        let world = Simulants.Hud.SetPersistent false world // do not persist the Hud
        World.subscribe4 (World.handleAsScreenTransition Simulants.Title) (Events.Click ->>- Simulants.HudBack) Simulants.Game world

    override dispatcher.Register _ world =
        let world = World.hintRenderPackageUse Constants.Assets.GuiPackageName world
        let world = World.hintRenderPackageUse Constants.Assets.GameplayPackageName world
        let world = createTitle world
        let world = createCredits world
        let world = createGameplay world
        let (splash, world) = World.createSplashScreen false Constants.InfinityRpg.NuSplashData typeof<ScreenDispatcher>.Name Simulants.Title (Some Simulants.NuSplashName) world
        World.selectScreen splash world