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
        let world = World.createDissolveScreenFromGroupFile false Constants.InfinityRpg.DissolveData Constants.FilePaths.TitleGroup typeof<ScreenDispatcher>.Name None (Some Simulants.Title.ScreenName) world |> snd
        let world = World.subscribe (World.handleAsScreenTransition Simulants.Credits) (Events.Click ->- Simulants.TitleCredits) Simulants.Game world
        let world = World.subscribe (handleAsScreenTransitionToGameplay false) (Events.Click ->- Simulants.TitleNewGame) Simulants.Game world
        let world = World.subscribe (handleAsScreenTransitionToGameplay true) (Events.Click ->- Simulants.TitleLoadGame) Simulants.Game world
        World.subscribe World.handleAsExit (Events.Click ->- Simulants.TitleExit) Simulants.Game world

    static let createCredits world =
        let world = World.createDissolveScreenFromGroupFile false Constants.InfinityRpg.DissolveData Constants.FilePaths.CreditsGroup typeof<ScreenDispatcher>.Name None (Some Simulants.Credits.ScreenName) world |> snd
        World.subscribe (World.handleAsScreenTransition Simulants.Title) (Events.Click ->- Simulants.CreditsBack) Simulants.Game world

    static let createGameplay world =
        let world = World.createDissolveScreenFromGroupFile true Constants.InfinityRpg.DissolveData Constants.FilePaths.HudGroup typeof<GameplayDispatcher>.Name None (Some Simulants.Gameplay.ScreenName) world |> snd
        let world = Simulants.Hud.SetPersistent false world // do not persist the Hud
        World.subscribe (World.handleAsScreenTransition Simulants.Title) (Events.Click ->- Simulants.HudBack) Simulants.Game world

    override dispatcher.Register (_, world) =
        let world = World.hintRenderPackageUse Constants.Assets.GuiPackageName world
        let world = World.hintRenderPackageUse Constants.Assets.GameplayPackageName world
        let world = createTitle world
        let world = createCredits world
        let world = createGameplay world
        let (splash, world) = World.createSplashScreen false Constants.InfinityRpg.NuSplashData Simulants.Title typeof<ScreenDispatcher>.Name None (Some Simulants.Splash.ScreenName) world
        World.selectScreen splash world