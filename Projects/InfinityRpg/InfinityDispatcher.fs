namespace InfinityRpg
open System
open Prime
open Nu
open InfinityRpg

type InfinityDispatcher () =
    inherit GameDispatcher ()

    static let handleAsScreenTransitionToGameplay shallLoadGame event world =
        let world = Simulants.Gameplay.SetShallLoadGame shallLoadGame world
        World.handleAsScreenTransition Simulants.Gameplay event world

    static let createOmniscreen world =
        let (omniscreen, world) = World.createScreen (Some Simulants.Omniscreen.ScreenName) world
        World.setOmniscreen omniscreen world

    static let createTitleScreen world =
        let world = World.createDissolveScreenFromLayerFile (Some Simulants.Title.ScreenName) Constants.InfinityRpg.DissolveData Assets.TitleLayerFilePath world |> snd
        let world = World.monitor (World.handleAsScreenTransition Simulants.Credits) (Events.Click ->- Simulants.TitleCredits) Simulants.Game world
        let world = World.monitor (handleAsScreenTransitionToGameplay false) (Events.Click ->- Simulants.TitleNewGame) Simulants.Game world
        let world = World.monitor (handleAsScreenTransitionToGameplay true) (Events.Click ->- Simulants.TitleLoadGame) Simulants.Game world
        World.monitorPlus World.handleAsExit (Events.Click ->- Simulants.TitleExit) Simulants.Game world |> snd

    static let createCreditsScreen world =
        let world = World.createDissolveScreenFromLayerFile (Some Simulants.Credits.ScreenName) Constants.InfinityRpg.DissolveData Assets.CreditsLayerFilePath world |> snd
        World.monitor (World.handleAsScreenTransition Simulants.Title) (Events.Click ->- Simulants.CreditsBack) Simulants.Game world

    static let createGameplayScreen world =
        let world = World.createDissolveScreenFromLayerFile<GameplayDispatcher> (Some Simulants.Gameplay.ScreenName) Constants.InfinityRpg.DissolveData Assets.HudLayerFilePath world |> snd
        let world = Simulants.Hud.SetPersistent false world // do not persist the Hud
        World.monitor (World.handleAsScreenTransition Simulants.Title) (Events.Click ->- Simulants.HudBack) Simulants.Game world

    override dispatcher.Register (_, world) =
        let world = World.hintRenderPackageUse Assets.GuiPackage world
        let world = World.hintRenderPackageUse Assets.GameplayPackage world
        let world = createOmniscreen world
        let world = createTitleScreen world
        let world = createCreditsScreen world
        let world = createGameplayScreen world
        let (splash, world) = World.createSplashScreen (Some Simulants.Splash.ScreenName) Constants.InfinityRpg.NuSplashData Simulants.Title world
        World.selectScreen splash world