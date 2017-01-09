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

    static let createTitle world =
        let world = World.createDissolveScreenFromLayerFile None (Some Simulants.Title.ScreenName) Constants.InfinityRpg.DissolveData Assets.TitleLayerFilePath world |> snd
        let world = World.subscribe (World.handleAsScreenTransition Simulants.Credits) (Events.Click ->- Simulants.TitleCredits) Simulants.Game world
        let world = World.subscribe (handleAsScreenTransitionToGameplay false) (Events.Click ->- Simulants.TitleNewGame) Simulants.Game world
        let world = World.subscribe (handleAsScreenTransitionToGameplay true) (Events.Click ->- Simulants.TitleLoadGame) Simulants.Game world
        let world =
            List.fold
                (fun world i ->
                    let (entity, world) = World.createEntity None (i |> scstring |> (fun s -> "X" + s ) |> Name.make |> Some) Simulants.TitleGui world
                    let world = entity.SetPublishChanges false world
                    entity.SetImperative true world)
                world
                [0 .. 6000]
        World.subscribe World.handleAsExit (Events.Click ->- Simulants.TitleExit) Simulants.Game world

    static let createCredits world =
        let world = World.createDissolveScreenFromLayerFile None (Some Simulants.Credits.ScreenName) Constants.InfinityRpg.DissolveData Assets.CreditsLayerFilePath world |> snd
        World.subscribe (World.handleAsScreenTransition Simulants.Title) (Events.Click ->- Simulants.CreditsBack) Simulants.Game world

    static let createGameplay world =
        let world = World.createDissolveScreenFromLayerFile<GameplayDispatcher> None (Some Simulants.Gameplay.ScreenName) Constants.InfinityRpg.DissolveData Assets.HudLayerFilePath world |> snd
        let world = Simulants.Hud.SetPersistent false world // do not persist the Hud
        World.subscribe (World.handleAsScreenTransition Simulants.Title) (Events.Click ->- Simulants.HudBack) Simulants.Game world

    override dispatcher.Register (_, world) =
        let world = World.hintRenderPackageUse Assets.GuiPackageName world
        let world = World.hintRenderPackageUse Assets.GameplayPackageName world
        let world = createTitle world
        let world = createCredits world
        let world = createGameplay world
        let (splash, world) = World.createSplashScreen None (Some Simulants.Splash.ScreenName) Constants.InfinityRpg.NuSplashData Simulants.Title world
        World.selectScreen splash world