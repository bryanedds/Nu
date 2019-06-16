namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module InfinityDispatcherModule =

    type [<NoComparison>] Simulants =
        { Splash : Screen
          Title : Screen
          TitleGui : Layer
          TitleNewGame : Entity
          TitleLoadGame : Entity
          TitleCredits : Entity
          TitleExit : Entity
          Credits : Screen
          CreditsGui : Layer
          CreditsBack : Entity
          Gameplay : Screen }

    type InfinityCommand =
        | PlayTitleSong
        | FadeSong
        | ShowTitle
        | ShowCredits
        | ShowGameplay of bool
        | ExitGame

    type Game with

        member this.GetSimulants world : Simulants = this.Get Property? Simulants world
        member internal this.SetSimulants (value : Simulants) world = this.Set Property? Simulants value world
        member this.Simulants = PropertyTag.make this Property? Simulants this.GetSimulants this.SetSimulants

    type InfinityDispatcher () =
        inherit GameDispatcher<Simulants, unit, InfinityCommand> (fun game -> game.Simulants)

        static member Properties =
            [define Game.Simulants
                { Splash = !! "Splash"
                  Title = !! "Title"
                  TitleGui = !! "Title/Gui"
                  TitleNewGame = !! "Title/Gui/NewGame"
                  TitleLoadGame = !! "Title/Gui/LoadGame"
                  TitleCredits = !! "Title/Gui/Credits"
                  TitleExit = !! "Title/Gui/Exit"
                  Credits = !! "Credits"
                  CreditsGui = !! "Credits/Gui"
                  CreditsBack = !! "Credits/Gui/Back"
                  Gameplay = Simulants.Gameplay }]

        override this.Register (game, world) =

            // just pre-load all assets in the application for simplicity
            let world = World.hintRenderPackageUse Assets.DefaultPackageName world
            let world = World.hintAudioPackageUse Assets.DefaultPackageName world
            let world = World.hintRenderPackageUse Assets.GuiPackage world
            let world = World.hintAudioPackageUse Assets.GuiPackage world
            let world = World.hintRenderPackageUse Assets.GameplayPackage world
            let world = World.hintAudioPackageUse Assets.GameplayPackage world

            // get based
            let world = base.Register (game, world)

            // do not persist the Hud when saving gameplay
            Simulants.Hud.SetPersistent false world

        override this.Bindings (simulants, _, _) =
            [simulants.Title.IncomingStartEvent ==>! PlayTitleSong
             simulants.Title.OutgoingStartEvent ==>! FadeSong
             simulants.TitleCredits.ClickEvent ==>! ShowCredits
             simulants.TitleNewGame.ClickEvent ==>! ShowGameplay false
             simulants.TitleLoadGame.ClickEvent ==>! ShowGameplay true
             simulants.TitleExit.ClickEvent ==>! ExitGame
             simulants.CreditsBack.ClickEvent ==>! ShowTitle
             simulants.Gameplay.OutgoingStartEvent ==>! FadeSong
             Simulants.HudBack.ClickEvent ==>! ShowTitle]

        override this.Update (_, simulants, _, _) =
            just simulants

        override this.Command (command, simulants, _, world) =
            match command with
            | PlayTitleSong -> World.playSong 0 1.0f Assets.ButterflyGirlSong world
            | FadeSong -> World.fadeOutSong Constants.Audio.DefaultTimeToFadeOutSongMs world
            | ShowTitle -> World.transitionScreen simulants.Title world
            | ShowCredits -> World.transitionScreen simulants.Credits world
            | ShowGameplay load -> world |> Simulants.Gameplay.SetShallLoadGame load |> World.transitionScreen simulants.Gameplay
            | ExitGame -> World.exit world

        override this.Layout (simulants, _, _) =
            [Layout.screen simulants.Splash (Splash (Constants.InfinityRpg.DissolveData, Constants.InfinityRpg.SplashData, simulants.Title)) [] []
             Layout.screenFromLayerFile simulants.Title (Dissolve Constants.InfinityRpg.DissolveData) Assets.TitleLayerFilePath
             Layout.screenFromLayerFile simulants.Credits (Dissolve Constants.InfinityRpg.DissolveData) Assets.CreditsLayerFilePath
             Layout.screenFromLayerFile<GameplayDispatcher> simulants.Gameplay (Dissolve Constants.InfinityRpg.DissolveData) Assets.HudLayerFilePath]