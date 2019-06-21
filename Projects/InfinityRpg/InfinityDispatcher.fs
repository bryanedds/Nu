namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module InfinityDispatcherModule =

    type InfinityModel =
        unit

    type InfinityCommand =
        | PlayTitleSong
        | FadeSong
        | ShowTitle
        | ShowCredits
        | ShowGameplay of bool
        | ExitGame

    type Game with

        member this.GetModel = this.Get Property? Model
        member this.SetModel = this.Set Property? Model
        member this.Model = Lens.make<unit, World> Property? Model this.GetModel this.SetModel this

    type InfinityDispatcher () =
        inherit GameDispatcher<unit, unit, InfinityCommand> (fun game -> game.Model)

        static member Properties =
            [define Game.Model ()]

        override this.Register (game, world) =

            // just pre-load all assets in the application for simplicity
            let world = World.hintRenderPackageUse Assets.DefaultPackage world
            let world = World.hintAudioPackageUse Assets.DefaultPackage world
            let world = World.hintRenderPackageUse Assets.GuiPackage world
            let world = World.hintAudioPackageUse Assets.GuiPackage world
            let world = World.hintRenderPackageUse Assets.GameplayPackage world
            let world = World.hintAudioPackageUse Assets.GameplayPackage world

            // get based
            let world = base.Register (game, world)

            // do not persist the hud when saving gameplay
            Hud.SetPersistent false world

        override this.Bindings (_, _, _) =
            [Title.IncomingStartEvent ==>! PlayTitleSong
             Title.OutgoingStartEvent ==>! FadeSong
             TitleCredits.ClickEvent ==>! ShowCredits
             TitleNewGame.ClickEvent ==>! ShowGameplay false
             TitleLoadGame.ClickEvent ==>! ShowGameplay true
             TitleExit.ClickEvent ==>! ExitGame
             CreditsBack.ClickEvent ==>! ShowTitle
             Gameplay.OutgoingStartEvent ==>! FadeSong
             HudBack.ClickEvent ==>! ShowTitle]

        override this.Command (command, _, _, world) =
            match command with
            | PlayTitleSong -> World.playSong 0 1.0f Assets.ButterflyGirlSong world
            | FadeSong -> World.fadeOutSong Constants.Audio.DefaultTimeToFadeOutSongMs world
            | ShowTitle -> World.transitionScreen Title world
            | ShowCredits -> World.transitionScreen Credits world
            | ShowGameplay load -> world |> Gameplay.SetShallLoadGame load |> World.transitionScreen Gameplay
            | ExitGame -> World.exit world

        override this.Layout (_, _, _) =
            [Layout.screen Splash (Nu.Splash (Constants.InfinityRpg.DissolveData, Constants.InfinityRpg.SplashData, Title)) [] []
             Layout.screenFromLayerFile Title (Dissolve Constants.InfinityRpg.DissolveData) Assets.TitleLayerFilePath
             Layout.screenFromLayerFile Credits (Dissolve Constants.InfinityRpg.DissolveData) Assets.CreditsLayerFilePath
             Layout.screenFromLayerFile<GameplayDispatcher> Gameplay (Dissolve Constants.InfinityRpg.DissolveData) Assets.HudLayerFilePath]