namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module InfinityDispatcher =

    type [<StructuralEquality; NoComparison>] Infinity =
        { Gameplay : Gameplay }
    
    type InfinityCommand =
        | ShowTitle
        | ShowCredits
        | ShowGameplay
        | SetShallLoadGame of bool
        | ExitGame

    type Game with

        member this.GetInfinity = this.GetModelGeneric<Infinity>
        member this.SetInfinity = this.SetModelGeneric<Infinity>
        member this.Infinity = this.ModelGeneric<Infinity> ()
    
    type InfinityDispatcher () =
        inherit GameDispatcher<Infinity, unit, InfinityCommand> ({ Gameplay = Gameplay.initial })

        override this.Register (game, world) =
            // NOTE: just pre-loading all assets in the application for simplicity...
            // May have to make this more efficient later.
            let world = World.hintRenderPackageUse Assets.Gui.PackageName world
            let world = World.hintAudioPackageUse Assets.Gui.PackageName world
            let world = World.hintRenderPackageUse Assets.Gameplay.PackageName world
            let world = World.hintAudioPackageUse Assets.Gameplay.PackageName world
            base.Register (game, world)

        override this.Channel (_, _) =
            [Simulants.Title.Gui.Credits.ClickEvent => cmd ShowCredits
             Simulants.Title.Gui.NewGame.ClickEvent => cmd (SetShallLoadGame false)
             Simulants.Title.Gui.LoadGame.ClickEvent => cmd (SetShallLoadGame true)
             Simulants.Title.Gui.Exit.ClickEvent => cmd ExitGame
             Simulants.Credits.Gui.Back.ClickEvent => cmd ShowTitle
             Simulants.Gameplay.Gui.Back.ClickEvent => cmd ShowTitle]

        override this.Command (_, command, _, world) =
            match command with
            | ShowTitle -> World.transitionScreen Simulants.Title.Screen world |> just
            | ShowCredits -> World.transitionScreen Simulants.Credits.Screen world |> just
            | ShowGameplay -> World.transitionScreen Simulants.Gameplay.Screen world |> just
            | SetShallLoadGame shallLoadGame -> Simulants.Gameplay.Screen.Gameplay.Update (fun infinity -> { infinity with ShallLoadGame = shallLoadGame }) world |> withCmd ShowGameplay
            | ExitGame -> World.exit world |> just

        override this.Content (infinity, _) =
            [Content.screen Simulants.Splash.Screen.Name (Splash (Constants.Gui.DissolveDescriptor, Constants.Gui.SplashData, None, Simulants.Title.Screen)) [] []
             Content.screenFromGroupFile Simulants.Title.Screen.Name (Dissolve (Constants.Gui.DissolveDescriptor, Some Assets.Gui.ButterflyGirlSong)) Assets.Gui.TitleGroupFilePath
             Content.screenFromGroupFile Simulants.Credits.Screen.Name (Dissolve (Constants.Gui.DissolveDescriptor, Some Assets.Gui.ButterflyGirlSong)) Assets.Gui.CreditsGroupFilePath
             Content.screen<GameplayDispatcher> Simulants.Gameplay.Screen.Name (Dissolve (Constants.Gui.DissolveDescriptor, Some Assets.Gameplay.HerosVengeanceSong)) [Screen.Gameplay <== infinity --> fun infinity -> infinity.Gameplay] []]