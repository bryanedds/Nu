// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// The top-level game state - the title screen, play, the game over screen, or the credits and then the ending, with
/// how long the escape took.
type GameState =
    | Title
    | Playing
    | GameOver
    | Credits of single
    | Ending of single

/// The game's state.
[<AutoOpen>]
module ZombieGameDispatcherExtensions =
    type Game with
        member this.GetGameState world : GameState = this.Get (nameof Game.GameState) world
        member this.SetGameState (value : GameState) world = this.Set (nameof Game.GameState) value world
        member this.GameState = lens (nameof Game.GameState) this this.GetGameState this.SetGameState

/// The game - its title, play, the game over and the ending, each a screen, the ending rolling the credits first.
type ZombieGameDispatcher () =
    inherit GameDispatcherImSim ()

    static member Properties =
        [define Game.GameState Title]

    override this.Process (game, world) =

        // title screen
        let dissolve = Dissolve (Constants.Dissolve.Default, None)
        World.beginScreen Simulants.Title.Name (game.GetGameState world).IsTitle dissolve [Screen.SongRequest .= RequestSong Assets.Gui.TitleSong] world |> ignore
        World.beginGroup "Scene" [] world
        if (game.GetGameState world).IsTitle then
            World.doEntity<MenuDispatcher> "Menu"
                [Entity.Elevation .= Constants.Gui.Elevation
                 Entity.Heading .= "ZOMBIE GAME"
                 Entity.HeadingColor .= Color (0.55f, 0.75f, 0.7f, 1.0f)
                 Entity.Subtitle .= "Blood of St. Agnes"
                 Entity.Choices .= (if Save.getExists () then [("Continue", ContinueChoice)] else []) @ [("New Game", NewGameChoice); ("Quit", QuitChoice)]] world
            for choice in World.doSubscription "Choose" world.DeclaredEntity.ChooseEvent world do
                match choice with
                | NewGameChoice ->
                    Simulants.Gameplay.Start (Save.makeStart (uint64 (Random.Shared.NextInt64 ()))) world
                    game.SetGameState Playing world
                | ContinueChoice ->
                    Simulants.Gameplay.Start (Save.read ()) world
                    game.SetGameState Playing world
                | QuitChoice | TitleChoice -> if world.Unaccompanied then World.exit world
        World.endGroup world
        World.endScreen world

        // gameplay screen, ending when it raises the player's fall or escape
        World.beginScreen<GameplayDispatcher> Simulants.Gameplay.Name (game.GetGameState world).IsPlaying dissolve [] world |> ignore
        if World.doSubscriptionAny "Fallen" Simulants.Gameplay.FallenEvent world then game.SetGameState GameOver world
        for time in World.doSubscription "Escaped" Simulants.Gameplay.EscapedEvent world do
            game.SetGameState (Credits time) world
        World.endScreen world

        // game over screen, its death sting carried on from play
        World.beginScreen Simulants.GameOver.Name (game.GetGameState world).IsGameOver dissolve [] world |> ignore
        World.beginGroup "Scene" [] world
        if (game.GetGameState world).IsGameOver then
            World.doEntity<MenuDispatcher> "Menu"
                [Entity.Elevation .= Constants.Gui.Elevation
                 Entity.Heading .= "YOU DIED"
                 Entity.HeadingColor .= Color (0.75f, 0.05f, 0.05f, 1.0f)
                 Entity.Choices .= (if Save.getExists () then [("Continue", ContinueChoice)] else []) @ [("Title", TitleChoice)]] world
            for choice in World.doSubscription "Choose" world.DeclaredEntity.ChooseEvent world do
                match choice with
                | ContinueChoice ->
                    Simulants.Gameplay.Start (Save.read ()) world
                    game.SetGameState Playing world
                | NewGameChoice | QuitChoice | TitleChoice -> game.SetGameState Title world
        World.endGroup world
        World.endScreen world

        // ending screen, its dawn music carried on from play - the credits rolling, then the time the escape took
        let endingScreenSelected = (game.GetGameState world).IsCredits || (game.GetGameState world).IsEnding
        World.beginScreen Simulants.Ending.Name endingScreenSelected dissolve [] world |> ignore
        World.beginGroup "Scene" [] world
        match game.GetGameState world with
        | Credits time ->
            World.doEntity<CreditsDispatcher> "Credits"
                [Entity.Elevation .= Constants.Gui.Elevation
                 Entity.Heading .= "ZOMBIE GAME"
                 Entity.Closing .= "Thank you for playing."] world
            if World.doSubscriptionAny "Finished" world.DeclaredEntity.FinishedEvent world then game.SetGameState (Ending time) world
        | Ending time ->
            let minutes = int time / 60
            let seconds = int time % 60
            let subtitle = "The handcar rattles off down the line toward the dawn.    Time " + string minutes + ":" + (string seconds).PadLeft (2, '0')
            World.doEntity<MenuDispatcher> "Menu"
                [Entity.Elevation .= Constants.Gui.Elevation
                 Entity.Heading .= "YOU ESCAPED"
                 Entity.HeadingColor .= Color (0.85f, 0.8f, 0.6f, 1.0f)
                 Entity.Subtitle .= subtitle
                 Entity.Choices .= [("Title", TitleChoice)]] world
            if World.doSubscriptionAny "Choose" world.DeclaredEntity.ChooseEvent world then game.SetGameState Title world
        | _ -> ()
        World.endGroup world
        World.endScreen world

        // when not in the editor, close on the window's close button or Alt+F4
        if world.Unaccompanied then
            if  World.doSubscriptionAny "Exit" game.ExitRequestEvent world ||
                World.isKeyboardAltDown world && World.isKeyboardKeyDown KeyboardKey.F4 world then
                World.exit world
