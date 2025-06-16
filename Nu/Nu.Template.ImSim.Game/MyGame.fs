namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

// this determines what state the game is in. To learn about ImSim in Nu, see -
// https://github.com/bryanedds/Nu/wiki/Immediate-Mode-for-Games-via-ImSim
type GameState =
    | Splash
    | Title
    | Credits
    | Gameplay

// this extends the Game API to expose the above ImSim model as a property.
[<AutoOpen>]
module MyGameExtensions =
    type Game with
        member this.GetGameState world : GameState = this.Get (nameof Game.GameState) world
        member this.SetGameState (value : GameState) world = this.Set (nameof Game.GameState) value world
        member this.GameState = lens (nameof Game.GameState) this this.GetGameState this.SetGameState

// this is the dispatcher that customizes the top-level behavior of our game.
type MyGameDispatcher () =
    inherit GameDispatcherImSim ()

    // here we define default property values
    static member Properties =
        [define Game.GameState Splash]

    // here we define the game's top-level behavior
    override this.Process (game, world) =

        // declare splash screen
        let behavior = Slide (Constants.Dissolve.Default, Constants.Slide.Default, None, Simulants.Title)
        let results = World.beginScreen Simulants.Splash.Name (game.GetGameState world = Splash) behavior [] world
        if FQueue.contains Deselecting results && game.GetGameState world = Splash then game.SetGameState Title world
        World.endScreen world

        // declare title screen
        let behavior = Dissolve (Constants.Dissolve.Default, None)
        let _ = World.beginScreenWithGroupFromFile Simulants.Title.Name (game.GetGameState world = Title) behavior "Assets/Gui/Title.nugroup" [] world
        World.beginGroup "Gui" [] world
        if World.doButton "Play" [] world then game.SetGameState Gameplay world
        if World.doButton "Credits" [] world then game.SetGameState Credits world
        if World.doButton "Exit" [] world && world.Unaccompanied then World.exit world
        World.endGroup world
        World.endScreen world

        // declare gameplay screen
        let behavior = Dissolve (Constants.Dissolve.Default, None)
        let results = World.beginScreen<GameplayDispatcher> Simulants.Gameplay.Name (game.GetGameState world = Gameplay) behavior [] world
        if FQueue.contains Select results then Simulants.Gameplay.SetGameplayState Playing world
        if FQueue.contains Deselecting results then Simulants.Gameplay.SetGameplayState Quit world
        if Simulants.Gameplay.GetSelected world && Simulants.Gameplay.GetGameplayState world = Quit then game.SetGameState Title world
        World.endScreen world

        // declare credits screen
        let behavior = Dissolve (Constants.Dissolve.Default, None)
        let _ = World.beginScreenWithGroupFromFile Simulants.Credits.Name (game.GetGameState world = Credits) behavior "Assets/Gui/Credits.nugroup" [] world
        World.beginGroup "Gui" [] world
        if World.doButton "Back" [] world then game.SetGameState Title world
        World.endGroup world
        World.endScreen world

        // handle Alt+F4 when not in editor
        if  World.isKeyboardAltDown world &&
            World.isKeyboardKeyDown KeyboardKey.F4 world &&
            world.Unaccompanied then
            World.exit world