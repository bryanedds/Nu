namespace TerraFirma
open System
open System.Numerics
open Prime
open Nu
open TerraFirma

// this determines what state the game is in. To learn about ImSim in Nu, see -
// https://github.com/bryanedds/Nu/wiki/Immediate-Mode-for-Games-via-ImSim
type GameState =
    | Splash
    | Title
    | Credits
    | Gameplay

// this extends the Game API to expose the above ImSim model as a property.
[<AutoOpen>]
module TerraFirmaExtensions =
    type Game with
        member this.GetGameState world : GameState = this.Get (nameof Game.GameState) world
        member this.SetGameState (value : GameState) world = this.Set (nameof Game.GameState) value world
        member this.GameState = lens (nameof Game.GameState) this this.GetGameState this.SetGameState

// this is the dispatcher that customizes the top-level behavior of our game.
type TerraFirmaDispatcher () =
    inherit GameDispatcherImSim ()

    // here we define default property values
    static member Properties =
        [define Game.GameState Splash]

    // here we define the game's top-level behavior
    override this.Process (game, world) =

        // declare splash screen
        let behavior = Slide (Constants.Dissolve.Default, Constants.Slide.Default, None, Simulants.Title)
        let (results, world) = World.beginScreen Simulants.Splash.Name (game.GetGameState world = Splash) behavior [] world
        let world = if FQueue.contains Deselecting results && game.GetGameState world = Splash then game.SetGameState Title world else world
        let world = World.endScreen world

        // declare title screen
        let behavior = Dissolve (Constants.Dissolve.Default, Some Assets.Gui.GuiSong)
        let (_, world) = World.beginScreenWithGroupFromFile Simulants.Title.Name (game.GetGameState world = Title) behavior "Assets/Gui/Title.nugroup" [] world
        let world = World.beginGroup "Gui" [] world
        let (clicked, world) = World.doButton "Play" [] world
        let world = if clicked then game.SetGameState Gameplay world else world
        let (clicked, world) = World.doButton "Credits" [] world
        let world = if clicked then game.SetGameState Credits world else world
        let (clicked, world) = World.doButton "Exit" [] world
        let world = if clicked && world.Unaccompanied then World.exit world else world
        let world = World.endGroup world
        let world = World.endScreen world

        // declare gameplay screen
        let behavior = Dissolve (Constants.Dissolve.Default, Some Assets.Gameplay.DesertSong)
        let (results, world) = World.beginScreen<GameplayDispatcher> Simulants.Gameplay.Name (game.GetGameState world = Gameplay) behavior [] world
        let world =
            if FQueue.contains Select results
            then Simulants.Gameplay.SetGameplayState Playing world
            else world
        let world =
            if FQueue.contains Deselecting results
            then Simulants.Gameplay.SetGameplayState Quit world
            else world
        let world =
            if Simulants.Gameplay.GetSelected world && Simulants.Gameplay.GetGameplayState world = Quit
            then game.SetGameState Title world
            else world
        let world = World.endScreen world

        // declare credits screen
        let behavior = Dissolve (Constants.Dissolve.Default, Some Assets.Gui.GuiSong)
        let (_, world) = World.beginScreenWithGroupFromFile Simulants.Credits.Name (game.GetGameState world = Credits) behavior "Assets/Gui/Credits.nugroup" [] world
        let world = World.beginGroup "Gui" [] world
        let (clicked, world) = World.doButton "Back" [] world
        let world = if clicked then game.SetGameState Title world else world
        let world = World.endGroup world
        let world = World.endScreen world
        world