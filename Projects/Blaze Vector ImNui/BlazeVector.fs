namespace BlazeVector
open System
open System.Numerics
open Prime
open Nu
open BlazeVector

// this determines what state the game is in. To learn about ImNui in Nu, see -
// https://github.com/bryanedds/Nu/wiki/Immediate-Mode-for-Games-via-ImNui
type GameState =
    | Splash
    | Title
    | Credits
    | Gameplay

// this extends the Game API to expose the above ImNui model as a property.
[<AutoOpen>]
module BlazeVectorExtensions =
    type Game with
        member this.GetGameState world : GameState = this.Get (nameof Game.GameState) world
        member this.SetGameState (value : GameState) world = this.Set (nameof Game.GameState) value world
        member this.GameState = lens (nameof Game.GameState) this this.GetGameState this.SetGameState

// this is the dispatcher that customizes the top-level behavior of our game.
type BlazeVectorDispatcher () =
    inherit GameDispatcherImNui ()

    // here we define default property values
    static member Properties =
        [define Game.GameState Splash]

    // here we define the game's top-level behavior
    override this.Process (blazeVector, world) =

        // declare splash screen
        let behavior = Slide (Constants.Dissolve.Default, Constants.Slide.Default, None, Simulants.Title)
        let (results, world) = World.beginScreen Simulants.Splash.Name (blazeVector.GetGameState world = Splash) behavior [] world
        let world = if FQueue.contains Deselecting results && not world.ContextInitializing then blazeVector.SetGameState Title world else world
        let world = World.endScreen world

        // declare title screen
        let behavior = Dissolve (Constants.Dissolve.Default, Some Assets.Gui.MachinerySong)
        let (_, world) = World.beginScreenWithGroupFromFile Simulants.Title.Name (blazeVector.GetGameState world = Title) behavior "Assets/Gui/Title.nugroup" [] world
        let world = World.beginGroup "Gui" [] world
        let (clicked, world) = World.doButton "Play" [] world
        let world = if clicked then blazeVector.SetGameState Gameplay world else world
        let (clicked, world) = World.doButton "Credits" [] world
        let world = if clicked then blazeVector.SetGameState Credits world else world
        let (clicked, world) = World.doButton "Exit" [] world
        let world = if clicked && world.Unaccompanied then World.exit world else world
        let world = World.endGroup world
        let world = World.endScreen world

        // declare gameplay screen
        let behavior = Dissolve (Constants.Dissolve.Default, Some Assets.Gameplay.DeadBlazeSong)
        let (results, world) = World.beginScreen<GameplayDispatcher> Simulants.Gameplay.Name (blazeVector.GetGameState world = Gameplay) behavior [] world
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
            then blazeVector.SetGameState Title world
            else world
        let world = World.endScreen world

        // declare credits screen
        let behavior = Dissolve (Constants.Dissolve.Default, Some Assets.Gui.MachinerySong)
        let (_, world) = World.beginScreenWithGroupFromFile Simulants.Credits.Name (blazeVector.GetGameState world = Credits) behavior "Assets/Gui/Credits.nugroup" [] world
        let world = World.beginGroup "Gui" [] world
        let (clicked, world) = World.doButton "Back" [] world
        let world = if clicked then blazeVector.SetGameState Title world else world
        let world = World.endGroup world
        let world = World.endScreen world
        world