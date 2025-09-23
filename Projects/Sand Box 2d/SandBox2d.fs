namespace SandBox2d
open System
open System.Numerics
open Prime
open Nu

// this determines what state the game is in. To learn about ImSim in Nu, see -
// https://github.com/bryanedds/Nu/wiki/Immediate-Mode-for-Games-via-ImSim
type GameState =
    | ToyBox
    | RaceCourse

// this extends the Game API to expose GameState as a property.
[<AutoOpen>]
module SandBox2dExtensions =
    type Game with
        member this.GetGameState world : GameState = this.Get (nameof Game.GameState) world
        member this.SetGameState (value : GameState) world = this.Set (nameof Game.GameState) value world
        member this.GameState = lens (nameof Game.GameState) this this.GetGameState this.SetGameState
        
// this is the dispatcher that customizes the top-level behavior of our game.
type SandBox2dDispatcher () =
    inherit GameDispatcherImSim ()

    // here we define default property values
    static member Properties =
        [define Game.GameState ToyBox]

    // here we define the game's top-level behavior
    override this.Process (game, world) =

        // declare toy box screen
        let behavior = Dissolve (Constants.Dissolve.Default, None)
        World.doScreen<ToyBoxDispatcher> Simulants.ToyBox.Name (game.GetGameState world = ToyBox) behavior [] world |> ignore
        if World.doSubscriptionAny "SwitchScreen" Simulants.ToyBoxSwitchScreen.ClickEvent world then game.SetGameState RaceCourse world

        // declare race course screen
        World.doScreen<RaceCourseDispatcher> Simulants.RaceCourse.Name (game.GetGameState world = RaceCourse) behavior [] world |> ignore
        if World.doSubscriptionAny "SwitchScreen" Simulants.RaceCourseSwitchScreen.ClickEvent world then game.SetGameState ToyBox world

        // TODO: add another screen, maybe like a high-performance testing scenario like a bunch of physics particles.

        // handle Alt+F4 when not in editor
        if  World.isKeyboardAltDown world &&
            World.isKeyboardKeyDown KeyboardKey.F4 world &&
            world.Unaccompanied then
            World.exit world