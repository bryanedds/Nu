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
        World.beginScreen<ToyBoxDispatcher> Simulants.ToyBox.Name (game.GetGameState world = ToyBox) behavior [] world |> ignore
        World.beginGroup Simulants.ToyBoxScene.Name [] world
        if World.doButton Simulants.ToyBoxSwitchScene.Name
            [Entity.Position .= v3 255f -100f 0f
             Entity.Text .= "Switch Scene"
             Entity.Elevation .= 1f] world then
            game.SetGameState RaceCourse world
        World.endGroup world
        World.endScreen world

        // declare race course screen
        World.beginScreen<RaceCourseDispatcher> Simulants.RaceCourse.Name (game.GetGameState world = RaceCourse) behavior [] world |> ignore
        World.beginGroup Simulants.RaceCourseScene.Name [] world
        if World.doButton Simulants.RaceCourseSwitchScene.Name
            [Entity.Position .= v3 230f -140f 0f
             Entity.Text .= "Switch Scene"
             Entity.Elevation .= 1f] world then
            game.SetGameState ToyBox world
        World.endGroup world
        World.endScreen world

        // handle Alt+F4 when not in editor
        if  World.isKeyboardAltDown world &&
            World.isKeyboardKeyDown KeyboardKey.F4 world &&
            world.Unaccompanied then
            World.exit world