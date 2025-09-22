namespace SandBox2d
open System
open System.Numerics
open Prime
open Nu
        
// this is the dispatcher that customizes the top-level behavior of our game.
type SandBox2dDispatcher () =
    inherit GameDispatcherImSim ()

    // here we define the game's top-level behavior
    override this.Process (game, world) =

        // declare sand box screen
        let behavior = Dissolve (Constants.Dissolve.Default, None)
        World.beginScreen<SandBoxDispatcher>
            Simulants.SandBox.Name
            (Simulants.SandBox.GetSelected world)
            behavior [] world |> ignore
        World.endScreen world

        // declare race course screen
        World.beginScreen<RaceCourseDispatcher>
            Simulants.RaceCourse.Name
            (Simulants.RaceCourse.GetSelected world)
            behavior [] world |> ignore
        World.endScreen world

        // set sand box as the initial screen
        if (game.GetSelectedScreenOpt world).IsNone then
            game.SetDesiredScreen (Desire Simulants.SandBox) world

        // handle Alt+F4 when not in editor
        if  World.isKeyboardAltDown world &&
            World.isKeyboardKeyDown KeyboardKey.F4 world &&
            world.Unaccompanied then
            World.exit world