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

        // declare enclosure screen
        let behavior = Dissolve (Constants.Dissolve.Default, None)
        World.beginScreen<SandBoxDispatcher>
            Simulants.SandBox.Name
            (Simulants.SandBox.GetExists world && Simulants.SandBox.GetSelected world) behavior
            [Screen.NextScreen .= Desire Simulants.RaceCourse] world |> ignore
        World.endScreen world

        // set the above screen as the initial screen
        if game.GetSelectedScreenOpt world = None then
            game.SetDesiredScreen (Desire world.DeclaredScreen) world
        
        // declare RaceCourse screen
        World.beginScreen<RaceCourseDispatcher>
            Simulants.RaceCourse.Name 
            (Simulants.RaceCourse.GetExists world && Simulants.RaceCourse.GetSelected world) behavior
            [Screen.NextScreen .= Desire Simulants.SandBox] world |> ignore
        World.endScreen world

        // handle Alt+F4 when not in editor
        if  World.isKeyboardAltDown world &&
            World.isKeyboardKeyDown KeyboardKey.F4 world &&
            world.Unaccompanied then
            World.exit world