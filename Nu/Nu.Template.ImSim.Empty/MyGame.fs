namespace MyGame
open System
open System.Numerics
open Prime
open Nu

// this is the dispatcher that customizes the top-level behavior of our game.
type MyGameDispatcher () =
    inherit GameDispatcherImSim ()

    // here we define the game's behavior
    override this.Process (_, world) =

        // process in the game's ImSim context
        let _ = World.beginScreen "Screen" true Vanilla [] world
        World.beginGroup "Scene" [] world
        let rotation = Quaternion.CreateFromAxisAngle ((v3 1.0f 0.75f 0.5f).Normalized, world.UpdateTime % 360L |> single |> Math.DegreesToRadians)
        World.doStaticModel "StaticModel" [Entity.Scale .= v3Dup 0.5f; Entity.Rotation @= rotation] world
        let exit = World.doButton "Exit" [Entity.Position .= v3 232.0f -144.0f 0.0f; Entity.Text .= "Exit"] world
        if exit && world.Unaccompanied then World.exit world
        World.endGroup world
        World.endScreen world

        // handle Alt+F4 when not in editor
        if  World.isKeyboardAltDown world &&
            World.isKeyboardKeyDown KeyboardKey.F4 world &&
            world.Unaccompanied then
            World.exit world