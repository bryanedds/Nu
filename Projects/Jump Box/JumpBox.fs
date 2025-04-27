namespace JumpBox
open System
open System.Numerics
open Prime
open Nu

// this extends the Game API to expose user-defined properties.
[<AutoOpen>]
module JumpBoxExtensions =
    type Game with
        member this.GetCollisions world : int = this.Get (nameof Game.Collisions) world
        member this.SetCollisions (value : int) world = this.Set (nameof Game.Collisions) value world
        member this.Collisions = lens (nameof Game.Collisions) this this.GetCollisions this.SetCollisions

// this is the dispatcher that customizes the top-level behavior of our game.
type JumpBoxDispatcher () =
    inherit GameDispatcherImSim ()

    // here we define default property values
    static member Properties =
        [define Game.Collisions 0]

    // here we define the game's behavior
    override this.Process (jumpBox, world) =

        // declare screen and group
        let (_, world) = World.beginScreen "Screen" true Vanilla [] world
        let world = World.beginGroup "Group" [] world

        // declare a block
        let (_, _, world) = World.doBlock2d "Block" [Entity.Position .= v3 128.0f -64.0f 0.0f] world

        // declare a box and then handle its body interactions for the frame
        let (boxBodyId, results, world) = World.doBox2d "Box" [Entity.Position .= v3 128.0f 64.0f 0.0f] world
        let world =
            FQueue.fold (fun world result ->
                match result with
                | BodyPenetrationData _ -> jumpBox.Collisions.Map inc world
                | _ -> world)
                world results

        // declare a control panel
        let world = World.beginPanel "Panel" [Entity.Position .= v3 -128.0f 0.0f 0.0f; Entity.Layout .= Flow (FlowDownward, FlowUnlimited)] world
        let world = World.doText "Collisions" [Entity.Text @= "Collisions: " + string (jumpBox.GetCollisions world)] world
        let (clicked, world) = World.doButton "Jump!" [Entity.EnabledLocal @= World.getBodyGrounded boxBodyId world; Entity.Text .= "Jump!"] world
        let world = if clicked then World.jumpBody false 8.0f boxBodyId world else world
        let world = World.doFillBar "FillBar" [Entity.Fill @= single (jumpBox.GetCollisions world) / 10.0f] world
        let world = if jumpBox.GetCollisions world >= 10 then World.doText "Full!" [Entity.Text .= "Full!"] world else world
        let world = World.endPanel world

        // finish declaring group and screen
        let world = World.endGroup world
        let world = World.endScreen world

        // handle Alt+F4 while unaccompanied
        if World.isKeyboardAltDown world && World.isKeyboardKeyDown KeyboardKey.F4 world && world.Unaccompanied
        then World.exit world
        else world