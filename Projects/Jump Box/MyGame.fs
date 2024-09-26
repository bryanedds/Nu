namespace MyGame
open System
open System.Numerics
open Prime
open Nu

// this is our top-level ImNui model type. It determines what state the game is in. To learn about ImNui in Nu, see -
// https://github.com/bryanedds/Nu/wiki/Immediate-Mode-for-Games-via-ImNui
type MyGame =
    { Collisions : int }
    static member initial = { Collisions = 0 }

// this extends the Game API to expose the above ImNui model as a property.
[<AutoOpen>]
module MyGameExtensions =
    type Game with
        member this.GetMyGame world = this.GetModelGeneric<MyGame> world
        member this.SetMyGame value world = this.SetModelGeneric<MyGame> value world
        member this.MyGame = this.ModelGeneric<MyGame> ()

// this is the dispatcher that customizes the top-level behavior of our game.
type MyGameDispatcher () =
    inherit GameDispatcher<MyGame> (MyGame.initial)

    // here we handle running the game
    override this.Run (myGame, _, world) =

        // declare screen and group
        let (_, world) = World.beginScreen "Screen" true Vanilla [] world
        let world = World.beginGroup "Group" [] world

        // declare a sky box
        let world = World.doSkyBox "SkyBox" [] world

        // declare a block
        let (_, world) = World.doBlock3d "Block3d" [Entity.Position .= v3 0.0f -4.0f -12.0f] world

        // declare a box, store its handle and body id for reference, then handle its body interactions
        let (results, world) = World.doBox3d "Box3d" [Entity.Position .= v3 0.0f 4.0f -12.0f; Entity.Observable .= true] world
        let box3d = world.RecentEntity
        let box3dBodyId = box3d.GetBodyId world
        let myGame =
            FQueue.fold (fun myGame result ->
                match result with
                | BodyPenetration _ -> { myGame with Collisions = inc myGame.Collisions }
                | _ -> myGame)
                myGame results

        // declare a control panel
        let world = World.beginPanel "Panel" [Entity.Position .= v3 -128.0f 0.0f 0.0f; Entity.Layout .= Flow (FlowDownward, FlowUnlimited)] world
        let world = World.doText "Collisions" [Entity.Text @= "Collisions: " + string myGame.Collisions] world
        let world =
            match World.doButton "Jump!" [Entity.Text .= "Jump!"; Entity.EnabledLocal @= World.getBodyGrounded box3dBodyId world] world with
            | (true, world) -> World.applyBodyLinearImpulse (v3Up * 12.0f) None box3dBodyId world
            | (false, world) -> world
        let world = World.doFillBar "FillBar" [Entity.Fill @= single myGame.Collisions / 25.0f] world
        let world = if myGame.Collisions >= 25 then World.doText "Full!" [Entity.Text .= "Full!"] world else world
        let world = World.endPanel world

        // finish declaring group and screen
        let world = World.endGroup world
        let world = World.endScreen world

        // handle Alt+F4
        let world =
            if World.isKeyboardAltDown world && World.isKeyboardKeyDown KeyboardKey.F4 world
            then World.exit world
            else world

        // fin
        (myGame, world)