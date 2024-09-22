namespace MyGame
open System
open System.Numerics
open Prime
open Nu

// this is our top-level ImNui model type. It determines what state the game is in. To learn about ImNui in Nu, see -
// https://github.com/bryanedds/Nu/wiki/Immediate-Mode-for-Games-via-ImNui
type MyGame =
    { Count : int }
    static member initial = { Count = 0 }

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
    override this.Run (counter, _, world) =

        // run game
        let world = World.beginGame world []
        let (_, world) = World.beginScreen "Screen" true Vanilla world []
        let world = World.beginGroup "Group" world []

        // create a sky box
        let world = World.doSkyBox "SkyBox" world []

        // create a rigid block
        let (_, world) = World.doBlock3d "Block3d" world [Entity.Position .= v3 0.0f -4.0f -12.0f]

        // create a rigid box, store its handle and body id for reference, then handle its body interactions
        let (results, world) = World.doBox3d "Box3d" world [Entity.Position .= v3 0.0f 4.0f -12.0f; Entity.Observable .= true]
        let box3d = world.RecentEntity
        let box3dBodyId = box3d.GetBodyId world
        let myGame =
            FQueue.fold (fun counter result ->
                match result with
                | BodyPenetration _ -> { counter with Count = inc counter.Count }
                | _ -> counter)
                counter results

        // expose a control panel
        let world = World.beginPanel "Panel" world [Entity.Position .= v3 -128.0f 0.0f 0.0f; Entity.Layout .= Flow (FlowDownward, FlowUnlimited)]
        let world = World.doText "Collisions" world [Entity.Text @= "Collisions: " + string myGame.Count]
        let world =
            match World.doButton "Jump!" world [Entity.Text .= "Jump!"; Entity.EnabledLocal @= World.getBodyGrounded box3dBodyId world] with
            | (true, world) -> World.applyBodyLinearImpulse (v3Up * 12.0f) None box3dBodyId world
            | (false, world) -> world
        let world = World.doFillBar "FillBar" world [Entity.Fill @= single myGame.Count / 25.0f]
        let world = if myGame.Count >= 25 then World.doText "Full!" world [Entity.Text .= "Full!"] else world
        let world = World.endPanel world

        // finish game
        let world = World.endGroup world
        let world = World.endScreen world
        let world = World.endGame world

        // handle Alt+F4
        let world =
            if World.isKeyboardAltDown world && World.isKeyboardKeyDown KeyboardKey.F4 world
            then World.exit world
            else world

        // fin
        (myGame, world)