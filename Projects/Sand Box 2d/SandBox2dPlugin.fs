namespace SandBox2d
open System
open Nu
open SandBox2d

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type SandBox2dPlugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor
    override this.EditModes =
        Map.ofList
            [("ToyBox", fun world -> Game.SetGameState ToyBox world)
             ("RaceCourse", fun world -> Game.SetGameState RaceCourse world)
             ("FluidSim", fun world -> Game.SetGameState FluidSim world)]

    override this.MakePhysicsEngine2d () = Box2dNetPhysicsEngine.make (Constants.Physics.GravityDefault * Constants.Engine.Meter2d)
    override this.MakePhysicsEngine2dRenderContext segments circles eyeBounds =
        { new Box2dNetPhysicsEngineRenderContext with
            override this.DrawLine (start, stop, color) =
                match segments.TryGetValue color with
                | (true, segmentList) -> segmentList.Add (start, stop)
                | (false, _) -> segments.Add (color, Collections.Generic.List [struct (start, stop)])
            override this.DrawCircle (center, radius, color) =
                match circles.TryGetValue struct (color, radius) with
                | (true, circleList) -> circleList.Add center
                | (false, _) -> circles.Add (struct (color, radius), Collections.Generic.List [center])
            override _.EyeBounds = eyeBounds }