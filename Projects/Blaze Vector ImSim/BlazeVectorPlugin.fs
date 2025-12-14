namespace BlazeVector
open System
open Nu
open BlazeVector

// this is a plugin for the Nu game engine that directs the execution of your application and editor.
type BlazeVectorPlugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor.
    override this.EditModes =
        Map.ofList
            [("Splash", fun world -> Game.SetGameState Splash world)
             ("Title", fun world -> Game.SetGameState Title world)
             ("Credits", fun world -> Game.SetGameState Credits world)
             ("Gameplay", fun world ->
                Simulants.Gameplay.SetGameplayState Playing world
                Game.SetGameState Gameplay world)]

    // this specifies which packages are automatically loaded at game start-up.
    override this.InitialPackages =
        [Assets.Gui.PackageName
         Assets.Gameplay.PackageName]

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