namespace Physics2D
open System
open System.Numerics
open Prime
open Nu
open nkast.Aether.Physics2D.Dynamics.Joints

// this extends the Screen API to expose the user-defined properties.
[<AutoOpen>]
module D01_SingleFixtureExtensions =
    type Screen with
        member this.GetDraggedEntity world : Entity option = this.Get (nameof Screen.DraggedEntity) world
        member this.SetDraggedEntity (value : Entity option) world = this.Set (nameof Screen.DraggedEntity) value world
        member this.DraggedEntity = lens (nameof Screen.DraggedEntity) this this.GetDraggedEntity this.SetDraggedEntity
        
// this is the dispatcher that customizes the top-level behavior of our game.
type D01_SingleFixtureDispatcher () =
    inherit ScreenDispatcherImSim ()
    
    // here we define default property values
    static member Properties =
        [define Screen.DraggedEntity None]

    // here we define the screen's behavior
    override this.Process (_, screen, world) =

        World.beginGroup Simulants.SceneGroup [] world
        
        let height = 320f
        let width = height / world.Eye2dSize.Y * world.Eye2dSize.X
        let _ =
            World.doBlock2d "Border" // A block uses static physics by default.
                [Entity.Size .= v3 width height 0f
                 Entity.BodyShape .= ContourShape
                    { Links =
                        [|v3 -0.5f 0.5f 0f
                          v3 0.5f 0.5f 0f
                          v3 0.5f -0.5f 0f
                          v3 -0.5f -0.5f 0f|]
                      Closed = true
                      TransformOpt = None
                      PropertiesOpt = None }
                 Entity.StaticImage .= Assets.Gameplay.SkyBoxFront] world
        let (agentBody, _) =
            World.doBox2d "Agent" // A box uses dynamic physics by default.
                [Entity.GravityOverride .= Some v3Zero
                 Entity.Elevation .= 10f] world
        let agent = world.DeclaredEntity
        
        // Mouse control
        let mousePosition = World.getMousePostion2dWorld false world
        if World.isMouseButtonPressed MouseLeft world then
            for entity in World.getEntities2dAtPoint mousePosition (new _()) world do
                if entity = agent then
                    screen.SetDraggedEntity (Some entity) world
        elif World.isMouseButtonUp MouseLeft world then
            screen.SetDraggedEntity None world

        match screen.GetDraggedEntity world with
        | Some entity ->
            let _ =
                World.doSphere2d "MouseSensor"
                    [Entity.BodyType .= Kinematic
                     Entity.BodyShape .= SphereShape
                        { Radius = 0.1f
                          PropertiesOpt = Some
                            { BodyShapeProperties.empty with SensorOpt = Some true } // No collision
                          TransformOpt = None }
                     Entity.Visible .= false
                     Entity.Position @= v3 mousePosition.X mousePosition.Y 0f] world
            let mouseSensor = world.DeclaredEntity
            let _ =
                let mouseJoint = world.ContextGroup / "MouseJoint"
                World.doBodyJoint2d mouseJoint.Name
                    [Entity.BodyJointTarget .= Relation.relate mouseJoint.EntityAddress entity.EntityAddress
                     Entity.BodyJointTarget2Opt .= Some (Relation.relate mouseJoint.EntityAddress mouseSensor.EntityAddress)
                     Entity.BodyJoint .= TwoBodyJoint2d
                        { CreateTwoBodyJoint = fun a b ->
                            let mousePosition = PhysicsEngine2d.toPhysicsV2 mousePosition.V3
                            WeldJoint(a, b, mousePosition, mousePosition, true, FrequencyHz = 1f, DampingRatio = 0.5f) }] world
            ()
        | None -> ()
        
        // Agent control
        let agentForce = 100f
        let agentTorque = 1f
        if World.isKeyboardKeyDown KeyboardKey.A world then
            World.applyBodyForce (v3 -1f 0f 0f * agentForce) None agentBody world
        if World.isKeyboardKeyDown KeyboardKey.D world then
            World.applyBodyForce (v3 1f 0f 0f * agentForce) None agentBody world
        if World.isKeyboardKeyDown KeyboardKey.W world then
            World.applyBodyForce (v3 0f 1f 0f * agentForce) None agentBody world
        if World.isKeyboardKeyDown KeyboardKey.S world then
            World.applyBodyForce (v3 0f -1f 0f * agentForce) None agentBody world
        if World.isKeyboardKeyDown KeyboardKey.Q world then
            World.applyBodyTorque (v3 -1f 0f 0f * agentTorque) agentBody world
        if World.isKeyboardKeyDown KeyboardKey.E world then
            World.applyBodyTorque (v3 1f 0f 0f * agentTorque) agentBody world

        // Exit button (click behavior specified at Physics2D.fs)
        let _ = World.doButton Simulants.BackEntity [Entity.Position .= v3 232.0f -144.0f 0.0f; Entity.Text .= "Exit"] world
        World.endGroup world