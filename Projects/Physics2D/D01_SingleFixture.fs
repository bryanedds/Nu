namespace Physics2D
open System
open System.Numerics
open Prime
open Nu
open nkast.Aether.Physics2D.Dynamics.Joints

type ExtraBodyType = Box | Ball | Brick

// this extends the Screen API to expose the user-defined properties.
[<AutoOpen>]
module D01_SingleFixtureExtensions =
    type Screen with
        member this.GetDraggedEntity world : (Entity * BodyType) option = this.Get (nameof Screen.DraggedEntity) world
        member this.SetDraggedEntity (value : (Entity * BodyType) option) world = this.Set (nameof Screen.DraggedEntity) value world
        member this.DraggedEntity = lens (nameof Screen.DraggedEntity) this this.GetDraggedEntity this.SetDraggedEntity
        member this.GetExtraEntities world : Map<string, ExtraBodyType> = this.Get (nameof Screen.ExtraBodies) world
        member this.SetExtraEntities (value : Map<string, ExtraBodyType>) world = this.Set (nameof Screen.ExtraBodies) value world
        member this.ExtraBodies = lens (nameof Screen.ExtraBodies) this this.GetExtraEntities this.SetExtraEntities
        
// this is the dispatcher that customizes the top-level behavior of our game.
type D01_SingleFixtureDispatcher () =
    inherit ScreenDispatcherImSim ()
    
    // here we define default property values
    static member Properties =
        [define Screen.DraggedEntity None
         define Screen.ExtraBodies Map.empty]

    // here we define the screen's behavior
    override this.Process (_, screen, world) =
        World.beginGroup Simulants.SceneGroup [] world // All entities must be in a group - groups are the unit of entity loading.
        
        // define border
        let _ =
            World.doBlock2d "Border" // A block uses static physics by default - it does not react to forces or collisions.
                [Entity.Size .= v3 500f 350f 0f
                 Entity.BodyShape .= ContourShape // The body shape handles collisions and is independent of how it's displayed.
                    { Links = // A contour shape, unlike other shapes, is hollow.
                        [|v3 -0.5f 0.5f 0f // Zero is the entity's center, one is the entity's size in positive direction.
                          v3 0.5f 0.5f 0f
                          v3 0.5f -0.5f 0f
                          v3 -0.5f -0.5f 0f|]
                      Closed = true // The last point connects to the first point.
                      TransformOpt = None
                      PropertiesOpt = None }
                 // Continuous collision detection adds additional checks between frame positions
                 // against high velocity objects tunneling through thin borders.
                 Entity.CollisionDetection .= Continuous
                 Entity.Elevation .= -1f // Draw order of the same elevation prioritizes entities with lower vertical position for 2D games.
                 Entity.StaticImage .= Assets.Gameplay.SkyBoxFront] world
            
        // define agent box
        let (agentBody, _) =
            World.doBox2d "Agent" // Unlike a block, a box uses dynamic physics by default - it reacts to forces and collisions.
                [Entity.Restitution .= 0.333f // bounciness
                 ] world
        
        // Mouse dragging
        let mousePosition = World.getMousePostion2dWorld false world
        if World.isMouseButtonPressed MouseLeft world then
            // (new _()) specifies a new set which is just the temporary container to hold the queried entities.
            // Optimizations can reuse the same set for different queries.
            for entity in World.getEntities2dAtPoint mousePosition (new _()) world do
                // Check rigid body facet existence to confirm the body type property's validity before reading it
                if entity.Has<RigidBodyFacet> world && entity.Name <> "Border" then
                    screen.SetDraggedEntity (Some (entity, entity.GetBodyType world)) world
                    entity.SetBodyType Dynamic world // Only dynamic bodies react to forces by the mouse joint below.

        match screen.GetDraggedEntity world with
        | Some (draggedEntity, draggedBodyType) ->

            // declare sensor for mouse body
            World.doSphere2d "MouseSensor" // A sphere uses static physics by default.
                [Entity.BodyShape .= SphereShape
                    { Radius = 0.1f
                      // A sensor body never collides with another body.
                      PropertiesOpt = Some { BodyShapeProperties.empty with SensorOpt = Some true }
                      TransformOpt = None }
                 Entity.Visible .= false
                 // Re-initialization of the entity position is required every frame, necessitating the dynamic property operator.
                 Entity.Position @= v3 mousePosition.X mousePosition.Y 0f] world |> ignore
            let mouseSensor = world.DeclaredEntity

            // declare distance joint for mouse body
            let mouseJoint = world.ContextGroup / "MouseJoint"
            World.doBodyJoint2d mouseJoint.Name
                [Entity.BodyJointTarget .= Relation.relate mouseJoint.EntityAddress draggedEntity.EntityAddress
                 Entity.BodyJointTarget2Opt .= Some (Relation.relate mouseJoint.EntityAddress mouseSensor.EntityAddress)
                 Entity.BodyJoint .= TwoBodyJoint2d
                    { CreateTwoBodyJoint = fun _ toPhysicsV2 a b ->
                        // Convert mouse position (Vector2) to world position (Vector3) to physics engine position (Aether.Physics2D Vector2)
                        let mousePosition = toPhysicsV2 mousePosition.V3
                        // Give dynamic bodies flick behavior, give static bodies weld behavior.
                        if draggedBodyType = Dynamic then
                            // Use true to supply physics engine position as world coordinates which are converted to local body positions.
                            DistanceJoint (a, b, mousePosition, mousePosition, true, Frequency = 1.5f, DampingRatio = 0.5f)
                        else WeldJoint (a, b, mousePosition, mousePosition, true) }] world |> ignore

            // for distance joint, apply damping to body in order to stabilize it while dragged
            draggedEntity.LinearVelocity.Map ((*) 0.9f) world
            draggedEntity.AngularVelocity.Map ((*) 0.9f) world

            if World.isMouseButtonUp MouseLeft world then
                screen.SetDraggedEntity None world
                draggedEntity.SetBodyType draggedBodyType world

        | None -> ()
        
        // Agent control
        let agentForce = 100f
        let agentTorque = 1f
        if World.isKeyboardKeyDown KeyboardKey.A world then
            World.applyBodyForce (v3 -1f 0f 0f * agentForce) None agentBody world
        if World.isKeyboardKeyDown KeyboardKey.D world then
            World.applyBodyForce (v3 1f 0f 0f * agentForce) None agentBody world
        if World.isKeyboardKeyDown KeyboardKey.W world then
            // Fly up despite gravity
            World.applyBodyForce (v3 0f 1f 0f * agentForce - World.getGravity true world) None agentBody world
        if World.isKeyboardKeyDown KeyboardKey.S world then
            // Glide down despite gravity
            World.applyBodyForce (v3 0f -1f 0f * agentForce - World.getGravity true world) None agentBody world
        if World.isKeyboardKeyDown KeyboardKey.Q world then
            World.applyBodyTorque (v3 1f 0f 0f * agentTorque) agentBody world
        if World.isKeyboardKeyDown KeyboardKey.E world then
            World.applyBodyTorque (v3 -1f 0f 0f * agentTorque) agentBody world

        // Add box button
        if World.doButton "Add Box"
            [Entity.Position .= v3 255f 160f 0f
             Entity.Text .= "Add Box"
             Entity.Elevation .= 1f] world then
            let newEntity = Gen.name
            let _ =
                World.doBox2d newEntity
                    [Entity.Restitution .= 0.333f
                     // Random color
                     Entity.Color .= color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f
                     // Avoid stacking new boxes perfectly on top of each other for push forces to occur.
                     Entity.Position .= v3 Gen.randomf Gen.randomf 0f] world
            screen.SetExtraEntities (screen.GetExtraEntities world |> Map.add newEntity Box) world

        // Add ball button
        if World.doButton "Add Ball"
            [Entity.Position .= v3 255f 130f 0f
             Entity.Text .= "Add Ball"
             Entity.Elevation .= 1f] world then
            let newEntity = Gen.name
            let _ =
                World.doBall2d newEntity // Compared to a sphere, a ball uses dynamic physics by default.
                    [Entity.Restitution .= 0.5f // A different bounciness specified
                     Entity.Color .= color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f
                     Entity.Position .= v3 Gen.randomf Gen.randomf 0f] world
            screen.SetExtraEntities (screen.GetExtraEntities world |> Map.add newEntity Ball) world

        // Add block button
        if World.doButton "Add Block"
            [Entity.Position .= v3 255f 100f 0f
             Entity.Text .= "Add Block"
             Entity.Elevation .= 1f] world then
            let newEntity = Gen.name
            let _ =
                World.doBlock2d newEntity
                    [// Place the new block somewhere random within the border.
                     Entity.Position .= v3 (Gen.randomf1 500f - 250f) (Gen.randomf1 350f - 175f) 0f
                     Entity.StaticImage .= Assets.Default.Brick] world
            screen.SetExtraEntities (screen.GetExtraEntities world |> Map.add newEntity Brick) world
        
        // Ensure the entities persist across ImSim renders.
        for entity in screen.GetExtraEntities world do
            match entity.Value with
            | Box | Brick -> World.doBox2d entity.Key [] world
            | Ball -> World.doBall2d entity.Key [] world
            |> ignore

        // Clear Entities button
        if World.doButton "Clear Entities"
            [Entity.Position .= v3 255f 70f 0f
             Entity.Text .= "Clear Entities"
             Entity.Elevation .= 1f] world then
            screen.SetExtraEntities Map.empty world

        // Gravity
        let gravityDisabled = World.getGravity true world = v3Zero
        if World.doButton "Gravity"
            [Entity.Position .= v3 255f 40f 0f
             Entity.Text @= "Gravity: " + if gravityDisabled then "off" else "on"
             Entity.Elevation .= 1f] world then
            World.setGravity true (if gravityDisabled then v3 0f (-9.80665f * Constants.Engine.Meter2d) 0f else v3Zero) world

        // Exit button (click behavior specified at Physics2D.fs)
        let _ =
            World.doButton Simulants.BackEntity
                [Entity.Position .= v3 255f -160f 0f
                 Entity.Elevation .= 1f
                 Entity.Text .= "Exit"] world
        World.endGroup world