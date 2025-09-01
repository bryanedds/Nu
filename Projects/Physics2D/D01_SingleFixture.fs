namespace Physics2D
open System
open System.Numerics
open Prime
open Nu
open nkast.Aether.Physics2D.Dynamics.Joints

type ExtraEntityType = Box | Ball | Block | KinematicBlock | BodyJoint

// this extends the Screen API to expose the user-defined properties.
[<AutoOpen>]
module D01_SingleFixtureExtensions =
    type Screen with
        member this.GetDraggedEntity world : (Entity * BodyType) option = this.Get (nameof Screen.DraggedEntity) world
        member this.SetDraggedEntity (value : (Entity * BodyType) option) world = this.Set (nameof Screen.DraggedEntity) value world
        member this.DraggedEntity = lens (nameof Screen.DraggedEntity) this this.GetDraggedEntity this.SetDraggedEntity
        member this.GetExtraEntities world : Map<Entity, ExtraEntityType> = this.Get (nameof Screen.ExtraEntities) world
        member this.SetExtraEntities (value : Map<Entity, ExtraEntityType>) world = this.Set (nameof Screen.ExtraEntities) value world
        member this.ExtraEntities = lens (nameof Screen.ExtraEntities) this this.GetExtraEntities this.SetExtraEntities
        member this.GetPhysicsAnchorEntity world : Map<Entity, Entity> = this.Get (nameof Screen.PhysicsAnchorEntity) world
        member this.SetPhysicsAnchorEntity (value : Map<Entity, Entity>) world = this.Set (nameof Screen.PhysicsAnchorEntity) value world
        member this.PhysicsAnchorEntity = lens (nameof Screen.PhysicsAnchorEntity) this this.GetPhysicsAnchorEntity this.SetPhysicsAnchorEntity
        
// this is the dispatcher that customizes the top-level behavior of our game.
type D01_SingleFixtureDispatcher () =
    inherit ScreenDispatcherImSim ()
    
    // here we define default property values
    static member Properties =
        [define Screen.DraggedEntity None
         define Screen.ExtraEntities Map.empty
         define Screen.PhysicsAnchorEntity Map.empty]

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
                      // There can be multiple shapes per body, TransformOpt and PropertiesOpt 
                      TransformOpt = None
                      PropertiesOpt = None }
                 // Continuous collision detection adds additional checks between frame positions
                 // against high velocity objects tunneling through thin borders.
                 Entity.CollisionDetection .= Continuous
                 // Collision categories is a binary mask, defaulting to "1" (units place).
                 // The border is set to be in a different category, "10" (twos place)
                 // because we define fans later to not collide with the border.
                 // Meanwhile, unless we change the collision mask (Entity.CollisionMask),
                 // all entites default to collide with "*" (i.e. all collision categories).
                 Entity.CollisionCategories .= "10"
                 Entity.Elevation .= -1f // Draw order of the same elevation prioritizes entities with lower vertical position for 2D games.
                 Entity.StaticImage .= Assets.Gameplay.SkyBoxFront] world
            
        // define agent box
        let (agentBody, _) =
            World.doBox2d "Agent" // Unlike a block, a box uses dynamic physics by default - it reacts to forces and collisions.
                [Entity.Restitution .= 0.333f // bounciness
                 ] world
        
        // Keyboard controls for agent
        let agentForce = 100f
        let agentTorque = 1f
        if World.isKeyboardKeyDown KeyboardKey.A world then
            World.applyBodyForce (v3 -1f 0f 0f * agentForce) None agentBody world
        if World.isKeyboardKeyDown KeyboardKey.D world then
            World.applyBodyForce (v3 1f 0f 0f * agentForce) None agentBody world
        if World.isKeyboardKeyDown KeyboardKey.W world then
            // Fly up despite gravity
            World.applyBodyForce (v3 0f 1f 0f * agentForce - World.getGravity2d world) None agentBody world
        if World.isKeyboardKeyDown KeyboardKey.S world then
            // Glide down despite gravity
            World.applyBodyForce (v3 0f -1f 0f * agentForce - World.getGravity2d world) None agentBody world
        if World.isKeyboardKeyDown KeyboardKey.Q world then
            World.applyBodyTorque (v3 1f 0f 0f * agentTorque) agentBody world
        if World.isKeyboardKeyDown KeyboardKey.E world then
            World.applyBodyTorque (v3 -1f 0f 0f * agentTorque) agentBody world
        
        // Mouse dragging
        let mousePosition = World.getMousePostion2dWorld false world
        if World.isMouseButtonPressed MouseLeft world then
            let physicsAnchors = screen.GetPhysicsAnchorEntity world
            // (new _()) specifies a new set which is just the temporary container to hold the queried entities.
            // Optimizations can reuse the same set for different queries.
            for entity in World.getEntities2dAtPoint mousePosition (new _()) world do
                // NOTE: Some dynamic bodies may escape the border due to collisions against kinematic bodies (e.g. fans here).
                // In Nu, when an entity falls outside of the spatial bounds, e.g. by gravity,
                // it becomes omnipresent, meaning that it is returned for all spatial queries
                // e.g. World.getEntities2dAtPoint (affecting mouse dragging here).
                // This happens because Nu's spatial tree (Quadtree for 2D, Octtree for 3D) has a limited size.
                // The simplest resolution by Nu (avoiding engine complexity) that doesn't cause exceptions
                // for MMCC and ImSim APIs (happens when the entity is silently dropped) is to make
                // them omnipresent with a warning in the logs when this occurs.
                // For normal games, a game bounds within the spatial bounds should be defined to handle out of bounds entities.
                // For illustrative purposes here, we allow the warning to occur and use the spatial bounds as game bounds.
                if (World.getSpatialBounds2d world).Contains (entity.GetBounds(world).Box2) <> ContainmentType.Contains then
                    screen.ExtraEntities.Map (Map.remove entity) world
                else
                    let entity = Map.tryFind entity physicsAnchors |> Option.defaultValue entity
                    // Check rigid body facet existence to confirm the body type property's validity before reading it
                    if entity.Has<RigidBodyFacet> world && entity.Name <> "Border" then
                        if screen.GetDraggedEntity world = None then // Don't change more than one body to dynamic physics
                            screen.SetDraggedEntity (Some (entity, entity.GetBodyType world)) world
                            entity.SetBodyType Dynamic world // Only dynamic bodies react to forces by the mouse joint below.

        match screen.GetDraggedEntity world with
        | Some (draggedEntity, draggedBodyType) when World.isMouseButtonUp MouseLeft world ->
            screen.SetDraggedEntity None world
            draggedEntity.SetBodyType draggedBodyType world
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
                // A relation can be specified by relating two entities directly using their EntityAddresses.
                [Entity.BodyJointTarget .= Relation.relate mouseJoint.EntityAddress draggedEntity.EntityAddress
                 Entity.BodyJointTarget2Opt .= Some (Relation.relate mouseJoint.EntityAddress mouseSensor.EntityAddress)
                 Entity.BreakingPoint .= infinityf // never drop the entity while dragging
                 Entity.BodyJoint .= TwoBodyJoint2d
                    { CreateTwoBodyJoint = fun _ toPhysicsV2 a b ->
                        // Convert mouse position (Vector2) to world position (Vector3) to physics engine position (Aether.Physics2D Vector2)
                        let mousePosition = toPhysicsV2 mousePosition.V3
                        // Give dynamic bodies flick behavior, give static or kinematic bodies weld behavior.
                        if draggedBodyType = Dynamic then
                            // Use true to supply physics engine position as world coordinates which are converted to local body positions.
                            DistanceJoint (a, b, mousePosition, mousePosition, true, Frequency = 1.5f, DampingRatio = 0.5f)
                        else WeldJoint (a, b, mousePosition, mousePosition, true) }] world |> ignore

            // for distance joint, apply damping to body in order to stabilize it while dragged
            draggedEntity.LinearVelocity.Map ((*) 0.9f) world
            draggedEntity.AngularVelocity.Map ((*) 0.9f) world

            // visualise the mouse joint
            World.doBlock2d "MouseJointVisual"
                [// Update position, size and rotation every frame to match the two bodies
                 Entity.Position @= (draggedEntity.GetPosition world + mouseSensor.GetPosition world) / 2f
                 Entity.Size @= v3 (Vector3.Distance (draggedEntity.GetPosition world, mouseSensor.GetPosition world)) 1f 0f
                 Entity.Rotation @= Quaternion.CreateFromAxisAngle (v3 0f 0f 1f, MathF.Atan2 (mousePosition.Y - (draggedEntity.GetPosition world).Y, mousePosition.X - (draggedEntity.GetPosition world).X))
                 // Make line red which is applied on top of white
                 Entity.Color .= color 1f 0f 0f 1f
                 Entity.StaticImage .= Assets.Default.White
                 // Elevate the line above other entities (elevation 0) but below buttons (elevation 1)
                 Entity.Elevation .= 0.5f
                 // The line is not part of the physics simulation, so it has an empty body shape.
                 Entity.BodyShape .= EmptyShape] world |> ignore

        | None -> ()

        // BUTTONS TO ADD DYNAMIC BODIES - moving and reacting to forces and collisions //

        // Add box button
        if World.doButton "Add Box"
            [Entity.Position .= v3 255f 160f 0f
             Entity.Text .= "Add Box"
             // Give buttons higher elevation to draw on top of entities that might appear outside the border.
             Entity.Elevation .= 1f] world then
            let _ =
                World.doBox2d Gen.name
                    [Entity.Restitution .= 0.333f
                     // Random color
                     Entity.Color .= color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f
                     // Avoid stacking new boxes perfectly on top of each other for push forces to occur.
                     Entity.Position .= v3 Gen.randomf Gen.randomf 0f] world
            screen.SetExtraEntities (screen.GetExtraEntities world |> Map.add world.DeclaredEntity Box) world

        // Add ball button
        if World.doButton "Add Ball"
            [Entity.Position .= v3 255f 130f 0f
             Entity.Text .= "Add Ball"
             Entity.Elevation .= 1f] world then
            let _ =
                World.doBall2d Gen.name // Unlike a sphere, a ball uses dynamic physics by default.
                    [Entity.Restitution .= 0.5f // A different bounciness specified
                     Entity.Color .= color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f
                     Entity.Position .= v3 Gen.randomf Gen.randomf 0f] world
            screen.SetExtraEntities (screen.GetExtraEntities world |> Map.add world.DeclaredEntity Ball) world

        // Add small balls button
        if World.doButton "Add Tiny Balls"
            [Entity.Position .= v3 255f 100f 0f
             Entity.Text .= "Add Tiny Balls"
             Entity.Elevation .= 1f] world then
            for _ in 1 .. 16 do
                let _ =
                    World.doBall2d Gen.name
                        [Entity.Restitution .= 0.666f
                         Entity.Size .= Constants.Engine.Entity2dSizeDefault / 4f
                         Entity.Substance .= Mass (1f / 16f) // Make tiny balls have tiny mass when colliding
                         Entity.Color .= color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f
                         Entity.Position .= v3 Gen.randomf Gen.randomf 0f] world
                screen.SetExtraEntities (screen.GetExtraEntities world |> Map.add world.DeclaredEntity Ball) world

        // WIP: Soft body button

        // BUTTONS TO ADD STATIC BODIES - not moving and not reacting to forces and collisions //

        // Add block button
        if World.doButton "Add Block"
            [Entity.Position .= v3 255f 40f 0f
             Entity.Text .= "Add Block"
             Entity.Elevation .= 1f] world then
            let _ =
                World.doBlock2d Gen.name
                    [// Place the new block somewhere random within the border.
                     Entity.Position .= v3 (Gen.randomf1 500f - 250f) (Gen.randomf1 350f - 175f) 0f
                     Entity.StaticImage .= Assets.Default.Brick] world
            screen.SetExtraEntities (screen.GetExtraEntities world |> Map.add world.DeclaredEntity Block) world

        // BUTTONS TO ADD KINEMATIC BODIES - moving and not reacting to forces and collisions //

        // Add fan button
        if World.doButton "Add Fan"
            [Entity.Position .= v3 255f 10f 0f
             Entity.Text .= "Add Fan"
             Entity.Elevation .= 1f] world then
            // A fan is made of two rectangular blocks (blades) welded together at the center with a weld body joint.
            // One is the blades is set as the "anchor", which is kinematic and is the actual entity dragged by mouse.
            let x = Gen.randomf1 500f - 250f
            let y = Gen.randomf1 350f - 175f
            // Declare anchor
            let _ =
                World.doBlock2d Gen.name
                    [Entity.Position .= v3 x y 0f
                     Entity.Size .= v3 64f 8f 0f
                     // Kinematic physics does not react to forces or collisions, but can be moved by setting its velocity (here angular).
                     Entity.BodyType .= Kinematic
                     Entity.AngularVelocity @= v3 10f 0f 0f
                     // Set fans to be treated the same way as borders when colliding with other fans using the same collision category as border.
                     Entity.CollisionCategories .= "10"
                     // Fans only collide with entities in the default collision category "1",
                     // not the border or other fans in category "10",
                     // otherwise the + shape of the fan deforms when dragging next to another fan or the border.
                     Entity.CollisionMask .= "01"
                     Entity.StaticImage .= Assets.Default.Black] world
            let anchor = world.DeclaredEntity
            screen.SetExtraEntities (screen.GetExtraEntities world |> Map.add anchor KinematicBlock) world
            
            // Declare other blade
            let _ =
                World.doBox2d Gen.name
                    [Entity.Position .= v3 x y 0f
                     Entity.Rotation .= Quaternion.CreateFromAxisAngle (v3 0f 0f 1f, MathF.PI / 2f) // Rotate 90 degrees
                     Entity.Size .= v3 64f 8f 0f
                     Entity.CollisionCategories .= "10"
                     Entity.CollisionMask .= "01"
                     Entity.StaticImage .= Assets.Default.Black] world
            let blade = world.DeclaredEntity
            screen.ExtraEntities.Map (Map.add blade Box) world
            screen.PhysicsAnchorEntity.Map (Map.add blade anchor) world
            
            // Declare weld joint to link the two blades together at the center point (x, y)
            let _ =
                World.doBodyJoint2d Gen.name
                    // Aside from using two entities directly, a relation of two entities in the same group can also be
                    // specified by starting with the parent link denoted by "^", then accessing the sub-entity using "/".
                    [Entity.BodyJointTarget .= Relation.makeFromString $"^/{anchor.Name}"
                     Entity.BodyJointTarget2Opt .= Some (Relation.makeFromString $"^/{blade.Name}")
                     Entity.CollideConnected .= false // When the two blades are set to collide, the + shape would deform on drag
                     Entity.BreakingPoint .= infinityf
                     Entity.BodyJoint .= TwoBodyJoint2d
                        { CreateTwoBodyJoint = fun _ toPhysicsV2 a b ->
                            let p = toPhysicsV2 (v3 x y 0f)
                            WeldJoint (a, b, p, p, true) }] world |> ignore
            screen.SetExtraEntities (screen.GetExtraEntities world |> Map.add world.DeclaredEntity BodyJoint) world

        // Ensure the entities persist across ImSim renders.
        for keyValue in screen.GetExtraEntities world do
            match keyValue.Value with
            | Box -> World.doBox2d keyValue.Key.Name [] world |> ignore
            | Ball -> World.doBall2d keyValue.Key.Name [] world |> ignore
            | Block -> World.doBlock2d keyValue.Key.Name [] world |> ignore
            | KinematicBlock ->
                // Mouse dragging stops its movement, force angular velocity after dragging
                World.doBlock2d keyValue.Key.Name [Entity.AngularVelocity @= v3 10f 0f 0f
                                                   // Don't keep linear velocity from being dragged to collide
                                                   Entity.LinearVelocity @= v3Zero] world |> ignore
            | BodyJoint -> World.doBodyJoint2d keyValue.Key.Name [] world |> ignore

        // OTHER BUTTONS //

        // Clear Entities button
        if World.doButton "Clear Entities"
            [Entity.Position .= v3 255f -20f 0f
             Entity.Text .= "Clear Entities"
             Entity.Elevation .= 1f] world then
            screen.SetExtraEntities Map.empty world
            screen.SetPhysicsAnchorEntity Map.empty world

        // Gravity
        let gravityDisabled = World.getGravity2d world = v3Zero
        if World.doButton "Gravity"
            [Entity.Position .= v3 255f -50f 0f
             Entity.Text @= "Gravity: " + if gravityDisabled then "off" else "on"
             Entity.Elevation .= 1f] world then
            World.setGravity2d (if gravityDisabled then World.getGravityDefault2d world else v3Zero) world

        // Exit button (click behavior specified at Physics2D.fs)
        let _ =
            World.doButton Simulants.BackEntity
                [Entity.Position .= v3 255f -160f 0f
                 Entity.Elevation .= 1f
                 Entity.Text .= "Exit"] world
        World.endGroup world