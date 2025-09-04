namespace Physics2D
open System
open System.Numerics
open Prime
open Nu
open nkast.Aether.Physics2D.Dynamics.Joints

type ExtraEntityType =
    | Box | Ball | TinyBalls | Spring | Block | Bridge | Fan
    | Clamp | SoftBody | Mystery
type Page = Page1 | Page2

// this extends the Screen API to expose the user-defined properties.
[<AutoOpen>]
module D01_SingleFixtureExtensions =
    type Screen with
        member this.GetExtraEntities world : Map<string, ExtraEntityType> = this.Get (nameof Screen.ExtraEntities) world
        member this.SetExtraEntities (value : Map<string, ExtraEntityType>) world = this.Set (nameof Screen.ExtraEntities) value world
        member this.ExtraEntities = lens (nameof Screen.ExtraEntities) this this.GetExtraEntities this.SetExtraEntities
        member this.GetDraggedEntity world : (Entity * BodyType) option = this.Get (nameof Screen.DraggedEntity) world
        member this.SetDraggedEntity (value : (Entity * BodyType) option) world = this.Set (nameof Screen.DraggedEntity) value world
        member this.DraggedEntity = lens (nameof Screen.DraggedEntity) this this.GetDraggedEntity this.SetDraggedEntity
        member this.GetMouseDragTarget world : Map<Entity, Entity> = this.Get (nameof Screen.MouseDragTarget) world
        member this.SetMouseDragTarget (value : Map<Entity, Entity>) world = this.Set (nameof Screen.MouseDragTarget) value world
        member this.MouseDragTarget = lens (nameof Screen.MouseDragTarget) this this.GetMouseDragTarget this.SetMouseDragTarget
        member this.GetSoftBodyContour world : Map<BodyId, Entity> = this.Get (nameof Screen.SoftBodyContour) world
        member this.SetSoftBodyContour (value : Map<BodyId, Entity>) world = this.Set (nameof Screen.SoftBodyContour) value world
        member this.SoftBodyContour = lens (nameof Screen.SoftBodyContour) this this.GetSoftBodyContour this.SetSoftBodyContour
        member this.GetExplosiveName world : string option = this.Get (nameof Screen.ExplosiveName) world
        member this.SetExplosiveName (value : string option) world = this.Set (nameof Screen.ExplosiveName) value world
        member this.ExplosiveName = lens (nameof Screen.ExplosiveName) this this.GetExplosiveName this.SetExplosiveName
        member this.GetPage world : Page = this.Get (nameof Screen.Page) world
        member this.SetPage (value : Page) world = this.Set (nameof Screen.Page) value world
        member this.Page = lens (nameof Screen.Page) this this.GetPage this.SetPage
        
// this is the dispatcher that customizes the top-level behavior of our game.
type D01_SingleFixtureDispatcher () =
    inherit ScreenDispatcherImSim ()
    
    // here we define default property values
    static member Properties =
        [define Screen.ExtraEntities Map.empty
         define Screen.DraggedEntity None
         define Screen.MouseDragTarget Map.empty
         define Screen.SoftBodyContour Map.empty
         define Screen.ExplosiveName None
         define Screen.Page Page1]

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
        
        // Mouse dragging - picking up the entity
        let mousePosition = World.getMousePostion2dWorld false world
        if World.isMouseButtonPressed MouseLeft world then
            let physicsAnchors = screen.GetMouseDragTarget world
            // (new _()) specifies a new set which is just the temporary container to hold the queried entities.
            // Optimizations can reuse the same set for different queries.
            for entity in World. mousePosition (new _()) world do
                let entity = Map.tryFind entity physicsAnchors |> Option.defaultValue entity
                // Check rigid body facet existence to confirm the body type property's validity before reading it
                if entity.Has<RigidBodyFacet> world && entity.Name <> "Border" then
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
                        // The body ID is unique across rigid bodies, it is just a registration in the physics engine.
                        if entity.GetBodyId world = agentBody then
                            // Reset agent position if it falls outside the world
                            entity.SetPosition v3Zero world
                            entity.SetLinearVelocity v3Zero world
                            entity.SetAngularVelocity v3Zero world
                        else // Remove other entities that fall outside the world
                            screen.ExtraEntities.Map (Map.remove entity.Name) world
                    elif screen.GetDraggedEntity world = None then // Don't change more than one body to dynamic physics
                        screen.SetDraggedEntity (Some (entity, entity.GetBodyType world)) world
                        entity.SetBodyType Dynamic world // Only dynamic bodies react to forces by the mouse joint below.
            if screen.GetDraggedEntity world = None then // No entity found via direct point test
                // Raycast entities to see if mouse location is inside a soft body enclosed area, then drag it
                let rayUp =
                    World.rayCastBodies2d (ray3 mousePosition.V3 (v3Up * 100f)) -1 false world
                    |> Seq.map _.BodyShapeIntersected.BodyId
                    |> Seq.choose (screen.GetSoftBodyContour world).TryFind
                    |> Set
                let rayDown =
                    World.rayCastBodies2d (ray3 mousePosition.V3 (v3Down * 100f)) -1 false world
                    |> Seq.map _.BodyShapeIntersected.BodyId
                    |> Seq.choose (screen.GetSoftBodyContour world).TryFind
                    |> Set
                let intersection = Set.intersect rayUp rayDown
                if Set.notEmpty intersection then
                    let entity = Set.minElement intersection
                    screen.SetDraggedEntity (Some (entity, entity.GetBodyType world)) world
                    entity.SetBodyType Dynamic world
        // Mouse dragging - moving the entity
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

        // Mouse scroll - rotating the entity
        for event in World.doSubscription "MouseWheel" Game.MouseWheelEvent world do
            let physicsAnchors = screen.GetMouseDragTarget world
            for entity in World.getEntities2dAtPoint mousePosition (new _()) world do
                let entity = Map.tryFind entity physicsAnchors |> Option.defaultValue entity
                if entity.Has<RigidBodyFacet> world && entity.Name <> "Border" then
                    World.applyBodyTorque (v3 40f 0f 0f * event.Travel) (entity.GetBodyId world) world

        match screen.GetPage world with
        | Page1 ->
            
            for (i, entityType) in List.indexed [Box; Ball; TinyBalls; Spring; Block; Bridge; Fan] do
                if World.doButton $"Add {entityType}"
                    [Entity.Position .= v3 255f (160f - 30f * float32 i) 0f
                     Entity.Text .= $"Add {entityType}"
                     // Give buttons higher elevation to draw on top of entities that might appear outside the border.
                     Entity.Elevation .= 1f] world then
                    screen.ExtraEntities.Map (Map.add Gen.name entityType) world
            
            if World.doButton "v"
                [Entity.Position .= v3 255f -50f 0f
                 Entity.Text .= "v"
                 Entity.Elevation .= 1f] world then
                screen.SetPage Page2 world

        | Page2 ->
            
            if World.doButton "^"
                [Entity.Position .= v3 255f 160f 0f
                 Entity.Text .= "^"
                 Entity.Elevation .= 1f] world then
                screen.SetPage Page1 world
                
            for (i, entityType) in List.indexed [SoftBody; Clamp] do
                if World.doButton $"Add {entityType}"
                    [Entity.Position .= v3 255f (130f - 30f * float32 i) 0f
                     Entity.Text .= $"Add {entityType}"
                     Entity.Elevation .= 1f] world then
                    screen.ExtraEntities.Map (Map.add Gen.name entityType) world

            if World.doButton "Add Mystery"
                [Entity.Position .= v3 255f -50f 0f
                 Entity.Text @= match screen.GetExplosiveName world with Some _ -> "Oh no" | None -> "Add ???"
                 Entity.Elevation .= 1f] world then
                match screen.GetExplosiveName world with
                | Some name -> 
                    screen.ExtraEntities.Map (Map.remove name) world
                    screen.SetExplosiveName None world
                | None ->
                    let name = Gen.name
                    screen.ExtraEntities.Map (Map.add name Mystery) world
                    screen.SetExplosiveName (Some name) world

        // Ensure the entities persist across ImSim renders.
        for KeyValue (name, entityType) in screen.GetExtraEntities world do
            match entityType with
            // DYNAMIC BODIES - moving and reacting to forces and collisions //
            // DYNAMIC BODIES - Box
            | Box ->
                let _ =
                    World.doBox2d name
                        [Entity.Restitution .= 0.333f
                         // Random color
                         Entity.Color .= color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f
                         // Avoid stacking new boxes perfectly on top of each other for push forces to occur.
                         Entity.Position .= v3 Gen.randomf Gen.randomf 0f] world
                ()
            // DYNAMIC BODIES - Ball
            | Ball ->
                let _ =
                    World.doBall2d name // Unlike a sphere, a ball uses dynamic physics by default.
                        [Entity.Restitution .= 0.5f // A different bounciness specified
                         Entity.Color .= color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f
                         Entity.Position .= v3 Gen.randomf Gen.randomf 0f] world
                ()
            // DYNAMIC BODIES - Tiny balls
            | TinyBalls ->
                for i in 1 .. 16 do
                    let _ =
                        World.doBall2d $"{name} Ball {i}"
                            [Entity.Restitution .= 0.666f
                             Entity.Size .= Constants.Engine.Entity2dSizeDefault / 4f
                             Entity.Substance .= Mass (1f / 16f) // Make tiny balls have tiny mass when colliding
                             Entity.Color .= color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f
                             Entity.Position .= v3 Gen.randomf Gen.randomf 0f] world
                    ()
            | Spring ->
                let color = color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f
                // TODO: Why are Face 1 and Face 2 present in the ubiquitous fallback of the quadtree?
                World.beginEntity<BodyJoint2dDispatcher> name
                    [Entity.BodyJointTarget .= Relation.makeFromString "Face 1"
                     Entity.BodyJointTarget2Opt .= Some (Relation.makeFromString "Face 2")
                     Entity.BodyJoint .= TwoBodyJoint2d
                        { CreateTwoBodyJoint = fun toPhysics _ a b ->
                            DistanceJoint (a, b, new _(0f, 0f), new _(0f, 0f), false, Length = toPhysics 60f, Frequency = 5f, DampingRatio = 0.3f) }] world
                let _ =
                    World.doBox2d "Face 1"
                        [Entity.Color .= color
                         Entity.Size .= v3 150f 10f 0f
                         Entity.StaticImage .= Assets.Default.Paddle] world
                let box1 = world.DeclaredEntity
                let _ =
                    World.doBox2d "Face 2"
                        [Entity.Color .= color
                         Entity.Position .= v3 0f -60f 0f
                         Entity.Size .= v3 150f 10f 0f
                         Entity.StaticImage .= Assets.Default.Paddle] world
                let box2 = world.DeclaredEntity
                let direction = box2.GetPosition world - box1.GetPosition world
                let _ =
                    World.doStaticSprite "JointVisual"
                        [Entity.Position @= (box1.GetPosition world + box2.GetPosition world) / 2f
                         Entity.Size @= v3 direction.Magnitude 1f 0f
                         Entity.Rotation @= Quaternion.CreateFromYawPitchRoll (0f, 0f, atan2 direction.Y direction.X)
                         Entity.Color .= color.WithA 0.5f
                         Entity.StaticImage .= Assets.Default.White
                         Entity.Elevation .= 0.5f] world
                let _ =
                    World.doBodyJoint2d "Prismatic joint"
                        [Entity.BodyJointTarget .= Relation.makeFromString "^/Face 1"
                         Entity.BodyJointTarget2Opt .= Some (Relation.makeFromString "^/Face 2")
                         Entity.BodyJoint .= TwoBodyJoint2d
                            { CreateTwoBodyJoint = fun _ toPhysicsV2 a b -> PrismaticJoint (a, b, new _(0f, 0f), toPhysicsV2 direction, useWorldCoordinates=false) }] world
                World.endEntity world
            // STATIC BODIES - not moving and not reacting to forces and collisions //
            // STATIC BODIES - Block
            | Block ->
                let _ =
                    World.doBlock2d name
                        [// Place the new block somewhere random within the border.
                         Entity.Position .= v3 (Gen.randomf1 500f - 250f) (Gen.randomf1 350f - 175f) 0f
                         Entity.StaticImage .= Assets.Default.Brick] world
                ()
            // STATIC BODIES (with a dynamic linkage) - Bridge [Revolute joint]
            | Bridge ->
                let x = Gen.randomf1 500f - 250f
                let y = Gen.randomf1 350f - 175f
                let _ =
                    World.doSphere2d name
                        [Entity.Position .= v3 x y 0f] world
                let anchor1 = world.DeclaredEntity
                let _ =
                    World.doSphere2d $"{name} Opposite end"
                        [Entity.Position .= v3 x y 0f] world
                let anchor2 = world.DeclaredEntity
                let direction = anchor2.GetPosition world - anchor1.GetPosition world
                for entity in [anchor1; anchor2] do
                    // Adjust position of link relative to each anchor as the anchors are dragged around
                    entity.SetRotation (Quaternion.CreateFromYawPitchRoll (0f, 0f, atan2 direction.Y direction.X + MathF.PI_OVER_2)) world
                let names = Array.init 6 (sprintf "%s Paddle %d" name)
                let boxHeight = direction.Length () / float32 (Array.length names)
                for i in 0 .. Array.length names - 1 do
                    let _ =
                        World.doBox2d names[i]
                            [Entity.Size @= v3 4f boxHeight 0f
                             Entity.StaticImage .= Assets.Default.Paddle
                             // The paddles are thin, use continuous collision detection to prevent tunnelling at high velocities
                             Entity.CollisionDetection .= Continuous] world
                    ()
                for (n1, n2) in Array.pairwise [|anchor1.Name; yield! names; anchor2.Name|] do
                    let _ =
                        World.doBodyJoint2d $"{n2} Link"
                            [Entity.BodyJointTarget .= Relation.makeFromString $"^/{n1}"
                             Entity.BodyJointTarget2Opt .= Some (Relation.makeFromString $"^/{n2}")
                             Entity.BodyJoint @= TwoBodyJoint2d
                                { CreateTwoBodyJoint = fun toPhysics _ a b ->
                                    RevoluteJoint (a, b, new _(0f, -0.5f * toPhysics boxHeight), new _(0f, 0.5f * toPhysics boxHeight), false) }] world
                    ()

            // DYNAMIC BODIES - Soft Body [Revolute joint, Distance joint]
            | SoftBody ->
                
                let color = color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f
                let boxNames = Array.init 32 (sprintf "%s Contour %d" name)
                let boxCount = float32 boxNames.Length
                let boxSize = 8f
                // This scale is large enough such that the soft body doesn't form knots,
                // but small enough to not spawn individual boxes outside the border.
                // The body joints will pull individual boxes together no matter how far apart the boxes spawn.
                let spawnScale = boxSize * boxCount / 8f
                let (spawnX, spawnY) = (0f, 0f)
            
                // define center for stabilizing the contour shape and for mouse dragging
                let _ =
                    World.doBall2d name
                        [Entity.Position .= v3 spawnX spawnY 0f
                         Entity.Size .= v3Dup 16f
                         Entity.Visible .= false] world
                let center = world.DeclaredEntity
            
                // define soft body countour boxes
                for i in 0 .. Array.length boxNames - 1 do
                    // Arrange 32 points in a circle for soft body
                    let boxAngle = MathF.Tau * float32 i / boxCount
                    let x = cos boxAngle * spawnScale + spawnX
                    let y = sin boxAngle * spawnScale + spawnY
                    let (declaredBodyId, _) =
                        World.doBox2d boxNames[i]
                            [Entity.Position .= v3 x y 0f
                             Entity.Restitution .= 0.333f
                             Entity.Size .= v3 boxSize boxSize 0f
                             Entity.Substance .= Mass (1f / boxCount) // Make mass evenly distributed between the contour and the center
                             Entity.CollisionDetection .= Continuous
                             Entity.Color .= color] world
                    // If the contour box is dragged directly, the many other joints counteract the mouse joint
                    // and the soft body stays mid-air away from the mouse
                    screen.MouseDragTarget.Map (Map.add world.DeclaredEntity center) world
                    screen.SoftBodyContour.Map (Map.add declaredBodyId center) world
            
                // declare revolute joint linkage between contour boxes
                for (n1, n2) in Array.pairwise boxNames |> Array.add (Array.last boxNames, Array.head boxNames) do
                    let _ =
                        World.doBodyJoint2d $"{n1} Joint contour"
                            // Aside from using two entities directly, a relation of two entities in the same group can also be
                            // specified by starting with the parent link denoted by "^", then accessing the sub-entity using "/".
                            [Entity.BodyJointTarget .= Relation.makeFromString $"^/{n1}"
                             Entity.BodyJointTarget2Opt .= Some (Relation.makeFromString $"^/{n2}")
                             Entity.CollideConnected .= true // Each box linked should collide with each other
                             Entity.BreakingPoint .= infinityf
                             Entity.BodyJoint .= TwoBodyJoint2d
                                { CreateTwoBodyJoint = fun toPhysics _ a b ->
                                    // Local coordinates are used here which centers at the body coordinates,
                                    // but we still have to convert from world scale to physics engine scale ourselves.
                                    let boxSize = toPhysics boxSize
                                    RevoluteJoint (a, b, new _(0f, 0.5f * boxSize), new _(0f, -0.5f * boxSize), false) }] world |> ignore
                    ()
                // declare distance joint linkage between contour boxes and center ball for stabilizing the shape
                for n in boxNames do
                    let _ =
                        World.doBodyJoint2d $"{n} Joint center"
                            // Aside from using two entities directly, a relation of two entities in the same group can also be
                            // specified by starting with the parent link denoted by "^", then accessing the sub-entity using "/".
                            [Entity.BodyJointTarget .= Relation.makeFromString $"^/{center.Name}"
                             Entity.BodyJointTarget2Opt .= Some (Relation.makeFromString $"^/{n}")
                             Entity.BreakingPoint .= infinityf
                             Entity.BodyJoint .= TwoBodyJoint2d
                            { CreateTwoBodyJoint = fun toPhysics _ a b ->
                                // Local coordinates are used here which centers at the body coordinates,
                                // but we still have to convert from world scale to physics engine scale ourselves.
                                let boxSize = toPhysics boxSize
                                DistanceJoint (a, b, new _(0f, 0f), new _(0f, 0f), false, Length = toPhysics spawnScale + boxSize, DampingRatio = 1f, Frequency = 5f) }] world
                    ()
            // KINEMATIC BODIES - moving and not reacting to forces and collisions //
            // KINEMATIC BODIES - Fan [Weld joint]
            | Fan ->
                // A fan is made of two rectangular blocks (blades) welded together at the center with a weld body joint.
                // One is the blades is set as the "anchor", which is kinematic and is the actual entity dragged by mouse.
                let x = Gen.randomf1 500f - 250f
                let y = Gen.randomf1 350f - 175f
                // Declare anchor
                let _ =
                    World.doBlock2d name
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
                         Entity.StaticImage .= Assets.Default.Label] world
                let anchor = world.DeclaredEntity
                
                // Declare other blade
                let _ =
                    World.doBox2d $"{name} Other blade"
                        [Entity.Position .= v3 x y 0f
                         Entity.Rotation .= Quaternion.CreateFromAxisAngle (v3 0f 0f 1f, MathF.PI / 2f) // Rotate 90 degrees
                         Entity.Size .= v3 64f 8f 0f
                         Entity.CollisionCategories .= "10"
                         Entity.CollisionMask .= "01"
                         Entity.StaticImage .= Assets.Default.Label
                         // Mouse dragging stops its movement, force angular velocity after dragging
                         Entity.AngularVelocity @= v3 10f 0f 0f
                         // Don't keep linear velocity from collisions on mouse drag release
                         Entity.LinearVelocity @= v3Zero
                         ] world
                let blade = world.DeclaredEntity
                screen.MouseDragTarget.Map (Map.add blade anchor) world
                
                // Declare weld joint to link the two blades together at the center point (x, y)
                let _ =
                    World.doBodyJoint2d $"{name} Weld joint"
                        [Entity.BodyJointTarget .= Relation.makeFromString $"^/{anchor.Name}"
                         Entity.BodyJointTarget2Opt .= Some (Relation.makeFromString $"^/{blade.Name}")
                         Entity.CollideConnected .= false // When the two blades are set to collide, the + shape would deform on drag
                         Entity.BreakingPoint .= infinityf
                         Entity.BodyJoint .= TwoBodyJoint2d
                            { CreateTwoBodyJoint = fun _ toPhysicsV2 a b ->
                                let p = toPhysicsV2 (v3 x y 0f)
                                WeldJoint (a, b, p, p, true) }] world
                ()
            // DYNAMIC BODIES - Clamp
            | Clamp ->
                // Center ball
                let ballSize = 32f
                let _ = World.doBall2d name [Entity.Size .= v3 ballSize ballSize 0f] world

                for (directionName, direction) in [("Left", -1f); ("Right", 1f)] do
                    let upperLeg = $"{name} {directionName} Upper leg"
                    for (newLeg, linkTo, image, angle) in
                        [(upperLeg, name, Assets.Default.Image, 0.2f)
                         ($"{name} {directionName} Lower leg", upperLeg, Assets.Default.Black, 0.4f)] do
                        let legLength = 30f
                        let _ =
                            World.doBox2d newLeg
                                [Entity.StaticImage .= image
                                 //Entity.Position .= v3 ((15f + 30f * i + ballSize) * direction) 0f 0f
                                 Entity.Size .= v3 legLength 4f 0f] world
                        let _ =
                            World.doBodyJoint2d $"{newLeg} Revolute joint"
                                [Entity.BodyJointTarget .= Relation.makeFromString $"^/{linkTo}"
                                 Entity.BodyJointTarget2Opt .= Some (Relation.makeFromString $"^/{newLeg}")
                                 Entity.CollideConnected .= false // Rotation movement would be limited if the upper leg collides with center
                                 Entity.BodyJoint .= TwoBodyJoint2d
                                    { CreateTwoBodyJoint = fun _ toPhysicsV2 a b ->
                                        let p = toPhysicsV2 (v3 (legLength * direction) 0f 0f)
                                        RevoluteJoint (a, b, p * 0.5f, p * -0.5f, false) }] world
                        let isExtended = world.DateTime.Second % 10 >= 5
                        let _ =
                            World.doBodyJoint2d $"""{newLeg} Angle joint {isExtended}"""
                                [Entity.BodyJointTarget .= Relation.makeFromString $"^/{linkTo}"
                                 Entity.BodyJointTarget2Opt .= Some (Relation.makeFromString $"^/{newLeg}")
                                 Entity.BodyJoint .= TwoBodyJoint2d
                                    { CreateTwoBodyJoint = fun _ _ a b ->
                                        AngleJoint (a, b, MaxImpulse = 3f,
                                            TargetAngle = (angle + if isExtended then 1f else 0f) * direction) }] world
                        ()
            // Derived from the soft body logic - press Ctrl+R to reload code if the physics engine disconnects
            | Mystery ->
                let color = color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f
                let names = Array.init 32 (sprintf "%s Particle %d" name)
                for i in 1 .. Array.length names do
                    let x = float32 (if i <= 8 then i elif i <= 16 then 8 elif i <= 24 then 25 - i else 0)
                    let y = float32 (if i <= 8 then 0 elif i <= 16 then i - 8 elif i <= 24 then 8 else i - 24)
                    let name = names[i - 1]
                    let _ =
                        World.doBox2d name
                            [Entity.Position .= v3 x y 0f
                             Entity.Restitution .= 0.333f
                             Entity.Size .= v3 1f 1f 0f
                             Entity.Substance .= Mass (1f / 16f)
                             Entity.Color .= color] world
                    ()
                for (n1, n2) in Array.pairwise names |> Array.add (Array.last names, Array.head names) do
                    let _ =
                        World.doBodyJoint2d $"{n1} Link"
                            [Entity.BodyJointTarget .= Relation.makeFromString $"^/{n1}"
                             Entity.BodyJointTarget2Opt .= Some (Relation.makeFromString $"^/{n2}")
                             Entity.BreakingPoint .= infinityf
                             Entity.BodyJoint .= TwoBodyJoint2d
                                { CreateTwoBodyJoint = fun _ _ a b ->
                                    RevoluteJoint (a, b, new _(0f, 0.5f), new _(0f, -0.5f), false) }] world |> ignore
                    ()

        // OTHER BUTTONS //

        // Clear Entities button
        if World.doButton "Clear Entities"
            [Entity.Position .= v3 255f -100f 0f
             Entity.Text .= "Clear Entities"
             Entity.Elevation .= 1f] world then
            screen.SetExtraEntities Map.empty world
            screen.SetMouseDragTarget Map.empty world
            screen.SetSoftBodyContour Map.empty world

        // Gravity
        let gravityDisabled = World.getGravity2d world = v3Zero
        if World.doButton "Gravity"
            [Entity.Position .= v3 255f -130f 0f
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