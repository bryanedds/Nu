namespace Physics2D
open System
open System.Numerics
open Prime
open Nu
open nkast.Aether.Physics2D.Dynamics.Joints

type ExtraEntityType =
    | Box | Ball | TinyBalls | Spring | Block | Bridge | Fan
    | Clamp | Ragdoll | SoftBody | Web | Strandbeest | Mystery
type Page = Page1 | Page2
// Using the Relation type allows referring to an entity before it is declared.
type CameraPosition = CameraAbsolute of Vector2 | CameraTracking of Entity Relation

// this extends the Screen API to expose the user-defined properties.
[<AutoOpen>]
module DemoScreenExtensions =
    type Screen with
        member this.GetCameraPosition world : CameraPosition option = this.Get (nameof Screen.CameraPosition) world
        member this.SetCameraPosition (value : CameraPosition option) world = this.Set (nameof Screen.CameraPosition) value world
        member this.CameraPosition = lens (nameof Screen.CameraPosition) this this.GetCameraPosition this.SetCameraPosition
        member this.GetCameraPositionDefault world : CameraPosition = this.Get (nameof Screen.CameraPositionDefault) world
        member this.SetCameraPositionDefault (value : CameraPosition) world = this.Set (nameof Screen.CameraPositionDefault) value world
        member this.CameraPositionDefault = lens (nameof Screen.CameraPositionDefault) this this.GetCameraPositionDefault this.SetCameraPositionDefault
        member this.GetExtraEntities world : Map<string, ExtraEntityType> = this.Get (nameof Screen.ExtraEntities) world
        member this.SetExtraEntities (value : Map<string, ExtraEntityType>) world = this.Set (nameof Screen.ExtraEntities) value world
        member this.ExtraEntities = lens (nameof Screen.ExtraEntities) this this.GetExtraEntities this.SetExtraEntities
        member this.GetDraggedEntity world : (Entity * Vector3 * BodyType) option = this.Get (nameof Screen.DraggedEntity) world
        member this.SetDraggedEntity (value : (Entity * Vector3 * BodyType) option) world = this.Set (nameof Screen.DraggedEntity) value world
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
        member this.GetNextScreen world : DesiredScreen = this.Get (nameof Screen.NextScreen) world
        member this.SetNextScreen (value : DesiredScreen) world = this.Set (nameof Screen.NextScreen) value world
        member this.NextScreen = lens (nameof Screen.NextScreen) this this.GetNextScreen this.SetNextScreen
        
// this is the dispatcher that customizes the top-level behavior of our game.
type DemoScreenDispatcher () =
    inherit ScreenDispatcherImSim ()
    
    // here we define default property values
    static member Properties =
        [define Screen.CameraPosition None
         define Screen.CameraPositionDefault (CameraAbsolute v2Zero)
         define Screen.ExtraEntities Map.empty
         define Screen.DraggedEntity None
         define Screen.MouseDragTarget Map.empty 
         define Screen.SoftBodyContour Map.empty
         define Screen.ExplosiveName None
         define Screen.Page Page1
         define Screen.NextScreen DesireNone]

    // here we define the screen's behavior
    override this.Process (_, screen, world) =
        World.beginGroup Simulants.SceneGroup [] world // All entities must be in a group - groups are the unit of entity loading.

        // The Process method is run even for unselected screens because the entity hierarchy
        // defined in code still needs to be preserved across screen switching.
        // This allows entities in one screen to modify entities in another screen.
        // We have to check if the current screen is selected,
        // otherwise we would run keyboard and mouse handlers even for unselected screens!
        if screen.GetSelected world then

            // Camera control
            let resolveCamera () =
                match screen.GetCameraPosition world |> Option.defaultWith (fun () -> screen.GetCameraPositionDefault world) with
                | CameraAbsolute position -> position
                | CameraTracking relation ->
                    match tryResolve screen relation with
                    | Some e -> e.GetPosition(world).V2 +
                                v2 100f 60f // Menu offset (X = 60) + Car lookahead (X = 40) + Make objects spawn above ground (Y = 60)
                    | None -> v2Zero
            if World.isKeyboardKeyDown KeyboardKey.Left world then
                screen.SetCameraPosition (resolveCamera () - v2 1f 0f |> CameraAbsolute |> Some) world
            if World.isKeyboardKeyDown KeyboardKey.Right world then
                screen.SetCameraPosition (resolveCamera () + v2 1f 0f |> CameraAbsolute |> Some) world
            if World.isKeyboardKeyDown KeyboardKey.Up world then
                screen.SetCameraPosition (resolveCamera () + v2 0f 1f |> CameraAbsolute |> Some) world
            if World.isKeyboardKeyDown KeyboardKey.Down world then
                screen.SetCameraPosition (resolveCamera () - v2 0f 1f |> CameraAbsolute |> Some) world
            if World.isKeyboardKeyDown KeyboardKey.Home world then
                screen.SetCameraPosition None world
            // an eye can render even with no screen. the eye is NOT a camera.
            // it's just a field that specifies where rendering happens.
            // it is up to the user to set the eye to follow a camera.
            World.setEye2dCenter (resolveCamera ()) world

            // Mouse dragging - picking up the entity

            // In Nu, while the physics engine subsystems abstract over 2d and 3d to conform
            // to a 3d interface using Vector3, mouse and spatial subsystems have no utility
            // for such abstraction - they need more specificity. As a result, getMousePostion2dWorld
            // return Vector2, getEntities2dAtPoint takes Vector2, while Entity.Position and
            // rayCastBodies2d expect Vector3. .V3 is used to convert Vector2 to Vector3, .V2 for vice versa.
            // That's the art of API design - sometimes the types are more specific and concrete,
            // while sometimes the types are less specific because it keeps things more general.
            let mousePosition = (World.getMousePosition2dWorld false world).V3
            let setDraggedEntity (entity : Entity) =
                let relativePosition =
                    (mousePosition - entity.GetPosition world).Transform (entity.GetRotation world).Inverted
                screen.SetDraggedEntity (Some (entity, relativePosition, entity.GetBodyType world)) world
                entity.SetBodyType Dynamic world // Only dynamic bodies react to forces by the mouse joint below.
            if World.isMouseButtonPressed MouseLeft world then
                let physicsAnchors = screen.GetMouseDragTarget world
                // (new _()) specifies a new set which is just the temporary container to hold the queried entities.
                // Optimizations can reuse the same set for different queries.
                for entity in World.getEntities2dAtPoint mousePosition.V2 (new _()) world do
                    let entity = Map.tryFind entity physicsAnchors |> Option.defaultValue entity
                    // Check rigid body facet existence to confirm the body type property's validity before reading it
                    if entity.Has<RigidBodyFacet> world
                    && entity.GetVisible world // Don't drag invisible entities
                    && entity.Name <> Simulants.BorderEntity
                    && screen.GetDraggedEntity world = None then // Don't change more than one body to dynamic physics
                        setDraggedEntity entity
                if screen.GetDraggedEntity world = None then // No entity found via direct point test
                    // Raycast entities to see if mouse location is inside a soft body enclosed area, then drag it
                    let rayUp =
                        World.rayCastBodies2d (ray3 mousePosition (v3Up * 100f)) -1 false world
                        |> Seq.map _.BodyShapeIntersected.BodyId
                        |> Seq.choose (screen.GetSoftBodyContour world).TryFind
                        |> Set
                    let rayDown =
                        World.rayCastBodies2d (ray3 mousePosition (v3Down * 100f)) -1 false world
                        |> Seq.map _.BodyShapeIntersected.BodyId
                        |> Seq.choose (screen.GetSoftBodyContour world).TryFind
                        |> Set
                    let intersection = Set.intersect rayUp rayDown
                    if Set.notEmpty intersection then
                        setDraggedEntity (Set.minElement intersection)

            // Mouse dragging - moving the entity
            match screen.GetDraggedEntity world with
            | Some (draggedEntity, _, draggedBodyType) when World.isMouseButtonUp MouseLeft world ->
                screen.SetDraggedEntity None world
                draggedEntity.SetBodyType draggedBodyType world
            | Some (draggedEntity, relativePosition, draggedBodyType) ->
                // declare sensor for mouse body
                World.doSphere2d "MouseSensor" // A sphere uses static physics by default.
                    [Entity.BodyShape .= SphereShape
                        { Radius = 0.1f
                          // A sensor body never collides with another body.
                          PropertiesOpt = Some { BodyShapeProperties.empty with SensorOpt = Some true }
                          TransformOpt = None }
                     Entity.Visible .= false
                     // Re-initialization of the entity position is required every frame, necessitating the dynamic property operator.
                     Entity.Position @= mousePosition] world |> ignore
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
                            let mousePosition = toPhysicsV2 mousePosition
                            // Give dynamic bodies flick behavior, give static or kinematic bodies weld behavior.
                            if draggedBodyType = Dynamic then
                                // Use true to supply physics engine position as world coordinates which are converted to local body positions.
                                DistanceJoint (a, b, mousePosition, mousePosition, true, Frequency = 1.5f, DampingRatio = 0.5f)
                            else WeldJoint (a, b, mousePosition, mousePosition, true) }] world |> ignore

                // for distance joint, apply damping to body in order to stabilize it while dragged
                draggedEntity.LinearVelocity.Map ((*) 0.9f) world
                draggedEntity.AngularVelocity.Map ((*) 0.9f) world

                let draggedPosition = relativePosition.Transform (draggedEntity.GetRotation world) + draggedEntity.GetPosition world
                // visualise the mouse joint
                World.doBlock2d "MouseJointVisual"
                    [// Update position, size and rotation every frame to match the two bodies
                     Entity.Position @= (draggedPosition + mousePosition) / 2f
                     Entity.Size @= v3 (Vector3.Distance (draggedPosition, mousePosition)) 1f 0f
                     Entity.Rotation @= Quaternion.CreateLookAt2d (mousePosition - draggedPosition).V2
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
                for entity in World.getEntities2dAtPoint mousePosition.V2 (new _()) world do
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
                
            for (i, entityType) in List.indexed [Clamp; Ragdoll; SoftBody; Web; Strandbeest] do
                if World.doButton $"Add {entityType}"
                    [Entity.Position .= v3 255f (130f - 30f * float32 i) 0f
                     Entity.Text .= $"Add {entityType}"
                     Entity.Elevation .= 1f] world then
                    screen.ExtraEntities.Map (Map.add Gen.name entityType) world

            if World.doButton "Add Mystery"
                [Entity.Position .= v3 255f -20f 0f
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

            // Gravity
            let gravityDisabled = World.getGravity2d world = v3Zero
            if World.doButton "Gravity"
                [Entity.Position .= v3 255f -50f 0f
                 Entity.Text @= "Gravity: " + if gravityDisabled then "off" else "on"
                 Entity.Elevation .= 1f] world then
                World.setGravity2d (if gravityDisabled then World.getGravityDefault2d world else v3Zero) world


        // OTHER BUTTONS //
        
        // Switch scene button
        if World.doButton "Switch Scene"
            [Entity.Position .= v3 255f -100f 0f
             Entity.Text .= "Switch Scene"
             Entity.Elevation .= 1f] world then
            Game.SetDesiredScreen (screen.GetNextScreen world) world

        // Clear Entities button
        if World.doButton "Clear Entities"
            [Entity.Position .= v3 255f -130f 0f
             Entity.Text .= "Clear Entities"
             Entity.Elevation .= 1f] world then
            screen.SetExtraEntities Map.empty world
            screen.SetMouseDragTarget Map.empty world
            screen.SetSoftBodyContour Map.empty world

        // Exit button (click behavior specified at Physics2D.fs)
        let _ =
            World.doButton Simulants.BackEntity
                [Entity.Position .= v3 255f -160f 0f
                 Entity.Elevation .= 1f
                 Entity.Text .= "Exit"] world

        let spawnCenter = (World.getEye2dCenter world - v2 60f 0f).V3
        // Ensure the entities persist across ImSim renders.
        for KeyValue (name, entityType) in screen.GetExtraEntities world do
            match entityType with
            // DYNAMIC BODIES - moving and reacting to forces and collisions //
            // DYNAMIC BODIES - Box
            | Box ->
                let _ =
                    World.doBox2d name // Unlike a block, a box uses dynamic physics by default - it reacts to forces and collisions.
                        [Entity.Restitution .= 0.333f
                         // Random color
                         Entity.Color .= color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f
                         // Avoid stacking new boxes perfectly on top of each other for push forces to occur.
                         Entity.Position .= spawnCenter + v3 Gen.randomf Gen.randomf 0f] world
                ()
            // DYNAMIC BODIES - Ball
            | Ball ->
                let _ =
                    World.doBall2d name // Unlike a sphere, a ball uses dynamic physics by default.
                        [Entity.Restitution .= 0.5f // A different bounciness specified
                         Entity.Color .= color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f
                         Entity.Position .= spawnCenter + v3 Gen.randomf Gen.randomf 0f] world
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
                             Entity.Position .= spawnCenter + v3 Gen.randomf Gen.randomf 0f] world
                    ()
            // DYNAMIC BODIES - Spring [Distance joint, Prismatic joint]
            | Spring ->
                let color = color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f
                // In Nu, each entity can contain arbitrarily many child entities.
                // Default dispatchers do not specify behaviours for entity hierarchies,
                // using child entities is just for better organization while editing in Gaia.
                // Here, we just show that relations point to child entities by default.
                // There is also mounting to make entities inherit transforms (position, rotation, scale)
                // from other entities, which can be set using the Entity.MountOpt property.
                // Mounting only works for entities that are not rigid bodies because it makes no sense
                // for both the physics engine and mounted entity to specify transforms together.
                World.beginEntity<BodyJoint2dDispatcher> name
                    [Entity.BodyJointTarget .= Relation.makeFromString "Face 1" // Points to child entity
                     Entity.BodyJointTarget2Opt .= Some (Relation.makeFromString "Face 2")
                     Entity.BodyJoint .= TwoBodyJoint2d
                        { CreateTwoBodyJoint = fun toPhysics _ a b ->
                            // A distance joint maintains fixed distance between two bodies, optionally with spring-like behaviour.
                            // It does not impose limits on relative positions or rotations.
                            DistanceJoint (a, b, new _(0f, 0f), new _(0f, 0f), false, Length = toPhysics 60f, Frequency = 5f, DampingRatio = 0.3f) }] world
                let _ =
                    World.doBox2d "Face 1" // Pointed to by parent body joint
                        [Entity.Color .= color
                         Entity.Position .= spawnCenter
                         Entity.Size .= v3 150f 10f 0f
                         Entity.StaticImage .= Assets.Default.Paddle
                         Entity.Substance .= Mass (1f / 2f)] world
                let box1 = world.DeclaredEntity
                let _ =
                    World.doBox2d "Face 2"
                        [Entity.Color .= color
                         Entity.Position .=spawnCenter + v3 0f -60f 0f
                         Entity.Size .= v3 150f 10f 0f
                         Entity.StaticImage .= Assets.Default.Paddle
                         Entity.Substance .= Mass (1f / 2f)] world
                let box2 = world.DeclaredEntity
                let direction = box2.GetPosition world - box1.GetPosition world
                let _ =
                    World.doStaticSprite "Joint visual"
                        [Entity.Position @= (box1.GetPosition world + box2.GetPosition world) / 2f
                         Entity.Size @= v3 direction.Magnitude 1f 0f
                         Entity.Rotation @= Quaternion.CreateLookAt2d direction.V2
                         Entity.Color .= color.WithA 0.5f
                         Entity.StaticImage .= Assets.Default.White
                         Entity.Elevation .= 0.5f] world
                let _ =
                    World.doBodyJoint2d "Prismatic joint"
                        [Entity.BodyJointTarget .= Relation.makeFromString "^/Face 1"
                         Entity.BodyJointTarget2Opt .= Some (Relation.makeFromString "^/Face 2")
                         Entity.BodyJoint .= TwoBodyJoint2d
                            { CreateTwoBodyJoint = fun _ toPhysicsV2 a b ->
                                // A prismatic joint maintains fixed position between two bodies to move linearly
                                // along a straight axis while disallowing relative rotation, without fixing distance.
                                PrismaticJoint (a, b, new _(0f, 0f), toPhysicsV2 direction, useWorldCoordinates=false) }] world
                World.endEntity world
            // STATIC BODIES - not moving and not reacting to forces and collisions //
            // STATIC BODIES - Block
            | Block ->
                let _ =
                    World.doBlock2d name
                        [// Place the new block somewhere random within the border.
                         Entity.Position .= spawnCenter + v3 (Gen.randomf1 500f - 250f) (Gen.randomf1 350f - 175f) 0f
                         Entity.StaticImage .= Assets.Default.Brick] world
                ()
            // STATIC BODIES (with a dynamic linkage) - Bridge [Revolute joint]
            | Bridge ->
                let x = Gen.randomf1 500f - 250f
                let y = Gen.randomf1 350f - 175f
                let _ =
                    World.doSphere2d name
                        [Entity.Position .= spawnCenter + v3 x y 0f] world
                let anchor1 = world.DeclaredEntity
                let _ =
                    World.doSphere2d $"{name} Opposite end"
                        [Entity.Position .= spawnCenter + v3 x y 0f] world
                let anchor2 = world.DeclaredEntity
                let direction = anchor1.GetPosition world - anchor2.GetPosition world
                if direction <> v3Zero then // OrthonormalUp for <0, 0, 0> is <nan, nan, nan>, so avoid it
                    for entity in [anchor1; anchor2] do
                        // Adjust position of link relative to each anchor as the anchors are dragged around
                        entity.SetRotation (Quaternion.CreateLookAt2d direction.OrthonormalUp.V2) world
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
                                    // A revolute joint is like a hinge or pin, where two bodies rotate about a common point.
                                    // In this case, the bottom center point of body A shares the same position
                                    // as the top center point of body B, where they can rotate freely relative to each other.
                                    RevoluteJoint (a, b, new _(0f, -0.5f * toPhysics boxHeight), new _(0f, 0.5f * toPhysics boxHeight), false) }] world
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
                        [Entity.Position .= spawnCenter + v3 x y 0f
                         Entity.Size .= v3 64f 8f 0f
                         // Kinematic physics does not react to forces or collisions, but can be moved by setting its velocity (here angular).
                         Entity.BodyType .= Kinematic
                         Entity.AngularVelocity @= v3 0f 0f 10f
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
                        [Entity.Position .= spawnCenter + v3 x y 0f
                         Entity.Rotation .= Quaternion.CreateFromAngle2d MathF.PI_OVER_2 // Rotate 90 degrees
                         Entity.Size .= v3 64f 8f 0f
                         Entity.CollisionCategories .= "10"
                         Entity.CollisionMask .= "01"
                         Entity.StaticImage .= Assets.Default.Label
                         // Mouse dragging stops its movement, force angular velocity after dragging
                         Entity.AngularVelocity @= v3 0f 0f 10f
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
                            { CreateTwoBodyJoint = fun _ _ a b ->
                                // A weld joint disallows changing relative position and rotation between two bodies.
                                // However, being a soft constraint, it may still deform with a heavy external force.
                                // If deforming is unwanted, the body shapes should be within same entity instead.
                                WeldJoint (a, b, a.Position, b.Position, true) }] world
                ()
            // DYNAMIC BODIES - Clamp [Revolute joint, Angle joint]
            | Clamp ->
                // Declare center ball
                let ballSize = 32f
                let _ = World.doBall2d name
                            [Entity.Position .= spawnCenter
                             Entity.Size .= v3 ballSize ballSize 0f] world
                // Declare legs
                for (directionName, direction) in [("Left", -1f); ("Right", 1f)] do
                    let upperLeg = $"{name} {directionName} Upper leg"
                    for (newLeg, linkTo, image, angle) in
                        [(upperLeg, name, Assets.Default.Image, 0.2f)
                         ($"{name} {directionName} Lower leg", upperLeg, Assets.Default.Black, 0.4f)] do
                        let legLength = 30f
                        let _ =
                            World.doBox2d newLeg
                                [Entity.StaticImage .= image
                                 Entity.Position .= spawnCenter
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
                        let isExtended = world.ClockTime % 10f >= 5f
                        let _ =
                            World.doBodyJoint2d $"""{newLeg} Angle joint {isExtended}"""
                                [Entity.BodyJointTarget .= Relation.makeFromString $"^/{linkTo}"
                                 Entity.BodyJointTarget2Opt .= Some (Relation.makeFromString $"^/{newLeg}")
                                 Entity.BodyJoint .= TwoBodyJoint2d
                                    { CreateTwoBodyJoint = fun _ _ a b ->
                                        // An angle joint links the rotation of two bodies together,
                                        // optionally specifying the target angle (difference in rotation).
                                        AngleJoint (a, b, MaxImpulse = 3f,
                                            TargetAngle = (angle + if isExtended then 1f else 0f) * direction) }] world
                        ()
            // DYNAMIC BODIES - Ragdoll [Distance joint, Revolute joint]
            | Ragdoll ->
                let ballY = 60f
                let ballSize = 20f
                let _ =
                    World.doBall2d $"{name} Head"
                        [Entity.Position .= spawnCenter + v3 0f 60f 0f
                         Entity.Size .= v3 ballSize ballSize 0f
                         Entity.AngularDamping .= 2f
                         Entity.Substance .= Mass 2f] world
                let torsoWidth = 40f
                let torsoHeight = torsoWidth / 2f
                for (i, componentName, connectsTo, revoluteAngle) in
                    [1f, "Torso upper", "Head", None
                     2f, "Torso middle", "Torso upper", Some (MathF.PI / 8f)
                     3f,  "Torso lower", "Torso middle", Some (MathF.PI / 16f)] do
                    let _ =
                        World.doBall2d $"{name} {componentName}"
                            [Entity.BodyShape .= CapsuleShape
                                { Height = 0.5f; Radius = 0.25f; PropertiesOpt = None
                                  // Capsule shapes are vertical by default. To get a horizontal capsule, we apply a 90 degrees rotation.
                                  // Moreover, since height here is relative to entity height, we also need to scale it by 2 to use entity width instead.
                                  TransformOpt = Some (Affine.make v3Zero (Quaternion.CreateFromAngle2d MathF.PI_OVER_2) (v3Dup 2f)) }
                             Entity.Size .= v3 torsoWidth torsoHeight 0f
                             Entity.Position .= spawnCenter + v3 0f (ballY - i * torsoHeight) 0f
                             Entity.StaticImage .= Assets.Gameplay.Capsule] world
                    let _ =
                        World.doBodyJoint2d $"{name} {connectsTo}<->{componentName}"
                            [Entity.BodyJointTarget .= Relation.makeFromString $"^/{name} {connectsTo}"
                             Entity.BodyJointTarget2Opt .= Some (Relation.makeFromString $"^/{name} {componentName}")
                             Entity.BodyJoint .= TwoBodyJoint2d
                                { CreateTwoBodyJoint = fun toPhysics _ a b ->
                                    match revoluteAngle with
                                    | Some revoluteAngle ->
                                        RevoluteJoint(a, b, new _(0f, -0.55f * toPhysics torsoHeight), new _(0f, 0.55f * toPhysics torsoHeight), false,
                                                      LimitEnabled = true, LowerLimit = -revoluteAngle, UpperLimit = revoluteAngle)
                                    | _ ->
                                        DistanceJoint (a, b, new _(0f, -0.5f * toPhysics torsoHeight), new _(0f, 0.5f * toPhysics torsoHeight), false,
                                                       Length = toPhysics 1f, Frequency = 25f, DampingRatio = 1f)
                                }] world
                    ()
                let armWidth = 30f
                let armHeight = armWidth / 2f
                for (side, direction) in ["Left", -1f; "Right", 1f] do
                    for (pos1, posIncrement, rotation, armOrLeg, connectsToTorso) in
                        [v3 (direction * torsoWidth) (ballY - ballSize / 2f - torsoHeight / 2f) 0f, v3 (direction * armWidth) 0f 0f, 0f, "arm", "upper"
                         v3 (direction * torsoWidth / 4f) (ballY - ballSize / 2f - 3f * torsoHeight - armHeight) 0f, v3 0f -armWidth 0f, MathF.PI_OVER_2, "leg", "lower"] do
                    for (pos, upperOrLower, connectsTo) in
                        [pos1, "upper", $"Torso {connectsToTorso}"
                         pos1 + posIncrement, "lower", $"{side} {armOrLeg} upper"] do
                    let componentName = $"{side} {armOrLeg} {upperOrLower}"
                    let _ =
                        World.doBall2d $"{name} {componentName}"
                            [Entity.BodyShape .= CapsuleShape
                                { Height = 0.5f; Radius = 0.25f; PropertiesOpt = None
                                  TransformOpt = Some (Affine.make v3Zero (Quaternion.CreateFromAngle2d MathF.PI_OVER_2) (v3Dup 2f)) }
                             Entity.Position .= spawnCenter + pos
                             Entity.Rotation .= Quaternion.CreateFromAngle2d rotation
                             Entity.Size .= v3 armWidth armHeight 0f
                             Entity.StaticImage .= Assets.Gameplay.Capsule] world
                    let _ =
                        World.doBodyJoint2d $"{name} {connectsTo}<->{componentName}"
                            [Entity.BodyJointTarget .= Relation.makeFromString $"^/{name} {connectsTo}"
                             Entity.BodyJointTarget2Opt .= Some (Relation.makeFromString $"^/{name} {componentName}")
                             Entity.BodyJoint .= TwoBodyJoint2d
                                { CreateTwoBodyJoint = fun toPhysics toPhysicsV2 a b ->
                                    let jointPosition = toPhysicsV2 (pos - posIncrement / 2f)
                                    DistanceJoint (a, b, jointPosition, jointPosition, true, Length = toPhysics 4f, Frequency = 25f, DampingRatio = 1f)
                                }] world
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
                        [Entity.Position .= spawnCenter + v3 spawnX spawnY 0f
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
                            [Entity.Position .= spawnCenter + v3 x y 0f
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
            // DYNAMIC BODIES - Web of Goo [Distance joint]
            // Body joints can break under stress - distance joints used here for example.
            | Web ->
                let numSides = 12
                let innerRadius = 30f
                let incrementalRadius = 12f
                let numLayers = 5
                let gooMass = 0.1f
                let spawnPositions =
                    Array.init numLayers (fun layer ->
                        nkast.Aether.Physics2D.Common.PolygonTools.CreateCircle(innerRadius + float32 layer * incrementalRadius, numSides)
                            .ConvertAll(fun p -> v3 p.X p.Y 0f))
                let spawnPositionToName (position : Vector3) =
                    $"{name} {position.X} {position.Y}"
                // declare goos
                for layer in 0 .. dec numLayers do
                    for vertex in 0 .. dec numSides do
                    let gooSpawnPosition = spawnPositions[layer][vertex]
                    let _ =
                        World.doBall2d (spawnPositionToName gooSpawnPosition)
                            [Entity.StaticImage .= Assets.Gameplay.Goo
                             Entity.Position .= spawnCenter + gooSpawnPosition
                             Entity.Size .= v3Dup 8f
                             Entity.Substance .= Mass gooMass
                             if layer = dec numLayers then
                                Entity.Visible .= false
                                Entity.BodyType .= Static
                                Entity.Sensor .= true] world
                    ()
                // declare links
                for layer in 0 .. dec numLayers do
                    for vertex in 0 .. dec numSides do
                    let gooName = spawnPositionToName (spawnPositions[layer][vertex])
                    let gooPosition = (world.ContextGroup / gooName).GetPosition world
                    for (linkRelation, otherGooSpawnPosition) in
                        [if layer < dec numLayers then
                            ("Previous", spawnPositions[layer][if vertex = 0 then dec numSides else dec vertex])
                         if layer > 0 then
                            ("Inner", spawnPositions[dec layer][vertex])] do
                        let otherGooName = spawnPositionToName otherGooSpawnPosition
                        let otherGooPosition = (world.ContextGroup / otherGooName).GetPosition world
                        let _ =
                            World.doBodyJoint2d $"{gooName} -> {linkRelation}"
                                [Entity.BodyJointTarget .= Relation.makeFromString $"^/{otherGooName}"
                                 Entity.BodyJointTarget2Opt .= Some (Relation.makeFromString $"^/{gooName}")
                                 Entity.BreakingPoint .= 10000f * gooMass
                                 Entity.BodyJoint .= TwoBodyJoint2d
                                    { CreateTwoBodyJoint = fun _ _ a b ->
                                        // Setting the Breakpoint property here in the joint constructor won't work
                                        // as the default value of Entity.BreakingPoint (10000f) will override it
                                        DistanceJoint (a, b, new _(0f, 0f), new _(0f, 0f), false,
                                            DampingRatio = 0.5f, Frequency = 1f / gooMass * if layer = dec numLayers then 4f else 2f) }] world
                        // visualize link
                        if not (world.DeclaredEntity.GetBroken world) then
                            let direction = otherGooPosition - gooPosition
                            let _ =
                                World.doStaticSprite $"{gooName} -> {linkRelation} visual"
                                    [Entity.Position @= (otherGooPosition + gooPosition) / 2f
                                     Entity.Size @= v3 direction.Magnitude 2f 0f
                                     Entity.Rotation @= Quaternion.CreateLookAt2d direction.V2
                                     Entity.StaticImage .= Assets.Gameplay.Link
                                     // Don't let the link sprite draw over the goo
                                     Entity.Elevation .= -0.5f] world
                            ()
                ()
            // DYNAMIC BODIES - Theo Jansen Walker - https://strandbeest.com/ [Distance joint]
            // The motor alone controls the entire body's movement!
            | Strandbeest ->
                let objectScale = 10f
                let density = Density (1f / objectScale ** 2f)
                let pivot = v3 0f 0.8f 0f
                let wheelAnchor = v3 0f -0.8f 0f
                World.doBox2d $"{name} Chassis" [
                    Entity.Substance .= density
                    Entity.Position .= spawnCenter + pivot * objectScale
                    Entity.Size .= v3 5f 2f 0f * objectScale
                    Entity.Elevation .= -0.7f] world |> ignore
                let chassis = world.DeclaredEntity
                World.doBall2d $"{name} Wheel" [
                    Entity.Substance .= density
                    Entity.Position .= spawnCenter + pivot * objectScale
                    Entity.Size .= v3Dup 3.2f * objectScale
                    Entity.Elevation .= -0.5f
                    ] world |> ignore
                let wheel = world.DeclaredEntity
                World.doBodyJoint2d $"{name} Motor" [
                    Entity.BodyJointTarget .= Relation.makeFromString $"^/{name} Wheel"
                    Entity.BodyJointTarget2Opt .= Some (Relation.makeFromString $"^/{name} Chassis")
                    Entity.BodyJoint .= TwoBodyJoint2d { CreateTwoBodyJoint = fun _ _ a b ->
                        // HACK: This is actually initialization of chassis and wheel CollisionGroup, this Aether property isn't exposed by Nu.
                        a.SetCollisionGroup -1s
                        b.SetCollisionGroup -1s
                        // Specifying a motor for the revolute joint rotates the first body with a constant angular velocity.
                        RevoluteJoint (a, b, b.Position, true, MotorEnabled = true, MotorSpeed = 2f, MaxMotorTorque = 400f) }
                    Entity.CollideConnected .= false] world |> ignore
                for rotation in [-1f; 0f; 1f] do
                    for (directionName, direction) in [("left", -1f); ("right", 1f)] do
                        let p1 = v3 (direction * 5.4f) -6.1f 0f
                        let p2 = v3 (direction * 7.2f) -1.2f 0f
                        let p3 = v3 (direction * 4.3f) -1.9f 0f
                        let p4 = v3 (direction * 3.1f) 0.8f 0f
                        let p5 = v3 (direction * 6.0f) 1.5f 0f
                        let p6 = v3 (direction * 2.5f) 3.7f 0f
                        let legPolygon = if direction > 0f then [|p1; p2; p3|] else [|p1; p3; p2|]
                        let shoulderPolygon =
                            if direction > 0f then [|v3Zero; p5 - p4; p6 - p4|] else [|v3Zero; p6 - p4; p5 - p4|]
                        World.doBox2d $"{name} {directionName} {rotation} Leg" [
                            Entity.Substance .= density
                            Entity.Position .= spawnCenter
                            Entity.Size .= v3Dup objectScale
                            Entity.BodyShape .= PointsShape
                                { Points = legPolygon; Profile = Convex; TransformOpt = None; PropertiesOpt = None }
                            Entity.AngularDamping .= 10f
                            Entity.Visible .= false] world |> ignore
                        let leg = world.DeclaredEntity
                        let legTransform = leg.GetTransform(world).AffineMatrix
                        legPolygon
                        |> Array.add legPolygon[0]
                        |> Array.map ((*) objectScale)
                        |> Array.pairwise
                        |> Array.iter (fun (p1, p2) ->
                            World.doStaticSprite $"{name} {directionName} {rotation} Leg render {p1} -> {p2}"
                                [let p1 = p1.Transform legTransform
                                 let p2 = p2.Transform legTransform
                                 Entity.Position @= (p1 + p2) / 2f
                                 Entity.Size @= v3 (p2 - p1).Magnitude 2f 0f
                                 Entity.Rotation @= Quaternion.CreateLookAt2d (p2 - p1).V2
                                 Entity.StaticImage .= Assets.Default.Black] world
                        )
                        World.doBox2d $"{name} {directionName} {rotation} Shoulder" [
                            Entity.Substance .= density
                            Entity.Position .= spawnCenter + p4 * objectScale
                            Entity.Size .= v3Dup objectScale
                            Entity.BodyShape .= PointsShape
                                { Points = shoulderPolygon; Profile = Convex; TransformOpt = None; PropertiesOpt = None }
                            Entity.AngularDamping .= 10f
                            Entity.Visible .= false] world |> ignore
                        let shoulder = world.DeclaredEntity
                        let shoulderTransform = shoulder.GetTransform(world).AffineMatrix
                        shoulderPolygon
                        |> Array.add shoulderPolygon[0]
                        |> Array.map ((*) objectScale)
                        |> Array.pairwise
                        |> Array.iter (fun (p1, p2) ->
                            World.doStaticSprite $"{name} {directionName} {rotation} Shoulder render {p1} -> {p2}"
                                [let p1 = p1.Transform shoulderTransform
                                 let p2 = p2.Transform shoulderTransform
                                 Entity.Position @= (p1 + p2) / 2f
                                 Entity.Size @= v3 (p2 - p1).Magnitude 2f 0f
                                 Entity.Rotation @= Quaternion.CreateLookAt2d (p2 - p1).V2
                                 Entity.StaticImage .= Assets.Default.Black] world
                        )
                        // Using a soft distance joint can reduce some jitter.
                        // It also makes the structure seem a bit more fluid by
                        // acting like a suspension system.
                        for (i, (entity1, entity2, position1, position2, entity1SpawnPosition, entity2SpawnPosition)) in
                            List.indexed [(leg, shoulder, p2, p5, v3Zero, p4)
                                          (leg, shoulder, p3, p4, v3Zero, p4)
                                          (leg, wheel, p3, pivot + wheelAnchor, v3Zero, (-wheelAnchor).Transform (Quaternion.CreateFromAngle2d (-rotation * 2f * MathF.PI_OVER_3)))
                                          (shoulder, wheel, p6, pivot + wheelAnchor, p4, (-wheelAnchor).Transform (Quaternion.CreateFromAngle2d (-rotation * 2f * MathF.PI_OVER_3)))
                                          ] do
                            World.doBodyJoint2d $"{name} {directionName} {rotation} Distance joint {i}"
                                [Entity.BodyJoint .= TwoBodyJoint2d { CreateTwoBodyJoint = fun _ toPhysicsV2 a b ->
                                    if i = 0 then
                                        // HACK: This is actually initialization of leg and shoulder CollisionGroup, this Aether property isn't exposed by Nu.
                                        a.SetCollisionGroup -1s
                                        b.SetCollisionGroup -1s
                                        // HACK: The Aether demo uses mutable rotations of the wheel when initializing, doing it here won't screw up the joint distances.
                                        wheel.SetRotation (Quaternion.CreateFromAngle2d (rotation * 2f * MathF.PI_OVER_3)) world
                                    DistanceJoint (a, b, toPhysicsV2 (position1 * objectScale + spawnCenter), toPhysicsV2 (position2 * objectScale + spawnCenter), true,
                                        Frequency = 10f, DampingRatio = 0.5f) }
                                 Entity.BodyJointTarget .= Relation.relate world.DeclaredEntity.EntityAddress entity1.EntityAddress
                                 Entity.BodyJointTarget2Opt .= Some (Relation.relate world.DeclaredEntity.EntityAddress entity2.EntityAddress)
                                 Entity.CollideConnected .= false] world |> ignore
                            World.doStaticSprite $"{name} {directionName} {rotation} Distance joint render {i}"
                                [let p1 = (position1 * objectScale - entity1SpawnPosition * objectScale).Transform (entity1.GetTransform(world).AffineMatrix)
                                 let p2 = (position2 * objectScale - entity2SpawnPosition * objectScale).Transform (entity2.GetTransform(world).AffineMatrix)
                                 Entity.Position @= (p1 + p2) / 2f
                                 Entity.Size @= v3 (p2 - p1).Magnitude 2f 0f
                                 Entity.Rotation @= Quaternion.CreateLookAt2d (p2 - p1).V2
                                 Entity.StaticImage .= Assets.Gameplay.Link
                                 Entity.Color .= color 1f 1f 1f 0.2f
                                 Entity.Elevation .= -0.6f] world
                        World.doBodyJoint2d $"{name} {directionName} {rotation} Revolute joint"
                            [Entity.BodyJoint .= TwoBodyJoint2d { CreateTwoBodyJoint = fun _ toPhysicsV2 a b ->
                                RevoluteJoint (a, b, toPhysicsV2 (p4 * objectScale + spawnCenter), true) }
                             Entity.BodyJointTarget .= Relation.relate world.DeclaredEntity.EntityAddress shoulder.EntityAddress
                             Entity.BodyJointTarget2Opt .= Some (Relation.relate world.DeclaredEntity.EntityAddress chassis.EntityAddress)
                             Entity.CollideConnected .= false] world |> ignore
            // Some joints, like the revolute joint, can cause chaos with poor parameters :)
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
                            [Entity.Position .= spawnCenter + v3 x y 0f
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

        World.endGroup world