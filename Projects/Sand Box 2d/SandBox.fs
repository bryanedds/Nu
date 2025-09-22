namespace SandBox2d
open System
open System.Diagnostics
open System.Numerics
open Prime
open Nu
open nkast.Aether.Physics2D
open nkast.Aether.Physics2D.Dynamics.Joints

/// A physics 'toy' that can be spawned into the world.
type Toy =
    | Box
    | Ball
    | TinyBalls
    | Spring
    | Block
    | Bridge
    | Fan
    | Clamp
    | Ragdoll
    | SoftBody
    | Web
    | Strandbeest

/// The different pages of the sandbox menu.
type Page =
    | Page1
    | Page2

// this extends the Screen API to expose the user-defined properties.
[<AutoOpen>]
module SandBoxExtensions =
    type Screen with
        member this.GetToys world : FMap<string, Toy> = this.Get (nameof Screen.Toys) world
        member this.SetToys (value : FMap<string, Toy>) world = this.Set (nameof Screen.Toys) value world
        member this.Toys = lens (nameof Screen.Toys) this this.GetToys this.SetToys
        member this.GetDraggedEntity world : (Entity * Vector3 * BodyType) option = this.Get (nameof Screen.DraggedEntity) world
        member this.SetDraggedEntity (value : (Entity * Vector3 * BodyType) option) world = this.Set (nameof Screen.DraggedEntity) value world
        member this.DraggedEntity = lens (nameof Screen.DraggedEntity) this this.GetDraggedEntity this.SetDraggedEntity
        member this.GetMouseDragTarget world : FMap<Entity, Entity> = this.Get (nameof Screen.MouseDragTarget) world
        member this.SetMouseDragTarget (value : FMap<Entity, Entity>) world = this.Set (nameof Screen.MouseDragTarget) value world
        member this.MouseDragTarget = lens (nameof Screen.MouseDragTarget) this this.GetMouseDragTarget this.SetMouseDragTarget
        member this.GetSoftBodyContour world : FMap<BodyId, Entity> = this.Get (nameof Screen.SoftBodyContour) world
        member this.SetSoftBodyContour (value : FMap<BodyId, Entity>) world = this.Set (nameof Screen.SoftBodyContour) value world
        member this.SoftBodyContour = lens (nameof Screen.SoftBodyContour) this this.GetSoftBodyContour this.SetSoftBodyContour
        member this.GetPage world : Page = this.Get (nameof Screen.Page) world
        member this.SetPage (value : Page) world = this.Set (nameof Screen.Page) value world
        member this.Page = lens (nameof Screen.Page) this this.GetPage this.SetPage
        member this.GetCreditsOpened world : bool = this.Get (nameof Screen.CreditsOpened) world
        member this.SetCreditsOpened (value : bool) world = this.Set (nameof Screen.CreditsOpened) value world
        member this.CreditsOpened = lens (nameof Screen.CreditsOpened) this this.GetCreditsOpened this.SetCreditsOpened
        
// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type SandBoxDispatcher () =
    inherit ScreenDispatcherImSim ()

    static let processMouseDragging (sandBox : Screen) world =

        // pick the entity to drag
        // In Nu, while the physics engine subsystems abstract over 2d and 3d to conform
        // to a 3d interface using Vector3, mouse and spatial subsystems have no utility
        // for such abstraction - they need more specificity. As a result, getMousePostion2dWorld
        // return Vector2, getEntities2dAtPoint takes Vector2, while Entity.Position and
        // rayCastBodies2d expect Vector3. .V3 is used to convert Vector2 to Vector3, .V2 for vice versa.
        // That's the art of API design - sometimes the types are more specific and concrete,
        // while sometimes the types are less specific because it keeps things more general.
        let mousePosition = (World.getMousePosition2dWorld false world).V3
        let setDraggedEntity (entity : Entity) =
            let relativePosition = (mousePosition - entity.GetPosition world).Transform (entity.GetRotation world).Inverted
            sandBox.SetDraggedEntity (Some (entity, relativePosition, entity.GetBodyType world)) world
            entity.SetBodyType Dynamic world // Only dynamic bodies react to forces by the mouse joint below.
        if World.isMouseButtonPressed MouseLeft world then

            // Optimizations can reuse the same set for different queries.
            let physicsAnchors = sandBox.GetMouseDragTarget world
            for entity in World.getEntities2dAtPoint mousePosition.V2 (hashSetPlus HashIdentity.Structural []) world do
                let entity = FMap.tryFind entity physicsAnchors |> Option.defaultValue entity
                // Check rigid body facet existence to confirm the body type property's validity before reading it
                if  entity.Has<RigidBodyFacet> world &&
                    entity.GetVisible world && // Don't drag invisible entities - relevant for Strandbeest shoulder and legs
                    entity.Name <> "Border" &&
                    sandBox.GetDraggedEntity world = None // Don't change more than one body to dynamic physics
                then setDraggedEntity entity
            if sandBox.GetDraggedEntity world = None then // No entity found via direct point test

                // Raycast entities to see if mouse location is inside a soft body enclosed area, then drag it
                let rayUp =
                    World.rayCastBodies2d (ray3 mousePosition (v3Up * 100f)) -1 false world
                    |> Seq.map _.BodyShapeIntersected.BodyId
                    |> Seq.choose (sandBox.GetSoftBodyContour world).TryFind
                    |> Set
                let rayDown =
                    World.rayCastBodies2d (ray3 mousePosition (v3Down * 100f)) -1 false world
                    |> Seq.map _.BodyShapeIntersected.BodyId
                    |> Seq.choose (sandBox.GetSoftBodyContour world).TryFind
                    |> Set
                let intersection = Set.intersect rayUp rayDown
                if Set.notEmpty intersection then
                    setDraggedEntity (Set.minElement intersection)

        // moving the picked entity
        match sandBox.GetDraggedEntity world with
        | Some (draggedEntity, _, draggedBodyType) when World.isMouseButtonUp MouseLeft world ->
            sandBox.SetDraggedEntity None world
            draggedEntity.SetBodyType draggedBodyType world
        | Some (draggedEntity, relativePosition, draggedBodyType) ->

            // declare sensor for mouse body
            World.doSphere2d "MouseSensor" // A sphere uses static physics by default.
                [Entity.BodyShape .= SphereShape
                    { Radius = 0.1f
                      PropertiesOpt = Some { BodyShapeProperties.empty with SensorOpt = Some true } // A sensor body never collides with another body.
                      TransformOpt = None }
                 Entity.Visible .= false
                 Entity.Position @= mousePosition] world |> ignore
            let mouseSensor = world.DeclaredEntity

            // declare distance joint for mouse body
            let mouseJoint = world.ContextGroup / "MouseJoint"
            World.doBodyJoint2d mouseJoint.Name
                // A relative address can be specified by relating two entities directly using their EntityAddresses.
                // Relative addresses can safeguard against dragging entities across different hierarchies in the Gaia editor.
                [Entity.BodyJointTarget .= Address.relate mouseJoint.EntityAddress draggedEntity.EntityAddress
                 // Though, in code, we usually can just specify the absolute address directly.
                 Entity.BodyJointTarget2Opt .= Some mouseSensor.EntityAddress
                 Entity.BreakingPoint .= infinityf // never drop the entity while dragging
                 Entity.BodyJoint .= TwoBodyJoint2d { CreateTwoBodyJoint = fun _ toPhysicsV2 a b ->
                    // Convert mouse position (Vector2) to world position (Vector3) to physics engine position (Aether.SandBox2d Vector2)
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

    static let processMouseScrolling (sandBox : Screen) world =
        let mousePosition = (World.getMousePosition2dWorld false world).V3
        for event in World.doSubscription "MouseWheel" Game.MouseWheelEvent world do
            let physicsAnchors = sandBox.GetMouseDragTarget world
            for entity in World.getEntities2dAtPoint mousePosition.V2 (hashSetPlus HashIdentity.Structural []) world do
                let entity = FMap.tryFind entity physicsAnchors |> Option.defaultValue entity
                if entity.Has<RigidBodyFacet> world && entity.Name <> "Border" then
                    World.applyBodyTorque (v3 0f 0f 40f * event.Travel) (entity.GetBodyId world) world

    static let declareBox name spawnCenter world =
        World.doBox2d name // unlike a block, a box uses dynamic physics by default - it reacts to forces and collisions.
            [Entity.Restitution .= 0.333f
             Entity.Color .= color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f
             Entity.Position .= spawnCenter + v3 Gen.randomf Gen.randomf 0f] world |> ignore

    static let declareBall name spawnCenter world =
        World.doBall2d name // unlike a sphere, a ball uses dynamic physics by default.
            [Entity.Restitution .= 0.5f // bouncier than default
             Entity.Color .= color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f
             Entity.Position .= spawnCenter + v3 Gen.randomf Gen.randomf 0f] world |> ignore

    static let declareTinyBalls name spawnCenter world =
        for i in 0 .. dec 16 do
            World.doBall2d $"{name} Ball {i}"
                [Entity.Restitution .= 0.5f // bouncier than default
                 Entity.Size .= Constants.Engine.Entity2dSizeDefault / 4f
                 Entity.Substance .= Mass (1f / 16f) // Make tiny balls have tiny mass when colliding
                 Entity.Color .= color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f
                 Entity.Position .= spawnCenter + v3 Gen.randomf Gen.randomf 0f] world |> ignore

    static let declareSpring name spawnCenter world =

        // In Nu, each entity can contain arbitrarily many child entities.
        // Each entity mounts, aka inherits transforms (position, rotation, scale) from,
        // their parent by default. This can be changed by setting the Entity.MountOpt property.
        // Mounting only works for entities that are not rigid bodies because it makes no sense
        // for both the physics engine and mounted entity to specify transforms together.
        // Here, we show how to point to child entities using relative addresses.

        let color = color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f
        World.beginEntity<BodyJoint2dDispatcher> name
            [Entity.BodyJointTarget .= Address.makeFromString "~/Face 1" // Points to child entity
             Entity.BodyJointTarget2Opt .= Some (Address.makeFromString "~/Face 2")
             Entity.BodyJoint .= TwoBodyJoint2d { CreateTwoBodyJoint = fun toPhysics _ a b ->
                // A distance joint maintains fixed distance between two bodies, optionally with spring-like behaviour.
                // It does not impose limits on relative positions or rotations.
                DistanceJoint (a, b, new _ (0f, 0f), new _ (0f, 0f), false, Length = toPhysics 60f, Frequency = 5f, DampingRatio = 0.3f) }] world

        World.doBox2d "Face 1" // Pointed to by parent body joint
            [Entity.Color .= color
             Entity.Position .= spawnCenter
             Entity.Size .= v3 150f 10f 0f
             Entity.StaticImage .= Assets.Default.Paddle
             Entity.Substance .= Mass (1f / 2f)] world |> ignore
        let box1 = world.DeclaredEntity

        World.doBox2d "Face 2"
            [Entity.Color .= color
             Entity.Position .=spawnCenter + v3 0f -60f 0f
             Entity.Size .= v3 150f 10f 0f
             Entity.StaticImage .= Assets.Default.Paddle
             Entity.Substance .= Mass (1f / 2f)] world |> ignore
        let box2 = world.DeclaredEntity

        let direction = box2.GetPosition world - box1.GetPosition world
        World.doStaticSprite "Joint visual"
            [Entity.Position @= (box1.GetPosition world + box2.GetPosition world) / 2f
             Entity.Size @= v3 direction.Magnitude 1f 0f
             Entity.Rotation @= Quaternion.CreateLookAt2d direction.V2
             Entity.Color .= color.WithA 0.5f
             Entity.StaticImage .= Assets.Default.White
             Entity.Elevation .= 0.5f] world |> ignore

        World.doBodyJoint2d "Prismatic joint"
            // A relative address can also point to parent using ^
            [Entity.BodyJointTarget .= Address.makeFromString "^/Face 1"
             Entity.BodyJointTarget2Opt .= Some (Address.makeFromString "^/Face 2")
             Entity.BodyJoint .= TwoBodyJoint2d { CreateTwoBodyJoint = fun _ toPhysicsV2 a b ->
                // A prismatic joint maintains fixed position between two bodies to move linearly
                // along a straight axis while disallowing relative rotation, without fixing distance.
                PrismaticJoint (a, b, new _ (0f, 0f), toPhysicsV2 direction, useWorldCoordinates=false) }] world |> ignore

        World.endEntity world

    static let declareBlock name spawnCenter world =
        World.doBlock2d name
            [Entity.Position .= spawnCenter + v3 (Gen.randomf1 500f - 250f) (Gen.randomf1 350f - 175f) 0f // random placement
             Entity.StaticImage .= Assets.Default.Brick] world |> ignore

    static let declareBridge name spawnCenter world =
        let x = Gen.randomf1 500f - 250f
        let y = Gen.randomf1 350f - 175f
        World.doSphere2d name [Entity.Position .= spawnCenter + v3 x y 0f] world |> ignore
        let anchor1 = world.DeclaredEntity
        World.doSphere2d $"{name} Opposite end" [Entity.Position .= spawnCenter + v3 x y 0f] world |> ignore
        let anchor2 = world.DeclaredEntity
        let direction = anchor1.GetPosition world - anchor2.GetPosition world
        if direction <> v3Zero then // OrthonormalUp for <0, 0, 0> is <nan, nan, nan>, so avoid it
            for entity in [anchor1; anchor2] do
                // Adjust position of link relative to each anchor as the anchors are dragged around
                entity.SetRotation (Quaternion.CreateLookAt2d direction.OrthonormalUp.V2) world
        let names = Array.init 6 (sprintf "%s Paddle %d" name)
        let boxHeight = direction.Length () / single (Array.length names)
        for i in 0 .. Array.length names - 1 do
            World.doBox2d names[i]
                [Entity.Size @= v3 4f boxHeight 0f
                 Entity.StaticImage .= Assets.Default.Paddle
                 // The paddles are thin, use continuous collision detection to prevent tunnelling at high velocities
                 Entity.CollisionDetection .= Continuous] world |> ignore
        for (n1, n2) in Array.pairwise [|anchor1.Name; yield! names; anchor2.Name|] do
            World.doBodyJoint2d $"{n2} Link"
                [Entity.BodyJointTarget .= Address.makeFromString $"^/{n1}"
                 Entity.BodyJointTarget2Opt .= Some (Address.makeFromString $"^/{n2}")
                 Entity.BodyJoint @= TwoBodyJoint2d { CreateTwoBodyJoint = fun toPhysics _ a b ->
                    // A revolute joint is like a hinge or pin, where two bodies rotate about a common point.
                    // In this case, the bottom center point of body A shares the same position
                    // as the top center point of body B, where they can rotate freely relative to each other.
                    RevoluteJoint (a, b, new _ (0f, -0.5f * toPhysics boxHeight), new _ (0f, 0.5f * toPhysics boxHeight), false) }] world |> ignore

    static let declareFan name spawnCenter (sandBox : Screen) world =

        // A fan is made of two rectangular blocks (blades) welded together at the center with a weld body joint.
        // One is the blades is set as the "anchor", which is kinematic and is the actual entity dragged by mouse.
        let x = Gen.randomf1 500f - 250f
        let y = Gen.randomf1 350f - 175f
        // Declare anchor
        World.doBlock2d name
            [Entity.Position .= spawnCenter + v3 x y 0f
             Entity.Size .= v3 64f 8f 0f
             // Kinematic physics does not react to forces or collisions, but can be moved by setting its velocity (here angular).
             Entity.BodyType .= Kinematic
             Entity.AngularVelocity @= v3 0f 0f 10f
             // Set fans to be treated the same way as borders when colliding with other fans using the same collision category as border.
             Entity.CollisionCategories .= "10"
             // Fans collide with entities in the default collision category "1",
             // not with the border or other fans in category "10",
             // but collides with strandbeest bodies in category "100".
             // otherwise the + shape of the fan deforms when dragging next to another fan or the border.
             Entity.CollisionMask .= "101"
             Entity.StaticImage .= Assets.Default.Label] world |> ignore
        let anchor = world.DeclaredEntity

        // Declare other blade
        World.doBox2d $"{name} Other blade"
            [Entity.Position .= spawnCenter + v3 x y 0f
             Entity.Rotation .= Quaternion.CreateFromAngle2d MathF.PI_OVER_2 // Rotate 90 degrees
             Entity.Size .= v3 64f 8f 0f
             Entity.CollisionCategories .= "10"
             Entity.CollisionMask .= "101"
             Entity.StaticImage .= Assets.Default.Label
             // Mouse dragging stops its movement, force angular velocity after dragging
             Entity.AngularVelocity @= v3 0f 0f 10f
             // Don't keep linear velocity from collisions on mouse drag release
             Entity.LinearVelocity @= v3Zero] world |> ignore
        let blade = world.DeclaredEntity
        sandBox.MouseDragTarget.Map (FMap.add blade anchor) world

        // Declare weld joint to link the two blades together at the center point (x, y)
        World.doBodyJoint2d $"{name} Weld joint"
            [Entity.BodyJointTarget .= anchor.EntityAddress
             Entity.BodyJointTarget2Opt .= Some blade.EntityAddress
             Entity.CollideConnected .= false // When the two blades are set to collide, the + shape would deform on drag
             Entity.BreakingPoint .= infinityf
             Entity.BodyJoint .= TwoBodyJoint2d { CreateTwoBodyJoint = fun _ _ a b ->
                // A weld joint disallows changing relative position and rotation between two bodies.
                // However, being a soft constraint, it may still deform with a heavy external force.
                // If deforming is unwanted, the body shapes should be within same entity instead.
                WeldJoint (a, b, a.Position, b.Position, true) }] world |> ignore

    static let declareClamp name spawnCenter world =

        // Declare center ball
        let ballSize = 32f
        World.doBall2d name
            [Entity.Position .= spawnCenter
             Entity.Size .= v3 ballSize ballSize 0f] world |> ignore

        // Declare legs
        for (directionName, direction) in [("Left", -1f); ("Right", 1f)] do
            let upperLeg = $"{name} {directionName} Upper leg"
            for (newLeg, linkTo, image, angle) in
                [(upperLeg, name, Assets.Default.Image, 0.2f)
                 ($"{name} {directionName} Lower leg", upperLeg, Assets.Default.Black, 0.4f)] do
                let legLength = 30f
                World.doBox2d newLeg
                    [Entity.StaticImage .= image
                     Entity.Position .= spawnCenter
                     Entity.Size .= v3 legLength 4f 0f] world |> ignore
                World.doBodyJoint2d $"{newLeg} Revolute joint"
                    [Entity.BodyJointTarget .= Address.makeFromString $"^/{linkTo}"
                     Entity.BodyJointTarget2Opt .= Some (Address.makeFromString $"^/{newLeg}")
                     Entity.CollideConnected .= false // Rotation movement would be limited if the upper leg collides with center
                     Entity.BodyJoint .= TwoBodyJoint2d { CreateTwoBodyJoint = fun _ toPhysicsV2 a b ->
                        let p = toPhysicsV2 (v3 (legLength * direction) 0f 0f)
                        RevoluteJoint (a, b, p * 0.5f, p * -0.5f, false) }] world |> ignore
                let isExtended =
                    world.ClockTime % 10f >= 5f
                let twoBodyJoint = TwoBodyJoint2d { CreateTwoBodyJoint = fun _ _ a b ->
                    // An angle joint links the rotation of two bodies together,
                    // optionally specifying the target angle (difference in rotation).
                    AngleJoint (a, b, MaxImpulse = 3f, TargetAngle = (angle + if isExtended then 1f else 0f) * direction) }
                World.doBodyJoint2d $"""{newLeg} Angle joint {isExtended}"""
                    [Entity.BodyJointTarget .= Address.makeFromString $"^/{linkTo}"
                     Entity.BodyJointTarget2Opt .= Some (Address.makeFromString $"^/{newLeg}")
                     Entity.BodyJoint .= twoBodyJoint] world |> ignore

    static let declareRagdoll name spawnCenter world =
    
        let ballY = 60f
        let ballSize = 20f
        World.doBall2d $"{name} Head"
            [Entity.Position .= spawnCenter + v3 0f 60f 0f
             Entity.Size .= v3 ballSize ballSize 0f
             Entity.AngularDamping .= 2f
             Entity.Substance .= Mass 2f] world |> ignore
        let torsoWidth = 40f
        let torsoHeight = torsoWidth / 2f
        for (i, componentName, connectsTo, revoluteAngle) in
            [1f, "Torso upper", "Head", None
             2f, "Torso middle", "Torso upper", Some (MathF.PI / 8f)
             3f,  "Torso lower", "Torso middle", Some (MathF.PI / 16f)] do
            World.doBall2d $"{name} {componentName}"
                [Entity.BodyShape .= CapsuleShape
                    { Height = 0.5f; Radius = 0.25f; PropertiesOpt = None
                      // Capsule shapes are vertical by default. To get a horizontal capsule, we apply a 90 degrees rotation.
                      // Moreover, since height here is relative to entity height, we also need to scale it by 2 to use entity width instead.
                      TransformOpt = Some (Affine.make v3Zero (Quaternion.CreateFromAngle2d MathF.PI_OVER_2) (v3Dup 2f)) }
                 Entity.Size .= v3 torsoWidth torsoHeight 0f
                 Entity.Position .= spawnCenter + v3 0f (ballY - i * torsoHeight) 0f
                 Entity.StaticImage .= Assets.Gameplay.Capsule] world |> ignore
            let twoBodyJoint = TwoBodyJoint2d { CreateTwoBodyJoint = fun toPhysics _ a b ->
                match revoluteAngle with
                | Some revoluteAngle ->
                    RevoluteJoint
                        (a, b, new _ (0f, -0.55f * toPhysics torsoHeight), new _ (0f, 0.55f * toPhysics torsoHeight), false,
                         LimitEnabled = true, LowerLimit = -revoluteAngle, UpperLimit = revoluteAngle)
                | _ ->
                    DistanceJoint
                        (a, b, new _ (0f, -0.5f * toPhysics torsoHeight), new _ (0f, 0.5f * toPhysics torsoHeight), false,
                         Length = toPhysics 1f, Frequency = 25f, DampingRatio = 1f) }
            World.doBodyJoint2d $"{name} {connectsTo}<->{componentName}"
                [Entity.BodyJointTarget .= Address.makeFromString $"^/{name} {connectsTo}"
                 Entity.BodyJointTarget2Opt .= Some (Address.makeFromString $"^/{name} {componentName}")
                 Entity.BodyJoint .= twoBodyJoint] world |> ignore

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
            World.doBall2d $"{name} {componentName}"
                [Entity.BodyShape .= CapsuleShape
                    { Height = 0.5f; Radius = 0.25f; PropertiesOpt = None
                      TransformOpt = Some (Affine.make v3Zero (Quaternion.CreateFromAngle2d MathF.PI_OVER_2) (v3Dup 2f)) }
                 Entity.Position .= spawnCenter + pos
                 Entity.Rotation .= Quaternion.CreateFromAngle2d rotation
                 Entity.Size .= v3 armWidth armHeight 0f
                 Entity.StaticImage .= Assets.Gameplay.Capsule] world |> ignore
            let twoBodyJoint = TwoBodyJoint2d { CreateTwoBodyJoint = fun toPhysics toPhysicsV2 a b ->
                let jointPosition = toPhysicsV2 (pos - posIncrement / 2f)
                DistanceJoint (a, b, jointPosition, jointPosition, true, Length = toPhysics 4f, Frequency = 25f, DampingRatio = 1f) }
            World.doBodyJoint2d $"{name} {connectsTo}<->{componentName}"
                [Entity.BodyJointTarget .= Address.makeFromString $"^/{name} {connectsTo}"
                 Entity.BodyJointTarget2Opt .= Some (Address.makeFromString $"^/{name} {componentName}")
                 Entity.BodyJoint .= twoBodyJoint] world |> ignore

    static let declareSoftBody name spawnCenter (sandBox : Screen) world =
                
        let color = color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f
        let boxNames = Array.init 32 (sprintf "%s Contour %d" name)
        let boxCount = single boxNames.Length
        let boxSize = 8f
        // This scale is large enough such that the soft body doesn't form knots,
        // but small enough to not spawn individual boxes outside the border.
        // The body joints will pull individual boxes together no matter how far apart the boxes spawn.
        let spawnScale = boxSize * boxCount / 8f
        let (spawnX, spawnY) = (0f, 0f)
    
        // define center for stabilizing the contour shape and for mouse dragging
        World.doBall2d name
            [Entity.Position .= spawnCenter + v3 spawnX spawnY 0f
             Entity.Size .= v3Dup 16f
             Entity.Visible .= false] world |> ignore
        let center = world.DeclaredEntity
    
        // define soft body countour boxes
        for i in 0 .. Array.length boxNames - 1 do
            // Arrange 32 points in a circle for soft body
            let boxAngle = MathF.Tau * single i / boxCount
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
            sandBox.MouseDragTarget.Map (FMap.add world.DeclaredEntity center) world
            sandBox.SoftBodyContour.Map (FMap.add declaredBodyId center) world
    
        // declare revolute joint linkage between contour boxes
        for (n1, n2) in Array.pairwise boxNames |> Array.add (Array.last boxNames, Array.head boxNames) do
            let twoBodyBodyJoint = TwoBodyJoint2d { CreateTwoBodyJoint = fun toPhysics _ a b ->
                // Local coordinates are used here which centers at the body coordinates,
                // but we still have to convert from world scale to physics engine scale ourselves.
                let boxSize = toPhysics boxSize
                RevoluteJoint (a, b, new _ (0f, 0.5f * boxSize), new _ (0f, -0.5f * boxSize), false) }
            World.doBodyJoint2d $"{n1} Joint contour"
                // Aside from using two entities directly, a relation of two entities in the same group can also be
                // specified by starting with the parent link denoted by "^", then accessing the sub-entity using "/".
                [Entity.BodyJointTarget .= Address.makeFromString $"^/{n1}"
                 Entity.BodyJointTarget2Opt .= Some (Address.makeFromString $"^/{n2}")
                 Entity.CollideConnected .= true // Each box linked should collide with each other
                 Entity.BreakingPoint .= infinityf
                 Entity.BodyJoint .= twoBodyBodyJoint] world |> ignore

        // declare distance joint linkage between contour boxes and center ball for stabilizing the shape
        for n in boxNames do
            let twoBodyJoint = TwoBodyJoint2d { CreateTwoBodyJoint = fun toPhysics _ a b ->
                // Local coordinates are used here which centers at the body coordinates,
                // but we still have to convert from world scale to physics engine scale ourselves.
                let boxSize = toPhysics boxSize
                DistanceJoint (a, b, new _ (0f, 0f), new _ (0f, 0f), false, Length = toPhysics spawnScale + boxSize, DampingRatio = 1f, Frequency = 5f) }
            World.doBodyJoint2d $"{n} Joint center"
                // Aside from using two entities directly, a relation of two entities in the same group can also be
                // specified by starting with the parent link denoted by "^", then accessing the sub-entity using "/".
                [Entity.BodyJointTarget .= center.EntityAddress
                 Entity.BodyJointTarget2Opt .= Some (Address.makeFromString $"^/{n}")
                 Entity.BreakingPoint .= infinityf
                 Entity.BodyJoint .= twoBodyJoint] world |> ignore

    static let declareWeb name spawnCenter world =

        let numSides = 12
        let innerRadius = 30f
        let incrementalRadius = 12f
        let numLayers = 5
        let gooMass = 0.1f
        let spawnPositions =
            Array.init numLayers (fun layer ->
                Common.PolygonTools
                    .CreateCircle(innerRadius + single layer * incrementalRadius, numSides)
                    .ConvertAll(fun p -> v3 p.X p.Y 0f))
        let spawnPositionToName (position : Vector3) =
            $"{name} {position.X} {position.Y}"

        // declare goos
        for layer in 0 .. dec numLayers do
            for vertex in 0 .. dec numSides do
            let gooSpawnPosition = spawnPositions[layer][vertex]
            World.doBall2d (spawnPositionToName gooSpawnPosition)
                [Entity.StaticImage .= Assets.Gameplay.Goo
                 Entity.Position .= spawnCenter + gooSpawnPosition
                 Entity.Size .= v3Dup 8f
                 Entity.Substance .= Mass gooMass
                 if layer = dec numLayers then
                    Entity.Visible .= false
                    Entity.BodyType .= Static
                    Entity.Sensor .= true] world |> ignore

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
                World.doBodyJoint2d $"{gooName} -> {linkRelation}"
                    [Entity.BodyJointTarget .= Address.makeFromString $"^/{otherGooName}"
                     Entity.BodyJointTarget2Opt .= Some (Address.makeFromString $"^/{gooName}")
                     Entity.BreakingPoint .= 10000f * gooMass
                     Entity.BodyJoint .= TwoBodyJoint2d { CreateTwoBodyJoint = fun _ _ a b ->
                        // setting the Breakpoint property here in the joint constructor won't work as the default
                        // value of Entity.BreakingPoint (10000f) will override it
                        DistanceJoint
                            (a, b, new _ (0f, 0f), new _ (0f, 0f), false,
                             DampingRatio = 0.5f, Frequency = 1f / gooMass * if layer = dec numLayers then 4f else 2f) }] world |> ignore
                // visualize link
                if not (world.DeclaredEntity.GetBroken world) then
                    let direction = otherGooPosition - gooPosition
                    World.doStaticSprite $"{gooName} -> {linkRelation} visual"
                        [Entity.Position @= (otherGooPosition + gooPosition) / 2f
                         Entity.Size @= v3 direction.Magnitude 2f 0f
                         Entity.Rotation @= Quaternion.CreateLookAt2d direction.V2
                         Entity.StaticImage .= Assets.Gameplay.Link
                         Entity.Elevation .= -0.5f] world |> ignore

    static let declareStrandbeest (name : string) spawnCenter world =

        // original design by Theo Jansen Walker - https://strandbeest.com/ [Distance joint]
        let objectScale = 10f
        let density = Density (1f / objectScale ** 2f)
        let pivot = v3 0f 0.8f 0f
        let wheelAnchor = v3 0f -0.8f 0f
        World.doBox2d $"{name} Chassis" [
            Entity.Substance .= density
            Entity.Position .= spawnCenter + pivot * objectScale
            Entity.Size .= v3 5f 2f 0f * objectScale
            Entity.Elevation .= -0.7f
            // Strandbeest bodies are set to a separate collision category so that they don't deform each other on contact.
            Entity.CollisionCategories .= "100"
            // But they still collide with borders and fans in category "10" and other entities in default category "1".
            Entity.CollisionMask .= "011"] world |> ignore
        let chassis = world.DeclaredEntity
        World.doBall2d $"{name} Wheel" [
            Entity.Substance .= density
            Entity.Position .= spawnCenter + pivot * objectScale
            Entity.Size .= v3Dup 3.2f * objectScale
            Entity.Elevation .= -0.5f
            Entity.CollisionCategories .= "100"
            Entity.CollisionMask .= "011"] world |> ignore
        let wheel = world.DeclaredEntity
        World.doBodyJoint2d $"{name} Motor" [
            Entity.BodyJointTarget .= wheel.EntityAddress
            Entity.BodyJointTarget2Opt .= Some chassis.EntityAddress
            Entity.BodyJoint .= TwoBodyJoint2d { CreateTwoBodyJoint = fun _ _ a b ->
                // specifying a motor for the revolute joint rotates the first body with a constant angular velocity.
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
                let shoulderPolygon = if direction > 0f then [|v3Zero; p5 - p4; p6 - p4|] else [|v3Zero; p6 - p4; p5 - p4|]
                World.doBox2d $"{name} {directionName} {rotation} Leg"
                    [Entity.Substance .= density
                     Entity.Position .= spawnCenter
                     Entity.Size .= v3Dup objectScale
                     Entity.BodyShape .= PointsShape { Points = legPolygon; Profile = Convex; TransformOpt = None; PropertiesOpt = None }
                     Entity.AngularDamping .= 10f
                     Entity.Visible .= false
                     Entity.CollisionCategories .= "100"
                     Entity.CollisionMask .= "011"] world |> ignore
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
                         Entity.StaticImage .= Assets.Default.Black] world)
                World.doBox2d $"{name} {directionName} {rotation} Shoulder"
                    [Entity.Substance .= density
                     Entity.Position .= spawnCenter + p4 * objectScale
                     Entity.Size .= v3Dup objectScale
                     Entity.BodyShape .= PointsShape { Points = shoulderPolygon; Profile = Convex; TransformOpt = None; PropertiesOpt = None }
                     Entity.AngularDamping .= 10f
                     Entity.Visible .= false
                     Entity.CollisionCategories .= "100"
                     Entity.CollisionMask .= "011"] world |> ignore
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
                         Entity.StaticImage .= Assets.Default.Black] world)
                // using a soft distance joint can reduce some jitter. it also makes the structure seem a bit more
                // fluid by acting like a suspension system.
                for (i, (entity1, entity2, position1, position2, entity1SpawnPosition, entity2SpawnPosition)) in
                    List.indexed
                        [(leg, shoulder, p2, p5, v3Zero, p4)
                         (leg, shoulder, p3, p4, v3Zero, p4)
                         (leg, wheel, p3, pivot + wheelAnchor, v3Zero, (-wheelAnchor).Transform (Quaternion.CreateFromAngle2d (-rotation * 2f * MathF.PI_OVER_3)))
                         (shoulder, wheel, p6, pivot + wheelAnchor, p4, (-wheelAnchor).Transform (Quaternion.CreateFromAngle2d (-rotation * 2f * MathF.PI_OVER_3)))] do
                    World.doBodyJoint2d $"{name} {directionName} {rotation} Distance joint {i}"
                        [Entity.BodyJoint .= TwoBodyJoint2d { CreateTwoBodyJoint = fun _ toPhysicsV2 a b ->
                            if i = 0 then
                                // HACK: the Aether demo uses mutable rotations of the wheel when initializing, doing it here won't screw up the joint distances.
                                wheel.SetRotation (Quaternion.CreateFromAngle2d (rotation * 2f * MathF.PI_OVER_3)) world
                            DistanceJoint
                                (a, b, toPhysicsV2 (position1 * objectScale + spawnCenter), toPhysicsV2 (position2 * objectScale + spawnCenter), true,
                                 Frequency = 10f, DampingRatio = 0.5f) }
                         Entity.BodyJointTarget .= entity1.EntityAddress
                         Entity.BodyJointTarget2Opt .= Some entity2.EntityAddress
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
                     Entity.BodyJointTarget .= shoulder.EntityAddress
                     Entity.BodyJointTarget2Opt .= Some chassis.EntityAddress
                     Entity.CollideConnected .= false] world |> ignore
    
    // here we define default property values
    static member Properties =
        [define Screen.Toys FMap.empty
         define Screen.DraggedEntity None
         define Screen.MouseDragTarget FMap.empty 
         define Screen.SoftBodyContour FMap.empty
         define Screen.Page Page1
         define Screen.CreditsOpened false]

    // here we define the sandBox's behavior
    override this.Process (_, sandBox, world) =

        // all entities must be in a group - groups are the unit of entity loading
        World.beginGroup Simulants.SandBoxScene.Name [] world

        // declare border
        World.doBlock2d Simulants.SandBoxBorder.Name // a block uses static physics by default - it does not react to forces or collisions.
            [Entity.Size .= v3 500f 350f 0f
             Entity.BodyShape .= ContourShape // the body shape handles collisions and is independent of how it's displayed
                { Links = // a contour shape, unlike other shapes, is hollow
                    [|v3 -0.5f 0.5f 0f // zero is the entity's center, one is the entity's size in positive direction
                      v3 0.5f 0.5f 0f
                      v3 0.5f -0.5f 0f
                      v3 -0.5f -0.5f 0f|]
                  Closed = true // The last point connects to the first point
                  TransformOpt = None
                  PropertiesOpt = None }
             // continuous collision detection adds additional checks between frame positions against high velocity
             // objects tunneling through thin borders
             Entity.CollisionDetection .= Continuous
             // collision categories is a binary mask, defaulting to "1" (units place). the border is set to be in a
             // different category, "10" (twos place) because we define fans later to not collide with the border.
             // meanwhile, unless we change the collision mask (Entity.CollisionMask), all entites default to collide
             // with "*" (i.e. all collision categories).
             Entity.CollisionCategories .= "10"
             Entity.Elevation .= -1f // draw order of the same elevation prioritizes entities with lower vertical position for 2D games.
             Entity.StaticImage .= Assets.Gameplay.SkyBoxFront] world |> ignore

        // declare avatar
        let (agentBody, _) =
            World.doCharacter2d "Avatar"
                [Entity.GravityOverride .= None] world // characters have 3x gravity by default, get rid of it

        // process agent input
        if sandBox.GetSelected world then
            if World.isKeyboardKeyDown KeyboardKey.Left world then
                World.applyBodyForce
                    (if World.getBodyGrounded agentBody world then v3 -500f 0f 0f else v3 -250f 0f 0f)
                    None agentBody world
            if World.isKeyboardKeyDown KeyboardKey.Right world then
                World.applyBodyForce
                    (if World.getBodyGrounded agentBody world then v3 500f 0f 0f else v3 250f 0f 0f)
                    None agentBody world
            if World.isKeyboardKeyPressed KeyboardKey.Up world then
                World.applyBodyForce
                    (if World.getGravity2d world = v3Zero then v3 0f 200f 0f
                     elif World.getBodyGrounded agentBody world then -90f * World.getGravity2d world
                     else v3Zero)
                    None agentBody world

        // process mouse interaction
        if sandBox.GetSelected world then
            processMouseDragging sandBox world
            processMouseScrolling sandBox world

        // declare paged menu
        match sandBox.GetPage world with
        | Page1 ->
            
            // first page of add toy buttons
            for (i, entityType) in List.indexed [Box; Ball; TinyBalls; Spring; Block; Bridge; Fan] do
                if World.doButton $"Add {scstringMemo entityType}"
                    [Entity.Position .= v3 255f (160f - 30f * single i) 0f
                     Entity.Text .= $"Add {scstringMemo entityType}"
                     Entity.Elevation .= 1f] world then
                    sandBox.Toys.Map (FMap.add Gen.name entityType) world
            
            // next page
            if World.doButton "Down"
                [Entity.Position .= v3 255f -50f 0f
                 Entity.Text .= "v"
                 Entity.Elevation .= 1f] world then
                sandBox.SetPage Page2 world

        | Page2 ->
            
            // previous page
            if World.doButton "Up"
                [Entity.Position .= v3 255f 160f 0f
                 Entity.Text .= "^"
                 Entity.Elevation .= 1f] world then
                sandBox.SetPage Page1 world
                
            // second page of add toy buttons
            for (i, entityType) in List.indexed [Clamp; Ragdoll; SoftBody; Web; Strandbeest] do
                if World.doButton $"Add {scstringMemo entityType}"
                    [Entity.Position .= v3 255f (130f - 30f * single i) 0f
                     Entity.Text .= $"Add {scstringMemo entityType}"
                     Entity.Elevation .= 1f] world then
                    sandBox.Toys.Map (FMap.add Gen.name entityType) world

            // gravity button
            let gravityDisabled = World.getGravity2d world = v3Zero
            if World.doButton "Gravity"
                [Entity.Position .= v3 255f -20f 0f
                 Entity.Text @= "Gravity: " + if gravityDisabled then "Off" else "On"
                 Entity.Elevation .= 1f] world then
                World.setGravity2d (if gravityDisabled then World.getGravityDefault2d world else v3Zero) world

        // switch scene button
        if World.doButton "Switch Scene"
            [Entity.Position .= v3 255f -100f 0f
             Entity.Text .= "Switch Scene"
             Entity.Elevation .= 1f] world then
            Game.SetDesiredScreen (Desire Simulants.RaceCourse) world

        // clear entities button
        if World.doButton "Clear Entities"
            [Entity.Position .= v3 255f -130f 0f
             Entity.Text .= "Clear Entities"
             Entity.Elevation .= 1f] world then
            sandBox.SetToys FMap.empty world
            sandBox.SetMouseDragTarget FMap.empty world
            sandBox.SetSoftBodyContour FMap.empty world

        // exit button (click behavior specified at SandBox2d.fs)
        if World.doButton "Info"
            [Entity.Position .= v3 255f -160f 0f
             Entity.Text .= "Info"
             Entity.Elevation .= 1f] world then
            sandBox.SetCreditsOpened true world

        // info panel
        if sandBox.GetCreditsOpened world then

            // declare info background
            World.doPanel "Info Background"
                [Entity.Size .= Constants.Render.DisplayVirtualResolution.V3
                 Entity.Elevation .= 10f
                 Entity.BackdropImageOpt .= Some Assets.Default.Black
                 Entity.Color .= color 0f 0f 0f 0.5f] world

            // being info panel declaration
            World.beginPanel "Info Panel"
                [Entity.Size .= Constants.Render.DisplayVirtualResolution.V3 * 0.8f
                 // we can use a grid to nicely organize Gui elements.
                 // flow direction first orders by Entity.LayoutOrder which is
                 // defined for all Gui elements (via LayoutFacet), then it orders by Entity.Order.
                 // ResizeChildren is set to true, so we can omit Entity.Size in child entities -
                 // they automatically take up all available grid space.
                 Entity.Layout .= Grid (v2i 1 5, Some FlowDownward, true)
                 Entity.Elevation .= 10f] world

            World.doText "Info Origin 1"
                [Entity.LayoutOrder .= 0
                 Entity.Text .= "Aether.SandBox2d demos by nkast (Nikos Kastellanos)"] world

            World.doText "Info Origin 2"
                [Entity.LayoutOrder .= 1
                 Entity.Text .= "Ported to Nu by Happypig375 (Hadrian Tang)"] world

            World.doText "Info Controls"
                [Entity.LayoutOrder .= 2
                 Entity.Justification .= Unjustified true
                 Entity.Text .=
                    "Controls: Left/Right/Up - Move Avatar. Left/Right - Accelerate Car, Down - Brake.\n\
                     Mouse Left - Click button or Drag entity.\n\
                     Mouse Wheel - Apply rotation to entity.\n\
                     Alt+F4 - Close game if not in Editor. Read source code for explanations!"
                 Entity.FontSizing .= Some 10
                 Entity.TextMargin .= v2 5f 0f] world

            if World.doButton "Info Close"
                [Entity.LayoutOrder .= 3
                 Entity.Text .= "Close"] world then
                sandBox.SetCreditsOpened false world

            if World.doButton "Info Exit"
                [Entity.LayoutOrder .= 4
                 Entity.Text .= "Exit"] world && world.Unaccompanied then
                World.exit world

            // end info panel declaration
            World.endPanel world

            // declare info links
            for (position, size, url) in
                [(v2   -126f  115f, v2 200f 32f, "https://github.com/nkast/Aether.SandBox2d/tree/main/Samples/NewSamples/Demos")
                 (v2     25f  115f, v2  50f 32f, "https://github.com/nkast")
                 (v2 -127.5f 57.5f, v2 115f 32f, "https://github.com/bryanedds/Nu/pull/1120")
                 (v2    3.5f 57.5f, v2 105f 32f, "https://github.com/Happypig375")] do
                if World.doButton $"""Info Origin Button {url.Replace ('/', '\\')}"""
                    [Entity.Elevation .= 11f
                     Entity.Position .= position.V3
                     Entity.Size .= size.V3] world then
                    Process.Start (ProcessStartInfo (url, UseShellExecute = true)) |> ignore

        // declare toys
        let spawnCenter = (World.getEye2dCenter world - v2 60f 0f).V3
        for KeyValue (name, toy) in sandBox.GetToys world do
            match toy with
            | Box -> declareBox name spawnCenter world
            | Ball -> declareBall name spawnCenter world
            | TinyBalls -> declareTinyBalls name spawnCenter world
            | Spring -> declareSpring name spawnCenter world
            | Block -> declareBlock name spawnCenter world
            | Bridge -> declareBridge name spawnCenter world
            | Fan -> declareFan name spawnCenter sandBox world
            | Clamp -> declareClamp name spawnCenter world
            | Ragdoll -> declareRagdoll name spawnCenter world
            | SoftBody -> declareSoftBody name spawnCenter sandBox world
            | Web -> declareWeb name spawnCenter world
            | Strandbeest -> declareStrandbeest name spawnCenter world

        // end scene declaration
        World.endGroup world

        // process camera as last task
        if sandBox.GetSelected world then
            World.setEye2dCenter (v2 60f 0f) world