namespace SandBox2d
open System
open System.Diagnostics
open System.Numerics
open Box2D.NET
open Prime
open Nu

/// A physics 'toy' that can be spawned into the toy box.
type Toy =
    | Box
    | Block
    | Ball
    | TinyBalls
    | Spring
    | Bridge
    | Fan
    | Clamp
    | Ragdoll
    | SoftBody
    | Web
    | Strandbeest

/// The different pages of the toy box menu.
type MenuPage =
    | MenuPage1
    | MenuPage2

// this extends the Screen API to expose the user-defined properties.
[<AutoOpen>]
module ToyBoxExtensions =
    type Screen with
        member this.GetEntityRedirects world : FMap<Entity, Entity> = this.Get (nameof Screen.EntityRedirects) world
        member this.SetEntityRedirects (value : FMap<Entity, Entity>) world = this.Set (nameof Screen.EntityRedirects) value world
        member this.EntityRedirects = lens (nameof Screen.EntityRedirects) this this.GetEntityRedirects this.SetEntityRedirects
        member this.GetBodyIdRedirects world : FMap<BodyId, Entity> = this.Get (nameof Screen.BodyIdRedirects) world
        member this.SetBodyIdRedirects (value : FMap<BodyId, Entity>) world = this.Set (nameof Screen.BodyIdRedirects) value world
        member this.BodyIdRedirects = lens (nameof Screen.BodyIdRedirects) this this.GetBodyIdRedirects this.SetBodyIdRedirects
        member this.GetToys world : FMap<string, Toy> = this.Get (nameof Screen.Toys) world
        member this.SetToys (value : FMap<string, Toy>) world = this.Set (nameof Screen.Toys) value world
        member this.Toys = lens (nameof Screen.Toys) this this.GetToys this.SetToys
        member this.GetDragState world : (Entity * Vector3 * BodyType) option = this.Get (nameof Screen.DragState) world
        member this.SetDragState (value : (Entity * Vector3 * BodyType) option) world = this.Set (nameof Screen.DragState) value world
        member this.DragState = lens (nameof Screen.DragState) this this.GetDragState this.SetDragState
        member this.GetMenuPage world : MenuPage = this.Get (nameof Screen.MenuPage) world
        member this.SetMenuPage (value : MenuPage) world = this.Set (nameof Screen.MenuPage) value world
        member this.MenuPage = lens (nameof Screen.MenuPage) this this.GetMenuPage this.SetMenuPage
        member this.GetInfoOpened world : bool = this.Get (nameof Screen.InfoOpened) world
        member this.SetInfoOpened (value : bool) world = this.Set (nameof Screen.InfoOpened) value world
        member this.InfoOpened = lens (nameof Screen.InfoOpened) this this.GetInfoOpened this.SetInfoOpened
        member this.GetGravities world : (string * Vector3) list = this.Get (nameof Screen.Gravities) world
        member this.SetGravities (value : (string * Vector3) list) world = this.Set (nameof Screen.Gravities) value world
        member this.Gravities = lens (nameof Screen.Gravities) this this.GetGravities this.SetGravities
        member this.GetAvatarGravities world : (string * Gravity) list = this.Get (nameof Screen.AvatarGravities) world
        member this.SetAvatarGravities (value : (string * Gravity) list) world = this.Set (nameof Screen.AvatarGravities) value world
        member this.AvatarGravities = lens (nameof Screen.AvatarGravities) this this.GetAvatarGravities this.SetAvatarGravities
        
// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type ToyBoxDispatcher () =
    inherit ScreenDispatcherImSim ()

    static let setDraggedEntity mousePosition (entity : Entity) (toyBox : Screen) world =
        let dragOffset = (mousePosition - entity.GetPosition world).Transform (entity.GetRotation world).Inverted
        toyBox.SetDragState (Some (entity, dragOffset, entity.GetBodyType world)) world
        entity.SetBodyType Dynamic world // Only dynamic bodies react to forces by the mouse joint below.

    static let processMouseDragging (toyBox : Screen) world =

        // attempt to select a drag an entity
        let mousePosition = (World.getMousePosition2dWorld false world).V3
        if world.Advancing && World.isMouseButtonPressed MouseLeft world then

            // attempt to establish a dragged entity via direct point test
            let entityRedirects = toyBox.GetEntityRedirects world
            for entity in World.getEntities2dAtPoint mousePosition.V2 (hashSetPlus HashIdentity.Structural []) world do
                let entity = FMap.tryFind entity entityRedirects |> Option.defaultValue entity
                if  (toyBox.GetDragState world).IsNone &&
                    entity.Has<RigidBodyFacet> world && // check rigid body facet existence to confirm the body type property's validity before reading it
                    entity.GetVisible world && // don't drag invisible entities, such as for Strandbeest shoulder or legs
                    entity.Name <> "Border" then
                    setDraggedEntity mousePosition entity toyBox world

            // raycast entities to see if mouse location is inside a soft body enclosed area, then drag it
            if (toyBox.GetDragState world).IsNone then
                let rayUp =
                    World.rayCastBodies2d (ray3 mousePosition (v3Up * 100f)) 1UL UInt64.MaxValue false world
                    |> Seq.map _.BodyShapeIntersected.BodyId
                    |> Seq.choose (toyBox.GetBodyIdRedirects world).TryFind
                    |> Set
                let rayDown =
                    World.rayCastBodies2d (ray3 mousePosition (v3Down * 100f)) 1UL UInt64.MaxValue false world
                    |> Seq.map _.BodyShapeIntersected.BodyId
                    |> Seq.choose (toyBox.GetBodyIdRedirects world).TryFind
                    |> Set
                let intersection = Set.intersect rayUp rayDown
                if Set.notEmpty intersection then
                    setDraggedEntity mousePosition (Set.minElement intersection) toyBox world

        // drag any dragged entity
        match toyBox.GetDragState world with
        | Some (draggedEntity, dragOffset, draggedBodyType) when World.isMouseButtonDown MouseLeft world ->

            // begin declaration of the mouse sensor - an empty body with the mouse position.
            World.beginEntity<Sphere2dDispatcher> "Mouse Sensor"
                [Entity.BodyShape .= EmptyShape
                 Entity.Visible .= false
                 Entity.Position @= mousePosition] world |> ignore
            let mouseSensor = world.ContextEntity

            // declare joint for mouse body
            let mouseJoint = mouseSensor / "Mouse Joint"
            World.doBodyJoint2d mouseJoint.Name
                [Entity.BodyJoint |= Box2dNetBodyJoint { CreateBodyJoint = fun _ toPhysicsV2 a b world ->
                    let mousePosition = toPhysicsV2 mousePosition // convert mouse position (Vector2) to world position (Vector3) to physics engine position (B2Vec2)
                    if draggedBodyType = Dynamic // give dynamic bodies flick behavior, give static or kinematic bodies weld behavior.
                    then
                        // here is a first look at how to create 2D joints in Nu. each joint type is showcased later.
                        // for details, refer to Box2D documentation on joints: https://box2d.org/documentation/md_simulation.html
                        // and samples for joints: https://github.com/erincatto/box2d/blob/main/samples/sample_joints.cpp
                        let mutable jointDef = B2Joints.b2DefaultDistanceJointDef () // all body joints are initialized with one of the JointDef functions.
                        jointDef.``base``.bodyIdA <- a // assigning body IDs is required.
                        jointDef.``base``.bodyIdB <- b
                        jointDef.``base``.localFrameA.p <- B2Bodies.b2Body_GetLocalPoint (a, mousePosition) // joint anchor points are defined in local body space.
                        jointDef.``base``.localFrameB.p <- B2Bodies.b2Body_GetLocalPoint (b, mousePosition)
                        jointDef.length <- 0f
                        jointDef.enableSpring <- true // optional behaviors correspond to enable flags.
                        jointDef.hertz <- 1.5f // enabled by enableSpring.
                        jointDef.dampingRatio <- 0.5f
                        B2Joints.b2CreateDistanceJoint (world, &jointDef) // joint creations should end with a create function call.
                    else
                        let mutable jointDef = B2Joints.b2DefaultWeldJointDef () // another joint type
                        jointDef.``base``.bodyIdA <- a
                        jointDef.``base``.bodyIdB <- b
                        jointDef.``base``.localFrameA.p <- B2Bodies.b2Body_GetLocalPoint (a, mousePosition)
                        jointDef.``base``.localFrameB.p <- B2Bodies.b2Body_GetLocalPoint (b, mousePosition)
                        B2Joints.b2CreateWeldJoint (world, &jointDef) }
                 Entity.BodyJointTarget .= draggedEntity.EntityAddress
                 Entity.BodyJointTarget2 .= mouseSensor.EntityAddress
                 Entity.MountOpt .= None] world |> ignore

            // declare mouse joint visualization
            let draggedPosition = dragOffset.Transform (draggedEntity.GetRotation world) + draggedEntity.GetPosition world
            World.doStaticSprite "Mouse Joint Visual"
                [Entity.Position @= (draggedPosition + mousePosition) / 2f
                 Entity.Size @= v3 (Vector3.Distance (draggedPosition, mousePosition)) 1f 0f
                 Entity.Rotation @= Quaternion.CreateLookAt2d (mousePosition - draggedPosition).V2
                 Entity.Color .= color 1f 0f 0f 1f
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Elevation .= 0.5f // elevate the line above other entities (elevation 0) but below buttons (elevation 1)
                 Entity.MountOpt .= None] world |> ignore

            // apply damping to body in order to stabilize it while dragged
            draggedEntity.LinearVelocity.Map ((*) 0.9f) world
            draggedEntity.AngularVelocity.Map ((*) 0.9f) world

            // end declaration of sensor for mouse body
            World.endEntity world

        // release any dragged entity
        | Some (draggedEntity, _, draggedBodyType) ->
            toyBox.SetDragState None world
            draggedEntity.SetBodyType draggedBodyType world

        // nothing to do
        | None -> ()

    static let processMouseScrolling (toyBox : Screen) world =
        let mousePosition = (World.getMousePosition2dWorld false world).V3
        for event in World.doSubscription "MouseWheel" Game.MouseWheelEvent world do
            let physicsAnchors = toyBox.GetEntityRedirects world
            for entity in World.getEntities2dAtPoint mousePosition.V2 (hashSetPlus HashIdentity.Structural []) world do
                let entity = FMap.tryFind entity physicsAnchors |> Option.defaultValue entity
                if entity.Has<RigidBodyFacet> world && entity.Name <> "Border" then
                    World.applyBodyTorque (v3 0f 0f 40f * event.Travel) (entity.GetBodyId world) world

    static let declareBox name spawnCenter world =
        World.doBox2d name // unlike a block, a box uses dynamic physics by default - it reacts to forces and collisions
            [Entity.Position |= spawnCenter + v3 Gen.randomf Gen.randomf 0f
             Entity.Color |= color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f
             Entity.Restitution .= 0.333f] world |> ignore

    static let declareBlock name spawnCenter world =
        World.doBlock2d name
            [Entity.Position |= spawnCenter + v3 (Gen.randomf1 500f - 250f) (Gen.randomf1 350f - 175f) 0f // random placement
             Entity.StaticImage .= Assets.Default.Brick] world |> ignore

    static let declareBall name spawnCenter world =
        World.doBall2d name // unlike a sphere, a ball uses dynamic physics by default
            [Entity.Position |= spawnCenter + v3 Gen.randomf Gen.randomf 0f
             Entity.Color |= color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f
             Entity.Restitution .= 0.5f] world |> ignore // bouncier than default

    static let declareTinyBalls name spawnCenter world =
        for i in 0 .. dec 16 do
            World.doBall2d $"{name} Ball {i}"
                [Entity.Position |= spawnCenter + v3 Gen.randomf Gen.randomf 0f
                 Entity.Color |= color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f
                 Entity.Size .= Constants.Engine.Entity2dSizeDefault / 4f
                 Entity.Restitution .= 0.5f // bouncier than default
                 Entity.Substance .= Mass (1f / 16f)] world |> ignore // have tiny mass when colliding

    static let declareSpring name spawnCenter world =

        // in Nu, each entity can contain arbitrarily many child entities. each entity mounts, aka inherits transforms
        // (position, rotation, scale) from, their parent by default. This can be changed by setting the
        // Entity.MountOpt property. mounting only applies to entities that are not rigid bodies because it makes no
        // sense for both the physics engine and mounted entity to specify transforms together. in the following code,
        // we show how to point to child entities using relative addresses.

        // begin parent entity declaration
        let color = color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f
        World.beginEntity<BodyJoint2dDispatcher> name
            [Entity.BodyJoint |= Box2dNetBodyJoint { CreateBodyJoint = fun toPhysics _ a b world ->
                // a distance joint maintains fixed distance between two bodies, optionally with spring-like behavior.
                // it does not impose limits on relative positions or rotations.
                let mutable jointDef = B2Joints.b2DefaultDistanceJointDef ()
                jointDef.``base``.bodyIdA <- a
                jointDef.``base``.bodyIdB <- b
                jointDef.length <- toPhysics 60f
                jointDef.enableSpring <- true // remember to explicitly enable spring behavior
                jointDef.hertz <- 5f
                jointDef.dampingRatio <- 0.3f
                B2Joints.b2CreateDistanceJoint (world, &jointDef) }
             Entity.BodyJointTarget .= Address.makeFromString "~/Face 1" // points to child entity
             Entity.BodyJointTarget2 .= Address.makeFromString "~/Face 2"] world

        // declare face 1
        World.doBox2d "Face 1" // pointed to by parent body joint
            [Entity.Position |= spawnCenter
             Entity.Color |= color
             Entity.Size .= v3 150f 10f 0f
             Entity.StaticImage .= Assets.Default.Paddle
             Entity.Substance .= Mass (1f / 2f)
             Entity.MountOpt .= None] world |> ignore
        let face1 = world.DeclaredEntity

        // declare face 2
        World.doBox2d "Face 2"
            [Entity.Position |= spawnCenter + v3 0f -60f 0f
             Entity.Color |= color
             Entity.Size .= v3 150f 10f 0f
             Entity.StaticImage .= Assets.Default.Paddle
             Entity.Substance .= Mass (1f / 2f)
             Entity.MountOpt .= None] world |> ignore
        let face2 = world.DeclaredEntity

        // declare spring visual
        let direction = face2.GetPosition world - face1.GetPosition world
        World.doStaticSprite "Joint Visual"
            [Entity.Color |= color.WithA 0.5f
             Entity.Position @= (face1.GetPosition world + face2.GetPosition world) / 2f
             Entity.Size @= v3 direction.Magnitude 1f 0f
             Entity.Rotation @= Quaternion.CreateLookAt2d direction.V2
             Entity.StaticImage .= Assets.Default.White
             Entity.Elevation .= 0.5f
             Entity.MountOpt .= None] world |> ignore

        // declare prismatic joint to limit movement to one axis
        World.doBodyJoint2d "Prismatic Joint"
            [Entity.BodyJoint |= Box2dNetBodyJoint { CreateBodyJoint = fun _ toPhysicsV2 a b world ->
                // a prismatic joint maintains fixed position between two bodies to move linearly along a straight axis
                // while disallowing relative rotation, without fixing distance
                let mutable jointDef = B2Joints.b2DefaultPrismaticJointDef ()
                jointDef.``base``.bodyIdA <- a
                jointDef.``base``.bodyIdB <- b
                // leave the local frame position as zero - the joint anchor point will be at the center of both bodies.
                // the frame rotation provides the local x and y axes. it lets you define a joint where the angle is not initially zero.
                // for a prismatic joint, the local x-axis defines the sliding axis.
                // frames apply rotation first. in other words, the frame rotation does not affect the position.
                jointDef.``base``.localFrameA.q <- toPhysicsV2 direction |> B2MathFunction.b2Normalize |> B2MathFunction.b2MakeRotFromUnitVector
                jointDef.``base``.localFrameB.q <- jointDef.``base``.localFrameA.q // both bodies should slide along the same axis
                B2Joints.b2CreatePrismaticJoint (world, &jointDef) }
             Entity.BodyJointTarget .= Address.makeFromString "^/Face 1"
             Entity.BodyJointTarget2 .= Address.makeFromString "^/Face 2"] world |> ignore

        // end parent entity declaration
        World.endEntity world

    static let declareBridge name spawnCenter world =

        // declare anchor 1
        let x = Gen.randomf1 500f - 250f
        let y = Gen.randomf1 350f - 175f
        World.doSphere2d name [Entity.Position |= spawnCenter + v3 x y 0f] world |> ignore
        let anchor1 = world.DeclaredEntity

        // declare anchor 2
        World.doSphere2d $"{name} Opposite End" [Entity.Position |= spawnCenter + v3 x y 0f] world |> ignore
        let anchor2 = world.DeclaredEntity

        // adjust position of link relative to each anchor as the anchors are dragged around
        let direction = anchor1.GetPosition world - anchor2.GetPosition world
        if direction <> v3Zero then
            let rotation = Quaternion.CreateLookAt2d (v2 -direction.Y direction.X)
            anchor1.SetRotation rotation world
            anchor2.SetRotation rotation world

        // declare bridge links
        let names = Array.init 6 (sprintf "%s Paddle %d" name)
        let boxHeight = direction.Magnitude / single (Array.length names)
        for i in 0 .. Array.length names - 1 do
            World.doBox2d names[i]
                [Entity.Size @= v3 4f boxHeight 0f
                 Entity.StaticImage .= Assets.Default.Paddle
                 // paddles are thin, so use continuous collision detection to prevent tunnelling at high velocities
                 Entity.CollisionDetection .= Continuous] world |> ignore

        // declare revolute joints to link the bridge links together and to the anchors
        for (n1, n2) in Array.pairwise [|anchor1.Name; yield! names; anchor2.Name|] do
            World.doBodyJoint2d $"{n2} Link"
                [Entity.BodyJointTarget .= Address.makeFromString $"^/{n1}"
                 Entity.BodyJointTarget2 .= Address.makeFromString $"^/{n2}"
                 Entity.BodyJoint @= Box2dNetBodyJoint { CreateBodyJoint = fun toPhysics _ a b world ->
                    // a revolute joint is like a hinge or pin, where two bodies rotate about a common point. in this
                    // case, the bottom center point of body A shares the same position as the top center point of body
                    // B, where they can rotate freely relative to each other.
                    let mutable jointDef = B2Joints.b2DefaultRevoluteJointDef ()
                    jointDef.``base``.bodyIdA <- a
                    jointDef.``base``.bodyIdB <- b
                    jointDef.``base``.localFrameA.p <- new _ (0f, 0.5f * toPhysics boxHeight)
                    jointDef.``base``.localFrameB.p <- new _ (0f, -0.5f * toPhysics boxHeight)
                    B2Joints.b2CreateRevoluteJoint (world, &jointDef) }] world |> ignore

    static let declareFan name spawnCenter (toyBox : Screen) world =

        // a fan is made of two rectangular blocks (blades) welded together at the center with a weld body joint. one
        // of the blades is set as the "anchor", which is kinematic and is the actual entity dragged by mouse.

        // begin anchor blade declaration
        let x = Gen.randomf1 500f - 250f
        let y = Gen.randomf1 350f - 175f
        World.beginEntity<Block2dDispatcher> name
            [Entity.Position |= spawnCenter + v3 x y 0f
             Entity.Size .= v3 64f 8f 0f
             Entity.BodyType .= Kinematic // does not react to forces or collisions, but can be moved by setting its velocity (here angular)
             Entity.AngularVelocity @= v3 0f 0f 10f
             Entity.CollisionCategories .= "10" // treated the same way as borders when colliding with other fans
             // fans collide with entities in the default collision category "1", not with the border or other fans in
             // category "10". otherwise the + shape of the fan deforms when dragging next to another fan or the border.
             Entity.CollisionMask .= "1"
             Entity.StaticImage .= Assets.Default.Label] world |> ignore
        let anchor = world.ContextEntity

        // declare other blade
        World.doBox2d $"{name} Other Blade"
            [Entity.Position |= spawnCenter + v3 x y 0f
             Entity.Size .= v3 64f 8f 0f
             Entity.CollisionCategories .= "10"
             Entity.CollisionMask .= "101"
             Entity.StaticImage .= Assets.Default.Label
             Entity.MountOpt .= None] world |> ignore
        let blade = world.DeclaredEntity
        toyBox.EntityRedirects.Map (FMap.add blade anchor) world

        // declare weld joint to link the two blades together at the center point (x, y)
        World.doBodyJoint2d $"{name} Weld Joint"
            [Entity.BodyJoint |= Box2dNetBodyJoint { CreateBodyJoint = fun _ _ a b world ->
                // a weld joint disallows changing relative position and rotation between two bodies. However, being a
                // soft constraint, it may still deform with a heavy external force. When deforming is unwanted, the
                // body shapes should be within same entity instead.
                let mutable jointDef = B2Joints.b2DefaultWeldJointDef ()
                jointDef.``base``.bodyIdA <- a
                jointDef.``base``.bodyIdB <- b
                jointDef.``base``.localFrameB.q <- B2MathFunction.b2MakeRot MathF.PI_OVER_2 // rotate the second body by 90 degrees for welding
                B2Joints.b2CreateWeldJoint (world, &jointDef) }
             Entity.BodyJointTarget .= anchor.EntityAddress
             Entity.BodyJointTarget2 .= blade.EntityAddress
             Entity.CollideConnected .= false // When the two blades are set to collide, the + shape would deform on drag
             ] world |> ignore

        // end anchor blade declaration
        World.endEntity world

    static let declareClamp name spawnCenter world =

        // begin center ball declaration
        let ballSize = 32f
        World.beginEntity<Ball2dDispatcher> name
            [Entity.Position |= spawnCenter
             Entity.Size .= v3 ballSize ballSize 0f] world |> ignore

        // declare legs
        for (directionName, direction) in [("Left", -1f); ("Right", 1f)] do
            let upperLeg = $"{name} {directionName} Upper Leg"
            for (newLeg, linkTo, image, angle) in
                [(upperLeg, $"^/{name}", Assets.Default.Image, 0.2f)
                 ($"{name} {directionName} Lower Leg", upperLeg, Assets.Default.Black, 0.4f)] do
                let legLength = 30f
                World.doBox2d newLeg
                    [Entity.Position |= spawnCenter
                     Entity.StaticImage .= image
                     Entity.Size .= v3 legLength 4f 0f
                     Entity.MountOpt .= None] world |> ignore
                let isExtended =
                    world.ClockTime % 10f >= 5f
                let twoBodyJoint = Box2dNetBodyJoint { CreateBodyJoint = fun _ toPhysicsV2 a b world ->
                    let p = toPhysicsV2 (v3 (legLength * direction) 0f 0f)
                    // an angle joint links the rotation of two bodies together, optionally specifying the target angle
                    // (difference in rotation)
                    let mutable jointDef = B2Joints.b2DefaultRevoluteJointDef ()
                    jointDef.``base``.bodyIdA <- a
                    jointDef.``base``.bodyIdB <- b
                    jointDef.``base``.localFrameA.p <- new _ (p.X * 0.5f, p.Y * 0.5f)
                    jointDef.``base``.localFrameB.p <- new _ (p.X * -0.5f, p.Y * -0.5f)
                    // a revolute joint can enforce a target angle using a spring instead of free rotation.
                    jointDef.enableSpring <- true
                    jointDef.targetAngle <- (angle + if isExtended then 1f else 0f) * direction // enforced by the spring
                    jointDef.hertz <- 20.0f
                    jointDef.dampingRatio <- 1.0f // critical damping - we don't want oscillation here.
                    B2Joints.b2CreateRevoluteJoint (world, &jointDef) }
                World.doBodyJoint2d $"{newLeg} Angle Joint {isExtended}"
                    [Entity.BodyJoint |= twoBodyJoint
                     Entity.BodyJointTarget .= Address.makeFromString $"^/{linkTo}"
                     Entity.BodyJointTarget2 .= Address.makeFromString $"^/{newLeg}"
                     Entity.MountOpt .= None] world |> ignore

        // end center ball declaration
        World.endEntity world

    static let declareRagdoll name spawnCenter world =
    
        // begin declaring head as parent
        let ballY = 60f
        let ballSize = 20f
        World.beginEntity<Ball2dDispatcher> $"{name}"
            [Entity.Position |= spawnCenter + v3 0f 60f 0f
             Entity.Size .= v3 ballSize ballSize 0f
             Entity.AngularDamping .= 2f
             Entity.Substance .= Mass 2f
             Entity.MountOpt .= None] world |> ignore

        // declare torso
        let torsoWidth = 40f
        let torsoHeight = torsoWidth / 2f
        for (i, componentName, connectsTo, revoluteAngle) in
            [1f, "Torso Upper", "Head", None
             2f, "Torso Middle", "Torso Upper", Some (MathF.PI / 8f)
             3f,  "Torso Lower", "Torso Middle", Some (MathF.PI / 16f)] do
            World.doBall2d $"{name} {componentName}"
                [Entity.Position |= spawnCenter + v3 0f (ballY - i * torsoHeight) 0f
                 Entity.BodyShape |= CapsuleShape
                    { Height = 0.5f; Radius = 0.25f; PropertiesOpt = None
                      // capsule shapes are vertical by default. To get a horizontal capsule, we apply a 90 degrees
                      // rotation. moreover, since height here is relative to entity height, we also need to scale it
                      // by 2 to use entity width instead.
                      TransformOpt = Some (Affine.make v3Zero (Quaternion.CreateFromAngle2d MathF.PI_OVER_2) (v3Dup 2f)) }
                 Entity.Size .= v3 torsoWidth torsoHeight 0f
                 Entity.StaticImage .= Assets.Gameplay.CapsuleImage
                 Entity.MountOpt .= None] world |> ignore
            let twoBodyJoint = Box2dNetBodyJoint { CreateBodyJoint = fun toPhysics _ a b world ->
                match revoluteAngle with
                | Some revoluteAngle ->
                    let mutable jointDef = B2Joints.b2DefaultRevoluteJointDef ()
                    jointDef.``base``.bodyIdA <- a
                    jointDef.``base``.bodyIdB <- b
                    jointDef.``base``.localFrameA.p <- new _ (0f, -0.55f * toPhysics torsoHeight)
                    jointDef.``base``.localFrameB.p <- new _ (0f, 0.55f * toPhysics torsoHeight)
                    jointDef.enableLimit <- true // angle limits are allowed for revolute joints
                    jointDef.lowerAngle <- -revoluteAngle
                    jointDef.upperAngle <- revoluteAngle
                    B2Joints.b2CreateRevoluteJoint (world, &jointDef)
                | _ ->
                    let mutable jointDef = B2Joints.b2DefaultDistanceJointDef ()
                    jointDef.``base``.bodyIdA <- a
                    jointDef.``base``.bodyIdB <- b
                    jointDef.``base``.localFrameA.p <- new _ (0f, -0.5f * toPhysics torsoHeight)
                    jointDef.``base``.localFrameB.p <- new _ (0f, 0.5f * toPhysics torsoHeight)
                    jointDef.length <- toPhysics 1f
                    jointDef.enableSpring <- true
                    jointDef.hertz <- 25f
                    jointDef.dampingRatio <- 1f
                    B2Joints.b2CreateDistanceJoint (world, &jointDef) }
            World.doBodyJoint2d $"{name} {connectsTo}<->{componentName}"
                [Entity.BodyJoint |= twoBodyJoint
                 Entity.BodyJointTarget .=
                    if connectsTo = "Head"
                    then Address.makeFromString $"^/^/{name}" // special case for head as parent
                    else Address.makeFromString $"^/{name} {connectsTo}"
                 Entity.BodyJointTarget2 .= Address.makeFromString $"^/{name} {componentName}"
                 Entity.MountOpt .= None] world |> ignore

        // declare arms and legs
        let armWidth = 30f
        let armHeight = armWidth / 2f
        for (side, direction) in ["Left", -1f; "Right", 1f] do
            for (pos1, posIncrement, rotation, armOrLeg, connectsToTorso) in
                [v3 (direction * torsoWidth * 0.825f) (ballY - ballSize / 2f - torsoHeight / 2f) 0f, v3 (direction * armWidth) 0f 0f, 0f, "Arm", "Upper"
                 v3 (direction * torsoWidth * 0.25f) (ballY - ballSize / 2f - 3f * torsoHeight - armHeight) 0f, v3 0f -armWidth 0f, MathF.PI_OVER_2, "Leg", "Lower"] do
            for (pos, upperOrLower, connectsTo) in
                [pos1, "Upper", $"Torso {connectsToTorso}"
                 pos1 + posIncrement, "Lower", $"{side} {armOrLeg} Upper"] do
            let componentName = $"{side} {armOrLeg} {upperOrLower}"
            World.doBall2d $"{name} {componentName}"
                [Entity.Position |= spawnCenter + pos
                 Entity.Rotation |= Quaternion.CreateFromAngle2d rotation
                 Entity.Size .= v3 armWidth armHeight 0f
                 Entity.BodyShape .= CapsuleShape
                    { Height = 0.5f; Radius = 0.25f; PropertiesOpt = None
                      TransformOpt = Some (Affine.make v3Zero (Quaternion.CreateFromAngle2d MathF.PI_OVER_2) (v3Dup 2f)) }
                 Entity.StaticImage .= Assets.Gameplay.CapsuleImage
                 Entity.MountOpt .= None] world |> ignore
            let twoBodyJoint = Box2dNetBodyJoint { CreateBodyJoint = fun toPhysics toPhysicsV2 a b world ->
                let jointPosition = toPhysicsV2 (pos - posIncrement / 2f)
                let mutable jointDef = B2Joints.b2DefaultDistanceJointDef ()
                jointDef.``base``.bodyIdA <- a
                jointDef.``base``.bodyIdB <- b
                jointDef.``base``.localFrameA.p <- B2Bodies.b2Body_GetLocalPoint (a, jointPosition)
                jointDef.``base``.localFrameB.p <- B2Bodies.b2Body_GetLocalPoint (b, jointPosition)
                jointDef.length <- toPhysics 4f
                jointDef.enableSpring <- true
                jointDef.hertz <- 25f
                jointDef.dampingRatio <- 1f
                B2Joints.b2CreateDistanceJoint (world, &jointDef) }
            World.doBodyJoint2d $"{name} {connectsTo}<->{componentName}"
                [Entity.BodyJoint |= twoBodyJoint
                 Entity.BodyJointTarget .= Address.makeFromString $"^/{name} {connectsTo}"
                 Entity.BodyJointTarget2 .= Address.makeFromString $"^/{name} {componentName}"
                 Entity.MountOpt .= None] world |> ignore

        // end declaring head as parent
        World.endEntity world

    static let declareSoftBody name spawnCenter (toyBox : Screen) world =
                
        // begin center declaration for stabilizing the contour shape and for mouse dragging
        let color = color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f
        let boxNames = Array.init 32 (sprintf "%s Contour %d" name)
        let boxCount = single boxNames.Length
        let boxSize = 8f
        let spawnScale = boxSize * boxCount / 8f
        let (spawnX, spawnY) = (0f, 0f)
        World.beginEntity<Ball2dDispatcher> name
            [Entity.Position |= spawnCenter + v3 spawnX spawnY 0f
             Entity.Size .= v3Dup 16f
             Entity.Visible .= false] world |> ignore
        let center = world.ContextEntity

        // define soft body countour boxes, with 32 points in a circle for soft body
        for i in 0 .. Array.length boxNames - 1 do
            let boxAngle = MathF.Tau * single i / boxCount
            let x = cos boxAngle * spawnScale + spawnX
            let y = sin boxAngle * spawnScale + spawnY
            let (declaredBodyId, _) =
                World.doBox2d boxNames.[i]
                    [Entity.Position |= spawnCenter + v3 x y 0f
                     Entity.Rotation |= Quaternion.CreateFromAngle2d (boxAngle + MathF.PI_OVER_2) // first box sprite at right side should orient right as up
                     Entity.Color |= color
                     Entity.Size .= v3 boxSize boxSize 0f
                     Entity.Restitution .= 0.333f
                     Entity.Substance .= Mass (1f / boxCount) // mass evenly distributed between the contour and the center
                     Entity.CollisionDetection .= Continuous
                     Entity.MountOpt .= None] world
            // when the contour box is dragged directly, the many other joints counteract the mouse joint and the soft
            // body stays mid-air away from the mouse
            toyBox.EntityRedirects.Map (FMap.add world.DeclaredEntity center) world
            toyBox.BodyIdRedirects.Map (FMap.add declaredBodyId center) world

        // declare revolute joint linkage between contour boxes
        for (n1, n2) in Array.pairwise boxNames |> Array.add (Array.last boxNames, Array.head boxNames) do
            let twoBodyBodyJoint = Box2dNetBodyJoint { CreateBodyJoint = fun toPhysics _ a b world ->
                // local coordinates are used here which centers at the body coordinates,
                // but we still have to convert from world scale to physics engine scale ourselves.
                let boxSize = toPhysics boxSize
                let mutable jointDef = B2Joints.b2DefaultRevoluteJointDef ()
                jointDef.``base``.bodyIdA <- a // boxNames start at right side and go clockwise - this is the right box.
                jointDef.``base``.bodyIdB <- b // this is the left box.
                // right box's left center point should be linked to left box's right center point
                jointDef.``base``.localFrameA.p <- new _ (0.5f * boxSize, 0f)
                jointDef.``base``.localFrameB.p <- new _ (-0.5f * boxSize, 0f)
                B2Joints.b2CreateRevoluteJoint (world, &jointDef) }
            World.doBodyJoint2d $"{n1} Joint Contour"
                // aside from using two entities directly, a relation of two entities in the same group can also be
                // specified by starting with the parent link denoted by "^", then accessing the sub-entity using "/".
                [Entity.BodyJoint |= twoBodyBodyJoint
                 Entity.BodyJointTarget .= Address.makeFromString $"^/{n1}"
                 Entity.BodyJointTarget2 .= Address.makeFromString $"^/{n2}"
                 Entity.CollideConnected .= true // Each box linked should collide with each other
                 ] world |> ignore

        // declare distance joint linkage between contour boxes and center ball for stabilizing the shape
        for n in boxNames do
            let twoBodyJoint = Box2dNetBodyJoint { CreateBodyJoint = fun toPhysics _ a b world ->
                // local coordinates are used here which centers at the body coordinates, but we still have to convert
                // from world scale to physics engine scale ourselves.
                let boxSize = toPhysics boxSize
                let mutable jointDef = B2Joints.b2DefaultDistanceJointDef ()
                jointDef.``base``.bodyIdA <- a
                jointDef.``base``.bodyIdB <- b
                // the local frame positions are relative to each body's center point - keep them zero.
                jointDef.length <- toPhysics spawnScale + boxSize
                jointDef.enableSpring <- true
                jointDef.hertz <- 5f
                jointDef.dampingRatio <- 1f
                B2Joints.b2CreateDistanceJoint (world, &jointDef) }
            World.doBodyJoint2d $"{n} Joint Center"
                // aside from using two entities directly, a relation of two entities in the same group can also be
                // specified by starting with the parent link denoted by "^", then accessing the sub-entity using "/".
                [Entity.BodyJoint |= twoBodyJoint
                 Entity.BodyJointTarget .= center.EntityAddress
                 Entity.BodyJointTarget2 .= Address.makeFromString $"^/{n}"] world |> ignore

        // end center ball declaration
        World.endEntity world

    static let declareWeb name spawnCenter world =

        // compute spawn positions
        let numSides = 12
        let objectScale = 0.2f
        let radius = 2.9f * objectScale
        let finalLayerInset = 0.9f * objectScale
        let numLayers = 5
        let spawnPositions =
            let stepSize = MathF.Tau / single numSides
            Array.init numLayers (fun layer ->
                let radius = (radius * single (layer + 1) - if layer = dec numLayers then finalLayerInset else 0.0f) * Constants.Engine.Meter2d
                Array.init numSides (fun i ->
                    let angle = stepSize * single i
                    let struct (sin, cos) = MathF.SinCos angle
                    v3 (radius * cos) (-radius * sin) 0f)) // a circle represented by numSides points clockwise

        // declare goos
        let spawnPositionToName (position : Vector3) = $"{name} {position.X} {position.Y}"
        for layer in 0 .. dec numLayers do
            for vertex in 0 .. dec numSides do
            let gooSpawnPosition = spawnPositions[layer][vertex]
            World.doBall2d (spawnPositionToName gooSpawnPosition)
                [Entity.Position |= spawnCenter + gooSpawnPosition
                 Entity.Size .= v3Dup 8f
                 Entity.StaticImage .= Assets.Gameplay.GooImage
                 Entity.Substance .= Density 0.2f
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
                    ("Previous", spawnPositions.[layer].[if vertex = 0 then dec numSides else dec vertex])
                 if layer > 0 then
                    ("Inner", spawnPositions.[dec layer].[vertex])] do

                // declare link
                let otherGooName = spawnPositionToName otherGooSpawnPosition
                let otherGooPosition = (world.ContextGroup / otherGooName).GetPosition world
                World.doBodyJoint2d $"{gooName} -> {linkRelation}"
                    [Entity.BodyJoint |= Box2dNetBodyJoint { CreateBodyJoint = fun _ _ a b world ->
                        let mutable jointDef = B2Joints.b2DefaultDistanceJointDef ()
                        jointDef.``base``.bodyIdA <- a
                        jointDef.``base``.bodyIdB <- b
                        // set the length to be based on initial position, otherwise the default length will be used
                        jointDef.length <- B2MathFunction.b2Distance (B2Bodies.b2Body_GetPosition a, B2Bodies.b2Body_GetPosition b)
                        jointDef.enableSpring <- true
                        jointDef.hertz <- (if layer = dec numLayers then 8f else 4f) / objectScale
                        jointDef.dampingRatio <- 0.5f
                        B2Joints.b2CreateDistanceJoint (world, &jointDef) }
                     Entity.BodyJointTarget .= Address.makeFromString $"^/{otherGooName}"
                     Entity.BodyJointTarget2 .= Address.makeFromString $"^/{gooName}"
                     Entity.BreakingPoint .= Some 100f] world |> ignore

                // declare link visualization
                if not (world.DeclaredEntity.GetBroken world) then
                    let direction = otherGooPosition - gooPosition
                    World.doStaticSprite $"{gooName} -> {linkRelation} Visual"
                        [Entity.Position @= (otherGooPosition + gooPosition) / 2f
                         Entity.Size @= v3 direction.Magnitude 2f 0f
                         Entity.Rotation @= Quaternion.CreateLookAt2d direction.V2
                         Entity.StaticImage .= Assets.Gameplay.LinkImage
                         Entity.Elevation .= -0.5f] world |> ignore

    static let declareStrandbeest (name : string) spawnCenter world =

        // original design by Theo Jansen Walker - https://strandbeest.com

        // declare chassis
        let objectScale = 10f
        let density = Density (1f / objectScale ** 2f)
        let pivot = v3 0f 0.8f 0f
        let wheelAnchor = v3 0f -0.8f 0f
        World.beginEntity<Box2dDispatcher> $"{name}"
            [Entity.Position |= spawnCenter + pivot * objectScale
             Entity.Size .= v3 5f 2f 0f * objectScale
             Entity.Elevation .= -0.7f
             Entity.Substance .= density
             Entity.CollisionGroup .= -1 // collision groups override collision categories and mask when non-0.
             // when two bodies have the same collision group and non-0, positive group means always collide, negative group means never collide.
             // it is useful to denote bodies that form the same object, e.g. the individual bodies within a strandbeest.
             // meanwhile, collision categories and mask are useful to denote body collision between different objects, like
             // between players and enemies.
             ]
            world |> ignore
        let chassis = world.ContextEntity

        // declare wheel
        World.doBall2d $"{name} Wheel"
            [Entity.Position |= spawnCenter + pivot * objectScale
             Entity.Size .= v3Dup 3.2f * objectScale
             Entity.Elevation .= -0.5f
             Entity.Substance .= density
             Entity.CollisionGroup .= -1
             Entity.MountOpt .= None] world |> ignore
        let wheel = world.DeclaredEntity
        
        // declare motor
        let (motor, _) =
            World.doBodyJoint2d $"{name} Motor"
                [Entity.BodyJoint |= Box2dNetBodyJoint { CreateBodyJoint = fun _ _ a b world ->
                    // specifying a motor for the revolute joint rotates the first body with a constant angular velocity.
                    let mutable jointDef = B2Joints.b2DefaultRevoluteJointDef ()
                    jointDef.``base``.bodyIdA <- a
                    jointDef.``base``.bodyIdB <- b
                    jointDef.enableMotor <- true
                    jointDef.motorSpeed <- 2f // radians per second. updated via World.setBodyJointMotorSpeed below
                    jointDef.maxMotorTorque <- 400f // maximum torque the motor can apply to achieve the desired motor speed
                    B2Joints.b2CreateRevoluteJoint (world, &jointDef) }
                 Entity.BodyJointTarget .= wheel.EntityAddress
                 Entity.BodyJointTarget2 .= chassis.EntityAddress
                 Entity.CollideConnected .= false
                 Entity.MountOpt .= None] world
        
        // declare legs
        for rotation in [-1f; 0f; 1f] do
            for (directionName, direction) in [("Left", -1f); ("Right", 1f)] do

                // declare leg body
                let p1 = v3 (direction * 5.4f) -6.1f 0f
                let p2 = v3 (direction * 7.2f) -1.2f 0f
                let p3 = v3 (direction * 4.3f) -1.9f 0f
                let p4 = v3 (direction * 3.1f) 0.8f 0f
                let p5 = v3 (direction * 6.0f) 1.5f 0f
                let p6 = v3 (direction * 2.5f) 3.7f 0f
                let legPolygon = if direction > 0f then [|p1; p2; p3|] else [|p1; p3; p2|]
                let shoulderPolygon = if direction > 0f then [|v3Zero; p5 - p4; p6 - p4|] else [|v3Zero; p6 - p4; p5 - p4|]
                let (_, legEvents) =
                    World.doBox2d $"{name} {directionName} {rotation} Leg"
                        [Entity.Position |= spawnCenter
                         Entity.Size .= v3Dup objectScale
                         Entity.Visible .= false
                         Entity.Substance .= density
                         Entity.BodyShape .= PointsShape { Points = legPolygon; Profile = Convex; TransformOpt = None; PropertiesOpt = None }
                         Entity.AngularDamping .= 10f
                         Entity.CollisionGroup .= -1
                         Entity.MountOpt .= None] world
                let leg = world.DeclaredEntity
                let legTransform = (leg.GetTransform world).AffineMatrix

                // declare visual representation of the leg
                legPolygon
                |> Array.add legPolygon[0]
                |> Array.map ((*) objectScale)
                |> Array.pairwise
                |> Array.iter (fun (p1, p2) ->
                    World.doStaticSprite $"{name} {directionName} {rotation} Leg Visual {p1} -> {p2}"
                        [let p1 = p1.Transform legTransform
                         let p2 = p2.Transform legTransform
                         Entity.Position @= (p1 + p2) / 2f
                         Entity.Size @= v3 (p2 - p1).Magnitude 2f 0f
                         Entity.Rotation @= Quaternion.CreateLookAt2d (p2 - p1).V2
                         Entity.StaticImage .= Assets.Default.Black
                         Entity.MountOpt .= None] world)

                // declare shoulder body
                let (_, shoulderEvents) =
                    World.doBox2d $"{name} {directionName} {rotation} Shoulder"
                        [Entity.Position |= spawnCenter + p4 * objectScale
                         Entity.Size .= v3Dup objectScale
                         Entity.Visible .= false
                         Entity.Substance .= density
                         Entity.BodyShape .= PointsShape { Points = shoulderPolygon; Profile = Convex; TransformOpt = None; PropertiesOpt = None }
                         Entity.AngularDamping .= 10f
                         Entity.CollisionGroup .= -1
                         Entity.MountOpt .= None] world
                let shoulder = world.DeclaredEntity
                let shoulderTransform = (shoulder.GetTransform world).AffineMatrix

                // declare visual representation of the shoulder
                shoulderPolygon
                |> Array.add shoulderPolygon.[0]
                |> Array.map ((*) objectScale)
                |> Array.pairwise
                |> Array.iter (fun (p1, p2) ->
                    World.doStaticSprite $"{name} {directionName} {rotation} Shoulder Visual {p1} -> {p2}"
                        [let p1 = p1.Transform shoulderTransform
                         let p2 = p2.Transform shoulderTransform
                         Entity.Position @= (p1 + p2) / 2f
                         Entity.Size @= v3 (p2 - p1).Magnitude 2f 0f
                         Entity.Rotation @= Quaternion.CreateLookAt2d (p2 - p1).V2
                         Entity.StaticImage .= Assets.Default.Black
                         Entity.MountOpt .= None] world)

                // handle motion reversal on collision
                for c in Seq.append shoulderEvents legEvents do
                    match c with
                    | BodyPenetrationData penetration ->
                        if acos (penetration.Normal.Dot v3Left) <= Constants.Physics.GroundAngleMax then // collision on strandbeest left side
                            World.setBodyJointMotorSpeed 2f motor world // move right
                        if acos (penetration.Normal.Dot v3Right) <= Constants.Physics.GroundAngleMax then // collision on strandbeest right side
                            World.setBodyJointMotorSpeed -2f motor world // move left
                    | _ -> ()

                // using a soft distance joint can reduce some jitter. it also makes the structure seem a bit more
                // fluid by acting like a suspension system.
                for (i, (entity1, entity2, position1, position2, entity1SpawnPosition, entity2SpawnPosition)) in

                    // declare distance joints between leg, shoulder, and wheel
                    List.indexed
                        [(leg, shoulder, p2, p5, v3Zero, p4)
                         (leg, shoulder, p3, p4, v3Zero, p4)
                         (leg, wheel, p3, pivot + wheelAnchor, v3Zero, (-wheelAnchor).Transform (Quaternion.CreateFromAngle2d (-rotation * 2f * MathF.PI_OVER_3)))
                         (shoulder, wheel, p6, pivot + wheelAnchor, p4, (-wheelAnchor).Transform (Quaternion.CreateFromAngle2d (-rotation * 2f * MathF.PI_OVER_3)))] do
                    World.doBodyJoint2d $"{name} {directionName} {rotation} Distance Joint {i}"
                        [Entity.BodyJoint |= Box2dNetBodyJoint { CreateBodyJoint = fun toPhysics toPhysicsV2 a b world' ->
                            if i = 0 then
                                // HACK: the Aether demo uses mutable rotations of the wheel when initializing, doing
                                // it here won't screw up the joint distances.
                                wheel.SetRotation (Quaternion.CreateFromAngle2d (rotation * 2f * MathF.PI_OVER_3)) world
                            let mutable jointDef = B2Joints.b2DefaultDistanceJointDef ()
                            jointDef.``base``.bodyIdA <- a
                            jointDef.``base``.bodyIdB <- b
                            jointDef.``base``.localFrameA.p <- B2Bodies.b2Body_GetLocalPoint (a, toPhysicsV2 (position1 * objectScale + spawnCenter))
                            jointDef.``base``.localFrameB.p <- B2Bodies.b2Body_GetLocalPoint (b, toPhysicsV2 (position2 * objectScale + spawnCenter))
                            jointDef.length <- toPhysics (position2 - position1).Magnitude * objectScale
                            jointDef.enableSpring <- true
                            jointDef.hertz <- 10f
                            jointDef.dampingRatio <- 0.5f
                            B2Joints.b2CreateDistanceJoint (world', &jointDef) }
                         Entity.BodyJointTarget .= entity1.EntityAddress
                         Entity.BodyJointTarget2 .= entity2.EntityAddress
                         Entity.MountOpt .= None] world |> ignore

                    // declare visual representation of the distance joint
                    World.doStaticSprite $"{name} {directionName} {rotation} Distance Joint Visual {i}"
                        [let p1 = (position1 * objectScale - entity1SpawnPosition * objectScale).Transform (entity1.GetTransform(world).AffineMatrix)
                         let p2 = (position2 * objectScale - entity2SpawnPosition * objectScale).Transform (entity2.GetTransform(world).AffineMatrix)
                         Entity.Position @= (p1 + p2) / 2f
                         Entity.Size @= v3 (p2 - p1).Magnitude 2f 0f
                         Entity.Rotation @= Quaternion.CreateLookAt2d (p2 - p1).V2
                         Entity.StaticImage .= Assets.Gameplay.LinkImage
                         Entity.Color .= color 1f 1f 1f 0.2f
                         Entity.Elevation .= -0.6f
                         Entity.MountOpt .= None] world

                // declare revolute joint between leg and shoulder
                World.doBodyJoint2d $"{name} {directionName} {rotation} Revolute Joint"
                    [Entity.BodyJoint |= Box2dNetBodyJoint { CreateBodyJoint = fun _ toPhysicsV2 a b world ->
                        let mutable jointDef = B2Joints.b2DefaultRevoluteJointDef ()
                        jointDef.``base``.bodyIdA <- a
                        jointDef.``base``.bodyIdB <- b
                        let revoluteCenter = toPhysicsV2 (p4 * objectScale + spawnCenter)
                        jointDef.``base``.localFrameA.p <- B2Bodies.b2Body_GetLocalPoint (a, revoluteCenter)
                        jointDef.``base``.localFrameB.p <- B2Bodies.b2Body_GetLocalPoint (b, revoluteCenter)
                        B2Joints.b2CreateRevoluteJoint (world, &jointDef) }
                     Entity.BodyJointTarget .= shoulder.EntityAddress
                     Entity.BodyJointTarget2 .= chassis.EntityAddress
                     Entity.MountOpt .= None] world |> ignore

        // end chassis declaration
        World.endEntity world

    static let generateGravities (world : World) =
        let defaultGravity = World.getGravityDefault2d world
        [(">", defaultGravity.Transform (Quaternion.CreateFromAngle2d MathF.PI_OVER_2))
         ("0", v3Zero)
         ("^", defaultGravity.Transform (Quaternion.CreateFromAngle2d MathF.PI))
         ("<", defaultGravity.Transform (Quaternion.CreateFromAngle2d -MathF.PI_OVER_2))]
        |> List.randomShuffle
        |> List.cons ("v", defaultGravity) // Always start with the default down gravity

    static let generateAvatarGravities (world : World) =
        let defaultGravity = World.getGravityDefault2d world
        [(">", GravityOverride <| defaultGravity.Transform (Quaternion.CreateFromAngle2d MathF.PI_OVER_2))
         ("0", GravityOverride <| v3Zero)
         ("^", GravityOverride <| defaultGravity.Transform (Quaternion.CreateFromAngle2d MathF.PI))
         ("<", GravityOverride <| defaultGravity.Transform (Quaternion.CreateFromAngle2d -MathF.PI_OVER_2))
         ("v", GravityOverride <| defaultGravity)]
        |> List.randomShuffle
        |> List.cons ("World", GravityWorld) // Always start with no override
    
    // here we define default property values
    static member Properties =
        [define Screen.EntityRedirects FMap.empty 
         define Screen.BodyIdRedirects FMap.empty
         define Screen.Toys FMap.empty
         define Screen.DragState None
         define Screen.MenuPage MenuPage1
         define Screen.InfoOpened false
         define Screen.Gravities []
         define Screen.AvatarGravities []]

    // here we define the toy box's behavior
    override this.Process (selectionResults, toyBox, world) =

        // declare scene when selected
        if toyBox.GetSelected world then

            // clean up toys and gravity when initializing
            if FQueue.contains Select selectionResults then
                toyBox.SetEntityRedirects FMap.empty world
                toyBox.SetBodyIdRedirects FMap.empty world
                toyBox.SetToys FMap.empty world
                toyBox.SetDragState None world
                toyBox.SetMenuPage MenuPage1 world
                toyBox.SetInfoOpened false world
                World.setGravity2d (World.getGravityDefault2d world) world
                toyBox.SetGravities (generateGravities world) world

            // all entities must be in a group - groups are the unit of entity loading
            World.beginGroup Simulants.ToyBoxScene.Name [] world

            // declare border
            World.doBlock2d Simulants.ToyBoxBorder.Name // uses static physics by default - it does not react to forces or collisions
                [Entity.Size .= v3 500f 350f 0f
                 Entity.BodyShape .= ContourShape // the body shape handles collisions and is independent of how it's displayed
                    { Links = // a contour shape provides one-sided collision for the right hand side of each link (inward in this case) allowing hollow shapes
                        [|v3 -0.5f 0.5f 0f // for entity body shapes, zero is the entity's center, one is the entity's size in positive direction
                          v3 0.5f 0.5f 0f
                          v3 0.5f -0.5f 0f
                          v3 -0.5f -0.5f 0f|]
                      Closed = true // specify the contour to connect the last point to the first point
                      TransformOpt = None
                      PropertiesOpt = None }
                 // collision categories is a binary mask, defaulting to "1" (units place). the border is set to be in a
                 // different category, "10" (twos place) because we define fans later to not collide with the border.
                 // meanwhile, unless we change the collision mask (Entity.CollisionMask), all entites default to collide
                 // with "*" (i.e. all collision categories).
                 Entity.CollisionCategories .= "10"
                 Entity.Elevation .= -1f // draw order of the same elevation prioritizes entities with lower vertical position for 2D games
                 Entity.StaticImage .= Assets.Gameplay.BackgroundImage] world |> ignore

            // declare avatar
            let (avatarBody, _) =
                World.doCharacter2d "Avatar"
                    [Entity.Gravity .= GravityWorld] world // characters have 3x gravity by default, get rid of it
            let avatar = world.DeclaredEntity

            // process avatar input
            if World.isKeyboardKeyDown KeyboardKey.Left world then
                World.applyBodyForce
                    ((-v3UnitX * if World.getBodyGrounded avatarBody world then 500f else 250f).Transform (avatar.GetRotation world))
                    None avatarBody world
            if World.isKeyboardKeyDown KeyboardKey.Right world then
                World.applyBodyForce
                    ((v3UnitX * if World.getBodyGrounded avatarBody world then 500f else 250f).Transform (avatar.GetRotation world))
                    None avatarBody world
            if Gravity.localize (World.getGravity2d world) (avatar.GetGravity world) = v3Zero then // float around when no gravity
                if World.isKeyboardKeyDown KeyboardKey.Up world then
                    World.applyBodyForce ((v3UnitY * 200f).Transform (avatar.GetRotation world))
                        None avatarBody world
                if World.isKeyboardKeyDown KeyboardKey.Down world then
                    World.applyBodyForce ((-v3UnitY * 200f).Transform (avatar.GetRotation world))
                        None avatarBody world
            elif World.isKeyboardKeyPressed KeyboardKey.Up world then
                World.jumpBody false 300f avatarBody world

            // process mouse interaction
            processMouseDragging toyBox world
            processMouseScrolling toyBox world
            if World.isMouseButtonPressed MouseRight world then
                World.applyExplosion2d (World.getMousePosition2dWorld false world).V3 200f 20f 100f UInt64.MaxValue world

            // declare paged menu
            match toyBox.GetMenuPage world with
            | MenuPage1 ->

                // first page of add toy buttons
                for (i, entityType) in List.indexed [Box; Ball; TinyBalls; Spring; Block; Bridge; Fan] do
                    if World.doButton $"Add {scstringMemo entityType}"
                        [Entity.Position .= v3 255f (160f - 30f * single i) 0f
                         Entity.Text .= $"Add {scstringMemo entityType}"
                         Entity.Elevation .= 1f] world then
                        toyBox.Toys.Map (FMap.add Gen.name entityType) world
                
                // next page
                if World.doButton "Down"
                    [Entity.Position .= v3 255f -50f 0f
                     Entity.Text .= "v"
                     Entity.Elevation .= 1f] world then
                    toyBox.SetMenuPage MenuPage2 world

            | MenuPage2 ->

                // previous page
                if World.doButton "Up"
                    [Entity.Position .= v3 255f 160f 0f
                     Entity.Text .= "^"
                     Entity.Elevation .= 1f] world then
                    toyBox.SetMenuPage MenuPage1 world

                // second page of add toy buttons
                for (i, entityType) in List.indexed [Clamp; Ragdoll; SoftBody; Web; Strandbeest] do
                    if World.doButton $"Add {scstringMemo entityType}"
                        [Entity.Position .= v3 255f (130f - 30f * single i) 0f
                         Entity.Text .= $"Add {scstringMemo entityType}"
                         Entity.Elevation .= 1f] world then
                        toyBox.Toys.Map (FMap.add Gen.name entityType) world

                // gravity button
                if toyBox.GetGravities world = [] then toyBox.SetGravities (generateGravities world) world
                let gravity = List.head (toyBox.GetGravities world)
                World.setGravity2d (snd gravity) world
                if World.doButton $"Gravity"
                    [Entity.Position .= v3 255f -20f 0f
                     Entity.Text @= $"Gravity: {fst gravity}"
                     Entity.Elevation .= 1f] world then
                    toyBox.Gravities.Map List.tail world

                // avatar gravity button
                if toyBox.GetAvatarGravities world = [] then toyBox.SetAvatarGravities (generateAvatarGravities world) world
                let gravity = List.head (toyBox.GetAvatarGravities world)
                avatar.SetGravity (snd gravity) world
                if World.doButton $"Avatar Gravity"
                    [Entity.Position .= v3 255f -50f 0f
                     Entity.Text @= $"Avatar Gravity: {fst gravity}"
                     Entity.Elevation .= 1f
                     Entity.FontSizing .= Some 10] world then
                    toyBox.AvatarGravities.Map List.tail world

            // clear toys button
            if World.doButton "Clear Toys"
                [Entity.Position .= v3 255f -100f 0f
                 Entity.Text .= "Clear Toys"
                 Entity.Elevation .= 1f] world then
                toyBox.SetToys FMap.empty world
                toyBox.SetEntityRedirects FMap.empty world
                toyBox.SetBodyIdRedirects FMap.empty world

            // switch screen button
            World.doButton Simulants.ToyBoxSwitchScreen.Name
                [Entity.Position .= v3 255f -130f 0f
                 Entity.Text .= "Switch Screen"
                 Entity.Elevation .= 1f] world |> ignore

            // info button
            if World.doButton "Info"
                [Entity.Position .= v3 255f -160f 0f
                 Entity.Text .= "Info"
                 Entity.Elevation .= 1f] world then
                toyBox.SetInfoOpened true world

            // info panel
            if toyBox.GetInfoOpened world then

                // declare info background - block button interactions behind info panel while opened
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

                // declare info entities
                World.doText "Info Origin 1"
                    [Entity.LayoutOrder .= 0
                     Entity.Text .= "Aether.Physics2D demos by nkast (Nikos Kastellanos)"] world
                World.doText "Info Origin 2"
                    [Entity.LayoutOrder .= 1
                     Entity.Text .= "Ported to Nu by Happypig375 (Hadrian Tang)"] world
                World.doText "Info Controls"
                    [Entity.LayoutOrder .= 2
                     Entity.Justification .= Unjustified true
                     Entity.Text .=
                        "Controls: Left/Right/Up - Move Avatar. Left/Right - Accelerate Car, Down - Brake.\n\
                         Mouse Left - Click button or Drag entity. Mouse Right - Cause an explosion.\n\
                         Mouse Wheel - Apply rotation to entity.\n\
                         Alt+F4 - Close game if not in Editor. Read source code for explanations!"
                     Entity.FontSizing .= Some 10
                     Entity.TextMargin .= v2 5f 0f] world
                if World.doButton "Info Close"
                    [Entity.LayoutOrder .= 3
                     Entity.Text .= "Close"] world then
                    toyBox.SetInfoOpened false world
                if World.doButton "Info Exit"
                    [Entity.LayoutOrder .= 4
                     Entity.Text .= "Exit"] world && world.Unaccompanied then
                    World.exit world

                // end info panel declaration
                World.endPanel world

                // declare info links
                for (position, size, url) in
                    [(v2 -126f 115f, v2 200f 32f, "https://github.com/nkast/Aether.Physics2D/tree/main/Samples/NewSamples/Demos")
                     (v2 25f 115f, v2 50f 32f, "https://github.com/nkast")
                     (v2 -127.5f 57.5f, v2 115f 32f, "https://github.com/bryanedds/Nu/pull/1120")
                     (v2 3.5f 57.5f, v2 105f 32f, "https://github.com/Happypig375")] do
                    if World.doButton $"Info Origin Button {url.Replace ('/', '\\')}"
                        [Entity.Position .= position.V3
                         Entity.Size .= size.V3
                         Entity.Elevation .= 11f] world then
                        Process.Start (ProcessStartInfo (url, UseShellExecute = true)) |> ignore

            // declare toys
            let spawnCenter = v3Zero
            for KeyValue (name, toy) in toyBox.GetToys world do
                match toy with
                | Box -> declareBox name spawnCenter world
                | Block -> declareBlock name spawnCenter world
                | Ball -> declareBall name spawnCenter world
                | TinyBalls -> declareTinyBalls name spawnCenter world
                | Spring -> declareSpring name spawnCenter world
                | Bridge -> declareBridge name spawnCenter world
                | Fan -> declareFan name spawnCenter toyBox world
                | Clamp -> declareClamp name spawnCenter world
                | Ragdoll -> declareRagdoll name spawnCenter world
                | SoftBody -> declareSoftBody name spawnCenter toyBox world
                | Web -> declareWeb name spawnCenter world
                | Strandbeest -> declareStrandbeest name spawnCenter world

            // end scene declaration
            World.endGroup world

            // process camera as last task
            World.setEye2dCenter (v2 60f 0f) world