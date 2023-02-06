// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open System.Runtime.CompilerServices
open BepuPhysics
open BepuPhysics.Collidables
open BepuPhysics.CollisionDetection
open BepuPhysics.Constraints
open BepuPhysics.EventSystem
open BepuUtilities
open BepuUtilities.Memory
open Prime
open Nu

type private PoseIntegratorCallbacks =
    struct
        val Gravity : Vector3
        val mutable GravityWideDelta : Vector3Wide
        end

    new (gravity) =
        { Gravity = gravity
          GravityWideDelta = Vector3Wide () }

    interface IPoseIntegratorCallbacks with
        member this.AngularIntegrationMode = AngularIntegrationMode.Nonconserving
        member this.AllowSubstepsForUnconstrainedBodies = false
        member this.IntegrateVelocityForKinematics = false
        member this.Initialize (_ : Simulation) = ()
        member this.PrepareForIntegration (clockDelta : single) = this.GravityWideDelta <- Vector3Wide.Broadcast (this.Gravity * clockDelta)
        member this.IntegrateVelocity (_ : Vector<int>, _ : Vector3Wide, _ : QuaternionWide, _ : BodyInertiaWide, _ : Vector<int>, _ : int, _ : Vector<single>, velocity : BodyVelocityWide byref) = velocity.Linear <- velocity.Linear + this.GravityWideDelta

type private NarrowPhaseCallbacks =
    struct
        val mutable ContactEvents : ContactEvents
        end

    member this.Dispose () =
        this.ContactEvents.Dispose ()

    interface INarrowPhaseCallbacks with

        [<MethodImpl (MethodImplOptions.AggressiveInlining)>]
        member this.AllowContactGeneration (_ : int, _ : CollidablePair, _ : int, _ : int) =
            true

        [<MethodImpl (MethodImplOptions.AggressiveInlining)>]
        member this.ConfigureContactManifold (_ : int, _ : CollidablePair, _ : int, _ : int, _ : ConvexContactManifold byref) =
            true

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.ConfigureContactManifold<'TManifold when 'TManifold : (new : unit -> 'TManifold) and 'TManifold :> IContactManifold<'TManifold>>
            (workerIndex : int, pair : CollidablePair, manifold : 'TManifold byref, pairMaterial : PairMaterialProperties byref) =
            pairMaterial.FrictionCoefficient <- 1f
            pairMaterial.MaximumRecoveryVelocity <- 2f
            pairMaterial.SpringSettings <- SpringSettings (30.0f, 1.0f)
            this.ContactEvents.HandleManifold (workerIndex, pair, &manifold)
            true

        [<MethodImpl (MethodImplOptions.AggressiveInlining)>]
        member this.AllowContactGeneration (_ : int, a : CollidableReference, b : CollidableReference, _ : single byref) =
            a.Mobility = CollidableMobility.Dynamic ||
            b.Mobility = CollidableMobility.Dynamic

        member this.Initialize (simulation : Simulation) =
            this.ContactEvents.Initialize simulation

        member this.Dispose () =
            this.Dispose ()

type private ContactEventHandler (integrationMessages : IntegrationMessage List, shapeSources : CollidableProperty<BodyShapeSourceInternalJib> array) =
    interface IContactEventHandler with

        member this.OnContactAdded (eventSource, pair, contactManifold, contactOffset, contactNormal, depth, featureId, contactIndex, workerIndex) = ()
        member this.OnContactRemoved (eventSource, pair, contactManifold, removedFeatureId, workerIndex) = ()
        member this.OnPairCreated (eventSource, pair, contactManifold, workerIndex) = ()
        member this.OnPairUpdated (arg1eventSource, pair, contactManifold, workerIndex) = ()
        member this.OnPairEnded (eventSource, pair) = ()
        member this.OnTouchingUpdated (eventSource, pair, contactManifold, workerIndex) = ()

        member this.OnTouchingStarted (eventSource, pair, contactManifold, workerIndex) =
            let (shapeSource, shapeSource2) =
                if pair.A = eventSource
                then (shapeSources.[0].[pair.A], shapeSources.[0].[pair.B])
                else (shapeSources.[0].[pair.B], shapeSources.[0].[pair.A])
            let bodyCollisionMessage =
                { BodyShapeSource = shapeSource.BodyShapeSourceInternalObj :?> BodyShapeSourceInternal
                  BodyShapeSource2 = shapeSource2.BodyShapeSourceInternalObj :?> BodyShapeSourceInternal
                  Normal = contactManifold.GetNormal (&contactManifold, 0) }
            integrationMessages.Add (IntegrationMessage.BodyCollisionMessage bodyCollisionMessage)

        member this.OnTouchingStopped (eventSource, pair, contactManifold, workerIndex) =
            let (shapeSource, shapeSource2) =
                if pair.A = eventSource
                then (shapeSources.[0].[pair.A], shapeSources.[0].[pair.B])
                else (shapeSources.[0].[pair.B], shapeSources.[0].[pair.A])
            let bodySeparationMessage =
                { BodyShapeSource = shapeSource.BodyShapeSourceInternalObj :?> BodyShapeSourceInternal
                  BodyShapeSource2 = shapeSource2.BodyShapeSourceInternalObj :?> BodyShapeSourceInternal }
            integrationMessages.Add (IntegrationMessage.BodySeparationMessage bodySeparationMessage)

/// The BepuPhysics 3d implementation of PhysicsEngine.
type [<ReferenceEquality>] BepuPhysicsEngine =
    private
        { PhysicsContext : Simulation
          PhysicsMessages : PhysicsMessage UList
          IntegrationMessages : IntegrationMessage List
          ThreadDispatcher : ThreadDispatcher
          ShapeSources : CollidableProperty<BodyShapeSourceInternalJib> array
          ContactEvents : ContactEvents
          ContactEventHandler : ContactEventHandler
          mutable RebuildingHack : bool }

    static member make () =
        let physicsMessages = UList.makeEmpty Imperative
        let integrationMessages = List ()
        let threadDispatcher = new ThreadDispatcher (Constants.Physics.TargetThreadCount)
        let bufferPool = new BufferPool ()
        let shapeSources = Array.init 1 (fun _ -> new CollidableProperty<BodyShapeSourceInternalJib> ())
        let contactEvents = ContactEvents (threadDispatcher, bufferPool)
        let contactEventHandler = ContactEventHandler (integrationMessages, shapeSources)
        let narrowPhaseCallbacks = NarrowPhaseCallbacks ()
        let poseIntegratorCallbacks = PoseIntegratorCallbacks Constants.Engine.GravityDefault
        let solveDescription = new SolveDescription (8, 1)
        let simulation = Simulation.Create (bufferPool, narrowPhaseCallbacks, poseIntegratorCallbacks, solveDescription)
        { PhysicsContext = simulation
          PhysicsMessages = physicsMessages
          IntegrationMessages = integrationMessages
          ThreadDispatcher = threadDispatcher
          ShapeSources = shapeSources
          ContactEvents = contactEvents
          ContactEventHandler = contactEventHandler
          RebuildingHack = false }

    static member cleanUp physicsEngine =
        physicsEngine.ContactEvents.Dispose ()
        physicsEngine.ShapeSources.[0].Dispose ()
        physicsEngine.ThreadDispatcher.Dispose ()
        physicsEngine.PhysicsContext.Dispose ()

    static member private createBody4 attachBodyShape bodyPropertiesOpt (bodyProperties : BodyProperties) (bodySource : BodySourceInternal) physicsEngine =

        let compoundBuilder = Array.init 1 (fun _ -> new CompoundBuilder (physicsEngine.PhysicsContext.BufferPool, physicsEngine.PhysicsContext.Shapes, 1))
        try attachBodyShape bodyProperties compoundBuilder physicsEngine
            let mutable compoundChildren = Buffer<CompoundChild> ()
            let mutable compoundInertia = BodyInertia ()
            let mutable compoundCenter = Vector3 ()
            compoundBuilder.[0].BuildDynamicCompound (&compoundChildren, &compoundInertia, &compoundCenter)

            let velocities = BodyVelocity (bodyProperties.LinearVelocity, bodyProperties.AngularVelocity)
            let compound = Compound compoundChildren
            let shapeIndex = physicsEngine.PhysicsContext.Shapes.Add &compound
            let activity = BodyActivityDescription Constants.Physics.SleepThreshold
            let handle =
                match bodyProperties.BodyType with
                | Static ->
                    let staticDescription = StaticDescription (RigidPose (), shapeIndex)
                    physicsEngine.PhysicsContext.Statics.Add &staticDescription |> Left
                | Dynamic ->
                    let bodyDescription = BodyDescription.CreateDynamic (RigidPose (), velocities, compoundInertia, shapeIndex, activity)
                    physicsEngine.PhysicsContext.Bodies.Add &bodyDescription |> Right
                | Kinematic ->
                    let bodyDescription = BodyDescription.CreateKinematic (RigidPose (), velocities, shapeIndex, activity)
                    physicsEngine.PhysicsContext.Bodies.Add &bodyDescription |> Right

            let bodyShapeSource =
                { Simulant = bodySource.Simulant
                  BodyId = bodySource.BodyId
                  ShapeId = match bodyPropertiesOpt with Some p -> p.BodyShapeId | None -> 0UL }
            match handle with
            | Left staticHandle ->
                physicsEngine.ShapeSources.[0].[staticHandle] <- BodyShapeSourceInternalJib bodyShapeSource
            | Right bodyHandle ->
                physicsEngine.ShapeSources.[0].[bodyHandle] <- BodyShapeSourceInternalJib bodyShapeSource
                if not bodyProperties.IgnoreEvents then
                    physicsEngine.ContactEvents.Register (bodyHandle, physicsEngine.ContactEventHandler)

        finally compoundBuilder.[0].Dispose ()

    static member private attachBodySphere (bodySphere : BodySphere) (bodyProperties : BodyProperties) (compoundBuilder : CompoundBuilder array) physicsEngine =
        let sphere = Collidables.Sphere bodySphere.Radius
        let volume = 4.0f / 3.0f * MathF.PI * pown bodySphere.Radius 3
        let mass = volume * bodyProperties.Density
        let pose = RigidPose (bodyProperties.Center, bodyProperties.Rotation)
        compoundBuilder.[0].Add (&sphere, &pose, mass) // NOTE: passing mass as weight.

    static member private attachBodyBox (bodyBox : BodyBox) (bodyProperties : BodyProperties) (compoundBuilder : CompoundBuilder array) physicsEngine =
        let box = Box (bodyBox.Size.X, bodyBox.Size.Y, bodyBox.Size.Z)
        let volume = bodyBox.Size.X * bodyBox.Size.Y * bodyBox.Size.Z
        let mass = volume * bodyProperties.Density
        let pose = RigidPose (bodyProperties.Center, bodyProperties.Rotation)
        compoundBuilder.[0].Add (&box, &pose, mass) // NOTE: passing mass as weight.

    static member private attachBodyCapsule (bodyCapsule : BodyCapsule) (bodyProperties : BodyProperties) (compoundBuilder : CompoundBuilder array) physicsEngine =
        let capsule = Capsule (bodyCapsule.Radius, bodyCapsule.Length)
        let volume = MathF.PI * bodyCapsule.Radius |> flip pown 2
        let mass = volume * bodyProperties.Density
        let pose = RigidPose (bodyProperties.Center, bodyProperties.Rotation)
        compoundBuilder.[0].Add (&capsule, &pose, mass) // NOTE: passing mass as weight.

    static member private attachBodyTriangle a b c (bodyProperties : BodyProperties) (compoundBuilder : CompoundBuilder array) physicsEngine =
        let capsule = Triangle (a, b, c)
        let ab = (b - a).Magnitude // NOTE: using Heron's formula.
        let bc = (c - b).Magnitude
        let ca = (a - c).Magnitude
        let s = (ab + bc + ca) * 0.5f
        let volume = sqrt (s * (s - ab) * (s - bc) * (s - ca))
        let mass = volume * bodyProperties.Density
        let pose = RigidPose (bodyProperties.Center, bodyProperties.Rotation)
        compoundBuilder.[0].Add (&capsule, &pose, mass) // NOTE: passing mass as weight.

    static member private attachBodyPolygon bodyPolygon bodyProperties compoundBuilder physicsEngine =
        if bodyPolygon.Vertices.Length >= 3 then
            let triangles = Array.windowed 3 bodyPolygon.Vertices
            for triangle in triangles do
                let (a, b, c) = (triangle.[0], triangle.[1], triangle.[2])
                BepuPhysicsEngine.attachBodyTriangle a b c bodyProperties compoundBuilder physicsEngine
        else Log.debug "Degenerate polygon sent to BepuPhysicsEngine; 3 or more vertices required."

    static member private attachBodyBoxRounded (bodyBoxRounded : BodyBoxRounded) (bodyProperties : BodyProperties) (compoundBuilder : CompoundBuilder array) physicsEngine =
        Log.debug "Rounded box not yet implemented via BepuPhysicsEngine; creating a normal box instead."
        let bodyBox = { Center = bodyBoxRounded.Center; Size = bodyBoxRounded.Size; PropertiesOpt = bodyBoxRounded.PropertiesOpt }
        BepuPhysicsEngine.attachBodyBox bodyBox bodyProperties compoundBuilder physicsEngine

    static member private attachBodyShapes bodyShapes bodyProperties compoundBuilder physicsEngine =
        for bodyShape in bodyShapes do
            BepuPhysicsEngine.attachBodyShape bodyShape bodyProperties compoundBuilder physicsEngine

    static member private attachBodyShape bodyShape bodyProperties compoundBuilder physicsEngine =
        match bodyShape with
        | BodyEmpty -> ()
        | BodyBox bodyBox -> BepuPhysicsEngine.attachBodyBox bodyBox bodyProperties compoundBuilder physicsEngine
        | BodySphere bodySphere -> BepuPhysicsEngine.attachBodySphere bodySphere bodyProperties compoundBuilder physicsEngine
        | BodyCapsule bodyCapsule -> BepuPhysicsEngine.attachBodyCapsule bodyCapsule bodyProperties compoundBuilder physicsEngine
        | BodyBoxRounded bodyBoxRounded -> BepuPhysicsEngine.attachBodyBoxRounded bodyBoxRounded bodyProperties compoundBuilder physicsEngine
        | BodyPolygon bodyPolygon -> BepuPhysicsEngine.attachBodyPolygon bodyPolygon bodyProperties compoundBuilder physicsEngine
        | BodyShapes bodyShapes -> BepuPhysicsEngine.attachBodyShapes bodyShapes bodyProperties compoundBuilder physicsEngine

    static member private createBody3 bodyShape bodyProperties bodyShapeSource physicsEngine =
        BepuPhysicsEngine.createBody4 (BepuPhysicsEngine.attachBodyShape bodyShape) bodyProperties bodyShapeSource physicsEngine

    static member private createBody (createBodyMessage : CreateBodyMessage) physicsEngine =

        // get fields
        let sourceSimulant = createBodyMessage.SourceSimulant
        let bodyProperties = createBodyMessage.BodyProperties
        let bodyRotation = bodyProperties.Rotation
        let bodySource = { Simulant = sourceSimulant; BodyId = bodyProperties.BodyId }

        // make the body
        BepuPhysicsEngine.createBody3 bodyProperties.BodyShape bodyProperties bodySource physicsEngine

        // configure body
        AetherPhysicsEngine.configureBodyProperties bodyProperties body

        // attach body shape
        AetherPhysicsEngine.attachBodyShape sourceSimulant bodyProperties.BodyShape bodyProperties body |> ignore

        // listen for collisions
        body.add_OnCollision (fun bodyShape bodyShape2 collision -> AetherPhysicsEngine.handleCollision physicsEngine bodyShape bodyShape2 collision)

        // listen for separations
        // TODO: P1: use the contact variable as well?
        body.add_OnSeparation (fun bodyShape bodyShape2 _ -> AetherPhysicsEngine.handleSeparation physicsEngine bodyShape bodyShape2)

        // attempt to add the body
        if not (physicsEngine.Bodies.TryAdd ({ SourceId = createBodyMessage.SourceId; CorrelationId = bodyProperties.BodyId }, (bodyProperties.GravityScale, body))) then
            Log.debug ("Could not add body via '" + scstring bodyProperties + "'.")

    static member private handlePhysicsMessage physicsEngine physicsMessage =
        match physicsMessage with
        | CreateBodyMessage createBodyMessage -> BepuPhysicsEngine.createBody createBodyMessage physicsEngine
        | CreateBodiesMessage createBodiesMessage -> BepuPhysicsEngine.createBodies createBodiesMessage physicsEngine
        | DestroyBodyMessage destroyBodyMessage -> BepuPhysicsEngine.destroyBody destroyBodyMessage physicsEngine
        | DestroyBodiesMessage destroyBodiesMessage -> BepuPhysicsEngine.destroyBodies destroyBodiesMessage physicsEngine
        | CreateJointMessage createJointMessage -> BepuPhysicsEngine.createJoint createJointMessage physicsEngine
        | CreateJointsMessage createJointsMessage -> BepuPhysicsEngine.createJoints createJointsMessage physicsEngine
        | DestroyJointMessage destroyJointMessage -> BepuPhysicsEngine.destroyJoint destroyJointMessage physicsEngine
        | DestroyJointsMessage destroyJointsMessage -> BepuPhysicsEngine.destroyJoints destroyJointsMessage physicsEngine
        | SetBodyEnabledMessage setBodyEnabledMessage -> BepuPhysicsEngine.setBodyEnabled setBodyEnabledMessage physicsEngine
        | SetBodyPositionMessage setBodyPositionMessage -> BepuPhysicsEngine.setBodyPosition setBodyPositionMessage physicsEngine
        | SetBodyRotationMessage setBodyRotationMessage -> BepuPhysicsEngine.setBodyRotation setBodyRotationMessage physicsEngine
        | SetBodyAngularVelocityMessage setBodyAngularVelocityMessage -> BepuPhysicsEngine.setBodyAngularVelocity setBodyAngularVelocityMessage physicsEngine
        | ApplyBodyAngularImpulseMessage applyBodyAngularImpulseMessage -> BepuPhysicsEngine.applyBodyAngularImpulse applyBodyAngularImpulseMessage physicsEngine
        | SetBodyLinearVelocityMessage setBodyLinearVelocityMessage -> BepuPhysicsEngine.setBodyLinearVelocity setBodyLinearVelocityMessage physicsEngine
        | ApplyBodyLinearImpulseMessage applyBodyLinearImpulseMessage -> BepuPhysicsEngine.applyBodyLinearImpulse applyBodyLinearImpulseMessage physicsEngine
        | ApplyBodyForceMessage applyBodyForceMessage -> BepuPhysicsEngine.applyBodyForce applyBodyForceMessage physicsEngine
        | ApplyBodyTorqueMessage applyBodyTorqueMessage -> BepuPhysicsEngine.applyBodyTorque applyBodyTorqueMessage physicsEngine
        | SetGravityMessage gravity -> physicsEngine.PhysicsContext.Gravity <- BepuPhysicsEngine.toPhysicsV2 gravity
        | RebuildPhysicsHackMessage ->
            physicsEngine.RebuildingHack <- true
            physicsEngine.PhysicsContext.Clear ()
            physicsEngine.Bodies.Clear ()
            physicsEngine.IntegrationMessages.Clear ()

    static member private integrate stepTime physicsEngine =

        let physicsStepAmount =
            match (Constants.Engine.DesiredFrameRate, stepTime) with
            | (StaticFrameRate frameRate, UpdateTime frames) -> 1.0f / single frameRate * single frames |> min 0.5f
            | (DynamicFrameRate _, ClockTime secs) -> secs |> min 0.5f
            | (_, _) -> failwithumf ()

        //Note that taking steps of variable length can reduce stability.
        if physicsStepAmount > 0.0f then
            physicsEngine.PhysicsContext.Timestep (physicsStepAmount, physicsEngine.ThreadDispatcher)

        // flush contact events
        physicsEngine.ContactEvents.Flush ()

    static member private createIntegrationMessages physicsEngine =
        let shapeSources = physicsEngine.ShapeSources
        let bodies = physicsEngine.PhysicsContext.Bodies
        let bodiesActive = bodies.ActiveSet
        for i in 0 .. bodiesActive.Count do
            let bodyHandle = bodiesActive.IndexToHandle.[i]
            let body = bodies.[bodyHandle]
            if body.Awake then
                let shapeSource = shapeSources.[0].[bodyHandle].BodyShapeSourceInternalObj :?> BodyShapeSourceInternal
                let bodySource = { BodySourceInternal.Simulant = shapeSource.Simulant; BodyId = shapeSource.BodyId }
                let bodyTransformMessage =
                    BodyTransformMessage
                        { BodySource = bodySource
                          Center = body.Pose.Position
                          Rotation = body.Pose.Orientation
                          LinearVelocity = body.Velocity.Linear
                          AngularVelocity = body.Velocity.Angular }
                physicsEngine.IntegrationMessages.Add bodyTransformMessage

        static member private handlePhysicsMessages physicsMessages physicsEngine = () // TODO.

    interface PhysicsEngine with

        member physicsEngine.BodyExists physicsId = () // TODO.
        member physicsEngine.GetBodyContactNormals physicsId = () // TODO.
        member physicsEngine.GetBodyLinearVelocity physicsId = () // TODO.
        member physicsEngine.GetBodyToGroundContactNormals physicsId = () // TODO.
        member physicsEngine.GetBodyToGroundContactNormalOpt physicsId = () // TODO.
        member physicsEngine.GetBodyToGroundContactTangentOpt physicsId = () // TODO.
        member physicsEngine.IsBodyOnGround physicsId = () // TODO.
        member physicsEngine.PopMessages () = () // TODO.
        member physicsEngine.ClearMessages () = () // TODO.
        member physicsEngine.EnqueueMessage physicsMessage = () // TODO.

        member physicsEngine.Integrate stepTime physicsMessages =
            BepuPhysicsEngine.handlePhysicsMessages physicsMessages physicsEngine
            BepuPhysicsEngine.integrate stepTime physicsEngine
            BepuPhysicsEngine.createIntegrationMessages physicsEngine
            let integrationMessages = SegmentedArray.ofSeq physicsEngine.IntegrationMessages
            physicsEngine.IntegrationMessages.Clear ()
            integrationMessages