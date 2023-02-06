// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open BepuPhysics
open BepuPhysics.Collidables
open BepuPhysics.EventSystem
open BepuUtilities
open BepuUtilities.Memory
open Prime
open Nu

type internal ContactEventHandler (integrationMessages : IntegrationMessage List, shapeSources : CollidableProperty<BodyShapeSourceInternalJib> array) =
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

    static member private attachBodyBox (bodyBox : BodyBox) (bodyProperties : BodyProperties) (compoundBuilder : CompoundBuilder array) physicsEngine =
        let box = Box (bodyBox.Size.X, bodyBox.Size.Y, bodyBox.Size.Z)
        let volume = bodyBox.Size.X * bodyBox.Size.Y * bodyBox.Size.Z
        let mass = volume * bodyProperties.Density
        let pose = RigidPose (bodyProperties.Center, bodyProperties.Rotation)
        compoundBuilder.[0].Add (&box, &pose, mass) // NOTE: passing mass as weight.

    static member private createCompoundSingleton attachBody bodyShapeSource (bodyProperties : BodyProperties) physicsEngine =

        let compoundBuilder = Array.init 1 (fun _ -> new CompoundBuilder (physicsEngine.PhysicsContext.BufferPool, physicsEngine.PhysicsContext.Shapes, 1))
        try attachBody bodyProperties compoundBuilder physicsEngine
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

            match handle with
            | Left staticHandle ->
                physicsEngine.ShapeSources.[0].[staticHandle] <- BodyShapeSourceInternalJib bodyShapeSource
            | Right bodyHandle ->
                physicsEngine.ShapeSources.[0].[bodyHandle] <- BodyShapeSourceInternalJib bodyShapeSource
                if not bodyProperties.IgnoreEvents then
                    physicsEngine.ContactEvents.Register (bodyHandle, physicsEngine.ContactEventHandler)

        finally compoundBuilder.[0].Dispose ()

    static member private createCompoundBox bodyBox bodyProperties physicsEngine =
        BepuPhysicsEngine.createCompoundSingleton (BepuPhysicsEngine.attachBodyBox bodyBox) bodyProperties physicsEngine

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