// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open BepuPhysics
open BepuUtilities
open BepuUtilities.Memory
open Prime
open Nu
open BepuPhysics.Collidables

/// The BepuPhysics 3d implementation of PhysicsEngine.
type [<ReferenceEquality>] BepuPhysicsEngine =
    private
        { PhysicsContext : Simulation
          PhysicsMessages : PhysicsMessage UList
          IntegrationMessages : IntegrationMessage List
          ThreadDispatcher : ThreadDispatcher
          ContactEvents : ContactEvents
          NarrowPhaseCallbacks : NarrowPhaseCallbacks
          PoseIntegratorCallbacks : PoseIntegratorCallbacks
          mutable RebuildingHack : bool }

    static member make () =
        let threadDispatcher = new ThreadDispatcher (Constants.Physics.TargetThreadCount)
        let bufferPool = new BufferPool ()
        let contactEvents = ContactEvents (threadDispatcher, bufferPool)
        let narrowPhaseCallbacks = NarrowPhaseCallbacks ()
        let poseIntegratorCallbacks = PoseIntegratorCallbacks Constants.Engine.GravityDefault
        let solveDescription = new SolveDescription (8, 1)
        let simulation = Simulation.Create (bufferPool, narrowPhaseCallbacks, poseIntegratorCallbacks, solveDescription)
        { PhysicsContext = simulation
          PhysicsMessages = UList.makeEmpty Imperative
          IntegrationMessages = List ()
          ThreadDispatcher = threadDispatcher
          ContactEvents = contactEvents
          NarrowPhaseCallbacks = narrowPhaseCallbacks
          PoseIntegratorCallbacks = poseIntegratorCallbacks
          RebuildingHack = false }

    static member private attachBodyBox (bodyBox : BodyBox) (bodyProperties : BodyProperties) (compoundBuilder : CompoundBuilder byref) physicsEngine =
        let box = Box (bodyBox.Size.X, bodyBox.Size.Y, bodyBox.Size.Z)
        let volume = bodyBox.Size.X * bodyBox.Size.Y * bodyBox.Size.Z
        let mass = volume * bodyProperties.Density
        let pose = RigidPose (bodyProperties.Center, bodyProperties.Rotation)
        compoundBuilder.Add (&box, &pose, mass) // NOTE: passing mass as weight.

    static member private createCompoundSingleton attachBody bodyProperties bodyBox physicsEngine =

        use compoundBuilder = CompoundBuilder (physicsEngine.PhysicsContext.BufferPool, physicsEngine.PhysicsContext.Shapes, 1)
        attachBody bodyProperties &compoundBuilder physicsEngine
        let mutable compoundChildren = Buffer<CompoundChild> ()
        let mutable compoundInertia = BodyInertia ()
        let mutable compoundCenter = Vector3 ()
        compoundBuilder.BuildDynamicCompound (&compoundChildren, &compoundInertia, &compoundCenter)
        
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
        
        if not bodyProperties.IgnoreEvents then
            match handle with
            | Left _ -> ()
            | Right bodyHandle -> physicsEngine.ContactEvents.Register (bodyHandle, _)

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
        // NOTE: P1: We should really be querying these bodies from the physics engine's internally-maintained
        // awake-body list for better performance. It's quite suboptimal to have to iterate through all bodies!
        // Note also that I tried building Farseer with #define USE_AWAKE_BODY_SET so we can query from that
        // AwakeBodyList, but there are compilation errors that, when I tried to fix, broke the whole system.
        for body in physicsEngine.PhysicsContext. do
            if body.Awake && body.BodyType <> Dynamics.BodyType.Static then
                let bodyTransformMessage =
                    BodyTransformMessage
                        { BodySource = body.Tag :?> BodySourceInternal
                          Center = AetherPhysicsEngine.toPixelV3 body.Position
                          Rotation = (v3 0.0f 0.0f -body.Rotation).RollPitchYaw
                          LinearVelocity = AetherPhysicsEngine.toPixelV3 body.LinearVelocity
                          AngularVelocity = v3 body.AngularVelocity 0.0f 0.0f }
                physicsEngine.IntegrationMessages.Add bodyTransformMessage

    interface PhysicsEngine with

        member physicsEngine.BodyExists physicsId =
            ()

        member physicsEngine.GetBodyContactNormals physicsId =
            ()

        member physicsEngine.GetBodyLinearVelocity physicsId =
            ()

        member physicsEngine.GetBodyToGroundContactNormals physicsId =
            ()

        member physicsEngine.GetBodyToGroundContactNormalOpt physicsId =
            ()

        member physicsEngine.GetBodyToGroundContactTangentOpt physicsId =
            ()

        member physicsEngine.IsBodyOnGround physicsId =
            ()

        member physicsEngine.PopMessages () =
            ()

        member physicsEngine.ClearMessages () =
            ()

        member physicsEngine.EnqueueMessage physicsMessage =
            ()

        member physicsEngine.Integrate stepTime physicsMessages =
            AetherPhysicsEngine.handlePhysicsMessages physicsMessages physicsEngine
            let physicsStepAmount =
                match (Constants.Engine.DesiredFrameRate, stepTime) with
                | (StaticFrameRate frameRate, UpdateTime frames) -> 1.0f / single frameRate * single frames
                | (DynamicFrameRate _, ClockTime secs) -> secs
                | (_, _) -> failwithumf ()
            physicsEngine.PhysicsContext.Solve physicsStepAmount
            AetherPhysicsEngine.createIntegrationMessages physicsEngine
            let integrationMessages = SegmentedArray.ofSeq physicsEngine.IntegrationMessages
            physicsEngine.IntegrationMessages.Clear ()
            integrationMessages