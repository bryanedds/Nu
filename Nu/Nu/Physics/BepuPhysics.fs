// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open BepuPhysics
open BepuPhysics.Collidables
open BepuPhysics.CollisionDetection
open BepuUtilities
open BepuUtilities.Memory
open Prime
open Nu

type [<Struct>] PoseIntegratorCallbacks =
    interface IPoseIntegratorCallbacks with
        member this.AngularIntegrationMode = failwithnie ()
        member this.AllowSubstepsForUnconstrainedBodies = failwithnie ()
        member this.IntegrateVelocityForKinematics = failwithnie ()
        member this.Initialize simulation = failwithnie ()
        member this.IntegrateVelocity (bodyIndices, position, orientation, localInertia, integrationMask, workerIndex, dt, velocity) = failwithnie ()
        member this.PrepareForIntegration dt = failwithnie ()

type [<Struct>] NarrowPhaseCallbacks =
    interface INarrowPhaseCallbacks with
        member this.AllowContactGeneration (workerIndex : int, a : CollidableReference, b : CollidableReference, speculativeMargin : single byref) = failwithnie () : bool
        member this.AllowContactGeneration (workerIndex : int, pair : CollidablePair, childIndexA : int, childIndexB : int) = failwithnie () : bool
        member this.ConfigureContactManifold (workerIndex, pair, manifold, pairMaterial) = failwithnie ()
        member this.ConfigureContactManifold (workerIndex, pair, childIndexA, childIndexB, manifold) = failwithnie ()
        member this.Dispose() = failwithnie ()
        member this.Initialize simulation = failwithnie ()

/// The BepuPhysics 3d implementation of PhysicsEngine.
type [<ReferenceEquality>] BepuPhysicsEngine =
    private
        { PhysicsContext : Simulation
          PhysicsMessages : PhysicsMessage UList
          IntegrationMessages : IntegrationMessage List
          mutable RebuildingHack : bool }

    static member make () =
        let bufferPool = new BufferPool ()
        let targetThreadCount = max 1 (if Environment.ProcessorCount > 4 then Environment.ProcessorCount - 3 else Environment.ProcessorCount / 2)
        use threadDispatcher = new ThreadDispatcher (targetThreadCount)
        { PhysicsContext = Simulation.Create (bufferPool, NarrowPhaseCallbacks (), PoseIntegratorCallbacks (), SolveDescription (8, 1))
          PhysicsMessages = UList.makeEmpty Imperative
          IntegrationMessages = List ()
          RebuildingHack = false }

    (*static member private createIntegrationMessages physicsEngine =
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
            integrationMessages*)