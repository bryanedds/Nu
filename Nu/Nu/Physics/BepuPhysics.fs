// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open BepuPhysics
open BepuUtilities
open BepuUtilities.Memory
open Prime
open Nu

/// The BepuPhysics 3d implementation of PhysicsEngine.
type [<ReferenceEquality>] BepuPhysicsEngine =
    private
        { PhysicsContext : Simulation
          PhysicsMessages : PhysicsMessage UList
          IntegrationMessages : IntegrationMessage List
          NarrowPhaseCallbacks : NarrowPhaseCallbacks
          PoseIntegratorCallbacks : PoseIntegratorCallbacks
          ThreadDispatcther : ThreadDispatcher
          mutable RebuildingHack : bool }

    static member make () =
        let bufferPool = new BufferPool ()
        let narrowPhaseCallbacks = NarrowPhaseCallbacks (dictPlus HashIdentity.Structural [])
        let poseIntegratorCallbacks = PoseIntegratorCallbacks Constants.Engine.GravityDefault
        let targetThreadCount = max 1 (if Environment.ProcessorCount > 4 then Environment.ProcessorCount - 3 else Environment.ProcessorCount / 2) // reduced from example since the renderer is probably taking up a whole additional core to itself
        use threadDispatcher = new ThreadDispatcher (targetThreadCount)
        { PhysicsContext = Simulation.Create (bufferPool, narrowPhaseCallbacks, poseIntegratorCallbacks, SolveDescription (8, 1))
          PhysicsMessages = UList.makeEmpty Imperative
          IntegrationMessages = List ()
          NarrowPhaseCallbacks = narrowPhaseCallbacks
          PoseIntegratorCallbacks = poseIntegratorCallbacks
          ThreadDispatcther = threadDispatcher
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