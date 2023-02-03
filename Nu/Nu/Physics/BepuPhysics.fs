// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open BepuPhysics
open BepuPhysics.Collidables
open BepuPhysics.CollisionDetection
open BepuPhysics.Constraints
open BepuUtilities
open BepuUtilities.Memory
open Prime
open Nu
open System.Runtime.CompilerServices

//The simulation has a variety of extension points that must be defined. 
//The demos tend to reuse a few types like the DemoNarrowPhaseCallbacks, but this demo will provide its own (super simple) versions.
//If you're wondering why the callbacks are interface implementing structs rather than classes or events, it's because 
//the compiler can specialize the implementation using the compile time type information. That avoids dispatch overhead associated
//with delegates or virtual dispatch and allows inlining, which is valuable for extremely high frequency logic like contact callbacks.
type [<Struct; UnsafeValueType>] NarrowPhaseCallbacks =

    interface INarrowPhaseCallbacks with

        /// <summary>
        /// Performs any required initialization logic after the Simulation instance has been constructed.
        /// </summary>
        /// <param name="simulation">Simulation that owns these callbacks.</param>
        member this.Initialize (_ : Simulation) =
            //Often, the callbacks type is created before the simulation instance is fully constructed, so the simulation will call this function when it's ready.
            //Any logic which depends on the simulation existing can be put here.
            ()

        /// <summary>
        /// Chooses whether to allow contact generation to proceed for two overlapping collidables.
        /// </summary>
        /// <param name="workerIndex">Index of the worker that identified the overlap.</param>
        /// <param name="a">Reference to the first collidable in the pair.</param>
        /// <param name="b">Reference to the second collidable in the pair.</param>
        /// <param name="speculativeMargin">Reference to the speculative margin used by the pair.
        /// The value was already initialized by the narrowphase by examining the speculative margins of the involved collidables, but it can be modified.</param>
        /// <returns>True if collision detection should proceed, false otherwise.</returns>
        [<MethodImpl (MethodImplOptions.AggressiveInlining)>]
        member this.AllowContactGeneration (_ : int, a : CollidableReference, b : CollidableReference, _ : single byref) =
            //Before creating a narrow phase pair, the broad phase asks this callback whether to bother with a given pair of objects.
            //This can be used to implement arbitrary forms of collision filtering. See the RagdollDemo or NewtDemo for examples.
            //Here, we'll make sure at least one of the two bodies is dynamic.
            //The engine won't generate static-static pairs, but it will generate kinematic-kinematic pairs.
            //That's useful if you're trying to make some sort of sensor/trigger object, but since kinematic-kinematic pairs
            //can't generate constraints (both bodies have infinite inertia), simple simulations can just ignore such pairs.
            //
            //This function also exposes the speculative margin. It can be validly written to, but that is a very rare use case.
            //Most of the time, you can ignore this function's speculativeMargin parameter entirely.
            a.Mobility = CollidableMobility.Dynamic ||
            b.Mobility = CollidableMobility.Dynamic

        /// <summary>
        /// Chooses whether to allow contact generation to proceed for the children of two overlapping collidables in a compound-including pair.
        /// </summary>
        /// <param name="workerIndex">Index of the worker thread processing this pair.</param>
        /// <param name="pair">Parent pair of the two child collidables.</param>
        /// <param name="childIndexA">Index of the child of collidable A in the pair. If collidable A is not compound, then this is always 0.</param>
        /// <param name="childIndexB">Index of the child of collidable B in the pair. If collidable B is not compound, then this is always 0.</param>
        /// <returns>True if collision detection should proceed, false otherwise.</returns>
        /// <remarks>This is called for each sub-overlap in a collidable pair involving compound collidables. If neither collidable in a pair is compound, this will not be called.
        /// For compound-including pairs, if the earlier call to AllowContactGeneration returns false for owning pair, this will not be called. Note that it is possible
        /// for this function to be called twice for the same subpair if the pair has continuous collision detection enabled; 
        /// the CCD sweep test that runs before the contact generation test also asks before performing child pair tests.</remarks>
        [<MethodImpl (MethodImplOptions.AggressiveInlining)>]
        member this.AllowContactGeneration (_ : int, _ : CollidablePair, _ : int, _ : int) =
            //This is similar to the top level broad phase callback above. It's called by the narrow phase before generating subpairs between children in parent shapes. 
            //This only gets called in pairs that involve at least one shape type that can contain multiple children, like a Compound.
            true

        /// <summary>
        /// Provides a notification that a manifold has been created for a pair. Offers an opportunity to change the manifold's details. 
        /// </summary>
        /// <param name="workerIndex">Index of the worker thread that created this manifold.</param>
        /// <param name="pair">Pair of collidables that the manifold was detected between.</param>
        /// <param name="manifold">Set of contacts detected between the collidables.</param>
        /// <param name="pairMaterial">Material properties of the manifold.</param>
        /// <returns>True if a constraint should be created for the manifold, false otherwise.</returns>
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.ConfigureContactManifold<'TManifold when 'TManifold : (new : unit -> 'TManifold) and 'TManifold :> IContactManifold<'TManifold>>(_ : int, _ : CollidablePair, _ : 'TManifold byref, pairMaterial : PairMaterialProperties byref) =
            //The IContactManifold parameter includes functions for accessing contact data regardless of what the underlying type of the manifold is.
            //If you want to have direct access to the underlying type, you can use the manifold.Convex property and a cast like Unsafe.As<TManifold, ConvexContactManifold or NonconvexContactManifold>(ref manifold).
            //
            //The engine does not define any per-body material properties. Instead, all material lookup and blending operations are handled by the callbacks.
            //For the purposes of this demo, we'll use the same settings for all pairs.
            //(Note that there's no 'bounciness' or 'coefficient of restitution' property!
            //Bounciness is handled through the contact spring settings instead. Setting See here for more details: https://github.com/bepu/bepuphysics2/issues/3 and check out the BouncinessDemo for some options.)
            pairMaterial.FrictionCoefficient <- 1f
            pairMaterial.MaximumRecoveryVelocity <- 2f
            pairMaterial.SpringSettings <- SpringSettings (30.0f, 1.0f)
            //For the purposes of the demo, contact constraints are always generated.
            true

        /// <summary>
        /// Provides a notification that a manifold has been created between the children of two collidables in a compound-including pair.
        /// Offers an opportunity to change the manifold's details. 
        /// </summary>
        /// <param name="workerIndex">Index of the worker thread that created this manifold.</param>
        /// <param name="pair">Pair of collidables that the manifold was detected between.</param>
        /// <param name="childIndexA">Index of the child of collidable A in the pair. If collidable A is not compound, then this is always 0.</param>
        /// <param name="childIndexB">Index of the child of collidable B in the pair. If collidable B is not compound, then this is always 0.</param>
        /// <param name="manifold">Set of contacts detected between the collidables.</param>
        /// <returns>True if this manifold should be considered for constraint generation, false otherwise.</returns>
        [<MethodImpl (MethodImplOptions.AggressiveInlining)>]
        member this.ConfigureContactManifold (_ : int, _ : CollidablePair, _ : int, _ : int, _ : ConvexContactManifold byref) =
            true

        /// <summary>
        /// Releases any resources held by the callbacks. Called by the owning narrow phase when it is being disposed.
        /// </summary>
        member this.Dispose () = ()

//Note that the engine does not require any particular form of gravity- it, like all the contact callbacks, is managed by a callback.
type [<Struct>] PoseIntegratorCallbacks =

    val Gravity : Vector3

    //Note that velocity integration uses "wide" types. These are array-of-struct-of-arrays types that use SIMD accelerated types underneath.
    //Rather than handling a single body at a time, the callback handles up to Vector<float>.Count bodies simultaneously.
    val mutable GravityWideDt : Vector3Wide

    new (gravity) =
        { Gravity = gravity
          GravityWideDt = Unchecked.defaultof<_> }

    interface IPoseIntegratorCallbacks with

        /// <summary>
        /// Gets how the pose integrator should handle angular velocity integration.
        /// </summary>
        member this.AngularIntegrationMode = AngularIntegrationMode.Nonconserving

        /// <summary>
        /// Gets whether the integrator should use substepping for unconstrained bodies when using a substepping solver.
        /// If true, unconstrained bodies will be integrated with the same number of substeps as the constrained bodies in the solver.
        /// If false, unconstrained bodies use a single step of length equal to the dt provided to Simulation.Timestep. 
        /// </summary>
        member this.AllowSubstepsForUnconstrainedBodies = false

        /// <summary>
        /// Gets whether the velocity integration callback should be called for kinematic bodies.
        /// If true, IntegrateVelocity will be called for bundles including kinematic bodies.
        /// If false, kinematic bodies will just continue using whatever velocity they have set.
        /// Most use cases should set this to false.
        /// </summary>
        member this.IntegrateVelocityForKinematics = false

        /// <summary>
        /// Performs any required initialization logic after the Simulation instance has been constructed.
        /// </summary>
        /// <param name="simulation">Simulation that owns these callbacks.</param>
        member this.Initialize (_ : Simulation) =
            //In this demo, we don't need to initialize anything.
            //If you had a simulation with per body gravity stored in a CollidableProperty<T> or something similar, having the simulation provided in a callback can be helpful.
            ()

        /// <summary>
        /// Callback invoked ahead of dispatches that may call into <see cref="IntegrateVelocity"/>.
        /// It may be called more than once with different values over a frame. For example, when performing bounding box prediction, velocity is integrated with a full frame time step duration.
        /// During substepped solves, integration is split into substepCount steps, each with fullFrameDuration / substepCount duration.
        /// The final integration pass for unconstrained bodies may be either fullFrameDuration or fullFrameDuration / substepCount, depending on the value of AllowSubstepsForUnconstrainedBodies. 
        /// </summary>
        /// <param name="dt">Current integration time step duration.</param>
        /// <remarks>This is typically used for precomputing anything expensive that will be used across velocity integration.</remarks>
        member this.PrepareForIntegration (dt : single) =
            //No reason to recalculate gravity * dt for every body; just cache it ahead of time.
            this.GravityWideDt <- Vector3Wide.Broadcast (this.Gravity * dt)

        /// <summary>
        /// Callback for a bundle of bodies being integrated.
        /// </summary>
        /// <param name="bodyIndices">Indices of the bodies being integrated in this bundle.</param>
        /// <param name="position">Current body positions.</param>
        /// <param name="orientation">Current body orientations.</param>
        /// <param name="localInertia">Body's current local inertia.</param>
        /// <param name="integrationMask">Mask indicating which lanes are active in the bundle. Active lanes will contain 0xFFFFFFFF, inactive lanes will contain 0.</param>
        /// <param name="workerIndex">Index of the worker thread processing this bundle.</param>
        /// <param name="dt">Durations to integrate the velocity over. Can vary over lanes.</param>
        /// <param name="velocity">Velocity of bodies in the bundle. Any changes to lanes which are not active by the integrationMask will be discarded.</param>
        member this.IntegrateVelocity (_ : Vector<int>, _ : Vector3Wide, _ : QuaternionWide, _ : BodyInertiaWide, _ : Vector<int>, _ : int, _ : Vector<single>, velocity : BodyVelocityWide byref) =
            //This also is a handy spot to implement things like position dependent gravity or per-body damping.
            //We don't have to check for kinematics; IntegrateVelocityForKinematics returns false in this type, so we'll never see them in this callback.
            //Note that these are SIMD operations and "Wide" types. There are Vector<float>.Count lanes of execution being evaluated simultaneously.
            //The types are laid out in array-of-structures-of-arrays (AOSOA) format. That's because this function is frequently called from vectorized contexts within the solver.
            //Transforming to "array of structures" (AOS) format for the callback and then back to AOSOA would involve a lot of overhead, so instead the callback works on the AOSOA representation directly.
            velocity.Linear <- velocity.Linear + this.GravityWideDt

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
        let narrowPhaseCallbacks = NarrowPhaseCallbacks ()
        let poseIntegratorCallbacks = PoseIntegratorCallbacks Constants.Engine.GravityDefault
        let targetThreadCount = max 1 (if Environment.ProcessorCount > 4 then Environment.ProcessorCount - 3 else Environment.ProcessorCount / 2)
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