// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Buffers
open System.Collections.Concurrent
open System.Collections.Generic
open System.Numerics
open System.Threading.Tasks
open Box2D.NET
open Prime

/// Represents a neighbor particle during fluid simulation.
type [<Struct>] private Box2dNetFluidParticleNeighbor =
    { mutable ParticleIndex : int
      mutable AccumulatedImpulse : B2Vec2 } // Thread-safe accumulation, unlike in sfml-box2d-fluid

/// Represents the state of a fluid particle during simulation.
type private Box2dNetFluidParticleState = // see sfml-box2d-fluid: GameObjects.h, struct Particle
    { // persistent state
      mutable Shape : B2ShapeId
      mutable Body : B2BodyId
      // computeAccumulatedImpulses internal data
      Neighbors : Box2dNetFluidParticleNeighbor List
      mutable CachedPosition : B2Vec2
      mutable CachedVelocity : B2Vec2
      mutable CachedMass : single
      mutable CachedRadius : single
      mutable CachedCellId : Vector2i
      mutable CachedConfig : FluidParticleConfig
      // computeAccumulatedImpulses output
      mutable AccumulatedImpulse : B2Vec2 }

/// Represents a Box2D.NET fluid emitter.
///
/// Follows sfml-box2d-fluid implementation: https://github.com/a-piece-of-snake/sfml-box2d-fluid/tree/699c5c874969c9c270300bf33e60c0b2ecf77c72
///
/// NOTE: there is too much MAGIC (see comments) going on, this implementation will hopefully be superceded in a future Box2D update.
type private Box2dNetFluidEmitter =
    { FluidEmitterDescriptor : FluidEmitterDescriptorBox2dNet
      PhysicsContextId : B2WorldId
      GravityOverrides : Dictionary<B2BodyId, B2Vec2>
      States : Box2dNetFluidParticleState array
      mutable StateCount : int
      Grid : Dictionary<Vector2i, int List>
      RemovedIndexes : int List // assumed to be ordered from smallest to largest
      BodySource : Simulant
      mutable NextBodyIndex : int }
      
    static let assertTrue v = assert v // don't erase the bool computation on release builds
    static let toPhysics v = v / Constants.Engine.Meter2d
    static let toPixel v = v * Constants.Engine.Meter2d
    static let toPhysicsV2 (v : Vector3) = B2Vec2 (toPhysics v.X, toPhysics v.Y)
    static let toPixelV3 (v : B2Vec2) = Vector3 (v.X * Constants.Engine.Meter2d, v.Y * Constants.Engine.Meter2d, 0.0f)
    static let toPhysicsB2Vec2 (v : B2Vec2) = B2Vec2 (toPhysics v.X, toPhysics v.Y)
    static let toPixelB2Vec2 (v : B2Vec2) = B2Vec2 (toPixel v.X, toPixel v.Y)

    static let getConfig configName (fluidEmitter : Box2dNetFluidEmitter) =
        match fluidEmitter.FluidEmitterDescriptor.Configs.TryGetValue configName with
        | (true, config) -> config
        | (false, _) ->
            Log.warnOnce $"Fluid particle config '{configName}' not found in Box2dNetFluidEmitter instance, assuming water properties as default."
            FluidParticleConfig.waterProperties

    static let createBodyForParticle (bodyShapeIndex : BodyShapeIndex) position velocity configName (fluidEmitter : Box2dNetFluidEmitter) =
        // see sfml-box2d-fluid: GameObjects.cpp, ParticleGroup::CreateParticles
        let config = getConfig configName fluidEmitter
        // create body
        let mutable bodyDef = B2Types.b2DefaultBodyDef ()
        bodyDef.position <- position
        bodyDef.linearVelocity <- velocity
        let gravityOverride =
            match config.Gravity with
            | GravityWorld -> bodyDef.gravityScale <- 1.0f; ValueNone
            | GravityIgnore -> bodyDef.gravityScale <- 0.0f; ValueNone
            | GravityScale scale -> bodyDef.gravityScale <- scale; ValueNone
            | GravityOverride gravity -> bodyDef.gravityScale <- 0.0f; ValueSome gravity
        bodyDef.``type`` <- B2BodyType.b2_dynamicBody
        bodyDef.linearDamping <- 0.01f
        bodyDef.angularDamping <- 0.01f
        bodyDef.fixedRotation <- true
        bodyDef.isBullet <- false
        bodyDef.name <- configName
        // don't assign body userData - we filter move events by non-existence of it
        let body = B2Bodies.b2CreateBody (fluidEmitter.PhysicsContextId, &bodyDef)
        match gravityOverride with
        | ValueSome gravity -> fluidEmitter.GravityOverrides.[body] <- toPhysicsV2 gravity
        | ValueNone -> ()

        // create circle shape
        let mutable shapeDef = B2Types.b2DefaultShapeDef ()
        shapeDef.density <- config.Density
        shapeDef.material.friction <- config.Friction
        shapeDef.material.restitution <- config.Restitution
        shapeDef.filter <- B2Filter (config.CollisionCategories, config.CollisionMask, -B2Constants.B2_SECRET_COOKIE) // random negative group to not use rigid collisions
        shapeDef.userData <- bodyShapeIndex // required to identify shape in collision events
        shapeDef.enablePreSolveEvents <- Constants.Physics.Collision2dFrameCompensation
        shapeDef.enableContactEvents <- true
        shapeDef.enableSensorEvents <- true
        let mutable circle = B2Circle (B2MathFunction.b2Vec2_zero, toPhysics config.Radius)
        struct (body, B2Shapes.b2CreateCircleShape (body, &shapeDef, &circle))

    static member positionToCellId cellSize (position : B2Vec2) =
        v2i (floor (position.X / cellSize) |> int) (floor (position.Y / cellSize) |> int)

    static member cellIdToBox cellSize (cellId : Vector2i) =
        box2 (cellId.V2 * cellSize) (v2Dup cellSize)

    static let updateConfig state config (fluidEmitter : Box2dNetFluidEmitter) =
        let bodyShapeIndex = B2Shapes.b2Shape_GetUserData state.Shape :?> BodyShapeIndex
        let position = B2Bodies.b2Body_GetPosition state.Body
        let velocity = B2Bodies.b2Body_GetLinearVelocity state.Body
        B2Bodies.b2DestroyBody state.Body
        fluidEmitter.GravityOverrides.Remove state.Body |> ignore<bool>
        let struct (body, shape) = createBodyForParticle bodyShapeIndex position velocity config fluidEmitter
        state.Body <- body
        state.Shape <- shape

    static let toFluid i (particle : FluidParticle) fluidEmitter =
        let state = fluidEmitter.States.[i]
        if B2Bodies.b2Body_GetName state.Body = particle.FluidParticleConfig then
            B2Bodies.b2Body_SetTransform (state.Body, toPhysicsV2 particle.FluidParticlePosition, B2MathFunction.b2Rot_identity)
            B2Bodies.b2Body_SetLinearVelocity (state.Body, toPhysicsV2 particle.FluidParticleVelocity)
        else
            updateConfig state particle.FluidParticleConfig fluidEmitter
            
    static let fromFluid (state : Box2dNetFluidParticleState) =
        { FluidParticlePosition = toPixelV3 (B2Bodies.b2Body_GetPosition state.Body)
          FluidParticleVelocity = toPixelV3 (B2Bodies.b2Body_GetLinearVelocity state.Body)
          FluidParticleConfig = B2Bodies.b2Body_GetName state.Body }

    static member updateDescriptor (descriptor : FluidEmitterDescriptorBox2dNet) (fluidEmitter : Box2dNetFluidEmitter) =
        if not descriptor.Enabled then
            Box2dNetFluidEmitter.clearParticles fluidEmitter
            { fluidEmitter with FluidEmitterDescriptor = descriptor } // clear all particles if disabled
        elif fluidEmitter.FluidEmitterDescriptor.ParticlesMax <> descriptor.ParticlesMax then // update state array size
            let newEmitter = Box2dNetFluidEmitter.make descriptor fluidEmitter.PhysicsContextId fluidEmitter.BodySource
            Box2dNetFluidEmitter.addParticles (Seq.init fluidEmitter.StateCount (fun i -> fromFluid fluidEmitter.States.[i]) |> _.GetEnumerator()) newEmitter
            newEmitter
        elif fluidEmitter.FluidEmitterDescriptor.Configs <> descriptor.Configs then
            let newEmitter = { fluidEmitter with FluidEmitterDescriptor = descriptor }
            for i in 0 .. dec fluidEmitter.StateCount do
                let state = newEmitter.States.[i]
                updateConfig state (B2Bodies.b2Body_GetName state.Body) newEmitter
            newEmitter
        else { fluidEmitter with FluidEmitterDescriptor = descriptor }

    static member addParticles (particlesEnr : FluidParticle IEnumerator) (fluidEmitter : Box2dNetFluidEmitter) =
        let descriptor = fluidEmitter.FluidEmitterDescriptor
        if descriptor.Enabled then
            while particlesEnr.MoveNext () && fluidEmitter.StateCount < descriptor.ParticlesMax do
                let particle = particlesEnr.Current
                let bodyShapeIndex =
                    { BodyId = { BodySource = fluidEmitter.BodySource; BodyIndex = fluidEmitter.NextBodyIndex }
                      BodyShapeIndex = Constants.Physics.InternalIndex }
                fluidEmitter.NextBodyIndex <- inc fluidEmitter.NextBodyIndex
                let position = toPhysicsV2 particle.FluidParticlePosition
                let velocity = toPhysicsV2 particle.FluidParticleVelocity
                let struct (body, shape) = createBodyForParticle bodyShapeIndex position velocity particle.FluidParticleConfig fluidEmitter

                // fin
                fluidEmitter.States.[fluidEmitter.StateCount] <-
                    { Shape = shape
                      Body = body
                      Neighbors = List ()
                      CachedPosition = B2Vec2 ()
                      CachedVelocity = B2Vec2 ()
                      CachedMass = 0.0f
                      CachedRadius = 0.0f
                      CachedCellId = Vector2i ()
                      CachedConfig = Unchecked.defaultof<FluidParticleConfig>
                      AccumulatedImpulse = B2Vec2 () }
                fluidEmitter.StateCount <- inc fluidEmitter.StateCount

    static member setParticles (particles : FluidParticle SArray) (fluidEmitter : Box2dNetFluidEmitter) =
        Box2dNetFluidEmitter.clearParticles fluidEmitter
        Box2dNetFluidEmitter.addParticles (particles.GetEnumerator ()) fluidEmitter

    static member private processRemovedIndexes (fluidEmitter : Box2dNetFluidEmitter) =
        for j in dec fluidEmitter.RemovedIndexes.Count .. -1 .. 0 do // process in reverse order to avoid removing last item after it already moved into earlier hole
            let i = fluidEmitter.RemovedIndexes.[j]
            let state = &fluidEmitter.States.[i]
            B2Bodies.b2DestroyBody state.Body
            fluidEmitter.GravityOverrides.Remove state.Body |> ignore<bool>
            fluidEmitter.StateCount <- dec fluidEmitter.StateCount
            state <- fluidEmitter.States.[fluidEmitter.StateCount]
            fluidEmitter.States.[fluidEmitter.StateCount] <- Unchecked.defaultof<_>
        fluidEmitter.RemovedIndexes.Clear ()

    static member collectParticles (discriminator : FluidParticle -> FluidParticle seq) (fluidEmitter : Box2dNetFluidEmitter) =
        for i in 0 .. dec fluidEmitter.StateCount do
            let state = fluidEmitter.States.[i]
            let newParticlesEnr = discriminator(fromFluid state).GetEnumerator ()
            if newParticlesEnr.MoveNext () then
                toFluid i newParticlesEnr.Current fluidEmitter
                Box2dNetFluidEmitter.addParticles newParticlesEnr fluidEmitter
            else
                fluidEmitter.RemovedIndexes.Add i
        Box2dNetFluidEmitter.processRemovedIndexes fluidEmitter

    static member clearParticles (fluidEmitter : Box2dNetFluidEmitter) =
        for i in 0 .. dec fluidEmitter.StateCount do
            B2Bodies.b2DestroyBody fluidEmitter.States.[i].Body
            fluidEmitter.GravityOverrides.Remove fluidEmitter.States.[i].Body |> ignore<bool>
        fluidEmitter.StateCount <- 0

    static member getForce dst radius = // see sfml-box2d-fluid: GameObjects.cpp, ParticleGroup::GetForce (without caching)
        if dst >= radius then 0.0f else
        let x = radius - dst
        let x_pow2_5 = x * x * MathF.Sqrt x
        let radius_sq = radius * radius
        x_pow2_5 / (MathF.PI_OVER_4 * radius_sq * radius_sq) // MAGIC: this formula is pure magic (@.@)
        
    static let Neighborhood = [|for x in -1 .. 1 do for y in -1 .. 1 do v2i x y|]
    static member computeAccumulatedImpulses idx timestep fluidEmitter = // see sfml-box2d-fluid: GameObjects.cpp, ParticleGroup::ComputeChunkForces
        let state = fluidEmitter.States.[idx]
        if B2Bodies.b2Body_IsAwake state.Body then
            let config = state.CachedConfig
            let radiusA = state.CachedRadius
            let range = radiusA * config.Impact
            let mutable density = 0.0f
            let posA = state.CachedPosition
            let mutable neighborSurfaceForce = B2MathFunction.b2Vec2_zero
            let velA = state.CachedVelocity
            
            // gather neighbors and accumulate density
            for neighborCell in Neighborhood do
                match fluidEmitter.Grid.TryGetValue (state.CachedCellId + neighborCell) with
                | (true, list) ->
                    for otherIdx in list do
                        if idx <> otherIdx then
                            let otherState = fluidEmitter.States.[otherIdx]
                            let dist = B2MathFunction.b2Length (otherState.CachedPosition - posA)
                            if dist < range && dist > 0.001f then
                                let force = Box2dNetFluidEmitter.getForce dist range
                                density <- density + ((state.CachedMass + otherState.CachedMass) * 0.5f) * min config.MaxForce force // MAGIC: density depends on force? (@.@)
                                state.Neighbors.Add { ParticleIndex = otherIdx; AccumulatedImpulse = B2Vec2 () }
                | (false, _) -> ()
            let neighbors = System.Runtime.InteropServices.CollectionsMarshal.AsSpan state.Neighbors
            for neighborIdx in 0 .. dec neighbors.Length do
                let other = &neighbors.[neighborIdx]
                let otherState = fluidEmitter.States.[other.ParticleIndex]
                let posB = otherState.CachedPosition
                let offset = posB - posA
                let radiusB = otherState.CachedRadius
                
                // if in range
                let offsetLength = B2MathFunction.b2Length offset
                if offsetLength < 0.001f then () else
                let dst = offsetLength / radiusA * config.Impact // MAGIC: dst scales inversely with radiusA but effectiveRange scales linearly with radiusA? (@.@)
                let effectiveRange = (radiusA + radiusB) * config.Impact
                if dst < effectiveRange then
                    let Density r = r * r * MathF.Sqrt r

                    // repulsion force
                    let forceDir = B2MathFunction.b2Normalize offset
                    let densityScale = B2MathFunction.b2ClampFloat (density, config.MinDensity, config.MaxDensity)
                    let distanceForceMag = min (Box2dNetFluidEmitter.getForce dst effectiveRange) config.MaxForce * Density densityScale
                    let repulsionForce = config.ForceMultiplier * forceDir * distanceForceMag * effectiveRange

                    // surface force
                    if (config.SurfaceWithOther && otherState.CachedConfig.SurfaceWithOther) || B2Bodies.b2Body_GetName state.Body = B2Bodies.b2Body_GetName otherState.Body then
                        let avgSurfaceForce = (otherState.CachedConfig.ForceSurface + config.ForceSurface) * 0.5f
                        let surfaceForce = (posB - posA) * avgSurfaceForce
                        neighborSurfaceForce <- neighborSurfaceForce + surfaceForce

                    // viscosity force
                    let velB = otherState.CachedVelocity
                    let rNorm = dst / effectiveRange
                    let ViscosityKernelSimple r = -0.5f * r * r * r + r * r - 1.0f
                    let kVisc = ViscosityKernelSimple rNorm
                    let mutable velDiff = velB - velA
                    let dot = B2MathFunction.b2Dot (velDiff, offset)
                    let leaveMul = if dot > 0.0f then config.ViscosityLeave else 1.0f
                    let viscForce = config.Viscosity * leaveMul * kVisc * velDiff

                    // friction force
                    let dir = B2MathFunction.b2Normalize offset
                    let tangentialSpeed = B2MathFunction.b2Dot (velB - velA, dir)
                    let attenuation = 1.0f - rNorm
                    let frictionForce = (-config.ShearViscosity * tangentialSpeed * dir) * attenuation

                    // damping force
                    let repulsionMagnitude = B2MathFunction.b2Length repulsionForce
                    let dampingForce = state.CachedVelocity * repulsionMagnitude * config.ForceDamping

                    // apply forces
                    let mutable totalForce = (repulsionForce + frictionForce + viscForce + dampingForce) * timestep
                    assert (Single.IsFinite totalForce.X && Single.IsFinite totalForce.Y)
                    totalForce <- B2MathFunction.b2Clamp (totalForce, B2Vec2 (-config.MaxGetForce, -config.MaxGetForce), B2Vec2 (config.MaxGetForce, config.MaxGetForce))
                    other.AccumulatedImpulse <- other.AccumulatedImpulse + totalForce
                    state.AccumulatedImpulse <- state.AccumulatedImpulse - totalForce

            // surface force
            if neighbors.Length > 0 then
                state.AccumulatedImpulse <- state.AccumulatedImpulse + neighborSurfaceForce * (timestep / single neighbors.Length)
            // adhesion force awaiting upstream implementation...
        else state.AccumulatedImpulse <- B2MathFunction.b2Vec2_zero
        
    static let CellCapacityDefault = 16
    static member preStep (clockDelta : single) (fluidEmitter : Box2dNetFluidEmitter) = // see sfml-box2d-fluid: GameObjects.cpp, ParticleGroup::ComputeChunkForces

        // OPTIMIZATION: early return when no particles (also applies to not enabled)
        if fluidEmitter.StateCount = 0 then () else

        for cell in fluidEmitter.Grid.Values do cell.Clear ()
        let cellSize = fluidEmitter.FluidEmitterDescriptor.CellSize
        for i in 0 .. dec fluidEmitter.StateCount do // see sfml-box2d-fluid: GameObjects.cpp, ParticleGroup::UpdateData
            let state = fluidEmitter.States.[i]
            state.Neighbors.Clear ()
            state.CachedPosition <- toPixelB2Vec2 (B2Bodies.b2Body_GetPosition state.Body) // MAGIC: somehow internal calculations use pixel coordinates instead of physics coordinates? (@.@)
            state.CachedVelocity <- toPixelB2Vec2 (B2Bodies.b2Body_GetLinearVelocity state.Body)
            state.CachedMass <- toPixel (B2Bodies.b2Body_GetMass state.Body) |> toPixel // NOTE: mass scales with area (length^2) so we have to scale it again
            state.CachedRadius <- toPixel (B2Shapes.b2Shape_GetCircle state.Shape).radius
            state.CachedCellId <- Box2dNetFluidEmitter.positionToCellId cellSize state.CachedPosition
            state.CachedConfig <- getConfig (B2Bodies.b2Body_GetName state.Body) fluidEmitter
            state.AccumulatedImpulse <- B2Vec2 ()
            match fluidEmitter.Grid.TryGetValue state.CachedCellId with
            | (true, cell) -> cell.Add i
            | (false, _) ->
                let cell = List CellCapacityDefault
                cell.Add i
                fluidEmitter.Grid.[state.CachedCellId] <- cell
        for KeyValue (cellId, cell) in fluidEmitter.Grid do if cell.Count = 0 then fluidEmitter.Grid.Remove cellId |> assertTrue

        // compute impulses in parallel
        // see sfml-box2d-fluid: GameObjects.cpp, ParticleGroup::ComputeParticleForces
        let result = Parallel.For (0, fluidEmitter.StateCount, fun i _ -> Box2dNetFluidEmitter.computeAccumulatedImpulses i clockDelta fluidEmitter)
        assert result.IsCompleted

        // accumulate and apply impulses
        for i in 0 .. dec fluidEmitter.StateCount do
            for neighbor in fluidEmitter.States.[i].Neighbors do
                let otherAccumulatedImpulse = &fluidEmitter.States.[neighbor.ParticleIndex].AccumulatedImpulse
                otherAccumulatedImpulse <- otherAccumulatedImpulse + neighbor.AccumulatedImpulse
        for i in 0 .. dec fluidEmitter.StateCount do
            let state = fluidEmitter.States.[i]
            B2Bodies.b2Body_ApplyLinearImpulseToCenter (state.Body, toPhysicsB2Vec2 (toPhysicsB2Vec2 state.AccumulatedImpulse), true) // NOTE: impulse scales with mass which scales with area (length^2)
            state.CachedConfig <- Unchecked.defaultof<FluidParticleConfig>

        // apply gravity overrides
        for KeyValue (body, gravityOverride) in fluidEmitter.GravityOverrides do
            if B2Bodies.b2Body_IsAwake body then
                B2Bodies.b2Body_SetLinearVelocity (body, B2Bodies.b2Body_GetLinearVelocity body + gravityOverride * clockDelta)

    static member postStep fluidEmitter =
        let bounds = fluidEmitter.FluidEmitterDescriptor.SimulationBounds

        // process and collect out of bounds particles
        let aabb = B2AABB (B2Vec2 (bounds.Min.X / Constants.Engine.Meter2d, bounds.Min.Y / Constants.Engine.Meter2d),
                           B2Vec2 (bounds.Max.X / Constants.Engine.Meter2d, bounds.Max.Y / Constants.Engine.Meter2d))
        for i in 0 .. dec fluidEmitter.StateCount do
            let state = fluidEmitter.States.[i]
            let pos = B2Bodies.b2Body_GetPosition state.Body
            if aabb.lowerBound.X > pos.X || aabb.upperBound.X < pos.X || aabb.lowerBound.Y > pos.Y || aabb.upperBound.Y < pos.Y then
                fluidEmitter.RemovedIndexes.Add i
        let removedParticles =
            if fluidEmitter.RemovedIndexes.Count = 0
            then SArray.empty
            else SArray.zeroCreate fluidEmitter.RemovedIndexes.Count
        for j in 0 .. dec removedParticles.Length do
            let i = fluidEmitter.RemovedIndexes.[j]
            removedParticles.[j] <- fromFluid fluidEmitter.States.[i]
        Box2dNetFluidEmitter.processRemovedIndexes fluidEmitter

        // collect current particles
        let particles = SArray.zeroCreate fluidEmitter.StateCount
        for i in 0 .. dec fluidEmitter.StateCount do
            particles.[i] <- fromFluid fluidEmitter.States.[i]
        struct (particles, removedParticles)

    static member make descriptor physicsContextId bodySource =
        { FluidEmitterDescriptor = descriptor
          PhysicsContextId = physicsContextId
          States = Array.zeroCreate descriptor.ParticlesMax
          StateCount = 0
          Grid = Dictionary HashIdentity.Structural
          RemovedIndexes = List ()
          BodySource = bodySource
          NextBodyIndex = 0
          GravityOverrides = Dictionary HashIdentity.Structural }

type private Box2dNetPhysicsEngineContactsTracker =
    { NewContacts : ConcurrentDictionary<struct (B2ShapeId * B2ShapeId), BodyPenetrationMessage>
      ExistingContacts : HashSet<struct (B2ShapeId * B2ShapeId)> }

/// The Box2D.NET interface of PhysicsEngineRenderContext.
type Box2dNetPhysicsEngineRenderContext =
    inherit PhysicsEngineRenderContext
    abstract EyeBounds : Box2
    abstract DrawLine : start : Vector2 * stop : Vector2 * color : Color -> unit
    abstract DrawCircle : position : Vector2 * radius : single * color : Color -> unit

type private Box2dNetCharacterSimulation =
    { // input parameters
      PogoRestLength : single
      PogoHertz : single
      PogoDampingRatio : single
      PogoProxy : B2ShapeProxy
      CollisionCategory : uint64
      CastMask : uint64
      CollisionMask : uint64
      Capsule : B2Capsule // point1 must be at the bottom for pogo casting!
      CapsuleShape : B2ShapeId
      Mass : single
      GravityScale : single
      GravityOverride : B2Vec2
      // input-output parameters
      mutable Transform : B2Transform
      mutable Velocity : B2Vec2
      mutable OnGround : B2ShapeId voption
      mutable PogoVelocity : single
      // output parameters
      mutable PogoOrigin : B2Vec2
      mutable PogoDelta : B2Vec2
      GroundCastResult : B2RayResult }
type private Box2dNetCharacterCollisionContext =
    { mutable Self : B2ShapeId
      SoftCollisionPushLimits : Dictionary<B2BodyId, single> // TODO: Replace with body shape def assignments once Box2D releases v3.2.
      PlaneResults : B2CollisionPlane List } // NOTE: Fixed capacity as 8.

/// The Box2D.NET implementation of PhysicsEngine.
type [<ReferenceEquality>] Box2dNetPhysicsEngine =
    private
        { mutable PhysicsContextId : B2WorldId
          Bodies : Dictionary<BodyId, B2BodyId>
          BodyGravityOverrides : Dictionary<BodyId, Vector3>
          Characters : Dictionary<BodyId, Box2dNetCharacterSimulation>
          Joints : Dictionary<BodyJointId, B2JointId>
          BreakableJoints : Dictionary<BodyJointId, struct {| BreakingPoint : single; BreakingPointSquared : single |}>
          CreateBodyJointMessages : Dictionary<BodyId, CreateBodyJointMessage List>
          FluidEmitters : Dictionary<FluidEmitterId, Box2dNetFluidEmitter>
          CharacterCollisionContext : Box2dNetCharacterCollisionContext // cached
          IntegrationMessages : IntegrationMessage List // OPTIMIZATION: cached to avoid large arrays filling up the LOH.
          ContactsTracker : Box2dNetPhysicsEngineContactsTracker } // NOTE: supports thread safety for b2PreSolveFcn.

    static member private toPixel value =
        value * Constants.Engine.Meter2d // TODO: try using b2SetLengthUnitsPerMeter to avoid all these conversions?

    static member private toPhysics value =
        value / Constants.Engine.Meter2d

    static member private toPixelV2 (v2 : B2Vec2) =
        Vector2 (Box2dNetPhysicsEngine.toPixel v2.X, Box2dNetPhysicsEngine.toPixel v2.Y)

    static member private toPixelV3 (v2 : B2Vec2) =
        (Box2dNetPhysicsEngine.toPixelV2 v2).V3

    static member private toPhysicsV2 (v3 : Vector3) =
        B2Vec2 (Box2dNetPhysicsEngine.toPhysics v3.X, Box2dNetPhysicsEngine.toPhysics v3.Y)

    static member private toPhysicsPolygonDiameter value =
        let value = Box2dNetPhysicsEngine.toPhysics value
        max (B2Constants.B2_LINEAR_SLOP * 2f) value

    static member private toPhysicsPolygonRadius value =
        let value = Box2dNetPhysicsEngine.toPhysics value
        max B2Constants.B2_LINEAR_SLOP value

    static member private quatToRot (q : Quaternion) =
        // NOTE: for a 2D rotation around Z-axis,
        // 1) The quaternion should be: w = cos(θ/2), x = 0, y = 0, z = sin(θ/2)
        // 2) The complex rotation should be: c = cos(θ), s = sin(θ)
        // Using double-angle formulas:
        B2Rot (1.0f - 2.0f * (q.Y * q.Y + q.Z * q.Z), 2.0f * (q.W * q.Z + q.X * q.Y))

    static member private rotToQuat (rot : B2Rot) =
        // NOTE: using half-angle formulas.
        let halfAngle = atan2 rot.s rot.c * 0.5f
        let struct (sin, cos) = MathF.SinCos halfAngle
        Quaternion (0.0f, 0.0f, sin, cos)

    // NOTE: since sensor events don't report collision normals, we have to compute them ourselves.
    static member private computeCollisionNormalForSensors shapeA shapeB =
        let transformA = B2Shapes.b2Shape_GetBody shapeA |> B2Bodies.b2Body_GetTransform
        let transformB = B2Shapes.b2Shape_GetBody shapeB |> B2Bodies.b2Body_GetTransform
        match (B2Shapes.b2Shape_GetType shapeA, B2Shapes.b2Shape_GetType shapeB) with
        | (B2ShapeType.b2_circleShape, B2ShapeType.b2_circleShape) ->
            let mutable circleA = B2Shapes.b2Shape_GetCircle shapeA
            let mutable circleB = B2Shapes.b2Shape_GetCircle shapeB
            B2Manifolds.b2CollideCircles(&circleA, transformA, &circleB, transformB).normal
        | (B2ShapeType.b2_circleShape, B2ShapeType.b2_capsuleShape) ->
            let mutable circle = B2Shapes.b2Shape_GetCircle shapeA
            let mutable capsule = B2Shapes.b2Shape_GetCapsule shapeB
            -B2Manifolds.b2CollideCapsuleAndCircle(&capsule, transformB, &circle, transformA).normal
        | (B2ShapeType.b2_circleShape, B2ShapeType.b2_segmentShape) ->
            let mutable circle = B2Shapes.b2Shape_GetCircle shapeA
            let mutable segment = B2Shapes.b2Shape_GetSegment shapeB
            -B2Manifolds.b2CollideSegmentAndCircle(&segment, transformB, &circle, transformA).normal
        | (B2ShapeType.b2_circleShape, B2ShapeType.b2_polygonShape) ->
            let mutable circle = B2Shapes.b2Shape_GetCircle shapeA
            let mutable polygon = B2Shapes.b2Shape_GetPolygon shapeB
            -B2Manifolds.b2CollidePolygonAndCircle(&polygon, transformB, &circle, transformA).normal
        | (B2ShapeType.b2_circleShape, B2ShapeType.b2_chainSegmentShape) ->
            let mutable circle = B2Shapes.b2Shape_GetCircle shapeA
            let mutable chainSegment = B2Shapes.b2Shape_GetChainSegment shapeB
            -B2Manifolds.b2CollideChainSegmentAndCircle(&chainSegment, transformB, &circle, transformA).normal
        | (B2ShapeType.b2_capsuleShape, B2ShapeType.b2_circleShape) ->
            let mutable capsule = B2Shapes.b2Shape_GetCapsule shapeA
            let mutable circle = B2Shapes.b2Shape_GetCircle shapeB
            B2Manifolds.b2CollideCapsuleAndCircle(&capsule, transformA, &circle, transformB).normal
        | (B2ShapeType.b2_capsuleShape, B2ShapeType.b2_capsuleShape) ->
            let mutable capsuleA = B2Shapes.b2Shape_GetCapsule shapeA
            let mutable capsuleB = B2Shapes.b2Shape_GetCapsule shapeB
            B2Manifolds.b2CollideCapsules(&capsuleA, transformA, &capsuleB, transformB).normal
        | (B2ShapeType.b2_capsuleShape, B2ShapeType.b2_segmentShape) ->
            let mutable capsule = B2Shapes.b2Shape_GetCapsule shapeA
            let mutable segment = B2Shapes.b2Shape_GetSegment shapeB
            -B2Manifolds.b2CollideSegmentAndCapsule(&segment, transformB, &capsule, transformA).normal
        | (B2ShapeType.b2_capsuleShape, B2ShapeType.b2_polygonShape) ->
            let mutable capsule = B2Shapes.b2Shape_GetCapsule shapeA
            let mutable polygon = B2Shapes.b2Shape_GetPolygon shapeB
            -B2Manifolds.b2CollidePolygonAndCapsule(&polygon, transformB, &capsule, transformA).normal
        | (B2ShapeType.b2_capsuleShape, B2ShapeType.b2_chainSegmentShape) ->
            let mutable capsule = B2Shapes.b2Shape_GetCapsule shapeA
            let mutable chainSegment = B2Shapes.b2Shape_GetChainSegment shapeB
            let mutable cache = B2Collisions.b2_emptySimplexCache
            -B2Manifolds.b2CollideChainSegmentAndCapsule(&chainSegment, transformB, &capsule, transformA, &cache).normal
        | (B2ShapeType.b2_segmentShape, B2ShapeType.b2_circleShape) ->
            let mutable segment = B2Shapes.b2Shape_GetSegment shapeA
            let mutable circle = B2Shapes.b2Shape_GetCircle shapeB
            B2Manifolds.b2CollideSegmentAndCircle(&segment, transformA, &circle, transformB).normal
        | (B2ShapeType.b2_segmentShape, B2ShapeType.b2_capsuleShape) ->
            let mutable segment = B2Shapes.b2Shape_GetSegment shapeA
            let mutable capsule = B2Shapes.b2Shape_GetCapsule shapeB
            B2Manifolds.b2CollideSegmentAndCapsule(&segment, transformA, &capsule, transformB).normal
        | (B2ShapeType.b2_segmentShape, B2ShapeType.b2_segmentShape) ->
            failwith "Unexpected segment to segment collision" // only shapes with volume can collide
        | (B2ShapeType.b2_segmentShape, B2ShapeType.b2_polygonShape) ->
            let mutable segment = B2Shapes.b2Shape_GetSegment shapeA
            let mutable polygon = B2Shapes.b2Shape_GetPolygon shapeB
            B2Manifolds.b2CollideSegmentAndPolygon(&segment, transformA, &polygon, transformB).normal
        | (B2ShapeType.b2_segmentShape, B2ShapeType.b2_chainSegmentShape) ->
            failwith "Unexpected segment to chain segment collision" // only shapes with volume can collide
        | (B2ShapeType.b2_polygonShape, B2ShapeType.b2_circleShape) ->
            let mutable polygon = B2Shapes.b2Shape_GetPolygon shapeA
            let mutable circle = B2Shapes.b2Shape_GetCircle shapeB
            B2Manifolds.b2CollidePolygonAndCircle(&polygon, transformA, &circle, transformB).normal
        | (B2ShapeType.b2_polygonShape, B2ShapeType.b2_capsuleShape) ->
            let mutable polygon = B2Shapes.b2Shape_GetPolygon shapeA
            let mutable capsule = B2Shapes.b2Shape_GetCapsule shapeB
            B2Manifolds.b2CollidePolygonAndCapsule(&polygon, transformA, &capsule, transformB).normal
        | (B2ShapeType.b2_polygonShape, B2ShapeType.b2_segmentShape) ->
            let mutable polygon = B2Shapes.b2Shape_GetPolygon shapeA
            let mutable segment = B2Shapes.b2Shape_GetSegment shapeB
            -B2Manifolds.b2CollideSegmentAndPolygon(&segment, transformB, &polygon, transformA).normal
        | (B2ShapeType.b2_polygonShape, B2ShapeType.b2_polygonShape) ->
            let mutable polygonA = B2Shapes.b2Shape_GetPolygon shapeA
            let mutable polygonB = B2Shapes.b2Shape_GetPolygon shapeB
            B2Manifolds.b2CollidePolygons(&polygonA, transformA, &polygonB, transformB).normal
        | (B2ShapeType.b2_polygonShape, B2ShapeType.b2_chainSegmentShape) ->
            let mutable polygon = B2Shapes.b2Shape_GetPolygon shapeA
            let mutable chainSegment = B2Shapes.b2Shape_GetChainSegment shapeB
            let mutable cache = B2Collisions.b2_emptySimplexCache
            -B2Manifolds.b2CollideChainSegmentAndPolygon(&chainSegment, transformB, &polygon, transformA, &cache).normal
        | (B2ShapeType.b2_chainSegmentShape, B2ShapeType.b2_circleShape) ->
            let mutable chainSegment = B2Shapes.b2Shape_GetChainSegment shapeA
            let mutable circle = B2Shapes.b2Shape_GetCircle shapeB
            B2Manifolds.b2CollideChainSegmentAndCircle(&chainSegment, transformA, &circle, transformB).normal
        | (B2ShapeType.b2_chainSegmentShape, B2ShapeType.b2_capsuleShape) ->
            let mutable chainSegment = B2Shapes.b2Shape_GetChainSegment shapeA
            let mutable capsule = B2Shapes.b2Shape_GetCapsule shapeB
            let mutable cache = B2Collisions.b2_emptySimplexCache
            B2Manifolds.b2CollideChainSegmentAndCapsule(&chainSegment, transformA, &capsule, transformB, &cache).normal
        | (B2ShapeType.b2_chainSegmentShape, B2ShapeType.b2_segmentShape) ->
            failwith "Unexpected chain segment to segment collision" // only shapes with volume can collide
        | (B2ShapeType.b2_chainSegmentShape, B2ShapeType.b2_polygonShape) ->
            let mutable chainSegment = B2Shapes.b2Shape_GetChainSegment shapeA
            let mutable polygon = B2Shapes.b2Shape_GetPolygon shapeB
            let mutable cache = B2Collisions.b2_emptySimplexCache
            B2Manifolds.b2CollideChainSegmentAndPolygon(&chainSegment, transformA, &polygon, transformB, &cache).normal
        | (B2ShapeType.b2_chainSegmentShape, B2ShapeType.b2_chainSegmentShape) ->
            failwith "Unexpected chain segment to chain segment collision" // only shapes with volume can collide
        | (a, b) -> failwith $"Unknown shape types {scstring a} and {scstring b} in collision."

    static let configureBodyShapeProperties (bodyShapeDef : _ byref) bodySource (bodyProperties : BodyProperties) bodyShapePropertiesOpt =
        bodyShapeDef <- B2Types.b2DefaultShapeDef ()
        match bodyShapePropertiesOpt with
        | Some bodyShapeProperties ->
            bodyShapeDef.material.friction <- match bodyShapeProperties.FrictionOpt with Some f -> f | None -> bodyProperties.Friction
            bodyShapeDef.material.restitution <- match bodyShapeProperties.RestitutionOpt with Some r -> r | None -> bodyProperties.Restitution
            bodyShapeDef.material.rollingResistance <- match bodyShapeProperties.RollingResistanceOpt with Some r -> r | None -> bodyProperties.RollingResistance
            bodyShapeDef.material.tangentSpeed <- Box2dNetPhysicsEngine.toPhysics <| match bodyShapeProperties.TangentialSpeedOpt with Some t -> t | None -> bodyProperties.LinearConveyorVelocity.X
            bodyShapeDef.filter.groupIndex <- match bodyShapeProperties.CollisionGroupOpt with Some cg -> cg | None -> bodyProperties.CollisionGroup
            bodyShapeDef.filter.categoryBits <- match bodyShapeProperties.CollisionCategoriesOpt with Some cc -> cc | None -> bodyProperties.CollisionCategories
            bodyShapeDef.filter.maskBits <- match bodyShapeProperties.CollisionMaskOpt with Some cm -> cm | None -> bodyProperties.CollisionMask
            bodyShapeDef.isSensor <- match bodyShapeProperties.SensorOpt with Some sensor -> sensor | None -> bodyProperties.Sensor
        | None ->
            bodyShapeDef.material.friction <- bodyProperties.Friction
            bodyShapeDef.material.restitution <- bodyProperties.Restitution
            bodyShapeDef.material.rollingResistance <- bodyProperties.RollingResistance
            bodyShapeDef.material.tangentSpeed <- Box2dNetPhysicsEngine.toPhysics bodyProperties.LinearConveyorVelocity.X
            bodyShapeDef.filter.groupIndex <- bodyProperties.CollisionGroup
            bodyShapeDef.filter.categoryBits <- bodyProperties.CollisionCategories
            bodyShapeDef.filter.maskBits <- bodyProperties.CollisionMask
            bodyShapeDef.isSensor <- bodyProperties.Sensor
        bodyShapeDef.enablePreSolveEvents <- Constants.Physics.Collision2dFrameCompensation // record non-sensor begin contact events via preSolveCallback only when needed
        bodyShapeDef.enableContactEvents <- true // record non-sensor contacts via b2World_GetContactEvents
        bodyShapeDef.enableSensorEvents <- true // record sensor contacts via b2World_GetSensorEvents
        bodyShapeDef.userData <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              BodyShapeIndex = match bodyShapePropertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }

    static let tryCreateShapeProxy (proxy : B2ShapeProxy byref) (shape : BodyShape) (extraTransformOpt : Affine option) (origin : Vector3) =
        match shape with
        | EmptyShape ->
            false
        | BoxShape { Size = size; TransformOpt = transformOpt } ->
            let transformOpt = Option.map2 Affine.combineAsMatrix transformOpt extraTransformOpt
            let halfExtent = size * 0.5f
            proxy.count <- 4
            proxy.points.[0] <- (halfExtent, transformOpt) ||> Option.fold _.Transform |> (+) origin |> Box2dNetPhysicsEngine.toPhysicsV2
            proxy.points.[1] <- (halfExtent.MapX (~-), transformOpt) ||> Option.fold _.Transform |> (+) origin |> Box2dNetPhysicsEngine.toPhysicsV2
            proxy.points.[2] <- (halfExtent.MapY (~-), transformOpt) ||> Option.fold _.Transform |> (+) origin |> Box2dNetPhysicsEngine.toPhysicsV2
            proxy.points.[3] <- (-halfExtent, transformOpt) ||> Option.fold _.Transform |> (+) origin |> Box2dNetPhysicsEngine.toPhysicsV2
            true
        | BoxRoundedShape { Size = size; TransformOpt = transformOpt; Radius = radius } ->
            let transformOpt = Option.map2 Affine.combineAsMatrix transformOpt extraTransformOpt
            let halfExtent = size * 0.5f - v3 radius radius 0.0f
            proxy.count <- 4
            proxy.points.[0] <- (halfExtent, transformOpt) ||> Option.fold _.Transform |> (+) origin |> Box2dNetPhysicsEngine.toPhysicsV2
            proxy.points.[1] <- (halfExtent.MapX (~-), transformOpt) ||> Option.fold _.Transform |> (+) origin |> Box2dNetPhysicsEngine.toPhysicsV2
            proxy.points.[2] <- (halfExtent.MapY (~-), transformOpt) ||> Option.fold _.Transform |> (+) origin |> Box2dNetPhysicsEngine.toPhysicsV2
            proxy.points.[3] <- (-halfExtent, transformOpt) ||> Option.fold _.Transform |> (+) origin |> Box2dNetPhysicsEngine.toPhysicsV2
            proxy.radius <- transformOpt |> Option.mapOrDefaultValue _.Scale.X 1.0f |> (*) radius |> Box2dNetPhysicsEngine.toPhysics
            true
        | SphereShape sphereShape ->
            let transformOpt = Option.map2 Affine.combineAsMatrix sphereShape.TransformOpt extraTransformOpt
            proxy.count <- 1
            proxy.points.[0] <- (v3Zero, transformOpt) ||> Option.fold _.Transform |> (+) origin |> Box2dNetPhysicsEngine.toPhysicsV2
            proxy.radius <- transformOpt |> Option.mapOrDefaultValue _.Scale.X 1.0f |> (*) sphereShape.Radius |> Box2dNetPhysicsEngine.toPhysics
            true
        | CapsuleShape capsuleShape ->
            let transformOpt = Option.map2 Affine.combineAsMatrix capsuleShape.TransformOpt extraTransformOpt
            let extent = capsuleShape.Height * 0.5f
            proxy.count <- 2
            proxy.points.[0] <- (v3 0f extent 0f, transformOpt) ||> Option.fold _.Transform |> (+) origin |> Box2dNetPhysicsEngine.toPhysicsV2
            proxy.points.[1] <- (v3 0f -extent 0f, transformOpt) ||> Option.fold _.Transform |> (+) origin |> Box2dNetPhysicsEngine.toPhysicsV2
            proxy.radius <- transformOpt |> Option.mapOrDefaultValue _.Scale.X 1.0f |> (*) capsuleShape.Radius |> Box2dNetPhysicsEngine.toPhysics
            true
        | EdgeShape edgeShape ->
            let transformOpt = Option.map2 Affine.combineAsMatrix edgeShape.TransformOpt extraTransformOpt
            proxy.count <- 2
            proxy.points.[0] <- (edgeShape.Start, transformOpt) ||> Option.fold _.Transform |> (+) origin |> Box2dNetPhysicsEngine.toPhysicsV2
            proxy.points.[1] <- (edgeShape.Stop, transformOpt) ||> Option.fold _.Transform |> (+) origin |> Box2dNetPhysicsEngine.toPhysicsV2
            true
        | ContourShape _ ->
            // this needs to be implemented using multiple shape casts against each link on the contour
            Log.warn "ContourShape casting is not implemented in Box2dNetPhysicsEngine."
            false
        | PointsShape { Profile = Convex; Points = points; TransformOpt = transformOpt }
        | GeometryShape { Profile = Convex; Vertices = points; TransformOpt = transformOpt } ->
            // even if the points are non-convex, Box2D's shape cast will use the convex hull implicitly
            let transformOpt = Option.map2 Affine.combineAsMatrix transformOpt extraTransformOpt
            if points.Length > B2Constants.B2_MAX_POLYGON_VERTICES then
                Log.warn $"2D Convex PointsShape has too many points (%d{points.Length}) for Box2D shape casting. Truncating to %d{B2Constants.B2_MAX_POLYGON_VERTICES}."
            proxy.count <- min B2Constants.B2_MAX_POLYGON_VERTICES points.Length
            for i in 0 .. dec proxy.count do
                proxy.points.[i] <- (points.[i], transformOpt) ||> Option.fold _.Transform |> (+) origin |> Box2dNetPhysicsEngine.toPhysicsV2
            true
        | PointsShape { Profile = Concave }
        | GeometryShape { Profile = Concave } ->
            // this needs to be implemented using multiple shape casts against each triangle of the concave shape
            Log.warn "Concave PointsShape/GeometryShape casting is not implemented in Box2dNetPhysicsEngine."
            false
        | PointsShape { Profile = Bounds; Points = points; TransformOpt = transformOpt }
        | GeometryShape { Profile = Bounds; Vertices = points; TransformOpt = transformOpt } ->
            let transformOpt = Option.map2 Affine.combineAsMatrix transformOpt extraTransformOpt
            let bounds = points |> Array.map (match transformOpt with Some t -> _.Transform(t).V2 | None -> _.V2) |> Box2.Enclose |> _.Translate(origin.V2)
            let toPhysicsV2 (v : Vector2) = B2Vec2 (Box2dNetPhysicsEngine.toPhysics v.X, Box2dNetPhysicsEngine.toPhysics v.Y)
            proxy.count <- 4
            proxy.points.[0] <- toPhysicsV2 bounds.TopLeft
            proxy.points.[1] <- toPhysicsV2 bounds.TopRight
            proxy.points.[2] <- toPhysicsV2 bounds.BottomLeft
            proxy.points.[3] <- toPhysicsV2 bounds.BottomRight
            true
        | StaticModelShape _ ->
            Log.warn "StaticModelShape is not supported in Box2dNetPhysicsEngine."
            false
        | StaticModelSurfaceShape _ ->
            Log.warn "StaticModelSurfaceShape is not supported in Box2dNetPhysicsEngine."
            false
        | TerrainShape _ ->
            Log.warn "TerrainShape is not supported in Box2dNetPhysicsEngine."
            false
        | BodyShapes _ ->
            // this needs to be implemented using multiple shape casts against each shape
            Log.warn "BodyShapes casting is not implemented in Box2dNetPhysicsEngine."
            false

    static member private attachBoxBody bodySource (bodyProperties : BodyProperties) (boxShape : BoxShape) (body : B2BodyId) =
        let transform = Option.defaultValue Affine.Identity boxShape.TransformOpt
        let width = Box2dNetPhysicsEngine.toPhysicsPolygonDiameter (boxShape.Size.X * transform.Scale.X)
        let height = Box2dNetPhysicsEngine.toPhysicsPolygonDiameter (boxShape.Size.Y * transform.Scale.Y)
        let offset = Box2dNetPhysicsEngine.toPhysicsV2 transform.Translation
        let mutable shapeDef = Unchecked.defaultof<_>
        configureBodyShapeProperties &shapeDef bodySource bodyProperties boxShape.PropertiesOpt
        shapeDef.density <-
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass -> mass / (width * height)
        let mutable rectangleVertices = B2Geometries.b2MakeOffsetBox (width * 0.5f, height * 0.5f, offset, Box2dNetPhysicsEngine.quatToRot transform.Rotation)
        B2Shapes.b2CreatePolygonShape (body, &shapeDef, &rectangleVertices) |> ignore<B2ShapeId>

    static member private attachSphereShape bodySource (bodyProperties : BodyProperties) (sphereShape : SphereShape) (body : B2BodyId) =
        let transform = Option.defaultValue Affine.Identity sphereShape.TransformOpt
        let radius = Box2dNetPhysicsEngine.toPhysicsPolygonRadius (sphereShape.Radius * transform.Scale.X)
        let offset = Box2dNetPhysicsEngine.toPhysicsV2 transform.Translation
        let mutable shapeDef = Unchecked.defaultof<_>
        configureBodyShapeProperties &shapeDef bodySource bodyProperties sphereShape.PropertiesOpt
        shapeDef.density <-
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass -> mass / (MathF.PI * radius * radius)
        let mutable circle = B2Circle (offset, radius)
        B2Shapes.b2CreateCircleShape (body, &shapeDef, &circle) |> ignore<B2ShapeId>

    static member private toPhysicsCapsule (capsuleShape : CapsuleShape) =
        let transform = Option.defaultValue Affine.Identity capsuleShape.TransformOpt
        let height = Box2dNetPhysicsEngine.toPhysicsPolygonDiameter (capsuleShape.Height * transform.Scale.Y)
        let endRadius = Box2dNetPhysicsEngine.toPhysicsPolygonRadius (capsuleShape.Radius * transform.Scale.Y)
        let offset = Box2dNetPhysicsEngine.toPhysicsV2 transform.Translation
        let circleOffset = B2MathFunction.b2RotateVector (Box2dNetPhysicsEngine.quatToRot transform.Rotation, B2Vec2 (0.0f, height * 0.5f))
        B2Capsule (circleOffset + offset, -circleOffset + offset, endRadius)

    static member private computeCapsuleDensity substance height radius transformOpt =
        match substance with
        | Density density -> density
        | Mass mass ->
            let height =
                Box2dNetPhysicsEngine.toPhysicsPolygonDiameter (height * Option.mapOrDefaultValue _.Scale.Y 1.0f transformOpt)
            let radius = Box2dNetPhysicsEngine.toPhysicsPolygonRadius radius
            mass / (radius * 2.0f * height + MathF.PI * radius * radius)

    static member private attachCapsuleShape bodySource (bodyProperties : BodyProperties) (capsuleShape : CapsuleShape) (body : B2BodyId) =
        let mutable shapeDef = Unchecked.defaultof<_>
        configureBodyShapeProperties &shapeDef bodySource bodyProperties capsuleShape.PropertiesOpt
        let mutable capsule = Box2dNetPhysicsEngine.toPhysicsCapsule capsuleShape
        shapeDef.density <- Box2dNetPhysicsEngine.computeCapsuleDensity bodyProperties.Substance capsuleShape.Height capsuleShape.Radius capsuleShape.TransformOpt
        B2Shapes.b2CreateCapsuleShape (body, &shapeDef, &capsule) |> ignore<B2ShapeId>

    static member private attachBoxRoundedShape bodySource (bodyProperties : BodyProperties) (boxRoundedShape : BoxRoundedShape) (body : B2BodyId) =
        let transform = Option.defaultValue Affine.Identity boxRoundedShape.TransformOpt
        let width = Box2dNetPhysicsEngine.toPhysicsPolygonDiameter (boxRoundedShape.Size.X * transform.Scale.X)
        let height = Box2dNetPhysicsEngine.toPhysicsPolygonDiameter (boxRoundedShape.Size.Y * transform.Scale.Y)
        let radius = Box2dNetPhysicsEngine.toPhysicsPolygonRadius (boxRoundedShape.Radius * transform.Scale.X)
        let center = Box2dNetPhysicsEngine.toPhysicsV2 transform.Translation
        let mutable shapeDef = Unchecked.defaultof<_>
        configureBodyShapeProperties &shapeDef bodySource bodyProperties boxRoundedShape.PropertiesOpt
        shapeDef.density <-
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass -> mass / (width * height - radius * radius * (4.0f - MathF.PI))
        let mutable polygon = B2Geometries.b2MakeOffsetRoundedBox (width * 0.5f - radius, height * 0.5f - radius, center, Box2dNetPhysicsEngine.quatToRot transform.Rotation, radius)
        B2Shapes.b2CreatePolygonShape (body, &shapeDef, &polygon) |> ignore<B2ShapeId>

    static member private attachEdgeShape bodySource bodyProperties (edgeShape : EdgeShape) (body : B2BodyId) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity edgeShape.TransformOpt
        let mutable segment =
            B2Segment
                (Box2dNetPhysicsEngine.toPhysicsV2 (edgeShape.Start.Transform transform),
                 Box2dNetPhysicsEngine.toPhysicsV2 (edgeShape.Stop.Transform transform))
        let mutable shapeDef = Unchecked.defaultof<_>
        configureBodyShapeProperties &shapeDef bodySource bodyProperties edgeShape.PropertiesOpt
        B2Shapes.b2CreateSegmentShape (body, &shapeDef, &segment) |> ignore<B2ShapeId>

    static member private attachContourShape bodySource (bodyProperties : BodyProperties) (contourShape : ContourShape) (body : B2BodyId) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity contourShape.TransformOpt
        let vertices' = Array.zeroCreate contourShape.Links.Length
        for i in 0 .. dec contourShape.Links.Length do
            vertices'.[i] <- Box2dNetPhysicsEngine.toPhysicsV2 (contourShape.Links.[i].Transform transform)
        let mutable chainDef = B2Types.b2DefaultChainDef ()
        chainDef.count <- vertices'.Length
        chainDef.points <- vertices'
        chainDef.isLoop <- contourShape.Closed
        match contourShape.PropertiesOpt with
        | Some bodyShapeProperties ->
            chainDef.materials.[0].friction <- match bodyShapeProperties.FrictionOpt with Some f -> f | None -> bodyProperties.Friction // default chain definition has 1 material
            chainDef.materials.[0].restitution <- match bodyShapeProperties.RestitutionOpt with Some r -> r | None -> bodyProperties.Restitution
            chainDef.filter.groupIndex <- match bodyShapeProperties.CollisionGroupOpt with Some cg -> cg | None -> bodyProperties.CollisionGroup
            chainDef.filter.categoryBits <- match bodyShapeProperties.CollisionCategoriesOpt with Some cc -> cc | None -> bodyProperties.CollisionCategories
            chainDef.filter.maskBits <- match bodyShapeProperties.CollisionMaskOpt with Some cm -> cm | None -> bodyProperties.CollisionMask
        | None ->
            chainDef.materials.[0].friction <- bodyProperties.Friction
            chainDef.materials.[0].restitution <- bodyProperties.Restitution
            chainDef.filter.groupIndex <- bodyProperties.CollisionGroup
            chainDef.filter.categoryBits <- bodyProperties.CollisionCategories
            chainDef.filter.maskBits <- bodyProperties.CollisionMask
        chainDef.enableSensorEvents <- true // record sensor contacts for b2World_GetSensorEvents
        chainDef.userData <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              BodyShapeIndex = match contourShape.PropertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
        B2Shapes.b2CreateChain (body, &chainDef) |> ignore<B2ChainId>

    static member private attachBodyConvexHull bodySource bodyProperties (points : Vector3 array) transformOpt (propertiesOpt : BodyShapeProperties option) (body : B2BodyId) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity transformOpt
        let points' = Array.zeroCreate points.Length
        for i in 0 .. dec points.Length do
            points'.[i] <- Box2dNetPhysicsEngine.toPhysicsV2 (points.[i].Transform transform)
        let mutable hull = B2Hulls.b2ComputeHull (points'.AsSpan (), points'.Length)
        if hull.count = 0 then
            Log.warn $"Failed to create convex hull polygon for {scstring points}. Maybe your points are too close together, are collinear, or consists of < 3 or > 8 points (please decompose them into smaller polygons)?"
        else
            let mutable shapeDef = Unchecked.defaultof<_>
            configureBodyShapeProperties &shapeDef bodySource bodyProperties propertiesOpt
            shapeDef.density <-
                match bodyProperties.Substance with
                | Density density -> density
                | Mass mass ->
                    // triangulate the polygon to compute area: https://github.com/ikpil/Box2D.NET/blob/bb5a9bb4b40d27007fb634c686f55eaa4a01ca52/src/Box2D.NET/B2Geometries.cs#L382-L395
                    let mutable doubleArea = 0.0f
                    let r = points'.[0]
                    for i in 1 .. points'.Length - 2 do
                        let e1 = points'.[i] - r
                        let e2 = points'.[i + 1] - r
                        doubleArea <- doubleArea + B2MathFunction.b2Cross (e1, e2)
                    mass * 2.0f / doubleArea
            let mutable polygon = B2Geometries.b2MakePolygon (&hull, 0.0f)
            B2Shapes.b2CreatePolygonShape (body, &shapeDef, &polygon) |> ignore<B2ShapeId>

    static member private attachBodyTriangles bodySource bodyProperties (vertices : Vector3 array) transformOpt (propertiesOpt : BodyShapeProperties option) body =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity transformOpt
        let vertices' = Array.zeroCreate vertices.Length
        for i in 0 .. dec vertices.Length do
            vertices'.[i] <- Box2dNetPhysicsEngine.toPhysicsV2 (vertices.[i].Transform transform)
        let mutable shapeDef = Unchecked.defaultof<_>
        configureBodyShapeProperties &shapeDef bodySource bodyProperties propertiesOpt
        shapeDef.density <-
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass ->
                let mutable doubleArea = 0.0f
                for i in 0 .. vertices'.Length / 3 - 1 do
                    let r = vertices'.[i * 3]
                    let e1 = vertices'.[i * 3 + 1] - r
                    let e2 = vertices'.[i * 3 + 2] - r
                    doubleArea <- doubleArea + B2MathFunction.b2Cross (e1, e2)
                mass * 2.0f / doubleArea
        let struct (triangleCount, ignoredPoints) = Math.DivRem (vertices'.Length, 3)
        if ignoredPoints <> 0 then
            Log.warn $"Ignoring points {scstring (Array.sub vertices' (triangleCount * 3) ignoredPoints)} at the end of the vertices array since there are not enough to form a triangle."
        for i in 0 .. triangleCount do
            let mutable hull = B2Hulls.b2ComputeHull (vertices'.AsSpan (i * 3, 3), 3)
            if hull.count = 0 then
                Log.warn $"Failed to create triangle for {scstring (Array.sub vertices' (i * 3) 3)}. Maybe your points are too close together or are collinear?"
            else
                let mutable polygon = B2Geometries.b2MakePolygon (&hull, 0.0f)
                B2Shapes.b2CreatePolygonShape (body, &shapeDef, &polygon) |> ignore<B2ShapeId>

    static member private attachBodyBounds bodySource bodyProperties (points : Vector3 array) transformOpt (propertiesOpt : BodyShapeProperties option) body =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity transformOpt
        let bounds = points |> Array.map _.Transform(transform).V2 |> Box2.Enclose
        let width = Box2dNetPhysicsEngine.toPhysicsPolygonDiameter bounds.Width
        let height = Box2dNetPhysicsEngine.toPhysicsPolygonDiameter bounds.Height
        let offset = Box2dNetPhysicsEngine.toPhysicsV2 bounds.Center.V3
        let mutable shapeDef = Unchecked.defaultof<_>
        configureBodyShapeProperties &shapeDef bodySource bodyProperties propertiesOpt
        shapeDef.density <-
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass -> mass / (width * height)
        let mutable rectangleVertices = B2Geometries.b2MakeOffsetBox (width * 0.5f, height * 0.5f, offset, B2MathFunction.b2Rot_identity)
        B2Shapes.b2CreatePolygonShape (body, &shapeDef, &rectangleVertices) |> ignore<B2ShapeId>

    static member private attachPointsShape bodySource bodyProperties (pointsShape : PointsShape) body =
        match pointsShape.Profile with
        | Convex -> Box2dNetPhysicsEngine.attachBodyConvexHull bodySource bodyProperties pointsShape.Points pointsShape.TransformOpt pointsShape.PropertiesOpt body
        | Concave ->
            Log.warnOnce "Creating a compound polygon with PointsShape; PointsShape generally specifies individual points rather than triangulated vertices, so unintended behavior may arise."
            Box2dNetPhysicsEngine.attachBodyTriangles bodySource bodyProperties pointsShape.Points pointsShape.TransformOpt pointsShape.PropertiesOpt body
        | Bounds -> Box2dNetPhysicsEngine.attachBodyBounds bodySource bodyProperties pointsShape.Points pointsShape.TransformOpt pointsShape.PropertiesOpt body

    static member private attachGeometryShape bodySource bodyProperties (geometryShape : GeometryShape) body =
        match geometryShape.Profile with
        | Convex -> Box2dNetPhysicsEngine.attachBodyConvexHull bodySource bodyProperties geometryShape.Vertices geometryShape.TransformOpt geometryShape.PropertiesOpt body
        | Concave -> Box2dNetPhysicsEngine.attachBodyTriangles bodySource bodyProperties geometryShape.Vertices geometryShape.TransformOpt geometryShape.PropertiesOpt body
        | Bounds -> Box2dNetPhysicsEngine.attachBodyBounds bodySource bodyProperties geometryShape.Vertices geometryShape.TransformOpt geometryShape.PropertiesOpt body

    static member private attachBodyShape bodySource bodyProperties bodyShape body =
        match bodyShape with
        | EmptyShape -> ()
        | BoxShape boxShape -> Box2dNetPhysicsEngine.attachBoxBody bodySource bodyProperties boxShape body
        | SphereShape sphereShape -> Box2dNetPhysicsEngine.attachSphereShape bodySource bodyProperties sphereShape body
        | CapsuleShape capsuleShape -> Box2dNetPhysicsEngine.attachCapsuleShape bodySource bodyProperties capsuleShape body
        | BoxRoundedShape boxRoundedShape -> Box2dNetPhysicsEngine.attachBoxRoundedShape bodySource bodyProperties boxRoundedShape body
        | EdgeShape edgeShape -> Box2dNetPhysicsEngine.attachEdgeShape bodySource bodyProperties edgeShape body
        | ContourShape contourShape -> Box2dNetPhysicsEngine.attachContourShape bodySource bodyProperties contourShape body
        | PointsShape pointsShape -> Box2dNetPhysicsEngine.attachPointsShape bodySource bodyProperties pointsShape body
        | GeometryShape geometryShape -> Box2dNetPhysicsEngine.attachGeometryShape bodySource bodyProperties geometryShape body
        | StaticModelShape _ -> ()
        | StaticModelSurfaceShape _ -> ()
        | TerrainShape _ -> ()
        | BodyShapes bodyShapes -> for bodyShape in bodyShapes do Box2dNetPhysicsEngine.attachBodyShape bodySource bodyProperties bodyShape body

    static member private createBody (createBodyMessage : CreateBodyMessage) physicsEngine =

        // get fields
        let bodyId = createBodyMessage.BodyId
        let bodyProperties = createBodyMessage.BodyProperties

        // configure body
        let mutable bodyDef = B2Types.b2DefaultBodyDef ()
        let isCharacter =
            match bodyProperties.BodyType with
            | Static -> bodyDef.``type`` <- B2BodyType.b2_staticBody; false
            | Kinematic -> bodyDef.``type`` <- B2BodyType.b2_kinematicBody; false
            | KinematicCharacter -> bodyDef.``type`` <- B2BodyType.b2_kinematicBody; true
            | Dynamic -> bodyDef.``type`` <- B2BodyType.b2_dynamicBody; false
            | DynamicCharacter -> bodyDef.``type`` <- B2BodyType.b2_dynamicBody; true
            | Vehicle ->
                Log.warn "Vehicle body type is not supported by Box2dNetPhysicsEngine, wheel joints should be used instead. Using Dynamic configuration."
                bodyDef.``type`` <- B2BodyType.b2_dynamicBody
                false
        bodyDef.isEnabled <- bodyProperties.Enabled
        bodyDef.enableSleep <- bodyProperties.SleepingAllowed
        bodyDef.position <- Box2dNetPhysicsEngine.toPhysicsV2 bodyProperties.Center
        bodyDef.rotation <- Box2dNetPhysicsEngine.quatToRot bodyProperties.Rotation
        bodyDef.linearVelocity <- Box2dNetPhysicsEngine.toPhysicsV2 bodyProperties.LinearVelocity
        bodyDef.linearDamping <- bodyProperties.LinearDamping
        bodyDef.angularVelocity <- bodyProperties.AngularVelocity.Z
        if bodyProperties.AngularVelocity.X <> 0.0f || bodyProperties.AngularVelocity.Y <> 0.0f then Log.warnOnce "AngularVelocity is only supported for the Z dimension in Box2dNetPhysicsEngine."
        if bodyProperties.LinearConveyorVelocity.Y <> 0.0f || bodyProperties.LinearConveyorVelocity.Z <> 0.0f then Log.warnOnce "LinearConveyorVelocity is only supported for the X dimension in Box2dNetPhysicsEngine."
        if bodyProperties.AngularConveyorVelocity <> v3Zero then Log.warnOnce "AngularConveyorVelocity is unsupported in Box2dNetPhysicsEngine."
        bodyDef.angularDamping <- bodyProperties.AngularDamping
        bodyDef.fixedRotation <- bodyProperties.AngularFactor.Z = 0.0f
        if bodyProperties.AngularFactor.X <> 1.0f || bodyProperties.AngularFactor.Y <> 1.0f then Log.warnOnce "AngularFactor is only supported for the Z dimension in Box2dNetPhysicsEngine."
        let gravityOverrideOpt =
            if bodyProperties.BodyType = Dynamic then // NOTE: characters have custom gravity handling.
                match bodyProperties.Gravity with
                | GravityWorld -> bodyDef.gravityScale <- 1.0f; ValueNone
                | GravityOverride gravity -> bodyDef.gravityScale <- 0.0f; ValueSome gravity // NOTE: gravity overrides are handled by applying a manual velocity each step.
                | GravityScale scale -> bodyDef.gravityScale <- scale; ValueNone
                | GravityIgnore -> bodyDef.gravityScale <- 0.0f; ValueNone
            else ValueNone
        bodyDef.isBullet <- match bodyProperties.CollisionDetection with Continuous -> true | Discrete -> false
        bodyDef.userData <- bodyId

        // make and attempt to add the body
        let bodyId = { BodySource = createBodyMessage.BodyId.BodySource; BodyIndex = bodyProperties.BodyIndex }
        let body = B2Bodies.b2CreateBody (physicsEngine.PhysicsContextId, &bodyDef)
        if physicsEngine.Bodies.TryAdd (bodyId, body) then

            // register gravity overrides
            match gravityOverrideOpt with
            | ValueSome gravityOverride ->
                physicsEngine.BodyGravityOverrides.[bodyId] <- gravityOverride
            | ValueNone -> ()

            // register character soft collision
            match bodyProperties.CharacterSoftCollisionPushLimitOpt with
            | Some pushLimit ->
                physicsEngine.CharacterCollisionContext.SoftCollisionPushLimits.[body] <- Box2dNetPhysicsEngine.toPhysics pushLimit
            | None -> ()

            if not isCharacter then
                // attach shapes
                Box2dNetPhysicsEngine.attachBodyShape bodyId.BodySource bodyProperties bodyProperties.BodyShape body
            else
                match bodyProperties.BodyShape with
                | CapsuleShape capsuleShape ->
                    match bodyProperties.CharacterProperties with
                    | PogoSpring properties ->
                        let mutable capsule = Box2dNetPhysicsEngine.toPhysicsCapsule capsuleShape

                        // ensure center1 is at the bottom for pogo casting
                        if capsule.center1.Y > capsule.center2.Y then
                            let topPoint = capsule.center1
                            capsule.center1 <- capsule.center2
                            capsule.center2 <- topPoint

                        // adjust the bottom point of the capsule upward by the projection of (0, pogoRestLength) onto the capsule vector
                        let capsuleVector = capsule.center2 - capsule.center1 // from the bottom point of the capsule
                        let pogoRestLength = properties.PogoRestLengthScalar * B2MathFunction.b2Length capsuleVector
                        let capsuleVectorPogoPortion = 
                            B2MathFunction.b2Dot (capsuleVector, B2Vec2 (0.0f, pogoRestLength)) /
                            B2MathFunction.b2LengthSquared capsuleVector
                        capsule.center1 <- capsule.center1 + capsuleVectorPogoPortion * capsuleVector
                                
                        let mutable shapeDef = Unchecked.defaultof<_>
                        configureBodyShapeProperties &shapeDef bodyId.BodySource bodyProperties capsuleShape.PropertiesOpt
                        shapeDef.density <- Box2dNetPhysicsEngine.computeCapsuleDensity bodyProperties.Substance (capsuleShape.Height * (1.0f - capsuleVectorPogoPortion)) capsuleShape.Radius capsuleShape.TransformOpt
                        let shape = B2Shapes.b2CreateCapsuleShape (body, &shapeDef, &capsule)
                        let (gravityScale, gravityOverride) =
                            match bodyProperties.Gravity with
                            | GravityWorld -> (1.0f, B2MathFunction.b2Vec2_zero)
                            | GravityOverride gravity -> (0.0f, Box2dNetPhysicsEngine.toPhysicsV2 gravity)
                            | GravityScale scale -> (scale, B2MathFunction.b2Vec2_zero)
                            | GravityIgnore -> (0.0f, B2MathFunction.b2Vec2_zero)

                        physicsEngine.Characters.[bodyId] <-
                            { PogoRestLength = pogoRestLength
                              PogoHertz = properties.PogoHertz
                              PogoDampingRatio = properties.PogoDampingRatio
                              PogoProxy =
                                match properties.PogoShape with
                                | PogoPoint -> B2Distances.b2MakeProxy (B2Vec2 (), 1, 0.0f)
                                | PogoCircle diameterScalar -> B2Distances.b2MakeProxy (B2Vec2 (), 1, diameterScalar * 0.5f * capsule.radius)
                                | PogoSegment widthScalar ->
                                    let segmentOffset = B2Vec2 (widthScalar * capsule.radius, 0.0f)
                                    B2Distances.b2MakeProxy (-segmentOffset, segmentOffset, 2, 0.0f)
                              CollisionCategory = shapeDef.filter.categoryBits
                              CastMask = shapeDef.filter.maskBits
                              CollisionMask = shapeDef.filter.maskBits ||| properties.AdditionalSoftCollisionMask
                              Capsule = capsule
                              CapsuleShape = shape
                              Mass = B2Bodies.b2Body_GetMass body
                              GravityScale = gravityScale
                              GravityOverride = gravityOverride
                              Transform = B2Transform (bodyDef.position, bodyDef.rotation)
                              Velocity = B2Vec2 ()
                              OnGround = ValueNone
                              PogoVelocity = 0.0f
                              PogoOrigin = B2Vec2 ()
                              PogoDelta = B2Vec2 ()
                              GroundCastResult = B2RayResult () }
                    | StairStepping _ ->
                        Log.warn "StairStepping CharacterProperties is not yet implemented in Box2dPhysicsEngine."
                | s -> Log.warn $"Characters in Box2dNetPhysicsEngine only support CapsuleShapes. Ignoring character body type for {scstring s}."

        else
            Log.error ("Could not add body for '" + scstring bodyId + "' as it already exists.")

        // attempt to run any related body joint creation functions
        match physicsEngine.CreateBodyJointMessages.TryGetValue bodyId with
        | (true, createBodyJointMessages) ->
            for createBodyJointMessage in createBodyJointMessages do
                let bodyJointId = { BodyJointSource = createBodyJointMessage.BodyJointSource; BodyJointIndex = createBodyJointMessage.BodyJointProperties.BodyJointIndex }
                Box2dNetPhysicsEngine.destroyBodyJointInternal bodyJointId physicsEngine
                Box2dNetPhysicsEngine.createBodyJointInternal createBodyJointMessage.BodyJointProperties bodyJointId physicsEngine
        | (false, _) -> ()

    static member private createBodies (createBodiesMessage : CreateBodiesMessage) physicsEngine =
        List.iter
            (fun (bodyProperties : BodyProperties) ->
                let createBodyMessage =
                    { BodyId = { BodySource = createBodiesMessage.BodySource; BodyIndex = bodyProperties.BodyIndex }
                      BodyProperties = bodyProperties }
                Box2dNetPhysicsEngine.createBody createBodyMessage physicsEngine)
            createBodiesMessage.BodiesProperties

    static member private destroyBody (destroyBodyMessage : DestroyBodyMessage) physicsEngine =

        // attempt to run any related body joint destruction functions but keep the messages themselves for body re-creation later
        let bodyId = destroyBodyMessage.BodyId
        match physicsEngine.CreateBodyJointMessages.TryGetValue bodyId with
        | (true, createBodyJointMessages) ->
            for createBodyJointMessage in createBodyJointMessages do
                let bodyJointId = { BodyJointSource = createBodyJointMessage.BodyJointSource; BodyJointIndex = createBodyJointMessage.BodyJointProperties.BodyJointIndex }
                Box2dNetPhysicsEngine.destroyBodyJointInternal bodyJointId physicsEngine
        | (false, _) -> ()

        // attempt to destroy body
        match physicsEngine.Bodies.TryGetValue bodyId with
        | (true, body) ->
            physicsEngine.Bodies.Remove bodyId |> ignore<bool>
            physicsEngine.BodyGravityOverrides.Remove bodyId |> ignore<bool>
            physicsEngine.Characters.Remove bodyId |> ignore<bool>
            physicsEngine.CharacterCollisionContext.SoftCollisionPushLimits.Remove body |> ignore<bool>
            B2Bodies.b2DestroyBody body
        | (false, _) -> ()

    static member private destroyBodies (destroyBodiesMessage : DestroyBodiesMessage) physicsEngine =
        List.iter (fun bodyId ->
            Box2dNetPhysicsEngine.destroyBody { BodyId = bodyId } physicsEngine)
            destroyBodiesMessage.BodyIds
            
    /// unlike createBodyJoint, whether the body joint is re-created on connected body re-creation is unchanged.
    static member private createBodyJointInternal bodyJointProperties bodyJointId physicsEngine =
        if bodyJointProperties.BodyJointEnabled && not bodyJointProperties.Broken then
            match bodyJointProperties.BodyJoint with
            | EmptyJoint -> ()
            | Box2dNetBodyJoint bodyJoint ->
                let bodyId = bodyJointProperties.BodyJointTarget
                let body2Id = bodyJointProperties.BodyJointTarget2
                match (physicsEngine.Bodies.TryGetValue bodyId, physicsEngine.Bodies.TryGetValue body2Id) with
                | ((true, body), (true, body2)) ->
                    let joint = bodyJoint.CreateBodyJoint Box2dNetPhysicsEngine.toPhysics Box2dNetPhysicsEngine.toPhysicsV2 body body2 physicsEngine.PhysicsContextId
                    B2Joints.b2Joint_SetUserData (joint, bodyJointId)
                    B2Joints.b2Joint_SetCollideConnected (joint, bodyJointProperties.CollideConnected)
                    B2Joints.b2Joint_WakeBodies joint
                    if physicsEngine.Joints.TryAdd (bodyJointId, joint) then
                        match bodyJointProperties.BreakingPointOpt with
                        | Some breakingPoint ->
                            let breakingPoint = Box2dNetPhysicsEngine.toPhysics breakingPoint
                            physicsEngine.BreakableJoints.Add (bodyJointId, struct {| BreakingPoint = breakingPoint; BreakingPointSquared = breakingPoint * breakingPoint |})
                        | None -> ()
                    else Log.warn ("Could not add body joint for '" + scstring bodyJointId + "' as it already exists.")
                | _ -> ()
            | _ -> Log.warn ("Joint type '" + getCaseName bodyJointProperties.BodyJoint + "' not implemented for Box2dNetPhysicsEngine.")

    static member private createBodyJoint (createBodyJointMessage : CreateBodyJointMessage) physicsEngine =

        // log creation message for body joint re-creation on connected body re-creation
        for bodyTarget in [createBodyJointMessage.BodyJointProperties.BodyJointTarget; createBodyJointMessage.BodyJointProperties.BodyJointTarget2] do
            match physicsEngine.CreateBodyJointMessages.TryGetValue bodyTarget with
            | (true, messages) -> messages.Add createBodyJointMessage
            | (false, _) -> physicsEngine.CreateBodyJointMessages.Add (bodyTarget, List [createBodyJointMessage])

        // attempt to add body joint
        let bodyJointId = { BodyJointSource = createBodyJointMessage.BodyJointSource; BodyJointIndex = createBodyJointMessage.BodyJointProperties.BodyJointIndex }
        Box2dNetPhysicsEngine.createBodyJointInternal createBodyJointMessage.BodyJointProperties bodyJointId physicsEngine
        
    /// unlike destroyBodyJoint, whether the body joint is re-created on connected body re-creation is unchanged.
    static member private destroyBodyJointInternal (bodyJointId : BodyJointId) physicsEngine =
        match physicsEngine.Joints.TryGetValue bodyJointId with
        | (true, joint) ->
            physicsEngine.Joints.Remove bodyJointId |> ignore<bool>
            physicsEngine.BreakableJoints.Remove bodyJointId |> ignore<bool>
            B2Joints.b2DestroyJoint joint
        | (false, _) -> ()

    static member private destroyBodyJoint (destroyBodyJointMessage : DestroyBodyJointMessage) physicsEngine =

        // unlog creation message for stopping body joint re-creation on connected body re-creation
        for bodyTarget in [destroyBodyJointMessage.BodyJointTarget; destroyBodyJointMessage.BodyJointTarget2] do
            match physicsEngine.CreateBodyJointMessages.TryGetValue bodyTarget with
            | (true, messages) ->
                messages.RemoveAll (fun message ->
                    message.BodyJointSource = destroyBodyJointMessage.BodyJointId.BodyJointSource &&
                    message.BodyJointProperties.BodyJointIndex = destroyBodyJointMessage.BodyJointId.BodyJointIndex)
                |> ignore<int>
            | (false, _) -> ()

        // attempt to destroy body joint
        Box2dNetPhysicsEngine.destroyBodyJointInternal destroyBodyJointMessage.BodyJointId physicsEngine

    static member private createFluidEmitter (createFluidEmitterMessage : CreateFluidEmitterMessage) physicsEngine =
        let id = createFluidEmitterMessage.FluidEmitterId
        match createFluidEmitterMessage.FluidEmitterDescriptor with
        | FluidEmitterDescriptorBox2dNet descriptor ->
            if not (physicsEngine.FluidEmitters.ContainsKey id) then physicsEngine.FluidEmitters.Add (id, Box2dNetFluidEmitter.make descriptor physicsEngine.PhysicsContextId id.FluidEmitterSource)
            Box2dNetFluidEmitter.addParticles (createFluidEmitterMessage.FluidParticles.GetEnumerator ()) physicsEngine.FluidEmitters.[id]
        | FluidEmitterDescriptorAether _ | FluidEmitterDescriptorJolt -> () // no support

    static member private destroyFluidEmitter (destroyFluidEmitterMessage : DestroyFluidEmitterMessage) physicsEngine =
        physicsEngine.FluidEmitters.Remove destroyFluidEmitterMessage.FluidEmitterId |> ignore<bool>

    static member private setBodyEnabled (setBodyEnabledMessage : SetBodyEnabledMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyEnabledMessage.BodyId with
        | (true, body) ->
            if setBodyEnabledMessage.Enabled
            then B2Bodies.b2Body_Enable body
            else B2Bodies.b2Body_Disable body
        | (false, _) -> ()

    // NOTE: https://box2d.org/documentation/md_simulation.html
    // "Caution: Generally you should not set the transform on bodies after creation. Box2D treats this as a teleport and may result in undesirable behavior and/or performance problems."
    // However, setting the body transform (center, rotation) is required for e.g. following mouse movement.
    // In that case, Erin Catto noted that kinematic bodies allows the solver to dribble the movement across the sub-steps, potentially yielding a smoother result, compared to static bodies.
    static member private setBodyCenter (setBodyCenterMessage : SetBodyCenterMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyCenterMessage.BodyId with
        | (true, body) ->
            let center = Box2dNetPhysicsEngine.toPhysicsV2 setBodyCenterMessage.Center
            let mutable transform = B2Bodies.b2Body_GetTransform body
            if transform.p <> center then
                B2Bodies.b2Body_SetTransform (body, center, transform.q) // teleport
                B2Bodies.b2Body_SetAwake (body, true) // force update collisions and create a body transform message in case sleeping
        | (false, _) -> ()

    static member private setBodyRotation (setBodyRotationMessage : SetBodyRotationMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyRotationMessage.BodyId with
        | (true, body) ->
            let rotation = Box2dNetPhysicsEngine.quatToRot setBodyRotationMessage.Rotation
            let mutable transform = B2Bodies.b2Body_GetTransform body
            if transform.q <> rotation then
                B2Bodies.b2Body_SetTransform (body, transform.p, rotation) // teleport
                B2Bodies.b2Body_SetAwake (body, true) // force update collisions and create a body transform message in case sleeping
        | (false, _) -> ()

    static member private setBodyLinearVelocity (setBodyLinearVelocityMessage : SetBodyLinearVelocityMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyLinearVelocityMessage.BodyId with
        | (true, body) -> B2Bodies.b2Body_SetLinearVelocity (body, Box2dNetPhysicsEngine.toPhysicsV2 setBodyLinearVelocityMessage.LinearVelocity) // NOTE: wakes body for non-zero velocity.
        | (false, _) -> ()

    static member private setBodyAngularVelocity (setBodyAngularVelocityMessage : SetBodyAngularVelocityMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyAngularVelocityMessage.BodyId with
        | (true, body) -> B2Bodies.b2Body_SetAngularVelocity (body, setBodyAngularVelocityMessage.AngularVelocity.Z) // NOTE: wakes body for non-zero velocity.
        | (false, _) -> ()

    static member private setBodyJointMotorEnabled (setBodyJointMotorEnabledMessage : SetBodyJointMotorEnabledMessage) physicsEngine =
        match physicsEngine.Joints.TryGetValue setBodyJointMotorEnabledMessage.BodyJointId with
        | (true, joint) ->
            match B2Joints.b2Joint_GetType joint with
            | B2JointType.b2_distanceJoint -> B2DistanceJoints.b2DistanceJoint_EnableMotor (joint, setBodyJointMotorEnabledMessage.MotorEnabled)
            | B2JointType.b2_prismaticJoint -> B2PrismaticJoints.b2PrismaticJoint_EnableMotor (joint, setBodyJointMotorEnabledMessage.MotorEnabled)
            | B2JointType.b2_revoluteJoint -> B2RevoluteJoints.b2RevoluteJoint_EnableMotor (joint, setBodyJointMotorEnabledMessage.MotorEnabled)
            | B2JointType.b2_wheelJoint -> B2WheelJoints.b2WheelJoint_EnableMotor (joint, setBodyJointMotorEnabledMessage.MotorEnabled)
            | _ -> ()
            if setBodyJointMotorEnabledMessage.MotorEnabled then
                B2Joints.b2Joint_WakeBodies joint
        | (false, _) -> ()

    static member private setBodyJointMotorSpeed (setBodyJointMotorSpeedMessage : SetBodyJointMotorSpeedMessage) physicsEngine =
        match physicsEngine.Joints.TryGetValue setBodyJointMotorSpeedMessage.BodyJointId with
        | (true, joint) ->
            match B2Joints.b2Joint_GetType joint with
            | B2JointType.b2_distanceJoint -> B2DistanceJoints.b2DistanceJoint_SetMotorSpeed (joint, setBodyJointMotorSpeedMessage.MotorSpeed)
            | B2JointType.b2_prismaticJoint -> B2PrismaticJoints.b2PrismaticJoint_SetMotorSpeed (joint, setBodyJointMotorSpeedMessage.MotorSpeed)
            | B2JointType.b2_revoluteJoint -> B2RevoluteJoints.b2RevoluteJoint_SetMotorSpeed (joint, setBodyJointMotorSpeedMessage.MotorSpeed)
            | B2JointType.b2_wheelJoint -> B2WheelJoints.b2WheelJoint_SetMotorSpeed (joint, setBodyJointMotorSpeedMessage.MotorSpeed)
            | _ -> ()
            if setBodyJointMotorSpeedMessage.MotorSpeed <> 0.0f then
                B2Joints.b2Joint_WakeBodies joint
        | (false, _) -> ()

    static member private setBodyJointTargetAngle (setBodyJointTargetAngleMessage : SetBodyJointTargetAngleMessage) physicsEngine =
        match physicsEngine.Joints.TryGetValue setBodyJointTargetAngleMessage.BodyJointId with
        | (true, joint) ->
            match B2Joints.b2Joint_GetType joint with
            | B2JointType.b2_revoluteJoint ->
                B2RevoluteJoints.b2RevoluteJoint_SetTargetAngle (joint, setBodyJointTargetAngleMessage.TargetAngle)
            | _ -> ()
            B2Joints.b2Joint_WakeBodies joint
        | (false, _) -> ()

    static member private applyBodyLinearImpulse (applyBodyLinearImpulseMessage : ApplyBodyLinearImpulseMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyLinearImpulseMessage.BodyId with
        | (true, body) ->
            if not (Single.IsNaN applyBodyLinearImpulseMessage.LinearImpulse.X) then
                match applyBodyLinearImpulseMessage.OriginWorldOpt with
                | Some originWorld ->
                    B2Bodies.b2Body_ApplyLinearImpulse
                        (body,
                         Box2dNetPhysicsEngine.toPhysicsV2 applyBodyLinearImpulseMessage.LinearImpulse,
                         Box2dNetPhysicsEngine.toPhysicsV2 originWorld,
                         true)
                | None ->
                    B2Bodies.b2Body_ApplyLinearImpulseToCenter
                        (body,
                         Box2dNetPhysicsEngine.toPhysicsV2 applyBodyLinearImpulseMessage.LinearImpulse,
                         true)
            else Log.warn "Ignoring NaN linear impulse."
        | (false, _) -> ()

    static member private applyBodyAngularImpulse (applyBodyAngularImpulseMessage : ApplyBodyAngularImpulseMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyAngularImpulseMessage.BodyId with
        | (true, body) ->
            if not (Single.IsNaN applyBodyAngularImpulseMessage.AngularImpulse.Z) then
                B2Bodies.b2Body_ApplyAngularImpulse (body, applyBodyAngularImpulseMessage.AngularImpulse.Z, true)
            else Log.warn "Ignoring NaN angular impulse."
        | (false, _) -> ()

    static member private applyBodyForce (applyBodyForceMessage : ApplyBodyForceMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyForceMessage.BodyId with
        | (true, body) ->
            if not (Single.IsNaN applyBodyForceMessage.Force.X) then
                match applyBodyForceMessage.OriginWorldOpt with
                | Some originWorld ->
                    B2Bodies.b2Body_ApplyForce  
                        (body,
                         Box2dNetPhysicsEngine.toPhysicsV2 applyBodyForceMessage.Force,
                         Box2dNetPhysicsEngine.toPhysicsV2 originWorld,
                         true)
                | None ->
                    B2Bodies.b2Body_ApplyForceToCenter (body, Box2dNetPhysicsEngine.toPhysicsV2 applyBodyForceMessage.Force, true)
            else Log.warn "Ignoring NaN body force."
        | (false, _) -> ()

    static member private applyBodyTorque (applyBodyTorqueMessage : ApplyBodyTorqueMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyTorqueMessage.BodyId with
        | (true, body) ->
            if not (Single.IsNaN applyBodyTorqueMessage.Torque.Z) then
                B2Bodies.b2Body_ApplyTorque (body, applyBodyTorqueMessage.Torque.Z, true)
            else Log.warn "Ignoring NaN body torque."
        | (false, _) -> ()

    static member private applyExplosion (applyExplosionMessage : ApplyExplosionMessage) physicsEngine =
        let mutable explosionDef = B2ExplosionDef ()
        explosionDef.position <- Box2dNetPhysicsEngine.toPhysicsV2 applyExplosionMessage.Center
        explosionDef.radius <- Box2dNetPhysicsEngine.toPhysics applyExplosionMessage.Radius
        explosionDef.falloff <- Box2dNetPhysicsEngine.toPhysics applyExplosionMessage.Falloff
        explosionDef.impulsePerLength <- Box2dNetPhysicsEngine.toPhysics applyExplosionMessage.Impulse
        explosionDef.maskBits <- applyExplosionMessage.CollisionMask
        B2Worlds.b2World_Explode (physicsEngine.PhysicsContextId, &explosionDef)
        
    // TODO: make characterGroundCastCallback store multiple contacts (fraction value minimal but around similar) and read them here
    static member private getCharacterGroundContactNormalOpt bodyId physicsEngine =
        match physicsEngine.Characters.TryGetValue bodyId with
        | (true, m) ->
            match m.OnGround with
            | ValueSome _ -> ValueSome m.GroundCastResult.normal
            | ValueNone -> ValueNone
        | (false, _) -> ValueNone
 
    static member private getBodyContactNormals bodyId physicsEngine =
        let body = physicsEngine.Bodies.[bodyId]
        let capacity = B2Bodies.b2Body_GetContactCapacity body
        let contacts =
            if capacity > 20
            then (Array.zeroCreate capacity).AsSpan ()
            else Span (NativeInterop.NativePtr.stackalloc<B2ContactData> capacity |> NativeInterop.NativePtr.toVoidPtr, capacity)
        let contacts = contacts.Slice (0, B2Bodies.b2Body_GetContactData (body, contacts, capacity))
        let characterGroundContactNormal = Box2dNetPhysicsEngine.getCharacterGroundContactNormalOpt bodyId physicsEngine
        let normals = Array.zeroCreate (contacts.Length + ValueOption.count characterGroundContactNormal)
        for i in 0 .. dec contacts.Length do
            let contact = &contacts.[i]
            let normal =
                if B2Shapes.b2Shape_GetBody contact.shapeIdA = body
                then -contact.manifold.normal // normal points from shapeIdA to shapeIdB, so invert if body is shapeA
                else contact.manifold.normal
            normals.[i] <- Vector3 (normal.X, normal.Y, 0.0f)
        match characterGroundContactNormal with
        | ValueSome normal ->
            normals.[contacts.Length] <- Vector3 (normal.X, normal.Y, 0.0f) // points from ground to body
        | ValueNone -> ()
        normals

    static member private getBodyGroundDirection bodyId physicsEngine =
        match physicsEngine.Bodies.TryGetValue bodyId with
        | (true, body) ->
            let gravity =
                match physicsEngine.BodyGravityOverrides.TryGetValue bodyId with
                | (true, gravityOverride) -> gravityOverride
                | (false, _) -> (physicsEngine :> PhysicsEngine).Gravity * B2Bodies.b2Body_GetGravityScale body
            if gravity <> v3Zero
            then gravity.Normalized // ground relative to gravity
            else v3Down.Transform (Box2dNetPhysicsEngine.rotToQuat (B2Bodies.b2Body_GetRotation body)) // ground relative to body rotation
        | (false, _) -> (physicsEngine :> PhysicsEngine).Gravity.Normalized

    static member private getBodyToGroundContactNormals groundDirection bodyId physicsEngine =
        match Box2dNetPhysicsEngine.getCharacterGroundContactNormalOpt bodyId physicsEngine with
        | ValueSome normal ->
            [| v3 normal.X normal.Y 0.0f |]
        | ValueNone ->
            assert (Constants.Physics.GroundAngleMax < MathF.PI_OVER_2) // any larger would allow wall jumping without pushing back against the wall
            let up = -groundDirection
            Box2dNetPhysicsEngine.getBodyContactNormals bodyId physicsEngine
            |> Array.filter (fun contactNormal ->
                let projectionToUp = contactNormal.Dot up
                // contactNormal and upDirection are normalized. -1 <= dot product <= 1. floating point imprecision is not a concern as NaN <= x is always false.
                let theta = acos projectionToUp
                theta <= Constants.Physics.GroundAngleMax)

    static member private getBodyToGroundContactNormalOpt bodyId physicsEngine =
        let groundDirection = Box2dNetPhysicsEngine.getBodyGroundDirection bodyId physicsEngine 
        match Box2dNetPhysicsEngine.getBodyToGroundContactNormals groundDirection bodyId physicsEngine with
        | [||] -> None
        | groundNormals -> groundNormals |> Array.maxBy (fun normal -> normal.Dot groundDirection) |> Some

    static member private getBodyGrounded groundDirection bodyId physicsEngine =
        match physicsEngine.Characters.TryGetValue bodyId with
        | (true, m) -> ValueOption.isSome m.OnGround
        | (false, _) -> Array.notEmpty (Box2dNetPhysicsEngine.getBodyToGroundContactNormals groundDirection bodyId physicsEngine)

    static member private jumpBody (jumpBodyMessage : JumpBodyMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue jumpBodyMessage.BodyId with
        | (true, body) ->
            let groundDirection = Box2dNetPhysicsEngine.getBodyGroundDirection jumpBodyMessage.BodyId physicsEngine
            if jumpBodyMessage.CanJumpInAir || Box2dNetPhysicsEngine.getBodyGrounded groundDirection jumpBodyMessage.BodyId physicsEngine then
                B2Bodies.b2Body_SetLinearVelocity // NOTE: wakes body for non-zero linear velocity.
                    (body,
                     B2Bodies.b2Body_GetLinearVelocity body +
                     Box2dNetPhysicsEngine.toPhysicsV2 (groundDirection * -jumpBodyMessage.JumpSpeed))
                match physicsEngine.Characters.TryGetValue jumpBodyMessage.BodyId with
                | (true, m) -> m.OnGround <- ValueNone
                | (false, _) -> ()
        | (false, _) -> ()

    static member private updateFluidEmitterMessage (updateFluidEmitterMessage : UpdateFluidEmitterMessage) physicsEngine =
        let id = updateFluidEmitterMessage.FluidEmitterId
        match physicsEngine.FluidEmitters.TryGetValue id with
        | (true, emitter) ->
            match updateFluidEmitterMessage.FluidEmitterDescriptor with
            | FluidEmitterDescriptorBox2dNet descriptor ->
                physicsEngine.FluidEmitters.[id] <- Box2dNetFluidEmitter.updateDescriptor descriptor emitter
            | FluidEmitterDescriptorAether _ | FluidEmitterDescriptorJolt -> () // no support
        | (false, _) -> ()

    static member private emitFluidParticlesMessage (emitFluidParticlesMessage : EmitFluidParticlesMessage) physicsEngine =
        let id = emitFluidParticlesMessage.FluidEmitterId
        match physicsEngine.FluidEmitters.TryGetValue id with
        | (true, emitter) -> Box2dNetFluidEmitter.addParticles (emitFluidParticlesMessage.FluidParticles.GetEnumerator ()) emitter
        | (false, _) -> ()

    static member private setFluidParticlesMessage (setFluidParticlesMessage : SetFluidParticlesMessage) physicsEngine =
        let id = setFluidParticlesMessage.FluidEmitterId
        match physicsEngine.FluidEmitters.TryGetValue id with
        | (true, emitter) -> Box2dNetFluidEmitter.setParticles setFluidParticlesMessage.FluidParticles emitter
        | (false, _) -> ()

    static let chooseFluidParticleTempHolder = [|Unchecked.defaultof<FluidParticle>|]
    static member private chooseFluidParticlesMessage (chooseFluidParticlesMessage : ChooseFluidParticlesMessage) physicsEngine =
        let id = chooseFluidParticlesMessage.FluidEmitterId
        match physicsEngine.FluidEmitters.TryGetValue id with
        | (true, emitter) ->
            Box2dNetFluidEmitter.collectParticles (fun p ->
                match chooseFluidParticlesMessage.FluidParticleDiscriminator p with
                | ValueSome fluidParticle ->
                    chooseFluidParticleTempHolder.[0] <- fluidParticle
                    chooseFluidParticleTempHolder
                | ValueNone -> [||]) emitter
        | (false, _) -> ()

    static member private clearFluidParticlesMessage (id : FluidEmitterId) physicsEngine =
        match physicsEngine.FluidEmitters.TryGetValue id with
        | (true, emitter) -> Box2dNetFluidEmitter.clearParticles emitter
        | (false, _) -> ()

    static member private setGravityMessage gravity physicsEngine =
        B2Worlds.b2World_SetGravity (physicsEngine.PhysicsContextId, Box2dNetPhysicsEngine.toPhysicsV2 gravity)

        // wake all bodies
        B2Worlds.b2World_EnableSleeping (physicsEngine.PhysicsContextId, false)
        B2Worlds.b2World_EnableSleeping (physicsEngine.PhysicsContextId, true)

    static member private handlePhysicsMessage physicsEngine physicsMessage =
        match physicsMessage with
        | CreateBodyMessage createBodyMessage -> Box2dNetPhysicsEngine.createBody createBodyMessage physicsEngine
        | CreateBodiesMessage createBodiesMessage -> Box2dNetPhysicsEngine.createBodies createBodiesMessage physicsEngine
        | DestroyBodyMessage destroyBodyMessage -> Box2dNetPhysicsEngine.destroyBody destroyBodyMessage physicsEngine
        | DestroyBodiesMessage destroyBodiesMessage -> Box2dNetPhysicsEngine.destroyBodies destroyBodiesMessage physicsEngine
        | CreateBodyJointMessage createBodyJointMessage -> Box2dNetPhysicsEngine.createBodyJoint createBodyJointMessage physicsEngine
        | DestroyBodyJointMessage destroyBodyJointMessage -> Box2dNetPhysicsEngine.destroyBodyJoint destroyBodyJointMessage physicsEngine
        | CreateFluidEmitterMessage createFluidEmitterMessage -> Box2dNetPhysicsEngine.createFluidEmitter createFluidEmitterMessage physicsEngine
        | DestroyFluidEmitterMessage destroyFluidEmitterMessage -> Box2dNetPhysicsEngine.destroyFluidEmitter destroyFluidEmitterMessage physicsEngine
        | SetBodyEnabledMessage setBodyEnabledMessage -> Box2dNetPhysicsEngine.setBodyEnabled setBodyEnabledMessage physicsEngine
        | SetBodyCenterMessage setBodyCenterMessage -> Box2dNetPhysicsEngine.setBodyCenter setBodyCenterMessage physicsEngine
        | SetBodyRotationMessage setBodyRotationMessage -> Box2dNetPhysicsEngine.setBodyRotation setBodyRotationMessage physicsEngine
        | SetBodyLinearVelocityMessage setBodyLinearVelocityMessage -> Box2dNetPhysicsEngine.setBodyLinearVelocity setBodyLinearVelocityMessage physicsEngine
        | SetBodyAngularVelocityMessage setBodyAngularVelocityMessage -> Box2dNetPhysicsEngine.setBodyAngularVelocity setBodyAngularVelocityMessage physicsEngine
        | SetBodyVehicleForwardInputMessage _ -> () // no vehicle controller support
        | SetBodyVehicleRightInputMessage _ -> () // no vehicle controller support
        | SetBodyVehicleBrakeInputMessage _ -> () // no vehicle controller support
        | SetBodyVehicleHandBrakeInputMessage _ -> () // no vehicle controller support
        | SetBodyJointMotorEnabledMessage setBodyJointMotorEnabledMessage -> Box2dNetPhysicsEngine.setBodyJointMotorEnabled setBodyJointMotorEnabledMessage physicsEngine
        | SetBodyJointMotorSpeedMessage setBodyJointMotorSpeedMessage -> Box2dNetPhysicsEngine.setBodyJointMotorSpeed setBodyJointMotorSpeedMessage physicsEngine
        | SetBodyJointTargetAngleMessage setBodyJointTargetAngleMessage -> Box2dNetPhysicsEngine.setBodyJointTargetAngle setBodyJointTargetAngleMessage physicsEngine
        | ApplyBodyLinearImpulseMessage applyBodyLinearImpulseMessage -> Box2dNetPhysicsEngine.applyBodyLinearImpulse applyBodyLinearImpulseMessage physicsEngine
        | ApplyBodyAngularImpulseMessage applyBodyAngularImpulseMessage -> Box2dNetPhysicsEngine.applyBodyAngularImpulse applyBodyAngularImpulseMessage physicsEngine
        | ApplyBodyForceMessage applyBodyForceMessage -> Box2dNetPhysicsEngine.applyBodyForce applyBodyForceMessage physicsEngine
        | ApplyBodyTorqueMessage applyBodyTorqueMessage -> Box2dNetPhysicsEngine.applyBodyTorque applyBodyTorqueMessage physicsEngine
        | ApplyExplosionMessage applyExplosionMessage -> Box2dNetPhysicsEngine.applyExplosion applyExplosionMessage physicsEngine
        | JumpBodyMessage jumpBodyMessage -> Box2dNetPhysicsEngine.jumpBody jumpBodyMessage physicsEngine
        | UpdateFluidEmitterMessage updateFluidEmitterMessage -> Box2dNetPhysicsEngine.updateFluidEmitterMessage updateFluidEmitterMessage physicsEngine
        | EmitFluidParticlesMessage emitFluidParticlesMessage -> Box2dNetPhysicsEngine.emitFluidParticlesMessage emitFluidParticlesMessage physicsEngine
        | SetFluidParticlesMessage setFluidParticlesMessage -> Box2dNetPhysicsEngine.setFluidParticlesMessage setFluidParticlesMessage physicsEngine
        | ChooseFluidParticlesMessage chooseFluidParticlesMessage -> Box2dNetPhysicsEngine.chooseFluidParticlesMessage chooseFluidParticlesMessage physicsEngine
        | ClearFluidParticlesMessage id -> Box2dNetPhysicsEngine.clearFluidParticlesMessage id physicsEngine
        | SetGravityMessage gravity -> Box2dNetPhysicsEngine.setGravityMessage gravity physicsEngine

    // NOTE: from Box2D documentation https://box2d.org/documentation/md_simulation.html
    // update transforms - "Note that continuous collision does not generate events. Instead they are generated the next time step. However, continuous collision will issue a b2PreSolveFcn callback."
    // we want penetration messages to be recorded on the same time step as penetration instead of the next, so we record it in the b2PreSolveFcn callback.
    static let preSolveCallback =
        b2PreSolveFcn (fun shapeIdA shapeIdB manifold context -> // can be called from multiple threads at once - write to concurrent collections only
            let contactsTracker = context :?> Box2dNetPhysicsEngineContactsTracker
            if not (contactsTracker.ExistingContacts.Contains (shapeIdA, shapeIdB)) then
                let bodyShapeA = B2Shapes.b2Shape_GetUserData shapeIdA :?> BodyShapeIndex
                let bodyShapeB = B2Shapes.b2Shape_GetUserData shapeIdB :?> BodyShapeIndex
                let normal = Vector3 (manifold.normal.X, manifold.normal.Y, 0.0f)
                contactsTracker.NewContacts.TryAdd
                    (struct (shapeIdA, shapeIdB),
                     { BodyShapeSource = bodyShapeA; BodyShapeTarget = bodyShapeB; Normal = normal })
                |> ignore
            true)

    static member private makePhysicsContext gravity contactsTracker =
        let mutable worldDef = B2Types.b2DefaultWorldDef ()
        worldDef.gravity <- gravity
        let world = B2Worlds.b2CreateWorld &worldDef
        B2Worlds.b2World_SetPreSolveCallback (world, preSolveCallback, (contactsTracker : Box2dNetPhysicsEngineContactsTracker))
        world

    /// Make a physics engine.
    static member make gravity =
        let contactsTracker =
            { NewContacts = ConcurrentDictionary ()
              ExistingContacts = HashSet () }
        { PhysicsContextId = Box2dNetPhysicsEngine.makePhysicsContext (Box2dNetPhysicsEngine.toPhysicsV2 gravity) contactsTracker
          Bodies = Dictionary HashIdentity.Structural
          BodyGravityOverrides = Dictionary HashIdentity.Structural
          Characters = Dictionary HashIdentity.Structural
          Joints = Dictionary HashIdentity.Structural
          BreakableJoints = Dictionary HashIdentity.Structural
          CreateBodyJointMessages = Dictionary HashIdentity.Structural
          FluidEmitters = Dictionary<FluidEmitterId, Box2dNetFluidEmitter> HashIdentity.Structural
          ContactsTracker = contactsTracker
          CharacterCollisionContext =
            { Self = B2Ids.b2_nullShapeId
              SoftCollisionPushLimits = Dictionary HashIdentity.Structural
              PlaneResults = List 8 }
          IntegrationMessages = List () } :> PhysicsEngine
          
    static let characterGroundCastCallback =
        b2CastResultFcn (fun shapeId point normal fraction context ->
            let m = context :?> Box2dNetCharacterSimulation
            if B2Ids.B2_ID_EQUALS (shapeId, m.CapsuleShape) then -1.0f else
            let result = m.GroundCastResult
            result.hit <- true
            result.shapeId <- shapeId
            result.point <- point
            result.normal <- normal
            result.fraction <- fraction
            fraction)

    static let characterCollisionCallback =
        b2PlaneResultFcn (fun shapeId planeResult context ->
            assert planeResult.hit
            let context = context :?> Box2dNetCharacterCollisionContext
            if not (B2Ids.B2_ID_EQUALS (context.Self, shapeId)) && context.PlaneResults.Count < context.PlaneResults.Capacity then
                assert B2MathFunction.b2IsValidPlane planeResult.plane
                let plane =
                    match context.SoftCollisionPushLimits.TryGetValue (B2Shapes.b2Shape_GetBody shapeId) with
                    | (true, pushLimit) -> B2CollisionPlane (planeResult.plane, pushLimit, 0.0f, false) // soft collision - don't clip velocity to plane, multi-frame position correction via push limit
                    | (false, _) -> B2CollisionPlane (planeResult.plane, Single.MaxValue, 0.0f, true) // hard collision - clip velocity to plane, immediate position correction with MaxValue push limit
                context.PlaneResults.Add plane
            true)

    // https://github.com/ikpil/Box2D.NET/blob/1881d86d07a9f1174a199c6c616e19379056bf10/src/Box2D.NET.Samples/Samples/Characters/Mover.cs#L280-L460
    static member private solveCharacter (m : Box2dNetCharacterSimulation) timeStep world collisionContext =
        // mover overlap filter, should include other movers
        let collideFilter = B2QueryFilter (m.CollisionCategory, m.CollisionMask)
        // movers shouldn't sweep against other movers to allow for soft collision
        let castFilter = B2QueryFilter (m.CollisionCategory, m.CastMask)
        
        // initialize gravity
        let gravity = m.GravityScale * B2Worlds.b2World_GetGravity world + m.GravityOverride
        let gravityDirection = B2MathFunction.b2Normalize gravity
        if ValueOption.isSome m.OnGround then // when on ground, stabilize pogo spring by clearing velocity in the direction of gravity
            let movementAxis = B2Vec2 (-gravityDirection.Y, gravityDirection.X) // perpendicular to gravity
            m.Velocity <- B2MathFunction.b2Dot (m.Velocity, movementAxis) * movementAxis // project m.Velocity onto movementAxis
        m.Velocity <- m.Velocity + timeStep * gravity

        // pogo cast downward from bottom point of capsule
        let rayLength = m.PogoRestLength + m.Capsule.radius
        m.PogoOrigin <- B2MathFunction.b2TransformPoint (&m.Transform, m.Capsule.center1)
        let mutable proxy = m.PogoProxy
        for i in 0 .. dec proxy.count do
            proxy.points.[i] <- proxy.points.[i] + m.PogoOrigin // apply origin to proxy
        let translation = (rayLength - proxy.radius) * gravityDirection
        let castResult = m.GroundCastResult
        castResult.hit <- false
        B2Worlds.b2World_CastShape (world, &proxy, translation, castFilter, characterGroundCastCallback, m)
        |> ignore<B2TreeStats>

        // avoid snapping to ground if still going opposite of gravity with magnitude at least 0.01
        m.OnGround <-
            if castResult.hit && (ValueOption.isSome m.OnGround ||
                let gravityProjection = B2MathFunction.b2Dot (m.Velocity, gravityDirection)
                gravityProjection >= -0.01f)
            then ValueSome castResult.shapeId
            else ValueNone

        // solve pogo state and apply pogo gravity to ground contact
        if not castResult.hit then
            m.PogoVelocity <- 0.0f
            m.PogoDelta <- translation
        else
            let pogoCurrentLength = castResult.fraction * rayLength - m.Capsule.radius
            let offset = pogoCurrentLength - m.PogoRestLength
            m.PogoVelocity <- B2MathFunction.b2SpringDamper (m.PogoHertz, m.PogoDampingRatio, offset, m.PogoVelocity, timeStep)
            m.PogoDelta <- castResult.fraction * translation
            B2Bodies.b2Body_ApplyForce (B2Shapes.b2Shape_GetBody castResult.shapeId, m.Mass * gravity, castResult.point, true)

        // solve collision planes for projected new position
        // TODO: is it possible to project external forces for dynamic characters?
        let target = m.Transform.p + timeStep * m.Velocity + timeStep * m.PogoVelocity * -gravityDirection
        let toleranceSquared = 0.01f * 0.01f
        let mutable i = 0
        let planeResults = collisionContext.PlaneResults
        while i < 5 do
            planeResults.Clear ()
            let mutable mover =
                B2Capsule
                    (B2MathFunction.b2TransformPoint (&m.Transform, m.Capsule.center1),
                     B2MathFunction.b2TransformPoint (&m.Transform, m.Capsule.center2),
                     m.Capsule.radius)
            collisionContext.Self <- m.CapsuleShape
            B2Worlds.b2World_CollideMover (world, &mover, collideFilter, characterCollisionCallback, (collisionContext : Box2dNetCharacterCollisionContext))
            let result = B2Movers.b2SolvePlanes (target - m.Transform.p, System.Runtime.InteropServices.CollectionsMarshal.AsSpan planeResults, planeResults.Count)
            let fraction = B2Worlds.b2World_CastMover (world, &mover, result.translation, castFilter)
            let delta = fraction * result.translation
            m.Transform.p <- m.Transform.p + delta
            if B2MathFunction.b2LengthSquared delta < toleranceSquared
            then i <- 5
            else i <- i + 1

        // clip velocity against collision planes
        m.Velocity <- B2Movers.b2ClipVector (m.Velocity, System.Runtime.InteropServices.CollectionsMarshal.AsSpan planeResults, planeResults.Count)

    static let renderCallback =
        b2OverlapResultFcn (fun shape context ->
            let renderContext = context :?> Box2dNetPhysicsEngineRenderContext

            // get body and transform
            let body = B2Shapes.b2Shape_GetBody shape
            let mutable transform = B2Bodies.b2Body_GetTransform body

            // compute color consistent with JoltSharp which defaults to MotionTypeColor: https://github.com/amerkoleci/JoltPhysicsSharp/blob/fbc0511c987043a16b6f985ae00633285ee56cb9/src/JoltPhysicsSharp/DrawSettings.cs#L33
            // which is defined here: https://github.com/amerkoleci/JoltPhysicsSharp/blob/fbc0511c987043a16b6f985ae00633285ee56cb9/src/JoltPhysicsSharp/ShapeColor.cs#L20
            let color =
                match B2Bodies.b2Body_GetType body with
                | B2BodyType.b2_dynamicBody -> // dynamic = random color per instance
                    match B2Bodies.b2Body_GetUserData body with null -> B2Bodies.b2Body_GetName body :> obj | d -> d
                    |> hash |> uint |> colorPacked |> _.WithA(1f) // use the Nu BodyIndex because physics engine bodies are recreated on property assignment
                | B2BodyType.b2_kinematicBody -> // keyframed
                    Color.Green
                | _ -> // static or anything else
                    Color.Gray

            // render shape
            match B2Shapes.b2Shape_GetType shape with
            | B2ShapeType.b2_circleShape ->
                let circle = B2Shapes.b2Shape_GetCircle shape
                let position = B2MathFunction.b2TransformPoint (&transform, circle.center) |> Box2dNetPhysicsEngine.toPixelV2
                let radius = Box2dNetPhysicsEngine.toPixel circle.radius
                renderContext.DrawCircle (position, radius, color)
            | B2ShapeType.b2_capsuleShape ->
                let capsule = B2Shapes.b2Shape_GetCapsule shape
                let center1 = B2MathFunction.b2TransformPoint (&transform, capsule.center1) |> Box2dNetPhysicsEngine.toPixelV2
                let center2 = B2MathFunction.b2TransformPoint (&transform, capsule.center2) |> Box2dNetPhysicsEngine.toPixelV2
                let radius = Box2dNetPhysicsEngine.toPixel capsule.radius
                let direction = center2 - center1
                let perpendicular = Vector2(-direction.Y, direction.X).Normalized * radius
                renderContext.DrawCircle (center1, radius, color)
                renderContext.DrawCircle (center2, radius, color)
                renderContext.DrawLine (center1 + perpendicular, center2 + perpendicular, color)
                renderContext.DrawLine (center1 - perpendicular, center2 - perpendicular, color)
            | B2ShapeType.b2_segmentShape ->
                let segment = B2Shapes.b2Shape_GetSegment shape
                let start = B2MathFunction.b2TransformPoint (&transform, segment.point1) |> Box2dNetPhysicsEngine.toPixelV2
                let stop = B2MathFunction.b2TransformPoint (&transform, segment.point2) |> Box2dNetPhysicsEngine.toPixelV2
                renderContext.DrawLine (start, stop, color)
            | B2ShapeType.b2_polygonShape ->
                let polygon = B2Shapes.b2Shape_GetPolygon shape
                if polygon.radius = 0.0f then
                    for i in 0 .. dec polygon.count do
                        let start = B2MathFunction.b2TransformPoint (&transform, polygon.vertices.[i]) |> Box2dNetPhysicsEngine.toPixelV2
                        let stop = B2MathFunction.b2TransformPoint (&transform, polygon.vertices.[if i < dec polygon.count then inc i else 0]) |> Box2dNetPhysicsEngine.toPixelV2
                        renderContext.DrawLine (start, stop, color)
                else
                    let radius = Box2dNetPhysicsEngine.toPixel polygon.radius
                    for i in 0 .. dec polygon.count do
                        let start = B2MathFunction.b2TransformPoint (&transform, polygon.vertices.[i]) |> Box2dNetPhysicsEngine.toPixelV2
                        let stop = B2MathFunction.b2TransformPoint (&transform, polygon.vertices.[if i < dec polygon.count then inc i else 0]) |> Box2dNetPhysicsEngine.toPixelV2
                        let perpendicular = B2MathFunction.b2RotateVector (transform.q, polygon.normals[i]) * radius
                        let perpendicular = v2 perpendicular.X perpendicular.Y
                        renderContext.DrawCircle (start, radius, color)
                        renderContext.DrawLine (start + perpendicular, stop + perpendicular, color)
            | B2ShapeType.b2_chainSegmentShape ->
                let segment = (B2Shapes.b2Shape_GetChainSegment shape).segment
                let start = B2MathFunction.b2TransformPoint (&transform, segment.point1) |> Box2dNetPhysicsEngine.toPixelV2
                let stop = B2MathFunction.b2TransformPoint (&transform, segment.point2) |> Box2dNetPhysicsEngine.toPixelV2
                renderContext.DrawLine (start, stop, color)
            | _ -> ()

            // continue querying
            true)

    interface PhysicsEngine with

        member physicsEngine.GravityDefault =
            let gravityDefault = B2Vec2 (Constants.Physics.GravityDefault.X, Constants.Physics.GravityDefault.Y)
            Box2dNetPhysicsEngine.toPixelV3 gravityDefault

        member physicsEngine.Gravity =
            Box2dNetPhysicsEngine.toPixelV3 (B2Worlds.b2World_GetGravity physicsEngine.PhysicsContextId)

        member physicsEngine.GetBodyExists bodyId =
            physicsEngine.Bodies.ContainsKey bodyId

        member physicsEngine.GetBodyContactNormals bodyId =
            Box2dNetPhysicsEngine.getBodyContactNormals bodyId physicsEngine

        member physicsEngine.GetBodyLinearVelocity bodyId =
            let body = physicsEngine.Bodies.[bodyId]
            Box2dNetPhysicsEngine.toPixelV3 (B2Bodies.b2Body_GetLinearVelocity body)

        member physicsEngine.GetBodyAngularVelocity bodyId =
            let body = physicsEngine.Bodies.[bodyId]
            v3 0.0f 0.0f (B2Bodies.b2Body_GetAngularVelocity body)

        member physicsEngine.GetBodyToGroundContactNormals bodyId =
            let groundDirection = Box2dNetPhysicsEngine.getBodyGroundDirection bodyId physicsEngine
            Box2dNetPhysicsEngine.getBodyToGroundContactNormals groundDirection bodyId physicsEngine

        member physicsEngine.GetBodyToGroundContactNormalOpt bodyId =
            Box2dNetPhysicsEngine.getBodyToGroundContactNormalOpt bodyId physicsEngine

        member physicsEngine.GetBodyToGroundContactTangentOpt bodyId =
            match (physicsEngine :> PhysicsEngine).GetBodyToGroundContactNormalOpt bodyId with
            | Some normal -> Some (Vector3 (normal.Y, -normal.X, 0.0f))
            | None -> None

        member physicsEngine.GetBodyGrounded bodyId =
            let groundDirection = Box2dNetPhysicsEngine.getBodyGroundDirection bodyId physicsEngine
            Box2dNetPhysicsEngine.getBodyGrounded groundDirection bodyId physicsEngine

        member physicsEngine.GetBodySensor bodyId =
            let body = physicsEngine.Bodies.[bodyId]
            let shapes = Span (NativeInterop.NativePtr.stackalloc<B2BodyId> 1 |> NativeInterop.NativePtr.toVoidPtr, 1)
            B2Bodies.b2Body_GetShapes (body, shapes, 1) = 1 && B2Shapes.b2Shape_IsSensor shapes.[0]

        member physicsEngine.GetBodyWheelSpeedAtClutch _ =
            0.0f // no vehicle controller support

        member physicsEngine.GetBodyWheelModelMatrix (_, _, _, _) =
            m4Identity // no vehicle controller support

        member physicsEngine.GetBodyWheelAngularVelocity (_, _) =
            0.0f // no vehicle controller support

        member physicsEngine.GetBodyJointExists bodyJointId =
            physicsEngine.Joints.ContainsKey bodyJointId

        member physicsEngine.GetBodyJointMotorSpeed bodyJointId =
            match physicsEngine.Joints.TryGetValue bodyJointId with
            | (true, joint) ->
                match B2Joints.b2Joint_GetType joint with
                | B2JointType.b2_distanceJoint -> B2DistanceJoints.b2DistanceJoint_GetMotorSpeed joint
                | B2JointType.b2_prismaticJoint -> B2PrismaticJoints.b2PrismaticJoint_GetMotorSpeed joint
                | B2JointType.b2_revoluteJoint -> B2RevoluteJoints.b2RevoluteJoint_GetMotorSpeed joint
                | B2JointType.b2_wheelJoint -> B2WheelJoints.b2WheelJoint_GetMotorSpeed joint
                | _ -> 0.0f
            | (false, _) -> 0.0f

        member physicsEngine.GetBodyJointTargetAngle bodyJointId =
            match physicsEngine.Joints.TryGetValue bodyJointId with
            | (true, joint) ->
                match B2Joints.b2Joint_GetType joint with
                | B2JointType.b2_revoluteJoint -> B2RevoluteJoints.b2RevoluteJoint_GetTargetAngle joint
                | _ -> 0.0f
            | (false, _) -> 0.0f

        member physicsEngine.RayCast (ray, rayCategory, collisionMask, closestOnly) =
            let origin = Box2dNetPhysicsEngine.toPhysicsV2 ray.Origin
            let translation = Box2dNetPhysicsEngine.toPhysicsV2 ray.Direction
            let filter = B2QueryFilter (rayCategory, collisionMask)
            if closestOnly then
                let result =
                    B2Worlds.b2World_CastRayClosest (physicsEngine.PhysicsContextId, origin, translation, filter)
                if result.hit then
                    BodyIntersection.make
                        (B2Shapes.b2Shape_GetUserData result.shapeId :?> BodyShapeIndex)
                        result.fraction
                        (Box2dNetPhysicsEngine.toPixelV3 result.point)
                        (v3 result.normal.X result.normal.Y 0.0f) |> Array.singleton
                else Array.empty
            else
                let results = PriorityQueue ()
                let callback =
                    b2CastResultFcn (fun shape point normal fraction _ ->
                        results.Enqueue
                            (BodyIntersection.make
                                (B2Shapes.b2Shape_GetUserData shape :?> BodyShapeIndex)
                                fraction
                                (Box2dNetPhysicsEngine.toPixelV3 point)
                                (v3 normal.X normal.Y 0.0f),
                             fraction)
                        1.0f)
                B2Worlds.b2World_CastRay (physicsEngine.PhysicsContextId, origin, translation, filter, callback, null) |> ignore<B2TreeStats>
                Array.init results.Count (fun _ -> results.Dequeue ())

        member physicsEngine.ShapeCast (shape, transformOpt, ray, shapeCategory, collisionMask, closestOnly) =
            let mutable proxy = Unchecked.defaultof<_>
            if tryCreateShapeProxy &proxy shape transformOpt ray.Origin then
                let filter = B2QueryFilter (shapeCategory, collisionMask)
                let results = PriorityQueue ()
                let callback =
                    b2CastResultFcn (fun shape point normal fraction _ ->
                        results.Enqueue
                            (BodyIntersection.make
                                (B2Shapes.b2Shape_GetUserData shape :?> BodyShapeIndex)
                                fraction
                                (Box2dNetPhysicsEngine.toPixelV3 point)
                                (v3 normal.X normal.Y 0.0f),
                             fraction)
                        if closestOnly then fraction else 1.0f)
                B2Worlds.b2World_CastShape
                    (physicsEngine.PhysicsContextId,
                     &proxy,
                     Box2dNetPhysicsEngine.toPhysicsV2 ray.Direction,
                     filter,
                     callback,
                     null) |> ignore<B2TreeStats>
                if closestOnly then
                    match results.TryDequeue () with
                    | (true, intersection, _) -> [|intersection|]
                    | (false, _, _) -> Array.empty
                else Array.init results.Count (fun _ -> results.Dequeue ())
            else Array.empty

        member physicsEngine.HandleMessage physicsMessage =
            Box2dNetPhysicsEngine.handlePhysicsMessage physicsEngine physicsMessage

        member physicsEngine.TryIntegrate gameDelta =

            // constrain step time
            let stepTime = gameDelta.SecondsF
            let stepTime =
                if stepTime > 0.0f && stepTime < 0.001f then 0.001f
                elif stepTime > 0.1f then 0.1f
                else stepTime

            // integrate only when time has passed
            if stepTime > 0.0f then

                // apply body-specific gravity for awake bodies
                for KeyValue (bodyId, gravityOverride) in physicsEngine.BodyGravityOverrides do
                    let body = physicsEngine.Bodies.[bodyId]
                    if B2Bodies.b2Body_IsAwake body then
                        let gravity = Box2dNetPhysicsEngine.toPhysicsV2 gravityOverride
                        B2Bodies.b2Body_SetLinearVelocity (body, B2Bodies.b2Body_GetLinearVelocity body + gravity * stepTime) // NOTE: wakes body when awake not checked.

                for emitter in physicsEngine.FluidEmitters.Values do Box2dNetFluidEmitter.preStep stepTime emitter

                // step the world
                B2Worlds.b2World_Step (physicsEngine.PhysicsContextId, stepTime, Constants.Physics.Collision2dSteps)

                // collect fluid emitter results
                for KeyValue (emitterId, emitter) in physicsEngine.FluidEmitters do
                    let struct (particles, removedParticles) = Box2dNetFluidEmitter.postStep emitter
                    physicsEngine.IntegrationMessages.Add
                        (FluidEmitterMessage
                            { FluidEmitterId = emitterId
                              FluidParticles = particles
                              OutOfBoundsParticles = removedParticles
                              FluidCollisions = SArray.empty })
                
                // collect joint breaks
                for KeyValue (jointId, breakableJoint) in physicsEngine.BreakableJoints do
                    let joint = physicsEngine.Joints.[jointId]
                    let force = B2Joints.b2Joint_GetConstraintForce joint
                    let forceSquared = B2MathFunction.b2LengthSquared force
                    if forceSquared > breakableJoint.BreakingPointSquared then
                        physicsEngine.IntegrationMessages.Add
                            (BodyJointBreakMessage
                                { BodyJointId = B2Joints.b2Joint_GetUserData joint :?> BodyJointId
                                  BreakingPoint = Box2dNetPhysicsEngine.toPixel breakableJoint.BreakingPoint
                                  BreakingOverflow = Box2dNetPhysicsEngine.toPixel (sqrt forceSquared - breakableJoint.BreakingPoint) })
                        Box2dNetPhysicsEngine.destroyBodyJointInternal jointId physicsEngine

                // collect penetrations for non-sensors that aren't ground penetrations by characters
                let contacts = B2Worlds.b2World_GetContactEvents physicsEngine.PhysicsContextId
                if Constants.Physics.Collision2dFrameCompensation then

                    // collect penetrations for non-sensors from preSolveCallback which is called on same time step as penetration unlike contact events which exist one step later
                    for KeyValue (shapeIds, penetration) in physicsEngine.ContactsTracker.NewContacts do
                        physicsEngine.IntegrationMessages.Add (BodyPenetrationMessage { BodyShapeSource = penetration.BodyShapeSource; BodyShapeTarget = penetration.BodyShapeTarget; Normal = penetration.Normal })
                        physicsEngine.IntegrationMessages.Add (BodyPenetrationMessage { BodyShapeSource = penetration.BodyShapeTarget; BodyShapeTarget = penetration.BodyShapeSource; Normal = -penetration.Normal })
                        physicsEngine.ContactsTracker.ExistingContacts.Add shapeIds |> ignore
                    physicsEngine.ContactsTracker.NewContacts.Clear ()

                else

                    // collect penetrations for non-sensors from begin contact events, which are performant but one time step late compared to the actual penetration
                    for i in 0 .. dec contacts.beginCount do
                        let penetration = &contacts.beginEvents.[i]
                        let bodyShapeA = B2Shapes.b2Shape_GetUserData penetration.shapeIdA :?> BodyShapeIndex
                        let bodyShapeB = B2Shapes.b2Shape_GetUserData penetration.shapeIdB :?> BodyShapeIndex
                        let normal = v3 penetration.manifold.normal.X penetration.manifold.normal.Y 0.0f
                        physicsEngine.IntegrationMessages.Add (BodyPenetrationMessage { BodyShapeSource = bodyShapeA; BodyShapeTarget = bodyShapeB; Normal = normal })
                        physicsEngine.IntegrationMessages.Add (BodyPenetrationMessage { BodyShapeSource = bodyShapeB; BodyShapeTarget = bodyShapeA; Normal = -normal })

                // collect separations for non-sensors that aren't ground separations by characters
                for i in 0 .. dec contacts.endCount do
                    let separation = &contacts.endEvents.[i]
                    physicsEngine.ContactsTracker.ExistingContacts.Remove (separation.shapeIdA, separation.shapeIdB) |> ignore
                    let bodyShapeA = B2Shapes.b2Shape_GetUserData separation.shapeIdA :?> BodyShapeIndex
                    let bodyShapeB = B2Shapes.b2Shape_GetUserData separation.shapeIdB :?> BodyShapeIndex
                    physicsEngine.IntegrationMessages.Add (BodySeparationMessage { BodyShapeSource = bodyShapeA; BodyShapeTarget = bodyShapeB })
                    physicsEngine.IntegrationMessages.Add (BodySeparationMessage { BodyShapeSource = bodyShapeB; BodyShapeTarget = bodyShapeA })

                // collect penetrations for sensors that aren't ground penetrations by characters
                let sensorEvents = B2Worlds.b2World_GetSensorEvents physicsEngine.PhysicsContextId
                for i in 0 .. dec sensorEvents.beginCount do
                    let sensorEvent = &sensorEvents.beginEvents.[i]
                    let bodyShapeA = B2Shapes.b2Shape_GetUserData sensorEvent.sensorShapeId :?> BodyShapeIndex
                    let bodyShapeB = B2Shapes.b2Shape_GetUserData sensorEvent.visitorShapeId :?> BodyShapeIndex
                    let normal = Box2dNetPhysicsEngine.computeCollisionNormalForSensors sensorEvent.sensorShapeId sensorEvent.visitorShapeId
                    let normal = Vector3 (normal.X, normal.Y, 0.0f)
                    physicsEngine.IntegrationMessages.Add (BodyPenetrationMessage { BodyShapeSource = bodyShapeA; BodyShapeTarget = bodyShapeB; Normal = normal })
                    physicsEngine.IntegrationMessages.Add (BodyPenetrationMessage { BodyShapeSource = bodyShapeB; BodyShapeTarget = bodyShapeA; Normal = -normal })

                // collect separations for sensors that aren't ground separations by characters
                for i in 0 .. dec sensorEvents.endCount do
                    let sensorEvent = &sensorEvents.endEvents.[i]
                    let bodyShapeA = B2Shapes.b2Shape_GetUserData sensorEvent.sensorShapeId :?> BodyShapeIndex
                    let bodyShapeB = B2Shapes.b2Shape_GetUserData sensorEvent.visitorShapeId :?> BodyShapeIndex
                    physicsEngine.IntegrationMessages.Add (BodySeparationMessage { BodyShapeSource = bodyShapeA; BodyShapeTarget = bodyShapeB })
                    physicsEngine.IntegrationMessages.Add (BodySeparationMessage { BodyShapeSource = bodyShapeB; BodyShapeTarget = bodyShapeA })
                    
                // collect transforms that aren't by characters nor fluid particles
                let bodyEvents = B2Worlds.b2World_GetBodyEvents physicsEngine.PhysicsContextId
                for i in 0 .. dec bodyEvents.moveCount do
                    let transform = &bodyEvents.moveEvents.[i]
                    match transform.userData with
                    | :? BodyId as bodyId ->
                        if not (physicsEngine.Characters.ContainsKey bodyId) then
                            physicsEngine.IntegrationMessages.Add 
                                (BodyTransformMessage
                                    { BodyId = bodyId
                                      Center = Box2dNetPhysicsEngine.toPixelV3 transform.transform.p
                                      Rotation = Box2dNetPhysicsEngine.rotToQuat transform.transform.q
                                      LinearVelocity = Box2dNetPhysicsEngine.toPixelV3 (B2Bodies.b2Body_GetLinearVelocity transform.bodyId)
                                      AngularVelocity = v3 0.0f 0.0f (B2Bodies.b2Body_GetAngularVelocity transform.bodyId) })
                    | _ -> () // fluid particle transforms are not reported as body transforms

                for KeyValue (bodyId, character) in physicsEngine.Characters do

                    // step character simulations
                    let body = physicsEngine.Bodies.[bodyId]
                    character.Transform <- B2Bodies.b2Body_GetTransform body
                    character.Velocity <- B2Bodies.b2Body_GetLinearVelocity body
                    let oldGround = character.OnGround
                    Box2dNetPhysicsEngine.solveCharacter character stepTime physicsEngine.PhysicsContextId physicsEngine.CharacterCollisionContext
                    B2Bodies.b2Body_SetTransform (body, character.Transform.p, character.Transform.q)
                    B2Bodies.b2Body_SetLinearVelocity (body, character.Velocity)

                    // collect character transform events
                    physicsEngine.IntegrationMessages.Add 
                        (BodyTransformMessage
                            { BodyId = bodyId
                              Center = Box2dNetPhysicsEngine.toPixelV3 character.Transform.p
                              Rotation = Box2dNetPhysicsEngine.rotToQuat character.Transform.q
                              LinearVelocity = Box2dNetPhysicsEngine.toPixelV3 character.Velocity
                              AngularVelocity = v3 0.0f 0.0f (B2Bodies.b2Body_GetAngularVelocity body) })

                    // collect character ground separation and penetration events
                    if oldGround <> character.OnGround then
                        let capsule = B2Shapes.b2Shape_GetUserData character.CapsuleShape :?> BodyShapeIndex
                        match oldGround with
                        | ValueSome oldGround ->
                            let oldGround = B2Shapes.b2Shape_GetUserData oldGround :?> BodyShapeIndex
                            physicsEngine.IntegrationMessages.Add (BodySeparationMessage { BodyShapeSource = capsule; BodyShapeTarget = oldGround })
                            physicsEngine.IntegrationMessages.Add (BodySeparationMessage { BodyShapeSource = oldGround; BodyShapeTarget = capsule })
                        | ValueNone -> ()
                        match character.OnGround with
                        | ValueSome newGround ->
                            let newGround = B2Shapes.b2Shape_GetUserData newGround :?> BodyShapeIndex
                            let normal = character.GroundCastResult.normal // NOTE: newGround to capsule, which is the opposite of our message
                            physicsEngine.IntegrationMessages.Add (BodyPenetrationMessage { BodyShapeSource = capsule; BodyShapeTarget = newGround; Normal = v3 -normal.X -normal.Y 0.0f })
                            physicsEngine.IntegrationMessages.Add (BodyPenetrationMessage { BodyShapeSource = newGround; BodyShapeTarget = capsule; Normal = v3 normal.X normal.Y 0.0f })
                        | ValueNone -> ()

                // contruct result and clear integration messages.
                let result = Some (SArray.ofSeq physicsEngine.IntegrationMessages)
                physicsEngine.IntegrationMessages.Clear ()
                result

            // no time passed
            else None

        member physicsEngine.TryRender renderContext =
            match renderContext with
            | :? Box2dNetPhysicsEngineRenderContext as renderContext ->

                // TODO: implement a better drawing procedure using B2DebugDraw, which requires new World functions to draw
                // solid shapes, arcs, etc. For now, we just draw lines and circles for each shape.
                let eyeBounds = renderContext.EyeBounds
                let v2ToB2Vec2 (v : Vector2) = B2Vec2 (Box2dNetPhysicsEngine.toPhysics v.X, Box2dNetPhysicsEngine.toPhysics v.Y)
                let eyeAabb = B2AABB (v2ToB2Vec2 eyeBounds.Min, v2ToB2Vec2 eyeBounds.Max)
                B2Worlds.b2World_OverlapAABB
                    (physicsEngine.PhysicsContextId,
                     eyeAabb,
                     B2QueryFilter (UInt64.MaxValue, UInt64.MaxValue),
                     renderCallback,
                     (renderContext : Box2dNetPhysicsEngineRenderContext)) |> ignore<B2TreeStats>

                // draw character pogos
                for KeyValue (bodyId, m) in physicsEngine.Characters do
                    let aabb = B2Bodies.b2Body_ComputeAABB physicsEngine.Bodies.[bodyId]
                    if B2MathFunction.b2AABB_Overlaps (eyeAabb, aabb) then
                        let pogoStop = m.PogoOrigin + m.PogoDelta

                        // pogo spring
                        let start = Box2dNetPhysicsEngine.toPixelV2 m.PogoOrigin
                        let stop = Box2dNetPhysicsEngine.toPixelV2 pogoStop
                        renderContext.DrawLine (start, stop, Color.Plum)

                        // pogo
                        let radius = Box2dNetPhysicsEngine.toPixel m.PogoProxy.radius
                        for i in 0 .. dec m.PogoProxy.count do
                            let start = m.PogoProxy.points.[i] + pogoStop |> Box2dNetPhysicsEngine.toPixelV2
                            let stop = m.PogoProxy.points.[if i < dec m.PogoProxy.count then inc i else 0] + pogoStop |> Box2dNetPhysicsEngine.toPixelV2
                            renderContext.DrawLine (start, stop, Color.Plum)
                            if radius <> 0.0f then
                                renderContext.DrawCircle (start, radius, Color.Plum)
            | _ -> ()

        member physicsEngine.ClearInternal () =
            physicsEngine.FluidEmitters.Clear ()
            physicsEngine.Joints.Clear ()
            physicsEngine.BreakableJoints.Clear ()
            physicsEngine.Bodies.Clear ()
            physicsEngine.BodyGravityOverrides.Clear ()
            physicsEngine.Characters.Clear ()
            physicsEngine.CharacterCollisionContext.SoftCollisionPushLimits.Clear ()
            physicsEngine.CreateBodyJointMessages.Clear ()
            let contextId = physicsEngine.PhysicsContextId
            physicsEngine.PhysicsContextId <- Box2dNetPhysicsEngine.makePhysicsContext (B2Worlds.b2World_GetGravity contextId) physicsEngine.ContactsTracker
            B2Worlds.b2DestroyWorld contextId

        member physicsEngine.CleanUp () =
            B2Worlds.b2DestroyWorld physicsEngine.PhysicsContextId
            physicsEngine.PhysicsContextId <- B2Ids.b2_nullWorldId // NOTE: Box2D.NET recommends nullifying references.

namespace Box2D.NET.Debugging
open System
open Box2D.NET

type B2WorldIdDebuggerDisplay (impl) =
    member _.AwakeBodyCount = B2Worlds.b2World_GetAwakeBodyCount impl
    member _.BodyEvents = B2Worlds.b2World_GetBodyEvents impl
    member _.ContactEvents = B2Worlds.b2World_GetContactEvents impl
    member _.Counters = B2Worlds.b2World_GetCounters impl
    member _.Gravity with get () = B2Worlds.b2World_GetGravity impl and set v = B2Worlds.b2World_SetGravity (impl, v)
    member _.HitEventThreshold with get () = B2Worlds.b2World_GetHitEventThreshold impl and set v = B2Worlds.b2World_SetHitEventThreshold (impl, v)
    member _.IsContinuousEnabled with get () = B2Worlds.b2World_IsContinuousEnabled impl and set v = B2Worlds.b2World_EnableContinuous (impl, v)
    member _.IsSleepingEnabled with get () = B2Worlds.b2World_IsSleepingEnabled impl and set v = B2Worlds.b2World_EnableSleeping (impl, v)
    member _.IsValid = B2Worlds.b2World_IsValid impl
    member _.IsWarmStartingEnabled with get () = B2Worlds.b2World_IsWarmStartingEnabled impl and set v = B2Worlds.b2World_EnableWarmStarting (impl, v)
    member _.JointEvents = B2Worlds.b2World_GetJointEvents impl
    member _.MaximumLinearSpeed with get () = B2Worlds.b2World_GetMaximumLinearSpeed impl and set v = B2Worlds.b2World_SetMaximumLinearSpeed (impl, v)
    member _.Profile = B2Worlds.b2World_GetProfile impl
    member _.RestitutionThreshold with get () = B2Worlds.b2World_GetRestitutionThreshold impl and set v = B2Worlds.b2World_SetRestitutionThreshold (impl, v)
    member _.SensorEvents = B2Worlds.b2World_GetSensorEvents impl
    member _.UserData with get () = B2Worlds.b2World_GetUserData impl and set v = B2Worlds.b2World_SetUserData (impl, v)

type B2BodyIdDebuggerDisplay (impl) =
    member _.AngularDamping with get () = B2Bodies.b2Body_GetAngularDamping impl and set v = B2Bodies.b2Body_SetAngularDamping (impl, v)
    member _.AngularVelocity with get () = B2Bodies.b2Body_GetAngularVelocity impl and set v = B2Bodies.b2Body_SetAngularVelocity (impl, v)
    member _.ContactCapacity = B2Bodies.b2Body_GetContactCapacity impl
    member _.ContactData =
        let capacity = B2Bodies.b2Body_GetContactCapacity impl
        let contacts = Array.zeroCreate capacity
        contacts.[0 .. B2Bodies.b2Body_GetContactData (impl, contacts.AsSpan (), capacity) - 1]
    member _.GravityScale with get () = B2Bodies.b2Body_GetGravityScale impl and set v = B2Bodies.b2Body_SetGravityScale (impl, v)
    member _.IsAwake with get () = B2Bodies.b2Body_IsAwake impl and set v = B2Bodies.b2Body_SetAwake (impl, v)
    member _.IsBullet with get () = B2Bodies.b2Body_IsBullet impl and set v = B2Bodies.b2Body_SetBullet (impl, v)
    member _.IsEnabled with get () = B2Bodies.b2Body_IsEnabled impl and set v = if v then B2Bodies.b2Body_Enable impl else B2Bodies.b2Body_Disable impl
    member _.IsFixedRotation with get () = B2Bodies.b2Body_IsFixedRotation impl and set v = B2Bodies.b2Body_SetFixedRotation (impl, v)
    member _.IsSleepEnabled with get () = B2Bodies.b2Body_IsSleepEnabled impl and set v = B2Bodies.b2Body_EnableSleep (impl, v)
    member _.IsValid = B2Worlds.b2Body_IsValid impl
    member _.Joints =
        let capacity = B2Bodies.b2Body_GetJointCount impl
        let joints = Array.zeroCreate capacity
        B2Bodies.b2Body_GetJoints (impl, joints.AsSpan(), capacity) |> ignore<int>
        joints
    member _.JointCount = B2Bodies.b2Body_GetJointCount impl
    member _.LinearDamping with get () = B2Bodies.b2Body_GetLinearDamping impl and set v = B2Bodies.b2Body_SetLinearDamping (impl, v)
    member _.LinearVelocity with get () = B2Bodies.b2Body_GetLinearVelocity impl and set v = B2Bodies.b2Body_SetLinearVelocity (impl, v)
    member _.LocalCenterOfMass
        with get () = B2Bodies.b2Body_GetLocalCenterOfMass impl
        and set v =
            let mutable massData = B2Bodies.b2Body_GetMassData impl
            massData.center <- v
            B2Bodies.b2Body_SetMassData (impl, massData)
    member _.Mass
        with get () = B2Bodies.b2Body_GetMass impl
        and set v =
            let mutable massData = B2Bodies.b2Body_GetMassData impl
            massData.mass <- v
            B2Bodies.b2Body_SetMassData (impl, massData)
    member _.MassData with get () = B2Bodies.b2Body_GetMassData impl and set v = B2Bodies.b2Body_SetMassData (impl, v)
    member _.Name with get () = B2Bodies.b2Body_GetName impl and set v = B2Bodies.b2Body_SetName (impl, v)
    member _.Position with get () = B2Bodies.b2Body_GetPosition impl and set v = B2Bodies.b2Body_SetTransform (impl, v, B2Bodies.b2Body_GetRotation impl)
    member _.Rotation with get () = B2Bodies.b2Body_GetRotation impl and set v = B2Bodies.b2Body_SetTransform (impl, B2Bodies.b2Body_GetPosition impl, v)
    member _.RotationalInertia
        with get () = B2Bodies.b2Body_GetRotationalInertia impl
        and set v =
            let mutable massData = B2Bodies.b2Body_GetMassData impl
            massData.rotationalInertia <- v
            B2Bodies.b2Body_SetMassData (impl, massData)
    member _.Shapes =
        let capacity = B2Bodies.b2Body_GetShapeCount impl
        let shapes = Array.zeroCreate capacity
        B2Bodies.b2Body_GetShapes (impl, shapes.AsSpan(), capacity) |> ignore<int>
        shapes
    member _.ShapeCount = B2Bodies.b2Body_GetShapeCount impl
    member _.SleepThreshold with get () = B2Bodies.b2Body_GetSleepThreshold impl and set v = B2Bodies.b2Body_SetSleepThreshold (impl, v)
    member _.Transform with get () = B2Bodies.b2Body_GetTransform impl and set (v : B2Transform) = B2Bodies.b2Body_SetTransform (impl, v.p, v.q)
    member _.Type with get () = B2Bodies.b2Body_GetType impl and set v = B2Bodies.b2Body_SetType (impl, v)
    member _.UserData with get () = B2Bodies.b2Body_GetUserData impl and set v = B2Bodies.b2Body_SetUserData (impl, v)
    member _.World = B2Bodies.b2Body_GetWorld impl
    member _.WorldCenterOfMass = B2Bodies.b2Body_GetWorldCenterOfMass impl
    
type B2ShapeIdDebuggerDisplay (impl) =
    member _.AABB = B2Shapes.b2Shape_GetAABB impl
    member _.AreContactEventsEnabled with get () = B2Shapes.b2Shape_AreContactEventsEnabled impl and set v = B2Shapes.b2Shape_EnableContactEvents (impl, v)
    member _.AreHitEventsEnabled with get () = B2Shapes.b2Shape_AreHitEventsEnabled impl and set v = B2Shapes.b2Shape_EnableHitEvents (impl, v)
    member _.ArePreSolveEventsEnabled with get () = B2Shapes.b2Shape_ArePreSolveEventsEnabled impl and set v = B2Shapes.b2Shape_EnablePreSolveEvents (impl, v)
    member _.AreSensorEventsEnabled with get () = B2Shapes.b2Shape_AreSensorEventsEnabled impl and set v = B2Shapes.b2Shape_EnableSensorEvents (impl, v)
    member _.Body = B2Shapes.b2Shape_GetBody impl
    member _.Capsule
        with get () = if B2Shapes.b2Shape_GetType impl = B2ShapeType.b2_capsuleShape then B2Shapes.b2Shape_GetCapsule impl else failwith "Not a capsule"
        and set v = let mutable v = v in B2Shapes.b2Shape_SetCapsule (impl, &v)
    member _.ChainSegment with get () = if B2Shapes.b2Shape_GetType impl = B2ShapeType.b2_chainSegmentShape then B2Shapes.b2Shape_GetChainSegment impl else failwith "Not a chain segment"
    member _.Circle
        with get () = if B2Shapes.b2Shape_GetType impl = B2ShapeType.b2_circleShape then B2Shapes.b2Shape_GetCircle impl else failwith "Not a circle"
        and set v = let mutable v = v in B2Shapes.b2Shape_SetCircle (impl, &v)
    member _.ContactCapacity = B2Shapes.b2Shape_GetContactCapacity impl
    member _.ContactData =
        let capacity = B2Shapes.b2Shape_GetContactCapacity impl
        let contacts = Array.zeroCreate capacity
        contacts.[0 .. B2Shapes.b2Shape_GetContactData (impl, contacts.AsSpan (), capacity) - 1]
    member _.Density with get () = B2Shapes.b2Shape_GetDensity impl and set v = B2Shapes.b2Shape_SetDensity (impl, v, true)
    member _.Filter with get () = B2Shapes.b2Shape_GetFilter impl and set v = B2Shapes.b2Shape_SetFilter (impl, v)
    member _.Friction with get () = B2Shapes.b2Shape_GetFriction impl and set v = B2Shapes.b2Shape_SetFriction (impl, v)
    member _.IsSensor = B2Shapes.b2Shape_IsSensor impl
    member _.IsValid = B2Worlds.b2Shape_IsValid impl
    member _.MassData with get () = B2Shapes.b2Shape_GetMassData impl
    member _.Material with get () = B2Shapes.b2Shape_GetMaterial impl and set v = B2Shapes.b2Shape_SetMaterial (impl, v)
    member _.ParentChain = B2Shapes.b2Shape_GetParentChain impl
    member _.Polygon
        with get () = if B2Shapes.b2Shape_GetType impl = B2ShapeType.b2_polygonShape then B2Shapes.b2Shape_GetPolygon impl else failwith "Not a polygon"
        and set v = let mutable v = v in B2Shapes.b2Shape_SetPolygon (impl, &v)
    member _.Restitution with get () = B2Shapes.b2Shape_GetRestitution impl and set v = B2Shapes.b2Shape_SetRestitution (impl, v)
    member _.Segment
        with get () = if B2Shapes.b2Shape_GetType impl = B2ShapeType.b2_segmentShape then B2Shapes.b2Shape_GetSegment impl else failwith "Not a segment"
        and set v = let mutable v = v in B2Shapes.b2Shape_SetSegment (impl, &v)
    member _.SensorCapacity = B2Shapes.b2Shape_GetSensorCapacity impl
    member _.SensorOverlaps =
        let capacity = B2Shapes.b2Shape_GetSensorCapacity impl
        let overlaps = Array.zeroCreate capacity
        overlaps.[0 .. B2Shapes.b2Shape_GetSensorOverlaps (impl, overlaps.AsSpan (), capacity) - 1]
    member _.SurfaceMaterial with get () = B2Shapes.b2Shape_GetSurfaceMaterial impl and set v = B2Shapes.b2Shape_SetSurfaceMaterial (impl, v)
    member _.Type = B2Shapes.b2Shape_GetType impl
    member _.UserData with get () = B2Shapes.b2Shape_GetUserData impl and set v = B2Shapes.b2Shape_SetUserData (impl, v)
    member _.World = B2Shapes.b2Shape_GetWorld impl

type B2ChainIdDebuggerDisplay (impl) =
    member _.Friction with get () = B2Shapes.b2Chain_GetFriction impl and set v = B2Shapes.b2Chain_SetFriction (impl, v)
    member _.IsValid = B2Worlds.b2Chain_IsValid impl
    member _.Material with get () = B2Shapes.b2Chain_GetMaterial impl and set v = B2Shapes.b2Chain_SetMaterial (impl, v)
    member _.Restitution with get () = B2Shapes.b2Chain_GetRestitution impl and set v = B2Shapes.b2Chain_SetRestitution (impl, v)
    member _.SegmentCount = B2Shapes.b2Chain_GetSegmentCount impl
    member _.Segments =
        let capacity = B2Shapes.b2Chain_GetSegmentCount impl
        let segments = Array.zeroCreate capacity
        B2Shapes.b2Chain_GetSegments (impl, segments, capacity) |> ignore<int>
        segments
    member _.World = B2Shapes.b2Chain_GetWorld impl

type B2JointIdDebuggerDisplay (impl) =
    member _.AngularSeparation = B2Joints.b2Joint_GetAngularSeparation impl
    member _.BodyA = B2Joints.b2Joint_GetBodyA impl
    member _.BodyB = B2Joints.b2Joint_GetBodyB impl
    member _.CollideConnected with get () = B2Joints.b2Joint_GetCollideConnected impl and set v = B2Joints.b2Joint_SetCollideConnected (impl, v)
    member _.ConstraintForce = B2Joints.b2Joint_GetConstraintForce impl
    member _.ConstraintTorque = B2Joints.b2Joint_GetConstraintTorque impl
    member _.ConstraintTuning with get () = B2Joints.b2Joint_GetConstraintTuning impl and set (hertz, dampingRatio) = B2Joints.b2Joint_SetConstraintTuning (impl, hertz, dampingRatio)
    member _.LinearSeparation = B2Joints.b2Joint_GetLinearSeparation impl
    member _.LocalFrameA with get () = B2Joints.b2Joint_GetLocalFrameA impl and set v = B2Joints.b2Joint_SetLocalFrameA (impl, v)
    member _.LocalFrameB with get () = B2Joints.b2Joint_GetLocalFrameB impl and set v = B2Joints.b2Joint_SetLocalFrameB (impl, v)
    member _.Type = B2Joints.b2Joint_GetType impl
    member _.UserData with get () = B2Joints.b2Joint_GetUserData impl and set v = B2Joints.b2Joint_SetUserData (impl, v)
    member _.World = B2Joints.b2Joint_GetWorld impl

open System.Diagnostics
[<DebuggerDisplay ("({X}, {Y})", Target = typeof<B2Vec2>)>]
[<DebuggerDisplay ("(c = {c}, s = {s})", Target = typeof<B2Rot>)>]
[<DebuggerDisplay ("\{lowerBound = {lowerBound}, upperBound = {upperBound}}", Target = typeof<B2AABB>)>]
[<DebuggerDisplay ("\{p = {p}, q = {q}}", Target = typeof<B2Transform>)>]
[<DebuggerDisplay ("\{index1 = {index1}, generation = {generation}}", Target = typeof<B2WorldId>)>]
[<DebuggerDisplay ("\{index1 = {index1}, world0 = {world0}, generation = {generation}}", Target = typeof<B2BodyId>)>]
[<DebuggerDisplay ("\{index1 = {index1}, world0 = {world0}, generation = {generation}}", Target = typeof<B2ShapeId>)>]
[<DebuggerDisplay ("\{index1 = {index1}, world0 = {world0}, generation = {generation}}", Target = typeof<B2ChainId>)>]
[<DebuggerDisplay ("\{index1 = {index1}, world0 = {world0}, generation = {generation}}", Target = typeof<B2JointId>)>]
[<DebuggerTypeProxy (typeof<B2WorldIdDebuggerDisplay>, Target = typeof<B2WorldId>)>]
[<DebuggerTypeProxy (typeof<B2BodyIdDebuggerDisplay>, Target = typeof<B2BodyId>)>]
[<DebuggerTypeProxy (typeof<B2ShapeIdDebuggerDisplay>, Target = typeof<B2ShapeId>)>]
[<DebuggerTypeProxy (typeof<B2ChainIdDebuggerDisplay>, Target = typeof<B2ChainId>)>]
[<DebuggerTypeProxy (typeof<B2JointIdDebuggerDisplay>, Target = typeof<B2JointId>)>]
do ()