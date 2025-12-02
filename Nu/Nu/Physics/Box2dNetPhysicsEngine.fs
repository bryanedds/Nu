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
      // computeAccumulatedImpulses output
      mutable AccumulatedImpulse : B2Vec2 }

/// Represents a Box2D.NET fluid emitter.
///
/// Follows sfml-box2d-fluid implementation: https://github.com/a-piece-of-snake/sfml-box2d-fluid/tree/3c25fef7a1ffe28ada8fe345358c4a3ac38df190
///
/// NOTE: there is too much MAGIC (see comments) going on, this implementation will hopefully be superceded in a future Box2D update.
type private Box2dNetFluidEmitter =
    { FluidEmitterDescriptor : FluidEmitterDescriptorBox2dNet
      PhysicsContextId : B2WorldId
      GravityOverrides : Dictionary<B2BodyId, B2Vec2>
      OwnShapes : HashSet<B2ShapeId>
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
        shapeDef.userData <- bodyShapeIndex // required to identify shape in ray/shape casts and collision events
        shapeDef.enablePreSolveEvents <- Constants.Physics.Collision2dFrameCompensation
        shapeDef.enableContactEvents <- true
        shapeDef.enableSensorEvents <- true
        let mutable circle = B2Circle (B2MathFunction.b2Vec2_zero, toPhysics config.Radius)
        let shape = B2Shapes.b2CreateCircleShape (body, &shapeDef, &circle)
        fluidEmitter.OwnShapes.Add shape |> assertTrue
        struct (body, shape)

    static member positionToCellId cellSize (position : B2Vec2) =
        v2i (floor (position.X / cellSize) |> int) (floor (position.Y / cellSize) |> int)

    static member cellIdToBox cellSize (cellId : Vector2i) =
        box2 (cellId.V2 * cellSize) (v2Dup cellSize)

    static let updateConfig state config (fluidEmitter : Box2dNetFluidEmitter) =
        let bodyShapeIndex = B2Shapes.b2Shape_GetUserData state.Shape :?> BodyShapeIndex
        let position = B2Bodies.b2Body_GetPosition state.Body
        let velocity = B2Bodies.b2Body_GetLinearVelocity state.Body
        B2Bodies.b2DestroyBody state.Body
        fluidEmitter.OwnShapes.Remove state.Shape |> assertTrue
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
            fluidEmitter.OwnShapes.Remove state.Shape |> assertTrue
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
            let state = fluidEmitter.States.[i]
            B2Bodies.b2DestroyBody state.Body
            fluidEmitter.OwnShapes.Remove state.Shape |> assertTrue
            fluidEmitter.GravityOverrides.Remove state.Body |> ignore<bool>
        fluidEmitter.StateCount <- 0

    static member getForce dst radius = // see sfml-box2d-fluid: GameObjects.cpp, ParticleGroup::GetForce
        if dst >= radius then 0.0f else
        let x = radius - dst
        let x_pow2_5 = x * x * MathF.Sqrt x
        let radius_sq = radius * radius
        x_pow2_5 / (MathF.PI * radius_sq * radius_sq * 0.25f) // MAGIC: this formula is pure magic (@.@)
        
    static let Neighborhood = [|for x in -1 .. 1 do for y in -1 .. 1 do v2i x y|]
    static member computeAccumulatedImpulses idx timestep fluidEmitter = // see sfml-box2d-fluid: GameObjects.cpp, ParticleGroup::ComputeChunkForces
        let state = fluidEmitter.States.[idx]
        if B2Bodies.b2Body_IsAwake state.Body then
            let config = getConfig (B2Bodies.b2Body_GetName state.Body) fluidEmitter
            let radiusA = state.CachedRadius
            let range = radiusA * config.Impact
            let mutable density = 0.0f
            let posA = state.CachedPosition
            let mutable neighborPosSum = B2MathFunction.b2Vec2_zero
            let mutable neighborVecSum = B2MathFunction.b2Vec2_zero
            let mutable attractCount = 0
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

                    // repulsion force
                    let forceDir = B2MathFunction.b2Normalize offset
                    let densityScale = B2MathFunction.b2ClampFloat (density, config.MinDensity, config.MaxDensity)
                    let distanceForceMag = min (Box2dNetFluidEmitter.getForce dst effectiveRange) config.MaxForce * densityScale * densityScale * sqrt (sqrt densityScale)
                    let repulsionForce = config.ForceMultiplier * forceDir * distanceForceMag * effectiveRange

                    // momentum force
                    let velB = otherState.CachedVelocity
                    let momentumForce = (velA - velB) * ((effectiveRange - dst) / effectiveRange) * config.MomentumCoefficient

                    // surface force accumulation
                    if B2Bodies.b2Body_GetName otherState.Body = B2Bodies.b2Body_GetName state.Body then
                        neighborPosSum <- neighborPosSum + posB
                        neighborVecSum <- neighborVecSum + velB
                        attractCount <- inc attractCount

                    // viscosity force
                    let rNorm = B2MathFunction.b2ClampFloat (dst / effectiveRange, 0.1f, 10.0f)
                    let ViscosityKernelSimple r = -0.5f * r * r * r + r * r - 1.0f
                    let kVisc = ViscosityKernelSimple rNorm
                    let mutable velDiff = velB - velA
                    let maxVelDiff = 10.0f
                    let len = B2MathFunction.b2Length velDiff
                    if 0.001f < len && len < maxVelDiff then
                        velDiff <- velDiff * (maxVelDiff / len)
                    let dot = B2MathFunction.b2Dot (velDiff, offset)
                    let leaveMul = if dot > 0.0f then config.ViscosityLeave else 1.0f
                    let viscForce = config.Viscosity * leaveMul * kVisc * velDiff

                    // friction force
                    let dir = B2MathFunction.b2Normalize offset
                    let tangent = B2Vec2 (-dir.Y, dir.X)
                    let tangentialSpeed = B2MathFunction.b2Dot (velB - velA, tangent)
                    let attenuation = 1.0f - rNorm
                    let frictionForce = (-config.ShearViscosity * tangentialSpeed * tangent) * attenuation

                    // apply forces
                    let mutable totalForce = (repulsionForce + momentumForce + frictionForce + viscForce) * timestep
                    assert (Single.IsFinite totalForce.X && Single.IsFinite totalForce.Y)
                    totalForce.X <- B2MathFunction.b2ClampFloat (totalForce.X, -config.MaxGetForce, config.MaxGetForce)
                    totalForce.Y <- B2MathFunction.b2ClampFloat (totalForce.Y, -config.MaxGetForce, config.MaxGetForce)
                    other.AccumulatedImpulse <- other.AccumulatedImpulse + totalForce
                    state.AccumulatedImpulse <- state.AccumulatedImpulse - totalForce

            // surface force
            if attractCount > 0 then
                let attractCount = single attractCount
                let avgPos = B2Vec2 (neighborPosSum.X / attractCount, neighborPosSum.Y / attractCount)
                let surfaceForce = config.ForceSurface * (avgPos - posA)
                assert (Single.IsFinite surfaceForce.X && Single.IsFinite surfaceForce.Y)
                state.AccumulatedImpulse <- state.AccumulatedImpulse + surfaceForce
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

        // collect fluid particle transforms
        struct (particles, removedParticles)

    static member make descriptor physicsContextId bodySource =
        { FluidEmitterDescriptor = descriptor
          PhysicsContextId = physicsContextId
          OwnShapes = HashSet HashIdentity.Structural
          States = Array.zeroCreate descriptor.ParticlesMax
          StateCount = 0
          Grid = Dictionary HashIdentity.Structural
          RemovedIndexes = List ()
          BodySource = bodySource
          NextBodyIndex = 0
          GravityOverrides = Dictionary HashIdentity.Structural }

type private Box2dNetPhysicsEngineContactsTracker =
    { NewContacts : ConcurrentDictionary<struct (B2ShapeId * B2ShapeId), struct {| Nearest : B2Vec2; Normal : B2Vec2 |}>
      ExistingContacts : HashSet<struct (B2ShapeId * B2ShapeId)> }

/// The Box2D.NET interface of PhysicsEngineRenderContext.
type Box2dNetPhysicsEngineRenderContext =
    inherit PhysicsEngineRenderContext
    abstract EyeBounds : Box2
    abstract DrawLine : start : Vector2 * stop : Vector2 * color : Color -> unit
    abstract DrawCircle : position : Vector2 * radius : single * color : Color -> unit

/// The Box2D.NET implementation of PhysicsEngine.
type [<ReferenceEquality>] Box2dNetPhysicsEngine =
    private
        { mutable PhysicsContextId : B2WorldId
          Bodies : Dictionary<BodyId, B2BodyId>
          BodyGravityOverrides : Dictionary<BodyId, Vector3>
          Joints : Dictionary<BodyJointId, B2JointId>
          BreakableJoints : Dictionary<BodyJointId, struct {| BreakingPoint : single; BreakingPointSquared : single |}>
          CreateBodyJointMessages : Dictionary<BodyId, CreateBodyJointMessage List>
          FluidEmitters : Dictionary<FluidEmitterId, Box2dNetFluidEmitter>
          FluidCollisions : Dictionary<FluidEmitterId, FluidCollision List> // OPTIMIZATION: cached to avoid large arrays filling up the LOH.
          IntegrationMessages : IntegrationMessage List // OPTIMIZATION: cached to avoid large arrays filling up the LOH.
          ContactsTracker : Box2dNetPhysicsEngineContactsTracker } // NOTE: supports thread safety for b2PreSolveFcn.

    member private this.PhysicsContext =
        B2Worlds.b2GetWorld (int this.PhysicsContextId.index1)

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
        Quaternion (0.0f, 0.0f, sin halfAngle, cos halfAngle)

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
            bodyShapeDef.filter.groupIndex <- match bodyShapeProperties.CollisionGroupOpt with Some cg -> cg | None -> bodyProperties.CollisionGroup
            bodyShapeDef.filter.categoryBits <- match bodyShapeProperties.CollisionCategoriesOpt with Some cc -> cc | None -> bodyProperties.CollisionCategories
            bodyShapeDef.filter.maskBits <- match bodyShapeProperties.CollisionMaskOpt with Some cm -> cm | None -> bodyProperties.CollisionMask
            bodyShapeDef.isSensor <- match bodyShapeProperties.SensorOpt with Some sensor -> sensor | None -> bodyProperties.Sensor
        | None ->
            bodyShapeDef.material.friction <- bodyProperties.Friction
            bodyShapeDef.material.restitution <- bodyProperties.Restitution
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

    static member private attachCapsuleShape bodySource (bodyProperties : BodyProperties) (capsuleShape : CapsuleShape) (body : B2BodyId) =
        let transform = Option.defaultValue Affine.Identity capsuleShape.TransformOpt
        let height = Box2dNetPhysicsEngine.toPhysicsPolygonDiameter (capsuleShape.Height * transform.Scale.Y)
        let endRadius = Box2dNetPhysicsEngine.toPhysicsPolygonRadius (capsuleShape.Radius * transform.Scale.Y)
        let offset = Box2dNetPhysicsEngine.toPhysicsV2 transform.Translation
        let circleOffset = B2MathFunction.b2RotateVector (Box2dNetPhysicsEngine.quatToRot transform.Rotation, B2Vec2 (0.0f, height * 0.5f))
        let mutable shapeDef = Unchecked.defaultof<_>
        configureBodyShapeProperties &shapeDef bodySource bodyProperties capsuleShape.PropertiesOpt
        shapeDef.density <-
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass -> mass / (endRadius * 2.0f * height + MathF.PI * endRadius * endRadius)
        let mutable capsule = B2Capsule (circleOffset + offset, -circleOffset + offset, endRadius)
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
        for i in 0 .. vertices'.Length / 3 do
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
        let mutable bodyDef = Unchecked.defaultof<_>
        bodyDef <- B2Types.b2DefaultBodyDef ()
        bodyDef.``type`` <-
            match bodyProperties.BodyType with
            | Static -> B2BodyType.b2_staticBody
            | Kinematic -> B2BodyType.b2_kinematicBody
            | KinematicCharacter -> Log.infoOnce "KinematicCharacter not yet supported by Box2dNetPhysicsEngine. Using Kinematic configuration instead."; B2BodyType.b2_kinematicBody
            | Dynamic -> B2BodyType.b2_dynamicBody
            | DynamicCharacter -> Log.infoOnce "DynamicCharacter not yet supported by Box2dNetPhysicsEngine. Using Dynamic configuration instead."; B2BodyType.b2_dynamicBody
            | Vehicle -> Log.infoOnce "Vehicle not supported by Box2dNetPhysicsEngine. Using Dynamic configuration instead."; B2BodyType.b2_dynamicBody
        bodyDef.isEnabled <- bodyProperties.Enabled
        bodyDef.enableSleep <- bodyProperties.SleepingAllowed
        bodyDef.position <- Box2dNetPhysicsEngine.toPhysicsV2 bodyProperties.Center
        bodyDef.rotation <- Box2dNetPhysicsEngine.quatToRot bodyProperties.Rotation
        bodyDef.linearVelocity <- Box2dNetPhysicsEngine.toPhysicsV2 bodyProperties.LinearVelocity
        bodyDef.linearDamping <- bodyProperties.LinearDamping
        bodyDef.angularVelocity <- bodyProperties.AngularVelocity.Z
        bodyDef.angularDamping <- bodyProperties.AngularDamping
        bodyDef.fixedRotation <- bodyProperties.AngularFactor.Z = 0.0f
        let gravityOverrideOpt =
            if bodyDef.``type`` = B2BodyType.b2_dynamicBody then
                match bodyProperties.Gravity with
                | GravityWorld -> bodyDef.gravityScale <- 1.0f; ValueNone
                | GravityOverride gravity -> bodyDef.gravityScale <- 0.0f; ValueSome gravity // NOTE: gravity overrides are handled by applying a manual force each step.
                | GravityScale scale -> bodyDef.gravityScale <- scale; ValueNone
                | GravityIgnore -> bodyDef.gravityScale <- 0.0f; ValueNone
            else ValueNone
        bodyDef.isBullet <- match bodyProperties.CollisionDetection with Continuous -> true | Discrete -> false
        bodyDef.isAwake <- bodyProperties.Awake
        bodyDef.userData <- bodyId

        // make the body
        let body = B2Bodies.b2CreateBody (physicsEngine.PhysicsContextId, &bodyDef)

        // attempt to attach body shape
        try Box2dNetPhysicsEngine.attachBodyShape bodyId.BodySource bodyProperties bodyProperties.BodyShape body
        with :? ArgumentOutOfRangeException -> ()

        // attempt to add the body
        let bodyId = { BodySource = createBodyMessage.BodyId.BodySource; BodyIndex = bodyProperties.BodyIndex }
        if physicsEngine.Bodies.TryAdd (bodyId, body) then
            match gravityOverrideOpt with
            | ValueSome gravityOverride ->
                physicsEngine.BodyGravityOverrides.[bodyId] <- gravityOverride
            | ValueNone -> ()
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
            B2Bodies.b2DestroyBody body
        | (false, _) -> ()

    static member private destroyBodies (destroyBodiesMessage : DestroyBodiesMessage) physicsEngine =
        List.iter (fun bodyId ->
            Box2dNetPhysicsEngine.destroyBody { BodyId = bodyId } physicsEngine)
            destroyBodiesMessage.BodyIds

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

        // log creation message
        for bodyTarget in [createBodyJointMessage.BodyJointProperties.BodyJointTarget; createBodyJointMessage.BodyJointProperties.BodyJointTarget2] do
            match physicsEngine.CreateBodyJointMessages.TryGetValue bodyTarget with
            | (true, messages) -> messages.Add createBodyJointMessage
            | (false, _) -> physicsEngine.CreateBodyJointMessages.Add (bodyTarget, List [createBodyJointMessage])

        // attempt to add body joint
        let bodyJointId = { BodyJointSource = createBodyJointMessage.BodyJointSource; BodyJointIndex = createBodyJointMessage.BodyJointProperties.BodyJointIndex }
        Box2dNetPhysicsEngine.createBodyJointInternal createBodyJointMessage.BodyJointProperties bodyJointId physicsEngine

    static member private destroyBodyJointInternal (bodyJointId : BodyJointId) physicsEngine =
        match physicsEngine.Joints.TryGetValue bodyJointId with
        | (true, joint) ->
            physicsEngine.Joints.Remove bodyJointId |> ignore<bool>
            physicsEngine.BreakableJoints.Remove bodyJointId |> ignore<bool>
            B2Joints.b2DestroyJoint joint
        | (false, _) -> ()

    static member private destroyBodyJoint (destroyBodyJointMessage : DestroyBodyJointMessage) physicsEngine =

        // unlog creation message
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
            if not (physicsEngine.FluidEmitters.ContainsKey id) then
                physicsEngine.FluidEmitters.Add (id, Box2dNetFluidEmitter.make descriptor physicsEngine.PhysicsContextId id.FluidEmitterSource)
                physicsEngine.FluidCollisions.Add (id, List ())
            Box2dNetFluidEmitter.addParticles (createFluidEmitterMessage.FluidParticles.GetEnumerator ()) physicsEngine.FluidEmitters.[id]
        | FluidEmitterDescriptorAether _ | FluidEmitterDescriptorJolt -> () // no support

    static member private destroyFluidEmitter (destroyFluidEmitterMessage : DestroyFluidEmitterMessage) physicsEngine =
        physicsEngine.FluidEmitters.Remove destroyFluidEmitterMessage.FluidEmitterId |> ignore<bool>
        physicsEngine.FluidCollisions.Remove destroyFluidEmitterMessage.FluidEmitterId |> ignore<bool>

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
        | (true, body) -> B2Bodies.b2Body_SetLinearVelocity (body, Box2dNetPhysicsEngine.toPhysicsV2 setBodyLinearVelocityMessage.LinearVelocity)
        | (false, _) -> ()

    static member private setBodyAngularVelocity (setBodyAngularVelocityMessage : SetBodyAngularVelocityMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyAngularVelocityMessage.BodyId with
        | (true, body) -> B2Bodies.b2Body_SetAngularVelocity (body, setBodyAngularVelocityMessage.AngularVelocity.Z)
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
 
    static member private getBodyContactNormals bodyId physicsEngine =
        let body = physicsEngine.Bodies.[bodyId]
        let capacity = B2Bodies.b2Body_GetContactCapacity body
        let contacts =
            if capacity > 20
            then (Array.zeroCreate capacity).AsSpan ()
            else Span (NativeInterop.NativePtr.stackalloc<B2ContactData> capacity |> NativeInterop.NativePtr.toVoidPtr, capacity)
        let contacts = contacts.Slice (0, B2Bodies.b2Body_GetContactData (body, contacts, capacity))
        let normals = Array.zeroCreate contacts.Length
        for i in 0 .. dec contacts.Length do
            let contact = &contacts.[i]
            let normal =
                if B2Shapes.b2Shape_GetBody contact.shapeIdA = body
                then -contact.manifold.normal // negate normal when contact stores this body as target
                else contact.manifold.normal
            normals.[i] <- Vector3 (normal.X, normal.Y, 0.0f)
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

    static member private jumpBody (jumpBodyMessage : JumpBodyMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue jumpBodyMessage.BodyId with
        | (true, body) ->
            let groundDirection = Box2dNetPhysicsEngine.getBodyGroundDirection jumpBodyMessage.BodyId physicsEngine
            if jumpBodyMessage.CanJumpInAir || Array.notEmpty (Box2dNetPhysicsEngine.getBodyToGroundContactNormals groundDirection jumpBodyMessage.BodyId physicsEngine) then
                B2Bodies.b2Body_ApplyLinearImpulseToCenter
                    (body,
                     Box2dNetPhysicsEngine.toPhysicsV2 (groundDirection * -jumpBodyMessage.JumpSpeed),
                     true)
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

    static let interpretManifold shapeIdA shapeIdB (manifold : B2Manifold byref) =
        struct {|
            Nearest =
                match manifold.pointCount with
                | 1 -> manifold.points.[0].point
                | 2 -> (manifold.points.[0].point + manifold.points.[1].point) * 0.5f
                | _ -> (B2Bodies.b2Body_GetPosition (B2Shapes.b2Shape_GetBody shapeIdA) + B2Bodies.b2Body_GetPosition (B2Shapes.b2Shape_GetBody shapeIdB)) * 0.5f // It's unlikely we need an accurate value in this case.
            Normal = manifold.normal |}
    // NOTE: from Box2D documentation https://box2d.org/documentation/md_simulation.html
    // update transforms - "Note that continuous collision does not generate events. Instead they are generated the next time step. However, continuous collision will issue a b2PreSolveFcn callback."
    // we want penetration messages to be recorded on the same time step as penetration instead of the next, so we record it in the b2PreSolveFcn callback.
    static let preSolveCallback =
        b2PreSolveFcn (fun shapeIdA shapeIdB manifold context -> // can be called from multiple threads at once - write to concurrent collections only
            let contactsTracker = context :?> Box2dNetPhysicsEngineContactsTracker
            if not (contactsTracker.ExistingContacts.Contains (shapeIdA, shapeIdB)) then
                contactsTracker.NewContacts.TryAdd (struct (shapeIdA, shapeIdB), interpretManifold shapeIdA shapeIdB &manifold) |> ignore<bool>
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
          Joints = Dictionary HashIdentity.Structural
          BreakableJoints = Dictionary HashIdentity.Structural
          CreateBodyJointMessages = Dictionary HashIdentity.Structural
          FluidEmitters = Dictionary<FluidEmitterId, Box2dNetFluidEmitter> HashIdentity.Structural
          FluidCollisions = Dictionary HashIdentity.Structural
          ContactsTracker = contactsTracker
          IntegrationMessages = List () } :> PhysicsEngine

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
            let groundNormals = (physicsEngine :> PhysicsEngine).GetBodyToGroundContactNormals bodyId
            Array.notEmpty groundNormals

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

                // collect transforms
                let bodyEvents = B2Worlds.b2World_GetBodyEvents physicsEngine.PhysicsContextId
                for i in 0 .. dec bodyEvents.moveCount do
                    let transform = &bodyEvents.moveEvents.[i]
                    match transform.userData with
                    | :? BodyId as bodyId ->
                        physicsEngine.IntegrationMessages.Add 
                            (BodyTransformMessage
                                { BodyId = bodyId
                                  Center = Box2dNetPhysicsEngine.toPixelV3 transform.transform.p
                                  Rotation = Box2dNetPhysicsEngine.rotToQuat transform.transform.q
                                  LinearVelocity = Box2dNetPhysicsEngine.toPixelV3 (B2Bodies.b2Body_GetLinearVelocity transform.bodyId)
                                  AngularVelocity = v3 0.0f 0.0f (B2Bodies.b2Body_GetAngularVelocity transform.bodyId) })
                    | _ -> () // fluid particle transforms are not reported as body transforms

                // collect penetrations and fluid collisions
                let contacts = B2Worlds.b2World_GetContactEvents physicsEngine.PhysicsContextId
                let addPenetration shapeIdA shapeIdB (contact : struct {| Nearest : B2Vec2; Normal : B2Vec2 |}) physicsEngine =
                    let mutable emittersEnr = physicsEngine.FluidEmitters.GetEnumerator ()
                    let mutable fluidFound = false
                    let mutable fluidCollider = Unchecked.defaultof<B2BodyId>
                    let mutable fluidCollidee = Unchecked.defaultof<B2ShapeId>
                    while emittersEnr.MoveNext () && not fluidFound do
                        let (KeyValue (emitterId, emitter)) = emittersEnr.Current
                        if emitter.OwnShapes.Contains shapeIdA then
                            fluidFound <- true
                            fluidCollider <- B2Shapes.b2Shape_GetBody shapeIdA
                            fluidCollidee <- shapeIdB
                        elif emitter.OwnShapes.Contains shapeIdB then
                            fluidFound <- true
                            fluidCollider <- B2Shapes.b2Shape_GetBody shapeIdB
                            fluidCollidee <- shapeIdA
                        if fluidFound then
                            physicsEngine.FluidCollisions.[emitterId].Add
                                { FluidCollider =
                                    { FluidParticlePosition = Box2dNetPhysicsEngine.toPixelV3 (B2Bodies.b2Body_GetPosition fluidCollider)
                                      FluidParticleVelocity = Box2dNetPhysicsEngine.toPixelV3 (B2Bodies.b2Body_GetLinearVelocity fluidCollider)
                                      FluidParticleConfig = B2Bodies.b2Body_GetName fluidCollider }
                                  FluidCollidee = B2Shapes.b2Shape_GetUserData fluidCollidee :?> BodyShapeIndex
                                  Nearest = Box2dNetPhysicsEngine.toPixelV3 contact.Nearest
                                  Normal = v3 contact.Normal.X contact.Normal.Y 0f }
                    if not fluidFound then
                        let bodyShapeA = B2Shapes.b2Shape_GetUserData shapeIdA :?> BodyShapeIndex
                        let bodyShapeB = B2Shapes.b2Shape_GetUserData shapeIdB :?> BodyShapeIndex
                        let normal = v3 contact.Normal.X contact.Normal.Y 0.0f
                        physicsEngine.IntegrationMessages.Add (BodyPenetrationMessage { BodyShapeSource = bodyShapeA; BodyShapeTarget = bodyShapeB; Normal = normal })
                        physicsEngine.IntegrationMessages.Add (BodyPenetrationMessage { BodyShapeSource = bodyShapeB; BodyShapeTarget = bodyShapeA; Normal = -normal })
                    
                if Constants.Physics.Collision2dFrameCompensation then

                    // collect penetrations for non-sensors from preSolveCallback which is called on same time step as penetration unlike contact events which exist one step later
                    for KeyValue ((shapeIdA, shapeIdB) as shapeIds, contact) in physicsEngine.ContactsTracker.NewContacts do
                        addPenetration shapeIdA shapeIdB contact physicsEngine
                        physicsEngine.ContactsTracker.ExistingContacts.Add shapeIds |> ignore
                    physicsEngine.ContactsTracker.NewContacts.Clear ()

                else

                    // collect penetrations for non-sensors from begin contact events, which are performant but one time step late compared to the actual penetration
                    for i in 0 .. dec contacts.beginCount do
                        let penetration = &contacts.beginEvents.[i]
                        addPenetration penetration.shapeIdA penetration.shapeIdB (interpretManifold penetration.shapeIdA penetration.shapeIdB &penetration.manifold) physicsEngine

                // collect separations for non-sensors
                for i in 0 .. dec contacts.endCount do
                    let separation = &contacts.endEvents.[i]
                    physicsEngine.ContactsTracker.ExistingContacts.Remove (separation.shapeIdA, separation.shapeIdB) |> ignore
                    let bodyShapeA = B2Shapes.b2Shape_GetUserData separation.shapeIdA :?> BodyShapeIndex
                    let bodyShapeB = B2Shapes.b2Shape_GetUserData separation.shapeIdB :?> BodyShapeIndex
                    physicsEngine.IntegrationMessages.Add (BodySeparationMessage { BodyShapeSource = bodyShapeA; BodyShapeTarget = bodyShapeB })
                    physicsEngine.IntegrationMessages.Add (BodySeparationMessage { BodyShapeSource = bodyShapeB; BodyShapeTarget = bodyShapeA })

                // collect penetrations for sensors
                let sensorEvents = B2Worlds.b2World_GetSensorEvents physicsEngine.PhysicsContextId
                for i in 0 .. dec sensorEvents.beginCount do
                    let sensorEvent = &sensorEvents.beginEvents.[i]
                    let bodyShapeA = B2Shapes.b2Shape_GetUserData sensorEvent.sensorShapeId :?> BodyShapeIndex
                    let bodyShapeB = B2Shapes.b2Shape_GetUserData sensorEvent.visitorShapeId :?> BodyShapeIndex
                    let normal = Box2dNetPhysicsEngine.computeCollisionNormalForSensors sensorEvent.sensorShapeId sensorEvent.visitorShapeId
                    let normal = Vector3 (normal.X, normal.Y, 0.0f)
                    physicsEngine.IntegrationMessages.Add (BodyPenetrationMessage { BodyShapeSource = bodyShapeA; BodyShapeTarget = bodyShapeB; Normal = normal })
                    physicsEngine.IntegrationMessages.Add (BodyPenetrationMessage { BodyShapeSource = bodyShapeB; BodyShapeTarget = bodyShapeA; Normal = -normal })

                // collect separations for sensors
                for i in 0 .. dec sensorEvents.endCount do
                    let sensorEvent = &sensorEvents.endEvents.[i]
                    let bodyShapeA = B2Shapes.b2Shape_GetUserData sensorEvent.sensorShapeId :?> BodyShapeIndex
                    let bodyShapeB = B2Shapes.b2Shape_GetUserData sensorEvent.visitorShapeId :?> BodyShapeIndex
                    physicsEngine.IntegrationMessages.Add (BodySeparationMessage { BodyShapeSource = bodyShapeA; BodyShapeTarget = bodyShapeB })
                    physicsEngine.IntegrationMessages.Add (BodySeparationMessage { BodyShapeSource = bodyShapeB; BodyShapeTarget = bodyShapeA })

                for KeyValue (emitterId, emitter) in physicsEngine.FluidEmitters do
                    let struct (particles, removedParticles) = Box2dNetFluidEmitter.postStep emitter
                    let fluidCollisions = physicsEngine.FluidCollisions.[emitterId]
                    physicsEngine.IntegrationMessages.Add
                        (FluidEmitterMessage
                            { FluidEmitterId = emitterId
                              FluidParticles = particles
                              OutOfBoundsParticles = removedParticles
                              FluidCollisions = SArray.ofSeq fluidCollisions })
                    fluidCollisions.Clear ()

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

            | _ -> ()

        member physicsEngine.ClearInternal () =
            physicsEngine.FluidEmitters.Clear ()
            physicsEngine.Joints.Clear ()
            physicsEngine.BreakableJoints.Clear ()
            physicsEngine.Bodies.Clear ()
            physicsEngine.BodyGravityOverrides.Clear ()
            physicsEngine.CreateBodyJointMessages.Clear ()
            let contextId = physicsEngine.PhysicsContextId
            physicsEngine.PhysicsContextId <- Box2dNetPhysicsEngine.makePhysicsContext (B2Worlds.b2World_GetGravity contextId) physicsEngine.ContactsTracker
            B2Worlds.b2DestroyWorld contextId

        member physicsEngine.CleanUp () =
            B2Worlds.b2DestroyWorld physicsEngine.PhysicsContextId
            physicsEngine.PhysicsContextId <- B2Ids.b2_nullWorldId // NOTE: Box2D.NET recommends nullifying references.