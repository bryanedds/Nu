// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Numerics
open System.Runtime.InteropServices
open System.Threading.Tasks
open Prime
open Box2D.NET
open Nu

type private Box2dNetKinematicCharacter =
    
    { (* Input Parameters *)
      PogoRestLength : single
      PogoFrequency : single
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
    
      (* Input-Output Parameters *)
      mutable Transform : B2Transform
      mutable Velocity : B2Vec2
      mutable OnGround : B2ShapeId voption
      mutable PogoVelocity : single

      (* Output Parameters *)
      mutable PogoOrigin : B2Vec2
      mutable PogoDelta : B2Vec2
      GroundCastResult : B2RayResult }

type private Box2dNetKinematicCollisionContext =
    { mutable Self : B2ShapeId
      PushLimits : Dictionary<B2BodyId, single> // TODO: replace with body shape def assignments once Box2D releases v3.2.
      PlaneResults : B2CollisionPlane List } // NOTE: fixed capacity as 8.

type private Box2dNetPhysicsEngineContactsTracker =
    { NewContacts : ConcurrentDictionary<struct (B2ShapeId * B2ShapeId), BodyPenetrationMessage>
      ExistingContacts : HashSet<struct (B2ShapeId * B2ShapeId)> }

/// Represents a neighbor particle during fluid simulation.
type [<Struct>] private Box2dNetFluidParticleNeighbor =
    { mutable ParticleIndex : int
      mutable AccumulatedImpulse : B2Vec2 } // Thread-safe accumulation, unlike in sfml-box2d-fluid

/// Represents the state of a fluid particle during simulation.
/// See sfml-box2d-fluid: GameObjects.h, struct Particle.
type private Box2dNetFluidParticleState =

    { (* Persistent State *)
      mutable Shape : B2ShapeId
      mutable Body : B2BodyId
      
      (* ComputeAccumulatedImpulses Internal Data *)
      Neighbors : Box2dNetFluidParticleNeighbor List
      mutable CachedPosition : B2Vec2
      mutable CachedVelocity : B2Vec2
      mutable CachedMass : single
      mutable CachedRadius : single
      mutable CachedCellId : Vector2i
      mutable CachedConfig : FluidParticleConfig
      
      (* ComputeAccumulatedImpulses Output *)
      mutable AccumulatedImpulse : B2Vec2 }

/// Represents a Box2D.NET fluid emitter.
/// Follows sfml-box2d-fluid implementation: https://github.com/a-piece-of-snake/sfml-box2d-fluid/tree/699c5c874969c9c270300bf33e60c0b2ecf77c72
/// NOTE: there is too much MAGIC (see comments) going on, this implementation will hopefully be superceded in a future Box2D.NET update.
type private Box2dNetFluidEmitter =
    { FluidEmitterDescriptor : Box2dNetFluidEmitterDescriptor
      PhysicsContextId : B2WorldId
      GravityOverrides : Dictionary<B2BodyId, B2Vec2>
      States : Box2dNetFluidParticleState array
      mutable StateCount : int
      Grid : Dictionary<Vector2i, int List>
      RemovedIndexes : int List // assumed to be ordered from smallest to largest
      BodySource : Simulant
      mutable NextBodyIndex : int }
        
    static let CellCapacityDefault = 16
    static let Neighborhood =
        [|for x in -1 .. 1 do
            for y in -1 .. 1 do
                v2i x y|]
      
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
            FluidParticleConfig.waterConfig

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
        else updateConfig state particle.FluidParticleConfig fluidEmitter
            
    static let fromFluid (state : Box2dNetFluidParticleState) =
        { FluidParticlePosition = toPixelV3 (B2Bodies.b2Body_GetPosition state.Body)
          FluidParticleVelocity = toPixelV3 (B2Bodies.b2Body_GetLinearVelocity state.Body)
          FluidParticleConfig = B2Bodies.b2Body_GetName state.Body }

    static member updateDescriptor (descriptor : Box2dNetFluidEmitterDescriptor) (fluidEmitter : Box2dNetFluidEmitter) =
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
        
        // process in reverse order to avoid removing last item after it already moved into earlier hole
        for j in dec fluidEmitter.RemovedIndexes.Count .. -1 .. 0 do
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
            else fluidEmitter.RemovedIndexes.Add i
        Box2dNetFluidEmitter.processRemovedIndexes fluidEmitter

    static member clearParticles (fluidEmitter : Box2dNetFluidEmitter) =
        for i in 0 .. dec fluidEmitter.StateCount do
            B2Bodies.b2DestroyBody fluidEmitter.States.[i].Body
            fluidEmitter.GravityOverrides.Remove fluidEmitter.States.[i].Body |> ignore<bool>
        fluidEmitter.StateCount <- 0

    static member getForce dst radius =

        // see sfml-box2d-fluid: GameObjects.cpp, ParticleGroup::GetForce (without caching)
        if dst >= radius then 0.0f else
        let x = radius - dst
        let x_pow2_5 = x * x * MathF.Sqrt x
        let radius_sq = radius * radius
        x_pow2_5 / (MathF.PI_OVER_4 * radius_sq * radius_sq) // MAGIC: this formula is pure magic (@.@)

    static member computeAccumulatedImpulses idx timestep fluidEmitter =

        // see sfml-box2d-fluid: GameObjects.cpp, ParticleGroup::ComputeChunkForces
        let state = fluidEmitter.States.[idx]
        if B2Bodies.b2Body_IsAwake state.Body then

            // gather neighbors and accumulate density
            let config = state.CachedConfig
            let radiusA = state.CachedRadius
            let range = radiusA * config.Impact
            let mutable density = 0.0f
            let posA = state.CachedPosition
            let mutable neighborSurfaceForce = B2MathFunction.b2Vec2_zero
            let velA = state.CachedVelocity
            for neighborCell in Neighborhood do
                match fluidEmitter.Grid.TryGetValue (state.CachedCellId + neighborCell) with
                | (true, list) ->
                    for otherIdx in list do
                        if idx <> otherIdx then
                            let otherState = fluidEmitter.States.[otherIdx]
                            let dist = B2MathFunction.b2Length (otherState.CachedPosition - posA)
                            if dist < range && dist > 0.001f then
                                let force = Box2dNetFluidEmitter.getForce dist range
                                density <- density + ((state.CachedMass + otherState.CachedMass) * 0.5f) * min config.ForceMax force // MAGIC: density depends on force? (@.@)
                                state.Neighbors.Add { ParticleIndex = otherIdx; AccumulatedImpulse = B2Vec2 () }
                | (false, _) -> ()
            let neighbors = CollectionsMarshal.AsSpan state.Neighbors

            // compute forces
            for neighborIdx in 0 .. dec neighbors.Length do
                let other = &neighbors.[neighborIdx]
                let otherState = fluidEmitter.States.[other.ParticleIndex]
                let posB = otherState.CachedPosition
                let offset = posB - posA
                let radiusB = otherState.CachedRadius
                
                // when in range...
                let offsetLength = B2MathFunction.b2Length offset
                if offsetLength < 0.001f then () else
                let dst = offsetLength / radiusA * config.Impact // MAGIC: dst scales inversely with radiusA but effectiveRange scales linearly with radiusA? (@.@)
                let effectiveRange = (radiusA + radiusB) * config.Impact
                if dst < effectiveRange then
                    let Density r = r * r * MathF.Sqrt r

                    // repulsion force
                    let forceDir = B2MathFunction.b2Normalize offset
                    let densityScale = B2MathFunction.b2ClampFloat (density, config.DensityMin, config.DensityMax)
                    let distanceForceMag = min (Box2dNetFluidEmitter.getForce dst effectiveRange) config.ForceMax * Density densityScale
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
                    totalForce <- B2MathFunction.b2Clamp (totalForce, B2Vec2 (-config.GetForceMax, -config.GetForceMax), B2Vec2 (config.GetForceMax, config.GetForceMax))
                    other.AccumulatedImpulse <- other.AccumulatedImpulse + totalForce
                    state.AccumulatedImpulse <- state.AccumulatedImpulse - totalForce

            // surface force
            if neighbors.Length > 0 then
                state.AccumulatedImpulse <- state.AccumulatedImpulse + neighborSurfaceForce * (timestep / single neighbors.Length)

            // adhesion force awaiting upstream implementation...
            ()

        // asleep
        else state.AccumulatedImpulse <- B2MathFunction.b2Vec2_zero
        
    static member preStep (clockDelta : single) (fluidEmitter : Box2dNetFluidEmitter) = // see sfml-box2d-fluid: GameObjects.cpp, ParticleGroup::ComputeChunkForces

        // OPTIMIZATION: early return when no particles (also applies to not enabled)
        if fluidEmitter.StateCount > 0 then

            // clear grid
            for cell in fluidEmitter.Grid.Values do
                cell.Clear ()

            // reset states and fill grid
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
            
            // remove empty cells
            for KeyValue (cellId, cell) in fluidEmitter.Grid do
                if cell.Count = 0 then
                    let result = fluidEmitter.Grid.Remove cellId
                    assert result

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

        // process and collect out of bounds particles
        let bounds = fluidEmitter.FluidEmitterDescriptor.SimulationBounds
        let aabb =
            B2AABB
                (B2Vec2 (bounds.Min.X / Constants.Engine.Meter2d, bounds.Min.Y / Constants.Engine.Meter2d),
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

        // fin
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

    static let rec [<TailCall>] spanAny i (span : _ Span) (discriminator : InRefFunc<_, _>) =
         i < span.Length && (discriminator.Invoke &span.[i] || spanAny (inc i) span discriminator)

    static member getFluidGrounded worldGravity fluidEmitter =
        let gravity = Gravity.localize worldGravity fluidEmitter.FluidEmitterDescriptor.Gravity
        // NOTE: we consider a particle to be on the ground if it has a contact with a non-fluid that isn't a sensor below it.
        let up = -gravity.Normalized
        spanAny 0 (fluidEmitter.States.AsSpan (0, fluidEmitter.StateCount)) (InRefFunc (fun state ->
            let body = state.Body
            let capacity = B2Bodies.b2Body_GetContactCapacity body
            let contacts =
                if capacity > 20
                then (Array.zeroCreate capacity).AsSpan ()
                else Span (NativeInterop.NativePtr.stackalloc<B2ContactData> capacity |> NativeInterop.NativePtr.toVoidPtr, capacity)
            spanAny 0 (contacts.Slice (0, B2Bodies.b2Body_GetContactData (body, contacts, capacity))) (InRefFunc (fun contact ->
                let shapeAIsSelf = B2Shapes.b2Shape_GetBody contact.shapeIdA = body
                let contactingGround =
                    let normal =
                        if shapeAIsSelf
                        then -contact.manifold.normal // normal points from shapeIdA to shapeIdB, so invert when body is shapeA
                        else contact.manifold.normal                                                                                                
                    let normal = Vector3 (normal.X, normal.Y, 0.0f)
                    // contactNormal and upDirection are normalized. -1 <= dot product <= 1. floating point imprecision is
                    // not a concern as NaN <= x is always false.
                    let projectionToUp = normal.Dot up
                    let theta = acos projectionToUp
                    theta <= Constants.Physics.GroundAngleMax
                let otherIsNotFluidOrSensor =
                    let shapeIdOther = if shapeAIsSelf then contact.shapeIdB else contact.shapeIdA
                    let bodyOther = B2Shapes.b2Shape_GetBody shapeIdOther
                    B2Bodies.b2Body_GetUserData bodyOther <> null && not (B2Shapes.b2Shape_IsSensor shapeIdOther)
                contactingGround && otherIsNotFluidOrSensor))))
and InRefFunc<'a, 'b> = delegate of 'a inref -> 'b

/// The Box2D.NET interface of PhysicsEngineRenderContext.
type Box2dNetPhysicsEngineRenderContext =
    inherit PhysicsEngineRenderContext
    abstract EyeBounds : Box2
    abstract DrawLine : Start : Vector2 * Stop : Vector2 * Color : Color -> unit
    abstract DrawCircle : Position : Vector2 * Radius : single * Color : Color -> unit

/// The Box2D.NET implementation of PhysicsEngine.
type [<ReferenceEquality>] Box2dNetPhysicsEngine =
    private
        { mutable PhysicsContextId : B2WorldId
          Bodies : Dictionary<BodyId, B2BodyId>
          BodyGravityOverrides : Dictionary<BodyId, Vector3>
          Characters : Dictionary<BodyId, Box2dNetKinematicCharacter>
          Joints : Dictionary<BodyJointId, B2JointId>
          BreakableJoints : Dictionary<BodyJointId, struct {| BreakingPoint : single; BreakingPointSquared : single |}>
          CreateBodyJointMessages : Dictionary<BodyId, CreateBodyJointMessage List>
          FluidEmitters : Dictionary<FluidEmitterId, Box2dNetFluidEmitter>
          KinematicCollisionContext : Box2dNetKinematicCollisionContext // OPTIMIZATION: presumably cached to avoid allocation?
          IntegrationMessages : IntegrationMessage List // OPTIMIZATION: cached to avoid large arrays filling up the LOH.
          ContactsTracker : Box2dNetPhysicsEngineContactsTracker } // NOTE: supports thread safety for b2PreSolveFcn.

    static member private toPixel value =
        value * Constants.Engine.Meter2d

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

        | ContourShape _ -> // implement using multiple shape casts against each link on the contour
            Log.warn "ContourShape casting is not implemented in Box2dNetPhysicsEngine."
            false

        | PointsShape { Profile = Convex; Points = points; TransformOpt = transformOpt }
        | GeometryShape { Profile = Convex; Vertices = points; TransformOpt = transformOpt } -> // even if the points are non-convex, Box2D's shape cast will use the convex hull implicitly
            let transformOpt = Option.map2 Affine.combineAsMatrix transformOpt extraTransformOpt
            if points.Length > B2Constants.B2_MAX_POLYGON_VERTICES then
                Log.warn $"2D Convex PointsShape has too many points (%d{points.Length}) for Box2D shape casting. Truncating to %d{B2Constants.B2_MAX_POLYGON_VERTICES}."
            proxy.count <- min B2Constants.B2_MAX_POLYGON_VERTICES points.Length
            for i in 0 .. dec proxy.count do
                proxy.points.[i] <- (points.[i], transformOpt) ||> Option.fold _.Transform |> (+) origin |> Box2dNetPhysicsEngine.toPhysicsV2
            true

        | PointsShape { Profile = Concave }
        | GeometryShape { Profile = Concave } -> // implement using multiple shape casts against each triangle of the concave shape
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

        | BodyShapes _ -> // implement using multiple shape casts against each shape
            Log.warn "BodyShapes casting is not implemented in Box2dNetPhysicsEngine."
            false

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
                let bodyShapeIndex = { BodyShapeSource = bodyShapeA; BodyShapeTarget = bodyShapeB; Normal = normal }
                contactsTracker.NewContacts.TryAdd (struct (shapeIdA, shapeIdB), bodyShapeIndex) |> ignore
            true)
          
    static let characterGroundCastCallback =
        b2CastResultFcn (fun shapeId point normal fraction context ->
            let character = context :?> Box2dNetKinematicCharacter
            if B2Ids.B2_ID_EQUALS (shapeId, character.CapsuleShape) then -1.0f else
            let result = character.GroundCastResult
            result.hit <- true
            result.shapeId <- shapeId
            result.point <- point
            result.normal <- normal
            result.fraction <- fraction
            fraction)

    static let characterCollisionCallback =
        b2PlaneResultFcn (fun shapeId planeResult context ->
            assert planeResult.hit
            let context = context :?> Box2dNetKinematicCollisionContext
            if not (B2Ids.B2_ID_EQUALS (context.Self, shapeId)) && context.PlaneResults.Count < context.PlaneResults.Capacity then
                assert B2MathFunction.b2IsValidPlane planeResult.plane
                let plane =
                    match context.PushLimits.TryGetValue (B2Shapes.b2Shape_GetBody shapeId) with
                    | (true, pushLimit) -> B2CollisionPlane (planeResult.plane, pushLimit, 0.0f, false) // soft collision - don't clip velocity to plane, multi-frame position correction via push limit
                    | (false, _) -> B2CollisionPlane (planeResult.plane, Single.MaxValue, 0.0f, true) // hard collision - clip velocity to plane, immediate position correction with MaxValue push limit
                context.PlaneResults.Add plane
            true)

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
            let scaleY = Option.mapOrDefaultValue (fun (transform : Affine) -> transform.Scale.Y) 1.0f transformOpt
            let height = Box2dNetPhysicsEngine.toPhysicsPolygonDiameter (height * scaleY)
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
        let mutable hull =
            B2Hulls.b2ComputeHull (points'.AsSpan (), points'.Length)
        if hull.count > 0 then
            let mutable shapeDef = Unchecked.defaultof<_>
            configureBodyShapeProperties &shapeDef bodySource bodyProperties propertiesOpt
            shapeDef.density <-
                match bodyProperties.Substance with
                | Density density -> density
                | Mass mass ->
                    let mutable doubleArea = 0.0f // triangulate the polygon to compute area: https://github.com/ikpil/Box2D.NET/blob/bb5a9bb4b40d27007fb634c686f55eaa4a01ca52/src/Box2D.NET/B2Geometries.cs#L382-L395
                    let r = points'.[0]
                    for i in 1 .. points'.Length - 2 do
                        let e1 = points'.[i] - r
                        let e2 = points'.[i + 1] - r
                        doubleArea <- doubleArea + B2MathFunction.b2Cross (e1, e2)
                    mass * 2.0f / doubleArea
            let mutable polygon = B2Geometries.b2MakePolygon (&hull, 0.0f)
            B2Shapes.b2CreatePolygonShape (body, &shapeDef, &polygon) |> ignore<B2ShapeId>
        else Log.warn $"Failed to create convex hull polygon for {scstring points}. Maybe your points are too close together, are collinear, or consists of < 3 or > 8 points (please decompose them into smaller polygons)?"

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
        bodyDef.angularDamping <- bodyProperties.AngularDamping
        bodyDef.fixedRotation <- bodyProperties.AngularFactor.Z = 0.0f
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

            // register any gravity override
            match gravityOverrideOpt with
            | ValueSome gravityOverride ->
                physicsEngine.BodyGravityOverrides.[bodyId] <- gravityOverride
            | ValueNone -> ()

            // register any kinematic push limit
            match bodyProperties.KinematicPushLimitOpt with
            | Some pushLimit ->
                physicsEngine.KinematicCollisionContext.PushLimits.[body] <- Box2dNetPhysicsEngine.toPhysics pushLimit
            | None -> ()

            // attach body shape as appropriate
            if isCharacter then
                match bodyProperties.BodyShape with
                | CapsuleShape capsuleShape ->
                    match bodyProperties.CharacterProperties with
                    | PogoSpringCharacterProperties properties ->

                        // ensure center1 is at the bottom for pogo casting
                        let mutable capsule = Box2dNetPhysicsEngine.toPhysicsCapsule capsuleShape
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
                                
                        // create the body shape
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

                        // add the character to the bookkeeping
                        physicsEngine.Characters.[bodyId] <-
                            { PogoRestLength = pogoRestLength
                              PogoFrequency = properties.PogoFrequency
                              PogoDampingRatio = properties.PogoDampingRatio
                              PogoProxy =
                                match properties.PogoShapeCast with
                                | PointPogoShapeCast -> B2Distances.b2MakeProxy (B2Vec2 (), 1, 0.0f)
                                | CirclePogoShapeCast diameterScalar -> B2Distances.b2MakeProxy (B2Vec2 (), 1, diameterScalar * 0.5f * capsule.radius)
                                | SegmentPogoShapeCast widthScalar ->
                                    let segmentOffset = B2Vec2 (widthScalar * capsule.radius, 0.0f)
                                    B2Distances.b2MakeProxy (-segmentOffset, segmentOffset, 2, 0.0f)
                              CollisionCategory = shapeDef.filter.categoryBits
                              CastMask = shapeDef.filter.maskBits
                              CollisionMask = shapeDef.filter.maskBits ||| properties.AdditionalCollisionMask
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

                    // stair-stepping unimplemented (but is it even needed with the pogo spring implementation?)
                    | StairSteppingCharacterProperties _ -> Log.warn "StairSteppingCharacterProperties not yet implemented in Box2dPhysicsEngine."
                
                // not a capsule shape - unsupported
                | shape -> Log.warn ("Characters in Box2dNetPhysicsEngine only support CapsuleShapes. Ignoring character body type for " + scstring shape + ".")
            
            // otherwise just attach the body shape normally
            else Box2dNetPhysicsEngine.attachBodyShape bodyId.BodySource bodyProperties bodyProperties.BodyShape body
        
        // failed to add body
        else Log.error ("Could not add body for '" + scstring bodyId + "' as it already exists.")

        // attempt to run any related body joint creation functions
        match physicsEngine.CreateBodyJointMessages.TryGetValue bodyId with
        | (true, createBodyJointMessages) ->
            for createBodyJointMessage in createBodyJointMessages do
                let bodyJointId = { BodyJointSource = createBodyJointMessage.BodyJointSource; BodyJointIndex = createBodyJointMessage.BodyJointProperties.BodyJointIndex }
                Box2dNetPhysicsEngine.destroyBodyJointInternal bodyJointId physicsEngine
                Box2dNetPhysicsEngine.createBodyJointInternal createBodyJointMessage.BodyJointProperties bodyJointId physicsEngine
        | (false, _) -> ()

    static member private createBodies (createBodiesMessage : CreateBodiesMessage) physicsEngine =
        for bodyProperties in createBodiesMessage.BodiesProperties do
            let createBodyMessage =
                { BodyId = { BodySource = createBodiesMessage.BodySource; BodyIndex = bodyProperties.BodyIndex }
                  BodyProperties = bodyProperties }
            Box2dNetPhysicsEngine.createBody createBodyMessage physicsEngine

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
            physicsEngine.KinematicCollisionContext.PushLimits.Remove body |> ignore<bool>
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
        
    static member private destroyBodyJointInternal (bodyJointId : BodyJointId) physicsEngine =

        // unlike destroyBodyJoint, whether the body joint is re-created on connected body re-creation is unchanged
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
        | Box2dNetFluidEmitterDescriptor descriptor ->
            if not (physicsEngine.FluidEmitters.ContainsKey id) then physicsEngine.FluidEmitters.Add (id, Box2dNetFluidEmitter.make descriptor physicsEngine.PhysicsContextId id.FluidEmitterSource)
            Box2dNetFluidEmitter.addParticles (createFluidEmitterMessage.FluidParticles.GetEnumerator ()) physicsEngine.FluidEmitters.[id]
        | _ -> () // unsupported. Log?

    static member private destroyFluidEmitter (destroyFluidEmitterMessage : DestroyFluidEmitterMessage) physicsEngine =
        physicsEngine.FluidEmitters.Remove destroyFluidEmitterMessage.FluidEmitterId |> ignore<bool>

    static member private setBodyEnabled (setBodyEnabledMessage : SetBodyEnabledMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyEnabledMessage.BodyId with
        | (true, body) ->
            if setBodyEnabledMessage.BodyEnabled
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
            if setBodyJointMotorEnabledMessage.MotorEnabled then B2Joints.b2Joint_WakeBodies joint
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
            if setBodyJointMotorSpeedMessage.MotorSpeed <> 0.0f then B2Joints.b2Joint_WakeBodies joint
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
        | (true, character) ->
            match character.OnGround with
            | ValueSome _ -> ValueSome character.GroundCastResult.normal
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
                then -contact.manifold.normal // normal points from shapeIdA to shapeIdB, so invert when body is shapeA
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

        // attempt to get ground contact normal from character ground cast, computing normals from body contacts otherwise
        match Box2dNetPhysicsEngine.getCharacterGroundContactNormalOpt bodyId physicsEngine with
        | ValueSome normal -> [|v3 normal.X normal.Y 0.0f|]
        | ValueNone ->

            // assert ground angle max is within expected range
            if Constants.Physics.GroundAngleMax >= MathF.PI_OVER_2 then
                Log.warnOnce "Constants.Physics.GroundAngleMax allows wall jumping without pushing back against the wall."

            // get contact normals from body contacts
            let up = -groundDirection
            physicsEngine
            |> Box2dNetPhysicsEngine.getBodyContactNormals bodyId
            |> Array.filter (fun contactNormal ->

                // contactNormal and upDirection are normalized. -1 <= dot product <= 1. floating point imprecision is
                // not a concern as NaN <= x is always false.
                let projectionToUp = contactNormal.Dot up
                let theta = acos projectionToUp
                theta <= Constants.Physics.GroundAngleMax)

    static member private getBodyToGroundContactNormalOpt bodyId physicsEngine =
        let groundDirection = Box2dNetPhysicsEngine.getBodyGroundDirection bodyId physicsEngine 
        match Box2dNetPhysicsEngine.getBodyToGroundContactNormals groundDirection bodyId physicsEngine with
        | [||] -> None
        | groundNormals -> groundNormals |> Array.maxBy (fun normal -> normal.Dot groundDirection) |> Some

    static member private getBodyGrounded groundDirection bodyId physicsEngine =
        match physicsEngine.Characters.TryGetValue bodyId with
        | (true, character) -> ValueOption.isSome character.OnGround
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
                | (true, character) -> character.OnGround <- ValueNone
                | (false, _) -> ()
        | (false, _) -> ()

    static member private updateFluidEmitterMessage (updateFluidEmitterMessage : UpdateFluidEmitterMessage) physicsEngine =
        let id = updateFluidEmitterMessage.FluidEmitterId
        match physicsEngine.FluidEmitters.TryGetValue id with
        | (true, emitter) ->
            match updateFluidEmitterMessage.FluidEmitterDescriptor with
            | Box2dNetFluidEmitterDescriptor descriptor ->
                physicsEngine.FluidEmitters.[id] <- Box2dNetFluidEmitter.updateDescriptor descriptor emitter
            | _ -> () // unsupported. Log?
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

        // set gravity
        B2Worlds.b2World_SetGravity (physicsEngine.PhysicsContextId, Box2dNetPhysicsEngine.toPhysicsV2 gravity)

        // forcibly wake all bodies (could slow down sim too much when a lot of bodies?)
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

    static member private makePhysicsContext gravity (contactsTracker : Box2dNetPhysicsEngineContactsTracker) =
        let mutable worldDef = B2Types.b2DefaultWorldDef ()
        worldDef.gravity <- gravity
        let world = B2Worlds.b2CreateWorld &worldDef
        B2Worlds.b2World_SetPreSolveCallback (world, preSolveCallback, contactsTracker)
        world

    /// Make a physics engine.
    static member make gravity =
        let contactsTracker =
            { NewContacts = ConcurrentDictionary ()
              ExistingContacts = HashSet () }
        let collisionContext =
            { Self = B2Ids.b2_nullShapeId
              PushLimits = Dictionary HashIdentity.Structural
              PlaneResults = List 8 }
        { PhysicsContextId = Box2dNetPhysicsEngine.makePhysicsContext (Box2dNetPhysicsEngine.toPhysicsV2 gravity) contactsTracker
          Bodies = Dictionary HashIdentity.Structural
          BodyGravityOverrides = Dictionary HashIdentity.Structural
          Characters = Dictionary HashIdentity.Structural
          Joints = Dictionary HashIdentity.Structural
          BreakableJoints = Dictionary HashIdentity.Structural
          CreateBodyJointMessages = Dictionary HashIdentity.Structural
          FluidEmitters = Dictionary<FluidEmitterId, Box2dNetFluidEmitter> HashIdentity.Structural
          ContactsTracker = contactsTracker
          KinematicCollisionContext = collisionContext
          IntegrationMessages = List () } :> PhysicsEngine

    // https://github.com/ikpil/Box2D.NET/blob/1881d86d07a9f1174a199c6c616e19379056bf10/src/Box2D.NET.Samples/Samples/Characters/Mover.cs#L280-L460
    static member private solveCharacter (character : Box2dNetKinematicCharacter) timeStep world collisionContext =
        
        // mover overlap filter, should include other movers
        let collideFilter = B2QueryFilter (character.CollisionCategory, character.CollisionMask)
        
        // movers shouldn't sweep against other movers to allow for soft collision
        let castFilter = B2QueryFilter (character.CollisionCategory, character.CastMask)
        
        // initialize gravity
        let gravity = character.GravityScale * B2Worlds.b2World_GetGravity world + character.GravityOverride
        let gravityDirection = B2MathFunction.b2Normalize gravity
        if ValueOption.isSome character.OnGround then // when on ground, stabilize pogo spring by clearing velocity in the direction of gravity
            let movementAxis = B2Vec2 (-gravityDirection.Y, gravityDirection.X) // perpendicular to gravity
            character.Velocity <- B2MathFunction.b2Dot (character.Velocity, movementAxis) * movementAxis // project m.Velocity onto movementAxis
        character.Velocity <- character.Velocity + timeStep * gravity

        // pogo cast downward from bottom point of capsule
        let rayLength = character.PogoRestLength + character.Capsule.radius
        character.PogoOrigin <- B2MathFunction.b2TransformPoint (&character.Transform, character.Capsule.center1)
        let mutable proxy = character.PogoProxy
        for i in 0 .. dec proxy.count do
            proxy.points.[i] <- proxy.points.[i] + character.PogoOrigin // apply origin to proxy
        let translation = (rayLength - proxy.radius) * gravityDirection
        let castResult = character.GroundCastResult
        castResult.hit <- false
        B2Worlds.b2World_CastShape (world, &proxy, translation, castFilter, characterGroundCastCallback, character) |> ignore<B2TreeStats>

        // avoid snapping to ground if still going opposite of gravity with magnitude at least 0.01
        character.OnGround <-
            if  castResult.hit && (ValueOption.isSome character.OnGround ||
                let gravityProjection = B2MathFunction.b2Dot (character.Velocity, gravityDirection)
                gravityProjection >= -0.01f)
            then ValueSome castResult.shapeId
            else ValueNone

        // solve pogo state and apply pogo gravity to ground contact
        if not castResult.hit then
            character.PogoVelocity <- 0.0f
            character.PogoDelta <- translation
        else
            let pogoCurrentLength = castResult.fraction * rayLength - character.Capsule.radius
            let offset = pogoCurrentLength - character.PogoRestLength
            character.PogoVelocity <- B2MathFunction.b2SpringDamper (character.PogoFrequency, character.PogoDampingRatio, offset, character.PogoVelocity, timeStep)
            character.PogoDelta <- castResult.fraction * translation
            B2Bodies.b2Body_ApplyForce (B2Shapes.b2Shape_GetBody castResult.shapeId, character.Mass * gravity, castResult.point, true)

        // solve collision planes for projected new position
        // TODO: is it possible to project external forces for dynamic characters?
        let target = character.Transform.p + timeStep * character.Velocity + timeStep * character.PogoVelocity * -gravityDirection
        let toleranceSquared = 0.01f * 0.01f
        let mutable i = 0
        let planeResults = collisionContext.PlaneResults
        while i < 5 do
            planeResults.Clear ()
            let mutable mover =
                B2Capsule
                    (B2MathFunction.b2TransformPoint (&character.Transform, character.Capsule.center1),
                     B2MathFunction.b2TransformPoint (&character.Transform, character.Capsule.center2),
                     character.Capsule.radius)
            collisionContext.Self <- character.CapsuleShape
            B2Worlds.b2World_CollideMover (world, &mover, collideFilter, characterCollisionCallback, (collisionContext : Box2dNetKinematicCollisionContext))
            let result = B2Movers.b2SolvePlanes (target - character.Transform.p, CollectionsMarshal.AsSpan planeResults, planeResults.Count)
            let fraction = B2Worlds.b2World_CastMover (world, &mover, result.translation, castFilter)
            let delta = fraction * result.translation
            character.Transform.p <- character.Transform.p + delta
            if B2MathFunction.b2LengthSquared delta < toleranceSquared
            then i <- 5
            else i <- i + 1

        // clip velocity against collision planes
        character.Velocity <- B2Movers.b2ClipVector (character.Velocity, CollectionsMarshal.AsSpan planeResults, planeResults.Count)

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

        member physicsEngine.GetFluidExists fluidEmitterId =
            physicsEngine.FluidEmitters.ContainsKey fluidEmitterId

        member physicsEngine.GetFluidGrounded fluidEmitterId =
            match physicsEngine.FluidEmitters.TryGetValue fluidEmitterId with
            | (true, emitter) ->
                Box2dNetFluidEmitter.getFluidGrounded (physicsEngine :> PhysicsEngine).Gravity emitter
            | (false, _) -> false

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

                // pre-step the fluid simulation
                for emitter in physicsEngine.FluidEmitters.Values do
                    Box2dNetFluidEmitter.preStep stepTime emitter

                // step the world
                B2Worlds.b2World_Step (physicsEngine.PhysicsContextId, stepTime, Constants.Physics.Collision2dSteps)

                // collect fluid emitter results
                for KeyValue (emitterId, emitter) in physicsEngine.FluidEmitters do
                    let struct (particles, removedParticles) = Box2dNetFluidEmitter.postStep emitter
                    physicsEngine.IntegrationMessages.Add
                        (FluidEmitterMessage
                            { FluidEmitterId = emitterId
                              FluidParticles = particles
                              OutOfBoundsParticles = removedParticles })
                
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

                // step characters
                for KeyValue (bodyId, character) in physicsEngine.Characters do

                    // step character simulations
                    let body = physicsEngine.Bodies.[bodyId]
                    character.Transform <- B2Bodies.b2Body_GetTransform body
                    character.Velocity <- B2Bodies.b2Body_GetLinearVelocity body
                    let oldGround = character.OnGround
                    Box2dNetPhysicsEngine.solveCharacter character stepTime physicsEngine.PhysicsContextId physicsEngine.KinematicCollisionContext
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

            // render when context matches
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
                for KeyValue (bodyId, character) in physicsEngine.Characters do
                    let aabb = B2Bodies.b2Body_ComputeAABB physicsEngine.Bodies.[bodyId]
                    if B2MathFunction.b2AABB_Overlaps (eyeAabb, aabb) then
                        let pogoStop = character.PogoOrigin + character.PogoDelta

                        // pogo spring
                        let start = Box2dNetPhysicsEngine.toPixelV2 character.PogoOrigin
                        let stop = Box2dNetPhysicsEngine.toPixelV2 pogoStop
                        renderContext.DrawLine (start, stop, Color.Plum)

                        // pogo
                        let radius = Box2dNetPhysicsEngine.toPixel character.PogoProxy.radius
                        for i in 0 .. dec character.PogoProxy.count do
                            let start = character.PogoProxy.points.[i] + pogoStop |> Box2dNetPhysicsEngine.toPixelV2
                            let stop = character.PogoProxy.points.[if i < dec character.PogoProxy.count then inc i else 0] + pogoStop |> Box2dNetPhysicsEngine.toPixelV2
                            renderContext.DrawLine (start, stop, Color.Plum)
                            if radius <> 0.0f then
                                renderContext.DrawCircle (start, radius, Color.Plum)
            
            // context doesn't match (TODO: log?)
            | _ -> ()

        member physicsEngine.ClearInternal () =
            physicsEngine.FluidEmitters.Clear ()
            physicsEngine.Joints.Clear ()
            physicsEngine.BreakableJoints.Clear ()
            physicsEngine.Bodies.Clear ()
            physicsEngine.BodyGravityOverrides.Clear ()
            physicsEngine.Characters.Clear ()
            physicsEngine.KinematicCollisionContext.PushLimits.Clear ()
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
    member this.AwakeBodyCount = B2Worlds.b2World_GetAwakeBodyCount impl
    member this.BodyEvents = B2Worlds.b2World_GetBodyEvents impl
    member this.ContactEvents = B2Worlds.b2World_GetContactEvents impl
    member this.Counters = B2Worlds.b2World_GetCounters impl
    member this.Gravity with get () = B2Worlds.b2World_GetGravity impl and set value = B2Worlds.b2World_SetGravity (impl, value)
    member this.HitEventThreshold with get () = B2Worlds.b2World_GetHitEventThreshold impl and set value = B2Worlds.b2World_SetHitEventThreshold (impl, value)
    member this.IsContinuousEnabled with get () = B2Worlds.b2World_IsContinuousEnabled impl and set value = B2Worlds.b2World_EnableContinuous (impl, value)
    member this.IsSleepingEnabled with get () = B2Worlds.b2World_IsSleepingEnabled impl and set value = B2Worlds.b2World_EnableSleeping (impl, value)
    member this.IsValid = B2Worlds.b2World_IsValid impl
    member this.IsWarmStartingEnabled with get () = B2Worlds.b2World_IsWarmStartingEnabled impl and set value = B2Worlds.b2World_EnableWarmStarting (impl, value)
    member this.JointEvents = B2Worlds.b2World_GetJointEvents impl
    member this.MaximumLinearSpeed with get () = B2Worlds.b2World_GetMaximumLinearSpeed impl and set value = B2Worlds.b2World_SetMaximumLinearSpeed (impl, value)
    member this.Profile = B2Worlds.b2World_GetProfile impl
    member this.RestitutionThreshold with get () = B2Worlds.b2World_GetRestitutionThreshold impl and set value = B2Worlds.b2World_SetRestitutionThreshold (impl, value)
    member this.SensorEvents = B2Worlds.b2World_GetSensorEvents impl
    member this.UserData with get () = B2Worlds.b2World_GetUserData impl and set value = B2Worlds.b2World_SetUserData (impl, value)

type B2BodyIdDebuggerDisplay (impl) =
    member this.AngularDamping with get () = B2Bodies.b2Body_GetAngularDamping impl and set value = B2Bodies.b2Body_SetAngularDamping (impl, value)
    member this.AngularVelocity with get () = B2Bodies.b2Body_GetAngularVelocity impl and set value = B2Bodies.b2Body_SetAngularVelocity (impl, value)
    member this.ContactCapacity = B2Bodies.b2Body_GetContactCapacity impl
    member this.ContactData =
        let capacity = B2Bodies.b2Body_GetContactCapacity impl
        let contacts = Array.zeroCreate capacity
        contacts.[0 .. B2Bodies.b2Body_GetContactData (impl, contacts.AsSpan (), capacity) - 1]
    member this.GravityScale with get () = B2Bodies.b2Body_GetGravityScale impl and set value = B2Bodies.b2Body_SetGravityScale (impl, value)
    member this.IsAwake with get () = B2Bodies.b2Body_IsAwake impl and set value = B2Bodies.b2Body_SetAwake (impl, value)
    member this.IsBullet with get () = B2Bodies.b2Body_IsBullet impl and set value = B2Bodies.b2Body_SetBullet (impl, value)
    member this.IsEnabled with get () = B2Bodies.b2Body_IsEnabled impl and set value = if value then B2Bodies.b2Body_Enable impl else B2Bodies.b2Body_Disable impl
    member this.IsFixedRotation with get () = B2Bodies.b2Body_IsFixedRotation impl and set value = B2Bodies.b2Body_SetFixedRotation (impl, value)
    member this.IsSleepEnabled with get () = B2Bodies.b2Body_IsSleepEnabled impl and set value = B2Bodies.b2Body_EnableSleep (impl, value)
    member this.IsValid = B2Worlds.b2Body_IsValid impl
    member this.Joints =
        let capacity = B2Bodies.b2Body_GetJointCount impl
        let joints = Array.zeroCreate capacity
        B2Bodies.b2Body_GetJoints (impl, joints.AsSpan(), capacity) |> ignore<int>
        joints
    member this.JointCount = B2Bodies.b2Body_GetJointCount impl
    member this.LinearDamping with get () = B2Bodies.b2Body_GetLinearDamping impl and set value = B2Bodies.b2Body_SetLinearDamping (impl, value)
    member this.LinearVelocity with get () = B2Bodies.b2Body_GetLinearVelocity impl and set value = B2Bodies.b2Body_SetLinearVelocity (impl, value)
    member this.LocalCenterOfMass
        with get () = B2Bodies.b2Body_GetLocalCenterOfMass impl
        and set value =
            let mutable massData = B2Bodies.b2Body_GetMassData impl
            massData.center <- value
            B2Bodies.b2Body_SetMassData (impl, massData)
    member this.Mass
        with get () = B2Bodies.b2Body_GetMass impl
        and set value =
            let mutable massData = B2Bodies.b2Body_GetMassData impl
            massData.mass <- value
            B2Bodies.b2Body_SetMassData (impl, massData)
    member this.MassData with get () = B2Bodies.b2Body_GetMassData impl and set value = B2Bodies.b2Body_SetMassData (impl, value)
    member this.Name with get () = B2Bodies.b2Body_GetName impl and set value = B2Bodies.b2Body_SetName (impl, value)
    member this.Position with get () = B2Bodies.b2Body_GetPosition impl and set value = B2Bodies.b2Body_SetTransform (impl, value, B2Bodies.b2Body_GetRotation impl)
    member this.Rotation with get () = B2Bodies.b2Body_GetRotation impl and set value = B2Bodies.b2Body_SetTransform (impl, B2Bodies.b2Body_GetPosition impl, value)
    member this.RotationalInertia
        with get () = B2Bodies.b2Body_GetRotationalInertia impl
        and set value =
            let mutable massData = B2Bodies.b2Body_GetMassData impl
            massData.rotationalInertia <- value
            B2Bodies.b2Body_SetMassData (impl, massData)
    member this.Shapes =
        let capacity = B2Bodies.b2Body_GetShapeCount impl
        let shapes = Array.zeroCreate capacity
        B2Bodies.b2Body_GetShapes (impl, shapes.AsSpan(), capacity) |> ignore<int>
        shapes
    member this.ShapeCount = B2Bodies.b2Body_GetShapeCount impl
    member this.SleepThreshold with get () = B2Bodies.b2Body_GetSleepThreshold impl and set value = B2Bodies.b2Body_SetSleepThreshold (impl, value)
    member this.Transform with get () = B2Bodies.b2Body_GetTransform impl and set (value : B2Transform) = B2Bodies.b2Body_SetTransform (impl, value.p, value.q)
    member this.Type with get () = B2Bodies.b2Body_GetType impl and set value = B2Bodies.b2Body_SetType (impl, value)
    member this.UserData with get () = B2Bodies.b2Body_GetUserData impl and set value = B2Bodies.b2Body_SetUserData (impl, value)
    member this.World = B2Bodies.b2Body_GetWorld impl
    member this.WorldCenterOfMass = B2Bodies.b2Body_GetWorldCenterOfMass impl
    
type B2ShapeIdDebuggerDisplay (impl) =
    member this.AABB = B2Shapes.b2Shape_GetAABB impl
    member this.AreContactEventsEnabled with get () = B2Shapes.b2Shape_AreContactEventsEnabled impl and set value = B2Shapes.b2Shape_EnableContactEvents (impl, value)
    member this.AreHitEventsEnabled with get () = B2Shapes.b2Shape_AreHitEventsEnabled impl and set value = B2Shapes.b2Shape_EnableHitEvents (impl, value)
    member this.ArePreSolveEventsEnabled with get () = B2Shapes.b2Shape_ArePreSolveEventsEnabled impl and set value = B2Shapes.b2Shape_EnablePreSolveEvents (impl, value)
    member this.AreSensorEventsEnabled with get () = B2Shapes.b2Shape_AreSensorEventsEnabled impl and set value = B2Shapes.b2Shape_EnableSensorEvents (impl, value)
    member this.Body = B2Shapes.b2Shape_GetBody impl
    member this.Capsule
        with get () = if B2Shapes.b2Shape_GetType impl = B2ShapeType.b2_capsuleShape then B2Shapes.b2Shape_GetCapsule impl else failwith "Not a capsule"
        and set value = let mutable value = value in B2Shapes.b2Shape_SetCapsule (impl, &value)
    member this.ChainSegment with get () = if B2Shapes.b2Shape_GetType impl = B2ShapeType.b2_chainSegmentShape then B2Shapes.b2Shape_GetChainSegment impl else failwith "Not a chain segment"
    member this.Circle
        with get () = if B2Shapes.b2Shape_GetType impl = B2ShapeType.b2_circleShape then B2Shapes.b2Shape_GetCircle impl else failwith "Not a circle"
        and set value = let mutable value = value in B2Shapes.b2Shape_SetCircle (impl, &value)
    member this.ContactCapacity = B2Shapes.b2Shape_GetContactCapacity impl
    member this.ContactData =
        let capacity = B2Shapes.b2Shape_GetContactCapacity impl
        let contacts = Array.zeroCreate capacity
        contacts.[0 .. B2Shapes.b2Shape_GetContactData (impl, contacts.AsSpan (), capacity) - 1]
    member this.Density with get () = B2Shapes.b2Shape_GetDensity impl and set value = B2Shapes.b2Shape_SetDensity (impl, value, true)
    member this.Filter with get () = B2Shapes.b2Shape_GetFilter impl and set value = B2Shapes.b2Shape_SetFilter (impl, value)
    member this.Friction with get () = B2Shapes.b2Shape_GetFriction impl and set value = B2Shapes.b2Shape_SetFriction (impl, value)
    member this.IsSensor = B2Shapes.b2Shape_IsSensor impl
    member this.IsValid = B2Worlds.b2Shape_IsValid impl
    member this.MassData with get () = B2Shapes.b2Shape_GetMassData impl
    member this.Material with get () = B2Shapes.b2Shape_GetMaterial impl and set value = B2Shapes.b2Shape_SetMaterial (impl, value)
    member this.ParentChain = B2Shapes.b2Shape_GetParentChain impl
    member this.Polygon
        with get () = if B2Shapes.b2Shape_GetType impl = B2ShapeType.b2_polygonShape then B2Shapes.b2Shape_GetPolygon impl else failwith "Not a polygon"
        and set value = let mutable value = value in B2Shapes.b2Shape_SetPolygon (impl, &value)
    member this.Restitution with get () = B2Shapes.b2Shape_GetRestitution impl and set value = B2Shapes.b2Shape_SetRestitution (impl, value)
    member this.Segment
        with get () = if B2Shapes.b2Shape_GetType impl = B2ShapeType.b2_segmentShape then B2Shapes.b2Shape_GetSegment impl else failwith "Not a segment"
        and set value = let mutable value = value in B2Shapes.b2Shape_SetSegment (impl, &value)
    member this.SensorCapacity = B2Shapes.b2Shape_GetSensorCapacity impl
    member this.SensorOverlaps =
        let capacity = B2Shapes.b2Shape_GetSensorCapacity impl
        let overlaps = Array.zeroCreate capacity
        overlaps.[0 .. B2Shapes.b2Shape_GetSensorOverlaps (impl, overlaps.AsSpan (), capacity) - 1]
    member this.SurfaceMaterial with get () = B2Shapes.b2Shape_GetSurfaceMaterial impl and set value = B2Shapes.b2Shape_SetSurfaceMaterial (impl, value)
    member this.Type = B2Shapes.b2Shape_GetType impl
    member this.UserData with get () = B2Shapes.b2Shape_GetUserData impl and set value = B2Shapes.b2Shape_SetUserData (impl, value)
    member this.World = B2Shapes.b2Shape_GetWorld impl

type B2ChainIdDebuggerDisplay (impl) =
    member this.Friction with get () = B2Shapes.b2Chain_GetFriction impl and set value = B2Shapes.b2Chain_SetFriction (impl, value)
    member this.IsValid = B2Worlds.b2Chain_IsValid impl
    member this.Material with get () = B2Shapes.b2Chain_GetMaterial impl and set value = B2Shapes.b2Chain_SetMaterial (impl, value)
    member this.Restitution with get () = B2Shapes.b2Chain_GetRestitution impl and set value = B2Shapes.b2Chain_SetRestitution (impl, value)
    member this.SegmentCount = B2Shapes.b2Chain_GetSegmentCount impl
    member this.Segments =
        let capacity = B2Shapes.b2Chain_GetSegmentCount impl
        let segments = Array.zeroCreate capacity
        B2Shapes.b2Chain_GetSegments (impl, segments, capacity) |> ignore<int>
        segments
    member this.World = B2Shapes.b2Chain_GetWorld impl

type B2JointIdDebuggerDisplay (impl) =
    member this.AngularSeparation = B2Joints.b2Joint_GetAngularSeparation impl
    member this.BodyA = B2Joints.b2Joint_GetBodyA impl
    member this.BodyB = B2Joints.b2Joint_GetBodyB impl
    member this.CollideConnected with get () = B2Joints.b2Joint_GetCollideConnected impl and set value = B2Joints.b2Joint_SetCollideConnected (impl, value)
    member this.ConstraintForce = B2Joints.b2Joint_GetConstraintForce impl
    member this.ConstraintTorque = B2Joints.b2Joint_GetConstraintTorque impl
    member this.ConstraintTuning with get () = B2Joints.b2Joint_GetConstraintTuning impl and set (hertz, dampingRatio) = B2Joints.b2Joint_SetConstraintTuning (impl, hertz, dampingRatio)
    member this.LinearSeparation = B2Joints.b2Joint_GetLinearSeparation impl
    member this.LocalFrameA with get () = B2Joints.b2Joint_GetLocalFrameA impl and set value = B2Joints.b2Joint_SetLocalFrameA (impl, value)
    member this.LocalFrameB with get () = B2Joints.b2Joint_GetLocalFrameB impl and set value = B2Joints.b2Joint_SetLocalFrameB (impl, value)
    member this.Type = B2Joints.b2Joint_GetType impl
    member this.UserData with get () = B2Joints.b2Joint_GetUserData impl and set value = B2Joints.b2Joint_SetUserData (impl, value)
    member this.World = B2Joints.b2Joint_GetWorld impl

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