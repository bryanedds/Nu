// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Buffers
open System.Collections.Concurrent
open System.Collections.Generic
open System.Numerics
open System.Threading.Tasks
open nkast.Aether.Physics2D
open nkast.Aether.Physics2D.Collision
open nkast.Aether.Physics2D.Dynamics
open nkast.Aether.Physics2D.Dynamics.Contacts
open nkast.Aether.Physics2D.Dynamics.Joints
open Prime

/// Represents a neighbor particle during fluid simulation.
type [<Struct>] private FluidParticleNeighbor2d =

    { (* Assigned during find neighbors: *)
      mutable ParticleIndex : int // parallel for 1 output

      (* Assigned during calculate pressures: *)
      mutable Distance : single

      (* Assigned during calculate interaction forces: *)
      mutable AccumulatedDelta : Vector2 } // parallel for 1 output

/// Represents the state of a fluid particle during simulation.
type [<Struct>] private FluidParticleState2d =

    { (* Global fields: *)
      mutable PositionUnscaled : Vector2 // updated during resolve collisions - parallel for 1 input, parallel for 2 in/output
      mutable VelocityUnscaled : Vector2 // updated during calculate interaction forces, resolve collisions - parallel for 1 in/output, parallel for 2 in/output
      mutable GravityOverride : Vector2 voption
      mutable Cell : Vector2i // parallel for 1 input

      (* Assigned during scale particles: *)
      mutable PositionScaled : Vector2 // parallel for 1 input
      mutable VelocityScaled : Vector2 // parallel for 1 input

      (* Assigned during prepare simulation: *)
      mutable Delta : Vector2 // updated during calculate interaction forces, accumulate deltas - parallel for 1 output, parallel for 2 in/output
      mutable PotentialFixtureCount : int // updated during prepare collisions - parallel for 2 input
      mutable PotentialFixtures : Fixture array // updated during prepare collisions - parallel for 2 input
      mutable PotentialFixtureChildIndexes : int array // updated during prepare collisions - parallel for 2 input

      (* Assigned during find neighbors: *)
      mutable NeighborCount : int // parallel for 1 output
      mutable Neighbors : FluidParticleNeighbor2d array } // parallel for 1 output

/// Represents a 2d fluid emitter.
///
/// Original C# algorithm from https://github.com/klutch/Box2DFluid, with additions to collide with EdgeShape and
/// ChainShape.
///
/// It fixes collision detection by detecting the final particle position properly or particles would tunnel through
/// EdgeShapes and ChainShapes, and added linear damping.
///
/// NOTE: this simple implementation will be replaced with a more general library that allows for particles
/// influencing rigid bodies in the future.
type private FluidEmitter2d =
    { FluidEmitterDescriptor : FluidEmitterDescriptor2d
      States : FluidParticleState2d array
      ActiveIndices : int HashSet
      Grid : Dictionary<Vector2i, int ResizeArray> }

    static let CellCapacityDefault = 20

    static let Neighborhood = [|for x in -1 .. 1 do for y in -1 .. 1 do v2i x y|]

    static let updateCell i (fluidEmitter : FluidEmitter2d) =
        let state = &fluidEmitter.States.[i]
        let newCell = FluidEmitter2d.positionToCell fluidEmitter.FluidEmitterDescriptor.CellSize state.PositionUnscaled
        if state.Cell <> newCell then
            let cell = fluidEmitter.Grid.[state.Cell]
            cell.Remove i |> ignore
            if cell.Count = 0 then fluidEmitter.Grid.Remove state.Cell |> ignore
            match fluidEmitter.Grid.TryGetValue newCell with
            | (true, cell) -> cell.Add i
            | (false, _) ->
                let singleton = ResizeArray CellCapacityDefault
                singleton.Add i
                fluidEmitter.Grid.[newCell] <- singleton
            state.Cell <- newCell

    static let toFluid (particleScale : single) (state : FluidParticleState2d byref) (particle : FluidParticle) =
        state.PositionUnscaled <- particle.FluidParticlePosition.V2
        state.VelocityUnscaled <- particle.FluidParticleVelocity.V2
        state.GravityOverride <- particle.GravityOverride |> ValueOption.map (fun g -> g.V2 * particleScale)

    static let fromFluid (particleScale : single) (state : FluidParticleState2d byref) =
        { FluidParticlePosition = state.PositionUnscaled.V3
          FluidParticleVelocity = state.VelocityUnscaled.V3
          GravityOverride = state.GravityOverride |> ValueOption.map (fun g -> (g / particleScale).V3) }

    static member positionToCell cellSize (position : Vector2) =
        v2i (floor (position.X / cellSize) |> int) (floor (position.Y / cellSize) |> int)

    static member cellToBox cellSize (cell : Vector2i) =
        box2 (cell.V2 * cellSize) (v2Dup cellSize)

    static member updateDescriptor (descriptor : FluidEmitterDescriptor2d) (fluidEmitter : FluidEmitter2d) =
        if not descriptor.Enabled then
            FluidEmitter2d.clearParticles fluidEmitter
            { fluidEmitter with FluidEmitterDescriptor = descriptor } // clear all particles if disabled
        elif fluidEmitter.FluidEmitterDescriptor.ParticlesMax <> descriptor.ParticlesMax then
            let newEmitter = FluidEmitter2d.make descriptor // re-add all particles
            FluidEmitter2d.addParticles (fluidEmitter.ActiveIndices |> Seq.map (fun i -> fromFluid descriptor.ParticleScale &fluidEmitter.States[i])) newEmitter
            newEmitter
        elif fluidEmitter.FluidEmitterDescriptor.CellSize <> descriptor.CellSize then
            let newEmitter = { fluidEmitter with FluidEmitterDescriptor = descriptor }
            for i in newEmitter.ActiveIndices do updateCell i newEmitter // update cells
            newEmitter
        else { fluidEmitter with FluidEmitterDescriptor = descriptor } // minimal updates

    static member addParticles (particles : FluidParticle seq) (fluidEmitter : FluidEmitter2d) =
        let mutable i = 0
        let descriptor = fluidEmitter.FluidEmitterDescriptor
        let particleEnr = particles.GetEnumerator ()
        if descriptor.Enabled && particleEnr.MoveNext () then
            let mutable continued = i <> descriptor.ParticlesMax
            while continued do
                let particleState = &fluidEmitter.States.[i]
                if fluidEmitter.ActiveIndices.Add i then
                    let particle = particleEnr.Current

                    // initialize particle
                    toFluid descriptor.ParticleScale &particleState particle

                    // initialize grid
                    let cell = FluidEmitter2d.positionToCell descriptor.CellSize particleState.PositionUnscaled
                    particleState.Cell <- cell
                    match fluidEmitter.Grid.TryGetValue cell with
                    | (true, resizeArray) -> resizeArray.Add i
                    | (false, _) ->
                        let singleton = ResizeArray CellCapacityDefault
                        singleton.Add i
                        fluidEmitter.Grid.[cell] <- singleton

                    // advance
                    continued <- particleEnr.MoveNext ()

                i <- inc i
                if i = descriptor.ParticlesMax then continued <- false

    static member setParticles (particles : FluidParticle seq) (fluidEmitter : FluidEmitter2d) =
        FluidEmitter2d.clearParticles fluidEmitter
        FluidEmitter2d.addParticles particles fluidEmitter

    static member mapParticles (mapping : FluidParticle -> FluidParticle) (fluidEmitter : FluidEmitter2d) =
        for i in fluidEmitter.ActiveIndices do
            let state = &fluidEmitter.States.[i]
            let particle = mapping (fromFluid fluidEmitter.FluidEmitterDescriptor.ParticleScale &state)
            toFluid fluidEmitter.FluidEmitterDescriptor.ParticleScale &state particle
            updateCell i fluidEmitter

    static member filterParticles (filter : FluidParticle -> bool) (fluidEmitter : FluidEmitter2d) =
        fluidEmitter.ActiveIndices.RemoveWhere (fun i ->
            let state = &fluidEmitter.States.[i]
            let removed = not (filter (fromFluid fluidEmitter.FluidEmitterDescriptor.ParticleScale &state))
            if removed then
                let cell = fluidEmitter.Grid.[state.Cell]
                cell.Remove i |> ignore
                if cell.Count = 0 then fluidEmitter.Grid.Remove state.Cell |> ignore
            removed)
        |> ignore

    static member clearParticles (fluidEmitter : FluidEmitter2d) =
        fluidEmitter.ActiveIndices.Clear ()
        fluidEmitter.Grid.Clear ()

    static member step (clockDelta : single) (gravity : Vector2) (fluidEmitter : FluidEmitter2d) (context : World) =

        // OPTIMIZATION: early return when no particles (also applies to not enabled)
        if fluidEmitter.ActiveIndices.Count > 0 then

            // scale particles for neighbor search
            let descriptor = fluidEmitter.FluidEmitterDescriptor
            let gravity = Option.defaultValue gravity descriptor.GravityOverride * descriptor.ParticleScale * clockDelta
            let radiusScaled = descriptor.ParticleScale
            for i in fluidEmitter.ActiveIndices do
                let state = &fluidEmitter.States.[i]
                state.PositionScaled <- state.PositionUnscaled * radiusScaled
                state.VelocityScaled <- state.VelocityUnscaled * radiusScaled

            // parallel for 1
            let loopResult = Parallel.ForEach (fluidEmitter.ActiveIndices, fun i ->

                // collect sim properties
                let descriptor = fluidEmitter.FluidEmitterDescriptor
                let neighborsMax = descriptor.NeighborsMax
                let particleRadius = descriptor.ParticleRadius
                let particleRadiusSquared = particleRadius * particleRadius
                let fixturesMax = descriptor.CollisionTestsMax

                // prepare simulation
                let state = &fluidEmitter.States.[i]
                state.Delta <- v2Zero
                state.PotentialFixtureCount <- 0
                state.PotentialFixtures <- ArrayPool.Shared.Rent fixturesMax
                state.PotentialFixtureChildIndexes <- ArrayPool.Shared.Rent fixturesMax

                // find neighbors
                state.NeighborCount <- 0
                state.Neighbors <- ArrayPool.Shared.Rent neighborsMax
                let cell = state.Cell
                for neighbor in
                    Neighborhood
                    |> Seq.collect (fun neighbour -> match fluidEmitter.Grid.TryGetValue (cell + neighbour) with (true, list) -> list :> _ seq | _ -> Seq.empty)
                    |> Seq.truncate neighborsMax do
                    if neighbor <> i then
                        state.Neighbors.[state.NeighborCount].ParticleIndex <- neighbor
                        state.NeighborCount <- inc state.NeighborCount

                // calculate pressures
                let mutable p = 0.0f
                let mutable pNear = 0.0f
                for n in 0 .. dec state.NeighborCount do
                    let neighbor = &state.Neighbors.[n]
                    let relativePosition = fluidEmitter.States.[neighbor.ParticleIndex].PositionScaled - state.PositionScaled
                    let distanceSquared = relativePosition.MagnitudeSquared
                    if distanceSquared < particleRadiusSquared then
                        neighbor.Distance <- sqrt distanceSquared
                        let oneMinusQ = 1.0f - neighbor.Distance / particleRadius
                        p <- p + oneMinusQ * oneMinusQ
                        pNear <- pNear + oneMinusQ * oneMinusQ * oneMinusQ
                    else neighbor.Distance <- nanf
                let pressure = (p - 5.0f) * 0.5f // normal pressure term
                let presnear = pNear * 0.5f // near particles term

                // calculate interaction forces
                for n in 0 .. dec state.NeighborCount do
                    let neighbor = &state.Neighbors.[n]
                    if not (Single.IsNaN neighbor.Distance) then

                        // compute pressure factor
                        let oneMinusQ = 1.0f - neighbor.Distance / particleRadius
                        let relativePosition = fluidEmitter.States.[neighbor.ParticleIndex].PositionScaled - state.PositionScaled
                        let pressureFactor = oneMinusQ * (pressure + presnear * oneMinusQ) / (2.0f * neighbor.Distance)

                        // compute viscosity factor
                        let relativeVelocity = fluidEmitter.States.[neighbor.ParticleIndex].VelocityScaled - state.VelocityScaled
                        let viscosityFactor = descriptor.Viscosity * oneMinusQ * clockDelta

                        // accumulate deltas
                        let delta = relativePosition * pressureFactor - relativeVelocity * viscosityFactor
                        neighbor.AccumulatedDelta <- delta
                        state.Delta <- state.Delta - delta

                    else neighbor.AccumulatedDelta <- v2Zero

                // apply gravity to velocity
                match state.GravityOverride with
                | ValueSome gravity -> state.VelocityUnscaled <- state.VelocityUnscaled + gravity * clockDelta * descriptor.ParticleScale
                | ValueNone -> state.VelocityUnscaled <- state.VelocityUnscaled + gravity)

            // assert loop completion
            assert loopResult.IsCompleted

            // accumulate deltas
            for i in fluidEmitter.ActiveIndices do
                let state = &fluidEmitter.States.[i]
                for j in 0 .. dec state.NeighborCount do
                    let neighbor = &state.Neighbors.[j]
                    fluidEmitter.States.[neighbor.ParticleIndex].Delta <- fluidEmitter.States.[neighbor.ParticleIndex].Delta + neighbor.AccumulatedDelta
            for i in fluidEmitter.ActiveIndices do
                fluidEmitter.States.[i].Delta <- fluidEmitter.States.[i].Delta / radiusScaled * (1.0f - descriptor.LinearDamping)

            // prepare collisions
            let toPhysicsV2 (v : Vector2) = Common.Vector2 (v.X, v.Y) / Constants.Engine.Meter2d
            let mutable aabb = AABB (toPhysicsV2 descriptor.SimulationBounds.Min, toPhysicsV2 descriptor.SimulationBounds.Max)
            let query (fixture : Fixture) = 
                let fromPhysicsV2 (v : Common.Vector2) = Vector2 (v.X, v.Y) * Constants.Engine.Meter2d
                let cellSize = fluidEmitter.FluidEmitterDescriptor.CellSize
                let mutable aabb = Unchecked.defaultof<_>
                let mutable transform = Unchecked.defaultof<_>
                fixture.Body.GetTransform &transform
                for c in 0 .. dec fixture.Shape.ChildCount do // chain shapes have edges as children, other shapes only have 1 child
                    fixture.Shape.ComputeAABB (&aabb, &transform, c)
                    let lowerBound = FluidEmitter2d.positionToCell cellSize (fromPhysicsV2 aabb.LowerBound)
                    let upperBound = FluidEmitter2d.positionToCell cellSize (fromPhysicsV2 aabb.UpperBound)
                    for gridX in dec lowerBound.X .. inc upperBound.X do // expand grid by one in case some fixtures perfectly align on cell boundary
                        for gridY in dec lowerBound.Y .. inc upperBound.Y do
                            match fluidEmitter.Grid.TryGetValue (v2i gridX gridY) with
                            | (true, particleIndexes) ->
                                for i in particleIndexes do
                                    let state = &fluidEmitter.States.[i]
                                    if state.PotentialFixtureCount < fluidEmitter.FluidEmitterDescriptor.CollisionTestsMax then
                                        state.PotentialFixtures.[state.PotentialFixtureCount] <- fixture
                                        state.PotentialFixtureChildIndexes.[state.PotentialFixtureCount] <- c
                                        state.PotentialFixtureCount <- inc state.PotentialFixtureCount
                            | (false, _) -> ()
                true
            context.QueryAABB (query, &aabb)

            // parallel for 2 - resolve collisions
            let collisions = ConcurrentBag ()
            let loopResult = Parallel.ForEach (fluidEmitter.ActiveIndices, fun i ->
                // NOTE: Collision testing must use physics engine units in calculations or the fluid collision in FluidSim page of
                // Sand Box 2d would either lose particles at corners when the fluid tank is filled, or the particles will be too jumpy
                let toPixelV2 (v : Common.Vector2) = Vector2 (v.X, v.Y) * Constants.Engine.Meter2d
                let toPhysicsV2 (v : Vector2) = Common.Vector2 (v.X, v.Y) / Constants.Engine.Meter2d
                let toPhysicsV2Normal (v : Vector2) = Common.Vector2 (v.X, v.Y)
                let state = &fluidEmitter.States.[i]
                for i in 0 .. dec state.PotentialFixtureCount do
                    let fixture = state.PotentialFixtures.[i]
                    let mutable colliding = false
                    let mutable nearest = Common.Vector2.Zero
                    let mutable normal = Common.Vector2.Zero
                    let (|EdgeFromEdgeShape|) (shape : Shapes.EdgeShape) = (shape.Vertex1, shape.Vertex2)
                    let (|EdgeFromChainShape|) (lookup : _ array) index (shape : Shapes.ChainShape) = (shape.Vertices.[lookup.[index]], shape.Vertices.[inc lookup.[index]])
                    match fixture.Shape with
                    | :? Shapes.PolygonShape as shape ->

                        // NOTE: original code uses (Position + Velocity + Delta) for solid shape collision testing even though particle
                        // movement update uses (Position + Velocity + 2 Delta). If the latter is used here, it causes particles to tunnel
                        // through the container corners in the FluidSim demo inside Sand Box 2d.
                        let mutable newPosition = toPhysicsV2 (state.PositionUnscaled + state.VelocityUnscaled + state.Delta)
                        if fixture.TestPoint &newPosition then
                            colliding <- true
                            let mutable collisionXF = Unchecked.defaultof<_>
                            fixture.Body.GetTransform &collisionXF
                            let mutable shortestDistance = infinityf // Find closest edge
                            for j in 0 .. dec shape.Vertices.Count do

                                // transform the shape's normals using the rotation (Complex) part of the transform
                                let mutable collisionNormal = Common.Complex.Multiply (shape.Normals.[j], &collisionXF.q)

                                // transform the shape's vertices from local space to world space
                                let mutable collisionDistance = Common.Transform.Multiply (shape.Vertices.[j], &collisionXF) - toPhysicsV2 state.PositionUnscaled

                                // project the vertex position relative to the particle position onto the edge's normal to find the distance
                                let mutable distance = Unchecked.defaultof<_>
                                Common.Vector2.Dot (&collisionNormal, &collisionDistance, &distance)
                                if distance < shortestDistance then
                                    shortestDistance <- distance
                                    nearest <- collisionNormal * distance + toPhysicsV2 state.PositionUnscaled // push the particle out of the shape in the direction of the closest edge's normal
                                    normal <- collisionNormal

                    | :? Shapes.CircleShape as shape ->
                        let mutable newPosition = toPhysicsV2 (state.PositionUnscaled + state.VelocityUnscaled + state.Delta)
                        if fixture.TestPoint &newPosition then
                            colliding <- true
                            // push the particle out of the circle by normalizing the circle's center relative to the
                            // particle position, and pushing the particle out in the direction of the normal
                            let center = shape.Position + fixture.Body.Position
                            normal <- toPhysicsV2 state.PositionUnscaled - center
                            normal.Normalize ()
                            nearest <- center + normal * shape.Radius

                    | (:? Shapes.EdgeShape as EdgeFromEdgeShape (edgeStart, edgeEnd))
                    | (:? Shapes.ChainShape as EdgeFromChainShape state.PotentialFixtureChildIndexes i (edgeStart, edgeEnd)) ->

                        // collision with an edge - use line-segment intersection

                        // transform the shape's vertices from local space to world space
                        let mutable collisionXF = Unchecked.defaultof<_>
                        fixture.Body.GetTransform &collisionXF
                        let edgeStart = toPixelV2 (Common.Transform.Multiply (edgeStart, &collisionXF))
                        let edgeEnd = toPixelV2 (Common.Transform.Multiply (edgeEnd, &collisionXF))
                    
                        // NOTE: unlike solid shape collision testing, we need to use (Position + Velocity + 2 Delta) as new position
                        // for edge collisions to prevent tunneling.
                        let particleMovement = state.VelocityUnscaled + 2.0f * state.Delta
                        let edgeSegment = edgeEnd - edgeStart

                        // shim for .NET 10 Vector2.Cross (Vector2, Vector2). TODO: use it when we upgrade to .NET 10.
                        let vector2Cross (v1 : Vector2, v2 : Vector2) = v1.X * v2.Y - v1.Y * v2.X
                        let cross_particleMovement_edgeSegment = vector2Cross (particleMovement, edgeSegment)
                        if abs cross_particleMovement_edgeSegment > 1e-6f then // non-collinear

                            // standard segment intersection formula:
                            // let A = edgeStart, B = edgeEnd (edge: A + u*(B-A))
                            // let C = particle.Position, D = newPosition (particle: C + t*(D-C))
                            // t = (AC × AB) / (CD × AB)
                            // u = (AC × CD) / (CD × AB)
                            let AC = edgeStart - state.PositionUnscaled
                            let t = vector2Cross (AC, edgeSegment) / cross_particleMovement_edgeSegment
                            let u = vector2Cross (AC, particleMovement) / cross_particleMovement_edgeSegment

                            // after solving t and u, the collision is only counted if the intersection point is within
                            // segments.
                            if t >= 0.0f && t <= 1.0f && u >= 0.0f && u <= 1.0f then
                                colliding <- true
                                nearest <- edgeStart + u * edgeSegment |> toPhysicsV2

                                // for two-sided collision, normal should point away from edge surface
                                let edgeNormal = Vector2.Normalize (Vector2 (-edgeSegment.Y, edgeSegment.X))

                                // determine which side the particle is approaching from
                                let approachDirection = Vector2.Normalize particleMovement
                                let dotProduct = Vector2.Dot (edgeNormal, approachDirection)

                                // if particle is moving toward the normal, keep it; otherwise flip
                                normal <- toPhysicsV2Normal <| if dotProduct < 0.0f then edgeNormal else -edgeNormal

                        else

                            // handle collinear case - particle moving parallel to edge.
                            // this can be implemented using point-line distance checks.
                            let edgeLengthSquared = edgeSegment.LengthSquared ()
                            if edgeLengthSquared > 1e-6f then

                                // project particle path onto edge to find closest approach
                                let toParticleStart = state.PositionUnscaled - edgeStart
                                let projection = Vector2.Dot (toParticleStart, edgeSegment) / edgeLengthSquared
                                let closestOnEdge = edgeStart + saturate projection * edgeSegment

                                // check if particle path comes close to the edge
                                let approachVector = closestOnEdge - state.PositionUnscaled
                                let distanceSquared = approachVector.LengthSquared ()
                                let collisionRadius = fluidEmitter.FluidEmitterDescriptor.ParticleRadius
                            
                                // push out using perpendicular to edge as normal when within collision radius
                                if distanceSquared <= collisionRadius * collisionRadius then
                                    colliding <- true
                                    nearest <- toPhysicsV2 closestOnEdge
                                    normal <- Vector2.Normalize (Vector2 (-edgeSegment.Y, edgeSegment.X)) |> toPhysicsV2Normal // use perpendicular to edge for normal in collinear case
                    | shape -> Log.warnOnce $"Shape not implemented: {shape}"

                    // handle collision response
                    if colliding then
                        collisions.Add
                            { FluidCollider = fromFluid fluidEmitter.FluidEmitterDescriptor.ParticleScale &state
                              FluidCollidee = fixture.Tag :?> BodyShapeIndex
                              Nearest = (toPixelV2 nearest).V3
                              Normal = (toPixelV2 normal).V3 }
                        if not fixture.IsSensor then
                            state.PositionUnscaled <- nearest + 0.05f * normal |> toPixelV2
                            let mutable dotResult = Unchecked.defaultof<_>
                            let mutable velocity = toPhysicsV2 state.VelocityUnscaled
                            Common.Vector2.Dot (&velocity, &normal, &dotResult)
                            state.VelocityUnscaled <- (velocity - 1.2f * dotResult * normal) * 0.85f |> toPixelV2
                            state.Delta <- v2Zero
                  
                    // don't leak memory for this fixture
                    state.PotentialFixtures.[i] <- null)

            // assert loop completion
            assert loopResult.IsCompleted

            // relocate particles
            fluidEmitter.ActiveIndices.RemoveWhere (fun i ->

                // NOTE: original code applies delta twice to position (Velocity already contains a Delta).
                let state = &fluidEmitter.States.[i]
                state.VelocityUnscaled <- state.VelocityUnscaled + state.Delta
                state.PositionUnscaled <- state.PositionUnscaled + state.VelocityUnscaled + state.Delta

                ArrayPool.Shared.Return state.PotentialFixtureChildIndexes
                ArrayPool.Shared.Return state.PotentialFixtures
                ArrayPool.Shared.Return state.Neighbors

                let bounds = fluidEmitter.FluidEmitterDescriptor.SimulationBounds
                let removed = bounds.Contains state.PositionUnscaled = ContainmentType.Disjoint
                if removed then
                    let cell = fluidEmitter.Grid.[state.Cell]
                    cell.Remove i |> ignore
                    if cell.Count = 0 then fluidEmitter.Grid.Remove state.Cell |> ignore
                else updateCell i fluidEmitter
                removed) |> ignore

            // aggregate state
            let particleStates = SArray.zeroCreate fluidEmitter.ActiveIndices.Count
            let mutable j = 0
            for i in fluidEmitter.ActiveIndices do
                let state = &fluidEmitter.States.[i]
                particleStates.[j] <- fromFluid fluidEmitter.FluidEmitterDescriptor.ParticleScale &state
                j <- inc j

            // fin
            (particleStates, collisions)

        // nothing to do
        else (SArray.empty, ConcurrentBag ())

    static member make descriptor =
        { FluidEmitterDescriptor = descriptor
          States = Array.zeroCreate descriptor.ParticlesMax
          ActiveIndices = HashSet (descriptor.ParticlesMax, HashIdentity.Structural)
          Grid = Dictionary HashIdentity.Structural }

/// The 2d interface of PhysicsEngineRenderContext in terms of Aether Physics.
type PhysicsEngine2dRenderContext =
    inherit PhysicsEngineRenderContext
    abstract EyeBounds : Box2
    abstract DrawLine : start : Vector2 * stop : Vector2 * color : Color -> unit
    abstract DrawCircle : position : Vector2 * radius : single * color : Color -> unit

/// The 2d implementation of PhysicsEngine in terms of Aether Physics.
and [<ReferenceEquality>] PhysicsEngine2d =
    private
        { PhysicsContext : Dynamics.World
          Bodies : Dictionary<BodyId, Vector3 option * Dynamics.Body>
          Joints : Dictionary<BodyJointId, Dynamics.Joints.Joint>
          CreateBodyJointMessages : Dictionary<BodyId, CreateBodyJointMessage List>
          IntegrationMessages : IntegrationMessage List
          PenetrationHandler : OnCollisionEventHandler
          SeparationHandler : OnSeparationEventHandler
          BreakHandler : Action<Joint, single>
          FluidEmitters : Dictionary<FluidEmitterId, FluidEmitter2d> }

    static member private toPixel value =
        value * Constants.Engine.Meter2d

    static member private toPhysics value =
        value / Constants.Engine.Meter2d

    static member private toPixelV2 (v2 : Common.Vector2) =
        Vector2 (PhysicsEngine2d.toPixel v2.X, PhysicsEngine2d.toPixel v2.Y)

    static member private toPixelV3 (v2 : Common.Vector2) =
        (PhysicsEngine2d.toPixelV2 v2).V3

    static member private toPhysicsV2 (v3 : Vector3) =
        Common.Vector2 (PhysicsEngine2d.toPhysics v3.X, PhysicsEngine2d.toPhysics v3.Y)

    static member private toPhysicsPolygonDiameter value =
        let value = PhysicsEngine2d.toPhysics value
        max Settings.PolygonRadius (value - Settings.PolygonRadius * 2.0f)

    static member private toPhysicsPolygonRadius value =
        let value = PhysicsEngine2d.toPhysics value
        max Settings.PolygonRadius (value - Settings.PolygonRadius)

    static member private toPhysicsBodyType bodyType =
        match bodyType with
        | Static -> Dynamics.BodyType.Static
        | Kinematic -> Dynamics.BodyType.Kinematic
        | KinematicCharacter -> Log.infoOnce "KinematicCharacter not supported by PhysicsEngine2d. Using Kinematic configuration instead."; Dynamics.BodyType.Kinematic
        | Dynamic -> Dynamics.BodyType.Dynamic
        | DynamicCharacter -> Log.infoOnce "DynamicCharacter not supported by PhysicsEngine2d. Using Dynamic configuration instead."; Dynamics.BodyType.Dynamic
        | Vehicle -> Log.infoOnce "Vehicle not supported by PhysicsEngine2d. Using Dynamic configuration instead."; Dynamics.BodyType.Dynamic

    static member private handlePenetration
        (bodyShape : Dynamics.Fixture)
        (bodyShape2 : Dynamics.Fixture)
        (contact : Dynamics.Contacts.Contact)
        (integrationMessages : IntegrationMessage List) =
        let normal = fst (contact.GetWorldManifold ())
        let bodyPenetrationMessage =
            { BodyShapeSource = bodyShape.Tag :?> BodyShapeIndex
              BodyShapeSource2 = bodyShape2.Tag :?> BodyShapeIndex
              Normal = Vector3 (normal.X, normal.Y, 0.0f) }
        let integrationMessage = BodyPenetrationMessage bodyPenetrationMessage
        integrationMessages.Add integrationMessage
        true

    static member private handleSeparation
        (bodyShape : Dynamics.Fixture)
        (bodyShape2 : Dynamics.Fixture)
        (integrationMessages : IntegrationMessage List) =
        let bodySeparationMessage =
            { BodyShapeSource = bodyShape.Tag :?> BodyShapeIndex
              BodyShapeSource2 = bodyShape2.Tag :?> BodyShapeIndex }
        let integrationMessage = BodySeparationMessage bodySeparationMessage
        integrationMessages.Add integrationMessage

    static member private handleBreak
        (joint : Joint)
        (jointError : single)
        (integrationMessages : IntegrationMessage List) =
        let jointBreakPointPixel = PhysicsEngine2d.toPixel joint.Breakpoint
        let jointErrorPixel = PhysicsEngine2d.toPixel jointError
        let bodyJointBreakMessage =
            { BodyJointId = joint.Tag :?> BodyJointId
              BreakingPoint = jointBreakPointPixel
              BreakingOverflow = jointErrorPixel - jointBreakPointPixel }
        let integrationMessage = BodyJointBreakMessage bodyJointBreakMessage
        integrationMessages.Add integrationMessage

    static member private getBodyContacts (bodyId : BodyId) physicsEngine =
        let (_, body) = physicsEngine.Bodies.[bodyId]
        let contacts = List<Contact> ()
        let mutable current = body.ContactList
        while notNull current do
            contacts.Add current.Contact
            current <- current.Next
        Array.ofSeq contacts

    static member private configureBodyShapeProperties bodyProperties bodyShapePropertiesOpt (bodyShape : Fixture) =
        match bodyShapePropertiesOpt with
        | Some bodyShapeProperties ->
            bodyShape.Friction <- match bodyShapeProperties.FrictionOpt with Some f -> f | None -> bodyProperties.Friction
            bodyShape.Restitution <- match bodyShapeProperties.RestitutionOpt with Some r -> r | None -> bodyProperties.Restitution
            bodyShape.CollisionCategories <- match bodyShapeProperties.CollisionCategoriesOpt with Some cc -> enum<Category> cc | None -> enum<Category> bodyProperties.CollisionCategories
            bodyShape.CollidesWith <- match bodyShapeProperties.CollisionMaskOpt with Some cm -> enum<Category> cm | None -> enum<Category> bodyProperties.CollisionMask
            bodyShape.IsSensor <- match bodyShapeProperties.SensorOpt with Some sensor -> sensor | None -> bodyProperties.Sensor
        | None ->
            bodyShape.Friction <- bodyProperties.Friction
            bodyShape.Restitution <- bodyProperties.Restitution
            bodyShape.CollisionCategories <- enum<Category> bodyProperties.CollisionCategories
            bodyShape.CollidesWith <- enum<Category> bodyProperties.CollisionMask
            bodyShape.IsSensor <- bodyProperties.Sensor

    static member private configureBodyProperties (bodyProperties : BodyProperties) (body : Body) =
        body.BodyType <- PhysicsEngine2d.toPhysicsBodyType bodyProperties.BodyType // NOTE: BodyType must be set first or other configurations may be ignored!
        body.Enabled <- bodyProperties.Enabled
        body.SleepingAllowed <- bodyProperties.SleepingAllowed
        body.Position <- PhysicsEngine2d.toPhysicsV2 bodyProperties.Center
        body.Rotation <- bodyProperties.Rotation.Angle2d
        body.LinearVelocity <- PhysicsEngine2d.toPhysicsV2 bodyProperties.LinearVelocity
        body.LinearDamping <- bodyProperties.LinearDamping
        body.AngularVelocity <- bodyProperties.AngularVelocity.Z
        body.AngularDamping <- bodyProperties.AngularDamping
        body.FixedRotation <- bodyProperties.AngularFactor.Z = 0.0f
        body.IgnoreGravity <- true // NOTE: body-specific gravity isn't supported by Aether, so we handle gravity ourselves.
        body.IgnoreCCD <- match bodyProperties.CollisionDetection with Discrete -> true | Continuous -> false
        body.Awake <- bodyProperties.Awake

    static member private attachBoxBody bodySource (bodyProperties : BodyProperties) (boxShape : BoxShape) (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity boxShape.TransformOpt
        let width = PhysicsEngine2d.toPhysicsPolygonDiameter (boxShape.Size.X * transform.Scale.X)
        let height = PhysicsEngine2d.toPhysicsPolygonDiameter (boxShape.Size.Y * transform.Scale.Y)
        let offset = PhysicsEngine2d.toPhysicsV2 transform.Translation
        let angle = transform.Rotation.Angle2d
        let density =
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass -> mass / (width * height)
        let density = max 0.001f density // NOTE: Aether has collision reponse issue when density is 0 even if it's for a static shape!
        let shape =
            let rectangleVertices = Common.PolygonTools.CreateRectangle (width / 2.0f, height / 2.0f, offset, angle);
            let rectangleShape = Collision.Shapes.PolygonShape (rectangleVertices, density)
            body.CreateFixture rectangleShape
        shape.Tag <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              BodyShapeIndex = match boxShape.PropertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
        PhysicsEngine2d.configureBodyShapeProperties bodyProperties boxShape.PropertiesOpt shape
        shape

    static member private attachSphereShape bodySource (bodyProperties : BodyProperties) (sphereShape : SphereShape) (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity sphereShape.TransformOpt
        let radius = PhysicsEngine2d.toPhysicsPolygonRadius (sphereShape.Radius * transform.Scale.X)
        let offset = PhysicsEngine2d.toPhysicsV2 transform.Translation
        let density =
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass -> mass / (MathF.PI * radius * radius)
        let density = max 0.001f density // NOTE: Aether has collision reponse issue when density is 0 even if it's for a static shape!
        let shape = body.CreateCircle (radius, density, offset)
        shape.Tag <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              BodyShapeIndex = match sphereShape.PropertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
        PhysicsEngine2d.configureBodyShapeProperties bodyProperties sphereShape.PropertiesOpt shape
        shape

    static member private attachCapsuleShape bodySource (bodyProperties : BodyProperties) (capsuleShape : CapsuleShape) (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity capsuleShape.TransformOpt
        let height = PhysicsEngine2d.toPhysicsPolygonDiameter (capsuleShape.Height * transform.Scale.Y)
        let endRadius = PhysicsEngine2d.toPhysicsPolygonRadius (capsuleShape.Radius * transform.Scale.Y)
        let skinnyScalar = 0.9f // scales in the capsule's width to stop corner sticking.
        let density =
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass -> mass / (endRadius * skinnyScalar * height * 0.5f + MathF.PI * endRadius * endRadius)
        let density = max 0.001f density // NOTE: Aether has collision reponse issue when density is 0 even if it's for a static shape!
        let offset = PhysicsEngine2d.toPhysicsV2 transform.Translation
        let angle = transform.Rotation.Angle2d
        let rectangle = Common.PolygonTools.CreateRectangle (endRadius * skinnyScalar, height * 0.5f, offset, angle)
        let list = List<Common.Vertices> ()
        list.Add rectangle
        let bodyShapes = body.CreateCompoundPolygon (list, density)
        let circleOffset = Common.Complex.FromAngle angle
        let circleOffset = Common.Complex.Multiply (Common.Vector2 (0.0f, height * 0.5f), ref circleOffset)
        let bodyShapeTop = body.CreateCircle (endRadius, density * 0.5f, circleOffset + offset)
        let bodyShapeBottom = body.CreateCircle (endRadius, density * 0.5f, -circleOffset + offset)
        bodyShapes.Add bodyShapeTop
        bodyShapes.Add bodyShapeBottom
        for bodyShape in bodyShapes do
            bodyShape.Tag <-
                { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
                  BodyShapeIndex = match capsuleShape.PropertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
            PhysicsEngine2d.configureBodyShapeProperties bodyProperties capsuleShape.PropertiesOpt bodyShape
        Array.ofSeq bodyShapes

    static member private attachBoxRoundedShape bodySource (bodyProperties : BodyProperties) (boxRoundedShape : BoxRoundedShape) (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (a : Affine) -> let mutable t = a in t.Matrix) m4Identity boxRoundedShape.TransformOpt
        if quatNeq transform.Rotation quatIdentity then Log.warnOnce "BoxRoundedShape rotation not yet supported by PhysicsEngine2d." // TODO: implement!
        let width = PhysicsEngine2d.toPhysicsPolygonDiameter (boxRoundedShape.Size.X * transform.Scale.X)
        let height = PhysicsEngine2d.toPhysicsPolygonDiameter (boxRoundedShape.Size.Y * transform.Scale.Y)
        let radius = PhysicsEngine2d.toPhysicsPolygonRadius (boxRoundedShape.Radius * transform.Scale.X)
        let center = PhysicsEngine2d.toPhysicsV2 transform.Translation
        let boxVerticalWidth = width - radius * 2.0f
        let boxHorizontalHeight = height - radius * 2.0f
        let density =
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass -> mass / (width * height)
        let density = max 0.001f density // NOTE: Aether has collision reponse issue when density is 0 even if it's for a static shape!
        let rectangleV = Common.PolygonTools.CreateRectangle (boxVerticalWidth * 0.5f, height * 0.5f * 0.9f, center, 0.0f) // scaled in height to stop corner sticking
        let rectangleH = Common.PolygonTools.CreateRectangle (width * 0.5f * 0.9f, boxHorizontalHeight * 0.5f, center, 0.0f) // scaled in width to stop corner sticking
        let list = List<Common.Vertices> ()
        list.Add rectangleV
        list.Add rectangleH
        let bodyShapes =            body.CreateCompoundPolygon (list, density)
        let bodyShapeTopLeft =      body.CreateCircle (radius, density * 0.25f, Common.Vector2 (-width * 0.5f + radius, +height * 0.5f - radius) + center)
        let bodyShapeTopRight =     body.CreateCircle (radius, density * 0.25f, Common.Vector2 (+width * 0.5f - radius, +height * 0.5f - radius) + center)
        let bodyShapeBottomLeft =   body.CreateCircle (radius, density * 0.25f, Common.Vector2 (-width * 0.5f + radius, -height * 0.5f + radius) + center)
        let bodyShapeBottomRight =  body.CreateCircle (radius, density * 0.25f, Common.Vector2 (+width * 0.5f - radius, -height * 0.5f + radius) + center)
        bodyShapes.Add bodyShapeTopLeft
        bodyShapes.Add bodyShapeTopRight
        bodyShapes.Add bodyShapeBottomLeft
        bodyShapes.Add bodyShapeBottomRight
        for bodyShape in bodyShapes do
            bodyShape.Tag <-
                { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
                  BodyShapeIndex = match boxRoundedShape.PropertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
            PhysicsEngine2d.configureBodyShapeProperties bodyProperties boxRoundedShape.PropertiesOpt bodyShape
        Array.ofSeq bodyShapes

    static member private attachEdgeShape bodySource bodyProperties (edgeShape : EdgeShape) (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity edgeShape.TransformOpt
        let bodyShape =
            body.CreateEdge
                (PhysicsEngine2d.toPhysicsV2 (edgeShape.Start.Transform transform),
                 PhysicsEngine2d.toPhysicsV2 (edgeShape.Stop.Transform transform))
        bodyShape.Tag <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              BodyShapeIndex = match edgeShape.PropertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
        PhysicsEngine2d.configureBodyShapeProperties bodyProperties edgeShape.PropertiesOpt bodyShape
        Array.singleton bodyShape

    static member private attachContourShape bodySource bodyProperties (contourShape : ContourShape) (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity contourShape.TransformOpt
        let vertices' = Array.zeroCreate contourShape.Links.Length
        for i in 0 .. dec contourShape.Links.Length do
            vertices'.[i] <- PhysicsEngine2d.toPhysicsV2 (contourShape.Links.[i].Transform transform)
        let bodyShape =
            if contourShape.Closed
            then body.CreateLoopShape (Common.Vertices vertices')
            else body.CreateChainShape (Common.Vertices vertices')
        bodyShape.Tag <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              BodyShapeIndex = match contourShape.PropertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
        PhysicsEngine2d.configureBodyShapeProperties bodyProperties contourShape.PropertiesOpt bodyShape
        Array.singleton bodyShape

    static member private attachBodyConvexHull bodySource bodyProperties (points : Vector3 array) transformOpt (propertiesOpt : BodyShapeProperties option) (body : Body) =
        assert Settings.UseConvexHullPolygons // NOTE: this approach seems to assume this.
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity transformOpt
        let points' = Array.zeroCreate points.Length
        for i in 0 .. dec points.Length do
            points'.[i] <- PhysicsEngine2d.toPhysicsV2 (points.[i].Transform transform)
        let density =
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass ->
                let box = points' |> Array.map (fun v -> v2 v.X v.Y) |> Box2.Enclose // TODO: perhaps use a Sphere or Circle instead?
                mass / (box.Width * box.Height)
        let density = max 0.001f density // NOTE: Aether has collision response issue when density is 0 even if it's for a static shape!
        let bodyShape = body.CreatePolygon (Common.Vertices points', density)
        bodyShape.Tag <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              BodyShapeIndex = match propertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
        PhysicsEngine2d.configureBodyShapeProperties bodyProperties propertiesOpt bodyShape
        bodyShape

    static member private attachBodyTriangles bodySource bodyProperties (vertices : Vector3 array) transformOpt (propertiesOpt : BodyShapeProperties option) (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity transformOpt
        let vertices' = Array.zeroCreate vertices.Length
        for i in 0 .. dec vertices.Length do
            vertices'.[i] <- PhysicsEngine2d.toPhysicsV2 (vertices.[i].Transform transform)
        let density =
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass ->
                let box = vertices' |> Array.map (fun v -> v2 v.X v.Y) |> Box2.Enclose // TODO: perhaps use a Sphere or Circle instead?
                mass / (box.Width * box.Height)
        let density = max 0.001f density // NOTE: Aether has collision response issue when density is 0 even if it's for a static shape!
        let triangles = vertices' |> Array.chunkBySize 3 |> Array.map Common.Vertices |> List
        let bodyShapes = body.CreateCompoundPolygon (triangles, density)
        for bodyShape in bodyShapes do
            bodyShape.Tag <-
                { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
                  BodyShapeIndex = match propertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
            PhysicsEngine2d.configureBodyShapeProperties bodyProperties propertiesOpt bodyShape
        Array.ofSeq bodyShapes

    static member private attachBodyBounds bodySource bodyProperties (points : Vector3 array) transformOpt (propertiesOpt : BodyShapeProperties option) (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity transformOpt
        let bounds = points |> Array.map _.V2 |> Box2.Enclose
        let corners = bounds.Corners
        let corners' = Array.zeroCreate points.Length
        for i in 0 .. dec corners.Length do
            corners'.[i] <- PhysicsEngine2d.toPhysicsV2 (corners.[i].V3.Transform transform)
        let density =
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass -> mass / (bounds.Width * bounds.Height)
        let density = max 0.001f density // NOTE: Aether has collision response issue when density is 0 even if it's for a static shape!
        let bodyShape = body.CreatePolygon (bounds.Corners |> Array.map (fun v -> Common.Vector2 (v.X, v.Y)) |> Common.Vertices, density)
        bodyShape.Tag <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              BodyShapeIndex = match propertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
        PhysicsEngine2d.configureBodyShapeProperties bodyProperties propertiesOpt bodyShape
        bodyShape

    static member private attachPointsShape bodySource bodyProperties (pointsShape : PointsShape) (body : Body) =
        match pointsShape.Profile with
        | Convex -> PhysicsEngine2d.attachBodyConvexHull bodySource bodyProperties pointsShape.Points pointsShape.TransformOpt pointsShape.PropertiesOpt body |> Array.singleton
        | Concave ->
            Log.warnOnce "Creating a compound polygon with PointsShape; PointsShape generally specifies individual points rather than triangulated vertices, so unintended behavior may arise."
            PhysicsEngine2d.attachBodyTriangles bodySource bodyProperties pointsShape.Points pointsShape.TransformOpt pointsShape.PropertiesOpt body
        | Bounds -> PhysicsEngine2d.attachBodyBounds bodySource bodyProperties pointsShape.Points pointsShape.TransformOpt pointsShape.PropertiesOpt body |> Array.singleton

    static member private attachGeometryShape bodySource bodyProperties (geometryShape : GeometryShape) body =
        match geometryShape.Profile with
        | Convex -> PhysicsEngine2d.attachBodyConvexHull bodySource bodyProperties geometryShape.Vertices geometryShape.TransformOpt geometryShape.PropertiesOpt body |> Array.singleton
        | Concave -> PhysicsEngine2d.attachBodyTriangles bodySource bodyProperties geometryShape.Vertices geometryShape.TransformOpt geometryShape.PropertiesOpt body
        | Bounds -> PhysicsEngine2d.attachBodyBounds bodySource bodyProperties geometryShape.Vertices geometryShape.TransformOpt geometryShape.PropertiesOpt body |> Array.singleton

    static member private attachBodyShapes bodySource bodyProperties bodyShapes (body : Body) =
        let list = List ()
        for bodyShape in bodyShapes do
            let bodyShapes = PhysicsEngine2d.attachBodyShape bodySource bodyProperties bodyShape body
            list.AddRange bodyShapes
        Array.ofSeq list

    static member private attachBodyShape bodySource bodyProperties bodyShape (body : Body) =
        match bodyShape with
        | EmptyShape -> [||]
        | BoxShape boxShape -> PhysicsEngine2d.attachBoxBody bodySource bodyProperties boxShape body |> Array.singleton
        | SphereShape sphereShape -> PhysicsEngine2d.attachSphereShape bodySource bodyProperties sphereShape body |> Array.singleton
        | CapsuleShape capsuleShape -> PhysicsEngine2d.attachCapsuleShape bodySource bodyProperties capsuleShape body |> Array.ofSeq
        | BoxRoundedShape boxRoundedShape -> PhysicsEngine2d.attachBoxRoundedShape bodySource bodyProperties boxRoundedShape body |> Array.ofSeq
        | EdgeShape edgeShape -> PhysicsEngine2d.attachEdgeShape bodySource bodyProperties edgeShape body
        | ContourShape contourShape -> PhysicsEngine2d.attachContourShape bodySource bodyProperties contourShape body
        | PointsShape pointsShape -> PhysicsEngine2d.attachPointsShape bodySource bodyProperties pointsShape body |> Array.ofSeq
        | GeometryShape geometryShape -> PhysicsEngine2d.attachGeometryShape bodySource bodyProperties geometryShape body
        | StaticModelShape _ -> [||]
        | StaticModelSurfaceShape _ -> [||]
        | TerrainShape _ -> [||]
        | BodyShapes bodyShapes -> PhysicsEngine2d.attachBodyShapes bodySource bodyProperties bodyShapes body

    static member private createBody (createBodyMessage : CreateBodyMessage) physicsEngine =

        // get fields
        let bodyId = createBodyMessage.BodyId
        let bodyProperties = createBodyMessage.BodyProperties
        let bodyRotation = bodyProperties.Rotation.Angle2d

        // make the body
        let body = physicsEngine.PhysicsContext.CreateBody (PhysicsEngine2d.toPhysicsV2 bodyProperties.Center, bodyRotation)
        body.Tag <- bodyId

        // configure body
        PhysicsEngine2d.configureBodyProperties bodyProperties body

        // attempt to attach body shape
        try PhysicsEngine2d.attachBodyShape bodyId.BodySource bodyProperties bodyProperties.BodyShape body |> ignore
        with :? ArgumentOutOfRangeException -> ()

        // listen for collisions
        body.add_OnCollision physicsEngine.PenetrationHandler
        body.add_OnSeparation physicsEngine.SeparationHandler

        // attempt to add the body
        let bodyId = { BodySource = createBodyMessage.BodyId.BodySource; BodyIndex = bodyProperties.BodyIndex }
        if not (physicsEngine.Bodies.TryAdd (bodyId, (bodyProperties.GravityOverride, body))) then
            Log.error ("Could not add body for '" + scstring bodyId + "'.")

        // attempt to run any related body joint creation functions
        match physicsEngine.CreateBodyJointMessages.TryGetValue bodyId with
        | (true, createBodyJointMessages) ->
            for createBodyJointMessage in createBodyJointMessages do
                let bodyJointId = { BodyJointSource = createBodyJointMessage.BodyJointSource; BodyJointIndex = createBodyJointMessage.BodyJointProperties.BodyJointIndex }
                PhysicsEngine2d.destroyBodyJointInternal bodyJointId physicsEngine
                PhysicsEngine2d.createBodyJointInternal createBodyJointMessage.BodyJointProperties bodyJointId physicsEngine
        | (false, _) -> ()

    static member private createBodies (createBodiesMessage : CreateBodiesMessage) physicsEngine =
        List.iter
            (fun (bodyProperties : BodyProperties) ->
                let createBodyMessage =
                    { BodyId = { BodySource = createBodiesMessage.BodySource; BodyIndex = bodyProperties.BodyIndex }
                      BodyProperties = bodyProperties }
                PhysicsEngine2d.createBody createBodyMessage physicsEngine)
            createBodiesMessage.BodiesProperties

    static member private destroyBody (destroyBodyMessage : DestroyBodyMessage) physicsEngine =

        // attempt to run any related body joint destruction functions
        let bodyId = destroyBodyMessage.BodyId
        match physicsEngine.CreateBodyJointMessages.TryGetValue bodyId with
        | (true, createBodyJointMessages) ->
            for createBodyJointMessage in createBodyJointMessages do
                let bodyJointId = { BodyJointSource = createBodyJointMessage.BodyJointSource; BodyJointIndex = createBodyJointMessage.BodyJointProperties.BodyJointIndex }
                PhysicsEngine2d.destroyBodyJointInternal bodyJointId physicsEngine
        | (false, _) -> ()

        // attempt to destroy body
        match physicsEngine.Bodies.TryGetValue bodyId with
        | (true, (_, body)) ->
            physicsEngine.Bodies.Remove bodyId |> ignore
            physicsEngine.PhysicsContext.Remove body
        | (false, _) -> ()

    static member private destroyBodies (destroyBodiesMessage : DestroyBodiesMessage) physicsEngine =
        List.iter (fun bodyId ->
            PhysicsEngine2d.destroyBody { BodyId = bodyId } physicsEngine)
            destroyBodiesMessage.BodyIds

    static member private createBodyJointInternal bodyJointProperties bodyJointId physicsEngine =
        let resultOpt =
            match bodyJointProperties.BodyJoint with
            | EmptyJoint ->
                None
            | OneBodyJoint2d oneBodyJoint ->
                let bodyId = bodyJointProperties.BodyJointTarget
                match physicsEngine.Bodies.TryGetValue bodyId with
                | (true, (_, body)) ->
                    let joint = oneBodyJoint.CreateOneBodyJoint PhysicsEngine2d.toPhysics PhysicsEngine2d.toPhysicsV2 body
                    Some (joint, body, None)
                | (false, _) -> None
            | TwoBodyJoint2d twoBodyJoint ->
                let bodyId = bodyJointProperties.BodyJointTarget
                let body2IdOpt = bodyJointProperties.BodyJointTarget2Opt
                match body2IdOpt with
                | Some body2Id ->
                    match (physicsEngine.Bodies.TryGetValue bodyId, physicsEngine.Bodies.TryGetValue body2Id) with
                    | ((true, (_, body)), (true, (_, body2))) ->
                        let joint = twoBodyJoint.CreateTwoBodyJoint PhysicsEngine2d.toPhysics PhysicsEngine2d.toPhysicsV2 body body2
                        Some (joint, body, Some body2)
                    | _ -> None
                | None -> None
            | OneBodyJoint3d _ | TwoBodyJoint3d _ ->
                Log.warn ("Joint type '" + getCaseName bodyJointProperties.BodyJoint + "' not implemented for PhysicsEngine2d.")
                None
        match resultOpt with
        | Some (joint, body, body2Opt) ->
            joint.Tag <- bodyJointId
            joint.Breakpoint <- PhysicsEngine2d.toPhysics bodyJointProperties.BreakingPoint
            joint.CollideConnected <- bodyJointProperties.CollideConnected
            joint.Enabled <- bodyJointProperties.BodyJointEnabled && not bodyJointProperties.Broken
            joint.add_Broke physicsEngine.BreakHandler
            body.Awake <- true
            match body2Opt with Some body2 -> body2.Awake <- true | None -> ()
            if physicsEngine.Joints.TryAdd (bodyJointId, joint)
            then physicsEngine.PhysicsContext.Add joint
            else Log.warn ("Could not add body joint for '" + scstring bodyJointId + "'.")
        | None -> ()

    static member private createBodyJoint (createBodyJointMessage : CreateBodyJointMessage) physicsEngine =

        // log creation message
        for bodyTargetOpt in [Some createBodyJointMessage.BodyJointProperties.BodyJointTarget; createBodyJointMessage.BodyJointProperties.BodyJointTarget2Opt] do
            match bodyTargetOpt with
            | Some bodyTarget ->
                match physicsEngine.CreateBodyJointMessages.TryGetValue bodyTarget with
                | (true, messages) -> messages.Add createBodyJointMessage
                | (false, _) -> physicsEngine.CreateBodyJointMessages.Add (bodyTarget, List [createBodyJointMessage])
            | None -> ()

        // attempt to add body joint
        let bodyJointId = { BodyJointSource = createBodyJointMessage.BodyJointSource; BodyJointIndex = createBodyJointMessage.BodyJointProperties.BodyJointIndex }
        PhysicsEngine2d.createBodyJointInternal createBodyJointMessage.BodyJointProperties bodyJointId physicsEngine

    static member private destroyBodyJointInternal (bodyJointId : BodyJointId) physicsEngine =
        match physicsEngine.Joints.TryGetValue bodyJointId with
        | (true, joint) ->
            physicsEngine.Joints.Remove bodyJointId |> ignore
            physicsEngine.PhysicsContext.Remove joint
        | (false, _) -> ()

    static member private destroyBodyJoint (destroyBodyJointMessage : DestroyBodyJointMessage) physicsEngine =

        // unlog creation message
        for bodyTargetOpt in [Some destroyBodyJointMessage.BodyJointTarget; destroyBodyJointMessage.BodyJointTarget2Opt] do
            match bodyTargetOpt with
            | Some bodyTarget ->
                match physicsEngine.CreateBodyJointMessages.TryGetValue bodyTarget with
                | (true, messages) ->
                    messages.RemoveAll (fun message ->
                        message.BodyJointSource = destroyBodyJointMessage.BodyJointId.BodyJointSource &&
                        message.BodyJointProperties.BodyJointIndex = destroyBodyJointMessage.BodyJointId.BodyJointIndex)
                    |> ignore<int>
                | (false, _) -> ()
            | None -> ()

        // attempt to destroy body joint
        PhysicsEngine2d.destroyBodyJointInternal destroyBodyJointMessage.BodyJointId physicsEngine

    static member private createFluidEmitter (createFluidEmitterMessage : CreateFluidEmitterMessage) physicsEngine =
        let id = createFluidEmitterMessage.FluidEmitterId
        match createFluidEmitterMessage.FluidEmitterDescriptor with
        | FluidEmitterDescriptor2d descriptor ->
            if not (physicsEngine.FluidEmitters.ContainsKey id) then physicsEngine.FluidEmitters.Add (id, FluidEmitter2d.make descriptor)
            FluidEmitter2d.addParticles createFluidEmitterMessage.FluidParticles physicsEngine.FluidEmitters.[id]
        | FluidEmitterDescriptor3d -> () // no 3d fluid emitter support

    static member private destroyFluidEmitter (destroyFluidEmitterMessage : DestroyFluidEmitterMessage) physicsEngine =
        physicsEngine.FluidEmitters.Remove destroyFluidEmitterMessage.FluidEmitterId |> ignore

    static member private setBodyEnabled (setBodyEnabledMessage : SetBodyEnabledMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyEnabledMessage.BodyId with
        | (true, (_, body)) -> body.Enabled <- setBodyEnabledMessage.Enabled
        | (false, _) -> ()

    static member private setBodyCenter (setBodyCenterMessage : SetBodyCenterMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyCenterMessage.BodyId with
        | (true, (_, body)) ->
            let center = PhysicsEngine2d.toPhysicsV2 setBodyCenterMessage.Center
            if body.Position <> center then
                body.Position <- center
                do (body.Awake <- false; body.Awake <- true) // force sleep time to zero so that a transform message will be produced
        | (false, _) -> ()

    static member private setBodyRotation (setBodyRotationMessage : SetBodyRotationMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyRotationMessage.BodyId with
        | (true, (_, body)) ->
            let rotation = setBodyRotationMessage.Rotation.Angle2d
            if body.Rotation <> rotation then
                body.Rotation <- rotation
                do (body.Awake <- false; body.Awake <- true) // force sleep time to zero so that a transform message will be produced
        | (false, _) -> ()

    static member private setBodyLinearVelocity (setBodyLinearVelocityMessage : SetBodyLinearVelocityMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyLinearVelocityMessage.BodyId with
        | (true, (_, body)) -> body.LinearVelocity <- PhysicsEngine2d.toPhysicsV2 setBodyLinearVelocityMessage.LinearVelocity
        | (false, _) -> ()

    static member private setBodyAngularVelocity (setBodyAngularVelocityMessage : SetBodyAngularVelocityMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyAngularVelocityMessage.BodyId with
        | (true, (_, body)) -> body.AngularVelocity <- setBodyAngularVelocityMessage.AngularVelocity.Z
        | (false, _) -> ()

    static member private setBodyJointMotorEnabled (setBodyJointMotorEnabledMessage : SetBodyJointMotorEnabledMessage) physicsEngine =
        match physicsEngine.Joints.TryGetValue setBodyJointMotorEnabledMessage.BodyJointId with
        | (true, joint) ->
            match joint with
            | :? Dynamics.Joints.PrismaticJoint as joint -> joint.MotorEnabled <- setBodyJointMotorEnabledMessage.MotorEnabled
            | :? Dynamics.Joints.RevoluteJoint as joint -> joint.MotorEnabled <- setBodyJointMotorEnabledMessage.MotorEnabled
            | :? Dynamics.Joints.WheelJoint as joint -> joint.MotorEnabled <- setBodyJointMotorEnabledMessage.MotorEnabled
            | _ -> ()
        | (false, _) -> ()

    static member private setBodyJointMotorSpeed (setBodyJointMotorSpeedMessage : SetBodyJointMotorSpeedMessage) physicsEngine =
        match physicsEngine.Joints.TryGetValue setBodyJointMotorSpeedMessage.BodyJointId with
        | (true, joint) ->
            match joint with
            | :? Dynamics.Joints.PrismaticJoint as joint -> joint.MotorSpeed <- setBodyJointMotorSpeedMessage.MotorSpeed
            | :? Dynamics.Joints.RevoluteJoint as joint -> joint.MotorSpeed <- setBodyJointMotorSpeedMessage.MotorSpeed
            | :? Dynamics.Joints.WheelJoint as joint -> joint.MotorSpeed <- setBodyJointMotorSpeedMessage.MotorSpeed
            | _ -> ()
        | (false, _) -> ()

    static member private setBodyJointTargetAngle (setBodyJointTargetAngleMessage : SetBodyJointTargetAngleMessage) physicsEngine =
        match physicsEngine.Joints.TryGetValue setBodyJointTargetAngleMessage.BodyJointId with
        | (true, joint) ->
            match joint with
            | :? Dynamics.Joints.AngleJoint as joint -> joint.TargetAngle <- setBodyJointTargetAngleMessage.TargetAngle
            | _ -> ()
        | (false, _) -> ()

    static member private applyBodyLinearImpulse (applyBodyLinearImpulseMessage : ApplyBodyLinearImpulseMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyLinearImpulseMessage.BodyId with
        | (true, (_, body)) ->
            if not (Single.IsNaN applyBodyLinearImpulseMessage.LinearImpulse.X) then
                match applyBodyLinearImpulseMessage.OriginWorldOpt with
                | Some originWorld ->
                    body.ApplyLinearImpulse
                        (PhysicsEngine2d.toPhysicsV2 applyBodyLinearImpulseMessage.LinearImpulse,
                         PhysicsEngine2d.toPhysicsV2 originWorld)
                | None ->
                    body.ApplyLinearImpulse
                        (PhysicsEngine2d.toPhysicsV2 applyBodyLinearImpulseMessage.LinearImpulse)
            else Log.info ("Applying invalid linear impulse '" + scstring applyBodyLinearImpulseMessage.LinearImpulse + "'; this may destabilize Aether.")
        | (false, _) -> ()

    static member private applyBodyAngularImpulse (applyBodyAngularImpulseMessage : ApplyBodyAngularImpulseMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyAngularImpulseMessage.BodyId with
        | (true, (_, body)) ->
            if not (Single.IsNaN applyBodyAngularImpulseMessage.AngularImpulse.Z) then
                body.ApplyAngularImpulse (applyBodyAngularImpulseMessage.AngularImpulse.Z)
            else Log.info ("Applying invalid angular impulse '" + scstring applyBodyAngularImpulseMessage.AngularImpulse + "'; this may destabilize Aether.")
        | (false, _) -> ()

    static member private applyBodyForce (applyBodyForceMessage : ApplyBodyForceMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyForceMessage.BodyId with
        | (true, (_, body)) ->
            if not (Single.IsNaN applyBodyForceMessage.Force.X) then
                match applyBodyForceMessage.OriginWorldOpt with
                | Some originWorld ->
                    body.ApplyForce
                        (PhysicsEngine2d.toPhysicsV2 applyBodyForceMessage.Force,
                         PhysicsEngine2d.toPhysicsV2 originWorld)
                | None ->
                    body.ApplyForce
                        (PhysicsEngine2d.toPhysicsV2 applyBodyForceMessage.Force)
            else Log.info ("Applying invalid force '" + scstring applyBodyForceMessage.Force + "'; this may destabilize Aether.")
        | (false, _) -> ()

    static member private applyBodyTorque (applyBodyTorqueMessage : ApplyBodyTorqueMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyTorqueMessage.BodyId with
        | (true, (_, body)) ->
            if not (Single.IsNaN applyBodyTorqueMessage.Torque.Z) then
                body.ApplyTorque (applyBodyTorqueMessage.Torque.Z)
            else Log.info ("Applying invalid torque '" + scstring applyBodyTorqueMessage.Torque + "'; this may destabilize Aether.")
        | (false, _) -> ()

    static member private getBodyContactNormals bodyId physicsEngine =
        [|for contact in PhysicsEngine2d.getBodyContacts bodyId physicsEngine do
            let normal = fst (contact.GetWorldManifold ())
            if normal <> Common.Vector2.Zero then // may be zero if from broad phase but not in narrow phase
                let bodyShapeIndex = contact.FixtureA.Tag :?> BodyShapeIndex
                let normal = if bodyShapeIndex.BodyId = bodyId then -normal else normal // negate normal when appropriate
                Vector3 (normal.X, normal.Y, 0.0f)|]

    static member private getGravity bodyId physicsEngine =
        match physicsEngine.Bodies.TryGetValue bodyId with
        | (true, (gravityOpt, _)) -> gravityOpt |> Option.defaultWith (fun () -> (physicsEngine :> PhysicsEngine).Gravity)
        | (false, _) -> (physicsEngine :> PhysicsEngine).Gravity

    static member private getBodyToGroundContactNormals bodyId physicsEngine =
        PhysicsEngine2d.getBodyContactNormals bodyId physicsEngine
        |> Array.filter (fun contactNormal ->
            let upDirection = -(PhysicsEngine2d.getGravity bodyId physicsEngine).Normalized // Up is opposite of gravity
            let projectionToUp = contactNormal.Dot upDirection
            let theta = projectionToUp |> max -1.0f |> min 1.0f |> acos
            theta <= Constants.Physics.GroundAngleMax && projectionToUp > 0.0f)
 
    static member private getBodyToGroundContactNormalOpt bodyId physicsEngine =
        match PhysicsEngine2d.getBodyToGroundContactNormals bodyId physicsEngine with
        | [||] -> None
        | groundNormals ->
            let gravityDirection = (PhysicsEngine2d.getGravity bodyId physicsEngine).Normalized
            groundNormals
            |> Seq.map (fun normal -> struct (normal.Dot gravityDirection, normal))
            |> Seq.maxBy fst'
            |> snd'
            |> Some

    static member private jumpBody (jumpBodyMessage : JumpBodyMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue jumpBodyMessage.BodyId with
        | (true, (gravityOpt, body)) ->
            if  jumpBodyMessage.CanJumpInAir ||
                Array.notEmpty (PhysicsEngine2d.getBodyToGroundContactNormals jumpBodyMessage.BodyId physicsEngine) then
                let mutable gravity = gravityOpt |> Option.mapOrDefaultValue PhysicsEngine2d.toPhysicsV2 physicsEngine.PhysicsContext.Gravity
                gravity.Normalize ()
                body.LinearVelocity <- body.LinearVelocity - gravity * PhysicsEngine2d.toPhysics jumpBodyMessage.JumpSpeed
                body.Awake <- true
        | (false, _) -> ()

    static member private updateFluidEmitterMessage (updateFluidEmitterMessage : UpdateFluidEmitterMessage) physicsEngine =
        let id = updateFluidEmitterMessage.FluidEmitterId
        match physicsEngine.FluidEmitters.TryGetValue id with
        | (true, emitter) ->
            match updateFluidEmitterMessage.FluidEmitterDescriptor with
            | FluidEmitterDescriptor2d descriptor ->
                physicsEngine.FluidEmitters.[id] <- FluidEmitter2d.updateDescriptor descriptor emitter
            | FluidEmitterDescriptor3d -> () // no 3d fluid emitter support
        | (false, _) -> ()

    static member private emitFluidParticlesMessage (emitFluidParticlesMessage : EmitFluidParticlesMessage) physicsEngine =
        let id = emitFluidParticlesMessage.FluidEmitterId
        match physicsEngine.FluidEmitters.TryGetValue id with
        | (true, emitter) -> FluidEmitter2d.addParticles emitFluidParticlesMessage.FluidParticles emitter
        | (false, _) -> ()

    static member private setFluidParticlesMessage (setFluidParticlesMessage : SetFluidParticlesMessage) physicsEngine =
        let id = setFluidParticlesMessage.FluidEmitterId
        match physicsEngine.FluidEmitters.TryGetValue id with
        | (true, emitter) -> FluidEmitter2d.setParticles setFluidParticlesMessage.FluidParticles emitter
        | (false, _) -> ()

    static member private mapFluidParticlesMessage (mapFluidParticlesMessage : MapFluidParticlesMessage) physicsEngine =
        let id = mapFluidParticlesMessage.FluidEmitterId
        match physicsEngine.FluidEmitters.TryGetValue id with
        | (true, emitter) -> FluidEmitter2d.mapParticles mapFluidParticlesMessage.FluidParticleMapper emitter
        | (false, _) -> ()

    static member private filterFluidParticlesMessage (filterFluidParticlesMessage : FilterFluidParticlesMessage) physicsEngine =
        let id = filterFluidParticlesMessage.FluidEmitterId
        match physicsEngine.FluidEmitters.TryGetValue id with
        | (true, emitter) -> FluidEmitter2d.filterParticles filterFluidParticlesMessage.FluidParticlePredicate emitter
        | (false, _) -> ()

    static member private clearFluidParticlesMessage (id : FluidEmitterId) physicsEngine =
        match physicsEngine.FluidEmitters.TryGetValue id with
        | (true, emitter) -> FluidEmitter2d.clearParticles emitter
        | (false, _) -> ()

    static member private handlePhysicsMessage physicsEngine physicsMessage =
        match physicsMessage with
        | CreateBodyMessage createBodyMessage -> PhysicsEngine2d.createBody createBodyMessage physicsEngine
        | CreateBodiesMessage createBodiesMessage -> PhysicsEngine2d.createBodies createBodiesMessage physicsEngine
        | DestroyBodyMessage destroyBodyMessage -> PhysicsEngine2d.destroyBody destroyBodyMessage physicsEngine
        | DestroyBodiesMessage destroyBodiesMessage -> PhysicsEngine2d.destroyBodies destroyBodiesMessage physicsEngine
        | CreateBodyJointMessage createBodyJointMessage -> PhysicsEngine2d.createBodyJoint createBodyJointMessage physicsEngine
        | DestroyBodyJointMessage destroyBodyJointMessage -> PhysicsEngine2d.destroyBodyJoint destroyBodyJointMessage physicsEngine
        | CreateFluidEmitterMessage createFluidEmitterMessage -> PhysicsEngine2d.createFluidEmitter createFluidEmitterMessage physicsEngine
        | DestroyFluidEmitterMessage destroyFluidEmitterMessage -> PhysicsEngine2d.destroyFluidEmitter destroyFluidEmitterMessage physicsEngine
        | SetBodyEnabledMessage setBodyEnabledMessage -> PhysicsEngine2d.setBodyEnabled setBodyEnabledMessage physicsEngine
        | SetBodyCenterMessage setBodyCenterMessage -> PhysicsEngine2d.setBodyCenter setBodyCenterMessage physicsEngine
        | SetBodyRotationMessage setBodyRotationMessage -> PhysicsEngine2d.setBodyRotation setBodyRotationMessage physicsEngine
        | SetBodyLinearVelocityMessage setBodyLinearVelocityMessage -> PhysicsEngine2d.setBodyLinearVelocity setBodyLinearVelocityMessage physicsEngine
        | SetBodyAngularVelocityMessage setBodyAngularVelocityMessage -> PhysicsEngine2d.setBodyAngularVelocity setBodyAngularVelocityMessage physicsEngine
        | SetBodyVehicleForwardInputMessage _ -> () // no vehicle controller support
        | SetBodyVehicleRightInputMessage _ -> () // no vehicle controller support
        | SetBodyVehicleBrakeInputMessage _ -> () // no vehicle controller support
        | SetBodyVehicleHandBrakeInputMessage _ -> () // no vehicle controller support
        | SetBodyJointMotorEnabledMessage setBodyJointMotorEnabledMessage -> PhysicsEngine2d.setBodyJointMotorEnabled setBodyJointMotorEnabledMessage physicsEngine
        | SetBodyJointMotorSpeedMessage setBodyJointMotorSpeedMessage -> PhysicsEngine2d.setBodyJointMotorSpeed setBodyJointMotorSpeedMessage physicsEngine
        | SetBodyJointTargetAngleMessage setBodyJointTargetAngleMessage -> PhysicsEngine2d.setBodyJointTargetAngle setBodyJointTargetAngleMessage physicsEngine
        | ApplyBodyLinearImpulseMessage applyBodyLinearImpulseMessage -> PhysicsEngine2d.applyBodyLinearImpulse applyBodyLinearImpulseMessage physicsEngine
        | ApplyBodyAngularImpulseMessage applyBodyAngularImpulseMessage -> PhysicsEngine2d.applyBodyAngularImpulse applyBodyAngularImpulseMessage physicsEngine
        | ApplyBodyForceMessage applyBodyForceMessage -> PhysicsEngine2d.applyBodyForce applyBodyForceMessage physicsEngine
        | ApplyBodyTorqueMessage applyBodyTorqueMessage -> PhysicsEngine2d.applyBodyTorque applyBodyTorqueMessage physicsEngine
        | JumpBodyMessage jumpBodyMessage -> PhysicsEngine2d.jumpBody jumpBodyMessage physicsEngine
        | UpdateFluidEmitterMessage updateFluidEmitterMessage -> PhysicsEngine2d.updateFluidEmitterMessage updateFluidEmitterMessage physicsEngine
        | EmitFluidParticlesMessage emitFluidParticlesMessage -> PhysicsEngine2d.emitFluidParticlesMessage emitFluidParticlesMessage physicsEngine
        | SetFluidParticlesMessage setFluidParticlesMessage -> PhysicsEngine2d.setFluidParticlesMessage setFluidParticlesMessage physicsEngine
        | MapFluidParticlesMessage mapFluidParticlesMessage -> PhysicsEngine2d.mapFluidParticlesMessage mapFluidParticlesMessage physicsEngine
        | FilterFluidParticlesMessage filterFluidParticlesMessage -> PhysicsEngine2d.filterFluidParticlesMessage filterFluidParticlesMessage physicsEngine
        | ClearFluidParticlesMessage id -> PhysicsEngine2d.clearFluidParticlesMessage id physicsEngine
        | SetGravityMessage gravity -> physicsEngine.PhysicsContext.Gravity <- PhysicsEngine2d.toPhysicsV2 gravity

    static member private createIntegrationMessagesAndSleepAwakeStaticBodies physicsEngine =
        for bodyEntry in physicsEngine.Bodies do
            let (_, body) = bodyEntry.Value
            if body.Awake then

                // append transform message
                let bodyTransformMessage =
                    BodyTransformMessage
                        { BodyId = body.Tag :?> BodyId
                          Center = PhysicsEngine2d.toPixelV3 body.Position
                          Rotation = Quaternion.CreateFromAngle2d body.Rotation
                          LinearVelocity = PhysicsEngine2d.toPixelV3 body.LinearVelocity
                          AngularVelocity = v3 0.0f 0.0f body.AngularVelocity }
                physicsEngine.IntegrationMessages.Add bodyTransformMessage

                // manually sleep static bodies since aether won't sleep them itself
                if body.BodyType = Dynamics.BodyType.Static then body.Awake <- false

    static member private applyGravity physicsStepAmount physicsEngine =
        for bodyEntry in physicsEngine.Bodies do
            let (gravityOverride, body) = bodyEntry.Value
            if body.BodyType = Dynamics.BodyType.Dynamic then
                let gravity =
                    match gravityOverride with
                    | Some gravity -> PhysicsEngine2d.toPhysicsV2 gravity
                    | None -> physicsEngine.PhysicsContext.Gravity
                body.LinearVelocity <- body.LinearVelocity + gravity * physicsStepAmount

    /// Make a physics engine.
    static member make gravity =
        Settings.UseConvexHullPolygons <- true
        let integrationMessages = List ()
        let penetrationHandler = fun fixture fixture2 collision -> PhysicsEngine2d.handlePenetration fixture fixture2 collision integrationMessages
        let separationHandler = fun fixture fixture2 _ -> PhysicsEngine2d.handleSeparation fixture fixture2 integrationMessages
        let breakHandler = fun joint jointError -> PhysicsEngine2d.handleBreak joint jointError integrationMessages
        let physicsEngine =
            { PhysicsContext = World (PhysicsEngine2d.toPhysicsV2 gravity)
              Bodies = Dictionary<BodyId, Vector3 option * Dynamics.Body> HashIdentity.Structural
              Joints = Dictionary<BodyJointId, Dynamics.Joints.Joint> HashIdentity.Structural
              CreateBodyJointMessages = Dictionary<BodyId, CreateBodyJointMessage List> HashIdentity.Structural
              IntegrationMessages = integrationMessages
              PenetrationHandler = penetrationHandler
              SeparationHandler = separationHandler
              BreakHandler = breakHandler
              FluidEmitters = Dictionary<FluidEmitterId, FluidEmitter2d> HashIdentity.Structural }
        physicsEngine :> PhysicsEngine

    interface PhysicsEngine with

        member physicsEngine.GravityDefault =
            let gravityDefault = Common.Vector2 (Constants.Physics.GravityDefault.X, Constants.Physics.GravityDefault.Y)
            PhysicsEngine2d.toPixelV3 gravityDefault

        member physicsEngine.Gravity =
            PhysicsEngine2d.toPixelV3 physicsEngine.PhysicsContext.Gravity

        member physicsEngine.GetBodyExists bodyId =
            physicsEngine.Bodies.ContainsKey bodyId

        member physicsEngine.GetBodyContactNormals bodyId =
            PhysicsEngine2d.getBodyContactNormals bodyId physicsEngine

        member physicsEngine.GetBodyLinearVelocity bodyId =
            let (_, body) = physicsEngine.Bodies.[bodyId]
            PhysicsEngine2d.toPixelV3 body.LinearVelocity

        member physicsEngine.GetBodyAngularVelocity bodyId =
            let (_, body) = physicsEngine.Bodies.[bodyId]
            v3 0.0f 0.0f body.AngularVelocity

        member physicsEngine.GetBodyToGroundContactNormals bodyId =
            PhysicsEngine2d.getBodyToGroundContactNormals bodyId physicsEngine

        member physicsEngine.GetBodyToGroundContactNormalOpt bodyId =
            PhysicsEngine2d.getBodyToGroundContactNormalOpt bodyId physicsEngine

        member physicsEngine.GetBodyToGroundContactTangentOpt bodyId =
            match (physicsEngine :> PhysicsEngine).GetBodyToGroundContactNormalOpt bodyId with
            | Some normal -> Some (Vector3 (normal.Y, -normal.X, 0.0f))
            | None -> None

        member physicsEngine.GetBodyGrounded bodyId =
            let groundNormals = (physicsEngine :> PhysicsEngine).GetBodyToGroundContactNormals bodyId
            Array.notEmpty groundNormals

        member physicsEngine.GetBodySensor bodyId =
            let (_, body) = physicsEngine.Bodies.[bodyId]
            let mutable found = false
            let mutable sensor = false
            let mutable i = 0
            while i < body.FixtureList.Count && not found do
                let fixture = body.FixtureList.[i]
                let fixtureBodyId = (fixture.Tag :?> BodyShapeIndex).BodyId
                if fixtureBodyId = bodyId then
                    sensor <- fixture.IsSensor
                    found <- true
                i <- inc i
            sensor

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
                match joint with
                | :? Dynamics.Joints.PrismaticJoint as joint -> joint.MotorSpeed
                | :? Dynamics.Joints.RevoluteJoint as joint -> joint.MotorSpeed
                | :? Dynamics.Joints.WheelJoint as joint -> joint.MotorSpeed
                | _ -> 0.0f
            | (false, _) -> 0.0f

        member physicsEngine.GetBodyJointTargetAngle bodyJointId =
            match physicsEngine.Joints.TryGetValue bodyJointId with
            | (true, joint) ->
                match joint with
                | :? Dynamics.Joints.AngleJoint as joint -> joint.TargetAngle
                | _ -> 0.0f
            | (false, _) -> 0.0f

        member physicsEngine.RayCast (ray, collisionMask, closestOnly) =
            let results = List ()
            let mutable fractionMin = Single.MaxValue
            let mutable closestOpt = None
            let callback =
                RayCastReportFixtureDelegate (fun fixture point normal fraction ->
                    match fixture.Tag with
                    | :? BodyShapeIndex as bodyShapeIndex ->
                        if (int fixture.CollidesWith &&& collisionMask) <> 0 then
                            let report = BodyIntersection.make bodyShapeIndex fraction (PhysicsEngine2d.toPixelV3 point) (v3 normal.X normal.Y 0.0f)
                            if fraction < fractionMin then
                                fractionMin <- fraction
                                closestOpt <- Some report
                            results.Add report
                    | _ -> ()
                    if closestOnly then fraction else 1.0f)
            let point = PhysicsEngine2d.toPhysicsV2 ray.Origin
            let offset = PhysicsEngine2d.toPhysicsV2 ray.Direction
            physicsEngine.PhysicsContext.RayCast (callback, point, point + offset)
            if closestOnly then
                match closestOpt with
                | Some closest -> [|closest|]
                | None -> [||]
            else Array.ofSeq results

        member physicsEngine.ShapeCast (_, _, _, _, _) =
            Log.warn "ShapeCast not implemented for PhysicsEngine2d."
            [||] // TODO: P1: implement.

        member physicsEngine.HandleMessage physicsMessage =
            PhysicsEngine2d.handlePhysicsMessage physicsEngine physicsMessage

        member physicsEngine.TryIntegrate stepTime =

            // constrain step time
            let stepTime = stepTime.Seconds
            let stepTime =
                if stepTime > 0.0f && stepTime < 0.001f then 0.001f
                elif stepTime > 0.1f then 0.1f
                else stepTime

            // integrate only when time has passed
            if stepTime > 0.0f then
                PhysicsEngine2d.applyGravity stepTime physicsEngine
                physicsEngine.PhysicsContext.Step stepTime
                PhysicsEngine2d.createIntegrationMessagesAndSleepAwakeStaticBodies physicsEngine
                let gravity = (physicsEngine :> PhysicsEngine).Gravity.V2
                for KeyValue (emitterId, emitter) in physicsEngine.FluidEmitters do
                    let (particles, collisions) = FluidEmitter2d.step stepTime (gravity / Constants.Engine.Meter2d) emitter physicsEngine.PhysicsContext
                    physicsEngine.IntegrationMessages.Add
                        (FluidEmitterMessage
                            { FluidEmitterId = emitterId
                              FluidParticles = particles
                              FluidCollisions = collisions })
                let integrationMessages = SArray.ofSeq physicsEngine.IntegrationMessages
                physicsEngine.IntegrationMessages.Clear ()
                Some integrationMessages
            else None

        member physicsEngine.TryRender renderContext =
            match renderContext with
            | :? PhysicsEngine2dRenderContext as renderContext ->
                for bodyEntry in physicsEngine.Bodies do

                    // render fixtures in body
                    let (_, body) = bodyEntry.Value
                    let transform =
                        Matrix3x2.CreateRotation body.Rotation *
                        Matrix3x2.CreateTranslation (PhysicsEngine2d.toPixelV2 body.Position)
                    let eyeBounds = renderContext.EyeBounds
                    for fixture in body.FixtureList do

                        // compute color consistent with JoltSharp which defaults to MotionTypeColor: https://github.com/amerkoleci/JoltPhysicsSharp/blob/fbc0511c987043a16b6f985ae00633285ee56cb9/src/JoltPhysicsSharp/DrawSettings.cs#L33
                        // which is defined here: https://github.com/amerkoleci/JoltPhysicsSharp/blob/fbc0511c987043a16b6f985ae00633285ee56cb9/src/JoltPhysicsSharp/ShapeColor.cs#L20
                        let color =
                            match body.BodyType with
                            | BodyType.Dynamic -> // dynamic = random color per instance
                                bodyEntry.Key.GetHashCode () |> uint |> colorPacked |> _.WithA(1f)
                            | BodyType.Kinematic -> // keyframed
                                Color.Green
                            | _ -> // static or anything else
                                Color.Gray

                        // render shape
                        // TODO: see if we can optimize these by quickly getting the shape bounds and checking for its
                        // view intersection instead of per-edge checking.
                        match fixture.Shape with
                        | :? Collision.Shapes.PolygonShape as polygonShape ->
                            let vertices = polygonShape.Vertices
                            for i in 0 .. dec vertices.Count do
                                let start = (PhysicsEngine2d.toPixelV2 vertices[i]).Transform transform
                                let stop = (PhysicsEngine2d.toPixelV2 vertices[if i < dec vertices.Count then inc i else 0]).Transform transform
                                let bounds = Box2.Enclose (start, stop)
                                if eyeBounds.Contains bounds <> ContainmentType.Disjoint then
                                    renderContext.DrawLine (start, stop, color)
                        | :? Collision.Shapes.CircleShape as circleShape ->
                            let position = (PhysicsEngine2d.toPixelV2 circleShape.Position).Transform transform
                            let radius = PhysicsEngine2d.toPixel circleShape.Radius
                            if eyeBounds.Contains (box2 (position - v2 radius radius) (v2 radius radius * 2f)) <> ContainmentType.Disjoint then
                                renderContext.DrawCircle (position, radius, color)
                        | :? Collision.Shapes.EdgeShape as edgeShape ->
                            let start = (PhysicsEngine2d.toPixelV2 edgeShape.Vertex1).Transform transform
                            let stop = (PhysicsEngine2d.toPixelV2 edgeShape.Vertex2).Transform transform
                            let bounds = Box2.Enclose (start, stop)
                            if eyeBounds.Contains bounds <> ContainmentType.Disjoint then
                                renderContext.DrawLine (start, stop, color)
                        | :? Collision.Shapes.ChainShape as chainShape ->
                            let vertices = chainShape.Vertices
                            if vertices.Count >= 2 then // when looped, the link from last point to first point is already included
                                for i in 0 .. vertices.Count - 2 do
                                    let start = (PhysicsEngine2d.toPixelV2 vertices.[i]).Transform transform
                                    let stop = (PhysicsEngine2d.toPixelV2 vertices.[inc i]).Transform transform
                                    let bounds = Box2.Enclose (start, stop)
                                    if eyeBounds.Contains bounds <> ContainmentType.Disjoint then
                                        renderContext.DrawLine (start, stop, color)
                        | _ -> ()

            | _ -> ()

        member physicsEngine.ClearInternal () =
            physicsEngine.FluidEmitters.Clear ()
            physicsEngine.Joints.Clear ()
            physicsEngine.Bodies.Clear ()
            physicsEngine.CreateBodyJointMessages.Clear ()
            physicsEngine.IntegrationMessages.Clear ()
            physicsEngine.PhysicsContext.Clear ()

        member physicsEngine.CleanUp () =
            ()