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
open Prime
open Nu

// Original C# algorithm from https://github.com/klutch/Box2DFluid, with additions to collide with EdgeShape and
// ChainShape.
//
// It fixes collision detection by detecting the final particle position properly or particles would tunnel through
// EdgeShapes and ChainShapes, and added linear damping.
//
// NOTE: this simple implementation will be replaced with a more general library in the future that allows for
// particles influencing rigid bodies in the future.

/// Represents a neighbor particle during fluid simulation.
type [<Struct>] private FluidParticleNeighbor2d =

    { (* Assigned during find neighbors: *)
      mutable ParticleIndex : int // parallel for 1 output

      (* Assigned during calculate pressures: *)
      mutable Distance : single

      (* Assigned during calculate interaction forces: *)
      mutable AccumulatedDelta : Vector2 } // parallel for 1 output

/// When it comes to heavy computations like particle simulations, data-orientation is usually a reasonable first approach.
/// Mutable structs put the least pressure on the garbage collector, and are also the write targets of parallel for loops.
type [<Struct>] private FluidParticleState2d =

    { (* Global fields: *)
      mutable Position : Vector2 // updated during resolve collisions - parallel for 1 input, parallel for 2 in/output
      mutable Velocity : Vector2 // updated during calculate interaction forces, resolve collisions - parallel for 1 in/output, parallel for 2 in/output
      mutable GravityOverride : Vector2 voption
      mutable Cell : Vector2i // parallel for 1 input
    
      (* Assigned during scale particles: *)
      mutable ScaledPosition : Vector2 // parallel for 1 input
      mutable ScaledVelocity : Vector2 // parallel for 1 input

      (* Assigned during prepare simulation: *)
      mutable Delta : Vector2 // updated during calculate interaction forces, accumulate deltas - parallel for 1 output, parallel for 2 in/output
      mutable PotentialFixtureCount : int // updated during prepare collisions - parallel for 2 input
      mutable PotentialFixtures : Dynamics.Fixture array // updated during prepare collisions - parallel for 2 input
      mutable PotentialFixtureChildIndexes : int array // updated during prepare collisions - parallel for 2 input

      (* Assigned during find neighbors: *)
      mutable NeighborCount : int // parallel for 1 output
      mutable Neighbors : FluidParticleNeighbor2d array } // parallel for 1 output

/// Represents a 2d fluid emitter.
type private FluidEmitter2d =
    { Particles : FluidParticleState2d array
      Descriptor : FluidEmitterDescriptor2d
      ActivityLog : int HashSet
      Grid : Dictionary<Vector2i, int ResizeArray> }

    static let CellCapacityDefault = 20
    
    static let Neighborhood = [|for x in -1 .. 1 do for y in -1 .. 1 do v2i x y|]

    static let updateCell i (fluidEmitter : FluidEmitter2d) =
        let particle = &fluidEmitter.Particles[i]
        let newCell = FluidEmitter2d.positionToCell fluidEmitter.Descriptor.CellSize particle.Position
        if particle.Cell <> newCell then
            let cell = fluidEmitter.Grid.[particle.Cell]
            cell.Remove i |> ignore
            if cell.Count = 0 then fluidEmitter.Grid.Remove particle.Cell |> ignore
            match fluidEmitter.Grid.TryGetValue newCell with
            | (true, cell) -> cell.Add i
            | (false, _) ->
                let singleton = ResizeArray CellCapacityDefault
                singleton.Add i
                fluidEmitter.Grid.[newCell] <- singleton
            particle.Cell <- newCell

    static let toFluid (state : FluidParticleState2d byref) (particle : FluidParticle) (fluidEmitter : FluidEmitter2d) =
        state.Position <- particle.Position.V2 / fluidEmitter.Descriptor.Meter
        state.Velocity <- particle.Velocity.V2 / fluidEmitter.Descriptor.Meter
        state.GravityOverride <- particle.GravityOverride |> ValueOption.map (fun g -> g.V2 / fluidEmitter.Descriptor.Meter / 50.0f)

    static let fromFluid (state : FluidParticleState2d byref) (fluidEmitter : FluidEmitter2d) =
        { Position = (state.Position * fluidEmitter.Descriptor.Meter).V3
          Velocity = (state.Velocity * fluidEmitter.Descriptor.Meter).V3
          GravityOverride = state.GravityOverride |> ValueOption.map (fun g -> (g * fluidEmitter.Descriptor.Meter * 50.0f).V3) }

    static member positionToCell cellSize (position : Vector2) =
        v2i (floor (position.X / cellSize) |> int) (floor (position.Y / cellSize) |> int)

    static member cellToBox cellSize (cell : Vector2i) =
        box2 (cell.V2 * cellSize) (v2Dup cellSize)

    static member updateDescriptor (descriptor : FluidEmitterDescriptor2d) (fluidEmitter : FluidEmitter2d) =
        if  fluidEmitter.Descriptor.ParticlesMax <> descriptor.ParticlesMax ||
            fluidEmitter.Descriptor.Meter <> descriptor.Meter then
            // re-add all particles
            let newEmitter = FluidEmitter2d.make descriptor
            FluidEmitter2d.addParticles (fluidEmitter.ActivityLog |> Seq.map (fun i -> fromFluid &fluidEmitter.Particles[i] fluidEmitter)) newEmitter
            newEmitter
        elif fluidEmitter.Descriptor.CellSize <> descriptor.CellSize then
            // update cells
            let newEmitter = { fluidEmitter with Descriptor = descriptor }
            for i in newEmitter.ActivityLog do updateCell i newEmitter
            newEmitter
        else { fluidEmitter with Descriptor = descriptor } // minimal updates

    static member addParticles (particles : FluidParticle seq) (fluidEmitter : FluidEmitter2d) =
        let mutable i = 0
        let descriptor = fluidEmitter.Descriptor
        let particleEnumerator = particles.GetEnumerator ()
        if particleEnumerator.MoveNext () then
            let mutable continued = i <> descriptor.ParticlesMax
            while continued do
                let p = &fluidEmitter.Particles[i]
                if fluidEmitter.ActivityLog.Add i then
                    let particle = particleEnumerator.Current

                    // initialize particle
                    toFluid &p particle fluidEmitter

                    // initialize grid
                    let cell = FluidEmitter2d.positionToCell descriptor.CellSize p.Position
                    p.Cell <- cell
                    match fluidEmitter.Grid.TryGetValue cell with
                    | (true, resizeArray) -> resizeArray.Add i
                    | (false, _) ->
                        let singleton = ResizeArray CellCapacityDefault
                        singleton.Add i
                        fluidEmitter.Grid.[cell] <- singleton

                    continued <- particleEnumerator.MoveNext ()
                i <- inc i
                if i = descriptor.ParticlesMax then continued <- false

    static member filterParticles (filter : FluidParticle -> bool) (fluidEmitter : FluidEmitter2d) =
        fluidEmitter.ActivityLog.RemoveWhere (fun i ->
            let p = &fluidEmitter.Particles.[i]
            let remove = not (filter (fromFluid &p fluidEmitter))
            if remove then
                let cell = fluidEmitter.Grid.[p.Cell]
                cell.Remove i |> ignore
                if cell.Count = 0 then fluidEmitter.Grid.Remove p.Cell |> ignore
            remove) |> ignore

    static member mapParticles (mapping : FluidParticle -> FluidParticle) (fluidEmitter : FluidEmitter2d) =
        for i in fluidEmitter.ActivityLog do
            let particle = &fluidEmitter.Particles.[i]
            let newParticle = mapping (fromFluid &particle fluidEmitter)
            toFluid &particle newParticle fluidEmitter
            updateCell i fluidEmitter

    static member clear (fluidEmitter : FluidEmitter2d) =
        fluidEmitter.ActivityLog.Clear ()
        fluidEmitter.Grid.Clear ()

    static member step (clockDelta : single) (rawGravity : Vector2) (fluidEmitter : FluidEmitter2d) (context : Dynamics.World) =
        let descriptor = fluidEmitter.Descriptor
        let gravity = descriptor.GravityOverride |> Option.defaultValue (rawGravity / descriptor.Meter * clockDelta / 50.0f)

        // scale particles
        for i in fluidEmitter.ActivityLog do
            let particle = &fluidEmitter.Particles.[i]
            particle.ScaledPosition <- particle.Position * descriptor.InteractionScale
            particle.ScaledVelocity <- particle.Velocity * descriptor.InteractionScale

        // parallel for 1
        let loopResult = Parallel.ForEach (fluidEmitter.ActivityLog, fun i ->

            // collect sim properties
            let particleRadius = descriptor.ParticleRadius
            let descriptor = fluidEmitter.Descriptor
            let neighborsMax = descriptor.NeighborsMax
            let interactionScale = descriptor.InteractionScale
            let idealRadius = particleRadius * interactionScale
            let idealRadiusSquared = idealRadius * idealRadius
            let maxFixtures = descriptor.CollisionTestsMax

            // prepare simulation
            let particle = &fluidEmitter.Particles.[i]
            particle.Delta <- v2Zero
            particle.PotentialFixtureCount <- 0
            particle.PotentialFixtures <- ArrayPool.Shared.Rent maxFixtures
            particle.PotentialFixtureChildIndexes <- ArrayPool.Shared.Rent maxFixtures

            // find neighbors
            particle.NeighborCount <- 0
            particle.Neighbors <- ArrayPool.Shared.Rent neighborsMax
            let cell = particle.Cell
            for neighbor in
                Neighborhood
                |> Seq.collect (fun neighbour -> match fluidEmitter.Grid.TryGetValue (cell + neighbour) with (true, list) -> list :> _ seq | _ -> Seq.empty)
                |> Seq.truncate neighborsMax do
                if neighbor <> i then
                    particle.Neighbors.[particle.NeighborCount].ParticleIndex <- neighbor
                    particle.NeighborCount <- inc particle.NeighborCount

            // calculate pressures
            let mutable p = 0.0f
            let mutable pnear = 0.0f
            for n in 0 .. dec particle.NeighborCount do
                let neighbor = &particle.Neighbors.[n]
                let relativePosition = fluidEmitter.Particles.[neighbor.ParticleIndex].ScaledPosition - particle.ScaledPosition
                let distanceSquared = relativePosition.MagnitudeSquared
                if distanceSquared < idealRadiusSquared then
                    neighbor.Distance <- sqrt distanceSquared
                    let oneMinusQ = 1.0f - (neighbor.Distance / idealRadius)
                    p <- p + oneMinusQ * oneMinusQ
                    pnear <- pnear + oneMinusQ * oneMinusQ * oneMinusQ
                else neighbor.Distance <- nanf
            let pressure = (p - 5.0f) * 0.5f // normal pressure term
            let presnear = pnear * 0.5f // near particles term

            // calculate interaction forces
            for n in 0 .. dec particle.NeighborCount do
                let neighbor = &particle.Neighbors.[n]
                if not (Single.IsNaN neighbor.Distance) then

                    // compute pressure factor
                    let q = neighbor.Distance / idealRadius
                    let oneMinusQ = 1f - q
                    let factor = oneMinusQ * (pressure + presnear * oneMinusQ) / (2.0f * neighbor.Distance)
                    let relativePosition = fluidEmitter.Particles.[neighbor.ParticleIndex].ScaledPosition - particle.ScaledPosition
                    let mutable d = relativePosition * factor

                    // compute viscosity factor
                    let relativeVelocity = fluidEmitter.Particles.[neighbor.ParticleIndex].ScaledVelocity - particle.ScaledVelocity
                    let viscosityFactor = descriptor.Viscosity * oneMinusQ * clockDelta
                        
                    // accumulate deltas
                    d <- d - relativeVelocity * viscosityFactor
                    neighbor.AccumulatedDelta <- d
                    particle.Delta <- particle.Delta - d

                else neighbor.AccumulatedDelta <- v2Zero

            // apply gravity to velocity
            match particle.GravityOverride with
            | ValueSome g -> particle.Velocity <- particle.Velocity + g * clockDelta
            | ValueNone -> particle.Velocity <- particle.Velocity + gravity)
                        
        // assert loop completion
        assert loopResult.IsCompleted

        // accumulate deltas
        for i in fluidEmitter.ActivityLog do
            let particle = &fluidEmitter.Particles.[i]
            for n in 0 .. dec particle.NeighborCount do
                let neighbor = &particle.Neighbors.[n]
                fluidEmitter.Particles.[neighbor.ParticleIndex].Delta <- fluidEmitter.Particles.[neighbor.ParticleIndex].Delta + neighbor.AccumulatedDelta
        for i in fluidEmitter.ActivityLog do
            fluidEmitter.Particles.[i].Delta <- fluidEmitter.Particles.[i].Delta / fluidEmitter.Descriptor.InteractionScale * (1f - descriptor.LinearDamping)

        // prepare collisions
        let toPhysicsV2 (v : Vector2) = Common.Vector2 (v.X, v.Y) / Constants.Engine.Meter2d
        let mutable aabb = AABB (toPhysicsV2 descriptor.SimulationBounds.Min, toPhysicsV2 descriptor.SimulationBounds.Max)
        let query (fixture : Dynamics.Fixture) = 
            let physicsToFluid (v : Common.Vector2) = Vector2 (v.X, v.Y) * Constants.Engine.Meter2d / fluidEmitter.Descriptor.Meter
            let cellSize = fluidEmitter.Descriptor.CellSize
            let mutable aabb = Unchecked.defaultof<_>
            let mutable transform = Unchecked.defaultof<_>
            fixture.Body.GetTransform &transform
            for c in 0 .. dec fixture.Shape.ChildCount do // chain shapes have edges as children, other shapes only have 1 child
                fixture.Shape.ComputeAABB (&aabb, &transform, c)
                let lowerBound = FluidEmitter2d.positionToCell cellSize (physicsToFluid aabb.LowerBound)
                let upperBound = FluidEmitter2d.positionToCell cellSize (physicsToFluid aabb.UpperBound)
                for gridX in dec lowerBound.X .. inc upperBound.X do // expand grid by one in case some fixtures perfectly align on cell boundary
                    for gridY in dec lowerBound.Y .. inc upperBound.Y do
                        match fluidEmitter.Grid.TryGetValue (v2i gridX gridY) with
                        | (true, particleIndexes) ->
                            for i in particleIndexes do
                                let particle = &fluidEmitter.Particles.[i]
                                if particle.PotentialFixtureCount < fluidEmitter.Descriptor.CollisionTestsMax then
                                    particle.PotentialFixtures.[particle.PotentialFixtureCount] <- fixture
                                    particle.PotentialFixtureChildIndexes.[particle.PotentialFixtureCount] <- c
                                    particle.PotentialFixtureCount <- inc particle.PotentialFixtureCount
                        | (false, _) -> ()
            true
        context.QueryAABB (query, &aabb)

        // parallel for 2 - resolve collisions
        let collisions = ConcurrentBag ()
        let loopResult = Parallel.ForEach (fluidEmitter.ActivityLog, fun i ->
            let physicsToFluid (v : Common.Vector2) = Vector2 (v.X, v.Y) * Constants.Engine.Meter2d / fluidEmitter.Descriptor.Meter
            let physicsToFluidNormal (v : Common.Vector2) = Vector2 (v.X, v.Y)
            let fluidToPhysics (v : Vector2) = Common.Vector2 (v.X, v.Y) / Constants.Engine.Meter2d * fluidEmitter.Descriptor.Meter
            let particle = &fluidEmitter.Particles.[i]
            for f in 0 .. dec particle.PotentialFixtureCount do
                let fixture = particle.PotentialFixtures.[f]
                let newPosition = particle.Position + particle.Velocity + particle.Delta * 2.0f
                let mutable isColliding = false
                let mutable closestPoint = v2Zero
                let mutable normal = v2Zero
                let (|EdgeFromEdgeShape|) (shape : Shapes.EdgeShape) = (shape.Vertex1, shape.Vertex2)
                let (|EdgeFromChainShape|) (lookup : _ array) index (shape : Shapes.ChainShape) = (shape.Vertices.[lookup.[index]], shape.Vertices.[inc lookup.[index]])
                match fixture.Shape with
                | :? Shapes.PolygonShape as shape ->
                    let mutable newPosition = fluidToPhysics newPosition
                    if fixture.TestPoint &newPosition then
                        isColliding <- true
                        let mutable collisionXF = Unchecked.defaultof<_>
                        fixture.Body.GetTransform &collisionXF
                        let mutable shortestDistance = infinityf // Find closest edge
                        for v in 0 .. dec shape.Vertices.Count do

                            // transform the shape's vertices from local space to world space
                            let collisionVertex = Common.Transform.Multiply (shape.Vertices.[v], &collisionXF) |> physicsToFluid

                            // transform the shape's normals using the rotation (Complex) part of the transform
                            let collisionNormal = Common.Complex.Multiply (shape.Normals.[v], &collisionXF.q) |> physicsToFluidNormal

                            // project the vertex position relative to the particle position onto the edge's normal to find the distance
                            let distance = Vector2.Dot (collisionNormal, collisionVertex - particle.Position)
                            if distance < shortestDistance then
                                shortestDistance <- distance
                                closestPoint <- collisionNormal * distance + particle.Position // push the particle out of the shape in the direction of the closest edge's normal
                                normal <- collisionNormal

                | :? Shapes.CircleShape as shape ->
                    let mutable newPosition = fluidToPhysics newPosition
                    if fixture.TestPoint &newPosition then
                        isColliding <- true
                        // push the particle out of the circle by normalizing the circle's center relative to the
                        // particle position, and pushing the particle out in the direction of the normal
                        let center = shape.Position + fixture.Body.Position |> physicsToFluid
                        normal <- (particle.Position - center).Normalized
                        closestPoint <- center + normal * shape.Radius * Constants.Engine.Meter2d / fluidEmitter.Descriptor.Meter

                | (:? Shapes.EdgeShape as EdgeFromEdgeShape (edgeStart, edgeEnd))
                | (:? Shapes.ChainShape as EdgeFromChainShape particle.PotentialFixtureChildIndexes f (edgeStart, edgeEnd)) ->

                    // collision with an edge - use line-segment intersection

                    // transform the shape's vertices from local space to world space
                    let mutable collisionXF = Unchecked.defaultof<_>
                    fixture.Body.GetTransform &collisionXF
                    let edgeStart = physicsToFluid (Common.Transform.Multiply (edgeStart, &collisionXF))
                    let edgeEnd = physicsToFluid (Common.Transform.Multiply (edgeEnd, &collisionXF))

                    let edgeSegment = edgeEnd - edgeStart
                    let particleMovement = newPosition - particle.Position

                    // shim for .NET 10 Vector2.Cross (Vector2, Vector2). TODO: use it when we upgrade to .NET 10.
                    let vector2Cross (v1 : Vector2, v2 : Vector2) = v1.X * v2.Y - v1.Y * v2.X
                    let cross_particleMovement_edgeSegment = vector2Cross (particleMovement, edgeSegment)
                    if abs cross_particleMovement_edgeSegment > 1e-6f then // non-collinear

                        // standard segment intersection formula:
                        // let A = edgeStart, B = edgeEnd (edge: A + u*(B-A))
                        // let C = particle.Position, D = newPosition (particle: C + t*(D-C))
                        let AC = edgeStart - particle.Position
                        let t = vector2Cross (AC, edgeSegment) / cross_particleMovement_edgeSegment // t = (AC × AB) / (CD × AB)
                        let u = vector2Cross (AC, particleMovement) / cross_particleMovement_edgeSegment // u = (AC × CD) / (CD × AB)  

                        // after solving t and u, the collision is only counted if the intersection point is within
                        // segments.
                        if t >= 0.0f && t <= 1.0f && u >= 0.0f && u <= 1.0f then
                            isColliding <- true
                            closestPoint <- edgeStart + u * edgeSegment

                            // for two-sided collision, normal should point away from edge surface
                            let edgeNormal = Vector2.Normalize (Vector2 (-edgeSegment.Y, edgeSegment.X))

                            // determine which side the particle is approaching from
                            let approachDirection = Vector2.Normalize particleMovement
                            let dotProduct = Vector2.Dot (edgeNormal, approachDirection)

                            // if particle is moving toward the normal, keep it; otherwise flip
                            normal <- if dotProduct < 0.0f then edgeNormal else -edgeNormal

                    else

                        // handle collinear case - particle moving parallel to edge.
                        // this can be implemented using point-line distance checks.
                        let edgeLengthSquared = edgeSegment.LengthSquared ()
                        if edgeLengthSquared > 1e-6f then

                            // project particle path onto edge to find closest approach
                            let toParticleStart = particle.Position - edgeStart
                            let projection = Vector2.Dot (toParticleStart, edgeSegment) / edgeLengthSquared
                            let closestOnEdge = edgeStart + saturate projection * edgeSegment

                            // check if particle path comes close to the edge
                            let approachVector = closestOnEdge - particle.Position
                            let distanceSquared = approachVector.LengthSquared ()
                            let collisionRadius = fluidEmitter.Descriptor.ParticleRadius
                            
                            // push out using perpendicular to edge as normal when within collision radius
                            if distanceSquared <= collisionRadius * collisionRadius then
                                isColliding <- true
                                closestPoint <- closestOnEdge
                                normal <- Vector2.Normalize (Vector2 (-edgeSegment.Y, edgeSegment.X)) // use perpendicular to edge for normal in collinear case

                | shape -> Log.warnOnce $"Shape not implemented: {shape}"

                // handle collision response
                if isColliding then
                    collisions.Add
                        { Particle = fromFluid &particle fluidEmitter
                          BodyShapeIndex = fixture.Tag :?> BodyShapeIndex
                          ClosestPoint = (closestPoint * fluidEmitter.Descriptor.Meter).V3
                          Normal = normal.V3 }
                    if not fixture.IsSensor then
                        particle.Position <- closestPoint + 0.05f * normal
                        particle.Velocity <- (particle.Velocity - 1.2f * Vector2.Dot (particle.Velocity, normal) * normal) * 0.85f
                        particle.Delta <- v2Zero
                  
                // don't leak memory for this fixture
                particle.PotentialFixtures.[f] <- null)

        // assert loop completion
        assert loopResult.IsCompleted

        // relocate particles
        fluidEmitter.ActivityLog.RemoveWhere (fun i ->
            let particle = &fluidEmitter.Particles.[i]

            particle.Velocity <- particle.Velocity + particle.Delta
            // NOTE: original code applies delta twice to position (Velocity already contains a Delta).
            // The collision test was updated to test for 2 * Delta movement rather than trying to fix this update.
            particle.Position <- particle.Position + particle.Velocity + particle.Delta

            ArrayPool.Shared.Return particle.PotentialFixtureChildIndexes
            ArrayPool.Shared.Return particle.PotentialFixtures
            ArrayPool.Shared.Return particle.Neighbors

            let removed = fluidEmitter.Descriptor.SimulationBounds.Contains (particle.Position * fluidEmitter.Descriptor.Meter) = ContainmentType.Disjoint
            if removed then
                let cell = fluidEmitter.Grid.[particle.Cell]
                cell.Remove i |> ignore
                if cell.Count = 0 then fluidEmitter.Grid.Remove particle.Cell |> ignore
            else updateCell i fluidEmitter
            removed) |> ignore

        // return state
        let state = SArray.zeroCreate fluidEmitter.ActivityLog.Count
        let mutable j = 0
        for i in fluidEmitter.ActivityLog do
            let p = &fluidEmitter.Particles.[i]
            state.[j] <- fromFluid &p fluidEmitter
            j <- inc j
        (state, collisions)

    static member make descriptor =
        { Particles = Array.zeroCreate descriptor.ParticlesMax
          Descriptor = descriptor
          ActivityLog = HashSet (descriptor.ParticlesMax, HashIdentity.Structural)
          Grid = Dictionary HashIdentity.Structural }