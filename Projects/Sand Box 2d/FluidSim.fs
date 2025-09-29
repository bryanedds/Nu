namespace SandBox2d
open System
open System.Buffers
open System.Collections.Generic
open System.Numerics
open System.Threading.Tasks
open nkast.Aether.Physics2D
open nkast.Aether.Physics2D.Collision
open Prime
open Nu

/// Describes a particle used in the following fluid simulation.
type [<Struct>] FluidParticle =
    { Position : Vector2
      Velocity : Vector2 }

/// When it comes to heavy computations like particle simulations, data-orientation is usually a reasonable first approach.
/// Mutable structs put the least pressure on the garbage collector, and are also the write targets of parallel for loops.
/// Original C# algorithm from https://github.com/klutch/Box2DFluid, with additions to collide with EdgeShape and ChainShape.
/// also fixed collision detection by detecting the final particle position properly or particles would tunnel through
/// EdgeShapes and ChainShapes, and added linear damping.
type [<Struct>] FluidParticleState =

    { (* Assigned during initialize particles: *)
      mutable Position : Vector2 // updated during resolve collisions - parallel for 1 input, parallel for 2 in/output
      mutable Velocity : Vector2 // updated during calculate interaction forces, resolve collisions - parallel for 1 in/output, parallel for 2 in/output
      mutable ScaledParticle : FluidParticle // parallel for 1 input

      (* Assigned during initialize grid: *)
      mutable Cell : Vector2i // parallel for 1 input

      (* Assigned during prepare simulation: *)
      mutable Delta : Vector2 // updated during calculate interaction forces, accumulate deltas - parallel for 1 output, parallel for 2 in/output
      mutable PotentialFixtureCount : int // updated during prepare collisions - parallel for 2 input
      mutable PotentialFixtures : Dynamics.Fixture array // updated during prepare collisions - parallel for 2 input
      mutable PotentialFixtureChildIndexes : int array // updated during prepare collisions - parallel for 2 input

      (* Assigned during find neighbors: *)
      mutable NeighborCount : int // parallel for 1 output
      mutable Neighbors : FluidParticleNeighbor array } // parallel for 1 output

and [<Struct>] FluidParticleNeighbor =

    { (* Assigned during find neighbors: *)
      mutable ParticleIndex : int // parallel for 1 output

      (* Assigned during calculate pressures: *)
      mutable Distance : single

      (* Assigned during calculate interaction forces: *)
      mutable AccumulatedDelta : Vector2 } // parallel for 1 output

// this extends the Entity API to expose the user-defined properties.
[<AutoOpen>]
module FluidSystemExtensions =
    type Entity with

        /// The fluid particles currently in the simulation.
        member this.GetFluidParticles world : FStack<FluidParticle> = this.Get (nameof Entity.FluidParticles) world
        member this.SetFluidParticles (value : FStack<FluidParticle>) world = this.Set (nameof Entity.FluidParticles) value world
        member this.FluidParticles = lens (nameof Entity.FluidParticles) this this.GetFluidParticles this.SetFluidParticles

        /// The current number of fluid particles in the simulation.
        member this.GetFluidParticleCount world : int = this.Get (nameof Entity.FluidParticleCount) world
        member this.SetFluidParticleCount (value : int) world = this.Set (nameof Entity.FluidParticleCount) value world
        member this.FluidParticleCount = lens (nameof Entity.FluidParticleCount) this this.GetFluidParticleCount this.SetFluidParticleCount

        /// The maximum number of fluid particles allowed in the simulation at any time.
        member this.GetFluidParticleMax world : int = this.Get (nameof Entity.FluidParticleMax) world
        member this.SetFluidParticleMax (value : int) world = this.Set (nameof Entity.FluidParticleMax) value world
        member this.FluidParticleMax = lens (nameof Entity.FluidParticleMax) this this.GetFluidParticleMax this.SetFluidParticleMax

        /// The maximum number of neighboring particles considered for each particle during force and pressure calculations.
        member this.GetFluidParticleNeighborMax world : int = this.Get (nameof Entity.FluidParticleNeighborMax) world
        member this.SetFluidParticleNeighborMax (value : int) world = this.Set (nameof Entity.FluidParticleNeighborMax) value world
        member this.FluidParticleNeighborMax = lens (nameof Entity.FluidParticleNeighborMax) this this.GetFluidParticleNeighborMax this.SetFluidParticleNeighborMax

        /// The base radius of each fluid particle, used for collision and interaction calculations.
        member this.GetFluidParticleRadius world : single = this.Get (nameof Entity.FluidParticleRadius) world
        member this.SetFluidParticleRadius (value : single) world = this.Set (nameof Entity.FluidParticleRadius) value world
        member this.FluidParticleRadius = lens (nameof Entity.FluidParticleRadius) this this.GetFluidParticleRadius this.SetFluidParticleRadius

        /// The ideal interaction radius for particles, as a multiple of Entity.FluidParticleRadius. Particles within this distance are considered neighbors and interact.
        member this.GetFluidParticleInteractionScale world : single = this.Get (nameof Entity.FluidParticleInteractionScale) world
        member this.SetFluidParticleInteractionScale (value : single) world = this.Set (nameof Entity.FluidParticleInteractionScale) value world
        member this.FluidParticleInteractionScale = lens (nameof Entity.FluidParticleInteractionScale) this this.GetFluidParticleInteractionScale this.SetFluidParticleInteractionScale

        /// The width and height of each grid cell used for spatial partitioning, as a multiple of Entity.FluidParticleRadius.
        member this.GetFluidParticleCellScale world : single = this.Get (nameof Entity.FluidParticleCellScale) world
        member this.SetFluidParticleCellScale (value : single) world = this.Set (nameof Entity.FluidParticleCellScale) value world
        member this.FluidParticleCellScale = lens (nameof Entity.FluidParticleCellScale) this this.GetFluidParticleCellScale this.SetFluidParticleCellScale

        /// When set to a color, the simulation will render the spatial grid cells for debugging or visualization.
        member this.GetFluidParticleCellColor world : Color option = this.Get (nameof Entity.FluidParticleCellColor) world
        member this.SetFluidParticleCellColor (value : Color option) world = this.Set (nameof Entity.FluidParticleCellColor) value world
        member this.FluidParticleCellColor = lens (nameof Entity.FluidParticleCellColor) this this.GetFluidParticleCellColor this.SetFluidParticleCellColor

        /// The maximum number of collision bodies to test against each particle during collision resolution.
        member this.GetFluidParticleCollisionTestsMax world : int = this.Get (nameof Entity.FluidParticleCollisionTestsMax) world
        member this.SetFluidParticleCollisionTestsMax (value : int) world = this.Set (nameof Entity.FluidParticleCollisionTestsMax) value world
        member this.FluidParticleCollisionTestsMax = lens (nameof Entity.FluidParticleCollisionTestsMax) this this.GetFluidParticleCollisionTestsMax this.SetFluidParticleCollisionTestsMax

        /// The size of the particle image - when None, uses Entity.FluidParticleRadius.
        member this.GetFluidParticleImageSizeOverride world : Vector2 option = this.Get (nameof Entity.FluidParticleImageSizeOverride) world
        member this.SetFluidParticleImageSizeOverride (value : Vector2 option) world = this.Set (nameof Entity.FluidParticleImageSizeOverride) value world
        member this.FluidParticleImageSizeOverride = lens (nameof Entity.FluidParticleImageSizeOverride) this this.GetFluidParticleImageSizeOverride this.SetFluidParticleImageSizeOverride

        /// The viscosity coefficient for relative velocity.
        member this.GetFluidViscosity world : single = this.Get (nameof Entity.FluidViscosity) world
        member this.SetFluidViscosity (value : single) world = this.Set (nameof Entity.FluidViscosity) value world
        member this.FluidViscosity = lens (nameof Entity.FluidViscosity) this this.GetFluidViscosity this.SetFluidViscosity

type FluidSystemDispatcher () =
    inherit Entity2dDispatcher (true, false, false)

    // each particle is associated with a cell in a spatial grid for neighbor searching
    static let neighborhood =
        [|for x in -1 .. 1 do for y in -1 .. 1 do v2i x y|]

    // convert a position to a cell coordinate
    static let positionToCell cellSize (position : Vector2) =
        v2i (floor (position.X / cellSize) |> int) (floor (position.Y / cellSize) |> int)

    // convert a cell coordinate to a box for rendering
    static let cellToBox cellSize (cell : Vector2i) =
        box2 (cell.V2 * cellSize) (v2Dup cellSize)

    // here we define default property values
    static member Properties =
        [nonPersistent Entity.FluidParticles FStack.empty
         nonPersistent Entity.FluidParticleCount 0
         define Entity.FluidParticleMax 20000
         define Entity.FluidParticleCellColor None
         define Entity.FluidParticleNeighborMax 75
         define Entity.FluidParticleRadius (0.9f * Constants.Engine.Meter2d)
         define Entity.FluidParticleCellScale (0.6f / 0.9f)
         define Entity.FluidParticleInteractionScale (50f / 0.9f)
         define Entity.FluidParticleCollisionTestsMax 20
         define Entity.FluidParticleImageSizeOverride None
         define Entity.FluidViscosity 0.004f
         define Entity.LinearDamping 0f
         define Entity.GravityOverride None
         define Entity.InsetOpt None
         define Entity.ClipOpt None
         define Entity.StaticImage Assets.Gameplay.Fluid
         define Entity.Color Color.One
         define Entity.Blend Transparent
         define Entity.Emission Color.Zero
         define Entity.Flip FlipNone]

    // here we define the entity's top-level behavior
    override _.Update (fluidSystem, world) =
        let sourceParticles = fluidSystem.GetFluidParticles world
        if FStack.notEmpty sourceParticles then

            // collect sim properties
            let maxParticles = fluidSystem.GetFluidParticleMax world
            let maxNeighbors = fluidSystem.GetFluidParticleNeighborMax world
            let particleRadius = fluidSystem.GetFluidParticleRadius world / Constants.Engine.Meter2d
            let interactionScale = fluidSystem.GetFluidParticleInteractionScale world
            let idealRadius = particleRadius * interactionScale
            let idealRadiusSquared = idealRadius * idealRadius
            let cellSize = particleRadius * fluidSystem.GetFluidParticleCellScale world
            let maxFixtures = fluidSystem.GetFluidParticleCollisionTestsMax world
            let physicalViscosity = fluidSystem.GetFluidViscosity world
            let linearDamping = fluidSystem.GetLinearDamping world
            let deltaTime = world.ClockDelta
            let gravity =
                (fluidSystem.GetGravityOverride world |> Option.defaultValue (World.getGravity2d world)).V2 /
                Constants.Engine.Meter2d * deltaTime /
                50f

            // initialize particles and grid
            let particleStates = ArrayPool<FluidParticleState>.Shared.Rent maxParticles
            let mutable activeParticleCount = 0
            let grid = Collections.Generic.Dictionary ()
            for particle in sourceParticles |> Seq.truncate maxParticles do

                // initialize particles - all internal calculations use physics engine units, so divide by meter2d.
                particleStates.[activeParticleCount].Position <- particle.Position / Constants.Engine.Meter2d
                particleStates.[activeParticleCount].Velocity <- particle.Velocity / Constants.Engine.Meter2d
                particleStates.[activeParticleCount].ScaledParticle <-
                    { Position = particleStates.[activeParticleCount].Position * interactionScale
                      Velocity = particleStates.[activeParticleCount].Velocity * interactionScale }

                // initialize grid
                let cell = positionToCell cellSize particleStates.[activeParticleCount].Position
                particleStates.[activeParticleCount].Cell <- cell
                match grid.TryGetValue cell with
                | (true, list) -> grid.[cell] <- activeParticleCount :: list
                | (false, _) -> grid.[cell] <- [activeParticleCount]
                activeParticleCount <- inc activeParticleCount

            // parallel for 1
            let loopResult = Parallel.For (0, activeParticleCount, fun i ->

                // prepare simulation
                let particle = &particleStates.[i]
                particle.Delta <- v2Zero
                particle.PotentialFixtureCount <- 0
                particle.PotentialFixtures <- Buffers.ArrayPool.Shared.Rent maxFixtures
                particle.PotentialFixtureChildIndexes <- Buffers.ArrayPool.Shared.Rent maxFixtures

                // find neighbors
                particle.NeighborCount <- 0
                particle.Neighbors <- Buffers.ArrayPool.Shared.Rent maxNeighbors
                let cell = particle.Cell
                for neighbor in
                    neighborhood
                    |> Seq.collect (fun neighbour -> match grid.TryGetValue (cell + neighbour) with (true, list) -> list | _ -> [])
                    |> Seq.truncate maxNeighbors do
                    if neighbor <> i then
                        particle.Neighbors.[particle.NeighborCount].ParticleIndex <- neighbor
                        particle.NeighborCount <- inc particle.NeighborCount

                // calculate pressures
                let mutable p = 0f
                let mutable pnear = 0f
                for n in 0 .. dec particle.NeighborCount do
                    let neighbor = &particle.Neighbors.[n]
                    let relativePosition = particleStates.[neighbor.ParticleIndex].ScaledParticle.Position - particle.ScaledParticle.Position
                    let distanceSquared = relativePosition.MagnitudeSquared
                    if distanceSquared < idealRadiusSquared then
                        neighbor.Distance <- sqrt distanceSquared
                        let oneMinusQ = 1f - (neighbor.Distance / idealRadius)
                        p <- p + oneMinusQ * oneMinusQ
                        pnear <- pnear + oneMinusQ * oneMinusQ * oneMinusQ
                    else neighbor.Distance <- nanf
                let pressure = (p - 5f) * 0.5f // normal pressure term
                let presnear = pnear * 0.5f // near particles term

                // calculate interaction forces
                for n in 0 .. dec particle.NeighborCount do
                    let neighbor = &particle.Neighbors.[n]
                    if not (Single.IsNaN neighbor.Distance) then

                        // compute pressure term
                        let q = neighbor.Distance / idealRadius
                        let oneMinusQ = 1f - q
                        let factor = oneMinusQ * (pressure + presnear * oneMinusQ) / (2f * neighbor.Distance)
                        let relativePosition = particleStates.[neighbor.ParticleIndex].ScaledParticle.Position - particle.ScaledParticle.Position
                        let mutable d = relativePosition * factor

                        // compute viscosity term
                        let relativeVelocity = particleStates.[neighbor.ParticleIndex].ScaledParticle.Velocity - particle.ScaledParticle.Velocity
                        let viscosityFactor = physicalViscosity * oneMinusQ * deltaTime
                        
                        // accumulate deltas
                        d <- d - relativeVelocity * viscosityFactor
                        neighbor.AccumulatedDelta <- d
                        particle.Delta <- particle.Delta - d

                    else neighbor.AccumulatedDelta <- v2Zero

                // apply velocity
                particle.Velocity <- particle.Velocity + gravity)
                        
            // assert loop completion
            assert loopResult.IsCompleted

            // accumulate deltas
            for i in 0 .. dec activeParticleCount do
                let particle = &particleStates.[i]
                for n in 0 .. dec particle.NeighborCount do
                    let neighbor = &particle.Neighbors.[n]
                    particleStates.[neighbor.ParticleIndex].Delta <- particleStates.[neighbor.ParticleIndex].Delta + neighbor.AccumulatedDelta
            for i in 0 .. dec activeParticleCount do
                particleStates.[i].Delta <- particleStates.[i].Delta / interactionScale * (1f - linearDamping)

            // prepare collisions
            World.iterateShapesInBounds2d (fun fixture body ->
                let mutable aabb = Unchecked.defaultof<_>
                let mutable transform = Unchecked.defaultof<_>
                body.GetTransform &transform
                for c in 0 .. dec fixture.Shape.ChildCount do // chain shapes have edges as children, other shapes only have 1 child
                    fixture.Shape.ComputeAABB (&aabb, &transform, c)
                    let lowerBound = positionToCell cellSize (v2 aabb.LowerBound.X aabb.LowerBound.Y)
                    let upperBound = positionToCell cellSize (v2 aabb.UpperBound.X aabb.UpperBound.Y)
                    for gridX in dec lowerBound.X .. inc upperBound.X do // expand grid by one in case some fixtures perfectly align on cell boundary
                        for gridY in dec lowerBound.Y .. inc upperBound.Y do
                            match grid.TryGetValue (v2i gridX gridY) with
                            | (true, particleIndexes) ->
                                for i in particleIndexes do
                                    let particle = &particleStates.[i]
                                    if particle.PotentialFixtureCount < maxFixtures then
                                        particle.PotentialFixtures.[particle.PotentialFixtureCount] <- fixture
                                        particle.PotentialFixtureChildIndexes.[particle.PotentialFixtureCount] <- c
                                        particle.PotentialFixtureCount <- inc particle.PotentialFixtureCount
                            | (false, _) -> ())
                (fluidSystem.GetBounds world)
                world

            // parallel for 2 - resolve collisions
            let loopResult = Parallel.For (0, activeParticleCount, fun i ->
                let convertVector (v : Common.Vector2) = Vector2 (v.X, v.Y)
                let particle = &particleStates.[i]
                for f in 0 .. dec particle.PotentialFixtureCount do
                    let fixture = particle.PotentialFixtures.[f]
                    let newPosition = particle.Position + particle.Velocity + particle.Delta * 2f
                    let mutable isColliding = false
                    let mutable closestPoint = v2Zero
                    let mutable normal = v2Zero
                    let (|EdgeFromEdgeShape|) (shape : Shapes.EdgeShape) = (shape.Vertex1, shape.Vertex2)
                    let (|EdgeFromChainShape|) (lookup : _ array) index (shape : Shapes.ChainShape) = (shape.Vertices.[lookup.[index]], shape.Vertices.[inc lookup.[index]])
                    match fixture.Shape with
                    | :? Shapes.PolygonShape as shape ->
                        let mutable newPosition = Common.Vector2 (newPosition.X, newPosition.Y)
                        if fixture.TestPoint &newPosition then
                            isColliding <- true
                            let mutable collisionXF = Unchecked.defaultof<_>
                            fixture.Body.GetTransform &collisionXF
                            let mutable shortestDistance = infinityf // Find closest edge
                            for v in 0 .. dec shape.Vertices.Count do

                                // transform the shape's vertices from local space to world space
                                let collisionVertex = Common.Transform.Multiply (shape.Vertices.[v], &collisionXF) |> convertVector

                                // transform the shape's normals using the rotation (Complex) part of the transform
                                let collisionNormal = Common.Complex.Multiply (shape.Normals.[v], &collisionXF.q) |> convertVector

                                // project the vertex position relative to the particle position onto the edge's normal to find the distance
                                let distance = Vector2.Dot (collisionNormal, collisionVertex - particle.Position)
                                if distance < shortestDistance then
                                    shortestDistance <- distance
                                    closestPoint <- collisionNormal * distance + particle.Position // push the particle out of the shape in the direction of the closest edge's normal
                                    normal <- collisionNormal

                    | :? Shapes.CircleShape as shape ->
                        let mutable newPosition = Common.Vector2 (newPosition.X, newPosition.Y)
                        if fixture.TestPoint &newPosition then
                            isColliding <- true
                            // push the particle out of the circle by normalizing the circle's center relative to the
                            // particle position, and pushing the particle out in the direction of the normal
                            let center = shape.Position + fixture.Body.Position |> convertVector
                            normal <- (particle.Position - center).Normalized
                            closestPoint <- center + normal * (shape.Radius / normal.Magnitude);

                    | (:? Shapes.EdgeShape as EdgeFromEdgeShape (edgeStart, edgeEnd))
                    | (:? Shapes.ChainShape as EdgeFromChainShape particle.PotentialFixtureChildIndexes f (edgeStart, edgeEnd)) ->

                        // collision with an edge - use line-segment intersection

                        // transform the shape's vertices from local space to world space
                        let mutable collisionXF = Unchecked.defaultof<_>
                        fixture.Body.GetTransform &collisionXF
                        let edgeStart = convertVector (Common.Transform.Multiply (edgeStart, &collisionXF))
                        let edgeEnd = convertVector (Common.Transform.Multiply (edgeEnd, &collisionXF))

                        let edgeSegment = edgeEnd - edgeStart
                        let particleMovement = newPosition - particle.Position
                        
                        // shim for .NET 10 Vector2.Cross(Vector2, Vector2). Use it when we upgrade to .NET 10
                        let vector2Cross (v1 : Vector2, v2 : Vector2) = v1.X * v2.Y - v1.Y * v2.X
                        let cross_particleMovement_edgeSegment = vector2Cross (particleMovement, edgeSegment)
                        if abs cross_particleMovement_edgeSegment > 1e-6f then // non-collinear

                            // standard segment intersection formula:
                            // let A = edgeStart, B = edgeEnd (edge: A + u*(B-A))
                            // let C = particle.Position, D = newPosition (particle: C + t*(D-C))
                            let AC = edgeStart - particle.Position
                            // t = (AC × AB) / (CD × AB)
                            let t = vector2Cross(AC, edgeSegment) / cross_particleMovement_edgeSegment
                            // u = (AC × CD) / (CD × AB)  
                            let u = vector2Cross(AC, particleMovement) / cross_particleMovement_edgeSegment

                            // after solving t and u, the collision is only counted if the intersection point is within
                            // segments.
                            if t >= 0f && t <= 1f && u >= 0f && u <= 1f then
                                isColliding <- true
                                closestPoint <- edgeStart + u * edgeSegment

                                // for two-sided collision, normal should point away from edge surface
                                let edgeNormal = Vector2.Normalize (Vector2 (-edgeSegment.Y, edgeSegment.X))

                                // determine which side the particle is approaching from
                                let approachDirection = Vector2.Normalize particleMovement
                                let dotProduct = Vector2.Dot (edgeNormal, approachDirection)

                                // if particle is moving toward the normal, keep it; otherwise flip
                                normal <- if dotProduct < 0f then edgeNormal else -edgeNormal

                        else

                            // handle collinear case - particle moving parallel to edge.
                            // this can be implemented using point-line distance checks.
                            let edgeLengthSquared = edgeSegment.LengthSquared ()
                            if edgeLengthSquared > 1e-6f then

                                // project particle path onto edge to find closest approach
                                let toParticleStart = particle.Position - edgeStart
                                let projection = Vector2.Dot (toParticleStart, edgeSegment) / edgeLengthSquared
                                let closestOnEdge = edgeStart + Math.Clamp (projection, 0f, 1f) * edgeSegment

                                // check if particle path comes close to the edge
                                let approachVector = closestOnEdge - particle.Position
                                let distanceSquared = approachVector.LengthSquared ()
                                let collisionRadius = particleRadius

                                if distanceSquared <= collisionRadius * collisionRadius then
                                    isColliding <- true
                                    closestPoint <- closestOnEdge
                                    normal <- Vector2.Normalize (Vector2 (-edgeSegment.Y, edgeSegment.X)) // use perpendicular to edge for normal in collinear case

                    | shape -> Log.warnOnce $"Shape not implemented: {shape}"

                    if isColliding then
                        particle.Position <- closestPoint + 0.05f * normal
                        particle.Velocity <- (particle.Velocity - 1.2f * Vector2.Dot (particle.Velocity, normal) * normal) * 0.85f
                        particle.Delta <- v2Zero)
                        
            // assert loop completion
            assert loopResult.IsCompleted

            // relocate particles
            let bounds = (fluidSystem.GetBounds world).Box2
            let newParticles = List activeParticleCount
            for i in 0 .. dec activeParticleCount do
                let particle = &particleStates.[i]
                let newVelocity = particle.Velocity + particle.Delta
                let newPosition = particle.Position + newVelocity + particle.Delta
                let newVelocity = newVelocity * Constants.Engine.Meter2d
                let newPosition = newPosition * Constants.Engine.Meter2d
                if bounds.Contains newPosition <> ContainmentType.Disjoint
                then newParticles.Add { Position = newPosition; Velocity = newVelocity }
                else activeParticleCount <- dec activeParticleCount
                ArrayPool.Shared.Return particle.PotentialFixtureChildIndexes
                ArrayPool.Shared.Return particle.PotentialFixtures
                ArrayPool.Shared.Return particle.Neighbors
            ArrayPool.Shared.Return particleStates

            // update state
            fluidSystem.SetFluidParticles (FStack.ofSeq newParticles) world
            fluidSystem.SetFluidParticleCount activeParticleCount world

    override _.Render (_, fluidSystem, world) =

        // collect sim properties
        let particleRadius = fluidSystem.GetFluidParticleRadius world
        let cellSize = particleRadius * fluidSystem.GetFluidParticleCellScale world
        let drawCells = fluidSystem.GetFluidParticleCellColor world
        let grid = Collections.Generic.HashSet ()
        let staticImage = fluidSystem.GetStaticImage world
        let insetOpt = match fluidSystem.GetInsetOpt world with Some inset -> ValueSome inset | None -> ValueNone
        let clipOpt = fluidSystem.GetClipOpt world |> Option.toValueOption
        let color = fluidSystem.GetColor world
        let blend = fluidSystem.GetBlend world
        let emission = fluidSystem.GetEmission world
        let flip = fluidSystem.GetFlip world
        let drawnSize = fluidSystem.GetFluidParticleImageSizeOverride world |> Option.defaultValue (v2Dup particleRadius)

        // render particles
        let mutable transform = Transform.makeIntuitive false v3Zero v3One v3Zero drawnSize.V3 v3Zero (fluidSystem.GetElevation world)
        for p in fluidSystem.GetFluidParticles world do
            transform.Position <- p.Position.V3
            World.renderLayeredSpriteFast (transform.Elevation, transform.Horizon, staticImage, &transform, &insetOpt, &clipOpt, staticImage, &color, blend, &emission, flip, world)
            if drawCells.IsSome then grid.Add (positionToCell cellSize p.Position) |> ignore

        // render cells when desired
        match drawCells with
        | Some color ->
            transform.Elevation <- transform.Elevation - 1f
            transform.Size <- v3Dup cellSize
            let staticImage = Assets.Default.White
            for g in grid do
                let box = cellToBox cellSize g
                transform.Position <- box.Center.V3
                World.renderLayeredSpriteFast (transform.Elevation, transform.Horizon, staticImage, &transform, &insetOpt, &clipOpt, staticImage, &color, blend, &emission, flip, world)
        | None -> ()

/// this extends the Entity API to expose the user-defined properties.
[<AutoOpen>]
module LineSegmentsExtensions =
    type Entity with

        /// The line segments that define the fluid boundaries.
        member this.GetLineSegments world : Vector2 array = this.Get (nameof Entity.LineSegments) world
        member this.SetLineSegments (value : Vector2 array) world = this.Set (nameof Entity.LineSegments) value world
        member this.LineSegments = lens (nameof Entity.LineSegments) this this.GetLineSegments this.SetLineSegments

        /// The width of the line segments when rendered.
        member this.GetLineWidth world : single = this.Get (nameof Entity.LineWidth) world
        member this.SetLineWidth (value : single) world = this.Set (nameof Entity.LineWidth) value world
        member this.LineWidth = lens (nameof Entity.LineWidth) this this.GetLineWidth this.SetLineWidth

/// this is the dispatcher that defines the behavior of the line segments used to contain the fluid.
type LineSegmentsDispatcher () =
    inherit Entity2dDispatcher (true, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>]

    static member Properties =
        [define Entity.LineSegments Array.empty
         define Entity.LineWidth 2f
         define Entity.Color colorOne]

    override _.Update (lineSegments, world) =
        let segments = lineSegments.GetLineSegments world
        if Array.notEmpty segments && lineSegments.GetEnabled world then
            let box = Box2.Enclose segments
            let lineWidth = lineSegments.GetLineWidth world
            let size = v2 (box.Width + lineWidth) (box.Height + lineWidth)
            lineSegments.SetPosition box.Center.V3 world
            lineSegments.SetSize size.V3 world
            lineSegments.SetBodyShape (
                ContourShape
                    { Links = segments |> Array.map (fun p -> ((p - box.Center) / size).V3)
                      Closed = false
                      TransformOpt = None
                      PropertiesOpt = None }) world

    override _.Render (_, lineSegments, world) =
        let staticImage = Assets.Default.White
        let insetOpt : Box2 voption = ValueNone
        let clipOpt : Box2 voption = ValueNone
        let color = lineSegments.GetColor world
        let blend = Additive
        let emission = colorZero
        let flip = FlipNone
        let segments = lineSegments.GetLineSegments world
        let lineWidth = lineSegments.GetLineWidth world
        let mutable transform = Transform.makeIntuitive false v3Zero v3One v3Zero v3Zero v3Zero (lineSegments.GetElevation world)
        for s in 0 .. segments.Length - 2 do
            let p1 = segments.[s]
            let p2 = segments.[inc s]
            transform.Position <- ((p1 + p2) * 0.5f).V3
            transform.Rotation <- Quaternion.CreateLookAt2d (p2 - p1)
            transform.Size <- v3 (p2 - p1).Magnitude lineWidth 0f
            World.renderLayeredSpriteFast (transform.Elevation, transform.Horizon, staticImage, &transform, &insetOpt, &clipOpt, staticImage, &color, blend, &emission, flip, world)

// this extends the Screen API to expose the user-defined properties.
[<AutoOpen>]
module FluidSimExtensions =
    type Screen with

        /// The line segments drawn by the user to contain the fluid.
        member this.GetLineSegments world : Vector2 array list = this.Get (nameof Screen.LineSegments) world
        member this.SetLineSegments (value : Vector2 array list) world = this.Set (nameof Screen.LineSegments) value world
        member this.LineSegments = lens (nameof Screen.LineSegments) this this.GetLineSegments this.SetLineSegments

        /// The hold duration before new line segments are created while drawing.
        member this.GetHoldDuration world : single = this.Get (nameof Screen.HoldDuration) world
        member this.SetHoldDuration (value : single) world = this.Set (nameof Screen.HoldDuration) value world
        member this.HoldDuration = lens (nameof Screen.HoldDuration) this this.GetHoldDuration this.SetHoldDuration

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type FluidSimDispatcher () =
    inherit ScreenDispatcherImSim ()

    // here we define default property values
    static member Properties =
        [define Screen.InfoOpened false
         define Screen.LineSegments []
         define Screen.HoldDuration 0f]

    // here we define the screen's top-level behavior
    override _.Process (selectionResults, fluidSim, world) =

        // process while selected
        if fluidSim.GetSelected world then

            // clean up lines and gravity when initializing
            if FQueue.contains Select selectionResults then
                fluidSim.SetInfoOpened false world
                fluidSim.SetLineSegments [] world
                fluidSim.SetHoldDuration 0f world
                World.setGravity2d (World.getGravityDefault2d world) world

            // begin scene declaration
            World.beginGroup Simulants.FluidSimScene.Name [] world

            // create test geometry
            let scale = 25f
            World.doBlock2d "Bottom"
                [Entity.Size .= v3 12f 1f 0f * scale * 2f 
                 Entity.Position .= v3 0f -7f 0f * scale
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= Color.DeepSkyBlue] world |> ignore
            World.doBlock2d "Left"
                [Entity.Size .= v3 1f 10f 0f * scale * 2f
                 Entity.Position .= v3 -11f 0f 0f * scale
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= Color.DeepSkyBlue] world |> ignore
            World.doBlock2d "Right"
                [Entity.Size .= v3 1f 10f 0f * scale * 2f
                 Entity.Position .= v3 11f 0f 0f * scale
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= Color.DeepSkyBlue] world |> ignore
            World.doBlock2d "Ramp"
                [Entity.Size .= v3 5.5f 0.5f 0f * scale * 2f
                 Entity.Position .= v3 6f 2f 0f * scale
                 Entity.Rotation .= Quaternion.CreateFromAngle2d 0.25f
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= Color.DeepSkyBlue] world |> ignore
            World.doSphere2d "Circle"
                [Entity.Size .= v3 2f 2f 0f * scale * 2f
                 Entity.Position .= v3 0f 0.2f 0f * scale
                 Entity.Color .= Color.DeepSkyBlue] world |> ignore

            // define fluid system
            World.doEntity<FluidSystemDispatcher> "Fluid System"
                [Entity.Size .= v3 640f 640f 0f
                 // individual sprites on the same elevation are ordered from top to bottom then by asset tag.
                 // here, we don't draw particles above the borders, especially relevant for the water sprite.
                 Entity.Elevation .= -1f] world
            let fluidSystem = world.DeclaredEntity

            // define menu position helper
            let menuPosition =
                let mutable y = 200f
                fun () ->
                    y <- y - 30f
                    Entity.Position .= v3 255f y 0f

            // particle count button
            World.doText $"Particle Count"
                [menuPosition ()
                 Entity.Text @= $"{fluidSystem.GetFluidParticleCount world} Particles"
                 Entity.Elevation .= 1f] world

            // clear button
            if World.doButton $"Clear"
                [menuPosition ()
                 Entity.Text .= "Clear"
                 Entity.Elevation .= 1f] world then
                fluidSystem.SetFluidParticles FStack.empty world
                fluidSim.SetLineSegments [] world

            // gravity button
            let gravities =
                [|("v", World.getGravityDefault2d world)
                  ("\\", (World.getGravityDefault2d world).Transform (Quaternion.CreateFromAngle2d MathF.PI_OVER_4))
                  (">", (World.getGravityDefault2d world).Transform (Quaternion.CreateFromAngle2d MathF.PI_OVER_2))
                  ("0", v3Zero)
                  ("<", (World.getGravityDefault2d world).Transform (Quaternion.CreateFromAngle2d -MathF.PI_OVER_2))
                  ("/", (World.getGravityDefault2d world).Transform (Quaternion.CreateFromAngle2d -MathF.PI_OVER_4))|]
            for i in 0 .. dec gravities.Length do
                if World.getGravity2d world = snd gravities.[i] then
                    if World.doButton $"Gravity"
                        [menuPosition ()
                         Entity.Text @= $"Gravity: {fst gravities.[i]}"
                         Entity.Elevation .= 1f] world then
                        World.setGravity2d (snd gravities.[(i + 1) % gravities.Length]) world

            // particle sprite button
            if World.doButton $"Particle Sprite"
                [menuPosition ()
                 Entity.Text @= $"Particle Sprite: {(fluidSystem.GetStaticImage world).AssetName}"
                 Entity.Elevation .= 1f
                 Entity.FontSizing .= Some 8] world then
                if fluidSystem.GetStaticImage world = Assets.Default.Ball then
                    // in Paint.NET (canvas size = 50 x 50), use the Brush (size = 50, hardness = 50%, fill = solid color #0094FF)
                    // and click the center once, to generate this Particle image.
                    fluidSystem.SetStaticImage Assets.Gameplay.Fluid world
                    fluidSystem.SetFluidParticleImageSizeOverride None world
                elif fluidSystem.GetStaticImage world = Assets.Gameplay.Fluid then
                    // credit: https://ena.our-dogs.info/spring-2023.html
                    fluidSystem.SetStaticImage Assets.Gameplay.Bubble world
                    fluidSystem.SetFluidParticleImageSizeOverride None world
                elif fluidSystem.GetStaticImage world = Assets.Gameplay.Bubble then
                    // credit: Aether.Physics2D demos
                    fluidSystem.SetStaticImage Assets.Gameplay.Goo world
                    fluidSystem.SetFluidParticleImageSizeOverride (v2Dup 8f |> Some) world
                else
                    fluidSystem.SetStaticImage Assets.Default.Ball world
                    fluidSystem.SetFluidParticleImageSizeOverride (v2Dup 2f |> Some) world

            // viscosity button
            if World.doButton $"Viscosity"
                [menuPosition ()
                 Entity.Text @= $"Viscosity: {fluidSystem.GetFluidViscosity world}"
                 Entity.Elevation .= 1f
                 Entity.FontSizing .= Some 12] world then
                fluidSystem.FluidViscosity.Map (function
                    | 0.004f -> 0.01f
                    | 0.01f -> 0.1f
                    | 0.1f -> 1f
                    | 1f -> 2f
                    | 2f -> 5f
                    | 5f -> 20f
                    | _ -> 0.004f) world

            // linear damping button
            if World.doButton $"Linear Damping"
                [menuPosition ()
                 Entity.Text @= $"Linear Damping: {fluidSystem.GetLinearDamping world}"
                 Entity.Elevation .= 1f
                 Entity.FontSizing .= Some 11] world then
                fluidSystem.LinearDamping.Map (function
                    | 0f -> 0.2f
                    | 0.2f -> 0.5f
                    | 0.5f -> 0.7f
                    | 0.7f -> 0.9f
                    | 0.9f -> 0.99f
                    | _ -> 0f) world

            // particle radius button
            if World.doButton $"Particle Radius"
                [menuPosition ()
                 Entity.Text @= $"Particle Radius: {fluidSystem.GetFluidParticleRadius world}"
                 Entity.Elevation .= 1f
                 Entity.FontSizing .= Some 9] world then
                fluidSystem.FluidParticleRadius.Map (flip (/) Constants.Engine.Meter2d >> function
                    | 0.9f -> 0.7f
                    | 0.7f -> 0.5f
                    | 0.5f -> 0.3f
                    | 0.3f -> 0.1f
                    | 0.1f -> 2f
                    | 2f -> 1.5f
                    | _ -> 0.9f
                    >> (*) Constants.Engine.Meter2d) world

            // cell scale button
            if World.doButton $"Cell Scale"
                [menuPosition ()
                 Entity.Text @= $"""Cell Scale: {fluidSystem.GetFluidParticleCellScale world |> function 0.66666666f -> "2/3" | n -> string n}"""
                 Entity.Elevation .= 1f
                 Entity.FontSizing .= Some 12] world then
                fluidSystem.FluidParticleCellScale.Map (
                    function
                    | 0.66666666f -> 1f
                    | 1f -> 2f
                    | 2f -> 0.2f
                    | 0.2f -> 0.4f
                    | _ -> 0.66666666f
                    ) world

            // draw cells button
            if World.doButton $"Draw Cells"
                [menuPosition ()
                 Entity.Text @= $"Draw Cells: {fluidSystem.GetFluidParticleCellColor world |> Option.isSome}"
                 Entity.Elevation .= 1f
                 Entity.FontSizing .= Some 12] world then
                fluidSystem.FluidParticleCellColor.Map (function Some _ -> None | None -> Some Color.LightBlue) world

            // squish button
            if World.doButton $"Squish"
                [menuPosition ()
                 Entity.Text .= "Squish"
                 Entity.Elevation .= 1f] world then
                let paddle = World.createEntity<Block2dDispatcher> None DefaultOverlay None world.ContextGroup world
                paddle.SetPosition (v3 -270f 0f 0f) world
                paddle.SetSize (v3 30f 500f 0f) world
                paddle.SetStaticImage Assets.Default.Paddle world
                paddle.SetBodyType Kinematic world
                paddle.SetLinearVelocity (v3 50f 0f 0f) world
                coroutine world.Launcher {
                    do! Coroutine.sleep (GameTime.ofSeconds 10f)
                    World.destroyEntity paddle world }

            // switch screen button
            World.doButton Simulants.ToyBoxSwitchScreen.Name
                [Entity.Position .= v3 255f -130f 0f
                 Entity.Text .= "Switch Screen"
                 Entity.Elevation .= 1f] world |> ignore

            // info button
            if World.doButton "Info"
                [Entity.Position .= v3 255f -160f 0f
                 Entity.Text .= "Info"
                 Entity.Elevation .= 1f] world then
                fluidSim.SetInfoOpened true world

            // info panel
            if fluidSim.GetInfoOpened world then

                // declare info background - block button interactions behind info panel while opened
                World.doPanel "Info Background"
                    [Entity.Size .= Constants.Render.DisplayVirtualResolution.V3
                     Entity.Elevation .= 10f
                     Entity.BackdropImageOpt .= Some Assets.Default.Black
                     Entity.Color .= color 0f 0f 0f 0.5f] world

                // being info panel declaration
                World.beginPanel "Info Panel"
                    [Entity.Size .= Constants.Render.DisplayVirtualResolution.V3 * 0.8f
                     Entity.Layout .= Grid (v2i 1 5, Some FlowDownward, true)
                     Entity.Elevation .= 10f] world

                // declare info entities
                World.doText "Info Origin 1"
                    [Entity.LayoutOrder .= 0
                     Entity.Text .= "Box2DFluid by klutch (Graeme Collins)"] world
                World.doText "Info Origin 2"
                    [Entity.LayoutOrder .= 1
                     Entity.Text .= "Ported to Nu by Happypig375 (Hadrian Tang)"] world
                World.doText "Info Controls"
                    [Entity.LayoutOrder .= 2
                     Entity.Justification .= Unjustified true
                     Entity.Text .=
                     "Controls: Mouse Left - Click button or Add particles. Mouse right - Delete particles.\n\
                        Mouse Left and Right - Summon a giant bubble that collides with particles.\n\
                        Mouse Middle - Draw contours that collide with particles. \n\
                        NOTE: Intersecting contours are not supported and will cause tunneling!"
                     Entity.FontSizing .= Some 10
                     Entity.TextMargin .= v2 5f 0f] world
                if World.doButton "Info Close"
                    [Entity.LayoutOrder .= 3
                     Entity.Text .= "Close"] world then
                    fluidSim.SetInfoOpened false world
                if World.doButton "Info Exit"
                    [Entity.LayoutOrder .= 4
                     Entity.Text .= "Exit"] world && world.Unaccompanied then
                    World.exit world

                // end info panel declaration
                World.endPanel world

                // declare info links
                for (position, size, url) in
                    [(v2 -115f 115f, v2 95f 32f, "https://github.com/klutch/Box2DFluid")
                     (v2 -12.5f 115f, v2 60f 32f, "https://github.com/klutch")
                     (v2 -127.5f 57.5f, v2 115f 32f, "https://github.com/bryanedds/Nu/pull/1162")
                     (v2 3.5f 57.5f, v2 105f 32f, "https://github.com/Happypig375")] do
                    if World.doButton $"Info Origin Button {url.Replace ('/', '\\')}"
                        [Entity.Position .= position.V3
                         Entity.Size .= size.V3
                         Entity.Elevation .= 11f] world then
                        System.Diagnostics.Process.Start (System.Diagnostics.ProcessStartInfo (url, UseShellExecute = true)) |> ignore

            // mouse interactions with fluid system
            if fluidSim.GetSelected world && world.Advancing then
                let mousePosition = World.getMousePosition2dWorld false world
                match (World.isMouseButtonDown MouseLeft world, World.isMouseButtonDown MouseRight world) with
                | (true, false) ->

                    // mouse left - create particles
                    let createParticle particles =
                        let jitter = v2 (Gen.randomf * 2f - 1f) (Gen.randomf - 0.5f) * Constants.Engine.Meter2d
                        FStack.conj { Position = mousePosition + jitter; Velocity = v2Zero } particles
                    fluidSystem.FluidParticles.Map (createParticle >> createParticle >> createParticle >> createParticle) world

                | (false, true) ->

                    // mouse right - delete particles
                    let filterParticle (particle : FluidParticle) =
                        let bounds = box2 (mousePosition - v2Dup (Constants.Engine.Meter2d * 0.5f)) (v2Dup Constants.Engine.Meter2d)
                        bounds.Contains particle.Position = ContainmentType.Disjoint
                    fluidSystem.FluidParticles.Map (FStack.filter filterParticle) world
                
                | (true, true) ->

                    // mouse both - summon a bubble
                    fluidSim.HoldDuration.Map inc world
                    World.doSphere2d "Bubble"
                        [Entity.Position @= mousePosition.V3
                         Entity.Size @= v3Dup (fluidSim.GetHoldDuration world)
                         Entity.StaticImage .= Assets.Gameplay.Bubble] world |> ignore
                
                | (false, false) ->

                    // only reset size when both mouse buttons up
                    fluidSim.SetHoldDuration 0f world
                
                // mouse middle - draw a contour
                if World.isMouseButtonPressed MouseMiddle world then
                    fluidSim.LineSegments.Map (fun lineSegments ->
                        List.cons [|mousePosition|] lineSegments) world
                elif World.isMouseButtonDown MouseMiddle world then
                    fluidSim.LineSegments.Map (fun lineSegments ->
                        let active = lineSegments.[0]
                        if Vector2.Distance (mousePosition, Array.last active) > 8f then
                            List.updateAt 0 (Array.add mousePosition active) lineSegments
                        else lineSegments) world

            // declare containment contour
            for segment in fluidSim.GetLineSegments world do
                World.doEntity<LineSegmentsDispatcher> $"Contour {segment.[0]}" [Entity.LineSegments @= segment] world

            // end scene declaration
            World.endGroup world

            // process camera as last task
            World.setEye2dCenter (v2 60f 10f) world