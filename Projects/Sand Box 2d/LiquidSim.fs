namespace SandBox2d
open System
open System.Numerics
open Prime
open Nu
open nkast.Aether.Physics2D.Collision.Shapes
open nkast.Aether.Physics2D.Dynamics

type [<Struct>] Particle =
    { Position : Vector2
      Velocity : Vector2 }

type [<Struct>] ParticleState =
    { // assigned during initialize particles
      mutable Position : Vector2 // updated during resolve collisions - parallel for 1 input, parallel for 2 in/output
      mutable Velocity : Vector2 // updated during calculate interaction forces, resolve collisions - parallel for 1 in/output, parallel for 2 in/output
      mutable ScaledParticle : Particle // parallel for 1 input
      // assigned during initialize grid
      mutable Cell : Vector2i // parallel for 1 input
      // assigned during prepare simulation
      mutable Delta : Vector2 // updated during calculate interaction forces, accumulate deltas - parallel for 1 output, parallel for 2 in/output
      mutable PotentialFixtureCount : int // updated during prepare collisions - parallel for 2 input
      mutable PotentialFixtures : Fixture array // updated during prepare collisions - parallel for 2 input
      mutable PotentialFixtureChildIndexes : int array // updated during prepare collisions - parallel for 2 input
      // assigned during find neighbors
      mutable NeighborCount : int // parallel for 1 output
      mutable Neighbors : ParticleNeighbor array // parallel for 1 output
      }
and [<Struct>] ParticleNeighbor =
    { // assigned during find neighbors
      mutable ParticleIndex : int // parallel for 1 output
      // assigned during calculate pressures
      mutable Distance : single
      // assigned during calculate interaction forces
      mutable AccumulatedDelta : Vector2 // parallel for 1 output
      }

// this extends the Entity API to expose the user-defined properties.
[<AutoOpen>]
module FluidSystemExtensions =
    type Entity with
        member this.Particles = lens (nameof Entity.Particles) this this.GetParticles this.SetParticles
        member this.GetParticles world : FList<Particle> = this.Get (nameof Entity.Particles) world
        member this.SetParticles (value : FList<Particle>) world = this.Set (nameof Entity.Particles) value world
        member this.ParticleSimulatedCount = lensReadOnly (nameof Entity.ParticleSimulatedCount) this this.GetParticleSimulatedCount
        member this.GetParticleSimulatedCount world : int = this.Get (nameof Entity.ParticleSimulatedCount) world
        /// The maximum number of fluid particles allowed in the simulation at any time.
        member this.ParticleSimulatedCountMax = lens (nameof Entity.ParticleSimulatedCountMax) this this.GetParticleSimulatedCountMax this.SetParticleSimulatedCountMax
        member this.GetParticleSimulatedCountMax world : int = this.Get (nameof Entity.ParticleSimulatedCountMax) world
        member this.SetParticleSimulatedCountMax (value : int) world = this.Set (nameof Entity.ParticleSimulatedCountMax) value world
        /// The maximum number of neighboring particles considered for each particle during force and pressure calculations.
        member this.ParticleNeighborCountMax = lens (nameof Entity.ParticleNeighborCountMax) this this.GetParticleNeighborCountMax this.SetParticleNeighborCountMax
        member this.GetParticleNeighborCountMax world : int = this.Get (nameof Entity.ParticleNeighborCountMax) world
        member this.SetParticleNeighborCountMax (value : int) world = this.Set (nameof Entity.ParticleNeighborCountMax) value world
        /// The base radius of each fluid particle, used for collision and interaction calculations.
        member this.ParticleRadius = lens (nameof Entity.ParticleRadius) this this.GetParticleRadius this.SetParticleRadius
        member this.GetParticleRadius world : single = this.Get (nameof Entity.ParticleRadius) world
        member this.SetParticleRadius (value : single) world = this.Set (nameof Entity.ParticleRadius) value world
        /// The ideal interaction radius for particles, as a multiple of Entity.ParticleRadius. Particles within this distance are considered neighbors and interact.
        member this.ParticleInteractionScale = lens (nameof Entity.ParticleInteractionScale) this this.GetParticleInteractionScale this.SetParticleInteractionScale
        member this.GetParticleInteractionScale world : single = this.Get (nameof Entity.ParticleInteractionScale) world
        member this.SetParticleInteractionScale (value : single) world = this.Set (nameof Entity.ParticleInteractionScale) value world
        /// The width and height of each grid cell used for spatial partitioning, as a multiple of Entity.ParticleRadius.
        member this.ParticleCellScale = lens (nameof Entity.ParticleCellScale) this this.GetParticleCellScale this.SetParticleCellScale
        member this.GetParticleCellScale world : single = this.Get (nameof Entity.ParticleCellScale) world
        member this.SetParticleCellScale (value : single) world = this.Set (nameof Entity.ParticleCellScale) value world
        /// When set to a color, the simulation will render the spatial grid cells for debugging or visualization.
        member this.ParticleCellColor = lens (nameof Entity.ParticleCellColor) this this.GetParticleCellColor this.SetParticleCellColor
        member this.GetParticleCellColor world : Color option = this.Get (nameof Entity.ParticleCellColor) world
        member this.SetParticleCellColor (value : Color option) world = this.Set (nameof Entity.ParticleCellColor) value world
        member this.ParticleTestedBodiesCountMax = lens (nameof Entity.ParticleTestedBodiesCountMax) this this.GetParticleTestedCollisionBodiesCountMax this.SetParticleTestedBodiesCountMax
        member this.GetParticleTestedCollisionBodiesCountMax world : int = this.Get (nameof Entity.ParticleTestedBodiesCountMax) world
        member this.SetParticleTestedBodiesCountMax (value : int) world = this.Set (nameof Entity.ParticleTestedBodiesCountMax) value world
        /// The size of the particle image - when None, uses Entity.ParticleRadius.
        member this.ParticleImageSizeOpt = lens (nameof Entity.ParticleImageSizeOpt) this this.GetParticleImageSizeOpt this.SetParticleImageSizeOpt
        member this.GetParticleImageSizeOpt world : Vector2 option = this.Get (nameof Entity.ParticleImageSizeOpt) world
        member this.SetParticleImageSizeOpt (value : Vector2 option) world = this.Set (nameof Entity.ParticleImageSizeOpt) value world
        /// The viscosity coefficient for relative velocity.
        member this.PhysicalViscosity = lens (nameof Entity.PhysicalViscosity) this this.GetPhysicalViscosity this.SetPhysicalViscosity
        member this.GetPhysicalViscosity world : single = this.Get (nameof Entity.PhysicalViscosity) world
        member this.SetPhysicalViscosity (value : single) world = this.Set (nameof Entity.PhysicalViscosity) value world
        (*
        Physical Viscosity (or dynamic viscosity): This is the viscosity that is calculated from the relative velocity and
        is part of the force calculation. It is based on the physical law of viscosity (Newton's law of viscosity).
        In SPH, this is often modeled by a term that is proportional to the Laplacian of the velocity field.
        This term models shear stress - the resistance to flow when fluid layers slide past each other.
        It smooths out velocity gradients and creates the familiar "sticky" behavior.
        
        Artificial Viscosity (or numerical viscosity): This is a numerical trick used to stabilize simulations or to achieve
        certain visual effects (like slime) without necessarily modeling the physical viscosity. It can be introduced in various ways,
        such as in the pressure projection step (as in PBD) or by adding a damping term.
        In many simulations, physical viscosity alone isn't enough to create the desired "slime" behavior,
        especially in particle-based methods where:
        - Stability Issues: High physical viscosity can make simulations numerically unstable
        - Different Effect: Physical viscosity mainly affects shear flow, while artificial viscosity affects volume conservation
        - Performance: The pressure-based approach is often more stable and controllable for artistic effects
        *)
        /// The viscosity coefficient for pressure projection.
        member this.ArtificialViscosity = lens (nameof Entity.ArtificialViscosity) this this.GetArtificialViscosity this.SetArtificialViscosity
        member this.GetArtificialViscosity world : single = this.Get (nameof Entity.ArtificialViscosity) world
        member this.SetArtificialViscosity (value : single) world = this.Set (nameof Entity.ArtificialViscosity) value world

type FluidSystemDispatcher () =
    inherit Entity2dDispatcher (true, false, false)

    // each particle is associated with a cell in a spatial grid for neighbor searching
    static let neighborhood = [|for x in -1 .. 1 do for y in -1 .. 1 do v2i x y|]
    static let positionToCell cellSize (position : Vector2) =
        v2i (floor (position.X / cellSize) |> int) (floor (position.Y / cellSize) |> int)
    static let cellToBox cellSize (cell : Vector2i) = box2 (cell.V2 * cellSize) (v2Dup cellSize)

    // here we define default property values
    static member Properties =
        [define Entity.Particles FList.empty
         define Entity.ParticleSimulatedCount 0
         define Entity.ParticleSimulatedCountMax 20000
         define Entity.ParticleCellColor None
         define Entity.ParticleNeighborCountMax 75
         define Entity.ParticleRadius (0.9f * Constants.Engine.Meter2d)
         define Entity.ParticleCellScale (0.6f / 0.9f)
         define Entity.ParticleInteractionScale (50f / 0.9f)
         define Entity.ParticleTestedBodiesCountMax 20
         define Entity.ParticleImageSizeOpt (Some (v2Dup 2f))
         define Entity.PhysicalViscosity 0.004f
         define Entity.ArtificialViscosity 0f
         define Entity.GravityOverride None
         // Static sprite properties
         define Entity.InsetOpt None
         define Entity.ClipOpt None
         define Entity.StaticImage Assets.Default.Ball
         define Entity.Color Color.One
         define Entity.Blend Transparent
         define Entity.Emission Color.Zero
         define Entity.Flip FlipNone]

    // here we define the entity's top-level behavior
    override _.Update (fluidSystem, world) =
        let sourceParticles = fluidSystem.GetParticles world
        if FList.isEmpty sourceParticles then () else
        let maxParticles = fluidSystem.GetParticleSimulatedCountMax world
        let maxNeighbors = fluidSystem.GetParticleNeighborCountMax world
        let particleRadius = fluidSystem.GetParticleRadius world / Constants.Engine.Meter2d
        let interactionScale = fluidSystem.GetParticleInteractionScale world
        let idealRadius = particleRadius * interactionScale
        let idealRadiusSquared = idealRadius * idealRadius
        let cellSize = particleRadius * fluidSystem.GetParticleCellScale world
        let maxFixtures = fluidSystem.GetParticleTestedCollisionBodiesCountMax world
        let physicalViscosity = fluidSystem.GetPhysicalViscosity world
        let deltaFactor = 1f - fluidSystem.GetArtificialViscosity world

        let deltaTime = world.ClockDelta
        let gravity =
            (fluidSystem.GetGravityOverride world |> Option.defaultValue (World.getGravity2d world))
                .V2 / Constants.Engine.Meter2d / 3000f

        let particles = Buffers.ArrayPool<ParticleState>.Shared.Rent maxParticles
        let mutable activeParticles = 0
        let grid = Collections.Generic.Dictionary ()
        for particle in sourceParticles |> Seq.truncate maxParticles do
            // initialize particles - all internal calculations use physics engine units.
            particles[activeParticles].Position <- particle.Position / Constants.Engine.Meter2d
            particles[activeParticles].Velocity <- particle.Velocity / Constants.Engine.Meter2d
            particles[activeParticles].ScaledParticle <-
                { Position = particles[activeParticles].Position * interactionScale
                  Velocity = particles[activeParticles].Velocity * interactionScale }

            // initialize grid
            let cell = positionToCell cellSize particles[activeParticles].Position
            particles[activeParticles].Cell <- cell
            match grid.TryGetValue cell with
            | (true, list) -> grid[cell] <- activeParticles :: list
            | (false, _) -> grid[cell] <- [activeParticles]
            activeParticles <- inc activeParticles

        // parallel for 1
        Threading.Tasks.Parallel.For(0, activeParticles, fun i ->
            // prepare simulation
            let particle = &particles[i]
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
                    particle.Neighbors[particle.NeighborCount].ParticleIndex <- neighbor
                    particle.NeighborCount <- inc particle.NeighborCount

            // calculate pressures
            let mutable p = 0f
            let mutable pnear = 0f
            for n in 0 .. dec particle.NeighborCount do
                let neighbor = &particle.Neighbors[n]
                let relativePosition = particles[neighbor.ParticleIndex].ScaledParticle.Position - particle.ScaledParticle.Position
                let distanceSquared = relativePosition.MagnitudeSquared
                if distanceSquared < idealRadiusSquared then
                    neighbor.Distance <- sqrt distanceSquared
                    let oneMinusQ = 1f - (neighbor.Distance / idealRadius)
                    p <- p + oneMinusQ * oneMinusQ
                    pnear <- pnear + oneMinusQ * oneMinusQ * oneMinusQ
                else neighbor.Distance <- nanf
            let pressure = (p - 5f) / 2f // normal pressure term
            let presnear = pnear / 2f // near particles term

            // calculate interaction forces
            for n in 0 .. dec particle.NeighborCount do
                let neighbor = &particle.Neighbors[n]
                if not (Single.IsNaN neighbor.Distance) then
                    let q = neighbor.Distance / idealRadius
                    let oneMinusQ = 1f - q
                    let factor = oneMinusQ * (pressure + presnear * oneMinusQ) / (2f * neighbor.Distance)
                    let relativePosition = particles[neighbor.ParticleIndex].ScaledParticle.Position - particle.ScaledParticle.Position
                    let d = relativePosition * factor
                    let relativeVelocity = particles[neighbor.ParticleIndex].ScaledParticle.Velocity - particle.ScaledParticle.Velocity
                    let factor = physicalViscosity * oneMinusQ * deltaTime
                    let d = d - relativeVelocity * factor
                    neighbor.AccumulatedDelta <- d
                    particle.Delta <- particle.Delta - d
                else neighbor.AccumulatedDelta <- v2Zero
            particle.Velocity <- particle.Velocity + gravity
        ) |> fun result -> assert result.IsCompleted

        // accumulate deltas
        for i in 0 .. dec activeParticles do
            let particle = &particles[i]
            for n in 0 .. dec particle.NeighborCount do
                let neighbor = &particle.Neighbors[n]
                particles[neighbor.ParticleIndex].Delta <- particles[neighbor.ParticleIndex].Delta + neighbor.AccumulatedDelta
        for i in 0 .. dec activeParticles do
            particles[i].Delta <- particles[i].Delta / interactionScale * deltaFactor
            
        // prepare collisions
        World.queryBodies2d (fluidSystem.GetBounds world) (fun fixture ->
            let mutable aabb = Unchecked.defaultof<_>
            let mutable transform = Unchecked.defaultof<_>
            fixture.Body.GetTransform &transform
            for c in 0 .. dec fixture.Shape.ChildCount do // chain shapes have edges as children, other shapes only have 1 child
                fixture.Shape.ComputeAABB (&aabb, &transform, c)
                let lowerBound = positionToCell cellSize (v2 aabb.LowerBound.X aabb.LowerBound.Y)
                let upperBound = positionToCell cellSize (v2 aabb.UpperBound.X aabb.UpperBound.Y)
                for gridX in dec lowerBound.X .. inc upperBound.X do // expand grid by one in case some fixtures perfectly align on cell boundary
                    for gridY in dec lowerBound.Y .. inc upperBound.Y do
                        match grid.TryGetValue (v2i gridX gridY) with
                        | (true, particleIndexes) ->
                            for i in particleIndexes do
                                let particle = &particles[i]
                                if particle.PotentialFixtureCount < maxFixtures then
                                    particle.PotentialFixtures[particle.PotentialFixtureCount] <- fixture
                                    particle.PotentialFixtureChildIndexes[particle.PotentialFixtureCount] <- c
                                    particle.PotentialFixtureCount <- inc particle.PotentialFixtureCount
                        | (false, _) -> ()
            true) world

        // parallel for 2 - resolve collisions
        Threading.Tasks.Parallel.For(0, activeParticles, fun i ->
            let convertVector (v : nkast.Aether.Physics2D.Common.Vector2) = Vector2 (v.X, v.Y)
            let particle = &particles[i]
            for f in 0 .. dec particle.PotentialFixtureCount do
                let fixture = particle.PotentialFixtures[f]
                let newPosition = particle.Position + particle.Velocity + particle.Delta

                let mutable isColliding = false
                let mutable closestPoint = v2Zero
                let mutable normal = v2Zero
                
                let (|EdgeFromEdgeShape|) (shape : EdgeShape) = (shape.Vertex1, shape.Vertex2)
                let (|EdgeFromChainShape|) (lookup : _ array) index (shape : ChainShape) = (shape.Vertices[lookup.[index]], shape.Vertices[inc lookup.[index]])
                match fixture.Shape with
                | :? PolygonShape as shape ->
                    let mutable newPosition = nkast.Aether.Physics2D.Common.Vector2 (newPosition.X, newPosition.Y)
                    if fixture.TestPoint &newPosition then
                        isColliding <- true

                        let mutable collisionXF = Unchecked.defaultof<_>
                        fixture.Body.GetTransform &collisionXF
                        let mutable shortestDistance = infinityf // Find closest edge
                        for v in 0 .. dec shape.Vertices.Count do
                            // Transform the shape's vertices from local space to world space
                            let collisionVertex = nkast.Aether.Physics2D.Common.Transform.Multiply (shape.Vertices[v], &collisionXF) |> convertVector
                            // Transform the shape's normals using the rotation (Complex) part of the transform
                            let collisionNormal = nkast.Aether.Physics2D.Common.Complex.Multiply (shape.Normals[v], &collisionXF.q) |> convertVector
                            // Project the vertex position relative to the particle position onto the edge's normal to find the distance
                            let distance = Vector2.Dot (collisionNormal, collisionVertex - particle.Position)
                            if distance < shortestDistance then
                                shortestDistance <- distance
                                // Push the particle out of the shape in the direction of the closest edge's normal
                                closestPoint <- collisionNormal * distance + particle.Position
                                normal <- collisionNormal

                | :? CircleShape as shape ->
                    let mutable newPosition = nkast.Aether.Physics2D.Common.Vector2 (newPosition.X, newPosition.Y)
                    if fixture.TestPoint &newPosition then
                        isColliding <- true
                        // Push the particle out of the circle by normalizing the circle's center relative to the particle position,
                        // and pushing the particle out in the direction of the normal
                        let center = shape.Position + fixture.Body.Position |> convertVector
                        normal <- (particle.Position - center).Normalized
                        closestPoint <- center + normal * (shape.Radius / normal.Magnitude);

                | (:? EdgeShape as EdgeFromEdgeShape (edgeStart, edgeEnd))
                | (:? ChainShape as EdgeFromChainShape particle.PotentialFixtureChildIndexes f (edgeStart, edgeEnd)) ->
                    // Collision with an edge - use line-segment intersection

                    // Transform the shape's vertices from local space to world space
                    let mutable collisionXF = Unchecked.defaultof<_>
                    fixture.Body.GetTransform &collisionXF
                    let edgeStart = convertVector (nkast.Aether.Physics2D.Common.Transform.Multiply (edgeStart, &collisionXF))
                    let edgeEnd = convertVector (nkast.Aether.Physics2D.Common.Transform.Multiply (edgeEnd, &collisionXF))

                    let edgeSegment = edgeEnd - edgeStart
                    let particleMovement = newPosition - particle.Position
                    
                    // Shim for .NET 10 Vector2.Cross(Vector2, Vector2). Use it when we upgrade to .NET 10
                    let vector2Cross (v1 : Vector2, v2 : Vector2) = v1.X * v2.Y - v1.Y * v2.X
                    let cross_particleMovement_edgeSegment = vector2Cross (particleMovement, edgeSegment)
                    if abs cross_particleMovement_edgeSegment > 1e-6f then // Not collinear

                        // particle movement path: P(t) = particle.Position + t * particleMovement; when collision, 0 <= t <= 1
                        let t = vector2Cross (particle.Position - edgeStart, edgeSegment) / cross_particleMovement_edgeSegment
                        // edge segment: Q(u) = edgeStart + u * edgeSegment; when collision, 0 <= u <= 1
                        let u = vector2Cross (particle.Position - edgeStart, particleMovement) / cross_particleMovement_edgeSegment
                        if t >= 0f && t <= 1f && u >= 0f && u <= 1f then

                            isColliding <- true
                            closestPoint <- particle.Position + t * particleMovement
                            // Determine the normal based on which side the particle started
                            let edgeNormal = Vector2.Normalize (Vector2 (-edgeSegment.Y, edgeSegment.X))
                            normal <- if Vector2.Dot (edgeNormal, particle.Position - edgeStart) < 0f then edgeNormal else -edgeNormal

                    else () // Collinear, handle separately if needed
                | shape -> Log.warnOnce $"Shape not implemented: {shape}"

                if isColliding then
                    particle.Position <- closestPoint + 0.05f * normal
                    particle.Velocity <- (particle.Velocity - 1.2f * Vector2.Dot (particle.Velocity, normal) * normal) * 0.85f
                    particle.Delta <- v2Zero

        ) |> fun result -> assert result.IsCompleted
        
        // move particles
        let bounds = (fluidSystem.GetBounds world).Box2
        let mutable newParticles = []
        for i in 0 .. dec activeParticles do
            let particle = &particles[i]
            let newVelocity = particle.Velocity + particle.Delta
            let newPosition = particle.Position + newVelocity + particle.Delta
            let newVelocity = newVelocity * Constants.Engine.Meter2d
            let newPosition = newPosition * Constants.Engine.Meter2d
            if bounds.Contains newPosition <> ContainmentType.Disjoint then
                newParticles <- { Position = newPosition; Velocity = newVelocity } :: newParticles
            else
                activeParticles <- dec activeParticles
            
            Buffers.ArrayPool.Shared.Return particle.PotentialFixtureChildIndexes
            Buffers.ArrayPool.Shared.Return particle.PotentialFixtures
            Buffers.ArrayPool.Shared.Return particle.Neighbors
        Buffers.ArrayPool.Shared.Return particles
        fluidSystem.SetParticles (FList.ofList newParticles) world
        fluidSystem.Set (nameof Entity.ParticleSimulatedCount) activeParticles world
    
    override _.Render (_, fluidSystem, world) =
        let particleRadius = fluidSystem.GetParticleRadius world
        let cellSize = particleRadius * fluidSystem.GetParticleCellScale world
        let drawCells = fluidSystem.GetParticleCellColor world
        let grid = Collections.Generic.HashSet ()

        let staticImage = fluidSystem.GetStaticImage world
        let insetOpt = match fluidSystem.GetInsetOpt world with Some inset -> ValueSome inset | None -> ValueNone
        let clipOpt = fluidSystem.GetClipOpt world |> Option.toValueOption
        let color = fluidSystem.GetColor world
        let blend = fluidSystem.GetBlend world
        let emission = fluidSystem.GetEmission world
        let flip = fluidSystem.GetFlip world
        let drawnSize = fluidSystem.GetParticleImageSizeOpt world |> Option.defaultValue (v2Dup particleRadius)
        let mutable transform = Transform.makeIntuitive false v3Zero v3One v3Zero drawnSize.V3 v3Zero (fluidSystem.GetElevation world)
        for p in fluidSystem.GetParticles world do
            transform.Position <- p.Position.V3
            World.renderLayeredSpriteFast (transform.Elevation, transform.Horizon, staticImage, &transform, &insetOpt, &clipOpt, staticImage, &color, blend, &emission, flip, world)
            if drawCells.IsSome then grid.Add (positionToCell cellSize p.Position) |> ignore

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
        
[<AutoOpen>]
module LineSegmentsExtensions =
    type Entity with
        member this.LineSegments = lens (nameof Entity.LineSegments) this this.GetLineSegments this.SetLineSegments
        member this.GetLineSegments world : Vector2 array = this.Get (nameof Entity.LineSegments) world
        member this.SetLineSegments (value : Vector2 array) world = this.Set (nameof Entity.LineSegments) value world
        member this.LineWidth = lens (nameof Entity.LineWidth) this this.GetLineWidth this.SetLineWidth
        member this.GetLineWidth world : single = this.Get (nameof Entity.LineWidth) world
        member this.SetLineWidth (value : single) world = this.Set (nameof Entity.LineWidth) value world
type LineSegmentsDispatcher () =
    inherit Entity2dDispatcher (true, false, false)

    static member Facets = [typeof<RigidBodyFacet>]
    static member Properties =
        [define Entity.LineSegments Array.empty
         define Entity.LineWidth 2f
         define Entity.Color colorOne
         ]

    override _.Update (lineSegments, world) =
        let segments = lineSegments.GetLineSegments world
        if Array.notEmpty segments then
            let box = Box2.Enclose segments
            let lineWidth = lineSegments.GetLineWidth world
            lineSegments.SetPosition box.Center.V3 world
            lineSegments.SetSize (v3 (box.Width + lineWidth) (box.Height + lineWidth) 0f) world
            lineSegments.SetBodyShape (
                ContourShape
                    { Links = segments |> Array.map (fun p -> ((p - box.Center) / box.Size).V3)
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
            let p1 = segments[s]
            let p2 = segments[inc s]
            transform.Position <- ((p1 + p2) / 2f).V3
            transform.Rotation <- Quaternion.CreateLookAt2d (p2 - p1)
            transform.Size <- v3 (p2 - p1).Magnitude lineWidth 0f
            World.renderLayeredSpriteFast (transform.Elevation, transform.Horizon, staticImage, &transform, &insetOpt, &clipOpt, staticImage, &color, blend, &emission, flip, world)
        
[<AutoOpen>]
module LiquidSimExtensions =
    type Screen with
        member this.LineSegments = lens (nameof Screen.LineSegments) this this.GetLineSegments this.SetLineSegments
        member this.GetLineSegments world : Vector2 array list = this.Get (nameof Screen.LineSegments) world
        member this.SetLineSegments (value : Vector2 array list) world = this.Set (nameof Screen.LineSegments) value world
// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type LiquidSimDispatcher () =
    inherit ScreenDispatcherImSim ()
    
    static member Properties =
        [define Screen.InfoOpened false
         define Screen.LineSegments []]

    // here we define the screen's top-level behavior
    override _.Process (_, liquidSim, world) =
        World.beginGroup Simulants.LiquidSimScene.Name [] world

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
             // Individual sprites on the same elevation are ordered from top to bottom then by asset tag.
             // Here, we don't draw water above the borders.
             Entity.Elevation .= -1f] world
        let fluidSystem = world.DeclaredEntity

        // define menu
        let menuPosition =
            let mutable y = 190f
            fun () ->
                y <- y - 30f
                Entity.Position .= v3 255f y 0f

        // particle count button
        World.doText $"Particle Count"
            [menuPosition ()
             Entity.Text @= $"{fluidSystem.GetParticleSimulatedCount world} Particles"
             Entity.Elevation .= 1f] world

        // clear button
        if World.doButton $"Clear"
            [menuPosition ()
             Entity.Text .= "Clear"
             Entity.Elevation .= 1f] world then
            fluidSystem.SetParticles FList.empty world
            liquidSim.SetLineSegments [] world

        // gravity button
        let gravities =
            [|("v", World.getGravityDefault2d world)
              ("\\", (World.getGravityDefault2d world).Transform (Quaternion.CreateFromAngle2d MathF.PI_OVER_4))
              (">", (World.getGravityDefault2d world).Transform (Quaternion.CreateFromAngle2d MathF.PI_OVER_2))
              ("0", v3Zero)
              ("<", (World.getGravityDefault2d world).Transform (Quaternion.CreateFromAngle2d -MathF.PI_OVER_2))
              ("/", (World.getGravityDefault2d world).Transform (Quaternion.CreateFromAngle2d -MathF.PI_OVER_4))|]
        for i in 0 .. dec gravities.Length do
            if World.getGravity2d world = snd gravities[i] then
                if World.doButton $"Gravity"
                    [menuPosition ()
                     Entity.Text @= $"Gravity: {fst gravities[i]}"
                     Entity.Elevation .= 1f] world then
                    World.setGravity2d (snd gravities[(i + 1) % gravities.Length]) world

        // particle sprite button
        if World.doButton $"Particle Sprite"
            [menuPosition ()
             Entity.Text @= $"Particle Sprite: {(fluidSystem.GetStaticImage world).AssetName}"
             Entity.Elevation .= 1f
             Entity.FontSizing .= Some 8] world then
            if fluidSystem.GetStaticImage world = Assets.Default.Ball then
                // In Paint.NET (canvas size = 50 x 50), use the Brush (size = 50, hardness = 50%, fill = solid color #0094FF)
                // and click the center once, to generate this Particle image.
                fluidSystem.SetStaticImage Assets.Gameplay.Liquid world
                fluidSystem.SetParticleImageSizeOpt None world
            else
                fluidSystem.SetStaticImage Assets.Default.Ball world
                fluidSystem.SetParticleImageSizeOpt (v2Dup 2f |> Some) world

        // physical viscosity button
        if World.doButton $"Physical Viscosity"
            [menuPosition ()
             Entity.Text @= $"Physical Viscosity: {fluidSystem.GetPhysicalViscosity world}"
             Entity.Elevation .= 1f
             Entity.FontSizing .= Some 8] world then
            fluidSystem.PhysicalViscosity.Map (function
                | 0.004f -> 0.01f // Very thin, water-like
                | 0.01f -> 0.1f // Light oil
                | 0.1f -> 1.0f // Honey-like flow
                | 1.0f -> 5.0f // Thick syrup
                | 5.0f -> 20.0f // Ketchup/molasses
                | 20.0f -> 100.0f // Very stiff, peanut butter-like
                | _ -> 0.004f) world

        // artificial viscosity button
        if World.doButton $"Artificial Viscosity"
            [menuPosition ()
             Entity.Text @= $"Artificial Viscosity: {fluidSystem.GetArtificialViscosity world}"
             Entity.Elevation .= 1f
             Entity.FontSizing .= Some 8] world then
            fluidSystem.ArtificialViscosity.Map (function
                | 0f -> 0.2f
                | 0.2f -> 0.5f
                | 0.5f -> 0.7f
                | 0.7f -> 0.9f
                | 0.9f -> 0.99f
                | _ -> 0f) world

        // particle radius button
        if World.doButton $"Particle Radius"
            [menuPosition ()
             Entity.Text @= $"Particle Radius: {fluidSystem.GetParticleRadius world}"
             Entity.Elevation .= 1f
             Entity.FontSizing .= Some 10] world then
            fluidSystem.SetParticleRadius (Constants.Engine.Meter2d * if fluidSystem.GetParticleRadius world = 0.9f * Constants.Engine.Meter2d then 0.7f else 0.9f) world
        
        // draw cells button
        if World.doButton $"Draw Cells"
            [menuPosition ()
             Entity.Text @= $"Draw Cells: {fluidSystem.GetParticleCellColor world |> Option.isSome}"
             Entity.Elevation .= 1f
             Entity.FontSizing .= Some 10] world then
            fluidSystem.ParticleCellColor.Map (function Some _ -> None | None -> Some Color.LightBlue) world

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
                World.destroyEntity paddle world
            }
            
        
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
            liquidSim.SetInfoOpened true world

        // info panel
        if liquidSim.GetInfoOpened world then
        
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
                 "Controls: Mouse Left - Click button or Add particles.\n\
                    Mouse Right - Delete particles.\n\
                    Mouse Middle - Summon a sphere."
                 Entity.FontSizing .= Some 10
                 Entity.TextMargin .= v2 5f 0f] world
            if World.doButton "Info Close"
                [Entity.LayoutOrder .= 3
                 Entity.Text .= "Close"] world then
                liquidSim.SetInfoOpened false world
            if World.doButton "Info Exit"
                [Entity.LayoutOrder .= 4
                 Entity.Text .= "Exit"] world && world.Unaccompanied then
                World.exit world

            // end info panel declaration
            World.endPanel world

            // declare info links
            for (position, size, url) in
                [(v2 -126f 115f, v2 200f 32f, "https://github.com/klutch/Box2DFluid")
                 (v2 25f 115f, v2 50f 32f, "https://github.com/klutch")
                 (v2 -127.5f 57.5f, v2 115f 32f, "https://github.com/bryanedds/Nu/pull/1120")
                 (v2 3.5f 57.5f, v2 105f 32f, "https://github.com/Happypig375")] do
                if World.doButton $"Info Origin Button {url.Replace ('/', '\\')}"
                    [Entity.Position .= position.V3
                     Entity.Size .= size.V3
                     Entity.Elevation .= 11f] world then
                    System.Diagnostics.Process.Start (System.Diagnostics.ProcessStartInfo (url, UseShellExecute = true)) |> ignore

        // mouse interactions with fluid system
        if liquidSim.GetSelected world && world.Advancing then
            let mouse = World.getMousePosition2dWorld false world
            
            // create particles
            if World.isMouseButtonDown MouseLeft world then
                let createParticle particles =
                    let jitter = v2 (Gen.randomf * 2f - 1f) (Gen.randomf - 0.5f) * Constants.Engine.Meter2d
                    FList.cons { Position = mouse + jitter; Velocity = v2Zero } particles
                fluidSystem.Particles.Map (createParticle >> createParticle >> createParticle >> createParticle) world
            
            // delete particles
            if World.isMouseButtonDown MouseRight world then
                fluidSystem.Particles.Map (
                    FList.filter (
                        _.Position
                        >> (box2 (mouse - v2Dup (Constants.Engine.Meter2d / 2f)) (v2Dup Constants.Engine.Meter2d)).Contains
                        >> (=) ContainmentType.Disjoint)) world
            
            // draw a contour
            if World.isMouseButtonPressed MouseMiddle world then
                liquidSim.LineSegments.Map (fun lineSegments ->
                    List.cons [|mouse|] lineSegments) world
            elif World.isMouseButtonDown MouseMiddle world then
                liquidSim.LineSegments.Map (fun lineSegments ->
                    let active = lineSegments[0]
                    if Vector2.Distance (mouse, Array.last active) > 3f then
                        List.updateAt 0 (Array.add mouse active) lineSegments
                    else lineSegments) world
            
            // summon a sphere
            if World.isKeyboardShiftDown world then
                World.doSphere2d "Mouse Sphere"
                    [Entity.Position @= mouse.V3
                     Entity.Size .= v3 64f 64f 0f] world |> ignore

        for segment in liquidSim.GetLineSegments world do
            World.doEntity<LineSegmentsDispatcher> $"Contour {segment[0]}" [Entity.LineSegments @= segment] world

        World.endGroup world
        // process camera as last task
        World.setEye2dCenter (v2 60f 10f) world