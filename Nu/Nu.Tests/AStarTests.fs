// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Tests
open System
open System.Collections.Generic
open NUnit.Framework
open Prime
open Nu
open Nu.Tests
module AStarTests =

    /// A grid whose cells are walkable unless walled.
    type Grid =
        { Width : int
          Height : int
          Walls : HashSet<struct (int * int)> }

    /// A cell in a grid, connected to its walkable up, down, left, and right neighbors.
    type [<CustomEquality; NoComparison>] GridNode =
        { X : int
          Y : int
          Grid : Grid }

        override this.Equals that =
            match that with
            | :? GridNode as that -> this.X = that.X && this.Y = that.Y
            | _ -> false

        override this.GetHashCode () =
            HashCode.Combine (this.X, this.Y)

        interface Neighborable<GridNode> with
            member this.Neighbors =
                seq {
                    for (dx, dy) in [(1, 0); (-1, 0); (0, 1); (0, -1)] do
                        let (x, y) = (this.X + dx, this.Y + dy)
                        if  x >= 0 && x < this.Grid.Width &&
                            y >= 0 && y < this.Grid.Height &&
                            not (this.Grid.Walls.Contains struct (x, y)) then
                            { X = x; Y = y; Grid = this.Grid } }

    /// A node in an explicitly-built graph with weighted, directed edges.
    type [<AllowNullLiteral>] GraphNode (name : string) =
        let edges = Dictionary<GraphNode, single> HashIdentity.Reference
        member this.Name = name
        member this.Edges = edges
        member this.Connect (that : GraphNode) cost = edges[that] <- cost
        override this.ToString () = name
        interface Neighborable<GraphNode> with
            member this.Neighbors = edges.Keys

    let makeGrid width height walls =
        { Width = width; Height = height; Walls = HashSet<struct (int * int)> (Seq.map (fun (x, y) -> struct (x, y)) walls) }

    let cell grid x y =
        { X = x; Y = y; Grid = grid }

    let gridDistance (_ : GridNode) (_ : GridNode) = 1.0f

    let manhattan (destination : GridNode) (node : GridNode) =
        single (abs (node.X - destination.X) + abs (node.Y - destination.Y))

    let findGridPath grid (sx, sy) (dx, dy) =
        let destination = cell grid dx dy
        AStar.FindPath (cell grid sx sy, destination, Func<_, _, _> gridDistance, Func<_, _> (manhattan destination))

    let graphDistance (source : GraphNode) (destination : GraphNode) =
        source.Edges[destination]

    let findGraphPath (source : GraphNode) (destination : GraphNode) =
        AStar.FindPath (source, destination, Func<_, _, _> graphDistance, Func<_, _> (fun _ -> 0.0f))

    /// The nodes of a path from start to destination (a path enumerates from its last step backward).
    let pathNodes (path : 'n Path) =
        path |> Seq.rev |> List.ofSeq

    /// Breadth-first shortest step count over a grid, as ground truth for unit-cost paths.
    let bfsDistance grid (sx, sy) (dx, dy) =
        let distances = Dictionary<GridNode, int> ()
        let queue = Queue<GridNode> ()
        let start = cell grid sx sy
        distances[start] <- 0
        queue.Enqueue start
        while queue.Count > 0 do
            let node = queue.Dequeue ()
            for neighbor in (node :> GridNode Neighborable).Neighbors do
                if not (distances.ContainsKey neighbor) then
                    distances[neighbor] <- distances[node] + 1
                    queue.Enqueue neighbor
        match distances.TryGetValue (cell grid dx dy) with
        | (true, distance) -> Some distance
        | (false, _) -> None

    /// Check that each step of a grid path moves to a walkable neighbor.
    let assertPathWalkable grid (nodes : GridNode list) =
        for (a, b) in List.pairwise nodes do
            Assert.Equal (1, abs (a.X - b.X) + abs (a.Y - b.Y))
            Assert.IsFalse (grid.Walls.Contains struct (b.X, b.Y))

    [<Test>]
    let ``Path enumerates from its last step back to its start and accumulates cost.`` () =
        let path = Path<string>("a").AddStep("b", 1.5f).AddStep("c", 2.0f)
        Assert.Equal (["c"; "b"; "a"], List.ofSeq path)
        Assert.Equal ("c", path.LastStep)
        Assert.Equal (3.5f, path.TotalCost)
        Assert.Equal (1.5f, path.PreviousSteps.TotalCost)
        Assert.IsNull path.PreviousSteps.PreviousSteps.PreviousSteps

    [<Test>]
    let ``Finding a path from a node to itself yields just that node at no cost.`` () =
        let grid = makeGrid 4 4 []
        let path = findGridPath grid (2, 2) (2, 2)
        Assert.IsNotNull path
        Assert.Equal ([cell grid 2 2], pathNodes path)
        Assert.Equal (0.0f, path.TotalCost)

    [<Test>]
    let ``Finding a path across an open grid yields a shortest path.`` () =
        let grid = makeGrid 8 8 []
        let path = findGridPath grid (0, 0) (5, 3)
        Assert.IsNotNull path
        let nodes = pathNodes path
        Assert.Equal (cell grid 0 0, List.head nodes)
        Assert.Equal (cell grid 5 3, List.last nodes)
        Assert.Equal (8.0f, path.TotalCost)
        Assert.Equal (9, List.length nodes)
        assertPathWalkable grid nodes

    [<Test>]
    let ``Finding a path around a wall takes the shortest detour.`` () =
        // a wall at x = 2 from y = 0 to y = 3 leaves a gap only at y = 4
        let grid = makeGrid 5 5 [for y in 0 .. 3 -> (2, y)]
        let path = findGridPath grid (0, 0) (4, 0)
        Assert.IsNotNull path
        let nodes = pathNodes path
        Assert.Equal (12.0f, path.TotalCost)
        Assert.Equal (cell grid 0 0, List.head nodes)
        Assert.Equal (cell grid 4 0, List.last nodes)
        Assert.IsTrue (List.contains (cell grid 2 4) nodes)
        assertPathWalkable grid nodes

    [<Test>]
    let ``Finding a path to an unreachable node yields null.`` () =
        // a full wall at x = 2 separates the left side from the right
        let grid = makeGrid 5 5 [for y in 0 .. 4 -> (2, y)]
        Assert.IsNull (findGridPath grid (0, 0) (4, 4))

    [<Test>]
    let ``Finding a path prefers a cheaper multi-step route over a costly direct edge.`` () =
        let a = GraphNode "a"
        let b = GraphNode "b"
        let c = GraphNode "c"
        let d = GraphNode "d"
        a.Connect d 10.0f
        a.Connect b 1.0f
        b.Connect c 1.0f
        c.Connect d 1.0f
        let path = findGraphPath a d
        Assert.IsNotNull path
        Assert.Equal (["a"; "b"; "c"; "d"], pathNodes path |> List.map _.Name)
        Assert.Equal (3.0f, path.TotalCost)

    [<Test>]
    let ``Finding a path follows edge direction.`` () =
        let a = GraphNode "a"
        let b = GraphNode "b"
        b.Connect a 1.0f
        Assert.IsNull (findGraphPath a b)
        Assert.IsNotNull (findGraphPath b a)

    [<Test>]
    let ``Finding a path through a graph with cycles terminates with the cheapest path.`` () =
        let a = GraphNode "a"
        let b = GraphNode "b"
        let c = GraphNode "c"
        let d = GraphNode "d"
        a.Connect b 1.0f
        b.Connect a 1.0f
        b.Connect c 5.0f
        c.Connect b 5.0f
        b.Connect d 2.0f
        d.Connect c 1.0f
        let path = findGraphPath a c
        Assert.IsNotNull path
        Assert.Equal (["a"; "b"; "d"; "c"], pathNodes path |> List.map _.Name)
        Assert.Equal (4.0f, path.TotalCost)

    [<Test>]
    let ``Finding paths over random grids matches breadth-first shortest distances.`` () =
        let random = Random 1200
        for _ in 0 .. dec 200 do
            let width = random.Next (2, 12)
            let height = random.Next (2, 12)
            let walls = [for x in 0 .. dec width do for y in 0 .. dec height do if random.NextDouble () < 0.3 then (x, y)]
            let grid = makeGrid width height walls
            let free = [for x in 0 .. dec width do for y in 0 .. dec height do if not (grid.Walls.Contains struct (x, y)) then (x, y)]
            if List.length free >= 2 then
                let start = free[random.Next free.Length]
                let destination = free[random.Next free.Length]
                let path = findGridPath grid start destination
                match bfsDistance grid start destination with
                | Some distance ->
                    Assert.IsNotNull path
                    Assert.Equal (single distance, path.TotalCost)
                    let nodes = pathNodes path
                    Assert.Equal (cell grid (fst start) (snd start), List.head nodes)
                    Assert.Equal (cell grid (fst destination) (snd destination), List.last nodes)
                    Assert.Equal (distance + 1, List.length nodes)
                    assertPathWalkable grid nodes
                | None -> Assert.IsNull path
