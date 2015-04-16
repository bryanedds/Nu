namespace Nu
open System
open System.Collections.Generic
open OpenTK
open Prime

type [<NoEquality; NoComparison>] QuadNode =
    private
        { Depth : int
          Position : Vector2
          Size : Vector2
          Nodes : QuadNode array
          Elements : obj Address HashSet }

    static member make depth position (size : Vector2) =
        let nodes =
            [|for i in 0 .. 3 do
                let childDepth = depth - 1
                let childSize = size * 0.25f
                let childPosition =
                    match i with
                    | 0 -> position
                    | 1 -> position + Vector2 (childSize.X, 0.0f)
                    | 2 -> position + Vector2 (0.0f, childSize.Y)
                    | 3 -> position + size
                    | _ -> failwithumf ()
                yield QuadNode.make childDepth childPosition childSize|]
        { Depth = depth
          Position = position
          Size = size
          Nodes = nodes
          Elements = HashSet () }
        
type [<NoEquality; NoComparison>] QuadTree =
    private
        { Node : QuadNode
          OmnipresentElements : obj Address HashSet }

    static member make depth position size =
        { Node = QuadNode.make depth position size
          OmnipresentElements = HashSet () }