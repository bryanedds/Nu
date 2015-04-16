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
          Children : QuadNode array
          Elements : obj Address HashSet }

    static member private makeChildPosition i position (childSize : Vector2) =
        match i with
        | 0 -> position
        | 1 -> position + Vector2 (childSize.X, 0.0f)
        | 2 -> position + Vector2 (0.0f, childSize.Y)
        | 3 -> position + childSize
        | _ -> failwithumf ()

    static member private makeChildren depth position (size : Vector2) =
        [|for i in 0 .. 3 do
            let childDepth = depth - 1
            let childSize = size * 0.25f
            let childPosition = QuadNode.makeChildPosition i position childSize
            yield QuadNode.make childDepth childPosition childSize|]
        
    static member tryAddElement _ _ _ _ =
        false

    static member make depth position (size : Vector2) =
        let children = if depth > 1 then QuadNode.makeChildren depth position size else [||]
        { Depth = depth
          Position = position
          Size = size
          Children = children
          Elements = HashSet () }
        
type [<NoEquality; NoComparison>] Quadtree =
    private
        { Node : QuadNode
          OmnipresentElements : obj Address HashSet }

    static member addElement omnipresence position size element quadtree =
        if omnipresence then
            ignore <| quadtree.OmnipresentElements.Add element
        elif not <| QuadNode.tryAddElement position size element quadtree.Node then
            note "Element is outside of quad tree's containment area."
            ignore <| quadtree.OmnipresentElements.Add element

    static member make depth position size =
        { Node = QuadNode.make depth position size
          OmnipresentElements = HashSet () }