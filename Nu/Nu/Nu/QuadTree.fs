namespace Nu
open System
open System.Collections.Generic
open OpenTK
open Prime

type [<NoEquality; NoComparison>] private 'e QuadNode =
    private
        { Depth : int
          Position : Vector2
          Size : Vector2
          Children : 'e QuadNode array
          Elements : 'e HashSet }

    static member internal makeChildPosition i position (childSize : Vector2) =
        match i with
        | 0 -> position
        | 1 -> position + Vector2 (childSize.X, 0.0f)
        | 2 -> position + Vector2 (0.0f, childSize.Y)
        | 3 -> position + childSize
        | _ -> failwithumf ()

    static member internal makeChildren depth position (size : Vector2) =
        [|for i in 0 .. 3 do
            let childDepth = depth - 1
            let childSize = size * 0.25f
            let childPosition = QuadNode<'e>.makeChildPosition i position childSize
            yield QuadNode<'e>.make childDepth childPosition childSize|]

    static member internal tryAddElement _ _ _ _ =
        false
        
    static member internal tryRemoveElement _ _ _ _ =
        false

    static member internal make depth position (size : Vector2) =
        if depth < 1 then failwith "Invalid depth for QuadNode. Expected depth >= 1."
        let children = if depth > 1 then QuadNode<'e>.makeChildren depth position size else [||]
        { Depth = depth
          Position = position
          Size = size
          Children = children
          Elements = HashSet () }
        
type [<NoEquality; NoComparison>] 'e QuadTree =
    private
        { Node : 'e QuadNode
          OmnipresentElements : 'e HashSet }

    static member internal addElement quadTree omnipresence position size element =
        if omnipresence then
            ignore <| quadTree.OmnipresentElements.Add element
        elif not <| QuadNode<'e>.tryAddElement quadTree.Node position size element then
            note "Element is outside of quad tree's containment area."
            ignore <| quadTree.OmnipresentElements.Add element

    static member internal removeElement quadTree omnipresence position size element =
        if omnipresence then
            ignore <| quadTree.OmnipresentElements.Remove element
        elif not <| QuadNode<'e>.tryRemoveElement quadTree.Node position size element then
            ignore <| quadTree.OmnipresentElements.Remove element

    static member internal updateElement
        quadTree
        oldOmnipresence oldPosition oldSize
        newOmnipresence newPosition newSize
        element =
        QuadTree<'e>.removeElement quadTree oldOmnipresence oldPosition oldSize element
        QuadTree<'e>.addElement quadTree newOmnipresence newPosition newSize element

    static member internal getElementsAtPoint =
        ()

    static member internal getElementsInRect =
        ()

    static member internal make depth position size =
        { Node = QuadNode<'e>.make depth position size
          OmnipresentElements = HashSet () }

[<RequireQualifiedAccess>]
module QuadTree =

    let addElement quadTree omnipresence position size element =
        QuadTree<_>.addElement quadTree omnipresence position size element

    let removeElement quadTree omnipresence position size element =
        QuadTree<_>.removeElement quadTree omnipresence position size element

    let updateElement quadTree oldOmnipresence oldPosition oldSize newOmnipresence newPosition newSize element =
        QuadTree<_>.updateElement quadTree oldOmnipresence oldPosition oldSize newOmnipresence newPosition newSize element

    let getElementsAtPoint =
        QuadTree<_>.getElementsAtPoint

    let getElementsInRect =
        QuadTree<_>.getElementsInRect

    let make depth position size =
        QuadTree<_>.make depth position size