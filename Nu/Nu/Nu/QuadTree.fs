namespace Nu
open System
open System.Collections.Generic
open OpenTK
open Prime

// NOTE: this code is a complete WIP - don't use it just yet :)

type [<NoEquality; NoComparison>] private 'e QuadNode =
    private
        { Depth : int
          Position : Vector2
          Size : Vector2
          Children : 'e QuadNode array
          Elements : 'e HashSet }

[<RequireQualifiedAccess>]
module private QuadNode =

    let private makeChildPosition i position (childSize : Vector2) =
        match i with
        | 0 -> position
        | 1 -> position + Vector2 (childSize.X, 0.0f)
        | 2 -> position + Vector2 (0.0f, childSize.Y)
        | 3 -> position + childSize
        | _ -> failwithumf ()

    let internal tryAddElement _ _ _ _ =
        false
        
    let internal tryRemoveElement _ _ _ _ =
        false

    let rec internal make<'e> depth position (size : Vector2) =
        if depth < 1 then failwith "Invalid depth for QuadNode. Expected depth >= 1."
        let children =
            if depth > 1 then 
                [|for i in 0 .. 3 do
                    let childDepth = depth - 1
                    let childSize = size * 0.25f
                    let childPosition = makeChildPosition i position childSize
                    yield make<'e> childDepth childPosition childSize|]
            else [||]
        { Depth = depth
          Position = position
          Size = size
          Children = (children : 'e QuadNode array)
          Elements = HashSet () } : 'e QuadNode
        
type [<NoEquality; NoComparison>] 'e QuadTree =
    private
        { Node : 'e QuadNode
          OmnipresentElements : 'e HashSet }

[<RequireQualifiedAccess>]
module QuadTree =

    let addElement quadTree omnipresence position size element =
        if omnipresence then
            ignore <| quadTree.OmnipresentElements.Add element
        elif not <| QuadNode.tryAddElement quadTree.Node position size element then
            note "Element is outside of quad tree's containment area."
            ignore <| quadTree.OmnipresentElements.Add element

    let removeElement quadTree omnipresence position size element =
        if omnipresence then
            ignore <| quadTree.OmnipresentElements.Remove element
        elif not <| QuadNode.tryRemoveElement quadTree.Node position size element then
            ignore <| quadTree.OmnipresentElements.Remove element

    let updateElement
        quadTree
        oldOmnipresence oldPosition oldSize
        newOmnipresence newPosition newSize
        element =
        removeElement quadTree oldOmnipresence oldPosition oldSize element
        addElement quadTree newOmnipresence newPosition newSize element

    let getElementsAtPoint _ _ =
        ()

    let getElementsInRect _ _ =
        ()

    let make depth position size =
        { Node = QuadNode.make<'e> depth position size
          OmnipresentElements = HashSet () }