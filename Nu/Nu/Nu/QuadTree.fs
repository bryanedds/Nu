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

[<RequireQualifiedAccess>]
module private QuadNode =

    let private makeChildPosition i position (childSize : Vector2) =
        match i with
        | 0 -> position
        | 1 -> position + Vector2 (childSize.X, 0.0f)
        | 2 -> position + Vector2 (0.0f, childSize.Y)
        | 3 -> position + childSize
        | _ -> failwithumf ()

    let rec internal tryAddElement position size element node =
        let nodeBounds = Vector4 (node.Position.X, node.Position.Y, node.Position.X + node.Size.X, node.Position.Y + node.Size.Y)
        if Math.isBoundsInBounds3 position size nodeBounds then
            let result = node.Elements.Add element
            Array.iter (tryAddElement position size element >> ignore) node.Children
            result
        else false

    let rec internal tryRemoveElement position size element node =
        let nodeBounds = Vector4 (node.Position.X, node.Position.Y, node.Position.X + node.Size.X, node.Position.Y + node.Size.Y)
        if Math.isBoundsInBounds3 position size nodeBounds then
            Array.iter (tryRemoveElement position size element >> ignore) node.Children
            node.Elements.Remove element
        else false

    let rec internal getElementsNearPoint position node =
        if Math.isPointInBounds3 position node.Position node.Size then
            match node.Children.Length with
            | 0 -> List.ofSeq node.Elements
            | _ -> List.ofSeq ^ Seq.concat ^ Seq.map (fun child -> getElementsNearPoint position child) node.Children
        else []

    let rec internal getElementsNearBounds bounds node =
        let nodeBounds = Vector4 (node.Position.X, node.Position.Y, node.Position.X + node.Size.X, node.Position.Y + node.Size.Y)
        if Math.isBoundsInBounds bounds nodeBounds then
            match node.Children.Length with
            | 0 -> List.ofSeq node.Elements
            | _ -> List.ofSeq ^ Seq.concat ^ Seq.map (fun child -> getElementsNearBounds bounds child) node.Children
        else []

    let internal getDepth node =
        node.Depth

    let rec internal clone node =
        { Depth = node.Depth
          Position = node.Position
          Size = node.Size
          Children = Array.map clone node.Children
          Elements = HashSet node.Elements }

    let rec internal make<'e> depth position (size : Vector2) =
        if depth < 1 then failwith "Invalid depth for QuadNode. Expected depth >= 1."
        let children =
            if depth > 1 then 
                [|for i in 0 .. 3 do
                    let childDepth = depth - 1
                    let childSize = size * 0.5f
                    let childPosition = makeChildPosition i position childSize
                    yield make<'e> childDepth childPosition childSize|]
            else [||]
        { Depth = depth
          Position = position
          Size = size
          Children = (children : 'e QuadNode array)
          Elements = HashSet () }
        
type [<NoEquality; NoComparison>] 'e QuadTree =
    private
        { Node : 'e QuadNode
          OmnipresentElements : 'e HashSet }

[<RequireQualifiedAccess>]
module QuadTree =

    let addElement omnipresence position size element tree =
        if omnipresence then
            ignore ^ tree.OmnipresentElements.Add element
        elif not ^ QuadNode.tryAddElement position size element tree.Node then
            note "Element is outside of quad tree's containment area."
            ignore ^ tree.OmnipresentElements.Add element

    let removeElement omnipresence position size element tree =
        if omnipresence then
            ignore ^ tree.OmnipresentElements.Remove element
        elif not ^ QuadNode.tryRemoveElement position size element tree.Node then
            ignore ^ tree.OmnipresentElements.Remove element

    let updateElement
        oldOmnipresence oldPosition oldSize
        newOmnipresence newPosition newSize
        element tree =
        removeElement oldOmnipresence oldPosition oldSize element tree
        addElement newOmnipresence newPosition newSize element tree

    let getElementsNearPoint position tree =
        let otherElements = QuadNode.getElementsNearPoint position tree.Node
        List.ofSeq ^ Seq.append tree.OmnipresentElements ^ Seq.distinct otherElements

    let getElementsNearBounds bounds tree =
        let otherElements = QuadNode.getElementsNearBounds bounds tree.Node
        List.ofSeq ^ Seq.append tree.OmnipresentElements ^ Seq.distinct otherElements

    let getDepth tree =
        QuadNode.getDepth tree.Node

    let clone tree =
        { Node = QuadNode.clone tree.Node
          OmnipresentElements = HashSet tree.OmnipresentElements }

    let make<'e> depth position size =
        { Node = QuadNode.make<'e> depth position size
          OmnipresentElements = HashSet () }