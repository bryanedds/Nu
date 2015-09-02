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
        if not ^ Seq.isEmpty node.Children then
            let child = Seq.find (fun node -> Math.isPointInBounds3 position node.Position node.Size) node.Children
            getElementsNearPoint position child
        else node.Elements

    let rec internal getElementsNearBounds bounds node =
        if not ^ Seq.isEmpty node.Children then
            let child =
                Seq.find (fun node ->
                    let nodeBounds = Vector4 (node.Position.X, node.Position.Y, node.Position.X + node.Size.X, node.Position.Y + node.Size.Y)
                    Math.isBoundsInBounds bounds nodeBounds)
                    node.Children
            getElementsNearBounds bounds child
        else node.Elements

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
        Seq.append tree.OmnipresentElements otherElements |> List.ofSeq

    let getElementsNearBounds bounds tree =
        let otherElements = QuadNode.getElementsNearBounds bounds tree.Node
        Seq.append tree.OmnipresentElements otherElements |> List.ofSeq

    let make depth position size =
        { Node = QuadNode.make<'e> depth position size
          OmnipresentElements = HashSet () }