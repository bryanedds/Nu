// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open Prime

[<RequireQualifiedAccess>]
module internal SpatialNode =

    type [<NoEquality; NoComparison>] SpatialNode<'e when 'e : equality> =
        private
            { Depth : int
              Bounds : Vector4
              Children : Either<'e SpatialNode array, 'e HashSet> }

    let internal atPoint point node =
        Math.isPointInBounds point node.Bounds

    let internal intersectingBounds bounds node =
        Math.isBoundsIntersectingBounds bounds node.Bounds

    let rec internal addElement bounds element node =
        if intersectingBounds bounds node then
            match node.Children with
            | Left nodes -> for node in nodes do addElement bounds element node
            | Right elements -> elements.Add element |> ignore

    let rec internal removeElement bounds element node =
        if intersectingBounds bounds node then
            match node.Children with
            | Left nodes -> for node in nodes do removeElement bounds element node
            | Right elements -> elements.Remove element |> ignore

    let rec internal updateElement oldBounds newBounds element node =
        match node.Children with
        | Left nodes ->
            for node in nodes do
                if intersectingBounds oldBounds node || intersectingBounds newBounds node then
                    updateElement oldBounds newBounds element node
        | Right elements ->
            if intersectingBounds oldBounds node then
                if not (intersectingBounds newBounds node) then elements.Remove element |> ignore
            elif intersectingBounds newBounds node then
                elements.Add element |> ignore

    let rec internal getElementsAtPoint point node (set : 'e HashSet) =
        match node.Children with
        | Left nodes -> for node in nodes do if atPoint point node then getElementsAtPoint point node set
        | Right elements -> for element in elements do set.Add element |> ignore

    let rec internal getElementsInBounds bounds node (set : 'e HashSet) =
        match node.Children with
        | Left nodes -> for node in nodes do if intersectingBounds bounds node then getElementsInBounds bounds node set
        | Right elements -> for element in elements do set.Add element |> ignore

    let internal getDepth node =
        node.Depth

    let rec internal clone node =
        { Depth = node.Depth
          Bounds = node.Bounds
          Children =
            match node.Children with
            | Right elements -> Right (HashSet (elements, HashIdentity.Structural))
            | Left nodes -> Left (Array.map clone nodes) }

    let rec internal make<'e when 'e : equality> granularity depth (bounds : Vector4) =
        if granularity < 2 then failwith "Invalid granularity for SpatialNode. Expected value of at least 2."
        if depth < 1 then failwith "Invalid depth for SpatialNode. Expected value of at least 1."
        let children =
            if depth > 1 then
                let (nodes : 'e SpatialNode array) =
                    [|for i in 0 .. granularity * granularity - 1 do
                        let childDepth = depth - 1
                        let childSize = v2 bounds.Z bounds.W / single granularity
                        let childPosition = bounds.Xy + v2 (childSize.X * single (i % granularity)) (childSize.Y * single (i / granularity))
                        let childBounds = v4Bounds childPosition childSize
                        yield make granularity childDepth childBounds|]
                Left nodes
            else Right (HashSet<'e> HashIdentity.Structural)
        { Depth = depth
          Bounds = bounds
          Children = children }

type internal SpatialNode<'e when 'e : equality> = SpatialNode.SpatialNode<'e>

[<RequireQualifiedAccess>]
module SpatialTree =

    /// A spatial structure that organizes elements on a 2D plane.
    type [<NoEquality; NoComparison>] SpatialTree<'e when 'e : equality> =
        private
            { Node : 'e SpatialNode
              OmnipresentElements : 'e HashSet }

    let addElement omnipresent bounds element tree =
        if omnipresent then
            tree.OmnipresentElements.Add element |> ignore
        else
            if not (SpatialNode.intersectingBounds bounds tree.Node) then
                Log.info "Element is outside spatial tree's containment area or is being added redundantly."
                tree.OmnipresentElements.Add element |> ignore
            else SpatialNode.addElement bounds element tree.Node

    let removeElement omnipresent bounds element tree =
        if omnipresent then 
            tree.OmnipresentElements.Remove element |> ignore
        else
            if not (SpatialNode.intersectingBounds bounds tree.Node) then
                Log.info "Element is outside spatial tree's containment area or is not present for removal."
                tree.OmnipresentElements.Remove element |> ignore
            else SpatialNode.removeElement bounds element tree.Node

    let updateElement oldBounds newBounds element tree =
        let oldInBounds = SpatialNode.intersectingBounds oldBounds tree.Node
        let newInBounds = SpatialNode.intersectingBounds newBounds tree.Node
        if oldInBounds && not newInBounds then
            // going out of bounds
            Log.info "Element is outside spatial tree's containment area."
            if not newInBounds then tree.OmnipresentElements.Add element |> ignore
            SpatialNode.updateElement oldBounds newBounds element tree.Node
        elif not oldInBounds && newInBounds then
            // going back in bounds
            if not oldInBounds then tree.OmnipresentElements.Remove element |> ignore
            SpatialNode.updateElement oldBounds newBounds element tree.Node
        elif oldInBounds && newInBounds then
            // staying in bounds
            SpatialNode.updateElement oldBounds newBounds element tree.Node
        else
            // staying out of bounds
            ()

    let getElementsAtPoint point tree =
        let set = HashSet tree.OmnipresentElements
        SpatialNode.getElementsAtPoint point tree.Node set
        set

    let getElementsInBounds bounds tree =
        let set = HashSet tree.OmnipresentElements
        SpatialNode.getElementsInBounds bounds tree.Node set
        set

    let getDepth tree =
        SpatialNode.getDepth tree.Node

    let clone tree =
        { Node = SpatialNode.clone tree.Node
          OmnipresentElements = HashSet (tree.OmnipresentElements, HashIdentity.Structural) }

    let make<'e when 'e : equality> granularity depth bounds =
        { Node = SpatialNode.make<'e> granularity depth bounds
          OmnipresentElements = HashSet HashIdentity.Structural }

/// A spatial structure that organizes elements on a 2D plane.
type SpatialTree<'e when 'e : equality> = SpatialTree.SpatialTree<'e>