// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Nu
open System
open System.Collections.Generic
open OpenTK
open Prime

[<AutoOpen>]
module internal SpatialNodeModule =

    type [<NoEquality; NoComparison>] SpatialNode<'e when 'e : equality> =
        private
            { Depth : int
              Bounds : Vector4
              Children : 'e SpatialNode array
              Elements : 'e HashSet }

    [<RequireQualifiedAccess>]
    module internal SpatialNode =

        let internal atPoint point node =
            Math.isPointInBounds point node.Bounds

        let internal inBounds bounds node =
            Math.isBoundsInBounds bounds node.Bounds

        let internal intersectingBounds bounds node =
            Math.isBoundsIntersectingBounds bounds node.Bounds

        let rec internal addElement bounds element node =
            let nodes =
                node.Children |>
                Seq.filter (fun node -> inBounds bounds node) |>
                Array.ofSeq
            match nodes.Length with
            | 0 -> node.Elements.Add element |> ignore
            | 1 -> let child = nodes.[0] in addElement bounds element child
            | _ -> failwithumf ()

        let rec internal removeElement bounds element node =
            let nodes =
                node.Children |>
                Seq.filter (fun node -> inBounds bounds node) |>
                Array.ofSeq
            match nodes.Length with
            | 0 -> node.Elements.Remove element |> ignore
            | 1 -> let child = nodes.[0] in removeElement bounds element child
            | _ -> failwithumf ()

        let rec internal getElementsAtPoint point node (list : 'e List) =
            for element in node.Elements do list.Add element
            for child in node.Children do
                if atPoint point child then
                    getElementsAtPoint point child list

        let rec internal getElementsInBounds bounds node (list : 'e List) =
            for element in node.Elements do list.Add element
            for child in node.Children do
                if intersectingBounds bounds child then
                    getElementsInBounds bounds child list

        let internal getDepth node =
            node.Depth

        let rec internal clone node =
            { Depth = node.Depth
              Bounds = node.Bounds
              Children = Array.map clone node.Children
              // NOTE: it is inefficient to shallow-clone a HashSet like this, but sadly, .NET does not provide a proper
              // Clone method! #ARGH!
              Elements = HashSet node.Elements (* HashIdentity *) }

        let rec internal make<'e when 'e : equality> granularity depth (bounds : Vector4) =
            if granularity < 2 then failwith "Invalid granularity for SpatialNode. Expected value of at least 2."
            if depth < 1 then failwith "Invalid depth for SpatialNode. Expected value of at least 1."
            let children =
                if depth > 1 then 
                    [|for i in 0 .. granularity * granularity - 1 do
                        let childDepth = depth - 1
                        let childSize = Vector2 (bounds.Z - bounds.X, bounds.W - bounds.Y) / single granularity
                        let childPosition = bounds.Xy + Vector2 (childSize.X * single (i % granularity), childSize.Y * single (i / granularity))
                        let childBounds = Vector4 (childPosition.X, childPosition.Y, childPosition.X + childSize.X, childPosition.Y + childSize.Y)
                        yield make<'e> granularity childDepth childBounds|]
                else [||]
            { Depth = depth
              Bounds = bounds
              Children = (children : 'e SpatialNode array)
              Elements = HashSet () (* HashIdentity *) }

[<AutoOpen>]
module SpatialTreeModule =

    /// A spatial structure that organizes elements on a 2D plane.
    type [<NoEquality; NoComparison>] SpatialTree<'e when 'e : equality> =
        private
            { Node : 'e SpatialNode
              OmnipresentElements : 'e HashSet }

    [<RequireQualifiedAccess>]
    module SpatialTree =
    
        let addElement omnipresence bounds element tree =
            if omnipresence then 
                tree.OmnipresentElements.Add element |> ignore
            elif not ^ SpatialNode.inBounds bounds tree.Node then
                Log.info "Element is outside of spatial tree's containment area or is being added redundantly."
                tree.OmnipresentElements.Add element |> ignore
            else SpatialNode.addElement bounds element tree.Node

        let removeElement omnipresence bounds element tree =
            if omnipresence then 
                tree.OmnipresentElements.Remove element |> ignore
            elif not ^ SpatialNode.inBounds bounds tree.Node then
                Log.info "Element is outside of spatial tree's containment area or is not present for removal."
                tree.OmnipresentElements.Remove element |> ignore
            else SpatialNode.removeElement bounds element tree.Node
    
        let updateElement
            oldOmnipresence oldBounds
            newOmnipresence newBounds
            element tree =
            removeElement oldOmnipresence oldBounds element tree
            addElement newOmnipresence newBounds element tree
    
        let getElementsAtPoint point tree =
            let list = List tree.OmnipresentElements
            SpatialNode.getElementsAtPoint point tree.Node list
            list
    
        let getElementsInBounds bounds tree =
            let list = List tree.OmnipresentElements
            SpatialNode.getElementsInBounds bounds tree.Node list
            list
    
        let getDepth tree =
            SpatialNode.getDepth tree.Node
    
        let clone tree =
            { Node = SpatialNode.clone tree.Node
              OmnipresentElements = HashSet tree.OmnipresentElements (* HashIdentity *) }
    
        let make<'e when 'e : equality> granularity depth bounds =
            { Node = SpatialNode.make<'e> granularity depth bounds
              OmnipresentElements = HashSet () (* HashIdentity *) }

/// A spatial structure that organizes elements on a 2D plane.
type SpatialTree<'e when 'e : equality> = SpatialTreeModule.SpatialTree<'e>