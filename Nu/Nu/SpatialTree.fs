// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

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
              ElementsOpt : 'e HashSet option }

    [<RequireQualifiedAccess>]
    module internal SpatialNode =

        let rec internal tryAddElement bounds element node =
            if Math.isBoundsInBounds bounds node.Bounds then
                match node.ElementsOpt with
                | Some elements -> elements.Add element
                | None ->
                    // OPTIMIZATION: imperative to avoid creating a lambda each call
                    let mutable result = false
                    for child in node.Children do
                        result <- tryAddElement bounds element child || result
                    result
            else false

        let rec internal tryRemoveElement bounds element node =
            if Math.isBoundsInBounds bounds node.Bounds then
                match node.ElementsOpt with
                | Some elements -> elements.Remove element
                | None ->
                    // OPTIMIZATION: imperative to avoid creating a lambda each call
                    let mutable result = false
                    for child in node.Children do
                        result <- tryRemoveElement bounds element child || result
                    result
            else false

        let rec internal getElementsNearPoint position node =
            if Math.isPointInBounds position node.Bounds then
                match node.ElementsOpt with
                | Some elements -> elements :> _ seq
                | None -> Seq.map (fun child -> getElementsNearPoint position child) node.Children |> Seq.concat
            else Seq.empty

        let rec internal getElementsNearBounds bounds node =
            if Math.isBoundsInBounds bounds node.Bounds then
                match node.ElementsOpt with
                | Some elements -> elements :> _ seq
                | None -> Seq.map (fun child -> getElementsNearBounds bounds child) node.Children |> Seq.concat
            else Seq.empty

        let internal getDepth node =
            node.Depth

        let rec internal clone node =
            { Depth = node.Depth
              Bounds = node.Bounds
              Children = Array.map clone node.Children
              // NOTE: it is inefficient to shallow-clone a HashSet like this, but sadly, .NET does not provide a proper
              // Clone method! #ARGH!
              ElementsOpt = Option.map (fun elements -> HashSet (elements, HashIdentity.Structural)) node.ElementsOpt }

        let rec internal make<'e when 'e : equality> depth (bounds : Vector4) =
            if depth < 1 then failwith "Invalid depth for SpatialNode. Expected depth of at least 1."
            let children =
                if depth > 1 then 
                    [|for i in 0 .. 15 do
                        let childDepth = depth - 1
                        let childSize = Vector2 (bounds.Z - bounds.X, bounds.W - bounds.Y) * 0.25f
                        let childPosition = bounds.Xy + Vector2 (childSize.X * single (i % 4), childSize.Y * single (i / 4))
                        let childBounds = Vector4 (childPosition.X, childPosition.Y, childPosition.X + childSize.X, childPosition.Y + childSize.Y)
                        yield make<'e> childDepth childBounds|]
                else [||]
            { Depth = depth
              Bounds = bounds
              Children = (children : 'e SpatialNode array)
              ElementsOpt = match depth with 1 -> Some ^ HashSet HashIdentity.Structural | _ -> None }

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
            elif not ^ SpatialNode.tryAddElement bounds element tree.Node then
                Log.info "Element is outside of spatial tree's containment area or is being added redundantly."
                tree.OmnipresentElements.Add element |> ignore
    
        let removeElement omnipresence bounds element tree =
            if omnipresence then
                tree.OmnipresentElements.Remove element |> ignore
            elif not ^ SpatialNode.tryRemoveElement bounds element tree.Node then
                Log.info "Element is outside of spatial tree's containment area or is not present for removal."
                tree.OmnipresentElements.Remove element |> ignore
    
        let updateElement
            oldOmnipresence oldBounds
            newOmnipresence newBounds
            element tree =
            removeElement oldOmnipresence oldBounds element tree
            addElement newOmnipresence newBounds element tree
    
        let getElementsNearPoint position tree =
            let otherElements = SpatialNode.getElementsNearPoint position tree.Node
            otherElements |> Seq.append tree.OmnipresentElements |> Seq.distinct |> List<'e>
    
        let getElementsNearBounds bounds tree =
            let otherElements = SpatialNode.getElementsNearBounds bounds tree.Node
            otherElements |> Seq.append tree.OmnipresentElements |> Seq.distinct |> List<'e>
    
        let getDepth tree =
            SpatialNode.getDepth tree.Node
    
        let clone tree =
            { Node = SpatialNode.clone tree.Node
              OmnipresentElements = HashSet (tree.OmnipresentElements, HashIdentity.Structural) }
    
        let make<'e when 'e : equality> depth bounds =
            { Node = SpatialNode.make<'e> depth bounds
              OmnipresentElements = HashSet HashIdentity.Structural }

/// A spatial structure that organizes elements on a 2D plane.
type SpatialTree<'e when 'e : equality> = SpatialTreeModule.SpatialTree<'e>