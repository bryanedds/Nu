// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections
open System.Collections.Generic
open System.Numerics
open Prime

[<RequireQualifiedAccess>]
module internal Quadnode =

    type [<ReferenceEquality; NoComparison>] Quadnode<'e when 'e : equality> =
        private
            { Depth : int
              Bounds : Box2
              Children : ValueEither<'e Quadnode array, 'e HashSet> }

    let internal atPoint (point : Vector2) node =
        node.Bounds.Intersects point

    let internal isIntersectingBounds (bounds : Box2) node =
        node.Bounds.Intersects bounds

    let rec internal addElement bounds element node =
        if isIntersectingBounds bounds node then
            match node.Children with
            | ValueLeft nodes -> for node in nodes do addElement bounds element node
            | ValueRight elements ->
                elements.Remove element |> ignore
                elements.Add element |> ignore

    let rec internal removeElement bounds element node =
        if isIntersectingBounds bounds node then
            match node.Children with
            | ValueLeft nodes -> for node in nodes do removeElement bounds element node
            | ValueRight elements -> elements.Remove element |> ignore

    let rec internal updateElement oldBounds newBounds element node =
        match node.Children with
        | ValueLeft nodes ->
            for node in nodes do
                if isIntersectingBounds oldBounds node || isIntersectingBounds newBounds node then
                    updateElement oldBounds newBounds element node
        | ValueRight elements ->
            if isIntersectingBounds newBounds node then
                elements.Remove element |> ignore
                elements.Add element |> ignore
            elif isIntersectingBounds oldBounds node then
                elements.Remove element |> ignore

    let rec internal getElementsAtPoint point node (set : 'e HashSet) =
        match node.Children with
        | ValueLeft nodes -> for node in nodes do if atPoint point node then getElementsAtPoint point node set
        | ValueRight elements -> for element in elements do set.Add element |> ignore

    let rec internal getElementsInBounds bounds node (set : 'e HashSet) =
        match node.Children with
        | ValueLeft nodes -> for node in nodes do if isIntersectingBounds bounds node then getElementsInBounds bounds node set
        | ValueRight elements -> for element in elements do set.Add element |> ignore

    let rec internal clone node =
        { Depth = node.Depth
          Bounds = node.Bounds
          Children =
            match node.Children with
            | ValueRight elements -> ValueRight (HashSet (elements, HashIdentity.Structural))
            | ValueLeft nodes -> ValueLeft (Array.map clone nodes) }

    let rec internal make<'e when 'e : equality> granularity depth (bounds : Box2) =
        if granularity < 2 then failwith "Invalid granularity for Quadnode. Expected value of at least 2."
        if depth < 1 then failwith "Invalid depth for Quadnode. Expected value of at least 1."
        let children =
            if depth > 1 then
                let (nodes : 'e Quadnode array) =
                    [|for i in 0 .. granularity * granularity - 1 do
                        let childDepth = depth - 1
                        let childSize = v2 bounds.Size.X bounds.Size.Y / single granularity
                        let childPosition = v2 bounds.Position.X bounds.Position.Y + v2 (childSize.X * single (i % granularity)) (childSize.Y * single (i / granularity))
                        let childBounds = box2 childPosition childSize
                        yield make granularity childDepth childBounds|]
                ValueLeft nodes
            else ValueRight (HashSet<'e> HashIdentity.Structural)
        { Depth = depth
          Bounds = bounds
          Children = children }

type internal Quadnode<'e when 'e : equality> = Quadnode.Quadnode<'e>

[<RequireQualifiedAccess>]
module Quadtree =

    /// Provides an enumerator interface to the quadtree queries.
    type internal QuadtreeEnumerator<'e when 'e : equality> (uncullable : 'e HashSet, cullable : 'e HashSet) =

        let uncullableArray = SegmentedArray.ofSeq uncullable // eagerly convert to segmented array to keep iteration valid
        let cullableArray = SegmentedArray.ofSeq cullable // eagerly convert to segmented array to keep iteration valid
        let mutable cullableEnrValid = false
        let mutable uncullableEnrValid = false
        let mutable cullableEnr = Unchecked.defaultof<_>
        let mutable uncullableEnr = Unchecked.defaultof<_>

        interface 'e IEnumerator with
            member this.MoveNext () =
                if not cullableEnrValid then
                    cullableEnr <- cullableArray.GetEnumerator ()
                    cullableEnrValid <- true
                    if not (cullableEnr.MoveNext ()) then
                        uncullableEnr <- uncullableArray.GetEnumerator ()
                        uncullableEnrValid <- true
                        uncullableEnr.MoveNext ()
                    else true
                else
                    if not (cullableEnr.MoveNext ()) then
                        if not uncullableEnrValid then
                            uncullableEnr <- uncullableArray.GetEnumerator ()
                            uncullableEnrValid <- true
                            uncullableEnr.MoveNext ()
                        else uncullableEnr.MoveNext ()
                    else true

            member this.Current =
                if uncullableEnrValid then uncullableEnr.Current
                elif cullableEnrValid then cullableEnr.Current
                else failwithumf ()

            member this.Current =
                (this :> 'e IEnumerator).Current :> obj

            member this.Reset () =
                cullableEnrValid <- false
                uncullableEnrValid <- false
                cullableEnr <- Unchecked.defaultof<_>
                uncullableEnr <- Unchecked.defaultof<_>

            member this.Dispose () =
                cullableEnr <- Unchecked.defaultof<_>
                uncullableEnr <- Unchecked.defaultof<_>
            
    /// Provides an enumerable interface to the quadtree queries.
    type internal QuadtreeEnumerable<'e when 'e : equality> (enr : 'e QuadtreeEnumerator) =
        interface IEnumerable<'e> with
            member this.GetEnumerator () = enr :> 'e IEnumerator
            member this.GetEnumerator () = enr :> IEnumerator

    /// A spatial structure that organizes elements on a 2d plane. TODO: document this.
    type [<ReferenceEquality; NoComparison>] Quadtree<'e when 'e : equality> =
        private
            { Node : 'e Quadnode
              Omnipresent : 'e HashSet
              Depth : int
              Granularity : int
              Bounds : Box2 }

    let addElement (presence : Presence) bounds element tree =
        if presence.OmnipresentType then
            tree.Omnipresent.Remove element |> ignore
            tree.Omnipresent.Add element |> ignore
        else
            if not (Quadnode.isIntersectingBounds bounds tree.Node) then
                Log.info "Element is outside the quadtree's containment area or is being added redundantly."
                tree.Omnipresent.Remove element |> ignore
                tree.Omnipresent.Add element |> ignore
            else Quadnode.addElement bounds element tree.Node

    let removeElement (presence : Presence) bounds element tree =
        if presence.OmnipresentType then 
            tree.Omnipresent.Remove element |> ignore
        else
            if not (Quadnode.isIntersectingBounds bounds tree.Node) then
                Log.info "Element is outside the quadtree's containment area or is not present for removal."
                tree.Omnipresent.Remove element |> ignore
            else Quadnode.removeElement bounds element tree.Node

    let updateElement (oldPresence : Presence) oldBounds (newPresence : Presence) newBounds element tree =
        let wasInNode = not oldPresence.OmnipresentType && Quadnode.isIntersectingBounds oldBounds tree.Node
        let isInNode = not newPresence.OmnipresentType && Quadnode.isIntersectingBounds newBounds tree.Node
        if wasInNode then
            if isInNode then
                Quadnode.updateElement oldBounds newBounds element tree.Node
            else
                Quadnode.removeElement oldBounds element tree.Node |> ignore
                tree.Omnipresent.Remove element |> ignore
                tree.Omnipresent.Add element |> ignore
        else
            if isInNode then
                tree.Omnipresent.Remove element |> ignore
                Quadnode.addElement newBounds element tree.Node
            else
                tree.Omnipresent.Remove element |> ignore
                tree.Omnipresent.Add element |> ignore

    let getElementsOmnipresent set tree =
        new QuadtreeEnumerable<'e> (new QuadtreeEnumerator<'e> (tree.Omnipresent, set)) :> 'e IEnumerable

    let getElementsAtPoint point set tree =
        Quadnode.getElementsAtPoint point tree.Node set
        new QuadtreeEnumerable<'e> (new QuadtreeEnumerator<'e> (tree.Omnipresent, set)) :> 'e IEnumerable

    let getElementsInBounds bounds set tree =
        Quadnode.getElementsInBounds bounds tree.Node set
        new QuadtreeEnumerable<'e> (new QuadtreeEnumerator<'e> (tree.Omnipresent, set)) :> 'e IEnumerable

    let getDepth tree =
        tree.Depth

    let clone tree =
        { Node = Quadnode.clone tree.Node
          Omnipresent = HashSet (tree.Omnipresent, HashIdentity.Structural)
          Depth = tree.Depth
          Granularity = tree.Granularity
          Bounds = tree.Bounds }

    let make<'e when 'e : equality> granularity depth bounds =
        { Node = Quadnode.make<'e> granularity depth bounds
          Omnipresent = HashSet HashIdentity.Structural
          Depth = depth
          Granularity = granularity
          Bounds = bounds }

/// A spatial structure that organizes elements on a 2d plane. TODO: document this.
type Quadtree<'e when 'e : equality> = Quadtree.Quadtree<'e>