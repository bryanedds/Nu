// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections
open System.Collections.Generic
open Prime

/// Describes the form of an element's presence.
type [<StructuralEquality; NoComparison; Struct>] Presence =
    | Enclosed
    | Unenclosed
    | Afatecs // TODO: 3D: can we think of a better name here?
    | Omnipresent
    member this.ISEnclosed with get () = match this with Enclosed -> true | _ -> false // TODO: 3D: come up with better getter names.
    member this.ISUnenclosed with get () = match this with Unenclosed -> true | _ -> false
    member this.ISAfatecs with get () = match this with Afatecs -> true | _ -> false
    member this.ISOmnipresent with get () = match this with Omnipresent -> true | _ -> false

[<RequireQualifiedAccess>]
module internal Quadnode =

    type [<NoEquality; NoComparison>] Quadnode<'e when 'e : equality> =
        private
            { Depth : int
              Bounds : Box2
              Children : ValueEither<'e Quadnode array, 'e HashSet> }

    let internal atPoint point node =
        Math.isPointInBounds2d point node.Bounds

    let internal isIntersectingBounds bounds node =
        Math.isBoundsIntersectingBounds2d bounds node.Bounds

    let rec internal addElement bounds element node =
        if isIntersectingBounds bounds node then
            match node.Children with
            | ValueLeft nodes -> for node in nodes do addElement bounds element node
            | ValueRight elements -> elements.Add element |> ignore

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
            if isIntersectingBounds oldBounds node then
                if not (isIntersectingBounds newBounds node) then elements.Remove element |> ignore
            elif isIntersectingBounds newBounds node then
                elements.Add element |> ignore

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
    type internal QuadtreeEnumerator<'e when 'e : equality> (omnipresentElements : 'e HashSet, localElements : 'e HashSet) =

        let omnipresentArray = SegmentedArray.ofSeq omnipresentElements // eagerly convert to segmented array to keep iteration valid
        let localArray = SegmentedArray.ofSeq localElements // eagerly convert to segmented array to keep iteration valid
        let mutable localEnrValid = false
        let mutable omnipresentEnrValid = false
        let mutable localEnr = Unchecked.defaultof<_>
        let mutable omnipresentEnr = Unchecked.defaultof<_>

        interface 'e IEnumerator with
            member this.MoveNext () =
                if not localEnrValid then
                    localEnr <- localArray.GetEnumerator ()
                    localEnrValid <- true
                    if not (localEnr.MoveNext ()) then
                        omnipresentEnr <- omnipresentArray.GetEnumerator ()
                        omnipresentEnrValid <- true
                        omnipresentEnr.MoveNext ()
                    else true
                else
                    if not (localEnr.MoveNext ()) then
                        if not omnipresentEnrValid then
                            omnipresentEnr <- omnipresentArray.GetEnumerator ()
                            omnipresentEnrValid <- true
                            omnipresentEnr.MoveNext ()
                        else omnipresentEnr.MoveNext ()
                    else true

            member this.Current =
                if omnipresentEnrValid then omnipresentEnr.Current
                elif localEnrValid then localEnr.Current
                else failwithumf ()

            member this.Current =
                (this :> 'e IEnumerator).Current :> obj

            member this.Reset () =
                localEnrValid <- false
                omnipresentEnrValid <- false
                localEnr <- Unchecked.defaultof<_>
                omnipresentEnr <- Unchecked.defaultof<_>

            member this.Dispose () =
                localEnr <- Unchecked.defaultof<_>
                omnipresentEnr <- Unchecked.defaultof<_>
            
    /// Provides an enumerable interface to the quadtree queries.
    type internal QuadtreeEnumerable<'e when 'e : equality> (enr : 'e QuadtreeEnumerator) =
        interface IEnumerable<'e> with
            member this.GetEnumerator () = enr :> 'e IEnumerator
            member this.GetEnumerator () = enr :> IEnumerator

    /// A spatial structure that organizes elements on a 2d plane. TODO: document this.
    type [<NoEquality; NoComparison>] Quadtree<'e when 'e : equality> =
        private
            { Node : 'e Quadnode
              OmnipresentElements : 'e HashSet
              Depth : int
              Granularity : int
              Bounds : Box2 }

    let addElement (presence : Presence) bounds element tree =
        if presence.ISOmnipresent then
            tree.OmnipresentElements.Add element |> ignore
        else
            if not (Quadnode.isIntersectingBounds bounds tree.Node) then
                Log.info "Element is outside the quadtree's containment area or is being added redundantly."
                tree.OmnipresentElements.Add element |> ignore
            else Quadnode.addElement bounds element tree.Node

    let removeElement (presence : Presence) bounds element tree =
        if presence.ISOmnipresent then 
            tree.OmnipresentElements.Remove element |> ignore
        else
            if not (Quadnode.isIntersectingBounds bounds tree.Node) then
                Log.info "Element is outside the quadtree's containment area or is not present for removal."
                tree.OmnipresentElements.Remove element |> ignore
            else Quadnode.removeElement bounds element tree.Node

    let updateElement oldBounds newBounds element tree =
        let oldInBounds = Quadnode.isIntersectingBounds oldBounds tree.Node
        let newInBounds = Quadnode.isIntersectingBounds newBounds tree.Node
        if oldInBounds && not newInBounds then
            // going out of bounds
            Log.info "Element is outside the quadtree's containment area."
            if not newInBounds then tree.OmnipresentElements.Add element |> ignore
            Quadnode.updateElement oldBounds newBounds element tree.Node
        elif not oldInBounds && newInBounds then
            // going back in bounds
            if not oldInBounds then tree.OmnipresentElements.Remove element |> ignore
            Quadnode.updateElement oldBounds newBounds element tree.Node
        elif oldInBounds && newInBounds then
            // staying in bounds
            let rootBounds = tree.Bounds
            let rootDepth = pown tree.Granularity tree.Depth
            let leafSize = rootBounds.Size / single rootDepth
            let leafPosition =
                v2
                    (oldBounds.Position.X - (rootBounds.Position.X + oldBounds.Position.X) % leafSize.X)
                    (oldBounds.Position.Y - (rootBounds.Position.Y + oldBounds.Position.Y) % leafSize.Y)
            let leafBounds = box2 leafPosition leafSize
            if  not (Math.isBoundsInBounds2d oldBounds leafBounds) ||
                not (Math.isBoundsInBounds2d newBounds leafBounds) then
                Quadnode.updateElement oldBounds newBounds element tree.Node
        else
            // staying out of bounds
            ()

    let getElementsOmnipresent set tree =
        new QuadtreeEnumerable<'e> (new QuadtreeEnumerator<'e> (tree.OmnipresentElements, set)) :> 'e IEnumerable

    let getElementsAtPoint point set tree =
        Quadnode.getElementsAtPoint point tree.Node set
        new QuadtreeEnumerable<'e> (new QuadtreeEnumerator<'e> (tree.OmnipresentElements, set)) :> 'e IEnumerable

    let getElementsInBounds bounds set tree =
        Quadnode.getElementsInBounds bounds tree.Node set
        new QuadtreeEnumerable<'e> (new QuadtreeEnumerator<'e> (tree.OmnipresentElements, set)) :> 'e IEnumerable

    let getDepth tree =
        tree.Depth

    let clone tree =
        { Node = Quadnode.clone tree.Node
          OmnipresentElements = HashSet (tree.OmnipresentElements, HashIdentity.Structural)
          Depth = tree.Depth
          Granularity = tree.Granularity
          Bounds = tree.Bounds }

    let make<'e when 'e : equality> granularity depth bounds =
        { Node = Quadnode.make<'e> granularity depth bounds
          OmnipresentElements = HashSet HashIdentity.Structural
          Depth = depth
          Granularity = granularity
          Bounds = bounds }
          
/// A spatial structure that organizes elements on a 2d plane. TODO: document this.
type Quadtree<'e when 'e : equality> = Quadtree.Quadtree<'e>