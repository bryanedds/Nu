// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections
open System.Collections.Generic
open System.Numerics
open Prime

/// Masks for Octelement flags.
module QuadelementMasks =

    // OPTIMIZATION: Octelement flag bit-masks for performance.
    let [<Literal>] VisibleMask =   0b00000001u

// NOTE: opening this in order to make the Quadelement property implementations reasonably succinct.
open QuadelementMasks

/// An element in an quadtree.
type [<CustomEquality; NoComparison>] Quadelement<'e when 'e : equality> = 
    { HashCode : int // OPTIMIZATION: cache hash code to increase look-up speed.
      Flags : uint
      Entry : 'e }
    member this.Visible = this.Flags &&& VisibleMask <> 0u
    override this.GetHashCode () = this.HashCode
    override this.Equals that = match that with :? Quadelement<'e> as that -> this.Entry.Equals that.Entry | _ -> false
    static member make visible (entry : 'e) =
        let hashCode = entry.GetHashCode ()
        let flags = if visible then VisibleMask else 0u
        { HashCode = hashCode; Flags = flags; Entry = entry }

[<RequireQualifiedAccess>]
module internal Quadnode =

    type [<ReferenceEquality>] Quadnode<'e when 'e : equality> =
        private
            { Depth : int
              Bounds : Box2
              Children : ValueEither<'e Quadnode array, 'e Quadelement HashSet> }

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

    let rec internal getElementsAtPoint point node (set : 'e Quadelement HashSet) =
        match node.Children with
        | ValueLeft nodes -> for node in nodes do if atPoint point node then getElementsAtPoint point node set
        | ValueRight elements -> for element in elements do set.Add element |> ignore

    let rec internal getElementsInBounds unfiltered bounds node (set : 'e Quadelement HashSet) =
        match node.Children with
        | ValueLeft nodes -> for node in nodes do if isIntersectingBounds bounds node then getElementsInBounds unfiltered bounds node set
        | ValueRight elements ->
            for element in elements do
                if element.Visible || unfiltered then
                    set.Add element |> ignore

    let rec internal make<'e when 'e : equality> granularity depth (bounds : Box2) =
        if granularity < 2 then failwith "Invalid granularity for Quadnode. Expected value of at least 2."
        if depth < 1 then failwith "Invalid depth for Quadnode. Expected value of at least 1."
        let children =
            if depth > 1 then
                let (nodes : 'e Quadnode array) =
                    [|for i in 0 .. granularity * granularity - 1 do
                        let childDepth = depth - 1
                        let childSize = v2 bounds.Size.X bounds.Size.Y / single granularity
                        let childPosition = v2 bounds.Min.X bounds.Min.Y + v2 (childSize.X * single (i % granularity)) (childSize.Y * single (i / granularity))
                        let childBounds = box2 childPosition childSize
                        yield make granularity childDepth childBounds|]
                ValueLeft nodes
            else ValueRight (HashSet<'e Quadelement> HashIdentity.Structural)
        { Depth = depth
          Bounds = bounds
          Children = children }

type internal Quadnode<'e when 'e : equality> = Quadnode.Quadnode<'e>

[<RequireQualifiedAccess>]
module Quadtree =

    /// Provides an enumerator interface to the quadtree queries.
    type internal QuadtreeEnumerator<'e when 'e : equality> (uncullable : 'e Quadelement seq, cullable : 'e Quadelement seq) =

        let uncullableArray = SegmentedArray.ofSeq uncullable // eagerly convert to segmented array to keep iteration valid
        let cullableArray = SegmentedArray.ofSeq cullable // eagerly convert to segmented array to keep iteration valid
        let mutable cullableEnrValid = false
        let mutable uncullableEnrValid = false
        let mutable cullableEnr = Unchecked.defaultof<_>
        let mutable uncullableEnr = Unchecked.defaultof<_>

        interface 'e Quadelement IEnumerator with
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
                (this :> 'e Quadelement IEnumerator).Current :> obj

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
        interface IEnumerable<'e Quadelement> with
            member this.GetEnumerator () = enr :> 'e Quadelement IEnumerator
            member this.GetEnumerator () = enr :> IEnumerator

    /// A spatial structure that organizes elements on a 2d plane. TODO: document this.
    type [<ReferenceEquality>] Quadtree<'e when 'e : equality> =
        private
            { Node : 'e Quadnode
              Omnipresent : 'e Quadelement HashSet
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
        new QuadtreeEnumerable<'e> (new QuadtreeEnumerator<'e> (tree.Omnipresent, set)) :> 'e Quadelement IEnumerable

    let getElementsAtPoint point set tree =
        Quadnode.getElementsAtPoint point tree.Node set
        new QuadtreeEnumerable<'e> (new QuadtreeEnumerator<'e> (tree.Omnipresent, set)) :> 'e Quadelement IEnumerable

    let getElementsInBounds bounds set tree =
        Quadnode.getElementsInBounds true bounds tree.Node set
        new QuadtreeEnumerable<'e> (new QuadtreeEnumerator<'e> (tree.Omnipresent, set)) :> 'e Quadelement IEnumerable

    let getElementsInView bounds set tree =
        Quadnode.getElementsInBounds false bounds tree.Node set
        let omnipresent = tree.Omnipresent |> Seq.filter (fun element -> element.Visible)
        new QuadtreeEnumerable<'e> (new QuadtreeEnumerator<'e> (omnipresent, set)) :> 'e Quadelement IEnumerable

    let getElementsInPlay bounds set tree =
        Quadnode.getElementsInBounds true bounds tree.Node set
        new QuadtreeEnumerable<'e> (new QuadtreeEnumerator<'e> (tree.Omnipresent, set)) :> 'e Quadelement IEnumerable

    let getDepth tree =
        tree.Depth

    let make<'e when 'e : equality> granularity depth bounds =
        { Node = Quadnode.make<'e> granularity depth bounds
          Omnipresent = HashSet HashIdentity.Structural
          Depth = depth
          Granularity = granularity
          Bounds = bounds }

/// A spatial structure that organizes elements on a 2d plane. TODO: document this.
type Quadtree<'e when 'e : equality> = Quadtree.Quadtree<'e>