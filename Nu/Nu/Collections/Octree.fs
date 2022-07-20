// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections
open System.Collections.Generic
open System.Numerics
open Prime

/// Masks for Octelement flags.
module OctelementMasks =

    // OPTIMIZATION: Octelement flag bit-masks for performance.
    let [<Literal>] StaticMask =    0b00000001u
    let [<Literal>] LightMask =     0b00000010u

// NOTE: opening this in order to make the Octelement property implementations reasonably succinct.
open OctelementMasks

/// An element in an octree.
type [<CustomEquality; NoComparison>] Octelement<'e when 'e : equality> = 
    { HashCode : int // OPTIMIZATION: cache hash code to increase look-up speed.
      Flags : uint
      Presence : Presence
      Entry : 'e }
    member this.Static with get () = this.Flags &&& StaticMask <> 0u
    member this.Light with get () = this.Flags &&& LightMask <> 0u
    member this.Enclosed with get () = this.Presence.EnclosedType
    member this.Exposed with get () = this.Presence.ExposedType
    member this.Imposter with get () = this.Presence.ImposterType
    member this.Prominent with get () = this.Presence.ProminentType
    member this.Omnipresent with get () = this.Presence.OmnipresentType
    member this.Uncullable with get () = this.Presence.Uncullable
    override this.GetHashCode () = this.HashCode
    override this.Equals that = match that with :? Octelement<'e> as that -> this.Entry.Equals that.Entry | _ -> false
    static member make static_ light presence (entry : 'e) =
        let hashCode = entry.GetHashCode ()
        let flags =
            (if static_ then StaticMask else 0u) |||
            (if light then LightMask else 0u)
        { HashCode = hashCode; Flags = flags; Presence = presence; Entry = entry }

[<RequireQualifiedAccess>]
module internal Octnode =

    type [<NoEquality; NoComparison>] Octnode<'e when 'e : equality> =
        private
            { Depth : int
              Bounds : Box3
              Children : ValueEither<'e Octnode array, 'e Octelement HashSet> }

    let internal atPoint (point : Vector3) node =
        node.Bounds.Intersects point

    let internal isIntersectingBox (bounds : Box3) node =
        node.Bounds.Intersects bounds

    let inline internal isIntersectingFrustum (frustum : Frustum) node =
        frustum.Intersects node.Bounds

    let rec internal addElement bounds element node =
        if isIntersectingBox bounds node then
            match node.Children with
            | ValueLeft nodes -> for node in nodes do addElement bounds element node
            | ValueRight elements ->
                elements.Remove element |> ignore
                elements.Add element |> ignore

    let rec internal removeElement bounds element node =
        if isIntersectingBox bounds node then
            match node.Children with
            | ValueLeft nodes -> for node in nodes do removeElement bounds element node
            | ValueRight elements -> elements.Remove element |> ignore

    let rec internal updateElement oldBounds newBounds element node =
        match node.Children with
        | ValueLeft nodes ->
            for node in nodes do
                if isIntersectingBox oldBounds node || isIntersectingBox newBounds node then
                    updateElement oldBounds newBounds element node
        | ValueRight elements ->
            if isIntersectingBox newBounds node then
                elements.Remove element |> ignore
                elements.Add element |> ignore
            elif isIntersectingBox oldBounds node then
                elements.Remove element |> ignore

    let rec internal getElementsAtPoint point node (set : 'e Octelement HashSet) =
        match node.Children with
        | ValueLeft nodes -> for node in nodes do if atPoint point node then getElementsAtPoint point node set
        | ValueRight elements -> for element in elements do set.Add element |> ignore

    let rec internal getElementsInBox box node (set : 'e Octelement HashSet) =
        match node.Children with
        | ValueLeft nodes -> for node in nodes do if isIntersectingBox box node then getElementsInBox box node set
        | ValueRight elements -> for element in elements do set.Add element |> ignore

    let rec internal getElementsInFrustum frustum node (set : 'e Octelement HashSet) =
        match node.Children with
        | ValueLeft nodes -> for node in nodes do if isIntersectingFrustum frustum node then getElementsInFrustum frustum node set
        | ValueRight elements -> for element in elements do set.Add element |> ignore

    let rec internal getUpdatingElementsInBox box node (set : 'e Octelement HashSet) =
        match node.Children with
        | ValueLeft nodes ->
            for node in nodes do
                if isIntersectingBox box node then
                    getUpdatingElementsInBox box node set
        | ValueRight elements ->
            for element in elements do
                if not element.Static then
                    set.Add element |> ignore

    let rec internal getLightsInBox box node (set : 'e Octelement HashSet) =
        match node.Children with
        | ValueLeft nodes ->
            for node in nodes do
                if isIntersectingBox box node then
                    getLightsInBox box node set
        | ValueRight elements ->
            for element in elements do
                if element.Light then
                    set.Add element |> ignore

    let rec internal getElementsInPlayFrustum frustum node (set : 'e Octelement HashSet) =
        match node.Children with
        | ValueLeft nodes ->
            for node in nodes do
                if isIntersectingFrustum frustum node then
                    getElementsInPlayFrustum frustum node set
        | ValueRight elements ->
            for element in elements do
                if not element.Static then
                    set.Add element |> ignore

    let rec internal getElementsInViewFrustum enclosed exposed imposter frustum node (set : 'e Octelement HashSet) =
        match node.Children with
        | ValueLeft nodes ->
            for node in nodes do
                if isIntersectingFrustum frustum node then
                    getElementsInViewFrustum enclosed exposed imposter frustum node set
        | ValueRight elements ->
            for element in elements do
                if element.Enclosed then
                    if enclosed then
                        set.Add element |> ignore
                elif element.Exposed || element.Prominent then
                    set.Add element |> ignore
                elif element.Imposter then
                    set.Add element |> ignore

    let rec internal getElementsInView frustumEnclosed frustumExposed frustumImposter lightBox node (set : 'e Octelement HashSet) =
        match node.Children with
        | ValueLeft nodes ->
            for node in nodes do
                let intersectingEnclosed = isIntersectingFrustum frustumEnclosed node
                let intersectingExposed = isIntersectingFrustum frustumExposed node
                if intersectingEnclosed || intersectingExposed then
                    if intersectingEnclosed then getElementsInViewFrustum true false false frustumEnclosed node set
                    if intersectingExposed then getElementsInViewFrustum false true false frustumExposed node set
                elif isIntersectingFrustum frustumImposter node then
                    getElementsInViewFrustum false false true frustumImposter node set
                if isIntersectingBox lightBox node then
                    getLightsInBox lightBox node set
        | ValueRight _ -> ()

    let rec internal getElementsInPlay playBox playFrustum node (set : 'e Octelement HashSet) =
        match node.Children with
        | ValueLeft nodes ->
            for node in nodes do
                if isIntersectingBox playBox node then
                    getUpdatingElementsInBox playBox node set
                if isIntersectingFrustum playFrustum node then
                    getElementsInPlayFrustum playFrustum node set
        | ValueRight _ -> ()

    let rec internal clone node =
        { Depth = node.Depth
          Bounds = node.Bounds
          Children =
            match node.Children with
            | ValueRight elements -> ValueRight (HashSet (elements, HashIdentity.Structural))
            | ValueLeft nodes -> ValueLeft (Array.map clone nodes) }

    let rec internal make<'e when 'e : equality> granularity depth (bounds : Box3) : 'e Octnode =
        if granularity < 2 then failwith "Invalid granularity for Octnode. Expected value of at least 2."
        if depth < 1 then failwith "Invalid depth for Octnode. Expected value of at least 1."
        let childDepth = depth - 1
        let childSize = bounds.Size / single granularity
        let children =
            if depth > 1 then
                let nodes =
                    [|for i in 0 .. granularity - 1 do
                        [|for j in 0 .. granularity - 1 do
                            [|for k in 0 .. granularity - 1 do
                                let childOffset = v3 (childSize.X * single i) (childSize.Y * single j) (childSize.Z * single k)
                                let childPosition = bounds.Position + childOffset
                                let childBounds = box3 childPosition childSize
                                yield make granularity childDepth childBounds|]|]|]
                ValueLeft (nodes |> Array.concat |> Array.concat)
            else ValueRight (HashSet<'e Octelement> HashIdentity.Structural)
        { Depth = depth
          Bounds = bounds
          Children = children }

type internal Octnode<'e when 'e : equality> = Octnode.Octnode<'e>

[<RequireQualifiedAccess>]
module Octree =

    /// Provides an enumerator interface to the octree queries.
    /// TODO: see if we can make this enumerator work when its results are evaluated multiple times in the debugger.
    type internal OctreeEnumerator<'e when 'e : equality> (uncullable : 'e Octelement seq, cullable : 'e Octelement seq) =

        let uncullableArray = SegmentedArray.ofSeq uncullable // eagerly convert to segmented array to keep iteration valid
        let cullableArray = SegmentedArray.ofSeq cullable // eagerly convert to segmented array to keep iteration valid
        let mutable cullableEnrValid = false
        let mutable uncullableEnrValid = false
        let mutable cullableEnr = Unchecked.defaultof<_>
        let mutable uncullableEnr = Unchecked.defaultof<_>

        interface Octelement<'e> IEnumerator with
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
                (this :> 'e Octelement IEnumerator).Current :> obj

            member this.Reset () =
                cullableEnrValid <- false
                uncullableEnrValid <- false
                cullableEnr <- Unchecked.defaultof<_>
                uncullableEnr <- Unchecked.defaultof<_>

            member this.Dispose () =
                cullableEnr <- Unchecked.defaultof<_>
                uncullableEnr <- Unchecked.defaultof<_>

    /// Provides an enumerable interface to the octree queries.
    type internal OctreeEnumerable<'e when 'e : equality> (enr : 'e OctreeEnumerator) =
        interface IEnumerable<'e Octelement> with
            member this.GetEnumerator () = enr :> 'e Octelement IEnumerator
            member this.GetEnumerator () = enr :> IEnumerator

    /// A spatial structure that organizes elements in a 3d grid.
    type [<NoEquality; NoComparison>] Octree<'e when 'e : equality> =
        private
            { Node : 'e Octnode
              Uncullable : 'e Octelement HashSet
              Depth : int
              Granularity : int
              Bounds : Box3 }

    let addElement bounds element tree =
        if element.Presence.Uncullable then
            tree.Uncullable.Remove element |> ignore
            tree.Uncullable.Add element |> ignore
        else
            if not (Octnode.isIntersectingBox bounds tree.Node) then
                Log.info "Element is outside the octree's containment area or is being added redundantly."
                tree.Uncullable.Remove element |> ignore
                tree.Uncullable.Add element |> ignore
            else Octnode.addElement bounds element tree.Node

    let removeElement bounds element tree =
        if element.Presence.Uncullable then 
            tree.Uncullable.Remove element |> ignore
        else
            if not (Octnode.isIntersectingBox bounds tree.Node) then
                Log.info "Element is outside the octree's containment area or is not present for removal."
                tree.Uncullable.Remove element |> ignore
            else Octnode.removeElement bounds element tree.Node

    let updateElement (oldPresence : Presence) oldBounds (newPresence : Presence) newBounds element tree =
        let wasInNode = oldPresence.Cullable && Octnode.isIntersectingBox oldBounds tree.Node
        let isInNode = newPresence.Cullable && Octnode.isIntersectingBox newBounds tree.Node
        if wasInNode then
            if isInNode then
                Octnode.updateElement oldBounds newBounds element tree.Node
            else
                Octnode.removeElement oldBounds element tree.Node |> ignore
                tree.Uncullable.Remove element |> ignore
                tree.Uncullable.Add element |> ignore
        else
            if isInNode then
                tree.Uncullable.Remove element |> ignore
                Octnode.addElement newBounds element tree.Node
            else
                tree.Uncullable.Remove element |> ignore
                tree.Uncullable.Add element |> ignore

    let getElementsUncullable (set : _ HashSet) tree =
        new OctreeEnumerable<'e> (new OctreeEnumerator<'e> (tree.Uncullable, set)) :> 'e Octelement IEnumerable

    let getElementsAtPoint point (set : _ HashSet) tree =
        Octnode.getElementsAtPoint point tree.Node set
        new OctreeEnumerable<'e> (new OctreeEnumerator<'e> (tree.Uncullable, set)) :> 'e Octelement IEnumerable

    let getElementsInBounds bounds (set : _ HashSet) tree =
        Octnode.getElementsInBox bounds tree.Node set
        new OctreeEnumerable<'e> (new OctreeEnumerator<'e> (tree.Uncullable, set)) :> 'e Octelement IEnumerable

    let getElementsInFrustum frustum (set : _ HashSet) tree =
        Octnode.getElementsInFrustum frustum tree.Node set
        new OctreeEnumerable<'e> (new OctreeEnumerator<'e> (tree.Uncullable, set)) :> 'e Octelement IEnumerable

    let getElementsInView frustumEnclosed frustumExposed frustumImposter lightBox (set : _ HashSet) tree =
        Octnode.getElementsInView frustumEnclosed frustumExposed frustumImposter lightBox tree.Node set
        new OctreeEnumerable<'e> (new OctreeEnumerator<'e> (tree.Uncullable, set)) :> 'e Octelement IEnumerable

    let getElementsInPlay playBox playFrustum (set : _ HashSet) tree =
        Octnode.getElementsInPlay playBox playFrustum tree.Node set
        new OctreeEnumerable<'e> (new OctreeEnumerator<'e> (tree.Uncullable, set)) :> 'e Octelement IEnumerable

    let getDepth tree =
        tree.Depth

    let clone tree =
        { Node = Octnode.clone tree.Node
          Uncullable = HashSet (tree.Uncullable, HashIdentity.Structural)
          Depth = tree.Depth
          Granularity = tree.Granularity
          Bounds = tree.Bounds }

    let make<'e when 'e : equality> granularity depth bounds =
        { Node = Octnode.make<'e> granularity depth bounds
          Uncullable = HashSet HashIdentity.Structural
          Depth = depth
          Granularity = granularity
          Bounds = bounds }

/// A spatial structure that organizes elements in a 3d grid.
type Octree<'e when 'e : equality> = Octree.Octree<'e>