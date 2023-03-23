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

    type [<ReferenceEquality>] Octnode<'e when 'e : equality> =
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

    let inline internal containsBox (bounds : Box3) node =
        node.Bounds.Combine bounds = node.Bounds

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

    let rec internal getElementsInPlayBox box node (set : 'e Octelement HashSet) =
        match node.Children with
        | ValueLeft nodes ->
            for node in nodes do
                if isIntersectingBox box node then
                    getElementsInPlayBox box node set
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
                if enclosed then
                    if element.Enclosed || element.Exposed || element.Prominent then
                        set.Add element |> ignore
                elif exposed then
                    if element.Exposed || element.Prominent then
                        set.Add element |> ignore
                elif imposter then
                    if element.Imposter || element.Prominent then
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
                    getElementsInPlayBox playBox node set
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

    let rec internal make<'e when 'e : equality> depth (bounds : Box3) (leaves : Dictionary<Vector3, 'e Octnode>) : 'e Octnode =
        if depth < 1 then failwith "Invalid depth for Octnode. Expected value of at least 1."
        let granularity = 2
        let childDepth = depth - 1
        let childSize = bounds.Size / single granularity
        let children =
            if depth > 1 then
                let nodes =
                    [|for i in 0 .. dec granularity do
                        [|for j in 0 .. dec granularity do
                            [|for k in 0 .. dec granularity do
                                let childOffset = v3 (childSize.X * single i) (childSize.Y * single j) (childSize.Z * single k)
                                let childMin = bounds.Min + childOffset
                                let childBounds = box3 childMin childSize
                                yield make childDepth childBounds leaves|]|]|]
                ValueLeft (nodes |> Array.concat |> Array.concat)
            else ValueRight (HashSet<'e Octelement> HashIdentity.Structural)
        let node =
            { Depth = depth
              Bounds = bounds
              Children = children }
        if depth = 1 then leaves.Add (bounds.Min, node)
        node

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
    type [<ReferenceEquality>] Octree<'e when 'e : equality> =
        private
            { Leaves : Dictionary<Vector3, 'e Octnode>
              LeafSize : Vector3
              Node : 'e Octnode
              Omnipresent : 'e Octelement HashSet
              Depth : int
              Bounds : Box3 }

    let private findNode (bounds : Box3) tree =
        let offset = -tree.Bounds.Min // use offset to bring div ops into positive space
        let divs = (bounds.Min + offset) / tree.LeafSize
        let evens = v3 (divs.X |> int |> single) (divs.Y |> int |> single) (divs.Z |> int |> single)
        let leafKey = evens * tree.LeafSize - offset
        match tree.Leaves.TryGetValue leafKey with
        | (true, leaf) when Octnode.containsBox bounds leaf -> leaf
        | (_, _) -> tree.Node

    let addElement bounds element tree =
        if element.Presence.OmnipresentType then
            tree.Omnipresent.Remove element |> ignore
            tree.Omnipresent.Add element |> ignore
        else
            if not (Octnode.isIntersectingBox bounds tree.Node) then
                Log.info "Element is outside the octree's containment area or is being added redundantly."
                tree.Omnipresent.Remove element |> ignore
                tree.Omnipresent.Add element |> ignore
            else
                let node = findNode bounds tree
                Octnode.addElement bounds element node

    let removeElement bounds element tree =
        if element.Presence.OmnipresentType then 
            tree.Omnipresent.Remove element |> ignore
        else
            if not (Octnode.isIntersectingBox bounds tree.Node) then
                Log.info "Element is outside the octree's containment area or is not present for removal."
                tree.Omnipresent.Remove element |> ignore
            else
                let node = findNode bounds tree
                Octnode.removeElement bounds element node

    let updateElement (oldPresence : Presence) oldBounds (newPresence : Presence) newBounds element tree =
        let wasInNode = not oldPresence.OmnipresentType && Octnode.isIntersectingBox oldBounds tree.Node
        if wasInNode then
            let oldNode = findNode oldBounds tree
            Octnode.removeElement oldBounds element oldNode |> ignore
        else tree.Omnipresent.Remove element |> ignore
        let isInNode = not newPresence.OmnipresentType && Octnode.isIntersectingBox newBounds tree.Node
        if isInNode then 
            let newNode = findNode oldBounds tree
            Octnode.addElement newBounds element newNode
        else tree.Omnipresent.Add element |> ignore

    let getElementsOmnipresent (set : _ HashSet) tree =
        new OctreeEnumerable<'e> (new OctreeEnumerator<'e> (tree.Omnipresent, set)) :> 'e Octelement IEnumerable

    let getElementsAtPoint point (set : _ HashSet) tree =
        let node = findNode (box3 point v3Zero) tree
        Octnode.getElementsAtPoint point node set
        new OctreeEnumerable<'e> (new OctreeEnumerator<'e> (tree.Omnipresent, set)) :> 'e Octelement IEnumerable

    let getElementsInBounds bounds (set : _ HashSet) tree =
        Octnode.getElementsInBox bounds tree.Node set
        new OctreeEnumerable<'e> (new OctreeEnumerator<'e> (tree.Omnipresent, set)) :> 'e Octelement IEnumerable

    let getElementsInFrustum frustum (set : _ HashSet) tree =
        Octnode.getElementsInFrustum frustum tree.Node set
        new OctreeEnumerable<'e> (new OctreeEnumerator<'e> (tree.Omnipresent, set)) :> 'e Octelement IEnumerable

    let getElementsInView frustumEnclosed frustumExposed frustumImposter lightBox (set : _ HashSet) tree =
        Octnode.getElementsInView frustumEnclosed frustumExposed frustumImposter lightBox tree.Node set
        new OctreeEnumerable<'e> (new OctreeEnumerator<'e> (tree.Omnipresent, set)) :> 'e Octelement IEnumerable

    let getElementsInPlay playBox playFrustum (set : _ HashSet) tree =
        Octnode.getElementsInPlay playBox playFrustum tree.Node set
        new OctreeEnumerable<'e> (new OctreeEnumerator<'e> (tree.Omnipresent, set)) :> 'e Octelement IEnumerable

    let getDepth tree =
        tree.Depth

    let make<'e when 'e : equality> (depth : int) (bounds : Box3) =
        let leaves = dictPlus HashIdentity.Structural []
        let mutable leafSize = bounds.Size
        for _ in 1 .. dec depth do leafSize <- leafSize * 0.5f
        { Leaves = leaves
          LeafSize = leafSize
          Node = Octnode.make<'e> depth bounds leaves
          Omnipresent = HashSet HashIdentity.Structural
          Depth = depth
          Bounds = bounds }

/// A spatial structure that organizes elements in a 3d grid.
type Octree<'e when 'e : equality> = Octree.Octree<'e>