// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections
open System.Collections.Generic
open System.Numerics
open Prime

/// Masks for Octelement flags.
module OctelementMasks =

    // OPTIMIZATION: Octelement flag bit-masks for performance.
    let [<Literal>] VisibleMask =   0b00000001u
    let [<Literal>] StaticMask =    0b00000010u
    let [<Literal>] LightMask =     0b00000100u

// NOTE: opening this in order to make the Octelement property implementations reasonably succinct.
open OctelementMasks

[<RequireQualifiedAccess>]
module Octelement =

    /// An element in an octree.
    /// NOTE: we intentionally use incomplete equality semantics here so these can be stored in a HashSet.
    type [<CustomEquality; NoComparison; Struct>] Octelement<'e when 'e : equality> =
        private
            { HashCode_ : int // OPTIMIZATION: cache hash code to increase look-up speed.
              Flags_ : uint
              Presence_ : Presence
              Bounds_ : Box3
              Entry_ : 'e }
        member this.Visible = this.Flags_ &&& VisibleMask <> 0u
        member this.Static = this.Flags_ &&& StaticMask <> 0u
        member this.Light = this.Flags_ &&& LightMask <> 0u
        member this.Enclosed = this.Presence_.EnclosedType
        member this.Exposed = this.Presence_.ExposedType
        member this.Imposter = this.Presence_.ImposterType
        member this.Prominent = this.Presence_.ProminentType
        member this.Omnipresent = this.Presence_.OmnipresentType
        member this.Presence = this.Presence_
        member this.Bounds = this.Bounds_
        member this.Entry = this.Entry_
        override this.GetHashCode () = this.HashCode_
        override this.Equals that = match that with :? Octelement<'e> as that -> this.Entry_.Equals that.Entry_ | _ -> false

    let intersects frustumEnclosed frustumExposed frustumImposter lightBox (element : _ Octelement) =
        Presence.intersects3d frustumEnclosed frustumExposed frustumImposter lightBox element.Light element.Bounds_ element.Presence_

    let make visible static_ light presence bounds (entry : 'e) =
        let hashCode = entry.GetHashCode ()
        let flags =
            (if visible then VisibleMask else 0u) |||
            (if static_ then StaticMask else 0u) |||
            (if light then LightMask else 0u)
        { HashCode_ = hashCode; Flags_ = flags; Presence_ = presence; Bounds_ = bounds; Entry_ = entry }

/// An element in an octree.
type Octelement<'e when 'e : equality> = Octelement.Octelement<'e>

[<RequireQualifiedAccess>]
module internal Octnode =

    type [<Struct>] Octnode<'e when 'e : equality> =
        private
            { Id_ : uint64
              Depth_ : int
              Bounds_ : Box3
              Children_ : ValueEither<'e Octnode array, 'e Octelement HashSet> }

        member this.Id = this.Id_

    let internal atPoint (point : Vector3) (node : 'e Octnode inref) =
        node.Bounds_.Intersects point

    let internal isIntersectingBox (bounds : Box3) (node : 'e Octnode inref) =
        node.Bounds_.Intersects bounds

    let inline internal isIntersectingFrustum (frustum : Frustum) (node : 'e Octnode inref) =
        frustum.Intersects node.Bounds_

    let inline internal containsBox (bounds : Box3) (node : 'e Octnode inref) =
        node.Bounds_.Contains bounds = ContainmentType.Contains

    let rec internal addElement bounds (element : 'e Octelement inref) (node : 'e Octnode inref) =
        if isIntersectingBox bounds &node then
            match node.Children_ with
            | ValueLeft nodes ->
                for i in 0 .. dec nodes.Length do
                    let node = &nodes.[i]
                    addElement bounds &element &node
            | ValueRight elements ->
                elements.Remove element |> ignore
                elements.Add element |> ignore

    let rec internal removeElement bounds (element : 'e Octelement inref) (node : 'e Octnode inref) =
        if isIntersectingBox bounds &node then
            match node.Children_ with
            | ValueLeft nodes ->
                for i in 0 .. dec nodes.Length do
                    let node = &nodes.[i]
                    removeElement bounds &element &node
            | ValueRight elements -> elements.Remove element |> ignore

    let rec internal updateElement oldBounds newBounds (element : 'e Octelement inref) (node : 'e Octnode inref) =
        match node.Children_ with
        | ValueLeft nodes ->
            for i in 0 .. dec nodes.Length do
                let node = &nodes.[i]
                if isIntersectingBox oldBounds &node || isIntersectingBox newBounds &node then
                    updateElement oldBounds newBounds &element &node
        | ValueRight elements ->
            if isIntersectingBox newBounds &node then
                elements.Remove element |> ignore
                elements.Add element |> ignore
            elif isIntersectingBox oldBounds &node then
                elements.Remove element |> ignore

    let rec internal getElementsAtPoint point (set : 'e Octelement HashSet) (node : 'e Octnode inref) =
        match node.Children_ with
        | ValueLeft nodes ->
            for i in 0 .. dec nodes.Length do
                let node = &nodes.[i]
                if atPoint point &node then
                    getElementsAtPoint point set &node
        | ValueRight elements ->
            for element in elements do
                let bounds = element.Bounds
                if bounds.Intersects point then
                    set.Add element |> ignore

    let rec internal getElementsInBox box (set : 'e Octelement HashSet) (node : 'e Octnode inref) =
        match node.Children_ with
        | ValueLeft nodes ->
            for i in 0 .. dec nodes.Length do
                let node = &nodes.[i]
                if isIntersectingBox box &node then
                    getElementsInBox box set &node
        | ValueRight elements ->
            for element in elements do
                let bounds = element.Bounds
                if bounds.Intersects box then
                    set.Add element |> ignore

    let rec internal getElementsInFrustum frustum (set : 'e Octelement HashSet) (node : 'e Octnode inref) =
        match node.Children_ with
        | ValueLeft nodes ->
            for i in 0 .. dec nodes.Length do
                let node = &nodes.[i]
                if isIntersectingFrustum frustum &node then
                    getElementsInFrustum frustum set &node
        | ValueRight elements ->
            for element in elements do
                let bounds = element.Bounds
                if frustum.Intersects bounds then
                    set.Add element |> ignore

    let rec internal getElementsInPlayBox box (set : 'e Octelement HashSet) (node : 'e Octnode inref) =
        match node.Children_ with
        | ValueLeft nodes ->
            for i in 0 .. dec nodes.Length do
                let node = &nodes.[i]
                if isIntersectingBox box &node then
                    getElementsInPlayBox box set &node
        | ValueRight elements ->
            for element in elements do
                if not element.Static then
                    let bounds = element.Bounds
                    if bounds.Intersects box then
                        set.Add element |> ignore

    let rec internal getLightsInBox box (set : 'e Octelement HashSet) (node : 'e Octnode inref) =
        match node.Children_ with
        | ValueLeft nodes ->
            for i in 0 .. dec nodes.Length do
                let node = &nodes.[i]
                if isIntersectingBox box &node then
                    getLightsInBox box set &node
        | ValueRight elements ->
            for element in elements do
                if element.Light then
                    let bounds = element.Bounds
                    if bounds.Intersects box then
                        set.Add element |> ignore

    let rec internal getElementsInPlayFrustum frustum (set : 'e Octelement HashSet) (node : 'e Octnode inref) =
        match node.Children_ with
        | ValueLeft nodes ->
            for i in 0 .. dec nodes.Length do
                let node = &nodes.[i]
                if isIntersectingFrustum frustum &node then
                    getElementsInPlayFrustum frustum set &node
        | ValueRight elements ->
            for element in elements do
                if not element.Static then
                    let bounds = element.Bounds
                    if frustum.Intersects bounds then
                        set.Add element |> ignore

    let rec internal getElementsInViewFrustum enclosed exposed imposter frustum (set : 'e Octelement HashSet) (node : 'e Octnode inref) =
        match node.Children_ with
        | ValueLeft nodes ->
            for i in 0 .. dec nodes.Length do
                let node = &nodes.[i]
                if isIntersectingFrustum frustum &node then
                    getElementsInViewFrustum enclosed exposed imposter frustum set &node
        | ValueRight elements ->
            for element in elements do
                if enclosed then
                    if element.Enclosed || element.Exposed || element.Prominent then
                        if frustum.Intersects element.Bounds then
                            set.Add element |> ignore
                elif exposed then
                    if element.Exposed || element.Prominent then
                        if frustum.Intersects element.Bounds then
                            set.Add element |> ignore
                elif imposter then
                    if element.Imposter || element.Prominent then
                        if frustum.Intersects element.Bounds then
                            set.Add element |> ignore

    let rec internal getElementsInView frustumEnclosed frustumExposed frustumImposter lightBox (set : 'e Octelement HashSet) (node : 'e Octnode inref) =
        match node.Children_ with
        | ValueLeft nodes ->
            for i in 0 .. dec nodes.Length do
                let node = &nodes.[i]
                let intersectingEnclosed = isIntersectingFrustum frustumEnclosed &node
                let intersectingExposed = isIntersectingFrustum frustumExposed &node
                if intersectingEnclosed || intersectingExposed then
                    if intersectingEnclosed then getElementsInViewFrustum true false false frustumEnclosed set &node
                    if intersectingExposed then getElementsInViewFrustum false true false frustumExposed set &node
                elif isIntersectingFrustum frustumImposter &node then
                    getElementsInViewFrustum false false true frustumImposter set &node
                if isIntersectingBox lightBox &node then
                    getLightsInBox lightBox set &node
        | ValueRight _ -> ()

    let rec internal getElementsInPlay playBox playFrustum (set : 'e Octelement HashSet) (node : 'e Octnode inref) =
        match node.Children_ with
        | ValueLeft nodes ->
            for i in 0 .. dec nodes.Length do
                let node = &nodes.[i]
                if isIntersectingBox playBox &node then
                    getElementsInPlayBox playBox set &node
                if isIntersectingFrustum playFrustum &node then
                    getElementsInPlayFrustum playFrustum set &node
        | ValueRight _ -> ()

    let rec internal getElements (set : 'e Octelement HashSet) (node : 'e Octnode inref) =
        match node.Children_ with
        | ValueLeft nodes ->
            for i in 0 .. dec nodes.Length do
                let node = &nodes.[i]
                getElements set &node
        | ValueRight children ->
            set.UnionWith children

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
            { Id_ = Gen.id64
              Depth_ = depth
              Bounds_ = bounds
              Children_ = children }
        if depth = 1 then leaves.Add (bounds.Min, node)
        node

type internal Octnode<'e when 'e : equality> = Octnode.Octnode<'e>

[<RequireQualifiedAccess>]
module Octree =

    /// Provides an enumerator interface to the octree queries.
    /// TODO: see if we can make this enumerator work when its results are evaluated multiple times in the debugger.
    type internal OctreeEnumerator<'e when 'e : equality> (uncullable : 'e Octelement seq, cullable : 'e Octelement seq) =

        let uncullableArray = SArray.ofSeq uncullable // eagerly convert to segmented array to keep iteration valid
        let cullableArray = SArray.ofSeq cullable // eagerly convert to segmented array to keep iteration valid
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
            { mutable ElementsModified : bool // OPTIMIZATION: short-circuit queries if tree has never had its elements modified.
              Leaves : Dictionary<Vector3, 'e Octnode>
              LeafSize : Vector3 // TODO: consider keeping the inverse of this to avoid divides.
              Omnipresent : 'e Octelement HashSet
              Node : 'e Octnode
              Depth : int
              Bounds : Box3 }

    let private findNode (bounds : Box3) tree : 'e Octnode =
        let offset = -tree.Bounds.Min // use offset to bring div ops into positive space
        let divs = (bounds.Min + offset) / tree.LeafSize
        let evens = v3 (divs.X |> int |> single) (divs.Y |> int |> single) (divs.Z |> int |> single)
        let leafKey = evens * tree.LeafSize - offset
        match tree.Leaves.TryGetValue leafKey with
        | (true, leaf) when Octnode.containsBox bounds &leaf -> leaf
        | (_, _) -> tree.Node

    let addElement bounds (element : 'e Octelement) tree =
        tree.ElementsModified <- true
        let presence = element.Presence
        if presence.OmnipresentType then
            tree.Omnipresent.Remove element |> ignore
            tree.Omnipresent.Add element |> ignore
        else
            if not (Octnode.isIntersectingBox bounds &tree.Node) then
                Log.info "Element is outside the octree's containment area or is being added redundantly."
                tree.Omnipresent.Remove element |> ignore
                tree.Omnipresent.Add element |> ignore
            else
                let node = findNode bounds tree
                Octnode.addElement bounds &element &node

    let removeElement bounds (element : 'e Octelement) tree =
        tree.ElementsModified <- true
        let presence = element.Presence
        if presence.OmnipresentType then 
            tree.Omnipresent.Remove element |> ignore
        else
            if not (Octnode.isIntersectingBox bounds &tree.Node) then
                Log.info "Element is outside the octree's containment area or is not present for removal."
                tree.Omnipresent.Remove element |> ignore
            else
                let node = findNode bounds tree
                Octnode.removeElement bounds &element &node

    let updateElement (oldPresence : Presence) oldBounds (newPresence : Presence) newBounds element tree =
        tree.ElementsModified <- true
        let wasInNode = not oldPresence.OmnipresentType && Octnode.isIntersectingBox oldBounds &tree.Node
        let isInNode = not newPresence.OmnipresentType && Octnode.isIntersectingBox newBounds &tree.Node
        if wasInNode then
            if isInNode then
                let oldNode = findNode oldBounds tree
                let newNode = findNode newBounds tree
                if oldNode.Id <> newNode.Id then
                    Octnode.removeElement oldBounds &element &oldNode
                    Octnode.addElement newBounds &element &newNode
                else Octnode.updateElement oldBounds newBounds &element &newNode
            else
                Octnode.removeElement oldBounds &element &tree.Node |> ignore
                tree.Omnipresent.Remove element |> ignore
                tree.Omnipresent.Add element |> ignore
        else
            if isInNode then
                tree.Omnipresent.Remove element |> ignore
                Octnode.addElement newBounds &element &tree.Node
            else
                tree.Omnipresent.Remove element |> ignore
                tree.Omnipresent.Add element |> ignore

    let getElementsOmnipresent (set : _ HashSet) tree =
        if tree.ElementsModified
        then new OctreeEnumerable<'e> (new OctreeEnumerator<'e> (tree.Omnipresent, set)) :> 'e Octelement IEnumerable
        else Seq.empty

    let getElementsAtPoint point (set : _ HashSet) tree =
        if tree.ElementsModified then
            let node = findNode (box3 point v3Zero) tree
            Octnode.getElementsAtPoint point set &node
            new OctreeEnumerable<'e> (new OctreeEnumerator<'e> (tree.Omnipresent, set)) :> 'e Octelement IEnumerable
        else Seq.empty

    let getElementsInBounds bounds (set : _ HashSet) tree =
        if tree.ElementsModified then
            Octnode.getElementsInBox bounds set &tree.Node
            new OctreeEnumerable<'e> (new OctreeEnumerator<'e> (tree.Omnipresent, set)) :> 'e Octelement IEnumerable
        else Seq.empty

    let getElementsInFrustum frustum (set : _ HashSet) tree =
        if tree.ElementsModified then
            Octnode.getElementsInFrustum frustum set &tree.Node
            new OctreeEnumerable<'e> (new OctreeEnumerator<'e> (tree.Omnipresent, set)) :> 'e Octelement IEnumerable
        else Seq.empty

    let getElementsInView frustumEnclosed frustumExposed frustumImposter lightBox (set : _ HashSet) tree =
        if tree.ElementsModified then
            Octnode.getElementsInView frustumEnclosed frustumExposed frustumImposter lightBox set &tree.Node
            new OctreeEnumerable<'e> (new OctreeEnumerator<'e> (tree.Omnipresent, set)) :> 'e Octelement IEnumerable
        else Seq.empty

    let getElementsInPlay playBox playFrustum (set : _ HashSet) tree =
        if tree.ElementsModified then
            Octnode.getElementsInPlay playBox playFrustum set &tree.Node
            new OctreeEnumerable<'e> (new OctreeEnumerator<'e> (tree.Omnipresent, set)) :> 'e Octelement IEnumerable
        else Seq.empty

    let getElements (set : _ HashSet) tree =
        if tree.ElementsModified then
            Octnode.getElements set &tree.Node
            new OctreeEnumerable<'e> (new OctreeEnumerator<'e> (tree.Omnipresent, set)) :> 'e Octelement IEnumerable
        else Seq.empty

    let getLightsInPlay lightBox (set : _ HashSet) tree =
        if tree.ElementsModified then
            Octnode.getLightsInBox lightBox set &tree.Node
            let omnipresent = tree.Omnipresent |> Seq.filter (fun element -> element.Light)
            new OctreeEnumerable<'e> (new OctreeEnumerator<'e> (omnipresent, set)) :> 'e Octelement IEnumerable
        else Seq.empty

    let getDepth tree =
        tree.Depth

    let make<'e when 'e : equality> (depth : int) (size : Vector3) =
        if  not (MathHelper.IsPowerOfTwo size.X) ||
            not (MathHelper.IsPowerOfTwo size.Y) ||
            not (MathHelper.IsPowerOfTwo size.Z) then
            failwith "Invalid size for Octtree. Expected value whose components are a power of two."
        let leaves = dictPlus HashIdentity.Structural []
        let mutable leafSize = size
        for _ in 1 .. dec depth do leafSize <- leafSize * 0.5f
        let min = size * -0.5f + leafSize * 0.5f // OPTIMIZATION: offset min by half leaf size to minimize margin hits at origin.
        let bounds = box3 min size
        { ElementsModified = false
          Leaves = leaves
          LeafSize = leafSize
          Omnipresent = HashSet HashIdentity.Structural
          Node = Octnode.make<'e> depth bounds leaves
          Depth = depth
          Bounds = bounds }

/// A spatial structure that organizes elements in a 3d grid.
type Octree<'e when 'e : equality> = Octree.Octree<'e>