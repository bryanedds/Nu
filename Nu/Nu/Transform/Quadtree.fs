// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open Prime

/// Masks for Quadelement flags.
module QuadelementMasks =

    // OPTIMIZATION: Quadelement flag bit-masks for performance.
    let [<Literal>] VisibleMask =   0b0001u
    let [<Literal>] StaticMask =    0b0010u

// NOTE: opening this in order to make the Quadelement property implementations reasonably succinct.
open QuadelementMasks

[<RequireQualifiedAccess>]
module Quadelement =

    /// An element in an quadtree.
    /// NOTE: we intentionally use incomplete equality semantics here so these can be stored in a HashSet.
    type [<CustomEquality; NoComparison; Struct>] Quadelement<'e when 'e : equality> = 
        private
            { HashCode_ : int // OPTIMIZATION: cache hash code to increase look-up speed.
              Flags_ : uint
              Entry_ : 'e }
        member this.Visible = this.Flags_ &&& VisibleMask <> 0u
        member this.Static = this.Flags_ &&& StaticMask <> 0u
        member this.Entry = this.Entry_
        override this.GetHashCode () = this.HashCode_
        override this.Equals that = match that with :? Quadelement<'e> as that -> this.Entry_.Equals that.Entry_ | _ -> false
        static member make visible static_ (entry : 'e) =
            let hashCode = entry.GetHashCode ()
            let flags =
                (if visible then VisibleMask else 0u) |||
                (if static_ then StaticMask else 0u)
            { HashCode_ = hashCode; Flags_ = flags; Entry_ = entry }

/// An element in a quadree.
type Quadelement<'e when 'e : equality> = Quadelement.Quadelement<'e>

/// Equality compares two quadelements.
type QuadelementEqualityComparer<'e when 'e : equality> () =
    interface 'e Quadelement IEqualityComparer with
        member this.Equals (left, right) = left.Entry = right.Entry // OPTIMIZATION: inline equality to avoid allocation.
        member this.GetHashCode element = element.GetHashCode ()

[<RequireQualifiedAccess>]
module internal Quadnode =

    type [<Struct>] internal Quadchildren<'e when 'e : equality> =
        | NoChildren
        | NodeChildren of NodeChildren : 'e Quadnode array
        | ElementChildren of ElementChildren : 'e Quadelement HashSet

    and internal Quadnode<'e when 'e : equality> =
        private
            { mutable ElementsCount_ : int // OPTIMIZATION: keeps track of total contained elements in order to get an early-out on queries.
              Id_ : uint64
              Depth_ : int
              Bounds_ : Box2
              mutable Children_ : 'e Quadchildren
              Comparer_ : 'e QuadelementEqualityComparer
              Leaves_ : Dictionary<Vector2, 'e Quadnode> }

        override this.ToString () =
            ""

        member this.Id = this.Id_

    let internal makeChildren<'e when 'e : equality> node =
        let childSize = node.Bounds_.Size * 0.5f
        let childDepth = dec node.Depth_
        if childDepth > 0 then
            let (nodeChildren : 'e Quadnode array) =
                [|for i in 0 .. dec 4 do
                    let childPosition = v2 node.Bounds_.Min.X node.Bounds_.Min.Y + v2 (childSize.X * single (i % 2)) (childSize.Y * single (i / 2))
                    let childBounds = box2 childPosition childSize
                    let child =
                        { ElementsCount_ = 0
                          Id_ = Gen.idForInternal
                          Depth_ = childDepth
                          Bounds_ = childBounds
                          Children_ = NoChildren
                          Comparer_ = node.Comparer_
                          Leaves_ = node.Leaves_ }
                    if childDepth = 1 then node.Leaves_.Add (childBounds.Min, child)
                    child|]
            NodeChildren nodeChildren
        else
            let children = HashSet<'e Quadelement> node.Comparer_
            ElementChildren children

    let internal atPoint (point : Vector2) (node : 'e Quadnode) =
        node.Bounds_.Intersects point

    let internal isIntersectingBounds (bounds : Box2) (node : 'e Quadnode) =
        node.Bounds_.Intersects bounds

    let inline internal containsBoundsExclusive (bounds : Box2) (node : 'e Quadnode) =
        node.Bounds_.ContainsExclusive bounds = ContainmentType.Contains

    let rec internal addElement bounds (element : 'e Quadelement inref) (node : 'e Quadnode) : int =
        let delta =
            if isIntersectingBounds bounds node then
                match node.Children_ with
                | NoChildren ->
                    node.Children_ <- makeChildren node
                    addElement bounds &element node
                | NodeChildren nodes ->
                    let mutable delta = 0
                    for i in 0 .. dec nodes.Length do
                        let node = nodes.[i]
                        delta <- delta + addElement bounds &element node
                    delta
                | ElementChildren elements ->
                    let removed = elements.Remove element
                    let added = elements.Add element
                    if removed
                    then (if not added then -1 else 0)
                    else (if not added then 0 else 1)
            else 0
        node.ElementsCount_ <- node.ElementsCount_ + delta
        delta

    let rec internal removeElement bounds (element : 'e Quadelement inref) (node : 'e Quadnode) =
        let delta =
            if isIntersectingBounds bounds node then
                match node.Children_ with
                | NoChildren ->
                    0
                | NodeChildren nodes ->
                    let mutable delta = 0
                    for i in 0 .. dec nodes.Length do
                        let node = nodes.[i]
                        delta <- delta + removeElement bounds &element node
                    delta
                | ElementChildren elements ->
                    if elements.Remove element then -1 else 0
            else 0
        node.ElementsCount_ <- node.ElementsCount_ + delta
        delta

    let rec internal updateElement boundsOld boundsNew (element : 'e Quadelement inref) (node : 'e Quadnode) =
        let delta =
            match node.Children_ with
            | NoChildren ->
                if isIntersectingBounds boundsOld node || isIntersectingBounds boundsNew node then
                    node.Children_ <- makeChildren node
                    updateElement boundsOld boundsNew &element node
                else 0
            | NodeChildren nodes ->
                let mutable delta = 0
                for i in 0 .. dec nodes.Length do
                    let node = nodes.[i]
                    if isIntersectingBounds boundsOld node || isIntersectingBounds boundsNew node then
                        delta <- delta + updateElement boundsOld boundsNew &element node
                delta
            | ElementChildren elements ->
                if isIntersectingBounds boundsNew node then
                    let removed = elements.Remove element
                    let added = elements.Add element
                    if removed
                    then (if not added then -1 else 0)
                    else (if not added then 0 else 1)
                elif isIntersectingBounds boundsOld node then
                    if elements.Remove element then -1 else 0
                else 0
        node.ElementsCount_ <- node.ElementsCount_ + delta
        delta

    let rec internal clearElements node =
        node.ElementsCount_ <- 0
        match node.Children_ with
        | NoChildren ->
            ()
        | NodeChildren nodes ->
            for i in 0 .. dec nodes.Length do
                let node = &nodes.[i]
                clearElements node
        | ElementChildren children ->
            children.Clear ()

    let rec internal getElementsAtPoint point (set : 'e Quadelement HashSet) (node : 'e Quadnode) =
        match node.Children_ with
        | NoChildren ->
            ()
        | NodeChildren nodes ->
            for i in 0 .. dec nodes.Length do
                let node = &nodes.[i]
                if node.ElementsCount_ > 0 && atPoint point node then
                    getElementsAtPoint point set node
        | ElementChildren elements ->
            for element in elements do
                set.Add element |> ignore

    let rec internal getElementsInBounds bounds (set : 'e Quadelement HashSet) (node : 'e Quadnode) =
        match node.Children_ with
        | NoChildren ->
            ()
        | NodeChildren nodes ->
            for i in 0 .. dec nodes.Length do
                let node = &nodes.[i]
                if node.ElementsCount_ > 0 && isIntersectingBounds bounds node then
                    getElementsInBounds bounds set node
        | ElementChildren elements ->
            for element in elements do
                set.Add element |> ignore

    let rec internal getElementsInView bounds (set : 'e Quadelement HashSet) (node : 'e Quadnode) =
        match node.Children_ with
        | NoChildren ->
            ()
        | NodeChildren nodes ->
            for i in 0 .. dec nodes.Length do
                let node = &nodes.[i]
                if node.ElementsCount_ > 0 && isIntersectingBounds bounds node then
                    getElementsInView bounds set node
        | ElementChildren elements ->
            for element in elements do
                if element.Visible then
                    set.Add element |> ignore

    let rec internal getElementsInPlay bounds (set : 'e Quadelement HashSet) (node : 'e Quadnode) =
        match node.Children_ with
        | NoChildren ->
            ()
        | NodeChildren nodes ->
            for i in 0 .. dec nodes.Length do
                let node = &nodes.[i]
                if node.ElementsCount_ > 0 && isIntersectingBounds bounds node then
                    getElementsInView bounds set node
        | ElementChildren elements ->
            for element in elements do
                if not element.Static then
                    set.Add element |> ignore

    let rec internal getElements (set : 'e Quadelement HashSet) (node : 'e Quadnode) =
        match node.Children_ with
        | NoChildren ->
            ()
        | NodeChildren nodes ->
            for i in 0 .. dec nodes.Length do
                let node = &nodes.[i]
                if node.ElementsCount_ > 0 then
                    getElements set node
        | ElementChildren children ->
            set.UnionWith children

    let rec internal sweep (node : 'e Quadnode) =
        if node.ElementsCount_ = 0 then
            match node.Children_ with
            | NoChildren ->
                ()
            | NodeChildren nodes ->
                for i in 0 .. dec nodes.Length do
                    let node = &nodes.[i]
                    sweep node
            | ElementChildren _ ->
                node.Leaves_.Remove node.Bounds_.Min |> ignore<bool>
            node.Children_ <- NoChildren

    let internal make<'e when 'e : equality> comparer depth (bounds : Box2) (leaves : Dictionary<Vector2, 'e Quadnode>) : 'e Quadnode =
        if depth < 1 then failwith "Invalid depth for Octnode. Expected value of at least 1."
        let node =
            { ElementsCount_ = 0
              Id_ = Gen.idForInternal
              Depth_ = depth
              Bounds_ = bounds
              Children_ = NoChildren
              Comparer_ = comparer
              Leaves_ = leaves }
        node

type internal Quadnode<'e when 'e : equality> = Quadnode.Quadnode<'e>

[<RequireQualifiedAccess>]
module Quadtree =

    /// A spatial structure that organizes elements on a 2d plane.
    type [<ReferenceEquality>] Quadtree<'e when 'e : equality> =
        private
            { Leaves : Dictionary<Vector2, 'e Quadnode>
              LeafSize : Vector2 // TODO: consider keeping the inverse of this to avoid divides.
              Ubiquitous : 'e Quadelement HashSet
              Node : 'e Quadnode
              Depth : int
              Bounds : Box2 }

    let private tryFindLeafFast (bounds : Box2) tree : 'e Quadnode option =
        let offset = -tree.Bounds.Min // use offset to bring div ops into positive space
        let divs = (bounds.Min + offset) / tree.LeafSize
        let evens = v2 (divs.X |> int |> single) (divs.Y |> int |> single)
        let leafKey = evens * tree.LeafSize - offset
        match tree.Leaves.TryGetValue leafKey with
        | (true, leaf) when Quadnode.containsBoundsExclusive bounds leaf -> Some leaf
        | (_, _) -> None

    /// Add an element with the given presence and bounds to the tree.
    let addElement (presence : Presence) bounds element tree =
        if presence.ImposterType || presence.OmnipresentType then
            tree.Ubiquitous.Remove element |> ignore
            tree.Ubiquitous.Add element |> ignore
        else
            if not (Quadnode.isIntersectingBounds bounds tree.Node) then
                tree.Ubiquitous.Remove element |> ignore
                tree.Ubiquitous.Add element |> ignore
            else
                Quadnode.addElement bounds &element tree.Node |> ignore

    /// Remove an element with the given presence and bounds from the tree.
    let removeElement (presence : Presence) bounds element tree =
        if presence.ImposterType || presence.OmnipresentType then 
            tree.Ubiquitous.Remove element |> ignore
        else
            if not (Quadnode.isIntersectingBounds bounds tree.Node) then
                tree.Ubiquitous.Remove element |> ignore
            else
                Quadnode.removeElement bounds &element tree.Node |> ignore

    /// Update an existing element in the tree.
    let updateElement (presenceOld : Presence) boundsOld (presenceNew : Presence) boundsNew element tree =
        let wasInNode = not presenceOld.ImposterType && not presenceOld.OmnipresentType && Quadnode.isIntersectingBounds boundsOld tree.Node
        let isInNode = not presenceNew.ImposterType && not presenceNew.OmnipresentType && Quadnode.isIntersectingBounds boundsNew tree.Node
        if wasInNode then
            if isInNode then
                match tryFindLeafFast boundsOld tree with
                | Some leafOld ->
                    match tryFindLeafFast boundsNew tree with
                    | Some leafNew ->
                        if leafOld.Id = leafNew.Id
                        then Quadnode.updateElement boundsOld boundsNew &element leafNew |> ignore
                        else Quadnode.updateElement boundsOld boundsNew &element tree.Node |> ignore
                    | None -> Quadnode.updateElement boundsOld boundsNew &element tree.Node |> ignore
                | None -> Quadnode.updateElement boundsOld boundsNew &element tree.Node |> ignore
            else
                Quadnode.removeElement boundsOld &element tree.Node |> ignore
                tree.Ubiquitous.Remove element |> ignore
                tree.Ubiquitous.Add element |> ignore
        else
            if isInNode then
                tree.Ubiquitous.Remove element |> ignore
                Quadnode.addElement boundsNew &element tree.Node |> ignore
            else
                tree.Ubiquitous.Remove element |> ignore
                tree.Ubiquitous.Add element |> ignore

    /// Clear the contents of the tree.
    let clear tree =
        tree.Ubiquitous.Clear ()
        Quadnode.clearElements tree.Node

    /// Get all of the elements in a tree that are in a node intersected by the given point.
    let getElementsAtPoint point set tree =
        Quadnode.getElementsAtPoint point set tree.Node
        for omnipresent in tree.Ubiquitous do
            set.Add omnipresent |> ignore<bool>

    /// Get all of the elements in a tree that are in a node intersected by the given bounds.
    let getElementsInBounds bounds set tree =
        Quadnode.getElementsInBounds bounds set tree.Node
        for omnipresent in tree.Ubiquitous do
            set.Add omnipresent |> ignore<bool>

    /// Get all of the elements in a tree that are in a node intersected by the given bounds.
    let getElementsInView bounds set tree =
        Quadnode.getElementsInView bounds set tree.Node
        for omnipresent in tree.Ubiquitous do
            set.Add omnipresent |> ignore<bool>

    /// Get all of the elements in a tree that are in a node intersected by the given bounds.
    let getElementsInPlay bounds set tree =
        Quadnode.getElementsInPlay bounds set tree.Node
        for omnipresent in tree.Ubiquitous do
            set.Add omnipresent |> ignore<bool>

    /// Get all of the elements in a tree.
    let getElements (set : _ HashSet) tree =
        Quadnode.getElements set tree.Node
        for omnipresent in tree.Ubiquitous do
            set.Add omnipresent |> ignore<bool>

    /// Get the size of the tree's leaves.
    let getLeafSize tree =
        tree.LeafSize

    /// Get the depth of the tree.
    let getDepth tree =
        tree.Depth

    /// Get the bounds of the tree.
    let getBounds tree =
        tree.Bounds

    /// Remove all unused non-root nodes in the tree.
    let sweep tree =
        Quadnode.sweep tree.Node

    /// Create a Quadtree with the given depth and overall size.
    /// Size dimensions must be a power of two.
    let make<'e when 'e : equality> depth (size : Vector2) =
        if  not (Math.PowerOfTwo size.X) ||
            not (Math.PowerOfTwo size.Y) then
            failwith "Invalid size for Quadtree. Expected value whose components are a power of two."
        let leaves = dictPlus HashIdentity.Structural []
        let mutable leafSize = size
        for _ in 0 .. dec depth do leafSize <- leafSize * 0.5f
        let comparer = QuadelementEqualityComparer<'e> ()
        let min = size * -0.5f + leafSize * 0.5f // OPTIMIZATION: offset min by half leaf size to minimize margin hits at origin.
        let bounds = box2 min size
        { Leaves = leaves
          LeafSize = leafSize
          Ubiquitous = HashSet comparer
          Node = Quadnode.make<'e> comparer (inc depth) bounds leaves
          Depth = depth
          Bounds = bounds }

/// A spatial structure that organizes elements on a 2d plane.
type Quadtree<'e when 'e : equality> = Quadtree.Quadtree<'e>