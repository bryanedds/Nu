// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections
open System.Collections.Generic
open System.Numerics
open Prime

[<RequireQualifiedAccess>]
module internal Octnode =

    type [<NoEquality; NoComparison>] Octnode<'e when 'e : equality> =
        private
            { Depth : int
              Bounds : Box3
              Children : ValueEither<'e Octnode array, 'e HashSet> }

    let internal atPoint point node =
        Math.isPointInBounds3d point node.Bounds

    let internal isIntersectingBounds bounds node =
        Math.isBoundsIntersectingBounds3d bounds node.Bounds

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
                                let childBounds = Box3 (childPosition, childSize)
                                yield make granularity childDepth childBounds|]|]|]
                ValueLeft (nodes |> Array.concat |> Array.concat)
            else ValueRight (HashSet<'e> HashIdentity.Structural)
        { Depth = depth
          Bounds = bounds
          Children = children }

type internal Octnode<'e when 'e : equality> = Octnode.Octnode<'e>

[<RequireQualifiedAccess>]
module Octree =

    /// Provides an enumerator interface to the octree queries.
    type internal OctreeEnumerator<'e when 'e : equality> (localElements : 'e HashSet, omnipresentElements : 'e HashSet) =

        let localList = List localElements // eagerly convert to list to keep iteration valid
        let omnipresentList = List omnipresentElements // eagerly convert to list to keep iteration valid
        let mutable localEnrValid = false
        let mutable omnipresentEnrValid = false
        let mutable localEnr = Unchecked.defaultof<_>
        let mutable omnipresentEnr = Unchecked.defaultof<_>

        interface 'e IEnumerator with
            member this.MoveNext () =
                if not localEnrValid then
                    localEnr <- localList.GetEnumerator ()
                    localEnrValid <- true
                    if not (localEnr.MoveNext ()) then
                        omnipresentEnr <- omnipresentList.GetEnumerator ()
                        omnipresentEnrValid <- true
                        omnipresentEnr.MoveNext ()
                    else true
                else
                    if not (localEnr.MoveNext ()) then
                        if not omnipresentEnrValid then
                            omnipresentEnr <- omnipresentList.GetEnumerator ()
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
            
    /// Provides an enumerable interface to the octree queries.
    type internal OctreeEnumerable<'e when 'e : equality> (enr : 'e OctreeEnumerator) =
        interface IEnumerable<'e> with
            member this.GetEnumerator () = enr :> 'e IEnumerator
            member this.GetEnumerator () = enr :> IEnumerator

    /// A spatial structure that organizes elements in a 3D grid.
    type [<NoEquality; NoComparison>] Octree<'e when 'e : equality> =
        private
            { Node : 'e Octnode
              OmnipresentElements : 'e HashSet
              Depth : int
              Granularity : int
              Bounds : Box3 }

    let addElement omnipresent bounds element tree =
        if omnipresent then
            tree.OmnipresentElements.Add element |> ignore
        else
            if not (Octnode.isIntersectingBounds bounds tree.Node) then
                Log.info "Element is outside the octree's containment area or is being added redundantly."
                tree.OmnipresentElements.Add element |> ignore
            else Octnode.addElement bounds element tree.Node

    let removeElement omnipresent bounds element tree =
        if omnipresent then 
            tree.OmnipresentElements.Remove element |> ignore
        else
            if not (Octnode.isIntersectingBounds bounds tree.Node) then
                Log.info "Element is outside the octree's containment area or is not present for removal."
                tree.OmnipresentElements.Remove element |> ignore
            else Octnode.removeElement bounds element tree.Node

    let updateElement oldBounds newBounds element tree =
        let oldInBounds = Octnode.isIntersectingBounds oldBounds tree.Node
        let newInBounds = Octnode.isIntersectingBounds newBounds tree.Node
        if oldInBounds && not newInBounds then
            // going out of bounds
            Log.info "Element is outside the octree's containment area."
            if not newInBounds then tree.OmnipresentElements.Add element |> ignore
            Octnode.updateElement oldBounds newBounds element tree.Node
        elif not oldInBounds && newInBounds then
            // going back in bounds
            if not oldInBounds then tree.OmnipresentElements.Remove element |> ignore
            Octnode.updateElement oldBounds newBounds element tree.Node
        elif oldInBounds && newInBounds then
            // staying in bounds
            let rootBounds = tree.Bounds
            let rootDepth = pown tree.Granularity tree.Depth
            let leafSize = rootBounds.Size / single rootDepth
            let leafPosition =
                v3
                    (oldBounds.Position.X - (rootBounds.Position.X + oldBounds.Position.X) % leafSize.X)
                    (oldBounds.Position.Y - (rootBounds.Position.Y + oldBounds.Position.Y) % leafSize.Y)
                    (oldBounds.Position.Z - (rootBounds.Position.Z + oldBounds.Position.Z) % leafSize.Z)
            let leafBounds = Box3 (leafPosition, leafSize)
            if  not (Math.isBoundsInBounds3d oldBounds leafBounds) ||
                not (Math.isBoundsInBounds3d newBounds leafBounds) then
                Octnode.updateElement oldBounds newBounds element tree.Node
        else
            // staying out of bounds
            ()

    let getElementsOmnipresent tree =
        let set = HashSet HashIdentity.Structural
        new OctreeEnumerable<'e> (new OctreeEnumerator<'e> (tree.OmnipresentElements, set)) :> 'e IEnumerable

    let getElementsAtPoint point tree =
        let set = HashSet HashIdentity.Structural
        Octnode.getElementsAtPoint point tree.Node set
        new OctreeEnumerable<'e> (new OctreeEnumerator<'e> (tree.OmnipresentElements, set)) :> 'e IEnumerable

    let getElementsInBounds bounds tree =
        let set = HashSet HashIdentity.Structural
        Octnode.getElementsInBounds bounds tree.Node set
        new OctreeEnumerable<'e> (new OctreeEnumerator<'e> (tree.OmnipresentElements, set)) :> 'e IEnumerable

    let getDepth tree =
        tree.Depth

    let clone tree =
        { Node = Octnode.clone tree.Node
          OmnipresentElements = HashSet (tree.OmnipresentElements, HashIdentity.Structural)
          Depth = tree.Depth
          Granularity = tree.Granularity
          Bounds = tree.Bounds }

    let make<'e when 'e : equality> granularity depth bounds =
        { Node = Octnode.make<'e> granularity depth bounds
          OmnipresentElements = HashSet HashIdentity.Structural
          Depth = depth
          Granularity = granularity
          Bounds = bounds }
          
/// A spatial structure that organizes elements in a 3D grid.
type Octree<'e when 'e : equality> = Octree.Octree<'e>