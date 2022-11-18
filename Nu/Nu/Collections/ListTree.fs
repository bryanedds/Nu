// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System.Collections.Generic

/// An imperative tree data structure.
/// TODO: move this to Prime once it's tested.
/// NOTE: not supported by SymbolicConverter.
type [<ReferenceEquality; NoComparison>] 'a ListTree =
    | ListRoot of 'a ListTree List
    | ListBranch of 'a * 'a ListTree List

[<RequireQualifiedAccess>]
module ListTree =

    /// Convert a list tree to an iterable sequences.
    let rec toSeq tree =
        seq {
            match tree with
            | ListRoot children ->
                for child in children do
                    yield! toSeq child
            | ListBranch (item, children) ->
                yield item
                for child in children do
                    yield! toSeq child }

    /// Find all items that satisfy the given predicate.
    let rec findAll pred tree =
        seq {
            match tree with
            | ListRoot children ->
                for child in children do
                    yield! findAll pred child
            | ListBranch (item, children) ->
                if pred item then yield item
                for child in children do
                    yield! findAll pred child }

    /// Find all parents that satisfy the given predicate.
    let rec findParents pred tree =
        seq {
            match tree with
            | ListRoot children ->
                for child in children do
                    yield! findParents pred child
            | ListBranch (item, children) ->
                for child in children do
                    if pred item then yield (tree, child)
                    yield! findParents pred child }

    /// Find the first item that satisfies the given predicate.
    let tryFind pred tree =
        Seq.tryHead (findAll pred tree)

    /// Find the first parent that satisfies the given predicate.
    let tryFindParent pred tree =
        Seq.tryHead (findParents pred tree)

    /// Try to add an item under the first existing item that satisfies a predicate.
    let tryAdd pred item tree =
        match tryFindParent pred tree with
        | Some (parent, _) ->
            let children =
                match parent with
                | ListRoot children -> children
                | ListBranch (_, children) -> children
            let node = ListBranch (item, List ())
            children.Add node
            Some (parent, node)
        | None -> None

    /// Try to insert an item under the first existing item that satisfies a predicate.
    let tryInsert pred (item : 'a) tree =
        match tryFindParent pred tree with
        | Some (parent, _) ->

            // get children
            let children =
                match parent with
                | ListRoot children -> children
                | ListBranch (_, children) -> children

            // move current parent's children to a new list
            let children' = List ()
            for child in children do children'.Add child
            do children.Clear ()

            // add item and new children to parent
            let node = ListBranch (item, children')
            do children.Add node
            Some (parent, node)

        | None -> None

    /// Remove the first item that satisfies a predicate.
    let removeFirst pred tree =
        match tryFindParent pred tree with
        | Some (parent, node) ->

            // get peers
            let peers =
                match parent with
                | ListRoot children -> children
                | ListBranch (_, children) -> children

            // get children
            let children =
                match node with
                | ListRoot children -> children
                | ListBranch (_, children) -> children

            // move children to peers
            for child in children do peers.Add child
            children.Clear ()

            // remove self from peers
            peers.Remove node

        | None -> false

    /// Fold over a list tree.
    let fold folder tree =
        Seq.fold folder (toSeq tree)

    /// Map over a list tree.
    let rec map mapper tree =
        match tree with
        | ListRoot children ->
            ListRoot (children |> Seq.map (map mapper) |> List)
        | ListBranch (item, children) ->
            ListBranch (mapper item, children |> Seq.map (map mapper) |> List)

    /// Make a singleton list tree.
    let singleton item =
        let branch = ListBranch (item, List<ListTree<'a>> ())
        ListRoot (List [branch])

    /// Make an empty list tree.
    let makeEmpty<'a> () =
        ListRoot (List<ListTree<'a>> ())