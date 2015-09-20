// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime

/// A tree onto which arbitrary recursive discriminated unions can be projected.
/// TODO: implement TypeConverter.
/// TODO: document.
[<CompilationRepresentation (CompilationRepresentationFlags.UseNullAsTrueValue)>]
type 'a RduTree =
    | Leaf
    | Branch of 'a * 'a RduTree list

[<RequireQualifiedAccess>]
module RduTree =

    let empty = Leaf

    let isEmpty tree =
        match tree with
        | Leaf -> true
        | Branch _ -> false

    let rec map mapper tree =
        match tree with
        | Leaf -> empty
        | Branch (parent, children) -> Branch (mapper parent, List.map (map mapper) children)

    let rec fold (folder : 's -> 'a -> 'a RduTree list -> 's) (state : 's) (tree : 'a RduTree) =
        match tree with
        | Leaf -> state
        | Branch (parent, children) ->
            let subtreeFolder = fun state subtree -> fold folder state subtree
            let subtreeFolded = List.fold subtreeFolder state children
            folder subtreeFolded parent children

    let rec foldSimple (folder : 's -> 'a -> 's) (state : 's) (tree : 'a RduTree) =
        match tree with
        | Leaf -> state
        | Branch (parent, children) ->
            let listFolder = fun state subtree -> foldSimple folder state subtree
            let listFolded = List.fold listFolder state children
            folder listFolded parent

    // TODO: test this!
    let rec foldParent (getChildren : 'a -> 'a list) (folder : 'a -> 'a list -> 's) (state : 's) (parent : 'a) : 's =
        let children = getChildren parent
        let listFolder = fun child -> foldParent getChildren folder child
        List.fold listFolder state children

    let rec filter (pred : 'a -> bool) (tree : 'a RduTree) : 'a option RduTree =
        match tree with
        | Leaf -> empty
        | Branch (parent, children) ->
            let mappedChildren = List.map (filter pred) children
            if pred parent then Branch (Some parent, mappedChildren)
            else Branch (None, mappedChildren)

    let rec skim (pred : 'a -> bool) (tree : 'a RduTree) : 'a list =
        match tree with
        | Leaf -> []
        | Branch (parent, children) -> if pred parent then [parent] else List.concat (List.map (skim pred) children)

    let rec fromParent getChildren parent =
        foldParent
            getChildren
            (fun parent children -> Branch (parent, List.map (fromParent getChildren) children))
            empty
            parent

    let toValueListBy by tree =
        foldSimple (fun state value -> by value :: state) [] tree

    let toValueList tree =
        toValueListBy (fun value -> value) tree

    let singleton value =
        Branch (value, [])