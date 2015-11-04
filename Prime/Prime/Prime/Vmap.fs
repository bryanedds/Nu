// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime
open System
open System.Collections
open System.Collections.Generic
open Prime

/// A hash-key-value triple, implemented with a struct for efficiency.
type internal Hkv<'k, 'v when 'k : comparison> =
    struct
        new (h, k, v) = { H = h; K = k; V = v }
        val H : int
        val K : 'k
        val V : 'v
        end

/// TODO: there's an F# issue where UseNullAsTrueValue does not work on unions with 4 or more cases
/// https://github.com/Microsoft/visualfsharp/issues/711 . Once resolved, should use it and be able
/// to make arrays with Array.zeroCreate alone without also copying over the empty array.
type internal Vnode<'k, 'v when 'k : comparison> =
    private
        | Nil
        | Singleton of Hkv<'k, 'v>
        | Multiple of Vnode<'k, 'v> array
        | Clash of Map<'k, 'v>

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module internal Vnode =

    /// OPTIMIZATION: Array.Clone () is not used since it's been profiled to be slower
    let inline cloneArray (arr : Vnode<'k, 'v> array) : Vnode<'k, 'v> array =
        let arr' = Array.zeroCreate 32 // there's an unecessary check against the size here, but that's the only inefficiency
        Array.Copy (arr, 0, arr', 0, 32) // param checks are inefficient, but hopefully there's at least a memcpy underneath...
        arr'

    let inline private hashToIndex h dep =
        (h >>> (dep * 5)) &&& 0x1F

    let isEmpty node =
        match node with
        | Nil -> true
        | _ -> false

    let rec toSeq node =
        seq {
            match node with
            | Nil -> yield! Seq.empty
            | Singleton hkv -> yield KeyValuePair<'k, 'v> (hkv.K, hkv.V)
            | Multiple arr -> for n in arr do yield! toSeq n
            | Clash clashMap -> yield! clashMap }

    /// OPTIMIZATION: Requires an empty array to use the source of new array clones in order to avoid Array.create.
    let rec add (hkv : Hkv<'k, 'v>) (earr : Vnode<'k, 'v> array) (mdep : int) (dep : int) (node : Vnode<'k, 'v>) : Vnode<'k, 'v> =

        // lower than max depth, non-clashing
        if dep < mdep then

            // handle non-clash cases
            match node with
            | Nil ->

                // make singleton entry
                Singleton hkv

            | Singleton hkv' ->
                
                // if additional entry; convert Singleton to Multiple
                let idx = hashToIndex hkv.H dep
                let idx' = hashToIndex hkv'.H dep
                if idx <> idx' then
                    let arr = cloneArray earr
                    arr.[idx] <- Singleton hkv
                    arr.[idx'] <- Singleton hkv'
                    Multiple arr

                // if replace entry; remain Singleton
                elif hkv.K == hkv'.K then
                    Singleton hkv

                // if add entry with same idx; add both in new node
                else
                    let dep' = dep + 1
                    let node' = add hkv earr mdep dep' Nil
                    let node' = add hkv' earr mdep dep' node'
                    let arr = cloneArray earr
                    arr.[idx] <- node'
                    Multiple arr

            | Multiple arr ->

                // add entry with recursion
                let idx = hashToIndex hkv.H dep
                let entry = arr.[idx]
                let arr = cloneArray arr
                arr.[idx] <- add hkv earr mdep (dep + 1) entry
                Multiple arr

            | Clash _ ->

                // logically should never hit here
                failwithumf ()

        // clashing
        else
            
            // handle clash cases
            match node with
            | Nil ->
                Clash ^ Map.singleton hkv.K hkv.V
            | Singleton hkv' ->
                Clash ^ Map.add hkv.K hkv.V ^ Map.singleton hkv'.K hkv'.V
            | Multiple _ ->
                failwithumf () // should never hit here
            | Clash clashMap ->
                Clash ^ Map.add hkv.K hkv.V clashMap

    let rec remove (h : int) (k : 'k) (dep : int) (node : Vnode<'k, 'v>) : Vnode<'k, 'v> =
        match node with
        | Nil -> node
        | Singleton hkv -> if hkv.K == k then Nil else node
        | Multiple arr ->
            let idx = hashToIndex h dep
            let entry = arr.[idx]
            let arr = cloneArray arr
            arr.[idx] <- remove h k (dep + 1) entry
            if Array.forall isEmpty arr then Nil else Multiple arr // does not collapse Multiple to Singleton, tho could?
        | Clash clashMap ->
            let clashMap = Map.remove k clashMap
            if Map.isEmpty clashMap then Nil else Clash clashMap

    let rec tryFind (h : int) (k : 'k) (dep : int) (node : Vnode<'k, 'v>) : 'v option =
        match node with
        | Nil -> None
        | Singleton hkv -> if hkv.K == k then Some hkv.V else None
        | Multiple arr -> let idx = hashToIndex h dep in tryFind h k (dep + 1) arr.[idx]
        | Clash clashMap -> Map.tryFind k clashMap

    let empty =
        Nil

/// A persistent hash map with depth variations.
/// TODO: document.
/// TODO: implement fold, map, and filter.
type [<NoEquality; NoComparison>] Vmap<'k, 'v when 'k : comparison> =
    private
        { Vnode : Vnode<'k, 'v>
          EmptyArray : Vnode<'k, 'v> array
          MaxDepth : int }

    interface IEnumerable<KeyValuePair<'k, 'v>> with
        member this.GetEnumerator () = (Vnode.toSeq this.Vnode).GetEnumerator ()

    interface IEnumerable with
        member this.GetEnumerator () = (Vnode.toSeq this.Vnode).GetEnumerator () :> IEnumerator

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Vmap =

    let isEmpty map =
        Vnode.isEmpty map.Vnode

    let makeEmpty mdep =
        if mdep > 7 then failwith "Vmap max depth should not be greater than 7."
        elif mdep < 0 then failwith "Vmap max depth should not be less than 0."
        else { Vnode = Vnode.empty; EmptyArray = Array.create 32 Vnode.empty; MaxDepth = mdep }

    let add (k : 'k) (v : 'v) map =
        let hkv = Hkv (k.GetHashCode (), k, v)
        let node = Vnode.add hkv map.EmptyArray map.MaxDepth 0 map.Vnode
        { map with Vnode = node }

    let addMany entries map =
        Seq.fold (fun map (k : 'k, v : 'v) -> add k v map) map entries

    let remove (k : 'k) map =
        let h = k.GetHashCode ()
        { map with Vnode = Vnode.remove h k 0 map.Vnode }

    let tryFind (k : 'k) map : 'v option =
        let h = k.GetHashCode ()
        Vnode.tryFind h k 0 map.Vnode

    let find (k : 'k) map : 'v =
        tryFind k map |> Option.get

    let containsKey k map =
        match tryFind k map with
        | Some _ -> true
        | None -> false

    let toSeq (map : Vmap<'k, 'v>) =
        map :> IEnumerable<KeyValuePair<'k, 'v>>