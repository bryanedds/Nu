// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.Collections
open System.Collections.Generic
open System.ComponentModel
open System.Reflection
open Microsoft.FSharp.Reflection
open Prime

/// A hash-key-value triple, implemented with a struct for efficiency.
type internal Hkv<'k, 'v when 'k : comparison> =
    struct
        new (h, k, v) = { H = h; K = k; V = v }
        val H : int
        val K : 'k
        val V : 'v
        end

[<AutoOpen>]
module internal VnodeModule =

    /// TODO: there's an F# issue where UseNullAsTrueValue does not work on unions with 4 or more cases
    /// https://github.com/Microsoft/visualfsharp/issues/711 . Once resolved, should use it and be able
    /// to make arrays with Array.zeroCreate alone without also copying over the empty array.
    type [<NoComparison>] internal Vnode<'k, 'v when 'k : comparison> =
        private
            | Nil
            | Singleton of Hkv<'k, 'v>
            | Multiple of Vnode<'k, 'v> array
            | Clash of Map<'k, 'v>

    [<RequireQualifiedAccess>]
    module internal Vnode =
    
        /// OPTIMIZATION: Array.Clone () is not used since it's been profiled to be slower
        let inline cloneArray (arr : Vnode<'k, 'v> array) : Vnode<'k, 'v> array =
            let arr' = Array.zeroCreate 32  // NOTE: there's an unecessary check against the size here, but that's the only inefficiency
                                            // TODO: use Array.zeroCreateUnchecked if / when it becomes available
            Array.Copy (arr, 0, arr', 0, 32) // param checks are inefficient, but hopefully there's at least a memcpy underneath...
            arr'
    
        let inline private hashToIndex h dep =
            (h >>> (dep * 5)) &&& 0x1F
    
        let isEmpty node =
            match node with
            | Nil -> true
            | _ -> false
    
        let rec fold folder state node =
            match node with
            | Nil -> state
            | Singleton hkv -> folder state hkv.K hkv.V
            | Multiple arr -> Array.fold (fold folder) state arr
            | Clash clashMap -> Map.fold folder state clashMap
    
        /// NOTE: This function seems to profile as being very slow. I don't know if it's the seq / yields syntax or what.
        let rec toSeq node =
            seq {
                match node with
                | Nil -> yield! Seq.empty
                | Singleton hkv -> yield (hkv.K, hkv.V)
                | Multiple arr -> for n in arr do yield! toSeq n
                | Clash clashMap -> yield! Map.toSeq clashMap }
    
        /// OPTIMIZATION: Requires an empty array to use the source of new array clones in order to avoid Array.create.
        let rec add (hkv : Hkv<'k, 'v>) (earr : Vnode<'k, 'v> array) (dep : int) (node : Vnode<'k, 'v>) : Vnode<'k, 'v> =
    
            // lower than max depth, non-clashing
            if dep < 8 then
    
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
                        let node' = add hkv earr dep' Nil
                        let node' = add hkv' earr dep' node'
                        let arr = cloneArray earr
                        arr.[idx] <- node'
                        Multiple arr
    
                | Multiple arr ->
    
                    // add entry with recursion
                    let idx = hashToIndex hkv.H dep
                    let entry = arr.[idx]
                    let arr = cloneArray arr
                    arr.[idx] <- add hkv earr (dep + 1) entry
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

[<AutoOpen>]
module VmapModule =

    /// A very fast persistent hash map.
    /// Works in effectively constant-time for look-ups and updates.
    type [<NoComparison>] Vmap<'k, 'v when 'k : comparison> =
        private
            { Node : Vnode<'k, 'v>
              EmptyArray : Vnode<'k, 'v> array }
    
        interface IEnumerable<'k * 'v> with
            member this.GetEnumerator () = (Vnode.toSeq this.Node).GetEnumerator ()
    
        interface IEnumerable with
            member this.GetEnumerator () = (Vnode.toSeq this.Node).GetEnumerator () :> IEnumerator

    [<RequireQualifiedAccess>]
    module Vmap =
    
        /// Create an empty Vmap.
        let makeEmpty () =
            { Node = Vnode.empty
              EmptyArray = Array.create 32 Vnode.empty }
    
        /// Check that a Vmap is empty.
        let isEmpty map =
            Vnode.isEmpty map.Node
    
        /// Check that a Vmap is empty.
        let notEmpty map =
            not ^ Vnode.isEmpty map.Node
    
        /// Add a value with the key to a Vmap.
        let add (k : 'k) (v : 'v) map =
            let hkv = Hkv (k.GetHashCode (), k, v)
            let node = Vnode.add hkv map.EmptyArray 0 map.Node
            { map with Node = node }
    
        /// Add a list of values with associated keys to a Vmap.
        let addMany entries map =
            Seq.fold (fun map (k : 'k, v : 'v) -> add k v map) map entries
    
        /// Remove a value with the given key from a Vmap.
        let remove (k : 'k) map =
            let h = k.GetHashCode ()
            { map with Node = Vnode.remove h k 0 map.Node }
    
        /// Remove all values with the given keys from a Vmap.
        let removeMany keys map =
            Seq.fold (fun map (k : 'k) -> remove k map) map keys
    
        /// Try to find a value with the given key in a Vmap.
        /// Constant-time complexity with approx. 1/3 speed of Dictionary.TryGetValue.
        let tryFind (k : 'k) map : 'v option =
            let h = k.GetHashCode ()
            Vnode.tryFind h k 0 map.Node
            
        /// Find a value with the given key in a Vmap.
        /// Constant-time complexity with approx. 1/3 speed of Dictionary.GetValue.
        let find (k : 'k) map : 'v =
            tryFind k map |> Option.get
    
        /// Check that a Vmap contains a value with the given key.
        let containsKey k map =
            match tryFind k map with
            | Some _ -> true
            | None -> false
            
        /// Combine the contents of two Vmaps, taking an item from the second map in the case of a key conflict.
        let concat map map2 =
            Seq.fold (fun map (k, v) -> add k v map) map map2
    
        /// Fold over a Vmap.
        let fold folder state (map : Vmap<'k, 'v>) =
            Vnode.fold folder state map.Node
    
        /// Map over a Vmap.
        let map mapper (map : Vmap<'k, 'v>) =
            fold
                (fun state k v -> add k (mapper v) state)
                (makeEmpty ())
                map
    
        /// Filter a Vmap.
        let filter pred (map : Vmap<'k, 'v>) =
            fold
                (fun state k v -> if pred k v then add k v state else state)
                (makeEmpty ())
                map
    
        /// Convert a Vmap to a sequence of pairs of keys and values.
        /// NOTE: This function seems to profile as being very slow. I don't know if it's the seq / yields syntax or what.
        /// Don't use it unless you need its laziness or if performance won't be affected significantly.
        let toSeq (map : Vmap<'k, 'v>) =
            map :> IEnumerable<'k * 'v>
    
        /// Convert a sequence of keys and values to a Vmap.
        let ofSeq pairs =
            Seq.fold
                (fun map (k, v) -> add k v map)
                (makeEmpty ())
                pairs

/// A very fast persistent hash map.
/// Works in effectively constant-time for look-ups and updates.
type Vmap<'k, 'v when 'k : comparison> = VmapModule.Vmap<'k, 'v>