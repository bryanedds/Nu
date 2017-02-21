// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Prime
open System
open System.Collections
open System.Collections.Generic
open Prime

[<AutoOpen>]
module HMapModule =

    /// A hash-key-value triple, implemented with a struct for efficiency.
    type private Hkv<'k, 'v when 'k :> IEquatable<'k>> =
        struct
            new (b, h, k, v) = { B = b; H = h; K = k; V = v }
            val B : bool
            val H : int
            val K : 'k
            val V : 'v
            end

    /// TODO: P1: there's an F# issue where UseNullAsTrueValue does not work on unions with 4 or
    /// more cases https://github.com/Microsoft/visualfsharp/issues/711 . Once resolved, should use
    /// it and be able to make arrays with Array.zeroCreate alone without also copying over the
    /// empty array.
    type [<NoComparison>] private HNode<'k, 'v when 'k :> IEquatable<'k>> =
        | Nil
        | Singleton of Hkv<'k, 'v>
        | Multiple of HNode<'k, 'v> array
        | Gutter of Hkv<'k, 'v> array

    [<RequireQualifiedAccess>]
    module private HNode =

        let inline failwithKeyNotFound (k : 'k) =
            raise ^ KeyNotFoundException ^ "Could not find HMap key '" + k.ToString () + "'."

        /// OPTIMIZATION: Array.Clone () is not used since it's been profiled to be slower
        let inline private cloneArray (arr : HNode<'k, 'v> array) =
            let arr' = Array.zeroCreate 16 : HNode<'k, 'v> array    // NOTE: there's an unecessary check against the size here, but that's the only inefficiency
                                                                    // TODO: P1: use Array.zeroCreateUnchecked if / when it becomes available
            Array.Copy (arr, 0, arr', 0, 16) // param checks are inefficient, but hopefully there's at least a memcpy underneath...
            arr'
    
        let inline private hashToIndex h dep =
            (h >>> (dep * 4)) &&& 0xF

        let private addToGutter (entry : Hkv<'k, 'v>) (gutter : Hkv<'k, 'v> array) =
            let gutterLength = gutter.Length
            let gutter2 = Array.zeroCreate 16 : Hkv<'k, 'v> array
            Array.Copy (gutter, 0, gutter2, 0, gutterLength)
            gutter2.[gutterLength] <- entry
            gutter2

        let private removeFromGutter (k : 'k) (gutter : Hkv<'k, 'v> array) =
            match Array.FindLastIndex (gutter, fun (entry2 : Hkv<'k, 'v>) -> entry2.B && entry2.K.Equals k) with
            | -1 -> gutter
            | index ->
                let gutter2 = Array.zeroCreate (dec gutter.Length) : Hkv<'k, 'v> array
                Array.Copy (gutter, 0, gutter2, 0, index)
                Array.Copy (gutter, inc index, gutter2, index, gutter2.Length - index)
                gutter2

        let private tryFindInGutter (k : 'k) (gutter : Hkv<'k, 'v> array) =
            match Array.FindLastIndex (gutter, fun (entry2 : Hkv<'k, 'v>) -> entry2.B && entry2.K.Equals k) with
            | -1 -> FOption.none ()
            | index -> FOption.some gutter.[index].V

        let empty =
            Nil

        let isEmpty node =
            match node with
            | Nil -> true
            | _ -> false
    
        /// OPTIMIZATION: Requires an empty array to use the source of new array clones in order to avoid Array.create.
        let rec add (hkv : Hkv<'k, 'v>) (earr : HNode<'k, 'v> array) (dep : int) (node : HNode<'k, 'v>) : HNode<'k, 'v> =
    
            // lower than max depth, non-clashing
            if dep < 9 then
    
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
                    elif hkv.K.Equals hkv'.K then
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
    
                | Gutter _ ->
    
                    // logically should never hit here
                    failwithumf ()
    
            // clashing
            else
                
                // handle clash cases
                match node with
                | Nil -> Gutter (Array.singleton hkv)
                | Singleton hkv' -> Gutter [|hkv'; hkv|]
                | Multiple _ -> failwithumf () // should never hit here
                | Gutter gutter -> Gutter (addToGutter hkv gutter)
    
        let rec remove (h : int) (k : 'k) (dep : int) (node : HNode<'k, 'v>) : HNode<'k, 'v> =
            match node with
            | Nil -> node
            | Singleton hkv -> if hkv.K.Equals k then Nil else node
            | Multiple arr ->
                let idx = hashToIndex h dep
                let entry = arr.[idx]
                let arr = cloneArray arr
                arr.[idx] <- remove h k (dep + 1) entry
                if Array.forall isEmpty arr then Nil else Multiple arr // does not collapse Multiple to Singleton, tho could?
            | Gutter gutter ->
                let gutter = removeFromGutter k gutter
                if Array.isEmpty gutter then Nil else Gutter gutter
    
        let rec tryFindFast (h : int) (k : 'k) (dep : int) (node : HNode<'k, 'v>) : 'v FOption =
            match node with
            | Nil -> FOption.none ()
            | Singleton hkv -> if hkv.K.Equals k then FOption.some hkv.V else FOption.none ()
            | Multiple arr -> let idx = hashToIndex h dep in tryFindFast h k (dep + 1) arr.[idx]
            | Gutter gutter -> tryFindInGutter k gutter
    
        let rec find (h : int) (k : 'k) (dep : int) (node : HNode<'k, 'v>) : 'v =
            match node with
            | Nil -> failwithKeyNotFound k
            | Singleton hkv -> if hkv.K.Equals k then hkv.V else failwithKeyNotFound k
            | Multiple arr -> let idx = hashToIndex h dep in find h k (dep + 1) arr.[idx]
            | Gutter gutter -> FOption.get (tryFindInGutter k gutter)
    
        let rec fold folder state node =
            match node with
            | Nil -> state
            | Singleton hkv -> folder state hkv.K hkv.V
            | Multiple arr -> Array.fold (fold folder) state arr
            | Gutter gutter -> Array.fold (fun state (hkv : Hkv<_, _>) -> folder state hkv.K hkv.V) state gutter
    
        /// NOTE: This function seems to profile as being very slow. I don't know if it's the seq / yields syntax or what.
        let rec toSeq node =
            seq {
                match node with
                | Nil -> yield! Seq.empty
                | Singleton hkv -> yield (hkv.K, hkv.V)
                | Multiple arr -> for n in arr do yield! toSeq n
                | Gutter gutter -> yield! Array.map (fun (hkv : Hkv<_, _>) -> (hkv.K, hkv.V)) gutter }

    /// A fast persistent hash map.
    /// Works in effectively constant-time for look-ups and updates.
    type [<NoComparison>] HMap<'k, 'v when 'k :> IEquatable<'k>> =
        private
            { Node : HNode<'k, 'v>
              EmptyArray : HNode<'k, 'v> array }
    
        interface IEnumerable<'k * 'v> with
            member this.GetEnumerator () = (HNode.toSeq this.Node).GetEnumerator ()
    
        interface IEnumerable with
            member this.GetEnumerator () = (HNode.toSeq this.Node).GetEnumerator () :> IEnumerator

    [<RequireQualifiedAccess>]
    module HMap =
    
        /// Create an empty HMap.
        let makeEmpty () =
            { Node = HNode.empty
              EmptyArray = Array.create 16 HNode.empty }

        /// Check that an HMap is empty.
        let isEmpty map =
            HNode.isEmpty map.Node
    
        /// Check that an HMap is empty.
        let notEmpty map =
            not ^ HNode.isEmpty map.Node
    
        /// Add a value with the key to an HMap.
        let add (key : 'k) (value : 'v) map =
            let hkv = Hkv (true, key.GetHashCode (), key, value)
            let node = HNode.add hkv map.EmptyArray 0 map.Node
            { map with Node = node }
    
        /// Remove a value with the given key from an HMap.
        let remove (key : 'k) map =
            let h = key.GetHashCode ()
            { map with Node = HNode.remove h key 0 map.Node }
    
        /// Add all the given entries to an HMap.
        let addMany entries map =
            Seq.fold (fun map (key : 'k, value : 'v) -> add key value map) map entries
    
        /// Remove all values with the given keys from an HMap.
        let removeMany keys map =
            Seq.fold (fun map (key : 'k) -> remove key map) map keys
    
        /// Try to find a value with the given key in an HMap.
        /// Constant-time complexity with approx. 1/3 speed of Dictionary.TryGetValue.
        let tryFindFast (key : 'k) map : 'v FOption =
            let h = key.GetHashCode ()
            HNode.tryFindFast h key 0 map.Node

        /// Try to find a value with the given key in an HMap.
        /// Constant-time complexity with approx. 1/3 speed of Dictionary.TryGetValue.
        let tryFind (key : 'k) map : 'v option =
            let fopt = tryFindFast key map
            FOption.toOpt fopt

        /// Find a value with the given key in an HMap.
        /// Constant-time complexity with approx. 1/3 speed of Dictionary.GetValue.
        let find (key : 'k) map : 'v =
            let h = key.GetHashCode ()
            HNode.find h key 0 map.Node
    
        /// Check that an HMap contains a value with the given key.
        let containsKey key map =
            let opt = tryFindFast key map
            FOption.isSome opt
            
        /// Combine the contents of two HMaps, taking an item from the second map in the case of a key conflict.
        let concat map map2 =
            Seq.fold (flip ^ uncurry add) map map2
    
        /// Fold over an HMap.
        let fold folder state (map : HMap<'k, 'v>) =
            HNode.fold folder state map.Node
    
        /// Map over an HMap.
        let map mapper (map : HMap<'k, 'v>) =
            fold
                (fun state key value -> add key (mapper value) state)
                (makeEmpty ())
                map
    
        /// Filter an HMap.
        let filter pred (map : HMap<'k, 'v>) =
            fold
                (fun state key value -> if pred key value then add key value state else state)
                (makeEmpty ())
                map
    
        /// Convert an HMap to a sequence of pairs of keys and values.
        /// NOTE: This function seems to profile as being very slow. I don't know if it's the seq / yields syntax or what.
        /// Don't use it unless you need its laziness or if performance won't be affected significantly.
        let toSeq (map : HMap<'k, 'v>) =
            map :> IEnumerable<'k * 'v>
    
        /// Convert a sequence of keys and values to an HMap.
        let ofSeq pairs =
            Seq.fold
                (fun map (key, value) -> add key value map)
                (makeEmpty ())
                pairs

/// A very fast persistent hash map.
/// Works in effectively constant-time for look-ups and updates.
type HMap<'k, 'v when 'k :> IEquatable<'k>> = HMapModule.HMap<'k, 'v>