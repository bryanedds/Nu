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
type internal 'a Hv =
    struct
        new (h, v) = { H = h; V = v }
        val H : int
        val V : 'a
        end

[<AutoOpen>]
module VsetModule =

    /// TODO: there's an F# issue where UseNullAsTrueValue does not work on unions with 4 or more cases
    /// https://github.com/Microsoft/visualfsharp/issues/711 . Once resolved, should use it and be able
    /// to make arrays with Array.zeroCreate alone without also copying over the empty array.
    type [<NoComparison>] private Vnode<'a when 'a : comparison> =
        | Nil
        | Singleton of 'a Hv
        | Multiple of 'a Vnode array
        | Clash of 'a Set

    [<RequireQualifiedAccess>]
    module private Vnode =
    
        /// OPTIMIZATION: Array.Clone () is not used since it's been profiled to be slower
        let inline cloneArray (arr : 'a Vnode array) : 'a Vnode array =
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
            | Singleton hv -> folder state hv.V
            | Multiple arr -> Array.fold (fold folder) state arr
            | Clash clashSet -> Set.fold folder state clashSet
    
        /// NOTE: This function seems to profile as being very slow. I don't know if it's the seq / yields syntax or what.
        let rec toSeq node =
            seq {
                match node with
                | Nil -> yield! Seq.empty
                | Singleton hv -> yield hv.V
                | Multiple arr -> for n in arr do yield! toSeq n
                | Clash clashSet -> yield! Set.toSeq clashSet }
    
        /// OPTIMIZATION: Requires an empty array to use the source of new array clones in order to avoid Array.create.
        let rec add (hv : 'a Hv) (earr : 'a Vnode array) (dep : int) (node : 'a Vnode) : 'a Vnode =
    
            // lower than max depth, non-clashing
            if dep < 8 then
    
                // handle non-clash cases
                match node with
                | Nil ->
    
                    // make singleton entry
                    Singleton hv
    
                | Singleton hv' ->
                    
                    // if additional entry; convert Singleton to Multiple
                    let idx = hashToIndex hv.H dep
                    let idx' = hashToIndex hv'.H dep
                    if idx <> idx' then
                        let arr = cloneArray earr
                        arr.[idx] <- Singleton hv
                        arr.[idx'] <- Singleton hv'
                        Multiple arr
    
                    // if replace entry; remain Singleton
                    elif hv.V == hv'.V then
                        Singleton hv
    
                    // if add entry with same idx; add both in new node
                    else
                        let dep' = dep + 1
                        let node' = add hv earr dep' Nil
                        let node' = add hv' earr dep' node'
                        let arr = cloneArray earr
                        arr.[idx] <- node'
                        Multiple arr
    
                | Multiple arr ->
    
                    // add entry with recursion
                    let idx = hashToIndex hv.H dep
                    let entry = arr.[idx]
                    let arr = cloneArray arr
                    arr.[idx] <- add hv earr (dep + 1) entry
                    Multiple arr
    
                | Clash _ ->
    
                    // logically should never hit here
                    failwithumf ()
    
            // clashing
            else
                
                // handle clash cases
                match node with
                | Nil -> Clash ^ Set.singleton hv.V
                | Singleton hv' -> Clash ^ Set.add hv.V ^ Set.singleton hv'.V
                | Multiple _ -> failwithumf () // should never hit here
                | Clash clashSet -> Clash ^ Set.add hv.V clashSet
    
        let rec remove (h : int) (v : 'a) (dep : int) (node : 'a Vnode) : 'a Vnode =
            match node with
            | Nil -> node
            | Singleton hv -> if hv.V == v then Nil else node
            | Multiple arr ->
                let idx = hashToIndex h dep
                let entry = arr.[idx]
                let arr = cloneArray arr
                arr.[idx] <- remove h v (dep + 1) entry
                if Array.forall isEmpty arr then Nil else Multiple arr // does not collapse Multiple to Singleton, tho could?
            | Clash clashSet ->
                let clashSet = Set.remove v clashSet
                if Set.isEmpty clashSet then Nil else Clash clashSet

        let rec contains (h : int) (v : 'a) (dep : int) (node : 'a Vnode) =
            match node with
            | Nil -> false
            | Singleton hv -> hv.V == v
            | Multiple arr -> let idx = hashToIndex h dep in contains h v (dep + 1) arr.[idx]
            | Clash clashSet -> Set.contains v clashSet
    
        let empty =
            Nil

    /// A very fast persistent hash set.
    /// Works in effectively constant-time for look-ups and updates.
    type [<NoComparison>] Vset<'a when 'a : comparison> =
        private
            { Node : 'a Vnode
              EmptyArray : 'a Vnode array }
    
        interface 'a IEnumerable with
            member this.GetEnumerator () = (Vnode.toSeq this.Node).GetEnumerator ()
    
        interface IEnumerable with
            member this.GetEnumerator () = (Vnode.toSeq this.Node).GetEnumerator () :> IEnumerator

    [<RequireQualifiedAccess>]
    module Vset =
    
        /// Create an empty Vset.
        let makeEmpty () =
            { Node = Vnode.empty
              EmptyArray = Array.create 32 Vnode.empty }
    
        /// Check that a Vset is empty.
        let isEmpty set =
            Vnode.isEmpty set.Node
    
        /// Check that a Vset is empty.
        let notEmpty set =
            not ^ Vnode.isEmpty set.Node
    
        /// Add a value with the key to a Vset.
        let add (value : 'a) set =
            let hv = Hv (value.GetHashCode (), value)
            let node = Vnode.add hv set.EmptyArray 0 set.Node
            { set with Node = node }
    
        /// Add a list of values with associated keys to a Vset.
        let addMany entries set =
            Seq.fold (fun set (v : 'a) -> add v set) set entries
    
        /// Remove a value with the given key from a Vset.
        let remove (value : 'a) set =
            let h = value.GetHashCode ()
            { set with Vset.Node = Vnode.remove h value 0 set.Node }
    
        /// Remove all values with the given keys from a Vset.
        let removeMany keys set =
            Seq.fold (fun set (v : 'a) -> remove v set) set keys
    
        /// Check that a Vset contains a value.
        let contains value set =
            let h = value.GetHashCode ()
            Vnode.contains h value 0 set.Node
            
        /// Combine the contents of two Vsets.
        let concat set set2 =
            Seq.fold (fun set v -> add v set) set set2
    
        /// Fold over a Vset.
        let fold folder state (set : 'a Vset) =
            Vnode.fold folder state set.Node
    
        /// Map over a Vset.
        let map mapper (set : 'a Vset) =
            fold
                (fun state v -> add (mapper v) state)
                (makeEmpty ())
                set
    
        /// Filter a Vset.
        let filter pred (set : 'a Vset) =
            fold
                (fun state v -> if pred v then add v state else state)
                (makeEmpty ())
                set
    
        /// Convert a Vset to a sequence of pairs of keys and values.
        /// NOTE: This function seems to profile as being very slow. I don't know if it's the seq / yields syntax or what.
        /// Don't use it unless you need its laziness or if performance won't be affected significantly.
        let toSeq (set : 'a Vset) =
            set :> 'a IEnumerable
    
        /// Convert a sequence of keys and values to a Vset.
        let ofSeq pairs =
            Seq.fold
                (fun set v -> add v set)
                (makeEmpty ())
                pairs

/// A very fast persistent hash set.
/// Works in effectively constant-time for look-ups and updates.
type Vset<'a when 'a : comparison> = 'a VsetModule.Vset