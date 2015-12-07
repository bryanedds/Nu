// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime
open System
open System.Collections
open System.Collections.Generic
open System.ComponentModel
open System.Reflection
open Microsoft.FSharp.Reflection
open Prime

/// Converts Vmap types.
type VmapConverter (targetType : Type) =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<string> ||
        destType = targetType

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<string> then
            let toStringMethod = targetType.GetMethod "ToString"
            toStringMethod.Invoke (source, null)
        elif destType = targetType then source
        else failwith "Invalid VmapConverter conversion to source."

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<string> ||
        sourceType = targetType

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? string as vmapStr ->
            let gargs = targetType.GetGenericArguments ()
            match gargs with
            | [|fstType; sndType|] ->
                let pairList = acvalue<obj list> vmapStr
                let pairType = typedefof<Tuple<_, _>>.MakeGenericType [|fstType; sndType|]
                let ofSeq = (((Assembly.GetExecutingAssembly ()).GetType "Prime.VmapModule").GetMethod ("ofSeq", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|fstType; sndType|]
                let cast = (typeof<System.Linq.Enumerable>.GetMethod ("Cast", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|pairType|]
                ofSeq.Invoke (null, [|cast.Invoke (null, [|pairList|])|])
            | _ -> failwith "Unexpected match failure in Nu.VmapConverter.ConvertFrom."
        | _ ->
            if targetType.IsInstanceOfType source then source
            else failwith "Invalid AddressConverter conversion from source."

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

/// A highly-optimized persistent hash map.
/// TODO: document.
/// TODO: implement filter.
type [<NoEquality; NoComparison; TypeConverter (typeof<VmapConverter>)>] Vmap<'k, 'v when 'k : comparison> =
    private
        { Node : Vnode<'k, 'v>
          EmptyArray : Vnode<'k, 'v> array }

    interface IEnumerable<'k * 'v> with
        member this.GetEnumerator () = (Vnode.toSeq this.Node).GetEnumerator ()

    interface IEnumerable with
        member this.GetEnumerator () = (Vnode.toSeq this.Node).GetEnumerator () :> IEnumerator

    override this.ToString () =
        let pairList = this :> _ seq |> List.ofSeq
        acstring pairList

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Vmap =

    let isEmpty map =
        Vnode.isEmpty map.Node

    let make () =
        { Node = Vnode.empty
          EmptyArray = Array.create 32 Vnode.empty }

    let add (k : 'k) (v : 'v) map =
        let hkv = Hkv (k.GetHashCode (), k, v)
        let node = Vnode.add hkv map.EmptyArray 0 map.Node
        { map with Node = node }

    let addMany entries map =
        Seq.fold (fun map (k : 'k, v : 'v) -> add k v map) map entries

    let remove (k : 'k) map =
        let h = k.GetHashCode ()
        { map with Node = Vnode.remove h k 0 map.Node }

    let tryFind (k : 'k) map : 'v option =
        let h = k.GetHashCode ()
        Vnode.tryFind h k 0 map.Node

    let find (k : 'k) map : 'v =
        tryFind k map |> Option.get

    let containsKey k map =
        match tryFind k map with
        | Some _ -> true
        | None -> false
        
    /// Combine the contents of two maps, taking an item from the second map in the case of a key
    /// conflict.
    let concat map map2 =
        Seq.fold (fun map (k, v) -> add k v map) map map2

    /// Fold over a Vmap.
    let fold folder state (map : Vmap<'k, 'v>) =
        Vnode.fold folder state map.Node

    /// Map over a Vmap.
    let map mapper (map : Vmap<'k, 'v>) =
        fold
            (fun state k v -> add k (mapper v) state)
            (make ())
            map

    /// Convert a Vmap to a sequence of pairs of keys and values.
    /// NOTE: This function seems to profile as being very slow. I don't know if it's the seq / yields syntax or what.
    /// Don't use it unless you need its laziness or if performance won't be affected significantly.
    let toSeq (map : Vmap<'k, 'v>) =
        map :> IEnumerable<'k * 'v>

    /// Convert a sequence of keys and values to a Vmap.
    let ofSeq kvps =
        Seq.fold
            (fun map (k, v) -> add k v map)
            (make ())
            kvps