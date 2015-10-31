// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime
open System
open Prime

type internal Hkv<'k, 'v when 'k : comparison> =
    struct
        new (k, v) = { H = k.GetHashCode (); K = k; V = v }
        val H : int
        val K : 'k
        val V : 'v
        end

/// TODO: there is an F# issue where UseNullAsTrueValue does not work on unions with 4 or more
/// cases - https://github.com/fsharp/fsharp/issues/510 . Once resolved, should use it and be able
/// to make arrays with Array.zeroCreate instead of Array.create.
/// NOTE: Turning this into a C-style union value will give faster look-up, but slower
/// modification.
type internal Vnode<'k, 'v when 'k : comparison> =
    private
        | Nil
        | Singlet of Hkv<'k, 'v>
        | Multiple of Vnode<'k, 'v> array
        | Clash of Map<'k, 'v>

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module internal Vnode =

    let inline cloneArray (arr : Vnode<'k, 'v> array) : Vnode<'k, 'v> array =
#if SIMPLE_CLONE_ARRAY
        arr.Clone () :?> Vnode<'k, 'v> array
#else // use of this presumes Array.Clone () is slower
        let arr' = Array.zeroCreate 32 // there's an unecessary check against the size here, but that's the only inefficiency
        Array.Copy (arr, 0, arr', 0, 32)
        arr'
#endif

    let inline private hashToIndex h dep =
        (h >>> (dep * 5)) &&& 0x1F

    let rec add (hkv : Hkv<'k, 'v>) (earr : Vnode<'k, 'v> array) (mdep : int) (dep : int) (node : Vnode<'k, 'v>) : Vnode<'k, 'v> =
        if dep <= mdep then

            // handle non-clash cases
            match node with
            | Nil ->

                // make singleton entry
                Singlet hkv

            | Singlet hkv' ->
                
                // additional entry; convert Singlet to Multiple
                let idx = hashToIndex hkv.H dep
                let idx' = hashToIndex hkv'.H dep
                if idx <> idx' then
                    let arr = cloneArray earr
                    arr.[idx] <- Singlet hkv
                    arr.[idx'] <- Singlet hkv'
                    Multiple arr

                // replace entry; remain Singlet
                elif hkv.K = hkv'.K then
                    Singlet hkv

                // add entry with same idx
                else
                    let dep' = dep + 1
                    let node' = add hkv earr mdep dep' Nil
                    let node' = add hkv' earr mdep dep' node'
                    let arr = cloneArray earr
                    arr.[idx'] <- node'
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

        else
            
            // handle clash cases
            match node with
            | Nil ->
                Clash ^ Map.singleton hkv.K hkv.V
            | Singlet hkv' ->
                Clash ^ Map.add hkv.K hkv.V ^ Map.singleton hkv'.K hkv'.V
            | Multiple _ ->
                failwithumf ()
            | Clash clashMap ->
                Clash ^ Map.add hkv.K hkv.V clashMap

    let empty =
        Nil

/// Variant map.
type Vmap<'k, 'v when 'k : comparison> =
    private
        { Vnode : Vnode<'k, 'v>
          EmptyArray : Vnode<'k, 'v> array
          MaxDepth : int }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Vmap =

    let makeEmpty mdep =
        if mdep > 6 then failwith "Vmap max depth should not be greater than 6."
        elif mdep < 0 then failwith "Vmap max depth should not be less than 0."
        else { Vnode = Vnode.empty; EmptyArray = Array.create 32 Vnode.empty; MaxDepth = mdep }

    let add (k : 'k) (v : 'v) map =
        { map with Vnode = Vnode.add (Hkv (k, v)) map.EmptyArray map.MaxDepth 0 map.Vnode }