// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime
open System
open Prime

type internal Hkv<'k, 'v when 'k : comparison> =
    struct
        new (h, k, v) = { h = h; k = k; v = v }
        val h : int
        val k : 'k
        val v : 'v
        end

/// TODO: there is an F# issue where UseNullAsTrueValue does not work on unions with 4 or more
/// cases - https://github.com/fsharp/fsharp/issues/510 . Once resolved, should use it and be able
/// to make arrays with Array.zeroCreate instead of Array.create.
/// NOTE: Turning this into a C-style union value will give faster look-up, but slower
/// modification.
type internal Vnode<'k, 'v when 'k : comparison> =
    private
        | Nil
        | Single of Hkv<'k, 'v>
        | Multiple of Vnode<'k, 'v> array
        | Clash of Map<'k, 'v>

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module internal Vnode =

    let private makeArray () =
        Array.create 32 Nil

    let private hashToIndex h dep =
        (h >>> dep * 5) &&& 0x1F

    let rec private add5 (h : int) (k : 'k) (v : 'v) (dep : int) (mdep : int) (node : Vnode<'k, 'v>) : Vnode<'k, 'v> =
        if dep <= mdep then

            // handle non-clash cases
            match node with
            | Nil ->

                // make singleton entry
                Single ^ Hkv (h, k, v)

            | Single hkv ->
                let idx = hashToIndex hkv.h dep
                let idx' = hashToIndex h dep
                
                // additional entry; convert Single to Multiple
                if idx <> idx' then
                    let arr = makeArray ()
                    arr.[idx] <- Single hkv
                    arr.[idx'] <- Single ^ Hkv (h, k, v)
                    Multiple arr

                // replace entry; remain Single
                elif hkv.k = k then
                    Single ^ Hkv (h, k, v)

                // add entry with same idx
                else
                    let node' = add5 hkv.h hkv.k hkv.v (inc dep) mdep Nil
                    let node' = add5 h k v (inc dep) mdep node'
                    let arr = makeArray ()
                    arr.[idx] <- node'
                    Multiple arr

            | Multiple arr ->

                // add entry with recursion
                let idx = hashToIndex h dep
                let entry = arr.[idx]
                let arr = arr.Clone () :?> Vnode<'k, 'v> array
                arr.[idx] <- add5 h k v (inc dep) mdep entry
                Multiple arr

            | Clash _ ->

                // logically should never hit here
                failwithumf ()

        else
            
            // handle clash cases
            match node with
            | Nil ->
                Clash ^ Map.singleton k v
            | Single hkv ->
                Clash ^ Map.add k v ^ Map.singleton hkv.k hkv.v
            | Multiple _ ->
                failwithumf ()
            | Clash clashMap ->
                Clash ^ Map.add k v clashMap

    let add k v node =
        add5 (k.GetHashCode ()) k v 0 node

    let empty =
        Nil

/// Variant map.
type Vmap<'k, 'v when 'k : comparison> =
    private
        { Mdep : int
          Vnode : Vnode<'k, 'v> }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Vmap =

    let makeEmpty mdep =
        if mdep > 6 then failwith "Vmap max depth should not be greater than 6."
        elif mdep < 0 then failwith "Vmap max depth should not be less than 0."
        else { Mdep = mdep; Vnode = Vnode.empty }

    let add k v map =
        { map with Vnode = Vnode.add k v map.Mdep map.Vnode }