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

    let private makeArray () =
        Array.create 32 Nil

    let private hashToIndex h dep =
        (h >>> (dep * 5)) &&& 0x1F

    let rec private add5 (hkv : Hkv<'k, 'v>) (dep : int) (mdep : int) (node : Vnode<'k, 'v>) : Vnode<'k, 'v> =
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
                    let arr = makeArray ()
                    arr.[idx] <- Singlet hkv
                    arr.[idx'] <- Singlet hkv'
                    Multiple arr

                // replace entry; remain Singlet
                elif hkv.K = hkv'.K then
                    Singlet hkv

                // add entry with same idx
                else
                    let dep' = dep + 1
                    let node' = add5 hkv dep' mdep Nil
                    let node' = add5 hkv' dep' mdep node'
                    let arr = makeArray ()
                    arr.[idx'] <- node'
                    Multiple arr

            | Multiple arr ->

                // add entry with recursion
                let idx = hashToIndex hkv.H dep
                let entry = arr.[idx]
                let arr = arr.Clone () :?> Vnode<'k, 'v> array
                arr.[idx] <- add5 hkv (dep + 1) mdep entry
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

    let add k v node =
        add5 (Hkv (k, v)) 0 node

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