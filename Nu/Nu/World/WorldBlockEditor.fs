// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module BlockMapFacetExtensions =
    type Entity with
        member this.GetBlockEditor world : BlockEditor = this.Get (nameof BlockEditor) world
        member this.SetBlockEditor (value : BlockEditor) world = this.Set (nameof BlockEditor) value world
        member this.BlockEditor = lens (nameof BlockEditor) this this.GetBlockEditor this.SetBlockEditor

type BlockMapFacet () =
    inherit Facet (false, false, false)

    static member Properties =
        [define Entity.BlockEditor BlockEditor.initial]