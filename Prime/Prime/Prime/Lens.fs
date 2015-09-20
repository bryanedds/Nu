// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace FSharpx
open FSharpx
module Lens =

    /// Update lens with subject in tow.
    let updateS updater lens subject =
        let value = lens.Get subject
        let value = updater value subject
        lens.Set value subject

    /// Combine two lenses into a single subject.
    let fuse (lens : Lens<'s, 'a>) (lens2 : Lens<'s, 'b>) =
        { Get = fun s -> (lens.Get s, lens2.Get s)
          Set = fun (a, b) s -> let s = lens.Set a s in lens2.Set b s }

[<AutoOpen>]
module LensOperators =

    /// Combine two lenses into a single subject.
    let (@->) (lens : Lens<'s, 'a>) (lens2 : Lens<'s, 'b>) =
        Lens.fuse lens lens2