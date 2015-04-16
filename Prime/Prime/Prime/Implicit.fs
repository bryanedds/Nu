// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime

[<AutoOpen>]
module Implicit =

    /// As close as we can get to F# implicits.
    let inline implicit arg =
        (^a : (static member op_Implicit : ^b -> ^a) arg)

(* Example definition of an implicitly-convertible type. *)

namespace Prime.Samples
open Prime

type T =
    { S : string }
    static member op_Implicit s = { S = s }

module ImplicitT =

    let (!!) : string -> T =
        implicit

module ImplicitExample =

    open ImplicitT
    let t = !!"Hello conversion!"