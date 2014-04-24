// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

namespace Prime

[<AutoOpen>]
module Implicit =

    /// As close as we can get to F# implicits.
    let inline implicit arg =
        (^a : (static member op_Implicit : ^b -> ^a) arg)

/// Implicit-conversion example.
module Example =

    (*
    Example definition of an implicitly-convertible type.
    *)
    
    type T =
        { S : string }
        static member op_Implicit s = { S = s }

    module ImplicitT =

        let (!!) : string -> T =
            implicit

    (*
    Example usage of implicit type conversion.
    *)

    open ImplicitT
    
    let t = !!"Hello conversion!"