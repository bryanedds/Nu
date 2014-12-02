namespace Prime
open System
open Prime

[<AutoOpen>]
module AlgebraicStringModule =

    /// Uses an algebraic converter to convert source to a string.
    let acstring (source : obj) =
        let converter = AlgebraicConverter (source.GetType ())
        converter.ConvertToString source