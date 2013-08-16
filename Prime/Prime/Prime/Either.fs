[<AutoOpen>]
module Either

/// Hsakell-style either type.
/// Probably needs some nice operators such as map, <|>, et al.
type Either<'a, 'b> =
    | Left of 'a
    | Right of 'b

/// Lower-case alias like option.
type either<'a, 'b> =
    Either<'a, 'b>