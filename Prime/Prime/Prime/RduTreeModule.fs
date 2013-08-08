// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2013.

[<AutoOpenAttribute>]
module RduTreeModule

/// A tree onto which arbitrary recursive discriminated unions can be projected.
[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type 'a RduTree =
    | Leaf
    | Branch of 'a * 'a RduTree list