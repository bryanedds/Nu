// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2013.

[<AutoOpenAttribute>]
module MapPlusModule
open System

// A Map with additional key input (such as a version number).
type MapPlus<'p, 'm, 'v when 'm : comparison and 'p : comparison> = Map<'p, Map<'m, 'v>>