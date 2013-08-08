// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2013.

[<AutoOpenAttribute>]
module LunTriePlusModule
open System

// A LunTrie with additional key input (such as a version number).
type LunTriePlus<'p, 'v when 'p : comparison> = Map<'p, 'v LunTrie>