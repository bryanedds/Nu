// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

[<AutoOpen>]
module MapOperators 

/// Combine the contents of two maps, taking an item from the second map in case of a key overlap.
let (@@) map map2 =
    Map.concat map map2