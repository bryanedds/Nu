[<AutoOpen>]
module MapOperators 

/// Combine the contents of two maps, taking an item from the second map in the case of a key
/// conflict.
let inline (@@) map map2 =
    Map.concat map map2