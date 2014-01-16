module Xtension

/// The empty extension.
let empty = { OptName = None; Fields = Map.empty }

// Some syntactic tests.
let test = { OptName = None; Fields = Map.empty }
let test' = test?SomeField <- 0
let test_ = test'?SomeField () : int
let test'' = test'?SomeMethod <| xdc Map.empty : unit