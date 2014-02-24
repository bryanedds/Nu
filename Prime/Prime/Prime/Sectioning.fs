[<AutoOpen>]
module Sectioning

let inline incUy (n : byte) = n + 1uy
let inline decUy (n : byte) = n - 1uy
let inline incY (n : sbyte) = n + 1y
let inline decY (n : sbyte) = n - 1y
let inline incI (n : int) = n + 1
let inline decI (n : int) = n - 1
let inline incU (n : uint32) = n + 1u
let inline decU (n : uint32) = n - 1u
let inline incL (n : int64) = n + 1L
let inline decL (n : int64) = n - 1L
let inline incUl (n : uint64) = n + 1UL
let inline decUl (n : uint64) = n - 1UL
let inline cons head tail = head :: tail
let plus = (+)
let minus = (-)
let mul = ( * )
let div = (/)