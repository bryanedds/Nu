[<AutoOpen>]
module Sectioning

let incUy (n : byte) = n + 1uy
let decUy (n : byte) = n - 1uy
let incY (n : sbyte) = n + 1y
let decY (n : sbyte) = n - 1y
let incI (n : int) = n + 1
let decI (n : int) = n - 1
let incU (n : uint32) = n + 1u
let decU (n : uint32) = n - 1u
let incL (n : int64) = n + 1L
let decL (n : int64) = n - 1L
let incUl (n : uint64) = n + 1UL
let decUl (n : uint64) = n - 1UL
let plus = (+)
let minus = (-)
let mul = ( * )
let div = (/)
let cons head tail = head :: tail