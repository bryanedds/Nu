// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime.Samples

(* This file merely contains code that exemplifies the concept of 'type-absorbing' operators. *)

type [<NoEquality; NoComparison>] 'a Absorb =
    { Number : int
      TypeCarrier : 'a -> unit }

    static member make number =
        { Number = number; TypeCarrier = fun (_ : 'a) -> () }

module TypeAbsorption =

    let ( ->- ) (a : 'a Absorb) (b : 'b Absorb) =
        Absorb<'a>.make <| a.Number + b.Number

    let ( -|- ) (a : 'a Absorb) (b : 'a Absorb) =
        Absorb<'a>.make <| a.Number + b.Number

    let ( -<- ) (a : 'a Absorb) (b : 'b Absorb) =
        Absorb<'b>.make <| a.Number + b.Number

    let x = Absorb<obj>.make 0 -|- Absorb<obj>.make 0
    let y = Absorb<int>.make 0 -|- Absorb<int>.make 0
    let z = Absorb<int>.make 0 ->- Absorb<obj>.make 0
    let w = Absorb<obj>.make 0 -<- Absorb<int>.make 0