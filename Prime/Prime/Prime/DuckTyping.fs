// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

namespace Prime
module DuckTypingExample =

    (* This file merely contains code that exemplifies F#'s compile-time duck-typing, AKA,
    Structural typing. *)

    type RedDuck =
        { Name : string }
        member this.Quack () = "Red"

    type BlueDuck =
        { Name : string }
        member this.Quack () = "Blue"

    let inline name this =
        (^a : (member Name : string) this)

    let inline quack this =
        (^a : (member Quack : unit -> string) this)

    let inline nameAndQuack this =
        let name = (^a : (member Name : string) this)
        let quack = (^a : (member Quack : unit -> string) this)
        name + " " + quack

    let howard = name { RedDuck.Name = "Howard" }
    let bob = name { BlueDuck.Name = "Bob" }
    let red = quack { RedDuck.Name = "Jim" }
    let blue = quack { BlueDuck.Name = "Fred" }
    let naq = nameAndQuack { BlueDuck.Name = "Fred" }

// TODO: move this into its own file
module TypeAbsorption =

    type [<NoEquality; NoComparison>] 't Absorb =
        { Number : int
          TypeCarrier : 't -> unit }

        static member make number =
            { Number = number; TypeCarrier = fun (_ : 't) -> () }

    let ( ->- ) (a : 'a Absorb) (b : 'b Absorb) =
        Absorb<'a>.make <| a.Number + b.Number

    let ( -|- ) (a : 't Absorb) (b : 't Absorb) =
        Absorb<'t>.make <| a.Number + b.Number

    let ( -<- ) (a : 'a Absorb) (b : 'b Absorb) =
        Absorb<'b>.make <| a.Number + b.Number

    let x = Absorb<obj>.make 0 -|- Absorb<obj>.make 0
    let y = Absorb<int>.make 0 -|- Absorb<int>.make 0
    let z = Absorb<int>.make 0 ->- Absorb<obj>.make 0
    let w = Absorb<obj>.make 0 -<- Absorb<int>.make 0