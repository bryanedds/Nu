// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

namespace Prime
module DuckTypingExample =

    (* This file merely contains code that exemplifies F#'s compile-time duck-typing. *)

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

    let howard = name { RedDuck.Name = "Howard" }
    let bob = name { BlueDuck.Name = "Bob" }
    let red = quack { RedDuck.Name = "Jim" }
    let blue = quack { BlueDuck.Name = "Fred" }