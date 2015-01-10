// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

namespace Prime
module ImmutableSubclass =

    (* This file merely contains code that demonstrates how to build immutable class families. *)

    type Class (f : int) =

        member this.F = f
        member this.SetF f = this.Clone f

        abstract Clone : int -> Class
        default this.Clone f = Class f

    type Subclass (f : int, g : int) =
        inherit Class (f)

        member this.G = g
        member this.SetG g = this.Clone (f, g)

        override this.Clone f = Subclass (f, g) :> Class
        abstract Clone : int * int -> Subclass
        default this.Clone (f, g) = Subclass (f, g)