[<AutoOpen>]
module Nu.WorldLens
open FSharpx
open Nu.World

// NOTE: there seems to be an interesting, if very fruitful, connection between lenses and
// networking. Further, since there is a definitely fruitful connection between lenses and the
// State monad, there might even be huge benefits by implementing networking in terms of both
// lenses and the State monad. Of course, working in a Monad is somewhat painful, so decisions
// based on that trade-off (and others) would still need to be made.

/// Lens for a particular address in a world.
/// There probably shouldn't be a direct coupling to World here, but damned if I can figure out
/// how to decouple it while retaining adequate type information...
let forAddress address =
    { Get = World.tryFind address
      Set = 
          function
          | Some value -> World.set address value
          | None -> World.remove address }