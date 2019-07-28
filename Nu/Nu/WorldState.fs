// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open Nu
module State =

    let peek f = fun (world : World) -> (f world, world)
    let poke f = fun (world : World) -> ((), f world) : unit * World
    let exec state world = FSharpx.State.exec world state
    let eval state world = FSharpx.State.eval world state
    let getState = FSharpx.State.getState
    let putState = FSharpx.State.putState
    let state = FSharpx.State.state

    // Here I'm sampling the state monad syntax for Nu.
    // I'm not completely happy with it as its rather noisy compared to Haskell.
    let private stateTest layer =
        state {
            let! entity = World.createEntity None NoOverlay layer
            let! i = peek $ entity.Depth + 1.0f
            do! poke $ entity.Depth += 1.0f
            return (i, entity) }