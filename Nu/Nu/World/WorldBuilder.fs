namespace Nu

/// Implementation of the state monad, where the state is a World.
type WorldBuilder() =
    // let! x: 'a = y: World -> 'a * World
    member inline _.Bind(m: World -> 'a * World, f: 'a -> World -> 'b * World) : World -> 'b * World =
        fun world ->
            let a, world' = m world
            f a world'
    // do! y: World -> World
    member inline _.Bind(m: World -> World, f: unit -> World -> 'a * World) : World -> 'a * World =
        fun world ->
            let world' = m world
            f () world'
    
    // return
    member inline _.Return(x: 'a) : World -> 'a * World = fun world -> x, world
    // return!
    member inline _.ReturnFrom(m: World -> 'a) = fun world -> m world, world
    member inline _.ReturnFrom(m: World -> World) = fun world -> (), m world
    member inline _.ReturnFrom(m: World -> 'a * World) = m
    // no return nor return!
    member inline _.Zero() : World -> unit * World = fun world -> (), world
    
    // while
    member inline _.While(guard: unit -> bool, body: unit -> World -> unit * World) =
        fun world ->
            let mutable world' = world
            while guard() do
                let (), w = body () world'
                world' <- w
            (), world'
    // for
    member inline _.For(sequence: seq<'a>, body: 'a -> World -> unit * World) : World -> unit * World =
        fun world ->
            let mutable world' = world
            for item in sequence do
                let (), w = body item world'
                world' <- w
            (), world'
    // try ... with
    member inline _.TryWith(r,fn) = try r() with ex -> fn ex

    // necessary for if, while, etc.
    member inline _.Delay(f: unit -> World -> 'a * World) = f
    // Sequencing different lines together
    member inline _.Combine(m1: World -> 'a * World, m2: 'a -> World -> 'b * World) : World -> 'b * World =
        fun world ->
            let a, world' = m1 world
            m2 a world'
    // final value for the computation expression
    member inline _.Run(f: unit -> World -> unit * World) = f() >> snd
    member inline _.Run(f: unit -> World -> 'a * World) = f()
// Using a type extension is needed without https://github.com/fsharp/fslang-suggestions/issues/905
// Otherwise, `let! _ =` will fail with an overload resolution error
module [<AutoOpen; System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)>] WorldBuilder =
    type WorldBuilder with
        // let! x: 'a = y: World -> 'a
        member inline _.Bind(m: World -> 'a, f: 'a -> World -> 'b * World) : World -> 'b * World =
            fun world ->
                let a = m world
                f a world
    /// Using this means you lose 1) understandable compiler error messages 2) debug stepping and variable information,
    /// in exchange for a more convenient syntax that elides the need to redefine world on every line.
    let world = WorldBuilder()
