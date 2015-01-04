namespace Prime
open Prime

[<AutoOpen>]
module DesyncModule =

    /// The Desync monad. Allows the user to define in a sequential style an operation that takes
    /// place across multiple events.
    type [<NoComparison; NoEquality>] Desync<'r, 'e, 's, 'a> =
        Desync of ('s -> ('s -> ('e -> Desync<'r, 'e, 's, 'a>) -> 'r) -> ('s -> 'a -> 'r) -> 'r)

    /// Monadic return for the Desync monad.
    let internal returnM (a : 'a) : Desync<'r, 'e, 's, 'a> = Desync <| fun s _ dunn -> dunn s a

    /// Monadic bind for the Desync monad.
    let rec internal bind (m : Desync<'r, 'e, 's, 'a>) (cont : 'a -> Desync<'r, 'e, 's, 'b>) : Desync<'r, 'e, 's, 'b> =
        let h = match m with Desync h -> h
        Desync
            (fun s wait dunn ->
                h s (fun s w ->
                    wait
                        s
                        (fun e -> bind (w e) cont))
                        (fun s a -> match cont a with Desync h -> h s wait dunn))

    /// Builds the Desync monad.
    type DesyncBuilder () =
        member this.Return a = returnM a
        member this.Bind (m, cont) = bind m cont

    /// The Desync builder.
    let desync = DesyncBuilder ()

module Desync =

    /// Monadic return for the Desync monad.
    let returnM = returnM

    /// Monadic bind for the Desync monad.
    let bind = bind

    let wait : Desync<'r, 'e, 's, 'e> =
        Desync (fun s w _ -> w s (fun e -> returnM e))

    let wait_ : Desync<'r, 'e, 's, unit> =
        Desync (fun s w _ -> w s (fun _ -> returnM ()))

    let get : Desync<'r, 'e, 's, 's> =
        Desync (fun s _ d -> d s s)

    let getBy by : Desync<'r, 'e, 's, 'a> =
        Desync (fun s _ d -> d s (by s))

    let set (s : 's) : Desync<'r, 'e, 's, unit> =
        Desync (fun _ _ d -> d s ())

    let setBy by (s : 's) : Desync<'r, 'e, 's, unit> =
        Desync (fun _ _ d -> d (by s) ())

    let updateBy by updater : Desync<'r, 'e, 's, unit> =
        Desync (fun s _ d -> d (updater (by s) s) ())

    let update updater : Desync<'r, 'e, 's, unit> =
        Desync (fun s _ d -> d (updater s) ())

    let react expr : Desync<'r, 'e, 's, unit> =
        desync {
            let! e = wait
            let! s = get
            let s = expr e s
            do! set s }
    
    let react_ expr : Desync<'r, 'e, 's, unit> =
        desync {
            let! _ = wait
            let! s = get
            let s = expr s
            do! set s }

    let rec loop (i : 'i) (next : 'i -> 'i) (pred : 'i -> 's -> bool) (m : 'i -> Desync<'r, 'e, 's, unit>) =
        desync {
            let! s = get
            do! if pred i s then
                    desync {
                        do! m i
                        let i = next i
                        do! loop i next pred m }
                else returnM () }

    let during (pred : 's -> bool) (m : Desync<'r, 'e, 's, unit>) =
        loop () id (fun _ -> pred) (fun _ -> m)

    let advanceDesync
        (s : 's)
        (wait : 's -> ('e -> Desync<'r, 'e, 's, 'a>) -> 'r)
        (dunn : 's -> 'a -> 'r)
        (m : Desync<'r, 'e, 's, 'a>) :
        'r =
        match m with Desync h -> h s wait dunn

    let rec runDesync
        (e : 'e)
        (s : 's)
        (m : Desync<'s * 'a, 'e, 's, 'a>) :
        's * 'a =
        advanceDesync s (fun s f -> runDesync e s (f e)) (fun s a -> (s, a)) m