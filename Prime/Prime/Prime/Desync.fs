namespace Prime
open Prime

[<AutoOpen>]
module DesyncModule =

    type [<NoComparison; NoEquality>] Desync<'e, 's, 'a> =
        Desync of ('s -> 's * Either<'e -> Desync<'e, 's, 'a>, 'a>)

    let internal step (m : Desync<'e, 's, 'a>) (s : 's) : 's * Either<'e -> Desync<'e, 's, 'a>, 'a> =
        match m with Desync f -> f s

    let internal returnM (a : 'a) : Desync<'e, 's, 'a> =
        Desync (fun s -> (s, Right a))
        
    let rec internal bind (m : Desync<'e, 's, 'a>) (cont : 'a -> Desync<'e, 's, 'b>) : Desync<'e, 's, 'b> =
        Desync (fun s ->
            match (match m with Desync f -> f s) with
            //                              ^--- unbounded recursion here
            | (s', Left m') -> (s', Left (fun e -> bind (m' e) cont))
            | (s', Right v) -> match cont v with Desync f -> f s')

    type DesyncBuilder () =

        member this.Return op = returnM op
        member this.Bind (m, cont) = bind m cont

    let desync =
        DesyncBuilder ()

module Desync =

    let returnM =
        returnM

    let waitE (m : 'e -> Desync<'e, 's, 'a>) : Desync<'e, 's, 'a> =
        Desync (fun s -> (s, Left m))

    let wait (m : Desync<'e, 's, 'a>) =
        Desync (fun s -> (s, Left (fun _ -> m)))

    let get : Desync<'e, 's, 's> =
        Desync (fun s -> (s, Right s))

    let getBy by : Desync<'e, 's, 'a> =
        Desync (fun s -> (s, Right <| by s))

    let set s : Desync<'e, 's, unit> =
        Desync (fun _ -> (s, Right ()))

    let advance (m : 'e -> Desync<'e, 's, 'a>) (e : 'e) (s : 's) : 's * Either<'e -> Desync<'e, 's, 'a>, 'a> =
        step (m e) s

    let rec run (m : Desync<'e, 's, 'a>) (e : 'e) (s : 's) : ('s * 'a) =
        match step m s with
        | (s', Left m') -> run (m' e) e s'
        | (s', Right v) -> (s', v)

    let nextE () : Desync<'e, 's, 'e> =
        Desync (fun s -> (s, Left returnM))

    let next () : Desync<'e, 's, unit> =
        Desync (fun s -> (s, Left (fun _ -> returnM ())))

    let reactE expr : Desync<'e, 's, unit> =
        desync {
            let! e = nextE ()
            let! s = get
            let s = expr e s
            do! set s }

    let react expr : Desync<'e, 's, unit> =
        desync {
            do! next ()
            let! s = get
            let s = expr s
            do! set s }

    let updateBy by expr : Desync<'e, 's, unit> =
        Desync (fun s -> (expr (by s) s, Right ()))

    let update expr : Desync<'e, 's, unit> =
        Desync (fun s -> (expr s, Right ()))

    let rec loop (i : 'i) (next : 'i -> 'i) (pred : 'i -> 's -> bool) (m : 'i -> Desync<'e, 's, unit>) =
        desync {
            let! s = get
            do! if pred i s then
                    desync {
                        do! m i
                        let i = next i
                        do! loop i next pred m }
                else returnM () }

    let during (pred : 's -> bool) (m : Desync<'e, 's, unit>) =
        loop () id (fun _ -> pred) (fun _ -> m)