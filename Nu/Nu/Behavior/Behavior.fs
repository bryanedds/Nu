namespace Nu
open System
open System.Numerics
open FSharp.Core
open Prime

/// Provides for time-driven behavior.
type 'a Behavior = GameTime -> 'a

[<RequireQualifiedAccess>]
module Behavior =

    let map<'a, 'b> mapper (bhvr : 'a Behavior) : 'b Behavior =
        bhvr >> mapper

    let lift1<'a, 'b>
        (op : 'a -> 'b)
        (bhvr : 'a Behavior) :
        Behavior<'b> =
        map op bhvr

    let lift2<'a, 'b, 'c>
        (op : 'a -> 'b -> 'c)
        (bhvr : 'a Behavior)
        (behv2 : 'b Behavior) :
        'c Behavior =
        fun time ->
            let a = bhvr time
            let b = behv2 time
            op a b

    let lift3<'a, 'b, 'c, 'd>
        (op : 'a -> 'b -> 'c -> 'd)
        (bhvr : 'a Behavior)
        (behv2 : 'b Behavior)
        (behv3 : 'c Behavior) :
        'd Behavior =
        fun time ->
            let a = bhvr time
            let b = behv2 time
            let c = behv3 time
            op a b c

    let lift4<'a, 'b, 'c, 'd, 'e>
        (op : 'a -> 'b -> 'c -> 'd -> 'e)
        (bhvr : 'a Behavior)
        (behv2 : 'b Behavior)
        (behv3 : 'c Behavior)
        (behv4 : 'd Behavior) :
        'e Behavior =
        fun time ->
            let a = bhvr time
            let b = behv2 time
            let c = behv3 time
            let d = behv4 time
            op a b c d

    let lift5<'a, 'b, 'c, 'd, 'e, 'f>
        (op : 'a -> 'b -> 'c -> 'd -> 'e -> 'f)
        (bhvr : 'a Behavior)
        (behv2 : 'b Behavior)
        (behv3 : 'c Behavior)
        (behv4 : 'd Behavior)
        (behv5 : 'e Behavior) :
        'f Behavior =
        fun time ->
            let a = bhvr time
            let b = behv2 time
            let c = behv3 time
            let d = behv4 time
            let e = behv5 time
            op a b c d e

    let product<'a, 'b> (bhvr : 'a Behavior) (behv2 : 'b Behavior) =
        fun time ->
            (bhvr time, behv2 time)

    let local<'a> start (bhvr : 'a Behavior) : 'a Behavior =
        fun time ->
            bhvr (time - start)

    let slice<'a> start length (bhvr : 'a Behavior) : 'a option Behavior =
        fun time ->
            if time >= start && time < start + length
            then Some (bhvr (time - start))
            else None

    let id bhvr = map id bhvr
    let constant k bhvr = map (constant k) bhvr
    let not bhvr = map not bhvr
    let fst bhvr = map fst bhvr
    let snd bhvr = map snd bhvr
    let dup bhvr = map dup bhvr
    let prepend a bhvr = map (fun b -> (a, b)) bhvr
    let append b bhvr = map (fun a -> (a, b)) bhvr
    let withFst a bhvr = map (fun (_, b) -> (a, b)) bhvr
    let withSnd b bhvr = map (fun (a, _) -> (a, b)) bhvr
    let mapFst mapper bhvr = map (fun (a, b) -> (mapper a, b)) bhvr
    let mapSnd mapper bhvr = map (fun (a, b) -> (a, mapper b)) bhvr
    let swap bhvr = map (fun (a, b) -> (b, a)) bhvr

    let unit : unit Behavior = fun _ -> ()
    let time (time : GameTime) = constant time unit
    let sin = fun (time : GameTime) -> sin time.Seconds
    let cos = fun (time : GameTime) -> cos time.Seconds
    let step (delay : GameTime) = fun (time : GameTime) -> time.Seconds / delay.Seconds |> int
    let pulse (delay : GameTime) = map (fun steps -> steps % 2 <> 0) (step delay)

    let inline lerp (start : GameTime) (length : GameTime) : single Behavior =
        fun (time : GameTime) ->
            let local = time - start
            local.Seconds / length.Seconds

    let inline normalize length (bhvr : _ Behavior) =
        fun time ->
            let b = bhvr time
            let c = b / length
            c

/// Modifies behaviors.
type Modifier<'a, 'b> = 'a Behavior -> 'b Behavior

[<RequireQualifiedAccess>]
module Modifier =

    let returnB : Modifier<'a, 'a> =
        fun bhvr ->
            bhvr

    let arrow (f : 'a -> 'b) : Modifier<'a, 'b> =
        fun bhvr ->
            bhvr >> f

    let map<'a, 'b, 'c>
        (op : 'b -> 'c)
        (mdfr : Modifier<'a, 'b>) :
        Modifier<'a, 'c> =
        fun bhvr ->
            fun time ->
                let b = mdfr bhvr time
                op b

    let lift1<'a, 'b, 'c>
        (op : 'b -> 'c)
        (mdfr : Modifier<'a, 'b>) :
        Modifier<'a, 'c> =
        map op mdfr

    let lift2<'a, 'b, 'c, 'd>
        (op : 'b -> 'c -> 'd)
        (mdfr : Modifier<'a, 'b>)
        (modifier2 : Modifier<'a, 'c>) :
        Modifier<'a, 'd> =
        fun bhvr ->
            fun time ->
                let b = mdfr bhvr time
                let c = modifier2 bhvr time
                op b c

    let lift3<'a, 'b, 'c, 'd, 'e>
        (op : 'b -> 'c -> 'd -> 'e)
        (mdfr : Modifier<'a, 'b>)
        (modifier2 : Modifier<'a, 'c>)
        (modifier3 : Modifier<'a, 'd>) :
        Modifier<'a, 'e> =
        fun bhvr ->
            fun time ->
                let b = mdfr bhvr time
                let c = modifier2 bhvr time
                let d = modifier3 bhvr time
                op b c d

    let lift4<'a, 'b, 'c, 'd, 'e, 'f>
        (op : 'b -> 'c -> 'd -> 'e -> 'f)
        (mdfr : Modifier<'a, 'b>)
        (modifier2 : Modifier<'a, 'c>)
        (modifier3 : Modifier<'a, 'd>)
        (modifier4 : Modifier<'a, 'e>) :
        Modifier<'a, 'f> =
        fun bhvr ->
            fun time ->
                let b = mdfr bhvr time
                let c = modifier2 bhvr time
                let d = modifier3 bhvr time
                let e = modifier4 bhvr time
                op b c d e

    let lift5<'a, 'b, 'c, 'd, 'e, 'f, 'g>
        (op : 'b -> 'c -> 'd -> 'e -> 'f -> 'g)
        (mdfr : Modifier<'a, 'b>)
        (modifier2 : Modifier<'a, 'c>)
        (modifier3 : Modifier<'a, 'd>)
        (modifier4 : Modifier<'a, 'e>)
        (modifier5 : Modifier<'a, 'f>) :
        Modifier<'a, 'g> =
        fun bhvr ->
            fun time ->
                let b = mdfr bhvr time
                let c = modifier2 bhvr time
                let d = modifier3 bhvr time
                let e = modifier4 bhvr time
                let f = modifier5 bhvr time
                op b c d e f

    let first (mdfr : Modifier<'a, 'b>) : Modifier<'a * 'c, 'b * 'c> =
        fun bhvr ->
            let b = mdfr (Behavior.fst bhvr)
            let c = Behavior.snd bhvr
            Behavior.product b c

    let second (mdfr : Modifier<'a, 'b>) : Modifier<'c * 'a, 'c * 'b> =
        fun bhvr ->
            let b = mdfr (Behavior.snd bhvr)
            let c = Behavior.fst bhvr
            Behavior.product c b

    let compose (left : Modifier<'a, 'b>) (right : Modifier<'b, 'c>) : Modifier<'a, 'c> =
        fun bhvr ->
            right $
                fun time ->
                    left (fun time -> bhvr time) time

    let composeFlip (left : Modifier<'b, 'c>) (right : Modifier<'a, 'b>) : Modifier<'a, 'c> =
        compose right left

    let split (mdfr : Modifier<'a, 'b>) (modifier2 : Modifier<'a2, 'b2>) : Modifier<'a * 'a2, 'b * 'b2> =
        fun bhvr ->
            let b = mdfr (Behavior.fst bhvr)
            let b2 = modifier2 (Behavior.snd bhvr)
            Behavior.product b b2

    let fanOut (mdfr : Modifier<'a, 'b>) (modifier2 : Modifier<'a, 'b2>) : Modifier<'a, 'b * 'b2> =
        fun bhvr ->
            let b = mdfr bhvr
            let b2 = modifier2 bhvr
            Behavior.product b b2
            
    let id mdfr = map id mdfr
    let constant k mdfr = map (constant k) mdfr
    let not mdfr = map not mdfr
    let dup mdfr = map dup mdfr
    let fst mdfr = map fst mdfr
    let snd mdfr = map snd mdfr
    let prepend a mdfr = map (fun b -> (a, b)) mdfr
    let append b mdfr = map (fun a -> (a, b)) mdfr
    let withFst a mdfr = map (fun (_, b) -> (a, b)) mdfr
    let withSnd b mdfr = map (fun (a, _) -> (a, b)) mdfr
    let mapFst mapper mdfr = map (fun (a, b) -> (mapper a, b)) mdfr
    let mapSnd mapper mdfr = map (fun (a, b) -> (a, mapper b)) mdfr
    let swap mdfr = map (fun (a, b) -> (b, a)) mdfr

    let (>>>) (left, right) = compose left right
    let (<<<) (left, right) = composeFlip left right
    let ( *** ) (left, right) = split left right
    let (&&&) (left, right) = fanOut left right

    let hop<'a> (p : Vector3) (p2 : Vector3) h (start : GameTime) (length : GameTime) =
        let lerp =
            Behavior.lerp start length |>
            Behavior.map (fun s -> p + s * p2 - p)
        let jump =
            Behavior.sin |>
            Behavior.map (fun s -> s * h)
        Behavior.lift2 (fun (p : Vector3) h -> v3 p.X (p.Y + h) p.Z) lerp jump

[<RequireQualifiedAccess>]
module GameTimeExtension =

    type GameTime with

        member this.Run (behavior : 'a Behavior) =
            behavior this

        member this.RunHop start length =
            Modifier.hop v3Zero v3One 10.0f start length |> this.Run