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
        (bhvr2 : 'b Behavior) :
        'c Behavior =
        let bhvr = fun time ->
            let a = bhvr time
            let b = bhvr2 time
            op a b
        bhvr

    let lift3<'a, 'b, 'c, 'd>
        (op : 'a -> 'b -> 'c -> 'd)
        (bhvr : 'a Behavior)
        (bhvr2 : 'b Behavior)
        (bhvr3 : 'c Behavior) :
        'd Behavior =
        let bhvr = fun time ->
            let a = bhvr time
            let b = bhvr2 time
            let c = bhvr3 time
            op a b c
        bhvr

    let lift4<'a, 'b, 'c, 'd, 'e>
        (op : 'a -> 'b -> 'c -> 'd -> 'e)
        (bhvr : 'a Behavior)
        (bhvr2 : 'b Behavior)
        (bhvr3 : 'c Behavior)
        (bhvr4 : 'd Behavior) :
        'e Behavior =
        let bhvr = fun time ->
            let a = bhvr time
            let b = bhvr2 time
            let c = bhvr3 time
            let d = bhvr4 time
            op a b c d
        bhvr

    let lift5<'a, 'b, 'c, 'd, 'e, 'f>
        (op : 'a -> 'b -> 'c -> 'd -> 'e -> 'f)
        (bhvr : 'a Behavior)
        (bhvr2 : 'b Behavior)
        (bhvr3 : 'c Behavior)
        (bhvr4 : 'd Behavior)
        (bhvr5 : 'e Behavior) :
        'f Behavior =
        let bhvr = fun time ->
            let a = bhvr time
            let b = bhvr2 time
            let c = bhvr3 time
            let d = bhvr4 time
            let e = bhvr5 time
            op a b c d e
        bhvr

    let inline loop stride bounce bhvr =
        map (fun a ->
            let local = a % stride
            if bounce then
                if int (a / stride) % 2 = 0
                then local
                else stride - local
            else local)
            bhvr

    let inline slice start length bhvr =
        map (fun a ->
            let local = a - start
            if local > length then length
            elif local < Generics.zero () then Generics.zero ()
            else local)
            bhvr

    let inline normalize (length : GameTime) bhvr =
        map (fun (time : GameTime) ->
            let length = length.Seconds
            let time = time.Seconds
            time / length)
            bhvr

    // basic behavior combinators
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
    let product bhvr bhvr2 = let bhvr3 = fun time -> (bhvr time, bhvr2 time) in bhvr3

    // boot-strapping combinators
    let unit = let (bhvr : unit Behavior) = fun _ -> () in bhvr
    let time : GameTime Behavior = let bhvr = fun (time : GameTime) -> time in bhvr
    let timeLoopRaw stride bounce = loop stride bounce time
    let timeSliceRaw start length = slice start length time
    let timeLoop stride bounce = let bhvr = timeLoopRaw stride bounce in normalize stride bhvr
    let timeSlice start length = let bhvr = timeSliceRaw start length in  normalize length bhvr

    // advanced behavior combinators
    let inline lerp a b bhvr = map (fun p -> a + p * (b - a)) bhvr
    let inline step stride bhvr = map (fun p -> int (p / stride)) bhvr
    let inline pulse stride bhvr = map (fun steps -> steps % 2 <> 0) (step stride bhvr)
    let inline sin bhvr = map sin bhvr
    let inline cos bhvr = map cos bhvr
    let inline sum addend bhvr = map (fun a -> a + addend) bhvr
    let inline delta difference bhvr = map (fun a -> a - difference) bhvr
    let inline scale scalar bhvr = map (fun a -> a * scalar) bhvr
    let inline ratio divisor bhvr = map (fun a -> a / divisor) bhvr

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

    let private hop (p : Vector3) (p2 : Vector3) (h : single) (start : GameTime) (length : GameTime) =
        let lerp =
            Behavior.timeSlice start length |>
            Behavior.lerp p p2
        let jump =
            Behavior.timeSlice start length |>
            Behavior.sin |>
            Behavior.scale h
        Behavior.lift2 (fun (p : Vector3) h -> v3 p.X (p.Y + h) p.Z) lerp jump

[<RequireQualifiedAccess>]
module GameTimeExtension =

    type GameTime with

        member this.Run (behavior : 'a Behavior) =
            behavior this