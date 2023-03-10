namespace Nu
open System
open FSharp.Core
open Prime

/// Modifies behaviors.
type Modifier<'a, 'b> = 'a Behavior -> 'b Behavior

[<RequireQualifiedAccess>]
module Modifier =

    let returnM : Modifier<'a, 'a> =
        let mdfr = fun bhvr -> bhvr
        mdfr

    let map<'a, 'b, 'c>
        (op : 'b -> 'c)
        (mdfr : Modifier<'a, 'b>) :
        Modifier<'a, 'c> =
        let mdfr = fun bhvr time ->
            let b = mdfr bhvr time
            op b
        mdfr

    let arrow (f : 'a -> 'b) : Modifier<'a, 'b> =
        let mdfr = fun bhvr -> bhvr >> f
        mdfr

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
        let mdfr = fun bhvr time ->
            let b = mdfr bhvr time
            let c = modifier2 bhvr time
            op b c
        mdfr

    let lift3<'a, 'b, 'c, 'd, 'e>
        (op : 'b -> 'c -> 'd -> 'e)
        (mdfr : Modifier<'a, 'b>)
        (modifier2 : Modifier<'a, 'c>)
        (modifier3 : Modifier<'a, 'd>) :
        Modifier<'a, 'e> =
        let mdfr = fun bhvr time ->
            let b = mdfr bhvr time
            let c = modifier2 bhvr time
            let d = modifier3 bhvr time
            op b c d
        mdfr

    let lift4<'a, 'b, 'c, 'd, 'e, 'f>
        (op : 'b -> 'c -> 'd -> 'e -> 'f)
        (mdfr : Modifier<'a, 'b>)
        (modifier2 : Modifier<'a, 'c>)
        (modifier3 : Modifier<'a, 'd>)
        (modifier4 : Modifier<'a, 'e>) :
        Modifier<'a, 'f> =
        let mdfr = fun bhvr time ->
            let b = mdfr bhvr time
            let c = modifier2 bhvr time
            let d = modifier3 bhvr time
            let e = modifier4 bhvr time
            op b c d e
        mdfr

    let lift5<'a, 'b, 'c, 'd, 'e, 'f, 'g>
        (op : 'b -> 'c -> 'd -> 'e -> 'f -> 'g)
        (mdfr : Modifier<'a, 'b>)
        (modifier2 : Modifier<'a, 'c>)
        (modifier3 : Modifier<'a, 'd>)
        (modifier4 : Modifier<'a, 'e>)
        (modifier5 : Modifier<'a, 'f>) :
        Modifier<'a, 'g> =
        let mdfr = fun bhvr time ->
            let b = mdfr bhvr time
            let c = modifier2 bhvr time
            let d = modifier3 bhvr time
            let e = modifier4 bhvr time
            let f = modifier5 bhvr time
            op b c d e f
        mdfr

    let first (mdfr : Modifier<'a, 'b>) : Modifier<'a * 'c, 'b * 'c> =
        let mdfr = fun bhvr ->
            let b = mdfr (Behavior.fst bhvr)
            let c = Behavior.snd bhvr
            Behavior.product b c
        mdfr

    let second (mdfr : Modifier<'a, 'b>) : Modifier<'c * 'a, 'c * 'b> =
        let mdfr = fun bhvr ->
            let b = mdfr (Behavior.snd bhvr)
            let c = Behavior.fst bhvr
            Behavior.product c b
        mdfr

    let compose (left : Modifier<'a, 'b>) (right : Modifier<'b, 'c>) : Modifier<'a, 'c> =
        let mdfr = fun bhvr ->
            right $
                fun time ->
                    left (fun time -> bhvr time) time
        mdfr

    let composeFlip (left : Modifier<'b, 'c>) (right : Modifier<'a, 'b>) : Modifier<'a, 'c> =
        compose right left

    let split (mdfr : Modifier<'a, 'b>) (modifier2 : Modifier<'a2, 'b2>) : Modifier<'a * 'a2, 'b * 'b2> =
        let mdfr = fun bhvr ->
            let b = mdfr (Behavior.fst bhvr)
            let b2 = modifier2 (Behavior.snd bhvr)
            Behavior.product b b2
        mdfr

    let fanOut (mdfr : Modifier<'a, 'b>) (modifier2 : Modifier<'a, 'b2>) : Modifier<'a, 'b * 'b2> =
        let mdfr = fun bhvr ->
            let b = mdfr bhvr
            let b2 = modifier2 bhvr
            Behavior.product b b2
        mdfr

    // operator combinators
    let (>>>) (left, right) = compose left right
    let (<<<) (left, right) = composeFlip left right
    let ( *** ) (left, right) = split left right
    let (&&&) (left, right) = fanOut left right