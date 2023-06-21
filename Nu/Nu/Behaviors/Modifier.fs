// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu.Behaviors
open System
open FSharp.Core
open Prime
open Nu

// TODO: document this!

/// Modifies behaviors.
/// TODO: implement arrow choice combinators.
type Modifier<'a, 'b> =
    | Modifier of ('a Behavior -> 'b Behavior)

    member internal this.Run bhvr =
        match this with
        | Modifier mdfr -> mdfr bhvr

    member internal this.Compose (that : Modifier<'b, 'c>) : Modifier<'a, 'c> =
        Modifier $ fun bhvrA ->
            let bhvrB = Behavior $ fun time ->
                let bhvrC = Behavior $ fun time -> Behavior.run time bhvrA
                Behavior.run time (this.Run bhvrC)
            that.Run bhvrB

    member internal this.Split (that : Modifier<'a2, 'b2>) : Modifier<'a * 'a2, 'b * 'b2> =
        Modifier $ fun bhvrAA2 ->
            let bhvrB = this.Run (Behavior.fst bhvrAA2)
            let bhvrB2 = that.Run (Behavior.snd bhvrAA2)
            Behavior.product bhvrB bhvrB2

    member internal this.FanOut (that : Modifier<'a, 'b2>) : Modifier<'a, 'b * 'b2> =
        Modifier $ fun bhvrA ->
            let bhvrB = this.Run bhvrA
            let bhvrB2 = that.Run bhvrA
            Behavior.product bhvrB bhvrB2

    member internal this.ComposeLeft (f : 'b -> 'c) : Modifier<'a, 'c> =
        Modifier $ fun bhvrA ->
            let bhvrB = this.Run bhvrA
            let bhvrC = Behavior.map f bhvrB
            bhvrC

    // TODO: make sure we implemented this properly.
    member internal this.ComposeRight (f : 'c -> 'a) : Modifier<'c, 'b> =
        Modifier $ fun (bhvrA : 'c Behavior) ->
            let bhvrA = Behavior.map f bhvrA
            let bhvrB = this.Run bhvrA
            bhvrB

    static member ( >>>> ) (left : Modifier<'a, 'b>, right : Modifier<'b, 'c>) = left.Compose right
    static member ( <<<< ) (left : Modifier<'b, 'c>, right : Modifier<'a, 'b>) = right.Compose left
    static member ( **** ) (left : Modifier<'a, 'b>, right : Modifier<'a2, 'b2>) = left.Split right
    static member ( &&&& ) (left : Modifier<'a, 'b>, right : Modifier<'a, 'b2>) = left.FanOut right
    static member ( ^>>> ) (left : Modifier<'a, 'b>, right : Func<'b, 'c>) = left.ComposeLeft right.Invoke
    static member ( ^<<< ) (left : Func<'b, 'c>,     right : Modifier<'a, 'b>) = right.ComposeLeft left.Invoke
    static member ( >>>^ ) (left : Modifier<'a, 'b>, right : Func<'c, 'a>) = left.ComposeRight right.Invoke
    static member ( <<<^ ) (left : Func<'c, 'a>,     right : Modifier<'a, 'b>) = right.ComposeRight left.Invoke

[<RequireQualifiedAccess>]
module Modifier =

    let run bhvr mdfr =
        match mdfr with
        | Modifier mdfr -> mdfr bhvr

    let returnM bhvr : Modifier<'a, 'a> =
        Modifier (fun _ -> bhvr)

    let map<'a, 'b, 'c>
        (op : 'b -> 'c)
        (mdfr : Modifier<'a, 'b>) :
        Modifier<'a, 'c> =
        Modifier $ fun bhvr ->
            Behavior $ fun time ->
                let b = Behavior.run time (run bhvr mdfr)
                op b

    let arrow (f : 'a -> 'b) : Modifier<'a, 'b> =
        Modifier $ fun bhvr ->
            Behavior $ fun time ->
                let a = Behavior.run time bhvr
                f a

    let lift1<'a, 'b, 'c>
        (op : 'b -> 'c)
        (mdfr : Modifier<'a, 'b>) :
        Modifier<'a, 'c> =
        map op mdfr

    let lift2<'a, 'b, 'c, 'd>
        (op : 'b -> 'c -> 'd)
        (mdfr : Modifier<'a, 'b>)
        (mdfr2 : Modifier<'a, 'c>) :
        Modifier<'a, 'd> =
        Modifier $ fun bhvr ->
            Behavior $ fun time ->
                let b = Behavior.run time (run bhvr mdfr)
                let c = Behavior.run time (run bhvr mdfr2)
                op b c

    let lift3<'a, 'b, 'c, 'd, 'e>
        (op : 'b -> 'c -> 'd -> 'e)
        (mdfr : Modifier<'a, 'b>)
        (mdfr2 : Modifier<'a, 'c>)
        (mdfr3 : Modifier<'a, 'd>) :
        Modifier<'a, 'e> =
        Modifier $ fun bhvr ->
            Behavior $ fun time ->
                let b = Behavior.run time (run bhvr mdfr)
                let c = Behavior.run time (run bhvr mdfr2)
                let d = Behavior.run time (run bhvr mdfr3)
                op b c d

    let lift4<'a, 'b, 'c, 'd, 'e, 'f>
        (op : 'b -> 'c -> 'd -> 'e -> 'f)
        (mdfr : Modifier<'a, 'b>)
        (mdfr2 : Modifier<'a, 'c>)
        (mdfr3 : Modifier<'a, 'd>)
        (mdfr4 : Modifier<'a, 'e>) :
        Modifier<'a, 'f> =
        Modifier $ fun bhvr ->
            Behavior $ fun time ->
                let b = Behavior.run time (run bhvr mdfr)
                let c = Behavior.run time (run bhvr mdfr2)
                let d = Behavior.run time (run bhvr mdfr3)
                let e = Behavior.run time (run bhvr mdfr4)
                op b c d e

    let lift5<'a, 'b, 'c, 'd, 'e, 'f, 'g>
        (op : 'b -> 'c -> 'd -> 'e -> 'f -> 'g)
        (mdfr : Modifier<'a, 'b>)
        (mdfr2 : Modifier<'a, 'c>)
        (mdfr3 : Modifier<'a, 'd>)
        (mdfr4 : Modifier<'a, 'e>)
        (mdfr5 : Modifier<'a, 'f>) :
        Modifier<'a, 'g> =
        Modifier $ fun bhvr ->
            Behavior $ fun time ->
                let b = Behavior.run time (run bhvr mdfr)
                let c = Behavior.run time (run bhvr mdfr2)
                let d = Behavior.run time (run bhvr mdfr3)
                let e = Behavior.run time (run bhvr mdfr4)
                let f = Behavior.run time (run bhvr mdfr5)
                op b c d e f

    let first (mdfr : Modifier<'a, 'b>) : Modifier<'a * 'c, 'b * 'c> =
        Modifier $ fun bhvrAC ->
            let bhvrB = run (Behavior.fst bhvrAC) mdfr
            let bhvrC = Behavior.snd bhvrAC
            Behavior.product bhvrB bhvrC

    let second (mdfr : Modifier<'a, 'b>) : Modifier<'c * 'a, 'c * 'b> =
        Modifier $ fun bhvrCA ->
            let bhvrB = run (Behavior.snd bhvrCA) mdfr
            let bhvrC = Behavior.fst bhvrCA
            Behavior.product bhvrC bhvrB

    let compose (mdfr : Modifier<'a, 'b>) (mdfr2 : Modifier<'b, 'c>) : Modifier<'a, 'c> =
        mdfr.Compose mdfr2

    let composeFlip (mdfr : Modifier<'b, 'c>) (mdfr2 : Modifier<'a, 'b>) : Modifier<'a, 'c> =
        mdfr2.Compose mdfr

    let split (mdfr : Modifier<'a, 'b>) (mdfr2 : Modifier<'a2, 'b2>) : Modifier<'a * 'a2, 'b * 'b2> =
        mdfr.Split mdfr2

    let fanOut (mdfr : Modifier<'a, 'b>) (mdfr2 : Modifier<'a, 'b2>) : Modifier<'a, 'b * 'b2> =
        mdfr.FanOut mdfr2

    let composeLeft (f : 'b -> 'c) (mdfr : Modifier<'a, 'b>) : Modifier<'a, 'c> =
        mdfr.ComposeLeft f

    let composeLeftFlip (mdfr : Modifier<'a, 'b>) (f : 'b -> 'c) : Modifier<'a, 'c> =
        mdfr.ComposeLeft f

    // TODO: figure out how to implement this properly.
    let private apply (mdfr : Modifier<Modifier<'a, 'b> * 'b, 'b>) =
        Modifier $ fun (_ : 'b Behavior) ->
            let _ = run Unchecked.defaultof<_> mdfr
            Unchecked.defaultof<_>

    // TODO: figure out how to implement this properly.
    let rec private delay (a : 'a) : Modifier<'a, 'a> =
        Modifier $ fun (bhvrA : 'a Behavior) ->
            Behavior $ fun time ->
                let _ = delay (Behavior.run time bhvrA)
                a

    let loop (mdfr : Modifier<'a * 'c, 'b * 'c>) : Modifier<'a, 'b> =
        Modifier $ fun (bhvrA : 'a Behavior) ->
            let mutable c = Unchecked.defaultof<'c>
            let bhvrAC = Behavior.append c bhvrA
            let bhvrBC = run bhvrAC mdfr
            Behavior $ fun time ->
                let (b, c') = Behavior.run time bhvrBC
                c <- c'
                b