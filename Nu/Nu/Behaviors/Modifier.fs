namespace Nu.Behaviors
open System
open FSharp.Core
open Nu

/// Modifies behaviors.
type Modifier<'a, 'b> =
    | Modifier of ('a Behavior -> 'b Behavior)

    member internal this.Run a =
        match this with
        | Modifier mdfr -> mdfr a

    member internal this.Compose (that : Modifier<'b, 'c>) : Modifier<'a, 'c> =
        Modifier $ fun bhvrA ->
            let bhvrB = Behavior $ fun time ->
                let bhvrC = Behavior $ fun time -> Behavior.run time bhvrA
                Behavior.run time (this.Run bhvrC)
            that.Run bhvrB

    member internal this.Split (that : Modifier<'a2, 'b2>) : Modifier<'a * 'a2, 'b * 'b2> =
        Modifier $ fun bhvr ->
            let b = this.Run (Behavior.fst bhvr)
            let b2 = that.Run (Behavior.snd bhvr)
            Behavior.product b b2

    member internal this.FanOut (that : Modifier<'a, 'b2>) : Modifier<'a, 'b * 'b2> =
        Modifier $ fun bhvr ->
            let b = this.Run bhvr
            let b2 = that.Run bhvr
            Behavior.product b b2

    member internal this.ComposeA (f : 'b -> 'c) : Modifier<'a, 'c> =
        Modifier $ fun bhvr ->
            let b = this.Run bhvr
            let c = Behavior.map f b
            c

    static member ( >>> ) (left : Modifier<'a, 'b>, right : Modifier<'b, 'c>) = left.Compose right
    static member ( <<< ) (left : Modifier<'b, 'c>, right : Modifier<'a, 'b>) = right.Compose left
    static member ( *** ) (left : Modifier<'a, 'b>, right : Modifier<'a2, 'b2>) = left.Split right
    static member ( &&& ) (left : Modifier<'a, 'b>, right : Modifier<'a, 'b2>) = left.FanOut right
    static member ( ^<< ) (left : Func<'b, 'c>,     right : Modifier<'a, 'b>) = right.ComposeA left.Invoke

[<RequireQualifiedAccess>]
module Modifier =

    let run a b =
        match b with
        | Modifier mdfr -> mdfr a

    let returnM a : Modifier<'a, 'a> =
        Modifier (fun _ -> a)

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
        Modifier $ fun bhvr ->
            let b = run (Behavior.fst bhvr) mdfr
            let c = Behavior.snd bhvr
            Behavior.product b c

    let second (mdfr : Modifier<'a, 'b>) : Modifier<'c * 'a, 'c * 'b> =
        Modifier $ fun bhvr ->
            let b = run (Behavior.snd bhvr) mdfr
            let c = Behavior.fst bhvr
            Behavior.product c b

    let compose (mdfr : Modifier<'a, 'b>) (mdfr2 : Modifier<'b, 'c>) : Modifier<'a, 'c> =
        mdfr.Compose mdfr2

    let composeFlip (mdfr : Modifier<'b, 'c>) (mdfr2 : Modifier<'a, 'b>) : Modifier<'a, 'c> =
        mdfr2.Compose mdfr

    let split (mdfr : Modifier<'a, 'b>) (mdfr2 : Modifier<'a2, 'b2>) : Modifier<'a * 'a2, 'b * 'b2> =
        mdfr.Split mdfr2

    let fanOut (mdfr : Modifier<'a, 'b>) (mdfr2 : Modifier<'a, 'b2>) : Modifier<'a, 'b * 'b2> =
        mdfr.FanOut mdfr2

    let composeA (f : 'b -> 'c) (mdfr : Modifier<'a, 'b>) : Modifier<'a, 'c> =
        mdfr.ComposeA f