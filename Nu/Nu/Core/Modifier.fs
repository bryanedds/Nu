// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Behaviors
open System
open FSharp.Core
open Prime
open Nu

/// Modifies behaviors. Modifiers form an arrow, with the choice and loop combinators of one.
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

    member internal this.ComposeRight (f : 'c -> 'a) : Modifier<'c, 'b> =
        Modifier $ fun (bhvrA : 'c Behavior) ->
            let bhvrA = Behavior.map f bhvrA
            let bhvrB = this.Run bhvrA
            bhvrB

    member internal this.Choose (that : Modifier<'a2, 'b2>) : Modifier<Choice<'a, 'a2>, Choice<'b, 'b2>> =
        Modifier $ fun (bhvrAA2 : Choice<'a, 'a2> Behavior) ->
            let bhvrA =
                Behavior $ fun time ->
                    match Behavior.run time bhvrAA2 with
                    | Choice1Of2 a -> a
                    | Choice2Of2 _ -> Unchecked.defaultof<'a> // NOTE: never sampled on this branch.
            let bhvrA2 =
                Behavior $ fun time ->
                    match Behavior.run time bhvrAA2 with
                    | Choice1Of2 _ -> Unchecked.defaultof<'a2> // NOTE: never sampled on this branch.
                    | Choice2Of2 a2 -> a2
            let bhvrB = this.Run bhvrA
            let bhvrB2 = that.Run bhvrA2
            Behavior $ fun time ->
                match Behavior.run time bhvrAA2 with
                | Choice1Of2 _ -> Choice1Of2 (Behavior.run time bhvrB)
                | Choice2Of2 _ -> Choice2Of2 (Behavior.run time bhvrB2)

    member internal this.FanIn (that : Modifier<'a2, 'b>) : Modifier<Choice<'a, 'a2>, 'b> =
        Modifier $ fun (bhvrAA2 : Choice<'a, 'a2> Behavior) ->
            let bhvrA =
                Behavior $ fun time ->
                    match Behavior.run time bhvrAA2 with
                    | Choice1Of2 a -> a
                    | Choice2Of2 _ -> Unchecked.defaultof<'a> // NOTE: never sampled on this branch.
            let bhvrA2 =
                Behavior $ fun time ->
                    match Behavior.run time bhvrAA2 with
                    | Choice1Of2 _ -> Unchecked.defaultof<'a2> // NOTE: never sampled on this branch.
                    | Choice2Of2 a2 -> a2
            let bhvrB = this.Run bhvrA
            let bhvrB2 = that.Run bhvrA2
            Behavior $ fun time ->
                match Behavior.run time bhvrAA2 with
                | Choice1Of2 _ -> Behavior.run time bhvrB
                | Choice2Of2 _ -> Behavior.run time bhvrB2

    /// Composes two modifiers, applying the left modifier first and then the right one.
    static member ( >>>> ) (left : Modifier<'a, 'b>, right : Modifier<'b, 'c>) = left.Compose right

    /// Composes two modifiers, applying the right modifier first and then the left one.
    static member ( <<<< ) (left : Modifier<'b, 'c>, right : Modifier<'a, 'b>) = right.Compose left

    /// Splits the input into two behaviors and runs the left and right modifiers on each part independently.
    static member ( **** ) (left : Modifier<'a, 'b>, right : Modifier<'a2, 'b2>) = left.Split right

    /// Fan-out composition of two modifiers. It runs both modifiers on the same input behavior and produces a pair of results.
    static member ( &&&& ) (left : Modifier<'a, 'b>, right : Modifier<'a, 'b2>) = left.FanOut right

    /// Left-to-right function composition of a modifier with a function.
    static member ( ^>>> ) (left : Modifier<'a, 'b>, right : Func<'b, 'c>) = left.ComposeLeft right.Invoke

    /// Right-to-left function composition of a function with a modifier.
    static member ( ^<<< ) (left : Func<'b, 'c>, right : Modifier<'a, 'b>) = right.ComposeLeft left.Invoke

    /// Right-to-left behavior composition of a modifier with a function.
    static member ( >>>^ ) (left : Modifier<'a, 'b>, right : Func<'c, 'a>) = left.ComposeRight right.Invoke

    /// Left-to-right behavior composition of a function with a modifier.
    static member ( <<<^ ) (left : Func<'c, 'a>, right : Modifier<'a, 'b>) = right.ComposeRight left.Invoke

    /// Choice composition of two modifiers. It runs the left modifier on Choice1Of2 inputs and the right one on
    /// Choice2Of2 inputs, tagging each output the same way.
    static member ( ++++ ) (left : Modifier<'a, 'b>, right : Modifier<'a2, 'b2>) = left.Choose right

    /// Fan-in composition of two modifiers. It runs the left modifier on Choice1Of2 inputs and the right one on
    /// Choice2Of2 inputs, merging their outputs.
    static member ( |||| ) (left : Modifier<'a, 'b>, right : Modifier<'a2, 'b>) = left.FanIn right

/// Modifier operators.
module Modifier =

    /// Run a behavior modifier.
    let run bhvr mdfr =
        match mdfr with
        | Modifier mdfr -> mdfr bhvr

    /// Monadic return. Lifts a behavior into a constant modifier, which ignores its input behavior and produces the
    /// given one.
    let returnM bhvr : Modifier<'a, 'a> =
        Modifier (fun _ -> bhvr)

    /// Functor map. Maps a function over a modifier's output behavior.
    let map<'a, 'b, 'c>
        (op : 'b -> 'c)
        (mdfr : Modifier<'a, 'b>) :
        Modifier<'a, 'c> =
        Modifier $ fun bhvr ->
            Behavior $ fun time ->
                let b = Behavior.run time (run bhvr mdfr)
                op b

    /// Creates an arrow from a function. The arrow takes a behavior as input and applies the function to it.
    let arrow (f : 'a -> 'b) : Modifier<'a, 'b> =
        Modifier $ fun bhvr ->
            Behavior $ fun time ->
                let a = Behavior.run time bhvr
                f a

    /// Lifts a unary function to operate on behaviors through a modifier.
    let lift1<'a, 'b, 'c>
        (op : 'b -> 'c)
        (mdfr : Modifier<'a, 'b>) :
        Modifier<'a, 'c> =
        map op mdfr

    /// Lifts a binary function to operate on behaviors through a modifier.
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

    /// Lifts a ternary function to operate on behaviors through a modifier.
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

    /// Lifts a 4-ary function to operate on behaviors through a modifier.
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

    /// Lifts a 5-ary function to operate on behaviors through a modifier.
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

    /// Composes the given modifier `mdfr` with the behavior of a pair of type `'a * 'c`.
    /// The `mdfr` is applied to the first component of the pair, while the second component remains unchanged.
    /// The output behavior is a pair of type `'b * 'c`, where the first component is the result of applying `mdfr` to
    /// the first component of the input pair, and the second component is the unchanged second component of the input
    /// pair.
    let first (mdfr : Modifier<'a, 'b>) : Modifier<'a * 'c, 'b * 'c> =
        Modifier $ fun bhvrAC ->
            let bhvrB = run (Behavior.fst bhvrAC) mdfr
            let bhvrC = Behavior.snd bhvrAC
            Behavior.product bhvrB bhvrC

    /// Composes the given modifier `mdfr` with the behavior of a pair of type `'c * 'a`.
    /// The `mdfr` is applied to the second component of the pair, while the first component remains unchanged.
    /// The output behavior is a pair of type `'c * 'b`, where the first component is the unchanged first component of
    /// the input pair, and the second component is the result of applying `mdfr` to the second component of the input
    /// pair.
    let second (mdfr : Modifier<'a, 'b>) : Modifier<'c * 'a, 'c * 'b> =
        Modifier $ fun bhvrCA ->
            let bhvrB = run (Behavior.snd bhvrCA) mdfr
            let bhvrC = Behavior.fst bhvrCA
            Behavior.product bhvrC bhvrB

    /// Composes the given modifier `mdfr` with another modifier `mdfr2`.
    /// The `mdfr` is applied first, and then the result is passed as input to `mdfr2`.
    let compose (mdfr : Modifier<'a, 'b>) (mdfr2 : Modifier<'b, 'c>) : Modifier<'a, 'c> =
        mdfr.Compose mdfr2

    /// Composes the given modifier `mdfr2` with another modifier `mdfr`.
    /// The `mdfr2` is applied first, and then the result is passed as input to `mdfr`.
    let composeFlip (mdfr : Modifier<'b, 'c>) (mdfr2 : Modifier<'a, 'b>) : Modifier<'a, 'c> =
        mdfr2.Compose mdfr

    /// Splits the input into two behaviors and runs the given modifiers `mdfr` and `mdfr2` on each part independently.
    let split (mdfr : Modifier<'a, 'b>) (mdfr2 : Modifier<'a2, 'b2>) : Modifier<'a * 'a2, 'b * 'b2> =
        mdfr.Split mdfr2

    /// Fan-out composition of two modifiers.
    /// It runs both modifiers `mdfr` and `mdfr2` on the same input behavior and produces a pair of results.
    let fanOut (mdfr : Modifier<'a, 'b>) (mdfr2 : Modifier<'a, 'b2>) : Modifier<'a, 'b * 'b2> =
        mdfr.FanOut mdfr2

    /// Choice composition of two modifiers.
    /// It runs `mdfr` on Choice1Of2 inputs and `mdfr2` on Choice2Of2 inputs, tagging each output the same way.
    let choose (mdfr : Modifier<'a, 'b>) (mdfr2 : Modifier<'a2, 'b2>) : Modifier<Choice<'a, 'a2>, Choice<'b, 'b2>> =
        mdfr.Choose mdfr2

    /// Fan-in composition of two modifiers.
    /// It runs `mdfr` on Choice1Of2 inputs and `mdfr2` on Choice2Of2 inputs, merging their outputs.
    let fanIn (mdfr : Modifier<'a, 'b>) (mdfr2 : Modifier<'a2, 'b>) : Modifier<Choice<'a, 'a2>, 'b> =
        mdfr.FanIn mdfr2

    /// Runs the given modifier `mdfr` on Choice1Of2 inputs, passing Choice2Of2 inputs through unchanged.
    let left (mdfr : Modifier<'a, 'b>) : Modifier<Choice<'a, 'c>, Choice<'b, 'c>> =
        mdfr.Choose (Modifier id)

    /// Runs the given modifier `mdfr` on Choice2Of2 inputs, passing Choice1Of2 inputs through unchanged.
    let right (mdfr : Modifier<'a, 'b>) : Modifier<Choice<'c, 'a>, Choice<'c, 'b>> =
        (Modifier id).Choose mdfr

    /// Composes the given modifier `mdfr` with the provided function `f`.
    /// The `mdfr` is applied first, and then the resulting behavior is mapped using `f`.
    let composeLeft (f : 'b -> 'c) (mdfr : Modifier<'a, 'b>) : Modifier<'a, 'c> =
        mdfr.ComposeLeft f

    /// Composes the given modifier `mdfr` with the provided function `f`.
    /// The `mdfr` is applied first, and then the resulting behavior is mapped using `f`.
    let composeLeftFlip (mdfr : Modifier<'a, 'b>) (f : 'b -> 'c) : Modifier<'a, 'c> =
        mdfr.ComposeLeft f

    // TODO: P1: figure out how to implement this properly.
    let private apply (mdfr : Modifier<Modifier<'a, 'b> * 'b, 'b>) =
        Modifier $ fun (_ : 'b Behavior) ->
            let _ = run Unchecked.defaultof<_> mdfr
            Unchecked.defaultof<_>

    // TODO: P1: figure out how to implement this properly.
    let rec private delay (a : 'a) : Modifier<'a, 'a> =
        Modifier $ fun (bhvrA : 'a Behavior) ->
            Behavior $ fun time ->
                let _ = delay (Behavior.run time bhvrA)
                a

    /// Looping behavior with an initial feedback value. Ties the knot by feeding the second output of `mdfr` back into
    /// its second input, starting from `initial`. Since behaviors are evaluated strictly, each sample receives the
    /// feedback value produced by the previous sample, which makes the resulting behavior stateful.
    let loopFrom (initial : 'c) (mdfr : Modifier<'a * 'c, 'b * 'c>) : Modifier<'a, 'b> =
        Modifier $ fun (bhvrA : 'a Behavior) ->
            let feedback = ref initial
            let bhvrAC = Behavior $ fun time -> (Behavior.run time bhvrA, feedback.Value)
            let bhvrBC = run bhvrAC mdfr
            Behavior $ fun time ->
                let (b, c) = Behavior.run time bhvrBC
                feedback.Value <- c
                b

    /// Looping behavior. Like loopFrom, starting from the default value of the feedback type.
    let loop (mdfr : Modifier<'a * 'c, 'b * 'c>) : Modifier<'a, 'b> =
        loopFrom Unchecked.defaultof<'c> mdfr