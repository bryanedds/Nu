// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu.Behaviors
open System
open System.Numerics
open FSharp.Core
open Prime
open Nu

/// Provides for time-driven behavior.
type 'a Behavior =
    | Behavior of (GameTime -> 'a)

    member internal this.Run a =
        match this with
        | Behavior f -> f a

    member internal this.Map2 mapper (that : _ Behavior) : _ Behavior =
        Behavior (fun time ->
            let a = this.Run time
            let b = that.Run time
            mapper (a, b))

    /// Map equality over two behaviors.
    static member (=.)   (left : _ Behavior, right : _ Behavior) : _ Behavior = left.Map2 (fun (a, b) -> a = b) right
    /// Map inequality over two behaviors.
    static member (<>.)  (left : _ Behavior, right : _ Behavior) : _ Behavior = left.Map2 (fun (a, b) -> a <> b) right
    /// Map less-than over two behaviors.
    static member (<.)   (left : _ Behavior, right : _ Behavior) : _ Behavior = left.Map2 (fun (a, b) -> a < b) right
    /// Map less-than-or-equal over two behaviors.
    static member (<=.)  (left : _ Behavior, right : _ Behavior) : _ Behavior = left.Map2 (fun (a, b) -> a <= b) right
    /// Map greater-than over two behaviors.
    static member (>.)   (left : _ Behavior, right : _ Behavior) : _ Behavior = left.Map2 (fun (a, b) -> a > b) right
    /// Map greater-than-or-equal over two behaviors.
    static member (>=.)  (left : _ Behavior, right : _ Behavior) : _ Behavior = left.Map2 (fun (a, b) -> a >= b) right
    /// Map logical or over two behaviors.
    static member (||.)  (left : _ Behavior, right : _ Behavior) : _ Behavior = left.Map2 (fun (a, b) -> a || b) right
    /// Map logical and over two behaviors.
    static member (&&.)  (left : _ Behavior, right : _ Behavior) : _ Behavior = left.Map2 (fun (a, b) -> a && b) right
    /// Map addition over two behaviors.
    static member (+)    (left : _ Behavior, right : _ Behavior) : _ Behavior = left.Map2 (fun (a, b) -> a + b) right
    /// Map subtraction over two behaviors.
    static member (-)    (left : _ Behavior, right : _ Behavior) : _ Behavior = left.Map2 (fun (a, b) -> a - b) right
    /// Map multiplication over two behaviors.
    static member (*)    (left : _ Behavior, right : _ Behavior) : _ Behavior = left.Map2 (fun (a, b) -> a * b) right
    /// Map division over two behaviors.
    static member (/)    (left : _ Behavior, right : _ Behavior) : _ Behavior = left.Map2 (fun (a, b) -> a / b) right
    /// Map remainder over two behaviors.
    static member (%)    (left : _ Behavior, right : _ Behavior) : _ Behavior = left.Map2 (fun (a, b) -> a % b) right
    /// Map right bitshift over two behaviors.
    static member (>>>)  (left : _ Behavior, right : _ Behavior) : _ Behavior = left.Map2 (fun (a, b) -> a >>> b) right
    /// Map left bitshift over two behaviors.
    static member (<<<)  (left : _ Behavior, right : _ Behavior) : _ Behavior = left.Map2 (fun (a, b) -> a <<< b) right
    /// Map logical xor over two behaviors.
    static member (^^^)  (left : _ Behavior, right : _ Behavior) : _ Behavior = left.Map2 (fun (a, b) -> a ^^^ b) right

[<RequireQualifiedAccess>]
module Behavior =

    /// Run a behavior monad.
    let run a (bhvr : 'a Behavior) =
        bhvr.Run a

    /// Monadic return.
    let returnB (a : 'a): 'a Behavior =
        Behavior (fun _ -> a)

    /// Functor map.
    let map<'a, 'b> mapper (bhvr : 'a Behavior) : 'b Behavior =
        Behavior (match bhvr with Behavior bhvr -> bhvr >> mapper)

    /// Applicative apply.
    let apply (bhvrF : ('a -> 'b) Behavior) (bhvrA : 'a Behavior) : 'b Behavior =
        Behavior (fun time ->
            let a = run time bhvrA
            let f = run time bhvrF
            f a)

    /// Monadic bind.
    let bind (bhvr : 'a Behavior) (f : 'a -> 'b Behavior) : 'b Behavior =
        Behavior (fun time ->
            let a = run time bhvr
            let b = f a
            run time b)

    /// Applicative lift 1.
    let lift1<'a, 'b>
        (op : 'a -> 'b)
        (bhvr : 'a Behavior) :
        Behavior<'b> =
        map op bhvr

    /// Applicative lift 2.
    let lift2<'a, 'b, 'c>
        (op : 'a -> 'b -> 'c)
        (bhvr : 'a Behavior)
        (bhvr2 : 'b Behavior) :
        'c Behavior =
        Behavior (fun time ->
            let a = run time bhvr
            let b = run time bhvr2
            op a b)

    /// Applicative lift 3.
    let lift3<'a, 'b, 'c, 'd>
        (op : 'a -> 'b -> 'c -> 'd)
        (bhvr : 'a Behavior)
        (bhvr2 : 'b Behavior)
        (bhvr3 : 'c Behavior) :
        'd Behavior =
        Behavior (fun time ->
            let a = run time bhvr
            let b = run time bhvr2
            let c = run time bhvr3
            op a b c)

    /// Applicative lift 4.
    let lift4<'a, 'b, 'c, 'd, 'e>
        (op : 'a -> 'b -> 'c -> 'd -> 'e)
        (bhvr : 'a Behavior)
        (bhvr2 : 'b Behavior)
        (bhvr3 : 'c Behavior)
        (bhvr4 : 'd Behavior) :
        'e Behavior =
        Behavior (fun time ->
            let a = run time bhvr
            let b = run time bhvr2
            let c = run time bhvr3
            let d = run time bhvr4
            op a b c d)

    /// Applicative lift 5.
    let lift5<'a, 'b, 'c, 'd, 'e, 'f>
        (op : 'a -> 'b -> 'c -> 'd -> 'e -> 'f)
        (bhvr : 'a Behavior)
        (bhvr2 : 'b Behavior)
        (bhvr3 : 'c Behavior)
        (bhvr4 : 'd Behavior)
        (bhvr5 : 'e Behavior) :
        'f Behavior =
        Behavior (fun time ->
            let a = run time bhvr
            let b = run time bhvr2
            let c = run time bhvr3
            let d = run time bhvr4
            let e = run time bhvr5
            op a b c d e)

    /// Loop over a behavior.
    let inline loop stride bounce bhvr =
        map (fun a ->
            let local = a % stride
            if bounce then
                if int (a / stride) % 2 = 0
                then local
                else stride - local
            else local)
            bhvr

    /// Slice a portion of behavior.
    let inline slice start length bhvr =
        map (fun a ->
            let local = a - start
            if local > length then length
            elif local < Generic.zero () then Generic.zero ()
            else local)
            bhvr

    /// Normalize the execution of a behavior over the given length of time.
    let inline normalize (length : GameTime) bhvr =
        map (fun (time : GameTime) ->
            let length = length.Seconds
            let time = time.Seconds
            time / length)
            bhvr

    (* Product Combinators *)
    let id bhvr = returnB bhvr
    let map2 mapper (bhvr : 'a Behavior) (bhvr2 : 'b Behavior) : 'c Behavior = bhvr.Map2 mapper bhvr2
    let mapProduct mapper bhvr = map (fun (a, b) -> mapper a b) bhvr
    let product (bhvr : 'a Behavior) (bhvr2 : 'b Behavior) : Behavior<'a * 'b> = Behavior (fun a -> (run a bhvr, run a bhvr2))
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

    (* Boot-Strapping Combinators *)

    /// The do-nothing behavior.
    let unit : unit Behavior = Behavior (fun _ -> ())

    /// The constant-emitting behavior.
    let constant k : _ Behavior = Behavior (fun _ -> k)

    /// The time-emitting behavior.
    let time : GameTime Behavior = Behavior (fun (time : GameTime) -> time)

    /// Time-emitting loop behavior.
    let timeLoopRaw stride bounce = loop stride bounce time

    /// Time-slicing behavior.
    let timeSliceRaw start length = slice start length time

    /// Normalized time-emitting loop behavior.
    let timeLoop stride bounce = let bhvr = timeLoopRaw stride bounce in normalize stride bhvr

    /// Normalized time-slicing behavior.
    let timeSlice start length = let bhvr = timeSliceRaw start length in normalize length bhvr

    (* Advanced Combinators *)
    let inline eq b bhvr = map (fun a -> a = b) bhvr
    let inline neq b bhvr = map (fun a -> a <> b) bhvr
    let inline not bhvr = map not bhvr
    let inline or_ b bhvr = map (fun a -> a || b) bhvr
    let inline nor b bhvr = map (fun a -> Operators.not a && Operators.not b) bhvr
    let inline xor (b : bool) bhvr = map (fun a -> a <> b) bhvr
    let inline and_ b bhvr = map (fun a -> a && b) bhvr
    let inline nand b bhvr = map (fun a -> Operators.not (a && b)) bhvr
    let inline isZero bhvr = map Generic.isZero bhvr
    let inline notZero bhvr = map Generic.notZero bhvr
    let inline isNeg bhvr = map (fun a -> a < Generic.zero ()) bhvr
    let inline isPositive bhvr = map (fun a -> a < Generic.zero ()) bhvr
    let inline negate bhvr = map (fun a -> -a) bhvr // TODO: redfine as Generics.negate after updating Prime.
    let inline inc bhvr = map inc bhvr
    let inline dec bhvr = map dec bhvr
    let inline min b bhvr = map (fun a -> min a b) bhvr
    let inline max b bhvr = map (fun a -> max a b) bhvr
    let inline random bhvr = map (fun a -> Random(hash a)) bhvr
    let inline randomb bhvr = map (fun a -> Random(hash a).Next() <= Int32.MaxValue / 2) bhvr
    let inline randomi bhvr = map (fun a -> Random(hash a).Next()) bhvr
    let inline randoml bhvr = map (fun a -> Random(hash a).NextInt64()) bhvr
    let inline randomf bhvr = map (fun a -> Random(hash a).NextDouble() |> single) bhvr
    let inline randomd bhvr = map (fun a -> Random(hash a).NextDouble()) bhvr
    let inline sum summand bhvr = map (fun a -> a + summand) bhvr
    let inline delta difference bhvr = map (fun a -> a - difference) bhvr
    let inline scale scalar bhvr = map (fun a -> a * scalar) bhvr
    let inline ratio divisor bhvr = map (fun a -> a / divisor) bhvr
    let inline modulo divisor bhvr = map (fun a -> a % divisor) bhvr
    let inline powf n bhvr = map (fun a -> single (Math.Pow (double a, double n))) bhvr
    let inline powd n bhvr = map (fun a -> Math.Pow (a, n)) bhvr
    let inline pow2 n bhvr = map (fun a -> Vector2.Pow (a, n)) bhvr
    let inline pow3 n bhvr = map (fun a -> Vector3.Pow (a, n)) bhvr
    let inline pow4 n bhvr = map (fun a -> Vector4.Pow (a, n)) bhvr
    let inline pow2i n bhvr = map (fun a -> Vector2i.Pow (a, n)) bhvr
    let inline pow3i n bhvr = map (fun a -> Vector3i.Pow (a, n)) bhvr
    let inline pow4i n bhvr = map (fun a -> Vector4i.Pow (a, n)) bhvr
    let inline powc n bhvr = map (fun a -> Color.Pow (a, n)) bhvr
    let inline pown n bhvr = map (fun a -> pown a n) bhvr
    let inline step stride bhvr = map (fun p -> int (p / stride)) bhvr
    let inline pulse stride bhvr = map (fun steps -> steps % 2 <> 0) (step stride bhvr)

    let inline constantTween (a :'a) (_ :'a) bhvr =
        map (fun (_ : 'b) -> a) bhvr

    let inline lerp a b bhvr =
        map (fun p -> a + p * (b - a)) bhvr

    let inline ease (scale : (^a * single) -> ^a) (a : ^a) (b : ^a) bhvr : ^a Behavior =
        map (fun (s : single) ->
            let scalar = single (Math.Pow (Math.Sin (Math.PI * double s * 0.5), 2.0))
            a + scale (b - a, scalar))
            bhvr

    let inline easeIn (scale : (^a * single) -> ^a) (a : ^a) (b : ^a) bhvr : ^a Behavior =
        map (fun (s : single) ->
            let scaled = float s * Math.PI * 0.5
            let scalar = single (1.0 + Math.Sin (scaled + Math.PI * 1.5))
            a + scale (b - a, scalar))
            bhvr

    let inline easeOut (scale : (^a * single) -> ^a) (a : ^a) (b : ^a) bhvr : ^a Behavior =
        map (fun (s : single) ->
            let scaled = float s * Math.PI * 0.5
            let scalar = single (Math.Sin scaled)
            a + scale (b - a, scalar))
            bhvr

    let inline sinTween (scale : (^a * single) -> ^a) (a : ^a) (b : ^a) bhvr : ^a Behavior =
        map (fun (s : single) ->
            let scaled = float s * Math.PI * 2.0
            let scalar = Math.Sin scaled
            a + scale (b - a, single scalar))
            bhvr

    let inline sinTweenScaled (scale : (^a * single) -> ^a) (a : ^a) (b : ^a) (scalar : single) bhvr : ^a Behavior =
        map (fun (s : single) ->
            let scaled = float s * Math.PI * 2.0 * float scalar
            let scalar = Math.Sin scaled
            a + scale (b - a, single scalar))
            bhvr

    let inline cosTween (scale : (^a * single) -> ^a) (a : ^a) (b : ^a) bhvr : ^a Behavior =
        map (fun (s : single) ->
            let scaled = float s * Math.PI * 2.0
            let scalar = Math.Cos scaled
            a + scale (b - a, single scalar))
            bhvr

    let inline cosTweenScaled (scale : (^a * single) -> ^a) (value : ^a) (value2 : ^a) (scalar : single) bhvr : ^a Behavior =
        map (fun (progress : single) ->
            let scaled = float progress * Math.PI * 2.0 * float scalar
            let scalar = Math.Cos scaled
            value + scale (value2 - value, single scalar))
            bhvr

    let inline easef a b bhvr = ease (fun (c, p) -> c * p) a b bhvr
    let inline easeInf a b bhvr = easeIn (fun (c, p) -> c * p) a b bhvr
    let inline easeOutf a b bhvr = easeOut (fun (c, p) -> c * p) a b bhvr
    let inline sinTweenf a b bhvr = sinTween (fun (c, p) -> c * p) a b bhvr
    let inline sinTweenScaledf a b scalar bhvr = sinTweenScaled (fun (c, p) -> c * p) a b scalar bhvr
    let inline cosTweenf a b bhvr = cosTween (fun (c, p) -> c * p) a b bhvr
    let inline cosTweenScaledf a b scalar bhvr = cosTweenScaled (fun (c, p) -> c * p) a b scalar bhvr

/// Builds behaviors.
type [<Sealed>] BehaviorBuilder () =

    /// Monadic return.
    member this.Return a = Behavior.returnB a

    /// Monadic return!.
    member this.ReturnFrom a = a

    /// Monadic bind.
    member this.Bind (a, f) = Behavior.bind a f

[<AutoOpen>]
module BehaviorBuilder =

    /// Builds behaviors.
    let behave = BehaviorBuilder ()

[<RequireQualifiedAccess>]
module GameTimeExtension =
    type GameTime with
        member this.Run (behavior : 'a Behavior) =
            Behavior.run this behavior