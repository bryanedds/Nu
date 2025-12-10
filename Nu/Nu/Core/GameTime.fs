// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System

/// The targeted frame rate.
type FrameRate =

    /// Specifies a static frame rate, which is a fixed number of updates per second.
    | StaticFrameRate of int64

    /// Specifies a dynamic frame rate, which is a variable number of updates per second.
    | DynamicFrameRate of int64

namespace Nu.Constants
open System
open System.Configuration
open Prime
open Nu

[<RequireQualifiedAccess>]
module GameTime =

    let [<Uniform>] mutable DesiredFrameRate = match ConfigurationManager.AppSettings.["DesiredFrameRate"] with null -> StaticFrameRate 60L | desiredFrameRate -> scvalue<FrameRate> desiredFrameRate
    let [<Literal>] DesiredFrameTimeSlop = 0.0001

namespace Nu
open System
open System.ComponentModel
open System.Diagnostics
open Prime

/// Type converter for GameTime.
type GameTimeConverter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<Symbol> ||
        destType = typeof<GameTime>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<Symbol> then
            let gameTime = source :?> GameTime
            match gameTime with
            | UpdateTime time -> Number (string time, ValueNone) :> obj
            | TickTime time -> Number (string (double time / double Stopwatch.Frequency), ValueNone) :> obj
        elif destType = typeof<GameTime> then source
        else failconv "Invalid GameTimeConverter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<GameTime>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Number (time, _) ->
                match Constants.GameTime.DesiredFrameRate with
                | StaticFrameRate _ -> UpdateTime (Int64.Parse time) :> obj
                | DynamicFrameRate _ -> TickTime (int64 (Double.Parse time * double Stopwatch.Frequency)) :> obj
            | _ -> failconv "Invalid GameTimeConverter conversion from source." (Some symbol)
        | :? GameTime -> source
        | _ -> failconv "Invalid GameTimeConverter conversion from source." None

/// Provides a variable representation of time based on whether the engine is configured to use a static or a dynamic
/// frame rate.
and [<Struct; CustomEquality; CustomComparison; TypeConverter (typeof<GameTimeConverter>)>] GameTime =
    | UpdateTime of UpdateTime : int64 // in updates
    | TickTime of TickTime : int64 // in ticks

    /// The minimum desired frame time.
    /// TODO: put this in World API instead?
    static member DesiredFrameTimeMinimum =
        match Constants.GameTime.DesiredFrameRate with
        | StaticFrameRate frameRate -> 1.0 / double frameRate - Constants.GameTime.DesiredFrameTimeSlop
        | DynamicFrameRate frameRate -> 1.0 / double frameRate - Constants.GameTime.DesiredFrameTimeSlop

    /// An unary operation on game time.
    static member inline unary op op2 time =
        match time with
        | UpdateTime time -> op time
        | TickTime time -> op2 time

    /// A binary operation on game time.
    static member inline binary op op2 left right =
        match struct (left, right) with
        | struct (UpdateTime leftTime, UpdateTime rightTime) -> op leftTime rightTime
        | struct (TickTime leftTime, TickTime rightTime) -> op2 leftTime rightTime
        | struct (_, _) -> failwith "Cannot apply operation to mixed GameTimes."

    /// Ap for game time (as in Haskell Apply).
    static member inline ap op op2 left right =
        match struct (left, right) with
        | struct (UpdateTime leftTime, UpdateTime rightTime) -> UpdateTime (op leftTime rightTime)
        | struct (TickTime leftTime, TickTime rightTime) -> TickTime (op2 leftTime rightTime)
        | struct (_, _) -> failwith "Cannot apply operation to mixed GameTimes."

    /// Construct a game time from updates or clock time.
    static member make updateTime (clockTime : double) =
        match Constants.GameTime.DesiredFrameRate with
        | StaticFrameRate _ -> UpdateTime updateTime
        | DynamicFrameRate _ -> TickTime (int64 (clockTime * double Stopwatch.Frequency))

    /// Construct a game time from a number of updates assuming desired frame rate is met.
    static member ofUpdates updates =
        match Constants.GameTime.DesiredFrameRate with
        | StaticFrameRate _ -> UpdateTime updates
        | DynamicFrameRate frameRate -> TickTime (int64 (1.0 / double frameRate * double updates * double Stopwatch.Frequency))

    /// Construct a game time from an amount of seconds assuming desired frame rate was met.
    static member ofSeconds seconds =
        match Constants.GameTime.DesiredFrameRate with
        | StaticFrameRate frameRate -> UpdateTime (int64 (double frameRate * seconds))
        | DynamicFrameRate _ -> TickTime (int64 (seconds * double Stopwatch.Frequency))

    /// Get the number of updates assuming desired frame rate is met.
    static member toUpdates time =
        match struct (Constants.GameTime.DesiredFrameRate, time) with
        | struct (_, UpdateTime time) -> time
        | struct (DynamicFrameRate frameRate, TickTime time) -> int64 (double time / (1.0 / double frameRate))
        | struct (_, _) -> failwith "Cannot apply operation to mixed GameTimes."

    /// Get the total amount of seconds assuming desired frame rate is met.
    static member toSeconds time =
        match struct (Constants.GameTime.DesiredFrameRate, time) with
        | struct (StaticFrameRate frameRate, UpdateTime time) -> 1.0 / double frameRate * double time
        | struct (_, TickTime time) -> double time / double Stopwatch.Frequency
        | struct (_, _) -> failwith "Cannot apply operation to mixed GameTimes."

    /// Get the total amount of milliseconds assuming desired frame rate is met.
    static member toMilliseconds time =
        GameTime.toSeconds time * 1000.0

    /// Equate GameTimes.
    static member equals left right =
        GameTime.binary (=) (=) left right

    /// Compare GameTimes.
    static member compare left right =
        match struct (left, right) with
        | struct (UpdateTime leftTime, UpdateTime rightTime) -> if leftTime < rightTime then -1 elif leftTime > rightTime then 1 else 0
        | struct (TickTime leftTime, TickTime rightTime) -> if leftTime < rightTime then -1 elif leftTime > rightTime then 1 else 0
        | struct (_, _) -> failwith "Cannot apply operation to mixed GameTimes."

    /// The progress of time down a unit-bounded range (double precision).
    static member progress startTime currentTime lifeTime =
        match struct (startTime, currentTime, lifeTime) with
        | struct (UpdateTime startTime, UpdateTime currentTime, UpdateTime lifeTime) -> double (currentTime - startTime) / double lifeTime |> max 0.0 |> min 1.0
        | struct (TickTime startTime, TickTime currentTime, TickTime lifeTime) -> double (currentTime - startTime) / double lifeTime |> max 0.0 |> min 1.0
        | struct (_, _, _) -> failwith "Cannot apply operation to mixed GameTimes."

    /// The progress of time down a unit-bounded range (single precision).
    /// NOTE: this loses some very minor precision but is fine for casual use.
    static member progressF startTime currentTime lifeTime =
        single (GameTime.progress startTime currentTime lifeTime)

    static member (+) (left, right) = GameTime.ap (+) (+) left right
    static member (-) (left, right) = GameTime.ap (-) (-) left right
    static member (*) (left, right) = GameTime.ap (*) (fun left right -> (left / Stopwatch.Frequency) * (right / Stopwatch.Frequency) / Stopwatch.Frequency |> int64) left right
    static member (*) (left, right) = match left with UpdateTime updates -> UpdateTime (int64 (single updates * right)) | TickTime ticks -> TickTime (int64 (single ticks * right))
    static member (*) (left, right) = match left with UpdateTime updates -> UpdateTime (int64 (int updates * right)) | TickTime ticks -> TickTime (int64 (int ticks * right))
    static member (/) (left, right) = GameTime.ap (/) (fun left right -> left / right * Stopwatch.Frequency |> int64) left right
    static member (/) (left, right) = match left with UpdateTime updates -> UpdateTime (int64 (single updates / right)) | TickTime ticks -> TickTime (int64 (single ticks / right))
    static member (/) (left, right) = match left with UpdateTime updates -> UpdateTime (int64 (int updates / right)) | TickTime ticks -> TickTime (int64 (int ticks / right))
    static member (%) (left, right) = GameTime.ap (%) (fun left right -> left % right) left right
    static member (%) (left, right) = match left with UpdateTime updates -> UpdateTime (int64 (single updates % right)) | TickTime ticks -> TickTime (int64 (single ticks % right))
    static member (%) (left, right) = match left with UpdateTime updates -> UpdateTime (int64 (int updates % right)) | TickTime ticks -> TickTime (int64 (int ticks % right))
    static member (~+) (time : GameTime) = time
    static member (~-) time = GameTime.unary (fun updates -> UpdateTime (-updates)) (fun ticks -> TickTime (-ticks)) time
    static member op_Implicit (i : int64) = UpdateTime i
    static member op_Implicit (d : double) = TickTime (int64 (d * double Stopwatch.Frequency))
    static member op_Explicit time = match time with UpdateTime time -> int64 time | TickTime time -> int64 (double time / double Stopwatch.Frequency)
    static member op_Explicit time = match time with UpdateTime time -> double time | TickTime time -> double (double time / double Stopwatch.Frequency)
    static member get_Zero () = GameTime.zero
    static member isZero time = GameTime.unary isZero isZero time
    static member notZero time = GameTime.unary notZero notZero time
    static member zero = GameTime.ofSeconds 0.0
    static member epsilon = match Constants.GameTime.DesiredFrameRate with StaticFrameRate _ -> UpdateTime 1L | DynamicFrameRate _ -> TickTime 1L
    static member min (left : GameTime) right = if left <= right then left else right
    static member max (left : GameTime) right = if left >= right then left else right
    static member MinValue = GameTime.make Int64.MinValue Double.MinValue
    static member MaxValue = GameTime.make Int64.MaxValue Double.MaxValue

    /// The total amount of elapsed updates.
    member this.Updates =
        GameTime.toUpdates this

    /// The total amount of elapsed seconds (double precision).
    member this.Seconds =
        GameTime.toSeconds this

    /// The total amount of elapsed seconds (single precision).
    /// NOTE: this loses precision for large GameTime values, such as a span of hours.
    member this.SecondsF =
        single this.Seconds

    /// The total amount of elapsed milliseconds (double precision).
    member this.Milliseconds =
        GameTime.toMilliseconds this

    /// The total amount of elapsed milliseconds (single precision).
    /// NOTE: this loses precision for large GameTime values, such as a span of hours.
    member this.MillisecondsF =
        single this.Milliseconds

    /// Check that the game time represents zero time.
    member this.IsZero =
        GameTime.isZero this

    /// Check that the game time represents non-zero time.
    member this.NotZero =
        GameTime.notZero this

    override this.Equals that =
        match that with
        | :? GameTime as that -> GameTime.equals this that
        | _ -> false

    override this.GetHashCode () =
        GameTime.unary hash hash this

    override this.ToString () =
        scstring this

    interface GameTime IEquatable with
        member this.Equals that =
            GameTime.equals this that

    interface GameTime IComparable with
        member this.CompareTo that =
            GameTime.compare this that

    interface IComparable with
        member this.CompareTo that =
            match that with
            | :? GameTime as that -> (this :> GameTime IComparable).CompareTo that
            | _ -> failwithumf ()