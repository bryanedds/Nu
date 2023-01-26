// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.ComponentModel
open Prime
open Nu

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
            | ClockTime time -> Number (string time, ValueNone) :> obj
        elif destType = typeof<GameTime> then source
        else failconv "Invalid GameTime conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<GameTime>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Number (time, _) ->
                match Constants.Engine.DesiredFrameRate with
                | StaticFrameRate _ -> UpdateTime (Int64.Parse time) :> obj
                | DynamicFrameRate _ -> ClockTime (Single.Parse time) :> obj
            | _ -> failconv "Invalid GameTimeConverter conversion from source." (Some symbol)
        | :? GameTime -> source
        | _ -> failconv "Invalid GameTimeConverter conversion from source." None

/// Provide a variable representation of time based on whether the engine is configured to use a static or a dynamic
/// frame rate.
and [<Struct; CustomEquality; CustomComparison; TypeConverter (typeof<GameTimeConverter>)>] GameTime =
    | UpdateTime of UpdateTime : int64 // in updates
    | ClockTime of ClockTime : single // in seconds

    static member inline unary op op2 time =
        match time with
        | UpdateTime time -> op time
        | ClockTime time -> op2 time

    static member inline binary op op2 left right =
        match (left, right) with
        | (UpdateTime leftTime, UpdateTime rightTime) -> op leftTime rightTime
        | (ClockTime leftTime, ClockTime rightTime) -> op2 leftTime rightTime
        | (_, _) -> failwith "Cannot apply operation to mixed GameTimes."

    static member inline ap op op2 left right =
        match (left, right) with
        | (UpdateTime leftTime, UpdateTime rightTime) -> UpdateTime (op leftTime rightTime)
        | (ClockTime leftTime, ClockTime rightTime) -> ClockTime (op2 leftTime rightTime)
        | (_, _) -> failwith "Cannot apply operation to mixed GameTimes."

    static member make updateTime clockTime =
        match Constants.Engine.DesiredFrameRate with StaticFrameRate _ -> UpdateTime updateTime | DynamicFrameRate _ -> ClockTime clockTime

    static member equals left right =
        GameTime.binary (=) (=) left right

    static member compare left right =
        match (left, right) with
        | (UpdateTime leftTime, UpdateTime rightTime) -> if leftTime < rightTime then -1 elif leftTime > rightTime then 1 else 0
        | (ClockTime leftTime, ClockTime rightTime) -> if leftTime < rightTime then -1 elif leftTime > rightTime then 1 else 0
        | (_, _) -> failwith "Cannot apply operation to mixed GameTim   es."

    static member progress startTime currentTime lifeTime =
        match (startTime, currentTime, lifeTime) with
        | (UpdateTime startTime, UpdateTime currentTime, UpdateTime lifeTime) -> (single (currentTime - startTime)) / single lifeTime
        | (ClockTime startTime, ClockTime currentTime, ClockTime lifeTime) -> (currentTime - startTime) / lifeTime
        | (_, _, _) -> failwith "Cannot apply operation to mixed GameTimes."

    static member (+) (left, right) = GameTime.ap (+) (+) left right
    static member (-) (left, right) = GameTime.ap (-) (-) left right
    static member (*) (left, right) = GameTime.ap (*) (*) left right
    static member (/) (left, right) = GameTime.ap (/) (/) left right
    static member (%) (left, right) = GameTime.ap (%) (%) left right
    static member (+) (left, right) = GameTime.unary ((+) (int64 right) >> UpdateTime) (flip (+) (single right) >> ClockTime) left
    static member (-) (left, right) = GameTime.unary ((-) (int64 right) >> UpdateTime) (flip (-) (single right) >> ClockTime) left
    static member (*) (left, right) = GameTime.unary ((*) (int64 right) >> UpdateTime) (flip (*) (single right) >> ClockTime) left
    static member (/) (left, right) = GameTime.unary ((/) (int64 right) >> UpdateTime) (flip (/) (single right) >> ClockTime) left
    static member (%) (left, right) = GameTime.unary ((%) (int64 right) >> UpdateTime) (flip (%) (single right) >> ClockTime) left
    static member (+) (left, right) = GameTime.unary ((+) (int64 left) >> UpdateTime) ((+) (single left) >> ClockTime) right
    static member (-) (left, right) = GameTime.unary ((-) (int64 left) >> UpdateTime) ((-) (single left) >> ClockTime) right
    static member (*) (left, right) = GameTime.unary ((*) (int64 left) >> UpdateTime) ((*) (single left) >> ClockTime) right
    static member (/) (left, right) = GameTime.unary ((/) (int64 left) >> UpdateTime) ((/) (single left) >> ClockTime) right
    static member (%) (left, right) = GameTime.unary ((%) (int64 left) >> UpdateTime) ((%) (single left) >> ClockTime) right
    static member op_Implicit (i : int64) = UpdateTime i
    static member op_Implicit (s : single) = ClockTime s
    static member op_Explicit time = match time with UpdateTime time -> int time | ClockTime time -> int time
    static member op_Explicit time = match time with UpdateTime time -> int64 time | ClockTime time -> int64 time
    static member op_Explicit time = match time with UpdateTime time -> single time | ClockTime time -> single time
    static member op_Explicit time = match time with UpdateTime time -> double time | ClockTime time -> double time
    static member isZero time = GameTime.unary isZero isZero time
    static member notZero time = GameTime.unary notZero notZero time
    static member zero = GameTime.make 0L 0.0f
    static member min (left : GameTime) right = if left <= right then left else right
    static member max (left : GameTime) right = if left >= right then left else right
    static member MinValue = GameTime.make Int64.MinValue Single.MinValue
    static member MaxValue = GameTime.make Int64.MaxValue Single.MaxValue

    override this.Equals that =
        match that with
        | :? GameTime as that -> GameTime.equals this that
        | _ -> false

    override this.GetHashCode () =
        GameTime.unary hash hash this

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