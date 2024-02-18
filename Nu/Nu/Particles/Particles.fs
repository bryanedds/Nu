// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu.Particles
open System
open System.Numerics
open Prime
open Nu

/// Describes the life of an instance value.
/// OPTIMIZATION: LifeTimeOpt uses GameTime.zero to represent infinite life.
/// OPTIMIZATION: doesn't use Liveness type to avoid its constructor calls.
/// OPTIMIZATION: pre-computes progress scalar to minimize number of divides.
type [<Struct>] Life =
    { StartTime : GameTime
      LifeTimeOpt : GameTime
      ProgressScalar : single }

    /// The progress made through the instance's life.
    static member getProgress (time : GameTime) life =
        if life.LifeTimeOpt.NotZero
        then single (time - life.StartTime) * life.ProgressScalar
        else 0.0f

    /// The progress made through the instance's life within a sub-range.
    static member getProgress3 (time : GameTime) sublife life =
        if sublife.LifeTimeOpt.NotZero then
            let localTime = time - life.StartTime
            Life.getProgress localTime sublife
        else Life.getProgress time life

    /// The liveness of the instance as a boolean.
    static member getLiveness (time : GameTime) life =
        if life.LifeTimeOpt.NotZero
        then time - life.StartTime < life.LifeTimeOpt
        else true

    /// Make a life value.
    static member make startTime lifeTimeOpt =
        { StartTime = startTime
          LifeTimeOpt = lifeTimeOpt
          ProgressScalar = 1.0f / single lifeTimeOpt }

/// A spatial constraint.
type Constraint =
    | Box of Box3
    | Sphere of single * Vector3
    | Constraints of Constraint SArray

    /// Combine two constraints.
    static member (+) (constrain, constrain2) =
        match (Constraint.isEmpty constrain, Constraint.isEmpty constrain2) with
        | (true, true) -> constrain // OPTIMIZATION: elide ctor
        | (_, true) -> constrain
        | (true, _) -> constrain2
        | (_, _) -> Constraints (SArray.ofList [constrain; constrain2])

    /// The empty constraint.
    static member empty =
        Constraints SArray.empty

    /// Checks that constraint is the empty constraint constant.
    static member isEmpty constrain =
        match constrain with
        | Constraints constraints -> constraints.Length = 0
        | _ -> false

    /// Checks that constraint is not the empty constraint constant.
    static member notEmpty constrain =
        not (Constraint.isEmpty constrain)

/// How a logic is to be applied.
type LogicType =
    | Or of bool
    | Nor of bool
    | Xor of bool
    | And of bool
    | Nand of bool
    | Equal of bool

/// Describes logic of behavior over a section of a target's life time.
type Logic =
    { LogicLife : Life
      LogicType : LogicType }

/// The type of range.
type 'a RangeType =
    | Constant of 'a
    | Linear of 'a * 'a
    | Random of 'a * 'a
    | Chaos of 'a * 'a
    | Ease of 'a * 'a
    | EaseIn of 'a * 'a
    | EaseOut of 'a * 'a
    | Sin of 'a * 'a
    | SinScaled of single * 'a * 'a
    | Cos of 'a * 'a
    | CosScaled of single * 'a * 'a

/// How a range is to be applied.
type RangeApplicator =
    | Sum
    | Delta
    | Scale
    | Ratio
    | Set

/// Describes range of behavior over a section of a target's life time.
type 'a Range =
    { RangeLife : Life
      RangeType : 'a RangeType
      RangeApplicator : RangeApplicator }

/// The forces that may operate on a target.
type Force =
    | Gravity of Vector3
    | Attractor of Vector3 * single * single
    | Drag of single * single
    | Velocity of Constraint

/// Describes the body of an instance value.
type [<Struct>] Body =
    { mutable Position : Vector3
      mutable Scale : Vector3
      mutable Angles : Vector3
      mutable LinearVelocity : Vector3
      mutable AngularVelocity : Vector3
      mutable Restitution : single }

    /// The default body.
    static member defaultBody =
        { Position = v3Zero
          Scale = v3One
          Angles = v3Zero
          LinearVelocity = v3Zero
          AngularVelocity = v3Zero
          Restitution = Constants.Particles.RestitutionDefault }

/// The base particle type.
type Particle =

    /// The life of the particle.
    abstract Life : Life with get, set

/// Describes particles in varying forms.
type [<NoEquality; NoComparison>] ParticlesDescriptor =
    | SpriteParticlesDescriptor of SpriteParticlesDescriptor
    | BillboardParticlesDescriptor of BillboardParticlesDescriptor

/// The output of a behavior.
type [<ReferenceEquality>] Output =
    | OutputEmitter of string * Emitter
    | Outputs of Output SArray

    /// Combine two outputs.
    static member (+) (output, output2) =
        match (Output.isEmpty output, Output.isEmpty output2) with
        | (true, true) -> output // OPTIMIZATION: elide ctor
        | (_, true) -> output
        | (true, _) -> output2
        | (_, _) -> Outputs (SArray.ofList [output; output2])

    /// The empty output.
    static member empty =
        Outputs SArray.empty

    /// Checks that output is the empty output constant.
    static member isEmpty output =
        match output with
        | Outputs outputs -> outputs.Length = 0
        | _ -> false

    /// Checks that output is not the empty output constant.
    static member notEmpty output =
        not (Output.isEmpty output)

/// The base particle emitter type.
and Emitter =

    /// Determine liveness of emitter.
    abstract GetLiveness : GameTime -> Liveness

    /// Run the emitter.
    abstract Run : GameTime -> GameTime -> Output * Emitter

    /// Convert the emitted particles to a ParticlesDescriptor.
    abstract ToParticlesDescriptor : GameTime -> ParticlesDescriptor

    /// Change the maximum number of allowable particles.
    abstract Resize : int -> Emitter

/// Transforms a constrained value.
type 'a Transformer =
    GameTime -> GameTime -> Constraint -> 'a SArray -> Output

[<RequireQualifiedAccess>]
module Transformer =

    /// Accelerate bodies both linearly and angularly.
    let accelerate (delta : GameTime) (bodies : Body SArray) =
        let mutable i = 0
        let scalar = single delta
        while i < dec bodies.Length do
            let body = &bodies.[i]
            body.Position <- body.Position + body.LinearVelocity * scalar
            body.Angles <- body.Angles + body.AngularVelocity * scalar
            i <- inc i

    /// Constrain bodies.
    let rec constrain (delta : GameTime) c (bodies : Body SArray) =
        let scalar = single delta
        match c with
        | Sphere (radius, center) ->
            let mutable i = 0
            while i < dec bodies.Length do
                let body = &bodies.[i]
                let positionNext = body.Position + body.LinearVelocity * scalar
                let distanceDelta = positionNext - center
                let distanceSquared = distanceDelta.MagnitudeSquared
                let radiusSquared = radius * radius
                if distanceSquared < radiusSquared then
                    let normal = Vector3.Normalize (center - positionNext)
                    let reflectedVelocity = Vector3.Reflect (body.LinearVelocity, normal)
                    body.LinearVelocity <- reflectedVelocity * body.Restitution
                i <- inc i
        | Box box ->
            // TODO: implement properly bouncing angles.
            let mutable i = 0
            while i < dec bodies.Length do
                let body = &bodies.[i]
                let positionNext = body.Position + body.LinearVelocity * scalar
                let distanceDelta = positionNext - box.Center
                if box.Intersects positionNext then
                    let speed = body.LinearVelocity.Magnitude
                    let distanceNormalized = Vector3.Normalize distanceDelta
                    body.LinearVelocity <- speed * distanceNormalized * body.Restitution
                i <- inc i
        | Constraints constraints ->
            let mutable i = 0
            while i < dec constraints.Length do
                let constrain' = constraints.[i]
                constrain delta constrain' bodies
                i <- inc i

    /// Make a force transformer.
    let force force : Body Transformer =
        fun delta _ c bodies ->
            match force with
            | Gravity gravity ->
                let scalar = single delta
                let mutable i = 0
                while i < dec bodies.Length do
                    let body = &bodies.[i]
                    body.LinearVelocity <- body.LinearVelocity + gravity * scalar
                    i <- inc i
                Output.empty
            | Attractor (position, radius, force) ->
                let scalar = single delta
                let mutable i = 0
                while i < dec bodies.Length do
                    let body = &bodies.[i]
                    let direction = position - body.Position
                    let distance = direction.Magnitude
                    let normal = direction / distance
                    if distance < radius then
                        let pull = (radius - distance) / radius
                        body.LinearVelocity <- body.LinearVelocity + normal * (pull * force * scalar)
                    i <- inc i
                Output.empty
            | Drag (linearDrag, angularDrag) ->
                let scalar = single delta
                let mutable i = 0
                while i < dec bodies.Length do
                    let body = &bodies.[i]
                    let linearDrag = body.LinearVelocity * linearDrag
                    let angularDrag = body.AngularVelocity * angularDrag
                    body.LinearVelocity <- body.LinearVelocity - linearDrag * scalar
                    body.AngularVelocity <- body.AngularVelocity - angularDrag * scalar
                    i <- inc i
                Output.empty
            | Velocity c2 ->
                let c3 = c + c2
                constrain delta c3 bodies
                accelerate delta bodies
                Output.empty

    /// Make a logic transformer.
    let logic logic : struct (Life * bool) Transformer =
        match logic.LogicType with
        | Or value ->
            fun _ _ _ targets ->
                let mutable i = 0
                while i < dec targets.Length do
                    let v = &targets.[i]
                    let struct (targetLife, targetValue) = v
                    targets.[i] <- struct (targetLife, targetValue || value)
                    i <- inc i
                Output.empty
        | Nor value ->
            fun _ _ _ targets ->
                let mutable i = 0
                while i < dec targets.Length do
                    let v = &targets.[i]
                    let struct (targetLife, targetValue) = v
                    targets.[i] <- struct (targetLife, not targetValue && not value)
                    i <- inc i
                Output.empty
        | Xor value ->
            fun _ _ _ targets ->
                let mutable i = 0
                while i < dec targets.Length do
                    let v = &targets.[i]
                    let struct (targetLife, targetValue) = v
                    targets.[i] <- struct (targetLife, targetValue <> value)
                    i <- inc i
                Output.empty
        | And value ->
            fun _ _ _ targets ->
                let mutable i = 0
                while i < dec targets.Length do
                    let v = &targets.[i]
                    let struct (targetLife, targetValue) = v
                    targets.[i] <- struct (targetLife, targetValue && value)
                    i <- inc i
                Output.empty
        | Nand value ->
            fun _ _ _ targets ->
                let mutable i = 0
                while i < dec targets.Length do
                    let v = &targets.[i]
                    let struct (targetLife, targetValue) = v
                    targets.[i] <- struct (targetLife, not (targetValue && value))
                    i <- inc i
                Output.empty
        | Equal value ->
            fun _ _ _ targets ->
                let mutable i = 0
                while i < dec targets.Length do
                    let v = &targets.[i]
                    let struct (targetLife, _) = v
                    targets.[i] <- struct (targetLife, value)
                    i <- inc i
                Output.empty

    /// Make a generic range transformer.
    let inline rangeSrtp mul div (scale : (^a * single) -> ^a) time (range : ^a Range) : struct (Life * ^a) Transformer =
        let applyRange =
            match range.RangeApplicator with
            | Sum -> fun value value2 -> value + value2
            | Delta -> fun value value2 -> value - value2
            | Scale -> fun value value2 -> mul (value, value2)
            | Ratio -> fun value value2 -> div (value, value2)
            | Set -> fun _ value2 -> value2
        match range.RangeType with
        | Constant value ->
            fun _ _ _ targets ->
                let mutable i = 0
                while i < dec targets.Length do
                    let v = &targets.[i]
                    let struct (targetLife, targetValue) = v
                    targets.[i] <- struct (targetLife, applyRange targetValue value)
                    i <- inc i
                Output.empty
        | Linear (value, value2) ->
            fun _ _ _ targets ->
                let mutable i = 0
                while i < dec targets.Length do
                    let v = &targets.[i]
                    let struct (targetLife, targetValue) = v
                    let progress = Life.getProgress3 time range.RangeLife targetLife
                    let result = applyRange targetValue (value + scale (value2 - value, progress))
                    targets.[i] <- struct (targetLife, result)
                    i <- inc i
                Output.empty
        | Random (value, value2) ->
            fun _ _ _ targets ->
                let mutable i = 0
                while i < dec targets.Length do
                    let v = &targets.[i]
                    let struct (targetLife, targetValue) = v
                    let progress = Life.getProgress3 time range.RangeLife targetLife
                    let rand = Rand.makeFromInt (int ((Math.Max (double progress, 0.000000001)) * double Int32.MaxValue))
                    let randValue = fst (Rand.nextSingle rand)
                    let result = applyRange targetValue (value + scale (value2 - value, randValue))
                    targets.[i] <- struct (targetLife, result)
                    i <- inc i
                Output.empty
        | Chaos (value, value2) ->
            fun _ _ _ targets ->
                let mutable i = 0
                while i < dec targets.Length do
                    let v = &targets.[i]
                    let struct (targetLife, targetValue) = v
                    let chaosValue = Gen.randomf
                    let result = applyRange targetValue (value + scale (value2 - value, chaosValue))
                    targets.[i] <- struct (targetLife, result)
                    i <- inc i
                Output.empty
        | Ease (value, value2) ->
            fun _ _ _ targets ->
                let mutable i = 0
                while i < dec targets.Length do
                    let v = &targets.[i]
                    let struct (targetLife, targetValue) = v
                    let progress = Life.getProgress3 time range.RangeLife targetLife
                    let progressEase = single (Math.Pow (Math.Sin (Math.PI * double progress * 0.5), 2.0))
                    let result = applyRange targetValue (value + scale (value2 - value, progressEase))
                    targets.[i] <- struct (targetLife, result)
                    i <- inc i
                Output.empty
        | EaseIn (value, value2) ->
            fun _ _ _ targets ->
                let mutable i = 0
                while i < dec targets.Length do
                    let v = &targets.[i]
                    let struct (targetLife, targetValue) = v
                    let progress = Life.getProgress3 time range.RangeLife targetLife
                    let progressScaled = float progress * Math.PI * 0.5
                    let progressEaseIn = 1.0 + Math.Sin (progressScaled + Math.PI * 1.5)
                    let result = applyRange targetValue (value + scale (value2 - value, single progressEaseIn))
                    targets.[i] <- struct (targetLife, result)
                    i <- inc i
                Output.empty
        | EaseOut (value, value2) ->
            fun _ _ _ targets ->
                let mutable i = 0
                while i < dec targets.Length do
                    let v = &targets.[i]
                    let struct (targetLife, targetValue) = v
                    let progress = Life.getProgress3 time range.RangeLife targetLife
                    let progressScaled = float progress * Math.PI * 0.5
                    let progressEaseOut = Math.Sin progressScaled
                    let result = applyRange targetValue (value + scale (value2 - value, single progressEaseOut))
                    targets.[i] <- struct (targetLife, result)
                    i <- inc i
                Output.empty
        | Sin (value, value2) ->
            fun _ _ _ targets ->
                let mutable i = 0
                while i < dec targets.Length do
                    let v = &targets.[i]
                    let struct (targetLife, targetValue) = v
                    let progress = Life.getProgress3 time range.RangeLife targetLife
                    let progressScaled = float progress * Math.PI * 2.0
                    let progressSin = Math.Sin progressScaled
                    let result = applyRange targetValue (value + scale (value2 - value, single progressSin))
                    targets.[i] <- struct (targetLife, result)
                    i <- inc i
                Output.empty
        | SinScaled (scalar, value, value2) ->
            fun _ _ _ targets ->
                let mutable i = 0
                while i < dec targets.Length do
                    let v = &targets.[i]
                    let struct (targetLife, targetValue) = v
                    let progress = Life.getProgress3 time range.RangeLife targetLife
                    let progressScaled = float progress * Math.PI * 2.0 * float scalar
                    let progressSin = Math.Sin progressScaled
                    let result = applyRange targetValue (value + scale (value2 - value, single progressSin))
                    targets.[i] <- struct (targetLife, result)
                    i <- inc i
                Output.empty
        | Cos (value, value2) ->
            fun _ _ _ targets ->
                let mutable i = 0
                while i < dec targets.Length do
                    let v = &targets.[i]
                    let struct (targetLife, targetValue) = v
                    let progress = Life.getProgress3 time range.RangeLife targetLife
                    let progressScaled = float progress * Math.PI * 2.0
                    let progressCos = Math.Cos progressScaled
                    let result = applyRange targetValue (value + scale (value2 - value, single progressCos))
                    targets.[i] <- struct (targetLife, result)
                    i <- inc i
                Output.empty
        | CosScaled (scalar, value, value2) ->
            fun _ _ _ targets ->
                let mutable i = 0
                while i < dec targets.Length do
                    let v = &targets.[i]
                    let struct (targetLife, targetValue) = v
                    let progress = Life.getProgress3 time range.RangeLife targetLife
                    let progressScaled = float progress * Math.PI * 2.0 * float scalar
                    let progressCos = Math.Cos progressScaled
                    let result = applyRange targetValue (value + scale (value2 - value, single progressCos))
                    targets.[i] <- struct (targetLife, result)
                    i <- inc i
                Output.empty

    /// Make an int range transformer.
    let rangeInt time range = rangeSrtp (fun (x : int, y) -> x * y) (fun (x, y) -> x / y) (fun (x, y) -> int (single x * y)) time range

    /// Make an int64 range transformer.
    let rangeInt64 time range = rangeSrtp (fun (x : int64, y) -> x * y) (fun (x, y) -> x / y) (fun (x, y) -> int64 (single x * y)) time range

    /// Make a single range transformer.
    let rangeSingle time range = rangeSrtp (fun (x : single, y) -> x * y) (fun (x, y) -> x / y) (fun (x, y) -> x * y) time range

    /// Make a double range transformer.
    let rangeDouble time range = rangeSrtp (fun (x : double, y) -> x * y) (fun (x, y) -> x / y) (fun (x, y) -> double (single x * y)) time range

    /// Make a Vector2 range transformer.
    let rangeVector2 time range = rangeSrtp Vector2.Multiply Vector2.Divide Vector2.op_Multiply time range

    /// Make a Vector3 range transformer.
    let rangeVector3 time range = rangeSrtp Vector3.Multiply Vector3.Divide Vector3.op_Multiply time range

    /// Make a Vector4 range transformer.
    let rangeVector4 time range = rangeSrtp Vector4.Multiply Vector4.Divide Vector4.op_Multiply time range

    /// Make a Color range transformer.
    let rangeColor time range = rangeSrtp Color.Multiply Color.Divide Color.op_Multiply time range

/// Scopes transformable values.
type [<ReferenceEquality>] Scope<'a, 'b when 'a : struct> =
    { In : 'a SArray -> 'b SArray
      Out : Output -> 'b SArray -> 'a SArray -> Output }

type In<'a, 'b when 'a : struct> =
    delegate of 'a byref * 'b byref -> unit

type Out<'a, 'b when 'a : struct> =
    delegate of 'b byref * 'a byref -> unit

[<RequireQualifiedAccess>]
module Scope =

    /// Make a scope with in-place in and out delegates.
    let inline make<'a, 'b when 'a : struct> (getField : In<'a, 'b>) (setField : Out<'a, 'b>) : Scope<'a, 'b> =
        { In =
            fun (targets : 'a SArray) ->
                let fields = SArray.zeroCreate targets.Length
                let mutable i = 0
                while i < dec targets.Length do
                    getField.Invoke (&targets.[i], &fields.[i])
                    i <- inc i
                fields
          Out = fun output fields (targets : 'a SArray) ->
            let mutable i = 0
            while i < dec targets.Length do
                setField.Invoke (&fields.[i], &targets.[i])
                i <- inc i
            output }

/// The base behavior type.
type Behavior =

    /// Run the behavior over a single target.
    abstract Run : GameTime -> GameTime -> Constraint -> obj -> (Output * obj)

    /// Run the behavior over multiple targets.
    abstract RunMany : GameTime -> GameTime -> Constraint -> obj -> Output

/// Defines a generic behavior.
type [<ReferenceEquality>] Behavior<'a, 'b when 'a : struct> =
    { Scope : Scope<'a, 'b>
      Transformers : 'b Transformer FStack }

    /// The singleton behavior.
    static member singleton scope transformer =
        { Scope = scope; Transformers = FStack.singleton transformer }

    /// Make from a scope and sequence of transformers.
    static member ofSeq scope transformers =
        { Scope = scope; Transformers = FStack.ofSeq transformers }

    /// Run the behavior over targets.
    /// OPTIMIZATION: runs transformers in batches for better utilization of instruction cache.
    static member runMany delta time (constrain : Constraint) (behavior : Behavior<'a, 'b>) (targets : 'a SArray) =
        let fields = behavior.Scope.In targets
        let output =
            FStack.fold (fun output transformer ->
                output + transformer delta time constrain fields)
                Output.empty
                behavior.Transformers
        behavior.Scope.Out output fields targets

    /// Run the behavior over a single target.
    static member run delta time (constrain : Constraint) (behavior : Behavior<'a, 'b>) (target : 'a) =
        let targets = SArray.singleton target
        let output = Behavior<'a, 'b>.runMany delta time constrain behavior targets
        let target = targets.[0]
        (output, target)

    interface Behavior with
        member this.Run delta time constrain targetObj =
            let (outputs, target) = Behavior<'a, 'b>.run delta time constrain this (targetObj :?> 'a)
            (outputs, target :> obj)
        member this.RunMany delta time constrain targetsObj =
            Behavior<'a, 'b>.runMany delta time constrain this (targetsObj :?> 'a SArray)

/// A composition of behaviors.
type [<ReferenceEquality>] Behaviors =
    { Behaviors : Behavior FStack }

    /// The empty behaviors.
    static member empty =
        { Behaviors = FStack.empty }

    /// The singleton behaviors.
    static member singleton behavior =
        { Behaviors = FStack.singleton behavior }

    /// Make from a sequence of behaviors.
    static member ofSeq behaviors =
        { Behaviors = FStack.ofSeq behaviors }

    /// Add a behavior.
    static member add behavior behaviors =
        { Behaviors = FStack.conj behavior behaviors.Behaviors }

    /// Add multiple behaviors.
    static member addMany behaviorsMany behaviors =
        { Behaviors = Seq.fold (fun behaviors behavior -> FStack.conj behavior behaviors) behaviors.Behaviors behaviorsMany }

    /// Run the behaviors over a single target.
    static member run delta time behaviors constrain (target : 'a) =
        let (outputs, targets) =
            FStack.fold (fun (output, target) (behavior : Behavior) ->
                let (output2, targets2) = behavior.Run delta time constrain target
                (output + output2, targets2))
                (Output.empty, target :> obj)
                behaviors.Behaviors
        (outputs, targets :?> 'a)

    /// Run the behaviors over targets.
    static member runMany delta time behaviors constrain (targets : 'a SArray) =
        let outputs =
            FStack.fold (fun output (behavior : Behavior) ->
                output + behavior.RunMany delta time constrain targets)
                Output.empty
                behaviors.Behaviors
        outputs

/// A basic particle.
type [<SymbolicExpansion; Struct>] BasicParticle =
    { mutable Life : Life
      mutable Body : Body
      mutable Offset : Vector3
      mutable Size : Vector3
      mutable Inset : Box2
      mutable Color : Color
      mutable Emission : Color
      mutable Flip : Flip }
    interface Particle with member this.Life with get () = this.Life and set value = this.Life <- value

[<RequireQualifiedAccess>]
module BasicParticle =
    let body = Scope.make (new In<_, _> (fun p v -> v <- p.Body)) (new Out<_, _> (fun v p -> p.Body <- v))
    let position = Scope.make (new In<_, _> (fun p v -> v <- struct (p.Life, p.Body.Position))) (new Out<_, _> (fun v p -> p.Body.Position <- snd' v))
    let scale = Scope.make (new In<_, _> (fun p v -> v <- struct (p.Life, p.Body.Scale))) (new Out<_, _> (fun v p -> p.Body.Scale <- snd' v))
    let angles = Scope.make (new In<_, _> (fun p v -> v <- struct (p.Life, p.Body.Angles))) (new Out<_, _> (fun v p -> p.Body.Angles <- snd' v))
    let offset = Scope.make (new In<_, _> (fun p v -> v <- struct (p.Life, p.Offset))) (new Out<_, _> (fun v p -> p.Offset <- snd' v))
    let size = Scope.make (new In<_, _> (fun p v -> v <- struct (p.Life, p.Size))) (new Out<_, _> (fun v p -> p.Size <- snd' v))
    let inset = Scope.make (new In<_, _> (fun p v -> v <- struct (p.Life, p.Inset))) (new Out<_, _> (fun v p -> p.Inset <- snd' v))
    let color = Scope.make (new In<_, _> (fun p v -> v <- struct (p.Life, p.Color))) (new Out<_, _> (fun v p -> p.Color <- snd' v))
    let emission = Scope.make (new In<_, _> (fun p v -> v <- struct (p.Life, p.Emission))) (new Out<_, _> (fun v p -> p.Emission <- snd' v))
    let flipH =
        Scope.make
            (new In<_, _> (fun p v ->
                let flipH = match p.Flip with FlipNone -> false | FlipH -> true | FlipV -> false | FlipHV -> true
                v <- struct (p.Life, flipH)))
            (new Out<_, _> (fun v p ->
                let flip =
                    match (p.Flip, snd' v) with
                    | (FlipNone, true) -> FlipH
                    | (FlipH, true) -> FlipH
                    | (FlipV, true) -> FlipHV
                    | (FlipHV, true) -> FlipHV
                    | (FlipNone, false) -> FlipNone
                    | (FlipH, false) -> FlipNone
                    | (FlipV, false) -> FlipV
                    | (FlipHV, false) -> FlipV
                p.Flip <- flip))
    let flipV =
        Scope.make
            (new In<_, _> (fun p v ->
                let flipV = match p.Flip with FlipNone -> false | FlipH -> false | FlipV -> true | FlipHV -> true
                v <- struct (p.Life, flipV)))
            (new Out<_, _> (fun v p ->
                let flip =
                    match (p.Flip, snd' v) with
                    | (FlipNone, true) -> FlipV
                    | (FlipH, true) -> FlipHV
                    | (FlipV, true) -> FlipV
                    | (FlipHV, true) -> FlipHV
                    | (FlipNone, false) -> FlipNone
                    | (FlipH, false) -> FlipH
                    | (FlipV, false) -> FlipNone
                    | (FlipHV, false) -> FlipH
                p.Flip <- flip))

/// A particle system.
/// TODO: consider making this an abstract data type?
type [<ReferenceEquality>] ParticleSystem =
    { Emitters : Map<string, Emitter> }
    
    /// Get the liveness of the particle system.
    static member getLiveness time particleSystem =
        let emittersLiveness =
            Map.exists (fun _ (emitter : Emitter) ->
                match emitter.GetLiveness time with Live -> true | Dead -> false)
                particleSystem.Emitters
        if emittersLiveness then Live else Dead

    /// Add an emitter to the particle system.
    static member add emitterId emitter particleSystem =
        { particleSystem with Emitters = Map.add emitterId emitter particleSystem.Emitters }

    /// Remove an emitter from the particle system.
    static member remove emitterId particleSystem =
        { particleSystem with Emitters = Map.remove emitterId particleSystem.Emitters }

    /// Run the particle system.
    static member run delta time particleSystem =
        let (output, emitters) =
            Map.fold (fun (output, emitters) emitterId (emitter : Emitter) ->
                let (output2, emitter) = emitter.Run delta time
                let emitters = match emitter.GetLiveness time with Live -> Map.add emitterId emitter emitters | Dead -> emitters
                (output + output2, emitters))
                (Output.empty, Map.empty)
                particleSystem.Emitters
        let particleSystem = { Emitters = emitters }
        (particleSystem, output)

    /// Convert the emitted particles to ParticlesDescriptors.
    static member toParticlesDescriptors time particleSystem =
        let descriptorsRev =
            Map.fold (fun descriptors _ (emitter : Emitter) ->
                (emitter.ToParticlesDescriptor time :: descriptors))
                [] particleSystem.Emitters
        List.rev descriptorsRev

    /// The empty particle system.
    static member empty =
        { Emitters = Map.empty }

/// The static sprite-based particle emitter.
type [<ReferenceEquality>] StaticSpriteEmitter<'a when 'a :> Particle and 'a : equality and 'a : struct> =
    { mutable Body : Body // mutable for animation
      Absolute : bool
      Elevation : single
      Blend : Blend
      Image : Image AssetTag
      Life : Life
      ParticleLifeTimeMaxOpt : GameTime // OPTIMIZATION: uses GameTime.zero to represent infinite particle life.
      ParticleRate : single
      mutable ParticleIndex : int // the current particle buffer insertion point
      mutable ParticleWatermark : int // tracks the highest active particle index; never decreases.
      ParticleRing : 'a SArray // operates as a ring-buffer
      ParticleSeed : 'a
      Constraint : Constraint
      ParticleInitializer : GameTime -> 'a StaticSpriteEmitter -> 'a
      ParticleBehavior : GameTime -> 'a StaticSpriteEmitter -> Output
      ParticleBehaviors : Behaviors
      EmitterBehavior : GameTime -> 'a StaticSpriteEmitter -> Output
      EmitterBehaviors : Behaviors
      ToParticlesDescriptor : GameTime -> 'a StaticSpriteEmitter -> SpriteParticlesDescriptor }

    static member private emit time emitter =
        let particle = &emitter.ParticleRing.[emitter.ParticleIndex]
        particle <- emitter.ParticleInitializer time emitter
        particle.Life <- Life.make time particle.Life.LifeTimeOpt
        emitter.ParticleIndex <-
            if emitter.ParticleIndex < dec emitter.ParticleRing.Length
            then inc emitter.ParticleIndex
            else 0
        emitter.ParticleWatermark <-
            if emitter.ParticleIndex <= emitter.ParticleWatermark
            then emitter.ParticleWatermark
            else emitter.ParticleIndex

    /// Determine emitter's liveness.
    static member getLiveness time emitter =
        if emitter.ParticleLifeTimeMaxOpt.NotZero then
            if Life.getLiveness (time - emitter.ParticleLifeTimeMaxOpt) emitter.Life
            then Live
            else Dead
        else Live

    /// Run the emitter.
    static member run delta time (emitter : 'a StaticSpriteEmitter) =

        // determine local time
        let localTime = time - emitter.Life.StartTime
        let localTimePrevious = localTime - delta

        // emit new particles if live
        if Life.getLiveness time emitter.Life then
            let emitCount = single localTime * emitter.ParticleRate
            let emitCountPrevious = single localTimePrevious * emitter.ParticleRate
            let emitCount = int emitCount - int emitCountPrevious
            for _ in 0 .. emitCount - 1 do StaticSpriteEmitter<'a>.emit time emitter

        // update emitter in-place
        let output = emitter.EmitterBehavior time emitter

        // update emitter compositionally
        let (output2, emitter) = Behaviors.run delta time emitter.EmitterBehaviors emitter.Constraint emitter

        // update existing particles in-place
        let output3 = emitter.ParticleBehavior time emitter

        // update existing particles compositionally
        let output4 = Behaviors.runMany delta time emitter.ParticleBehaviors emitter.Constraint emitter.ParticleRing

        // fin
        (output + output2 + output3 + output4, emitter)

    /// Make a basic particle emitter.
    static member make<'a>
        time body absolute elevation blend image lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax particleSeed
        constrain particleInitializer particleBehavior particleBehaviors emitterBehavior emitterBehaviors toParticlesDescriptor : 'a StaticSpriteEmitter =
        { Body = body
          Absolute = absolute
          Elevation = elevation
          Blend = blend
          Image = image
          Life = Life.make time lifeTimeOpt
          ParticleLifeTimeMaxOpt = particleLifeTimeMaxOpt
          ParticleRate = particleRate
          ParticleIndex = 0
          ParticleWatermark = 0
          ParticleRing = SArray.zeroCreate particleMax
          ParticleSeed = particleSeed
          Constraint = constrain
          ParticleInitializer = particleInitializer
          ParticleBehavior = particleBehavior
          ParticleBehaviors = particleBehaviors
          EmitterBehavior = emitterBehavior
          EmitterBehaviors = emitterBehaviors
          ToParticlesDescriptor = toParticlesDescriptor }

    interface Emitter with
        member this.GetLiveness time =
            StaticSpriteEmitter<'a>.getLiveness time this
        member this.Run delta time =
            let (output, emitter) = StaticSpriteEmitter<'a>.run delta time this
            (output, emitter :> Emitter)
        member this.ToParticlesDescriptor time =
            SpriteParticlesDescriptor (this.ToParticlesDescriptor time this)
        member this.Resize particleMax =
            if  this.ParticleRing.Length <> particleMax then
                this.ParticleIndex <- 0
                this.ParticleWatermark <- 0
                { this with ParticleRing = SArray.zeroCreate<'a> particleMax } :> Emitter
            else this :> Emitter

/// A static sprite particle emitter.
type BasicStaticSpriteEmitter =
    StaticSpriteEmitter<BasicParticle>

[<RequireQualifiedAccess>]
module BasicStaticSpriteEmitter =

    let private toParticlesDescriptor time (emitter : BasicStaticSpriteEmitter) =
        let particles =
            SArray.append
                (if emitter.ParticleWatermark > emitter.ParticleIndex
                 then SArray.skip emitter.ParticleIndex emitter.ParticleRing
                 else SArray.empty)
                (SArray.take emitter.ParticleIndex emitter.ParticleRing)
        let particles' =
            SArray.zeroCreate<Nu.Particle> particles.Length
        for index in 0 .. particles.Length - 1 do
            let particle = &particles.[index]
            if Life.getLiveness time particle.Life then
                let particle' = &particles'.[index]
                particle'.Transform.Position <- particle.Body.Position
                particle'.Transform.Scale <- particle.Body.Scale
                particle'.Transform.Angles <- particle.Body.Angles
                particle'.Transform.Offset <- particle.Offset
                particle'.Transform.Size <- particle.Size
                particle'.Transform.PerimeterCentered <- true
                particle'.Color <- particle.Color
                particle'.Emission <- particle.Emission
                particle'.InsetOpt <- if particle.Inset.Equals box2Zero then ValueNone else ValueSome particle.Inset
                particle'.Flip <- particle.Flip
        { Absolute = emitter.Absolute
          Elevation = emitter.Elevation
          Horizon = emitter.Body.Position.Y
          Blend = emitter.Blend
          Image = emitter.Image
          Particles = particles' }

    /// Resize the emitter.
    let resize particleMax (emitter : BasicStaticSpriteEmitter) =
        let particleMax = max 1 particleMax
        (emitter :> Emitter).Resize particleMax :?> BasicStaticSpriteEmitter

    /// Make a basic static sprite particle emitter.
    let make
        time body elevation absolute blend image lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax particleSeed
        constrain particleInitializer particleBehavior particleBehaviors emitterBehavior emitterBehaviors =
        BasicStaticSpriteEmitter.make
            time body elevation absolute blend image lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax particleSeed
            constrain particleInitializer particleBehavior particleBehaviors emitterBehavior emitterBehaviors toParticlesDescriptor

    /// Make an empty basic sprite particle emitter.
    let makeEmpty time lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax =
        let image = asset Assets.Default.PackageName Assets.Default.ImageName
        let particleSeed = Unchecked.defaultof<BasicParticle>
        let particleInitializer = fun _ (emitter : BasicStaticSpriteEmitter) -> emitter.ParticleSeed
        let particleBehavior = fun _ _ -> Output.empty
        let particleBehaviors = Behaviors.empty
        let emitterBehavior = fun _ _ -> Output.empty
        let emitterBehaviors = Behaviors.empty
        make
            time Body.defaultBody false 0.0f Transparent image lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax particleSeed
            Constraint.empty particleInitializer particleBehavior particleBehaviors emitterBehavior emitterBehaviors

    /// Make the default basic sprite particle emitter.
    let makeDefault time lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax =
        let image = asset Assets.Default.PackageName Assets.Default.ImageName
        let particleSeed =
            { Life = Life.make GameTime.zero (GameTime.ofSeconds 2.0f)
              Body = Body.defaultBody
              Offset = v3Zero
              Size = Constants.Engine.Particle2dSizeDefault
              Inset = box2Zero
              Color = Color.One
              Emission = Color.Zero
              Flip = FlipNone }
        let particleScalar =
            match Constants.GameTime.DesiredFrameRate with
            | StaticFrameRate frameRate -> 1.0f / single frameRate
            | DynamicFrameRate _ -> 1.0f
        let particleInitializer = fun _ (emitter : BasicStaticSpriteEmitter) ->
            let particle = emitter.ParticleSeed
            particle.Body.Position <- emitter.Body.Position
            particle.Body.Angles <- emitter.Body.Angles
            particle.Body.LinearVelocity <- (v3 (Gen.randomf - 0.5f) Gen.randomf 0.0f) * v3Dup 1000.0f * particleScalar
            particle.Body.AngularVelocity <- v3 0.0f 0.0f (Gen.randomf * 30.0f - 15.0f) * particleScalar
            particle
        let particleBehavior = fun time emitter ->
            let watermark = emitter.ParticleWatermark
            let mutable index = 0
            while index <= watermark do
                let particle = &emitter.ParticleRing.[index]
                let progress = Life.getProgress time particle.Life
                particle.Color.A <- 1.0f - progress
                index <- inc index
            Output.empty
        let gravity =
            match Constants.GameTime.DesiredFrameRate with
            | StaticFrameRate frameRate -> v3 0.0f -Constants.Engine.Meter2d 0.0f / single frameRate
            | DynamicFrameRate _ -> v3 0.0f -Constants.Engine.Meter2d 0.0f * Constants.Engine.Meter2d
        let particleBehaviors =
            Behaviors.singleton
                (Behavior.ofSeq BasicParticle.body
                    [Transformer.force (Gravity gravity)
                     Transformer.force (Velocity Constraint.empty)])
        let emitterBehavior = fun _ (emitter : BasicStaticSpriteEmitter) ->
            emitter.Body.Angles <- emitter.Body.Angles + v3 0.0f 0.0f 0.1f
            Output.empty
        let emitterBehaviors =
            Behaviors.empty
        make
            time Body.defaultBody false 0.0f Transparent image lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax particleSeed
            Constraint.empty particleInitializer particleBehavior particleBehaviors emitterBehavior emitterBehaviors

/// Describes a sprite emitter.
type [<ReferenceEquality>] SpriteEmitterDescriptor<'a when 'a :> Particle and 'a : struct> =
    { Body : Body
      Blend : Blend
      Image : Image AssetTag
      LifeTimeOpt : GameTime
      ParticleLifeTimeMaxOpt : GameTime
      ParticleRate : single
      ParticleMax : int
      ParticleSeed : 'a
      Constraint : Constraint
      Style : string }

/// A map of basic sprite emitters.
type SpriteEmitterDescriptors<'a when 'a :> Particle and 'a : struct> =
    Map<string, 'a SpriteEmitterDescriptor>

/// Describes a basic sprite emitter.
type BasicSpriteEmitterDescriptor =
    BasicParticle SpriteEmitterDescriptor

/// A map of basic sprite emitters.
type BasicSpriteEmitterDescriptors =
    BasicParticle SpriteEmitterDescriptors

/// The static billboard-based particle emitter.
type [<ReferenceEquality>] StaticBillboardEmitter<'a when 'a :> Particle and 'a : equality and 'a : struct> =
    { mutable Body : Body // mutable for animation
      Absolute : bool
      MaterialProperties : MaterialProperties
      Material : Material
      RenderType : RenderType
      Life : Life
      ParticleLifeTimeMaxOpt : GameTime // OPTIMIZATION: uses GameTime.zero to represent infinite particle life.
      ParticleRate : single
      mutable ParticleIndex : int // the current particle buffer insertion point
      mutable ParticleWatermark : int // tracks the highest active particle index; never decreases.
      ParticleRing : 'a SArray // operates as a ring-buffer
      ParticleSeed : 'a
      Constraint : Constraint
      ParticleInitializer : GameTime -> 'a StaticBillboardEmitter -> 'a
      ParticleBehavior : GameTime -> 'a StaticBillboardEmitter -> Output
      ParticleBehaviors : Behaviors
      EmitterBehavior : GameTime -> 'a StaticBillboardEmitter -> Output
      EmitterBehaviors : Behaviors
      ToParticlesDescriptor : GameTime -> 'a StaticBillboardEmitter -> BillboardParticlesDescriptor }

    static member private emit time emitter =
        let particle = &emitter.ParticleRing.[emitter.ParticleIndex]
        particle <- emitter.ParticleInitializer time emitter
        particle.Life <- Life.make time particle.Life.LifeTimeOpt
        emitter.ParticleIndex <-
            if emitter.ParticleIndex < dec emitter.ParticleRing.Length
            then inc emitter.ParticleIndex
            else 0
        emitter.ParticleWatermark <-
            if emitter.ParticleIndex <= emitter.ParticleWatermark
            then emitter.ParticleWatermark
            else emitter.ParticleIndex

    /// Determine emitter's liveness.
    static member getLiveness time emitter =
        if emitter.ParticleLifeTimeMaxOpt.NotZero then
            if Life.getLiveness (time - emitter.ParticleLifeTimeMaxOpt) emitter.Life
            then Live
            else Dead
        else Live

    /// Run the emitter.
    static member run delta time (emitter : 'a StaticBillboardEmitter) =

        // determine local time
        let localTime = time - emitter.Life.StartTime
        let localTimePrevious = localTime - delta

        // emit new particles if live
        if Life.getLiveness time emitter.Life then
            let emitCount = single localTime * emitter.ParticleRate
            let emitCountPrevious = single localTimePrevious * emitter.ParticleRate
            let emitCount = int emitCount - int emitCountPrevious
            for _ in 0 .. emitCount - 1 do StaticBillboardEmitter<'a>.emit time emitter

        // update emitter in-place
        let output = emitter.EmitterBehavior time emitter

        // update emitter compositionally
        let (output2, emitter) = Behaviors.run delta time emitter.EmitterBehaviors emitter.Constraint emitter

        // update existing particles in-place
        let output3 = emitter.ParticleBehavior time emitter

        // update existing particles compositionally
        let output4 = Behaviors.runMany delta time emitter.ParticleBehaviors emitter.Constraint emitter.ParticleRing

        // fin
        (output + output2 + output3 + output4, emitter)

    /// Make a basic particle emitter.
    static member make<'a>
        time body absolute materialProperties material
        renderType lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax particleSeed
        constrain particleInitializer particleBehavior particleBehaviors emitterBehavior emitterBehaviors toParticlesDescriptor : 'a StaticBillboardEmitter =
        let particleMax = max 1 particleMax
        { Body = body
          Absolute = absolute
          MaterialProperties = materialProperties
          Material = material
          RenderType = renderType
          Life = Life.make time lifeTimeOpt
          ParticleLifeTimeMaxOpt = particleLifeTimeMaxOpt
          ParticleRate = particleRate
          ParticleIndex = 0
          ParticleWatermark = 0
          ParticleRing = SArray.zeroCreate particleMax
          ParticleSeed = particleSeed
          Constraint = constrain
          ParticleInitializer = particleInitializer
          ParticleBehavior = particleBehavior
          ParticleBehaviors = particleBehaviors
          EmitterBehavior = emitterBehavior
          EmitterBehaviors = emitterBehaviors
          ToParticlesDescriptor = toParticlesDescriptor }

    interface Emitter with
        member this.GetLiveness time =
            StaticBillboardEmitter<'a>.getLiveness time this
        member this.Run delta time =
            let (output, emitter) = StaticBillboardEmitter<'a>.run delta time this
            (output, emitter :> Emitter)
        member this.ToParticlesDescriptor time =
            BillboardParticlesDescriptor (this.ToParticlesDescriptor time this)
        member this.Resize particleMax =
            let particleMax = max 1 particleMax
            if  this.ParticleRing.Length <> particleMax then
                this.ParticleIndex <- 0
                this.ParticleWatermark <- 0
                { this with ParticleRing = SArray.zeroCreate<'a> particleMax } :> Emitter
            else this :> Emitter

/// A static billboard particle emitter.
type BasicStaticBillboardEmitter =
    StaticBillboardEmitter<BasicParticle>

[<RequireQualifiedAccess>]
module BasicStaticBillboardEmitter =

    let private toParticlesDescriptor time (emitter : BasicStaticBillboardEmitter) =
        let particles =
            SArray.append
                (if emitter.ParticleWatermark > emitter.ParticleIndex
                 then SArray.skip emitter.ParticleIndex emitter.ParticleRing
                 else SArray.empty)
                (SArray.take emitter.ParticleIndex emitter.ParticleRing)
        let particles' =
            SArray.zeroCreate<Nu.Particle> particles.Length
        for index in 0 .. particles.Length - 1 do
            let particle = &particles.[index]
            if Life.getLiveness time particle.Life then
                let particle' = &particles'.[index]
                particle'.Transform.Position <- particle.Body.Position
                particle'.Transform.Scale <- particle.Body.Scale
                particle'.Transform.Angles <- particle.Body.Angles
                particle'.Transform.Offset <- particle.Offset
                particle'.Transform.Size <- particle.Size
                particle'.Transform.PerimeterCentered <- true
                particle'.Color <- particle.Color
                particle'.Emission <- particle.Emission
                particle'.InsetOpt <- if particle.Inset.Equals box2Zero then ValueNone else ValueSome particle.Inset
                particle'.Flip <- particle.Flip
        let descriptor =
            { BillboardParticlesDescriptor.Absolute = emitter.Absolute
              MaterialProperties = emitter.MaterialProperties
              Material = emitter.Material
              Particles = particles'
              RenderType = emitter.RenderType }
        descriptor

    /// Resize the emitter.
    let resize particleMax (emitter : BasicStaticBillboardEmitter) =
        (emitter :> Emitter).Resize particleMax :?> BasicStaticBillboardEmitter

    /// Make a basic static billboard particle emitter.
    let make
        time body absolute materialProperties material
        renderType lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax particleSeed
        constrain particleInitializer particleBehavior particleBehaviors emitterBehavior emitterBehaviors =
        BasicStaticBillboardEmitter.make
            time body absolute materialProperties material
            renderType lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax particleSeed
            constrain particleInitializer particleBehavior particleBehaviors emitterBehavior emitterBehaviors toParticlesDescriptor

    /// Make an empty basic billboard particle emitter.
    let makeEmpty time lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax =
        let particleSeed = Unchecked.defaultof<BasicParticle>
        let particleInitializer = fun _ (emitter : BasicStaticBillboardEmitter) -> emitter.ParticleSeed
        let particleBehavior = fun _ _ -> Output.empty
        let particleBehaviors = Behaviors.empty
        let emitterBehavior = fun _ _ -> Output.empty
        let emitterBehaviors = Behaviors.empty
        make
            time Body.defaultBody false MaterialProperties.defaultProperties Material.defaultMaterial
            (ForwardRenderType (0.0f, 0.0f)) lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax particleSeed
            Constraint.empty particleInitializer particleBehavior particleBehaviors emitterBehavior emitterBehaviors

    /// Make the default basic billboard particle emitter.
    let makeDefault time lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax =
        let particleSeed =
            { Life = Life.make GameTime.zero (GameTime.ofSeconds 2.0f)
              Body = Body.defaultBody
              Offset = v3Zero
              Size = Constants.Engine.Particle3dSizeDefault
              Inset = box2Zero
              Color = Color.One
              Emission = Color.Zero
              Flip = FlipNone }
        let particleScalar =
            match Constants.GameTime.DesiredFrameRate with
            | StaticFrameRate frameRate -> 1.0f / single frameRate
            | DynamicFrameRate _ -> 1.0f
        let particleInitializer = fun _ (emitter : BasicStaticBillboardEmitter) ->
            let particle = emitter.ParticleSeed
            particle.Body.Position <- emitter.Body.Position
            particle.Body.Angles <- emitter.Body.Angles
            particle.Body.LinearVelocity <- (v3 (Gen.randomf - 0.25f) Gen.randomf (Gen.randomf - 0.25f)) * v3Dup 20.0f * particleScalar
            particle.Body.AngularVelocity <- v3 0.0f 0.0f (Gen.randomf * 30.0f - 15.0f) * particleScalar
            particle
        let particleBehavior = fun time emitter ->
            let watermark = emitter.ParticleWatermark
            let mutable index = 0
            while index <= watermark do
                let particle = &emitter.ParticleRing.[index]
                let progress = Life.getProgress time particle.Life
                particle.Color.A <- 1.0f - progress
                index <- inc index
            Output.empty
        let gravity =
            match Constants.GameTime.DesiredFrameRate with
            | StaticFrameRate frameRate -> v3 0.0f -Constants.Engine.Meter3d 0.0f / single frameRate
            | DynamicFrameRate _ -> v3 0.0f -Constants.Engine.Meter3d 0.0f * Constants.Engine.Meter3d
        let particleBehaviors =
            Behaviors.singleton
                (Behavior.ofSeq BasicParticle.body
                    [Transformer.force (Gravity gravity)
                     Transformer.force (Velocity Constraint.empty)])
        let emitterBehavior = fun _ (emitter : BasicStaticBillboardEmitter) ->
            emitter.Body.Angles <- emitter.Body.Angles + v3 0.0f 0.0f 0.1f
            Output.empty
        let emitterBehaviors =
            Behaviors.empty
        make
            time Body.defaultBody false MaterialProperties.defaultProperties Material.defaultMaterial
            (ForwardRenderType (0.0f, 0.0f)) lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax particleSeed
            Constraint.empty particleInitializer particleBehavior particleBehaviors emitterBehavior emitterBehaviors

/// Describes a billboard emitter.
type [<ReferenceEquality>] BillboardEmitterDescriptor<'a when 'a :> Particle and 'a : struct> =
    { Body : Body
      Material : Material
      RenderType : RenderType
      LifeTimeOpt : GameTime
      ParticleLifeTimeMaxOpt : GameTime
      ParticleRate : single
      ParticleMax : int
      ParticleSeed : 'a
      Constraint : Constraint
      Style : string }

/// A map of basic billboard emitters.
type BillboardEmitterDescriptors<'a when 'a :> Particle and 'a : struct> =
    Map<string, 'a BillboardEmitterDescriptor>

/// Describes a basic billboard emitter.
type BasicBillboardEmitterDescriptor =
    BasicParticle BillboardEmitterDescriptor

/// A map of basic billboard emitters.
type BasicBillboardEmitterDescriptors =
    BasicParticle BillboardEmitterDescriptors