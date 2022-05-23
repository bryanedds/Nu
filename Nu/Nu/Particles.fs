// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Numerics
open Prime
module Particles =

    /// Describes the life of an instance value.
    /// OPTIMIZATION: LifeTimeOpt uses 0L to represent infinite life.
    /// OPTIMIZATION: doesn't use Liveness type to avoid its constructor calls.
    /// OPTIMIZATION: pre-computes progress scalar to minimize number of divides.
    type [<StructuralEquality; NoComparison; Struct>] Life =
        { StartTime : int64
          LifeTimeOpt : int64
          ProgressScalar : single }

        /// The progress made through the instance's life.
        static member getProgress time life =
            match life.LifeTimeOpt with
            | 0L -> 0.0f
            | _ -> single (time - life.StartTime) * life.ProgressScalar

        /// The progress made through the instance's life within a sub-range.
        static member getProgress3 time sublife life =
            match sublife.LifeTimeOpt with
            | 0L -> Life.getProgress time life
            | _ ->
                let localTime = time - life.StartTime
                Life.getProgress localTime sublife

        /// The liveness of the instance as a boolean.
        static member getLiveness time life =
            match life.LifeTimeOpt with
            | 0L -> true
            | lifeTime -> time - life.StartTime < lifeTime

        /// Make a life value.
        static member make startTime lifeTimeOpt =
            { StartTime = startTime
              LifeTimeOpt = lifeTimeOpt
              ProgressScalar = 1.0f / single lifeTimeOpt }

    /// A spatial constraint.
    type [<StructuralEquality; NoComparison>] Constraint =
        | Box of Box3
        | Sphere of single * Vector3
        | Constraints of Constraint SegmentedArray

        /// Combine two constraints.
        static member (+) (constrain, constrain2) =
            match (Constraint.isEmpty constrain, Constraint.isEmpty constrain2) with
            | (true, true) -> constrain // OPTIMIZATION: elide ctor
            | (_, true) -> constrain
            | (true, _) -> constrain2
            | (_, _) -> Constraints (SegmentedArray.ofList [constrain; constrain2])

        /// The empty constraint.
        static member empty = Constraints SegmentedArray.empty

        /// Checks that constraint is the empty constraint constant.
        static member isEmpty constrain =
             match constrain with
             | Constraints constraints -> constraints.Length = 0
             | _ -> false

        /// Checks that constraint is not the empty constraint constant.
        static member notEmpty constrain =
            not (Constraint.isEmpty constrain)

    /// How a logic is to be applied.
    type [<StructuralEquality; StructuralComparison>] LogicType =
        | Or of bool
        | Nor of bool
        | Xor of bool
        | And of bool
        | Nand of bool
        | Equal of bool

    /// Describes logic of behavior over a section of a target's life time.
    type [<StructuralEquality; NoComparison>] Logic =
        { LogicLife : Life
          LogicType : LogicType }

    /// The type of range.
    type [<StructuralEquality; NoComparison>] 'a RangeType =
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
    type [<StructuralEquality; NoComparison>] RangeApplicator =
        | Sum
        | Delta
        | Scale
        | Ratio
        | Set

    /// Describes range of behavior over a section of a target's life time.
    type [<StructuralEquality; NoComparison>] 'a Range =
        { RangeLife : Life
          RangeType : 'a RangeType
          RangeApplicator : RangeApplicator }

    /// The forces that may operate on a target.
    type [<StructuralEquality; NoComparison>] Force =
        | Gravity of Vector3
        | Attractor of Vector3 * single * single
        | Drag of single * single
        | Velocity of Constraint

    /// Describes the body of an instance value.
    type [<StructuralEquality; NoComparison; Struct>] Body =
        { mutable Position : Vector3
          mutable Scale : Vector3
          mutable Angles : Vector3
          mutable LinearVelocity : Vector3
          mutable AngularVelocity : Vector3
          mutable Restitution : single }

        /// The default 2d body.
        static member defaultBody2d =
            { Position = v3Zero
              Scale = v3One
              Angles = v3Zero
              LinearVelocity = v3Zero
              AngularVelocity = v3Zero
              Restitution = Constants.Particles.RestitutionDefault }

        /// The default 3d body.
        static member defaultBody3d =
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

    /// The output of a behavior.
    type [<NoEquality; NoComparison>] Output =
        | OutputEmitter of string * Emitter
        | OutputSound of single * Sound AssetTag
        | Outputs of Output SegmentedArray

        /// Combine two outputs.
        static member (+) (output, output2) =
            match (Output.isEmpty output, Output.isEmpty output2) with
            | (true, true) -> output // OPTIMIZATION: elide ctor
            | (_, true) -> output
            | (true, _) -> output2
            | (_, _) -> Outputs (SegmentedArray.ofList [output; output2])

        /// The empty output.
        static member empty = Outputs SegmentedArray.empty

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
        abstract GetLiveness : int64 -> Liveness

        /// Run the emitter.
        abstract Run : int64 -> Output * Emitter

        /// Convert the emitted particles to a ParticlesDescriptor.
        abstract ToParticlesDescriptor : int64 -> ParticlesDescriptor

        /// Change the maximum number of allowable particles.
        abstract Resize : int -> Emitter

    /// Transforms a constrained value.
    type 'a Transformer =
        int64 -> Constraint -> 'a SegmentedArray -> (Output * 'a SegmentedArray)

    [<RequireQualifiedAccess>]
    module Transformer =

        /// Accelerate bodies both linearly and angularly.
        let accelerate bodies =
            SegmentedArray.transform (fun (body : Body) ->
                { body with Position = body.Position + body.LinearVelocity; Angles = body.Angles + body.AngularVelocity })
                bodies

        /// Constrain bodies.
        let rec constrain c bodies =
            match c with
            | Sphere (radius, center) ->
                SegmentedArray.transform (fun (body : Body) ->
                    let positionNext = body.Position + body.LinearVelocity
                    let delta = positionNext - center
                    let distanceSquared = delta.MagnitudeSquared
                    let radiusSquared = radius * radius
                    if distanceSquared < radiusSquared then
                        let normal = Vector3.Normalize (center - positionNext)
                        let reflectedVelocity = Vector3.Reflect (body.LinearVelocity, normal)
                        let linearVelocity = reflectedVelocity * body.Restitution
                        { body with LinearVelocity = linearVelocity }
                    else body)
                    bodies
            | Box box ->
                // TODO: implement properly bouncing angles.
                SegmentedArray.transform (fun (body : Body) ->
                    let positionNext = body.Position + body.LinearVelocity
                    let delta = positionNext - box.Center
                    if Math.isPointInBounds3d positionNext box then
                        let speed = body.LinearVelocity.Magnitude
                        let distanceNormalized = Vector3.Normalize delta
                        let linearVelocity = speed * distanceNormalized * body.Restitution
                        { body with LinearVelocity = linearVelocity }
                    else body)
                    bodies
            | Constraints constraints ->
                SegmentedArray.fold (flip constrain) bodies constraints

        /// Make a force transformer.
        let force force : Body Transformer =
            fun _ c bodies ->
                match force with
                | Gravity gravity ->
                    let bodies = SegmentedArray.transform (fun (body : Body) -> { body with LinearVelocity = body.LinearVelocity + gravity }) bodies
                    (Output.empty, bodies)
                | Attractor (position, radius, force) ->
                    let bodies =
                        SegmentedArray.transform (fun (body : Body) ->
                            let direction = position - body.Position
                            let distance = direction.Magnitude
                            let normal = direction / distance
                            if distance < radius then
                                let pull = (radius - distance) / radius
                                let pullForce = pull * force
                                { body with LinearVelocity = body.LinearVelocity + pullForce * normal }
                            else body)
                            bodies
                    (Output.empty, bodies)
                | Drag (linearDrag, angularDrag) ->
                    let bodies =
                        SegmentedArray.transform (fun (body : Body) ->
                            let linearDrag = body.LinearVelocity * linearDrag
                            let angularDrag = body.AngularVelocity * angularDrag
                            { body with
                                LinearVelocity = body.LinearVelocity - linearDrag
                                AngularVelocity = body.AngularVelocity - angularDrag })
                            bodies
                    (Output.empty, bodies)
                | Velocity c2 ->
                    let c3 = c + c2
                    let bodies = constrain c3 bodies
                    let bodies = accelerate bodies
                    (Output.empty, bodies)

        /// Make a logic transformer.
        let logic logic : struct (Life * bool) Transformer =
            match logic.LogicType with
            | Or value ->
                fun _ _ targets ->
                    let targets = SegmentedArray.transform (fun struct (targetLife, targetValue) -> struct (targetLife, targetValue || value)) targets
                    (Output.empty, targets)
            | Nor value ->
                fun _ _ targets ->
                    let targets = SegmentedArray.transform (fun struct (targetLife, targetValue) -> struct (targetLife, not targetValue && not value)) targets
                    (Output.empty, targets)
            | Xor value ->
                fun _ _ targets ->
                    let targets = SegmentedArray.transform (fun struct (targetLife, targetValue) -> struct (targetLife, targetValue <> value)) targets
                    (Output.empty, targets)
            | And value ->
                fun _ _ targets ->
                    let targets = SegmentedArray.transform (fun struct (targetLife, targetValue) -> struct (targetLife, targetValue && value)) targets
                    (Output.empty, targets)
            | Nand value ->
                fun _ _ targets ->
                    let targets = SegmentedArray.transform (fun struct (targetLife, targetValue) -> struct (targetLife, not (targetValue && value))) targets
                    (Output.empty, targets)
            | Equal value ->
                fun _ _ targets ->
                    let targets = SegmentedArray.transform (fun struct (targetLife, _) -> struct (targetLife, value)) targets
                    (Output.empty, targets)

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
                fun _ _ targets ->
                    let targets =
                        SegmentedArray.transform (fun struct (targetLife, targetValue) ->
                            let result = applyRange targetValue value
                            struct (targetLife, result)) targets
                    (Output.empty, targets)
            | Linear (value, value2) ->
                fun _ _ targets ->
                    let targets =
                        SegmentedArray.transform (fun struct (targetLife, targetValue) ->
                            let progress = Life.getProgress3 time range.RangeLife targetLife
                            let result = applyRange targetValue (value + scale (value2 - value, progress))
                            struct (targetLife, result))
                            targets
                    (Output.empty, targets)
            | Random (value, value2) ->
                fun _ _ targets ->
                    let targets =
                        SegmentedArray.transform (fun struct (targetLife, targetValue) ->
                            let progress = Life.getProgress3 time range.RangeLife targetLife
                            let rand = Rand.makeFromInt (int ((Math.Max (double progress, 0.000000001)) * double Int32.MaxValue))
                            let randValue = fst (Rand.nextSingle rand)
                            let result = applyRange targetValue (value + scale (value2 - value, randValue))
                            struct (targetLife, result))
                            targets
                    (Output.empty, targets)
            | Chaos (value, value2) ->
                fun _ _ targets ->
                    let targets =
                        SegmentedArray.transform (fun struct (targetLife, targetValue) ->
                            let chaosValue = Gen.randomf
                            let result = applyRange targetValue (value + scale (value2 - value, chaosValue))
                            struct (targetLife, result))
                            targets
                    (Output.empty, targets)
            | Ease (value, value2) ->
                fun _ _ targets ->
                    let targets =
                        SegmentedArray.transform (fun struct (targetLife, targetValue) ->
                            let progress = Life.getProgress3 time range.RangeLife targetLife
                            let progressEase = single (Math.Pow (Math.Sin (Math.PI * double progress * 0.5), 2.0))
                            let result = applyRange targetValue (value + scale (value2 - value, progressEase))
                            struct (targetLife, result))
                            targets
                    (Output.empty, targets)
            | EaseIn (value, value2) ->
                fun _ _ targets ->
                    let targets =
                        SegmentedArray.transform (fun struct (targetLife, targetValue) ->
                            let progress = Life.getProgress3 time range.RangeLife targetLife
                            let progressScaled = float progress * Math.PI * 0.5
                            let progressEaseIn = 1.0 + Math.Sin (progressScaled + Math.PI * 1.5)
                            let result = applyRange targetValue (value + scale (value2 - value, single progressEaseIn))
                            struct (targetLife, result))
                            targets
                    (Output.empty, targets)
            | EaseOut (value, value2) ->
                fun _ _ targets ->
                    let targets =
                        SegmentedArray.transform (fun struct (targetLife, targetValue) ->
                            let progress = Life.getProgress3 time range.RangeLife targetLife
                            let progressScaled = float progress * Math.PI * 0.5
                            let progressEaseOut = Math.Sin progressScaled
                            let result = applyRange targetValue (value + scale (value2 - value, single progressEaseOut))
                            struct (targetLife, result))
                            targets
                    (Output.empty, targets)
            | Sin (value, value2) ->
                fun _ _ targets ->
                    let targets =
                        SegmentedArray.transform (fun struct (targetLife, targetValue) ->
                            let progress = Life.getProgress3 time range.RangeLife targetLife
                            let progressScaled = float progress * Math.PI * 2.0
                            let progressSin = Math.Sin progressScaled
                            let result = applyRange targetValue (value + scale (value2 - value, single progressSin))
                            struct (targetLife, result))
                            targets
                    (Output.empty, targets)
            | SinScaled (scalar, value, value2) ->
                fun _ _ targets ->
                    let targets =
                        SegmentedArray.transform (fun struct (targetLife, targetValue) ->
                            let progress = Life.getProgress3 time range.RangeLife targetLife
                            let progressScaled = float progress * Math.PI * 2.0 * float scalar
                            let progressSin = Math.Sin progressScaled
                            let result = applyRange targetValue (value + scale (value2 - value, single progressSin))
                            struct (targetLife, result))
                            targets
                    (Output.empty, targets)
            | Cos (value, value2) ->
                fun _ _ targets ->
                    let targets =
                        SegmentedArray.transform (fun struct (targetLife, targetValue) ->
                            let progress = Life.getProgress3 time range.RangeLife targetLife
                            let progressScaled = float progress * Math.PI * 2.0
                            let progressCos = Math.Cos progressScaled
                            let result = applyRange targetValue (value + scale (value2 - value, single progressCos))
                            struct (targetLife, result))
                            targets
                    (Output.empty, targets)
            | CosScaled (scalar, value, value2) ->
                fun _ _ targets ->
                    let targets =
                        SegmentedArray.transform (fun struct (targetLife, targetValue) ->
                            let progress = Life.getProgress3 time range.RangeLife targetLife
                            let progressScaled = float progress * Math.PI * 2.0 * float scalar
                            let progressCos = Math.Cos progressScaled
                            let result = applyRange targetValue (value + scale (value2 - value, single progressCos))
                            struct (targetLife, result))
                            targets
                    (Output.empty, targets)

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
    type [<NoEquality; NoComparison>] Scope<'a, 'b when 'a : struct> =
        { In : 'a SegmentedArray -> 'b SegmentedArray
          Out : (Output * 'b SegmentedArray) -> 'a SegmentedArray -> (Output * 'a SegmentedArray) }

    [<RequireQualifiedAccess>]
    module Scope =

        /// Make a scope.
        let inline make<'a, 'b when 'a : struct> (getField : 'a -> 'b) (setField : 'b -> 'a -> 'a) : Scope<'a, 'b> =
            { In = SegmentedArray.map getField
              Out = fun (output, fields) (targets : 'a SegmentedArray) -> (output, SegmentedArray.map2 setField fields targets) }

    /// The base behavior type.
    type Behavior =

        /// Run the behavior over a single target.
        abstract Run : int64 -> Constraint -> obj -> (Output * obj)

        /// Run the behavior over multiple targets.
        abstract RunMany : int64 -> Constraint -> obj -> (Output * obj)

    /// Defines a generic behavior.
    type [<NoEquality; NoComparison>] Behavior<'a, 'b when 'a : struct> =
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
        static member runMany time (constrain : Constraint) (behavior : Behavior<'a, 'b>) (targets : 'a SegmentedArray) =
            let targets2 = behavior.Scope.In targets
            let (output, targets3) =
                FStack.fold (fun (output, targets) transformer ->
                    let (output2, targets) = transformer time constrain targets
                    (output + output2, targets))
                    (Output.empty, targets2)
                    behavior.Transformers
            let (output, targets4) = behavior.Scope.Out (output, targets3) targets
            (output, targets4)

        /// Run the behavior over a single target.
        static member run time (constrain : Constraint) (behavior : Behavior<'a, 'b>) (target : 'a) =
            let (output, targets) = Behavior<'a, 'b>.runMany time constrain behavior (SegmentedArray.singleton target)
            let target = SegmentedArray.item 0 targets
            (output, target)

        interface Behavior with
            member this.Run time constrain targetObj =
                let (outputs, target) = Behavior<'a, 'b>.run time constrain this (targetObj :?> 'a)
                (outputs, target :> obj)
            member this.RunMany time constrain targetsObj =
                let (outputs, targets) = Behavior<'a, 'b>.runMany time constrain this (targetsObj :?> 'a SegmentedArray)
                (outputs, targets :> obj)

    /// A composition of behaviors.
    type [<NoEquality; NoComparison>] Behaviors =
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
        static member run time behaviors constrain (target : 'a) =
            let (outputs, targets) =
                FStack.fold (fun (output, target) (behavior : Behavior) ->
                    let (output2, targets2) = behavior.Run time constrain target
                    (output + output2, targets2))
                    (Output.empty, target :> obj)
                    behaviors.Behaviors
            (outputs, targets :?> 'a)

        /// Run the behaviors over targets.
        static member runMany time behaviors constrain (targets : 'a SegmentedArray) =
            let (outputs, targets) =
                FStack.fold (fun (output, targets) (behavior : Behavior) ->
                    let (output2, targets2) = behavior.RunMany time constrain targets
                    (output + output2, targets2))
                    (Output.empty, targets :> obj)
                    behaviors.Behaviors
            (outputs, targets :?> 'a SegmentedArray)

    /// Describes an emitter.
    and [<NoEquality; NoComparison>] EmitterDescriptor<'a when 'a :> Particle and 'a : struct> =
        { Body : Body
          Blend : Blend
          Image : Image AssetTag
          LifeTimeOpt : int64
          ParticleLifeTimeMaxOpt : int64
          ParticleRate : single
          ParticleMax : int
          ParticleSeed : 'a
          Constraint : Constraint
          Style : string }

    /// Describes a map of basic emitters.
    and EmitterDescriptors<'a when 'a :> Particle and 'a : struct> =
        Map<string, 'a EmitterDescriptor>

    /// The default particle emitter.
    /// NOTE: ideally, this would be an abstract data type, but I feel that would discourage users from making their
    /// own emitters - it would looks like making an emitter would require a lot of additional boilerplate as well as
    /// making it harder to use this existing emitter as an example.
    and [<NoEquality; NoComparison>] Emitter<'a when 'a :> Particle and 'a : equality and 'a : struct> =
        { mutable Body : Body // mutable for animation
          Elevation : single
          Absolute : bool
          Blend : Blend
          Image : Image AssetTag
          Life : Life
          ParticleLifeTimeMaxOpt : int64 // OPTIMIZATION: uses 0L to represent infinite particle life.
          ParticleRate : single
          mutable ParticleIndex : int // the current particle buffer insertion point
          mutable ParticleWatermark : int // tracks the highest active particle index; never decreases.
          ParticleRing : 'a SegmentedArray // operates as a ring-buffer
          ParticleSeed : 'a
          Constraint : Constraint
          ParticleInitializer : int64 -> 'a Emitter -> 'a
          ParticleBehavior : int64 -> 'a Emitter -> Output
          ParticleBehaviors : Behaviors
          EmitterBehavior : int64 -> 'a Emitter -> Output
          EmitterBehaviors : Behaviors
          ToParticlesDescriptor : int64 -> 'a Emitter -> ParticlesDescriptor }

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
            match emitter.ParticleLifeTimeMaxOpt with
            | 0L -> Live
            | lifeTime -> if Life.getLiveness (time - lifeTime) emitter.Life then Live else Dead

        /// Run the emitter.
        static member run time (emitter : 'a Emitter) =

            // determine local time
            let localTime = time - emitter.Life.StartTime

            // emit new particles if live
            if Life.getLiveness time emitter.Life then
                let emitCountLastFrame = single (dec localTime) * emitter.ParticleRate
                let emitCountThisFrame = single localTime * emitter.ParticleRate
                let emitCount = int emitCountThisFrame - int emitCountLastFrame
                for _ in 0 .. emitCount - 1 do Emitter<'a>.emit time emitter

            // update emitter in-place
            let output = emitter.EmitterBehavior time emitter

            // update emitter compositionally
            let (output2, emitter) = Behaviors.run time emitter.EmitterBehaviors emitter.Constraint emitter

            // update existing particles in-place
            let output3 = emitter.ParticleBehavior time emitter

            // update existing particles compositionally
            let (output4, particleBuffer) = Behaviors.runMany time emitter.ParticleBehaviors emitter.Constraint emitter.ParticleRing
            let emitter = { emitter with ParticleRing = particleBuffer }

            // fin
            (output + output2 + output3 + output4, emitter)

        /// Make a basic particle emitter.
        static member make<'a>
            time body elevation absolute blend image lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax particleSeed
            constrain particleInitializer particleBehavior particleBehaviors emitterBehavior emitterBehaviors toParticlesDescriptor : 'a Emitter =
            { Body = body
              Elevation = elevation
              Absolute = absolute
              Blend = blend
              Image = image
              Life = Life.make time lifeTimeOpt
              ParticleLifeTimeMaxOpt = particleLifeTimeMaxOpt
              ParticleRate = particleRate
              ParticleIndex = 0
              ParticleWatermark = 0
              ParticleRing = SegmentedArray.zeroCreate particleMax
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
                Emitter<'a>.getLiveness time this
            member this.Run time =
                let (output, emitter) = Emitter<'a>.run time this
                (output, emitter :> Emitter)
            member this.ToParticlesDescriptor time =
                this.ToParticlesDescriptor time this
            member this.Resize particleMax =
                if  this.ParticleRing.Length <> particleMax then
                    this.ParticleIndex <- 0
                    this.ParticleWatermark <- 0
                    { this with ParticleRing = SegmentedArray.zeroCreate<'a> particleMax } :> Emitter
                else this :> Emitter
            end

    /// A basic particle.
    type [<StructuralEquality; NoComparison; Struct>] BasicParticle =
        { mutable Life : Life
          mutable Body : Body
          mutable Offset : Vector3
          mutable Size : Vector3
          mutable Inset : Box2
          mutable Color : Color
          mutable Glow : Color
          mutable Flip : Flip }
        interface Particle with member this.Life with get () = this.Life and set value = this.Life <- value

    [<RequireQualifiedAccess>]
    module BasicParticle =
        let body = Scope.make (fun p -> p.Body) (fun v p -> { p with Body = v })
        let position = Scope.make (fun p -> struct (p.Life, p.Body.Position)) (fun struct (_, v) p -> { p with Body = { p.Body with Position = v }})
        let scale = Scope.make (fun p -> struct (p.Life, p.Body.Scale)) (fun struct (_, v) p -> { p with Body = { p.Body with Scale = v }})
        let angles = Scope.make (fun p -> struct (p.Life, p.Body.Angles)) (fun struct (_, v) p -> { p with Body = { p.Body with Angles = v }})
        let offset = Scope.make (fun p -> struct (p.Life, p.Offset)) (fun struct (_, v) p -> { p with Offset = v })
        let size = Scope.make (fun p -> struct (p.Life, p.Size)) (fun struct (_, v) p -> { p with Size = v })
        let inset = Scope.make (fun p -> struct (p.Life, p.Inset)) (fun struct (_, v) p -> { p with Inset = v })
        let color = Scope.make (fun p -> struct (p.Life, p.Color)) (fun struct (_, v) p -> { p with Color = v })
        let glow = Scope.make (fun p -> struct (p.Life, p.Glow)) (fun struct (_, v) p -> { p with Glow = v })
        let flipH =
            Scope.make
                (fun p ->
                    let flipH = match p.Flip with FlipNone -> false | FlipH -> true | FlipV -> false | FlipHV -> true
                    struct (p.Life, flipH))
                (fun struct (_, v) p ->
                    let flip =
                        match (p.Flip, v) with
                        | (FlipNone, true) -> FlipH
                        | (FlipH, true) -> FlipH
                        | (FlipV, true) -> FlipHV
                        | (FlipHV, true) -> FlipHV
                        | (FlipNone, false) -> FlipNone
                        | (FlipH, false) -> FlipNone
                        | (FlipV, false) -> FlipV
                        | (FlipHV, false) -> FlipV
                    { p with Flip = flip })
        let flipV =
            Scope.make
                (fun p ->
                    let flipV = match p.Flip with FlipNone -> false | FlipH -> false | FlipV -> true | FlipHV -> true
                    struct (p.Life, flipV))
                (fun struct (_, v) p ->
                    let flip =
                        match (p.Flip, v) with
                        | (FlipNone, true) -> FlipV
                        | (FlipH, true) -> FlipHV
                        | (FlipV, true) -> FlipV
                        | (FlipHV, true) -> FlipHV
                        | (FlipNone, false) -> FlipNone
                        | (FlipH, false) -> FlipH
                        | (FlipV, false) -> FlipNone
                        | (FlipHV, false) -> FlipH
                    { p with Flip = flip })

    /// Describes a basic emitter.
    type BasicEmitterDescriptor =
        BasicParticle EmitterDescriptor

    /// Describes a map of basic emitters.
    type BasicEmitterDescriptors =
        BasicParticle EmitterDescriptors

    /// A basic particle emitter.
    type BasicEmitter =
        Emitter<BasicParticle>

    [<RequireQualifiedAccess>]
    module BasicEmitter2d =

        let private toParticlesDescriptor time (emitter : BasicEmitter) =
            let particles =
                SegmentedArray.append
                    (if emitter.ParticleWatermark > emitter.ParticleIndex
                     then SegmentedArray.skip emitter.ParticleIndex emitter.ParticleRing
                     else SegmentedArray.empty)
                    (SegmentedArray.take emitter.ParticleIndex emitter.ParticleRing)
            let particles' =
                SegmentedArray.zeroCreate<Nu.Particle> particles.Length
            for index in 0 .. particles.Length - 1 do
                let particle = &particles.[index]
                if Life.getLiveness time particle.Life then
                    let particle' = &particles'.[index]
                    particle'.Transform.Position <- particle.Body.Position
                    particle'.Transform.Scale <- particle.Body.Scale
                    particle'.Transform.Angles <- particle.Body.Angles
                    particle'.Transform.Offset <- particle.Offset
                    particle'.Transform.Size <- particle.Size
                    particle'.Color <- particle.Color
                    particle'.Glow <- particle.Glow
                    particle'.InsetOpt <- ValueSome particle.Inset // TODO: 3D: see if we can utilize opt-ness of inset in particle operators.
                    particle'.Flip <- particle.Flip
            { Elevation = emitter.Elevation
              Horizon = emitter.Body.Position.Y
              Absolute = emitter.Absolute
              Blend = emitter.Blend
              Image = emitter.Image
              Particles = particles' }

        /// Resize the emitter.
        let resize particleMax (emitter : BasicEmitter) =
            (emitter :> Emitter).Resize particleMax :?> BasicEmitter

        /// Make a basic 2d particle emitter.
        let make
            time body elevation absolute blend image lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax particleSeed
            constrain particleInitializer particleBehavior particleBehaviors emitterBehavior emitterBehaviors =
            BasicEmitter.make
                time body elevation absolute blend image lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax particleSeed
                constrain particleInitializer particleBehavior particleBehaviors emitterBehavior emitterBehaviors toParticlesDescriptor

        /// Make an empty basic 2d particle emitter.
        let makeEmpty time lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax =
            let image = asset Assets.Default.PackageName Assets.Default.ImageName
            let particleSeed = Unchecked.defaultof<BasicParticle>
            let particleInitializer = fun _ (emitter : BasicEmitter) -> emitter.ParticleSeed
            let particleBehavior = fun _ _ -> Output.empty
            let particleBehaviors = Behaviors.empty
            let emitterBehavior = fun _ _ -> Output.empty
            let emitterBehaviors = Behaviors.empty
            make
                time Body.defaultBody2d 0.0f false Transparent image lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax particleSeed
                Constraint.empty particleInitializer particleBehavior particleBehaviors emitterBehavior emitterBehaviors

        /// Make the default basic 2d particle emitter.
        let makeDefault time lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax =
            let image = asset Assets.Default.PackageName Assets.Default.ImageName
            let particleSeed =
                { Life = Life.make 0L 120L
                  Body = Body.defaultBody2d
                  Offset = v3Zero
                  Size = Constants.Engine.ParticleSize2dDefault
                  Inset = box2Zero
                  Color = Color.One
                  Glow = Color.Zero
                  Flip = FlipNone }
            let particleInitializer = fun _ (emitter : BasicEmitter) ->
                let particle = emitter.ParticleSeed
                particle.Body.Position <- emitter.Body.Position
                particle.Body.Angles <- emitter.Body.Angles
                particle.Body.LinearVelocity <- v3 Gen.randomf Gen.randomf Gen.randomf * 10.0f
                particle.Body.AngularVelocity <- v3 Gen.randomf Gen.randomf Gen.randomf
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
            let particleBehaviors =
                Behaviors.singleton
                    (Behavior.ofSeq BasicParticle.body
                        [Transformer.force (Gravity (Constants.Engine.GravityDefault / single Constants.Engine.DesiredFps))
                         Transformer.force (Velocity Constraint.empty)])
            let emitterBehavior = fun _ (emitter : BasicEmitter) ->
                emitter.Body.Angles <- emitter.Body.Angles + v3 0.03f 0.2f 0.1f
                Output.empty
            let emitterBehaviors =
                Behaviors.empty
            make
                time Body.defaultBody2d 0.0f false Transparent image lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax particleSeed
                Constraint.empty particleInitializer particleBehavior particleBehaviors emitterBehavior emitterBehaviors

    /// A particle system.
    /// TODO: consider making this an abstract data type?
    type [<NoEquality; NoComparison>] ParticleSystem =
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
        static member run time particleSystem =
            let (output, emitters) =
                Map.fold (fun (output, emitters) emitterId (emitter : Emitter) ->
                    let (output2, emitter) = emitter.Run time
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

/// A particle system.
type ParticleSystem = Particles.ParticleSystem