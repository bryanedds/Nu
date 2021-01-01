// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Numerics
open Prime
module Particles =

    /// A spatial constraint.
    type [<StructuralEquality; NoComparison>] Constraint =
        | Rectangle of Vector4
        | Circle of single * Vector2
        | Constraints of Constraint array

        /// Combine two constraints.
        static member (+) (constrain, constrain2) =
            match (constrain, constrain2) with
            | (Constraints [||], Constraints [||]) -> constrain // OPTIMIZATION: elide Constraint ctor
            | (_, Constraints [||]) -> constrain
            | (Constraints [||], _) -> constrain2
            | (_, _) -> Constraints [|constrain; constrain2|]

        /// The empty constraint.
        static member empty = Constraints [||]

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
        { LogicType : LogicType
          (*LogicBegin : single*)
          (*LogicEnd : single*) }

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
    /// TODO: consider implementing range slicing with RangeBegin and RangeEnd.
    type [<StructuralEquality; NoComparison>] 'a Range =
        { RangeType : 'a RangeType
          RangeApplicator : RangeApplicator
          (*RangeBegin : single*)
          (*RangeEnd : single*) }

    /// The forces that may operate on a target.
    type [<StructuralEquality; NoComparison>] Force =
        | Gravity of Vector2
        | Attractor of Vector2 * single * single
        | Velocity

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

        /// The liveness of the instance as a boolean.
        static member getLiveness time life =
            match life.LifeTimeOpt with
            | 0L -> true
            | lifeTime -> lifeTime < time - life.StartTime

        static member make startTime lifeTimeOpt =
            { StartTime = startTime
              LifeTimeOpt = lifeTimeOpt
              ProgressScalar = 1.0f / single lifeTimeOpt }

    /// Describes the body of an instance value.
    type [<StructuralEquality; NoComparison; Struct>] Body =
        { mutable Position : Vector2
          mutable LinearVelocity : Vector2
          mutable Rotation : single
          mutable AngularVelocity : single }

        /// The default body.
        static member defaultBody =
            { Position = v2Zero
              LinearVelocity = v2Zero
              Rotation = 0.0f
              AngularVelocity = 0.0f }

    /// The base particle type.
    type Particle =

        /// The life of the particle.
        abstract Life : Life with get, set

    /// The output of a behavior.
    type [<NoEquality; NoComparison>] Output =
        | OutputEmitter of string * Emitter
        | OutputSound of single * Sound AssetTag
        | Outputs of Output array

        /// Combine two outputs.
        static member (+) (output, output2) =
            match (output, output2) with
            | (Outputs [||], Outputs [||]) -> output // OPTIMIZATION: elide Output ctor
            | (_, Outputs [||]) -> output
            | (Outputs [||], _) -> output2
            | (_, _) -> Outputs [|output; output2|]

        /// The empty output.
        static member empty = Outputs [||]

    /// The base particle emitter type.
    and Emitter =

        /// Determine liveness of emitter.
        abstract GetLiveness : int64 -> Liveness

        /// Run the emitter.
        abstract Run : int64 -> Constraint -> Emitter * Output

        /// Convert the emitted particles to a ParticlesDescriptor.
        abstract ToParticlesDescriptor : unit -> ParticlesDescriptor

        /// Change the maximum number of allowable particles.
        abstract Resize : int -> Emitter

    /// Transforms a constrained value.
    type 'a Transformer =
        int64 -> Constraint -> 'a array -> ('a array * Output)

    [<AutoOpen>]
    module TransformerOperators =

        /// Pipe two transformers.
        let pipe (transformer : 'a Transformer) (transformer2 : 'a Transformer) : 'a Transformer =
            fun time constrain targets ->
                let (targets, output) = transformer time constrain targets
                let (targets, output2) = transformer2 time constrain targets
                (targets, output + output2)

        /// Pipe multiple transformers.
        let pipeMany transformers =
            Seq.reduce pipe transformers

    [<RequireQualifiedAccess>]
    module Transformer =

        /// Make a force transformer.
        let force force : Body Transformer =
            fun _ constrain bodies ->
                match force with
                | Gravity gravity ->
                    let bodies = Array.map (fun (body : Body) -> { body with LinearVelocity = body.LinearVelocity + gravity }) bodies
                    (bodies, Output.empty)
                | Attractor (position, radius, force) ->
                    let bodies =
                        Array.map (fun (body : Body) ->
                            let direction = position - body.Position
                            let distance = direction.Length ()
                            let normal = direction / distance
                            if distance < radius then
                                let pull = (radius - distance) / radius
                                let pullForce = pull * force
                                { body with LinearVelocity = body.LinearVelocity + pullForce * normal }
                            else body)
                            bodies
                    (bodies, Output.empty)
                | Velocity ->
                    let bodies =
                        Array.map (fun (body : Body) ->
                            { body with
                                Position = body.Position + body.LinearVelocity // TODO: apply constraints
                                Rotation = body.Rotation + body.AngularVelocity })
                            bodies
                    (bodies, Output.empty)

        /// Make a logic transformer.
        let logic (logic : Logic) : struct (Life * bool) Transformer =
            match logic.LogicType with
            | Or value ->
                fun _ _ targets ->
                    let targets = Array.map (fun struct (targetLife, targetValue) -> struct (targetLife, targetValue || value)) targets
                    (targets, Output.empty)
            | Nor value ->
                fun _ _ targets ->
                    let targets = Array.map (fun struct (targetLife, targetValue) -> struct (targetLife, not targetValue && not value)) targets
                    (targets, Output.empty)
            | Xor value ->
                fun _ _ targets ->
                    let targets = Array.map (fun struct (targetLife, targetValue) -> struct (targetLife, targetValue <> value)) targets
                    (targets, Output.empty)
            | And value ->
                fun _ _ targets ->
                    let targets = Array.map (fun struct (targetLife, targetValue) -> struct (targetLife, targetValue && value)) targets
                    (targets, Output.empty)
            | Nand value ->
                fun _ _ targets ->
                    let targets = Array.map (fun struct (targetLife, targetValue) -> struct (targetLife, not (targetValue && value))) targets
                    (targets, Output.empty)
            | Equal value ->
                fun _ _ targets ->
                    let targets = Array.map (fun struct (targetLife, _) -> struct (targetLife, value)) targets
                    (targets, Output.empty)

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
                        Array.map (fun struct (targetLife, targetValue) ->
                            let result = applyRange targetValue value
                            struct (targetLife, result)) targets
                    (targets, Output.empty)
            | Linear (value, value2) ->
                fun _ _ targets ->
                    let targets =
                        Array.map (fun struct (targetLife, targetValue) ->
                            let progress = Life.getProgress time targetLife
                            let result = applyRange targetValue (value + scale (value2 - value, progress))
                            struct (targetLife, result))
                            targets
                    (targets, Output.empty)
            | Random (value, value2) ->
                fun _ _ targets ->
                    let targets =
                        Array.map (fun struct (targetLife, targetValue) ->
                            let progress = Life.getProgress time targetLife
                            let rand = Rand.makeFromInt (int ((Math.Max (double progress, 0.000000001)) * double Int32.MaxValue))
                            let randValue = fst (Rand.nextSingle rand)
                            let result = applyRange targetValue (value + scale (value2 - value, randValue))
                            struct (targetLife, result))
                            targets
                    (targets, Output.empty)
            | Chaos (value, value2) ->
                fun _ _ targets ->
                    let targets =
                        Array.map (fun struct (targetLife, targetValue) ->
                            let chaosValue = Gen.randomf
                            let result = applyRange targetValue (value + scale (value2 - value, chaosValue))
                            struct (targetLife, result))
                            targets
                    (targets, Output.empty)
            | Ease (value, value2) ->
                fun _ _ targets ->
                    let targets =
                        Array.map (fun struct (targetLife, targetValue) ->
                            let progress = Life.getProgress time targetLife
                            let progressEase = single (Math.Pow (Math.Sin (Math.PI * double progress * 0.5), 2.0))
                            let result = applyRange targetValue (value + scale (value2 - value, progressEase))
                            struct (targetLife, result))
                            targets
                    (targets, Output.empty)
            | EaseIn (value, value2) ->
                fun _ _ targets ->
                    let targets =
                        Array.map (fun struct (targetLife, targetValue) ->
                            let progress = Life.getProgress time targetLife
                            let progressScaled = float progress * Math.PI * 0.5
                            let progressEaseIn = 1.0 + Math.Sin (progressScaled + Math.PI * 1.5)
                            let result = applyRange targetValue (value + scale (value2 - value, single progressEaseIn))
                            struct (targetLife, result))
                            targets
                    (targets, Output.empty)
            | EaseOut (value, value2) ->
                fun _ _ targets ->
                    let targets =
                        Array.map (fun struct (targetLife, targetValue) ->
                            let progress = Life.getProgress time targetLife
                            let progressScaled = float progress * Math.PI * 0.5
                            let progressEaseOut = Math.Sin progressScaled
                            let result = applyRange targetValue (value + scale (value2 - value, single progressEaseOut))
                            struct (targetLife, result))
                            targets
                    (targets, Output.empty)
            | Sin (value, value2) ->
                fun _ _ targets ->
                    let targets =
                        Array.map (fun struct (targetLife, targetValue) ->
                            let progress = Life.getProgress time targetLife
                            let progressScaled = float progress * Math.PI * 2.0
                            let progressSin = Math.Sin progressScaled
                            let result = applyRange targetValue (value + scale (value2 - value, single progressSin))
                            struct (targetLife, result))
                            targets
                    (targets, Output.empty)
            | SinScaled (scalar, value, value2) ->
                fun _ _ targets ->
                    let targets =
                        Array.map (fun struct (targetLife, targetValue) ->
                            let progress = Life.getProgress time targetLife
                            let progressScaled = float progress * Math.PI * 2.0 * float scalar
                            let progressSin = Math.Sin progressScaled
                            let result = applyRange targetValue (value + scale (value2 - value, single progressSin))
                            struct (targetLife, result))
                            targets
                    (targets, Output.empty)
            | Cos (value, value2) ->
                fun _ _ targets ->
                    let targets =
                        Array.map (fun struct (targetLife, targetValue) ->
                            let progress = Life.getProgress time targetLife
                            let progressScaled = float progress * Math.PI * 2.0
                            let progressCos = Math.Cos progressScaled
                            let result = applyRange targetValue (value + scale (value2 - value, single progressCos))
                            struct (targetLife, result))
                            targets
                    (targets, Output.empty)
            | CosScaled (scalar, value, value2) ->
                fun _ _ targets ->
                    let targets =
                        Array.map (fun struct (targetLife, targetValue) ->
                            let progress = Life.getProgress time targetLife
                            let progressScaled = float progress * Math.PI * 2.0 * float scalar
                            let progressCos = Math.Cos progressScaled
                            let result = applyRange targetValue (value + scale (value2 - value, single progressCos))
                            struct (targetLife, result))
                            targets
                    (targets, Output.empty)

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
        { In : 'a array -> 'b array
          Out : ('b array * Output) -> 'a array -> ('a array * Output) }

    [<RequireQualifiedAccess>]
    module Scope =

        /// Make a scope.
        let inline make<'a, 'b when 'a : struct> (getField : 'a -> 'b) (setField : 'b -> 'a -> 'a) : Scope<'a, 'b> =
            { In = Array.map getField
              Out = fun (fields, output) (targets : 'a array) -> (Array.map2 setField fields targets, output) }

    /// The base behavior type.
    type Behavior =

        /// Run the behavior over a single target.
        abstract Run : int64 -> Constraint -> obj -> (obj * Output)

        /// Run the behavior over multiple targets.
        abstract RunMany : int64 -> Constraint -> obj -> (obj * Output)

    /// Defines a generic behavior.
    type [<NoEquality; NoComparison>] Behavior<'a, 'b when 'a : struct> =
        { Scope : Scope<'a, 'b>
          Transformers : 'b Transformer FStack }

        /// Run the behavior over a single target.
        static member run time (constrain : Constraint) (behavior : Behavior<'a, 'b>) (target : 'a) =
            let (targets, output) = Behavior<'a, 'b>.runMany time constrain behavior [|target|]
            let target = Array.item 0 targets
            (target, output)

        /// Run the behavior over an array of targets.
        /// OPTIMIZATION: runs transformers in batches for better utilization of instruction cache.
        static member runMany time (constrain : Constraint) (behavior : Behavior<'a, 'b>) (targets : 'a array) =
            let targets2 = behavior.Scope.In targets
            let (targets3, output) =
                FStack.fold (fun (targets, output) transformer ->
                    let (targets, output2) = transformer time constrain targets
                    (targets, output + output2))
                    (targets2, Output.empty)
                    behavior.Transformers
            let (targets4, output) = behavior.Scope.Out (targets3, output) targets
            (targets4, output)

        interface Behavior with
            member this.Run time constrain targetObj =
                let (target, outputs) = Behavior<'a, 'b>.run time constrain this (targetObj :?> 'a)
                (target :> obj, outputs)
            member this.RunMany time constrain targetsObj =
                let (targets, outputs) = Behavior<'a, 'b>.runMany time constrain this (targetsObj :?> 'a array)
                (targets :> obj, outputs)

    /// A composition of behaviors.
    type [<NoEquality; NoComparison>] Behaviors =
        { Behaviors : Behavior FStack }

        /// The empty behaviors.
        static member empty =
            { Behaviors = FStack.empty }

        /// Add a behavior.
        static member add behavior behaviors =
            { Behaviors = FStack.conj behavior behaviors.Behaviors }

        /// Add multiple behaviors.
        static member addMany behaviorsMany behaviors =
            { Behaviors = Seq.fold (fun behaviors behavior -> FStack.conj behavior behaviors) behaviors.Behaviors behaviorsMany }

        /// Make a behavior from a sequence of behaviors.
        static member ofSeq seq =
            Behaviors.addMany seq Behaviors.empty

        /// Run the behaviors over a single target.
        static member run time behaviors constrain (target : 'a) =
            let (targets, outputs) =
                FStack.fold (fun (target, output) (behavior : Behavior) ->
                    let (targets2, output2) = behavior.Run time constrain target
                    (targets2, output + output2))
                    (target :> obj, Output.empty)
                    behaviors.Behaviors
            (targets :?> 'a, outputs)

        /// Run the behaviors over an array of targets.
        static member runMany time behaviors constrain (targets : 'a array) =
            let (targets, outputs) =
                FStack.fold (fun (targets, output) (behavior : Behavior) ->
                    let (targets2, output2) = behavior.RunMany time constrain targets
                    (targets2, output + output2))
                    (targets :> obj, Output.empty)
                    behaviors.Behaviors
            (targets :?> 'a array, outputs)

    /// Describes an emitter.
    and [<NoEquality; NoComparison>] EmitterDescriptor<'a when 'a :> Particle and 'a : struct> =
        { Body : Body
          Blend : Blend
          Image : Image AssetTag
          LifeTimeOpt : int64
          ParticleLifeTimeOpt : int64
          ParticleRate : single
          ParticleMax : int
          ParticleSeed : 'a
          Constraint : Constraint
          EmitterName : string }

    /// Describes a map of basic emitters.
    and EmitterDescriptors<'a when 'a :> Particle and 'a : struct> =
        Map<string, 'a EmitterDescriptor>

    /// The default particle emitter.
    /// NOTE: ideally, this would be an abstract data type, but I feel that would discourage users from making their
    /// own emitters - it would looks like making an emitter would require a lot of additional boilerplate as well as
    /// making it harder to use this existing emitter as an example.
    and [<NoEquality; NoComparison>] Emitter<'a when 'a :> Particle and 'a : equality and 'a : struct> =
        { Body : Body
          Elevation : single
          Absolute : bool
          Blend : Blend
          Image : Image AssetTag
          Life : Life
          ParticleLifeTimeOpt : int64 // OPTIMIZATION: uses 0L to represent infinite particle life.
          ParticleRate : single
          mutable ParticleIndex : int // the current particle buffer insertion point
          mutable ParticleWatermark : int // tracks the highest active particle index; never decreases.
          ParticleBuffer : 'a array // operates as a ring-buffer
          ParticleSeed : 'a
          Constraint : Constraint
          ParticleInitializer : int64 -> Constraint -> 'a Emitter -> 'a
          ParticleBehavior : int64 -> Constraint -> 'a Emitter -> Output
          ParticleBehaviors : Behaviors
          EmitterBehavior : int64 -> Constraint -> 'a Emitter -> Output
          EmitterBehaviors : Behaviors
          ToParticlesDescriptor : 'a Emitter -> ParticlesDescriptor }

        static member private emit time constrain emitter =
            let particleIndex = if emitter.ParticleIndex >= emitter.ParticleBuffer.Length then 0 else inc emitter.ParticleIndex
            if particleIndex > emitter.ParticleWatermark then emitter.ParticleWatermark <- particleIndex
            emitter.ParticleIndex <- particleIndex
            let particle = &emitter.ParticleBuffer.[particleIndex]
            particle.Life <- Life.make time particle.Life.LifeTimeOpt
            particle <- emitter.ParticleInitializer time constrain emitter

        /// Determine emitter's liveness.
        static member getLiveness time emitter =
            match emitter.ParticleLifeTimeOpt with
            | 0L -> Live
            | lifeTime -> if Life.getLiveness (time - lifeTime) emitter.Life then Live else Dead

        /// Run the emitter.
        static member run time (constrain : Constraint) (emitter : 'a Emitter) =

            // determine local time
            let localTime = time - emitter.Life.StartTime

            // combine constraints
            let constrain = constrain + emitter.Constraint

            // emit new particles if live
            if Life.getLiveness time emitter.Life then
                let emitCountLastFrame = single (dec localTime) * emitter.ParticleRate
                let emitCountThisFrame = single localTime * emitter.ParticleRate
                let emitCount = int emitCountThisFrame - int emitCountLastFrame
                for _ in 0 .. emitCount - 1 do Emitter<'a>.emit time constrain emitter

            // update emitter in-place
            let output = emitter.EmitterBehavior time constrain emitter

            // update emitter compositionally
            let (emitter, output2) = Behaviors.run time emitter.EmitterBehaviors constrain emitter

            // update existing particles in-place
            let output3 = emitter.ParticleBehavior time constrain emitter

            // update existing particles compositionally
            let (particleBuffer, output4) = Behaviors.runMany time emitter.ParticleBehaviors constrain emitter.ParticleBuffer
            let emitter = { emitter with ParticleBuffer = particleBuffer }

            // fin
            (emitter, output + output2 + output3 + output4)

        /// Make a basic particle emitter.
        static member make<'a>
            time body elevation absolute blend image lifeTimeOpt particleLifeTimeOpt particleRate particleMax particleSeed
            constrain particleInitializer particleBehavior particleBehaviors emitterBehavior emitterBehaviors toParticlesDescriptor : 'a Emitter =
            { Body = body
              Elevation = elevation
              Absolute = absolute
              Blend = blend
              Image = image
              Life = Life.make time lifeTimeOpt
              ParticleLifeTimeOpt = particleLifeTimeOpt
              ParticleRate = particleRate
              ParticleIndex = 0
              ParticleWatermark = 0
              ParticleBuffer = Array.zeroCreate particleMax
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
            member this.Run time constrain =
                let (emitter, output) = Emitter<'a>.run time constrain this
                (emitter :> Emitter, output)
            member this.ToParticlesDescriptor () =
                this.ToParticlesDescriptor this
            member this.Resize particleMax =
                if  this.ParticleBuffer.Length <> particleMax then
                    this.ParticleIndex <- 0
                    this.ParticleWatermark <- 0
                    { this with ParticleBuffer = Array.zeroCreate<'a> particleMax } :> Emitter
                else this :> Emitter
            end

    /// A particle system.
    type [<NoEquality; NoComparison>] ParticleSystem =
        { Emitters : Map<string, Emitter> }

        /// Run the particle system.
        static member run time constrain particleSystem =
            let (emitters, output) =
                Map.fold (fun (emitters, output) emitterId (emitter : Emitter) ->
                    let (emitter, output2) = emitter.Run time constrain
                    let emitters = match emitter.GetLiveness time with Live -> Map.add emitterId emitter emitters | Dead -> emitters
                    (emitters, output + output2))
                    (Map.empty, Output.empty)
                    particleSystem.Emitters
            let particleSystem = { Emitters = emitters }
            (particleSystem, output)

        /// Convert the emitted particles to ParticlesDescriptors.
        static member toParticleDescriptors particleSystem =
            let descriptorsRev =
                Map.fold (fun descriptors _ (emitter : Emitter) ->
                    (emitter.ToParticlesDescriptor () :: descriptors))
                    [] particleSystem.Emitters
            List.rev descriptorsRev

        /// The empty particle system.
        static member empty =
            { Emitters = Map.empty }

    /// A basic particle.
    type [<StructuralEquality; NoComparison; Struct>] BasicParticle =
        { mutable Life : Life
          mutable Body : Body
          mutable Size : Vector2
          mutable Offset : Vector2
          mutable Inset : Vector4
          mutable Color : Color
          mutable Glow : Color
          mutable Flip : Flip }
        interface Particle with member this.Life with get () = this.Life and set value = this.Life <- value
        static member inline body = Scope.make (fun p -> p.Body) (fun v p -> { p with Body = v })
        static member inline position = Scope.make (fun p -> struct (p.Life, p.Body.Position)) (fun struct (_, v) p -> { p with Body = { p.Body with Position = v }})
        static member inline rotation = Scope.make (fun p -> struct (p.Life, p.Body.Rotation)) (fun struct (_, v) p -> { p with Body = { p.Body with Rotation = v }})
        static member inline size = Scope.make (fun p -> struct (p.Life, p.Size)) (fun struct (_, v) p -> { p with Size = v })
        static member inline offset = Scope.make (fun p -> struct (p.Life, p.Offset)) (fun struct (_, v) p -> { p with Offset = v })
        static member inline inset = Scope.make (fun p -> struct (p.Life, p.Inset)) (fun struct (_, v) p -> { p with Inset = v })
        static member inline color = Scope.make (fun p -> struct (p.Life, p.Color)) (fun struct (_, v) p -> { p with Color = v })
        static member inline glow = Scope.make (fun p -> struct (p.Life, p.Glow)) (fun struct (_, v) p -> { p with Glow = v })
        //static member inline flipH = Scope.make (fun p -> struct (p.Life, p.Flip)) (fun struct (_, v) p -> { p with Flip = v })
        //static member inline flipV = Scope.make (fun p -> struct (p.Life, p.Flip)) (fun struct (_, v) p -> { p with Flip = v })

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
    module BasicEmitter =

        let private toParticlesDescriptor (emitter : BasicEmitter) =
            let particles =
                Array.append
                    (if emitter.ParticleWatermark > emitter.ParticleIndex then Array.skip emitter.ParticleIndex emitter.ParticleBuffer else [||])
                    (Array.take emitter.ParticleIndex emitter.ParticleBuffer)
            let descriptors =
                Array.zeroCreate<ParticleDescriptor> particles.Length
            for index in 0 .. descriptors.Length - 1 do
                let particle = &particles.[index]
                let descriptor = &descriptors.[index]
                descriptor.Transform.Position <- particle.Body.Position
                descriptor.Transform.Rotation <- particle.Body.Rotation
                descriptor.Transform.Size <- particle.Size
                descriptor.Color <- particle.Color
                descriptor.Glow <- particle.Glow
                descriptor.Offset <- particle.Offset
                descriptor.Inset <- particle.Inset
                descriptor.Flip <- particle.Flip
            { Elevation = emitter.Elevation
              PositionY = emitter.Body.Position.Y
              Absolute = emitter.Absolute
              Blend = emitter.Blend
              Image = emitter.Image
              Particles = descriptors }

        /// Resize the emitter.
        let resize particleMax (emitter : BasicEmitter) =
            (emitter :> Emitter).Resize particleMax :?> BasicEmitter

        /// Make a basic particle emitter.
        let make
            time body elevation absolute blend image lifeTimeOpt particleLifeTimeOpt particleRate particleMax particleSeed
            constrain particleInitializer particleBehavior particleBehaviors emitterBehavior emitterBehaviors =
            BasicEmitter.make
                time body elevation absolute blend image lifeTimeOpt particleLifeTimeOpt particleRate particleMax particleSeed
                constrain particleInitializer particleBehavior particleBehaviors emitterBehavior emitterBehaviors toParticlesDescriptor

        /// Make an empty basic particle emitter.
        let makeEmpty time lifeTimeOpt particleLifeTimeOpt particleRate particleMax =
            let image = asset Assets.Default.PackageName Assets.Default.ImageName
            let particleSeed = Unchecked.defaultof<BasicParticle>
            let particleInitializer = fun _ _ (emitter : BasicEmitter) -> emitter.ParticleSeed
            let particleBehavior = fun _ _ _ -> Output.empty
            let particleBehaviors = Behaviors.empty
            let emitterBehavior = fun _ _ _ -> Output.empty
            let emitterBehaviors = Behaviors.empty
            make
                time Body.defaultBody 0.0f false Transparent image lifeTimeOpt particleLifeTimeOpt particleRate particleMax particleSeed
                Constraint.empty particleInitializer particleBehavior particleBehaviors emitterBehavior emitterBehaviors

        /// Make the default basic particle emitter.
        let makeDefault time lifeTimeOpt particleLifeTimeOpt particleRate particleMax =
            let image = asset Assets.Default.PackageName Assets.Default.ImageName
            let particleSeed =
                { Life = Life.make 0L 60L
                  Body = Body.defaultBody
                  Size = Constants.Engine.ParticleSizeDefault
                  Offset = v2Dup 0.5f
                  Inset = v4Zero
                  Color = Color.White
                  Glow = Color.Zero
                  Flip = FlipNone }
            let particleInitializer = fun _ _ (emitter : BasicEmitter) -> emitter.ParticleSeed
            let particleBehavior = fun _ _ _ -> Output.empty
            let particleBehaviors = Behaviors.empty
            let emitterBehavior = fun _ _ _ -> Output.empty
            let emitterBehaviors = Behaviors.empty
            make
                time Body.defaultBody 0.0f false Transparent image lifeTimeOpt particleLifeTimeOpt particleRate particleMax particleSeed
                Constraint.empty particleInitializer particleBehavior particleBehaviors emitterBehavior emitterBehaviors