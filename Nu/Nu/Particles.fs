// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Numerics
open Prime
module Particles =

    /// The type of range.
    type [<StructuralEquality; StructuralComparison; Struct>] RangeType =
        | Constant
        | Linear
        | Random
        | Chaos
        | Ease
        | EaseIn
        | EaseOut
        | Sin
        | SinScaled
        | Cos
        | CosScaled

    /// Describes range of behavior over a section of a target's life time.
    type [<NoEquality; NoComparison; Struct>] 'a Range =
        { RangeType : RangeType
          RangeScalar : single
          RangeBegin : 'a
          RangeEnd : 'a
          RangeProgressBegin : single
          RangeProgressEnd : single }

    /// The forces that may operate on a target.
    type [<NoEquality; NoComparison>] Force =
        | Gravity of Vector2
        | Attractor of Vector2 * single

    /// Describes the life of an instance value.
    /// OPTIMIZATION: LifeTimeOpt uses 0L to represent infinite life.
    /// OPTIMIZATION: doesn't use Liveness type to avoid its constructor calls.
    type [<StructuralEquality; NoComparison; Struct>] Life =
        { StartTime : int64
          LifeTimeOpt : int64 }

        /// The progress made through the instance's life.
        static member getProgress time life =
            match life.LifeTimeOpt with
            | 0L -> 1.0f
            | lifeTime -> (single time - single life.StartTime) / single lifeTime

        /// The liveness of the instance as a boolean.
        static member getLiveness time life =
            match life.LifeTimeOpt with
            | 0L -> true
            | lifeTime -> lifeTime < time - life.StartTime

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

    /// A behavioral constraint.
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
    module Transformer =

        /// Pipe two transformers.
        let pipe (transformer : 'a Transformer) (transformer2 : 'a Transformer) : 'a Transformer =
            fun time constrain targets ->
                let (targets, output) = transformer time constrain targets
                let (targets, output2) = transformer2 time constrain targets
                (targets, output + output2)

        /// Pipe multiple transformers.
        let pipeMany transformers =
            Seq.reduce pipe transformers

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
          Transformer : 'b Transformer }

        /// Run the behavior over a single target.
        static member run time (constrain : Constraint) (behavior : Behavior<'a, 'b>) (target : 'a) =
            let (targets, output) = Behavior<'a, 'b>.runMany time constrain behavior [|target|]
            let target = Array.item 0 targets
            (target, output)

        /// Run the behavior over an array of targets.
        /// OPTIMIZATION: runs transformers in batches for better utilization of instruction cache.
        static member runMany time (constrain : Constraint) (behavior : Behavior<'a, 'b>) (targets : 'a array) =
            let targets2 = behavior.Scope.In targets
            let (targets3, output) = behavior.Transformer time constrain targets2
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
            particle.Life <- { LifeTimeOpt = particle.Life.LifeTimeOpt; StartTime = time }
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
              Life = { StartTime = time; LifeTimeOpt = lifeTimeOpt }
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
        static member inline size = Scope.make (fun p -> p.Size) (fun v p -> { p with Size = v })
        static member inline offset = Scope.make (fun p -> p.Offset) (fun v p -> { p with Offset = v })
        static member inline inset = Scope.make (fun p -> p.Inset) (fun v p -> { p with Inset = v })
        static member inline color = Scope.make (fun p -> p.Color) (fun v p -> { p with Color = v })
        static member inline glow = Scope.make (fun p -> p.Glow) (fun v p -> { p with Glow = v })
        static member inline flip = Scope.make (fun p -> p.Flip) (fun v p -> { p with Flip = v })

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
                { Life = { StartTime = 0L; LifeTimeOpt = 60L }
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