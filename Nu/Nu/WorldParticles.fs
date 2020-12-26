// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Numerics
open Prime
module Particles =

    /// Describes the life of an instance value.
    /// OPTIMIZATION: LifeTimeOpt uses 0L to represent infinite life.
    /// OPTIMIZATION: avoids use of Liveness type to avoid its constructor calls.
    type [<NoEquality; NoComparison; Struct>] Life =
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
    type [<NoEquality; NoComparison; Struct>] Body =
        { mutable Position : Vector2
          mutable LinearVelocity : Vector2
          mutable Rotation : single
          mutable AngularVelocity : single
          mutable Gravity : Vector2 }

    /// The base particle type.
    type Particle =

        /// The life of the particle.
        abstract Life : Life with get, set

    /// A particle constraint.
    type [<StructuralEquality; NoComparison; CompilationRepresentation (CompilationRepresentationFlags.UseNullAsTrueValue)>] Constraint =
        | Rectangle of Vector4
        | Circle of single * Vector2
        | Constraints of Constraint array
        | Unconstrained // OPTIMIZATION: elide Option indirection

        static member (+) (constrain, constrain2) =
            match (constrain, constrain2) with
            | (Unconstrained, Unconstrained) -> constrain // OPTIMIZATION: elide Constraint ctor
            | (_, Unconstrained) -> constrain
            | (Unconstrained, _) -> constrain2
            | (_, _) -> Constraints [|constrain; constrain2|]

    /// The output of a particle behavior.
    type [<NoEquality; NoComparison; CompilationRepresentation (CompilationRepresentationFlags.UseNullAsTrueValue)>] Output =
        | EmitterOutput of Emitter
        | SoundOutput of single * Sound AssetTag
        | Outputs of Output array
        | NoOutput // OPTIMIZATION: elide Option indirection

        static member (+) (output, output2) =
            match (output, output2) with
            | (NoOutput, NoOutput) -> output // OPTIMIZATION: elide Output ctor
            | (_, NoOutput) -> output
            | (NoOutput, _) -> output2
            | (_, _) -> Outputs [|output; output2|]

    /// The base particle emitter type.
    and Emitter =

        /// Determine liveness of emitter.
        abstract GetLiveness : int64 -> Liveness

        /// Run the emitter.
        abstract Run : int64 -> Constraint -> Emitter * Output

    /// Transforms a particle value.
    type 'a Transformer =
        single -> 'a -> struct ('a * Output)

    [<AutoOpen>]
    module Transformer =

        /// Pipe two transformers.
        let pipe (transformer : 'a Transformer) (transformer2 : 'a Transformer) : 'a Transformer =
            fun progress a ->
                let struct (a, output) = transformer progress a
                let struct (a, output2) = transformer2 progress a
                struct (a, output + output2)

        /// Pipe multiple transformers.
        let pipeMany transformers =
            Seq.reduce pipe transformers

    /// Scopes transformable values.
    type [<NoEquality; NoComparison>] Scope<'a, 'b when 'a : struct> =
        { In : int64 -> 'a -> struct (single * 'b)
          Out : struct ('b * Output) -> 'a -> struct ('a * Output) }

    [<RequireQualifiedAccess>]
    module Scope =

        /// Make a scope.
        let inline make< ^p, ^q when ^p : struct and ^p : (member Life : Life)> (getField : ^p -> ^q) (setField : ^q -> ^p -> ^p) : Scope< ^p, ^q> =
            { In = fun time (particle : 'p) -> struct (Life.getProgress time (^p : (member Life : Life) particle), getField particle)
              Out = fun struct (field, output) (particle : 'p) -> struct (setField field particle, output) : struct ('p * Output) }

    /// The base particle behavior type.
    type Behavior =

        /// Run the behavior.
        abstract Run : int64 -> obj -> (obj * Output)

    /// Defines a particle behavior.
    type [<NoEquality; NoComparison>] Behavior<'a, 'b when 'a : struct> =
        { Scope : Scope<'a, 'b>
          Transformer : 'b Transformer }

        /// Run the behavior over an array of particles.
        static member run time (behavior : Behavior<'a, 'b>) (particles : 'a array) =
            let particles2 = Array.map (behavior.Scope.In time) particles
            let particles3 = Array.map (fun struct (progress, field) -> behavior.Transformer progress field) particles2
            let particles4 = Array.map2 behavior.Scope.Out particles3 particles
            let particles5 = Array.map fst' particles4
            let outputs = particles4 |> Array.filter (function struct (_, NoOutput) -> false | struct (_, _) -> true) |> Array.map snd'
            let output = match outputs with [||] -> NoOutput | [|output|] -> output | outputs -> Outputs outputs
            (particles5, output)

        interface Behavior with
            member this.Run time particlesObj =
                let (particles, outputs) = Behavior<'a, 'b>.run time this (particlesObj :?> 'a array)
                (particles :> obj, outputs)

    /// A composition of behaviors.
    type [<NoEquality; NoComparison>] CompositionalBehaviors =
        { Behaviors : Behavior FStack }

        /// The empty compositional behavior.
        static member empty =
            { Behaviors = FStack.empty }

        /// Add a behavior.
        static member add behavior behaviors =
            { Behaviors = FStack.conj behavior behaviors.Behaviors }

        /// Add multiple behaviors.
        static member addMany behaviorsMany behaviors =
            { Behaviors = Seq.fold (fun behaviors behavior -> FStack.conj behavior behaviors) behaviors.Behaviors behaviorsMany }

        /// Make a compositional behavior from a sequence of behaviors.
        static member ofSeq seq =
            CompositionalBehaviors.addMany seq CompositionalBehaviors.empty

        /// Run the compositional behaviors over an array.
        static member run time behaviors (particles : 'a array) =
            let (particles, outputs) =
                FStack.fold (fun (particles, output) (behavior : Behavior) ->
                    let (particles2, output2) = behavior.Run time particles
                    (particles2, output + output2))
                    (particles :> obj, NoOutput)
                    behaviors.Behaviors
            (particles :?> 'a array, outputs)

    /// The default particle emitter.
    /// NOTE: ideally, this would be an abstract data type, but I feel that would discourage users from making their
    /// own emitters - it would looks like making an emitter would require a lot of additional boilerplate as well as
    /// making it harder to use this existing emitter as an example.
    type [<NoEquality; NoComparison>] Emitter<'a when 'a :> Particle and 'a : struct> =
        { Life : Life
          ParticleLifeTimeOpt : int64 // OPTIMIZATION: uses 0L to represent infinite life.
          mutable ParticleIndex : int // operates as a ring-buffer
          mutable ParticleWatermark : int // tracks the highest active particle index; never decreases.
          Particles : 'a array // operates as a ring-buffer
          Initializer : int64 -> Constraint -> 'a Emitter -> 'a
          InPlaceBehavior : int64 -> Constraint -> 'a Emitter -> Output
          CompositionalBehaviors : CompositionalBehaviors
          ToParticlesDescriptor : int64 -> 'a Emitter -> ParticlesDescriptor
          Constraint : Constraint
          Body : Body
          Image : Image AssetTag
          Rate : single
          Id : Guid }

        static member private emit time constrain emitter =
            let particleIndex = if emitter.ParticleIndex >= emitter.Particles.Length then 0 else inc emitter.ParticleIndex
            if particleIndex > emitter.ParticleWatermark then emitter.ParticleWatermark <- particleIndex
            emitter.ParticleIndex <- particleIndex
            let particle = &emitter.Particles.[particleIndex]
            particle.Life <- { LifeTimeOpt = particle.Life.LifeTimeOpt; StartTime = time }
            particle <- emitter.Initializer time constrain emitter

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
                let emitCountLastFrame = single (dec localTime) * emitter.Rate
                let emitCountThisFrame = single localTime * emitter.Rate
                let emitCount = int emitCountThisFrame - int emitCountLastFrame
                for _ in 0 .. emitCount - 1 do Emitter<'a>.emit time constrain emitter

            // update existing particles in-place
            let outputInPlace = emitter.InPlaceBehavior time constrain emitter

            // update existing particles compositionally
            let (particles, outputCompositional) = CompositionalBehaviors.run time emitter.CompositionalBehaviors emitter.Particles
            let emitter = { emitter with Particles = particles }

            // compose output
            let output = outputCompositional + outputInPlace
            (emitter, output)

        interface Emitter with
            member this.GetLiveness time =
                Emitter<'a>.getLiveness time this
            member this.Run time constrain =
                let (emitter, output) = Emitter<'a>.run time constrain this
                (emitter :> Emitter, output)
            end

    /// A particle system.
    type [<NoEquality; NoComparison>] ParticleSystem =
        { Emitters : Map<Guid, Emitter> }

        /// Run the particle system.
        static member run time constrain particleSystem =
            let (emitters, output) =
                Map.fold (fun (emitters, output) emitterId (emitter : Emitter) ->
                    let (emitter, output2) = emitter.Run time constrain
                    let emitters = match emitter.GetLiveness time with Live -> Map.add emitterId emitter emitters | Dead -> emitters
                    (emitters, output + output2))
                    (Map.empty, NoOutput)
                    particleSystem.Emitters
            let particleSystem = { Emitters = emitters }
            (particleSystem, output)

    /// A basic particle.
    type [<NoEquality; NoComparison; Struct>] BasicParticle =
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

    /// A basic particle emitter.
    type BasicEmitter =
        Emitter<BasicParticle>

    [<RequireQualifiedAccess>]
    module BasicEmitter =

        let private toParticlesDescriptor (_ : int64) (emitter : BasicEmitter) =
            let particles =
                Array.append
                    (if emitter.ParticleWatermark > emitter.ParticleIndex then Array.skip emitter.ParticleIndex emitter.Particles else [||])
                    (Array.take emitter.ParticleIndex emitter.Particles)
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
            { Particles = descriptors; Image = emitter.Image }

        let make time lifeTimeOpt particleLifeTimeOpt particleMax initializer inPlaceBehavior behaviors constrain image rate =
            { Life = { StartTime = time; LifeTimeOpt = lifeTimeOpt }
              ParticleLifeTimeOpt = particleLifeTimeOpt
              ParticleIndex = 0
              ParticleWatermark = 0
              Particles = Array.zeroCreate particleMax
              Initializer = initializer
              InPlaceBehavior = inPlaceBehavior
              CompositionalBehaviors = behaviors
              ToParticlesDescriptor = toParticlesDescriptor
              Constraint = constrain
              Body = { Position = v2Zero; LinearVelocity = v2Zero; Rotation = 0.0f; AngularVelocity = 0.0f; Gravity = v2 0.0f -9.81f }
              Image = image
              Rate = rate
              Id = Gen.id }