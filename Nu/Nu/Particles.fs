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
          mutable AngularVelocity : single
          mutable Gravity : Vector2 }

        /// The default body.
        static member defaultBody =
            { Position = v2Zero
              LinearVelocity = v2Zero
              Rotation = 0.0f
              AngularVelocity = 0.0f
              Gravity = Constants.Engine.Gravity }

    /// The base particle type.
    type Particle =

        /// The life of the particle.
        abstract Life : Life with get, set

    /// A particle constraint.
    type [<StructuralEquality; NoComparison; CompilationRepresentation (CompilationRepresentationFlags.UseNullAsTrueValue)>] Constraint =
        | Rectangle of Vector4
        | Circle of single * Vector2
        | Constraints of Constraint array
        | NoConstraint // OPTIMIZATION: elide Option indirection

        /// Combine two constraints.
        static member (+) (constrain, constrain2) =
            match (constrain, constrain2) with
            | (NoConstraint, NoConstraint) -> constrain // OPTIMIZATION: elide Constraint ctor
            | (_, NoConstraint) -> constrain
            | (NoConstraint, _) -> constrain2
            | (_, _) -> Constraints [|constrain; constrain2|]

    /// The output of a particle behavior.
    type [<NoEquality; NoComparison; CompilationRepresentation (CompilationRepresentationFlags.UseNullAsTrueValue)>] Output =
        | EmitterOutput of Emitter
        | PlaySoundOutput of single * Sound AssetTag
        | Outputs of Output array
        | NoOutput // OPTIMIZATION: elide Option indirection

        /// Combine two outputs.
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

        /// Convert the emitted particles to a ParticlesDescriptor.
        abstract ToParticlesDescriptor : unit -> ParticlesDescriptor

        /// Change the maximum number of allowable particles.
        abstract Resize : int -> Emitter

    /// Transforms a particle value.
    type 'a Transformer =
        single -> Constraint -> 'a -> struct ('a * Output)

    [<AutoOpen>]
    module Transformer =

        /// Pipe two transformers.
        let pipe (transformer : 'a Transformer) (transformer2 : 'a Transformer) : 'a Transformer =
            fun progress constrain a ->
                let struct (a, output) = transformer progress constrain a
                let struct (a, output2) = transformer2 progress constrain a
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
        abstract Run : int64 -> Constraint -> obj -> (obj * Output)

    /// Defines a particle behavior.
    type [<NoEquality; NoComparison>] Behavior<'a, 'b when 'a : struct> =
        { Scope : Scope<'a, 'b>
          Transformer : 'b Transformer }

        /// Run the behavior over an array of particles.
        static member run time (constrain : Constraint) (behavior : Behavior<'a, 'b>) (particles : 'a array) =
            let particles2 = Array.map (behavior.Scope.In time) particles
            let particles3 = Array.map (fun struct (progress, field) -> behavior.Transformer progress constrain field) particles2
            let particles4 = Array.map2 behavior.Scope.Out particles3 particles
            let particles5 = Array.map fst' particles4
            let outputs = particles4 |> Array.filter (function struct (_, NoOutput) -> false | struct (_, _) -> true) |> Array.map snd'
            let output = match outputs with [||] -> NoOutput | [|output|] -> output | outputs -> Outputs outputs
            (particles5, output)

        interface Behavior with
            member this.Run time constrain particlesObj =
                let (particles, outputs) = Behavior<'a, 'b>.run time constrain this (particlesObj :?> 'a array)
                (particles :> obj, outputs)

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

        /// Run the behaviors over an array.
        static member run time behaviors constrain (particles : 'a array) =
            let (particles, outputs) =
                FStack.fold (fun (particles, output) (behavior : Behavior) ->
                    let (particles2, output2) = behavior.Run time constrain particles
                    (particles2, output + output2))
                    (particles :> obj, NoOutput)
                    behaviors.Behaviors
            (particles :?> 'a array, outputs)

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
          Initializer : int64 -> Constraint -> 'a Emitter -> 'a
          InPlaceBehavior : int64 -> Constraint -> 'a Emitter -> Output
          CompositionalBehaviors : Behaviors
          ToParticlesDescriptor : 'a Emitter -> ParticlesDescriptor }

        static member private emit time constrain emitter =
            let particleIndex = if emitter.ParticleIndex >= emitter.ParticleBuffer.Length then 0 else inc emitter.ParticleIndex
            if particleIndex > emitter.ParticleWatermark then emitter.ParticleWatermark <- particleIndex
            emitter.ParticleIndex <- particleIndex
            let particle = &emitter.ParticleBuffer.[particleIndex]
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
                let emitCountLastFrame = single (dec localTime) * emitter.ParticleRate
                let emitCountThisFrame = single localTime * emitter.ParticleRate
                let emitCount = int emitCountThisFrame - int emitCountLastFrame
                for _ in 0 .. emitCount - 1 do Emitter<'a>.emit time constrain emitter

            // update existing particles in-place
            let outputInPlace = emitter.InPlaceBehavior time constrain emitter

            // update existing particles compositionally
            let (particleBuffer, outputCompositional) = Behaviors.run time emitter.CompositionalBehaviors constrain emitter.ParticleBuffer
            let emitter = { emitter with ParticleBuffer = particleBuffer }

            // compose output
            let output = outputCompositional + outputInPlace
            (emitter, output)

        /// Make a basic particle emitter.
        static member make<'a> time body elevation absolute blend image lifeTimeOpt particleLifeTimeOpt particleRate particleMax particleSeed constrain initializer inPlaceBehavior behaviors toParticlesDescriptor : 'a Emitter =
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
              Initializer = initializer
              InPlaceBehavior = inPlaceBehavior
              CompositionalBehaviors = behaviors
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
                    (Map.empty, NoOutput)
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
        let make time body elevation absolute blend image lifeTimeOpt particleLifeTimeOpt particleRate particleMax particleSeed constrain initializer inPlaceBehavior behaviors =
            BasicEmitter.make time body elevation absolute blend image lifeTimeOpt particleLifeTimeOpt particleRate particleMax particleSeed constrain initializer inPlaceBehavior behaviors toParticlesDescriptor

        /// Make an empty basic particle emitter.
        let makeEmpty time =
            let image = asset Assets.Default.PackageName Assets.Default.ImageName
            let particleSeed = Unchecked.defaultof<BasicParticle>
            let initializer = fun _ _ (emitter : BasicEmitter) -> emitter.ParticleSeed
            let inPlaceBehavior = fun _ _ _ -> NoOutput
            let behaviors = Behaviors.empty
            make time Body.defaultBody 0.0f false Transparent image 60L 60L 1.0f 60 particleSeed NoConstraint initializer inPlaceBehavior behaviors
