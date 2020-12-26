// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Numerics
open Prime
module Particles =

    /// Describes the life of an instance value.
    type [<NoEquality; NoComparison; Struct>] Life =
        { LifeTime : int64
          StartTime : int64 }

        /// The progress made through the instance's life.
        static member getProgress (time : int64) life =
            (single time - single life.StartTime) / single life.LifeTime

        /// The liveness of the instance.
        /// OPTIMIZATION: avoiding use of Liveness type to avoid its constructor calls.
        static member getLiveness (time : int64) life =
            let localTime = time - life.StartTime
            life.LifeTime < localTime

    /// Describes the body of an instance value.
    type [<NoEquality; NoComparison; Struct>] Body =
        { Position : Vector2
          Velocity : Vector2
          Gravity : Vector2 }

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

    /// A collection of behaviors.
    type [<NoEquality; NoComparison>] Behaviors =
        { Behaviors : Behavior FStack }

        /// The empty behavior.
        static member empty =
            { Behaviors = FStack.empty }

        /// Combine two behaviors.
        static member add behavior behaviors =
            { Behaviors = FStack.conj behavior behaviors.Behaviors }

        /// Combine multiple behaviors.
        static member addMany behaviorsMany behaviors =
            { Behaviors = Seq.fold (fun behaviors behavior -> FStack.conj behavior behaviors) behaviors.Behaviors behaviorsMany }

        /// Make a collection of behaviors from a sequence of behaviors.
        static member ofSeq seq =
            Behaviors.addMany seq Behaviors.empty

        /// Run the behaviors over an array.
        static member run time behaviors (particles : 'a array) =
            let (particles, outputs) =
                FStack.fold (fun (particles, output) (behavior : Behavior) ->
                    let (particles2, output2) = behavior.Run time particles
                    (particles2, output + output2))
                    (particles :> obj, NoOutput)
                    behaviors.Behaviors
            (particles :?> 'a array, outputs)

    /// The default particle emitter.
    type [<NoEquality; NoComparison>] Emitter<'a when 'a :> Particle and 'a : struct> =
        { StartTime : int64
          LifeTimeOpt : int64 option
          ParticleLifeTimeMax : int64
          mutable ParticleIndex : int // operates as a ring-buffer
          Particles : 'a array // operates as a ring-buffer
          Initializer : int64 -> Constraint -> 'a Emitter -> 'a
          Behaviors : Behaviors
          Constraint : Constraint
          Body : Body
          Rate : single
          Id : Guid }

        static member private emit time constrain emitter =
            let particleIndex = if emitter.ParticleIndex >= emitter.Particles.Length then 0 else inc emitter.ParticleIndex
            emitter.ParticleIndex <- particleIndex
            let particle = &emitter.Particles.[particleIndex]
            particle.Life <- { LifeTime = particle.Life.LifeTime; StartTime = time }
            particle <- emitter.Initializer time constrain emitter

        static member private getEmitting time emitter =
            match emitter.LifeTimeOpt with
            | Some lifeTime ->
                let life = { StartTime = emitter.StartTime; LifeTime = lifeTime }
                Life.getLiveness time life
            | None -> true

        /// Determine emitter's liveness.
        static member getLiveness time emitter =
            match emitter.LifeTimeOpt with
            | Some lifeTime ->
                let life = { StartTime = emitter.StartTime; LifeTime = lifeTime + emitter.ParticleLifeTimeMax }
                if Life.getLiveness time life then Live else Dead
            | None -> Live

        /// Run the emitter.
        static member run time (constrain : Constraint) (emitter : 'a Emitter) =

            // determine local time
            let localTime = time - emitter.StartTime

            // combine constraints
            let constrain = constrain + emitter.Constraint

            // emit new particles if live
            if Emitter<'a>.getEmitting time emitter then
                let emitCountLastFrame = single (dec localTime) * emitter.Rate
                let emitCountThisFrame = single localTime * emitter.Rate
                let emitCount = int emitCountThisFrame - int emitCountLastFrame
                for _ in 0 .. dec emitCount do Emitter<'a>.emit time constrain emitter

            // update existing particles
            let (particles, output) = Behaviors.run time emitter.Behaviors emitter.Particles
            let emitter = { emitter with Particles = particles }
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

    /// An example particle.
    type [<NoEquality; NoComparison; Struct>] Pex =
        { mutable Life : Life
          Bod : Body
          Col : Color
          Ins : Vector4 }
        interface Particle with member this.Life with get () = this.Life and set value = this.Life <- value
        static member inline bod = Scope.make (fun pex -> pex.Bod) (fun bod pex -> { pex with Bod = bod })
        static member inline col = Scope.make (fun pex -> pex.Col) (fun col pex -> { pex with Col = col })
        static member inline ins = Scope.make (fun pex -> pex.Ins) (fun ins pex -> { pex with Ins = ins })
