// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Numerics
open Prime
module Particles =

    type [<StructuralEquality; NoComparison; Struct>] Life =
        { LifeTime : int64
          StartTime : int64 }
        static member getProgress (time : int64) life =
            (single time - single life.StartTime) / single life.LifeTime
        static member getLiveness (time : int64) life =
            let localTime = time - life.StartTime
            if life.LifeTime < localTime
            then Running
            else Exiting

    type Particle =
        abstract Life : Life with get, set

    type [<StructuralEquality; NoComparison; Struct>] Body =
        { Position : Vector2
          Velocity : Vector2
          Gravity : Vector2 }

    type [<StructuralEquality; NoComparison; CompilationRepresentation (CompilationRepresentationFlags.UseNullAsTrueValue)>] Constraint =
        | Rectangle of Vector4
        | Circle of single * Vector2
        | Constraints of Constraint array
        | Unconstrained // OPTIMIZATION: elide Option indirection

        static member (+) (constrain, constrain2) =
            match (constrain, constrain2) with
            | (Unconstrained, Unconstrained) -> Unconstrained
            | (_, Unconstrained) -> constrain
            | (Unconstrained, _) -> constrain2
            | (_, _) -> Constraints [|constrain; constrain2|]

    type [<NoEquality; NoComparison; CompilationRepresentation (CompilationRepresentationFlags.UseNullAsTrueValue)>] Output =
        | EmitterOutput of Emitter
        | SoundOutput of single * Sound AssetTag
        | Outputs of Output array
        | NoOutput // OPTIMIZATION: elide Option indirection

        static member (+) (output, output2) =
            match (output, output2) with
            | (NoOutput, NoOutput) -> NoOutput
            | (_, NoOutput) -> output
            | (NoOutput, _) -> output2
            | (_, _) -> Outputs [|output; output2|]

    and Emitter =
        abstract Run : int64 -> Constraint -> Liveness * Emitter * Output

    type 'a Transformer =
        single -> 'a -> struct ('a * Output)

    [<AutoOpen>]
    module Transformer =

        let pipe (transformer : 'a Transformer) (transformer2 : 'a Transformer) : 'a Transformer =
            fun progress a ->
                let struct (a, output) = transformer progress a
                let struct (a, output2) = transformer2 progress a
                struct (a, output + output2)

        let pipeMany transformers =
            Seq.reduce pipe transformers

    type [<NoEquality; NoComparison>] Scope<'a, 'b when 'a : struct> =
        { In : int64 -> 'a -> struct (single * 'b)
          Out : struct ('b * Output) -> 'a -> struct ('a * Output) }

    module Scope =

        let inline make< ^p, ^q when ^p : struct and ^p : (member Life : Life)> (getField : ^p -> ^q) (setField : ^q -> ^p -> ^p) : Scope< ^p, ^q> =
            { In = fun time (particle : 'p) -> struct (Life.getProgress time (^p : (member Life : Life) particle), getField particle)
              Out = fun struct (field, output) (particle : 'p) -> struct (setField field particle, output) : struct ('p * Output) }

    type Behavior =
        abstract Run : int64 -> obj -> (obj * Output)

    type [<NoEquality; NoComparison>] Behavior<'a, 'b when 'a : struct> =
        { Scope : Scope<'a, 'b>
          Transformer : 'b Transformer }

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

    type [<NoEquality; NoComparison>] Behaviors =
        { Behaviors : Behavior FStack }

        static member empty =
            { Behaviors = FStack.empty }

        static member add behavior behaviors =
            { Behaviors = FStack.conj behavior behaviors.Behaviors }

        static member addMany behaviorsMany behaviors =
            { Behaviors = Seq.fold (fun behaviors behavior -> FStack.conj behavior behaviors) behaviors.Behaviors behaviorsMany }

        static member ofSeq seq =
            Behaviors.addMany seq Behaviors.empty

        static member run time behaviors (particles : 'a array) =
            let (particles, outputs) =
                FStack.fold (fun (particles, output) (behavior : Behavior) ->
                    let (particles2, output2) = behavior.Run time particles
                    (particles2, output + output2))
                    (particles :> obj, NoOutput)
                    behaviors.Behaviors
            (particles :?> 'a array, outputs)

    type [<NoEquality; NoComparison>] Emitter<'a when 'a :> Particle and 'a : struct> =
        { Life : Life
          Body : Body
          mutable ParticleIndex : int // operates as a ring-buffer
          Particles : 'a array // operates as a ring-buffer
          Initializer : int64 -> Constraint -> 'a Emitter -> 'a
          Behaviors : Behaviors
          Constraint : Constraint
          Rate : single
          Id : Guid }

        static member computeLiveness time emitter =
            let length = emitter.Particles.Length
            let mutable index = 0
            let mutable result = Exiting
            while (match result with Exiting -> true | Running -> false) && index < length do
                let particle = &emitter.Particles.[index]
                match Life.getLiveness time particle.Life with
                | Running -> result <- Running
                | Exiting -> index <- inc index
            result

        static member emit time constrain emitter =
            let particleIndex = if emitter.ParticleIndex >= emitter.Particles.Length then 0 else inc emitter.ParticleIndex
            emitter.ParticleIndex <- particleIndex
            let particle = &emitter.Particles.[particleIndex]
            particle.Life <- { LifeTime = particle.Life.LifeTime; StartTime = time }
            particle <- emitter.Initializer time constrain emitter

        static member run time (constrain : Constraint) (emitter : 'a Emitter) =

            // combine constraints
            let constrain = constrain + emitter.Constraint
            
            // determine liveness
            let localTime = time - emitter.Life.StartTime
            let liveness = if localTime < emitter.Life.LifeTime then Running else Exiting

            // emit new particles
            match liveness with
            | Running ->
                let emitCountLastFrame = single (dec localTime) * emitter.Rate
                let emitCountThisFrame = single localTime * emitter.Rate
                let emitCount = int emitCountThisFrame - int emitCountLastFrame
                for _ in 0 .. dec emitCount do Emitter<'a>.emit time constrain emitter
            | Exiting -> ()

            // update existing particles
            let (particles, output) = Behaviors.run time emitter.Behaviors emitter.Particles
            let emitter = { emitter with Particles = particles }

            // compute result
            match liveness with
            | Running -> (liveness, emitter, output)
            | Exiting -> (Emitter<'a>.computeLiveness time emitter, emitter, output)

        interface Emitter with
            member this.Run time constrain =
                let (liveness, emitter, output) = Emitter<'a>.run time constrain this
                (liveness, emitter :> Emitter, output)
            end

    type [<NoEquality; NoComparison>] ParticleSystem =
        { Emitters : Map<Guid, Emitter> }

        static member run time constrain particleSystem =
            let (emitters, output) =
                Map.fold (fun (emitters, output) emitterId (emitter : Emitter) ->
                    let (liveness, emitter, output2) = emitter.Run time constrain
                    let emitters = match liveness with Running -> Map.add emitterId emitter emitters | Exiting -> emitters
                    let output3 = output + output2
                    (emitters, output3))
                    (Map.empty, NoOutput)
                    particleSystem.Emitters
            let particleSystem = { Emitters = emitters }
            (particleSystem, output)

    /// An example particle.
    type [<StructuralEquality; NoComparison; Struct>] Pex =
        { mutable Life : Life
          Bod : Body
          Col : Color
          Ins : Vector4 }
        interface Particle with member this.Life with get () = this.Life and set value = this.Life <- value
        static member inline bod = Scope.make (fun pex -> pex.Bod) (fun bod pex -> { pex with Bod = bod })
        static member inline col = Scope.make (fun pex -> pex.Col) (fun col pex -> { pex with Col = col })
        static member inline ins = Scope.make (fun pex -> pex.Ins) (fun ins pex -> { pex with Ins = ins })
