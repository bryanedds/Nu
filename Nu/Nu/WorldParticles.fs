// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open Prime
module Particles =

    type [<Struct>] Life =
        { LifeTime : int64
          StartTime : int64 }
        static member getProgress (currentTime : int64) life =
            (single currentTime - single life.StartTime) / single life.LifeTime

    type Particle =
        abstract Life : Life

    type Body =
        { Position : Vector2
          Velocity : Vector2
          Gravity : Vector2 }

    type [<CompilationRepresentation (CompilationRepresentationFlags.UseNullAsTrueValue)>] Constraint =
        | Rectangle of Vector4
        | Circle of single * Vector2
        | Constraints of Constraint array
        | Unconstrained // OPTIMIZATION: elide Option indirection

    type Emitter =
        abstract Run : int64 -> Constraint -> Liveness * Emitter

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
            { In = fun currentTime (particle : 'p) -> struct (Life.getProgress currentTime (^p : (member Life : Life) particle), getField particle)
              Out = fun struct (field, output) (particle : 'p) -> struct (setField field particle, output) : struct ('p * Output) }

    type Behavior =
        abstract Run : int64 -> obj -> (obj * Output)

    type [<NoEquality; NoComparison>] Behavior<'a, 'b when 'a : struct> =
        { Scope : Scope<'a, 'b>
          Transformer : 'b Transformer }

        static member run currentTime (behavior : Behavior<'a, 'b>) (particles : 'a array) =
            let particles2 = Array.map (behavior.Scope.In currentTime) particles
            let particles3 = Array.map (fun struct (progress, field) -> behavior.Transformer progress field) particles2
            let particles4 = Array.map2 behavior.Scope.Out particles3 particles
            let particles5 = Array.map fst' particles4
            let outputs = particles4 |> Array.filter (function struct (_, NoOutput) -> false | (_, _) -> true) |> Array.map snd'
            let output = match outputs with [||] -> NoOutput | [|output|] -> output | outputs -> Outputs outputs
            (particles5, output)

        interface Behavior with
            member this.Run currentTime particlesObj =
                let (particles, outputs) = Behavior<'a, 'b>.run currentTime this (particlesObj :?> 'a array)
                (particles :> obj, outputs)

    type [<NoEquality; NoComparison>] Behaviors =
        { Behaviors : Behavior FStack }

        static member empty =
            { Behaviors = FStack.empty }

        static member add behavior behaviors =
            { Behaviors = FStack.conj behavior behaviors.Behaviors }

        static member addMany behaviorsMany behaviors =
            { Behaviors = Seq.fold (fun behaviors behavior -> FStack.conj behavior behaviors) behaviors.Behaviors behaviorsMany }

        static member fromSeq seq =
            Behaviors.addMany seq Behaviors.empty

        static member run currentTime behaviors (particles : 'a array) =
            let (particles, outputs) =
                Seq.fold (fun (particles, output) (behavior : Behavior) ->
                    let (particles2, output2) = behavior.Run currentTime particles
                    (particles2, output + output2))
                    (particles :> obj, NoOutput)
                    behaviors
            (particles :?> 'a array, outputs)

    type [<NoEquality; NoComparison>] Emitter<'a when 'a : struct> =
        { LifeTime : int64
          StartTime : int64
          ParticleSeed : 'a
          ParticleIndex : int // operates as a ring-buffer
          Particles : 'a array
          Id : Guid }

        static member run (_ : int64) (_ : Constraint) emitter =
            let particles = emitter.Particles
            let liveness = Running
            // TODO: and now for the tricky bit...
            (liveness, { emitter with Particles = particles })

        interface Emitter with
            member this.Run currentTime constrain =
                let (liveness, emitter) = Emitter<'a>.run currentTime constrain this
                (liveness, emitter :> Emitter)
            end

    type [<NoEquality; NoComparison>] ParticleSystem =
        { Emitters : Map<Guid, Emitter> }

        static member run (_ : int64) particleSystem =
            let emittersExpired = []
            // TODO: ...
            { Emitters = Map.removeMany emittersExpired particleSystem.Emitters }

    type [<Struct>] Pex =
        { Life : Life
          Bod : Body
          Col : Color
          Ins : Vector4 }
        interface Particle with member this.Life = this.Life
        static member inline bod = Scope.make (fun pex -> pex.Bod) (fun bod pex -> { pex with Bod = bod })
        static member inline col = Scope.make (fun pex -> pex.Col) (fun col pex -> { pex with Col = col })
        static member inline ins = Scope.make (fun pex -> pex.Ins) (fun ins pex -> { pex with Ins = ins })
