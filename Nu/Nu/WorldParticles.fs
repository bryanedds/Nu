// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open Prime

type Emitter =
    unit

type [<Struct>] Life =
    { LifeTime : int64
      StartTime : int64 }
    static member getProgress (currentTime : int64) life =
        (single currentTime - single life.StartTime) / single life.LifeTime

type Particle =
    abstract Life : Life

type [<NoEquality; NoComparison; CompilationRepresentation (CompilationRepresentationFlags.UseNullAsTrueValue)>] Output =
    | EmitterOutput of Emitter
    | SoundOutput of single * Sound AssetTag
    | Outputs of Output list
    | NoOutput // OPTIMIZATION: elide Option indirection

type 'a Transformer =
    single -> 'a -> struct ('a * Output)

type [<NoEquality; NoComparison>] Scope<'a, 'b when 'a : struct> =
    { In : int64 -> 'a -> struct (single * 'b)
      Out : struct ('b * Output) -> 'a -> struct ('a * Output) }

module Scope =
    let inline make< ^p, ^q when ^p : struct and ^p : (member Life : Life)> (getField : ^p -> ^q) (setField : ^q -> ^p -> ^p) : Scope< ^p, ^q> =
        { In = fun currentTime (particle : 'p) -> struct (Life.getProgress currentTime (^p : (member Life : Life) particle), getField particle)
          Out = fun struct (field, output) (particle : 'p) -> struct (setField field particle, output) : struct ('p * Output) }

type Behavior =
    abstract Run : int64 -> obj -> obj

type [<NoEquality; NoComparison>] Behavior<'a, 'b when 'a : struct> =
    { Scope : Scope<'a, 'b>
      Transformer : 'b Transformer }

    static member run currentTime (behavior : Behavior<'a, 'b>) (particles : 'a array) =
        let particles' = Array.map (behavior.Scope.In currentTime) particles
        let particles'' = Array.map (fun struct (progress, b) -> behavior.Transformer progress b) particles'
        Array.map2 behavior.Scope.Out particles'' particles

    interface Behavior with
        member this.Run currentTime particlesObj =
            Behavior<'a, 'b>.run currentTime this (particlesObj :?> 'a array) :> obj

type [<NoEquality; NoComparison>] Behaviors =
    { Behaviors : Behavior FStack }
    static member empty =
        { Behaviors = FStack.empty }
    static member add behavior behaviors =
        { Behaviors = FStack.conj behavior behaviors.Behaviors }
    static member run progress behaviors (particles : 'a array) =
        let result =
            Seq.fold (fun particles (behavior : Behavior) ->
                behavior.Run progress particles)
                (particles :> obj)
                behaviors
        result :?> 'a array

type [<NoEquality; NoComparison>] Emitter<'a when 'a : struct> =
    { LifeTime : int64
      StartTime : int64
      ParticleSeed : 'a
      ParticleIndex : int // operates as a ring-buffer
      Particles : 'a array }

type [<NoEquality; NoComparison>] ParticleSystem =
    { Emitters : Emitter FStack }

module Particles =

    let pipe transformer transformer2 =
        fun progress a ->
            transformer2 progress (transformer progress a)

    type [<Struct>] Pex =
        { Life : Life
          Pos : Vector2
          Cen : Vector2
          Col : Color }
        interface Particle with member this.Life = this.Life
        static member inline pos = Scope.make (fun pex -> pex.Pos) (fun pos pex -> { pex with Pos = pos })
        static member inline cen = Scope.make (fun pex -> pex.Cen) (fun cen pex -> { pex with Cen = cen })
        static member inline col = Scope.make (fun pex -> pex.Col) (fun col pex -> { pex with Col = col })