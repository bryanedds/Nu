// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.ComponentModel
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module WorldView =

    type World with

        static member internal renderView view world =
            match view with
            | Render2d (elevation, horizon, assetTag, descriptor) ->
                let message = { Elevation = elevation; Horizon = horizon; AssetTag = AssetTag.generalize assetTag; RenderDescriptor2d = descriptor }
                World.enqueueRenderLayeredMessage2d message world
            | Render3d renderMessage -> World.enqueueRenderMessage3d renderMessage world
            | PlaySound (volume, assetTag) -> World.playSound volume assetTag world
            | PlaySong (fadeIn, fadeOut, start, volume, assetTag) -> World.playSong fadeIn fadeOut start volume assetTag world
            | FadeOutSong fade -> World.fadeOutSong fade world
            | StopSong -> World.stopSong world
            | SpawnEmitter (_, _) -> world
            | Tag _ -> world
            | Views views -> Array.fold (fun world view -> World.renderView view world) world views
            | SegmentedViews views -> SegmentedArray.fold (fun world view -> World.renderView view world) world views

[<RequireQualifiedAccess>]
module Effect =

    /// Converts effects, always providing an empty effect when converting from Symbol.
    type EffectConverter () =
        inherit TypeConverter ()

        override this.CanConvertTo (_, destType) =
            destType = typeof<Symbol> ||
            destType = typeof<Effect>

        override this.ConvertTo (_, _, source, destType) =
            if destType = typeof<Symbol> then Symbol.Symbols ([], ValueNone)
            elif destType = typeof<Effect> then source
            else failconv "Invalid EffectConverter conversion to source." None

        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Symbol> ||
            sourceType = typeof<GameTime>

        override this.ConvertFrom (_, _, source) =
            match source with
            | :? Symbol -> WorldModule.getEmptyEffect ()
            | :? Effect -> source
            | _ -> failconv "Invalid EffectConverter conversion from source." None

    /// An effect.
    and [<ReferenceEquality; NoComparison; TypeConverter (typeof<EffectConverter>)>] Effect =
        private
            { StartTime_ : GameTime
              Centered_ : bool
              Offset_ : Vector3
              Transform_ : Transform
              ParticleSystem_ : Particles.ParticleSystem
              HistoryMax_ : int
              History_ : Effects.Slice Nito.Collections.Deque
              Definitions_ : Effects.Definitions
              Tags_ : Map<string, Effects.Slice>
              Descriptor_ : EffectDescriptor }

        member this.StartTime = this.StartTime_
        member this.Transform = this.Transform_
        member this.Definitions = this.Definitions_
        member this.ParticleSystem = this.ParticleSystem_
        member this.HistoryMax = this.HistoryMax_
        member this.History = this.History_
        member this.Tags = this.Tags_
        member this.Descriptor = this.Descriptor_

    let rec private processParticleSystemOutput output effect world =
        match output with
        | Particles.OutputSound (volume, sound) ->
            let world = World.enqueueAudioMessage (PlaySoundMessage { Volume = volume; Sound = sound }) world
            (effect, world)
        | Particles.OutputEmitter (name, emitter) ->
            let particleSystem = { effect.ParticleSystem_ with Emitters = Map.add name emitter effect.ParticleSystem_.Emitters }
            let effect = { effect with ParticleSystem_ = particleSystem }
            (effect, world)
        | Particles.Outputs outputs ->
            SegmentedArray.fold (fun (effect, world) output ->
                processParticleSystemOutput output effect world)
                (effect, world)
                outputs

    let private liveness effect world =
        let time = World.getGameTime world
        let particleSystem = effect.ParticleSystem_
        let effectDescriptor = effect.Descriptor_
        match effectDescriptor.LifeTimeOpt with
        | Some lifeTime ->
            let localTime = time - effect.StartTime_
            if localTime <= lifeTime then Live
            else ParticleSystem.getLiveness time particleSystem
        | None -> Live

    /// Run an effect, applying side-effects such as issuing rendering and audio commands as needed.
    let run effect (world : World) =

        // run if live
        match liveness effect world with
        | Live ->

            // set up effect system to evaluate effect
            let time = world.GameTime
            let localTime = time - effect.StartTime_
            let delta = world.GameDelta
            let mutable transform = effect.Transform_
            let effectSlice =
                { Effects.Position = transform.Position
                  Effects.Scale = transform.Scale
                  Effects.Angles = transform.Angles
                  Effects.Elevation = transform.Elevation
                  Effects.Offset = effect.Offset_
                  Effects.Size = transform.Size
                  Effects.Inset = Box2.Zero
                  Effects.Color = Color.One
                  Effects.Blend = Transparent
                  Effects.Glow = Color.Zero
                  Effects.Flip = FlipNone
                  Effects.Volume = Constants.Audio.SoundVolumeDefault
                  Effects.Enabled = true
                  Effects.Centered = effect.Centered_ }
            let effectSystem = EffectSystem.make localTime delta transform.Absolute effect.Definitions_

            // evaluate effect with effect system
            let (view, _) = EffectSystem.eval effect.Descriptor_ effectSlice effect.History_ effectSystem

            // render effect view
            let world = World.renderView view world

            // convert view to array for storing tags and spawning emitters
            let views = View.toSeq view

            // extract tags
            let tags =
                views |>
                Seq.choose (function Tag (name, value) -> Some (name, value :?> Effects.Slice) | _ -> None) |>
                Map.ofSeq

            // spawn emitters
            let particleSystem =
                views |>
                Seq.choose (function SpawnEmitter (name, descriptor) -> Some (name, descriptor) | _ -> None) |>
                Seq.choose (fun (name : string, descriptor : EmitterDescriptor) ->
                    match descriptor with
                    | :? Particles.BasicEmitterDescriptor as descriptor ->
                        match World.tryMakeEmitter time descriptor.LifeTimeOpt descriptor.ParticleLifeTimeMaxOpt descriptor.ParticleRate descriptor.ParticleMax descriptor.Style world with
                        | Some (:? Particles.BasicEmitter as emitter) ->
                            let emitter =
                                { emitter with
                                    Body = descriptor.Body
                                    Blend = descriptor.Blend
                                    Image = descriptor.Image
                                    ParticleSeed = descriptor.ParticleSeed
                                    Constraint = descriptor.Constraint }
                            Some (name, emitter)
                        | _ -> None
                    | _ -> None) |>
                Seq.fold (fun particleSystem (name, emitter) ->
                    ParticleSystem.add name emitter particleSystem)
                    effect.ParticleSystem_

            // update effect history in-place
            effect.History_.AddToFront effectSlice
            if  effect.History_.Count > effect.HistoryMax_ then
                effect.History_.RemoveFromBack () |> ignore

            // update tags
            let effect = { effect with Tags_ = tags }

            // run particles
            let (particleSystem, output) = ParticleSystem.run delta time particleSystem
            let effect = { effect with ParticleSystem_ = particleSystem }
            let (effect, world) = processParticleSystemOutput output effect world

            // render particles
            let particlesMessages =
                particleSystem |>
                ParticleSystem.toParticlesDescriptors time |>
                List.map (fun (descriptor : ParticlesDescriptor) ->
                    { Elevation = descriptor.Elevation
                      Horizon = descriptor.Horizon
                      AssetTag = AssetTag.generalize descriptor.Image
                      RenderDescriptor2d = ParticlesDescriptor descriptor })
            let world = World.enqueueRenderLayeredMessages2d particlesMessages world

            // fin
            (Live, effect, world)

        // dead
        | Dead -> (Dead, effect, world)

    /// Make an effect.
    let makePlus startTime centered offset transform particleSystem historyMax history definitions descriptor =
        { StartTime_ = startTime
          Centered_ = centered
          Offset_ = offset
          Transform_ = transform
          ParticleSystem_ = particleSystem
          HistoryMax_ = historyMax
          History_ = history
          Definitions_ = definitions
          Tags_ = Map.empty
          Descriptor_ = descriptor }

    /// Make an effect.
    let make startTime offset transform descriptor =
        makePlus startTime true offset transform ParticleSystem.empty Constants.Effects.EffectHistoryMaxDefault (Nito.Collections.Deque ()) Map.empty descriptor

    /// The empty effect.
    let empty =
        make GameTime.zero v3Zero (Transform.makeEmpty ()) EffectDescriptor.empty

type Effect = Effect.Effect