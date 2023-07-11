// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.ComponentModel
open System.Numerics
open Prime
open Nu
open Nu.Effects
open Nu.Particles

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
              RenderType_ : RenderType
              ParticleSystem_ : ParticleSystem
              HistoryMax_ : int
              History_ : Effects.Slice Deque
              Definitions_ : Definitions
              Tags_ : Map<string, Slice>
              Descriptor_ : EffectDescriptor }

        member this.StartTime = this.StartTime_
        member this.Transform = this.Transform_
        member this.RenderType = this.RenderType_
        member this.Definitions = this.Definitions_
        member this.ParticleSystem = this.ParticleSystem_
        member this.HistoryMax = this.HistoryMax_
        member this.History = this.History_
        member this.Tags = this.Tags_
        member this.Descriptor = this.Descriptor_

    let rec private processParticleSystemOutput output effect world =
        match output with
        | OutputSound (volume, sound) ->
            let world = World.enqueueAudioMessage (PlaySoundMessage { Volume = volume; Sound = sound }) world
            (effect, world)
        | OutputEmitter (name, emitter) ->
            let particleSystem = { effect.ParticleSystem_ with Emitters = Map.add name emitter effect.ParticleSystem_.Emitters }
            let effect = { effect with ParticleSystem_ = particleSystem }
            (effect, world)
        | Outputs outputs ->
            SArray.fold (fun (effect, world) output ->
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
                { Position = transform.Position
                  Scale = transform.Scale
                  Angles = transform.Angles
                  Elevation = transform.Elevation
                  Offset = effect.Offset_
                  Size = transform.Size
                  Inset = Box2.Zero
                  Color = Color.One
                  Blend = Transparent
                  Emission = Color.Zero
                  Height = Constants.Render.HeightDefault
                  Flip = FlipNone
                  Brightness = Constants.Render.BrightnessDefault
                  AttenuationLinear = Constants.Render.AttenuationLinearDefault
                  AttenuationQuadratic = Constants.Render.AttenuationQuadraticDefault
                  Cutoff = Constants.Render.CutoffDefault
                  Volume = Constants.Audio.SoundVolumeDefault
                  Enabled = true
                  Centered = effect.Centered_ }
            let effectSystem = EffectSystem.make localTime delta transform.Absolute transform.Presence effect.RenderType_ effect.Definitions_

            // evaluate effect with effect system
            let (view, _) = EffectSystem.eval effect.Descriptor_ effectSlice effect.History_ effectSystem

            // render effect view
            let world = World.renderView view world

            // convert view to array for storing tags and spawning emitters
            let views = View.toSeq view

            // extract tags
            let tags =
                views |>
                Seq.choose (function Nu.Tag (name, value) -> Some (name, value :?> Slice) | _ -> None) |>
                Map.ofSeq

            // spawn emitters
            let particleSystem =
                views |>
                Seq.choose (function SpawnEmitter (name, descriptor) -> Some (name, descriptor) | _ -> None) |>
                Seq.choose (fun (name : string, descriptor : EmitterDescriptor) ->
                    match descriptor with
                    | :? BasicSpriteEmitterDescriptor as descriptor ->
                        match World.tryMakeEmitter time descriptor.LifeTimeOpt descriptor.ParticleLifeTimeMaxOpt descriptor.ParticleRate descriptor.ParticleMax descriptor.Style world with
                        | Some (:? BasicStaticSpriteEmitter as emitter) ->
                            let emitter =
                                { emitter with
                                    Body = descriptor.Body
                                    Blend = descriptor.Blend
                                    Image = descriptor.Image
                                    ParticleSeed = descriptor.ParticleSeed
                                    Constraint = descriptor.Constraint }
                            Some (name, emitter :> Emitter)
                        | _ -> None
                    | :? BasicBillboardEmitterDescriptor as descriptor ->
                        match World.tryMakeEmitter time descriptor.LifeTimeOpt descriptor.ParticleLifeTimeMaxOpt descriptor.ParticleRate descriptor.ParticleMax descriptor.Style world with
                        | Some (:? BasicStaticBillboardEmitter as emitter) ->
                            let emitter =
                                { emitter with
                                    Body = descriptor.Body
                                    AlbedoImage = descriptor.AlbedoImage
                                    MetallicImage = descriptor.MetallicImage
                                    RoughnessImage = descriptor.RoughnessImage
                                    AmbientOcclusionImage = descriptor.AmbientOcclusionImage
                                    EmissionImage = descriptor.EmissionImage
                                    NormalImage = descriptor.NormalImage
                                    HeightImage = descriptor.HeightImage
                                    RenderType = emitter.RenderType
                                    ParticleSeed = descriptor.ParticleSeed
                                    Constraint = descriptor.Constraint }
                            Some (name, emitter :> Emitter)
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
            let descriptors = ParticleSystem.toParticlesDescriptors time particleSystem
            let world =
                List.fold (fun world descriptor ->
                    match descriptor with
                    | SpriteParticlesDescriptor descriptor ->
                        let message =
                            { Elevation = descriptor.Elevation
                              Horizon = descriptor.Horizon
                              AssetTag = AssetTag.generalize descriptor.Image
                              RenderOperation2d = RenderSpriteParticles descriptor }
                        World.enqueueLayeredOperation2d message world
                    | BillboardParticlesDescriptor descriptor ->
                        let message =
                            RenderBillboardParticles
                                { Absolute = descriptor.Absolute
                                  MaterialProperties = descriptor.MaterialProperties
                                  AlbedoImage = descriptor.AlbedoImage
                                  MetallicImage = descriptor.MetallicImage
                                  RoughnessImage = descriptor.RoughnessImage
                                  AmbientOcclusionImage = descriptor.AmbientOcclusionImage
                                  EmissionImage = descriptor.EmissionImage
                                  NormalImage = descriptor.NormalImage
                                  HeightImage = descriptor.HeightImage
                                  MinFilterOpt = descriptor.MinFilterOpt
                                  MagFilterOpt = descriptor.MagFilterOpt
                                  RenderType = descriptor.RenderType
                                  Particles = descriptor.Particles }
                        World.enqueueRenderMessage3d message world)
                    world descriptors

            // fin
            (Live, effect, world)

        // dead
        | Dead -> (Dead, effect, world)

    /// Make an effect.
    let makePlus startTime centered offset transform renderType particleSystem historyMax history definitions descriptor =
        { StartTime_ = startTime
          Centered_ = centered
          Offset_ = offset
          Transform_ = transform
          RenderType_ = renderType
          ParticleSystem_ = particleSystem
          HistoryMax_ = historyMax
          History_ = history
          Definitions_ = definitions
          Tags_ = Map.empty
          Descriptor_ = descriptor }

    /// Make an effect.
    let make startTime offset transform renderType descriptor =
        makePlus startTime true offset transform renderType ParticleSystem.empty Constants.Effects.EffectHistoryMaxDefault (Deque ()) Map.empty descriptor

    /// The empty effect.
    let empty =
        make GameTime.zero v3Zero (Transform.makeEmpty ()) DeferredRenderType EffectDescriptor.empty

type Effect = Effect.Effect