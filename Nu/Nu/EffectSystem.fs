// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open System.Collections.Generic
open Prime
open OpenTK
open Effects

/// The artifacts produced by an effect.
type [<NoEquality; NoComparison>] EffectArtifacts =
    { RenderArtifacts : RenderArtifact List
      SoundArtifacts : SoundArtifact List
      TagArtifacts : TagArtifact List }

[<AutoOpen>]
module EffectSystemModule =

    /// An abstract data type for executing effects.
    type [<NoEquality; NoComparison>] EffectSystem =
        private
            { ViewType : ViewType
              Artifacts : EffectArtifacts
              History : Slice seq
              ProgressOffset : single
              EffectTime : int64
              EffectEnv : Definitions
              Chaos : System.Random }

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module EffectSystem =

        let rec private addRenderArtifact artifact effectSystem =
            effectSystem.Artifacts.RenderArtifacts.Add artifact
            effectSystem

        let rec private addSoundArtifact artifact effectSystem =
            effectSystem.Artifacts.SoundArtifacts.Add artifact
            effectSystem

        let rec private addTagArtifact artifact effectSystem =
            effectSystem.Artifacts.TagArtifacts.Add artifact
            effectSystem

        let rec private selectKeyFrames2<'kf when 'kf :> KeyFrame> localTime playback (keyFrames : 'kf array) =
            match playback with
            | Once ->
                match keyFrames with
                | [||] -> failwithumf ()
                | [|head|] -> (localTime, head, head)
                | _ ->
                    let (head, next, tail) = (keyFrames.[0], keyFrames.[1], Array.skip 2 keyFrames)
                    if localTime > head.KeyFrameLength then
                        match tail with
                        | [||] -> (head.KeyFrameLength, next, next)
                        | _ -> selectKeyFrames2 (localTime - head.KeyFrameLength) playback (Array.cons next tail)
                    else (localTime, head, next)
            | Loop ->
                let totalTime = Array.fold (fun totalTime (keyFrame : 'kf) -> totalTime + keyFrame.KeyFrameLength) 0L keyFrames 
                if totalTime <> 0L then
                    let moduloTime = localTime % totalTime
                    selectKeyFrames2 moduloTime Once keyFrames
                else (0L, Array.head keyFrames, Array.head keyFrames)
            | Bounce ->
                let totalTime = Array.fold (fun totalTime (keyFrame : 'kf) -> totalTime + keyFrame.KeyFrameLength) 0L keyFrames
                if totalTime <> 0L then
                    let moduloTime = localTime % totalTime
                    let bouncing = localTime / totalTime % 2L = 1L
                    let bounceTime = if bouncing then totalTime - moduloTime else moduloTime
                    selectKeyFrames2 bounceTime Once keyFrames
                else (0L, Array.head keyFrames, Array.head keyFrames)

        let private selectKeyFrames<'kf when 'kf :> KeyFrame> localTime playback (keyFrames : 'kf array) =
            keyFrames |>
            selectKeyFrames2 localTime playback |>
            fun (fst, snd, thd) -> (fst, snd, thd)

        let inline private tween (scale : (^a * single) -> ^a) (value : ^a) (value2 : ^a) progress algorithm effectSystem =
            match algorithm with
            | Const ->
                value2
            | Linear ->
                value + scale (value2 - value, progress)
            | Random ->
                let rand = Rand.makeFromInt (int ((Math.Max (double progress, 0.000000001)) * double Int32.MaxValue))
                let randValue = fst (Rand.nextSingle rand)
                value + scale (value2 - value, randValue)
            | Chaos ->
                let chaosValue = single (effectSystem.Chaos.NextDouble ())
                value + scale (value2 - value, chaosValue)
            | Ease ->
                let progressEase = single (Math.Pow (Math.Sin (Math.PI * double progress * 0.5), 2.0))
                value + scale (value2 - value, progressEase)
            | EaseIn ->
                let progressScaled = float progress * Math.PI * 0.5
                let progressEaseIn = 1.0 + Math.Sin (progressScaled + Math.PI * 1.5)
                value + scale (value2 - value, single progressEaseIn)
            | EaseOut ->
                let progressScaled = float progress * Math.PI * 0.5
                let progressEaseOut = Math.Sin progressScaled
                value + scale (value2 - value, single progressEaseOut)
            | Sin ->
                let progressScaled = float progress * Math.PI * 2.0
                let progressSin = Math.Sin progressScaled
                value + scale (value2 - value, single progressSin)
            | Cos ->
                let progressScaled = float progress * Math.PI * 2.0
                let progressCos = Math.Cos progressScaled
                value + scale (value2 - value, single progressCos)

        let private applyLogic value value2 applicator =
            match applicator with
            | Or -> value || value2
            | Nor -> not value && not value2
            | Xor -> value <> value2
            | And -> value && value2
            | Nand -> not (value && value2)
            | Equal -> value2

        let inline private applyTween mul div (value : ^a) (value2 : ^a) applicator =
            match applicator with
            | Sum -> value + value2
            | Delta -> value - value2
            | Scale -> mul (value, value2)
            | Ratio -> div (value, value2)
            | Set -> value2

        let private evalInsetOpt (celSize : Vector2i) celRun celCount delay effectSystem =
            if delay <> 0L && celRun <> 0 then
                let cel = int (effectSystem.EffectTime / delay) % celCount
                let celI = cel % celRun
                let celJ = cel / celRun
                let celX = celI * celSize.X
                let celY = celJ * celSize.Y
                let celPosition = Vector2 (single celX, single celY)
                let celSize = Vector2 (single celSize.X, single celSize.Y)
                Some (Math.makeBounds celPosition celSize)
            else None

        let evalArgument (argument : Argument) : Definition =
            match argument with
            | SymbolicCompressionA resource ->
                { DefinitionParams = [||]; DefinitionBody = SymbolicCompressionA resource }
            | SymbolicCompressionB (SymbolicCompressionA aspect) ->
                { DefinitionParams = [||]; DefinitionBody = SymbolicCompressionB (SymbolicCompressionA aspect) }
            | SymbolicCompressionB (SymbolicCompressionB content) ->
                { DefinitionParams = [||]; DefinitionBody = SymbolicCompressionB (SymbolicCompressionB content) }

        let rec evalResource resource effectSystem : obj AssetTag =
            match resource with
            | Resource (packageName, assetName) -> AssetTag.make<obj> packageName assetName
            | Resource.Expand (definitionName, _) ->
                match Map.tryFind definitionName effectSystem.EffectEnv with
                | Some definition ->
                    match definition.DefinitionBody with
                    | SymbolicCompressionA resource -> evalResource resource effectSystem
                    | _ ->
                        Log.info ("Expected Resource for definition '" + definitionName + ".")
                        scvalue<obj AssetTag> Assets.DefaultImageString
                | None ->
                    Log.info ("Could not find definition with name '" + definitionName + "'.")
                    scvalue<obj AssetTag> Assets.DefaultImageString

        let rec private iterateArtifacts incrementAspects content slice effectSystem =
            let effectSystem = { effectSystem with ProgressOffset = 0.0f }
            let slice = evalAspects incrementAspects slice effectSystem
            (slice, evalContent content slice effectSystem)

        and private cycleArtifacts incrementAspects content slice effectSystem =
            let slice = evalAspects incrementAspects slice effectSystem
            evalContent content slice effectSystem

        and private evalProgress keyFrameTime keyFrameLength effectSystem =
            let progress = if keyFrameLength = 0L then 1.0f else single keyFrameTime / single keyFrameLength
            let progress = progress + effectSystem.ProgressOffset
            if progress > 1.0f then progress - 1.0f else progress

        and private evalAspect aspect slice effectSystem =
            match aspect with
            | Enabled (applicator, playback, keyFrames) ->
                if Array.notEmpty keyFrames then
                    let (_, keyFrame, _) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                    let applied = applyLogic slice.Enabled keyFrame.LogicValue applicator
                    { slice with Enabled = applied }
                else slice
            | Position (applicator, algorithm, playback, keyFrames) ->
                if Array.notEmpty keyFrames then
                    let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                    let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                    let tweened = tween Vector2.op_Multiply keyFrame.TweenValue keyFrame2.TweenValue progress algorithm effectSystem
                    let applied = applyTween Vector2.Multiply Vector2.Divide slice.Position tweened applicator
                    { slice with Position = applied }
                else slice
            | Translation (applicator, algorithm, playback, keyFrames) ->
                if Array.notEmpty keyFrames then
                    let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                    let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                    let tweened = tween Vector2.op_Multiply keyFrame.TweenValue keyFrame2.TweenValue progress algorithm effectSystem
                    let oriented = Vector2.Transform (tweened, Quaternion.FromAxisAngle (Vector3.UnitZ, slice.Rotation))
                    let applied = applyTween Vector2.Multiply Vector2.Divide slice.Position oriented applicator
                    { slice with Position = applied }
                else slice
            | Size (applicator, algorithm, playback, keyFrames) ->
                if Array.notEmpty keyFrames then
                    let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                    let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                    let tweened = tween Vector2.op_Multiply keyFrame.TweenValue keyFrame2.TweenValue progress algorithm effectSystem
                    let applied = applyTween Vector2.Multiply Vector2.Divide slice.Size tweened applicator
                    { slice with Size = applied }
                else slice
            | Rotation (applicator, algorithm, playback, keyFrames) ->
                if Array.notEmpty keyFrames then
                    let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                    let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                    let tweened = tween (fun (x, y) -> x * y) keyFrame.TweenValue keyFrame2.TweenValue progress algorithm effectSystem
                    let applied = applyTween (fun (x, y) -> x * y) (fun (x, y) -> x / y) slice.Rotation tweened applicator
                    { slice with Rotation = applied }
                else slice
            | Depth (applicator, algorithm, playback, keyFrames) ->
                if Array.notEmpty keyFrames then
                    let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                    let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                    let tweened = tween (fun (x, y) -> x * y) keyFrame.TweenValue keyFrame2.TweenValue progress algorithm effectSystem
                    let applied = applyTween (fun (x, y) -> x * y) (fun (x, y) -> x / y) slice.Depth tweened applicator
                    { slice with Depth = applied }
                else slice
            | Offset (applicator, algorithm, playback, keyFrames) ->
                if Array.notEmpty keyFrames then
                    let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                    let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                    let tweened = tween Vector2.op_Multiply keyFrame.TweenValue keyFrame2.TweenValue progress algorithm effectSystem
                    let applied = applyTween Vector2.Multiply Vector2.Divide slice.Size tweened applicator
                    { slice with Offset = applied }
                else slice
            | Color (applicator, algorithm, playback, keyFrames) ->
                if Array.notEmpty keyFrames then
                    let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                    let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                    let tweened = tween Vector4.op_Multiply keyFrame.TweenValue keyFrame2.TweenValue progress algorithm effectSystem
                    let applied = applyTween Vector4.Multiply Vector4.Divide slice.Color tweened applicator
                    { slice with Color = applied }
                else slice
            | Volume (applicator, algorithm, playback, keyFrames) ->
                if Array.notEmpty keyFrames then
                    let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                    let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                    let tweened = tween (fun (x, y) -> x * y) keyFrame.TweenValue keyFrame2.TweenValue progress algorithm effectSystem
                    let applied = applyTween (fun (x, y) -> x * y) (fun (x, y) -> x / y) slice.Volume tweened applicator
                    { slice with Volume = applied }
                else slice
            | Bone ->
                slice
            | Aspect.Expand (definitionName, _) ->
                match Map.tryFind definitionName effectSystem.EffectEnv with
                | Some definition ->
                    match definition.DefinitionBody with
                    | SymbolicCompressionB (SymbolicCompressionA aspect) -> evalAspect aspect slice effectSystem
                    | _ -> Log.info ("Expected Aspect for definition '" + definitionName + "'."); slice
                | None -> Log.info ("Could not find definition with name '" + definitionName + "'."); slice
        
        and private evalAspects aspects slice effectSystem =
            Array.fold (fun slice aspect -> evalAspect aspect slice effectSystem) slice aspects

        and private evalExpand definitionName arguments slice effectSystem =
            match Map.tryFind definitionName effectSystem.EffectEnv with
            | Some definition ->
                match definition.DefinitionBody with
                |  SymbolicCompressionB (SymbolicCompressionB content) ->
                    let localDefinitions = Array.map evalArgument arguments
                    match (try Array.zip definition.DefinitionParams localDefinitions |> Some with _ -> None) with
                    | Some localDefinitionEntries ->
                        let effectSystem = { effectSystem with EffectEnv = Map.addMany localDefinitionEntries effectSystem.EffectEnv }
                        evalContent content slice effectSystem
                    | None -> Log.info "Wrong number of arguments provided to ExpandContent."; effectSystem
                | _ -> Log.info ("Expected Content for definition '" + definitionName + "'."); effectSystem
            | None -> Log.info ("Could not find definition with name '" + definitionName + "'."); effectSystem

        and private evalStaticSprite resource aspects content slice effectSystem =

            // pull image from resource
            let image = evalResource resource effectSystem

            // eval aspects
            let slice = evalAspects aspects slice effectSystem

            // build sprite artifacts
            let effectSystem =
                if slice.Enabled then
                    let spriteArtifact =
                        RenderArtifact
                            (LayerableDescriptor
                                { Depth = slice.Depth
                                  PositionY = slice.Position.Y
                                  LayeredDescriptor =
                                    SpriteDescriptor 
                                        { Position = slice.Position
                                          Size = slice.Size
                                          Rotation = slice.Rotation
                                          Offset = slice.Offset
                                          InsetOpt = None
                                          Image = AssetTag.specialize<Image> image
                                          ViewType = effectSystem.ViewType
                                          Color = slice.Color }})
                    addRenderArtifact spriteArtifact effectSystem
                else effectSystem

            // build implicitly mounted content
            evalContent content slice effectSystem

        and private evalAnimatedSprite resource celSize celRun celCount delay aspects content slice effectSystem =

            // pull image from resource
            let image = evalResource resource effectSystem

            // eval aspects
            let slice = evalAspects aspects slice effectSystem

            // eval inset
            let insetOpt = evalInsetOpt celSize celRun celCount delay effectSystem

            // build animated sprite artifacts
            let effectSystem =
                if slice.Enabled then
                    let animatedSpriteArtifact =
                        RenderArtifact
                            (LayerableDescriptor
                                { Depth = slice.Depth
                                  PositionY = slice.Position.Y
                                  LayeredDescriptor =
                                  SpriteDescriptor 
                                    { Position = slice.Position
                                      Size = slice.Size
                                      Rotation = slice.Rotation
                                      Offset = slice.Offset
                                      InsetOpt = insetOpt
                                      Image = AssetTag.specialize<Image> image
                                      ViewType = effectSystem.ViewType
                                      Color = slice.Color }})
                    addRenderArtifact animatedSpriteArtifact effectSystem
                else effectSystem

            // build implicitly mounted content
            evalContent content slice effectSystem

        and private evalSoundEffect resource aspects content slice effectSystem =

            // pull sound from resource
            let sound = evalResource resource effectSystem

            // eval aspects
            let slice = evalAspects aspects slice effectSystem

            // build sprite artifacts
            let effectSystem =
                if slice.Enabled
                then addSoundArtifact (SoundArtifact (slice.Volume, AssetTag.specialize<Audio> sound)) effectSystem
                else effectSystem

            // build implicitly mounted content
            evalContent content slice effectSystem

        and private evalMount shift aspects content (slice : Slice) effectSystem =
            let slice = { slice with Depth = slice.Depth + shift }
            let slice = evalAspects aspects slice effectSystem
            evalContent content slice effectSystem

        and private evalRepeat shift repetition incrementAspects content (slice : Slice) effectSystem =
        
            // eval repeat either as iterative or cycling
            let slice = { slice with Depth = slice.Depth + shift }
            match repetition with
        
            // eval iterative repeat
            | Iterate count ->
                Array.fold
                    (fun (slice, effectSystem) _ ->
                        let (slice, effectSystem) = iterateArtifacts incrementAspects content slice effectSystem
                        (slice, effectSystem))
                    (slice, effectSystem)
                    [|0 .. count - 1|] |>
                snd

            // eval cycling repeat
            | Cycle count ->
                Array.fold
                    (fun effectSystem i ->
                        let effectSystem = { effectSystem with ProgressOffset = 1.0f / single count * single i }
                        cycleArtifacts incrementAspects content slice effectSystem)
                    effectSystem
                    [|0 .. count - 1|]

        and private evalEmit shift rate emitterAspects aspects content effectSystem =
            let effectSystem =
                Seq.foldi
                    (fun i effectSystem (slice : Slice) ->
                        let oldHistory = effectSystem.History
                        let oldEffectTime = effectSystem.EffectTime
                        let timePassed = int64 i
                        let slice = { slice with Depth = slice.Depth + shift }
                        let slice = evalAspects emitterAspects slice { effectSystem with EffectTime = effectSystem.EffectTime - timePassed }
                        let emitCountLastFrame = single (effectSystem.EffectTime - timePassed - 1L) * rate
                        let emitCountThisFrame = single (effectSystem.EffectTime - timePassed) * rate
                        let emitCount = int emitCountThisFrame - int emitCountLastFrame
                        let effectSystem = { effectSystem with EffectTime = timePassed }
                        let effectSystem =
                            Array.fold
                                (fun effectSystem _ ->
                                    let slice = evalAspects aspects slice effectSystem
                                    if slice.Enabled
                                    then evalContent content slice effectSystem
                                    else effectSystem)
                                effectSystem
                                [|0 .. emitCount - 1|]
                        { effectSystem with History = oldHistory; EffectTime = oldEffectTime })
                    effectSystem
                    effectSystem.History
            effectSystem

        and private evalComposite shift contents slice effectSystem =
            let slice = { slice with Slice.Depth = slice.Depth + shift }
            evalContents contents slice effectSystem

        and private evalContent content slice effectSystem =
            match content with
            | Nil ->
                effectSystem
            | Tag (name, metadata) ->
                addTagArtifact (TagArtifact (name, metadata, slice)) effectSystem
            | StaticSprite (resource, aspects, content) ->
                evalStaticSprite resource aspects content slice effectSystem
            | AnimatedSprite (resource, celSize, celRun, celCount, delay, aspects, content) ->
                evalAnimatedSprite resource celSize celRun celCount delay aspects content slice effectSystem
            | SoundEffect (resource, aspects, content) ->
                evalSoundEffect resource aspects content slice effectSystem
            | Mount (Shift shift, aspects, content) ->
                evalMount shift aspects content slice effectSystem
            | Repeat (Shift shift, repetition, incrementAspects, content) ->
                evalRepeat shift repetition incrementAspects content slice effectSystem
            | Emit (Shift shift, Rate rate, emitterAspects, aspects, content) ->
                evalEmit shift rate emitterAspects aspects content effectSystem
            | Composite (Shift shift, contents) ->
                evalComposite shift contents slice effectSystem
            | Expand (definitionName, arguments) ->
                evalExpand definitionName arguments slice effectSystem

        and private evalContents contents slice effectSystem =
            Array.fold
                (fun effectSystem content -> evalContent content slice effectSystem)
                effectSystem
                contents

        let private release effectSystem =
            let artifacts = effectSystem.Artifacts
            let effectSystem =
                { effectSystem with
                    Artifacts =
                        { RenderArtifacts = List<RenderArtifact> ()
                          SoundArtifacts = List<SoundArtifact> ()
                          TagArtifacts = List<TagArtifact> () }}
            (artifacts, effectSystem)

        let eval effect slice effectSystem =
            let alive =
                match effect.LifetimeOpt with
                | Some lifetime -> lifetime <= 0L || effectSystem.EffectTime <= lifetime
                | None -> true
            if alive then
                let effectSystem = { effectSystem with EffectEnv = Map.concat effectSystem.EffectEnv effect.Definitions }
                try let effectSystem = evalContent effect.Content slice effectSystem
                    release effectSystem
                with exn ->
                    let prettyPrinter = (SyntaxAttribute.getOrDefault typeof<Effect>).PrettyPrinter
                    let effectStr = PrettyPrinter.prettyPrint (scstring effect) prettyPrinter
                    Log.debug ("Error in effect:\n" + effectStr + "\n due to: " + scstring exn)
                    release effectSystem
            else release effectSystem

        let combineEffects effects =
            let effectCombined =
                { EffectName = String.concat "+" (List.map (fun effect -> effect.EffectName) effects)
                  LifetimeOpt = None
                  Definitions = List.fold (fun definitions effect -> Map.concat definitions effect.Definitions) Map.empty effects
                  Content = Composite (Shift 0.0f, effects |> List.map (fun effect -> effect.Content) |> Array.ofList) }
            effectCombined

        let make viewType history effectTime globalEnv = 
            let artifacts =
                { RenderArtifacts = List<RenderArtifact> ()
                  SoundArtifacts = List<SoundArtifact> ()
                  TagArtifacts = List<TagArtifact> () }
            { ViewType = viewType
              Artifacts = artifacts
              History = history
              ProgressOffset = 0.0f
              EffectTime = effectTime
              EffectEnv = globalEnv
              Chaos = System.Random () }

/// An abstract data type for executing effects.
type EffectSystem = EffectSystemModule.EffectSystem