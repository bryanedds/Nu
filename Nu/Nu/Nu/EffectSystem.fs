// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open Prime
open OpenTK

[<AutoOpen>]
module EffectSystemModule =

    /// An abstract data type for executing Effects.
    type [<NoEquality; NoComparison>] EffectSystem =
        private
            { ViewType : ViewType
              History : Slice seq
              ProgressOffset : single
              EffectRate : int64
              EffectTime : int64
              EffectEnv : Definitions
              Chaos : System.Random }

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module EffectSystem =
    
        let rec private selectKeyFrames2 localTime playback (keyFrames : IKeyFrame list) =
            match playback with
            | Once ->
                match keyFrames with
                | [] -> failwithumf ()
                | head :: [] -> (localTime, head, head)
                | head :: next :: tail ->
                    if localTime > head.KeyFrameLength then
                        match tail with
                        | _ :: _ -> selectKeyFrames2 (localTime - head.KeyFrameLength) playback (next :: tail)
                        | [] -> (head.KeyFrameLength, next, next)
                    else (localTime, head, next)
            | Loop ->
                let totalTime = List.fold (fun totalTime (keyFrame : IKeyFrame) -> totalTime + keyFrame.KeyFrameLength) 0L keyFrames 
                let moduloTime = localTime % totalTime
                selectKeyFrames2 moduloTime Once keyFrames
            | Bounce ->
                let totalTime = List.fold (fun totalTime (keyFrame : IKeyFrame) -> totalTime + keyFrame.KeyFrameLength) 0L keyFrames 
                let moduloTime = localTime % totalTime
                let bouncing = localTime / totalTime % 2L = 1L
                let bounceTime = if bouncing then totalTime - moduloTime else moduloTime
                selectKeyFrames2 bounceTime Once keyFrames
    
        let private selectKeyFrames<'n when 'n :> IKeyFrame> localTime playback (keyFrames : 'n list) =
            keyFrames |>
            List.map (fun keyFrame -> keyFrame :> IKeyFrame) |>
            selectKeyFrames2 localTime playback |>
            fun (fst, snd, thd) -> (fst, snd :?> 'n, thd :?> 'n)
    
        let inline private tween (scale : (^a * single) -> ^a) (value : ^a) (value2 : ^a) progress algorithm effectSystem =
            match algorithm with
            | Const ->
                value2
            | Linear ->
                value + scale (value2 - value, progress)
            | Random ->
                let rand = Rand.makeFromInt ^ int ^ (Math.Max (double progress, 0.000000001)) * double Int32.MaxValue
                let randValue = fst ^ Rand.nextSingle rand
                value + scale (value2 - value, randValue)
            | Chaos ->
                let chaosValue = single ^ effectSystem.Chaos.NextDouble ()
                value + scale (value2 - value, chaosValue)
            | Ease ->
                let progressEase = single ^ Math.Pow (Math.Sin (Math.PI * double progress * 0.5), 2.0)
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
            | LogicApplicator.Eq -> value2
    
        let inline private applyTween mul div (value : ^a) (value2 : ^a) applicator =
            match applicator with
            | Add -> value + value2
            | Sub -> value - value2
            | Mul -> mul (value, value2)
            | Div -> div (value, value2)
            | TweenApplicator.Eq -> value2
    
        let private evalInset (celSize : Vector2i) celRun celCount stutter effectSystem =
            let cel = int (effectSystem.EffectTime / stutter) % celCount
            let celI = cel % celRun
            let celJ = cel / celRun
            let celX = celI * celSize.X
            let celY = celJ * celSize.Y
            let celPosition = Vector2 (single celX, single celY)
            let celSize = Vector2 (single celSize.X, single celSize.Y)
            Math.makeBounds celPosition celSize
    
        let evalArgument (argument : Argument) : Definition =
            match argument with
            | SymbolicCompressionA resource ->
                { DefinitionParams = []; DefinitionBody = SymbolicCompressionA resource }
            | SymbolicCompressionB (SymbolicCompressionA aspect) ->
                { DefinitionParams = []; DefinitionBody = SymbolicCompressionB (SymbolicCompressionA aspect) }
            | SymbolicCompressionB (SymbolicCompressionB content) ->
                { DefinitionParams = []; DefinitionBody = SymbolicCompressionB (SymbolicCompressionB content) }
    
        let rec evalResource resource effectSystem : AssetTag =
            match resource with
            | Resource.Expand (definitionName, _) ->
                match Map.tryFind definitionName effectSystem.EffectEnv with
                | Some definition ->
                    match definition.DefinitionBody with
                    | SymbolicCompressionA resource -> evalResource resource effectSystem
                    | _ ->
                        Log.info ^ "Expected Resource for definition '" + definitionName + "."
                        scvalue<AssetTag> Constants.Assets.DefaultImageValue
                | None ->
                    Log.info ^ "Could not find definition with name '" + definitionName + "'."
                    scvalue<AssetTag> Constants.Assets.DefaultImageValue
            | Resource (packageName, assetName) -> { PackageName = packageName; AssetName = assetName }
    
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
            | Aspect.Expand (definitionName, _) ->
                match Map.tryFind definitionName effectSystem.EffectEnv with
                | Some definition ->
                    match definition.DefinitionBody with
                    | SymbolicCompressionB (SymbolicCompressionA aspect) -> evalAspect aspect slice effectSystem
                    | _ -> Log.info ^ "Expected Aspect for definition '" + definitionName + "'."; slice
                | None -> Log.info ^ "Could not find definition with name '" + definitionName + "'."; slice
            | Enabled (applicator, playback, keyFrames) ->
                let (_, keyFrame, _) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let applied = applyLogic slice.Enabled keyFrame.LogicValue applicator
                { slice with Enabled = applied }
            | Position (applicator, algorithm, playback, keyFrames) ->
                let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                let tweened = tween Vector2.op_Multiply keyFrame.TweenValue keyFrame2.TweenValue progress algorithm effectSystem
                let applied = applyTween Vector2.Multiply Vector2.Divide slice.Position tweened applicator
                { slice with Position = applied }
            | Translation (applicator, algorithm, playback, keyFrames) ->
                let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                let tweened = tween Vector2.op_Multiply keyFrame.TweenValue keyFrame2.TweenValue progress algorithm effectSystem
                let oriented = Vector2.Transform (tweened, Quaternion.FromAxisAngle (Vector3.UnitZ, slice.Rotation))
                let applied = applyTween Vector2.Multiply Vector2.Divide slice.Position oriented applicator
                { slice with Position = applied }
            | Size (applicator, algorithm, playback, keyFrames) ->
                let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                let tweened = tween Vector2.op_Multiply keyFrame.TweenValue keyFrame2.TweenValue progress algorithm effectSystem
                let applied = applyTween Vector2.Multiply Vector2.Divide slice.Size tweened applicator
                { slice with Size = applied }
            | Rotation (applicator, algorithm, playback, keyFrames) ->
                let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                let tweened = tween (fun (x, y) -> x * y) keyFrame.TweenValue keyFrame2.TweenValue progress algorithm effectSystem
                let applied = applyTween (fun (x, y) -> x * y) (fun (x, y) -> x / y) slice.Rotation tweened applicator
                { slice with Rotation = applied }
            | Depth (applicator, algorithm, playback, keyFrames) ->
                let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                let tweened = tween (fun (x, y) -> x * y) keyFrame.TweenValue keyFrame2.TweenValue progress algorithm effectSystem
                let applied = applyTween (fun (x, y) -> x * y) (fun (x, y) -> x / y) slice.Depth tweened applicator
                { slice with Depth = applied }
            | Offset (applicator, algorithm, playback, keyFrames) ->
                let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                let tweened = tween Vector2.op_Multiply keyFrame.TweenValue keyFrame2.TweenValue progress algorithm effectSystem
                let applied = applyTween Vector2.Multiply Vector2.Divide slice.Size tweened applicator
                { slice with Offset = applied }
            | Color (applicator, algorithm, playback, keyFrames) ->
                let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                let tweened = tween Vector4.op_Multiply keyFrame.TweenValue keyFrame2.TweenValue progress algorithm effectSystem
                let applied = applyTween Vector4.Multiply Vector4.Divide slice.Color tweened applicator
                { slice with Color = applied }
            | Volume (applicator, algorithm, playback, keyFrames) ->
                let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                let tweened = tween (fun (x, y) -> x * y) keyFrame.TweenValue keyFrame2.TweenValue progress algorithm effectSystem
                let applied = applyTween (fun (x, y) -> x * y) (fun (x, y) -> x / y) slice.Volume tweened applicator
                { slice with Volume = applied }
            | Bone -> slice
    
        and private evalAspects aspects slice effectSystem =
            List.fold (fun slice aspect -> evalAspect aspect slice effectSystem) slice aspects
    
        and private evalExpand definitionName arguments slice effectSystem =
            match Map.tryFind definitionName effectSystem.EffectEnv with
            | Some definition ->
                match definition.DefinitionBody with
                |  SymbolicCompressionB (SymbolicCompressionB content) ->
                    let localDefinitions = List.map evalArgument arguments
                    match (try List.zip definition.DefinitionParams localDefinitions |> Some with _ -> None) with
                    | Some localDefinitionEntries ->
                        let effectSystem = { effectSystem with EffectEnv = Map.addMany localDefinitionEntries effectSystem.EffectEnv }
                        evalContent content slice effectSystem
                    | None -> Log.info "Wrong number of arguments provided to ExpandContent."; []
                | _ -> Log.info ^ "Expected Content for definition '" + definitionName + "'."; []
            | None -> Log.info ^ "Could not find definition with name '" + definitionName + "'."; []
    
        and private evalStaticSprite resource aspects content slice effectSystem =
    
            // pull image from resource
            let image = evalResource resource effectSystem
    
            // eval aspects
            let slice = evalAspects aspects slice effectSystem
    
            // build sprite artifacts
            let spriteArtifacts =
                if slice.Enabled then
                    [RenderArtifact
                        [LayerableDescriptor
                            { Depth = slice.Depth
                              LayeredDescriptor =
                                SpriteDescriptor 
                                    { Position = slice.Position
                                      Size = slice.Size
                                      Rotation = slice.Rotation
                                      Offset = slice.Offset
                                      OptInset = None
                                      Image = image
                                      ViewType = effectSystem.ViewType
                                      Color = slice.Color }}]]
                else []
    
            // build implicitly mounted content
            let mountedArtifacts = evalContent content slice effectSystem
    
            // return artifacts
            mountedArtifacts @ spriteArtifacts
    
        and private evalAnimatedSprite resource celSize celRun celCount stutter aspects content slice effectSystem =
    
            // pull image from resource
            let image = evalResource resource effectSystem
    
            // eval aspects
            let slice = evalAspects aspects slice effectSystem
    
            // eval inset
            let inset = evalInset celSize celRun celCount stutter effectSystem
    
            // build animated sprite artifacts
            let animatedSpriteArtifacts =
                if slice.Enabled then
                    [RenderArtifact
                        [LayerableDescriptor
                            { Depth = slice.Depth
                              LayeredDescriptor =
                                SpriteDescriptor 
                                    { Position = slice.Position
                                      Size = slice.Size
                                      Rotation = slice.Rotation
                                      Offset = slice.Offset
                                      OptInset = Some inset
                                      Image = image
                                      ViewType = effectSystem.ViewType
                                      Color = slice.Color }}]]
                else []
    
            // build implicitly mounted content
            let mountedArtifacts = evalContent content slice effectSystem
    
            // return artifacts
            mountedArtifacts @ animatedSpriteArtifacts
    
        and private evalSoundEffect resource aspects content slice effectSystem =
    
            // pull sound from resource
            let sound = evalResource resource effectSystem
    
            // eval aspects
            let slice = evalAspects aspects slice effectSystem
    
            // build sprite artifacts
            let soundArtifacts =
                if slice.Enabled
                then [SoundArtifact (slice.Volume, sound)]
                else []
    
            // build implicitly mounted content
            let mountedArtifacts = evalContent content slice effectSystem
    
            // return artifacts
            mountedArtifacts @ soundArtifacts
    
        and private evalMount shift aspects content (slice : Slice) effectSystem =
            let slice = { slice with Depth = slice.Depth + shift }
            let slice = evalAspects aspects slice effectSystem
            evalContent content slice effectSystem
    
        and private evalRepeat shift repetition incrementAspects content (slice : Slice) effectSystem =
            
            // eval repeat either as iterative or cylcing
            let slice = { slice with Depth = slice.Depth + shift }
            match repetition with
            
            // eval iterative repeat
            | Iterate count ->
                List.fold
                    (fun (slice, artifacts) _ ->
                        let (slice, artifacts') = iterateArtifacts incrementAspects content slice effectSystem
                        (slice, artifacts @ artifacts'))
                    (slice, [])
                    [0 .. count - 1] |>
                snd
    
            // eval cycling repeat
            | Cycle count ->
                List.fold
                    (fun artifacts i ->
                        let effectSystem = { effectSystem with ProgressOffset = 1.0f / single count * single i }
                        let artifacts' = cycleArtifacts incrementAspects content slice effectSystem
                        artifacts @ artifacts')
                    [] [0 .. count - 1]
    
        and private evalEmit shift rate emitterAspects aspects content effectSystem =
            let artifacts =
                Seq.foldi
                    (fun i artifacts (slice : Slice) ->
                        let timePassed = int64 i * effectSystem.EffectRate
                        let slice = { slice with Depth = slice.Depth + shift }
                        let slice = evalAspects emitterAspects slice { effectSystem with EffectTime = effectSystem.EffectTime - timePassed }
                        let emitCountLastFrame = single (effectSystem.EffectTime - timePassed - effectSystem.EffectRate) * rate
                        let emitCountThisFrame = single (effectSystem.EffectTime - timePassed) * rate
                        let emitCount = int emitCountThisFrame - int emitCountLastFrame
                        let effectSystem =
                            let history =
                                // TODO: emits on emits is broken, so fix this!
                                match content with
                                | Emit _ ->
                                    Seq.mapi
                                        (fun j slice ->
                                            let timePassed = int64 (i + j) * effectSystem.EffectRate
                                            evalAspects emitterAspects slice { effectSystem with EffectTime = effectSystem.EffectTime - timePassed })
                                        effectSystem.History
                                | _ -> effectSystem.History
                            { effectSystem with History = history; EffectTime = timePassed }
                        let artifacts' =
                            List.fold
                                (fun artifacts' _ ->
                                    let slice = evalAspects aspects slice effectSystem
                                    let artifacts'' = if slice.Enabled then evalContent content slice effectSystem else []
                                    artifacts'' @ artifacts')
                                []
                                [0 .. emitCount - 1]
                        artifacts' @ artifacts)
                    []
                    effectSystem.History
            artifacts
    
        and private evalComposite shift contents (slice : Slice) effectSystem =
            let slice = { slice with Depth = slice.Depth + shift }
            evalContents contents slice effectSystem
    
        and private evalContent content slice effectSystem =
            match content with
            | Expand (definitionName, arguments) ->
                evalExpand definitionName arguments slice effectSystem
            | StaticSprite (resource, aspects, content) ->
                evalStaticSprite resource aspects content slice effectSystem
            | AnimatedSprite (resource, celSize, celRun, celCount, stutter, aspects, content) ->
                evalAnimatedSprite resource celSize celRun celCount stutter aspects content slice effectSystem
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
            | Tag (name, metadata) ->
                [TagArtifact (name, metadata, slice)]
            | Nil -> []
    
        and private evalContents contents slice effectSystem =
            List.fold
                (fun artifacts content ->
                    let artifacts' = evalContent content slice effectSystem
                    artifacts' @ artifacts)
                []
                contents
    
        let eval (Effect (_, optLifetime, definitions, content)) slice effectSystem =
            let localTime =
                match optLifetime with
                | Some lifetime -> effectSystem.EffectTime % lifetime
                | None -> effectSystem.EffectTime
            let effectSystem =
                { effectSystem with
                    EffectEnv = Map.concat effectSystem.EffectEnv definitions
                    EffectTime = localTime }
            evalContent content slice effectSystem
    
        let make viewType history tickRate tickTime globalEnv = 
            { ViewType = viewType
              History = history
              ProgressOffset = 0.0f
              EffectRate = tickRate
              EffectTime = tickTime
              EffectEnv = globalEnv
              Chaos = System.Random () }