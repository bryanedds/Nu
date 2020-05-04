// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open Prime
module Effects =

    type Algorithm =
        | Const
        | Linear
        | Random
        | Chaos
        | Ease
        | EaseIn
        | EaseOut
        | Sin
        | Cos

    type LogicApplicator =
        | Or
        | Nor
        | Xor
        | And
        | Nand
        | Equal

    type TweenApplicator =
        | Sum
        | Delta
        | Scale
        | Ratio
        | Set

    type [<NoComparison>] Slice =
        { Position : Vector2
          Size : Vector2
          Rotation : single
          Depth : single
          Offset : Vector2
          InsetOpt : Vector4 option
          Color : Vector4
          Glow : Vector4
          Text : string
          Volume : single
          Enabled : bool }

    type KeyFrame =
        abstract KeyFrameLength : int64

    type LogicKeyFrame =
        { LogicValue : bool
          LogicLength : int64 }
        interface KeyFrame with
            member this.KeyFrameLength = this.LogicLength

    type TweenKeyFrame =
        { TweenValue : single
          TweenLength : int64 }
        interface KeyFrame with
            member this.KeyFrameLength = this.TweenLength

    type [<NoComparison>] Tween2KeyFrame =
        { TweenValue : Vector2
          TweenLength : int64 }
        interface KeyFrame with
            member this.KeyFrameLength = this.TweenLength

    type [<NoComparison>] Tween3KeyFrame =
        { TweenValue : Vector3
          TweenLength : int64 }
        interface KeyFrame with
            member this.KeyFrameLength = this.TweenLength

    type [<NoComparison>] Tween4KeyFrame =
        { TweenValue : Vector4
          TweenLength : int64 }
        interface KeyFrame with
            member this.KeyFrameLength = this.TweenLength

    type TweenIKeyFrame =
        { TweenValue : int
          TweenLength : int64 }
        interface KeyFrame with
            member this.KeyFrameLength = this.TweenLength

    type Tween2IKeyFrame =
        { TweenValue : Vector2i
          TweenLength : int64 }
        interface KeyFrame with
            member this.KeyFrameLength = this.TweenLength
            
    type Playback =
        | Once
        | Loop
        | Bounce
        
    type Repetition =
        | Cycle of Cycles : int
        | Iterate of Iterations : int

    type Rate =
        Rate of single

    type Shift =
        Shift of single

    type [<NoComparison>] Resource =
        | Resource of string * string
        | Expand of string * Argument array

    and [<NoComparison>] Aspect =
        | Enabled of LogicApplicator * Playback * LogicKeyFrame array
        | Position of TweenApplicator * Algorithm * Playback * Tween2KeyFrame array
        | Translation of TweenApplicator * Algorithm * Playback * Tween2KeyFrame array
        | Offset of TweenApplicator * Algorithm * Playback * Tween2KeyFrame array
        | Size of TweenApplicator * Algorithm * Playback * Tween2KeyFrame array
        | Rotation of TweenApplicator * Algorithm * Playback * TweenKeyFrame array
        | Depth of TweenApplicator * Algorithm * Playback * TweenKeyFrame array
        | Inset of TweenApplicator * Algorithm * Playback * Tween4KeyFrame array
        | Color of TweenApplicator * Algorithm * Playback * Tween4KeyFrame array
        | Glow of TweenApplicator * Algorithm * Playback * Tween4KeyFrame array
        | Text of string
        | Volume of TweenApplicator * Algorithm * Playback * TweenKeyFrame array
        | Bone // TODO: implement bone aspect
        | Expand of string * Argument array

    and [<NoComparison>] Content =
        | Nil // first to make default value when missing
        | StaticSprite of Resource * Aspect array * Content
        | AnimatedSprite of Resource * Vector2i * int * int * int64 * Aspect array * Content
        | TextSprite of Resource * Aspect array * Content
        | SoundEffect of Resource * Aspect array * Content
        | Mount of Shift * Aspect array * Content
        | Repeat of Shift * Repetition * Aspect array * Content
        | Emit of Shift * Rate * Aspect array * Aspect array * Content
        | Composite of Shift * Content array
        | Expand of string * Argument array

    and Argument =
        SymbolicCompression<Resource, SymbolicCompression<Aspect, Content>>

    type [<NoComparison>] Definition =
        { DefinitionParams : string array
          DefinitionBody : SymbolicCompression<Resource, SymbolicCompression<Aspect, Content>> }

    type Definitions =
        Map<string, Definition>

/// Describes an effect in a compositional manner.
[<Syntax   ("Const Linear Random Chaos Ease EaseIn EaseOut Sin Cos " +
            "Or Nor Xor And Nand Equal " +
            "Sum Delta Scale Ratio Set " +
            "Position Size Rotation Depth Offset Color Volume Enabled " +
            "Once Loop Bounce " +
            "Cycle Iterate " +
            "Rate " +
            "Shift " +
            "Expand Resource " +
            "Expand Enabled Position Translation Offset Inset Size Rotation Depth Color Glow Volume Bone " +
            "Expand StaticSprite AnimatedSprite TextSprite SoundEffect Mount Repeat Emit Composite Tag Nil " +
            "View",
            "", "", "", "",
            Constants.PrettyPrinter.DefaultThresholdMin,
            Constants.PrettyPrinter.CompositionalThresholdMax)>]
type [<NoEquality; NoComparison>] Effect =
    { EffectName : string
      LifetimeOpt : int64 option
      Definitions : Effects.Definitions
      Content : Effects.Content }

    static member empty =
        { EffectName = Constants.Engine.DefaultEffectName
          LifetimeOpt = None
          Definitions = Map.empty
          Content = Effects.Composite (Effects.Shift 0.0f, [||]) }

[<AutoOpen>]
module EffectSystemModule =

    // effects
    open Effects

    /// An abstract data type for executing effects.
    type [<NoEquality; NoComparison>] EffectSystem =
        private
            { ViewType : ViewType
              Views : View List
              History : Slice seq
              ProgressOffset : single
              EffectTime : int64
              EffectEnv : Definitions
              Chaos : System.Random }

    [<RequireQualifiedAccess>]
    module EffectSystem =

        let rec private addView view effectSystem =
            effectSystem.Views.Add view
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
                value
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

        let rec private iterateViews incrementAspects content slice effectSystem =
            let effectSystem = { effectSystem with ProgressOffset = 0.0f }
            let slice = evalAspects incrementAspects slice effectSystem
            (slice, evalContent content slice effectSystem)

        and private cycleViews incrementAspects content slice effectSystem =
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
            | Inset (applicator, algorithm, playback, keyFrames) ->
                if Array.notEmpty keyFrames then
                    let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                    let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                    let tweened = tween Vector4.op_Multiply keyFrame.TweenValue keyFrame2.TweenValue progress algorithm effectSystem
                    let applied = applyTween Vector4.Multiply Vector4.Divide slice.Color tweened applicator
                    { slice with InsetOpt = Some applied }
                else slice
            | Color (applicator, algorithm, playback, keyFrames) ->
                if Array.notEmpty keyFrames then
                    let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                    let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                    let tweened = tween Vector4.op_Multiply keyFrame.TweenValue keyFrame2.TweenValue progress algorithm effectSystem
                    let applied = applyTween Vector4.Multiply Vector4.Divide slice.Color tweened applicator
                    { slice with Color = applied }
                else slice
            | Glow (applicator, algorithm, playback, keyFrames) ->
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
            | Text text ->
                { slice with Text = text }
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

            // build sprite views
            let effectSystem =
                if slice.Enabled then
                    let spriteView =
                        Render
                            (LayerableDescriptor
                                { Depth = slice.Depth
                                  AssetTag = image
                                  PositionY = slice.Position.Y
                                  LayeredDescriptor =
                                    SpriteDescriptor 
                                        { Position = slice.Position
                                          Size = slice.Size
                                          Rotation = slice.Rotation
                                          Offset = slice.Offset
                                          ViewType = effectSystem.ViewType
                                          InsetOpt = slice.InsetOpt
                                          Image = AssetTag.specialize<Image> image
                                          Color = slice.Color
                                          Glow = Vector4.Zero
                                          Flip = FlipNone }})
                    addView spriteView effectSystem
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

            // build animated sprite views
            let effectSystem =
                if slice.Enabled then
                    let animatedSpriteView =
                        Render
                            (LayerableDescriptor
                                { Depth = slice.Depth
                                  AssetTag = image
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
                                      Color = slice.Color
                                      Glow = slice.Glow
                                      Flip = FlipNone }})
                    addView animatedSpriteView effectSystem
                else effectSystem

            // build implicitly mounted content
            evalContent content slice effectSystem

        and private evalTextSprite resource aspects content slice effectSystem =

            // pull font from resource
            let font = evalResource resource effectSystem

            // eval aspects
            let slice = evalAspects aspects slice effectSystem

            // build sprite views
            let effectSystem =
                if slice.Enabled then
                    let spriteView =
                        Render
                            (LayerableDescriptor
                                { Depth = slice.Depth
                                  AssetTag = font
                                  PositionY = slice.Position.Y
                                  LayeredDescriptor =
                                    TextDescriptor 
                                        { Position = slice.Position - slice.Size * 0.5f
                                          Size = slice.Size
                                          ViewType = effectSystem.ViewType
                                          Text = slice.Text
                                          Font = AssetTag.specialize<Font> font
                                          Color = slice.Color
                                          Justification = Justified (JustifyCenter, JustifyMiddle) }})
                    addView spriteView effectSystem
                else effectSystem

            // build implicitly mounted content
            evalContent content slice effectSystem

        and private evalSoundEffect resource aspects content slice effectSystem =

            // pull sound from resource
            let sound = evalResource resource effectSystem

            // eval aspects
            let slice = evalAspects aspects slice effectSystem

            // build sound views
            let effectSystem =
                if slice.Enabled
                then addView (PlaySound (slice.Volume, AssetTag.specialize<Audio> sound)) effectSystem
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
                        let (slice, effectSystem) = iterateViews incrementAspects content slice effectSystem
                        (slice, effectSystem))
                    (slice, effectSystem)
                    [|0 .. count - 1|] |>
                snd

            // eval cycling repeat
            | Cycle count ->
                Array.fold
                    (fun effectSystem i ->
                        let effectSystem = { effectSystem with ProgressOffset = 1.0f / single count * single i }
                        cycleViews incrementAspects content slice effectSystem)
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
            | StaticSprite (resource, aspects, content) ->
                evalStaticSprite resource aspects content slice effectSystem
            | AnimatedSprite (resource, celSize, celRun, celCount, delay, aspects, content) ->
                evalAnimatedSprite resource celSize celRun celCount delay aspects content slice effectSystem
            | TextSprite (resource, aspects, content) ->
                evalTextSprite resource aspects content slice effectSystem
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
            let views = effectSystem.Views
            let effectSystem = { effectSystem with Views = List<View> () }
            (views, effectSystem)

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
            { ViewType = viewType
              Views = List<View> ()
              History = history
              ProgressOffset = 0.0f
              EffectTime = effectTime
              EffectEnv = globalEnv
              Chaos = System.Random () }

/// An abstract data type for executing effects.
type EffectSystem = EffectSystemModule.EffectSystem