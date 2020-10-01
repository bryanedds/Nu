// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open Prime
module Effects =

    type [<StructuralEquality; StructuralComparison>] Algorithm =
        | Constant
        | Linear
        | Random
        | Chaos
        | Ease
        | EaseIn
        | EaseOut
        | Sin
        | SinScaled of single
        | Cos
        | CosScaled of single

    type [<StructuralEquality; StructuralComparison>] LogicApplicator =
        | Or
        | Nor
        | Xor
        | And
        | Nand
        | Equal

    type [<StructuralEquality; StructuralComparison>] TweenApplicator =
        | Sum
        | Delta
        | Scale
        | Ratio
        | Set

    type [<StructuralEquality; StructuralComparison>] Slice =
        { Position : Vector2
          Size : Vector2
          Rotation : single
          Depth : single
          Offset : Vector2
          InsetOpt : Vector4 option
          Color : Color
          Glow : Color
          Volume : single
          Enabled : bool }

    type KeyFrame =
        abstract KeyFrameLength : int64

    type [<StructuralEquality; StructuralComparison>] LogicKeyFrame =
        { LogicValue : bool
          LogicLength : int64 }
        interface KeyFrame with
            member this.KeyFrameLength = this.LogicLength

    type [<StructuralEquality; StructuralComparison>] TweenKeyFrame =
        { TweenValue : single
          TweenLength : int64 }
        interface KeyFrame with
            member this.KeyFrameLength = this.TweenLength

    type [<StructuralEquality; StructuralComparison>] Tween2KeyFrame =
        { TweenValue : Vector2
          TweenLength : int64 }
        interface KeyFrame with
            member this.KeyFrameLength = this.TweenLength

    type [<StructuralEquality; StructuralComparison>] Tween3KeyFrame =
        { TweenValue : Vector3
          TweenLength : int64 }
        interface KeyFrame with
            member this.KeyFrameLength = this.TweenLength

    type [<StructuralEquality; StructuralComparison>] Tween4KeyFrame =
        { TweenValue : Vector4
          TweenLength : int64 }
        interface KeyFrame with
            member this.KeyFrameLength = this.TweenLength

    type [<StructuralEquality; StructuralComparison>] TweenCKeyFrame =
        { TweenValue : Color
          TweenLength : int64 }
        interface KeyFrame with
            member this.KeyFrameLength = this.TweenLength

    type [<StructuralEquality; StructuralComparison>] TweenIKeyFrame =
        { TweenValue : int
          TweenLength : int64 }
        interface KeyFrame with
            member this.KeyFrameLength = this.TweenLength

    type [<StructuralEquality; StructuralComparison>] Tween2IKeyFrame =
        { TweenValue : Vector2i
          TweenLength : int64 }
        interface KeyFrame with
            member this.KeyFrameLength = this.TweenLength
            
    type [<StructuralEquality; StructuralComparison>] Playback =
        | Once
        | Loop
        | Bounce
        
    type [<StructuralEquality; StructuralComparison>] Repetition =
        | Cycle of Cycles : int
        | Iterate of Iterations : int

    type [<StructuralEquality; StructuralComparison>] Rate =
        Rate of single

    type [<StructuralEquality; StructuralComparison>] Shift =
        Shift of single

    type [<StructuralEquality; NoComparison>] Resource =
        | Resource of string * string
        | Expand of string * Argument array

    and [<StructuralEquality; NoComparison>] Aspect =
        | Enabled of LogicApplicator * Playback * LogicKeyFrame array
        | Position of TweenApplicator * Algorithm * Playback * Tween2KeyFrame array
        | Translation of TweenApplicator * Algorithm * Playback * Tween2KeyFrame array
        | Offset of TweenApplicator * Algorithm * Playback * Tween2KeyFrame array
        | Size of TweenApplicator * Algorithm * Playback * Tween2KeyFrame array
        | Rotation of TweenApplicator * Algorithm * Playback * TweenKeyFrame array
        | Depth of TweenApplicator * Algorithm * Playback * TweenKeyFrame array
        | Inset of TweenApplicator * Algorithm * Playback * Tween4KeyFrame array
        | Color of TweenApplicator * Algorithm * Playback * TweenCKeyFrame array
        | Glow of TweenApplicator * Algorithm * Playback * TweenCKeyFrame array
        | Volume of TweenApplicator * Algorithm * Playback * TweenKeyFrame array
        | Bone // TODO: implement bone aspect
        | Expand of string * Argument array
        | Aspects of Aspect array

    and [<StructuralEquality; NoComparison>] Content =
        | Nil // first to make default value when missing
        | StaticSprite of Resource * Aspect array * Content
        | AnimatedSprite of Resource * Vector2i * int * int * int64 * Playback * Aspect array * Content
        | TextSprite of Resource * string * Aspect array * Content
        | SoundEffect of Resource * Aspect array * Content
        | Mount of Shift * Aspect array * Content
        | Repeat of Shift * Repetition * Aspect array * Content
        | Emit of Shift * Rate * Aspect array * Aspect array * Content
        | Tag of string * Aspect array * Content
        | Delay of int64 * Content
        | Segment of int64 * int64 * Content
        | Expand of string * Argument array
        | Contents of Shift * Content array

    and Argument =
        SymbolicCompression<Resource, SymbolicCompression<Aspect, Content>>

    type [<StructuralEquality; NoComparison>] Definition =
        { DefinitionParams : string array
          DefinitionBody : SymbolicCompression<Resource, SymbolicCompression<Aspect, Content>> }

    type Definitions =
        Map<string, Definition>

/// Describes an effect in a compositional manner.
[<Syntax   ("Constant Linear Random Chaos Ease EaseIn EaseOut Sin SinScaled Cos CosScaled " +
            "Or Nor Xor And Nand Equal " +
            "Sum Delta Scale Ratio Set " +
            "Once Loop Bounce " +
            "Cycle Iterate " +
            "Rate " +
            "Shift " +
            "Expand Resource " +
            "Expand Enabled Position Translation Offset Inset Size Rotation Depth Color Glow Volume Bone Aspects " +
            "Expand StaticSprite AnimatedSprite TextSprite SoundEffect Mount Repeat Emit Delay Segment Composite Tag Nil " +
            "View",
            "", "", "", "",
            Constants.PrettyPrinter.DefaultThresholdMin,
            Constants.PrettyPrinter.CompositionalThresholdMax)>]
type [<StructuralEquality; NoComparison>] Effect =
    { EffectName : string
      LifetimeOpt : int64 option
      Definitions : Effects.Definitions
      Content : Effects.Content }

[<RequireQualifiedAccess>]
module Effect =

    /// The empty effect.
    let empty =
        { EffectName = Constants.Engine.DefaultEffectName
          LifetimeOpt = None
          Definitions = Map.empty
          Content = Effects.Contents (Effects.Shift 0.0f, [||]) }

[<RequireQualifiedAccess>]
module EffectSystem =

    // effects
    open Effects

    /// An abstract data type for executing effects.
    type [<StructuralEquality; NoComparison>] EffectSystem =
        private
            { Absolute : bool
              Views : View List
              History : Slice seq
              ProgressOffset : single
              EffectTime : int64
              EffectEnv : Definitions
              Chaos : Random }

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
        | Constant ->
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
        | SinScaled scalar ->
            let progressScaled = float progress * Math.PI * 2.0 * float scalar
            let progressSin = Math.Sin progressScaled
            value + scale (value2 - value, single progressSin)
        | Cos ->
            let progressScaled = float progress * Math.PI * 2.0
            let progressCos = Math.Cos progressScaled
            value + scale (value2 - value, single progressCos)
        | CosScaled scalar ->
            let progressScaled = float progress * Math.PI * 2.0 * float scalar
            let progressCos = Math.Cos progressScaled
            let result = value + scale (value2 - value, single progressCos)
            result

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

    let private evalInset (celSize : Vector2i) celRun celCount stutter playback effectSystem =
        // TODO: make sure Bounce playback works as intended!
        let celUnmodulated = int (effectSystem.EffectTime / stutter)
        let cel = celUnmodulated % celCount
        let celI = cel % celRun
        let celJ = cel / celRun
        let bouncing =
            match playback with
            | Bounce -> celUnmodulated % (celCount * 2) >= celCount
            | Once | Loop -> false
        let (celI, celJ) =
            if bouncing
            then (celRun - celI, (celRun % celCount) - celJ)
            else (celI, celJ)
        let celX = celI * celSize.X
        let celY = celJ * celSize.Y
        let celPosition = Vector2 (single celX, single celY)
        let celSize = Vector2 (single celSize.X, single celSize.Y)
        v4Bounds celPosition celSize

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
                let inset = match slice.InsetOpt with Some inset -> inset | None -> Vector4 (0.0f, 0.0f, 1.0f, 1.0f)
                let applied = applyTween Vector4.Multiply Vector4.Divide inset tweened applicator
                { slice with InsetOpt = Some applied }
            else slice
        | Color (applicator, algorithm, playback, keyFrames) ->
            if Array.notEmpty keyFrames then
                let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                let tweened = tween Vector4.op_Multiply (keyFrame.TweenValue.ToVector4 ()) (keyFrame2.TweenValue.ToVector4 ()) progress algorithm effectSystem
                let applied = applyTween Color.Multiply Color.Divide slice.Color (tweened.ToColor ()) applicator
                { slice with Color = applied }
            else slice
        | Glow (applicator, algorithm, playback, keyFrames) ->
            if Array.notEmpty keyFrames then
                let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                let tweened = tween Color.op_Multiply keyFrame.TweenValue keyFrame2.TweenValue progress algorithm effectSystem
                let applied = applyTween Color.Multiply Color.Divide slice.Color tweened applicator
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

        | Aspects aspects ->
            Array.fold
                (fun slice aspect -> evalAspect aspect slice effectSystem)
                slice
                aspects
    
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
                let mutable transform =
                    { Position = slice.Position
                      Size = slice.Size
                      Rotation = slice.Rotation
                      Depth = slice.Depth
                      Flags = 0
                      RefCount = 0 }
                transform.Absolute <- effectSystem.Absolute
                let spriteView =
                    Render (transform.Depth, transform.Position.Y, AssetTag.generalize image,
                        SpriteDescriptor 
                            { Transform = transform
                              Offset = slice.Offset
                              InsetOpt = slice.InsetOpt
                              Image = AssetTag.specialize<Image> image
                              Color = slice.Color
                              Glow = Color.Zero
                              Flip = FlipNone })
                addView spriteView effectSystem
            else effectSystem

        // build implicitly mounted content
        evalContent content slice effectSystem

    and private evalAnimatedSprite resource (celSize : Vector2i) celRun celCount stutter playback aspects content slice effectSystem =

        // pull image from resource
        let image = evalResource resource effectSystem

        // eval aspects
        let slice = evalAspects aspects slice effectSystem

        // ensure valid data
        if stutter <> 0L && celRun <> 0 then

            // compute cel
            let cel = int (effectSystem.EffectTime / stutter)

            // eval inset
            let inset = evalInset celSize celRun celCount stutter playback effectSystem

            // build animated sprite views
            let effectSystem =
                if  slice.Enabled &&
                    not (playback = Once && cel >= celCount) then
                    let mutable transform =
                        { Position = slice.Position
                          Size = slice.Size
                          Rotation = slice.Rotation
                          Depth = slice.Depth
                          Flags = 0
                          RefCount = 0 }
                    transform.Absolute <- effectSystem.Absolute
                    let animatedSpriteView =
                        Render (transform.Depth, transform.Position.Y, AssetTag.generalize image,
                            SpriteDescriptor
                               { Transform = transform
                                 Offset = slice.Offset
                                 InsetOpt = Some inset
                                 Image = AssetTag.specialize<Image> image
                                 Color = slice.Color
                                 Glow = slice.Glow
                                 Flip = FlipNone })
                    addView animatedSpriteView effectSystem
                else effectSystem

            // build implicitly mounted content
            evalContent content slice effectSystem

        // abandon evaL
        else effectSystem

    and private evalTextSprite resource text aspects content slice effectSystem =

        // pull font from resource
        let font = evalResource resource effectSystem

        // eval aspects
        let slice = evalAspects aspects slice effectSystem

        // build sprite views
        let effectSystem =
            if slice.Enabled then
                let mutable transform =
                    { Position = slice.Position - slice.Size * 0.5f
                      Size = slice.Size
                      Rotation = slice.Rotation
                      Depth = slice.Depth
                      Flags = 0
                      RefCount = 0 }
                transform.Absolute <- effectSystem.Absolute
                let spriteView =
                    Render (transform.Depth, transform.Position.Y, font,
                        TextDescriptor 
                            { Transform = transform
                              Text = text
                              Font = AssetTag.specialize<Font> font
                              Color = slice.Color
                              Justification = Justified (JustifyCenter, JustifyMiddle) })
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
            then addView (PlaySound (slice.Volume, AssetTag.specialize<Sound> sound)) effectSystem
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

    and private evalTag name aspects content (slice : Slice) effectSystem =

        // eval aspects
        let slice = evalAspects aspects slice effectSystem

        // build tag view
        let effectSystem =
            if slice.Enabled then
                let tagView = Nu.Tag (name, slice)
                addView tagView effectSystem
            else effectSystem

        // build implicitly mounted content
        evalContent content slice effectSystem

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

    and private evalSegment start stop content slice effectSystem =
        if  effectSystem.EffectTime >= start &&
            effectSystem.EffectTime < stop then
            let effectSystem = { effectSystem with EffectTime = effectSystem.EffectTime - start }
            let effectSystem = evalContent content slice effectSystem
            let effectSystem = { effectSystem with EffectTime = effectSystem.EffectTime + start }
            effectSystem
        else effectSystem

    and private evalContents shift contents slice effectSystem =
        let slice = { slice with Slice.Depth = slice.Depth + shift }
        evalContents3 contents slice effectSystem

    and private evalContent content slice effectSystem =
        match content with
        | Nil ->
            effectSystem
        | StaticSprite (resource, aspects, content) ->
            evalStaticSprite resource aspects content slice effectSystem
        | AnimatedSprite (resource, celSize, celRun, celCount, stutter, playback, aspects, content) ->
            evalAnimatedSprite resource celSize celRun celCount stutter playback aspects content slice effectSystem
        | TextSprite (resource, text, aspects, content) ->
            evalTextSprite resource text aspects content slice effectSystem
        | SoundEffect (resource, aspects, content) ->
            evalSoundEffect resource aspects content slice effectSystem
        | Mount (Shift shift, aspects, content) ->
            evalMount shift aspects content slice effectSystem
        | Repeat (Shift shift, repetition, incrementAspects, content) ->
            evalRepeat shift repetition incrementAspects content slice effectSystem
        | Emit (Shift shift, Rate rate, emitterAspects, aspects, content) ->
            evalEmit shift rate emitterAspects aspects content effectSystem
        | Tag (name, aspects, content) ->
            evalTag name aspects content slice effectSystem
        | Delay (delay, content) ->
            evalSegment delay Int64.MaxValue content slice effectSystem
        | Segment (start, stop, content) ->
            evalSegment start stop content slice effectSystem
        | Contents (Shift shift, contents) ->
            evalContents shift contents slice effectSystem
        | Expand (definitionName, arguments) ->
            evalExpand definitionName arguments slice effectSystem

    and private evalContents3 contents slice effectSystem =
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
              Content = Contents (Shift 0.0f, effects |> List.map (fun effect -> effect.Content) |> Array.ofList) }
        effectCombined

    let make absolute history effectTime globalEnv = 
        { Absolute = absolute
          Views = List<View> ()
          History = history
          ProgressOffset = 0.0f
          EffectTime = effectTime
          EffectEnv = globalEnv
          Chaos = System.Random () }

/// An abstract data type for executing effects.
type EffectSystem = EffectSystem.EffectSystem