// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Effects
open System
open System.Numerics
open Prime
open Nu
open Nu.Effects

[<RequireQualifiedAccess>]
module EffectSystem =

    /// Evaluates effect descriptors.
    type [<ReferenceEquality>] EffectSystem =
        private
            { EffectTime : GameTime
              EffectTimeOriginal : GameTime
              EffectProgressOffset : single
              EffectAbsolute : bool
              EffectCastShadow : bool
              EffectPresence : Presence
              EffectClipOpt : Box2 option
              EffectShadowOffset : single
              EffectRenderType : RenderType
              EffectDataTokens : DataToken SList
              EffectEnv : Definitions }

    let rec private addDataToken dataToken effectSystem =
        effectSystem.EffectDataTokens.Add dataToken
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
            let totalTime = Array.fold (fun totalTime (keyFrame : 'kf) -> totalTime + keyFrame.KeyFrameLength) GameTime.zero keyFrames 
            if totalTime <> GameTime.zero then
                let moduloTime = localTime % totalTime
                selectKeyFrames2 moduloTime Once keyFrames
            else (GameTime.zero, Array.head keyFrames, Array.head keyFrames)
        | Bounce ->
            let totalTime = Array.fold (fun totalTime (keyFrame : 'kf) -> totalTime + keyFrame.KeyFrameLength) GameTime.zero keyFrames
            if totalTime <> GameTime.zero then
                let moduloTime = localTime % totalTime
                let bouncing = int (localTime / totalTime) % 2 = 1
                let bounceTime = if bouncing then totalTime - moduloTime else moduloTime
                selectKeyFrames2 bounceTime Once keyFrames
            else (GameTime.zero, Array.head keyFrames, Array.head keyFrames)

    let private selectKeyFrames<'kf when 'kf :> KeyFrame> localTime playback (keyFrames : 'kf array) =
        keyFrames
        |> selectKeyFrames2 localTime playback
        |> fun (fst, snd, thd) -> (fst, snd, thd)

    let inline private tween (scale : (^a * single) -> ^a) (value : ^a) (value2 : ^a) progress algorithm =
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
            let chaosValue = Gen.randomf
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
            value + scale (value2 - value, single progressCos)

    let private applyLogic value value2 applicator =
        match applicator with
        | Or -> value || value2
        | Nor -> not value && not value2
        | Xor -> value <> value2
        | And -> value && value2
        | Nand -> not (value && value2)
        | Equal -> value2

    let inline private applyTween mul div pow mod_ (value : ^a) (value2 : ^a) applicator =
        match applicator with
        | Sum -> value + value2
        | Delta -> value - value2
        | Scalar -> mul (value, value2)
        | Ratio -> div (value, value2)
        | Modulo -> mod_ (value, value2)
        | Pow -> pow (value, value2)
        | Set -> value2

    let private evalInset (celSize : Vector2i) celCount celRun delay playback effectSystem =
        // TODO: stop assuming that animation sheets are fully and evenly populated when flipping!
        let celUnmodulated = int (effectSystem.EffectTime / delay)
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
        Box2 (celPosition, celSize)

    let private evalArgument (argument : Argument) : Definition =
        match argument with
        | SymbolicCompressionA resource ->
            { DefinitionParams = [||]; DefinitionBody = SymbolicCompressionA resource }
        | SymbolicCompressionB (SymbolicCompressionA aspect) ->
            { DefinitionParams = [||]; DefinitionBody = SymbolicCompressionB (SymbolicCompressionA aspect) }
        | SymbolicCompressionB (SymbolicCompressionB content) ->
            { DefinitionParams = [||]; DefinitionBody = SymbolicCompressionB (SymbolicCompressionB content) }

    let rec private evalResource resource effectSystem : AssetTag =
        match resource with
        | Resource (packageName, assetName) -> AssetTag.make<obj> packageName assetName
        | Resource.Expand (definitionName, _) ->
            match Map.tryFind definitionName effectSystem.EffectEnv with
            | Some definition ->
                match definition.DefinitionBody with
                | SymbolicCompressionA resource -> evalResource resource effectSystem
                | _ ->
                    Log.info ("Expected Resource for definition '" + definitionName + ".")
                    asset Assets.Default.PackageName Assets.Default.ImageName
            | None ->
                Log.info ("Could not find definition with name '" + definitionName + "'.")
                asset Assets.Default.PackageName Assets.Default.ImageName

    let rec private iterateDataTokens incrementAspects content slice history effectSystem =
        let effectSystem = { effectSystem with EffectProgressOffset = 0.0f }
        let slice = evalAspects incrementAspects slice effectSystem
        (slice, evalContent content slice history effectSystem)

    and private cycleDataTokens incrementAspects content slice history effectSystem =
        let slice = evalAspects incrementAspects slice effectSystem
        evalContent content slice history effectSystem

    and private evalProgress keyFrameTime keyFrameLength effectSystem =
        let progress = if GameTime.isZero keyFrameLength then 1.0f else single keyFrameTime / single keyFrameLength
        let progress = progress + effectSystem.EffectProgressOffset
        if progress > 1.0f then progress - 1.0f else progress

    and private evalAspect aspect slice effectSystem =
        match aspect with
        | Position position -> slice.Position <- slice.Position + position; slice
        | PositionLocal positionLocal ->
            let oriented = positionLocal.Transform slice.Angles.RollPitchYaw
            let translated = slice.Position + oriented
            slice.Position <- translated
            slice
        | PositionAbsolute position -> slice.Position <- position; slice
        | Scale scale -> slice.Scale <- scale; slice
        | Offset offset -> slice.Offset <- offset; slice
        | Angles angles -> slice.Angles <- angles; slice
        | Degrees degrees -> slice.Angles <- Math.DegreesToRadians3d degrees; slice
        | Size size -> slice.Size <- size; slice
        | Elevation elevation -> slice.Elevation <- elevation; slice
        | Inset inset -> slice.Inset <- inset; slice
        | Color color -> slice.Color <- color; slice
        | Blend blend -> slice.Blend <- blend; slice
        | Emission emission -> slice.Emission <- emission; slice
        | Height height -> slice.Height <- height; slice
        | IgnoreLightMaps ignoreLightMaps -> slice.IgnoreLightMaps <- ignoreLightMaps; slice
        | Flip flip -> slice.Flip <- flip; slice
        | Brightness brightness -> slice.Brightness <- brightness; slice
        | LightCutoff lightCutoff -> slice.LightCutoff <- lightCutoff; slice
        | Volume volume -> slice.Volume <- volume; slice
        | Enabled enabled -> slice.Enabled <- enabled; slice
        | Positions (applicator, algorithm, playback, keyFrames) ->
            if Array.notEmpty keyFrames then
                let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                let tweened = tween (fun (a, b) -> a * b) keyFrame.TweenValue keyFrame2.TweenValue progress algorithm
                let applied = applyTween Vector3.Multiply Vector3.Divide Vector3.Pow Vector3.Modulo slice.Position tweened applicator
                slice.Position <- applied
            slice
        | PositionLocals (applicator, algorithm, playback, keyFrames) ->
            if Array.notEmpty keyFrames then
                let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                let tweened = tween Vector3.op_Multiply keyFrame.TweenValue keyFrame2.TweenValue progress algorithm
                let oriented = tweened.Transform slice.Angles.RollPitchYaw
                let applied = applyTween Vector3.Multiply Vector3.Divide Vector3.Pow Vector3.Modulo slice.Position oriented applicator
                slice.Position <- applied
            slice
        | Scales (applicator, algorithm, playback, keyFrames) ->
            if Array.notEmpty keyFrames then
                let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                let tweened = tween Vector3.op_Multiply keyFrame.TweenValue keyFrame2.TweenValue progress algorithm
                let applied = applyTween Vector3.Multiply Vector3.Divide Vector3.Pow Vector3.Modulo slice.Size tweened applicator
                slice.Scale <- applied
            slice
        | Offsets (applicator, algorithm, playback, keyFrames) ->
            if Array.notEmpty keyFrames then
                let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                let tweened = tween Vector3.op_Multiply keyFrame.TweenValue keyFrame2.TweenValue progress algorithm
                let applied = applyTween Vector3.Multiply Vector3.Divide Vector3.Pow Vector3.Modulo slice.Position tweened applicator
                slice.Offset <- applied
            slice
        | Sizes (applicator, algorithm, playback, keyFrames) ->
            if Array.notEmpty keyFrames then
                let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                let tweened = tween Vector3.op_Multiply keyFrame.TweenValue keyFrame2.TweenValue progress algorithm
                let applied = applyTween Vector3.Multiply Vector3.Divide Vector3.Pow Vector3.Modulo slice.Size tweened applicator
                slice.Size <- applied
            slice
        | Angleses (applicator, algorithm, playback, keyFrames) ->
            if Array.notEmpty keyFrames then
                let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                let tweened = tween Vector3.Multiply keyFrame.TweenValue keyFrame2.TweenValue progress algorithm
                let applied = applyTween Vector3.Multiply Vector3.Divide Vector3.Pow Vector3.Modulo slice.Angles tweened applicator
                slice.Angles <- applied
            slice
        | Degreeses (applicator, algorithm, playback, keyFrames) ->
            if Array.notEmpty keyFrames then
                let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                let tweened = tween Vector3.Multiply keyFrame.TweenValue keyFrame2.TweenValue progress algorithm
                let applied = applyTween Vector3.Multiply Vector3.Divide Vector3.Pow Vector3.Modulo (Math.RadiansToDegrees3d slice.Angles) tweened applicator
                slice.Angles <- Math.DegreesToRadians3d applied
            slice
        | Elevations (applicator, algorithm, playback, keyFrames) ->
            if Array.notEmpty keyFrames then
                let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                let tweened = tween (fun (x, y) -> x * y) keyFrame.TweenValue keyFrame2.TweenValue progress algorithm
                let applied = applyTween (fun (x, y) -> x * y) (fun (x, y) -> x / y) (fun (x, y) -> single (Math.Pow (double x, double y))) (fun (x, y) -> x % y) slice.Elevation tweened applicator
                slice.Elevation <- applied
            slice
        | Insets (_, _, playback, keyFrames) ->
            if Array.notEmpty keyFrames then
                let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                let applied = if progress < 0.5f then keyFrame.TweenValue else keyFrame2.TweenValue
                slice.Inset <- applied
            slice
        | Colors (applicator, algorithm, playback, keyFrames) ->
            if Array.notEmpty keyFrames then
                let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                let tweened = tween Vector4.op_Multiply (keyFrame.TweenValue.V4) (keyFrame2.TweenValue.V4) progress algorithm
                let applied = applyTween Color.Multiply Color.Divide Color.Pow Color.Modulo slice.Color (System.Numerics.Color tweened) applicator
                slice.Color <- applied
            slice
        | Emissions (applicator, algorithm, playback, keyFrames) ->
            if Array.notEmpty keyFrames then
                let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                let tweened = tween Color.op_Multiply keyFrame.TweenValue keyFrame2.TweenValue progress algorithm
                let applied = applyTween Color.Multiply Color.Divide Color.Pow Color.Modulo slice.Color tweened applicator
                slice.Emission <- applied
            slice
        | Heights (applicator, algorithm, playback, keyFrames) ->
            if Array.notEmpty keyFrames then
                let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                let tweened = tween (fun (x, y) -> x * y) keyFrame.TweenValue keyFrame2.TweenValue progress algorithm
                let applied = applyTween (fun (x, y) -> x * y) (fun (x, y) -> x / y) (fun (x, y) -> single (Math.Pow (double x, double y))) (fun (x, y) -> x % y) slice.Height tweened applicator
                slice.Height <- applied
            slice
        | IgnoreLightMapses (applicator, playback, keyFrames) ->
            if Array.notEmpty keyFrames then
                let (_, keyFrame, _) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let applied = applyLogic slice.Enabled keyFrame.LogicValue applicator
                slice.IgnoreLightMaps <- applied
            slice
        | Brightnesses (applicator, algorithm, playback, keyFrames) ->
            if Array.notEmpty keyFrames then
                let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                let tweened = tween (fun (x, y) -> x * y) keyFrame.TweenValue keyFrame2.TweenValue progress algorithm
                let applied = applyTween (fun (x, y) -> x * y) (fun (x, y) -> x / y) (fun (x, y) -> single (Math.Pow (double x, double y))) (fun (x, y) -> x % y) slice.Brightness tweened applicator
                slice.Brightness <- applied
            slice
        | LightCutoffs (applicator, algorithm, playback, keyFrames) ->
            if Array.notEmpty keyFrames then
                let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                let tweened = tween (fun (x, y) -> x * y) keyFrame.TweenValue keyFrame2.TweenValue progress algorithm
                let applied = applyTween (fun (x, y) -> x * y) (fun (x, y) -> x / y) (fun (x, y) -> single (Math.Pow (double x, double y))) (fun (x, y) -> x % y) slice.LightCutoff tweened applicator
                slice.LightCutoff <- applied
            slice
        | Volumes (applicator, algorithm, playback, keyFrames) ->
            if Array.notEmpty keyFrames then
                let (keyFrameTime, keyFrame, keyFrame2) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let progress = evalProgress keyFrameTime keyFrame.TweenLength effectSystem
                let tweened = tween (fun (x, y) -> x * y) keyFrame.TweenValue keyFrame2.TweenValue progress algorithm
                let applied = applyTween (fun (x, y) -> x * y) (fun (x, y) -> x / y) (fun (x, y) -> single (Math.Pow (double x, double y))) (fun (x, y) -> x % y) slice.Volume tweened applicator
                slice.Volume <- applied
            slice
        | Enableds (applicator, playback, keyFrames) ->
            if Array.notEmpty keyFrames then
                let (_, keyFrame, _) = selectKeyFrames effectSystem.EffectTime playback keyFrames
                let applied = applyLogic slice.Enabled keyFrame.LogicValue applicator
                slice.Enabled <- applied
            slice
        | Aspect.Expand (definitionName, _) ->
            match Map.tryFind definitionName effectSystem.EffectEnv with
            | Some definition ->
                match definition.DefinitionBody with
                | SymbolicCompressionB (SymbolicCompressionA aspect) -> evalAspect aspect slice effectSystem
                | _ -> Log.info ("Expected Aspect for definition '" + definitionName + "'."); slice
            | None -> Log.info ("Could not find definition with name '" + definitionName + "'."); slice
        | Aspects aspects ->
            Array.fold (fun slice aspect -> evalAspect aspect slice effectSystem) slice aspects

    and private evalAspects aspects (slice : Slice) effectSystem =
        Array.fold (fun slice aspect -> evalAspect aspect slice effectSystem) slice aspects

    and private evalExpand definitionName arguments slice history effectSystem =
        match Map.tryFind definitionName effectSystem.EffectEnv with
        | Some definition ->
            match definition.DefinitionBody with
            |  SymbolicCompressionB (SymbolicCompressionB content) ->
                let localDefinitions = Array.map evalArgument arguments
                match (try Array.zip definition.DefinitionParams localDefinitions |> Some with _ -> None) with
                | Some localDefinitionEntries ->
                    let effectSystem = { effectSystem with EffectEnv = Map.addMany localDefinitionEntries effectSystem.EffectEnv }
                    evalContent content slice history effectSystem
                | None -> Log.info "Wrong number of arguments provided to ExpandContent."; effectSystem
            | _ -> Log.info ("Expected Content for definition '" + definitionName + "'."); effectSystem
        | None -> Log.info ("Could not find definition with name '" + definitionName + "'."); effectSystem

    and private evalStaticSprite resource aspects content (slice : Slice) history effectSystem =

        // pull image from resource
        let image = evalResource resource effectSystem

        // eval aspects
        let slice = evalAspects aspects slice effectSystem

        // build sprite tokens
        let effectSystem =
            if slice.Enabled then
                let mutable transform = Transform.makeIntuitive effectSystem.EffectAbsolute slice.Position slice.Scale slice.Offset slice.Size slice.Angles slice.Elevation
                let sprite =
                    { SpriteValue.Transform = transform
                      InsetOpt = if slice.Inset.Equals box2Zero then ValueNone else ValueSome slice.Inset
                      ClipOpt = Option.toValueOption effectSystem.EffectClipOpt
                      Image = AssetTag.specialize<Image> image
                      Color = slice.Color
                      Blend = slice.Blend
                      Emission = slice.Emission
                      Flip = slice.Flip }
                let spriteToken = SpriteToken (transform.Elevation, transform.Horizon, image, sprite)
                addDataToken spriteToken effectSystem
            else effectSystem

        // build implicitly mounted content
        evalContent content slice history effectSystem

    and private evalAnimatedSprite resource (celSize : Vector2i) celCount celRun delay playback aspects content slice history effectSystem =

        // pull image from resource
        let image = evalResource resource effectSystem

        // eval aspects
        let slice = evalAspects aspects slice effectSystem

        // ensure valid data
        if GameTime.notZero delay && celRun <> 0 then

            // compute cel
            let cel = int (effectSystem.EffectTime / delay)

            // eval inset
            let inset = evalInset celSize celCount celRun delay playback effectSystem

            // build animated sprite tokens
            let effectSystem =
                if  slice.Enabled &&
                    not (playback = Once && cel >= celCount) then
                    let mutable transform = Transform.makeIntuitive effectSystem.EffectAbsolute slice.Position slice.Scale slice.Offset slice.Size slice.Angles slice.Elevation
                    let sprite =
                        { SpriteValue.Transform = transform
                          InsetOpt = ValueSome inset
                          ClipOpt = Option.toValueOption effectSystem.EffectClipOpt
                          Image = AssetTag.specialize<Image> image
                          Color = slice.Color
                          Blend = slice.Blend
                          Emission = slice.Emission
                          Flip = slice.Flip }
                    let spriteToken = SpriteToken (transform.Elevation, transform.Horizon, image, sprite)
                    addDataToken spriteToken effectSystem
                else effectSystem

            // build implicitly mounted content
            evalContent content slice history effectSystem

        // abandon evaL
        else effectSystem

    and private evalTextSprite resource text fontSizing fontStyling aspects content slice history effectSystem =

        // pull font from resource
        let font = evalResource resource effectSystem

        // eval aspects
        let slice = evalAspects aspects slice effectSystem

        // build text tokens
        let effectSystem =
            if slice.Enabled then
                let mutable transform = Transform.makeIntuitive effectSystem.EffectAbsolute slice.Position slice.Scale slice.Offset slice.Size slice.Angles slice.Elevation
                let text =
                    { TextValue.Transform = transform
                      ClipOpt = Option.toValueOption effectSystem.EffectClipOpt
                      Text = text
                      Font = AssetTag.specialize<Font> font
                      FontSizing = fontSizing
                      FontStyling = fontStyling
                      Color = slice.Color
                      Justification = Justified (JustifyCenter, JustifyMiddle)
                      CaretOpt = None }
                let textToken = TextToken (transform.Elevation, transform.Horizon, font, text)
                addDataToken textToken effectSystem
            else effectSystem

        // build implicitly mounted content
        evalContent content slice history effectSystem

    and private evalLight3d lightType aspects content (slice : Slice) history effectSystem =

        // eval aspects
        let slice = evalAspects aspects slice effectSystem

        // build light tokens
        let effectSystem =
            if slice.Enabled then
                let rotation = Quaternion.CreateFromYawPitchRoll (slice.Angles.Y, slice.Angles.X, slice.Angles.Z)
                let direction = rotation.Down
                let bounds = Box3 (slice.Position - v3Dup slice.LightCutoff, v3Dup slice.LightCutoff * 2.0f)
                let lightToken =
                    Light3dToken
                        { LightId = 0UL
                          Origin = slice.Position
                          Rotation = rotation
                          Direction = direction
                          Presence = effectSystem.EffectPresence
                          Color = slice.Color
                          Brightness = slice.Brightness
                          AttenuationLinear = 1.0f / (slice.Brightness * slice.LightCutoff)
                          AttenuationQuadratic = 1.0f / (slice.Brightness * slice.LightCutoff * slice.LightCutoff)
                          LightCutoff = slice.LightCutoff
                          LightType = lightType
                          DesireShadows = false
                          Bounds = bounds }
                addDataToken lightToken effectSystem
            else effectSystem

        // build implicitly mounted content
        evalContent content slice history effectSystem

    and private evalBillboard albedo roughness metallic ambientOcclusion emission normal height twoSided clipped aspects content (slice : Slice) history effectSystem =

        // pull image from resource
        let imageAlbedo = evalResource albedo effectSystem
        let imageRoughness = evalResource roughness effectSystem
        let imageMetallic = evalResource metallic effectSystem
        let imageAmbientOcclusion = evalResource ambientOcclusion effectSystem
        let imageEmission = evalResource emission effectSystem
        let imageNormal = evalResource normal effectSystem
        let imageHeight = evalResource height effectSystem

        // eval aspects
        let slice = evalAspects aspects slice effectSystem

        // build billboard tokens
        let effectSystem =
            if slice.Enabled then
                let affineMatrix = Matrix4x4.CreateAffine (slice.Position, slice.Angles.RollPitchYaw, slice.Scale)
                let insetOpt = if slice.Inset.Equals box2Zero then None else Some slice.Inset
                let properties =
                    { AlbedoOpt = ValueSome slice.Color
                      RoughnessOpt = ValueNone
                      MetallicOpt = ValueNone
                      AmbientOcclusionOpt = ValueNone
                      EmissionOpt = ValueSome slice.Emission.R
                      HeightOpt = ValueSome slice.Height
                      IgnoreLightMapsOpt = ValueSome slice.IgnoreLightMaps
                      OpaqueDistanceOpt = ValueNone
                      FinenessOffsetOpt = ValueNone
                      ScatterTypeOpt = ValueNone
                      SpecularScalarOpt = ValueSome 0.0f // TODO: consider making this an aspect?
                      RefractiveIndexOpt = ValueNone } // TODO: consider making this an aspect?
                let material =
                    { AlbedoImageOpt = ValueSome (AssetTag.specialize<Image> imageAlbedo)
                      RoughnessImageOpt = ValueSome (AssetTag.specialize<Image> imageRoughness)
                      MetallicImageOpt = ValueSome (AssetTag.specialize<Image> imageMetallic)
                      AmbientOcclusionImageOpt = ValueSome (AssetTag.specialize<Image> imageAmbientOcclusion)
                      EmissionImageOpt = ValueSome (AssetTag.specialize<Image> imageEmission)
                      NormalImageOpt = ValueSome (AssetTag.specialize<Image> imageNormal)
                      HeightImageOpt = ValueSome (AssetTag.specialize<Image> imageHeight)
                      SubdermalImageOpt = ValueNone
                      FinenessImageOpt = ValueNone
                      ScatterImageOpt = ValueNone
                      TwoSidedOpt = ValueSome twoSided
                      ClippedOpt = ValueSome clipped }
                let billboardToken =
                    BillboardToken
                        { ModelMatrix = affineMatrix
                          CastShadow = effectSystem.EffectCastShadow
                          Presence = effectSystem.EffectPresence
                          InsetOpt = insetOpt
                          MaterialProperties = properties
                          Material = material
                          ShadowOffset = effectSystem.EffectShadowOffset
                          DepthTest = LessThanOrEqualTest
                          RenderType = effectSystem.EffectRenderType }
                addDataToken billboardToken effectSystem
            else effectSystem

        // build implicitly mounted content
        evalContent content slice history effectSystem

    and private evalStaticModel resource clipped aspects content (slice : Slice) history effectSystem =

        // pull image from resource
        let staticModel = evalResource resource effectSystem

        // eval aspects
        let slice = evalAspects aspects slice effectSystem

        // build static model tokens
        let effectSystem =
            if slice.Enabled then
                let staticModel = AssetTag.specialize<StaticModel> staticModel
                let affineMatrix = Matrix4x4.CreateAffine (slice.Position, slice.Angles.RollPitchYaw, slice.Scale)
                let insetOpt = if slice.Inset.Equals box2Zero then None else Some slice.Inset
                let properties =
                    { AlbedoOpt = ValueSome slice.Color
                      RoughnessOpt = ValueNone
                      MetallicOpt = ValueNone
                      AmbientOcclusionOpt = ValueNone
                      EmissionOpt = ValueSome slice.Emission.R
                      HeightOpt = ValueSome slice.Height
                      IgnoreLightMapsOpt = ValueSome slice.IgnoreLightMaps
                      OpaqueDistanceOpt = ValueNone
                      FinenessOffsetOpt = ValueNone
                      ScatterTypeOpt = ValueNone
                      SpecularScalarOpt = ValueNone
                      RefractiveIndexOpt = ValueNone }
                let staticModelToken =
                    StaticModelToken
                        { ModelMatrix = affineMatrix
                          CastShadow = effectSystem.EffectCastShadow
                          Presence = effectSystem.EffectPresence
                          InsetOpt = insetOpt
                          MaterialProperties = properties
                          StaticModel = staticModel
                          Clipped = clipped
                          DepthTest = LessThanOrEqualTest
                          RenderType = effectSystem.EffectRenderType }
                addDataToken staticModelToken effectSystem
            else effectSystem

        // build implicitly mounted content
        evalContent content slice history effectSystem

    and private evalMount shift aspects content (slice : Slice) history effectSystem =
        let slice = { slice with Elevation = slice.Elevation + shift }
        let slice = evalAspects aspects slice effectSystem
        evalContent content slice history effectSystem

    and private evalRepeat shift repetition incrementAspects content (slice : Slice) history effectSystem =

        // eval repeat either as iterative or cycling
        let slice = { slice with Elevation = slice.Elevation + shift }
        match repetition with

        // eval iterative repeat
        | Iterate count ->
            [|0 .. count - 1|]
            |> Array.fold
                (fun (slice, effectSystem) _ ->
                    let (slice, effectSystem) = iterateDataTokens incrementAspects content slice history effectSystem
                    (slice, effectSystem))
                (slice, effectSystem)
            |> snd

        // eval cycling repeat
        | Cycle count ->
            Array.fold
                (fun effectSystem i ->
                    let effectSystem = { effectSystem with EffectProgressOffset = 1.0f / single count * single i }
                    cycleDataTokens incrementAspects content slice history effectSystem)
                effectSystem
                [|0 .. count - 1|]

    and private evalTag name aspects content (slice : Slice) history effectSystem =

        // eval aspects
        let slice = evalAspects aspects slice effectSystem

        // build tag token
        let effectSystem =
            if slice.Enabled then
                let tagToken = Nu.TagToken (name, slice)
                addDataToken tagToken effectSystem
            else effectSystem

        // build implicitly mounted content
        evalContent content slice history effectSystem

    and private evalEmit shift rate emitterAspects aspects content history effectSystem =
        Seq.fold (fun effectSystem slice ->
            let effectTimeOld = effectSystem.EffectTime
            let effectTime = effectSystem.EffectTimeOriginal - slice.SliceTime
            let slice = { slice with Elevation = slice.Elevation + shift }
            let slice = evalAspects emitterAspects slice { effectSystem with EffectTime = effectSystem.EffectTime - effectTime }
            let emitCountLastFrame = single (effectSystem.EffectTime - effectTime - slice.SliceDelta) * rate
            let emitCountThisFrame = single (effectSystem.EffectTime - effectTime) * rate
            let emitCount = int emitCountThisFrame - int emitCountLastFrame
            let effectSystem =
                Array.fold (fun effectSystem _ ->
                    let emission = Slice.copy slice // protect original slice from mutation
                    let emission = evalAspects aspects emission effectSystem
                    if emission.Enabled
                    then evalContent content emission history effectSystem
                    else effectSystem)
                    { effectSystem with EffectTime = effectTime }
                    [|0 .. emitCount - 1|]
            { effectSystem with EffectTime = effectTimeOld })
            effectSystem
            history

    and private evalSegment start stop content slice history effectSystem =
        if  effectSystem.EffectTime >= start &&
            effectSystem.EffectTime < stop then
            let effectSystem = { effectSystem with EffectTime = effectSystem.EffectTime - start }
            let effectSystem = evalContent content slice history effectSystem
            let effectSystem = { effectSystem with EffectTime = effectSystem.EffectTime + start }
            effectSystem
        else effectSystem

    and private evalContents shift contents slice history effectSystem =
        let slice = { slice with Slice.Elevation = slice.Elevation + shift }
        evalContents3 contents slice history effectSystem

    and private evalContent content slice history effectSystem =
        let slice = Slice.copy slice // protect original slice from mutation
        match content with
        | Nil ->
            effectSystem
        | StaticSprite (resource, aspects, content) ->
            evalStaticSprite resource aspects content slice history effectSystem
        | AnimatedSprite (resource, celSize, celCount, celRun, delay, playback, aspects, content) ->
            evalAnimatedSprite resource celSize celCount celRun delay playback aspects content slice history effectSystem
        | TextSprite (resource, text, fontSizing, fontStyling, aspects, content) ->
            evalTextSprite resource text fontSizing fontStyling aspects content slice history effectSystem
        | Light3d (lightType, aspects, content) ->
            evalLight3d lightType aspects content slice history effectSystem
        | Billboard (resourceAlbedo, resourceRoughness, resourceMetallic, resourceAmbientOcclusion, resourceEmission, resourceNormal, resourceHeight, twoSided, clipped, aspects, content) ->
            evalBillboard resourceAlbedo resourceRoughness resourceMetallic resourceAmbientOcclusion resourceEmission resourceNormal resourceHeight twoSided clipped aspects content slice history effectSystem
        | StaticModel (resource, clipped, aspects, content) ->
            evalStaticModel resource clipped aspects content slice history effectSystem
        | Mount (Shift shift, aspects, content) ->
            evalMount shift aspects content slice history effectSystem
        | Repeat (Shift shift, repetition, incrementAspects, content) ->
            evalRepeat shift repetition incrementAspects content slice history effectSystem
        | Emit (Shift shift, Rate rate, emitterAspects, aspects, content) ->
            evalEmit shift rate emitterAspects aspects content history effectSystem
        | Tag (name, aspects, content) ->
            evalTag name aspects content slice history effectSystem
        | Delay (delay, content) ->
            evalSegment delay GameTime.MaxValue content slice history effectSystem
        | Segment (start, stop, content) ->
            evalSegment start stop content slice history effectSystem
        | Contents (Shift shift, contents) ->
            evalContents shift contents slice history effectSystem
        | Expand (definitionName, arguments) ->
            evalExpand definitionName arguments slice history effectSystem

    and private evalContents3 contents slice history effectSystem =
        Array.fold
            (fun effectSystem content -> evalContent content slice history effectSystem)
            effectSystem
            contents

    let private release effectSystem =
        let dataTokens = DataTokens (SArray.ofSeq effectSystem.EffectDataTokens)
        let effectSystem = { effectSystem with EffectDataTokens = SList.make () }
        (dataTokens, effectSystem)

    /// Evaluates an EffectDescriptor, applying the effect if it is still alive, with the following parameters:
    ///   - descriptor: The EffectDescriptor to be evaluated.
    ///   - slice: The Slice to apply the effect on.
    ///   - history: A history of the effect's previous states.
    ///   - effectSystem: The current state of the effect system.
    /// The function evaluates the EffectDescriptor to determine if the effect is still alive.
    /// If the effect is alive, it modifies the effectSystem to apply the effect to the given slice.
    /// If the effect is not alive, it simply releases the effectSystem without applying the effect.
    let eval descriptor slice history effectSystem =
        let alive =
            match descriptor.LifeTimeOpt with
            | Some lifetime -> lifetime <= GameTime.zero || effectSystem.EffectTime <= lifetime
            | None -> true
        if alive then
            let effectSystem = { effectSystem with EffectEnv = Map.concat effectSystem.EffectEnv descriptor.Definitions }
            try let effectSystem = evalContent descriptor.Content slice history effectSystem
                release effectSystem
            with exn ->
                let prettyPrinter = (SyntaxAttribute.defaultValue typeof<EffectDescriptor>).PrettyPrinter
                let effectStr = PrettyPrinter.prettyPrint (scstring descriptor) prettyPrinter
                Log.error ("Error in effect descriptor:\n" + effectStr + "\n due to: " + scstring exn)
                release effectSystem
        else release effectSystem
    
    /// Creates a new EffectSystem with the following parameters -
    ///   - localTime: The time basis of the effect.
    ///   - absolute: A flag indicating if the effect is absolute.
    ///   - presence: The presence of the effect.
    ///   - shadowOffset: How far to offset shadows of any billboards.
    ///   - clipOpt: Optional view scissor clipping.
    ///   - renderType: The render type of the effect.
    ///   - globalEnv: The global environment for the effect.
    let make localTime absolute castShadow presence clipOpt shadowOffset renderType globalEnv =
        { EffectTime = localTime
          EffectTimeOriginal = localTime
          EffectProgressOffset = 0.0f
          EffectAbsolute = absolute
          EffectCastShadow = castShadow
          EffectPresence = presence
          EffectClipOpt = clipOpt
          EffectShadowOffset = shadowOffset
          EffectRenderType = renderType
          EffectDataTokens = SList.make ()
          EffectEnv = globalEnv }

/// Evaluates effect descriptors.
type EffectSystem = EffectSystem.EffectSystem