namespace Nu
open System
open Prime
open OpenTK

type [<NoEquality; NoComparison>] Effector =
    private
        { ViewType : ViewType
          History : Slice list
          HistoryLength : int
          ProgressOffset : single
          EffectRate : int64
          EffectTime : int64
          Chaos : System.Random }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Effector =

    let expandArgument (argument : Argument) : Definition =
        match argument with
        | PassPlayback playback -> AsPlayback playback
        | PassResource resource -> AsResource resource
        | PassAspect aspect -> AsAspect aspect
        | PassContent content -> AsContent ([], content)

    let rec expandResource (env : Definitions) (resource : Resource) : Either<string, Resource> =
        match resource with
        | ExpandResource resourceName ->
            match Map.tryFind resourceName env with
            | Some resource ->
                match resource with
                | AsResource resource -> expandResource env resource
                | AsPlayback _ -> Left "Expected Resource argument but received Playback."
                | AsAspect _ -> Left "Expected Resource argument but received Aspect."
                | AsContent _ -> Left "Expected Resource argument but received Content."
            | None -> Left "Expected Content argument but received none."
        | Resource _ -> Right resource

    let rec expandAspect (env : Definitions) (aspect : Aspect) : Either<string, Aspect> =
        match aspect with
        | ExpandAspect aspectName ->
            match Map.tryFind aspectName env with
            | Some aspect ->
                match aspect with
                | AsAspect aspect -> expandAspect env aspect
                | AsPlayback _ -> Left "Expected Aspect argument but received Playback."
                | AsResource _ -> Left "Expected Aspect argument but received Resource."
                | AsContent _ -> Left "Expected Aspect argument but received Content."
            | None -> Left "Expected Content argument but received none."
        | Visible _
        | Enabled _
        | Position _
        | Size _
        | Rotation _
        | Depth _
        | Offset _
        | Color _ -> Right aspect

    let rec expandContent (env : Definitions) (content : Content) : Either<string, Content> =
        match content with
        | ExpandContent (contentName, arguments) ->
            match Map.tryFind contentName env with
            | Some definition ->
                match definition with
                | AsContent (parameters, content) ->
                    let localDefinitions = List.map expandArgument arguments
                    match (try List.zip parameters localDefinitions |> Some with _ -> None) with
                    | Some localDefinitionEntries ->
                        let env = Map.addMany localDefinitionEntries env
                        expandContent env content
                    | None -> Left "Wrong number of arguments provided to ExpandContent."
                | AsPlayback _ -> Left "Expected Content argument but received Playback."
                | AsResource _ -> Left "Expected Content argument but received Resource."
                | AsAspect _ -> Left "Expected Content argument but received Aspect."
            | None -> Left "Expected Content argument but received none."
        | StaticSprite _
        | AnimatedSprite _
        | PhysicsShape _
        | Tag _
        | Mount _
        | Repeat _
        | Emit _
        | Bone
        | Composite _
        | End -> Right content

    let rec private selectNodes2 localTime playback (nodes : INode list) =
        match playback with
        | Once ->
            match nodes with
            | [] -> failwithumf ()
            | head :: [] -> (localTime, head, head)
            | head :: next :: tail ->
                if localTime > head.NodeLength then
                    match tail with
                    | _ :: _ -> selectNodes2 (localTime - head.NodeLength) playback (next :: tail)
                    | [] -> (head.NodeLength, head, next)
                else (localTime, head, next)
        | Loop ->
            let totalTime = List.fold (fun totalTime (node : INode) -> totalTime + node.NodeLength) 0L nodes 
            let moduloTime = localTime % totalTime
            selectNodes2 moduloTime Once nodes
        | Bounce ->
            let totalTime = List.fold (fun totalTime (node : INode) -> totalTime + node.NodeLength) 0L nodes 
            let moduloTime = localTime % totalTime
            let bouncing = localTime / totalTime % 2L = 1L
            let bounceTime = if bouncing then totalTime - moduloTime else moduloTime
            selectNodes2 bounceTime Once nodes

    let private selectNodes<'n when 'n :> INode> localTime playback (nodes : 'n list) =
        nodes |>
        List.map (fun node -> node :> INode) |>
        selectNodes2 localTime playback |>
        fun (fst, snd, thd) -> (fst, snd :?> 'n, thd :?> 'n)

    let inline private tween (scale : (^a * single) -> ^a) (value : ^a) (value2 : ^a) progress algorithm effector =
        match algorithm with
        | Constant ->
            value2
        | Linear ->
            value + scale (value2 - value, progress)
        | Random ->
            let rand = Rand.makeFromInt ^ int ^ (Math.Max (double progress, 0.000000001)) * double Int32.MaxValue
            let randValue = fst ^ Rand.nextSingle rand
            value + scale (value2 - value, randValue)
        | Chaos ->
            let chaosValue = single ^ effector.Chaos.NextDouble ()
            value + scale (value2 - value, chaosValue)
        | Ease ->
            let progressEaseIn = single ^ Math.Pow (Math.Sin (Math.PI * double progress * 0.5), 2.0)
            value + scale (value2 - value, progressEaseIn)
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
        | LogicApplicator.Put -> value2

    let inline private applyTween scale ratio (value : ^a) (value2 : ^a) applicator =
        match applicator with
        | Sum -> value + value2
        | Diff -> value - value2
        | Scale -> scale (value, value2)
        | Ratio -> ratio (value, value2)
        | TweenApplicator.Put -> value2

    let private evalInset (celSize : Vector2i) celRun celCount stutter effector =
        let cel = int (effector.EffectTime / stutter) % celCount
        let celI = cel % celRun
        let celJ = cel / celRun
        let celX = celI * celSize.X
        let celY = celJ * celSize.Y
        let celPosition = Vector2 (single celX, single celY)
        let celSize = Vector2 (single celSize.X, single celSize.Y)
        Math.makeBounds celPosition celSize

    let rec private iterateArtifacts slice incrementers content effector =
        let effector = { effector with ProgressOffset = 0.0f }
        let slice = evalAspects slice incrementers effector
        (slice, evalContent slice content effector)

    and private cycleArtifacts slice incrementers content effector =
        let slice = evalAspects slice incrementers effector
        evalContent slice content effector

    and private evalProgress nodeTime nodeLength effector =
        let progress = if nodeLength = 0L then 1.0f else single nodeTime / single nodeLength
        let progress = progress + effector.ProgressOffset
        if progress > 1.0f then progress - 1.0f else progress

    and private evalAspect slice aspect effector =
        match aspect with
        | ExpandAspect _ -> failwithumf ()
        | Visible (applicator, playback, nodes) ->
            let (_, _, node) = selectNodes effector.EffectTime playback nodes
            let applied = applyLogic slice.Visible node.LogicValue applicator
            { slice with Visible = applied }
        | Enabled (applicator, playback, nodes) ->
            let (_, _, node) = selectNodes effector.EffectTime playback nodes
            let applied = applyLogic slice.Enabled node.LogicValue applicator
            { slice with Enabled = applied }
        | Position (applicator, algorithm, playback, nodes) ->
            let (nodeTime, node, node2) = selectNodes effector.EffectTime playback nodes
            let progress = evalProgress nodeTime node.TweenLength effector
            let tweened = tween Vector2.op_Multiply node.TweenValue node2.TweenValue progress algorithm effector
            let applied = applyTween Vector2.Multiply Vector2.Divide slice.Position tweened applicator
            { slice with Position = applied }
        | Size (applicator, algorithm, playback, nodes) ->
            let (nodeTime, node, node2) = selectNodes effector.EffectTime playback nodes
            let progress = evalProgress nodeTime node.TweenLength effector
            let tweened = tween Vector2.op_Multiply node.TweenValue node2.TweenValue progress algorithm effector
            let applied = applyTween Vector2.Multiply Vector2.Divide slice.Size tweened applicator
            { slice with Size = applied }
        | Rotation (applicator, algorithm, playback, nodes) ->
            let (nodeTime, node, node2) = selectNodes effector.EffectTime playback nodes
            let progress = evalProgress nodeTime node.TweenLength effector
            let tweened = tween (fun (x, y) -> x * y) node.TweenValue node2.TweenValue progress algorithm effector
            let applied = applyTween (fun (x, y) -> x * y) (fun (x, y) -> x / y) slice.Rotation tweened applicator
            { slice with Rotation = applied }
        | Depth (applicator, algorithm, playback, nodes) ->
            let (nodeTime, node, node2) = selectNodes effector.EffectTime playback nodes
            let progress = evalProgress nodeTime node.TweenLength effector
            let tweened = tween (fun (x, y) -> x * y) node.TweenValue node2.TweenValue progress algorithm effector
            let applied = applyTween (fun (x, y) -> x * y) (fun (x, y) -> x / y) slice.Depth tweened applicator
            { slice with Depth = applied }
        | Offset (applicator, algorithm, playback, nodes) ->
            let (nodeTime, node, node2) = selectNodes effector.EffectTime playback nodes
            let progress = evalProgress nodeTime node.TweenLength effector
            let tweened = tween Vector2.op_Multiply node.TweenValue node2.TweenValue progress algorithm effector
            let applied = applyTween Vector2.Multiply Vector2.Divide slice.Size tweened applicator
            { slice with Offset = applied }
        | Color (applicator, algorithm, playback, nodes) ->
            let (nodeTime, node, node2) = selectNodes effector.EffectTime playback nodes
            let progress = evalProgress nodeTime node.TweenLength effector
            let tweened = tween Vector4.op_Multiply node.TweenValue node2.TweenValue progress algorithm effector
            let applied = applyTween Vector4.Multiply Vector4.Divide slice.Color tweened applicator
            { slice with Color = applied }

    and private evalAspects slice aspects effector =
        List.fold
            (fun slice aspect -> evalAspect slice aspect effector)
            slice
            aspects

    and private evalStaticSprite slice resource aspects content effector =

        // pull image from resource
        let image =
            match resource with
            | ExpandResource _ -> failwithumf ()
            | Resource (packageName, assetName) -> { PackageName = packageName; AssetName = assetName }

        // eval aspects
        let slice = evalAspects slice aspects effector

        // build sprite artifacts
        let spriteArtifacts =
            if slice.Visible then
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
                                  ViewType = effector.ViewType
                                  Color = slice.Color }}]]
            else []

        // build implicitly mounted content
        let mountedArtifacts = evalContent slice content effector

        // return artifacts
        mountedArtifacts @ spriteArtifacts

    and private evalAnimatedSprite slice resource celSize celRun celCount stutter aspects content effector =

        // pull image from resource
        let image =
            match resource with
            | ExpandResource _ -> failwithumf ()
            | Resource (packageName, assetName) -> { PackageName = packageName; AssetName = assetName }

        // eval aspects
        let slice = evalAspects slice aspects effector

        // eval inset
        let inset = evalInset celSize celRun celCount stutter effector

        // build animated sprite artifacts
        let animatedSpriteArtifacts =
            if slice.Visible then
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
                                  ViewType = effector.ViewType
                                  Color = slice.Color }}]]
            else []

        // build implicitly mounted content
        let mountedArtifacts = evalContent slice content effector

        // return artifacts
        mountedArtifacts @ animatedSpriteArtifacts

    and private evalComposite slice contents effector =
        evalContents slice contents effector

    and private evalContent slice content effector =
        match content with
        | ExpandContent _ ->
            failwithumf ()
        | StaticSprite (resource, aspects, content) ->
            evalStaticSprite slice resource aspects content effector
        | AnimatedSprite (resource, celSize, celRun, celCount, stutter, aspects, content) ->
            evalAnimatedSprite slice resource celSize celRun celCount stutter aspects content effector
        | PhysicsShape (label, bodyShape, collisionCategories, collisionMask, aspects, content) ->
            ignore (label, bodyShape, collisionCategories, collisionMask, aspects, content); [] // TODO: implement
        | Tag name ->
            [TagArtifact (name, slice)]
        | Mount (Shift shift, aspects, content) ->
            let slice = { slice with Depth = slice.Depth + shift }
            let slice = evalAspects slice aspects effector
            evalContent slice content effector
        | Repeat (Shift shift, repetition, incrementers, content) ->
            let slice = { slice with Depth = slice.Depth + shift }
            match repetition with
            | Iterate count ->
                List.fold
                    (fun (slice, artifacts) _ ->
                        let (slice, artifacts') = iterateArtifacts slice incrementers content effector
                        (slice, artifacts @ artifacts'))
                    (slice, [])
                    [0 .. count - 1] |>
                snd
            | Cycle count ->
                List.fold
                    (fun artifacts i ->
                        let effector = { effector with ProgressOffset = 1.0f / single count * single i }
                        let artifacts' = cycleArtifacts slice incrementers content effector
                        artifacts @ artifacts')
                    [] [0 .. count - 1]
        | Emit (Shift shift, Rate rate, emitterAspects, aspects, content) ->
            let artifacts =
                List.foldi
                    (fun i artifacts (slice : Slice) ->
                        let slice = { slice with Depth = slice.Depth + shift }
                        let slice = evalAspects slice emitterAspects { effector with EffectTime = effector.EffectTime + int64 i * effector.EffectRate }
                        let emitCountLastFrame = single (effector.EffectTime - (int64 i + 1L) * effector.EffectRate) * rate
                        let emitCountThisFrame = single (effector.EffectTime - int64 i * effector.EffectRate) * rate
                        let emitCount = int emitCountThisFrame - int emitCountLastFrame
                        let effector = { effector with EffectTime = int64 i * effector.EffectRate }
                        let artifacts' =
                            List.fold
                                (fun artifacts' _ ->
                                    let slice = evalAspects slice aspects effector
                                    let artifacts'' =
                                        if slice.Enabled
                                        then evalContent slice content effector
                                        else []
                                    artifacts'' @ artifacts')
                                []
                                [0 .. emitCount - 1]
                        artifacts' @ artifacts)
                    []
                    effector.History
            artifacts // temp for debuggability
        | Bone -> [] // TODO: implement
        | Composite (Shift shift, contents) ->
            let slice = { slice with Depth = slice.Depth + shift }
            evalComposite slice contents effector
        | End -> []

    and private evalContents slice contents effector =
        List.fold
            (fun artifacts content ->
                let artifacts' = evalContent slice content effector
                artifacts' @ artifacts)
            []
            contents

    let eval slice (globalEnv : Definitions) (effect : Effect) (effector : Effector) : Either<string, EffectArtifact list> =
        let localTime =
            match effect.OptLifetime with
            | Some lifetime -> effector.EffectTime % lifetime
            | None -> effector.EffectTime
        let env = Map.concat globalEnv effect.Definitions
        match expandContent env effect.Content with
        | Right content ->
            let effector = { effector with EffectTime = localTime }
            let artifacts = evalContent slice content effector
            Right artifacts
        | Left error -> Left error

    let make viewType history tickRate tickTime = 
        { ViewType = viewType
          History = history
          HistoryLength = List.length history
          ProgressOffset = 0.0f
          EffectRate = tickRate
          EffectTime = tickTime
          Chaos = System.Random () }