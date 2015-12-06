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
          TickRate : int64
          TickTime : int64
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
            match Vmap.tryFind resourceName env with
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
            match Vmap.tryFind aspectName env with
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
        | Color _ -> Right aspect

    let rec expandContent (env : Definitions) (content : Content) : Either<string, Content> =
        match content with
        | ExpandContent (contentName, arguments) ->
            match Vmap.tryFind contentName env with
            | Some definition ->
                match definition with
                | AsContent (parameters, content) ->
                    let localDefinitions = List.map expandArgument arguments
                    match (try List.zip parameters localDefinitions |> Some with _ -> None) with
                    | Some localDefinitionEntries ->
                        let env = Vmap.addMany localDefinitionEntries env
                        expandContent env content
                    | None -> Left "Wrong number of arguments provided to ExpandContent."
                | AsPlayback _ -> Left "Expected Content argument but received Playback."
                | AsResource _ -> Left "Expected Content argument but received Resource."
                | AsAspect _ -> Left "Expected Content argument but received Aspect."
            | None -> Left "Expected Content argument but received none."
        | StaticSprite _
        | AnimatedSprite _
        | PhysicsShape _
        | Composite _
        | Mount _
        | Repeat _
        | Emit _
        | Bone -> Right content

    let rec private selectNodes2 localTime (nodes : INode list) =
        match nodes with
        | [] -> failwithumf ()
        | head :: tail ->
            let nodeLocalTime = head.NodeLength - localTime
            if nodeLocalTime >= head.NodeLength then // TODO: ensure this is supposed to be (>=)
                match tail with
                | [] -> (nodeLocalTime, head, head)
                | _ :: _ -> selectNodes2 localTime tail
            else
                match tail with
                | [] -> (nodeLocalTime, head, head)
                | next :: _ -> (nodeLocalTime, head, next)

    let private selectNodes<'n when 'n :> INode> localTime (nodes : 'n list) =
        nodes |>
        List.map (fun node -> node :> INode) |>
        selectNodes2 localTime |>
        fun (fst, snd, thd) -> (fst, snd :?> 'n, thd :?> 'n)

    let inline private tween (scale : (^a * single) -> ^a) (value : ^a) (value2 : ^a) (progress : single) algorithm effector =
        let progress = progress + effector.ProgressOffset
        let progress = if progress > 1.0f then progress - 1.0f else progress
        match algorithm with
        | Const -> value2
        | Linear -> scale (value2 - value, progress)
        | Random ->
            // NOTE: random is deterministically based on progress
            let rand = Rand.makeFromInt ^ int ^ double progress * double Int32.MaxValue
            let randValue = fst ^ Rand.nextSingle rand
            scale (value2 - value, randValue)
        | Chaos ->
            // NOTE: chaos in non-deterministically based on progress
            let chaosValue = single ^ effector.Chaos.NextDouble ()
            scale (value2 - value, chaosValue)
        | Ease ->
            let progressEaseIn = single ^ Math.Pow (Math.Sin (Math.PI * double progress * 0.5), 2.0)
            scale (value2 - value, progressEaseIn)
        | Sin ->
            let progressScaled = float progress * Math.PI * 2.0
            let progressScaledSin = Math.Sin progressScaled
            let progressPolar = progressScaledSin / (Math.PI * 2.0)
            scale (value2 - value, single progressPolar)
        | Cos ->
            let progressScaled = float progress * Math.PI * 2.0
            let progressScaledCos = Math.Cos progressScaled
            let progressPolar = progressScaledCos / (Math.PI * 2.0)
            scale (value2 - value, single progressPolar)

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
        | Diff -> value2 - value
        | Scale -> scale (value, value2)
        | Ratio -> ratio (value, value2)
        | TweenApplicator.Put -> value2

    let private evalInset (celSize : Vector2i) celRun celCount stutter effector =
        let cel = int (effector.TickTime / stutter) % celCount
        let celI = cel % celRun
        let celJ = cel / celRun
        let celX = celI * celSize.X
        let celY = celJ * celSize.Y
        let celPosition = Vector2 (single celX, single celY)
        let celSize = Vector2 (single celSize.X, single celSize.Y)
        Math.makeBounds celPosition celSize

    let rec private iterateArtifacts slice incrementers content artifacts effector =
        List.fold
            (fun (slice, artifacts') incrementer ->
                let effector = { effector with ProgressOffset = 0.0f }
                let slice = evalAspect slice incrementer effector
                let artifacts'' = evalContent slice content effector
                (slice, artifacts'' @ artifacts')) 
            (slice, artifacts)
            incrementers |>
        snd

    and private cycleArtifacts slice incrementers content artifacts effector =
        List.fold
            (fun artifacts' incrementer ->
                let effector = { effector with ProgressOffset = 0.0f }
                let slice = evalAspect slice incrementer effector
                let artifacts'' = evalContent slice content effector
                (artifacts'' @ artifacts')) 
            artifacts
            incrementers

    and private evalAspect slice aspect effector =
        match aspect with
        | ExpandAspect _ -> failwithumf ()
        | Visible (applicator, nodes) ->
            let (_, _, node) = selectNodes effector.TickTime nodes
            let applied = applyLogic true node.LogicValue applicator
            { slice with Visible = applied }
        | Enabled (applicator, nodes) ->
            let (_, _, node) = selectNodes effector.TickTime nodes
            let applied = applyLogic true node.LogicValue applicator
            { slice with Enabled = applied }
        | Position (applicator, algorithm, nodes) ->
            let (nodeTime, node, node2) = selectNodes effector.TickTime nodes
            let progress = single nodeTime / single node.TweenLength
            let tweened = tween Vector2.op_Multiply node.TweenValue node2.TweenValue progress algorithm effector
            let applied = applyTween Vector2.Multiply Vector2.Divide slice.Position tweened applicator
            { slice with Position = applied }
        | Size (applicator, algorithm, nodes) ->
            let (nodeTime, node, node2) = selectNodes effector.TickTime nodes
            let progress = single nodeTime / single node.TweenLength
            let tweened = tween Vector2.op_Multiply node.TweenValue node2.TweenValue progress algorithm effector
            let applied = applyTween Vector2.Multiply Vector2.Divide slice.Size tweened applicator
            { slice with Size = applied }
        | Rotation (applicator, algorithm, nodes) ->
            let (nodeTime, node, node2) = selectNodes effector.TickTime nodes
            let progress = single nodeTime / single node.TweenLength
            let tweened = tween (fun (x, y) -> x * y) node.TweenValue node2.TweenValue progress algorithm effector
            let applied = applyTween (fun (x, y) -> x * y) (fun (x, y) -> x / y) slice.Rotation tweened applicator
            { slice with Rotation = applied }
        | Depth (applicator, algorithm, nodes) ->
            let (nodeTime, node, node2) = selectNodes effector.TickTime nodes
            let progress = single nodeTime / single node.TweenLength
            let tweened = tween (fun (x, y) -> x * y) node.TweenValue node2.TweenValue progress algorithm effector
            let applied = applyTween (fun (x, y) -> x * y) (fun (x, y) -> x / y) slice.Depth tweened applicator
            { slice with Depth = applied }
        | Color (applicator, algorithm, nodes) ->
            let (nodeTime, node, node2) = selectNodes effector.TickTime nodes
            let progress = single nodeTime / single node.TweenLength
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
        | Composite contents ->
            evalComposite slice contents effector
        | Mount (aspects, content) ->
            let slice = evalAspects slice aspects effector
            evalContent slice content effector
        | Repeat (repetition, incrementers, content) ->
            match repetition with
            | Iterate count ->
                List.fold
                    (fun artifacts _ -> iterateArtifacts slice incrementers content artifacts effector)
                    [] [0 .. count - 1]
            | Cycle count ->
                List.fold
                    (fun artifacts i ->
                        let effector = { effector with ProgressOffset = 1.0f / single count * single i }
                        cycleArtifacts slice incrementers content artifacts effector)
                    [] [0 .. count - 1]
        | Emit (Rate rate, aspects, content) ->
            // NOTE: the Enabled field turns emission on and off
            let artifactLists =
                List.mapi
                    (fun i slice ->
                        // the following is actually wrong; the history of the tick rate also needs to be accounted for
                        let effector = { effector with TickTime = effector.TickTime + effector.TickRate * int64 i }
                        let artifacts =
                            List.fold
                                (fun artifacts _ ->
                                    let slice = evalAspects slice aspects effector
                                    let artifacts' = evalContent slice content effector
                                    artifacts' @ artifacts)
                                []
                                [0 .. int rate - 1]
                        artifacts)
                    (slice :: effector.History)
            List.concat artifactLists
        | Bone ->
            [] // TODO: implement

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
            | Some lifetime -> effector.TickTime % lifetime
            | None -> effector.TickTime
        let env = Vmap.concat globalEnv effect.Definitions
        match expandContent env effect.Content with
        | Right content ->
            let effector = { effector with TickTime = localTime }
            let artifacts = evalContent slice content effector
            Right artifacts
        | Left error -> Left error

    let make viewType history tickRate tickTime = 
        { ViewType = viewType
          History = history
          HistoryLength = List.length history
          ProgressOffset = 0.0f
          TickRate = tickRate
          TickTime = tickTime
          Chaos = System.Random () }