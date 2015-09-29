namespace Nu
open System
open Prime
open OpenTK

type Algorithm =
    | Constant
    | Linear
    | Ease // TODO: EaseIn and Out

type LogicApplicator =
    | Or
    | Nor
    | Xor
    | And
    | Nand
    | Over

type TweenApplicator =
    | Sum
    | Diff
    | Scale
    | Ratio
    | Over

type [<StructuralEquality; NoComparison>] Slice =
    { Position : Vector2
      Size : Vector2
      Rotation : single
      Depth : single
      Color : Vector4
      Visible : bool }

type INode =
    abstract NodeLength : int64

type LogicNode =
    { LogicValue : bool
      LogicLength : int64 }
    interface INode with
        member this.NodeLength = this.LogicLength

type TweenNode =
    { TweenValue : single
      TweenLength : int64 }
    interface INode with
        member this.NodeLength = this.TweenLength

type [<NoComparison>] Tween2Node =
    { TweenValue : Vector2
      TweenLength : int64 }
    interface INode with
        member this.NodeLength = this.TweenLength

type [<NoComparison>] Tween3Node =
    { TweenValue : Vector3
      TweenLength : int64 }
    interface INode with
        member this.NodeLength = this.TweenLength

type [<NoComparison>] Tween4Node =
    { TweenValue : Vector4
      TweenLength : int64 }
    interface INode with
        member this.NodeLength = this.TweenLength

type TweenINode =
    { TweenValue : int
      TweenLength : int64 }
    interface INode with
        member this.NodeLength = this.TweenLength

type Tween2INode =
    { TweenValue : Vector2i
      TweenLength : int64 }
    interface INode with
        member this.NodeLength = this.TweenLength

type Playback =
    | Once
    | Loop of int64
    | Bounce of int64

type Resource =
    | ExpandResource of string
    | Resource of string * string

type [<NoComparison>] Aspect =
    | ExpandAspect of string
    | Visible of LogicApplicator * LogicNode list
    | Position of TweenApplicator * Algorithm * Tween2Node list
    | Size of TweenApplicator * Algorithm * Tween2Node list
    | Rotation of TweenApplicator * Algorithm * TweenNode list
    | Depth of TweenApplicator * Algorithm * TweenNode list
    | Color of TweenApplicator * Algorithm * Tween4Node list
    | Mount of Content
    | Emit // TODO
    | Bone // TODO

and [<NoComparison>] Content =
    | ExpandContent of string * Argument list
    | StaticSprite of Resource * Aspect list
    | AnimatedSprite of Resource * Vector2i * int * int * int64 * Aspect list
    | PhysicsShape of BodyShape * string * string * string * Aspect list
    | Composite of Aspect list

and [<NoComparison>] Argument =
    | PassPlayback of Playback
    | PassResource of Resource
    | PassAspect of Aspect
    | PassContent of Content

type [<NoComparison>] Definition =
    | AsPlayback of Playback
    | AsResource of Resource
    | AsAspect of Aspect
    | AsContent of string list * Content

type [<NoComparison>] EffectArtifect =
    | RenderArtifact of RenderDescriptor list
    | SoundArtifact of PlaySoundMessage

type Definitions =
    Map<string, Definition>

type [<NoComparison>] Effect =
    { EffectName : string
      Playback : Playback
      OptLifetime : int64 option
      Definitions : Definitions
      Content : Content }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Argument =

    let expand (argument : Argument) : Definition =
        match argument with
        | PassPlayback playback -> AsPlayback playback
        | PassResource resource -> AsResource resource
        | PassAspect aspect -> AsAspect aspect
        | PassContent content -> AsContent ([], content)

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Resource =

    let rec expand (env : Definitions) (resource : Resource) : Either<string, Resource> =
        match resource with
        | ExpandResource resourceName ->
            match Map.tryFind resourceName env with
            | Some resource ->
                match resource with
                | AsResource resource -> expand env resource
                | AsPlayback _ -> Left "Expected Resource argument but received Playback."
                | AsAspect _ -> Left "Expected Resource argument but received Aspect."
                | AsContent _ -> Left "Expected Resource argument but received Content."
            | None -> Left "Expected Content argument but received none."
        | Resource _ -> Right resource

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Aspect =

    let rec expand (env : Definitions) (aspect : Aspect) : Either<string, Aspect> =
        match aspect with
        | ExpandAspect aspectName ->
            match Map.tryFind aspectName env with
            | Some aspect ->
                match aspect with
                | AsAspect aspect -> expand env aspect
                | AsPlayback _ -> Left "Expected Aspect argument but received Playback."
                | AsResource _ -> Left "Expected Aspect argument but received Resource."
                | AsContent _ -> Left "Expected Aspect argument but received Content."
            | None -> Left "Expected Content argument but received none."
        | Visible _ -> Right aspect
        | Position _ -> Right aspect
        | Size _ -> Right aspect
        | Rotation _ -> Right aspect
        | Depth _ -> Right aspect
        | Color _ -> Right aspect
        | Mount _ -> Right aspect
        | Emit -> Right aspect
        | Bone -> Right aspect

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Content =

    let rec expand (env : Definitions) (content : Content) : Either<string, Content> =
        match content with
        | ExpandContent (contentName, arguments) ->
            match Map.tryFind contentName env with
            | Some definition ->
                match definition with
                | AsContent (parameters, content) ->
                    let localDefinitions = List.map Argument.expand arguments
                    match (try List.zip parameters localDefinitions |> Some with _ -> None) with
                    | Some localDefinitionEntries ->
                        let localEnv = Map.ofList localDefinitionEntries
                        let env = env @@ localEnv
                        expand env content
                    | None -> Left "Wrong number of arguments provided to ExpandContent."
                | AsPlayback _ -> Left "Expected Content argument but received Playback."
                | AsResource _ -> Left "Expected Content argument but received Resource."
                | AsAspect _ -> Left "Expected Content argument but received Aspect."
            | None -> Left "Expected Content argument but received none."
        | StaticSprite _ -> Right content
        | AnimatedSprite _ -> Right content
        | PhysicsShape _ -> Right content
        | Composite _ -> Right content

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Effect =

    let rec selectNodes2 localTime (nodes : INode list) =
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

    let selectNodes<'n when 'n :> INode> localTime (nodes : 'n list) =
        nodes |>
            List.map (fun node -> node :> INode) |>
            selectNodes2 localTime |>
            fun (fst, snd, thd) -> (fst, snd :?> 'n, thd :?> 'n)

    let inline tween (scale : (^a * single) -> ^a) (value : ^a) (value2 : ^a) (progress : single) algorithm =
        match algorithm with
        | Constant -> value2
        | Linear -> scale (value2 - value, progress)
        | Ease ->
            let progressEaseIn = single ^ Math.Pow (Math.Sin (Math.PI * double progress * 0.5), 2.0)
            scale (value2 - value, progressEaseIn)

    let applyLogic value value2 applicator =
        match applicator with
        | Or -> value || value2
        | Nor -> not value && not value2
        | Xor -> value <> value2
        | And -> value && value2
        | Nand -> not (value && value2)
        | LogicApplicator.Over -> value2

    let inline applyTween scale ratio (value : ^a) (value2 : ^a) applicator =
        match applicator with
        | Sum -> value + value2
        | Diff -> value2 - value
        | Scale -> scale (value, value2)
        | Ratio -> ratio (value, value2)
        | TweenApplicator.Over -> value2

    let evalInset (celSize : Vector2i) celRun celCount stutter time =
        let cel = int (time / stutter) % celCount
        let celI = cel % celRun
        let celJ = cel / celRun
        let celX = celI * celSize.X
        let celY = celJ * celSize.Y
        let celPosition = Vector2 (single celX, single celY)
        let celSize = Vector2 (single celSize.X, single celSize.Y)
        Math.makeBounds celPosition celSize

    let rec evalAspects viewType slice history aspects time =
        List.fold
            (fun (slice, artifacts) aspect ->
                match aspect with
                | ExpandAspect _ -> failwithumf ()
                | Visible (applicator, nodes) ->
                    let (_, _, node) = selectNodes time nodes
                    let applied = applyLogic true node.LogicValue applicator
                    ({ slice with Visible = applied }, artifacts)
                | Position (applicator, algorithm, nodes) ->
                    let (nodeTime, node, node2) = selectNodes time nodes
                    let progress = single nodeTime / single node.TweenLength
                    let tweened = tween Vector2.op_Multiply node.TweenValue node2.TweenValue progress algorithm
                    let applied = applyTween Vector2.Multiply Vector2.Divide slice.Position tweened applicator
                    ({ slice with Position = applied }, artifacts)
                | Size (applicator, algorithm, nodes) ->
                    let (nodeTime, node, node2) = selectNodes time nodes
                    let progress = single nodeTime / single node.TweenLength
                    let tweened = tween Vector2.op_Multiply node.TweenValue node2.TweenValue progress algorithm
                    let applied = applyTween Vector2.Multiply Vector2.Divide slice.Size tweened applicator
                    ({ slice with Size = applied }, artifacts)
                | Rotation (applicator, algorithm, nodes) ->
                    let (nodeTime, node, node2) = selectNodes time nodes
                    let progress = single nodeTime / single node.TweenLength
                    let tweened = tween (fun (x, y) -> x * y) node.TweenValue node2.TweenValue progress algorithm
                    let applied = applyTween (fun (x, y) -> x * y) (fun (x, y) -> x / y) slice.Rotation tweened applicator
                    ({ slice with Rotation = applied }, artifacts)
                | Depth (applicator, algorithm, nodes) ->
                    let (nodeTime, node, node2) = selectNodes time nodes
                    let progress = single nodeTime / single node.TweenLength
                    let tweened = tween (fun (x, y) -> x * y) node.TweenValue node2.TweenValue progress algorithm
                    let applied = applyTween (fun (x, y) -> x * y) (fun (x, y) -> x / y) slice.Depth tweened applicator
                    ({ slice with Depth = applied }, artifacts)
                | Color (applicator, algorithm, nodes) ->
                    let (nodeTime, node, node2) = selectNodes time nodes
                    let progress = single nodeTime / single node.TweenLength
                    let tweened = tween Vector4.op_Multiply node.TweenValue node2.TweenValue progress algorithm
                    let applied = applyTween Vector4.Multiply Vector4.Divide slice.Color tweened applicator
                    ({ slice with Color = applied }, artifacts)
                | Mount content ->
                    (slice, evalContent viewType slice history content time @ artifacts)
                | Emit ->
                    // emitted = (max time lifetimes) / stutter
                    //Seq.unfold
                    failwith "Unimplemented."
                | Bone ->
                    (slice, []))
            (slice, [])
            aspects

    and evalStaticSprite viewType slice history resource aspects time =

        // pull image from resource
        let image =
            match resource with
            | ExpandResource _ -> failwithumf ()
            | Resource (packageName, assetName) -> { PackageName = packageName; AssetName = assetName }

        // eval aspects
        let (slice, artifacts) =
            evalAspects viewType slice history aspects time

        // return artifacts
        if slice.Visible then
            let artifact =
                RenderArtifact
                    [LayerableDescriptor
                        { Depth = slice.Depth
                          LayeredDescriptor =
                            SpriteDescriptor 
                                { Position = slice.Position
                                  Size = slice.Size
                                  Rotation = slice.Rotation
                                  OptInset = None
                                  Image = image
                                  ViewType = viewType
                                  Color = slice.Color }}]
            artifact :: artifacts
        else artifacts

    and evalAnimatedSprite viewType slice history resource celSize celRun celCount stutter aspects time =

        // pull image from resource
        let image =
            match resource with
            | ExpandResource _ -> failwithumf ()
            | Resource (packageName, assetName) -> { PackageName = packageName; AssetName = assetName }

        // eval aspects
        let (slice, artifacts) =
            evalAspects viewType slice history aspects time

        // eval inset
        let inset =
            evalInset celSize celRun celCount stutter time

        // return artifacts
        if slice.Visible then
            let artifact =
                RenderArtifact
                    [LayerableDescriptor
                        { Depth = slice.Depth
                          LayeredDescriptor =
                            SpriteDescriptor 
                                { Position = slice.Position
                                  Size = slice.Size
                                  Rotation = slice.Rotation
                                  OptInset = Some inset
                                  Image = image
                                  ViewType = viewType
                                  Color = slice.Color }}]
            artifact :: artifacts
        else artifacts

    and evalComposite viewType slice history aspects time =
        let (_, artifacts) = evalAspects viewType slice history aspects time
        artifacts

    and evalContent viewType slice history content time =
        match content with
        | ExpandContent _ ->
            failwithumf ()
        | StaticSprite (resource, aspects) ->
            evalStaticSprite viewType slice history resource aspects time
        | AnimatedSprite (resource, celSize, celRun, celCount, stutter, aspects) ->
            evalAnimatedSprite viewType slice history resource celSize celRun celCount stutter aspects time
        | PhysicsShape (label, bodyShape, collisionCategories, collisionMask, aspects) ->
            ignore (label, bodyShape, collisionCategories, collisionMask, aspects); failwith "TODO"
        | Composite aspects ->
            evalComposite viewType slice history aspects time

    let eval viewType slice history (globalEnv : Definitions) (effect : Effect) (time : int64) : Either<string, EffectArtifect list> =
        let localTime =
            match effect.OptLifetime with
            | Some lifetime -> time % lifetime
            | None -> time
        match Content.expand (globalEnv @@ effect.Definitions) effect.Content with
        | Right content ->
            let artifacts = evalContent viewType slice history content localTime
            Right artifacts
        | Left error -> Left error

    let empty =
        { EffectName = "Anonymous"
          Playback = Once
          OptLifetime = None
          Definitions = Map.empty
          Content = Composite [] }