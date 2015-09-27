namespace Nu
open System
open Prime
open OpenTK

type Algorithm =
    | Constant
    | Linear
    | EaseIn
    | EaseOut
    | Ease

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

type [<StructuralEquality; NoComparison>] RenderSprite =
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

type [<NoComparison>] Gesture =
    | ExpandGesture of string
    | Visible of LogicApplicator * LogicNode list
    | Position of TweenApplicator * Algorithm * Tween2Node list
    | Size of TweenApplicator * Algorithm * Tween2Node list
    | Rotation of TweenApplicator * Algorithm * TweenNode list
    | Depth of TweenApplicator * Algorithm * TweenNode list
    | Color of TweenApplicator * Algorithm * Tween4Node list
    | Mount of Animation
    | Emit // TODO

and [<NoComparison>] Animation =
    | ExpandAnimation of string * Argument list
    | StaticSprite of Resource * Gesture list
    | AnimatedSprite of Resource * Vector2i * int * int * int * Gesture list
    | Container of Gesture list

and [<NoComparison>] Argument =
    | PassPlayback of Playback
    | PassResource of Resource
    | PassGesture of Gesture
    | PassAnimation of Animation

type [<NoComparison>] Definition =
    | AsPlayback of Playback
    | AsResource of Resource
    | AsGesture of Gesture
    | AsAnimation of string list * Animation

type [<NoComparison>] Realization =
    | RenderRealization of RenderDescriptor
    | SoundRealization of PlaySoundMessage
    | SongRealization of PlaySongMessage

type Definitions =
    Map<string, Definition>

type [<NoComparison>] Effect =
    { Name : string
      Playback : Playback
      Lifetime : int64
      Definitions : Definitions
      Animation : Animation }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Argument =

    let eval (argument : Argument) : Definition =
        match argument with
        | PassPlayback playback -> AsPlayback playback
        | PassResource resource -> AsResource resource
        | PassGesture gesture -> AsGesture gesture
        | PassAnimation animation -> AsAnimation ([], animation)

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Resource =

    let rec eval (env : Definitions) (resource : Resource) : Either<string, Resource> =
        match resource with
        | ExpandResource resourceName ->
            match Map.tryFind resourceName env with
            | Some resource ->
                match resource with
                | AsResource resource -> eval env resource
                | AsPlayback _ -> Left "Expected Resource argument but received Playback."
                | AsGesture _ -> Left "Expected Resource argument but received Gesture."
                | AsAnimation _ -> Left "Expected Resource argument but received Animation."
            | None -> Left "Expected Animation argument but received none."
        | Resource _ -> Right resource

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Gesture =

    let rec eval (env : Definitions) (gesture : Gesture) : Either<string, Gesture> =
        match gesture with
        | ExpandGesture gestureName ->
            match Map.tryFind gestureName env with
            | Some gesture ->
                match gesture with
                | AsGesture gesture -> eval env gesture
                | AsPlayback _ -> Left "Expected Gesture argument but received Playback."
                | AsResource _ -> Left "Expected Gesture argument but received Resource."
                | AsAnimation _ -> Left "Expected Gesture argument but received Animation."
            | None -> Left "Expected Animation argument but received none."
        | Visible _ -> Right gesture
        | Position _ -> Right gesture
        | Size _ -> Right gesture
        | Rotation _ -> Right gesture
        | Depth _ -> Right gesture
        | Color _ -> Right gesture
        | Mount _ -> Right gesture
        | Emit -> Right gesture

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Animation =

    let rec eval (env : Definitions) (animation : Animation) : Either<string, Animation> =
        match animation with
        | ExpandAnimation (animationName, arguments) ->
            match Map.tryFind animationName env with
            | Some definition ->
                match definition with
                | AsAnimation (parameters, animation) ->
                    let localDefinitions = List.map Argument.eval arguments
                    match (try List.zip parameters localDefinitions |> Some with _ -> None) with
                    | Some localDefinitionEntries ->
                        let localEnv = Map.ofList localDefinitionEntries
                        let env = env @@ localEnv
                        eval env animation
                    | None -> Left "Wrong number of arguments provided to ExpandAnimation."
                | AsPlayback _ -> Left "Expected Animation argument but received Playback."
                | AsResource _ -> Left "Expected Animation argument but received Resource."
                | AsGesture _ -> Left "Expected Animation argument but received Gesture."
            | None -> Left "Expected Animation argument but received none."
        | StaticSprite _ -> Right animation
        | AnimatedSprite _ -> Right animation
        | Container _ -> Right animation

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
        | EaseIn -> failwith "TODO"
        | EaseOut -> failwith "TODO"
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
        | Diff -> value2 - value // TODO: make sure this is correct difference semantics
        | Scale -> scale (value, value2)
        | Ratio -> ratio (value, value2)
        | TweenApplicator.Over -> value2

    let eval position size rotation depth viewType color (time : int64) (globalEnv : Definitions) (effect : Effect) : string option * Realization list =
        let localTime = time % effect.Lifetime
        match Animation.eval (effect.Definitions @@ globalEnv) effect.Animation with
        | Right animation ->
            match animation with
            | ExpandAnimation _ -> failwithumf ()
            | StaticSprite (resource, gestures) ->

                let image =
                    match resource with
                    | ExpandResource _ -> failwithumf ()
                    | Resource (packageName, assetName) -> { PackageName = packageName; AssetName = assetName }

                let sprite =
                    { Position = position
                      Size = size
                      Rotation = rotation
                      Depth = depth
                      Color = Vector4.One
                      Visible = true }

                let sprite =
                    List.fold
                        (fun sprite gesture ->
                            match gesture with
                            | ExpandGesture _ -> failwithumf ()
                            | Visible (applicator, nodes) ->
                                let (_, _, node) = selectNodes localTime nodes
                                let applied = applyLogic true node.LogicValue applicator
                                { sprite with Visible = applied }
                            | Position (applicator, algorithm, nodes) ->
                                let (nodeLocalTime, node, node2) = selectNodes localTime nodes
                                let progress = single nodeLocalTime / single node.TweenLength
                                let tweened = tween Vector2.op_Multiply node.TweenValue node2.TweenValue progress algorithm
                                let applied = applyTween Vector2.Multiply Vector2.Divide position tweened applicator
                                { sprite with Position = applied }
                            | Size (applicator, algorithm, nodes) ->
                                let (nodeLocalTime, node, node2) = selectNodes localTime nodes
                                let progress = single nodeLocalTime / single node.TweenLength
                                let tweened = tween Vector2.op_Multiply node.TweenValue node2.TweenValue progress algorithm
                                let applied = applyTween Vector2.Multiply Vector2.Divide size tweened applicator
                                { sprite with Size = applied }
                            | Rotation (applicator, algorithm, nodes) ->
                                let (nodeLocalTime, node, node2) = selectNodes localTime nodes
                                let progress = single nodeLocalTime / single node.TweenLength
                                let tweened = tween (fun (x, y) -> x * y) node.TweenValue node2.TweenValue progress algorithm
                                let applied = applyTween (fun (x, y) -> x * y) (fun (x, y) -> x / y) rotation tweened applicator
                                { sprite with Rotation = applied }
                            | Depth (applicator, algorithm, nodes) ->
                                let (nodeLocalTime, node, node2) = selectNodes localTime nodes
                                let progress = single nodeLocalTime / single node.TweenLength
                                let tweened = tween (fun (x, y) -> x * y) node.TweenValue node2.TweenValue progress algorithm
                                let applied = applyTween (fun (x, y) -> x * y) (fun (x, y) -> x / y) depth tweened applicator
                                { sprite with Depth = applied }
                            | Color (applicator, algorithm, nodes) ->
                                let (nodeLocalTime, node, node2) = selectNodes localTime nodes
                                let progress = single nodeLocalTime / single node.TweenLength
                                let tweened = tween Vector4.op_Multiply node.TweenValue node2.TweenValue progress algorithm
                                let applied = applyTween Vector4.Multiply Vector4.Divide color tweened applicator
                                { sprite with Color = applied }
                            | Mount _ -> failwith "Unimplemented."
                            | Emit -> failwith "Unimplemented.")
                        sprite
                        gestures

                let renderRealizations =
                    if sprite.Visible then
                        [RenderRealization ^
                            LayerableDescriptor
                                { Depth = sprite.Depth
                                  LayeredDescriptor =
                                    SpriteDescriptor 
                                        { Position = sprite.Position
                                          Size = sprite.Size
                                          Rotation = sprite.Rotation
                                          OptInset = None
                                          Image = image
                                          ViewType = viewType
                                          Color = sprite.Color }}]
                    else []

                (None, renderRealizations)

            | AnimatedSprite (resource, celSize, celRun, celCount, stutter, gestures) ->
                ignore (resource, celSize, celRun, celCount, stutter, gestures)
                failwith "TODO"

            | Container gestures ->
                ignore gestures
                failwith "TODO"

        | Left error ->
            (Some error, [])

    let empty =
        { Name = "Empty"
          Playback = Once
          Lifetime = 0L
          Definitions = Map.empty
          Animation = Container [] }