namespace Nu
open Prime
open OpenTK

type Algorithm =
    | Constant
    | Linear
    | EaseIn
    | EaseOut
    | Ease

type FlipApplicator =
    | Or
    | Nor
    | Xor
    | Xnor
    | And
    | Nand
    | Over

type TweenApplicator =
    | Sum
    | Diff
    | Mult
    | Ratio
    | Over

type FlipNode =
    { FlipValue : bool
      FlipLength : int64 }

type UpdateStrNode =
    { FlipValue : string
      FlipLength : int64 }

type TweenNode =
    { TweenValue : single
      TweenLength : int64 }

type [<NoComparison>] Tween2Node =
    { TweenValue : Vector2
      TweenLength : int64 }

type [<NoComparison>] Tween3Node =
    { TweenValue : Vector3
      TweenLength : int64 }

type [<NoComparison>] Tween4Node =
    { TweenValue : Vector4
      TweenLength : int64 }

type TweenINode =
    { TweenValue : int
      TweenLength : int64 }

type Tween2INode =
    { TweenValue : Vector2i
      TweenLength : int64 }

type Playback =
    | Once
    | Loop of int64
    | Bounce of int64

type Resource =
    | ExpandResource of string
    | Resource of string * string

type [<NoComparison>] Gesture =
    | ExpandGesture of string
    | UpdateStr of string * UpdateStrNode list
    | Flip of string * FlipApplicator * FlipNode list
    | Tween of string * TweenApplicator * Algorithm * TweenNode list
    | Tween2 of string * TweenApplicator * Algorithm * Tween2Node list
    | Tween3 of string * TweenApplicator * Algorithm * Tween3Node list
    | Tween4 of string * TweenApplicator * Algorithm * Tween4Node list
    | Mount of Animation
    | Emit // TODO

and [<NoComparison>] Animation =
    | ExpandAnimation of string * Argument list
    | StaticSprite of Resource * Gesture list
    | AnimatedSprite of Resource * Vector2i * Vector2i * int * Gesture list
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
    | RenderRealization of RenderDescriptor list
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
        | UpdateStr _ -> Right gesture
        | Flip _ -> Right gesture
        | Tween _ -> Right gesture
        | Tween2 _ -> Right gesture
        | Tween3 _ -> Right gesture
        | Tween4 _ -> Right gesture
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

    let eval (time : int64) (globalEnv : Definitions) (effect : Effect) : string option * Realization list =
        match Animation.eval (effect.Definitions @@ globalEnv) effect.Animation with
        | Right animation ->
            ignore (time, animation)
            failwith ""
        | Left error ->
            (Some error, [])

    let empty =
        { Name = "Empty"
          Playback = Once
          Lifetime = 0L
          Definitions = Map.empty
          Animation = Container [] }