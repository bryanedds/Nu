namespace Nu
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
      FlipLength : int }

type UpdateNodeNode =
    { FlipValue : bool
      FlipLength : string }

type TweenNode =
    { TweenValue : single
      TweenLength : int }

type [<NoComparison>] Tween2Node =
    { TweenValue : Vector2
      TweenLength : int }

type [<NoComparison>] Tween3Node =
    { TweenValue : Vector3
      TweenLength : int }

type [<NoComparison>] Tween4Node =
    { TweenValue : Vector4
      TweenLength : int }

type TweenINode =
    { TweenValue : int
      TweenLength : int }

type Tween2INode =
    { TweenValue : Vector2i
      TweenLength : int }

type Playback =
    | Once
    | Loop of int
    | Bounce of int

type Resource =
    | Resource of string * string
    | ExpandResource of string * unit list

type [<NoComparison>] Gesture =
    | ExpandGesture of string
    | Update of string * UpdateNodeNode list
    | Flip of string * FlipApplicator * FlipNode list
    | Tween of string * TweenApplicator * Algorithm * TweenNode list
    | Tween2 of string * TweenApplicator * Algorithm * Tween2Node list
    | Tween3 of string * TweenApplicator * Algorithm * Tween3Node list
    | Tween4 of string * TweenApplicator * Algorithm * Tween4Node list
    | Mount of Animation
    | Emit // TODO

and [<NoComparison>] Animation =
    | ExpandAnimation of string * Argument list
    | Container of Gesture list
    | StaticSprite of Resource * Gesture list
    | AnimatedSprite of Resource * Vector2i * Vector2i * int * Gesture list

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

type [<NoComparison>] Effect =
    { Name : string
      Playback : Playback
      Lifetime : int
      Definitions : Map<string, Definition>
      Animation : Animation }