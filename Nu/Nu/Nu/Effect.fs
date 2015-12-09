namespace Nu
open System
open Prime
open OpenTK

type Algorithm =
    | Const
    | Linear
    | Random
    | Chaos
    | Ease // TODO: EaseIn and Out
    | Sin
    | Cos

type LogicApplicator =
    | Or
    | Nor
    | Xor
    | And
    | Nand
    | Put

type TweenApplicator =
    | Sum
    | Diff
    | Scale
    | Ratio
    | Put

type [<StructuralEquality; NoComparison>] Slice =
    { Position : Vector2
      Size : Vector2
      Rotation : single
      Depth : single
      Color : Vector4
      Visible : bool
      Enabled : bool }

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

type Repetition =
    | Cycle of int
    | Iterate of int

type Rate =
    Rate of single

type Resource =
    | ExpandResource of string
    | Resource of string * string

type [<NoComparison>] Aspect =
    | ExpandAspect of string
    | Visible of LogicApplicator * LogicNode list
    | Enabled of LogicApplicator * LogicNode list
    | Position of TweenApplicator * Algorithm * Tween2Node list
    | Size of TweenApplicator * Algorithm * Tween2Node list
    | Rotation of TweenApplicator * Algorithm * TweenNode list
    | Depth of TweenApplicator * Algorithm * TweenNode list
    | Color of TweenApplicator * Algorithm * Tween4Node list

and [<NoComparison>] Content =
    | ExpandContent of string * Argument list
    | StaticSprite of Resource * Aspect list * Content
    | AnimatedSprite of Resource * Vector2i * int * int * int64 * Aspect list * Content
    | PhysicsShape of BodyShape * string * string * string * Aspect list * Content
    | Composite of Content list
    | Mount of Aspect list * Content
    | Repeat of Repetition * Aspect list * Content
    | Emit of Rate * Aspect list * Content
    | Bone // TODO

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

type [<NoComparison>] EffectArtifact =
    | RenderArtifact of RenderDescriptor list
    | SoundArtifact of PlaySoundMessage

type Definitions =
    Map<string, Definition>

type [<NoEquality; NoComparison>] Effect =
    { EffectName : string
      Playback : Playback
      OptLifetime : int64 option
      Definitions : Definitions
      Content : Content }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Effect =

    let empty =
        { EffectName = "Empty"
          Playback = Once
          OptLifetime = None
          Definitions = Map.empty
          Content = Composite [] }