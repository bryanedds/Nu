namespace Nu
open System
open Prime
open OpenTK

type Algorithm =
    | Constant
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
      Offset : Vector2
      Color : Vector4
      Volume : single
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
    | Loop
    | Bounce

type Repetition =
    | Cycle of int
    | Iterate of int

type Rate =
    Rate of single

type Shift =
    Shift of single

type [<NoComparison>] Resource =
    | Expand of string * Argument list
    | Resource of string * string

and [<NoComparison>] Aspect =
    | Expand of string * Argument list
    | Enabled of LogicApplicator * Playback * LogicNode list
    | Position of TweenApplicator * Algorithm * Playback * Tween2Node list
    | Translation of TweenApplicator * Algorithm * Playback * Tween2Node list
    | Offset of TweenApplicator * Algorithm * Playback * Tween2Node list
    | Size of TweenApplicator * Algorithm * Playback * Tween2Node list
    | Rotation of TweenApplicator * Algorithm * Playback * TweenNode list
    | Depth of TweenApplicator * Algorithm * Playback * TweenNode list
    | Color of TweenApplicator * Algorithm * Playback * Tween4Node list
    | Volume of TweenApplicator * Algorithm * Playback * TweenNode list
    | Bone // TODO: implement bone aspect

and [<NoComparison>] Content =
    | Expand of string * Argument list
    | StaticSprite of Resource * Aspect list * Content
    | AnimatedSprite of Resource * Vector2i * int * int * int64 * Aspect list * Content
    | SoundEffect of Resource * Aspect list * Content
    | Mount of Shift * Aspect list * Content
    | Repeat of Shift * Repetition * Aspect list * Content
    | Emit of Shift * Rate * Aspect list * Aspect list * Content
    | Composite of Shift * Content list
    | Tag of string * AlgebraicQuote
    | Nil

and Argument =
    AlgebraicCompression<Resource, AlgebraicCompression<Aspect, Content>>

type [<NoComparison>] Definition =
    { DefinitionParams : string list
      DefinitionBody : AlgebraicCompression<Resource, AlgebraicCompression<Aspect, Content>> }

type [<NoComparison>] EffectArtifact =
    | RenderArtifact of RenderDescriptor list
    | SoundArtifact of single * AssetTag
    | TagArtifact of string * AlgebraicQuote * Slice

type Definitions =
    Map<string, Definition>

type [<NoEquality; NoComparison>] Effect =
    { EffectName : string
      OptLifetime : int64 option
      Definitions : Definitions
      Content : Content }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Effect =

    let empty =
        { EffectName = "Empty"
          OptLifetime = None
          Definitions = Map.empty
          Content = Composite (Shift 0.0f, []) }