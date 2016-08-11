// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open Prime
open OpenTK
module Effects =

    type Algorithm =
        | Const
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
        | Equal

    type TweenApplicator =
        | Sum
        | Delta
        | Scale
        | Ratio
        | Set

    type [<StructuralEquality; NoComparison>] Slice =
        { Position : Vector2
          Size : Vector2
          Rotation : single
          Depth : single
          Offset : Vector2
          Color : Vector4
          Volume : single
          Enabled : bool }

    type IKeyFrame =
        abstract KeyFrameLength : int64

    type LogicKeyFrame =
        { LogicValue : bool
          LogicLength : int64 }
        interface IKeyFrame with
            member this.KeyFrameLength = this.LogicLength

    type TweenKeyFrame =
        { TweenValue : single
          TweenLength : int64 }
        interface IKeyFrame with
            member this.KeyFrameLength = this.TweenLength

    type [<NoComparison>] Tween2KeyFrame =
        { TweenValue : Vector2
          TweenLength : int64 }
        interface IKeyFrame with
            member this.KeyFrameLength = this.TweenLength

    type [<NoComparison>] Tween3KeyFrame =
        { TweenValue : Vector3
          TweenLength : int64 }
        interface IKeyFrame with
            member this.KeyFrameLength = this.TweenLength

    type [<NoComparison>] Tween4KeyFrame =
        { TweenValue : Vector4
          TweenLength : int64 }
        interface IKeyFrame with
            member this.KeyFrameLength = this.TweenLength

    type TweenIKeyFrame =
        { TweenValue : int
          TweenLength : int64 }
        interface IKeyFrame with
            member this.KeyFrameLength = this.TweenLength

    type Tween2IKeyFrame =
        { TweenValue : Vector2i
          TweenLength : int64 }
        interface IKeyFrame with
            member this.KeyFrameLength = this.TweenLength

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
        | Resource of string * string
        | Expand of string * Argument list

    and [<NoComparison>] Aspect =
        | Enabled of LogicApplicator * Playback * LogicKeyFrame list
        | Position of TweenApplicator * Algorithm * Playback * Tween2KeyFrame list
        | Translation of TweenApplicator * Algorithm * Playback * Tween2KeyFrame list
        | Offset of TweenApplicator * Algorithm * Playback * Tween2KeyFrame list
        | Size of TweenApplicator * Algorithm * Playback * Tween2KeyFrame list
        | Rotation of TweenApplicator * Algorithm * Playback * TweenKeyFrame list
        | Depth of TweenApplicator * Algorithm * Playback * TweenKeyFrame list
        | Color of TweenApplicator * Algorithm * Playback * Tween4KeyFrame list
        | Volume of TweenApplicator * Algorithm * Playback * TweenKeyFrame list
        | Bone // TODO: implement bone aspect
        | Expand of string * Argument list

    and [<NoComparison>] Content =
        | Nil // first to make default value when missing
        | Tag of string * Symbol
        | StaticSprite of Resource * Aspect list * Content
        | AnimatedSprite of Resource * Vector2i * int * int * int64 * Aspect list * Content
        | SoundEffect of Resource * Aspect list * Content
        | Mount of Shift * Aspect list * Content
        | Repeat of Shift * Repetition * Aspect list * Content
        | Emit of Shift * Rate * Aspect list * Aspect list * Content
        | Composite of Shift * Content list
        | Expand of string * Argument list

    and Argument =
        SymbolicCompression<Resource, SymbolicCompression<Aspect, Content>>

    type [<NoComparison>] Definition =
        { DefinitionParams : string list
          DefinitionBody : SymbolicCompression<Resource, SymbolicCompression<Aspect, Content>> }

    type [<NoComparison>] Artifact =
        | RenderArtifact of RenderDescriptor list
        | SoundArtifact of single * AssetTag
        | TagArtifact of string * Symbol * Slice

    type Definitions =
        Map<string, Definition>

/// Describes an effect in a compositional manner.
[<Syntax   ("Const Linear Random Chaos Ease EaseIn EaseOut Sin Cos " +
            "Or Nor Xor And Nand Equal " +
            "Sum Delta Scale Ratio Set " +
            "Position Size Rotation Depth Offset Color Volume Enabled " +
            "Once Loop Bounce " +
            "Cycle Iterate " +
            "Rate " +
            "Shift " +
            "Expand Resource " +
            "Expand Enabled Position Translation Offset Size Rotation Depth Color Volume Bone " +
            "Expand StaticSprite AnimatedSprite SoundEffect Mount Repeat Emit Composite Tag Nil " +
            "RenderArtifact SoundArtifact TagArtifact",
            "")>]
type [<NoEquality; NoComparison>] Effect =
    { EffectName : string
      OptLifetime : int64 option
      Definitions : Effects.Definitions
      Content : Effects.Content }

    static member empty =
        { EffectName = "Empty"
          OptLifetime = None
          Definitions = Map.empty
          Content = Effects.Composite (Effects.Shift 0.0f, []) }