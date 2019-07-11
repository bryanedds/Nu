// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open Prime
module Effects =

    type [<Struct>] Algorithm =
        | Const
        | Linear
        | Random
        | Chaos
        | Ease
        | EaseIn
        | EaseOut
        | Sin
        | Cos

    type [<Struct>] LogicApplicator =
        | Or
        | Nor
        | Xor
        | And
        | Nand
        | Equal

    type [<Struct>] TweenApplicator =
        | Sum
        | Delta
        | Scale
        | Ratio
        | Set

    type [<Struct; NoComparison>] Slice =
        { Position : Vector2
          Size : Vector2
          Rotation : single
          Depth : single
          Offset : Vector2
          Color : Vector4
          Volume : single
          Enabled : bool }

    type KeyFrame =
        abstract KeyFrameLength : int64

    type [<Struct>] LogicKeyFrame =
        { LogicValue : bool
          LogicLength : int64 }
        interface KeyFrame with
            member this.KeyFrameLength = this.LogicLength

    type [<Struct>] TweenKeyFrame =
        { TweenValue : single
          TweenLength : int64 }
        interface KeyFrame with
            member this.KeyFrameLength = this.TweenLength

    type [<Struct; NoComparison>] Tween2KeyFrame =
        { TweenValue : Vector2
          TweenLength : int64 }
        interface KeyFrame with
            member this.KeyFrameLength = this.TweenLength

    type [<Struct; NoComparison>] Tween3KeyFrame =
        { TweenValue : Vector3
          TweenLength : int64 }
        interface KeyFrame with
            member this.KeyFrameLength = this.TweenLength

    type [<Struct; NoComparison>] Tween4KeyFrame =
        { TweenValue : Vector4
          TweenLength : int64 }
        interface KeyFrame with
            member this.KeyFrameLength = this.TweenLength

    type [<Struct>] TweenIKeyFrame =
        { TweenValue : int
          TweenLength : int64 }
        interface KeyFrame with
            member this.KeyFrameLength = this.TweenLength

    type [<Struct>] Tween2IKeyFrame =
        { TweenValue : Vector2i
          TweenLength : int64 }
        interface KeyFrame with
            member this.KeyFrameLength = this.TweenLength
            
    type [<Struct>] Playback =
        | Once
        | Loop
        | Bounce
        
    type [<Struct>] Repetition =
        | Cycle of Cycles : int
        | Iterate of Iterations : int

    type [<Struct>] Rate =
        Rate of single

    type [<Struct>] Shift =
        Shift of single

    type [<NoComparison>] Resource =
        | Resource of string * string
        | Expand of string * Argument array

    and [<NoComparison>] Aspect =
        | Enabled of LogicApplicator * Playback * LogicKeyFrame array
        | Position of TweenApplicator * Algorithm * Playback * Tween2KeyFrame array
        | Translation of TweenApplicator * Algorithm * Playback * Tween2KeyFrame array
        | Offset of TweenApplicator * Algorithm * Playback * Tween2KeyFrame array
        | Size of TweenApplicator * Algorithm * Playback * Tween2KeyFrame array
        | Rotation of TweenApplicator * Algorithm * Playback * TweenKeyFrame array
        | Depth of TweenApplicator * Algorithm * Playback * TweenKeyFrame array
        | Color of TweenApplicator * Algorithm * Playback * Tween4KeyFrame array
        | Volume of TweenApplicator * Algorithm * Playback * TweenKeyFrame array
        | Bone // TODO: implement bone aspect
        | Expand of string * Argument array

    and [<NoComparison>] Content =
        | Nil // first to make default value when missing
        | Tag of string * Symbol
        | StaticSprite of Resource * Aspect array * Content
        | AnimatedSprite of Resource * Vector2i * int * int * int64 * Aspect array * Content
        | SoundEffect of Resource * Aspect array * Content
        | Mount of Shift * Aspect array * Content
        | Repeat of Shift * Repetition * Aspect array * Content
        | Emit of Shift * Rate * Aspect array * Aspect array * Content
        | Composite of Shift * Content array
        | Expand of string * Argument array

    and Argument =
        SymbolicCompression<Resource, SymbolicCompression<Aspect, Content>>

    type [<NoComparison>] Definition =
        { DefinitionParams : string array
          DefinitionBody : SymbolicCompression<Resource, SymbolicCompression<Aspect, Content>> }

    type [<Struct; NoComparison>] RenderArtifact =
        RenderArtifact of RenderDescriptor

    type [<Struct; NoComparison>] SoundArtifact =
        SoundArtifact of single * Audio AssetTag

    type [<Struct; NoComparison>] TagArtifact =
        TagArtifact of string * Symbol * Slice

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
            "", "", "", "",
            Constants.PrettyPrinter.DefaultThresholdMin,
            Constants.PrettyPrinter.CompositionalThresholdMax)>]
type [<NoEquality; NoComparison>] Effect =
    { EffectName : string
      LifetimeOpt : int64 option
      Definitions : Effects.Definitions
      Content : Effects.Content }

    static member empty =
        { EffectName = Constants.Engine.DefaultEffectName
          LifetimeOpt = None
          Definitions = Map.empty
          Content = Effects.Composite (Effects.Shift 0.0f, [||]) }