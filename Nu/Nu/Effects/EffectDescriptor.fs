// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Effects
open System
open System.Numerics
open Prime
open Nu

/// Logical operations that can be applied to an effect behavior.
type [<Struct>] LogicApplicator =
    | Or
    | Nor
    | Xor
    | And
    | Nand
    | Equal

/// Algorithms for tweening (interpolating) effect behavior.
type [<Struct>] TweenAlgorithm =
    | Constant
    | Linear
    | Random
    | Chaos
    | Ease
    | EaseIn
    | EaseOut
    | Sin
    | SinScaled of Scalar : single
    | Cos
    | CosScaled of Scalar : single

/// The manners in which to apply tweening to effect values.
type [<Struct>] TweenApplicator =
    | Sum
    | Delta
    | Scalar
    | Ratio
    | Modulo
    | Pow
    | Set

/// A snapshot of an active piece of effect content.
type Slice =
    { SliceDelta : GameTime
      SliceTime : GameTime
      mutable Position : Vector3
      mutable Scale : Vector3
      mutable Offset : Vector3
      mutable Size : Vector3
      mutable Angles : Vector3
      mutable Elevation : single
      mutable Inset : Box2
      mutable Color : Color
      mutable Blend : Blend
      mutable Emission : Color
      mutable Height : single
      mutable IgnoreLightMaps : bool
      mutable Flip : Flip
      mutable Brightness : single
      mutable LightCutoff : single
      mutable Volume : single
      mutable Enabled : bool }
    static member copy slice =
        { slice with SliceDelta = slice.SliceDelta }

/// An effect key frame with abstract properties.
type KeyFrame =
    abstract KeyFrameLength : GameTime

/// An effect key frame used for logic values.
type LogicKeyFrame =
    { LogicValue : bool
      LogicLength : GameTime }
    interface KeyFrame with
        member this.KeyFrameLength = this.LogicLength

/// An effect key frame used for tweening single values.
type TweenKeyFrame =
    { TweenValue : single
      TweenLength : GameTime }
    interface KeyFrame with
        member this.KeyFrameLength = this.TweenLength

/// An effect key frame used for tweening Vector2 values.
type Tween2KeyFrame =
    { TweenValue : Vector2
      TweenLength : GameTime }
    interface KeyFrame with
        member this.KeyFrameLength = this.TweenLength

/// An effect key frame used for tweening Vector3 values.
type Tween3KeyFrame =
    { TweenValue : Vector3
      TweenLength : GameTime }
    interface KeyFrame with
        member this.KeyFrameLength = this.TweenLength

/// An effect key frame used for tweening Vector4 values.
type Tween4KeyFrame =
    { TweenValue : Vector4
      TweenLength : GameTime }
    interface KeyFrame with
        member this.KeyFrameLength = this.TweenLength

/// An effect key frame used for tweening Box2 values.
type TweenBox2KeyFrame =
    { TweenValue : Box2
      TweenLength : GameTime }
    interface KeyFrame with
        member this.KeyFrameLength = this.TweenLength

/// An effect key frame used for tweening Color values.
type TweenCKeyFrame =
    { TweenValue : Color
      TweenLength : GameTime }
    interface KeyFrame with
        member this.KeyFrameLength = this.TweenLength

/// An effect key frame used for tweening integer values.
type TweenIKeyFrame =
    { TweenValue : int
      TweenLength : GameTime }
    interface KeyFrame with
        member this.KeyFrameLength = this.TweenLength

/// An effect key frame used for tweening Vector2i values.
type Tween2IKeyFrame =
    { TweenValue : Vector2i
      TweenLength : GameTime }
    interface KeyFrame with
        member this.KeyFrameLength = this.TweenLength

/// Represents a rate of progress for an effect behavior.
type [<Struct>] Rate =
    Rate of Rate : single

/// Represents a shift (offset) of an effect value.
type [<Struct>] Shift =
    Shift of Shift : single

/// Represents a resource used in effect content.
type Resource =
    | Resource of string * string
    | Expand of string * Argument array

/// An aspect (property) of a piece of effect content.
and Aspect =
    | Enabled of bool
    | PositionAbsolute of Vector3
    | Position of Vector3
    | PositionLocal of Vector3
    | Scale of Vector3
    | Offset of Vector3
    | Angles of Vector3
    | Degrees of Vector3
    | Size of Vector3
    | Elevation of single
    | Inset of Box2
    | Color of Color
    | Blend of Blend
    | Emission of Color
    | Height of single
    | IgnoreLightMaps of bool
    | Flip of Flip
    | Brightness of single
    | LightCutoff of single
    | Volume of single
    | Enableds of Applicator : LogicApplicator * Playback : Playback * KeyFrames : LogicKeyFrame array
    | Positions of Applicator : TweenApplicator * Algorithm : TweenAlgorithm * Playback : Playback * KeyFrames : Tween3KeyFrame array
    | PositionLocals of Applicator : TweenApplicator * Algorithm : TweenAlgorithm * Playback : Playback * KeyFrames : Tween3KeyFrame array
    | Scales of Applicator : TweenApplicator * Algorithm : TweenAlgorithm * Playback : Playback * KeyFrames : Tween3KeyFrame array
    | Offsets of Applicator : TweenApplicator * Algorithm : TweenAlgorithm * Playback : Playback * KeyFrames : Tween3KeyFrame array
    | Angleses of Applicator : TweenApplicator * Algorithm : TweenAlgorithm * Playback : Playback * KeyFrames : Tween3KeyFrame array
    | Degreeses of Applicator : TweenApplicator * Algorithm : TweenAlgorithm * Playback : Playback * KeyFrames : Tween3KeyFrame array
    | Sizes of Applicator : TweenApplicator * Algorithm : TweenAlgorithm * Playback : Playback * KeyFrames : Tween3KeyFrame array
    | Elevations of Applicator : TweenApplicator * Algorithm : TweenAlgorithm * Playback : Playback * KeyFrames : TweenKeyFrame array
    | Insets of Applicator : TweenApplicator * Algorithm : TweenAlgorithm * Playback : Playback * KeyFrames : TweenBox2KeyFrame array
    | Colors of Applicator : TweenApplicator * Algorithm : TweenAlgorithm * Playback : Playback * KeyFrames : TweenCKeyFrame array
    | Emissions of Applicator : TweenApplicator * Algorithm : TweenAlgorithm * Playback : Playback * KeyFrames : TweenCKeyFrame array
    | Heights of Applicator : TweenApplicator * Algorithm : TweenAlgorithm * Playback : Playback * KeyFrames : TweenKeyFrame array
    | IgnoreLightMapses of Applicator : LogicApplicator * Playback : Playback * KeyFrames : LogicKeyFrame array
    | Brightnesses of Applicator : TweenApplicator * Algorithm : TweenAlgorithm * Playback : Playback * KeyFrames : TweenKeyFrame array
    | LightCutoffs of Applicator : TweenApplicator * Algorithm : TweenAlgorithm * Playback : Playback * KeyFrames : TweenKeyFrame array
    | Volumes of Applicator : TweenApplicator * Algorithm : TweenAlgorithm * Playback : Playback * KeyFrames : TweenKeyFrame array
    | Expand of Name : string * Args : Argument array
    | Aspects of Aspects : Aspect array

/// Represents types of content used in an effect.
and Content =
    | Nil // first to make default value when missing
    | StaticSprite of Image : Resource * Aspects : Aspect array * Content : Content
    | AnimatedSprite of Image : Resource * CelSize : Vector2i * CelCount : int * CelRun : int * CelDelay : GameTime * Playback : Playback * Aspects : Aspect array * Content : Content
    | TextSprite of Font : Resource * Text : string * FontSizing : int option * FontStyling : FontStyle Set * Aspects : Aspect array * Content : Content
    | Billboard of Albedo : Resource * Roughness : Resource * Metallic : Resource * AmbientOcclusion : Resource * Emission : Resource * Normal : Resource * HeightMap : Resource * TwoSided : bool * Aspects : Aspect array * Content : Content
    | StaticModel of Resource * Aspects : Aspect array * Content : Content
    | Light3d of LightType * Aspects : Aspect array * Content : Content
    | Mount of Shift : Shift * Aspects : Aspect array * Content : Content
    | Repeat of Shift : Shift * Repetition * Aspects : Aspect array * Content : Content
    | Emit of Shift : Shift * Rate : Rate * EmitterAspects : Aspect array * Aspects : Aspect array * Content : Content
    | Tag of Name : string * Aspects : Aspect array * Content : Content
    | Delay of Time : GameTime * Content : Content
    | Segment of StartTime : GameTime * StopTime : GameTime * Content : Content
    | Expand of Name : string * Args : Argument array
    | Contents of Shift : Shift * Contents : Content array

/// Represents an argument used in content definitions.
and Argument =
    SymbolicCompression<Resource, SymbolicCompression<Aspect, Content>>

/// Represents a definition for content elements.
type Definition =
    { DefinitionParams : string array
      DefinitionBody : SymbolicCompression<Resource, SymbolicCompression<Aspect, Content>> }

/// Represents a collection of content variables.
type Definitions =
    Map<string, Definition>

/// Describes an effect in a compositional manner.
[<Syntax
    ("", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.CompositionalThresholdMax)>]
[<SymbolicExpansion>]
type EffectDescriptor =
    { EffectName : string
      LifeTimeOpt : GameTime option
      Definitions : Definitions
      Content : Content }

[<RequireQualifiedAccess>]
module EffectDescriptor =

    /// Combine multiple effect descriptors into one.
    let concat descriptors =
        { EffectName = String.concat "+" (Seq.map (fun descriptor -> descriptor.EffectName) descriptors)
          LifeTimeOpt = None
          Definitions = Seq.fold (fun definitions descriptor -> Map.concat definitions descriptor.Definitions) Map.empty descriptors
          Content = Contents (Shift 0.0f, descriptors |> Seq.map (fun descriptor -> descriptor.Content) |> Array.ofSeq) }

    /// Make an effect descriptor.
    let make name lifeTimeOpt definitions content =
        { EffectName = name
          LifeTimeOpt = lifeTimeOpt
          Definitions = definitions
          Content = content }

    /// The default effect descriptor.
    let defaultDescriptor = make Constants.Engine.EffectNameDefault None Map.empty (Contents (Shift 0.0f, [||]))

    /// The empty effect descriptor.
    let empty = make String.Empty None Map.empty (Contents (Shift 0.0f, [||]))