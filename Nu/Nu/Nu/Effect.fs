namespace Nu
open OpenTK

type PlaybackValue =
    | Once
    | Loop of int
    | Bounce of int

type Playback =
    | PlaybackValue of PlaybackValue
    | PlaybackVar of string

type App =
    | Or
    | Nor
    | Xor
    | Xnor
    | And
    | Nand
    | Sum
    | Diff
    | Mult
    | Ratio
    | Over

type Resource =
    | Resource of AssetTag
    | ResourceVar of string

type Gesture =
    | Tween of unit
    | Mount of unit
    | Emit of unit
    | GestureVar of string

type StaticSpriteEffect =
    { SpriteResource : AssetTag }

type Animation =
    | Container of unit
    | StaticSprite of StaticSpriteEffect
    | AnimatedSprite of unit
    | AnimationVar of unit

type Define =
    | PlaybackDefine of PlaybackValue
    | TweenDefine of unit
    | MountDefine of unit
    | EmitDefine of unit
    | ResourceDefine of unit

type [<NoComparison>] Effect =
    { Name : string
      Playback : Playback
      Animations : Map<string, Animation>
      Defines : Map<string, Define>
      Gestures : unit list }