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

type AsDefinition =
    | AsPlayback of PlaybackValue
    | AsTween of unit
    | AsMount of unit
    | AsEmit of unit
    | AsResource of unit

type [<NoComparison>] Effect =
    { Name : string
      Playback : Playback
      Definitions : Map<string, AsDefinition>
      Animations : Map<string, Animation> }