namespace Nu
open OpenTK

type Playback =
    | Once
    | Loop
    | Bounce

type Application =
    | Or
    | Xor
    | And
    | Nand
    | Nor
    | Add
    | Sub
    | Mul
    | Div
    | Ovr

type StaticSpriteEffect =
    { SpriteAsset : AssetTag }

type Effect =
    | StaticSprite of StaticSpriteEffect

type [<NoComparison>] Template =
    { Name : string
      Playback : Playback
      Effect : Effect
      Gestures : unit list
      Instances : unit list }

type [<NoComparison>] EffectExpr =
    | Template of Template
    | Instance of unit