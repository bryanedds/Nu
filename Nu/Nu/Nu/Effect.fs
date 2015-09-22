namespace Nu
open OpenTK

type [<NoComparison>] Linear2 =
    { Name : string
      Begin : Vector2
      End : Vector2 }

type Lifetime =
    { Begin : single
      End : single }

type [<NoComparison>] Tween =
    | Linear2 of Linear2

type [<NoComparison>] EffectSprite =
    { Name : string
      Lifetime : Lifetime
      Tweens : Tween list }

type [<NoComparison>] Effect =
    | Sprite of EffectSprite