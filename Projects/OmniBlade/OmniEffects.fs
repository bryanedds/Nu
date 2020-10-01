namespace OmniBlade
open System
open Prime
open Nu
open Nu.Effects

[<RequireQualifiedAccess>]
module Effects =

    let Hop (start, stop, height, hopLength, landLength) =
        Aspects
            [|Position
                (Sum, Linear, Once,
                 [|{ TweenValue = start; TweenLength = hopLength }
                   { TweenValue = stop; TweenLength = landLength }
                   { TweenValue = stop; TweenLength = 0L }|])
              Position
                (Sum, SinScaled 0.5f, Once,
                 [|{ TweenValue = v2Zero; TweenLength = hopLength }
                   { TweenValue = v2 0.0f height; TweenLength = landLength }
                   { TweenValue = v2Zero; TweenLength = 0L }|])|]

    let Circle (radius, repetitions, length) =
        Aspects
            [|Position
               (Sum, SinScaled repetitions, Once,
                [|{ TweenValue = v2Zero; TweenLength = length }
                  { TweenValue = v2 -radius 0.0f; TweenLength = 0L }|])
              Position
               (Sum, CosScaled repetitions, Once,
                [|{ TweenValue = v2Zero; TweenLength = length }
                  { TweenValue = v2 0.0f -radius; TweenLength = 0L }|])
              Position
               (Sum, Constant, Once,
                [|{ TweenValue = v2 0.0f radius; TweenLength = length }
                  { TweenValue = v2 0.0f radius; TweenLength = 0L }|])|]

    let makeHitPointsChangeEffect delta =
        let colorOpaque =
            if delta < 0
            then col (byte 255) (byte 255) (byte 255) (byte 255)
            else col (byte 0) (byte 255) (byte 255) (byte 255)
        let colorTransparent =
            colorOpaque.WithA (byte 0)
        { EffectName = "HitPointsChange"
          LifetimeOpt = Some 80L
          Definitions = Map.empty
          Content =
            TextSprite
                (Resource (Assets.Font.PackageName, Assets.Font.AssetName),
                 scstring (abs delta),
                 [|Position
                    (Sum, Linear, Bounce,
                     [|{ TweenValue = v2Zero; TweenLength = 10L }
                       { TweenValue = v2 0.0f 48.0f; TweenLength = 10L }
                       { TweenValue = v2Zero; TweenLength = 10L }
                       { TweenValue = v2Zero; TweenLength = 50L }|])
                   Color
                    (Set, EaseOut, Once,
                     [|{ TweenValue = colorOpaque; TweenLength = 50L }
                       { TweenValue = colorOpaque; TweenLength = 30L }
                       { TweenValue = colorTransparent; TweenLength = 0L }|])|],
                 Nil) }

    let makeCancelEffect () =
        { EffectName = "Cancel"
          LifetimeOpt = Some 40L
          Definitions = Map.empty
          Content =
            StaticSprite
                (Resource (Assets.CancelImage.PackageName, Assets.CancelImage.AssetName),
                 [|Rotation
                    (Sum, Linear, Bounce,
                     [|{ TweenValue = single Math.PI * -2.0f; TweenLength = 10L }
                       { TweenValue = 0.0f; TweenLength = 30L }
                       { TweenValue = 0.0f; TweenLength = 0L }|])
                   Size
                    (Set, EaseOut, Once,
                     [|{ TweenValue = v2Zero; TweenLength = 10L }
                       { TweenValue = v2 208.0f 64.0f; TweenLength = 30L }
                       { TweenValue = v2 208.0f 64.0f; TweenLength = 0L }|])|],
                 Nil) }

    let makeBoltEffect () =
        let boltSprite =
            StaticSprite
                (Resource (Assets.BoltAnimationSheet.PackageName, Assets.BoltAnimationSheet.AssetName),
                 [|Inset
                    (Set, Constant, Once,
                     [|{ TweenValue = v4 0.0f   0.0f    256.0f  1024.0f; TweenLength = 5L }
                       { TweenValue = v4 256.0f 0.0f    256.0f  1024.0f; TweenLength = 5L }
                       { TweenValue = v4 512.0f 0.0f    256.0f  1024.0f; TweenLength = 5L }
                       { TweenValue = v4 512.0f 0.0f    256.0f  1024.0f; TweenLength = 65L }|])
                   Color
                    (Set, EaseOut, Once,
                     [|{ TweenValue = Color.White; TweenLength = 40L }
                       { TweenValue = Color.White; TweenLength = 40L }
                       { TweenValue = Color.White.WithA (byte 0); TweenLength = 0L }|])|],
                 Nil)
        let explosionSprite =
            AnimatedSprite
                (Resource (Assets.ExplosionAnimationSheet.PackageName, Assets.ExplosionAnimationSheet.AssetName),
                 v2i 128 128, 4, 12, 2L, Once,
                 [|Size (Set, Constant, Once, [|{ TweenValue = v2 128.0f 128.0f; TweenLength = 0L }|])
                   Position (Sum, Constant, Once, [|{ TweenValue = v2 0.0f -512.0f; TweenLength = 0L }|])
                   Color
                    (Set, EaseOut, Once,
                     [|{ TweenValue = Color.White; TweenLength = 30L }
                       { TweenValue = Color.White; TweenLength = 30L }
                       { TweenValue = Color.White.WithA (byte 0); TweenLength = 0L }|])|],
                 Nil)
        let explostionSoundEffect =
            SoundEffect
                (Resource (Assets.ExplosionSound.PackageName, Assets.ExplosionSound.AssetName),
                 [|Enabled (Equal, Once, [|{ LogicValue = true; LogicLength = 0L }; { LogicValue = false; LogicLength = 70L }|])|],
                 Nil)
        { EffectName = "Bolt"
          LifetimeOpt = Some 80L
          Definitions = Map.empty
          Content =
            Contents
                (Shift 0.0f,
                 [|boltSprite
                   Delay (10L, explosionSprite)
                   Delay (10L, explostionSoundEffect)|]) }

    let makeHopEffect start stop =
        { EffectName = "Hop"
          LifetimeOpt = Some 20L
          Definitions = Map.empty
          Content = Tag ("Tag", [|Hop (start, stop, 32.0f, 15L, 5L)|], Nil) }

    let makeCircleEffect radius =
        { EffectName = "Circle"
          LifetimeOpt = Some 100L
          Definitions = Map.empty
          Content = Tag ("Tag", [|Circle (radius, 2.0f, 100L)|], Nil) }