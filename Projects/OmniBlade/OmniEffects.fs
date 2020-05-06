namespace OmniBlade
open System
open Prime
open Nu
open Nu.Effects

[<RequireQualifiedAccess>]
module Effects =

    let makeHitPointsChangeEffect delta =
        let colorOpaque =
            if delta < 0
            then v4 1.0f 1.0f 1.0f 1.0f
            else v4 0.0f 1.0f 1.0f 1.0f
        let colorTransparent =
            colorOpaque.WithW 0.0f
        { EffectName = "HitPointsChange"
          LifetimeOpt = Some 80L
          Definitions = Map.empty
          Content =
            TextSprite
                (Resource (Assets.DefaultPackageName, Assets.DefaultFontName),
                 [|Text (scstring (abs delta))
                   Position
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
                    (Set, Const, Once,
                     [|{ TweenValue = v4 0.0f   0.0f    256.0f  1024.0f; TweenLength = 5L }
                       { TweenValue = v4 256.0f 0.0f    512.0f  1024.0f; TweenLength = 5L }
                       { TweenValue = v4 512.0f 0.0f    768.0f  1024.0f; TweenLength = 5L }
                       { TweenValue = v4 512.0f 0.0f    768.0f  1024.0f; TweenLength = 65L }|])
                   Color
                    (Set, EaseOut, Once,
                     [|{ TweenValue = v4One; TweenLength = 40L }
                       { TweenValue = v4One; TweenLength = 40L }
                       { TweenValue = v4One.WithW 0.0f; TweenLength = 0L }|])|],
                 Nil)
        let explosionSprite =
            AnimatedSprite
                (Resource (Assets.ExplosionAnimationSheet.PackageName, Assets.ExplosionAnimationSheet.AssetName),
                 v2i 128 128, 4, 12, 2L, Once,
                 [|Size (Set, Const, Once, [|{ TweenValue = v2 128.0f 128.0f; TweenLength = 0L }|])
                   Position (Sum, Const, Once, [|{ TweenValue = v2 0.0f -512.0f; TweenLength = 0L }|])
                   Color
                    (Set, EaseOut, Once,
                     [|{ TweenValue = v4One; TweenLength = 30L }
                       { TweenValue = v4One; TweenLength = 30L }
                       { TweenValue = v4One.WithW 0.0f; TweenLength = 0L }|])|],
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
            Composite
                (Shift 0.0f,
                 [|boltSprite
                   Delay (10L, explosionSprite)
                   Delay (10L, explostionSoundEffect)|]) }

    let makeHopEffect start stop =
        { EffectName = "Hop"
          LifetimeOpt = Some 40L
          Definitions = Map.empty
          Content =
            Tag
                ("Hop",
                 [|Position
                    (Sum, Linear, Bounce,
                     [|{ TweenValue = start; TweenLength = 30L }
                       { TweenValue = stop; TweenLength = 10L }
                       { TweenValue = stop; TweenLength = 0L }|])|],
                 Nil) }