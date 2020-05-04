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
          LifetimeOpt = Some 70L
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
                       { TweenValue = v2Zero; TweenLength = 40L }|])
                   Color
                    (Set, EaseOut, Once,
                     [|{ TweenValue = colorOpaque; TweenLength = 40L }
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
        let staticSprite =
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
        let soundEffect =
            SoundEffect
                (Resource (Assets.Explosion3Sound.PackageName, Assets.Explosion3Sound.AssetName),
                 [|Enabled
                    (Equal, Once,
                     [|{ LogicValue = false; LogicLength = 9L }
                       { LogicValue = true; LogicLength = 1L }
                       { LogicValue = false; LogicLength = 70L }|])|],
                 Nil)
        { EffectName = "Bolt"
          LifetimeOpt = Some 80L
          Definitions = Map.empty
          Content = Composite (Shift 0.0f, [|staticSprite; soundEffect|]) }