namespace OmniBlade
open System
open Prime
open Nu

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
            Effects.TextSprite
                (Effects.Resource (Assets.DefaultPackageName, Assets.DefaultFontName),
                 [|Effects.Text (scstring (abs delta))
                   Effects.Position
                    (Effects.Sum, Effects.Linear, Effects.Bounce,
                     [|{ TweenValue = v2Zero; TweenLength = 10L }
                       { TweenValue = v2 0.0f 48.0f; TweenLength = 10L }
                       { TweenValue = v2Zero; TweenLength = 10L }
                       { TweenValue = v2Zero; TweenLength = 40L }|])
                   Effects.Color
                    (Effects.Set, Effects.EaseOut, Effects.Once,
                     [|{ TweenValue = colorOpaque; TweenLength = 40L }
                       { TweenValue = colorOpaque; TweenLength = 30L }
                       { TweenValue = colorTransparent; TweenLength = 0L }|])|],
                 Effects.Nil) }
    
    let makeCancelEffect () =
        { EffectName = "Cancel"
          LifetimeOpt = Some 40L
          Definitions = Map.empty
          Content =
            Effects.StaticSprite
                (Effects.Resource (Assets.CancelImage.PackageName, Assets.CancelImage.AssetName),
                 [|Effects.Rotation
                    (Effects.Sum, Effects.Linear, Effects.Bounce,
                     [|{ TweenValue = single Math.PI * -2.0f; TweenLength = 10L }
                       { TweenValue = 0.0f; TweenLength = 30L }
                       { TweenValue = 0.0f; TweenLength = 0L }|])
                   Effects.Size
                    (Effects.Set, Effects.EaseOut, Effects.Once,
                     [|{ TweenValue = v2Zero; TweenLength = 10L }
                       { TweenValue = v2 208.0f 64.0f; TweenLength = 30L }
                       { TweenValue = v2 208.0f 64.0f; TweenLength = 0L }|])|],
                 Effects.Nil) }