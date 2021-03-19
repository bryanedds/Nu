// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open Prime
open Nu
open Nu.Effects

[<RequireQualifiedAccess>]
module Effects =

    let Hop (start, stop, height, hopLength, landLength) =
        Aspects
            [|Positions
                (Sum, Linear, Once,
                 [|{ TweenValue = start; TweenLength = hopLength }
                   { TweenValue = stop; TweenLength = landLength }
                   { TweenValue = stop; TweenLength = 0L }|])
              Positions
                (Sum, SinScaled 0.5f, Once,
                 [|{ TweenValue = v2Zero; TweenLength = hopLength }
                   { TweenValue = v2 0.0f height; TweenLength = landLength }
                   { TweenValue = v2Zero; TweenLength = 0L }|])|]

    let Circle (radius, repetitions, length) =
        Aspects
            [|Positions
               (Sum, SinScaled repetitions, Once,
                [|{ TweenValue = v2Zero; TweenLength = length }
                  { TweenValue = v2 -radius 0.0f; TweenLength = 0L }|])
              Positions
               (Sum, CosScaled repetitions, Once,
                [|{ TweenValue = v2Zero; TweenLength = length }
                  { TweenValue = v2 0.0f -radius; TweenLength = 0L }|])
              Positions
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
          LifeTimeOpt = Some 80L
          Definitions = Map.empty
          Content =
            TextSprite
                (Resource (AssetTag.toPair Assets.Gui.Font),
                 scstring (abs delta),
                 [|Positions
                    (Sum, Linear, Bounce,
                     [|{ TweenValue = v2Zero; TweenLength = 10L }
                       { TweenValue = v2 0.0f 36.0f; TweenLength = 10L }
                       { TweenValue = v2Zero; TweenLength = 10L }
                       { TweenValue = v2Zero; TweenLength = 50L }|])
                   Colors
                    (Set, EaseOut, Once,
                     [|{ TweenValue = colorOpaque; TweenLength = 50L }
                       { TweenValue = colorOpaque; TweenLength = 30L }
                       { TweenValue = colorTransparent; TweenLength = 0L }|])|],
                 Nil) }

    let makeCancelEffect () =
        { EffectName = "Cancel"
          LifeTimeOpt = Some 40L
          Definitions = Map.empty
          Content =
            StaticSprite
                (Resource (AssetTag.toPair Assets.Battle.CancelImage),
                 [|Rotations
                    (Sum, Linear, Bounce,
                     [|{ TweenValue = single Math.PI * -2.0f; TweenLength = 10L }
                       { TweenValue = 0.0f; TweenLength = 30L }
                       { TweenValue = 0.0f; TweenLength = 0L }|])
                   Sizes
                    (Set, EaseOut, Once,
                     [|{ TweenValue = v2Zero; TweenLength = 10L }
                       { TweenValue = v2 156.0f 48.0f; TweenLength = 30L }
                       { TweenValue = v2 156.0f 48.0f; TweenLength = 0L }|])|],
                 Nil) }

    let makeBoltEffect () =
        let boltSprite =
            StaticSprite
                (Resource (AssetTag.toPair Assets.Battle.BoltAnimationSheet),
                 [|Insets
                    (Set, Constant, Once,
                     [|{ TweenValue = v4 0.0f   0.0f    192.0f  768.0f; TweenLength = 5L }
                       { TweenValue = v4 192.0f 0.0f    192.0f  768.0f; TweenLength = 5L }
                       { TweenValue = v4 384.0f 0.0f    192.0f  768.0f; TweenLength = 5L }
                       { TweenValue = v4 384.0f 0.0f    192.0f  768.0f; TweenLength = 65L }|])
                   Colors
                    (Set, EaseOut, Once,
                     [|{ TweenValue = Color.White; TweenLength = 40L }
                       { TweenValue = Color.White; TweenLength = 40L }
                       { TweenValue = Color.White.WithA (byte 0); TweenLength = 0L }|])|],
                 Nil)
        let explosionSprite =
            AnimatedSprite
                (Resource (AssetTag.toPair Assets.Battle.ExplosionAnimationSheet),
                 v2i 96 96, 4, 12, 2L, Once,
                 [|PositionRelative (v2 0.0f -384.0f)
                   Size (v2 96.0f 96.0f)
                   Colors
                    (Set, EaseOut, Once,
                     [|{ TweenValue = Color.White; TweenLength = 30L }
                       { TweenValue = Color.White; TweenLength = 30L }
                       { TweenValue = Color.White.WithA (byte 0); TweenLength = 0L }|])|],
                 Nil)
        let thunderSoundEffect =
            SoundEffect
                (Resource (AssetTag.toPair Assets.Field.ThunderSound),
                 [|Enableds (Equal, Once, [|{ LogicValue = true; LogicLength = 0L }; { LogicValue = false; LogicLength = 70L }|])|],
                 Nil)
        { EffectName = "Bolt"
          LifeTimeOpt = Some 80L
          Definitions = Map.empty
          Content =
            Contents
                (Shift 0.0f,
                 [|boltSprite
                   Delay (10L, explosionSprite)
                   Delay (10L, thunderSoundEffect)|]) }

    let makeImpactSplashEffect () =
        { EffectName = "ImpactSplash"
          LifeTimeOpt = Some 24L
          Definitions = Map.empty
          Content =
            Contents
                (Shift 0.0f,
                 [|AnimatedSprite
                    (Resource (AssetTag.toPair Assets.Battle.ImpactSplashAnimationSheet),
                     v2i 96 96, 3, 3, 8L, Once,
                     [|PositionRelative (v2 -48.0f 0.0f); Size (v2 96.0f 96.0f)|],
                     Nil)
                   AnimatedSprite
                    (Resource (AssetTag.toPair Assets.Battle.ImpactSplashAnimationSheet),
                     v2i 96 96, 3, 3, 8L, Once,
                     [|PositionRelative (v2 48.0f 0.0f); Size (v2 96.0f 96.0f)|],
                     Nil)|]) }

    let makeSlashSpikeEffect position position2 =
        let spike = AnimatedSprite (Resource (AssetTag.toPair Assets.Battle.SpikeAnimationSheet), v2i 96 96, 5, 5, 3L, Once, [||], Nil)
        let emit =
            Emit
                (Shift 0.1f,
                 Rate 0.2f,
                 [|Positions (Set, Linear, Once, [|{ TweenValue = position; TweenLength = 60L }; { TweenValue = position2; TweenLength = 0L }|])|],
                 [|Size (v2 96.0f 96.0f)|],
                 spike)
        { EffectName = "SlashSpike"
          LifeTimeOpt = Some 75L
          Definitions = Map.empty
          Content = emit }

    let makeCycloneBlurEffect radius =
        { EffectName = "CycloneBlur"
          LifeTimeOpt = Some 100L
          Definitions = Map.empty
          Content =
              AnimatedSprite
               (Resource (AssetTag.toPair Assets.Battle.CycloneBlurAnimationSheet),
                v2i 234 234, 2, 4, 3L, Loop,
                [|Circle (radius, 2.0f, 100L)|],
                Nil) }

    let makeHopEffect start stop =
        { EffectName = "Hop"
          LifeTimeOpt = Some 20L
          Definitions = Map.empty
          Content = Tag ("Tag", [|Hop (start, stop, 24.0f, 15L, 5L)|], Nil) }

    let makeCircleEffect radius =
        { EffectName = "Circle"
          LifeTimeOpt = Some 100L
          Definitions = Map.empty
          Content = Tag ("Tag", [|Circle (radius, 2.0f, 100L)|], Nil) }