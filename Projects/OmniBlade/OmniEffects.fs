// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open Prime
open Nu
open Nu.Effects

[<RequireQualifiedAccess>]
module Effects =

    let Hop (start, stop, height, hopLength) =
        Aspects
            [|Positions
                (Sum, Linear, Once,
                 [|{ TweenValue = start; TweenLength = hopLength }
                   { TweenValue = stop; TweenLength = 0L }|])
              Positions
                (Sum, SinScaled 0.5f, Once,
                 [|{ TweenValue = v2Zero; TweenLength = hopLength }
                   { TweenValue = v2 0.0f height; TweenLength = 0L }|])|]

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
                     [|{ TweenValue = v4 0.0f   0.0f    64.0f   256.0f; TweenLength = 5L }
                       { TweenValue = v4 64.0f 0.0f     64.0f   256.0f; TweenLength = 5L }
                       { TweenValue = v4 128.0f 0.0f    64.0f   256.0f; TweenLength = 5L }
                       { TweenValue = v4 128.0f 0.0f    64.0f   256.0f; TweenLength = 65L }|])
                   Colors
                    (Set, EaseOut, Once,
                     [|{ TweenValue = Color.White; TweenLength = 40L }
                       { TweenValue = Color.White; TweenLength = 40L }
                       { TweenValue = Color.White.WithA (byte 0); TweenLength = 0L }|])|],
                 Nil)
        let explosionSprite =
            AnimatedSprite
                (Resource (AssetTag.toPair Assets.Battle.ExplosionAnimationSheet),
                 v2i 32 32, 4, 12, 2L, Once,
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
                     v2i 32 32, 3, 3, 8L, Once,
                     [|PositionRelative (v2 -48.0f 0.0f); Size (v2 96.0f 96.0f); Flip FlipH|],
                     Nil)
                   AnimatedSprite
                    (Resource (AssetTag.toPair Assets.Battle.ImpactSplashAnimationSheet),
                     v2i 32 32, 3, 3, 8L, Once,
                     [|PositionRelative (v2 48.0f 0.0f); Size (v2 96.0f 96.0f); Flip FlipNone|],
                     Nil)|]) }

    let makeCutEffect light =
        let image = if light then Assets.Battle.LightCutImage else Assets.Battle.CutImage
        { EffectName = "Cut"
          LifeTimeOpt = Some 24L
          Definitions = Map.empty
          Content =
              StaticSprite
               (Resource (AssetTag.toPair image),
                [|Colors
                   (Set, EaseOut, Once,
                    [|{ TweenValue = Color.White; TweenLength = 24L }
                      { TweenValue = Color.White.WithA (byte 0); TweenLength = 0L }|])|],
                Nil) }

    let makeShurikenEffect position position2 =
        { EffectName = "Shuriken"
          LifeTimeOpt = Some 30L
          Definitions = Map.empty
          Content =
              AnimatedSprite
               (Resource (AssetTag.toPair Assets.Battle.ShurikenAnimationSheet),
                v2i 64 64, 2, 2, 3L, Loop,
                [|Size (v2 192.0f 192.0f);
                  Positions (Set, Linear, Once, [|{ TweenValue = position; TweenLength = 30L }; { TweenValue = position2; TweenLength = 0L }|])|],
                Nil) }
    
    let makeSlashSpikeEffect position position2 =
        let spike = AnimatedSprite (Resource (AssetTag.toPair Assets.Battle.SpikeAnimationSheet), v2i 32 32, 5, 5, 3L, Once, [||], Nil)
        let emit =
            Emit
                (Shift 0.1f,
                 Rate 0.5f,
                 [|Positions (Set, Linear, Once, [|{ TweenValue = position; TweenLength = 60L }; { TweenValue = position2; TweenLength = 0L }|])|],
                 [|Size (v2 96.0f 96.0f); Offset (v2 0.5f 0.0f)|],
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
                v2i 78 78, 2, 4, 3L, Loop,
                [|Circle (radius, 2.0f, 100L)|],
                Nil) }

    let makeArcaneCastEffect () =
        let halfWidth = 50.0f
        let altitude = halfWidth * 2.0f * 0.866f
        let candle position = AnimatedSprite (Resource (AssetTag.toPair Assets.Battle.CandleAnimationSheet), v2i 16 20, 3, 3, 5L, Loop, [|Size (v2 64.0f 80.0f); position|], Nil)
        let staticEffect position angle = AnimatedSprite (Resource (AssetTag.toPair Assets.Battle.StaticAnimationSheet), v2i 64 64, 5, 5, 3L, Loop, [|Size (v2 128.0f 128.0f); position; angle|], Nil)
        { EffectName = "ArcaneCast"
          LifeTimeOpt = Some 36L
          Definitions = Map.empty
          Content =
            Contents
                (Shift 0.0f,
                 [|candle (PositionRelative (v2 0.0f altitude))
                   candle (PositionRelative (v2 -halfWidth 0.0f))
                   candle (PositionRelative (v2 halfWidth 0.0f))
                   staticEffect (PositionRelative (v2 0.0f 0.0f)) (Angle 90.0f)
                   staticEffect (PositionRelative (v2 -25.0f 50.0f)) (Angle 30.0f)
                   staticEffect (PositionRelative (v2 25.0f 50.0f)) (Angle -30.0f)|]) }
    
    let makeFireEffect position position2 =
        let fireSize = Size (v2 64.0f 64.0f)
        let activation timeOn timeOff = Enableds (Equal, Once, [|{ LogicValue = true; LogicLength = timeOn}; { LogicValue = false; LogicLength = timeOff }|])
        let linearTravel position position2 duration = Positions (Set, EaseOut, Once, [|{ TweenValue = position; TweenLength = duration }; { TweenValue = position2; TweenLength = 0L }|])
        let fire playback aspects =
            AnimatedSprite
             (Resource (AssetTag.toPair Assets.Battle.FireAnimationSheet),
              v2i 16 16, 4, 4, 3L, playback, aspects, Nil)
        let burn =
            AnimatedSprite
             (Resource (AssetTag.toPair Assets.Battle.BurnAnimationSheet),
              v2i 16 16, 4, 4, 3L, Once, [||], Nil)
        let fireball travel activation =
            Contents
                (Shift 0.0f,
                 [|fire Loop [|travel; fireSize; activation|];
                   Emit (Shift 0.0f, Rate 0.3f, [|travel; activation|], [||], fire Once [|fireSize|]) |])
        { EffectName = "Fire"
          LifeTimeOpt = Some 100L
          Definitions = Map.empty
          Content = 
            Contents
                (Shift 0.0f,
                 [|fireball (Circle (64.0f, 1.5f, 40L)) (activation 40L 60L);
                   Delay (40L, fireball (Aspects [|linearTravel position position2 20L|]) (activation 20L 40L));
                   Delay (60L, Emit (Shift 0.0f, Rate 0.1f, [||], [|linearTravel position2 (position2 + (v2 0.0f 70.0f)) 20L|], burn))|]) }

    let makeFlameEffect position position2 =
        { EffectName = "Flame"
          LifeTimeOpt = Some 100L
          Definitions = Map.empty
          Content =
            Emit
                (Shift 0.0f,
                 Rate 0.25f,
                 [|Enableds (Equal, Once, [|{ LogicValue = true; LogicLength = 64L }; { LogicValue = false; LogicLength = 0L }|])|],
                 [|Sizes (Set, Linear, Once, [|{ TweenValue = v2 32.0f 32.0f; TweenLength = 36L }; { TweenValue = v2 192.0f 192.0f; TweenLength = 0L }|])
                   Positions (Set, EaseIn, Once, [|{ TweenValue = position; TweenLength = 36L }; { TweenValue = position2; TweenLength = 0L }|])
                   Color (colWhite.WithA (byte 207))|],
                 AnimatedSprite (Resource (AssetTag.toPair Assets.Battle.FlameAnimationSheet), v2i 64 64, 6, 6, 6L, Once, [||], Nil))}

    let makeIceEffect () =
        let coverRadius = 50.0f
        let bombardActivation = Enableds (Equal, Once, [|{ LogicValue = true; LogicLength = 10L };{ LogicValue = false; LogicLength = 0L }|])
        let bombardTravel origin = Positions (Sum, Linear, Once, [|{ TweenValue = origin; TweenLength = 10L };{ TweenValue = v2Zero; TweenLength = 0L }|])
        let coverTravel =
            Aspects
                [|Positions
                   (Sum, Linear, Loop,
                    [|{ TweenValue = v2 0.0f -coverRadius; TweenLength = 10L }
                      { TweenValue = v2 coverRadius 0.0f; TweenLength = 0L }|])
                  Positions
                   (Sum, Random, Loop,
                    [|{ TweenValue = v2Zero; TweenLength = 80L }
                      { TweenValue = v2 -coverRadius coverRadius; TweenLength = 0L }|])|]
        let ice = StaticSprite (Resource (AssetTag.toPair Assets.Battle.IceImage), [|Size (v2 192.0f 192.0f)|], Nil)
        let iceBombard origin = Emit (Shift 0.0f, Rate 0.2f, [||], [|bombardTravel origin; bombardActivation|], ice)
        let iceCover = Emit (Shift 0.0f, Rate 1.0f, [|coverTravel|], [||], ice)
        { EffectName = "Ice"
          LifeTimeOpt = Some 80L
          Definitions = Map.empty
          Content =
            Contents
                (Shift 0.0f,
                 [|iceCover; iceBombard (v2 -700.0f 0.0f); iceBombard (v2 500.0f 500.0f); iceBombard (v2 500.0f -500.0f)|])}
    
    let makeSnowballEffect () =
        let fall = Positions (Sum, Linear, Once, [|{ TweenValue = v2 0.0f 800.0f; TweenLength = 80L };{ TweenValue = v2 0.0f -800.0f; TweenLength = 0L }|])
        let rotate = Rotations (Set, Constant, Loop, [|{ TweenValue = 0.0f; TweenLength = 5L };{ TweenValue = 90.0f; TweenLength = 5L };{ TweenValue = 180.0f; TweenLength = 5L };{ TweenValue = 270.0f; TweenLength = 5L }|])
        { EffectName = "Snowball"
          LifeTimeOpt = Some 80L
          Definitions = Map.empty
          Content =
              StaticSprite
               (Resource (AssetTag.toPair Assets.Battle.SnowballImage),
                [|Size (v2 432.0f 432.0f); fall; rotate|], Nil) }
    
    let makeHolyCastEffect () =
        { EffectName = "HolyCast"
          LifeTimeOpt = Some 36L
          Definitions = Map.empty
          Content =
              AnimatedSprite
               (Resource (AssetTag.toPair Assets.Battle.HolyCastAnimationSheet),
                v2i 100 100, 6, 36, 1L, Once, [||], Nil) }
    
    let makePurifyEffect () =
        let sprite position =
            AnimatedSprite
             (Resource (AssetTag.toPair Assets.Battle.PurifyAnimationSheet),
              v2i 64 64, 5, 10, 3L, Once, [|PositionRelative position|], Nil)
        { EffectName = "Purify"
          LifeTimeOpt = Some 54L
          Definitions = Map.empty
          Content =
            Contents
                (Shift 0.0f,
                 [|sprite (v2 0.0f 0.0f);
                   Delay (12L, sprite (v2 -16.0f 64.0f));
                   Delay (24L, sprite (v2 16.0f 32.0f))|])}

    let makeCureEffect () =
        let path =
            Aspects
                [|Positions
                   (Sum, Sin, Loop,
                    [|{ TweenValue = v2Zero; TweenLength = 3L }
                      { TweenValue = v2 70.0f 0.0f; TweenLength = 0L }|])
                  Positions
                   (Sum, EaseOut, Loop,
                    [|{ TweenValue = v2Zero; TweenLength = 21L }
                      { TweenValue = v2 0.0f 250.0f; TweenLength = 0L }|])
                  Positions
                   (Sum, Constant, Loop,
                    [|{ TweenValue = v2 0.0f -100.0f; TweenLength = 1L }|])|]
        let sparkle =
            AnimatedSprite
             (Resource (AssetTag.toPair Assets.Battle.SparkleAnimationSheet),
              v2i 16 16, 6, 6, 4L, Once, [||], Nil)
        { EffectName = "Cure"
          LifeTimeOpt = Some 100L
          Definitions = Map.empty
          Content = Emit (Shift 0.0f, Rate 0.2f, [|path|], [||], sparkle)}

    let makeEmpowerEffect () =
        let length = 60L
        let quarterLength = length / 4L
        let size = Size (v2 48.0f 111.0f)
        let elevation0 =
            Aspects
                [|Elevations
                   (Set, Constant, Loop,
                    [|{ TweenValue = Constants.Battle.ForegroundElevation - 1.0f; TweenLength = quarterLength }
                      { TweenValue = Constants.Battle.EffectElevation; TweenLength = quarterLength }
                      { TweenValue = Constants.Battle.EffectElevation; TweenLength = quarterLength }
                      { TweenValue = Constants.Battle.ForegroundElevation - 1.0f; TweenLength = quarterLength }
                      { TweenValue = Constants.Battle.ForegroundElevation - 1.0f; TweenLength = 0L }|])|]
        let elevation1 =
            Aspects
                [|Elevations
                   (Set, Constant, Loop,
                    [|{ TweenValue = Constants.Battle.EffectElevation; TweenLength = quarterLength }
                      { TweenValue = Constants.Battle.ForegroundElevation - 1.0f; TweenLength = quarterLength }
                      { TweenValue = Constants.Battle.ForegroundElevation - 1.0f; TweenLength = quarterLength }
                      { TweenValue = Constants.Battle.EffectElevation; TweenLength = quarterLength }
                      { TweenValue = Constants.Battle.EffectElevation - 1.0f; TweenLength = 0L }|])|]
        let elevation2 =
            Aspects
                [|Elevations
                   (Set, Constant, Loop,
                    [|{ TweenValue = Constants.Battle.ForegroundElevation - 1.0f; TweenLength = quarterLength }
                      { TweenValue = Constants.Battle.ForegroundElevation - 1.0f; TweenLength = quarterLength }
                      { TweenValue = Constants.Battle.EffectElevation; TweenLength = quarterLength }
                      { TweenValue = Constants.Battle.EffectElevation; TweenLength = quarterLength }
                      { TweenValue = Constants.Battle.EffectElevation; TweenLength = 0L }|])|]
        let elevation3 =
            Aspects
                [|Elevations
                   (Set, Constant, Loop,
                    [|{ TweenValue = Constants.Battle.EffectElevation; TweenLength = quarterLength }
                      { TweenValue = Constants.Battle.EffectElevation; TweenLength = quarterLength }
                      { TweenValue = Constants.Battle.ForegroundElevation - 1.0f; TweenLength = quarterLength }
                      { TweenValue = Constants.Battle.ForegroundElevation - 1.0f; TweenLength = quarterLength }
                      { TweenValue = Constants.Battle.ForegroundElevation - 1.0f; TweenLength = 0L }|])|]
        let orbit0 radiusX radiusY =
            Aspects
                [|Positions
                   (Sum, SinScaled 1.0f, Loop,
                    [|{ TweenValue = v2Zero; TweenLength = length }
                      { TweenValue = v2 radiusX 0.0f; TweenLength = 0L}|])
                  Positions
                   (Sum, CosScaled 1.0f, Loop,
                    [|{ TweenValue = v2Zero; TweenLength = length }
                      { TweenValue = v2 0.0f radiusY; TweenLength = 0L}|])
                  Positions
                   (Sum, Constant, Loop,
                    [|{ TweenValue = v2 0.0f -48.0f; TweenLength = 40L }
                      { TweenValue = v2 0.0f -48.0f; TweenLength = 0L }|])|]
        let orbit1 radiusX radiusY =
            Aspects
                [|Positions
                   (Sum, SinScaled 1.0f, Loop,
                    [|{ TweenValue = v2Zero; TweenLength = length }
                      { TweenValue = v2 -radiusX 0.0f; TweenLength = 0L}|])
                  Positions
                   (Sum, CosScaled 1.0f, Loop,
                    [|{ TweenValue = v2Zero; TweenLength = length }
                      { TweenValue = v2 0.0f -radiusY; TweenLength = 0L }|])
                  Positions
                   (Sum, Constant, Loop,
                    [|{ TweenValue = v2 0.0f -48.0f; TweenLength = 40L }
                      { TweenValue = v2 0.0f -48.0f; TweenLength = 0L }|])|]
        let orbit2 radiusX radiusY =
            Aspects
                [|Positions
                   (Sum, CosScaled 1.0f, Loop,
                    [|{ TweenValue = v2 radiusX 0.0f; TweenLength = length }
                      { TweenValue = v2 0.0f 0.0f; TweenLength = 0L }|])
                  Positions
                   (Sum, SinScaled 1.0f, Loop,
                    [|{ TweenValue = v2Zero; TweenLength = length }
                      { TweenValue = v2 0.0f radiusY; TweenLength = 0L }|])
                  Positions
                   (Sum, Constant, Loop,
                    [|{ TweenValue = v2 -radiusX -48.0f; TweenLength = 40L }
                      { TweenValue = v2 -radiusX -48.0f; TweenLength = 0L }|])|]
        let orbit3 radiusX radiusY =
            Aspects
                [|Positions
                   (Sum, CosScaled 1.0f, Loop,
                    [|{ TweenValue = v2 -radiusX 0.0f; TweenLength = length }
                      { TweenValue = v2 0.0f 0.0f; TweenLength = 0L }|])
                  Positions
                   (Sum, SinScaled 1.0f, Loop,
                    [|{ TweenValue = v2Zero; TweenLength = length }
                      { TweenValue = v2 0.0f -radiusY; TweenLength = 0L }|])
                  Positions
                   (Sum, Constant, Loop,
                    [|{ TweenValue = v2 radiusX -48.0f; TweenLength = 40L }
                      { TweenValue = v2 radiusX -48.0f; TweenLength = 0L }|])|]
        let sprite0 = StaticSprite (Resource (AssetTag.toPair Assets.Battle.StrengthSymbolImage), [|orbit0 90.0f 30.0f; size; elevation0|], Nil)
        let sprite1 = StaticSprite (Resource (AssetTag.toPair Assets.Battle.StrengthSymbolImage), [|orbit1 90.0f 30.0f; size; elevation1|], Nil)
        let sprite2 = StaticSprite (Resource (AssetTag.toPair Assets.Battle.StrengthSymbolImage), [|orbit2 90.0f 30.0f; size; elevation2|], Nil)
        let sprite3 = StaticSprite (Resource (AssetTag.toPair Assets.Battle.StrengthSymbolImage), [|orbit3 90.0f 30.0f; size; elevation3|], Nil)
        { EffectName = "Empower"
          LifeTimeOpt = Some 80L
          Definitions = Map.empty
          Content =
            Contents
                (Shift 0.0f,
                [|sprite0; sprite1; sprite2; sprite3|])}

    let makeProtectEffect () =
        let protection aspects = StaticSprite (Resource (AssetTag.toPair Assets.Battle.ProtectSphereImage), aspects, Nil)
        let blink = Enableds (Equal, Loop, [|{ LogicValue = true; LogicLength = 1L };{ LogicValue = false; LogicLength = 2L }|])
        let outwardReach = 64.0f
        let clockwiseBias = 50.0f
        let bend dest = Positions (Sum, EaseIn, Once, [|{ TweenValue = v2Zero; TweenLength = 30L};{ TweenValue = dest; TweenLength = 0L }|])
        let outwardMovement dest = Positions (Sum, Linear, Once, [|{ TweenValue = v2Zero; TweenLength = 30L};{ TweenValue = dest; TweenLength = 0L }|])
        { EffectName = "Protect"
          LifeTimeOpt = Some 80L
          Definitions = Map.empty
          Content =
            Contents
                (Shift 0.0f,
                 [|protection [|blink; Enableds (And, Once, [|{ LogicValue = true; LogicLength = 50L }; { LogicValue = false; LogicLength = 0L}|]); Sizes (Set, Ease, Once, [|{ TweenValue = v2 192.0f 192.0f; TweenLength = 50L }; { TweenValue = v2 64.0f 64.0f; TweenLength = 0L }|])|]
                   Delay (50L,
                     (Contents
                          (Shift 0.0f,
                           [|protection [|blink; outwardMovement (v2 outwardReach outwardReach); bend (v2 0.0f -clockwiseBias)|]
                             protection [|blink; outwardMovement (v2 outwardReach -outwardReach); bend (v2 -clockwiseBias 0.0f)|]
                             protection [|blink; outwardMovement (v2 -outwardReach -outwardReach); bend (v2 0.0f clockwiseBias)|]
                             protection [|blink; outwardMovement (v2 -outwardReach outwardReach); bend (v2 clockwiseBias 0.0f)|]|])))|])}
    
    let makeDimensionalCastEffect () =
        let length = 60L
        let electronSize = Size (v2 9.0f 9.0f)
        let nonLocationSize = Size (v2 3.0f 3.0f)
        let positionAdjustY = PositionRelative (v2 0.0f -36.0f)
        let fade =
            Colors
               (Set, EaseOut, Once,
                [|{ TweenValue = Color.White; TweenLength = 24L }
                  { TweenValue = Color.White.WithA (byte 0); TweenLength = 0L }|])
        let orbit radiusX radiusY repetitions =
            Aspects
                [|Positions
                   (Sum, SinScaled repetitions, Once,
                    [|{ TweenValue = v2Zero; TweenLength = length }
                      { TweenValue = v2 -radiusX 0.0f; TweenLength = 0L }|])
                  Positions
                   (Sum, CosScaled repetitions, Once,
                    [|{ TweenValue = v2Zero; TweenLength = length }
                      { TweenValue = v2 0.0f -radiusY; TweenLength = 0L }|])
                  Positions
                   (Sum, Constant, Once,
                    [|{ TweenValue = v2 0.0f radiusY; TweenLength = length }
                      { TweenValue = v2 0.0f radiusY; TweenLength = 0L }|])|]
        let orbitH = orbit 90.0f 30.0f 2.0f
        let orbitV = orbit 50.0f 70.0f 1.5f
        { EffectName = "DimensionalCast"
          LifeTimeOpt = Some length
          Definitions = Map.empty
          Content =
              Contents
                  (Shift 0.0f,
                   [|StaticSprite (Resource (AssetTag.toPair Assets.Battle.ElectronBlueImage), [|orbitH; electronSize|], Nil);
                     Emit (Shift 0.0f, Rate 1.0f, [|orbitH|], [||], StaticSprite (Resource (AssetTag.toPair Assets.Battle.NonLocationBlueImage), [|nonLocationSize; fade|], Nil));
                     StaticSprite (Resource (AssetTag.toPair Assets.Battle.ElectronGreenImage), [|orbitV; electronSize; positionAdjustY|], Nil);
                     Emit (Shift 0.0f, Rate 1.0f, [|orbitV; positionAdjustY|], [||], StaticSprite (Resource (AssetTag.toPair Assets.Battle.NonLocationGreenImage), [|nonLocationSize; fade|], Nil))|])}
    
    let makeBuffEffect statusType =
        let image =
            match statusType with
            | Power (_, _) -> Assets.Battle.PowerBuffImage
            | Magic (_, _) -> Assets.Battle.MagicBuffImage
            | Shield (_, _) -> Assets.Battle.ShieldBuffImage
            | Time _ -> Assets.Battle.TimeBuffImage
            | _ -> Assets.Default.ImageEmpty
        let shrink =
            Sizes
               (Set, EaseIn, Once,
                [|{ TweenValue = v2 0.0f 0.0f; TweenLength = 20L }
                  { TweenValue = v2 96.0f 96.0f; TweenLength = 10L }
                  { TweenValue = v2 96.0f 96.0f; TweenLength = 20L }|])
        { EffectName = "Buff"
          LifeTimeOpt = Some 50L
          Definitions = Map.empty
          Content =
              Contents
                  (Shift 0.0f,
                   [|StaticSprite (Resource (AssetTag.toPair image), [|shrink; PositionRelative (v2 0.0f 32.0f)|], Nil)|])}
    
    let makeDebuffEffect statusType =
        let image =
            match statusType with
            | Power (_, _) -> Assets.Battle.PowerDebuffImage
            | Magic (_, _) -> Assets.Battle.MagicDebuffImage
            | Shield (_, _) -> Assets.Battle.ShieldDebuffImage
            | Time _ -> Assets.Battle.TimeDebuffImage
            | _ -> Assets.Default.ImageEmpty
        let shrink =
            Sizes
               (Set, EaseIn, Once,
                [|{ TweenValue = v2 96.0f 96.0f; TweenLength = 20L }
                  { TweenValue = v2 96.0f 96.0f; TweenLength = 10L }
                  { TweenValue = v2 48.0f 48.0f; TweenLength = 20L }|])
        { EffectName = "Debuff"
          LifeTimeOpt = Some 50L
          Definitions = Map.empty
          Content =
              Contents
                  (Shift 0.0f,
                   [|StaticSprite (Resource (AssetTag.toPair image), [|shrink; PositionRelative (v2 0.0f 32.0f)|], Nil)|])}
    
    let makeConjureIfritEffect () =
        let fireSpinSize = Size (v2 600.0f 600.0f)
        let fireSpin aspects =
            AnimatedSprite
             (Resource (AssetTag.toPair Assets.Battle.FireSpinAnimationSheet),
              v2i 100 100, 8, 61, 1L, Loop, aspects, Nil)
        { EffectName = "ConjureIfrit"
          LifeTimeOpt = Some 80L
          Definitions = Map.empty
          Content =
              Contents
                  (Shift 0.0f,
                   [|fireSpin [|fireSpinSize; PositionAbsolute (v2 -144.0f 168.0f)|]
                     fireSpin [|fireSpinSize; PositionAbsolute (v2 144.0f 168.0f)|]
                     fireSpin [|fireSpinSize; PositionAbsolute (v2 -144.0f -111.0f)|]
                     fireSpin [|fireSpinSize; PositionAbsolute (v2 144.0f -111.0f)|]|])}
    
    let makeHopEffect start stop =
        { EffectName = "Hop"
          LifeTimeOpt = Some 20L // +2 due to actualization / update order
          Definitions = Map.empty
          Content = Tag ("Tag", [|Hop (start, stop, 24.0f, 18L)|], Nil) }

    let makeCircleEffect radius =
        { EffectName = "Circle"
          LifeTimeOpt = Some 100L // +2 due to actualization / update order
          Definitions = Map.empty
          Content = Tag ("Tag", [|Circle (radius, 2.0f, 98L)|], Nil) }