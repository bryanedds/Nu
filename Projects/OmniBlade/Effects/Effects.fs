// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open Prime
open Nu
open Nu.Effects
open OmniBlade

[<RequireQualifiedAccess>]
module Effects =

    let Hop (start, stop, height, hopLength) =
        Aspects
            [|Positions
                (Sum, Linear, Once,
                 [|{ TweenValue = start; TweenLength = hopLength }
                   { TweenValue = stop; TweenLength = 0u.u }|])
              Positions
                (Sum, SinScaled 0.5f, Once,
                 [|{ TweenValue = v3Zero; TweenLength = hopLength }
                   { TweenValue = v3 0.0f height 0.0f; TweenLength = 0u.u }|])|]

    let Circle (radius, repetitions, length) =
        Aspects
            [|Positions
               (Sum, SinScaled repetitions, Once,
                [|{ TweenValue = v3Zero; TweenLength = length }
                  { TweenValue = v3 -radius 0.0f 0.0f; TweenLength = 0u.u }|])
              Positions
               (Sum, CosScaled repetitions, Once,
                [|{ TweenValue = v3Zero; TweenLength = length }
                  { TweenValue = v3 0.0f -radius 0.0f; TweenLength = 0u.u }|])
              Positions
               (Sum, Constant, Once,
                [|{ TweenValue = v3 0.0f radius 0.0f; TweenLength = length }
                  { TweenValue = v3 0.0f radius 0.0f; TweenLength = 0u.u }|])|]

    let makeHitPointsChangeEffect delta =
        let colorOpaque =
            if delta < 0
            then color8 (byte 255) (byte 255) (byte 255) (byte 255)
            else color8 (byte 0) (byte 255) (byte 255) (byte 255)
        let colorTransparent =
            colorOpaque.WithA8 (byte 0)
        { EffectName = "HitPointsChange"
          LifeTimeOpt = Some 80u.u
          Definitions = Map.empty
          Content =
            TextSprite
                (Resource (AssetTag.toPair Assets.Gui.Font),
                 scstring (abs delta),
                 [|Positions
                    (Sum, Linear, Bounce,
                     [|{ TweenValue = v3Zero; TweenLength = 10u.u }
                       { TweenValue = v3 0.0f 36.0f 0.0f; TweenLength = 10u.u }
                       { TweenValue = v3Zero; TweenLength = 10u.u }
                       { TweenValue = v3Zero; TweenLength = 50u.u }|])
                   Colors
                    (Set, EaseOut, Once,
                     [|{ TweenValue = colorOpaque; TweenLength = 50u.u }
                       { TweenValue = colorOpaque; TweenLength = 30u.u }
                       { TweenValue = colorTransparent; TweenLength = 0u.u }|])|],
                 Nil) }

    let makeCancelEffect () =
        { EffectName = "Cancel"
          LifeTimeOpt = Some 40u.u
          Definitions = Map.empty
          Content =
            StaticSprite
                (Resource (AssetTag.toPair Assets.Battle.CancelImage),
                 [|Angleses
                    (Sum, Linear, Bounce,
                     [|{ TweenValue = v3 0.0f 0.0f (single Math.PI * 2.0f); TweenLength = 10u.u }
                       { TweenValue = v3Zero; TweenLength = 30u.u }
                       { TweenValue = v3Zero; TweenLength = 0u.u }|])
                   Sizes
                    (Set, EaseOut, Once,
                     [|{ TweenValue = v3Zero; TweenLength = 10u.u }
                       { TweenValue = v3 156.0f 48.0f 0.0f; TweenLength = 30u.u }
                       { TweenValue = v3 156.0f 48.0f 0.0f; TweenLength = 0u.u }|])|],
                 Nil) }

    let makeBoltEffect () =
        let boltSprite =
            StaticSprite
                (Resource (AssetTag.toPair Assets.Battle.BoltAnimationSheet),
                 [|Insets
                    (Set, Constant, Once,
                     [|{ TweenValue = Box2 (0.0f,   0.0f,   64.0f,  256.0f); TweenLength = 5u.u }
                       { TweenValue = Box2 (64.0f,  0.0f,   64.0f,  256.0f); TweenLength = 5u.u }
                       { TweenValue = Box2 (128.0f, 0.0f,   64.0f,  256.0f); TweenLength = 5u.u }
                       { TweenValue = Box2 (128.0f, 0.0f,   64.0f,  256.0f); TweenLength = 65u.u }|])
                   Colors
                    (Set, EaseOut, Once,
                     [|{ TweenValue = Color.One; TweenLength = 40u.u }
                       { TweenValue = Color.One; TweenLength = 40u.u }
                       { TweenValue = Color.One.WithA8 (byte 0); TweenLength = 0u.u }|])|],
                 Nil)
        let explosionSprite =
            AnimatedSprite
                (Resource (AssetTag.toPair Assets.Battle.ExplosionAnimationSheet),
                 v2i 32 32, 4, 12, 2u.u, Once,
                 [|PositionRelative (v3 0.0f -384.0f 0.0f)
                   Size (v3 96.0f 96.0f 0.0f)
                   Colors
                    (Set, EaseOut, Once,
                     [|{ TweenValue = Color.One; TweenLength = 30u.u }
                       { TweenValue = Color.One; TweenLength = 30u.u }
                       { TweenValue = Color.One.WithA8 (byte 0); TweenLength = 0u.u }|])|],
                 Nil)
        let thunderSoundEffect =
            SoundEffect
                (Resource (AssetTag.toPair Assets.Field.ThunderSound),
                 [|Enableds (Equal, Once, [|{ LogicValue = true; LogicLength = 0u.u }; { LogicValue = false; LogicLength = 70u.u }|])|],
                 Nil)
        { EffectName = "Bolt"
          LifeTimeOpt = Some 80u.u
          Definitions = Map.empty
          Content =
            Contents
                (Shift 0.0f,
                 [|boltSprite
                   Delay (10u.u, explosionSprite)
                   Delay (10u.u, thunderSoundEffect)|]) }

    let makeImpactSplashEffect () =
        { EffectName = "ImpactSplash"
          LifeTimeOpt = Some 24u.u
          Definitions = Map.empty
          Content =
            Contents
                (Shift 0.0f,
                 [|AnimatedSprite
                    (Resource (AssetTag.toPair Assets.Battle.ImpactSplashAnimationSheet),
                     v2i 32 32, 3, 3, 8u.u, Once,
                     [|PositionRelative (v3 -48.0f 0.0f 0.0f); Size (v3 96.0f 96.0f 0.0f); Flip FlipH|],
                     Nil)
                   AnimatedSprite
                    (Resource (AssetTag.toPair Assets.Battle.ImpactSplashAnimationSheet),
                     v2i 32 32, 3, 3, 8u.u, Once,
                     [|PositionRelative (v3 48.0f 0.0f 0.0f); Size (v3 96.0f 96.0f 0.0f); Flip FlipNone|],
                     Nil)|]) }

    let makeCutEffect light =
        let image = if light then Assets.Battle.LightCutImage else Assets.Battle.CutImage
        { EffectName = "Cut"
          LifeTimeOpt = Some 24u.u
          Definitions = Map.empty
          Content =
              StaticSprite
               (Resource (AssetTag.toPair image),
                [|Colors
                   (Set, EaseOut, Once,
                    [|{ TweenValue = Color.One; TweenLength = 24u.u }
                      { TweenValue = Color.One.WithA8 (byte 0); TweenLength = 0u.u }|])|],
                Nil) }

    let makeSlashSpikeEffect position position2 =
        let spike = AnimatedSprite (Resource (AssetTag.toPair Assets.Battle.SpikeAnimationSheet), v2i 32 32, 5, 5, 3u.u, Once, [||], Nil)
        let emit =
            Emit
                (Shift 0.1f,
                 Rate 0.5f,
                 [|Positions (Set, Linear, Once, [|{ TweenValue = position; TweenLength = 60u.u }; { TweenValue = position2; TweenLength = 0u.u }|])|],
                 [|Size (v3 96.0f 96.0f 0.0f); Offset (v3 0.0f 0.5f 0.0f)|],
                 spike)
        { EffectName = "SlashSpike"
          LifeTimeOpt = Some 75u.u
          Definitions = Map.empty
          Content = emit }

    let makeCycloneBlurEffect radius =
        { EffectName = "CycloneBlur"
          LifeTimeOpt = Some 100u.u
          Definitions = Map.empty
          Content =
              AnimatedSprite
               (Resource (AssetTag.toPair Assets.Battle.CycloneBlurAnimationSheet),
                v2i 78 78, 2, 4, 3u.u, Loop,
                [|Circle (radius, 2.0f, 100u.u); Size (v3 234.0f 234.0f 0.0f)|],
                Nil) }

    let makeArcaneCastEffect () =
        let halfWidth = 50.0f
        let altitude = halfWidth * 2.0f * 0.866f
        let candle position = AnimatedSprite (Resource (AssetTag.toPair Assets.Battle.CandleAnimationSheet), v2i 16 20, 3, 3, 5u.u, Loop, [|Size (v3 64.0f 80.0f 0.0f); position|], Nil)
        let staticEffect position degrees = AnimatedSprite (Resource (AssetTag.toPair Assets.Battle.StaticAnimationSheet), v2i 64 64, 5, 5, 3u.u, Loop, [|Size (v3 128.0f 128.0f 0.0f); position; degrees|], Nil)
        { EffectName = "ArcaneCast"
          LifeTimeOpt = Some 36u.u
          Definitions = Map.empty
          Content =
            Contents
                (Shift 0.0f,
                 [|candle (PositionRelative (v3 0.0f altitude 0.0f))
                   candle (PositionRelative (v3 -halfWidth 0.0f 0.0f))
                   candle (PositionRelative (v3 halfWidth 0.0f 0.0f))
                   staticEffect (PositionRelative (v3 0.0f 0.0f 0.0f)) (Degrees (v3 0.0f 0.0f 90.0f))
                   staticEffect (PositionRelative (v3 -25.0f 50.0f 0.0f)) (Degrees (v3 0.0f 0.0f 30.0f))
                   staticEffect (PositionRelative (v3 25.0f 50.0f 0.0f)) (Degrees (v3 0.0f 0.0f -30.0f))|]) }

    let makeFireEffect position position2 =
        let fireSize = Size (v3 64.0f 64.0f 0.0f)
        let activation timeOn timeOff = Enableds (Equal, Once, [|{ LogicValue = true; LogicLength = timeOn}; { LogicValue = false; LogicLength = timeOff }|])
        let linearTravel position position2 duration = Positions (Set, EaseOut, Once, [|{ TweenValue = position; TweenLength = duration }; { TweenValue = position2; TweenLength = 0u.u }|])
        let fire playback aspects =
            AnimatedSprite
             (Resource (AssetTag.toPair Assets.Battle.FireAnimationSheet),
              v2i 16 16, 4, 4, 3u.u, playback, aspects, Nil)
        let burn =
            AnimatedSprite
             (Resource (AssetTag.toPair Assets.Battle.BurnAnimationSheet),
              v2i 16 16, 4, 4, 3u.u, Once, [||], Nil)
        let fireball travel activation =
            Contents
                (Shift 0.0f,
                 [|fire Loop [|travel; fireSize; activation|];
                   Emit (Shift 0.0f, Rate 0.3f, [|travel; activation|], [||], fire Once [|fireSize|]) |])
        { EffectName = "Fire"
          LifeTimeOpt = Some 100u.u
          Definitions = Map.empty
          Content = 
            Contents
                (Shift 0.0f,
                 [|fireball (Circle (64.0f, 1.5f, 40u.u)) (activation 40u.u 60u.u)
                   Delay (40u.u, fireball (Aspects [|linearTravel position position2 20u.u|]) (activation 20u.u 40u.u))
                   Delay (60u.u, Emit (Shift 0.0f, Rate 0.1f, [||], [|linearTravel position2 (position2 + (v3 0.0f 70.0f 0.0f)) 20u.u|], burn))|]) }

    let makeFlameEffect position position2 =
        { EffectName = "Flame"
          LifeTimeOpt = Some 100u.u
          Definitions = Map.empty
          Content =
            Emit
                (Shift 0.0f,
                 Rate 0.25f,
                 [|Enableds (Equal, Once, [|{ LogicValue = true; LogicLength = 64u.u }; { LogicValue = false; LogicLength = 0u.u }|])|],
                 [|Positions (Set, EaseIn, Once, [|{ TweenValue = position; TweenLength = 36u.u }; { TweenValue = position2; TweenLength = 0u.u }|])
                   Sizes (Set, Linear, Once, [|{ TweenValue = v3 32.0f 32.0f 0.0f; TweenLength = 36u.u }; { TweenValue = v3 192.0f 192.0f 0.0f; TweenLength = 0u.u }|])
                   Degrees (v3 0.0f 0.0f 0.0f)
                   Color (Color.One.WithA8 (byte 207))|],
                 AnimatedSprite (Resource (AssetTag.toPair Assets.Battle.FlameAnimationSheet), v2i 64 64, 6, 6, 6u.u, Once, [||], Nil))}

    let makeIceEffect () =
        let coverRadius = 50.0f
        let bombardActivation = Enableds (Equal, Once, [|{ LogicValue = true; LogicLength = 10u.u };{ LogicValue = false; LogicLength = 0u.u }|])
        let bombardTravel origin = Positions (Sum, Linear, Once, [|{ TweenValue = origin; TweenLength = 10u.u };{ TweenValue = v3Zero; TweenLength = 0u.u }|])
        let coverTravel =
            Aspects
                [|Positions
                   (Sum, Linear, Loop,
                    [|{ TweenValue = v3 0.0f -coverRadius 0.0f; TweenLength = 10u.u }
                      { TweenValue = v3 coverRadius 0.0f 0.0f; TweenLength = 0u.u }|])
                  Positions
                   (Sum, Random, Loop,
                    [|{ TweenValue = v3Zero; TweenLength = 80u.u }
                      { TweenValue = v3 -coverRadius coverRadius 0.0f; TweenLength = 0u.u }|])|]
        let ice = StaticSprite (Resource (AssetTag.toPair Assets.Battle.IceImage), [|Size (v3 192.0f 192.0f 0.0f)|], Nil)
        let iceBombard origin = Emit (Shift 0.0f, Rate 0.2f, [||], [|bombardTravel origin; bombardActivation|], ice)
        let iceCover = Emit (Shift 0.0f, Rate 1.0f, [|coverTravel|], [||], ice)
        { EffectName = "Ice"
          LifeTimeOpt = Some 80u.u
          Definitions = Map.empty
          Content =
            Contents
                (Shift 0.0f,
                 [|iceCover
                   iceBombard (v3 -700.0f 0.0f 0.0f)
                   iceBombard (v3 500.0f 500.0f 0.0f)
                   iceBombard (v3 500.0f -500.0f 0.0f)|])}

    let makeSnowballEffect () =
        let fall = Positions (Sum, Linear, Once, [|{ TweenValue = v3 0.0f 800.0f 0.0f; TweenLength = 80u.u };{ TweenValue = v3 0.0f -800.0f 0.0f; TweenLength = 0u.u }|])
        let rotate =
            Degreeses (Set, Constant, Loop,
                [|{ TweenValue = v3Zero; TweenLength = 5u.u }
                  { TweenValue = v3 0.0f 0.0f 90.0f; TweenLength = 5u.u }
                  { TweenValue = v3 0.0f 0.0f 180.0f; TweenLength = 5u.u }
                  { TweenValue = v3 0.0f 0.0f 270.0f; TweenLength = 5u.u }|])
        { EffectName = "Snowball"
          LifeTimeOpt = Some 80u.u
          Definitions = Map.empty
          Content =
              StaticSprite
               (Resource (AssetTag.toPair Assets.Battle.SnowballImage),
                [|Size (v3 432.0f 432.0f 0.0f); fall; rotate|], Nil) }

    let makeHolyCastEffect () =
        { EffectName = "HolyCast"
          LifeTimeOpt = Some 36u.u
          Definitions = Map.empty
          Content =
              AnimatedSprite
               (Resource (AssetTag.toPair Assets.Battle.HolyCastAnimationSheet),
                v2i 100 100, 6, 36, 1u.u, Once, [||], Nil) }

    let makePurifyEffect () =
        let sprite position =
            AnimatedSprite
             (Resource (AssetTag.toPair Assets.Battle.PurifyAnimationSheet),
              v2i 64 64, 5, 10, 3u.u, Once, [|PositionRelative position|], Nil)
        { EffectName = "Purify"
          LifeTimeOpt = Some 54u.u
          Definitions = Map.empty
          Content =
            Contents
                (Shift 0.0f,
                 [|sprite v3Zero;
                   Delay (12u.u, sprite (v3 -16.0f 64.0f 0.0f));
                   Delay (24u.u, sprite (v3 16.0f 32.0f 0.0f))|])}

    let makeCureEffect () =
        let path =
            Aspects
                [|Positions
                   (Sum, Sin, Loop,
                    [|{ TweenValue = v3Zero; TweenLength = 3u.u }
                      { TweenValue = v3 70.0f 0.0f 0.0f; TweenLength = 0u.u }|])
                  Positions
                   (Sum, EaseOut, Loop,
                    [|{ TweenValue = v3Zero; TweenLength = 21u.u }
                      { TweenValue = v3 0.0f 250.0f 0.0f; TweenLength = 0u.u }|])
                  Positions
                   (Sum, Constant, Loop,
                    [|{ TweenValue = v3 0.0f -100.0f 0.0f; TweenLength = 1u.u }|])|]
        let sparkle =
            AnimatedSprite
             (Resource (AssetTag.toPair Assets.Battle.SparkleAnimationSheet),
              v2i 16 16, 6, 6, 4u.u, Once, [||], Nil)
        { EffectName = "Cure"
          LifeTimeOpt = Some 100u.u
          Definitions = Map.empty
          Content = Emit (Shift 0.0f, Rate 0.2f, [|path|], [||], sparkle)}

    let makeProtectEffect () =
        let protection aspects = StaticSprite (Resource (AssetTag.toPair Assets.Battle.ProtectSphereImage), aspects, Nil)
        let blink = Enableds (Equal, Loop, [|{ LogicValue = true; LogicLength = 1u.u };{ LogicValue = false; LogicLength = 2u.u }|])
        let outwardReach = 64.0f
        let clockwiseBias = 50.0f
        let bend dest = Positions (Sum, EaseIn, Once, [|{ TweenValue = v3Zero; TweenLength = 30u.u }; { TweenValue = dest; TweenLength = 0u.u }|])
        let outwardMovement dest = Positions (Sum, Linear, Once, [|{ TweenValue = v3Zero; TweenLength = 30u.u }; { TweenValue = dest; TweenLength = 0u.u }|])
        { EffectName = "Protect"
          LifeTimeOpt = Some 80u.u
          Definitions = Map.empty
          Content =
            Contents
                (Shift 0.0f,
                 [|protection
                    [|blink
                      Enableds (And, Once, [|{ LogicValue = true; LogicLength = 50u.u }; { LogicValue = false; LogicLength = 0u.u }|])
                      Sizes (Set, Ease, Once, [|{ TweenValue = v3 192.0f 192.0f 0.0f; TweenLength = 50u.u }; { TweenValue = v3 64.0f 64.0f 0.0f; TweenLength = 0u.u }|])|]
                   Delay (50u.u,
                     (Contents
                          (Shift 0.0f,
                           [|protection [|blink; outwardMovement (v3 outwardReach outwardReach 0.0f); bend (v3 0.0f -clockwiseBias 0.0f)|]
                             protection [|blink; outwardMovement (v3 outwardReach -outwardReach 0.0f); bend (v3 -clockwiseBias 0.0f 0.0f)|]
                             protection [|blink; outwardMovement (v3 -outwardReach -outwardReach 0.0f); bend (v3 0.0f clockwiseBias 0.0f)|]
                             protection [|blink; outwardMovement (v3 -outwardReach outwardReach 0.0f); bend (v3 clockwiseBias 0.0f 0.0f)|]|])))|])}

    let makeDimensionalCastEffect () =
        let length = 60u.u
        let electronSize = Size (v3 9.0f 9.0f 0.0f)
        let nonLocationSize = Size (v3 3.0f 3.0f 0.0f)
        let positionAdjustY = PositionRelative (v3 0.0f -36.0f 0.0f)
        let fade =
            Colors
               (Set, EaseOut, Once,
                [|{ TweenValue = Color.One; TweenLength = 24u.u }
                  { TweenValue = Color.One.WithA8 (byte 0); TweenLength = 0u.u }|])
        let orbit radiusX radiusY repetitions =
            Aspects
                [|Positions
                   (Sum, SinScaled repetitions, Once,
                    [|{ TweenValue = v3Zero; TweenLength = length }
                      { TweenValue = v3 -radiusX 0.0f 0.0f; TweenLength = 0u.u }|])
                  Positions
                   (Sum, CosScaled repetitions, Once,
                    [|{ TweenValue = v3Zero; TweenLength = length }
                      { TweenValue = v3 0.0f -radiusY 0.0f; TweenLength = 0u.u }|])
                  Positions
                   (Sum, Constant, Once,
                    [|{ TweenValue = v3 0.0f radiusY 0.0f; TweenLength = length }
                      { TweenValue = v3 0.0f radiusY 0.0f; TweenLength = 0u.u }|])|]
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
                [|{ TweenValue = v3Zero; TweenLength = 20u.u }
                  { TweenValue = v3 96.0f 96.0f 0.0f; TweenLength = 10u.u }
                  { TweenValue = v3 96.0f 96.0f 0.0f; TweenLength = 20u.u }|])
        { EffectName = "Buff"
          LifeTimeOpt = Some 50u.u
          Definitions = Map.empty
          Content =
              Contents
                  (Shift 0.0f,
                   [|StaticSprite (Resource (AssetTag.toPair image), [|shrink; PositionRelative (v3 0.0f 32.0f 0.0f)|], Nil)|])}

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
                [|{ TweenValue = v3 96.0f 96.0f 0.0f; TweenLength = 20u.u }
                  { TweenValue = v3 96.0f 96.0f 0.0f; TweenLength = 10u.u }
                  { TweenValue = v3 48.0f 48.0f 0.0f; TweenLength = 20u.u }|])
        { EffectName = "Debuff"
          LifeTimeOpt = Some 50u.u
          Definitions = Map.empty
          Content =
              Contents
                  (Shift 0.0f,
                   [|StaticSprite (Resource (AssetTag.toPair image), [|shrink; PositionRelative (v3 0.0f 32.0f 0.0f)|], Nil)|])}

    let makeConjureIfritEffect () =
        let fireSpinSize = Size (v3 600.0f 600.0f 0.0f)
        let fireSpin aspects =
            AnimatedSprite
             (Resource (AssetTag.toPair Assets.Battle.FireSpinAnimationSheet),
              v2i 100 100, 8, 61, 1u.u, Loop, aspects, Nil)
        { EffectName = "ConjureIfrit"
          LifeTimeOpt = Some 80u.u
          Definitions = Map.empty
          Content =
              Contents
                  (Shift 0.0f,
                   [|fireSpin [|fireSpinSize; PositionAbsolute (v3 -144.0f 168.0f 0.0f)|]
                     fireSpin [|fireSpinSize; PositionAbsolute (v3 144.0f 168.0f 0.0f)|]
                     fireSpin [|fireSpinSize; PositionAbsolute (v3 -144.0f -111.0f 0.0f)|]
                     fireSpin [|fireSpinSize; PositionAbsolute (v3 144.0f -111.0f 0.0f)|]|])}

    let makeHopEffect start stop =
        { EffectName = "Hop"
          LifeTimeOpt = Some 20u.u // +2 due to rendering / update order
          Definitions = Map.empty
          Content = Tag ("Tag", [|Hop (start, stop, 24.0f, 18u.u)|], Nil) }

    let makeCircleEffect radius =
        { EffectName = "Circle"
          LifeTimeOpt = Some 100u.u // +2 due to rendering / update order
          Definitions = Map.empty
          Content = Tag ("Tag", [|Circle (radius, 2.0f, 98u.u)|], Nil) }