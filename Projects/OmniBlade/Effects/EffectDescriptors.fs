// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open Prime
open Nu
open Nu.Effects

[<RequireQualifiedAccess>]
module EffectDescriptors =

    let hop start stop =
        { EffectName = "Hop"
          LifeTimeOpt = Some 20L
          Definitions = Map.empty
          Content = Tag ("Tag", [|Hop (start, stop, 24.0f, 20L)|], Nil) }

    let circle radius =
        { EffectName = "Circle"
          LifeTimeOpt = Some 100L
          Definitions = Map.empty
          Content = Tag ("Tag", [|Circle (radius, 2.0f, 100L)|], Nil) }

    let hitPointsChange delta =
        let colorOpaque =
            if delta < 0
            then color8 (byte 255) (byte 255) (byte 255) (byte 255)
            else color8 (byte 0) (byte 255) (byte 255) (byte 255)
        let colorTransparent =
            colorOpaque.WithA8 (byte 0)
        { EffectName = "HitPointsChange"
          LifeTimeOpt = Some 80L
          Definitions = Map.empty
          Content =
            TextSprite
                (Resource (AssetTag.toPair Assets.Gui.Font),
                 scstring (abs delta), None, Set.empty,
                 [|Positions
                    (Sum, Linear, Bounce,
                     [|{ TweenValue = v3Zero; TweenLength = 10L }
                       { TweenValue = v3 0.0f 36.0f 0.0f; TweenLength = 10L }
                       { TweenValue = v3Zero; TweenLength = 10L }
                       { TweenValue = v3Zero; TweenLength = 50L }|])
                   Colors
                    (Set, EaseOut, Once,
                     [|{ TweenValue = colorOpaque; TweenLength = 50L }
                       { TweenValue = colorOpaque; TweenLength = 30L }
                       { TweenValue = colorTransparent; TweenLength = 0L }|])|],
                 Nil) }

    let cancel =
        { EffectName = "Cancel"
          LifeTimeOpt = Some 40L
          Definitions = Map.empty
          Content =
            StaticSprite
                (Resource (AssetTag.toPair Assets.Battle.CancelImage),
                 [|Angleses
                    (Sum, Linear, Bounce,
                     [|{ TweenValue = v3 0.0f 0.0f (MathF.TWO_PI); TweenLength = 10L }
                       { TweenValue = v3Zero; TweenLength = 30L }
                       { TweenValue = v3Zero; TweenLength = 0L }|])
                   Sizes
                    (Set, EaseOut, Once,
                     [|{ TweenValue = v3Zero; TweenLength = 10L }
                       { TweenValue = v3 156.0f 48.0f 0.0f; TweenLength = 30L }
                       { TweenValue = v3 156.0f 48.0f 0.0f; TweenLength = 0L }|])|],
                 Nil) }

    let cut light =
        let image = if light then Assets.Battle.LightCutImage else Assets.Battle.CutImage
        { EffectName = "Cut"
          LifeTimeOpt = Some 24L
          Definitions = Map.empty
          Content =
              StaticSprite
               (Resource (AssetTag.toPair image),
                [|Colors
                   (Set, EaseOut, Once,
                    [|{ TweenValue = Color.One; TweenLength = 24L }
                      { TweenValue = Color.One.WithA8 (byte 0); TweenLength = 0L }|])|],
                Nil) }

    let critical =
        { EffectName = "Critical"
          LifeTimeOpt = Some 24L
          Definitions = Map.empty
          Content =
            AnimatedSprite
                (Resource (AssetTag.toPair Assets.Battle.CriticalAnimationSheet),
                 v2i 32 32, 8, 8, 3L, Once,
                 [|Size (v3 96.0f 96.0f 0.0f)|],
                 Nil) }

    let heavyCritical =
        { EffectName = "HeavyCritical"
          LifeTimeOpt = Some 42L
          Definitions = Map.empty
          Content =
            AnimatedSprite
                (Resource (AssetTag.toPair Assets.Battle.HeavyCriticalAnimationSheet),
                 v2i 32 32, 14, 14, 3L, Once,
                 [|Size (v3 96.0f 96.0f 0.0f)|],
                 Nil) }

    let poisonCut =
        { EffectName = "PoisonCut"
          LifeTimeOpt = Some 25L
          Definitions = Map.empty
          Content =
            AnimatedSprite
                (Resource (AssetTag.toPair Assets.Battle.PoisonCutAnimationSheet),
                 v2i 32 32, 5, 5, 5L, Once,
                 [|Size (v3 96.0f 96.0f 0.0f)|],
                 Nil) }

    let powerCut =
        { EffectName = "PowerCut"
          LifeTimeOpt = Some 24L
          Definitions = Map.empty
          Content =
            AnimatedSprite
                (Resource (AssetTag.toPair Assets.Battle.PowerCutAnimationSheet),
                 v2i 48 48, 8, 8, 3L, Once,
                 [|Size (v3 144.0f 144.0f 0.0f)|],
                 Nil) }

    let dispelCut =
        { EffectName = "DispelCut"
          LifeTimeOpt = Some 24L
          Definitions = Map.empty
          Content =
            AnimatedSprite
                (Resource (AssetTag.toPair Assets.Battle.DispelCutAnimationSheet),
                 v2i 64 64, 8, 8, 3L, Once,
                 [|Size (v3 192.0f 192.0f 0.0f)|],
                 Nil) }

    let doubleCut =
        { EffectName = "DoubleCut"
          LifeTimeOpt = Some 30L
          Definitions = Map.empty
          Content =
            AnimatedSprite
                (Resource (AssetTag.toPair Assets.Battle.DoubleCutAnimationSheet),
                 v2i 48 48, 10, 8, 3L, Once,
                 [|Size (v3 144.0f 144.0f 0.0f)|],
                 Nil) }

    let slashSpike position position2 =
        let spike = AnimatedSprite (Resource (AssetTag.toPair Assets.Battle.SpikeAnimationSheet), v2i 32 32, 5, 5, 3L, Once, [||], Nil)
        let emit =
            Emit
                (Shift 0.1f,
                 Rate 0.5f,
                 [|Positions (Set, Linear, Once, [|{ TweenValue = position; TweenLength = 60L }; { TweenValue = position2; TweenLength = 0L }|])|],
                 [|Size (v3 96.0f 96.0f 0.0f); Offset (v3 0.0f 0.5f 0.0f)|],
                 spike)
        { EffectName = "SlashSpike"
          LifeTimeOpt = Some 75L
          Definitions = Map.empty
          Content = emit }

    let slashWind position position2 =
        let twister = AnimatedSprite (Resource (AssetTag.toPair Assets.Battle.CriticalSlashAnimationSheet), v2i 32 32, 8, 8, 3L, Once, [||], Nil)
        let emit =
            Emit
                (Shift 0.1f,
                 Rate 0.333f,
                 [|Positions (Set, Linear, Once, [|{ TweenValue = position; TweenLength = 60L }; { TweenValue = position2; TweenLength = 0L }|])|],
                 [|Size (v3 96.0f 96.0f 0.0f); Offset (v3 0.0f 0.5f 0.0f)|],
                 twister)
        { EffectName = "SlashTwister"
          LifeTimeOpt = Some 75L
          Definitions = Map.empty
          Content = emit }

    let cycloneBlur radius =
        { EffectName = "CycloneBlur"
          LifeTimeOpt = Some 98L
          Definitions = Map.empty
          Content =
              AnimatedSprite
               (Resource (AssetTag.toPair Assets.Battle.CycloneBlurAnimationSheet),
                v2i 78 78, 4, 2, 3L, Loop,
                [|Circle (radius, 2.0f, 98L); Size (v3 234.0f 234.0f 0.0f)|],
                Nil) }

    let buff statusType =
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
                [|{ TweenValue = v3Zero; TweenLength = 20L }
                  { TweenValue = v3 96.0f 96.0f 0.0f; TweenLength = 10L }
                  { TweenValue = v3 96.0f 96.0f 0.0f; TweenLength = 20L }|])
        { EffectName = "Buff"
          LifeTimeOpt = Some 50L
          Definitions = Map.empty
          Content =
              Contents
                  (Shift 0.0f,
                   [|StaticSprite (Resource (AssetTag.toPair image), [|shrink; Position (v3 0.0f 32.0f 0.0f)|], Nil)|]) }

    let debuff statusType =
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
                [|{ TweenValue = v3 96.0f 96.0f 0.0f; TweenLength = 20L }
                  { TweenValue = v3 96.0f 96.0f 0.0f; TweenLength = 10L }
                  { TweenValue = v3 48.0f 48.0f 0.0f; TweenLength = 20L }|])
        { EffectName = "Debuff"
          LifeTimeOpt = Some 50L
          Definitions = Map.empty
          Content =
              Contents
                  (Shift 0.0f,
                   [|StaticSprite (Resource (AssetTag.toPair image), [|shrink; Position (v3 0.0f 32.0f 0.0f)|], Nil)|]) }

    let impactSplash =
        { EffectName = "ImpactSplash"
          LifeTimeOpt = Some 24L
          Definitions = Map.empty
          Content =
            Contents
                (Shift 0.0f,
                 [|AnimatedSprite
                    (Resource (AssetTag.toPair Assets.Battle.ImpactSplashAnimationSheet),
                     v2i 32 32, 3, 3, 8L, Once,
                     [|Position (v3 -48.0f 0.0f 0.0f); Size (v3 96.0f 96.0f 0.0f); Flip FlipH|],
                     Nil)
                   AnimatedSprite
                    (Resource (AssetTag.toPair Assets.Battle.ImpactSplashAnimationSheet),
                     v2i 32 32, 3, 3, 8L, Once,
                     [|Position (v3 48.0f 0.0f 0.0f); Size (v3 96.0f 96.0f 0.0f); Flip FlipNone|],
                     Nil)|]) }

    let arcaneCast =
        let fade =
            Colors
               (Set, EaseOut, Once,
                [|{ TweenValue = Color.One; TweenLength = 30L }
                  { TweenValue = Color.One; TweenLength = 30L }
                  { TweenValue = Color.One.WithA8 (byte 0); TweenLength = 0L }|])
        let candle position =
            AnimatedSprite
                (Resource (AssetTag.toPair Assets.Battle.CandleAnimationSheet), v2i 16 20, 3, 3, 5L, Loop,
                 [|Size (v3 48.0f 60.0f 0.0f); position; fade|],
                 Nil)
        let staticEffect position degrees =
            AnimatedSprite
                (Resource (AssetTag.toPair Assets.Battle.StaticAnimationSheet), v2i 64 64, 5, 5, 3L, Loop,
                 [|Size (v3 192.0f 192.0f 0.0f); position; degrees; fade|],
                 Nil)
        { EffectName = "ArcaneCast"
          LifeTimeOpt = Some 60L
          Definitions = Map.empty
          Content =
            Contents
                (Shift 0.0f,
                 [|candle (Position (v3 -72.0f 0.0f 0.0f))
                   candle (Position (v3 72.0f 0.0f 0.0f))
                   staticEffect (Position (v3 0.0f 18.0f 0.0f)) (Degrees (v3 0.0f 0.0f -90.0f))|]) }

    let holyCast =
        { EffectName = "HolyCast"
          LifeTimeOpt = Some 36L
          Definitions = Map.empty
          Content =
              AnimatedSprite
               (Resource (AssetTag.toPair Assets.Battle.HolyCastAnimationSheet),
                v2i 100 100, 36, 6, 1L, Once, [||], Nil) }

    let genericCast =
        let fadeOut =
            Colors
               (Set, Linear, Once,
                [|{ TweenValue = Color.One; TweenLength = 40L }
                  { TweenValue = Color.One.WithA8 (byte 0); TweenLength = 25L }|])
        let fadeIn =
            Colors
               (Set, Linear, Once,
                [|{ TweenValue = Color.One.WithA8 (byte 0); TweenLength = 30L }
                  { TweenValue = Color.One; TweenLength = 35L }
                  { TweenValue = Color.One.WithA8 (byte 0); TweenLength = 0L }|])
        let floorLight =
            AnimatedSprite
               (Resource (AssetTag.toPair Assets.Battle.FloorLightAnimationSheet),
                v2i 64 64, 12, 12, 2L, Loop, [|Size (v3 192.0f 192.0f 0.0f); fadeOut|], Nil)
        let floorBeam =
            AnimatedSprite
               (Resource (AssetTag.toPair Assets.Battle.FloorBeamAnimationSheet),
                v2i 64 64, 5, 5, 2L, Loop, [|Size (v3 192.0f 192.0f 0.0f); fadeIn|], Nil)
        { EffectName = "GenericCast"
          LifeTimeOpt = Some 65L
          Definitions = Map.empty
          Content =
            Contents
                (Shift 0.0f,
                 [|floorLight
                   floorBeam|]) }

    let dimensionalCast =
        let length = 60L
        let electronSize = Size (v3 9.0f 9.0f 0.0f)
        let nonLocationSize = Size (v3 3.0f 3.0f 0.0f)
        let positionAdjustY = Position (v3 0.0f -36.0f 0.0f)
        let fade =
            Colors
               (Set, EaseOut, Once,
                [|{ TweenValue = Color.One; TweenLength = 24L }
                  { TweenValue = Color.One.WithA8 (byte 0); TweenLength = 0L }|])
        let orbit radiusX radiusY repetitions =
            Aspects
                [|Positions
                   (Sum, SinScaled repetitions, Once,
                    [|{ TweenValue = v3Zero; TweenLength = length }
                      { TweenValue = v3 -radiusX 0.0f 0.0f; TweenLength = 0L }|])
                  Positions
                   (Sum, CosScaled repetitions, Once,
                    [|{ TweenValue = v3Zero; TweenLength = length }
                      { TweenValue = v3 0.0f -radiusY 0.0f; TweenLength = 0L }|])
                  Positions
                   (Sum, Constant, Once,
                    [|{ TweenValue = v3 0.0f radiusY 0.0f; TweenLength = length }
                      { TweenValue = v3 0.0f radiusY 0.0f; TweenLength = 0L }|])|]
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
                     StaticSprite (Resource (AssetTag.toPair Assets.Battle.ElectronGreenImage), [|orbitV; electronSize; positionAdjustY|], Nil)
                     Emit (Shift 0.0f, Rate 1.0f, [|orbitV; positionAdjustY|], [||], StaticSprite (Resource (AssetTag.toPair Assets.Battle.NonLocationGreenImage), [|nonLocationSize; fade|], Nil))|]) }

    let fire position position2 =
        let fireSize = Size (v3 64.0f 64.0f 0.0f)
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
          LifeTimeOpt = Some 129L
          Definitions = Map.empty
          Content = 
            Contents
                (Shift 0.0f,
                 [|fireball (Circle (64.0f, 1.5f, 40L)) (activation 40L 60L)
                   Delay (40L, fireball (Aspects [|linearTravel position position2 20L|]) (activation 20L 40L))
                   Delay (60L, Emit (Shift 0.0f, Rate 0.1f, [||], [|linearTravel position2 (position2 + (v3 0.0f 72.0f 0.0f)) 20L|], burn))|]) }

    let flame position position2 =
        { EffectName = "Flame"
          LifeTimeOpt = Some 100L
          Definitions = Map.empty
          Content =
            Emit
                (Shift 0.0f,
                 Rate 0.25f,
                 [|Enableds (Equal, Once, [|{ LogicValue = true; LogicLength = 64L }; { LogicValue = false; LogicLength = 0L }|])|],
                 [|Positions (Set, EaseIn, Once, [|{ TweenValue = position; TweenLength = 36L }; { TweenValue = position2; TweenLength = 0L }|])
                   Sizes (Set, Linear, Once, [|{ TweenValue = v3 32.0f 32.0f 0.0f; TweenLength = 36L }; { TweenValue = v3 192.0f 192.0f 0.0f; TweenLength = 0L }|])
                   Degrees (v3 0.0f 0.0f 0.0f)
                   Color (Color.One.WithA8 (byte 207))|],
                 AnimatedSprite (Resource (AssetTag.toPair Assets.Battle.FlameAnimationSheet), v2i 64 64, 6, 6, 6L, Once, [||], Nil)) }

    let ice =
        let coverRadius = 50.0f
        let bombardActivation = Enableds (Equal, Once, [|{ LogicValue = true; LogicLength = 10L };{ LogicValue = false; LogicLength = 0L }|])
        let bombardTravel origin = Positions (Sum, Linear, Once, [|{ TweenValue = origin; TweenLength = 10L };{ TweenValue = v3Zero; TweenLength = 0L }|])
        let coverTravel =
            Aspects
                [|Positions
                   (Sum, Linear, Loop,
                    [|{ TweenValue = v3 0.0f -coverRadius 0.0f; TweenLength = 10L }
                      { TweenValue = v3 coverRadius 0.0f 0.0f; TweenLength = 0L }|])
                  Positions
                   (Sum, Random, Loop,
                    [|{ TweenValue = v3Zero; TweenLength = 80L }
                      { TweenValue = v3 -coverRadius coverRadius 0.0f; TweenLength = 0L }|])|]
        let ice = StaticSprite (Resource (AssetTag.toPair Assets.Battle.IceImage), [|Size (v3 192.0f 192.0f 0.0f)|], Nil)
        let iceBombard origin = Emit (Shift 0.0f, Rate 0.2f, [||], [|bombardTravel origin; bombardActivation|], ice)
        let iceCover = Emit (Shift 0.0f, Rate 1.0f, [|coverTravel|], [||], ice)
        { EffectName = "Ice"
          LifeTimeOpt = Some 80L
          Definitions = Map.empty
          Content =
            Contents
                (Shift 0.0f,
                 [|iceCover
                   iceBombard (v3 -700.0f 0.0f 0.0f)
                   iceBombard (v3 500.0f 500.0f 0.0f)
                   iceBombard (v3 500.0f -500.0f 0.0f)|]) }

    let snowball =
        let fall = Positions (Sum, Linear, Once, [|{ TweenValue = v3 0.0f 800.0f 0.0f; TweenLength = 80L }; { TweenValue = v3 0.0f -800.0f 0.0f; TweenLength = 0L }|])
        let rotate =
            Degreeses (Set, Constant, Loop,
                [|{ TweenValue = v3Zero; TweenLength = 5L }
                  { TweenValue = v3 0.0f 0.0f 90.0f; TweenLength = 5L }
                  { TweenValue = v3 0.0f 0.0f 180.0f; TweenLength = 5L }
                  { TweenValue = v3 0.0f 0.0f 270.0f; TweenLength = 5L }|])
        { EffectName = "Snowball"
          LifeTimeOpt = Some 80L
          Definitions = Map.empty
          Content =
              StaticSprite
               (Resource (AssetTag.toPair Assets.Battle.SnowballImage),
                [|Size (v3 432.0f 432.0f 0.0f); fall; rotate|], Nil) }

    let cure =
        let path =
            Aspects
                [|Positions
                   (Sum, Sin, Loop,
                    [|{ TweenValue = v3Zero; TweenLength = 3L }
                      { TweenValue = v3 70.0f 0.0f 0.0f; TweenLength = 0L }|])
                  Positions
                   (Sum, EaseOut, Loop,
                    [|{ TweenValue = v3Zero; TweenLength = 21L }
                      { TweenValue = v3 0.0f 250.0f 0.0f; TweenLength = 0L }|])
                  Positions
                   (Sum, Constant, Loop,
                    [|{ TweenValue = v3 0.0f -100.0f 0.0f; TweenLength = 1L }|])|]
        let sparkle =
            AnimatedSprite
             (Resource (AssetTag.toPair Assets.Battle.SparkleAnimationSheet),
              v2i 16 16, 6, 6, 4L, Once, [||], Nil)
        { EffectName = "Cure"
          LifeTimeOpt = Some 100L
          Definitions = Map.empty
          Content = Emit (Shift 0.0f, Rate 0.2f, [|path|], [||], sparkle) }

    let protect =
        let protection aspects = StaticSprite (Resource (AssetTag.toPair Assets.Battle.ProtectSphereImage), aspects, Nil)
        let blink = Enableds (Equal, Loop, [|{ LogicValue = true; LogicLength = 1L };{ LogicValue = false; LogicLength = 2L }|])
        let outwardReach = 64.0f
        let clockwiseBias = 50.0f
        let bend dest = Positions (Sum, EaseIn, Once, [|{ TweenValue = v3Zero; TweenLength = 30L }; { TweenValue = dest; TweenLength = 0L }|])
        let outwardMovement dest = Positions (Sum, Linear, Once, [|{ TweenValue = v3Zero; TweenLength = 30L }; { TweenValue = dest; TweenLength = 0L }|])
        { EffectName = "Protect"
          LifeTimeOpt = Some 80L
          Definitions = Map.empty
          Content =
            Contents
                (Shift 0.0f,
                 [|protection
                    [|blink
                      Enableds (And, Once, [|{ LogicValue = true; LogicLength = 50L }; { LogicValue = false; LogicLength = 0L }|])
                      Sizes (Set, Ease, Once, [|{ TweenValue = v3 192.0f 192.0f 0.0f; TweenLength = 50L }; { TweenValue = v3 64.0f 64.0f 0.0f; TweenLength = 0L }|])|]
                   Delay (50L,
                     (Contents
                          (Shift 0.0f,
                           [|protection [|blink; outwardMovement (v3 outwardReach outwardReach 0.0f); bend (v3 0.0f -clockwiseBias 0.0f)|]
                             protection [|blink; outwardMovement (v3 outwardReach -outwardReach 0.0f); bend (v3 -clockwiseBias 0.0f 0.0f)|]
                             protection [|blink; outwardMovement (v3 -outwardReach -outwardReach 0.0f); bend (v3 0.0f clockwiseBias 0.0f)|]
                             protection [|blink; outwardMovement (v3 -outwardReach outwardReach 0.0f); bend (v3 clockwiseBias 0.0f 0.0f)|]|])))|]) }

    let purify =
        let sprite position =
            AnimatedSprite
             (Resource (AssetTag.toPair Assets.Battle.PurifyAnimationSheet),
              v2i 64 64, 10, 5, 3L, Once, [|Position position|], Nil)
        { EffectName = "Purify"
          LifeTimeOpt = Some 54L
          Definitions = Map.empty
          Content =
            Contents
                (Shift 0.0f,
                 [|sprite v3Zero;
                   Delay (12L, sprite (v3 -16.0f 64.0f 0.0f));
                   Delay (24L, sprite (v3 16.0f 32.0f 0.0f))|]) }

    let bolt =
        let boltSprite =
            StaticSprite
                (Resource (AssetTag.toPair Assets.Battle.BoltAnimationSheet),
                 [|Insets
                    (Set, Constant, Once,
                     [|{ TweenValue = Box2 (0.0f,   0.0f,   64.0f,  256.0f); TweenLength = 5L }
                       { TweenValue = Box2 (64.0f,  0.0f,   64.0f,  256.0f); TweenLength = 5L }
                       { TweenValue = Box2 (128.0f, 0.0f,   64.0f,  256.0f); TweenLength = 5L }
                       { TweenValue = Box2 (128.0f, 0.0f,   64.0f,  256.0f); TweenLength = 65L }|])
                   Colors
                    (Set, EaseOut, Once,
                     [|{ TweenValue = Color.One; TweenLength = 40L }
                       { TweenValue = Color.One; TweenLength = 40L }
                       { TweenValue = Color.One.WithA8 (byte 0); TweenLength = 0L }|])|],
                 Nil)
        let explosionSprite =
            AnimatedSprite
                (Resource (AssetTag.toPair Assets.Battle.ExplosionAnimationSheet),
                 v2i 32 32, 12, 4, 4L, Once,
                 [|Position (v3 0.0f -384.0f 0.0f)
                   Size (v3 96.0f 96.0f 0.0f)
                   Colors
                    (Set, EaseOut, Once,
                     [|{ TweenValue = Color.One; TweenLength = 48L }
                       { TweenValue = Color.One; TweenLength = 48L }
                       { TweenValue = Color.One.WithA8 (byte 0); TweenLength = 0L }|])|],
                 Nil)
        { EffectName = "Bolt"
          LifeTimeOpt = Some 80L
          Definitions = Map.empty
          Content =
            Contents
                (Shift 0.0f,
                 [|boltSprite
                   Delay (10L, explosionSprite)|]) }

    let inferno =
        let fireSpinSize = Size (v3 600.0f 600.0f 0.0f)
        let fireSpin aspects =
            AnimatedSprite
             (Resource (AssetTag.toPair Assets.Battle.FireSpinAnimationSheet),
              v2i 100 100, 61, 8, 1L, Loop, aspects, Nil)
        { EffectName = "Inferno"
          LifeTimeOpt = Some 80L
          Definitions = Map.empty
          Content =
              Contents
                  (Shift 0.0f,
                   [|fireSpin [|fireSpinSize; PositionAbsolute (v3 -144.0f 168.0f 0.0f)|]
                     fireSpin [|fireSpinSize; PositionAbsolute (v3 144.0f 168.0f 0.0f)|]
                     fireSpin [|fireSpinSize; PositionAbsolute (v3 -144.0f -111.0f 0.0f)|]
                     fireSpin [|fireSpinSize; PositionAbsolute (v3 144.0f -111.0f 0.0f)|]|]) }

    let silk =
        { EffectName = "Silk"
          LifeTimeOpt = Some 63L
          Definitions = Map.empty
          Content =
            AnimatedSprite
                (Resource (AssetTag.toPair Assets.Battle.SilkAnimationSheet),
                 v2i 48 48, 21, 21, 3L, Once,
                 [|Size (v3 144.0f 144.0f 0.0f)|],
                 Nil) }