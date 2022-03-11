namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Effects

[<RequireQualifiedAccess>]
module Effects =

    let makeSwordStrikeEffect direction =

        let resource =
            match direction with
            | Upward -> Assets.Gameplay.OakSwordStrikeUp
            | Rightward -> Assets.Gameplay.OakSwordStrikeRight
            | Downward -> Assets.Gameplay.OakSwordStrikeDown
            | Leftward -> Assets.Gameplay.OakSwordStrikeLeft

        let animationSprite =
            AnimatedSprite
                (Resource (AssetTag.toPair resource),
                 v2iDup 144, 2, 2, Constants.Gameplay.CharacterAnimationActingDelay, Once, [||], Nil)

        { EffectName = "SwordStrike"
          LifeTimeOpt = Some (Constants.Gameplay.CharacterAnimationActingDelay * 2L)
          Definitions = Map.empty
          Content = animationSprite }

    let makeMagicMissileImpactEffect () =

        let animationSprite =
            AnimatedSprite
                (Resource (AssetTag.toPair Assets.Gameplay.MagicMissileImpact),
                 v2iDup 48, 2, 2, Constants.Gameplay.CharacterAnimationActingDelay, Once, [||], Nil)

        { EffectName = "MagicMissileImpact"
          LifeTimeOpt = Some (Constants.Gameplay.CharacterAnimationActingDelay * 2L)
          Definitions = Map.empty
          Content = animationSprite }