// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module Gui =

        let Dissolve =
            { IncomingTime = 40L
              OutgoingTime = 60L
              DissolveImage = Assets.Default.Black }

        let Splash =
            { DissolveDescriptor = Constants.Dissolve.Default
              IdlingTime = 60L
              SlideImageOpt = Some Assets.Gui.NuSlide }

    [<RequireQualifiedAccess>]
    module Intro =

        let Dissolve =
            { IncomingTime = 95L
              OutgoingTime = 95L
              DissolveImage = Assets.Default.Black }

        let Splash =
            { DissolveDescriptor = Constants.Dissolve.Default
              IdlingTime = 160L
              SlideImageOpt = None }

    [<RequireQualifiedAccess>]
    module Gameplay =

        let TileSize = v3 48.0f 48.0f 0.0f
        let TileCelSize = v2 16.0f 16.0f
        let CharacterSize = v3 144.0f 144.0f 0.0f
        let CharacterCelSize = v2 48.0f 48.0f
        let BossSize = v3 324.0f 276.0f 0.0f
        let BossCelSize = v2 108.0f 92.0f
        let DialogSplit = '^'
        let ArmorStatBaseDisplayDivisor = 4
        let ItemLimit = 9
        let CueWalkSpeed = 1.0f
        let CueRunSpeed = 2.0f
        let CueMoseySpeed = 0.5f
        let CueCrawlSpeed = 0.25f

    [<RequireQualifiedAccess>]
    module Field =

        let PropsGroupName = "Props"
        let TransitionTime = 60L
        let RandMapSize = v2iDup 9
        let RoomSize = v2iDup 32
        let AvatarCollisionShapeIndex = 0
        let AvatarSensorShapeIndex = 1
#if DEV
        let AvatarWalkForce = 60000.0f
        let AvatarWalkForceMouse = 66000.0f
#else
        let AvatarWalkForce = 20000.0f
        let AvatarWalkForceMouse = 22000.0f
#endif
        let AvatarIdleSpeedMax = 5.0f
        let CharacterBottomOffset = v3 0.0f 24.0f 0.0f
        let SpiritMovementDuration = 60L
        let SpiritWalkSpeed = 2.75f
        let SpiritRunSpeed = 5.5f
        let SpiritOrbSize = v3 192.0f 192.0f 0.0f
        let SpiritOrbRatio = 0.075f
        let SpiritOrbBlipSize = v3 21.0f 21.0f 0.0f
#if DEV
        let SpiritActivityThreshold = 10000L
#else
        let SpiritActivityThreshold = 240L
#endif
        let SpiritActivityAggressionThreshold = 4
        let SpiritRadius = 90.0f / SpiritOrbRatio
        let SpiritCollisionRadius = 72.0f
        let WalkLengthScalar = 0.55f // NOTE: higher is longer distance to stronger spirits.
        let TreasureProbability = 0.667f
        let SpawnRoomPropId = -1 // NOTE: negative prop ids are reserved as hard-coded constants.
        let RecruitmentFees = [|200; 1000; 2000; 5000|]
        let ConnectorFadeYMin = 96.0f
        let ConnectorFadeYMax = 1296.0f
        let FeelerElevation = -100.0f
        let BackgroundElevation = -30.0f
        let FlooringElevation = -10.0f
        let ForegroundElevation = 0.0f
        let EffectElevation = 10.0f
        let SpiritOrbElevation = 20.0f
        let TintElevation = 25.0f
        let GuiElevation = 30.0f
        let GuiEffectElevation = 40.0f
        let FieldSongFadeInTime = 60L

    [<RequireQualifiedAccess>]
    module Battle =

        let AllyMax = 3.0f
        let ActionTime = 1000.0f
        let ActionTimeSlowestScalar = 2.0f / 3.0f
        let ActionTimeSlowerScalar = 0.75f
        let ActionTimeSlowScalar = 0.8f
        let ActionTimeHasteScalar = 1.5f
        let ActionTimeAutoBattleReady = 50.0f
        let ActionTimeCancelReduction = 333.0f
        let ActionTimeCancelMinimum = ActionTimeAutoBattleReady + 25.0f // must be more than ActionTimeAutoBattleReady
        let StatusBurndownTime = 3500.0f
        let AffinityResistanceScalar = 0.75f
        let AffinityVulnerabilityScalar = 1.25f
        let AllyActionTimeDelta = 4.0f
        let AllyActionTimeSpacing = 1000.0f / 3.0f
        let EnemyActionTimeDelta = 3.0f
        let SwiftSpeedScalar = 0.5f
        let PacedSpeedScalar = 0.25f
        let DefendingScalar = 0.5f
        let TechProbabilityReductionScalar = 0.667f // for when Jinn isn't in battle to cancel
        let PoisonDrainRateSlow = 0.05f
        let PoisonDrainRateMedium = 0.1f
        let PoisonDrainRateFast = 0.15f
        let ItemDropRate = 0.02f
        let EnemySplitScalar = 0.5f
        let ConjureChargeRate = 6
        let ChargeMax = 12
        let CancelPosition = v3 -444.0f -237.0f 0.0f
        let StrikingDistance = 48.0f
        let AfflictionSize = v3 96.0f 96.0f 0.0f
        let AfflictionCelSize = v2 32.0f 32.0f
        let ChargeOrbSize = v3 96.0f 96.0f 0.0f
        let ChargeOrbCelSize = v2 32.0f 32.0f
        let CharacterCenterOffset = v3 0.0f -30.0f 0.0f
        let CharacterCenterOffset2 = v3 0.0f -96.0f 0.0f
        let CharacterCenterOffset3 = v3 0.0f -36.0f 0.0f
        let CharacterCenterOffset4 = v3 0.0f 36.0f 0.0f
        let CharacterBottomOffset = v3 0.0f -7.0f 0.0f
        let CharacterBottomOffset2 = v3 0.0f -11.0f 0.0f
        let CharacterBottomOffset3 = v3 0.0f -48.0f 0.0f
        let CharacterBottomOffset4 = v3 0.0f 30.0f 0.0f
        let CharacterOffset = v3 -96.0f 0.0f 0.0f
        let CharacterSpritePulseDuration = 60L
        let CharacterFillColorPulseDuration = 30L
        let CharacterMaterializeDuration = 60L
        let CharacterDematerializeDuration = 60L
        let RingMenuRadius = 84.0f
        let BackgroundElevation = -30.0f
        let EffectElevationUnder = -2.5f
        let GuiBackgroundElevation = -5.0f
        let ForegroundElevation = 0.0f
        let GuiForegroundElevation = 5.0f
        let EffectElevationOver = 10.0f
        let GuiOutputElevation = 20.0f
        let GuiInputElevation = 30.0f
        let GuiEffectElevation = 40.0f