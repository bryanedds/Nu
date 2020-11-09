// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open Prime
open Nu
open OmniBlade

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module Gui =

        let Dissolve =
            { IncomingTime = 40L
              OutgoingTime = 60L
              DissolveImage = Assets.DefaultImage8 }

        let Splash =
            { DissolveDescriptor = Constants.Dissolve.Default
              IdlingTime = 60L
              SplashImageOpt = Some Assets.DefaultImage5 }

    [<RequireQualifiedAccess>]
    module Intro =

        let Dissolve =
            { IncomingTime = 40L
              OutgoingTime = 60L
              DissolveImage = Assets.DefaultImage8 }

        let Splash =
            { DissolveDescriptor = Constants.Dissolve.Default
              IdlingTime = 180L
              SplashImageOpt = None }

    [<RequireQualifiedAccess>]
    module Gameplay =

        let TileSize = v2 64.0f 64.0f
        let CharacterSize = v2 160.0f 160.0f
        let DialogSplit = '^'

    [<RequireQualifiedAccess>]
    module Field =

#if DEV
        let WalkForce = 26000.0f * 3.0f
#else
        let WalkForce = 26000.0f
#endif
        let LinearDamping = 19.0f
        let BackgroundDepth = -10.0f
        let ForgroundDepth = 0.0f
        let EffectDepth = 10.0f
        let GuiDepth = 20.0f
        let GuiEffectDepth = 30.0f
        let PropsLayerName = "Props"
        let TransitionTime = 60L
        let MapRandSize = v2iDup 7
        let AvatarBottomInset = v2 0.0f 32.0f
        let TreasureProbability = 0.75f

    [<RequireQualifiedAccess>]
    module Battle =

        let AllyMax = 3
        let ActionTime = 1000
        let AutoBattleReadyTime = 50
        let AutoBattleTechFrequency = 3
        let AllyActionTimeInitial = 700
        let AllyActionTimeDelta = 4
        let EnemyActionTimeInitial = 0
        let EnemyActionTimeDelta = 3
        let DefendingCounterBuff = 0.5f
        let CancelPosition = v2 -448.0f -240.0f
        let CharacterCenterOffset = v2 0.0f -16.0f
        let CharacterCenterOffset2 = v2 0.0f -32.0f
        let CharacterCenterOffset3 = v2 0.0f 32.0f
        let CharacterBottomOffset = v2 0.0f -8.0f
        let CharacterBottomOffset2 = v2 0.0f -32.0f
        let CharacterBottomOffset3 = v2 0.0f -64.0f
        let CharacterPulseLength = 60L
        let LineWidth = 16.0f
        let BackgroundDepth = -10.0f
        let ForgroundDepth = 0.0f
        let EffectDepth = 10.0f
        let GuiDepth = 20.0f
        let GuiEffectDepth = 30.0f