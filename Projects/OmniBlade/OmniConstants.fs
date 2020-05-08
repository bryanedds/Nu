namespace OmniBlade
open System
open Prime
open Nu
open OmniBlade

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module Battle =

        let AllyMax = 3
        let ActionTime = 999
        let AutoBattleReadyTime = 48
        let AutoBattleSpecialFrequency = 3
        let ActionTimeInc = 3
        let DefendingCounterBuff = 0.5f
        let CancelPosition = v2 -448.0f -240.0f
        let CharacterCenterOffset = v2 0.0f -16.0f
        let CharacterCenterOffset2 = v2 0.0f -32.0f
        let CharacterCenterOffset3 = v2 0.0f 32.0f
        let CharacterBottomOffset = v2 0.0f -8.0f
        let CharacterBottomOffset2 = v2 0.0f -32.0f
        let CharacterPulseLength = 60L
        let BackgroundDepth = -10.0f
        let ForgroundDepth = 0.0f
        let EffectDepth = 10.0f
        let GuiDepth = 20.0f
        let GuiEffectDepth = 30.0f

    [<RequireQualifiedAccess>]
    module OmniBlade =

        let DissolveData =
            { IncomingTime = 20L
              OutgoingTime = 30L
              DissolveImage = asset<Image> Assets.GuiPackageName "Dissolve" }

        let SplashData =
            { DissolveData = DissolveData
              IdlingTime = 60L
              SplashImage = asset<Image> Assets.GuiPackageName "Nu" }