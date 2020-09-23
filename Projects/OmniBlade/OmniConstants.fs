namespace OmniBlade
open Prime
open Nu
open OmniBlade

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module Gameplay =

        let TileSize = v2 64.0f 64.0f
        let CharacterSize = v2 160.0f 160.0f

    [<RequireQualifiedAccess>]
    module Field =

        let WalkForce = 26000.0f
        let LinearDamping = 19.0f
        let BackgroundDepth = -10.0f
        let ForgroundDepth = 0.0f
        let EffectDepth = 10.0f
        let GuiDepth = 20.0f
        let GuiEffectDepth = 30.0f
        let PropsLayerName = "Props"
        let TransitionTime = 40L

    [<RequireQualifiedAccess>]
    module Battle =

        let AllyMax = 3
        let ActionTime = 999
        let AutoBattleReadyTime = 48
        let AutoBattleTechFrequency = 3
        let ActionTimeInc = 3
        let DefendingCounterBuff = 0.5f
        let CancelPosition = v2 -448.0f -240.0f
        let CharacterCenterOffset = v2 0.0f -16.0f
        let CharacterCenterOffset2 = v2 0.0f -32.0f
        let CharacterCenterOffset3 = v2 0.0f 32.0f
        let CharacterBottomOffset = v2 0.0f -8.0f
        let CharacterBottomOffset2 = v2 0.0f -32.0f
        let CharacterPulseLength = 60L
        let LineWidth = 16.0f
        let BackgroundDepth = -10.0f
        let ForgroundDepth = 0.0f
        let EffectDepth = 10.0f
        let GuiDepth = 20.0f
        let GuiEffectDepth = 30.0f

    [<RequireQualifiedAccess>]
    module Dissolve =
    
        let Default =
            { IncomingTime = 20L
              OutgoingTime = 30L
              DissolveImage = asset<Image> Assets.GuiPackageName "Dissolve" }
                  
    [<RequireQualifiedAccess>]
    module Splash =
    
        let Default =
            { DissolveDescriptor = Dissolve.Default
              IdlingTime = 60L
              SplashImage = asset<Image> Assets.GuiPackageName "Nu" }