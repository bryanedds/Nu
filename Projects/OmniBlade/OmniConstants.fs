namespace OmniBlade
open System
open Prime
open Nu
open OmniBlade

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module Audio =

        let MasterSongVolume = 0.5f

    [<RequireQualifiedAccess>]
    module Battle =

        let AllyMax = 3
        let ActionTime = 999
        let ActionTimeInc = 3
        let DefendingCounterBuff = 0.5f

    [<RequireQualifiedAccess>]
    module OmniBlade =

        let DissolveData =
            { IncomingTime = 20L
              OutgoingTime = 30L
              DissolveImage = asset<Image> Assets.GuiPackage "Dissolve" }
    
        let SplashData =
            { DissolveData = DissolveData
              IdlingTime = 60L
              SplashImage = asset<Image> Assets.GuiPackage "Nu" }