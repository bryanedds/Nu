namespace BlazeVector
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module BlazeVector =

        // this constant describes the 'dissolving' transition behavior of game's screens
        let DissolveData =
            { IncomingTime = 20L
              OutgoingTime = 30L
              DissolveImage = { PackageName = Assets.DefaultPackageName; AssetName = "Image8" }}
    
        // this constant describes the 'splashing' behavior of game's splash screen
        let SplashData =
            { DissolveData = DissolveData
              IdlingTime = 60L
              SplashImage = { PackageName = Assets.DefaultPackageName; AssetName = "Image5" }}

        // and finally, this constant simply specifies how many sections are added to a game
        let SectionCount = 32