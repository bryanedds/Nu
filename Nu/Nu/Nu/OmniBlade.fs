namespace Nu
open System
open SDL2
open OpenTK
open TiledSharp
open Nu.Core
open Nu.Constants
open Nu.Sdl
open Nu.Audio
open Nu.Rendering
open Nu.Physics
open Nu.Metadata
open Nu.Entities
open Nu.Groups
open Nu.Screens
open Nu.Games
open Nu.Sim
module OmniBlade =

    let SplashScreenAddress = [Lun.make "splash"]
    let TitleScreenAddress = [Lun.make "title"]
    let LoadScreenAddress = [Lun.make "load"]
    let OmniScreenAddress = [Lun.make "omni"]
    let CreditsScreenAddress = [Lun.make "credits"]

    let tryCreateOmniBladeWorld sdlDeps extData =
        let optWorld = tryCreateEmptyWorld sdlDeps extData
        match optWorld with
        | Left _ as left -> left
        | Right world ->
            let splashScreenSprite = { SpriteAssetName = Lun.make "Image5"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
            let world' = addSplashScreen (changeSelectedScreen SplashScreenAddress(*TitleScreenAddress*)) SplashScreenAddress 90 45 90 splashScreenSprite world
            let world'' = set (Some SplashScreenAddress) world' worldOptSelectedScreenModelAddressLens
            Right world''