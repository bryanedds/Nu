namespace OmniBlade
open System
open Prime
open Nu
open OmniBlade
open OmniBlade.OmniConstants
module OmniFlow =

    let addTitleScreen world =
        let world_ = World.addDissolveScreenFromFile typeof<ScreenDispatcher>.Name TitleGroupFileName (List.last TitleGroupAddress) IncomingTime OutgoingTime TitleAddress true world
        let world_ = World.subscribe ClickTitleNewGameEvent [] (ScreenTransitionSub FieldAddress) world_
        let world_ = World.subscribe ClickTitleLoadGameEvent [] (ScreenTransitionSub LoadGameAddress) world_
        let world_ = World.subscribe ClickTitleCreditsEvent [] (ScreenTransitionSub CreditsAddress) world_
        World.subscribe ClickTitleExitEvent [] ExitSub world_

    let addLoadGameScreen world =
        let world' = World.addDissolveScreenFromFile typeof<ScreenDispatcher>.Name LoadGameGroupFileName (List.last LoadGameGroupAddress) IncomingTime OutgoingTime LoadGameAddress true world
        World.subscribe ClickLoadGameBackEvent [] (ScreenTransitionSub TitleAddress) world'

    let addCreditsScreen world =
        let world' = World.addDissolveScreenFromFile typeof<ScreenDispatcher>.Name CreditsGroupFileName (List.last CreditsGroupAddress) IncomingTime OutgoingTime CreditsAddress true world
        World.subscribe ClickCreditsBackEvent [] (ScreenTransitionSub TitleAddress) world'

    let addFieldScreen world =
        let world' = World.addDissolveScreenFromFile typeof<ScreenDispatcher>.Name FieldGroupFileName (List.last FieldGroupAddress) IncomingTime OutgoingTime FieldAddress true world
        World.subscribe ClickFieldBackEvent [] (ScreenTransitionSub TitleAddress) world'

    let tryMakeOmniBladeWorld sdlDeps extData =
        let gameDispatcher = OmniGameDispatcher () :> obj
        let optWorld = World.tryMakeEmpty sdlDeps gameDispatcher extData
        match optWorld with
        | Left _ as left -> left
        | Right world ->
            let hintRenderPackageUse = HintRenderingPackageUse { FileName = NuConstants.AssetGraphFileName; PackageName = OmniGuiPackageName } 
            let world_ = { world with RenderMessages = hintRenderPackageUse :: world.RenderMessages }
            let gameSong = { SongAssetName = "Song"; PackageName = NuConstants.DefaultPackageName; PackageFileName = NuConstants.AssetGraphFileName }
            let playSongMessage = PlaySong { Song = gameSong; FadeOutCurrentSong = true }
            let world_ = { world_ with AudioMessages = playSongMessage :: world_.AudioMessages }
            let splashScreenSprite = { SpriteAssetName = "Image5"; PackageName = NuConstants.DefaultPackageName; PackageFileName = NuConstants.AssetGraphFileName }
            let world_ = World.addSplashScreenFromData (ScreenTransitionSub TitleAddress) SplashAddress typeof<ScreenDispatcher>.Name IncomingTimeSplash IdlingTime OutgoingTimeSplash splashScreenSprite world_
            let world_ = addTitleScreen world_
            let world_ = addLoadGameScreen world_
            let world_ = addCreditsScreen world_
            let world_ = addFieldScreen world_
            let world_ = World.transitionScreen SplashAddress world_
            Right world_