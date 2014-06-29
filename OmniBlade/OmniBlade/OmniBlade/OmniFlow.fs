namespace OmniBlade
open System
open Prime
open Nu
open Nu.NuConstants
open OmniBlade
open OmniBlade.OmniConstants
module OmniFlow =

    let addTitleScreen world =
        let world = World.addDissolveScreenFromFile typeof<ScreenDispatcher>.Name TitleGroupFileName (List.last TitleGroupAddress) IncomingTime OutgoingTime TitleAddress world
        let world = World.subscribe ClickTitleNewGameEvent [] (ScreenTransitionSub FieldAddress) world
        let world = World.subscribe ClickTitleLoadGameEvent [] (ScreenTransitionSub LoadGameAddress) world
        let world = World.subscribe ClickTitleCreditsEvent [] (ScreenTransitionSub CreditsAddress) world
        World.subscribe ClickTitleExitEvent [] ExitSub world

    let addLoadGameScreen world =
        let world = World.addDissolveScreenFromFile typeof<ScreenDispatcher>.Name LoadGameGroupFileName (List.last LoadGameGroupAddress) IncomingTime OutgoingTime LoadGameAddress world
        World.subscribe ClickLoadGameBackEvent [] (ScreenTransitionSub TitleAddress) world

    let addCreditsScreen world =
        let world = World.addDissolveScreenFromFile typeof<ScreenDispatcher>.Name CreditsGroupFileName (List.last CreditsGroupAddress) IncomingTime OutgoingTime CreditsAddress world
        World.subscribe ClickCreditsBackEvent [] (ScreenTransitionSub TitleAddress) world

    let addFieldScreen world =
        let world = World.addDissolveScreenFromFile typeof<ScreenDispatcher>.Name FieldGroupFileName (List.last FieldGroupAddress) IncomingTime OutgoingTime FieldAddress world
        World.subscribe ClickFieldBackEvent [] (ScreenTransitionSub TitleAddress) world

    let tryMakeOmniBladeWorld sdlDeps extData =
        let gameDispatcher = OmniBladeDispatcher () :> obj
        let optWorld = World.tryMakeEmpty sdlDeps gameDispatcher true extData
        match optWorld with
        | Left _ as left -> left
        | Right world ->
            let hintRenderPackageUse = HintRenderingPackageUseMessage { FileName = AssetGraphFileName; PackageName = GuiPackageName } 
            let world = { world with RenderMessages = hintRenderPackageUse :: world.RenderMessages }
            let gameSong = { SongAssetName = "Song"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName }
            let playSongMessage = PlaySongMessage { Song = gameSong; TimeToFadeOutSongMs = DefaultTimeToFadeOutSongMs }
            let world = { world with AudioMessages = playSongMessage :: world.AudioMessages }
            let splashScreenSprite = { SpriteAssetName = "Image5"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName }
            let world = World.addSplashScreenFromData TitleAddress SplashAddress typeof<ScreenDispatcher>.Name IncomingTimeSplash IdlingTime OutgoingTimeSplash splashScreenSprite world
            let world = addTitleScreen world
            let world = addLoadGameScreen world
            let world = addCreditsScreen world
            let world = addFieldScreen world
            let world = World.selectScreen SplashAddress world
            Right world