namespace OmniBlade
open System
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open OmniBlade
open OmniBlade.OmniConstants
module OmniProgression =

    type OmniComponentFactory () =
        inherit UserComponentFactory ()

        override this.MakeGroupDispatchers () =
            Map.ofList
                [(typeof<BattleGroupDispatcher>.Name, BattleGroupDispatcher () :> GroupDispatcher)
                 (typeof<FieldGroupDispatcher>.Name, FieldGroupDispatcher () :> GroupDispatcher)]

        override this.MakeOptGameDispatcher () =
            Some (typeof<OmniBladeDispatcher>.Name, OmniBladeDispatcher () :> GameDispatcher)

    let addTitleScreen world =
        let world = snd <| World.addDissolveScreenFromGroupFile false DissolveData typeof<ScreenDispatcher>.Name TitleAddress TitleGroupFilePath world
        let world = World.subscribe4 (World.handleAsScreenTransition FieldAddress) ClickTitleNewGameEvent GameAddress world
        let world = World.subscribe4 (World.handleAsScreenTransition LoadGameAddress) ClickTitleLoadGameEvent GameAddress world
        let world = World.subscribe4 (World.handleAsScreenTransition CreditsAddress) ClickTitleCreditsEvent GameAddress world
        World.subscribe4 World.handleAsExit ClickTitleExitEvent GameAddress world

    let addLoadGameScreen world =
        let world = snd <| World.addDissolveScreenFromGroupFile false DissolveData typeof<ScreenDispatcher>.Name LoadGameAddress LoadGameGroupFilePath world
        World.subscribe4 (World.handleAsScreenTransition TitleAddress) ClickLoadGameBackEvent GameAddress world

    let addCreditsScreen world =
        let world = snd <| World.addDissolveScreenFromGroupFile false DissolveData typeof<ScreenDispatcher>.Name CreditsAddress CreditsGroupFilePath world
        World.subscribe4 (World.handleAsScreenTransition TitleAddress) ClickCreditsBackEvent GameAddress world

    let addFieldScreen world =
        let world = snd <| World.addDissolveScreenFromGroupFile false DissolveData typeof<ScreenDispatcher>.Name FieldAddress FieldGroupFilePath world
        World.subscribe4 (World.handleAsScreenTransition TitleAddress) ClickFieldBackEvent GameAddress world

    let tryMakeOmniBladeWorld userState sdlDeps =
        let omniComponentFactory = OmniComponentFactory ()
        let eitherWorld = World.tryMake false true GuiAndPhysics userState omniComponentFactory sdlDeps
        match eitherWorld with
        | Right world ->
            let world = World.hintRenderPackageUse GuiPackageName world
            let world = World.playSong GameSong 1.0f DefaultTimeToFadeOutSongMs world
            let (splashScreen, world) = World.addSplashScreen false SplashData typeof<ScreenDispatcher>.Name SplashAddress TitleAddress world
            let world = addTitleScreen world
            let world = addLoadGameScreen world
            let world = addCreditsScreen world
            let world = addFieldScreen world
            let world = snd <| World.selectScreen SplashAddress splashScreen world
            Right world
        | Left _ as left -> left