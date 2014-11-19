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
                [typeof<BattleGroupDispatcher>.Name, BattleGroupDispatcher () :> GroupDispatcher
                 typeof<FieldGroupDispatcher>.Name, FieldGroupDispatcher () :> GroupDispatcher]

        override this.MakeGameDispatchers () =
            Map.ofList
                [typeof<OmniBladeDispatcher>.Name, OmniBladeDispatcher () :> GameDispatcher]

    let addTitleScreen world =
        let world = snd <| World.addDissolveScreenFromGroupFile false DissolveData typeof<ScreenDispatcher>.Name TitleAddress TitleGroupFilePath world
        let world = World.subscribe4 GameAddress ClickTitleNewGameEvent (World.handleAsScreenTransition FieldAddress) world
        let world = World.subscribe4 GameAddress ClickTitleLoadGameEvent (World.handleAsScreenTransition LoadGameAddress) world
        let world = World.subscribe4 GameAddress ClickTitleCreditsEvent (World.handleAsScreenTransition CreditsAddress) world
        World.subscribe4 GameAddress ClickTitleExitEvent World.handleAsExit world

    let addLoadGameScreen world =
        let world = snd <| World.addDissolveScreenFromGroupFile false DissolveData typeof<ScreenDispatcher>.Name LoadGameAddress LoadGameGroupFilePath world
        World.subscribe4 GameAddress ClickLoadGameBackEvent (World.handleAsScreenTransition TitleAddress) world

    let addCreditsScreen world =
        let world = snd <| World.addDissolveScreenFromGroupFile false DissolveData typeof<ScreenDispatcher>.Name CreditsAddress CreditsGroupFilePath world
        World.subscribe4 GameAddress ClickCreditsBackEvent (World.handleAsScreenTransition TitleAddress) world

    let addFieldScreen world =
        let world = snd <| World.addDissolveScreenFromGroupFile false DissolveData typeof<ScreenDispatcher>.Name FieldAddress FieldGroupFilePath world
        World.subscribe4 GameAddress ClickFieldBackEvent (World.handleAsScreenTransition TitleAddress) world

    let tryMakeOmniBladeWorld sdlDeps userState =
        let omniComponentFactory = OmniComponentFactory ()
        let optWorld = World.tryMake sdlDeps omniComponentFactory GuiAndPhysics false userState
        match optWorld with
        | Right world ->
            let world = World.hintRenderingPackageUse GuiPackageName world
            let world = World.playSong GameSong 1.0f DefaultTimeToFadeOutSongMs world
            let (splashScreen, world) = World.addSplashScreen false SplashData typeof<ScreenDispatcher>.Name SplashAddress TitleAddress world
            let world = addTitleScreen world
            let world = addLoadGameScreen world
            let world = addCreditsScreen world
            let world = addFieldScreen world
            let world = snd <| World.selectScreen SplashAddress splashScreen world
            Right world
        | Left _ as left -> left