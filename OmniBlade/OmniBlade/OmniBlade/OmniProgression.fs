namespace OmniBlade
open System
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open OmniBlade
open OmniBlade.OmniConstants
module OmniProgression =

    type OmniPlugin () =
        inherit NuPlugin ()

        override this.MakeGroupDispatchers () =
            [BattleGroupDispatcher () :> GroupDispatcher
             FieldGroupDispatcher () :> GroupDispatcher]

        override this.MakeOptGameDispatcher () =
            Some (OmniBladeDispatcher () :> GameDispatcher)

    let addTitleScreen world =
        let world = snd <| World.addDissolveScreenFromGroupFile false DissolveData typeof<ScreenDispatcher>.Name TitleGroupFilePath TitleAddress world
        let world = World.subscribe4 ClickTitleNewGameEvent GameAddress (World.handleAsScreenTransition FieldAddress) world
        let world = World.subscribe4 ClickTitleLoadGameEvent GameAddress (World.handleAsScreenTransition LoadGameAddress) world
        let world = World.subscribe4 ClickTitleCreditsEvent GameAddress (World.handleAsScreenTransition CreditsAddress) world
        World.subscribe4 ClickTitleExitEvent GameAddress World.handleAsExit world

    let addLoadGameScreen world =
        let world = snd <| World.addDissolveScreenFromGroupFile false DissolveData typeof<ScreenDispatcher>.Name LoadGameGroupFilePath LoadGameAddress world
        World.subscribe4 ClickLoadGameBackEvent GameAddress (World.handleAsScreenTransition TitleAddress) world

    let addCreditsScreen world =
        let world = snd <| World.addDissolveScreenFromGroupFile false DissolveData typeof<ScreenDispatcher>.Name CreditsGroupFilePath CreditsAddress world
        World.subscribe4 ClickCreditsBackEvent GameAddress (World.handleAsScreenTransition TitleAddress) world

    let addFieldScreen world =
        let world = snd <| World.addDissolveScreenFromGroupFile false DissolveData typeof<ScreenDispatcher>.Name FieldGroupFilePath FieldAddress world
        World.subscribe4 ClickFieldBackEvent GameAddress (World.handleAsScreenTransition TitleAddress) world

    let tryMakeOmniBladeWorld userState sdlDeps =
        let omniPlugin = OmniPlugin ()
        let eitherWorld = World.tryMake false true GuiAndPhysics userState omniPlugin sdlDeps
        match eitherWorld with
        | Right world ->
            let world = World.hintRenderPackageUse GuiPackageName world
            let world = World.playSong DefaultTimeToFadeOutSongMs 1.0f GameSong world
            let (splashScreen, world) = World.addSplashScreen false SplashData typeof<ScreenDispatcher>.Name SplashAddress TitleAddress world
            let world = addTitleScreen world
            let world = addLoadGameScreen world
            let world = addCreditsScreen world
            let world = addFieldScreen world
            let world = snd <| World.selectScreen splashScreen SplashAddress world
            Right world
        | Left _ as left -> left