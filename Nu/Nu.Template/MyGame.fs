namespace MyGame
open Prime
open Nu

// this is our Elm-style command type. To learn about the Elm-style, read this article here -
// https://vsyncronicity.com/2020/03/01/a-game-engine-in-the-elm-style/
type MyGameCommand =
    | ShowTitle
    | ShowCredits
    | ShowGameplay
    | ExitGame

// this is the game dispatcher that is customized for our game. In here, we create screens and wire
// them up with events and transitions.
type MyGameDispatcher () =
    inherit GameDispatcher<unit, unit, MyGameCommand> ()

    // here we channel from events to signals
    override this.Channel (_, _) =
        [Simulants.TitleCredits.ClickEvent => cmd ShowCredits
         Simulants.TitlePlay.ClickEvent => cmd ShowGameplay
         Simulants.TitleExit.ClickEvent => cmd ExitGame
         Simulants.CreditsBack.ClickEvent => cmd ShowTitle
         Simulants.Back.ClickEvent => cmd ShowTitle]

    // here we handle the above commands
    override this.Command (_, command, _, world) =
        let world =
            match command with
            | ShowTitle -> World.transitionScreen Simulants.Title world
            | ShowCredits -> World.transitionScreen Simulants.Credits world
            | ShowGameplay -> World.transitionScreen Simulants.Gameplay world
            | ExitGame -> World.exit world
        just world

    // here we describe the content of the game including all of its screens.
    override this.Content (_, _) =
        [Content.screen Simulants.Splash.Name (Splash (Constants.Dissolve.Default, Constants.Splash.Default, Simulants.Title)) [] []
         Content.screenFromLayerFile Simulants.Title.Name (Dissolve (Constants.Dissolve.Default, None)) "Assets/Gui/Title.nulyr"
         Content.screenFromLayerFile Simulants.Credits.Name (Dissolve (Constants.Dissolve.Default, None)) "Assets/Gui/Credits.nulyr"
         Content.screen<MyGameplayDispatcher> Simulants.Gameplay.Name (Dissolve (Constants.Dissolve.Default, None)) [] []]

    // here we hint to the renderer and audio system that the 'Gui' package should be loaded ahead of time
    override this.Register (game, world) =
        let world = World.hintRenderPackageUse "Gui" world
        let world = World.hintAudioPackageUse "Gui" world
        base.Register (game, world)