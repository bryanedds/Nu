namespace BlazeVector
open System
open Prime
open Nu
module Simulants =

    // this constant is the result of converting a name to a Screen proxy. A proxy is like an
    // 'address' with which to affect a simulant.
    let Splash = !> "Splash"

    // same as above, but for the title screen
    let Title = !> "Title"

    // this is the layer that is loaded into the title screen that contains all of its gui
    // entities. You'll notice that the layer is built from a combination of the title screen's
    // proxy as well as its own personal name as found in its originating document,
    // 'Assets/Gui/Title.nulyr'.
    let TitleGui = Title => "Gui"

    // this is like the above, but for the play button found in the above layer
    let TitlePlay = TitleGui => "Play"
    
    // and so on for the title screens credits and exit buttons.
    let TitleCredits = TitleGui => "Credits"
    let TitleExit = TitleGui => "Exit"

    // like those proceeding them, these are proxies for various simulants of the gameplay screen
    let Gameplay = !> "Gameplay"
    let GameplayGui = Gameplay => "Gui"
    let GameplayBack = GameplayGui => "Back"
    let GameplayScene = Gameplay => "Scene"
    let Player = GameplayScene => "Player"

    // proxies for the credits screen
    let Credits = !> "Credits"
    let CreditsGui = Credits => "Gui"
    let CreditsBack = CreditsGui => "Back"