namespace BlazeVector
open Nu
module Simulants =

    // here we derive a screen handle from its name so that we can interface with it in code
    let Splash = !> "Splash"

    // same as above, but for the title screen
    let Title = !> "Title"

    // this is the layer that is loaded into the title screen that contains all of its gui
    // entities. You'll notice that the layer is built from a combination of the title screen as
    // well as its own individual name as found in its document, 'Assets/Gui/Title.nulyr'.
    let TitleGui = Title => "Gui"

    // this is like the above, but for the play button found in the above layer
    let TitlePlay = TitleGui => "Play"
    
    // and so on for the title screens credits and exit buttons
    let TitleCredits = TitleGui => "Credits"
    let TitleExit = TitleGui => "Exit"

    // like those proceeding them, these are the various simulants of the gameplay screen
    let Gameplay = !> "Gameplay"
    let GameplayGui = Gameplay => "Gui"
    let GameplayBack = GameplayGui => "Back"
    let GameplayScene = Gameplay => "Scene"
    let Player = GameplayScene => "Player"

    // credits screen simulants
    let Credits = !> "Credits"
    let CreditsGui = Credits => "Gui"
    let CreditsBack = CreditsGui => "Back"