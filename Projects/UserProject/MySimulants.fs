namespace MyGame
open Nu

[<RequireQualifiedAccess>]
module Simulants =

    // same as above, but for the splash screen
    let Splash = Screen "Splash"

    // same as above, but for the title screen and its children
    let Title = Screen "Title"
    let TitleGui = Title / "Gui"
    let TitlePlay = TitleGui / "Play"
    let TitleCredits = TitleGui / "Credits"
    let TitleExit = TitleGui / "Exit"

    // credits screen handles
    let Credits = Screen "Credits"
    let CreditsGui = Credits / "Gui"
    let CreditsBack = CreditsGui / "Back"

    // gameplay screen handles
    let Gameplay = Screen "Gameplay"
    let Hud = Gameplay / "Hud"
    let Back = Hud / "Back"
    let Scene = Gameplay / "Scene"
    let Player = Scene / "Player"
    let Level = Gameplay / "Level"