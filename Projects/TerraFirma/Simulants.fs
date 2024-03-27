namespace TerraFirma
open System
open Prime
open Nu

// this module provides global handles to the game's key simulants.
// having a Simulants module for your game is optional, but can be nice to avoid duplicating string literals across
// the code base.
[<RequireQualifiedAccess>]
module Simulants =

    // splash screen
    let Splash = Game / "Splash"

    // title screen
    let Title = Game / "Title"
    let TitleGui = Title / "Gui"
    let TitlePlay = TitleGui / "Play"
    let TitleCredits = TitleGui / "Credits"
    let TitleExit = TitleGui / "Exit"

    // credits screen
    let Credits = Game / "Credits"
    let CreditsGui = Credits / "Gui"
    let CreditsBack = CreditsGui / "Back"

    // gameplay screen
    let Gameplay = Game / "Gameplay"
    let GameplayGui = Gameplay / "Gui"
    let GameplayTime = GameplayGui / "Time"
    let GameplayQuit = GameplayGui / "Quit"
    let GameplayScene = Gameplay / "Scene"
    let GameplayPlayer = GameplayScene / string Constants.Gameplay.PlayerGuids.[0]
    let GameplayEnemy (enemyId : Guid) = GameplayScene / string enemyId