namespace OmniBlade
open Prime
open Nu

[<RequireQualifiedAccess>]
module Simulants =

    // global
    let Game = Default.Game
    let Splash = Screen "Splash"

    // title
    let Title = Screen "Title"
    let TitleGui = Title / "Gui"
    let TitlePlay = TitleGui / "Play"
    let TitleCredits = TitleGui / "Credits"
    let TitleExit = TitleGui / "Exit"

    // credits
    let Credits = Screen "Credits"
    let CreditsGui = Credits / "Gui"
    let CreditsBack = CreditsGui / "Back"

    // battle
    let Battle = Default.Screen
    let BattleHud = Battle / "Hud"
    let BattleBack = BattleHud / "Back"
    let BattleScene = Battle / "Scene"
    let BattleHop = BattleScene / "Hop"