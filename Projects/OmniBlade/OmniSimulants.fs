namespace OmniBlade
open Prime
open Nu

type CharacterIndex =
    | AllyIndex of int
    | EnemyIndex of int

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
    let Scene = Battle / "Scene"
    let Character characterIndex = match characterIndex with AllyIndex index -> Scene / ("Ally" + "+" + scstring index) | EnemyIndex index -> Scene / ("Enemy" + "+" + scstring index)
    let Ally index = Character (AllyIndex index)
    let Enemy index = Character (EnemyIndex index)
    let Input (index : int) = Battle / ("Input" + "+" + scstring index)
    let RegularMenu index = Input index / "RegularMenu"
    let SpecialMenu index = Input index / "SpecialMenu"
    let ItemMenu index = Input index / "ItemMenu"
    let Reticles index = Input index / "Reticles"
    let AllInputEntities index = [RegularMenu index; SpecialMenu index; ItemMenu index; Reticles index]