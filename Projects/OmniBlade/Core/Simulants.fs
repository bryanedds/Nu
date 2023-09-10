// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open Prime
open Nu
open OmniBlade

[<RequireQualifiedAccess>]
module Simulants =

    // game
    let Game = Game.Handle

    // splash screen
    let Splash = Screen "Splash"

    // intro screens
    let Intro = Screen "Intro"
    let Intro2 = Screen "Intro2"
    let Intro3 = Screen "Intro3"
    let Intro4 = Screen "Intro4"
    let Intro5 = Screen "Intro5"

    // title screen
    let Title = Screen "Title"
    let TitleGui = Title / "Gui"
    let TitleGuiPlay = TitleGui / "Play"
    let TitleGuiCredits = TitleGui / "Credits"
    let TitleGuiExit = TitleGui / "Exit"

    // pick screen
    let Pick = Screen "Pick"
    let PickGui = Pick / "Gui"
    let PickGuiNewGame1 = PickGui / "NewGame1"
    let PickGuiNewGame2 = PickGui / "NewGame2"
    let PickGuiNewGame3 = PickGui / "NewGame3"
    let PickGuiLoadGame1 = PickGui / "LoadGame1"
    let PickGuiLoadGame2 = PickGui / "LoadGame2"
    let PickGuiLoadGame3 = PickGui / "LoadGame3"
    let PickGuiBack = PickGui / "Back"

    // credits screen
    let Credits = Screen "Credits"
    let CreditsGui = Credits / "Gui"
    let CreditsGuiBack = CreditsGui / "Back"

    // field screen
    let Field = Screen "Field"
    let FieldScene = Field / "Scene"
    let FieldSceneFeeler = FieldScene / "Feeler"
    let FieldSceneAvatar = FieldScene / "Avatar"
    let FieldSceneTileMap = FieldScene / "TileMap"

    // battle screen
    let Battle = Screen "Battle"
    let BattleScene = Battle / "Scene"
    let BattleSceneRide = BattleScene / "Ride"
    let BattleInputs = Battle / "Inputs"