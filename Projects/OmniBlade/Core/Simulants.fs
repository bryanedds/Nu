// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open Prime
open Nu

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

    // credits rolling screen
    let CreditsRolling = Game / "CreditsRolling"
    let CreditsRollingGui = Credits / "Gui"
    let CreditsRollingBack = CreditsGui / "Back"

    // pick screen
    let Pick = Game / "Pick"
    let PickGui = Pick / "Gui"
    let PickNewGame1 = PickGui / "NewGame1"
    let PickNewGame2 = PickGui / "NewGame2"
    let PickNewGame3 = PickGui / "NewGame3"
    let PickLoadGame1 = PickGui / "LoadGame1"
    let PickLoadGame2 = PickGui / "LoadGame2"
    let PickLoadGame3 = PickGui / "LoadGame3"
    let PickBack = PickGui / "Back"

    // intro screens
    let Intro = Game / "Intro"
    let Intro2 = Game / "Intro2"
    let Intro3 = Game / "Intro3"
    let Intro4 = Game / "Intro4"
    let Intro5 = Game / "Intro5"

    // field screen
    let Field = Game / "Field"
    let FieldScene = Field / "Scene"
    let FieldFeeler = FieldScene / "Feeler"
    let FieldAvatar = FieldScene / "Avatar"
    let FieldTileMap = FieldScene / "TileMap"

    // battle screen
    let Battle = Game / "Battle"
    let BattleScene = Battle / "Scene"
    let BattleRide = BattleScene / "Ride"
    let BattleInputs = Battle / "Inputs"