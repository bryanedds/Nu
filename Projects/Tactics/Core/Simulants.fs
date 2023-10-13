// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Tactics
open System
open Prime
open Nu
open Tactics

[<RequireQualifiedAccess>]
module Simulants =

    // splash screen
    let Splash = Game / "Splash"

    // intro screens
    let Intro = Game / "Intro"
    let Intro2 = Game / "Intro2"
    let Intro3 = Game / "Intro3"
    let Intro4 = Game / "Intro4"
    let Intro5 = Game / "Intro5"

    // title screen
    let Title = Game / "Title"
    let TitleGui = Title / "Gui"
    let TitleGuiPlay = TitleGui / "Play"
    let TitleGuiCredits = TitleGui / "Credits"
    let TitleGuiExit = TitleGui / "Exit"

    // pick screen
    let Pick = Game / "Pick"
    let PickGui = Pick / "Gui"
    let PickGuiNewGame1 = PickGui / "NewGame1"
    let PickGuiNewGame2 = PickGui / "NewGame2"
    let PickGuiNewGame3 = PickGui / "NewGame3"
    let PickGuiLoadGame1 = PickGui / "LoadGame1"
    let PickGuiLoadGame2 = PickGui / "LoadGame2"
    let PickGuiLoadGame3 = PickGui / "LoadGame3"
    let PickGuiBack = PickGui / "Back"

    // credits screen
    let Credits = Game / "Credits"
    let CreditsGui = Credits / "Gui"
    let CreditsGuiBack = CreditsGui / "Back"

    // atals screen
    let Atlas = Game / "Atlas"
    let AtlasScene = Atlas / "Scene"

    // field screen
    let Field = Game / "Field"
    let FieldScene = Field / "Scene"