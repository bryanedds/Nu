// OmniBlade.
// Copyright (C) Bryan Edds, 2013-2014.

namespace OmniBlade
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
module OmniConstants =

    // misc constants
    let GuiPackageName = "Gui"
    let FieldFeelerName = "Feeler"
    let FieldCharacterName = "Character"

    // dissolve constants
    let DissolveData =
        { IncomingTime = 20L
          OutgoingTime = 30L
          DissolveImage = DefaultDissolveImage }

    // splash constants
    let SplashAddress = stoa<Screen> "Splash"
    let SplashData =
        { DissolveData = DissolveData
          IdlingTime = 60L
          SplashImage = { PackageName = DefaultPackageName; AssetName = "Image5" }}

    // asset constants
    let GameSong = { PackageName = DefaultPackageName; AssetName = "Song" }

    // title constants
    let TitleAddress = stoa<Screen> "Title"
    let TitleGroupFilePath = "Assets/OmniBlade/Groups/Title.nugroup"
    let ClickTitleNewGameEvent = stoa<unit> "Click/Title/Group/NewGame"
    let ClickTitleLoadGameEvent = stoa<unit> "Click/Title/Group/LoadGame"
    let ClickTitleCreditsEvent = stoa<unit> "Click/Title/Group/Credits"
    let ClickTitleExitEvent = stoa<unit> "Click/Title/Group/Exit"

    // load game constants
    let LoadGameAddress = stoa<Screen> "LoadGame"
    let LoadGameGroupFilePath = "Assets/OmniBlade/Groups/LoadGame.nugroup"
    let ClickLoadGameBackEvent = stoa<unit> "Click/LoadGame/Group/Back"

    // credits constants
    let CreditsAddress = stoa<Screen> "Credits"
    let CreditsGroupFilePath = "Assets/OmniBlade/Groups/Credits.nugroup"
    let ClickCreditsBackEvent = stoa<unit> "Click/Credits/Group/Back"

    // field constants
    let FieldAddress = stoa<Screen> "Field"
    let FieldGroupFilePath = "Assets/OmniBlade/Groups/Field.nugroup"
    let ClickFieldBackEvent = stoa<unit> "Click/Field/Group/Back"

    // time constants
    let TimeAddress = stoa<Screen> "Time"
