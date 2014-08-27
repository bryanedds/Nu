// OmniBlade.
// Copyright (C) Bryan Edds, 2013-2014.

namespace OmniBlade
open Prime
open Nu
open Nu.NuConstants
module OmniConstants =

    // misc constants
    let GuiPackageName = "Gui"
    let FieldFeelerName = "Feeler"
    let FieldAvatarName = "Avatar"

    // asset constants
    let GameSong = { SongAssetName = "Song"; PackageName = DefaultPackageName }

    // transition constants
    let IncomingTimeSplash = 60L
    let IncomingTime = 20L
    let IdlingTime = 60L
    let OutgoingTimeSplash = 40L
    let OutgoingTime = 30L

    // splash constants
    let SplashAddress = addr "Splash"

    // title constants
    let TitleAddress = addr "Title"
    let TitleGroupFileName = "Assets/OmniBlade/Groups/Title.nugroup"
    let TitleGroupAddress = addr "Title/Group"
    let ClickTitleNewGameEvent = addr "Click/Title/Group/NewGame"
    let ClickTitleLoadGameEvent = addr "Click/Title/Group/LoadGame"
    let ClickTitleCreditsEvent = addr "Click/Title/Group/Credits"
    let ClickTitleExitEvent = addr "Click/Title/Group/Exit"

    // load game constants
    let LoadGameAddress = addr "LoadGame"
    let LoadGameGroupFileName = "Assets/OmniBlade/Groups/LoadGame.nugroup"
    let LoadGameGroupAddress = addr "LoadGame/Group"
    let ClickLoadGameBackEvent = addr "Click/LoadGame/Group/Back"

    // credits constants
    let CreditsAddress = addr "Credits"
    let CreditsGroupFileName = "Assets/OmniBlade/Groups/Credits.nugroup"
    let CreditsGroupAddress = addr "Credits/Group"
    let ClickCreditsBackEvent = addr "Click/Credits/Group/Back"

    // field constants
    let FieldAddress = addr "Field"
    let FieldGroupFileName = "Assets/OmniBlade/Groups/Field.nugroup"
    let FieldGroupAddress = addr "Field/Group"
    let ClickFieldBackEvent = addr "Click/Field/Group/Back"

    // time constants
    let TimeAddress = addr "Time"
