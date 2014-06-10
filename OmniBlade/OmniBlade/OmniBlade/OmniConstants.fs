// OmniBlade.
// Copyright (C) Bryan Edds, 2013-2014.

namespace OmniBlade
open Prime
open Nu
module OmniConstants =

    // misc constants
    let OmniGuiPackageName = "OmniGui"
    let FieldFeelerName = "Feeler"
    let FieldAvatarName = "Avatar"

    // transition constants
    let IncomingTimeSplash = 60
    let IncomingTime = 20
    let IdlingTime = 60
    let OutgoingTimeSplash = 40
    let OutgoingTime = 20

    // splash constants
    let SplashAddress = addr "Splash"

    // title constants
    let TitleGroupFileName = "Assets/OmniBlade/Groups/Title.nugroup"
    let TitleAddress = addr "Title"
    let TitleGroupAddress = addr "Title/Group"
    let ClickTitleNewGameEvent = addr "Click/Title/Group/NewGame"
    let ClickTitleLoadGameEvent = addr "Click/Title/Group/LoadGame"
    let ClickTitleCreditsEvent = addr "Click/Title/Group/Credits"
    let ClickTitleExitEvent = addr "Click/Title/Group/Exit"

    // load game constants
    let LoadGameGroupFileName = "Assets/OmniBlade/Groups/LoadGame.nugroup"
    let LoadGameAddress = addr "LoadGame"
    let LoadGameGroupAddress = addr "LoadGame/Group"
    let ClickLoadGameBackEvent = addr "Click/LoadGame/Group/Back"

    // credits constants
    let CreditsGroupFileName = "Assets/OmniBlade/Groups/Credits.nugroup"
    let CreditsAddress = addr "Credits"
    let CreditsGroupAddress = addr "Credits/Group"
    let ClickCreditsBackEvent = addr "Click/Credits/Group/Back"

    // field constants
    let FieldGroupFileName = "Assets/OmniBlade/Groups/Field.nugroup"
    let FieldAddress = addr "Field"
    let FieldGroupAddress = addr "Field/Group"
    let ClickFieldBackEvent = addr "Click/Field/Group/Back"

    // time constants
    let TimeAddress = addr "Time"
