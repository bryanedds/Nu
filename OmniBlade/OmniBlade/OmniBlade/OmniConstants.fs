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
    let SplashAddress = NuCore.addr "Splash"

    // title constants
    let TitleGroupFileName = "Assets/OmniBlade/Groups/Title.nugroup"
    let TitleAddress = NuCore.addr "Title"
    let TitleGroupAddress = NuCore.addr "Title/Group"
    let ClickTitleNewGameEvent = NuCore.addr "Click/Title/Group/NewGame"
    let ClickTitleLoadGameEvent = NuCore.addr "Click/Title/Group/LoadGame"
    let ClickTitleCreditsEvent = NuCore.addr "Click/Title/Group/Credits"
    let ClickTitleExitEvent = NuCore.addr "Click/Title/Group/Exit"

    // load game constants
    let LoadGameGroupFileName = "Assets/OmniBlade/Groups/LoadGame.nugroup"
    let LoadGameAddress = NuCore.addr "LoadGame"
    let LoadGameGroupAddress = NuCore.addr "LoadGame/Group"
    let ClickLoadGameBackEvent = NuCore.addr "Click/LoadGame/Group/Back"

    // credits constants
    let CreditsGroupFileName = "Assets/OmniBlade/Groups/Credits.nugroup"
    let CreditsAddress = NuCore.addr "Credits"
    let CreditsGroupAddress = NuCore.addr "Credits/Group"
    let ClickCreditsBackEvent = NuCore.addr "Click/Credits/Group/Back"

    // field constants
    let FieldGroupFileName = "Assets/OmniBlade/Groups/Field.nugroup"
    let FieldAddress = NuCore.addr "Field"
    let FieldGroupAddress = NuCore.addr "Field/Group"
    let ClickFieldBackEvent = NuCore.addr "Click/Field/Group/Back"

    // time constants
    let TimeAddress = NuCore.addr "Time"