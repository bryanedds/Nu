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
    let TitleAddress = NuCore.addr "Title"
    let TitleGroupName = "Group"
    let TitleGroupAddress = TitleAddress @ [TitleGroupName]
    let TitleGroupFileName = "Assets/OmniBlade/Groups/Title.nugroup"
    let ClickTitleNewGameEvent = NuCore.straddrstr "Click" TitleGroupAddress "NewGame"
    let ClickTitleLoadGameEvent = NuCore.straddrstr "Click" TitleGroupAddress "LoadGame"
    let ClickTitleCreditsEvent = NuCore.straddrstr "Click" TitleGroupAddress "Credits"
    let ClickTitleExitEvent = NuCore.straddrstr "Click" TitleGroupAddress "Exit"

    // load game constants
    let LoadGameAddress = NuCore.addr "LoadGame"
    let LoadGameGroupName = "Group"
    let LoadGameGroupAddress = LoadGameAddress @ [LoadGameGroupName]
    let LoadGameGroupFileName = "Assets/OmniBlade/Groups/LoadGame.nugroup"
    let ClickLoadGameBackEvent = NuCore.straddrstr "Click" LoadGameGroupAddress "Back"

    // credits constants
    let CreditsAddress = NuCore.addr "Credits"
    let CreditsGroupName = "Group"
    let CreditsGroupAddress = CreditsAddress @ [CreditsGroupName]
    let CreditsGroupFileName = "Assets/OmniBlade/Groups/Credits.nugroup"
    let ClickCreditsBackEvent = NuCore.straddrstr "Click" CreditsGroupAddress "Back"

    // field constants
    let FieldAddress = NuCore.addr "Field"
    let FieldGroupName = "Group"
    let FieldGroupAddress = FieldAddress @ [FieldGroupName]
    let FieldGroupFileName = "Assets/OmniBlade/Groups/Field.nugroup"
    let ClickFieldBackEvent = NuCore.straddrstr "Click" FieldGroupAddress "Back"

    // time constants
    let TimeAddress = NuCore.addr "Time"