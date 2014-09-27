// OmniBlade.
// Copyright (C) Bryan Edds, 2013-2014.

namespace OmniBlade
open Prime
open Nu
open Nu.Constants
module OmniConstants =

    // misc constants
    let UIPackageName = "UI"
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
    let SplashAddress = !* "Splash"

    // title constants
    let TitleAddress = !* "Title"
    let TitleGroupFileName = "Assets/OmniBlade/Groups/Title.nugroup"
    let TitleGroupAddress = !* "Title/Group"
    let ClickTitleNewGameEvent = !* "Click/Title/Group/NewGame"
    let ClickTitleLoadGameEvent = !* "Click/Title/Group/LoadGame"
    let ClickTitleCreditsEvent = !* "Click/Title/Group/Credits"
    let ClickTitleExitEvent = !* "Click/Title/Group/Exit"

    // load game constants
    let LoadGameAddress = !* "LoadGame"
    let LoadGameGroupFileName = "Assets/OmniBlade/Groups/LoadGame.nugroup"
    let LoadGameGroupAddress = !* "LoadGame/Group"
    let ClickLoadGameBackEvent = !* "Click/LoadGame/Group/Back"

    // credits constants
    let CreditsAddress = !* "Credits"
    let CreditsGroupFileName = "Assets/OmniBlade/Groups/Credits.nugroup"
    let CreditsGroupAddress = !* "Credits/Group"
    let ClickCreditsBackEvent = !* "Click/Credits/Group/Back"

    // field constants
    let FieldAddress = !* "Field"
    let FieldGroupFileName = "Assets/OmniBlade/Groups/Field.nugroup"
    let FieldGroupAddress = !* "Field/Group"
    let ClickFieldBackEvent = !* "Click/Field/Group/Back"

    // time constants
    let TimeAddress = !* "Time"
