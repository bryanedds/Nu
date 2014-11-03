// OmniBlade.
// Copyright (C) Bryan Edds, 2013-2014.

namespace OmniBlade
open Prime
open Nu
open Nu.Constants
module OmniConstants =

    // misc constants
    let GuiPackageName = "Gui"
    let FieldFeelerName = "Feeler"
    let FieldAvatarName = "Avatar"

    // asset constants
    let GameSong = { SongAssetName = "Song"; PackageName = DefaultPackageName }

    // transition constants
    let IncomingTime = 20L
    let OutgoingTime = 30L
    let StageOutgoingTime = 90L

    // splash constants
    let SplashAddress = !* "Splash"
    let SplashIncomingTime = 60L
    let SplashIdlingTime = 60L
    let SplashOutgoingTime = 40L

    // title constants
    let TitleAddress = !* "Title"
    let TitleGroupFilePath = "Assets/OmniBlade/Groups/Title.nugroup"
    let ClickTitleNewGameEvent = !* "Click/Title/Group/NewGame"
    let ClickTitleLoadGameEvent = !* "Click/Title/Group/LoadGame"
    let ClickTitleCreditsEvent = !* "Click/Title/Group/Credits"
    let ClickTitleExitEvent = !* "Click/Title/Group/Exit"

    // load game constants
    let LoadGameAddress = !* "LoadGame"
    let LoadGameGroupFilePath = "Assets/OmniBlade/Groups/LoadGame.nugroup"
    let ClickLoadGameBackEvent = !* "Click/LoadGame/Group/Back"

    // credits constants
    let CreditsAddress = !* "Credits"
    let CreditsGroupFilePath = "Assets/OmniBlade/Groups/Credits.nugroup"
    let ClickCreditsBackEvent = !* "Click/Credits/Group/Back"

    // field constants
    let FieldAddress = !* "Field"
    let FieldGroupFilePath = "Assets/OmniBlade/Groups/Field.nugroup"
    let ClickFieldBackEvent = !* "Click/Field/Group/Back"

    // time constants
    let TimeAddress = !* "Time"
