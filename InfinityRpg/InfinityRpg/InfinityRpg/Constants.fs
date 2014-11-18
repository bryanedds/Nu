namespace InfinityRpg
open Nu
open Nu.Constants
open Nu.WorldConstants
module Constants =

    // package constants
    let GuiPackageName = "Gui"
    let GameplayPackageName = "Gameplay"

    // dissolve constants
    let DissolveImage = { ImagePackageName = GuiPackageName; ImageAssetName = "Dissolve" }

    // asset constants
    let PlayerImage = { ImagePackageName = GameplayPackageName; ImageAssetName = "Player" }
    let FieldTileSheetImage = { ImagePackageName = GameplayPackageName; ImageAssetName = "FieldTileSheet" }

    // splash constants
    let SplashNu = "Nu"

    // title constants
    let TitleAddress = stoa<Screen> "Title"
    let TitleGroupFilePath = "Assets/Gui/Title.nugroup"
    let ClickTitleNewGameEventAddress = stoa<unit> "Click/Title/Group/NewGame"
    let ClickTitleCreditsEventAddress = stoa<unit> "Click/Title/Group/Credits"
    let ClickTitleExitEventAddress = stoa<unit> "Click/Title/Group/Exit"

    // credits constants
    let CreditsAddress = stoa<Screen> "Credits"
    let CreditsGroupFilePath = "Assets/Gui/Credits.nugroup"
    let ClickCreditsBackEventAddress = stoa<unit> "Click/Credits/Group/Back"

    // transition constants
    let IncomingTime = 20L
    let OutgoingTime = 30L
    let StageOutgoingTime = 90L

    // splash constants
    let SplashAddress = stoa<Screen> "Splash"
    let SplashIncomingTime = 60L
    let SplashIdlingTime = 60L
    let SplashOutgoingTime = 40L