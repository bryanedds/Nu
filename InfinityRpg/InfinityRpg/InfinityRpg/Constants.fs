namespace InfinityRpg
open Nu
open Nu.Constants
open Nu.WorldConstants
module Constants =

    // package constants
    let GuiPackageName = "Gui"
    let GameplayPackageName = "Gameplay"

    // dissolve constants
    let DissolveImage = { ImageAssetName = "Dissolve"; PackageName = GuiPackageName }

    // asset constants
    let PlayerImage = { ImageAssetName = "Player"; PackageName = GameplayPackageName }

    // splash constants
    let SplashNu = "Nu"

    // title constants
    let TitleAddress = stoa<obj> "Title"
    let TitleGroupFilePath = "Assets/Gui/Title.nugroup"
    let ClickTitleNewGameEventAddress = stoa<unit> "Click/Title/Group/NewGame"
    let ClickTitleCreditsEventAddress = stoa<unit> "Click/Title/Group/Credits"
    let ClickTitleExitEventAddress = stoa<unit> "Click/Title/Group/Exit"

    // credits constants
    let CreditsAddress = stoa<obj> "Credits"
    let CreditsGroupFilePath = "Assets/Gui/Credits.nugroup"
    let ClickCreditsBackEventAddress = stoa<unit> "Click/Credits/Group/Back"

    // transition constants
    let IncomingTime = 20L
    let OutgoingTime = 30L
    let StageOutgoingTime = 90L

    // splash constants
    let SplashAddress = stoa<obj> "Splash"
    let SplashIncomingTime = 60L
    let SplashIdlingTime = 60L
    let SplashOutgoingTime = 40L