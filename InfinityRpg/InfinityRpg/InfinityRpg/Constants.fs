namespace InfinityRpg
open Nu
open Nu.Constants
open Nu.WorldConstants
module Constants =

    // misc
    let SaveFilePath = "InfinityRpg.sav"

    // package constants
    let GuiPackageName = "Gui"
    let GameplayPackageName = "Gameplay"

    // dissolve constants
    let DissolveData =
        { IncomingTime = 20L
          OutgoingTime = 30L
          DissolveImage = { ImagePackageName = GuiPackageName; ImageAssetName = "Dissolve" }}

    // splash constants
    let NuSplashAddress = stoa<Screen> "Splash"
    let NuSplashData =
        { DissolveData = DissolveData
          IdlingTime = 60L
          SplashImage = { ImagePackageName = GuiPackageName; ImageAssetName = "Nu" }}

    // asset constants
    let PlayerImage = { ImagePackageName = GameplayPackageName; ImageAssetName = "Player" }
    let FieldTileSheetImage = { ImagePackageName = GameplayPackageName; ImageAssetName = "FieldTileSheet" }

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

    // gameplay constants
    let GameplayAddress = stoa<Screen> "Gameplay"
    let GameplayGroupFilePath = "Assets/Gui/Gameplay.nugroup"
    let ClickGameplayBackEventAddress = stoa<unit> "Click/Gameplay/Group/Back"
    let ClickGameplaySaveGameEventAddress = stoa<unit> "Click/Gameplay/Group/SaveGame"