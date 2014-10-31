namespace InfinityRpg
open Nu
module Constants =

    // package constants
    let UIPackageName = "UI"
    let GameplayPackageName = "Gameplay"

    // title constants
    let TitleAddress = !* "Title"
    let TitleGroupFilePath = "Assets/UI/Title.nugroup"
    let ClickTitleNewGameEventAddress = !* "Click/Title/Group/NewGame"
    let ClickTitleCreditsEventAddress = !* "Click/Title/Group/Credits"
    let ClickTitleExitEventAddress = !* "Click/Title/Group/Exit"

    // credits constants
    let CreditsAddress = !* "Credits"
    let CreditsGroupFilePath = "Assets/UI/Credits.nugroup"
    let ClickCreditsBackEventAddress = !* "Click/Credits/Group/Back"

    // transition constants
    let IncomingTime = 20L
    let OutgoingTime = 30L
    let StageOutgoingTime = 90L

    // splash constants
    let SplashAddress = !* "Splash"
    let SplashIncomingTime = 60L
    let SplashIdlingTime = 60L
    let SplashOutgoingTime = 40L