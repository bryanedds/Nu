namespace BlazeVector
open Nu
module BlazeConstants =

    // misc constants. These, and the following constants, will be explained in depth later. Just
    // scan over them for now, or look at them in the debugger on your own.
    let BlazeGuiPackageName = "BlazeGui"

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
    let TitleGroupFileName = "Assets/BlazeVector/Groups/Title.nugroup"
    let ClickTitlePlayEvent = NuCore.straddrstr "Click" TitleGroupAddress "Play"
    let ClickTitleCreditsEvent = NuCore.straddrstr "Click" TitleGroupAddress "Credits"
    let ClickTitleExitEvent = NuCore.straddrstr "Click" TitleGroupAddress "Exit"

    // stage constants
    let StageAddress = NuCore.addr "Stage"
    let StageGroupName = "Group"
    let StageGroupAddress = StageAddress @ [StageGroupName]
    let StageGroupFileName = "Assets/BlazeVector/Groups/Stage.nugroup"
    let ClickStageBackEvent = NuCore.straddrstr "Click" StageGroupAddress "Back"

    // credits constants
    let CreditsAddress = NuCore.addr "Credits"
    let CreditsGroupName = "Group"
    let CreditsGroupAddress = CreditsAddress @ [CreditsGroupName]
    let CreditsGroupFileName = "Assets/BlazeVector/Groups/Credits.nugroup"
    let ClickCreditsBackEvent = NuCore.straddrstr "Click" CreditsGroupAddress "Back"