namespace BlazeVector
open Nu
module BlazeConstants =

    // misc constants. These, and the following constants, will be explained in depth later. Just
    // scan over them for now, or look at them in the debugger on your own.
    let BlazeGuiPackageName = "BlazeGui"
    let StageAvatarName = "Avatar"
    
    // transition constants
    let IncomingTimeSplash = 60
    let IncomingTime = 20
    let IdlingTime = 60
    let OutgoingTimeSplash = 40
    let OutgoingTime = 20

    // splash constants
    let SplashAddress = NuCore.addr "Splash"

    // title constants
    let TitleGroupFileName = "Assets/BlazeVector/Groups/Title.nugroup"
    let TitleAddress = NuCore.addr "Title"
    let TitleGroupAddress = NuCore.addr "Title/Group"
    let ClickTitlePlayEvent = NuCore.addr "Click/Title/Group/Play"
    let ClickTitleCreditsEvent = NuCore.addr "Click/Title/Group/Credits"
    let ClickTitleExitEvent = NuCore.addr "Click/Title/Group/Exit"

    // stage constants
    let StageGroupFileName = "Assets/BlazeVector/Groups/Stage.nugroup"
    let StageAddress = NuCore.addr "Stage"
    let StageGroupAddress = NuCore.addr "Stage/Group"
    let ClickStageBackEvent = NuCore.addr "Click/Stage/Group/Back"

    // credits constants
    let CreditsGroupFileName = "Assets/BlazeVector/Groups/Credits.nugroup"
    let CreditsAddress = NuCore.addr "Credits"
    let CreditsGroupAddress = NuCore.addr "Credits/Group"
    let ClickCreditsBackEvent = NuCore.addr "Click/Credits/Group/Back"