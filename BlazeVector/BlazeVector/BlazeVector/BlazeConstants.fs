namespace BlazeVector
open Nu
module BlazeConstants =

    // misc constants. These, and the following constants, will be explained in depth later. Just
    // scan over them for now, or look at them in the debugger on your own.
    let BlazeGuiPackageName = "BlazeGui"
    let BlazeStagesPackageName = "BlazeStages"
    let StagePlayerName = "Player"
    let StagePlayName = "StagePlay"
    let StagePlayFileName = "Assets/BlazeVector/Groups/StagePlay.nugroup"
    let Section0Name = "Section0"
    let Section0FileName = "Assets/BlazeVector/Groups/Section0.nugroup"
    let Section1Name = "Section1"
    let Section1FileName = "Assets/BlazeVector/Groups/Section1.nugroup"
    let Section2Name = "Section2"
    let Section2FileName = "Assets/BlazeVector/Groups/Section2.nugroup"
    let Section3Name = "Section3"
    let Section3FileName = "Assets/BlazeVector/Groups/Section3.nugroup"
    
    // transition constants
    let IncomingTimeSplash = 60L
    let IncomingTime = 20L
    let IdlingTime = 60L
    let OutgoingTimeSplash = 40L
    let OutgoingTime = 20L

    // splash constants
    let SplashAddress = addr "Splash"

    // title constants
    let TitleAddress = addr "Title"
    let TitleGroupFileName = "Assets/BlazeVector/Groups/Title.nugroup"
    let TitleGroupAddress = addr "Title/Group"
    let SelectedTitleEvent = addr "Selected/Title"
    let ClickTitlePlayEvent = addr "Click/Title/Group/Play"
    let ClickTitleCreditsEvent = addr "Click/Title/Group/Credits"
    let ClickTitleExitEvent = addr "Click/Title/Group/Exit"

    // stage constants
    let StageAddress = addr "Stage"
    let StageGroupFileName = "Assets/BlazeVector/Groups/StageGui.nugroup"
    let StageGroupAddress = addr "Stage/Group"
    let ClickStageBackEvent = addr "Click/Stage/Group/Back"

    // credits constants
    let CreditsAddress = addr "Credits"
    let CreditsGroupFileName = "Assets/BlazeVector/Groups/Credits.nugroup"
    let CreditsGroupAddress = addr "Credits/Group"
    let ClickCreditsBackEvent = addr "Click/Credits/Group/Back"