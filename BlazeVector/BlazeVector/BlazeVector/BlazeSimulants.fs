namespace BlazeVector
open Nu
open BlazeVector

[<RequireQualifiedAccess>]
module Simulants =

    // this constant is the result of converting a name to a Screen proxy. A proxy is like an
    // 'address' with which to affect a simulant.
    let Splash = ntos "Splash"

    // same as above, but for the title screen
    let Title = ntos "Title"

    // this is the group that is loaded into the title screen that contains all of its gui
    // entities. You'll notice that the group is built from a combination of the title screen's
    // proxy as well as its own personal name as found in its originating document,
    // 'Assets/Gui/Title.nugroup'.
    let TitleGroup = stog Title "Group"

    // this ia like the above, but for the play button found in the above group
    let TitlePlay = gtoe TitleGroup "Play"
    
    // and so on for the title screens credits and exit buttons.
    let TitleCredits = gtoe TitleGroup "Credits"
    let TitleExit = gtoe TitleGroup "Exit"

    // like those proceeding them, these are proxies for various simulants of the gameplay screen
    let Gameplay = ntos "Gameplay"
    let GameplayGroup = stog Gameplay "Group"
    let GameplayBack = gtoe GameplayGroup "Back"
    let PlayerGroup = stog Gameplay "Player"
    let Player = gtoe PlayerGroup "Player"

    // proxies for the credits screen
    let Credits = ntos "Credits"
    let CreditsGroup = stog Credits "Group"
    let CreditsBack = gtoe CreditsGroup "Back"