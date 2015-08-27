namespace InfinityRpg
open OpenTK
open Nu
open InfinityRpg

[<RequireQualifiedAccess>]
module Simulants =

    // nu splash screen
    let NuSplash = ntos "Splash"

    // title screen
    let Title = ntos "Title"
    let TitleGroup = stog Title "Group"
    let TitleNewGame = gtoe TitleGroup "NewGame"
    let TitleLoadGame = gtoe TitleGroup "LoadGame"
    let TitleCredits = gtoe TitleGroup "Credits"
    let TitleExit = gtoe TitleGroup "Exit"

    // credits screen
    let Credits = ntos "Credits"
    let CreditsGroup = stog Credits "Group"
    let CreditsBack = gtoe CreditsGroup "Back"

    // gameplay screen
    let Gameplay = ntos "Gameplay"
    
    // hud group
    let Hud = stog Gameplay "Hud"
    let HudBack = gtoe Hud "Back"
    let HudSaveGame = gtoe Hud "SaveGame"
    let HudHalt = gtoe Hud "Halt"
    let HudFeeler = gtoe Hud "Feeler"
    let HudDetailUp = gtoe Hud "DetailUp"
    let HudDetailRight = gtoe Hud "DetailRight"
    let HudDetailDown = gtoe Hud "DetailDown"
    let HudDetailLeft = gtoe Hud "DetailLeft"
    
    // scene group
    let Scene = stog Gameplay "Scene"
    let Field = gtoe Scene "Field"
    let Player = gtoe Scene "Player"