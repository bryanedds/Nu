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
    let TitleGroup = Title => "Group"
    let TitleNewGame = TitleGroup => "NewGame"
    let TitleLoadGame = TitleGroup => "LoadGame"
    let TitleCredits = TitleGroup => "Credits"
    let TitleExit = TitleGroup => "Exit"

    // credits screen
    let Credits = ntos "Credits"
    let CreditsGroup = Credits => "Group"
    let CreditsBack = CreditsGroup => "Back"

    // gameplay screen
    let Gameplay = ntos "Gameplay"
    
    // hud group
    let Hud = Gameplay => "Hud"
    let HudBack = Hud => "Back"
    let HudSaveGame = Hud => "SaveGame"
    let HudHalt = Hud => "Halt"
    let HudFeeler = Hud => "Feeler"
    let HudDetailUp = Hud => "DetailUp"
    let HudDetailRight = Hud => "DetailRight"
    let HudDetailDown = Hud => "DetailDown"
    let HudDetailLeft = Hud => "DetailLeft"
    
    // scene group
    let Scene = Gameplay => "Scene"
    let Field = Scene => "Field"
    let Player = Scene => "Player"