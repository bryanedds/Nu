namespace InfinityRpg
open OpenTK
open Nu
open InfinityRpg

[<RequireQualifiedAccess>]
module Simulants =

    // nu splash screen
    let NuSplashName = "Splash"
    let NuSplash = Screen.proxy <| ntoa NuSplashName

    // title screen
    let TitleName = "Title"
    let Title = Screen.proxy <| ntoa TitleName
    let TitleGroupName = "Group"
    let TitleGroup = Group.proxy <| satoga Title.ScreenAddress TitleGroupName
    let TitleNewGameName = "NewGame"
    let TitleNewGame = Entity.proxy <| gatoea TitleGroup.GroupAddress TitleNewGameName
    let TitleLoadGameName = "LoadGame"
    let TitleLoadGame = Entity.proxy <| gatoea TitleGroup.GroupAddress TitleLoadGameName
    let TitleCreditsName = "Credits"
    let TitleCredits = Entity.proxy <| gatoea TitleGroup.GroupAddress TitleCreditsName
    let TitleExitName = "Exit"
    let TitleExit = Entity.proxy <| gatoea TitleGroup.GroupAddress TitleExitName

    // credits screen
    let CreditsName = "Credits"
    let Credits = Screen.proxy <| ntoa CreditsName
    let CreditsGroupName = "Group"
    let CreditsGroup = Group.proxy <| satoga Credits.ScreenAddress CreditsGroupName
    let CreditsBackName = "Back"
    let CreditsBack = Entity.proxy <| gatoea CreditsGroup.GroupAddress CreditsBackName

    // gameplay screen
    let GameplayName = "Gameplay"
    let Gameplay = Screen.proxy <| ntoa GameplayName
    
    // hud group
    let HudName = "Hud"
    let Hud = Group.proxy <| satoga Gameplay.ScreenAddress HudName
    let HudBackName = "Back"
    let HudBack = Entity.proxy <| gatoea Hud.GroupAddress HudBackName
    let HudSaveGameName = "SaveGame"
    let HudHaltName = "Halt"
    let HudFeelerName = "Feeler"
    let HudDetailUpName = "DetailUp"
    let HudDetailRightName = "DetailRight"
    let HudDetailDownName = "DetailDown"
    let HudDetailLeftName = "DetailLeft"
    let SceneName = "Scene"
    
    // scene group
    let Scene = Group.proxy <| satoga Gameplay.ScreenAddress SceneName
    let FieldName = "Field"
    let PlayerName = "Player"