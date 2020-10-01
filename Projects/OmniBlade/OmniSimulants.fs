namespace OmniBlade
open Prime
open Nu

[<RequireQualifiedAccess>]
module Simulants =

    // global
    let Splash = Screen "Splash"

    // title
    let Title = Screen "Title"
    let TitleGui = Title / "Gui"
    let TitlePlay = TitleGui / "Play"
    let TitleCredits = TitleGui / "Credits"
    let TitleExit = TitleGui / "Exit"

    // credits
    let Credits = Screen "Credits"
    let CreditsGui = Credits / "Gui"
    let CreditsBack = CreditsGui / "Back"

    // field
    let Field = Simulants.DefaultScreen
    let FieldHud = Field / "Hud"
    let FieldBack = FieldHud / "Back"
    let FieldScene = Field / "Scene"
    let FieldBackdrop = FieldScene / "Backdrop"
    let FieldPortalFade = FieldScene / "PortalFade"
    let FieldTileMap = FieldScene / "TileMap"
    let FieldAvatar = FieldScene / "Avatar"
    let FieldInteract = FieldScene / "Interact"
    let FieldDialog = FieldScene / "Dialog"

    // battle
    let Battle = Screen "Battle"
    let BattleHud = Battle / "Hud"
    let BattleScene = Battle / "Scene"
    let BattleRide = BattleScene / "Ride"