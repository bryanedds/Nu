namespace InfinityRpg
open Nu
module Assets =

    let GuiPackageName = "Gui"
    let GameplayPackageName = "Gameplay"
    let PlayerImage = asset<Image> GameplayPackageName "Player"
    let GoopyImage = asset<Image> GameplayPackageName "Goopy"
    let BatsyImage = asset<Image> GameplayPackageName "Batsy"
    let ZommieImage = asset<Image> GameplayPackageName "Zommie"
    let FieldTileSheetImage = asset<Image> GameplayPackageName "FieldTileSheet"
    let HerosVengeanceSong = asset<Audio> GameplayPackageName "Hero'sVengeance"
    let ButterflyGirlSong = asset<Audio> GuiPackageName "ButterflyGirl"
    let TitleLayerFilePath = "Assets/Gui/Title.nulyr"
    let CreditsLayerFilePath = "Assets/Gui/Credits.nulyr"
    let HudLayerFilePath = "Assets/Gui/Hud.nulyr"
    let SaveFilePath = "InfinityRpg.sav"