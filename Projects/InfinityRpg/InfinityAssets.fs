namespace InfinityRpg
open Nu
module Assets =

    let GuiPackage = "Gui"
    let GameplayPackage = "Gameplay"
    let PlayerImage = asset<Image> GameplayPackage "Player"
    let GoopyImage = asset<Image> GameplayPackage "Goopy"
    let BatsyImage = asset<Image> GameplayPackage "Batsy"
    let ZommieImage = asset<Image> GameplayPackage "Zommie"
    let FieldTileSheetImage = asset<Image> GameplayPackage "FieldTileSheet"
    let HerosVengeanceSong = asset<Audio> GameplayPackage "Hero'sVengeance"
    let ButterflyGirlSong = asset<Audio> GuiPackage "ButterflyGirl"
    let TitleLayerFilePath = "Assets/Gui/Title.nulyr"
    let CreditsLayerFilePath = "Assets/Gui/Credits.nulyr"
    let HudLayerFilePath = "Assets/Gui/Hud.nulyr"
    let SaveFilePath = "InfinityRpg.sav"