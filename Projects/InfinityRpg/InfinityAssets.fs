namespace InfinityRpg
open Nu
module Assets =

    let GuiPackage = "Gui"
    let GameplayPackage = "Gameplay"
    let PlayerImage = AssetTag.make<Image> GameplayPackage "Player"
    let GoopyImage = AssetTag.make<Image> GameplayPackage "Goopy"
    let BatsyImage = AssetTag.make<Image> GameplayPackage "Batsy"
    let ZommieImage = AssetTag.make<Image> GameplayPackage "Zommie"
    let FieldTileSheetImage = AssetTag.make<Image> GameplayPackage "FieldTileSheet"
    let HerosVengeanceSong = AssetTag.make<Audio> GameplayPackage "Hero'sVengeance"
    let ButterflyGirlSong = AssetTag.make<Audio> GuiPackage "ButterflyGirl"
    let TitleLayerFilePath = "Assets/Gui/Title.nulyr"
    let CreditsLayerFilePath = "Assets/Gui/Credits.nulyr"
    let HudLayerFilePath = "Assets/Gui/Hud.nulyr"
    let SaveFilePath = "InfinityRpg.sav"