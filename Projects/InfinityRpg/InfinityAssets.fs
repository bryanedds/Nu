namespace InfinityRpg
open OpenTK
open Nu
module Assets =

    let GuiPackageName = "Gui"
    let GameplayPackageName = "Gameplay"
    let PlayerImage = AssetTag.make<Image> GameplayPackageName "Player"
    let GoopyImage = AssetTag.make<Image> GameplayPackageName "Goopy"
    let BatsyImage = AssetTag.make<Image> GameplayPackageName "Batsy"
    let ZommieImage = AssetTag.make<Image> GameplayPackageName "Zommie"
    let FieldTileSheetImage = AssetTag.make<Image> GameplayPackageName "FieldTileSheet"
    let HerosVengeanceSong = AssetTag.make<Audio> GameplayPackageName "Hero'sVengeance"
    let ButterflyGirlSong = AssetTag.make<Audio> GuiPackageName "ButterflyGirl"
    let TitleLayerFilePath = "Assets/Gui/Title.nulyr"
    let CreditsLayerFilePath = "Assets/Gui/Credits.nulyr"
    let HudLayerFilePath = "Assets/Gui/Hud.nulyr"
    let SaveFilePath = "InfinityRpg.sav"