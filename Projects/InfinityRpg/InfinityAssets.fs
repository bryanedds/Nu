namespace InfinityRpg
open OpenTK
open Nu
module Assets =

    let GuiPackageName = "Gui"
    let GameplayPackageName = "Gameplay"
    let PlayerImage = { PackageName = GameplayPackageName; AssetName = "Player" }
    let GoopyImage = { PackageName = GameplayPackageName; AssetName = "Goopy" }
    let BatsyImage = { PackageName = GameplayPackageName; AssetName = "Batsy" }
    let ZommieImage = { PackageName = GameplayPackageName; AssetName = "Zommie" }
    let FieldTileSheetImage = { PackageName = GameplayPackageName; AssetName = "FieldTileSheet" }
    let HerosVengeanceSong = { PackageName = GameplayPackageName; AssetName = "Hero'sVengeance" }
    let ButterflyGirlSong = { PackageName = GuiPackageName; AssetName = "ButterflyGirl" }
    let TitleLayerFilePath = "Assets/Gui/Title.nulyr"
    let CreditsLayerFilePath = "Assets/Gui/Credits.nulyr"
    let HudLayerFilePath = "Assets/Gui/Hud.nulyr"
    let SaveFilePath = "InfinityRpg.sav"