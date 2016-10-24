namespace InfinityRpg
open OpenTK
open Nu
module Assets =

    let SaveFilePath = "InfinityRpg.sav"
    let GuiPackageName = "Gui"
    let GameplayPackageName = "Gameplay"
    let PlayerImage = { PackageName = GameplayPackageName; AssetName = "Player" }
    let GoopyImage = { PackageName = GameplayPackageName; AssetName = "Goopy" }
    let BatsyImage = { PackageName = GameplayPackageName; AssetName = "Batsy" }
    let ZommieImage = { PackageName = GameplayPackageName; AssetName = "Zommie" }
    let FieldTileSheetImage = { PackageName = GameplayPackageName; AssetName = "FieldTileSheet" }
    let HerosVengeanceSong = { PackageName = GameplayPackageName; AssetName = "Hero'sVengeance" }
    let ButterflyGirlSong = { PackageName = GuiPackageName; AssetName = "ButterflyGirl" }
    let TitleGroupFilePath = "Assets/Gui/Title.nugroup"
    let CreditsGroupFilePath = "Assets/Gui/Credits.nugroup"
    let HudGroupFilePath = "Assets/Gui/Hud.nugroup"