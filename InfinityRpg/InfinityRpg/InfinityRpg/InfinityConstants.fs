namespace InfinityRpg
open OpenTK
open Nu

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module FilePaths =

        let TitleGroupFilePath = "Assets/Gui/Title.nugroup"
        let CreditsGroupFilePath = "Assets/Gui/Credits.nugroup"
        let HudGroupFilePath = "Assets/Gui/Hud.nugroup"
        let SaveFilePath = "InfinityRpg.sav"

    [<RequireQualifiedAccess>]
    module PackageNames =

        let Gui = "Gui"
        let Gameplay = "Gameplay"

    [<RequireQualifiedAccess>]
    module Assets =

        let PlayerImage = { PackageName = PackageNames.Gameplay; AssetName = "Player" }
        let GoopyImage = { PackageName = PackageNames.Gameplay; AssetName = "Goopy" }
        let BatsyImage = { PackageName = PackageNames.Gameplay; AssetName = "Batsy" }
        let ZommieImage = { PackageName = PackageNames.Gameplay; AssetName = "Zommie" }
        let FieldTileSheetImage = { PackageName = PackageNames.Gameplay; AssetName = "FieldTileSheet" }
        let HerosVengeanceSong = { PackageName = PackageNames.Gameplay; AssetName = "Hero'sVengeance" }
        let ButterflyGirlSong = { PackageName = PackageNames.Gui; AssetName = "ButterflyGirl" }

    [<RequireQualifiedAccess>]
    module Layout =

        let TileSizeI = Vector2i 64
        let TileSize = let t = TileSizeI in t.Vector2
        let TileSheetSizeM = Vector2i 4
        let TileSheetSizeI = Vector2i.Multiply (TileSheetSizeM, TileSizeI)
        let TileSheetSize = let t = TileSheetSizeI in t.Vector2
        let CharacterDepth = 1.0f
        let CharacterWalkSpeed = 4.0f

    [<RequireQualifiedAccess>]
    module InfinityRpg =

        // dissolve constants
        let DissolveData =
            { IncomingTime = 20L
              OutgoingTime = 30L
              DissolveImage = { PackageName = PackageNames.Gui; AssetName = "Dissolve" }}

        // nu splash constants
        let NuSplashData =
            { DissolveData = DissolveData
              IdlingTime = 60L
              SplashImage = { PackageName = PackageNames.Gui; AssetName = "Nu" }}

        // general data
        let AttackName = "Attack"
        let CharacterAnimationFacingStutter = 16L
        let CharacterAnimationActingStutter = 24L
        let ActionTicksMax = CharacterAnimationActingStutter * 3L