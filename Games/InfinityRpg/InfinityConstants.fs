namespace InfinityRpg
open OpenTK
open Nu

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module FilePaths =

        let TitleGroup = "Assets/Gui/Title.nugroup"
        let CreditsGroup = "Assets/Gui/Credits.nugroup"
        let HudGroup = "Assets/Gui/Hud.nugroup"
        let SaveFile = "InfinityRpg.sav"

    [<RequireQualifiedAccess>]
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
              DissolveImage = { PackageName = Assets.GuiPackageName; AssetName = "Dissolve" }}

        // nu splash constants
        let NuSplashData =
            { DissolveData = DissolveData
              IdlingTime = 60L
              SplashImage = { PackageName = Assets.GuiPackageName; AssetName = "Nu" }}

        // general data
        let AttackName = "Attack"
        let CharacterAnimationFacingStutter = 16L
        let CharacterAnimationActingStutter = 24L
        let ActionTicksMax = CharacterAnimationActingStutter * 3L