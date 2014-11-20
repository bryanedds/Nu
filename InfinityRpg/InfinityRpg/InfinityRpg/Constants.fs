namespace InfinityRpg
open OpenTK
open Nu
open Nu.Constants
open Nu.WorldConstants
module Constants =

    // misc constants
    let TileSizeI = Vector2i 64
    let TileSize = let t = TileSizeI in t.Vector2
    let TileSheetSizeM = Vector2i 4
    let TileSheetSizeI = TileSheetSizeM * 64
    let TileSheetSize = let t = TileSheetSizeI in t.Vector2

    // file paths
    let TitleGroupFilePath = "Assets/Gui/Title.nugroup"
    let CreditsGroupFilePath = "Assets/Gui/Credits.nugroup"
    let HudFilePath = "Assets/Gui/Hud.nugroup"
    let SaveFilePath = "InfinityRpg.sav"

    // package constants
    let GuiPackageName = "Gui"
    let GameplayPackageName = "Gameplay"

    // dissolve constants
    let DissolveData =
        { IncomingTime = 20L
          OutgoingTime = 30L
          DissolveImage = { ImagePackageName = GuiPackageName; ImageAssetName = "Dissolve" }}

    // splash constants
    let NuSplashName = "Splash"
    let NuSplashAddress = ltoa<Screen> [NuSplashName]
    let NuSplashData =
        { DissolveData = DissolveData
          IdlingTime = 60L
          SplashImage = { ImagePackageName = GuiPackageName; ImageAssetName = "Nu" }}

    // asset constants
    let PlayerImage = { ImagePackageName = GameplayPackageName; ImageAssetName = "Player" }
    let FieldTileSheetImage = { ImagePackageName = GameplayPackageName; ImageAssetName = "FieldTileSheet" }

    // title constants
    let TitleName = "Title"
    let TitleAddress = ltoa<Screen> [TitleName]
    let TitleGroupName = DefaultGroupName
    let TitleGroupAddress = satoga TitleAddress TitleGroupName
    let ClickTitleNewGameEventAddress = ClickEventAddress ->>- TitleGroupAddress ->- ltoa ["NewGame"]
    let ClickTitleLoadGameEventAddress = ClickEventAddress ->>- TitleGroupAddress ->- ltoa ["LoadGame"]
    let ClickTitleCreditsEventAddress = ClickEventAddress ->>- TitleGroupAddress ->- ltoa ["Credits"]
    let ClickTitleExitEventAddress = ClickEventAddress ->>- TitleGroupAddress ->- ltoa ["Exit"]

    // credits constants
    let CreditsName = "Credits"
    let CreditsAddress = ltoa<Screen> [CreditsName]
    let CreditsGroupName = DefaultGroupName
    let CreditsGroupAddress = satoga CreditsAddress CreditsGroupName
    let ClickCreditsBackEventAddress = ClickEventAddress ->>- CreditsGroupAddress ->- ltoa ["Back"]

    // gameplay constants
    let GameplayName = "Gameplay"
    let GameplayAddress = ltoa<Screen> [GameplayName]

    // hud constants
    let HudName = "Hud"
    let HudAddress = satoga GameplayAddress HudName
    let HudFeelerName = "HudFeeler"
    let HudFeelerAddress = gatoea HudAddress HudFeelerName
    let ClickHudBackEventAddress = ClickEventAddress ->>- HudAddress ->- ltoa ["Back"]
    let ClickHudSaveGameEventAddress = ClickEventAddress ->>- HudAddress ->- ltoa ["SaveGame"]

    // scene constants
    let SceneName = "Scene"
    let SceneAddress = satoga GameplayAddress SceneName

    // field constants
    let FieldName = "Field"
    let FieldAddress = gatoea SceneAddress FieldName

    // player character constants
    let PlayerCharacterName = "PlayerCharacter"
    let PlayerCharacterAddress = gatoea SceneAddress PlayerCharacterName