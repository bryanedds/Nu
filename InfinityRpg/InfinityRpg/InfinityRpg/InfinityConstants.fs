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
    let TileSheetSizeI = Vector2i.Multiply (TileSheetSizeM, TileSizeI)
    let TileSheetSize = let t = TileSheetSizeI in t.Vector2
    let CharacterDepth = 1.0f
    let CharacterWalkSpeed = 4.0f

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
          DissolveImage = { PackageName = GuiPackageName; AssetName = "Dissolve" }}

    // splash constants
    let NuSplashName = "Splash"
    let NuSplash = Screen.proxy <| ntoa NuSplashName
    let NuSplashData =
        { DissolveData = DissolveData
          IdlingTime = 60L
          SplashImage = { PackageName = GuiPackageName; AssetName = "Nu" }}

    // asset constants
    let PlayerImage = { PackageName = GameplayPackageName; AssetName = "Player" }
    let GoopyImage = { PackageName = GameplayPackageName; AssetName = "Goopy" }
    let BatsyImage = { PackageName = GameplayPackageName; AssetName = "Batsy" }
    let ZommieImage = { PackageName = GameplayPackageName; AssetName = "Zommie" }
    let FieldTileSheetImage = { PackageName = GameplayPackageName; AssetName = "FieldTileSheet" }
    let HerosVengeanceSong = { PackageName = GameplayPackageName; AssetName = "Hero'sVengeance" }
    let ButterflyGirlSong = { PackageName = GuiPackageName; AssetName = "ButterflyGirl" }

    // title constants
    let TitleName = "Title"
    let Title = Screen.proxy <| ntoa TitleName
    let TitleGroupName = DefaultGroupName
    let TitleGroup = Group.proxy <| satoga Title.ScreenAddress TitleGroupName
    let ClickTitleNewGameEventAddress = ClickEventAddress ->>- TitleGroup.GroupAddress ->- ntoa "NewGame"
    let ClickTitleLoadGameEventAddress = ClickEventAddress ->>- TitleGroup.GroupAddress ->- ntoa "LoadGame"
    let ClickTitleCreditsEventAddress = ClickEventAddress ->>- TitleGroup.GroupAddress ->- ntoa "Credits"
    let ClickTitleExitEventAddress = ClickEventAddress ->>- TitleGroup.GroupAddress ->- ntoa "Exit"

    // credits constants
    let CreditsName = "Credits"
    let Credits = Screen.proxy <| ntoa CreditsName
    let CreditsGroupName = DefaultGroupName
    let CreditsGroup = Group.proxy <| satoga Credits.ScreenAddress CreditsGroupName
    let ClickCreditsBackEventAddress = ClickEventAddress ->>- CreditsGroup.GroupAddress ->- ntoa "Back"

    // gameplay constants
    let GameplayName = "Gameplay"
    let Gameplay = Screen.proxy <| ntoa GameplayName
    
    // hud constants
    let HudName = "Hud"
    let Hud = Group.proxy <| satoga Gameplay.ScreenAddress HudName
    let HudBackName = "Back"
    let HudBack = Entity.proxy <| gatoea Hud.GroupAddress HudBackName
    let HudSaveGameName = "SaveGame"
    let HudHaltName = "Halt"
    let HudFeelerName = "Feeler"
    let HudDetailUpName = "DetailUp"
    let HudDetailRightName = "DetailRight"
    let HudDetailDownName = "DetailDown"
    let HudDetailLeftName = "DetailLeft"
    let SceneName = "Scene"
    
    // scene constants
    let Scene = Group.proxy <| satoga Gameplay.ScreenAddress SceneName
    let FieldName = "Field"
    let PlayerName = "Player"

    // data
    let AttackName = "Attack"
    let CharacterAnimationFacingStutter = 16L
    let CharacterAnimationActingStutter = 24L
    let ActionTicksMax = CharacterAnimationActingStutter * 3L