namespace BlazeVector
open Nu
open Nu.Constants
open Nu.WorldConstants
module BlazeConstants =

    // misc constants. These, and the following constants, will be explained in depth later. Just
    // scan over them for now, or look at them in the debugger on your own.
    let GuiPackageName = "Gui"
    let StagePackageName = "Stage"
    let StagePlayerName = "Player"
    let StagePlayName = "StagePlay"
    let StagePlayFilePath = "Assets/BlazeVector/Stage/StagePlay.nugroup"
    let SectionName = "Section"
    let Section0FilePath = "Assets/BlazeVector/Stage/Section0.nugroup"
    let Section1FilePath = "Assets/BlazeVector/Stage/Section1.nugroup"
    let Section2FilePath = "Assets/BlazeVector/Stage/Section2.nugroup"
    let Section3FilePath = "Assets/BlazeVector/Stage/Section3.nugroup"
    let SectionFilePaths = [Section0FilePath; Section1FilePath; Section2FilePath; Section3FilePath]
    let SectionCount = 32

    // asset constants
    let NuSplashSound = { PackageName = GuiPackageName; AssetName = "Nu" }
    let MachinerySong = { PackageName = GuiPackageName; AssetName = "Machinery" }
    let DeadBlazeSong = { PackageName = StagePackageName; AssetName = "DeadBlaze" }
    let HitSound = { PackageName = StagePackageName; AssetName = "Hit" }
    let ExplosionSound = { PackageName = StagePackageName; AssetName = "Explosion" }
    let ShotSound = { PackageName = StagePackageName; AssetName = "Shot" }
    let JumpSound = { PackageName = StagePackageName; AssetName = "Jump" }
    let DeathSound = { PackageName = StagePackageName; AssetName = "Death" }
    let EnemyBulletImage = { PackageName = StagePackageName; AssetName = "EnemyBullet" }
    let PlayerBulletImage = { PackageName = StagePackageName; AssetName = "PlayerBullet" }
    let EnemyImage = { PackageName = StagePackageName; AssetName = "Enemy" }
    let PlayerImage = { PackageName = StagePackageName; AssetName = "Player" }

    // dissolve constants
    let DissolveData =
        { IncomingTime = 20L
          OutgoingTime = 30L
          DissolveImage = DefaultDissolveImage }

    // splash constants
    let SplashAddress = stoa<Screen> "Splash"
    let SplashData =
        { DissolveData = DissolveData
          IdlingTime = 60L
          SplashImage = { PackageName = DefaultPackageName; AssetName = "Image5" }}

    // title constants
    let TitleAddress = stoa<Screen> "Title"
    let TitleGroupFilePath = "Assets/BlazeVector/Gui/Title.nugroup"
    let SelectTitleEventAddress = stoa<unit> "Select/Title"
    let ClickTitlePlayEventAddress = stoa<unit> "Click/Title/Group/Play"
    let ClickTitleCreditsEventAddress = stoa<unit> "Click/Title/Group/Credits"
    let ClickTitleExitEventAddress = stoa<unit> "Click/Title/Group/Exit"

    // stage constants
    let StageAddress = stoa<Screen> "Stage"
    let StageGroupFilePath = "Assets/BlazeVector/Gui/StageGui.nugroup"
    let ClickStageBackEventAddress = stoa<unit> "Click/Stage/Group/Back"

    // credits constants
    let CreditsAddress = stoa<Screen> "Credits"
    let CreditsGroupFilePath = "Assets/BlazeVector/Gui/Credits.nugroup"
    let ClickCreditsBackEventAddress = stoa<unit> "Click/Credits/Group/Back"