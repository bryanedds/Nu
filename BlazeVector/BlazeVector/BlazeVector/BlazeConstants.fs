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
    let StagePlayFilePath = "Assets/BlazeVector/Groups/StagePlay.nugroup"
    let SectionName = "Section"
    let Section0FilePath = "Assets/BlazeVector/Groups/Section0.nugroup"
    let Section1FilePath = "Assets/BlazeVector/Groups/Section1.nugroup"
    let Section2FilePath = "Assets/BlazeVector/Groups/Section2.nugroup"
    let Section3FilePath = "Assets/BlazeVector/Groups/Section3.nugroup"
    let SectionFilePaths = [Section0FilePath; Section1FilePath; Section2FilePath; Section3FilePath]
    let SectionCount = 32

    // asset constants
    let NuSplashSound = { SoundPackageName = GuiPackageName; SoundAssetName = "Nu" }
    let MachinerySong = { SongPackageName = GuiPackageName; SongAssetName = "Machinery" }
    let DeadBlazeSong = { SongPackageName = StagePackageName; SongAssetName = "DeadBlaze" }
    let HitSound = { SoundPackageName = StagePackageName; SoundAssetName = "Hit" }
    let ExplosionSound = { SoundPackageName = StagePackageName; SoundAssetName = "Explosion" }
    let ShotSound = { SoundPackageName = StagePackageName; SoundAssetName = "Shot" }
    let JumpSound = { SoundPackageName = StagePackageName; SoundAssetName = "Jump" }
    let DeathSound = { SoundPackageName = StagePackageName; SoundAssetName = "Death" }
    let EnemyBulletImage = { ImagePackageName = StagePackageName; ImageAssetName = "EnemyBullet" }
    let PlayerBulletImage = { ImagePackageName = StagePackageName; ImageAssetName = "PlayerBullet" }
    let EnemyImage = { ImagePackageName = StagePackageName; ImageAssetName = "Enemy" }
    let PlayerImage = { ImagePackageName = StagePackageName; ImageAssetName = "Player" }

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
          SplashImage = { ImagePackageName = DefaultPackageName; ImageAssetName = "Image5" }}

    // title constants
    let TitleAddress = stoa<Screen> "Title"
    let TitleGroupFilePath = "Assets/BlazeVector/Groups/Title.nugroup"
    let SelectTitleEventAddress = stoa<unit> "Select/Title"
    let ClickTitlePlayEventAddress = stoa<unit> "Click/Title/Group/Play"
    let ClickTitleCreditsEventAddress = stoa<unit> "Click/Title/Group/Credits"
    let ClickTitleExitEventAddress = stoa<unit> "Click/Title/Group/Exit"

    // stage constants
    let StageAddress = stoa<Screen> "Stage"
    let StageGroupFilePath = "Assets/BlazeVector/Groups/StageGui.nugroup"
    let ClickStageBackEventAddress = stoa<unit> "Click/Stage/Group/Back"

    // credits constants
    let CreditsAddress = stoa<Screen> "Credits"
    let CreditsGroupFilePath = "Assets/BlazeVector/Groups/Credits.nugroup"
    let ClickCreditsBackEventAddress = stoa<unit> "Click/Credits/Group/Back"