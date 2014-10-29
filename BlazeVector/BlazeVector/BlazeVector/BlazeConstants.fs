namespace BlazeVector
open Nu
open Nu.Constants
module BlazeConstants =

    // misc constants. These, and the following constants, will be explained in depth later. Just
    // scan over them for now, or look at them in the debugger on your own.
    let UIPackageName = "UI"
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
    let NuSplashSound = { SoundAssetName = "Nu"; PackageName = UIPackageName }
    let MachinerySong = { SongAssetName = "Machinery"; PackageName = UIPackageName }
    let DeadBlazeSong = { SongAssetName = "DeadBlaze"; PackageName = StagePackageName }
    let HitSound = { SoundAssetName = "Hit"; PackageName = StagePackageName }
    let ExplosionSound = { SoundAssetName = "Explosion"; PackageName = StagePackageName }
    let ShotSound = { SoundAssetName = "Shot"; PackageName = StagePackageName }
    let JumpSound = { SoundAssetName = "Jump"; PackageName = StagePackageName }
    let DeathSound = { SoundAssetName = "Death"; PackageName = StagePackageName }
    let EnemyBulletImage = { ImageAssetName = "EnemyBullet"; PackageName = StagePackageName }
    let PlayerBulletImage = { ImageAssetName = "PlayerBullet"; PackageName = StagePackageName }
    let EnemyImage = { ImageAssetName = "Enemy"; PackageName = StagePackageName }
    let PlayerImage = { ImageAssetName = "Player"; PackageName = StagePackageName }

    // transition constants
    let IncomingTimeSplash = 60L
    let IncomingTime = 20L
    let IdlingTime = 60L
    let OutgoingTimeSplash = 40L
    let OutgoingTime = 30L
    let StageOutgoingTime = 90L

    // splash constants
    let SplashAddress = !* "Splash"

    // title constants
    let TitleAddress = !* "Title"
    let TitleGroupFilePath = "Assets/BlazeVector/Groups/Title.nugroup"
    let SelectTitleEventAddress = !* "Select/Title"
    let ClickTitlePlayEventAddress = !* "Click/Title/Group/Play"
    let ClickTitleCreditsEventAddress = !* "Click/Title/Group/Credits"
    let ClickTitleExitEventAddress = !* "Click/Title/Group/Exit"

    // stage constants
    let StageAddress = !* "Stage"
    let StageGroupFilePath = "Assets/BlazeVector/Groups/StageUI.nugroup"
    let ClickStageBackEventAddress = !* "Click/Stage/Group/Back"

    // credits constants
    let CreditsAddress = !* "Credits"
    let CreditsGroupFilePath = "Assets/BlazeVector/Groups/Credits.nugroup"
    let ClickCreditsBackEventAddress = !* "Click/Credits/Group/Back"