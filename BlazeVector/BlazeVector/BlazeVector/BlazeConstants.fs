namespace BlazeVector
open Nu
open Nu.NuConstants
module BlazeConstants =

    // misc constants. These, and the following constants, will be explained in depth later. Just
    // scan over them for now, or look at them in the debugger on your own.
    let GuiPackageName = "Gui"
    let StagePackageName = "Stage"
    let StagePlayerName = "Player"
    let StagePlayName = "StagePlay"
    let StagePlayFileName = "Assets/BlazeVector/Groups/StagePlay.nugroup"
    let SectionName = "Section"
    let Section0FileName = "Assets/BlazeVector/Groups/Section0.nugroup"
    let Section1FileName = "Assets/BlazeVector/Groups/Section1.nugroup"
    let Section2FileName = "Assets/BlazeVector/Groups/Section2.nugroup"
    let Section3FileName = "Assets/BlazeVector/Groups/Section3.nugroup"
    let SectionFileNames = [Section0FileName; Section1FileName; Section2FileName; Section3FileName]
    let SectionCount = 32

    // asset constants
    let NuSplashSound = { SoundAssetName = "Nu"; PackageName = GuiPackageName; PackageFileName = AssetGraphFileName }
    let MachinerySong = { SongAssetName = "Machinery"; PackageName = GuiPackageName; PackageFileName = AssetGraphFileName }
    let DeadBlazeSong = { SongAssetName = "DeadBlaze"; PackageName = StagePackageName; PackageFileName = AssetGraphFileName }
    let HitSound = { SoundAssetName = "Hit"; PackageName = StagePackageName; PackageFileName = AssetGraphFileName }
    let ExplosionSound = { SoundAssetName = "Explosion"; PackageName = StagePackageName; PackageFileName = AssetGraphFileName }
    let ShotSound = { SoundAssetName = "Shot"; PackageName = StagePackageName; PackageFileName = AssetGraphFileName }
    let JumpSound = { SoundAssetName = "Jump"; PackageName = StagePackageName; PackageFileName = AssetGraphFileName }
    let DeathSound = { SoundAssetName = "Death"; PackageName = StagePackageName; PackageFileName = AssetGraphFileName }
    let EnemyBulletImage = { ImageAssetName = "EnemyBullet"; PackageName = StagePackageName; PackageFileName = AssetGraphFileName }
    let PlayerBulletImage = { ImageAssetName = "PlayerBullet"; PackageName = StagePackageName; PackageFileName = AssetGraphFileName }
    let EnemyImage = { ImageAssetName = "Enemy"; PackageName = StagePackageName; PackageFileName = AssetGraphFileName }
    let PlayerImage = { ImageAssetName = "Player"; PackageName = StagePackageName; PackageFileName = AssetGraphFileName }

    // transition constants
    let IncomingTimeSplash = 60L
    let IncomingTime = 20L
    let IdlingTime = 60L
    let OutgoingTimeSplash = 40L
    let OutgoingTime = 30L
    let StageOutgoingTime = 90L

    // splash constants
    let SplashAddress = addr "Splash"

    // title constants
    let TitleAddress = addr "Title"
    let TitleGroupFileName = "Assets/BlazeVector/Groups/Title.nugroup"
    let TitleGroupAddress = addr "Title/Group"
    let SelectTitleEventName = addr "Select/Title"
    let ClickTitlePlayEventName = addr "Click/Title/Group/Play"
    let ClickTitleCreditsEventName = addr "Click/Title/Group/Credits"
    let ClickTitleExitEventName = addr "Click/Title/Group/Exit"

    // stage constants
    let StageAddress = addr "Stage"
    let StageGroupFileName = "Assets/BlazeVector/Groups/StageGui.nugroup"
    let StageGroupAddress = addr "Stage/Group"
    let ClickStageBackEventName = addr "Click/Stage/Group/Back"

    // credits constants
    let CreditsAddress = addr "Credits"
    let CreditsGroupFileName = "Assets/BlazeVector/Groups/Credits.nugroup"
    let CreditsGroupAddress = addr "Credits/Group"
    let ClickCreditsBackEventName = addr "Click/Credits/Group/Back"