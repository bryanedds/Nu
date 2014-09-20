namespace BlazeVector
open Nu
open Nu.Constants
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
    let NuSplashSound = { SoundAssetName = "Nu"; PackageName = GuiPackageName }
    let MachinerySong = { SongAssetName = "Machinery"; PackageName = GuiPackageName }
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
    let TitleGroupFileName = "Assets/BlazeVector/Groups/Title.nugroup"
    let TitleGroupAddress = !* "Title/Group"
    let SelectTitleEventName = !* "Select/Title"
    let ClickTitlePlayEventName = !* "Click/Title/Group/Play"
    let ClickTitleCreditsEventName = !* "Click/Title/Group/Credits"
    let ClickTitleExitEventName = !* "Click/Title/Group/Exit"

    // stage constants
    let StageAddress = !* "Stage"
    let StageGroupFileName = "Assets/BlazeVector/Groups/StageGui.nugroup"
    let StageGroupAddress = !* "Stage/Group"
    let ClickStageBackEventName = !* "Click/Stage/Group/Back"

    // credits constants
    let CreditsAddress = !* "Credits"
    let CreditsGroupFileName = "Assets/BlazeVector/Groups/Credits.nugroup"
    let CreditsGroupAddress = !* "Credits/Group"
    let ClickCreditsBackEventName = !* "Click/Credits/Group/Back"