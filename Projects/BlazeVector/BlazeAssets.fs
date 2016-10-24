namespace BlazeVector
open Nu
module Assets =

    // the packages as named in the project's 'AssetGraph.nuag' file
    let GuiPackageName = "Gui"
    let GameplayPackageName = "Gameplay"

    // the various assets described by the project's 'AssetGraph.nuag' file
    let NuSplashSound = { PackageName = GuiPackageName; AssetName = "Nu" }
    let MachinerySong = { PackageName = GuiPackageName; AssetName = "Machinery" }
    let DeadBlazeSong = { PackageName = GameplayPackageName; AssetName = "DeadBlaze" }
    let HitSound = { PackageName = GameplayPackageName; AssetName = "Hit" }
    let ExplosionSound = { PackageName = GameplayPackageName; AssetName = "Explosion" }
    let ShotSound = { PackageName = GameplayPackageName; AssetName = "Shot" }
    let JumpSound = { PackageName = GameplayPackageName; AssetName = "Jump" }
    let DeathSound = { PackageName = GameplayPackageName; AssetName = "Death" }
    let EnemyBulletImage = { PackageName = GameplayPackageName; AssetName = "EnemyBullet" }
    let PlayerBulletImage = { PackageName = GameplayPackageName; AssetName = "PlayerBullet" }
    let EnemyImage = { PackageName = GameplayPackageName; AssetName = "Enemy" }
    let PlayerImage = { PackageName = GameplayPackageName; AssetName = "Player" }

    // the file paths from which various simulants are loaded
    let TitleGroupFilePath = "Assets/Gui/Title.nugroup"
    let CreditsGroupFilePath = "Assets/Gui/Credits.nugroup"
    let GameplayGroupFilePath = "Assets/Gui/Gameplay.nugroup"
    let PlayerGroupFilePath = "Assets/Gameplay/Player.nugroup"
    let Section0FilePath = "Assets/Gameplay/Section0.nugroup"
    let Section1FilePath = "Assets/Gameplay/Section1.nugroup"
    let Section2FilePath = "Assets/Gameplay/Section2.nugroup"
    let Section3FilePath = "Assets/Gameplay/Section3.nugroup"
    let SectionFilePaths = [Section0FilePath; Section1FilePath; Section2FilePath; Section3FilePath]