namespace BlazeVector
open Nu
module Assets =

    // the packages as named in the project's 'AssetGraph.nuag' file
    let GuiPackageName = "Gui"
    let GameplayPackageName = "Gameplay"

    // the various assets described by the project's 'AssetGraph.nuag' file
    let NuSplashSound = AssetTag.make<Audio> GuiPackageName "Nu"
    let MachinerySong = AssetTag.make<Audio> GuiPackageName "Machinery"
    let DeadBlazeSong = AssetTag.make<Audio> GameplayPackageName "DeadBlaze"
    let HitSound = AssetTag.make<Audio> GameplayPackageName "Hit"
    let ExplosionSound = AssetTag.make<Audio> GameplayPackageName "Explosion"
    let ShotSound = AssetTag.make<Audio> GameplayPackageName "Shot"
    let JumpSound = AssetTag.make<Audio> GameplayPackageName "Jump"
    let DeathSound = AssetTag.make<Audio> GameplayPackageName "Death"
    let EnemyBulletImage = AssetTag.make<Image> GameplayPackageName "EnemyBullet"
    let PlayerBulletImage = AssetTag.make<Image> GameplayPackageName "PlayerBullet"
    let EnemyImage = AssetTag.make<Image> GameplayPackageName "Enemy"
    let PlayerImage = AssetTag.make<Image> GameplayPackageName "Player"

    // the file paths from which various simulants are loaded
    let TitleLayerFilePath = "Assets/Gui/Title.nulyr"
    let CreditsLayerFilePath = "Assets/Gui/Credits.nulyr"
    let GameplayLayerFilePath = "Assets/Gui/Gameplay.nulyr"
    let PlayerLayerFilePath = "Assets/Gameplay/Player.nulyr"
    let Section0FilePath = "Assets/Gameplay/Section0.nulyr"
    let Section1FilePath = "Assets/Gameplay/Section1.nulyr"
    let Section2FilePath = "Assets/Gameplay/Section2.nulyr"
    let Section3FilePath = "Assets/Gameplay/Section3.nulyr"
    let SectionFilePaths = [Section0FilePath; Section1FilePath; Section2FilePath; Section3FilePath]