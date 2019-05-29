namespace BlazeVector
open Nu
module Assets =

    // the packages as named in the project's 'AssetGraph.nuag' file
    let GuiPackage = "Gui"
    let GameplayPackage = "Gameplay"

    // the various assets described by the project's 'AssetGraph.nuag' file
    let NuSplashSound = AssetTag.make<Audio> GuiPackage "Nu"
    let MachinerySong = AssetTag.make<Audio> GuiPackage "Machinery"
    let DeadBlazeSong = AssetTag.make<Audio> GameplayPackage "DeadBlaze"
    let HitSound = AssetTag.make<Audio> GameplayPackage "Hit"
    let ExplosionSound = AssetTag.make<Audio> GameplayPackage "Explosion"
    let ShotSound = AssetTag.make<Audio> GameplayPackage "Shot"
    let JumpSound = AssetTag.make<Audio> GameplayPackage "Jump"
    let DeathSound = AssetTag.make<Audio> GameplayPackage "Death"
    let EnemyBulletImage = AssetTag.make<Image> GameplayPackage "EnemyBullet"
    let PlayerBulletImage = AssetTag.make<Image> GameplayPackage "PlayerBullet"
    let EnemyImage = AssetTag.make<Image> GameplayPackage "Enemy"
    let PlayerImage = AssetTag.make<Image> GameplayPackage "Player"

    // the file paths from which various simulants are loaded
    let TitleLayerFilePath = "Assets/Gui/Title.nulyr"
    let CreditsLayerFilePath = "Assets/Gui/Credits.nulyr"
    let GameplayLayerFilePath = "Assets/Gui/Gameplay.nulyr"
    let SceneLayerFilePath = "Assets/Gameplay/Scene.nulyr"
    let Section0FilePath = "Assets/Gameplay/Section0.nulyr"
    let Section1FilePath = "Assets/Gameplay/Section1.nulyr"
    let Section2FilePath = "Assets/Gameplay/Section2.nulyr"
    let Section3FilePath = "Assets/Gameplay/Section3.nulyr"
    let SectionFilePaths = [Section0FilePath; Section1FilePath; Section2FilePath; Section3FilePath]