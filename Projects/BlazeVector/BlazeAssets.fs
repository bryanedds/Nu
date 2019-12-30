namespace BlazeVector
open Nu
module Assets =

    // the packages as named in the project's 'AssetGraph.nuag' file
    let GuiPackage = "Gui"
    let GameplayPackage = "Gameplay"

    // the various assets described by the project's 'AssetGraph.nuag' file
    let NuSplashSound = asset<Audio> GuiPackage "Nu"
    let MachinerySong = asset<Audio> GuiPackage "Machinery"
    let DeadBlazeSong = asset<Audio> GameplayPackage "DeadBlaze"
    let HitSound = asset<Audio> GameplayPackage "Hit"
    let ExplosionSound = asset<Audio> GameplayPackage "Explosion"
    let ShotSound = asset<Audio> GameplayPackage "Shot"
    let JumpSound = asset<Audio> GameplayPackage "Jump"
    let DeathSound = asset<Audio> GameplayPackage "Death"
    let EnemyBulletImage = asset<Image> GameplayPackage "EnemyBullet"
    let PlayerBulletImage = asset<Image> GameplayPackage "PlayerBullet"
    let EnemyImage = asset<Image> GameplayPackage "Enemy"
    let PlayerImage = asset<Image> GameplayPackage "Player"

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