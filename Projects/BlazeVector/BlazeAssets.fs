namespace BlazeVector
open Nu

[<RequireQualifiedAccess>]
module Assets =

    // the packages as named in the project's 'AssetGraph.nuag' file
    let GuiPackageName = "Gui"
    let GameplayPackageName = "Gameplay"

    // the various assets described by the project's 'AssetGraph.nuag' file
    let NuSplashSound = asset<Audio> GuiPackageName "Nu"
    let MachinerySong = asset<Audio> GuiPackageName "Machinery"
    let DeadBlazeSong = asset<Audio> GameplayPackageName "DeadBlaze"
    let HitSound = asset<Audio> GameplayPackageName "Hit"
    let ExplosionSound = asset<Audio> GameplayPackageName "Explosion"
    let ShotSound = asset<Audio> GameplayPackageName "Shot"
    let JumpSound = asset<Audio> GameplayPackageName "Jump"
    let DeathSound = asset<Audio> GameplayPackageName "Death"
    let EnemyBulletImage = asset<Image> GameplayPackageName "EnemyBullet"
    let PlayerBulletImage = asset<Image> GameplayPackageName "PlayerBullet"
    let EnemyImage = asset<Image> GameplayPackageName "Enemy"
    let PlayerImage = asset<Image> GameplayPackageName "Player"

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