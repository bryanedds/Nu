namespace BlazeVector
open Nu

[<RequireQualifiedAccess>]
module Assets =

    [<RequireQualifiedAccess>]
    module Gui =

        let PackageName = "Gui"
        let SplashSound = asset<Sound> PackageName "Splash"
        let MachinerySong = { Volume = Constants.Audio.SongVolumeDefault; FadeOutMs = Constants.Audio.FadeOutMsDefault; Song = asset<Song> PackageName "Machinery" }
        let TitleGroupFilePath = "Assets/Gui/Title.nugroup"
        let CreditsGroupFilePath = "Assets/Gui/Credits.nugroup"
        let GameplayGroupFilePath = "Assets/Gui/Gameplay.nugroup"

    [<RequireQualifiedAccess>]
    module Gameplay =
        
        let PackageName = "Gameplay"
        let EnemyBulletImage = asset<Image> PackageName "EnemyBullet"
        let PlayerBulletImage = asset<Image> PackageName "PlayerBullet"
        let EnemyImage = asset<Image> PackageName "Enemy"
        let PlayerImage = asset<Image> PackageName "Player"
        let HitSound = asset<Sound> PackageName "Hit"
        let ExplosionSound = asset<Sound> PackageName "Explosion"
        let ShotSound = asset<Sound> PackageName "Shot"
        let JumpSound = asset<Sound> PackageName "Jump"
        let DeathSound = asset<Sound> PackageName "Death"
        let DeadBlazeSong = { Volume = Constants.Audio.SongVolumeDefault * 2.0f; FadeOutMs = Constants.Audio.FadeOutMsDefault; Song = asset<Song> PackageName "DeadBlaze" }
        let SceneGroupFilePath = "Assets/Gameplay/Scene.nugroup"
        let Section0FilePath = "Assets/Gameplay/Section0.nugroup"
        let Section1FilePath = "Assets/Gameplay/Section1.nugroup"
        let Section2FilePath = "Assets/Gameplay/Section2.nugroup"
        let Section3FilePath = "Assets/Gameplay/Section3.nugroup"
        let SectionFilePaths = [Section0FilePath; Section1FilePath; Section2FilePath; Section3FilePath]