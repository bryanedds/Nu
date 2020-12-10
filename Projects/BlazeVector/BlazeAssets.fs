namespace BlazeVector
open Nu

[<RequireQualifiedAccess>]
module Assets =

    [<RequireQualifiedAccess>]
    module Gui =

        let PackageName = "Gui"
        let SplashSound = asset<Sound> PackageName "Splash"
        let MachinerySong = { Volume = Constants.Audio.SongVolumeDefault; FadeOutMs = Constants.Audio.FadeOutMsDefault; Song = asset<Song> PackageName "Machinery" }
        let TitleLayerFilePath = "Assets/Gui/Title.nulyr"
        let CreditsLayerFilePath = "Assets/Gui/Credits.nulyr"
        let GameplayLayerFilePath = "Assets/Gui/Gameplay.nulyr" // TODO: move this to gameplay?

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
        let SceneLayerFilePath = "Assets/Gameplay/Scene.nulyr"
        let Section0FilePath = "Assets/Gameplay/Section0.nulyr"
        let Section1FilePath = "Assets/Gameplay/Section1.nulyr"
        let Section2FilePath = "Assets/Gameplay/Section2.nulyr"
        let Section3FilePath = "Assets/Gameplay/Section3.nulyr"
        let SectionFilePaths = [Section0FilePath; Section1FilePath; Section2FilePath; Section3FilePath]