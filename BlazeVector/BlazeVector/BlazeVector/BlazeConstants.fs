namespace BlazeVector
open Nu

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module FilePaths =

        // these constants specify the file paths from which various simulants are loaded
        let TitleGroup = "Assets/Gui/Title.nugroup"
        let CreditsGroup = "Assets/Gui/Credits.nugroup"
        let GameplayGroup = "Assets/Gui/Gameplay.nugroup"
        let PlayerGroup = "Assets/Gameplay/Player.nugroup"
        let Section0 = "Assets/Gameplay/Section0.nugroup"
        let Section1 = "Assets/Gameplay/Section1.nugroup"
        let Section2 = "Assets/Gameplay/Section2.nugroup"
        let Section3 = "Assets/Gameplay/Section3.nugroup"
        let Sections = [Section0; Section1; Section2; Section3]

    [<RequireQualifiedAccess>]
    module Assets =

        // these constants specify the packages as named in the project's 'AssetGraph.nuag' file
        let GuiPackageName = "Gui"
        let GameplayPackageName = "Gameplay"

        // these constants locate various assets described by the project's 'AssetGraph.nuag' file
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

    [<RequireQualifiedAccess>]
    module BlazeVector =

        // this constant describes the 'dissolving' transition behavior of game's screens
        let DissolveData =
            { IncomingTime = 20L
              OutgoingTime = 30L
              DissolveImage = { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image8" }}
    
        // this constant describes the 'splashing' behavior of game's splash screen
        let SplashData =
            { DissolveData = DissolveData
              IdlingTime = 60L
              SplashImage = { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image5" }}

        // and finally, this constant simply specifies how many sections are added to a game
        let SectionCount = 32