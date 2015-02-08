namespace BlazeVector
open Nu
open Nu.Constants
open Nu.WorldConstants
module BlazeConstants =

    // these constants specify the packages as named in the project's 'AssetGraph.xml' file
    let GuiPackageName = "Gui"
    let GameplayPackageName = "Gameplay"

    // these constants specify the file paths from which various simulants are loaded
    let TitleGroupFilePath = "Assets/Gui/Title.nugroup"
    let CreditsGroupFilePath = "Assets/Gui/Credits.nugroup"
    let HudGroupFilePath = "Assets/Gui/Hud.nugroup"
    let PlayerGroupFilePath = "Assets/Gameplay/Player.nugroup"
    let Section0FilePath = "Assets/Gameplay/Section0.nugroup"
    let Section1FilePath = "Assets/Gameplay/Section1.nugroup"
    let Section2FilePath = "Assets/Gameplay/Section2.nugroup"
    let Section3FilePath = "Assets/Gameplay/Section3.nugroup"
    let SectionFilePaths = [Section0FilePath; Section1FilePath; Section2FilePath; Section3FilePath]

    // these constants locate various assets described by the project's 'AssetGraph.xml' file
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

    // this constant describes the 'dissolving' transition behavior of game's screens
    let DissolveData =
        { IncomingTime = 20L
          OutgoingTime = 30L
          DissolveImage = DefaultDissolveImage }

    // these pair of constants are -
    //  a) a string used to give a name to the splash screen
    //  b) a proxy used to locate and operate upon the splash screen
    // A screen proxy is created by first converting a name to an address with the ntoa function, and
    // then passing the address into the Screen.proxy function.
    let SplashName = "Splash"
    let Splash = Screen.proxy <| ntoa SplashName

    // this constant describes the 'splashing' behavior of game's splash screen
    let SplashData =
        { DissolveData = DissolveData
          IdlingTime = 60L
          SplashImage = { PackageName = DefaultPackageName; AssetName = "Image5" }}

    // these are like the pair of constants for splash screen above, but for the title screen
    let TitleName = "Title"
    let Title = Screen.proxy <| ntoa TitleName
    
    // these are also like the pair of constants above, but for the group that is loaded into the
    // title that contains all of its gui entities. You'll notice that the group is proxied from a
    // combination of the address of its containing screen as well as its own personal name as
    // found in its originating document, 'Assets/Gui/Title.nugroup'.
    //
    // You'll need to familiarize yourself with the 'satoga' operator and its relatives by reading
    // their respective documentation comments.
    let TitleGroupName = "Group"
    let TitleGroup = Group.proxy <| satoga Title.ScreenAddress TitleGroupName

    // these are like the above, but for the play button found in the above group
    let TitlePlayName = "Play"
    let TitlePlay = Entity.proxy <| gatoea TitleGroup.GroupAddress TitlePlayName
    
    // and so on...
    let TitleCreditsName = "Credits"
    let TitleCredits = Entity.proxy <| gatoea TitleGroup.GroupAddress TitleCreditsName
    
    // and so on.
    let TitleExitName = "Exit"
    let TitleExit = Entity.proxy <| gatoea TitleGroup.GroupAddress TitleExitName

    // these constants specify names and proxies for various simulants of the gameplay screen
    let GameplayName = "Gameplay"
    let Gameplay = Screen.proxy <| ntoa GameplayName
    let GameplayGroupName = "Group"
    let GameplayGroup = Group.proxy <| satoga Gameplay.ScreenAddress GameplayGroupName
    let GameplayBackName = "Back"
    let GameplayBack = Entity.proxy <| gatoea GameplayGroup.GroupAddress GameplayBackName
    let PlayerGroupName = "Player"
    let PlayerGroup = Group.proxy <| satoga Gameplay.ScreenAddress PlayerGroupName
    let PlayerName = "Player"
    let Player = Entity.proxy <| gatoea PlayerGroup.GroupAddress PlayerName

    // these constants specify names and proxies for various simulants of the credits screen
    let CreditsName = "Credits"
    let Credits = Screen.proxy <| ntoa CreditsName
    let CreditsGroupName = "Group"
    let CreditsGroup = Group.proxy <| satoga Credits.ScreenAddress CreditsGroupName
    let CreditsBackName = "Back"
    let CreditsBack = Entity.proxy <| gatoea CreditsGroup.GroupAddress CreditsBackName

    // and finally, this constant simply specifies how many sections are added to a game
    let SectionCount = 32