namespace OmniBlade
open Nu
module Assets =

    // the packages as named in the project's 'AssetGraph.nuag' file
    let GuiPackage = "Gui"
    let BattlePackage = "Battle"

    // the various assets described by the project's 'AssetGraph.nuag' file
    let NuSplashSound = AssetTag.make<Audio> GuiPackage "Nu"
    let TitleSong = AssetTag.make<Audio> GuiPackage "Guitar in the Sand"
    let BattleSong = AssetTag.make<Audio> BattlePackage "FightingBlue"
    let HitSound = AssetTag.make<Audio> BattlePackage "Hit"
    let ExplosionSound = AssetTag.make<Audio> BattlePackage "Explosion"
    let ShotSound = AssetTag.make<Audio> BattlePackage "Shot"
    let JumpSound = AssetTag.make<Audio> BattlePackage "Jump"
    let DeathSound = AssetTag.make<Audio> BattlePackage "Death"
    let JinnAnimationSheet = AssetTag.make<Image> BattlePackage "Jinn"

    // the file paths from which various simulants are loaded
    let TitleLayerFilePath = "Assets/Gui/Title.nulyr"
    let CreditsLayerFilePath = "Assets/Gui/Credits.nulyr"
    let BattleHudLayerFilePath = "Assets/Battle/Hud.nulyr"
    let BattleSceneLayerFilePath = "Assets/Battle/Scene.nulyr"

    // Rom files.
    let WeaponDataFilePath = "Assets/Battle/WeaponData.csv"
    let ArmorDataFilePath = "Assets/Battle/ArmorData.csv"
    let RelicDataFilePath = "Assets/Battle/RelicData.csv"