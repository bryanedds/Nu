namespace OmniBlade
open Nu
module Assets =

    // the packages as named in the project's 'AssetGraph.nuag' file
    let GuiPackage = "Gui"
    let BattlePackage = "Battle"

    // the various assets described by the project's 'AssetGraph.nuag' file
    let NuSplashSound = asset<Audio> GuiPackage "Nu"
    let TitleSong = asset<Audio> GuiPackage "Title"
    let AffirmSound = asset<Audio> GuiPackage "Affirm"
    let BattleSong = asset<Audio> BattlePackage "Battle"
    let HitSound = asset<Audio> BattlePackage "Hit"
    let HealSound = asset<Audio> BattlePackage "Heal"
    let DeathSound = asset<Audio> BattlePackage "Death"
    let JinnAnimationSheet = asset<Image> BattlePackage "Jinn"
    let GlennAnimationSheet = asset<Image> BattlePackage "Glenn"
    let GoblinAnimationSheet = asset<Image> BattlePackage "Goblin"

    // the file paths from which various simulants are loaded
    let TitleLayerFilePath = "Assets/Gui/Title.nulyr"
    let CreditsLayerFilePath = "Assets/Gui/Credits.nulyr"
    let BattleHudLayerFilePath = "Assets/Battle/Hud.nulyr"
    let BattleSceneLayerFilePath = "Assets/Battle/Scene.nulyr"

    // Rom files.
    let WeaponDataFilePath = "Assets/Battle/WeaponData.csv"
    let ArmorDataFilePath = "Assets/Battle/ArmorData.csv"
    let RelicDataFilePath = "Assets/Battle/RelicData.csv"