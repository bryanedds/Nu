namespace OmniBlade
open Nu
module Assets =

    // the packages as named in the project's 'AssetGraph.nuag' file
    let GuiPackageName = "Gui"
    let FieldPackageName = "Field"
    let BattlePackageName = "Battle"

    // the various assets described by the project's 'AssetGraph.nuag' file
    let NuSplashSound = asset<Audio> GuiPackageName "Nu"
    let TitleSong = asset<Audio> GuiPackageName "Title"
    let AffirmSound = asset<Audio> GuiPackageName "Affirm"
    let DebugRoomTileMap = asset<TileMap> FieldPackageName "DebugRoom"
    let BattleSong = asset<Audio> BattlePackageName "Battle"
    let HitSound = asset<Audio> BattlePackageName "Hit"
    let ExplosionSound = asset<Audio> BattlePackageName "Explosion"
    let HealSound = asset<Audio> BattlePackageName "Heal"
    let DeathSound = asset<Audio> BattlePackageName "Death"
    let CancelImage = asset<Image> BattlePackageName "Cancel"
    let BoltAnimationSheet = asset<Image> BattlePackageName "Bolt"
    let ExplosionAnimationSheet = asset<Image> BattlePackageName "Explosion"
    let JinnAnimationSheet = asset<Image> BattlePackageName "Jinn"
    let GlennAnimationSheet = asset<Image> BattlePackageName "Glenn"
    let GoblinAnimationSheet = asset<Image> BattlePackageName "Goblin"

    // the file paths from which various simulants are loaded
    let TitleLayerFilePath = "Assets/Gui/Title.nulyr"
    let CreditsLayerFilePath = "Assets/Gui/Credits.nulyr"
    let BattleHudLayerFilePath = "Assets/Battle/Hud.nulyr"
    let BattleSceneLayerFilePath = "Assets/Battle/Scene.nulyr"

    // Rom files.
    let WeaponDataFilePath = "Assets/Battle/WeaponData.csv"
    let ArmorDataFilePath = "Assets/Battle/ArmorData.csv"
    let AccessoryDataFilePath = "Assets/Battle/AccessoryData.csv"
    let ConsumableDataFilePath = "Assets/Battle/ConsumableData.csv"
    let TechDataFilePath = "Assets/Battle/TechData.csv"
    let TechAnimationDataFilePath = "Assets/Battle/TechAnimationData.csv"
    let CharacterDataFilePath = "Assets/Battle/CharacterData.csv"
    let CharacterAnimationDataFilePath = "Assets/Battle/CharacterAnimationData.csv"