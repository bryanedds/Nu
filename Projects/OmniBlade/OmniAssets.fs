namespace OmniBlade
open Nu
module Assets =

    // package names
    let GuiPackageName = "Gui"
    let FieldPackageName = "Field"
    let BattlePackageName = "Battle"
    let GameplayPackageName = "Gameplay"

    // gui assets
    let NuSplashSound = asset<Audio> GuiPackageName "Nu"
    let TitleSong = asset<Audio> GuiPackageName "Title"
    let AffirmSound = asset<Audio> GuiPackageName "Affirm"

    // gameplay assets
    let FinnAnimationSheet = asset<Image> GameplayPackageName "Finn"
    let GlennAnimationSheet = asset<Image> GameplayPackageName "Glenn"
    let BlueGoblinAnimationSheet = asset<Image> GameplayPackageName "BlueGoblin"

    // field assets
    let DebugRoomTileMap = asset<TileMap> FieldPackageName "DebugRoom"

    // battle assets
    let BattleSong = asset<Audio> BattlePackageName "Battle"
    let HitSound = asset<Audio> BattlePackageName "Hit"
    let ExplosionSound = asset<Audio> BattlePackageName "Explosion"
    let HealSound = asset<Audio> BattlePackageName "Heal"
    let DeathSound = asset<Audio> BattlePackageName "Death"
    let CancelImage = asset<Image> BattlePackageName "Cancel"
    let BoltAnimationSheet = asset<Image> BattlePackageName "Bolt"
    let ExplosionAnimationSheet = asset<Image> BattlePackageName "Explosion"

    // layer file paths
    let TitleLayerFilePath = "Assets/Gui/Title.nulyr"
    let CreditsLayerFilePath = "Assets/Gui/Credits.nulyr"
    let FieldHudLayerFilePath = "Assets/Field/Hud.nulyr"
    let BattleSceneLayerFilePath = "Assets/Battle/Scene.nulyr"

    // data file paths
    let WeaponDataFilePath = "Assets/Gameplay/WeaponData.csv"
    let ArmorDataFilePath = "Assets/Gameplay/ArmorData.csv"
    let AccessoryDataFilePath = "Assets/Gameplay/AccessoryData.csv"
    let ConsumableDataFilePath = "Assets/Gameplay/ConsumableData.csv"
    let TechDataFilePath = "Assets/Gameplay/TechData.csv"
    let ArchetypeDataFilePath = "Assets/Gameplay/ArchetypeData.csv"
    let CharacterDataFilePath = "Assets/Gameplay/CharacterData.csv"
    let FieldDataFilePath = "Assets/Gameplay/FieldData.csv"
    let BattleDataFilePath = "Assets/Gameplay/BattleData.csv"
    let TechAnimationDataFilePath = "Assets/Gameplay/TechAnimationData.csv"
    let CharacterAnimationDataFilePath = "Assets/Gameplay/CharacterAnimationData.csv"