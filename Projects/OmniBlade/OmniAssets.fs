namespace OmniBlade
open Nu
module Assets =

    // package names
    let GuiPackageName = "Gui"
    let FieldPackageName = "Field"
    let BattlePackageName = "Battle"
    let GameplayPackageName = "Gameplay"

    // gui assets
    let TitleSong = asset<Song> GuiPackageName "Title"
    let AffirmSound = asset<Sound> GuiPackageName "Affirm"
    let DialogThin = asset<Image> GuiPackageName "DialogThin"
    let DialogMedium = asset<Image> GuiPackageName "DialogMedium"
    let DialogLarge = asset<Image> GuiPackageName "DialogLarge"

    // gameplay assets
    let FinnAnimationSheet = asset<Image> GameplayPackageName "Finn"
    let GlennAnimationSheet = asset<Image> GameplayPackageName "Glenn"
    let BlueGoblinAnimationSheet = asset<Image> GameplayPackageName "BlueGoblin"
    let HitSound = asset<Sound> GameplayPackageName "Hit"
    let ExplosionSound = asset<Sound> GameplayPackageName "Explosion"
    let HealSound = asset<Sound> GameplayPackageName "Heal"
    let DeathSound = asset<Sound> GameplayPackageName "Death"
    let OpenChestSound = asset<Sound> GameplayPackageName "Unlatch"

    // field assets
    let DebugRoomTileMap = asset<TileMap> FieldPackageName "DebugRoom"
    let WoodenChestImageOpened = asset<Image> FieldPackageName "WoodenChestOpened"
    let WoodenChestImageClosed = asset<Image> FieldPackageName "WoodenChestClosed"
    let BrassChestImageOpened = asset<Image> FieldPackageName "BrassChestOpened"
    let BrassChestImageClosed = asset<Image> FieldPackageName "BrassChestClosed"

    // battle assets
    let BattleSong = asset<Song> BattlePackageName "Battle"
    let CancelImage = asset<Image> BattlePackageName "Cancel"
    let BoltAnimationSheet = asset<Image> BattlePackageName "Bolt"
    let ExplosionAnimationSheet = asset<Image> BattlePackageName "Explosion"

    // layer file paths
    let TitleLayerFilePath = "Assets/Gui/Title.nulyr"
    let CreditsLayerFilePath = "Assets/Gui/Credits.nulyr"

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