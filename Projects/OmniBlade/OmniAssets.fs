namespace OmniBlade
open Nu
module Assets =

    // package names
    let GuiPackageName = "Gui"
    let FieldPackageName = "Field"
    let BattlePackageName = "Battle"

    // gui assets
    let Font = asset<Font> GuiPackageName "Font"
    let DialogThin = asset<Image> GuiPackageName "DialogThin"
    let DialogMedium = asset<Image> GuiPackageName "DialogMedium"
    let DialogLarge = asset<Image> GuiPackageName "DialogLarge"
    let AffirmSound = asset<Sound> GuiPackageName "Affirm"
    let TitleSong = { Volume = Constants.Audio.DefaultSongVolume; FadeOutMs = Constants.Audio.DefaultFadeOutMs; Song = asset<Song> GuiPackageName "Title" }

    // field assets
    let DebugRoomTileMap = asset<TileMap> FieldPackageName "DebugRoom"
    let WoodenChestOpenedImage = asset<Image> FieldPackageName "WoodenChestOpened"
    let WoodenChestClosedImage = asset<Image> FieldPackageName "WoodenChestClosed"
    let BrassChestOpenedImage = asset<Image> FieldPackageName "BrassChestOpened"
    let BrassChestClosedImage = asset<Image> FieldPackageName "BrassChestClosed"
    let WoodenDoorOpenedImage = asset<Image> FieldPackageName "WoodenDoorOpened"
    let WoodenDoorClosedImage = asset<Image> FieldPackageName "WoodenDoorClosed"
    let EmptyImage = asset<Image> FieldPackageName "Empty"
    let NpcAnimationSheet = asset<Image> FieldPackageName "Npcs"
    let FinnAnimationSheet = asset<Image> FieldPackageName "Finn"
    let GlennAnimationSheet = asset<Image> FieldPackageName "Glenn"
    let GoblinAnimationSheet = asset<Image> FieldPackageName "Goblin"
    let HitSound = asset<Sound> FieldPackageName "Hit"
    let ExplosionSound = asset<Sound> FieldPackageName "Explosion"
    let EnterBattleSound = asset<Sound> FieldPackageName "EnterBattle"
    let HealSound = asset<Sound> FieldPackageName "Heal"
    let DeathSound = asset<Sound> FieldPackageName "Death"
    let OpenChestSound = asset<Sound> FieldPackageName "Unlatch"
    let StairStepsSound = asset<Sound> FieldPackageName "StairSteps"
    let FieldSong = { Volume = Constants.Audio.DefaultSongVolume; FadeOutMs = Constants.Audio.DefaultFadeOutMs; Song = asset<Song> FieldPackageName "Field" }

    // battle assets
    let CancelImage = asset<Image> BattlePackageName "Cancel"
    let BoltAnimationSheet = asset<Image> BattlePackageName "Bolt"
    let ExplosionAnimationSheet = asset<Image> BattlePackageName "Explosion"
    let BattleSong = { Volume = Constants.Audio.DefaultSongVolume; FadeOutMs = Constants.Audio.DefaultFadeOutMs; Song = asset<Song> BattlePackageName "Battle" }

    // layer file paths
    let TitleLayerFilePath = "Assets/Gui/Title.nulyr"
    let CreditsLayerFilePath = "Assets/Gui/Credits.nulyr"

    // data file paths
    let WeaponDataFilePath = "Assets/Field/WeaponData.csv"
    let ArmorDataFilePath = "Assets/Field/ArmorData.csv"
    let AccessoryDataFilePath = "Assets/Field/AccessoryData.csv"
    let ConsumableDataFilePath = "Assets/Field/ConsumableData.csv"
    let TechDataFilePath = "Assets/Field/TechData.csv"
    let ArchetypeDataFilePath = "Assets/Field/ArchetypeData.csv"
    let CharacterDataFilePath = "Assets/Field/CharacterData.csv"
    let FieldDataFilePath = "Assets/Field/FieldData.csv"
    let BattleDataFilePath = "Assets/Field/BattleData.csv"
    let TechAnimationDataFilePath = "Assets/Field/TechAnimationData.csv"
    let CharacterAnimationDataFilePath = "Assets/Field/CharacterAnimationData.csv"