// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open Nu
module Assets =

    // package names
    let GuiPackageName = "Gui"
    let FieldPackageName = "Field"
    let BattlePackageName = "Battle"

    // gui assets
    let Font = asset<Font> GuiPackageName "Font"
    let DialogThinImage = asset<Image> GuiPackageName "DialogThin"
    let DialogThickImage = asset<Image> GuiPackageName "DialogThick"
    let DialogXLImage = asset<Image> GuiPackageName "DialogXL"
    let DialogXXLImage = asset<Image> GuiPackageName "DialogXXL"
    let ButtonUpImage = asset<Image> GuiPackageName "ButtonUp"
    let ButtonDownImage = asset<Image> GuiPackageName "ButtonDown"
    let ButtonShortUpImage = asset<Image> GuiPackageName "ButtonShortUp"
    let ButtonShortDownImage = asset<Image> GuiPackageName "ButtonShortDown"
    let AffirmSound = asset<Sound> GuiPackageName "Affirm"
    let MistakeSound = asset<Sound> GuiPackageName "Mistake"
    let WindSong = { Volume = Constants.Audio.DefaultSongVolume; FadeOutMs = Constants.Audio.DefaultFadeOutMs; Song = asset<Song> GuiPackageName "Wind" }
    let TitleSong = { Volume = Constants.Audio.DefaultSongVolume; FadeOutMs = Constants.Audio.DefaultFadeOutMs; Song = asset<Song> GuiPackageName "Title" }

    // field assets
    let DebugRoomTileMap = asset<TileMap> FieldPackageName "DebugRoom"
    let WoodenChestOpenedImage = asset<Image> FieldPackageName "WoodenChestOpened"
    let WoodenChestClosedImage = asset<Image> FieldPackageName "WoodenChestClosed"
    let BrassChestOpenedImage = asset<Image> FieldPackageName "BrassChestOpened"
    let BrassChestClosedImage = asset<Image> FieldPackageName "BrassChestClosed"
    let WoodenDoorOpenedImage = asset<Image> FieldPackageName "WoodenDoorOpened"
    let WoodenDoorClosedImage = asset<Image> FieldPackageName "WoodenDoorClosed"
    let ThrowSwitchOffImage = asset<Image> FieldPackageName "ThrowSwitchOff"
    let ThrowSwitchOnImage = asset<Image> FieldPackageName "ThrowSwitchOn"
    let StepPlateImage = asset<Image> FieldPackageName "StepPlate"
    let SavePointImage = asset<Image> FieldPackageName "SavePoint"
    let EmptyImage = asset<Image> FieldPackageName "Empty"
    let NpcAnimationSheet = asset<Image> FieldPackageName "Npcs"
    let ShopkeepAnimationSheet = asset<Image> FieldPackageName "Shopkeep"
    let FinnAnimationSheet = asset<Image> FieldPackageName "Finn"
    let FinnMugImage = asset<Image> FieldPackageName "FinnMug"
    let GlennAnimationSheet = asset<Image> FieldPackageName "Glenn"
    let GlennMugImage = asset<Image> FieldPackageName "GlennMug"
    let GoblinAnimationSheet = asset<Image> FieldPackageName "Goblin"
    let UnsheatheSound = asset<Sound> FieldPackageName "Unsheathe"
    let GrowthSound = asset<Sound> FieldPackageName "Growth"
    let BeastScreamSound = asset<Sound> FieldPackageName "BeastScream"
    let HitSound = asset<Sound> FieldPackageName "Hit"
    let SlashSound = asset<Sound> FieldPackageName "Slash"
    let ExplosionSound = asset<Sound> FieldPackageName "Explosion"
    let EnterBattleSound = asset<Sound> FieldPackageName "EnterBattle"
    let HealSound = asset<Sound> FieldPackageName "Heal"
    let DeathSound = asset<Sound> FieldPackageName "Death"
    let PurchaseSound = asset<Sound> FieldPackageName "Purchase"
    let SaveSound = asset<Sound> FieldPackageName "Save"
    let OpenDoorSound = asset<Sound> FieldPackageName "Unlatch"
    let OpenChestSound = asset<Sound> FieldPackageName "Unlatch2"
    let TriggerSound = asset<Sound> FieldPackageName "Unlatch3"
    let UseSwitchSound = asset<Sound> FieldPackageName "Unlatch4"
    let StairStepsSound = asset<Sound> FieldPackageName "StairSteps"
    let FieldSong = { Volume = Constants.Audio.DefaultSongVolume; FadeOutMs = Constants.Audio.DefaultFadeOutMs; Song = asset<Song> FieldPackageName "Field" }

    // battle assets
    let CancelImage = asset<Image> BattlePackageName "Cancel"
    let BoltAnimationSheet = asset<Image> BattlePackageName "Bolt"
    let CycloneBlurAnimationSheet = asset<Image> BattlePackageName "CycloneBlur"
    let SpikeAnimationSheet = asset<Image> BattlePackageName "Spike"
    let ExplosionAnimationSheet = asset<Image> BattlePackageName "Explosion"
    let ImpactSplashAnimationSheet = asset<Image> BattlePackageName "ImpactSplash"
    let BattleSong = { Volume = Constants.Audio.DefaultSongVolume; FadeOutMs = Constants.Audio.DefaultFadeOutMs; Song = asset<Song> BattlePackageName "Battle" }

    // layer file paths
    let TitleLayerFilePath = "Assets/Gui/Title.nulyr"
    let CreditsLayerFilePath = "Assets/Gui/Credits.nulyr"
    let Intro1LayerFilePath = "Assets/Gui/Intro1.nulyr"
    let Intro2LayerFilePath = "Assets/Gui/Intro2.nulyr"
    let Intro3LayerFilePath = "Assets/Gui/Intro3.nulyr"
    let Intro4LayerFilePath = "Assets/Gui/Intro4.nulyr"
    let Intro5LayerFilePath = "Assets/Gui/Intro5.nulyr"

    // data file paths
    let WeaponDataFilePath = "Assets/Field/WeaponData.csv"
    let ArmorDataFilePath = "Assets/Field/ArmorData.csv"
    let AccessoryDataFilePath = "Assets/Field/AccessoryData.csv"
    let ConsumableDataFilePath = "Assets/Field/ConsumableData.csv"
    let TechDataFilePath = "Assets/Field/TechData.csv"
    let ArchetypeDataFilePath = "Assets/Field/ArchetypeData.csv"
    let CharacterDataFilePath = "Assets/Field/CharacterData.csv"
    let ShopDataFilePath = "Assets/Field/ShopData.csv"
    let FieldDataFilePath = "Assets/Field/FieldData.csv"
    let BattleDataFilePath = "Assets/Field/BattleData.csv"
    let EncounterDataFilePath = "Assets/Field/EncounterData.csv"
    let TechAnimationDataFilePath = "Assets/Field/TechAnimationData.csv"
    let CharacterAnimationDataFilePath = "Assets/Field/CharacterAnimationData.csv"
    let SaveFilePath = "OmniBlade.sav"