// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open Nu

[<RequireQualifiedAccess>]
module Assets =

    [<RequireQualifiedAccess>]
    module Global =

        let SaveFilePath = "OmniBlade.sav"

    [<RequireQualifiedAccess>]
    module Gui =

        let PackageName = "Gui"
        let Font = asset<Font> PackageName "Font"
        let DialogThinImage = asset<Image> PackageName "DialogThin"
        let DialogThickImage = asset<Image> PackageName "DialogThick"
        let DialogXLImage = asset<Image> PackageName "DialogXL"
        let DialogXXLImage = asset<Image> PackageName "DialogXXL"
        let ButtonUpImage = asset<Image> PackageName "ButtonUp"
        let ButtonDownImage = asset<Image> PackageName "ButtonDown"
        let ButtonShortUpImage = asset<Image> PackageName "ButtonShortUp"
        let ButtonShortDownImage = asset<Image> PackageName "ButtonShortDown"
        let AffirmSound = asset<Sound> PackageName "Affirm"
        let MistakeSound = asset<Sound> PackageName "Mistake"
        let WindSong = { Volume = Constants.Audio.SongVolumeDefault; FadeOutMs = Constants.Audio.FadeOutMsDefault; Song = asset<Song> PackageName "Wind" }
        let TitleSong = { Volume = Constants.Audio.SongVolumeDefault; FadeOutMs = Constants.Audio.FadeOutMsDefault; Song = asset<Song> PackageName "Title" }
        let TitleLayerFilePath = "Assets/Gui/Title.nulyr"
        let CreditsLayerFilePath = "Assets/Gui/Credits.nulyr"
        let IntroLayerFilePath = "Assets/Gui/Intro.nulyr"
        let Intro2LayerFilePath = "Assets/Gui/Intro2.nulyr"
        let Intro3LayerFilePath = "Assets/Gui/Intro3.nulyr"
        let Intro4LayerFilePath = "Assets/Gui/Intro4.nulyr"
        let Intro5LayerFilePath = "Assets/Gui/Intro5.nulyr"
        
    [<RequireQualifiedAccess>]
    module Field =

        let PackageName = "Field"
        let DebugRoomTileMap = asset<TileMap> PackageName "DebugRoom"
        let WoodenChestOpenedImage = asset<Image> PackageName "WoodenChestOpened"
        let WoodenChestClosedImage = asset<Image> PackageName "WoodenChestClosed"
        let BrassChestOpenedImage = asset<Image> PackageName "BrassChestOpened"
        let BrassChestClosedImage = asset<Image> PackageName "BrassChestClosed"
        let WoodenDoorOpenedImage = asset<Image> PackageName "WoodenDoorOpened"
        let WoodenDoorClosedImage = asset<Image> PackageName "WoodenDoorClosed"
        let ThrowSwitchOffImage = asset<Image> PackageName "ThrowSwitchOff"
        let ThrowSwitchOnImage = asset<Image> PackageName "ThrowSwitchOn"
        let StepPlateImage = asset<Image> PackageName "StepPlate"
        let SavePointImage = asset<Image> PackageName "SavePoint"
        let NpcAnimationSheet = asset<Image> PackageName "Npcs"
        let ShopkeepAnimationSheet = asset<Image> PackageName "Shopkeep"
        let FinnAnimationSheet = asset<Image> PackageName "Finn"
        let FinnMugImage = asset<Image> PackageName "FinnMug"
        let GlennAnimationSheet = asset<Image> PackageName "Glenn"
        let GlennMugImage = asset<Image> PackageName "GlennMug"
        let GoblinAnimationSheet = asset<Image> PackageName "Goblin"
        let UnsheatheSound = asset<Sound> PackageName "Unsheathe" // TODO: move to Battle
        let GrowthSound = asset<Sound> PackageName "Growth" // TODO: move to Battle
        let BeastScreamSound = asset<Sound> PackageName "BeastScream"
        let HitSound = asset<Sound> PackageName "Hit" // TODO: move to Battle
        let SlashSound = asset<Sound> PackageName "Slash" // TODO: move to Battle
        let ExplosionSound = asset<Sound> PackageName "Explosion" // TODO: move to Battle
        let DeathSound = asset<Sound> PackageName "Death" // TODO: move to Battle
        let EnterBattleSound = asset<Sound> PackageName "EnterBattle"
        let HealSound = asset<Sound> PackageName "Heal"
        let PurchaseSound = asset<Sound> PackageName "Purchase"
        let SaveSound = asset<Sound> PackageName "Save"
        let OpenDoorSound = asset<Sound> PackageName "Unlatch"
        let OpenChestSound = asset<Sound> PackageName "Unlatch2"
        let TriggerSound = asset<Sound> PackageName "Unlatch3"
        let UseSwitchSound = asset<Sound> PackageName "Unlatch4"
        let StairStepsSound = asset<Sound> PackageName "StairSteps"
        let FieldSong = { Volume = Constants.Audio.SongVolumeDefault; FadeOutMs = Constants.Audio.FadeOutMsDefault; Song = asset<Song> PackageName "Field" }
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

    [<RequireQualifiedAccess>]
    module Battle =
        
        let PackageName = "Battle"
        let CancelImage = asset<Image> PackageName "Cancel"
        let BoltAnimationSheet = asset<Image> PackageName "Bolt"
        let CycloneBlurAnimationSheet = asset<Image> PackageName "CycloneBlur"
        let SpikeAnimationSheet = asset<Image> PackageName "Spike"
        let ExplosionAnimationSheet = asset<Image> PackageName "Explosion"
        let ImpactSplashAnimationSheet = asset<Image> PackageName "ImpactSplash"
        let BattleSong = { Volume = Constants.Audio.SongVolumeDefault; FadeOutMs = Constants.Audio.FadeOutMsDefault; Song = asset<Song> PackageName "Battle" }