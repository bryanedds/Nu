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
    module Data =
        
        let WeaponDataFilePath = "Assets/Data/WeaponData.csv"
        let ArmorDataFilePath = "Assets/Data/ArmorData.csv"
        let AccessoryDataFilePath = "Assets/Data/AccessoryData.csv"
        let ConsumableDataFilePath = "Assets/Data/ConsumableData.csv"
        let TechDataFilePath = "Assets/Data/TechData.csv"
        let ArchetypeDataFilePath = "Assets/Data/ArchetypeData.csv"
        let CharacterDataFilePath = "Assets/Data/CharacterData.csv"
        let ShopDataFilePath = "Assets/Data/ShopData.csv"
        let FieldDataFilePath = "Assets/Data/FieldData.csv"
        let BattleDataFilePath = "Assets/Data/BattleData.csv"
        let EncounterDataFilePath = "Assets/Data/EncounterData.csv"
        let TechAnimationDataFilePath = "Assets/Data/TechAnimationData.csv"
        let CharacterAnimationDataFilePath = "Assets/Data/CharacterAnimationData.csv"

    [<RequireQualifiedAccess>]
    module Gui =

        let PackageName = "Gui"
        let Font = asset<Font> PackageName "Font" // TODO: P1: See if this is openly licensed (manaspace font).
        let Splash = asset<Image> PackageName "Splash"
        let DialogThinImage = asset<Image> PackageName "DialogThin" // TODO: P1: Convert tile sheet.
        let DialogThickImage = asset<Image> PackageName "DialogThick" // TODO: P1: Convert tile sheet.
        let DialogXLImage = asset<Image> PackageName "DialogXL" // TODO: P1: Convert tile sheet.
        let DialogXXLImage = asset<Image> PackageName "DialogXXL" // TODO: P1: Convert tile sheet.
        let ButtonUpImage = asset<Image> PackageName "ButtonUp" // TODO: P1: Convert tile sheet.
        let ButtonDownImage = asset<Image> PackageName "ButtonDown" // TODO: P1: Convert tile sheet.
        let ButtonShortUpImage = asset<Image> PackageName "ButtonShortUp" // TODO: P1: Convert tile sheet.
        let ButtonShortDownImage = asset<Image> PackageName "ButtonShortDown" // TODO: P1: Convert tile sheet.
        let AffirmSound = asset<Sound> PackageName "Affirm"
        let MistakeSound = asset<Sound> PackageName "Mistake"
        let TitleSong = { Volume = Constants.Audio.SongVolumeDefault; FadeOutMs = Constants.Audio.FadeOutMsDefault; Song = asset<Song> PackageName "Title" }
        let IntroSong = { Volume = Constants.Audio.SongVolumeDefault; FadeOutMs = 1500; Song = asset<Song> PackageName "Intro" }
        let TitleGroupFilePath = "Assets/Gui/Groups/Title.nugroup"
        let CreditsGroupFilePath = "Assets/Gui/Groups/Credits.nugroup"
        let IntroGroupFilePath = "Assets/Gui/Groups/Intro.nugroup"
        let Intro2GroupFilePath = "Assets/Gui/Groups/Intro2.nugroup"
        let Intro3GroupFilePath = "Assets/Gui/Groups/Intro3.nugroup"
        let Intro4GroupFilePath = "Assets/Gui/Groups/Intro4.nugroup"
        let Intro5GroupFilePath = "Assets/Gui/Groups/Intro5.nugroup"

    [<RequireQualifiedAccess>]
    module Field =

        let PackageName = "Field"
        let DebugRoomTileMap = asset<TileMap> PackageName "DebugRoom" // TODO: P1: Convert tile sheet.
        let DebugBattleTileMap = asset<TileMap> PackageName "DebugBattle" // TODO: P1: Convert tile sheet.
        let WoodenChestOpenedImage = asset<Image> PackageName "WoodenChestOpened"
        let WoodenChestClosedImage = asset<Image> PackageName "WoodenChestClosed"
        let BrassChestOpenedImage = asset<Image> PackageName "BrassChestOpened"
        let BrassChestClosedImage = asset<Image> PackageName "BrassChestClosed"
        let WoodenDoorOpenedImage = asset<Image> PackageName "WoodenDoorOpened" // TODO: P1: Convert art.
        let WoodenDoorClosedImage = asset<Image> PackageName "WoodenDoorClosed" // TODO: P1: Convert art.
        let ThrowSwitchOffImage = asset<Image> PackageName "ThrowSwitchOff" // TODO: P1: Convert art.
        let ThrowSwitchOnImage = asset<Image> PackageName "ThrowSwitchOn" // TODO: P1: Convert art.
        let StepPlateImage = asset<Image> PackageName "StepPlate" // TODO: P1: Convert art.
        let SavePointImage = asset<Image> PackageName "SavePoint" // TODO: P1: Convert art.
        let NpcAnimationSheet = asset<Image> PackageName "Npcs"
        let ShopkeepAnimationSheet = asset<Image> PackageName "Shopkeep"
        let JinnAnimationSheet = asset<Image> PackageName "Jinn"
        let JinnPortraitImage = asset<Image> PackageName "JinnPortrait"
        let StepStairSound = asset<Sound> PackageName "StepStair"
        let StepPlateSound = asset<Sound> PackageName "StepPlate"
        let StepSaveSound = asset<Sound> PackageName "StepSave"
        let BeastGrowlSound = asset<Sound> PackageName "BeastGrowl"
        let BeastDeathSound = asset<Sound> PackageName "BeastDeath"
        let HealSound = asset<Sound> PackageName "Heal"
        let PurchaseSound = asset<Sound> PackageName "Purchase"
        let SaveSound = asset<Sound> PackageName "Save"
        let DoorOpenSound = asset<Sound> PackageName "DoorOpen"
        let ChestOpenSound = asset<Sound> PackageName "ChestOpen"
        let UseSwitchSound = asset<Sound> PackageName "UseSwitch"
        let UnsheatheSound = asset<Sound> PackageName "Unsheathe"
        let GrowthSound = asset<Sound> PackageName "Growth"
        let HitSound = asset<Sound> PackageName "Hit"
        let SlashSound = asset<Sound> PackageName "Slash"
        let ThunderSound = asset<Sound> PackageName "Thunder"
        let ExplosionSound = asset<Sound> PackageName "Explosion"
        let FieldSong = { Volume = Constants.Audio.SongVolumeDefault; FadeOutMs = Constants.Audio.FadeOutMsDefault; Song = asset<Song> PackageName "Field" }

    [<RequireQualifiedAccess>]
    module Battle =
        
        let PackageName = "Battle"
        let CancelImage = asset<Image> PackageName "Cancel" // TODO: P1: Convert all icon art.
        let BoltAnimationSheet = asset<Image> PackageName "Bolt" // TODO: P1: Convert effect art.
        let CycloneBlurAnimationSheet = asset<Image> PackageName "CycloneBlur" // TODO: P1: Convert effect art.
        let SpikeAnimationSheet = asset<Image> PackageName "Spike" // TODO: P1: Convert effect art.
        let ExplosionAnimationSheet = asset<Image> PackageName "Explosion" // TODO: P1: Convert effect art.
        let ImpactSplashAnimationSheet = asset<Image> PackageName "ImpactSplash" // TODO: P1: Convert effect art.