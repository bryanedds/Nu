// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open Nu

[<RequireQualifiedAccess>]
module Assets =

    [<RequireQualifiedAccess>]
    module Global =

        let SaveFilePath1 = "OmniBlade1.sav"
        let SaveFilePath2 = "OmniBlade2.sav"
        let SaveFilePath3 = "OmniBlade3.sav"

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
        let DialogThinImage = asset<Image> PackageName "DialogThin" // TODO: P1: Convert art.
        let DialogThickImage = asset<Image> PackageName "DialogThick" // TODO: P1: Convert art.
        let DialogXLImage = asset<Image> PackageName "DialogXL" // TODO: P1: Convert art.
        let DialogXXLImage = asset<Image> PackageName "DialogXXL" // TODO: P1: Convert art.
        let ButtonUpImage = asset<Image> PackageName "ButtonUp" // TODO: P1: Convert art.
        let ButtonDownImage = asset<Image> PackageName "ButtonDown" // TODO: P1: Convert art.
        let ButtonShortUpImage = asset<Image> PackageName "ButtonShortUp" // TODO: P1: Convert art.
        let ButtonShortDownImage = asset<Image> PackageName "ButtonShortDown" // TODO: P1: Convert art.
        let AffirmSound = asset<Sound> PackageName "Affirm"
        let MistakeSound = asset<Sound> PackageName "Mistake"
        let SlotSound = asset<Sound> PackageName "Slot"
        let TitleSong = { FadeInMs = 0; FadeOutMs = Constants.Audio.FadeOutMsDefault; Volume = Constants.Audio.SongVolumeDefault; Start = 0.0; Song = asset<Song> PackageName "Title" }
        let IntroSong = { FadeInMs = 0; FadeOutMs = 1500; Volume = Constants.Audio.SongVolumeDefault; Start = 0.0; Song = asset<Song> PackageName "Intro" }
        let TitleGroupFilePath = "Assets/Gui/Groups/Title.nugroup"
        let PickGroupFilePath = "Assets/Gui/Groups/Pick.nugroup"
        let IntroGroupFilePath = "Assets/Gui/Groups/Intro.nugroup"
        let Intro2GroupFilePath = "Assets/Gui/Groups/Intro2.nugroup"
        let Intro3GroupFilePath = "Assets/Gui/Groups/Intro3.nugroup"
        let Intro4GroupFilePath = "Assets/Gui/Groups/Intro4.nugroup"
        let Intro5GroupFilePath = "Assets/Gui/Groups/Intro5.nugroup"
        let CreditsGroupFilePath = "Assets/Gui/Groups/Credits.nugroup"

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
        let FlameImage = asset<Image> PackageName "Flame"
        let SavePointImage = asset<Image> PackageName "SavePoint" // TODO: P1: Convert art.
        let StairsImage = asset<Image> PackageName "Stairs"
        let SpiritImage = asset<Image> PackageName "Spirit"
        let SpiritChestClosedImage = asset<Image> PackageName "SpiritChestClosed"
        let SpiritChestOpenedImage = asset<Image> PackageName "SpiritChestOpened"
        let SpiritAvatarImage = asset<Image> PackageName "SpiritAvatar"
        let SpiritOrbImage = asset<Image> PackageName "SpiritOrb"
        let SealAnimationSheet = asset<Image> PackageName "Seal"
        let NpcAnimationSheet = asset<Image> PackageName "Npcs"
        let BossAnimationSheet = asset<Image> PackageName "Bosses"
        let ShopkeepAnimationSheet = asset<Image> PackageName "Shopkeep"
        let JinnAnimationSheet = asset<Image> PackageName "Jinn"
        let ShadeAnimationSheet = asset<Image> PackageName "Shade"
        let MaelAnimationSheet = asset<Image> PackageName "Mael"
        let RiainAnimationSheet = asset<Image> PackageName "Riain"
        let PericAnimationSheet = asset<Image> PackageName "Peric"
        let JinnPortraitImage = asset<Image> PackageName "JinnPortrait"
        let ShadePortraitImage = asset<Image> PackageName "ShadePortrait"
        let MaelPortraitImage = asset<Image> PackageName "MaelPortrait"
        let RiainPortraitImage = asset<Image> PackageName "RiainPortrait"
        let PericPortraitImage = asset<Image> PackageName "PericPortrait"
        let StepStairSound = asset<Sound> PackageName "StepStair"
        let StepPlateSound = asset<Sound> PackageName "StepPlate"
        let StepSaveSound = asset<Sound> PackageName "StepSave"
        let BeastGrowlSound = asset<Sound> PackageName "BeastGrowl"
        let BeastDeathSound = asset<Sound> PackageName "BeastDeath"
        let HealSound = asset<Sound> PackageName "Heal"
        let AuraSound = asset<Sound> PackageName "Aura"
        let PurchaseSound = asset<Sound> PackageName "Purchase" // TODO: P1: Move this to Gui?
        let DoorOpenSound = asset<Sound> PackageName "DoorOpen"
        let ChestOpenSound = asset<Sound> PackageName "ChestOpen"
        let UseSwitchSound = asset<Sound> PackageName "UseSwitch"
        let SealedSound = asset<Sound> PackageName "Sealed"
        let UnsheatheSound = asset<Sound> PackageName "Unsheathe"
        let GrowthSound = asset<Sound> PackageName "Growth"
        let HitSound = asset<Sound> PackageName "Hit"
        let ChargeHolySound = asset<Sound> PackageName "ChargeHoly"
        let ChargeDimensionSound = asset<Sound> PackageName "ChargeDimension"
        let SlashSound = asset<Sound> PackageName "Slash"
        let IceSound = asset<Sound> PackageName "Ice"
        let SnowballSound = asset<Sound> PackageName "Snowball"
        let FireSound = asset<Sound> PackageName "Fire"
        let FlameSound = asset<Sound> PackageName "Flame"
        let ThunderSound = asset<Sound> PackageName "Thunder"
        let ExplosionSound = asset<Sound> PackageName "Explosion"
        let FieldSong = { FadeInMs = 0; FadeOutMs = Constants.Audio.FadeOutMsDefault; Volume = Constants.Audio.SongVolumeDefault; Start = 0.0; Song = asset<Song> PackageName "Field" }

    [<RequireQualifiedAccess>]
    module Battle =
        
        let PackageName = "Battle"
        let CancelImage = asset<Image> PackageName "Cancel"
        let BoltAnimationSheet = asset<Image> PackageName "Bolt" // TODO: P1: Convert effect art.
        let CycloneBlurAnimationSheet = asset<Image> PackageName "CycloneBlur" // TODO: P1: Convert effect art.
        let SpikeAnimationSheet = asset<Image> PackageName "Spike" // TODO: P1: Convert effect art.
        let ExplosionAnimationSheet = asset<Image> PackageName "Explosion" // TODO: P1: Convert effect art.
        let ImpactSplashAnimationSheet = asset<Image> PackageName "ImpactSplash" // TODO: P1: Convert effect art.
        let CutImage = asset<Image> PackageName "Cut"
        let LightCutImage = asset<Image> PackageName "LightCut"
        let CandleAnimationSheet = asset<Image> PackageName "Candle"
        let StaticAnimationSheet = asset<Image> PackageName "Static"
        let FireAnimationSheet = asset<Image> PackageName "Fire"
        let BurnAnimationSheet = asset<Image> PackageName "Burn"
        let FlameAnimationSheet = asset<Image> PackageName "Flame"
        let IceImage = asset<Image> PackageName "Ice"
        let SnowballImage = asset<Image> PackageName "Snowball"
        let HolyCastAnimationSheet = asset<Image> PackageName "HolyCast"
        let PurifyAnimationSheet = asset<Image> PackageName "Purify"
        let SparkleAnimationSheet = asset<Image> PackageName "Sparkle"
        let ProtectSphereImage = asset<Image> PackageName "ProtectSphere"
        let ElectronBlueImage = asset<Image> PackageName "ElectronBlue"
        let NonLocationBlueImage = asset<Image> PackageName "NonLocationBlue"
        let ElectronGreenImage = asset<Image> PackageName "ElectronGreen"
        let NonLocationGreenImage = asset<Image> PackageName "NonLocationGreen"
        let ShieldImage = asset<Image> PackageName "Shield"
        let PowerImage = asset<Image> PackageName "Power"
        let FireSpinAnimationSheet = asset<Image> PackageName "FireSpin"