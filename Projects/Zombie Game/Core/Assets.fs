// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open Prime
open Nu

/// Where the game's files are - its data sheets, and its gui and gameplay assets by package.
[<RequireQualifiedAccess>]
module Assets =

    /// The data sheets, read straight from their files rather than loaded as assets.
    [<RequireQualifiedAccess>]
    module Data =

        let ItemDataFilePath = "Assets/Data/ItemData.csv"
        let CombinationDataFilePath = "Assets/Data/CombinationData.csv"
        let CreatureDataFilePath = "Assets/Data/CreatureData.csv"
        let StartItemDataFilePath = "Assets/Data/StartItemData.csv"
        let SurfaceDataFilePath = "Assets/Data/SurfaceData.csv"
        let CreditDataFilePath = "Assets/Data/CreditData.csv"

    /// The gui package's assets - the title, ending and game over songs.
    [<RequireQualifiedAccess>]
    module Gui =

        let PackageName = "Gui"
        let TitleSong = { SongDescriptor.defaultDescriptor with FadeInTime = GameTime.ofSeconds 1.5; FadeOutTime = GameTime.ofSeconds 1.5; Volume = 0.8f; Song = asset<Song> PackageName "TitleSong" }
        let EndingSong = { SongDescriptor.defaultDescriptor with FadeInTime = GameTime.ofSeconds 1.5; FadeOutTime = GameTime.ofSeconds 1.5; RepeatLimitOpt = Some 0u; Volume = 0.8f; Song = asset<Song> PackageName "EndingSong" } // played through once
        let GameOverSong = { SongDescriptor.defaultDescriptor with FadeOutTime = GameTime.ofSeconds 1.5; RepeatLimitOpt = Some 0u; Volume = 0.85f; Song = asset<Song> PackageName "GameOverSong" } // a sting, at once, played through once

    /// The gameplay package's assets - models, maps and sounds - and the folders the scene and puppet files are read
    /// from.
    [<RequireQualifiedAccess>]
    module Gameplay =

        let PackageName = "Gameplay"
        let BoxModel = asset<StaticModel> PackageName "Box"
        let BallModel = asset<StaticModel> PackageName "Ball"
        let CylinderModel = asset<StaticModel> PackageName "Cylinder"
        let ConeModel = asset<StaticModel> PackageName "Cone"
        let QuadModel = asset<StaticModel> PackageName "Quad" // a single unit square facing +z
        let InteriorSkyBoxMap = asset<CubeMap> PackageName "InteriorSkyBoxMap" // a dim all-round surround, so no daylight lights the areas
        let WhiteImage = asset<Image> PackageName "White" // a solid white map, so material properties alone decide each surface
        let FootstepSound = asset<Sound> PackageName "Footstep"
        let GroanSounds =
            [|asset<Sound> PackageName "ZombieGroan0"
              asset<Sound> PackageName "ZombieGroan1"
              asset<Sound> PackageName "ZombieGroan2"|]
        let HandgunShotSound = asset<Sound> PackageName "HandgunShot"
        let ShotgunShotSound = asset<Sound> PackageName "ShotgunShot"
        let MagnumShotSound = asset<Sound> PackageName "MagnumShot"
        let GrenadeShotSound = asset<Sound> PackageName "GrenadeShot"
        let EmptyClickSound = asset<Sound> PackageName "EmptyClick"
        let KnifeSwingSound = asset<Sound> PackageName "KnifeSwing"
        let FleshHitSound = asset<Sound> PackageName "FleshHit"
        let HeadBurstSound = asset<Sound> PackageName "HeadBurst"
        let BiteSound = asset<Sound> PackageName "Bite"
        let PlayerHurtSound = asset<Sound> PackageName "PlayerHurt"
        let PlayerDeathSound = asset<Sound> PackageName "PlayerDeath"
        let ZombieDeathSound = asset<Sound> PackageName "ZombieDeath"
        let DogGrowlSound = asset<Sound> PackageName "DogGrowl"
        let DogBarkSound = asset<Sound> PackageName "DogBark"
        let DogDeathSound = asset<Sound> PackageName "DogDeath"
        let SurgeonRoarSound = asset<Sound> PackageName "SurgeonRoar"
        let SurgeonSwipeSound = asset<Sound> PackageName "SurgeonSwipe"
        let SurgeonDeathSound = asset<Sound> PackageName "SurgeonDeath"
        let SprinterHissSound = asset<Sound> PackageName "SprinterHiss"
        let SprinterShriekSound = asset<Sound> PackageName "SprinterShriek"
        let SprinterDeathSound = asset<Sound> PackageName "SprinterDeath"
        let LinebackerRoarSound = asset<Sound> PackageName "LinebackerRoar"
        let LinebackerSmashSound = asset<Sound> PackageName "LinebackerSmash"
        let LinebackerDeathSound = asset<Sound> PackageName "LinebackerDeath"
        let ButcherGruntSound = asset<Sound> PackageName "ButcherGrunt"
        let ButcherChopSound = asset<Sound> PackageName "ButcherChop"
        let ButcherDeathSound = asset<Sound> PackageName "ButcherDeath"
        let SowSquealSound = asset<Sound> PackageName "SowSqueal"
        let SowGoreSound = asset<Sound> PackageName "SowGore"
        let SowDeathSound = asset<Sound> PackageName "SowDeath"
        let MeatKingRoarSound = asset<Sound> PackageName "MeatKingRoar"
        let MeatKingChopSound = asset<Sound> PackageName "MeatKingChop"
        let MeatKingDeathSound = asset<Sound> PackageName "MeatKingDeath"
        let DoorSound = asset<Sound> PackageName "Door"
        let DoubleDoorSound = asset<Sound> PackageName "DoubleDoor"
        let ShutterSound = asset<Sound> PackageName "Shutter"
        let DoorLockedSound = asset<Sound> PackageName "DoorLocked"
        let KeyUnlockSound = asset<Sound> PackageName "KeyUnlock"
        let ItemPickupSound = asset<Sound> PackageName "ItemPickup"
        let TypewriterSound = asset<Sound> PackageName "Typewriter"
        let BoxOpenSound = asset<Sound> PackageName "BoxOpen"
        let MenuMoveSound = asset<Sound> PackageName "MenuMove"
        let MenuConfirmSound = asset<Sound> PackageName "MenuConfirm"
        let MenuBackSound = asset<Sound> PackageName "MenuBack"
        let HealSound = asset<Sound> PackageName "Heal"
        let GunReadySound = asset<Sound> PackageName "GunReady"
        let PageSound = asset<Sound> PackageName "Page"
        let KeypadSound = asset<Sound> PackageName "Keypad"
        let KeypadWrongSound = asset<Sound> PackageName "KeypadWrong"
        let DrawerOpenSound = asset<Sound> PackageName "DrawerOpen"
        let PowerOnSound = asset<Sound> PackageName "PowerOn"
        let FuseSound = asset<Sound> PackageName "Fuse"
        let DrawerSlamSound = asset<Sound> PackageName "DrawerSlam" // a morgue drawer flung open from within
        let EngineSound = asset<Sound> PackageName "Engine"
        let SteamSound = asset<Sound> PackageName "Steam"
        let HospitalSong = { SongDescriptor.defaultDescriptor with FadeInTime = GameTime.ofSeconds 1.5; FadeOutTime = GameTime.ofSeconds 1.5; Volume = 0.55f; Song = asset<Song> PackageName "HospitalSong" } // not a tune - the building itself
        let HospitalPoweredSong = { SongDescriptor.defaultDescriptor with FadeInTime = GameTime.ofSeconds 1.5; FadeOutTime = GameTime.ofSeconds 1.5; Volume = 0.55f; Song = asset<Song> PackageName "HospitalPoweredSong" } // the building with its power back - the lights humming
        let SafeRoomSong = { SongDescriptor.defaultDescriptor with FadeInTime = GameTime.ofSeconds 1.5; FadeOutTime = GameTime.ofSeconds 1.5; Volume = 0.7f; Song = asset<Song> PackageName "SafeRoomSong" }
        let BossSong = { SongDescriptor.defaultDescriptor with FadeInTime = GameTime.ofSeconds 1.5; FadeOutTime = GameTime.ofSeconds 1.5; Volume = 0.75f; Song = asset<Song> PackageName "BossSong" }
        let DormSong = { SongDescriptor.defaultDescriptor with FadeInTime = GameTime.ofSeconds 1.5; FadeOutTime = GameTime.ofSeconds 1.5; Volume = 0.55f; Song = asset<Song> PackageName "DormSong" } // not a tune - the dorm at night
        let DormBossSong = { SongDescriptor.defaultDescriptor with FadeInTime = GameTime.ofSeconds 1.5; FadeOutTime = GameTime.ofSeconds 1.5; Volume = 0.75f; Song = asset<Song> PackageName "DormBossSong" }
        let PlantSong = { SongDescriptor.defaultDescriptor with FadeInTime = GameTime.ofSeconds 1.5; FadeOutTime = GameTime.ofSeconds 1.5; Volume = 0.55f; Song = asset<Song> PackageName "PlantSong" } // not a tune - the plant's machinery, still running for no one
        let PlantBossSong = { SongDescriptor.defaultDescriptor with FadeInTime = GameTime.ofSeconds 1.5; FadeOutTime = GameTime.ofSeconds 1.5; Volume = 0.75f; Song = asset<Song> PackageName "PlantBossSong" }
        let FinalBossSong = { SongDescriptor.defaultDescriptor with FadeInTime = GameTime.ofSeconds 1.5; FadeOutTime = GameTime.ofSeconds 1.5; Volume = 0.8f; Song = asset<Song> PackageName "FinalBossSong" }
        let FinalBossRageSong = { SongDescriptor.defaultDescriptor with FadeInTime = GameTime.ofSeconds 0.5; FadeOutTime = GameTime.ofSeconds 0.5; Volume = 0.85f; Song = asset<Song> PackageName "FinalBossRageSong" } // cutting in at once, as he rages
        let SceneDirPath = "Assets/Gameplay" // where the scene files are, each named for its scene
        let PuppetDirPath = "Assets/Gameplay" // where the puppet files are, each named for its character
