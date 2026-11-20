// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open Prime
open Nu

/// A sound the data names - a cue's, a weapon's as it's used, or a creature's crying out, attacking or dying.
type SoundType =
    | DrawerOpenSound
    | LockReleaseSound
    | DrawerSlamSound
    | FuseSound
    | PowerOnSound
    | EngineSound
    | SteamSound // steam driven hammering through the pipes
    | HandgunShotSound
    | ShotgunShotSound
    | MagnumShotSound
    | GrenadeShotSound
    | KnifeSwingSound
    | GroanSound // one of a dead thing's several groans
    | BiteSound
    | ZombieDeathSound
    | DogGrowlSound
    | DogBarkSound
    | DogDeathSound
    | SurgeonRoarSound
    | SurgeonSwipeSound
    | SurgeonDeathSound
    | SprinterHissSound
    | SprinterShriekSound
    | SprinterDeathSound
    | LinebackerRoarSound
    | LinebackerSmashSound
    | LinebackerDeathSound
    | ButcherGruntSound
    | ButcherChopSound
    | ButcherDeathSound
    | SowSquealSound
    | SowGoreSound
    | SowDeathSound
    | MeatKingRoarSound
    | MeatKingChopSound
    | MeatKingDeathSound

    /// The sounds it's one of, picked among by chance - most are one sound alone.
    member this.Sounds =
        match this with
        | DrawerOpenSound -> [|Assets.Gameplay.DrawerOpenSound|]
        | LockReleaseSound -> [|Assets.Gameplay.KeyUnlockSound|]
        | DrawerSlamSound -> [|Assets.Gameplay.DrawerSlamSound|]
        | FuseSound -> [|Assets.Gameplay.FuseSound|]
        | PowerOnSound -> [|Assets.Gameplay.PowerOnSound|]
        | EngineSound -> [|Assets.Gameplay.EngineSound|]
        | SteamSound -> [|Assets.Gameplay.SteamSound|]
        | HandgunShotSound -> [|Assets.Gameplay.HandgunShotSound|]
        | ShotgunShotSound -> [|Assets.Gameplay.ShotgunShotSound|]
        | MagnumShotSound -> [|Assets.Gameplay.MagnumShotSound|]
        | GrenadeShotSound -> [|Assets.Gameplay.GrenadeShotSound|]
        | KnifeSwingSound -> [|Assets.Gameplay.KnifeSwingSound|]
        | GroanSound -> Assets.Gameplay.GroanSounds
        | BiteSound -> [|Assets.Gameplay.BiteSound|]
        | ZombieDeathSound -> [|Assets.Gameplay.ZombieDeathSound|]
        | DogGrowlSound -> [|Assets.Gameplay.DogGrowlSound|]
        | DogBarkSound -> [|Assets.Gameplay.DogBarkSound|]
        | DogDeathSound -> [|Assets.Gameplay.DogDeathSound|]
        | SurgeonRoarSound -> [|Assets.Gameplay.SurgeonRoarSound|]
        | SurgeonSwipeSound -> [|Assets.Gameplay.SurgeonSwipeSound|]
        | SurgeonDeathSound -> [|Assets.Gameplay.SurgeonDeathSound|]
        | SprinterHissSound -> [|Assets.Gameplay.SprinterHissSound|]
        | SprinterShriekSound -> [|Assets.Gameplay.SprinterShriekSound|]
        | SprinterDeathSound -> [|Assets.Gameplay.SprinterDeathSound|]
        | LinebackerRoarSound -> [|Assets.Gameplay.LinebackerRoarSound|]
        | LinebackerSmashSound -> [|Assets.Gameplay.LinebackerSmashSound|]
        | LinebackerDeathSound -> [|Assets.Gameplay.LinebackerDeathSound|]
        | ButcherGruntSound -> [|Assets.Gameplay.ButcherGruntSound|]
        | ButcherChopSound -> [|Assets.Gameplay.ButcherChopSound|]
        | ButcherDeathSound -> [|Assets.Gameplay.ButcherDeathSound|]
        | SowSquealSound -> [|Assets.Gameplay.SowSquealSound|]
        | SowGoreSound -> [|Assets.Gameplay.SowGoreSound|]
        | SowDeathSound -> [|Assets.Gameplay.SowDeathSound|]
        | MeatKingRoarSound -> [|Assets.Gameplay.MeatKingRoarSound|]
        | MeatKingChopSound -> [|Assets.Gameplay.MeatKingChopSound|]
        | MeatKingDeathSound -> [|Assets.Gameplay.MeatKingDeathSound|]

    /// The sound itself, where it's one sound alone, or the first of those it's one of.
    member this.Sound =
        this.Sounds[0]

    /// How loud the sound plays, as its loudness balances with the rest - a creature's a little louder than it would be
    /// otherwise, for its being heard from across a room.
    member this.Volume =
        match this with
        | DrawerOpenSound -> 0.2f
        | KnifeSwingSound | LockReleaseSound | SteamSound | GroanSound | DogGrowlSound -> 0.4f
        | PowerOnSound -> 0.5f
        | DrawerSlamSound | EngineSound | SprinterHissSound | ButcherGruntSound | SprinterShriekSound | LinebackerSmashSound | SowGoreSound -> 0.55f
        | FuseSound | SprinterDeathSound | ButcherDeathSound | SowSquealSound | SurgeonSwipeSound -> 0.65f
        | ZombieDeathSound | DogDeathSound | DogBarkSound -> 0.7f
        | MagnumShotSound | GrenadeShotSound | SurgeonRoarSound | LinebackerRoarSound -> 0.85f
        | HandgunShotSound | SurgeonDeathSound | LinebackerDeathSound | SowDeathSound | MeatKingDeathSound -> 0.9f
        | ShotgunShotSound | BiteSound | ButcherChopSound | MeatKingRoarSound | MeatKingChopSound -> 1.0f
