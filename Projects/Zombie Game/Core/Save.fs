// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.IO
open System.Numerics
open Prime
open Nu

/// What a save remembers - where the player stood, what it carried and how it was, and what had been done.
type Save =
    { SaveArea : AreaType
      SavePosition : Vector3
      SaveYaw : single
      SaveHealth : int
      SaveInventory : Inventory
      SaveFlags : Advent Set
      SaveCount : int
      SavePlayTime : single
      SaveGameSeed : uint64 }

    /// Whether there's a save.
    static member getExists () =
        File.Exists Constants.Gameplay.SaveFilePath

    /// Write a save, in place of any before it.
    static member write (save : Save) =
        File.WriteAllText (Constants.Gameplay.SaveFilePath, scstring save)

    /// Read the save.
    static member read () =
        scvalue<Save> (File.ReadAllText Constants.Gameplay.SaveFilePath)

    /// A new game's start, its randomness keyed by a game seed.
    static member makeStart gameSeed =
        { SaveArea = Data.StartArea
          SavePosition = fst Data.Start
          SaveYaw = snd Data.Start
          SaveHealth = Constants.Gameplay.PlayerHitPoints
          SaveInventory = Inventory.Initial
          SaveFlags = Set.empty
          SaveCount = 0
          SavePlayTime = 0.0f
          SaveGameSeed = gameSeed }

    /// A start in an area with the fixed game seed - where play starts, if it's play's first area, else as though come
    /// in through its first door - with what's been done and what's carried.
    static member makeIn area flags inventory =
        let (position, yaw) = if area = Data.StartArea then Data.Start else Data.Entrances[area]
        { Save.makeStart Constants.Gameplay.FixedGameSeed with
            SaveArea = area
            SavePosition = position
            SaveYaw = yaw
            SaveFlags = flags
            SaveInventory = inventory }
