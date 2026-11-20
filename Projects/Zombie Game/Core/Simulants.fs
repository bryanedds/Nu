// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module Simulants =

    let Title = Game / "Title"
    let Gameplay = Game / "Gameplay"
    let GameplayScene = Gameplay / "Scene"
    let GameplayPlayer = GameplayScene / "Player"
    let GameplayDoorScene = Gameplay / "DoorScene"
    let GameplayArea name = GameplayScene / name
    let GameplayInventory = GameplayScene / "Inventory"
    let GameplayItemBox = GameplayScene / "ItemBox"
    let GameplayKeypad = GameplayScene / "Keypad"
    let GameplaySavePrompt = GameplayScene / "SavePrompt"
    let GameplayReading = GameplayScene / "Reading"
    let GameOver = Game / "GameOver"
    let Ending = Game / "Ending"