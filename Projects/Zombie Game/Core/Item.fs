// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// An item the player can carry.
type ItemType =
    | Knife
    | Handgun
    | Shotgun
    | Magnum
    | GrenadeLauncher
    | HandgunBullets
    | ShotgunShells
    | MagnumRounds
    | Grenades
    | Antiseptic // the green herb of the place - heals a little
    | Coagulant // good only mixed with antiseptic
    | DoubleAntiseptic // two antiseptics mixed
    | TraumaMix // antiseptic and coagulant mixed - heals entirely
    | FirstAidSpray
    | InkRibbon
    | PharmacyKey
    | MorgueKey
    | Fuse
    | AmbulanceKeys
    | StudentId
    | MasterKey
    | VanKeys
    | BoltCutters
    | Keycard
    | ElevatorKey

/// Two items that combine into a third.
type CombinationData =
    { First : ItemType
      Second : ItemType
      Result : ItemType }

/// What the player sets out with, in its slots in order.
type StartItemData =
    { StartItem : ItemType
      StartCount : int }

/// How a weapon fights - the ammunition it fires, if any, the harm each hit does, how far it reaches, the seconds
/// between uses, whether a hit knocks a creature down, how many pellets a shot fans out, and the sound of its use. One
/// that fires nothing is swung, hitting what its sweep reaches.
type WeaponData =
    { AmmunitionOpt : ItemType option
      Damage : int
      Range : single
      Cooldown : single
      KnocksDown : bool
      Pellets : int
      Sound : SoundType }

/// What sort of thing an item is, which decides what using it does - a weapon carrying how it fights.
type ItemKind =
    | Weapon of WeaponData // equipped, to fight with
    | Ammunition // loaded into the gun that takes it as it's fired
    | Remedy of int // restores this much health, the whole of it at most
    | Ingredient // good only combined with another
    | Ribbon // spent at a typewriter to save
    | KeyItem // used where it fits

/// An item - its name and description, how many of it fit in one slot, and what sort of thing it is.
type ItemData =
    { Item : ItemType
      ItemName : string
      Description : string
      Stack : int
      ItemKind : ItemKind }