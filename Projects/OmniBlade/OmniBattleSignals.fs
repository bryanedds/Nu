// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open Nu
open Nu.Declarative
open OmniBlade

type [<NoComparison>] Hop =
    { HopStart : Vector3
      HopStop : Vector3 }

type BattleMessage =
    | Update
    | InteractDialog
    | RegularItemSelect of CharacterIndex * string
    | RegularItemCancel of CharacterIndex
    | ConsumableItemSelect of CharacterIndex * string
    | ConsumableItemCancel of CharacterIndex
    | TechItemSelect of CharacterIndex * string
    | TechItemCancel of CharacterIndex
    | ReticlesSelect of CharacterIndex * CharacterIndex
    | ReticlesCancel of CharacterIndex
    | ReadyCharacters of int64
    | PoiseCharacters
    | CelebrateCharacters of bool
    | AttackCharacter1 of CharacterIndex
    | AttackCharacter2 of CharacterIndex * CharacterIndex
    | ConsumeCharacter1 of ConsumableType * CharacterIndex
    | ConsumeCharacter2 of ConsumableType * CharacterIndex
    | TechCharacter1 of CharacterIndex * CharacterIndex * TechType
    | TechCharacter2 of CharacterIndex * CharacterIndex * TechType
    | TechCharacter3 of CharacterIndex * CharacterIndex * TechType
    | TechCharacter4 of CharacterIndex * CharacterIndex * TechType
    | TechCharacter5 of CharacterIndex * CharacterIndex * TechType
    | TechCharacter6 of CharacterIndex * CharacterIndex * TechType
    | TechCharacterAmbient of CharacterIndex * CharacterIndex * TechType
    | AutoBattleEnemies
    | ChargeCharacter of CharacterIndex
    | PoiseCharacter of CharacterIndex
    | WoundCharacter of CharacterIndex
    | ResetCharacter of CharacterIndex
    | DestroyCharacter of CharacterIndex
    | Nop

type [<NoComparison>] BattleCommand =
    | UpdateEye
    | DisplayCancel of CharacterIndex
    | DisplayHitPointsChange of CharacterIndex * int
    | DisplayBolt of int64 * CharacterIndex
    | DisplayCycloneBlur of int64 * CharacterIndex * single
    | DisplayImpactSplash of int64 * CharacterIndex
    | DisplayCut of int64 * bool * CharacterIndex
    | DisplaySlashSpike of int64 * Vector3 * CharacterIndex
    | DisplayArcaneCast of int64 * CharacterIndex
    | DisplayFire of int64 * CharacterIndex * CharacterIndex
    | DisplayFlame of int64 * CharacterIndex * CharacterIndex
    | DisplayIce of int64 * CharacterIndex
    | DisplaySnowball of int64 * CharacterIndex
    | DisplayHolyCast of int64 * CharacterIndex
    | DisplayPurify of int64 * CharacterIndex
    | DisplayCure of int64 * CharacterIndex
    | DisplayProtect of int64 * CharacterIndex
    | DisplayDimensionalCast of int64 * CharacterIndex
    | DisplayBuff of int64 * StatusType * CharacterIndex
    | DisplayDebuff of int64 * StatusType * CharacterIndex
    | DisplayConjureIfrit of int64
    | DisplayHop of Hop
    | DisplayCircle of Vector3 * single
    | PlaySound of int64 * single * AssetTag<Sound>
    | PlaySong of int * int * single * double * Song AssetTag
    | FadeOutSong of int