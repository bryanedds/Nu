// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

type SpiritOrbInhabitant =
    | ChestInhabitant of Chest
    | PortalInhabitant of Portal
    | NarrativeInhabitant of Narrative
    | SpiritInhabitant of Spirit

type [<ReferenceEquality>] SpiritOrb =
    { AvatarLowerCenter : Vector3
      ShowUnopenedChests : bool
      Chests : Chest array
      Portals : Portal array
      Narratives : Narrative array
      Spirits : Spirit array }