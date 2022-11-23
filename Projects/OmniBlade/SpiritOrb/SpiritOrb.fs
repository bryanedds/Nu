// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

type [<NoComparison>] SpiritOrbInhabitant =
    | ChestInhabitant of Chest
    | PortalInhabitant of Portal
    | SpiritInhabitant of Spirit

type [<NoComparison>] SpiritOrb =
    { AvatarLowerCenter : Vector3
      Chests : Chest array
      Portals : Portal array
      Spirits : Spirit array }