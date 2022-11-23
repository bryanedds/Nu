// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu
open OmniBlade

type [<NoComparison>] SpiritOrbInhabitant =
    | ChestInhabitant of Chest
    | PortalInhabitant of Portal
    | SpiritInhabitant of Spirit

// TODO: consider making this an algebraic data type.
type [<NoComparison>] SpiritOrb =
    { AvatarLowerCenter : Vector3
      Chests : Chest array
      Portals : Portal array
      Spirits : Spirit array }