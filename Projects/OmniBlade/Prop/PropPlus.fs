// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu
open OmniBlade

type PropPlus =
    { Prop : Prop
      Advents : Advent Set
      PointOfInterest : Vector3 }

    static member make prop advents pointOfInterest =
        { Prop = prop
          Advents = advents
          PointOfInterest = pointOfInterest }

    static member empty =
        { Prop = Prop.empty
          Advents = Set.empty
          PointOfInterest = v3Zero }