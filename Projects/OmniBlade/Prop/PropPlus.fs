// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu
open OmniBlade

type [<NoComparison>] PropPlus =
    { PropValue : Prop
      Advents : Advent Set
      PointOfInterest : Vector3 }

    static member make prop advents pointOfInterest =
        { PropValue = prop
          Advents = advents
          PointOfInterest = pointOfInterest }

    static member empty =
        { PropValue = Prop.empty
          Advents = Set.empty
          PointOfInterest = v3Zero }