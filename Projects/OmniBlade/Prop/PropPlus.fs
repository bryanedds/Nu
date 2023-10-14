// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

type [<CustomEquality; NoComparison>] PropPlus =
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

    override this.Equals (that : obj) =
        match that with
        | :? PropPlus as that ->
            this.Prop = that.Prop &&
            refEq this.Advents that.Advents && // OPTIMIZATION: presume advents come from a relatively static source.
            v3Eq this.PointOfInterest that.PointOfInterest
        | _ -> false

    override this.GetHashCode () =
        hash this.Prop ^^^
        hash this.Advents ^^^
        hash this.PointOfInterest