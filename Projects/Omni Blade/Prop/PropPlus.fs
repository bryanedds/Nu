// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

type [<CustomEquality; NoComparison>] PropPlus =
    { UpdateTime : int64
      PointOfInterest : Vector3
      Advents : Advent Set
      Prop : Prop }

    static member make time pointOfInterest advents prop =
        { UpdateTime = time
          PointOfInterest = pointOfInterest
          Advents = advents
          Prop = prop }

    static member empty =
        { UpdateTime = 0L
          PointOfInterest = v3Zero
          Advents = Set.empty
          Prop = Prop.empty }

    override this.Equals (that : obj) =
        match that with
        | :? PropPlus as that ->
            this.UpdateTime = that.UpdateTime &&
            v3Eq this.PointOfInterest that.PointOfInterest &&
            refEq this.Advents that.Advents && // OPTIMIZATION: presume advents come from a relatively static source.
            this.Prop = that.Prop
        | _ -> false

    override this.GetHashCode () =
        hash this.UpdateTime ^^^
        hash this.PointOfInterest ^^^
        hash this.Advents ^^^
        hash this.Prop