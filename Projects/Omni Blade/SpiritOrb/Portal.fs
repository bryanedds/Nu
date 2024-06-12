// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Portal =

    type Portal =
        { Perimeter : Box3
          Active : bool }

        static member make bounds active =
            { Perimeter = bounds
              Active = active }

type Portal = Portal.Portal