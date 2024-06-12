// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Chest =

    type Chest =
        { Perimeter : Box3
          Opened : bool }

        static member make bounds opened =
            { Perimeter = bounds
              Opened = opened }

type Chest = Chest.Chest