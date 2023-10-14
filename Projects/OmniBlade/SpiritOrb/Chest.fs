// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Chest =

    type Chest =
        private
            { Perimeter_ : Box3
              Opened_ : bool }

        (* Perimeter Properties *)
        member this.Perimeter = this.Perimeter_
        member this.Center = this.Perimeter_.Center
        member this.Bottom = this.Perimeter_.Bottom
        member this.Size = this.Perimeter_.Size

        (* Local Properties *)
        member this.Opened = this.Opened_

        static member make bounds opened =
            { Perimeter_ = bounds
              Opened_ = opened }

type Chest = Chest.Chest