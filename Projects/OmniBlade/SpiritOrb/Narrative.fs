// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Narrative =

    type Narrative =
        private
            { Perimeter_ : Box3
              Active_ : bool }

        (* Perimeter Properties *)
        member this.Perimeter = this.Perimeter_
        member this.Center = this.Perimeter_.Center
        member this.Bottom = this.Perimeter_.Bottom
        member this.Size = this.Perimeter_.Size

        (* Local Properties *)
        member this.Active = this.Active_

        static member make bounds active =
            { Perimeter_ = bounds
              Active_ = active }

type Narrative = Narrative.Narrative