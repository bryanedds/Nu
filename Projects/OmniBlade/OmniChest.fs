// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module Chest =

    type [<ReferenceEquality; NoComparison>] Chest =
        private
            { Bounds_ : Box3
              Opened_ : bool }

        (* Bounds Properties *)
        member this.Bounds = this.Bounds_
        member this.Position = this.Bounds_.Position
        member this.Center = this.Bounds_.Center
        member this.Bottom = this.Bounds_.Bottom
        member this.Size = this.Bounds_.Size

        (* Local Properties *)
        member this.Opened = this.Opened_

        static member make bounds opened =
            { Bounds_ = bounds
              Opened_ = opened }

type Chest = Chest.Chest