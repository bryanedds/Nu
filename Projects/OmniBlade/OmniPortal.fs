// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module Portal =

    type [<ReferenceEquality; NoComparison>] Portal =
        private
            { Bounds_ : Vector4
              Active_ : bool }

        (* Bounds Properties *)
        member this.Bounds = this.Bounds_
        member this.Position = this.Bounds_.Position
        member this.Center = this.Bounds_.Center
        member this.Bottom = this.Bounds_.Bottom
        member this.Size = this.Bounds_.Size

        (* Local Properties *)
        member this.Active = this.Active_

        static member make bounds active =
            { Bounds_ = bounds
              Active_ = active }

type Portal = Portal.Portal