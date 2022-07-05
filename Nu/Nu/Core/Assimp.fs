// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Numerics
open Nu

/// Specifies how to interpret imported asset units.
type UnitType =
    | UnitMeters
    | UnitCentimeters

[<AutoOpen>]
module AssimpExtensions =

    /// Node extensions.
    type Assimp.Node with

        /// Collect all the child nodes of a node, including the node itself.
        member this.CollectNodes () =
            seq {
                yield this
                for child in this.Children do
                    yield! child.CollectNodes () }

        /// Collect all the child nodes and transforms of a node, including the node itself.
        member this.CollectNodesAndTransforms (unitType, parentTransform : Matrix4x4) =
            seq {
                let scalar = match unitType with UnitMeters -> 1.0f | UnitCentimeters -> 0.01f
                let nodeTransform =
                    Matrix4x4
                        (this.Transform.A1, this.Transform.B1, this.Transform.C1, this.Transform.D1,
                         this.Transform.A2, this.Transform.B2, this.Transform.C2, this.Transform.D2,
                         this.Transform.A3, this.Transform.B3, this.Transform.C3, this.Transform.D3,
                         this.Transform.A4 * scalar, this.Transform.B4 * scalar, this.Transform.C4 * scalar, this.Transform.D4)
                let transform = nodeTransform * parentTransform
                yield (this, transform)
                for child in this.Children do
                    yield! child.CollectNodesAndTransforms (unitType, transform) }