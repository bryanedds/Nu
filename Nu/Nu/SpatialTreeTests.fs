// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu.Tests
open System
open System.IO
open Xunit
open OpenTK
open Prime
open Nu
module SpatialTreeTests =

    let [<Fact>] spatialTreeTwoWorks () =
        let tree = SpatialTree.make<string> 2 2 (Vector4 (0.0f, 0.0f, 1.0f, 1.0f))
        SpatialTree.addElement true (Vector4 (0.0f, 0.0f, 0.0f, 0.0f)) "Omni" tree
        SpatialTree.addElement false (Vector4 (0.0f, 0.0f, 0.0f, 0.0f)) "Zero" tree
        Assert.Equal (2, Seq.length (SpatialTree.getElementsAtPoint Vector2.Zero tree))
        Assert.Equal (1, Seq.length (SpatialTree.getElementsAtPoint Vector2.One tree))
        Assert.Equal (2, Seq.length (SpatialTree.getElementsAtPoint (Vector2 0.5f) tree))
        Assert.Equal (1, Seq.length (SpatialTree.getElementsAtPoint (Vector2 2.0f) tree))

    let [<Fact>] spatialTreeThreeWorks () =
        let tree = SpatialTree.make<string> 2 3 (Vector4 (0.0f, 0.0f, 1.0f, 1.0f))
        SpatialTree.addElement true (Vector4 (0.0f, 0.0f, 0.0f, 0.0f)) "Omni" tree
        SpatialTree.addElement false (Vector4 (0.0f, 0.0f, 0.0f, 0.0f)) "Zero" tree
        SpatialTree.addElement false (Vector4 (0.5f, 0.5f, 0.5f, 0.5f)) "Half" tree
        SpatialTree.addElement false (Vector4 (0.0f, 0.0f, 1.0f, 1.0f)) "ZeroOne" tree
        SpatialTree.addElement false (Vector4 (1.0f, 1.0f, 0.0f, 0.0f)) "OneZero" tree
        Assert.Equal (3, Seq.length (SpatialTree.getElementsAtPoint Vector2.Zero tree))
        Assert.Equal (4, Seq.length (SpatialTree.getElementsAtPoint Vector2.One tree))
        Assert.Equal (3, Seq.length (SpatialTree.getElementsAtPoint (Vector2 0.5f) tree))
        Assert.Equal (1, Seq.length (SpatialTree.getElementsAtPoint (Vector2 2.0f) tree))