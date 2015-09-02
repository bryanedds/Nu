// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open System.IO
open System.Xml
open Xunit
open OpenTK
open Prime
open Nu
module QuadTreeTests =

    let [<Fact>] quadTreeTwoWorks () =
        let tree = QuadTree.make<string> 2 Vector2.Zero Vector2.One
        QuadTree.addElement true Vector2.Zero Vector2.Zero "Omni" tree
        QuadTree.addElement false Vector2.Zero Vector2.Zero "Zero" tree
        Assert.Equal (2, List.length ^ QuadTree.getElementsNearPoint Vector2.Zero tree)
        Assert.Equal (1, List.length ^ QuadTree.getElementsNearPoint Vector2.One tree)
        Assert.Equal (2, List.length ^ QuadTree.getElementsNearPoint (Vector2 0.5f) tree)
        Assert.Equal (1, List.length ^ QuadTree.getElementsNearPoint (Vector2 2.0f) tree)

    let [<Fact>] quadTreeThreeWorks () =
        let tree = QuadTree.make<string> 3 Vector2.Zero Vector2.One
        QuadTree.addElement true Vector2.Zero Vector2.Zero "Omni" tree
        QuadTree.addElement false Vector2.Zero Vector2.Zero "ZeroZero" tree
        QuadTree.addElement false (Vector2 0.5f) (Vector2 0.5f) "HalfHalf" tree
        QuadTree.addElement false Vector2.Zero Vector2.One "ZeroOne" tree
        QuadTree.addElement false Vector2.One Vector2.Zero "OneZero" tree
        Assert.Equal (3, List.length ^ QuadTree.getElementsNearPoint Vector2.Zero tree)
        Assert.Equal (4, List.length ^ QuadTree.getElementsNearPoint Vector2.One tree)
        Assert.Equal (3, List.length ^ QuadTree.getElementsNearPoint (Vector2 0.5f) tree)
        Assert.Equal (1, List.length ^ QuadTree.getElementsNearPoint (Vector2 2.0f) tree)