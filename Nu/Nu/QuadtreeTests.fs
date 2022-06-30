// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Tests
open System
open System.Collections.Generic
open System.Numerics
open Xunit
open Prime
open Nu
module QuadtreeTests =

    let [<Fact>] quadtreeTwoWorks () =
        let tree = Quadtree.make<string> 2 2 (Box2 (0.0f, 0.0f, 1.0f, 1.0f))
        Quadtree.addElement true (Box2 (0.0f, 0.0f, 0.0f, 0.0f)) "Omni" tree
        Quadtree.addElement false (Box2 (0.0f, 0.0f, 0.0f, 0.0f)) "Zero" tree
        Assert.Equal (2, Seq.length (Quadtree.getElementsAtPoint Vector2.Zero (HashSet ()) tree))
        Assert.Equal (2, Seq.length (Quadtree.getElementsAtPoint Vector2.One (HashSet ()) tree))
        Assert.Equal (2, Seq.length (Quadtree.getElementsAtPoint (Vector2 0.5f) (HashSet ()) tree))
        Assert.Equal (2, Seq.length (Quadtree.getElementsAtPoint (Vector2 2.0f) (HashSet ()) tree))

    let [<Fact>] quadtreeThreeWorks () =
        let tree = Quadtree.make<string> 2 3 (Box2 (0.0f, 0.0f, 1.0f, 1.0f))
        Quadtree.addElement true (Box2 (0.0f, 0.0f, 0.0f, 0.0f)) "Omni" tree
        Quadtree.addElement false (Box2 (0.0f, 0.0f, 0.0f, 0.0f)) "Zero" tree
        Quadtree.addElement false (Box2 (0.5f, 0.5f, 0.5f, 0.5f)) "Half" tree
        Quadtree.addElement false (Box2 (0.0f, 0.0f, 1.0f, 1.0f)) "ZeroOne" tree
        Quadtree.addElement false (Box2 (1.0f, 1.0f, 0.0f, 0.0f)) "OneZero" tree
        Assert.Equal (4, Seq.length (Quadtree.getElementsAtPoint Vector2.Zero (HashSet ()) tree))
        Assert.Equal (5, Seq.length (Quadtree.getElementsAtPoint Vector2.One (HashSet ()) tree))
        Assert.Equal (5, Seq.length (Quadtree.getElementsAtPoint (Vector2 0.5f) (HashSet ()) tree))
        Assert.Equal (3, Seq.length (Quadtree.getElementsAtPoint (Vector2 2.0f) (HashSet ()) tree))