// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Tests
open System
open System.Collections.Generic
open System.Numerics
open NUnit.Framework
open Prime
open Nu
open Nu.Tests
module QuadtreeTests =

    type [<CustomEquality; NoComparison>] TestElement = 
        { Id : int
          Name : string }

        override this.Equals that = 
            match that with 
            | :? TestElement as e -> this.Id = e.Id 
            | _ -> false

        override this.GetHashCode () =
            this.Id.GetHashCode ()

    let makeTestElement id name =
        { Id = id; Name = name }
    
    let makeTestQuadelement id name visibleInView staticInPlay presence presenceInPlay bounds =
        let element = makeTestElement id name
        Quadelement.make visibleInView staticInPlay presence presenceInPlay bounds element

    // Helper to create a standard test tree
    let makeTestTree depth size =
        Quadtree.make<TestElement> depth (v2 size size)

    // Helper to collect elements from HashSet into list for easier testing
    let collectElements (set: TestElement Quadelement HashSet) =
        set |> Seq.toList |> List.sortBy (fun e -> e.Entry.Id)

    [<Test>]
    let ``Quadtree creation with valid parameters succeeds`` () =
        let tree = makeTestTree 3 16.0f
        Assert.Equal (3, Quadtree.getDepth tree)
        let bounds = Quadtree.getBounds tree
        Assert.Equal (v2 16.0f 16.0f, bounds.Size)

    [<Test>]
    let ``Quadtree creation with power of two size succeeds`` () =
        let tree = Quadtree.make<TestElement> 4 (v2 32.0f 64.0f)
        Assert.Equal (4, Quadtree.getDepth tree)
        let bounds = Quadtree.getBounds tree
        Assert.Equal (v2 32.0f 64.0f, bounds.Size)

    [<Test>]
    let ``Quadtree creation with non-power-of-two size fails`` () =
        Assert.Throws<Exception> (fun () -> 
            Quadtree.make<TestElement> 3 (v2 15.0f 15.0f) |> ignore) |> ignore

    [<Test>]
    let ``Quadtree leaf size calculation is correct`` () =
        let tree = makeTestTree 3 16.0f
        let leafSize = Quadtree.getLeafSize tree
        Assert.Equal (v2 2.0f 2.0f, leafSize) // with depth 3, leaf size should be 16 / 2^3 = 2

    [<Test>]
    let ``Adding element to empty tree increases element count`` () =
        
        // create empty tree and add element
        let tree = makeTestTree 3 16.0f
        let element = makeTestQuadelement 1 "test1" true false Presence.Exterior Presence.Exterior (box2 (v2 0.0f 0.0f) (v2 1.0f 1.0f))
        let presence = Presence.Exterior
        let bounds = box2 (v2 0.0f 0.0f) (v2 1.0f 1.0f)
        Quadtree.addElement presence presence bounds element tree
        
        // verify element count is one
        let results = HashSet<TestElement Quadelement> (QuadelementEqualityComparer<TestElement> ())
        Quadtree.getElements results tree
        Assert.Equal (1, results.Count)

    [<Test>]
    let ``Adding multiple elements with different presence types`` () =
        
        // create tree and add elements with different presence types
        let tree = makeTestTree 4 32.0f
        let element1 = makeTestQuadelement 1 "exterior" true false Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 2.0f 2.0f))
        let element2 = makeTestQuadelement 2 "interior" true false Presence.Interior Presence.Interior (box2 (v2 5.0f 5.0f) (v2 2.0f 2.0f))
        let element3 = makeTestQuadelement 3 "omnipresent" true false Presence.Omnipresent Presence.Omnipresent (box2 (v2 10.0f 10.0f) (v2 1.0f 1.0f))
        let element4 = makeTestQuadelement 4 "imposter" true false Presence.Imposter Presence.Imposter (box2 (v2 15.0f 15.0f) (v2 1.0f 1.0f))
        
        // add elements with different presence types
        Quadtree.addElement Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 2.0f 2.0f)) element1 tree
        Quadtree.addElement Presence.Interior Presence.Interior (box2 (v2 5.0f 5.0f) (v2 2.0f 2.0f)) element2 tree
        Quadtree.addElement Presence.Omnipresent Presence.Omnipresent (box2 (v2 10.0f 10.0f) (v2 1.0f 1.0f)) element3 tree
        Quadtree.addElement Presence.Imposter Presence.Imposter (box2 (v2 15.0f 15.0f) (v2 1.0f 1.0f)) element4 tree
        
        // verify all elements were added
        let results = HashSet<TestElement Quadelement> (QuadelementEqualityComparer<TestElement> ())
        Quadtree.getElements results tree
        Assert.Equal (4, results.Count)

    [<Test>]
    let ``Removing element from tree decreases element count`` () =
        
        // add then remove
        let tree = makeTestTree 3 16.0f
        let element = makeTestQuadelement 1 "test1" true false Presence.Exterior Presence.Exterior (box2 (v2 0.0f 0.0f) (v2 1.0f 1.0f))
        let presence = Presence.Exterior
        let bounds = box2 (v2 0.0f 0.0f) (v2 1.0f 1.0f)
        Quadtree.addElement presence presence bounds element tree
        Quadtree.removeElement presence presence bounds element tree
        
        // verify element count is zero
        let results = HashSet<TestElement Quadelement> (QuadelementEqualityComparer<TestElement> ())
        Quadtree.getElements results tree
        Assert.Equal (0, results.Count)

    [<Test>]
    let ``Updating element position works correctly`` () =
        
        // add element at initial position
        let tree = makeTestTree 4 32.0f
        let element = makeTestQuadelement 1 "moving" true false Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 2.0f 2.0f))
        let presence = Presence.Exterior
        let boundsOld = box2 (v2 1.0f 1.0f) (v2 2.0f 2.0f)
        let boundsNew = box2 (v2 10.0f 10.0f) (v2 2.0f 2.0f)
        Quadtree.addElement presence presence boundsOld element tree
        
        // update to new position
        let elementNew = makeTestQuadelement 1 "moving" true false Presence.Exterior Presence.Exterior boundsNew
        Quadtree.updateElement presence presence boundsOld presence presence boundsNew elementNew tree
        
        // query at old position should return nothing
        let resultsOld = HashSet<TestElement Quadelement> (QuadelementEqualityComparer<TestElement> ())
        Quadtree.getElementsAtPoint (v2 2.0f 2.0f) resultsOld tree
        Assert.Equal (0, resultsOld.Count)
        
        // query at new position should return the element
        let resultsNew = HashSet<TestElement Quadelement> (QuadelementEqualityComparer<TestElement> ())
        Quadtree.getElementsAtPoint (v2 11.0f 11.0f) resultsNew tree
        Assert.Equal (1, resultsNew.Count)

    [<Test>]
    let ``Query at point returns correct elements`` () =
        
        // create elements at different locations
        let tree = makeTestTree 4 32.0f
        let element1 = makeTestQuadelement 1 "at-origin" true false Presence.Exterior Presence.Exterior (box2 (v2 -1.0f -1.0f) (v2 2.0f 2.0f))
        let element2 = makeTestQuadelement 2 "far-away" true false Presence.Exterior Presence.Exterior (box2 (v2 10.0f 10.0f) (v2 2.0f 2.0f))
        Quadtree.addElement Presence.Exterior Presence.Exterior (box2 (v2 -1.0f -1.0f) (v2 2.0f 2.0f)) element1 tree
        Quadtree.addElement Presence.Exterior Presence.Exterior (box2 (v2 10.0f 10.0f) (v2 2.0f 2.0f)) element2 tree
        
        // query at origin should return only first element
        let results = HashSet<TestElement Quadelement> (QuadelementEqualityComparer<TestElement> ())
        Quadtree.getElementsAtPoint (v2 0.0f 0.0f) results tree
        let elements = collectElements results
        Assert.Equal (1, elements.Length)
        Assert.Equal (1, elements.[0].Entry.Id)

    [<Test>]
    let ``Query in bounds returns intersecting elements`` () =
        
        // create elements with different bounds
        let tree = makeTestTree 4 32.0f
        let element1 = makeTestQuadelement 1 "inside" true false Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 2.0f 2.0f))
        let element2 = makeTestQuadelement 2 "outside" true false Presence.Exterior Presence.Exterior (box2 (v2 10.0f 10.0f) (v2 2.0f 2.0f))
        let element3 = makeTestQuadelement 3 "overlapping" true false Presence.Exterior Presence.Exterior (box2 (v2 4.0f 4.0f) (v2 2.0f 2.0f))
        Quadtree.addElement Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 2.0f 2.0f)) element1 tree
        Quadtree.addElement Presence.Exterior Presence.Exterior (box2 (v2 10.0f 10.0f) (v2 2.0f 2.0f)) element2 tree
        Quadtree.addElement Presence.Exterior Presence.Exterior (box2 (v2 4.0f 4.0f) (v2 2.0f 2.0f)) element3 tree
        
        // query bounds that should intersect elements 1 and 3
        let queryBounds = box2 (v2 0.0f 0.0f) (v2 5.0f 5.0f)
        let results = HashSet<TestElement Quadelement> (QuadelementEqualityComparer<TestElement> ())
        Quadtree.getElementsInBounds queryBounds results tree
        let elements = collectElements results
        Assert.Equal (2, elements.Length)
        Assert.Equal (1, elements.[0].Entry.Id)
        Assert.Equal (3, elements.[1].Entry.Id)

    [<Test>]
    let ``Query in view returns only visible elements`` () =

        // create elements with different visibility states
        let tree = makeTestTree 4 32.0f
        let element1 = makeTestQuadelement 1 "visible" true false Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 2.0f 2.0f))
        let element2 = makeTestQuadelement 2 "invisible" false false Presence.Exterior Presence.Exterior (box2 (v2 3.0f 3.0f) (v2 2.0f 2.0f))
        Quadtree.addElement Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 2.0f 2.0f)) element1 tree
        Quadtree.addElement Presence.Exterior Presence.Exterior (box2 (v2 3.0f 3.0f) (v2 2.0f 2.0f)) element2 tree
        
        // query bounds that should intersect visible element
        let queryBounds = box2 (v2 0.0f 0.0f) (v2 6.0f 6.0f)
        let results = HashSet<TestElement Quadelement> (QuadelementEqualityComparer<TestElement> ())
        Quadtree.getElementsInView queryBounds results tree
        let elements = collectElements results
        Assert.Equal (1, elements.Length)
        Assert.Equal (1, elements.[0].Entry.Id)

    [<Test>]
    let ``Query in play returns non-static elements`` () =

        // create elements with different presence types
        let tree = makeTestTree 4 32.0f
        let element1 = makeTestQuadelement 1 "dynamic" true false Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 2.0f 2.0f))
        let element2 = makeTestQuadelement 2 "static" true true Presence.Exterior Presence.Exterior (box2 (v2 3.0f 3.0f) (v2 2.0f 2.0f))
        Quadtree.addElement Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 2.0f 2.0f)) element1 tree
        Quadtree.addElement Presence.Exterior Presence.Exterior (box2 (v2 3.0f 3.0f) (v2 2.0f 2.0f)) element2 tree
        
        let queryBounds = box2 (v2 0.0f 0.0f) (v2 6.0f 6.0f)
        let results = HashSet<TestElement Quadelement> (QuadelementEqualityComparer<TestElement> ())
        Quadtree.getElementsInPlay queryBounds results tree
        let elements = collectElements results
        Assert.Equal (1, elements.Length)
        Assert.Equal (1, elements.[0].Entry.Id)

    [<Test>]
    let ``Omnipresent elements appear in all queries`` () =
    
        // create omnipresent element that should be returned in all queries
        let tree = makeTestTree 4 32.0f
        let element = makeTestQuadelement 1 "omnipresent" true false Presence.Omnipresent Presence.Omnipresent (box2 (v2 100.0f 100.0f) (v2 1.0f 1.0f))
        Quadtree.addElement Presence.Omnipresent Presence.Omnipresent (box2 (v2 100.0f 100.0f) (v2 1.0f 1.0f)) element tree
        
        // Query at different locations should all return the omnipresent element
        let locations = [v2 0.0f 0.0f; v2 15.0f 15.0f; v2 -10.0f -10.0f]
        for location in locations do
            let results = HashSet<TestElement Quadelement> (QuadelementEqualityComparer<TestElement> ())
            Quadtree.getElementsAtPoint location results tree
            Assert.Equal (1, results.Count)

    [<Test>]
    let ``Large elements go to ubiquitous fallback`` () =

        // create element larger than the magnitude threshold
        let tree = makeTestTree 4 32.0f
        let largeBounds = box2 (v2 0.0f 0.0f) (v2 1000.0f 1000.0f)
        let element = makeTestQuadelement 1 "large" true false Presence.Exterior Presence.Exterior largeBounds
        Quadtree.addElement Presence.Exterior Presence.Exterior largeBounds element tree
        
        // should still be queryable
        let results = HashSet<TestElement Quadelement> (QuadelementEqualityComparer<TestElement> ())
        Quadtree.getElementsAtPoint (v2 500.0f 500.0f) results tree
        Assert.Equal (1, results.Count)

    [<Test>]
    let ``Elements outside tree bounds go to ubiquitous fallback`` () =

        // create element outside tree bounds
        let tree = makeTestTree 3 16.0f
        let bounds = Quadtree.getBounds tree
        let outsideBounds = box2 (v2 (bounds.Max.X + 10.0f) (bounds.Max.Y + 10.0f)) (v2 2.0f 2.0f)
        let element = makeTestQuadelement 1 "outside" true false Presence.Exterior Presence.Exterior outsideBounds
        Quadtree.addElement Presence.Exterior Presence.Exterior outsideBounds element tree
        
        // should still be queryable
        let results = HashSet<TestElement Quadelement> (QuadelementEqualityComparer<TestElement> ())
        Quadtree.getElements results tree
        Assert.Equal (1, results.Count)

    [<Test>]
    let ``Clear removes all elements`` () =
        
        // add multiple elements
        let tree = makeTestTree 4 32.0f
        for i in 1..5 do
            let element = makeTestQuadelement i $"element{i}" true false Presence.Exterior Presence.Exterior (box2 (v2 (single i) (single i)) (v2 1.0f 1.0f))
            Quadtree.addElement Presence.Exterior Presence.Exterior (box2 (v2 (single i) (single i)) (v2 1.0f 1.0f)) element tree
        
        // verify elements were added
        let resultsBefore = HashSet<TestElement Quadelement> (QuadelementEqualityComparer<TestElement> ())
        Quadtree.getElements resultsBefore tree
        Assert.Equal (5, resultsBefore.Count)
        
        // clear tree
        Quadtree.clear tree
        
        // verify all elements removed
        let resultsAfter = HashSet<TestElement Quadelement> (QuadelementEqualityComparer<TestElement> ())
        Quadtree.getElements resultsAfter tree
        Assert.Equal (0, resultsAfter.Count)

    [<Test>]
    let ``Sweep removes unused nodes`` () =
        
        // add element to create internal structure
        let tree = makeTestTree 4 32.0f
        let element = makeTestQuadelement 1 "temporary" true false Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 1.0f 1.0f))
        Quadtree.addElement Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 1.0f 1.0f)) element tree
        
        // remove element to leave empty nodes
        Quadtree.removeElement Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 1.0f 1.0f)) element tree
        
        // sweep should clean up unused nodes (this is mostly to ensure it doesn't crash)
        Quadtree.sweep tree
        
        // tree should still be functional
        let results = HashSet<TestElement Quadelement> (QuadelementEqualityComparer<TestElement> ())
        Quadtree.getElements results tree
        Assert.Equal (0, results.Count)

    [<Test>]
    let ``Adding same element multiple times handles correctly`` () =
        
        // add same element multiple times
        let tree = makeTestTree 4 32.0f
        let element = makeTestQuadelement 1 "duplicate" true false Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 1.0f 1.0f))
        let bounds = box2 (v2 1.0f 1.0f) (v2 1.0f 1.0f)
        Quadtree.addElement Presence.Exterior Presence.Exterior bounds element tree
        Quadtree.addElement Presence.Exterior Presence.Exterior bounds element tree
        Quadtree.addElement Presence.Exterior Presence.Exterior bounds element tree
        
        // should only appear once in results
        let results = HashSet<TestElement Quadelement> (QuadelementEqualityComparer<TestElement> ())
        Quadtree.getElements results tree
        Assert.Equal (1, results.Count)

    [<Test>]
    let ``Removing non-existent element handles gracefully`` () =
        
        // remove element that was never added
        let tree = makeTestTree 4 32.0f
        let element = makeTestQuadelement 1 "nonexistent" true false Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 1.0f 1.0f))
        Quadtree.removeElement Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 1.0f 1.0f)) element tree
        
        // tree should still be functional
        let results = HashSet<TestElement Quadelement> (QuadelementEqualityComparer<TestElement> ())
        Quadtree.getElements results tree
        Assert.Equal (0, results.Count)

    [<Test>]
    let ``Stress test with many elements`` () =
        
        // add many elements
        let tree = makeTestTree 6 64.0f
        let elementCount = 100
        for i in 1..elementCount do
            let x = (single i % 60.0f) - 30.0f
            let y = (single (i * 7) % 60.0f) - 30.0f
            let bounds = box2 (v2 x y) (v2 1.0f 1.0f)
            let element = makeTestQuadelement i $"stress{i}" true false Presence.Exterior Presence.Exterior bounds
            Quadtree.addElement Presence.Exterior Presence.Exterior bounds element tree
        
        // verify all elements were added
        let results = HashSet<TestElement Quadelement> (QuadelementEqualityComparer<TestElement> ())
        Quadtree.getElements results tree
        Assert.Equal (elementCount, results.Count)
        
        // test spatial queries still work
        let queryResults = HashSet<TestElement Quadelement> (QuadelementEqualityComparer<TestElement> ())
        Quadtree.getElementsInBounds (box2 (v2 -5.0f -5.0f) (v2 10.0f 10.0f)) queryResults tree
        Assert.True (queryResults.Count > 0)
        Assert.True (queryResults.Count <= elementCount)