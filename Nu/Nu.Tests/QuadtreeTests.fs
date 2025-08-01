// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Tests
open System
open System.Collections.Generic
open System.Numerics
open NUnit.Framework
open Prime
open Nu

module QuadtreeTests =

    // Test data types and helpers
    type [<CustomEquality; NoComparison>] TestElement = 
        { Id: int; Name: string }
        override this.Equals(other) = 
            match other with 
            | :? TestElement as e -> this.Id = e.Id 
            | _ -> false
        override this.GetHashCode() = this.Id.GetHashCode()

    let makeTestElement id name = { Id = id; Name = name }
    
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
        Assert.Throws<System.Exception>(fun () -> 
            Quadtree.make<TestElement> 3 (v2 15.0f 15.0f) |> ignore) |> ignore

    [<Test>]
    let ``Quadtree leaf size calculation is correct`` () =
        let tree = makeTestTree 3 16.0f
        let leafSize = Quadtree.getLeafSize tree
        // With depth 3, leaf size should be 16 / 2^3 = 2
        Assert.Equal (v2 2.0f 2.0f, leafSize)

    [<Test>]
    let ``Adding element to empty tree increases element count`` () =
        let tree = makeTestTree 3 16.0f
        let element = makeTestQuadelement 1 "test1" true false Presence.Exterior Presence.Exterior (box2 (v2 0.0f 0.0f) (v2 1.0f 1.0f))
        let presence = Presence.Exterior
        let bounds = box2 (v2 0.0f 0.0f) (v2 1.0f 1.0f)
        
        Quadtree.addElement presence presence bounds element tree
        
        let results = HashSet<TestElement Quadelement>(QuadelementEqualityComparer<TestElement>())
        Quadtree.getElements results tree
        Assert.Equal (1, results.Count)

    [<Test>]
    let ``Adding multiple elements with different presence types`` () =
        let tree = makeTestTree 4 32.0f
        
        let element1 = makeTestQuadelement 1 "exterior" true false Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 2.0f 2.0f))
        let element2 = makeTestQuadelement 2 "interior" true false Presence.Interior Presence.Interior (box2 (v2 5.0f 5.0f) (v2 2.0f 2.0f))
        let element3 = makeTestQuadelement 3 "omnipresent" true false Presence.Omnipresent Presence.Omnipresent (box2 (v2 10.0f 10.0f) (v2 1.0f 1.0f))
        let element4 = makeTestQuadelement 4 "imposter" true false Presence.Imposter Presence.Imposter (box2 (v2 15.0f 15.0f) (v2 1.0f 1.0f))
        
        Quadtree.addElement Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 2.0f 2.0f)) element1 tree
        Quadtree.addElement Presence.Interior Presence.Interior (box2 (v2 5.0f 5.0f) (v2 2.0f 2.0f)) element2 tree
        Quadtree.addElement Presence.Omnipresent Presence.Omnipresent (box2 (v2 10.0f 10.0f) (v2 1.0f 1.0f)) element3 tree
        Quadtree.addElement Presence.Imposter Presence.Imposter (box2 (v2 15.0f 15.0f) (v2 1.0f 1.0f)) element4 tree
        
        let results = HashSet<TestElement Quadelement>(QuadelementEqualityComparer<TestElement>())
        Quadtree.getElements results tree
        Assert.Equal (4, results.Count)

    [<Test>]
    let ``Removing element from tree decreases element count`` () =
        let tree = makeTestTree 3 16.0f
        let element = makeTestQuadelement 1 "test1" true false Presence.Exterior Presence.Exterior (box2 (v2 0.0f 0.0f) (v2 1.0f 1.0f))
        let presence = Presence.Exterior
        let bounds = box2 (v2 0.0f 0.0f) (v2 1.0f 1.0f)
        
        // Add then remove
        Quadtree.addElement presence presence bounds element tree
        Quadtree.removeElement presence presence bounds element tree
        
        let results = HashSet<TestElement Quadelement>(QuadelementEqualityComparer<TestElement>())
        Quadtree.getElements results tree
        Assert.Equal (0, results.Count)

    [<Test>]
    let ``Updating element position works correctly`` () =
        let tree = makeTestTree 4 32.0f
        let element = makeTestQuadelement 1 "moving" true false Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 2.0f 2.0f))
        let presence = Presence.Exterior
        let boundsOld = box2 (v2 1.0f 1.0f) (v2 2.0f 2.0f)
        let boundsNew = box2 (v2 10.0f 10.0f) (v2 2.0f 2.0f)
        
        // Add element at initial position
        Quadtree.addElement presence presence boundsOld element tree
        
        // Update to new position
        let elementNew = makeTestQuadelement 1 "moving" true false Presence.Exterior Presence.Exterior boundsNew
        Quadtree.updateElement presence presence boundsOld presence presence boundsNew elementNew tree
        
        // Query at old position should return nothing
        let resultsOld = HashSet<TestElement Quadelement>(QuadelementEqualityComparer<TestElement>())
        Quadtree.getElementsAtPoint (v2 2.0f 2.0f) resultsOld tree
        Assert.Equal (0, resultsOld.Count)
        
        // Query at new position should return the element
        let resultsNew = HashSet<TestElement Quadelement>(QuadelementEqualityComparer<TestElement>())
        Quadtree.getElementsAtPoint (v2 11.0f 11.0f) resultsNew tree
        Assert.Equal (1, resultsNew.Count)

    [<Test>]
    let ``Query at point returns correct elements`` () =
        let tree = makeTestTree 4 32.0f
        let element1 = makeTestQuadelement 1 "at-origin" true false Presence.Exterior Presence.Exterior (box2 (v2 -1.0f -1.0f) (v2 2.0f 2.0f))
        let element2 = makeTestQuadelement 2 "far-away" true false Presence.Exterior Presence.Exterior (box2 (v2 10.0f 10.0f) (v2 2.0f 2.0f))
        
        Quadtree.addElement Presence.Exterior Presence.Exterior (box2 (v2 -1.0f -1.0f) (v2 2.0f 2.0f)) element1 tree
        Quadtree.addElement Presence.Exterior Presence.Exterior (box2 (v2 10.0f 10.0f) (v2 2.0f 2.0f)) element2 tree
        
        // Query at origin should return only first element
        let results = HashSet<TestElement Quadelement>(QuadelementEqualityComparer<TestElement>())
        Quadtree.getElementsAtPoint (v2 0.0f 0.0f) results tree
        let elements = collectElements results
        Assert.Equal (1, elements.Length)
        Assert.Equal (1, elements.[0].Entry.Id)

    [<Test>]
    let ``Query in bounds returns intersecting elements`` () =
        let tree = makeTestTree 4 32.0f
        let element1 = makeTestQuadelement 1 "inside" true false Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 2.0f 2.0f))
        let element2 = makeTestQuadelement 2 "outside" true false Presence.Exterior Presence.Exterior (box2 (v2 10.0f 10.0f) (v2 2.0f 2.0f))
        let element3 = makeTestQuadelement 3 "overlapping" true false Presence.Exterior Presence.Exterior (box2 (v2 4.0f 4.0f) (v2 2.0f 2.0f))
        
        Quadtree.addElement Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 2.0f 2.0f)) element1 tree
        Quadtree.addElement Presence.Exterior Presence.Exterior (box2 (v2 10.0f 10.0f) (v2 2.0f 2.0f)) element2 tree
        Quadtree.addElement Presence.Exterior Presence.Exterior (box2 (v2 4.0f 4.0f) (v2 2.0f 2.0f)) element3 tree
        
        // Query bounds that should intersect elements 1 and 3
        let queryBounds = box2 (v2 0.0f 0.0f) (v2 5.0f 5.0f)
        let results = HashSet<TestElement Quadelement>(QuadelementEqualityComparer<TestElement>())
        Quadtree.getElementsInBounds queryBounds results tree
        let elements = collectElements results
        Assert.Equal (2, elements.Length)
        Assert.Equal (1, elements.[0].Entry.Id)
        Assert.Equal (3, elements.[1].Entry.Id)

    [<Test>]
    let ``Query in view returns only visible elements`` () =
        let tree = makeTestTree 4 32.0f
        let element1 = makeTestQuadelement 1 "visible" true false Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 2.0f 2.0f))
        let element2 = makeTestQuadelement 2 "invisible" false false Presence.Exterior Presence.Exterior (box2 (v2 3.0f 3.0f) (v2 2.0f 2.0f))
        
        Quadtree.addElement Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 2.0f 2.0f)) element1 tree
        Quadtree.addElement Presence.Exterior Presence.Exterior (box2 (v2 3.0f 3.0f) (v2 2.0f 2.0f)) element2 tree
        
        let queryBounds = box2 (v2 0.0f 0.0f) (v2 6.0f 6.0f)
        let results = HashSet<TestElement Quadelement>(QuadelementEqualityComparer<TestElement>())
        Quadtree.getElementsInView queryBounds results tree
        let elements = collectElements results
        Assert.Equal (1, elements.Length)
        Assert.Equal (1, elements.[0].Entry.Id)

    [<Test>]
    let ``Query in play returns non-static elements`` () =
        let tree = makeTestTree 4 32.0f
        let element1 = makeTestQuadelement 1 "dynamic" true false Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 2.0f 2.0f))
        let element2 = makeTestQuadelement 2 "static" true true Presence.Exterior Presence.Exterior (box2 (v2 3.0f 3.0f) (v2 2.0f 2.0f))
        
        Quadtree.addElement Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 2.0f 2.0f)) element1 tree
        Quadtree.addElement Presence.Exterior Presence.Exterior (box2 (v2 3.0f 3.0f) (v2 2.0f 2.0f)) element2 tree
        
        let queryBounds = box2 (v2 0.0f 0.0f) (v2 6.0f 6.0f)
        let results = HashSet<TestElement Quadelement>(QuadelementEqualityComparer<TestElement>())
        Quadtree.getElementsInPlay queryBounds results tree
        let elements = collectElements results
        Assert.Equal (1, elements.Length)
        Assert.Equal (1, elements.[0].Entry.Id)

    [<Test>]
    let ``Omnipresent elements appear in all queries`` () =
        let tree = makeTestTree 4 32.0f
        let element = makeTestQuadelement 1 "omnipresent" true false Presence.Omnipresent Presence.Omnipresent (box2 (v2 100.0f 100.0f) (v2 1.0f 1.0f))
        
        Quadtree.addElement Presence.Omnipresent Presence.Omnipresent (box2 (v2 100.0f 100.0f) (v2 1.0f 1.0f)) element tree
        
        // Query at different locations should all return the omnipresent element
        let locations = [v2 0.0f 0.0f; v2 15.0f 15.0f; v2 -10.0f -10.0f]
        for location in locations do
            let results = HashSet<TestElement Quadelement>(QuadelementEqualityComparer<TestElement>())
            Quadtree.getElementsAtPoint location results tree
            Assert.Equal (1, results.Count)

    [<Test>]
    let ``Large elements go to ubiquitous fallback`` () =
        let tree = makeTestTree 4 32.0f
        // Create element larger than the magnitude threshold
        let largeBounds = box2 (v2 0.0f 0.0f) (v2 1000.0f 1000.0f)
        let element = makeTestQuadelement 1 "large" true false Presence.Exterior Presence.Exterior largeBounds
        
        Quadtree.addElement Presence.Exterior Presence.Exterior largeBounds element tree
        
        // Should still be queryable
        let results = HashSet<TestElement Quadelement>(QuadelementEqualityComparer<TestElement>())
        Quadtree.getElementsAtPoint (v2 500.0f 500.0f) results tree
        Assert.Equal (1, results.Count)

    [<Test>]
    let ``Elements outside tree bounds go to ubiquitous fallback`` () =
        let tree = makeTestTree 3 16.0f
        let bounds = Quadtree.getBounds tree
        // Create element outside tree bounds
        let outsideBounds = box2 (v2 (bounds.Max.X + 10.0f) (bounds.Max.Y + 10.0f)) (v2 2.0f 2.0f)
        let element = makeTestQuadelement 1 "outside" true false Presence.Exterior Presence.Exterior outsideBounds
        
        Quadtree.addElement Presence.Exterior Presence.Exterior outsideBounds element tree
        
        // Should still be queryable
        let results = HashSet<TestElement Quadelement>(QuadelementEqualityComparer<TestElement>())
        Quadtree.getElements results tree
        Assert.Equal (1, results.Count)

    [<Test>]
    let ``Clear removes all elements`` () =
        let tree = makeTestTree 4 32.0f
        
        // Add multiple elements
        for i in 1..5 do
            let element = makeTestQuadelement i $"element{i}" true false Presence.Exterior Presence.Exterior (box2 (v2 (float32 i) (float32 i)) (v2 1.0f 1.0f))
            Quadtree.addElement Presence.Exterior Presence.Exterior (box2 (v2 (float32 i) (float32 i)) (v2 1.0f 1.0f)) element tree
        
        // Verify elements were added
        let resultsBefore = HashSet<TestElement Quadelement>(QuadelementEqualityComparer<TestElement>())
        Quadtree.getElements resultsBefore tree
        Assert.Equal (5, resultsBefore.Count)
        
        // Clear tree
        Quadtree.clear tree
        
        // Verify all elements removed
        let resultsAfter = HashSet<TestElement Quadelement>(QuadelementEqualityComparer<TestElement>())
        Quadtree.getElements resultsAfter tree
        Assert.Equal (0, resultsAfter.Count)

    [<Test>]
    let ``Sweep removes unused nodes`` () =
        let tree = makeTestTree 4 32.0f
        let element = makeTestQuadelement 1 "temporary" true false Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 1.0f 1.0f))
        
        // Add element to create internal structure
        Quadtree.addElement Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 1.0f 1.0f)) element tree
        
        // Remove element to leave empty nodes
        Quadtree.removeElement Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 1.0f 1.0f)) element tree
        
        // Sweep should clean up unused nodes (this is mostly to ensure it doesn't crash)
        Quadtree.sweep tree
        
        // Tree should still be functional
        let results = HashSet<TestElement Quadelement>(QuadelementEqualityComparer<TestElement>())
        Quadtree.getElements results tree
        Assert.Equal (0, results.Count)

    [<Test>]
    let ``Adding same element multiple times handles correctly`` () =
        let tree = makeTestTree 4 32.0f
        let element = makeTestQuadelement 1 "duplicate" true false Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 1.0f 1.0f))
        let bounds = box2 (v2 1.0f 1.0f) (v2 1.0f 1.0f)
        
        // Add same element multiple times
        Quadtree.addElement Presence.Exterior Presence.Exterior bounds element tree
        Quadtree.addElement Presence.Exterior Presence.Exterior bounds element tree
        Quadtree.addElement Presence.Exterior Presence.Exterior bounds element tree
        
        // Should only appear once in results
        let results = HashSet<TestElement Quadelement>(QuadelementEqualityComparer<TestElement>())
        Quadtree.getElements results tree
        Assert.Equal (1, results.Count)

    [<Test>]
    let ``Removing non-existent element handles gracefully`` () =
        let tree = makeTestTree 4 32.0f
        let element = makeTestQuadelement 1 "nonexistent" true false Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 1.0f 1.0f))
        
        // Remove element that was never added
        Quadtree.removeElement Presence.Exterior Presence.Exterior (box2 (v2 1.0f 1.0f) (v2 1.0f 1.0f)) element tree
        
        // Tree should still be functional
        let results = HashSet<TestElement Quadelement>(QuadelementEqualityComparer<TestElement>())
        Quadtree.getElements results tree
        Assert.Equal (0, results.Count)

    [<Test>]
    let ``Stress test with many elements`` () =
        let tree = makeTestTree 6 64.0f
        let elementCount = 100
        
        // Add many elements
        for i in 1..elementCount do
            let x = (float32 i % 60.0f) - 30.0f
            let y = (float32 (i * 7) % 60.0f) - 30.0f
            let bounds = box2 (v2 x y) (v2 1.0f 1.0f)
            let element = makeTestQuadelement i $"stress{i}" true false Presence.Exterior Presence.Exterior bounds
            Quadtree.addElement Presence.Exterior Presence.Exterior bounds element tree
        
        // Verify all elements were added
        let results = HashSet<TestElement Quadelement>(QuadelementEqualityComparer<TestElement>())
        Quadtree.getElements results tree
        Assert.Equal (elementCount, results.Count)
        
        // Test spatial queries still work
        let queryResults = HashSet<TestElement Quadelement>(QuadelementEqualityComparer<TestElement>())
        Quadtree.getElementsInBounds (box2 (v2 -5.0f -5.0f) (v2 10.0f 10.0f)) queryResults tree
        Assert.True (queryResults.Count > 0)
        Assert.True (queryResults.Count <= elementCount)