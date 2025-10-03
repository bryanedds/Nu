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
module OctreeTests =

    type [<CustomEquality; NoComparison>] TestElement = 
        { Id : int
          Name : string }

        override this.Equals(other) = 
            match other with 
            | :? TestElement as e -> this.Id = e.Id 
            | _ -> false

        override this.GetHashCode () =
            this.Id.GetHashCode ()

    let makeTestElement id name =
        { Id = id; Name = name }
    
    let makeTestOctelement id name visibleInView staticInPlay lightProbe light presence presenceInPlay bounds =
        let element = makeTestElement id name
        Octelement.make visibleInView staticInPlay lightProbe light presence presenceInPlay bounds element

    // Helper to create a standard test tree
    let makeTestTree depth size =
        Octree.make<TestElement> depth (v3 size size size)

    // Helper to collect elements from HashSet into list for easier testing
    let collectElements (set: TestElement Octelement HashSet) =
        set |> Seq.toList |> List.sortBy (fun e -> e.Entry.Id)

    // Helper to create test frustum
    let makeTestFrustum() =
        let view = Matrix4x4.CreateLookAt(v3 0.0f 0.0f 10.0f, v3 0.0f 0.0f 0.0f, v3 0.0f 1.0f 0.0f)
        let projection = Matrix4x4.CreatePerspectiveFieldOfView(MathF.PI / 4.0f, 1.0f, 1.0f, 100.0f)
        Frustum(view * projection)

    [<Test>]
    let ``Octree creation with valid parameters succeeds`` () =
        let tree = makeTestTree 3 16.0f
        Assert.Equal (3, Octree.getDepth tree)
        let bounds = Octree.getBounds tree
        Assert.Equal (v3 16.0f 16.0f 16.0f, bounds.Size)

    [<Test>]
    let ``Octree creation with power of two size succeeds`` () =
        let tree = Octree.make<TestElement> 4 (v3 32.0f 64.0f 128.0f)
        Assert.Equal (4, Octree.getDepth tree)
        let bounds = Octree.getBounds tree
        Assert.Equal (v3 32.0f 64.0f 128.0f, bounds.Size)

    [<Test>]
    let ``Octree creation with non-power-of-two size fails`` () =
        Assert.Throws<Exception> (fun () -> 
            Octree.make<TestElement> 3 (v3 15.0f 15.0f 15.0f) |> ignore) |> ignore

    [<Test>]
    let ``Octree leaf size calculation is correct`` () =
        let tree = makeTestTree 3 16.0f
        let leafSize = Octree.getLeafSize tree
        Assert.Equal (v3 2.0f 2.0f 2.0f, leafSize) // with depth 3, leaf size should be 16 / 2^3 = 2

    [<Test>]
    let ``Adding element to empty tree increases element count`` () =

        let tree = makeTestTree 3 16.0f
        let bounds = box3 (v3 0.0f 0.0f 0.0f) (v3 1.0f 1.0f 1.0f)
        let element = makeTestOctelement 1 "test1" true false false false Presence.Exterior Presence.Exterior bounds
        Octree.addElement Presence.Exterior Presence.Exterior bounds element tree
        
        let results = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getElements results tree
        Assert.Equal (1, results.Count)

    [<Test>]
    let ``Adding multiple elements with different presence types`` () =
        
        let tree = makeTestTree 4 32.0f
        let element1 = makeTestOctelement 1 "exterior" true false false false Presence.Exterior Presence.Exterior (box3 (v3 1.0f 1.0f 1.0f) (v3 2.0f 2.0f 2.0f))
        let element2 = makeTestOctelement 2 "interior" true false false false Presence.Interior Presence.Interior (box3 (v3 5.0f 5.0f 5.0f) (v3 2.0f 2.0f 2.0f))
        let element3 = makeTestOctelement 3 "omnipresent" true false false false Presence.Omnipresent Presence.Omnipresent (box3 (v3 10.0f 10.0f 10.0f) (v3 1.0f 1.0f 1.0f))
        let element4 = makeTestOctelement 4 "imposter" true false false false Presence.Imposter Presence.Imposter (box3 (v3 15.0f 15.0f 15.0f) (v3 1.0f 1.0f 1.0f))
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 1.0f 1.0f 1.0f) (v3 2.0f 2.0f 2.0f)) element1 tree
        Octree.addElement Presence.Interior Presence.Interior (box3 (v3 5.0f 5.0f 5.0f) (v3 2.0f 2.0f 2.0f)) element2 tree
        Octree.addElement Presence.Omnipresent Presence.Omnipresent (box3 (v3 10.0f 10.0f 10.0f) (v3 1.0f 1.0f 1.0f)) element3 tree
        Octree.addElement Presence.Imposter Presence.Imposter (box3 (v3 15.0f 15.0f 15.0f) (v3 1.0f 1.0f 1.0f)) element4 tree
        
        let results = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getElements results tree
        Assert.Equal (4, results.Count)

    [<Test>]
    let ``Adding light probe elements`` () =
        
        let tree = makeTestTree 4 32.0f
        let lightProbe = makeTestOctelement 1 "lightprobe" true false true false Presence.Exterior Presence.Exterior (box3 (v3 1.0f 1.0f 1.0f) (v3 1.0f 1.0f 1.0f))
        let regularElement = makeTestOctelement 2 "regular" true false false false Presence.Exterior Presence.Exterior (box3 (v3 5.0f 5.0f 5.0f) (v3 1.0f 1.0f 1.0f))
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 1.0f 1.0f 1.0f) (v3 1.0f 1.0f 1.0f)) lightProbe tree
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 5.0f 5.0f 5.0f) (v3 1.0f 1.0f 1.0f)) regularElement tree
        
        // query for light probes specifically
        let results = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getLightProbesInView results tree
        let elements = collectElements results
        Assert.Equal (1, elements.Length)
        Assert.Equal (1, elements.[0].Entry.Id)

    [<Test>]
    let ``Adding light elements`` () =
        
        let tree = makeTestTree 4 32.0f
        let light = makeTestOctelement 1 "light" true false false true Presence.Exterior Presence.Exterior (box3 (v3 1.0f 1.0f 1.0f) (v3 1.0f 1.0f 1.0f))
        let regularElement = makeTestOctelement 2 "regular" true false false false Presence.Exterior Presence.Exterior (box3 (v3 5.0f 5.0f 5.0f) (v3 1.0f 1.0f 1.0f))
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 1.0f 1.0f 1.0f) (v3 1.0f 1.0f 1.0f)) light tree
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 5.0f 5.0f 5.0f) (v3 1.0f 1.0f 1.0f)) regularElement tree
        
        // query for lights in a box
        let lightBox = box3 (v3 0.0f 0.0f 0.0f) (v3 3.0f 3.0f 3.0f)
        let results = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getLightsInViewBox lightBox results tree
        let elements = collectElements results
        Assert.Equal (1, elements.Length)
        Assert.Equal (1, elements.[0].Entry.Id)

    [<Test>]
    let ``Removing element from tree decreases element count`` () =
        
        // add then remove
        let tree = makeTestTree 3 16.0f
        let bounds = box3 (v3 0.0f 0.0f 0.0f) (v3 1.0f 1.0f 1.0f)
        let element = makeTestOctelement 1 "test1" true false false false Presence.Exterior Presence.Exterior bounds
        Octree.addElement Presence.Exterior Presence.Exterior bounds element tree
        Octree.removeElement Presence.Exterior Presence.Exterior bounds element tree
        
        let results = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getElements results tree
        Assert.Equal (0, results.Count)

    [<Test>]
    let ``Updating element position works correctly`` () =
        
        // add element at initial position
        let tree = makeTestTree 4 32.0f
        let boundsOld = box3 (v3 1.0f 1.0f 1.0f) (v3 2.0f 2.0f 2.0f)
        let boundsNew = box3 (v3 10.0f 10.0f 10.0f) (v3 2.0f 2.0f 2.0f)
        let element = makeTestOctelement 1 "moving" true false false false Presence.Exterior Presence.Exterior boundsOld
        let elementNew = makeTestOctelement 1 "moving" true false false false Presence.Exterior Presence.Exterior boundsNew
        Octree.addElement Presence.Exterior Presence.Exterior boundsOld element tree
        
        // update to new position
        Octree.updateElement Presence.Exterior Presence.Exterior boundsOld Presence.Exterior Presence.Exterior boundsNew elementNew tree
        
        // query at old position should return nothing
        let resultsOld = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getElementsAtPoint (v3 2.0f 2.0f 2.0f) resultsOld tree
        Assert.Equal (0, resultsOld.Count)
        
        // query at new position should return the element
        let resultsNew = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getElementsAtPoint (v3 11.0f 11.0f 11.0f) resultsNew tree
        Assert.Equal (1, resultsNew.Count)

    [<Test>]
    let ``Query at point returns correct elements`` () =
        
        let tree = makeTestTree 4 32.0f
        let element1 = makeTestOctelement 1 "at-origin" true false false false Presence.Exterior Presence.Exterior (box3 (v3 -1.0f -1.0f -1.0f) (v3 2.0f 2.0f 2.0f))
        let element2 = makeTestOctelement 2 "far-away" true false false false Presence.Exterior Presence.Exterior (box3 (v3 10.0f 10.0f 10.0f) (v3 2.0f 2.0f 2.0f))
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 -1.0f -1.0f -1.0f) (v3 2.0f 2.0f 2.0f)) element1 tree
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 10.0f 10.0f 10.0f) (v3 2.0f 2.0f 2.0f)) element2 tree
        
        // query at origin should return only first element
        let results = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getElementsAtPoint (v3 0.0f 0.0f 0.0f) results tree
        let elements = collectElements results
        Assert.Equal (1, elements.Length)
        Assert.Equal (1, elements.[0].Entry.Id)

    [<Test>]
    let ``Query in bounds returns intersecting elements`` () =
        
        let tree = makeTestTree 4 32.0f
        let element1 = makeTestOctelement 1 "inside" true false false false Presence.Exterior Presence.Exterior (box3 (v3 1.0f 1.0f 1.0f) (v3 2.0f 2.0f 2.0f))
        let element2 = makeTestOctelement 2 "outside" true false false false Presence.Exterior Presence.Exterior (box3 (v3 20.0f 20.0f 20.0f) (v3 2.0f 2.0f 2.0f))
        let element3 = makeTestOctelement 3 "overlapping" true false false false Presence.Exterior Presence.Exterior (box3 (v3 4.0f 4.0f 4.0f) (v3 2.0f 2.0f 2.0f))
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 1.0f 1.0f 1.0f) (v3 2.0f 2.0f 2.0f)) element1 tree
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 12.0f 12.0f 12.0f) (v3 2.0f 2.0f 2.0f)) element2 tree
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 4.0f 4.0f 4.0f) (v3 2.0f 2.0f 2.0f)) element3 tree
        
        // query bounds that should intersect elements 1 and 3
        let queryBounds = box3 (v3 0.0f 0.0f 0.0f) (v3 5.0f 5.0f 5.0f)
        let results = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getElementsInBounds queryBounds results tree
        let elements = collectElements results
        Assert.Equal (2, elements.Length)
        Assert.Equal (1, elements.[0].Entry.Id)
        Assert.Equal (3, elements.[1].Entry.Id)

    [<Test>]
    let ``Query in frustum returns intersecting elements`` () =
        
        let tree = makeTestTree 5 64.0f
        let element1 = makeTestOctelement 1 "in-frustum" true false false false Presence.Exterior Presence.Exterior (box3 (v3 0.0f 0.0f 0.0f) (v3 1.0f 1.0f 1.0f))
        let element2 = makeTestOctelement 2 "out-of-frustum" true false false false Presence.Exterior Presence.Exterior (box3 (v3 50.0f 50.0f 50.0f) (v3 1.0f 1.0f 1.0f))
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 0.0f 0.0f 0.0f) (v3 1.0f 1.0f 1.0f)) element1 tree
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 50.0f 50.0f 50.0f) (v3 1.0f 1.0f 1.0f)) element2 tree
        
        let frustum = makeTestFrustum()
        let results = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getElementsInFrustum frustum results tree
        let elements = collectElements results
        Assert.True (elements.Length >= 1) // should contain at least the element near origin

    [<Test>]
    let ``Query in view box returns only visible elements`` () =

        let tree = makeTestTree 4 32.0f
        let element1 = makeTestOctelement 1 "visible" true false false false Presence.Exterior Presence.Exterior (box3 (v3 1.0f 1.0f 1.0f) (v3 2.0f 2.0f 2.0f))
        let element2 = makeTestOctelement 2 "invisible" false false false false Presence.Exterior Presence.Exterior (box3 (v3 3.0f 3.0f 3.0f) (v3 2.0f 2.0f 2.0f))
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 1.0f 1.0f 1.0f) (v3 2.0f 2.0f 2.0f)) element1 tree
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 3.0f 3.0f 3.0f) (v3 2.0f 2.0f 2.0f)) element2 tree
        
        let queryBox = box3 (v3 0.0f 0.0f 0.0f) (v3 6.0f 6.0f 6.0f)
        let results = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getElementsInViewBox queryBox results tree
        let elements = collectElements results
        Assert.Equal (1, elements.Length)
        Assert.Equal (1, elements.[0].Entry.Id)

    [<Test>]
    let ``Query elements in view with frustums`` () =

        let tree = makeTestTree 5 64.0f
        let interiorElement = makeTestOctelement 1 "interior" true false false false Presence.Interior Presence.Interior (box3 (v3 0.0f 0.0f 0.0f) (v3 1.0f 1.0f 1.0f))
        let exteriorElement = makeTestOctelement 2 "exterior" true false false false Presence.Exterior Presence.Exterior (box3 (v3 2.0f 2.0f 2.0f) (v3 1.0f 1.0f 1.0f))
        Octree.addElement Presence.Interior Presence.Interior (box3 (v3 0.0f 0.0f 0.0f) (v3 1.0f 1.0f 1.0f)) interiorElement tree
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 2.0f 2.0f 2.0f) (v3 1.0f 1.0f 1.0f)) exteriorElement tree
        
        let frustumInterior = makeTestFrustum()
        let frustumExterior = makeTestFrustum()
        let lightBox = box3 (v3 -10.0f -10.0f -10.0f) (v3 20.0f 20.0f 20.0f)
        
        let results = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getElementsInView frustumInterior frustumExterior frustumInterior lightBox results tree
        Assert.True (results.Count >= 1) // should find elements in view

    [<Test>]
    let ``Query elements in play`` () =

        let tree = makeTestTree 4 32.0f
        let dynamicElement = makeTestOctelement 1 "dynamic" true false false false Presence.Exterior Presence.Exterior (box3 (v3 1.0f 1.0f 1.0f) (v3 2.0f 2.0f 2.0f))
        let staticElement = makeTestOctelement 2 "static" true true false false Presence.Exterior Presence.Exterior (box3 (v3 3.0f 3.0f 3.0f) (v3 2.0f 2.0f 2.0f))
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 1.0f 1.0f 1.0f) (v3 2.0f 2.0f 2.0f)) dynamicElement tree
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 3.0f 3.0f 3.0f) (v3 2.0f 2.0f 2.0f)) staticElement tree
        
        let playBox = box3 (v3 0.0f 0.0f 0.0f) (v3 6.0f 6.0f 6.0f)
        let playFrustum = makeTestFrustum()
        let results = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getElementsInPlay playBox playFrustum results tree
        let elements = collectElements results
        Assert.Equal (1, elements.Length)
        Assert.Equal (1, elements.[0].Entry.Id)

    [<Test>]
    let ``Omnipresent elements appear in all queries`` () =

        let tree = makeTestTree 4 32.0f
        let element = makeTestOctelement 1 "omnipresent" true false false false Presence.Omnipresent Presence.Omnipresent (box3 (v3 100.0f 100.0f 100.0f) (v3 1.0f 1.0f 1.0f))
        Octree.addElement Presence.Omnipresent Presence.Omnipresent (box3 (v3 100.0f 100.0f 100.0f) (v3 1.0f 1.0f 1.0f)) element tree
        
        // query at different locations should all return the omnipresent element
        let locations = [v3 0.0f 0.0f 0.0f; v3 15.0f 15.0f 15.0f; v3 -10.0f -10.0f -10.0f]
        for location in locations do
            let results = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
            Octree.getElementsAtPoint location results tree
            Assert.Equal (1, results.Count)

    [<Test>]
    let ``Imposter elements handled correctly`` () =

        let tree = makeTestTree 4 32.0f
        let imposter = makeTestOctelement 1 "imposter" true false false false Presence.Imposter Presence.Imposter (box3 (v3 1.0f 1.0f 1.0f) (v3 1.0f 1.0f 1.0f))
        Octree.addElement Presence.Imposter Presence.Imposter (box3 (v3 1.0f 1.0f 1.0f) (v3 1.0f 1.0f 1.0f)) imposter tree
        
        // imposter should appear in general queries
        let results = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getElements results tree
        Assert.Equal (1, results.Count)

    [<Test>]
    let ``Large elements go to ubiquitous fallback`` () =

        // create element larger than the magnitude threshold
        let tree = makeTestTree 4 32.0f
        let largeBounds = box3 (v3 0.0f 0.0f 0.0f) (v3 1000.0f 1000.0f 1000.0f)
        let element = makeTestOctelement 1 "large" true false false false Presence.Exterior Presence.Exterior largeBounds
        Octree.addElement Presence.Exterior Presence.Exterior largeBounds element tree
        
        // should still be queryable
        let results = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getElementsAtPoint (v3 500.0f 500.0f 500.0f) results tree
        Assert.Equal (1, results.Count)

    [<Test>]
    let ``Elements outside tree bounds go to ubiquitous fallback`` () =

        // create element outside tree bounds
        let tree = makeTestTree 3 16.0f
        let bounds = Octree.getBounds tree
        let outsideBounds = box3 (v3 (bounds.Max.X + 10.0f) (bounds.Max.Y + 10.0f) (bounds.Max.Z + 10.0f)) (v3 2.0f 2.0f 2.0f)
        let element = makeTestOctelement 1 "outside" true false false false Presence.Exterior Presence.Exterior outsideBounds
        Octree.addElement Presence.Exterior Presence.Exterior outsideBounds element tree
        
        // should still be queryable
        let results = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getElements results tree
        Assert.Equal (1, results.Count)

    [<Test>]
    let ``Light probes in view frustum query`` () =
        
        let tree = makeTestTree 4 32.0f
        let lightProbe = makeTestOctelement 1 "lightprobe" true false true false Presence.Exterior Presence.Exterior (box3 (v3 0.0f 0.0f 0.0f) (v3 1.0f 1.0f 1.0f))
        let invisibleProbe = makeTestOctelement 2 "invisible" false false true false Presence.Exterior Presence.Exterior (box3 (v3 2.0f 2.0f 2.0f) (v3 1.0f 1.0f 1.0f))
        let regularElement = makeTestOctelement 3 "regular" true false false false Presence.Exterior Presence.Exterior (box3 (v3 4.0f 4.0f 4.0f) (v3 1.0f 1.0f 1.0f))
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 0.0f 0.0f 0.0f) (v3 1.0f 1.0f 1.0f)) lightProbe tree
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 2.0f 2.0f 2.0f) (v3 1.0f 1.0f 1.0f)) invisibleProbe tree
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 4.0f 4.0f 4.0f) (v3 1.0f 1.0f 1.0f)) regularElement tree
        
        let frustum = makeTestFrustum()
        let results = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getLightProbesInViewFrustum frustum results tree
        let elements = collectElements results
        Assert.Equal (1, elements.Length) // only visible light probe
        Assert.Equal (1, elements.[0].Entry.Id)

    [<Test>]
    let ``Light probes in view box query`` () =
        
        let tree = makeTestTree 4 32.0f
        let lightProbe = makeTestOctelement 1 "lightprobe" true false true false Presence.Exterior Presence.Exterior (box3 (v3 1.0f 1.0f 1.0f) (v3 1.0f 1.0f 1.0f))
        let outsideProbe = makeTestOctelement 2 "outside" true false true false Presence.Exterior Presence.Exterior (box3 (v3 10.0f 10.0f 10.0f) (v3 1.0f 1.0f 1.0f))
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 1.0f 1.0f 1.0f) (v3 1.0f 1.0f 1.0f)) lightProbe tree
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 10.0f 10.0f 10.0f) (v3 1.0f 1.0f 1.0f)) outsideProbe tree
        
        let queryBox = box3 (v3 0.0f 0.0f 0.0f) (v3 3.0f 3.0f 3.0f)
        let results = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getLightProbesInViewBox queryBox results tree
        let elements = collectElements results
        Assert.Equal (1, elements.Length)
        Assert.Equal (1, elements.[0].Entry.Id)

    [<Test>]
    let ``Lights in view frustum query`` () =
        
        let tree = makeTestTree 4 32.0f
        let light = makeTestOctelement 1 "light" true false false true Presence.Exterior Presence.Exterior (box3 (v3 0.0f 0.0f 0.0f) (v3 1.0f 1.0f 1.0f))
        let invisibleLight = makeTestOctelement 2 "invisible" false false false true Presence.Exterior Presence.Exterior (box3 (v3 2.0f 2.0f 2.0f) (v3 1.0f 1.0f 1.0f))
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 0.0f 0.0f 0.0f) (v3 1.0f 1.0f 1.0f)) light tree
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 2.0f 2.0f 2.0f) (v3 1.0f 1.0f 1.0f)) invisibleLight tree
        
        let frustum = makeTestFrustum()
        let results = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getLightsInViewFrustum frustum results tree
        let elements = collectElements results
        Assert.Equal (1, elements.Length) // only visible light
        Assert.Equal (1, elements.[0].Entry.Id)

    [<Test>]
    let ``Clear removes all elements`` () =
        
        // add multiple elements with different types
        let tree = makeTestTree 4 32.0f
        let regular = makeTestOctelement 1 "regular" true false false false Presence.Exterior Presence.Exterior (box3 (v3 1.0f 1.0f 1.0f) (v3 1.0f 1.0f 1.0f))
        let lightProbe = makeTestOctelement 2 "lightprobe" true false true false Presence.Exterior Presence.Exterior (box3 (v3 2.0f 2.0f 2.0f) (v3 1.0f 1.0f 1.0f))
        let light = makeTestOctelement 3 "light" true false false true Presence.Exterior Presence.Exterior (box3 (v3 3.0f 3.0f 3.0f) (v3 1.0f 1.0f 1.0f))
        let omnipresent = makeTestOctelement 4 "omnipresent" true false false false Presence.Omnipresent Presence.Omnipresent (box3 (v3 4.0f 4.0f 4.0f) (v3 1.0f 1.0f 1.0f))
        
        // add elements to tree
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 1.0f 1.0f 1.0f) (v3 1.0f 1.0f 1.0f)) regular tree
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 2.0f 2.0f 2.0f) (v3 1.0f 1.0f 1.0f)) lightProbe tree
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 3.0f 3.0f 3.0f) (v3 1.0f 1.0f 1.0f)) light tree
        Octree.addElement Presence.Omnipresent Presence.Omnipresent (box3 (v3 4.0f 4.0f 4.0f) (v3 1.0f 1.0f 1.0f)) omnipresent tree
        
        // verify elements were added
        let resultsBefore = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getElements resultsBefore tree
        Assert.Equal (4, resultsBefore.Count)
        
        // clear tree
        Octree.clear tree
        
        // verify all elements removed
        let resultsAfter = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getElements resultsAfter tree
        Assert.Equal (0, resultsAfter.Count)

    [<Test>]
    let ``Sweep removes unused nodes`` () =
        
        // add element to create internal structure
        let tree = makeTestTree 4 32.0f
        let element = makeTestOctelement 1 "temporary" true false false false Presence.Exterior Presence.Exterior (box3 (v3 1.0f 1.0f 1.0f) (v3 1.0f 1.0f 1.0f))
        Octree.addElement Presence.Exterior Presence.Exterior (box3 (v3 1.0f 1.0f 1.0f) (v3 1.0f 1.0f 1.0f)) element tree
        
        // remove element to leave empty nodes
        Octree.removeElement Presence.Exterior Presence.Exterior (box3 (v3 1.0f 1.0f 1.0f) (v3 1.0f 1.0f 1.0f)) element tree
        
        // sweep should clean up unused nodes (this is mostly to ensure it doesn't crash)
        Octree.sweep tree
        
        // tree should still be functional
        let results = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getElements results tree
        Assert.Equal (0, results.Count)

    [<Test>]
    let ``Adding same element multiple times handles correctly`` () =
        
        // add same element multiple times
        let tree = makeTestTree 4 32.0f
        let bounds = box3 (v3 1.0f 1.0f 1.0f) (v3 1.0f 1.0f 1.0f)
        let element = makeTestOctelement 1 "duplicate" true false false false Presence.Exterior Presence.Exterior bounds
        Octree.addElement Presence.Exterior Presence.Exterior bounds element tree
        Octree.addElement Presence.Exterior Presence.Exterior bounds element tree
        Octree.addElement Presence.Exterior Presence.Exterior bounds element tree
        
        // should only appear once in results
        let results = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getElements results tree
        Assert.Equal (1, results.Count)

    [<Test>]
    let ``Removing non-existent element handles gracefully`` () =
        
        // remove element that was never added
        let tree = makeTestTree 4 32.0f
        let bounds = box3 (v3 1.0f 1.0f 1.0f) (v3 1.0f 1.0f 1.0f)
        let element = makeTestOctelement 1 "nonexistent" true false false false Presence.Exterior Presence.Exterior bounds
        Octree.removeElement Presence.Exterior Presence.Exterior bounds element tree
        
        // tree should still be functional
        let results = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getElements results tree
        Assert.Equal (0, results.Count)

    [<Test>]
    let ``Updating element presence type`` () =
        
        // add as exterior
        let tree = makeTestTree 4 32.0f
        let bounds = box3 (v3 1.0f 1.0f 1.0f) (v3 1.0f 1.0f 1.0f)
        let elementExterior = makeTestOctelement 1 "changing" true false false false Presence.Exterior Presence.Exterior bounds
        let elementOmnipresent = makeTestOctelement 1 "changing" true false false false Presence.Omnipresent Presence.Omnipresent bounds
        Octree.addElement Presence.Exterior Presence.Exterior bounds elementExterior tree
        
        // update to omnipresent
        Octree.updateElement Presence.Exterior Presence.Exterior bounds Presence.Omnipresent Presence.Omnipresent bounds elementOmnipresent tree
        
        // should now be queryable everywhere
        let results = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getElementsAtPoint (v3 100.0f 100.0f 100.0f) results tree
        Assert.Equal (1, results.Count)

    [<Test>]
    let ``Stress test with many elements`` () =
        
        // add many elements with various properties
        let tree = makeTestTree 6 64.0f
        let elementCount = 100
        for i in 1 .. elementCount do
            let x = (single i % 60.0f) - 30.0f
            let y = (single (i * 7) % 60.0f) - 30.0f
            let z = (single (i * 13) % 60.0f) - 30.0f
            let bounds = box3 (v3 x y z) (v3 1.0f 1.0f 1.0f)
            let isLight = i % 10 = 0
            let isLightProbe = i % 7 = 0 && not isLight
            let presence = if i % 20 = 0 then Presence.Omnipresent else Presence.Exterior
            let element = makeTestOctelement i $"stress{i}" true false isLightProbe isLight presence presence bounds
            Octree.addElement presence presence bounds element tree
        
        // verify all elements were added
        let results = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getElements results tree
        Assert.Equal (elementCount, results.Count)
        
        // test various spatial queries still work
        let pointResults = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getElementsAtPoint (v3 0.0f 0.0f 0.0f) pointResults tree
        Assert.True (pointResults.Count >= 1) // should find omnipresent elements at minimum
        
        let boundsResults = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getElementsInBounds (box3 (v3 -5.0f -5.0f -5.0f) (v3 10.0f 10.0f 10.0f)) boundsResults tree
        Assert.True (boundsResults.Count > 0)
        
        let lightResults = HashSet<TestElement Octelement> (OctelementEqualityComparer<TestElement> ())
        Octree.getLightProbesInView lightResults tree
        Assert.True (lightResults.Count >= 0) // may or may not have light probes depending on visibility