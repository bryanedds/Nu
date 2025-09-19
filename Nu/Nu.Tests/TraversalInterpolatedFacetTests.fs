module [<NUnit.Framework.DefaultFloatingPointTolerance 1e-6>] Nu.Tests.TraversalInterpolatedFacetTests
    open NUnit.Framework
    open System.Numerics
    open Nu
    // Entity.TraversalHistoryMax default (GameTime.ofUpdates 4) plus current value results in 5 values to traverse.
    let [<TestCase([|0f;   1f; 2f;   3f; 4f; 5f; 6f|],
                   [|0f; 0.5f; 1f; 1.5f; 2f; 3f; 4f|]);
          TestCase([|10f;   9f; 8f;   7f; 6f; 5f; 4f|],
                   [|10f; 9.5f; 9f; 8.5f; 8f; 7f; 6f|]);
          TestCase([|5f; -5f;     4f;     -4f;     3f;    -3f;     2f|],
                   [|5f;  0f; -1f/4f;  -1f/6f; -1f/4f; -1f/4f; -1f/4f|]);
          TestCase([|0f;    10f;     0f;     0f;     0f; -10f;      0f;      0f|],
                   [|0f; 10f/2f; 10f/2f; 10f/3f; 10f/4f;   0f; -10f/4f; -10f/4f|]);
          TestCase([|1f;    10f;     1f;     1f;     1f;  -10f;    1f;      1f|],
                   [|1f; 11f/2f; 11f/2f; 12f/3f; 13f/4f; 3f/4f; -7f/4f; -7f/4f|])>]
        ``Interpolation should be interpolate latest 5 values by default.`` (inputs : single array, expected : single array) =
        Nu.init ()
        let world = World.makeStub { WorldConfig.defaultConfig with Accompanied = true } (TestPlugin ())
        let runWhile (world : World) = world.UpdateTime < inputs.LongLength
        let preProcess (world : World) = // Using preProcess instead of perProcess is needed to declare entities before facets update.
            World.beginGame [] world
            World.beginScreen "A" true Vanilla [] world |> ignore
            World.beginGroup "B" [] world
            World.doEntity "C"
                [Entity.FacetNames .= Set.ofList [nameof RigidBodyFacet; nameof TraversalInterpolatedFacet]] world |> ignore
            let c = world.DeclaredEntity

            // position
            c.SetPosition (v3 inputs[int world.UpdateTime] 0f 0f) world
            let actual = c.GetPositionInterpolated world
            Assert.AreEqual (expected[int world.UpdateTime], actual.X)
            Assert.AreEqual (0f, actual.Y)
            Assert.AreEqual (0f, actual.Z)

            // linear velocity
            c.SetLinearVelocity (v3 inputs[int world.UpdateTime] 0f 0f) world
            let actual = c.GetLinearVelocityInterpolated world
            Assert.AreEqual (expected[int world.UpdateTime], actual.X)
            Assert.AreEqual (0f, actual.Y)
            Assert.AreEqual (0f, actual.Z)

            // angular velocity
            c.SetAngularVelocity (v3 inputs[int world.UpdateTime] 0f 0f) world
            let actual = c.GetAngularVelocityInterpolated world
            Assert.AreEqual (expected[int world.UpdateTime], actual.X)
            Assert.AreEqual (0f, actual.Y)
            Assert.AreEqual (0f, actual.Z)

            // rotation - divide by 10 to operate within 0 to 1 range for angle2d
            c.SetRotation (Quaternion.CreateFromAngle2d (inputs[int world.UpdateTime] / 10f)) world
            let actual = c.GetRotationInterpolated world
            let expected = expected[int world.UpdateTime]
            if expected = 10f/3f then Assert.AreEqual (0.333721161f, actual.Angle2d) // floating point imprecision
            elif inputs[int world.UpdateTime] = 1f && expected = 12f/3f then Assert.AreEqual (0.400282443f, actual.Angle2d)
            elif inputs[int world.UpdateTime] = -10f && expected = 3f/4f then Assert.AreEqual (0.0753947347f, actual.Angle2d)
            else Assert.AreEqual (expected / 10f, actual.Angle2d)
            Assert.AreEqual (1f, actual.W ** 2f + actual.Z ** 2f) // Is a unit quaternion
            Assert.AreEqual (0f, actual.X)
            Assert.AreEqual (0f, actual.Y)

            World.endGroup world
            World.endScreen world
            World.endGame world
        let result = World.runWithCleanUp runWhile preProcess ignore ignore ignore ignore true world
        Assert.Equal (Constants.Engine.ExitCodeSuccess, result)

    let [<TestCase true; TestCase false>]
        ``Interpolation should work with a different history and history size.`` (testOutOfRangeHistory : bool) =
        Nu.init ()
        let world = World.makeStub { WorldConfig.defaultConfig with Accompanied = true } (TestPlugin ())
        let runWhile (world : World) = world.UpdateTime <= 20L
        let preProcess (world : World) = // Using preProcess instead of perProcess is needed to declare entities before facets update.
            World.beginGame [] world
            World.beginScreen "A" true Vanilla [] world |> ignore
            World.beginGroup "B" [] world
            World.doEntity "C"
                [Entity.FacetNames .= Set.ofList [nameof RigidBodyFacet; nameof TraversalInterpolatedFacet]
                 Entity.TraversalHistoryMax .= GameTime.ofUpdates 10L
                 Entity.Position @= v3 30f 0f 0f
                 Entity.LinearVelocity @= v3 30f 0f 0f
                 Entity.AngularVelocity @= v3 30f 0f 0f
                 Entity.Rotation @= Quaternion.CreateFromAngle2d 0.3f] world |> ignore
            let c = world.DeclaredEntity

            if world.UpdateTime = 19L then
                let input = [if testOutOfRangeHistory then (9L, 999999f) // should be filtered away by history max
                             (10L, 99f)]
                c.SetPositionHistory (Prime.FQueue.ofList [for (t, x) in input do GameTime.ofUpdates t, v3 x 0f 0f]) world
                c.SetLinearVelocityHistory (Prime.FQueue.ofList [for (t, x) in input do GameTime.ofUpdates t, v3 x 0f 0f]) world
                c.SetAngularVelocityHistory (Prime.FQueue.ofList [for (t, x) in input do GameTime.ofUpdates t, v3 x 0f 0f]) world
                c.SetRotationHistory (Prime.FQueue.ofList [for (t, x) in input do GameTime.ofUpdates t, Quaternion.CreateFromAngle2d (x / 100f)]) world
            elif world.UpdateTime = 20L then
            (*
            There are 3 points: (t=10, x=99), (t=19, x=30), (t=20, x=30)
            Total time duration: t_n - t_1 = 20 - 10 = 10
            Interval from t = 10 to t = 19:
            - Time difference: Δt_1 = 19 - 10 = 9
            - Average position: (x_1 + x_2)/2 = (99 + 30)/2 = 64.5
            - Integral contribution: 64.5 * 9 = 580.5
            Interval from t = 19 to t = 20:
            - Time difference: Δt_2 = 20 - 19 = 1
            - Average position: (x_2 + x_3)/2 = (30 + 30)/2 = 30
            - Integral contribution: 30 * 1 = 30
            Total integral: 580.5 + 30 = 610.5
            Time-averaged position: 610.5/10 = 61.05
            *)
                let expected = 61.05f
                // position
                let actual = c.GetPositionInterpolated world
                Assert.AreEqual (expected, actual.X)
                Assert.AreEqual (0f, actual.Y)
                Assert.AreEqual (0f, actual.Z)

                // linear velocity
                let actual = c.GetLinearVelocityInterpolated world
                Assert.AreEqual (expected, actual.X)
                Assert.AreEqual (0f, actual.Y)
                Assert.AreEqual (0f, actual.Z)

                // angular velocity
                let actual = c.GetAngularVelocityInterpolated world
                Assert.AreEqual (expected, actual.X)
                Assert.AreEqual (0f, actual.Y)
                Assert.AreEqual (0f, actual.Z)

                // rotation - divide by 100 to operate within 0 to 1 range for angle2d
                let actual = c.GetRotationInterpolated world
                Assert.AreEqual (0.610623121f, actual.Angle2d) // floating point imprecision
                Assert.AreEqual (1f, actual.W ** 2f + actual.Z ** 2f) // Is a unit quaternion
                Assert.AreEqual (0f, actual.X)
                Assert.AreEqual (0f, actual.Y)

            World.endGroup world
            World.endScreen world
            World.endGame world
        let result = World.runWithCleanUp runWhile preProcess ignore ignore ignore ignore true world
        Assert.Equal (Constants.Engine.ExitCodeSuccess, result)