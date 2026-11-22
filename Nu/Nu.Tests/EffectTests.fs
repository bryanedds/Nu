// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Tests
open System
open System.Numerics
open NUnit.Framework
open Prime
open Nu
open Nu.Effects
module EffectTests =

    let private keyFrame value length : Tween3KeyFrame =
        { TweenValue = value; TweenLength = GameTime.ofUpdates length }

    /// A straight line from the origin to the given end over 61 updates.
    let private line ending =
        PositionLocals (Sum, Linear, Once, [|keyFrame v3Zero 61L; keyFrame ending 0L|])

    let private makeSlice time =
        { SliceDelta = GameTime.ofUpdates 1L
          SliceTime = GameTime.ofUpdates time
          Position = v3Zero
          Scale = v3One
          Offset = v3Zero
          Size = v3One
          Angles = v3Zero
          Elevation = 0.0f
          Inset = box2Zero
          Color = Color.One
          Blend = Transparent
          Emission = Color.Zero
          IgnoreLightMaps = false
          Anisotropy = 0.0f
          Flip = Unflipped
          Brightness = 1.0f
          Specularity = 1.0f
          LightCutoff = 1.0f
          Volume = 1.0f
          Enabled = true
          UserDefinedSettings = UserDefinedSettings.defaultSettings }

    /// Run an effect for the given number of updates from an effect source at the origin, keeping its history as
    /// Effect.run does, and return the tags the last update produced, by name, along with the history.
    let private run updates descriptor =
        let history = System.Collections.Generic.Deque<Slice> ()
        let mutable tags = [||]
        for time in 1L .. updates do
            let slice = makeSlice time
            let effectSystem =
                EffectSystem.make (GameTime.ofUpdates time) false false Exterior None 0.0f DeferredRenderType Map.empty
            let (dataToken, _) = EffectSystem.eval descriptor slice history effectSystem
            tags <-
                dataToken
                |> DataToken.toSeq
                |> Seq.choose (function TagToken (name, slice) -> Some (name, (slice :?> Slice).Position) | _ -> None)
                |> Array.ofSeq
            history.AddToFront slice
            if history.Count > Constants.Effects.EffectHistoryMaxDefault then history.RemoveFromBack () |> ignore
        (tags, history)

    /// Parents emitted upward that each emit children drifting rightward, as in issue #141.
    let private nestedEmitDescriptor childEmitterAspects =
        let child =
            Emit (Shift 0.0f, Rate 0.2, childEmitterAspects, [|line (v3 100.0f 0.0f 0.0f)|], Tag ("Child", [||], Nil))
        let parent =
            let content = Contents (Shift 0.0f, [|Tag ("Parent", [||], Nil); child|])
            Emit (Shift 0.0f, Rate 0.1, [||], [|line (v3 0.0f 100.0f 0.0f)|], content)
        EffectDescriptor.make "NestedEmit" None Map.empty parent

    let private positionsOf name tags =
        tags |> Array.filter (fst >> (=) name) |> Array.map snd

    [<Test>]
    let ``An emitter in an emission emits from that emission rather than from the effect's source.`` () =
        Nu.init ()
        let (tags, _) = run 90L (nestedEmitDescriptor [||])
        let parents = positionsOf "Parent" tags
        let children = positionsOf "Child" tags
        Assert.That (parents, Is.Not.Empty)
        Assert.That (children, Is.Not.Empty)
        // every child starts on a parent's upward path, so none is left at the source's height
        for child in children do Assert.That (child.Y, Is.GreaterThan 0.0f)
        // each child is emitted once, not once for every parent emission
        Assert.Equal (children.Length, (Array.distinct children).Length)

    [<Test>]
    let ``An emitter without emitter aspects emits as one whose aspects change nothing.`` () =
        Nu.init ()
        let (plainTags, _) = run 90L (nestedEmitDescriptor [||])
        let (identityTags, _) = run 90L (nestedEmitDescriptor [|Degrees v3Zero|])
        Assert.Equal (plainTags, identityTags)

    [<Test>]
    let ``Evaluating an effect leaves its history unchanged.`` () =
        Nu.init ()
        let (_, history) = run 90L (nestedEmitDescriptor [|Degrees (v3 0.0f 0.0f 45.0f)|])
        for slice in history do
            Assert.Equal (v3Zero, slice.Position)
            Assert.Equal (v3Zero, slice.Angles)
