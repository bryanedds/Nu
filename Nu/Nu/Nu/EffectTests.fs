// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu.Tests
open System
open System.IO
open System.Xml
open OpenTK
open Xunit
open Prime
open Nu
module EffectTests =

    let [<Fact>] readEffectWorks () =
        Nu.init ()

        let effectStr =
            "[Effect [Ifrit [Loop 10] 20
                [[FlameResource [AsResource [[]
                    [Resource [Gameplay Esper]]]
                 [FlameSprite [AsAnimation [[FlamePosition]
                    [StaticSprite [ExpandResource FlameResource []]
                        [[ExpandGesture FlamePosition []]]
                         [Tween [Color Constant Over [[FFFFFFAA 0]]]]]
                 [IfritSprite [AsAnimation [[]
                    [AnimatedSprite [Resource [Gameplay Esper]] [4 4] [16 16] 6
                        [[Tween [Position Linear Sum [[0 0] 10] [[10 10] 0]]]
                         [Tween [Visible Constant Sum [[False 2] [True 16] [False 0]]
                         [Mount [ExpandSprite [FlameSprite [[PassGesture [Tween [Position Linear Sum [[0 0] 10] [[10 10] 0]]]
                         [Mount [ExpandSprite [FlameSprite [[PassGesture [Tween [Position Linear Sum [[10 0] 10] [[0 10] 0]]]
                         [Emit ...]]]
                [ExpandAnimation IfritSprite []]]]"

        let alucard =
            "[Effect [Alucard [Loop 16] 16
                [[Pose [AsResource [[]
                    [Resource [Gameplay AlucardWalkLeft]]]]
                [[AnimatedSprite [ResourceVar Pose] [4 4] [48 48] 5
                    [...]]]]"
        ignore alucard

        let effect = (AlgebraicConverter typeof<Effect>).ConvertFromString effectStr :?> Effect
        Assert.Equal<string> ("Ifrit", effect.Name)