// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu.Tests
open System
open System.IO
open OpenTK
open Xunit
open Prime
open Nu
module EffectSystemTests =

    let [<Fact>] readEffectWorks () =
        Math.init ()
        let effectStr =
            "[TestEffect None 
              [[BoxSprite [[] [StaticSprite [Resource Default Image] [] Nil]]]] 
              [Emit [Shift 0.1] [Rate 0.1] 
               [[Rotation Sum Ease Bounce [[-1 180] [1 0]]]] 
               [[Translation Sum Linear Once [[[0 0] 180] [[80 500] 0]]] 
                [Size Scale Linear Once [[[0 0] 180] [[1 1] 0]]] 
                [Color Put Linear Bounce [[[1 0 1 1] 180] [[1 1 0 0] 0]]]] 
               [Expand BoxSprite []]]]"
        let effect = scvalue<Effect> effectStr
        Assert.Equal<string> ("TestEffect", effect.EffectName) // TODO: more assertions