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
           "[Template
             [Ifrit Loop 20
              [AnimatedSprite [Gameplay Esper] [16 16] [4 4]]
              [[Gesture [Position Linear Add [[0 0] 10] [[10 10] 0]]]
               [Gesture [Color Flip Ovr [[FFFFFFAA 0]]]]
               [Gesture [Visible Flip And [[False 2] [True 16] [False 0]]]]
              [[Instance
                [Shiva Looping 20
                 [[Gesture [Position Linear Sum [[0 0] 10] [[10 10] 0]]]]]]]]]"
        let effect = (AlgebraicConverter typeof<EffectExpr>).ConvertFromString effectStr :?> EffectExpr
        match effect with
        | Template template -> Assert.Equal<string> ("Ifrit", template.Name)
        | _ -> failwithumf ()