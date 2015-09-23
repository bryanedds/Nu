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
           "[Sprite
             [Esper [0 10]
              [[Linear2 [Position [0 0] [10 10]]]
               [Linear2 [Size [0 0] [10 10]]]]]]"
        let effect = (AlgebraicConverter typeof<Effect>).ConvertFromString effectStr :?> Effect
        match effect with Sprite sprite -> Assert.Equal<string> ("Esper", sprite.Name)