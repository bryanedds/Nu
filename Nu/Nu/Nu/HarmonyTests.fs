// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu.Tests
open System
open Xunit
open Prime
open Nu
module HarmonyTests =

    let TestFilePath = "../../TestData/Test.xstage"

    let [<Fact>] loadHarmonyProjectTest () =
        let project = Harmony.readHarmonyProjectFromFile TestFilePath
        Assert.True (project.Creator = "harmony")