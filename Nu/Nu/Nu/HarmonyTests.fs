// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu.Tests
open System
open OpenTK
open Xunit
open Prime
open Nu
module HarmonyTests =

    let [<Literal>] TestFilePath = "../../Testing/Harmony/Alucard/Alucard.xstage"

    let [<Fact>] loadHarmonyProjectWorks () =
        let project = Harmony.readProjectFromFile TestFilePath
        Assert.True (project.Creator = "harmony")
        Assert.True (project.ProjectOptions.Resolution = Vector2i (512, 512))