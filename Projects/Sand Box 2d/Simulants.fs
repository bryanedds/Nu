namespace SandBox2d
open System
open Nu

// this module provides global handles to the game's key simulants.
// having a Simulants module for your game is optional, but can be nice to avoid duplicating string literals across
// the code base.
[<RequireQualifiedAccess>]
module Simulants =

    // toy box screen
    let ToyBox = Game / "ToyBox"
    let ToyBoxScene = ToyBox / "Scene"
    let ToyBoxBorder = ToyBoxScene / "Border"
    let ToyBoxSwitchScreen = ToyBoxScene / "SwitchScreen"

    // race course screen
    let RaceCourse = Game / "RaceCourse"
    let RaceCourseScene = RaceCourse / "Scene"
    let RaceCourseBorder = RaceCourseScene / "Border"
    let RaceCourseSwitchScreen = RaceCourseScene / "SwitchScreen"

    // fluid sim screen
    let FluidSim = Game / "FluidSim"
    let FluidSimScene = FluidSim / "Scene"
    let FluidSimSwitchScreen = FluidSimScene / "SwitchScreen"