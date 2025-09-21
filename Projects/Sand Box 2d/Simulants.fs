namespace SandBox2d
open System
open Nu

// this module provides global handles to the game's key simulants.
// having a Simulants module for your game is optional, but can be nice to avoid duplicating string literals across
// the code base.
[<RequireQualifiedAccess>]
module Simulants =

    let SandBox = Game / "SandBox"
    let SandBoxScene = SandBox / "Scene"
    let SandBoxBorder = SandBoxScene / "Border"
    let RaceCourse = Game / "RaceCourse"
    let RaceCourseScene = RaceCourse / "Scene"
    let RaceCourseBorder = RaceCourseScene / "Border"

