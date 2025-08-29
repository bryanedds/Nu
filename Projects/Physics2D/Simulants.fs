namespace Physics2D
open System
open Nu

// this module provides global handles to the game's key simulants.
// having a Simulants module for your game is optional, but can be nice to avoid duplicating string literals across
// the code base.
[<RequireQualifiedAccess>]
module Simulants =

    let rec [<Literal>] SceneGroup = "Scene"
    let rec [<Literal>] BackEntity = "Back"

    let D01_SingleFixture = Game / "D01_SingleFixture"
