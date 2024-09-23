namespace MyGame
open System
open Nu

// this module provides global handles to the game's key simulants.
// having a Simulants module for your game is optional, but can be nice to avoid duplicating string literals across
// the code base.
[<RequireQualifiedAccess>]
module Simulants =

    // splash screen
    let Splash = Game / "Splash"

    // title screen
    let Title = Game / "Title"

    // credits screen
    let Credits = Game / "Credits"

    // gameplay screen
    let Gameplay = Game / "Gameplay"