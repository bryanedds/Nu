namespace Physics2D
open System
open Prime
open Nu

// this module contains asset constants that are used by the game.
// having an Assets module is optional, but can prevent you from duplicating string literals across the code base.
[<RequireQualifiedAccess>]
module Assets =

    // these are assets from the Gameplay package.
    [<RequireQualifiedAccess>]
    module Gameplay =
        let [<Literal>] PackageName = "Gameplay"

        let SkyBoxFront = asset<Image> PackageName "SkyBoxFront"