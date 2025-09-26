namespace SandBox2d
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

        let PackageName = "Gameplay"
        let Background = asset<Image> PackageName "Background"
        let Capsule = asset<Image> PackageName "Capsule"
        let Goo = asset<Image> PackageName "Goo"
        let Link = asset<Image> PackageName "Link"
        let Car = asset<Image> PackageName "Car"
        let Wheel = asset<Image> PackageName "Wheel"