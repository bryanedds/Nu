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
    module rec Gameplay =

        let PackageName = nameof Gameplay
        let SkyBoxFront = asset<Image> PackageName (nameof SkyBoxFront)
        let Capsule = asset<Image> PackageName (nameof Capsule)
        let Goo = asset<Image> PackageName (nameof Goo)
        let Link = asset<Image> PackageName (nameof Link)
        let Car = asset<Image> PackageName (nameof Car)
        let Wheel = asset<Image> PackageName (nameof Wheel)