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
        let BackgroundImage = asset<Image> PackageName "Background"
        let CapsuleImage = asset<Image> PackageName "Capsule"
        let GooImage = asset<Image> PackageName "Goo"
        let LinkImage = asset<Image> PackageName "Link"
        let CarImage = asset<Image> PackageName "Car"
        let WheelImage = asset<Image> PackageName "Wheel"
        let BubbleImage = asset<Image> PackageName "Bubble"
        let DropletCursor = asset<Cursor> PackageName "Droplet"
