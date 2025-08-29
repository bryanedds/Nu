namespace Physics2D
open System
open Prime
open Nu

// this module contains asset constants that are used by the game.
// having an Assets module is optional, but can prevent you from duplicating string literals across the code base.
[<RequireQualifiedAccess>]
module Assets =

    // NOTE: the asset graph is modified to import all images as 2D assets, not 3D.
    [<RequireQualifiedAccess>]
    module Default =
        let SkyBoxFront = asset<Image> Assets.Default.PackageName "SkyBoxFront"