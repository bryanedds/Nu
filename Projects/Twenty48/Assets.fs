namespace Twenty48
open System
open Prime
open Nu

// this module contains asset constants that are used by the game.
// having an Assets module is optional, but can prevent you from duplicating string literals across the code base.
[<RequireQualifiedAccess>]
module Assets =

    // these are assets from the Gui package.
    [<RequireQualifiedAccess>]
    module Gui =

        let PackageName = "Gui"
        let ClearSansFont = asset<Font> PackageName "ClearSansFont"

    // these are assets from the Gameplay package.
    [<RequireQualifiedAccess>]
    module Gameplay =
        
        let PackageName = "Gameplay"
        let BoardImage = asset<Image> PackageName "Board"
        let TileImage value = asset<Image> PackageName (if value < 4096 then sprintf "Tile%i" value else "TileSuper")