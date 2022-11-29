namespace Elmario
open Nu

// this module provides global handles to the game's key simulants.
// having a Simulants module for your game is optional, but can be nice to avoid duplicating string literals across
// the code base.
[<RequireQualifiedAccess>]
module Simulants =

    [<RequireQualifiedAccess>]
    module Screen =

        let Screen = Screen "Screen"

        [<RequireQualifiedAccess>]
        module Group =

            let Group = Screen / "Group"
            let Elmario = Group / "Elmario"