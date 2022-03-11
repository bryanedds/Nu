namespace InfinityRpg
open System
open Nu

[<RequireQualifiedAccess>]
module Simulants =

    [<RequireQualifiedAccess>]
    module Splash =

        let Screen = Screen "Splash"

    [<RequireQualifiedAccess>]
    module Title =

        let Screen = Screen "Title"

        [<RequireQualifiedAccess>]
        module Gui =

            let Group = Screen / "Gui"
            let NewGame = Group / "NewGame"
            let LoadGame = Group / "LoadGame"
            let Credits = Group / "Credits"
            let Exit = Group / "Exit"

    [<RequireQualifiedAccess>]
    module Credits =
        
        let Screen = Screen "Credits"

        [<RequireQualifiedAccess>]
        module Gui =

            let Group = Screen / "Gui"
            let Back = Group / "Back"

    [<RequireQualifiedAccess>]
    module Gameplay =

        let Screen = Screen "Gameplay"

        [<RequireQualifiedAccess>]
        module Gui =

            let Group = Screen / "Gui"
            let Back = Group / "Back"

        [<RequireQualifiedAccess>]
        module Scene =

            let Group = Screen / "Scene"
            let Field = Group / "Field"
            let Player = Group / "Player"