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

            let Layer = Screen / "Gui"
            let NewGame = Layer / "NewGame"
            let LoadGame = Layer / "LoadGame"
            let Credits = Layer / "Credits"
            let Exit = Layer / "Exit"

    [<RequireQualifiedAccess>]
    module Credits =
        
        let Screen = Screen "Credits"

        [<RequireQualifiedAccess>]
        module Gui =

            let Layer = Screen / "Gui"
            let Back = Layer / "Back"

    [<RequireQualifiedAccess>]
    module Gameplay =

        let Screen = Screen "Gameplay"

        [<RequireQualifiedAccess>]
        module Gui =

            let Layer = Screen / "Gui"
            let Back = Layer / "Back"
            let SaveGame = Layer / "SaveGame"
            let Halt = Layer / "Halt"
            let Feeler = Layer / "Feeler"
            let DetailUpward = Layer / "DetailUpward"
            let DetailRightward = Layer / "DetailRightward"
            let DetailDownward = Layer / "DetailDownward"
            let DetailLeftward = Layer / "DetailLeftward"
            let Wait = Layer / "Wait"

        [<RequireQualifiedAccess>]
        module Scene =

            let Layer = Screen / "Scene"
            let Field = Layer / "Field"
            let Player = Layer / "Player"