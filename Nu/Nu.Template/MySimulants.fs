namespace MyGame
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
            let Play = Layer / "Play"
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
        module Level =

            let Layer = Screen / "Level"

        [<RequireQualifiedAccess>]
        module Scene =

            let Layer = Screen / "Scene"
            let Player = Layer / "Player"