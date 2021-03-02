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

            let Group = Screen / "Gui"
            let Play = Group / "Play"
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
        module Level =

            let Group = Screen / "Level"

        [<RequireQualifiedAccess>]
        module Scene =

            let Group = Screen / "Scene"
            let Player = Group / "Player"