namespace MyGame
open Nu

// this module provides global handles to the game's key simulants.
// having a Simulants module for your game is optional, but can be nice to avoid duplicating string literals across
// the code base.
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
        module Gui =
            
            let Group = Screen / "Gui"
            let Quit = Group / "Quit"

        [<RequireQualifiedAccess>]
        module Player =

            let Group = Screen / "Player"
            let Character = Group / "Character"

        [<RequireQualifiedAccess>]
        module Scene =

            let Group = Screen / "Scene"
            let BallLeft = Group / "BallLeft"
            let BallRight = Group / "BallRight"