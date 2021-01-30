// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open Prime
open Nu

[<RequireQualifiedAccess>]
module Simulants =

    [<RequireQualifiedAccess>]
    module Splash = let Screen = Screen "Splash"

    [<RequireQualifiedAccess>]
    module Intro = let Screen = Screen "Intro"
    
    [<RequireQualifiedAccess>]
    module Intro2 = let Screen = Screen "Intro2"
    
    [<RequireQualifiedAccess>]
    module Intro3 = let Screen = Screen "Intro3"
    
    [<RequireQualifiedAccess>]
    module Intro4 = let Screen = Screen "Intro4"
    
    [<RequireQualifiedAccess>]
    module Intro5 = let Screen = Screen "Intro5"

    [<RequireQualifiedAccess>]
    module Title =

        let Screen = Screen "Title"
    
        [<RequireQualifiedAccess>]
        module Gui =

            let Layer = Screen / "Gui"
            let New = Layer / "New"
            let Load = Layer / "Load"
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
    module Field =

        let Screen = Simulants.DefaultScreen

        [<RequireQualifiedAccess>]
        module Hud =

            let Layer = Screen / "Hud"
            let Submenu = Layer / "Submenu"

        [<RequireQualifiedAccess>]
        module Scene =

            let Layer = Screen / "Scene"
            let Backdrop = Layer / "Backdrop"
            let TransitionFade = Layer / "TransitionFade"
            let TileMap = Layer / "TileMap"
            let Avatar = Layer / "Avatar"
            let Interact = Layer / "Interact"
            let Dialog = Layer / "Dialog"

        [<RequireQualifiedAccess>]
        module Submenu =

            let Layer = Screen / "Submenu"
            let Team = Layer / "Team"
            let Item = Layer / "Item"
            let Close = Layer / "Close"
            let Use = Layer / "Use"

        [<RequireQualifiedAccess>]
        module Shop =

            let Layer = Screen / "Shop"
            let Panel = Layer / "Panel"

    [<RequireQualifiedAccess>]
    module Battle =

        let Screen = Screen "Battle"

        [<RequireQualifiedAccess>]
        module Hud =
    
            let Layer = Screen / "Hud"
            let Dialog = Layer / "Dialog"
            let Interact = Layer / "Interact"
    
        [<RequireQualifiedAccess>]
        module Scene =

            let Layer = Screen / "Scene"
            let Ride = Layer / "Ride"