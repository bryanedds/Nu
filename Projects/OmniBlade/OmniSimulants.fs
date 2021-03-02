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

            let Group = Screen / "Gui"
            let New = Group / "New"
            let Load = Group / "Load"
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
    module Field =

        let Screen = Simulants.DefaultScreen

        [<RequireQualifiedAccess>]
        module Gui =

            let Group = Screen / "Gui"
            let Submenu = Group / "Submenu"

        [<RequireQualifiedAccess>]
        module Scene =

            let Group = Screen / "Scene"
            let Backdrop = Group / "Backdrop"
            let TransitionFade = Group / "TransitionFade"
            let TileMap = Group / "TileMap"
            let Avatar = Group / "Avatar"
            let Interact = Group / "Interact"
            let Dialog = Group / "Dialog"

        [<RequireQualifiedAccess>]
        module Submenu =

            let Group = Screen / "Submenu"
            let Team = Group / "Team"
            let Item = Group / "Item"
            let Close = Group / "Close"
            let Use = Group / "Use"

        [<RequireQualifiedAccess>]
        module Shop =

            let Group = Screen / "Shop"
            let Panel = Group / "Panel"

    [<RequireQualifiedAccess>]
    module Battle =

        let Screen = Screen "Battle"

        [<RequireQualifiedAccess>]
        module Gui =
    
            let Group = Screen / "Gui"
            let Dialog = Group / "Dialog"
            let Interact = Group / "Interact"
    
        [<RequireQualifiedAccess>]
        module Scene =

            let Group = Screen / "Scene"
            let Ride = Group / "Ride"