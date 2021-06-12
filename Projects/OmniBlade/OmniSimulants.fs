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
    module Start =

        let Screen = Screen "Start"

        [<RequireQualifiedAccess>]
        module Gui =

            let Group = Screen / "Gui"
            let New = Group / "New"
            let Load = Group / "Load"
            let Back = Group / "Back"
    
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
        module Scene =

            let Group = Screen / "Scene"
            let Avatar = Group / "Avatar"
            let TileMap = Group / "TileMap"

    [<RequireQualifiedAccess>]
    module Battle =

        let Screen = Screen "Battle"

        [<RequireQualifiedAccess>]
        module Scene =

            let Group = Screen / "Scene"
            let Ride = Group / "Ride"