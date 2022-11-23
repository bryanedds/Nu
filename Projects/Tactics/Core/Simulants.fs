// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Tactics
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
            let Play = Group / "Play"
            let Credits = Group / "Credits"
            let Exit = Group / "Exit"

    [<RequireQualifiedAccess>]
    module Pick =

        let Screen = Screen "Pick"

        [<RequireQualifiedAccess>]
        module Gui =

            let Group = Screen / "Gui"
            let NewGame1 = Group / "NewGame1"
            let NewGame2 = Group / "NewGame2"
            let NewGame3 = Group / "NewGame3"
            let LoadGame1 = Group / "LoadGame1"
            let LoadGame2 = Group / "LoadGame2"
            let LoadGame3 = Group / "LoadGame3"
            let Back = Group / "Back"
    
    [<RequireQualifiedAccess>]
    module Credits =

        let Screen = Screen "Credits"
    
        [<RequireQualifiedAccess>]
        module Gui =
    
            let Group = Screen / "Gui"
            let Back = Group / "Back"

    [<RequireQualifiedAccess>]
    module Atlas =

        let Screen = Screen "Atlas"

        [<RequireQualifiedAccess>]
        module Scene =

            let Group = Screen / "Scene"
            let Feeler = Group / "Feeler"
            let Avatar = Group / "Avatar"

    [<RequireQualifiedAccess>]
    module Field =

        let Screen = Screen "Field"

        [<RequireQualifiedAccess>]
        module Scene =

            let Group = Screen / "Scene"
            let Feeler = Group / "Feeler"