// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace TerraFirma
open System
open Nu

[<RequireQualifiedAccess>]
module Assets =

    [<RequireQualifiedAccess>]
    module Gameplay =

        let PackageName = "Gameplay"
        let SlashSound = asset<Sound> PackageName "Slash"
        let Slash2Sound = asset<Sound> PackageName "Slash2"
        let JoanModel = asset<AnimatedModel> PackageName "Joan"