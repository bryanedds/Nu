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
        let JoanModel = asset<AnimatedModel> PackageName "Joan"