// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace TerraFirma
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module Gameplay =

        let [<Literal>] PlayerWalkSpeed = 0.06f
        let [<Literal>] PlayerTurnSpeed = 0.035f
        let [<Literal>] PlayerJumpSpeed = 6.0f