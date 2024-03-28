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

        let [<Literal>] CharacterInterpolationSteps = 4
        let [<Literal>] CharacterWeaponHandBoneIndex = 39
        let [<Literal>] PlayerWalkSpeed = 0.1f
        let [<Literal>] PlayerTurnSpeed = 0.05f
        let [<Literal>] PlayerJumpSpeed = 10.0f