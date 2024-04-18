namespace TerraFirma
open System
open Nu

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module Gameplay =

        let [<Literal>] CharacterInterpolationSteps = 4
        let [<Literal>] CharacterPositionInterpDistanceMax = 1.0f
        let [<Literal>] CharacterAnimatedModelName = "AnimatedModel"
        let [<Literal>] CharacterWeaponName = "Weapon"
        let [<Literal>] CharacterWeaponHandBoneName = "mixamorig:RightHand"
        let [<Literal>] PlayerWalkSpeed = 0.1f
        let [<Literal>] PlayerTurnSpeed = 0.05f
        let [<Literal>] PlayerJumpSpeed = 10.0f