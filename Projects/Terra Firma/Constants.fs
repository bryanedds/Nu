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
        let [<Literal>] EnemyHitPoints = 3
        let [<Literal>] PlayerHitPoints = 5