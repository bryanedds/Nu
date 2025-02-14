namespace TerraFirma
open System
open Nu

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module Gameplay =

        let [<Literal>] CharacterAnimatedModelName = "AnimatedModel"
        let [<Literal>] CharacterWeaponName = "Weapon"
        let [<Literal>] CharacterWeaponHandBoneName = "mixamorig:RightHand"
        let [<Literal>] EnemyWalkSpeed = 2.0f
        let [<Literal>] EnemyTurnSpeed = 5.0f
        let [<Literal>] EnemyHitPointsMax = 3
        let [<Literal>] PlayerWalkSpeed = 3.0f
        let [<Literal>] PlayerTurnSpeed = 3.0f
        let [<Literal>] PlayerHitPointsMax = 5