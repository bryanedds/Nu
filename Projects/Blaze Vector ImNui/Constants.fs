namespace BlazeVector
open System
open Nu
open Prime

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module Gameplay =

        let [<Literal>] SectionCount = 16
        let [<Literal>] BulletLifeTime = 27L
        let [<Literal>] BulletForce = 25.0f
        let [<Uniform>] EnemyWalkForce = v3 -300.0f -1500.0f 0.0f
        let [<Literal>] PlayerWalkForce = 700.0f
        let [<Literal>] PlayerFallForce = -2500.0f
        let [<Literal>] PlayerClimbForce = 1500.0f
        let [<Literal>] PlayerJumpForce = 1250.0f