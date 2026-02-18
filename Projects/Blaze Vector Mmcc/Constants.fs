namespace BlazeVector
open System
open System.Numerics
open Nu
open Prime

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module Gameplay =

        let [<Literal>] SectionCount = 16
        let [<Literal>] BulletLifeTime = 32L
        let [<Literal>] BulletImpulse = 25.0f
        let [<Uniform>] EnemyWalkForce = v3 -150.0f -1500.0f 0.0f
        let [<Literal>] PlayerWalkForce = 425.0f
        let [<Literal>] PlayerFallForce = -2000.0f
        let [<Literal>] PlayerClimbForce = 100.0f
        let [<Literal>] PlayerJumpForce = 800.0f