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
        let [<Literal>] BulletLifeTime = 27L
        let [<Literal>] BulletImpulse = 30.0f
        let [<Uniform>] EnemyWalkForce = v3 -150.0f -750.0f 0.0f
        let [<Literal>] PlayerWalkForce = 350.0f
        let [<Literal>] PlayerFallForce = -1250.0f
        let [<Literal>] PlayerClimbForce = 750.0f
        let [<Literal>] PlayerJumpForce = 625.0f