// Zombie Game.
// Copyright (C) Bryan Edds.

namespace System.Numerics
open Prime
open Nu

/// The math the game adds to Nu's - its yaws turned from south (+z) toward east (+x), as scenes and puppets turn.
[<RequireQualifiedAccess>]
module Math =

    /// The way a yaw faces, on the ground.
    let YawForward yaw = v3 (sin yaw) 0.0f (cos yaw)

    /// The way a yaw faces, on the floor plane (x, z).
    let YawFacing yaw = v2 (sin yaw) (cos yaw)

    /// The yaw facing a direction on the ground (x, z).
    let YawOf (direction : Vector2) = atan2 direction.X direction.Y

    /// An ease from 0 to 1 as t goes from 0 to 1, slow at either end, held beyond them.
    let Smoothstep (t : single) =
        let t = max 0.0f (min 1.0f t)
        t * t * (3.0f - 2.0f * t)
