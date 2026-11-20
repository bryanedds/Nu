// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// A shape on the floor that walkers can't pass - the game simulates its own collisions on the floor's plane rather
/// than using a physics engine. Points on the floor are (x, z).
type Collider =
    | BoxCollider of Center : Vector2 * HalfExtents : Vector2 * Yaw : single
    | CircleCollider of Center : Vector2 * Radius : single

    /// A point in a box's own frame, where its local x runs along (cos yaw, -sin yaw) and its local z along
    /// (sin yaw, cos yaw).
    static let toBoxFrame (center : Vector2) angle (point : Vector2) =
        let c = cos angle
        let s = sin angle
        let delta = point - center
        v2 (delta.X * c - delta.Y * s) (delta.X * s + delta.Y * c)

    /// A point in a box's own frame back out in the world.
    static let ofBoxFrame (center : Vector2) angle (local : Vector2) =
        let c = cos angle
        let s = sin angle
        center + v2 (local.X * c + local.Y * s) (-local.X * s + local.Y * c)

    /// A point on the floor below a position.
    static member toFloor (position : Vector3) = v2 position.X position.Z

    /// Push a circle out of a collider, if it overlaps it.
    static member separate radius (point : Vector2) collider =
        match collider with
        | CircleCollider (center, otherRadius) ->
            let delta = point - center
            let distance = delta.Length ()
            let reach = radius + otherRadius
            if distance >= reach then point
            elif distance < 0.0001f then center + v2 reach 0.0f
            else center + delta / distance * reach
        | BoxCollider (center, half, angle) ->
            let local = toBoxFrame center angle point
            let clamped = Vector2.Clamp (local, -half, half)
            let delta = local - clamped
            let distance = delta.Length ()
            if distance >= radius then point
            elif distance > 0.0001f then ofBoxFrame center angle (clamped + delta / distance * radius)
            else
                // within the box - out the nearest side
                let pushes = [|(half.X - local.X, v2 1.0f 0.0f); (half.X + local.X, v2 -1.0f 0.0f); (half.Y - local.Y, v2 0.0f 1.0f); (half.Y + local.Y, v2 0.0f -1.0f)|]
                let (depth, normal) = Array.minBy fst pushes
                ofBoxFrame center angle (local + normal * (depth + radius))

    /// Whether a circle overlaps a collider.
    static member touches radius (point : Vector2) collider =
        (Collider.separate radius point collider - point).LengthSquared () > 0.000001f

    /// Settle a circle among colliders and other circles - pushed out of each in turn a few times over, so it comes to
    /// rest clear of them all, sliding along whatever it pressed against.
    static member settle radius (point : Vector2) (colliders : Collider seq) (others : (Vector2 * single) seq) =
        let mutable point = point
        for _ in 0 .. 2 do
            for collider in colliders do
                point <- Collider.separate radius point collider
            for (center, otherRadius) in others do
                point <- Collider.separate radius point (CircleCollider (center, otherRadius))
        point

    /// Move a circle from one point toward another, settling it at every short step along the way, so a long move
    /// can't carry it through a thin wall to settle on the far side.
    static member slide radius (start : Vector2) (stop : Vector2) (colliders : Collider seq) (others : (Vector2 * single) seq) =
        let steps = max 1 (int (ceil ((stop - start).Length () / (radius * 0.5f))))
        let mutable point = start
        for i in 1 .. steps do
            point <- Collider.settle radius (point + (stop - start) / single steps) colliders others
        point

    /// How far along a segment it first meets a collider, as a share of its length, if it does.
    static member tryCast (start : Vector2) (stop : Vector2) collider =
        match collider with
        | CircleCollider (center, radius) ->
            let d = stop - start
            let f = start - center
            let a = Vector2.Dot (d, d)
            let b = 2.0f * Vector2.Dot (f, d)
            let c = Vector2.Dot (f, f) - radius * radius
            if c <= 0.0f then Some 0.0f
            elif a < 0.000001f then None
            else
                let discriminant = b * b - 4.0f * a * c
                if discriminant < 0.0f then None
                else
                    let t = (-b - sqrt discriminant) / (2.0f * a)
                    if t >= 0.0f && t <= 1.0f then Some t else None
        | BoxCollider (center, half, angle) ->
            let a = toBoxFrame center angle start
            let b = toBoxFrame center angle stop
            let mutable enter = 0.0f
            let mutable leave = 1.0f
            for (origin, direction, extent) in [|(a.X, b.X - a.X, half.X); (a.Y, b.Y - a.Y, half.Y)|] do
                if abs direction < 0.000001f then
                    if abs origin > extent then leave <- -1.0f
                else
                    let t0 = (-extent - origin) / direction
                    let t1 = (extent - origin) / direction
                    enter <- max enter (min t0 t1)
                    leave <- min leave (max t0 t1)
            if enter <= leave then Some enter else None

    /// Whether a segment passes through a collider.
    static member crosses start stop collider =
        (Collider.tryCast start stop collider).IsSome

    /// Whether a point lies within a collider.
    static member contains (point : Vector2) collider =
        match collider with
        | CircleCollider (center, radius) -> (point - center).Length () <= radius
        | BoxCollider (center, half, angle) ->
            let local = toBoxFrame center angle point
            abs local.X <= half.X && abs local.Y <= half.Y

    /// How far a collider reaches out from a point to its nearest face - so a thing's middle reached across its body.
    static member getReach (point : Vector2) collider =
        match collider with
        | CircleCollider (center, radius) -> (center - point).Length () + radius
        | BoxCollider (center, half, _) -> (center - point).Length () + min half.X half.Y

    /// Whether one standing at a spot facing a way reaches the middle of a thing to act on it, its body reaching out so
    /// far from it - near enough, and facing it, or, right at it, at least not turned away from it.
    static member reaches (at : Vector2) yaw (target : Vector2) reach =
        let delta = target - at
        let distance = delta.Length ()
        if distance < 0.01f then true
        else
            let facing = Vector2.Dot (delta / distance, Math.YawFacing yaw)
            distance < Constants.Gameplay.InteractDistance + reach &&
            (facing > Constants.Gameplay.InteractCone || distance < 0.5f + reach && facing > 0.0f)
