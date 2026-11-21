// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// When a head burst, and the height of the floor its bits fall to.
[<AutoOpen>]
module HeadBurstDispatcherExtensions =
    type Entity with
        member this.GetBurstStart world : GameTime = this.Get (nameof this.BurstStart) world
        member this.SetBurstStart (value : GameTime) world = this.Set (nameof this.BurstStart) value world
        member this.BurstStart = lens (nameof this.BurstStart) this this.GetBurstStart this.SetBurstStart
        member this.GetFloorHeight world : single = this.Get (nameof this.FloorHeight) world
        member this.SetFloorHeight (value : single) world = this.Set (nameof this.FloorHeight) value world
        member this.FloorHeight = lens (nameof this.FloorHeight) this this.GetFloorHeight this.SetFloorHeight

/// A head bursting where it was shot - a spray of blood swelling and gone in an instant, and bits of blood, flesh and
/// bone flung out of it, tumbling as they fall and lying where they land. Each bit's flight is worked out afresh from
/// the time since the burst, so it's the same however often it's drawn. A creature shot dead in the head declares one
/// as its child while its body's there.
type HeadBurstDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    /// A bit's value between zero and one, set apart by what it's for.
    static let roll (key : RandKeyed) (name : string) =
        key |> RandKeyed.withName name |> RandKeyed.toSingle

    /// A value between two others, as far along as a share of the way.
    static let lerp (a : single) (b : single) (share : single) =
        a + (b - a) * share

    static member Properties =
        [define Entity.BurstStart GameTime.zero
         define Entity.FloorHeight 0.0f
         define Entity.Size (v3 3.0f 4.0f 3.0f)]

    override this.Render (renderPass, entity, world) =
        if Draw.passes renderPass false world then
            let age = max 0.0f (world.ScaledTime - entity.GetBurstStart world).SecondsF
            let origin = entity.GetPosition world
            let floor = entity.GetFloorHeight world
            let gravity = Constants.Gameplay.HeadBurstGravity

            // the spray, swelling and gone
            let puff = age / Constants.Gameplay.HeadBurstPuffDuration
            if puff < 1.0f then
                let size = Constants.Gameplay.HeadBurstPuffSize * MathF.Sin (MathF.PI * puff)
                Draw.shape renderPass Blood PartBall origin quatIdentity (v3Dup size) Constants.Gameplay.HeadBurstBloodColor 0.0f false world

            // the bits flung out of it, tumbling as they fall and lying where they land
            let key = RandKeyed.make 0UL |> RandKeyed.withName "burst" |> RandKeyed.withName (string entity.EntityAddress)
            for index in 0 .. dec Constants.Gameplay.HeadBurstBits do
                let bit = key |> RandKeyed.withIndex index
                let (surface, color, size) =
                    if index < Constants.Gameplay.HeadBurstBloodBits then (Blood, Constants.Gameplay.HeadBurstBloodColor, lerp 0.03f 0.065f (roll bit "size"))
                    elif index < Constants.Gameplay.HeadBurstBits - Constants.Gameplay.HeadBurstBoneBits then (Rot, Constants.Gameplay.HeadBurstFleshColor, lerp 0.035f 0.06f (roll bit "size"))
                    else (Bone, Constants.Gameplay.HeadBurstBoneColor, lerp 0.02f 0.035f (roll bit "size"))
                let heading = roll bit "heading" * MathF.Tau
                let outward = lerp 0.4f 1.4f (roll bit "outward")
                let upward = lerp 0.6f 2.4f (roll bit "upward")
                let velocity = v3 (MathF.Cos heading * outward) upward (MathF.Sin heading * outward)
                let rest = floor + size * 0.15f
                let drop = max 0.0f (origin.Y - rest)
                let landing = (upward + MathF.Sqrt (upward * upward + 2.0f * gravity * drop)) / gravity
                let flight = min age landing
                let position = origin + velocity * flight + v3 0.0f (-0.5f * gravity * flight * flight) 0.0f
                if age < landing then
                    let axis = Vector3.Normalize (v3 (roll bit "axisX" - 0.5f) 0.5f (roll bit "axisZ" - 0.5f))
                    let spin = Quaternion.CreateFromAxisAngle (axis, age * lerp 4.0f 14.0f (roll bit "spin"))
                    Draw.shape renderPass surface PartBox position spin (v3Dup size) color 0.0f false world
                else
                    let lying = Quaternion.CreateFromAxisAngle (v3Up, heading)
                    let scale = match surface with Bone -> v3Dup size | _ -> v3 (size * 1.5f) (size * 0.3f) (size * 1.5f)
                    Draw.shape renderPass surface PartBox (position.WithY rest) lying scale color 0.0f false world
