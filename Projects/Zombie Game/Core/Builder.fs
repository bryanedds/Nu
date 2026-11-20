// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// Builds one prop of a scene in the prop's own frame - its origin at its feet, its front facing +z, which is south
/// when it's unturned - gathering the parts, colliders and lights it makes, placed in the scene's frame. Its randomness
/// is keyed by the scene, the prop and how many values it has drawn, so nothing else in the scene changes it, and the
/// scene's palette gives the colors the scene file names.
type Builder (sceneName : string, propName : string, framing : Framing, palette : Map<string, Color>, seed : uint64) =

    let parts = ResizeArray<Part> ()
    let leaves = ResizeArray<Part> ()
    let colliders = ResizeArray<Collider> ()
    let lights = ResizeArray<Light> ()
    let rotation = Quaternion.CreateFromYawPitchRoll (framing.Yaw, 0.0f, 0.0f)
    let key = RandomKey.make seed |> RandomKey.withName sceneName |> RandomKey.withName propName
    let mutable draws = 0

    /// Where the prop stands and which way it faces.
    member this.Framing = framing

    /// A point in the prop's frame, in the scene's.
    member this.ToScene (local : Vector3) =
        framing.Origin + Vector3.Transform (local * framing.Scale, rotation)

    /// A point on the ground in the prop's frame (x, z), on the ground in the scene's.
    member this.ToSceneGround (local : Vector2) =
        let point = this.ToScene (v3 local.X 0.0f local.Y)
        v2 point.X point.Z

    /// A part, casting a shadow or not, glowing as brightly as it does, its glow following the power or not, and a
    /// door's leaf or not.
    member this.AddPart leaf shadowed glow powered unreflected surface shape (local : Vector3) (localRotation : Quaternion) (scale : Vector3) color =
        let part =
            { PartShape = shape; PartPosition = this.ToScene local
              PartRotation = Quaternion.Concatenate (localRotation, rotation)
              PartScale = scale * framing.Scale; PartColor = color
              PartSurface = surface
              PartShadowed = shadowed
              PartGlow = glow
              PartPowered = powered
              PartUnreflected = unreflected }
        parts.Add part
        if leaf then leaves.Add part

    /// A part, at a point and turned in the prop's frame.
    member this.Part surface shape local localRotation scale color =
        this.AddPart false true (match surface with Light -> Constants.Scenery.PartGlow | _ -> 0.0f) false false surface shape local localRotation scale color

    /// A part casting no shadow, as a thin sign needn't - never a floor, which must.
    member this.Flat surface shape local localRotation scale color =
        this.AddPart false false (match surface with Light -> Constants.Scenery.PartGlow | _ -> 0.0f) false false surface shape local localRotation scale color

    /// A part left out of screen-space reflections, drawn forward, as a ceiling.
    member this.Unreflected surface shape local localRotation scale color =
        this.AddPart false true (match surface with Light -> Constants.Scenery.PartGlow | _ -> 0.0f) false true surface shape local localRotation scale color

    /// A box walkers can't pass, its center and half extents on the ground in the prop's frame.
    member this.Box (center : Vector2) (halfExtents : Vector2) =
        colliders.Add (BoxCollider (this.ToSceneGround center, halfExtents * framing.Scale, framing.Yaw))

    /// A box walkers can't pass, turned in the prop's frame by a yaw.
    member this.BoxTurned (center : Vector2) (halfExtents : Vector2) (yaw : single) =
        colliders.Add (BoxCollider (this.ToSceneGround center, halfExtents * framing.Scale, framing.Yaw + yaw))

    /// A circle walkers can't pass, its center on the ground in the prop's frame.
    member this.Circle (center : Vector2) radius =
        colliders.Add (CircleCollider (this.ToSceneGround center, radius * framing.Scale))

    /// A light, at a point in the prop's frame.
    member this.Light (local : Vector3) color brightness reach shadowed powered =
        lights.Add
            { LightPosition = this.ToScene local
              LightColor = color
              LightBrightness = brightness
              LightReach = reach
              LightShadowed = shadowed
              LightPowered = powered }

    /// A color of the scene's palette - its own colors over the shared ones - by name.
    member this.Color name =
        match Map.tryFind name palette with
        | Some color -> color
        | None -> failwith ("'" + propName + "' of scene '" + sceneName + "' names the color '" + name + "', which isn't in the scene's palette.")

    /// The prop's next random value, between zero (included) and one (excluded).
    member this.Random () =
        let value = key |> RandomKey.withIndex draws |> RandomKey.toSingle
        draws <- inc draws
        value

    /// The prop's next random value between two others.
    member this.Range low high =
        low + (high - low) * this.Random ()

    /// The prop's next random whole number, from zero up to, but not including, a count.
    member this.RandomIndex count =
        let value = key |> RandomKey.withIndex draws |> RandomKey.toIndex count
        draws <- inc draws
        value

    /// A color shaded randomly lighter or darker by up to half an amount, as no two chairs are quite alike.
    member this.Jitter (color : Color) amount =
        let shade = 1.0f + (this.Random () - 0.5f) * amount
        Color (min 1.0f (color.R * shade), min 1.0f (color.G * shade), min 1.0f (color.B * shade), color.A)

    /// The prop's parts, placed in the scene's frame.
    member this.Parts =
        parts.ToArray ()

    /// The leaves among the prop's parts, if it's a door, placed in the scene's frame.
    member this.Leaves =
        leaves.ToArray ()

    /// The prop's colliders, placed in the scene's frame.
    member this.Colliders =
        colliders.ToArray ()

    /// The prop's lights, placed in the scene's frame.
    member this.Lights =
        List.ofSeq lights
