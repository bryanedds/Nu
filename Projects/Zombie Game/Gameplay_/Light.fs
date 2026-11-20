// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// What a light shines as with the power on - its own color and brightness, which the entity's color and brightness
/// follow, dimmed and flickering as much as the power it's told lets it shine.
[<AutoOpen>]
module LightDispatcherExtensions =
    type Entity with
        member this.GetLightColor world : Color = this.Get (nameof this.LightColor) world
        member this.SetLightColor (value : Color) world = this.Set (nameof this.LightColor) value world
        member this.LightColor = lens (nameof this.LightColor) this this.GetLightColor this.SetLightColor
        member this.GetLightBrightness world : single = this.Get (nameof this.LightBrightness) world
        member this.SetLightBrightness (value : single) world = this.Set (nameof this.LightBrightness) value world
        member this.LightBrightness = lens (nameof this.LightBrightness) this this.GetLightBrightness this.SetLightBrightness

/// A light of an area - dimmed and flickering as much as the power scale it's told. Its reach is its cutoff and its
/// shadowing whether it casts shadows.
type LightDispatcher () =
    inherit Entity3dDispatcherImSim (false, false, true)

    static member Facets =
        [typeof<Light3dFacet>]

    static member Properties =
        [define Entity.LightColor Color.White
         define Entity.LightBrightness 1.0f
         define Entity.PowerScale 1.0f
         define Entity.LightType (PointLight(0.0f, None))
         define Entity.DesireFog false
         define Entity.Static false]

    override this.Process (entity, world) =
        let lightColor = entity.GetLightColor world
        let scale = entity.GetPowerScale world
        let dimmed = scale < 1.0f
        entity.SetColor (if dimmed then Color (lightColor.R * 0.82f, lightColor.G * 0.89f, lightColor.B, 1.0f) else lightColor) world
        entity.SetBrightness (entity.GetLightBrightness world * scale) world

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.important (v3Dup 0.25f) v3Zero
