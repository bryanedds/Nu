// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// What a part is made of and how it glows - its shape, surface and glow, how much the power lets it glow, and whether
/// it's left out of screen-space reflections - beside its place, size, color and shadow, which are the entity's own.
[<AutoOpen>]
module PartDispatcherExtensions =
    type Entity with
        member this.GetShape world : PartShapeType = this.Get (nameof this.Shape) world
        member this.SetShape (value : PartShapeType) world = this.Set (nameof this.Shape) value world
        member this.Shape = lens (nameof this.Shape) this this.GetShape this.SetShape
        member this.GetSurfaceType world : SurfaceType = this.Get (nameof this.SurfaceType) world
        member this.SetSurfaceType (value : SurfaceType) world = this.Set (nameof this.SurfaceType) value world
        member this.SurfaceType = lens (nameof this.SurfaceType) this this.GetSurfaceType this.SetSurfaceType
        member this.GetGlow world : single = this.Get (nameof this.Glow) world
        member this.SetGlow (value : single) world = this.Set (nameof this.Glow) value world
        member this.Glow = lens (nameof this.Glow) this this.GetGlow this.SetGlow
        member this.GetPowerScale world : single = this.Get (nameof this.PowerScale) world
        member this.SetPowerScale (value : single) world = this.Set (nameof this.PowerScale) value world
        member this.PowerScale = lens (nameof this.PowerScale) this this.GetPowerScale this.SetPowerScale
        member this.GetUnreflected world : bool = this.Get (nameof this.Unreflected) world
        member this.SetUnreflected (value : bool) world = this.Set (nameof this.Unreflected) value world
        member this.Unreflected = lens (nameof this.Unreflected) this this.GetUnreflected this.SetUnreflected

/// A part of an area declared every frame rather than frozen with the scenery - one glowing with the power, dim and
/// flickering until it's restored, or one there only while a condition holds - drawn as its shape, made of its surface
/// in its color.
type PartDispatcher () =
    inherit Entity3dDispatcherImSim (false, false, false)

    static member Facets =
        [typeof<StaticModelFacet>]

    static member Properties =
        [define Entity.Shape PartBox
         define Entity.SurfaceType Paint
         define Entity.Color Color.White
         define Entity.Glow 0.0f
         define Entity.PowerScale 1.0f
         define Entity.Unreflected false
         define Entity.Static false]

    override this.Process (entity, world) =
        let surfaceType = entity.GetSurfaceType world
        let color = entity.GetColor world
        let glow = entity.GetGlow world * (1.0f + 2.0f * entity.GetPowerScale world) / 3.0f
        entity.SetStaticModel (PartShapeType.getModel (entity.GetShape world)) world
        entity.SetMaterial (SurfaceType.getMaterial surfaceType color) world
        entity.SetRenderStyle (SurfaceType.getRenderStyle surfaceType color (entity.GetUnreflected world)) world
        entity.SetMaterialProperties (SurfaceData.makeProperties Data.Surfaces[surfaceType] color glow) world
