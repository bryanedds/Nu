// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open Prime

/// Entity functions for the world (2/2).
[<AutoOpen>]
module WorldEntityModule =

    /// Mutable clipboard that allows its state to persist beyond undo / redo.
    let mutable private Clipboard : (bool * EntityDescriptor * Entity) option = None

    [<RequireQualifiedAccess>]
    module private Cached =
        let mutable Dispatcher = Unchecked.defaultof<Lens<EntityDispatcher, Entity>>
        let mutable Facets = Unchecked.defaultof<Lens<Facet array, Entity>>
        let mutable Transform = Unchecked.defaultof<Lens<Transform, Entity>>
        let mutable PerimeterCenter = Unchecked.defaultof<Lens<Vector3, Entity>>
        let mutable PerimeterBottom = Unchecked.defaultof<Lens<Vector3, Entity>>
        let mutable PerimeterBottomLeft = Unchecked.defaultof<Lens<Vector3, Entity>>
        let mutable PerimeterMin = Unchecked.defaultof<Lens<Vector3, Entity>>
        let mutable PerimeterMax = Unchecked.defaultof<Lens<Vector3, Entity>>
        let mutable PerimeterCenterLocal = Unchecked.defaultof<Lens<Vector3, Entity>>
        let mutable PerimeterBottomLocal = Unchecked.defaultof<Lens<Vector3, Entity>>
        let mutable PerimeterBottomLeftLocal = Unchecked.defaultof<Lens<Vector3, Entity>>
        let mutable PerimeterMinLocal = Unchecked.defaultof<Lens<Vector3, Entity>>
        let mutable PerimeterMaxLocal = Unchecked.defaultof<Lens<Vector3, Entity>>
        let mutable Position = Unchecked.defaultof<Lens<Vector3, Entity>>
        let mutable PositionLocal = Unchecked.defaultof<Lens<Vector3, Entity>>
        let mutable Rotation = Unchecked.defaultof<Lens<Quaternion, Entity>>
        let mutable RotationLocal = Unchecked.defaultof<Lens<Quaternion, Entity>>
        let mutable Scale = Unchecked.defaultof<Lens<Vector3, Entity>>
        let mutable ScaleLocal = Unchecked.defaultof<Lens<Vector3, Entity>>
        let mutable Offset = Unchecked.defaultof<Lens<Vector3, Entity>>
        let mutable Angles = Unchecked.defaultof<Lens<Vector3, Entity>>
        let mutable AnglesLocal = Unchecked.defaultof<Lens<Vector3, Entity>>
        let mutable Degrees = Unchecked.defaultof<Lens<Vector3, Entity>>
        let mutable DegreesLocal = Unchecked.defaultof<Lens<Vector3, Entity>>
        let mutable Size = Unchecked.defaultof<Lens<Vector3, Entity>>
        let mutable Elevation = Unchecked.defaultof<Lens<single, Entity>>
        let mutable ElevationLocal = Unchecked.defaultof<Lens<single, Entity>>
        let mutable Overflow = Unchecked.defaultof<Lens<single, Entity>>
        let mutable AffineMatrix = Unchecked.defaultof<Lens<Matrix4x4, Entity>>
        let mutable AffineMatrixLocal = Unchecked.defaultof<Lens<Matrix4x4, Entity>>
        let mutable PerimeterUnscaled = Unchecked.defaultof<Lens<Box3, Entity>>
        let mutable Perimeter = Unchecked.defaultof<Lens<Box3, Entity>>
        let mutable Bounds = Unchecked.defaultof<Lens<Box3, Entity>>
        let mutable Presence = Unchecked.defaultof<Lens<Presence, Entity>>
        let mutable Absolute = Unchecked.defaultof<Lens<bool, Entity>>
        let mutable MountOpt = Unchecked.defaultof<Lens<Entity Address option, Entity>>
        let mutable PropagationSourceOpt = Unchecked.defaultof<Lens<Entity option, Entity>>
        let mutable Enabled = Unchecked.defaultof<Lens<bool, Entity>>
        let mutable EnabledLocal = Unchecked.defaultof<Lens<bool, Entity>>
        let mutable Visible = Unchecked.defaultof<Lens<bool, Entity>>
        let mutable VisibleLocal = Unchecked.defaultof<Lens<bool, Entity>>
        let mutable CastShadow = Unchecked.defaultof<Lens<bool, Entity>>
        let mutable Pickable = Unchecked.defaultof<Lens<bool, Entity>>
        let mutable AlwaysUpdate = Unchecked.defaultof<Lens<bool, Entity>>
        let mutable AlwaysRender = Unchecked.defaultof<Lens<bool, Entity>>
        let mutable Protected = Unchecked.defaultof<Lens<bool, Entity>>
        let mutable Persistent = Unchecked.defaultof<Lens<bool, Entity>>
        let mutable Is2d = Unchecked.defaultof<Lens<bool, Entity>>
        let mutable Is3d = Unchecked.defaultof<Lens<bool, Entity>>
        let mutable Static = Unchecked.defaultof<Lens<bool, Entity>>
        let mutable Physical = Unchecked.defaultof<Lens<bool, Entity>>
        let mutable LightProbe = Unchecked.defaultof<Lens<bool, Entity>>
        let mutable Light = Unchecked.defaultof<Lens<bool, Entity>>
        let mutable Optimized = Unchecked.defaultof<Lens<bool, Entity>>
        let mutable Destroying = Unchecked.defaultof<Lens<bool, Entity>>
        let mutable OverlayNameOpt = Unchecked.defaultof<Lens<string option, Entity>>
        let mutable FacetNames = Unchecked.defaultof<Lens<string Set, Entity>>
        let mutable PropagatedDescriptorOpt = Unchecked.defaultof<Lens<EntityDescriptor option, Entity>>
        let mutable Order = Unchecked.defaultof<Lens<int64, Entity>>
        let mutable Id = Unchecked.defaultof<Lens<uint64, Entity>>

    type Entity with
        member this.GetDispatcher world = World.getEntityDispatcher this world
        member this.Dispatcher = if notNull (this :> obj) then lensReadOnly (nameof this.Dispatcher) this this.GetDispatcher else Cached.Dispatcher
        member this.GetModelGeneric<'a> world = World.getEntityModelGeneric<'a> this world
        member this.SetModelGeneric<'a> value world = World.setEntityModelGeneric<'a> false false value this world |> ignore<bool>
        member this.ModelGeneric<'a> () = lens Constants.Engine.ModelPropertyName this this.GetModelGeneric<'a> this.SetModelGeneric<'a>
        member this.GetFacets world = World.getEntityFacets this world
        member this.Facets = if notNull (this :> obj) then lensReadOnly (nameof this.Facets) this this.GetFacets else Cached.Facets
        member this.GetTransform world = World.getEntityTransform this world
        member this.SetTransform value world = let mutable value = value in World.setEntityTransformByRef (&value, World.getEntityState this world, this, world) |> ignore<bool>
        member this.Transform = if notNull (this :> obj) then lens (nameof this.Transform) this this.GetTransform this.SetTransform else Cached.Transform
        member this.SetPerimeterCenter value world = World.setEntityPerimeterCenter value this world |> ignore<bool>
        member this.GetPerimeterCenter world = World.getEntityPerimeterCenter this world
        member this.PerimeterCenter = if notNull (this :> obj) then lens (nameof this.PerimeterCenter) this this.GetPerimeterCenter this.SetPerimeterCenter else Cached.PerimeterCenter
        member this.SetPerimeterBottom value world = World.setEntityPerimeterBottom value this world |> ignore<bool>
        member this.GetPerimeterBottom world = World.getEntityPerimeterBottom this world
        member this.PerimeterBottom = if notNull (this :> obj) then lens (nameof this.PerimeterBottom) this this.GetPerimeterBottom this.SetPerimeterBottom else Cached.PerimeterBottom
        member this.SetPerimeterBottomLeft value world = World.setEntityPerimeterBottomLeft value this world |> ignore<bool>
        member this.GetPerimeterBottomLeft world = World.getEntityPerimeterBottomLeft this world
        member this.PerimeterBottomLeft = if notNull (this :> obj) then lens (nameof this.PerimeterBottomLeft) this this.GetPerimeterBottomLeft this.SetPerimeterBottomLeft else Cached.PerimeterBottomLeft
        member this.SetPerimeterMin value world = World.setEntityPerimeterMin value this world |> ignore<bool>
        member this.GetPerimeterMin world = World.getEntityPerimeterMin this world
        member this.PerimeterMin = if notNull (this :> obj) then lens (nameof this.PerimeterMin) this this.GetPerimeterMin this.SetPerimeterMin else Cached.PerimeterMin
        member this.SetPerimeterMax value world = World.setEntityPerimeterMax value this world |> ignore<bool>
        member this.GetPerimeterMax world = World.getEntityPerimeterMax this world
        member this.PerimeterMax = if notNull (this :> obj) then lens (nameof this.PerimeterMax) this this.GetPerimeterMax this.SetPerimeterMax else Cached.PerimeterMax
        member this.GetPerimeterCenterLocal world = World.getEntityPerimeterCenterLocal this world
        member this.SetPerimeterCenterLocal value world = World.setEntityPerimeterCenterLocal value this world |> ignore<bool>
        member this.PerimeterCenterLocal = if notNull (this :> obj) then lens (nameof this.PerimeterCenterLocal) this this.GetPerimeterCenterLocal this.SetPerimeterCenterLocal else Cached.PerimeterCenterLocal
        member this.GetPerimeterBottomLocal world = World.getEntityPerimeterBottomLocal this world
        member this.SetPerimeterBottomLocal value world = World.setEntityPerimeterBottomLocal value this world |> ignore<bool>
        member this.PerimeterBottomLocal = if notNull (this :> obj) then lens (nameof this.PerimeterBottomLocal) this this.GetPerimeterBottomLocal this.SetPerimeterBottomLocal else Cached.PerimeterBottomLocal
        member this.GetPerimeterBottomLeftLocal world = World.getEntityPerimeterBottomLeftLocal this world
        member this.SetPerimeterBottomLeftLocal value world = World.setEntityPerimeterBottomLeftLocal value this world |> ignore<bool>
        member this.PerimeterBottomLeftLocal = if notNull (this :> obj) then lens (nameof this.PerimeterBottomLeftLocal) this this.GetPerimeterBottomLeftLocal this.SetPerimeterBottomLeftLocal else Cached.PerimeterBottomLeftLocal
        member this.GetPerimeterMinLocal world = World.getEntityPerimeterMinLocal this world
        member this.SetPerimeterMinLocal value world = World.setEntityPerimeterMinLocal value this world |> ignore<bool>
        member this.PerimeterMinLocal = if notNull (this :> obj) then lens (nameof this.PerimeterMinLocal) this this.GetPerimeterMinLocal this.SetPerimeterMinLocal else Cached.PerimeterMinLocal
        member this.GetPerimeterMaxLocal world = World.getEntityPerimeterMaxLocal this world
        member this.SetPerimeterMaxLocal value world = World.setEntityPerimeterMaxLocal value this world |> ignore<bool>
        member this.PerimeterMaxLocal = if notNull (this :> obj) then lens (nameof this.PerimeterMaxLocal) this this.GetPerimeterMaxLocal this.SetPerimeterMaxLocal else Cached.PerimeterMaxLocal
        member this.GetPosition world = World.getEntityPosition this world
        member this.SetPosition value world = World.setEntityPosition value this world |> ignore<bool>
        member this.Position = if notNull (this :> obj) then lens (nameof this.Position) this this.GetPosition this.SetPosition else Cached.Position
        member this.GetPositionLocal world = World.getEntityPositionLocal this world
        member this.SetPositionLocal value world = World.setEntityPositionLocal value this world |> ignore<bool>
        member this.PositionLocal = if notNull (this :> obj) then lens (nameof this.PositionLocal) this this.GetPositionLocal this.SetPositionLocal else Cached.PositionLocal
        member this.GetRotation world = World.getEntityRotation this world
        member this.SetRotation value world = World.setEntityRotation value this world |> ignore<bool>
        member this.Rotation = if notNull (this :> obj) then lens (nameof this.Rotation) this this.GetRotation this.SetRotation else Cached.Rotation
        member this.GetRotationLocal world = World.getEntityRotationLocal this world
        member this.SetRotationLocal value world = World.setEntityRotationLocal value this world |> ignore<bool>
        member this.RotationLocal = if notNull (this :> obj) then lens (nameof this.RotationLocal) this this.GetRotationLocal this.SetRotationLocal else Cached.RotationLocal
        member this.GetScale world = World.getEntityScale this world
        member this.SetScale value world = World.setEntityScale value this world |> ignore<bool>
        member this.Scale = if notNull (this :> obj) then lens (nameof this.Scale) this this.GetScale this.SetScale else Cached.Scale
        member this.GetScaleLocal world = World.getEntityScaleLocal this world
        member this.SetScaleLocal value world = World.setEntityScaleLocal value this world |> ignore<bool>
        member this.ScaleLocal = if notNull (this :> obj) then lens (nameof this.ScaleLocal) this this.GetScaleLocal this.SetScaleLocal else Cached.ScaleLocal
        member this.GetOffset world = World.getEntityOffset this world
        member this.SetOffset value world = World.setEntityOffset value this world |> ignore<bool>
        member this.Offset = if notNull (this :> obj) then lens (nameof this.Offset) this this.GetOffset this.SetOffset else Cached.Offset
        member this.GetAngles world = World.getEntityAngles this world
        member this.SetAngles value world = World.setEntityAngles value this world |> ignore<bool>
        member this.Angles = if notNull (this :> obj) then lens (nameof this.Angles) this this.GetAngles this.SetAngles else Cached.Angles
        member this.GetAnglesLocal world = World.getEntityAnglesLocal this world
        member this.SetAnglesLocal value world = World.setEntityAnglesLocal value this world |> ignore<bool>
        member this.AnglesLocal = if notNull (this :> obj) then lens (nameof this.AnglesLocal) this this.GetAnglesLocal this.SetAnglesLocal else Cached.AnglesLocal
        member this.GetDegrees world = World.getEntityDegrees this world
        member this.SetDegrees value world = World.setEntityDegrees value this world |> ignore<bool>
        member this.Degrees = if notNull (this :> obj) then lens (nameof this.Degrees) this this.GetDegrees this.SetDegrees else Cached.Degrees
        member this.GetDegreesLocal world = World.getEntityDegreesLocal this world
        member this.SetDegreesLocal value world = World.setEntityDegreesLocal value this world |> ignore<bool>
        member this.DegreesLocal = if notNull (this :> obj) then lens (nameof this.DegreesLocal) this this.GetDegreesLocal this.SetDegreesLocal else Cached.DegreesLocal
        member this.GetSize world = World.getEntitySize this world
        member this.SetSize value world = World.setEntitySize value this world |> ignore<bool>
        member this.Size = if notNull (this :> obj) then lens (nameof this.Size) this this.GetSize this.SetSize else Cached.Size
        member this.GetElevation world = World.getEntityElevation this world
        member this.SetElevation value world = World.setEntityElevation value this world |> ignore<bool>
        member this.Elevation = if notNull (this :> obj) then lens (nameof this.Elevation) this this.GetElevation this.SetElevation else Cached.Elevation
        member this.GetElevationLocal world = World.getEntityElevationLocal this world
        member this.SetElevationLocal value world = World.setEntityElevationLocal value this world |> ignore<bool>
        member this.ElevationLocal = if notNull (this :> obj) then lens (nameof this.ElevationLocal) this this.GetElevationLocal this.SetElevationLocal else Cached.ElevationLocal
        member this.GetOverflow world = World.getEntityOverflow this world
        member this.SetOverflow value world = World.setEntityOverflow value this world |> ignore<bool>
        member this.Overflow = if notNull (this :> obj) then lens (nameof this.Overflow) this this.GetOverflow this.SetOverflow else Cached.Overflow
        member this.GetAffineMatrix world = World.getEntityAffineMatrix this world
        member this.AffineMatrix = if notNull (this :> obj) then lensReadOnly (nameof this.AffineMatrix) this this.GetAffineMatrix else Cached.AffineMatrix
        member this.GetAffineMatrixLocal world = World.getEntityAffineMatrixLocal this world
        member this.AffineMatrixLocal = if notNull (this :> obj) then lensReadOnly (nameof this.AffineMatrixLocal) this this.GetAffineMatrixLocal else Cached.AffineMatrixLocal
        member this.SetPerimeterUnscaled value world = World.setEntityPerimeterUnscaled value this world |> ignore<bool>
        member this.GetPerimeterUnscaled world = World.getEntityPerimeterUnscaled this world
        member this.PerimeterUnscaled = if notNull (this :> obj) then lens (nameof this.PerimeterUnscaled) this this.GetPerimeterUnscaled this.SetPerimeterUnscaled else Cached.PerimeterUnscaled
        member this.SetPerimeter value world = World.setEntityPerimeter value this world |> ignore<bool>
        member this.GetPerimeter world = World.getEntityPerimeter this world
        member this.Perimeter = if notNull (this :> obj) then lens (nameof this.Perimeter) this this.GetPerimeter this.SetPerimeter else Cached.Perimeter
        member this.GetBounds world = World.getEntityBounds this world
        member this.Bounds = if notNull (this :> obj) then lensReadOnly (nameof this.Bounds) this this.GetBounds else Cached.Bounds
        member this.GetMountOpt world = World.getEntityMountOpt this world
        member this.SetMountOpt value world = World.setEntityMountOpt value this world |> ignore<bool>
        member this.MountOpt = if notNull (this :> obj) then lens (nameof this.MountOpt) this this.GetMountOpt this.SetMountOpt else Cached.MountOpt
        member this.GetPropagationSourceOpt world = World.getEntityPropagationSourceOpt this world
        member this.SetPropagationSourceOpt value world = World.setEntityPropagationSourceOpt value this world |> ignore<bool>
        member this.PropagationSourceOpt = if notNull (this :> obj) then lens (nameof this.PropagationSourceOpt) this this.GetPropagationSourceOpt this.SetPropagationSourceOpt else Cached.PropagationSourceOpt
        member this.GetPresence world = World.getEntityPresence this world
        member this.SetPresence value world = World.setEntityPresence value this world |> ignore<bool>
        member this.Presence = if notNull (this :> obj) then lens (nameof this.Presence) this this.GetPresence this.SetPresence else Cached.Presence
        member this.GetAbsolute world = World.getEntityAbsolute this world
        member this.SetAbsolute value world = World.setEntityAbsolute value this world |> ignore<bool>
        member this.Absolute = if notNull (this :> obj) then lens (nameof this.Absolute) this this.GetAbsolute this.SetAbsolute else Cached.Absolute
        member this.GetEnabled world = World.getEntityEnabled this world
        member this.SetEnabled value world = World.setEntityEnabled value this world |> ignore<bool>
        member this.Enabled = if notNull (this :> obj) then lens (nameof this.Enabled) this this.GetEnabled this.SetEnabled else Cached.Enabled
        member this.GetEnabledLocal world = World.getEntityEnabledLocal this world
        member this.SetEnabledLocal value world = World.setEntityEnabledLocal value this world |> ignore<bool>
        member this.EnabledLocal = if notNull (this :> obj) then lens (nameof this.EnabledLocal) this this.GetEnabledLocal this.SetEnabledLocal else Cached.EnabledLocal
        member this.GetVisible world = World.getEntityVisible this world
        member this.SetVisible value world = World.setEntityVisible value this world |> ignore<bool>
        member this.Visible = if notNull (this :> obj) then lens (nameof this.Visible) this this.GetVisible this.SetVisible else Cached.Visible
        member this.GetVisibleLocal world = World.getEntityVisibleLocal this world
        member this.SetVisibleLocal value world = World.setEntityVisibleLocal value this world |> ignore<bool>
        member this.VisibleLocal = if notNull (this :> obj) then lens (nameof this.VisibleLocal) this this.GetVisibleLocal this.SetVisibleLocal else Cached.VisibleLocal
        member this.GetCastShadow world = World.getEntityCastShadow this world
        member this.SetCastShadow value world = World.setEntityCastShadow value this world |> ignore<bool>
        member this.CastShadow = if notNull (this :> obj) then lens (nameof this.CastShadow) this this.GetCastShadow this.SetCastShadow else Cached.CastShadow
        member this.GetPickable world = World.getEntityPickable this world
        member this.SetPickable value world = World.setEntityPickable value this world |> ignore<bool>
        member this.Pickable = if notNull (this :> obj) then lens (nameof this.Pickable) this this.GetPickable this.SetPickable else Cached.Pickable
        member this.GetAlwaysUpdate world = World.getEntityAlwaysUpdate this world
        member this.SetAlwaysUpdate value world = World.setEntityAlwaysUpdate value this world |> ignore<bool>
        member this.AlwaysUpdate = if notNull (this :> obj) then lens (nameof this.AlwaysUpdate) this this.GetAlwaysUpdate this.SetAlwaysUpdate else Cached.AlwaysUpdate
        member this.GetAlwaysRender world = World.getEntityAlwaysRender this world
        member this.SetAlwaysRender value world = World.setEntityAlwaysRender value this world |> ignore<bool>
        member this.AlwaysRender = if notNull (this :> obj) then lens (nameof this.AlwaysRender) this this.GetAlwaysRender this.SetAlwaysRender else Cached.AlwaysRender
        member this.GetProtected world = World.getEntityProtected this world
        member this.Protected = if notNull (this :> obj) then lensReadOnly (nameof this.Protected) this this.GetProtected else Cached.Protected
        member this.GetPersistent world = World.getEntityPersistent this world
        member this.SetPersistent value world = World.setEntityPersistent value this world |> ignore<bool>
        member this.Persistent = if notNull (this :> obj) then lens (nameof this.Persistent) this this.GetPersistent this.SetPersistent else Cached.Persistent
        member this.GetIs2d world = World.getEntityIs2d this world
        member this.Is2d = if notNull (this :> obj) then lensReadOnly (nameof this.Is2d) this this.GetIs2d else Cached.Is2d
        member this.GetIs3d world = World.getEntityIs3d this world
        member this.Is3d = if notNull (this :> obj) then lensReadOnly (nameof this.Is3d) this this.GetIs3d else Cached.Is3d
        member this.GetStatic world = World.getEntityStatic this world
        member this.SetStatic value world = World.setEntityStatic value this world |> ignore<bool>
        member this.Static = if notNull (this :> obj) then lens (nameof this.Static) this this.GetStatic this.SetStatic else Cached.Static
        member this.GetPhysical world = World.getEntityPhysical this world
        member this.Physical = if notNull (this :> obj) then lensReadOnly (nameof this.Physical) this this.GetPhysical else Cached.Physical
        member this.GetLightProbe world = World.getEntityLightProbe this world
        member this.LightProbe = if notNull (this :> obj) then lensReadOnly (nameof this.LightProbe) this this.GetLightProbe else Cached.LightProbe
        member this.GetLight world = World.getEntityLight this world
        member this.Light = if notNull (this :> obj) then lensReadOnly (nameof this.Light) this this.GetLight else Cached.Light
        member this.GetDestroying world = World.getEntityDestroying this world
        member this.Destroying = if notNull (this :> obj) then lensReadOnly (nameof this.Destroying) this this.GetDestroying else Cached.Destroying
        member this.GetOverlayNameOpt world = World.getEntityOverlayNameOpt this world
        member this.OverlayNameOpt = if notNull (this :> obj) then lensReadOnly (nameof this.OverlayNameOpt) this this.GetOverlayNameOpt else Cached.OverlayNameOpt
        member this.SetFacetNames value world = World.setEntityFacetNames value this world |> ignore<bool>
        member this.GetFacetNames world = World.getEntityFacetNames this world
        member this.FacetNames = if notNull (this :> obj) then lens (nameof this.FacetNames) this this.GetFacetNames this.SetFacetNames else Cached.FacetNames
        member this.GetPropagatedDescriptorOpt world = World.getEntityPropagatedDescriptorOpt this world
        member this.SetPropagatedDescriptorOpt value world = World.setEntityPropagatedDescriptorOpt value this world |> ignore<bool>
        member this.PropagatedDescriptorOpt = if notNull (this :> obj) then lens (nameof this.PropagatedDescriptorOpt) this this.GetPropagatedDescriptorOpt this.SetPropagatedDescriptorOpt else Cached.PropagatedDescriptorOpt
        member this.GetOrder world = World.getEntityOrder this world
        member this.SetOrder value world = World.setEntityOrder value this world |> ignore<bool>
        member this.Order = if notNull (this :> obj) then lens (nameof this.Order) this this.GetOrder this.SetOrder else Cached.Order
        member this.GetId world = World.getEntityId this world
        member this.Id = if notNull (this :> obj) then lensReadOnly (nameof this.Id) this this.GetId else Cached.Id
        static member internal init () =
            Cached.Dispatcher <- lensReadOnly (nameof Cached.Dispatcher) Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Facets <- lensReadOnly (nameof Cached.Facets) Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Transform <- lens (nameof Cached.Transform) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.PerimeterUnscaled <- lens (nameof Cached.PerimeterUnscaled) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Perimeter <- lens (nameof Cached.Perimeter) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Bounds <- lensReadOnly (nameof Cached.Bounds) Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.PerimeterCenter <- lens (nameof Cached.PerimeterCenter) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.PerimeterBottom <- lens (nameof Cached.PerimeterBottom) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.PerimeterBottomLeft <- lens (nameof Cached.PerimeterBottomLeft) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.PerimeterMin <- lens (nameof Cached.PerimeterMin) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.PerimeterMax <- lens (nameof Cached.PerimeterMax) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.PerimeterCenterLocal <- lens (nameof Cached.PerimeterCenterLocal) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.PerimeterBottomLocal <- lens (nameof Cached.PerimeterBottomLocal) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.PerimeterBottomLeftLocal <- lens (nameof Cached.PerimeterBottomLeftLocal) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.PerimeterMinLocal <- lens (nameof Cached.PerimeterMinLocal) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.PerimeterMaxLocal <- lens (nameof Cached.PerimeterMaxLocal) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Position <- lens (nameof Cached.Position) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.PositionLocal <- lens (nameof Cached.PositionLocal) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Rotation <- lens (nameof Cached.Rotation) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.RotationLocal <- lens (nameof Cached.RotationLocal) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Scale <- lens (nameof Cached.Scale) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.ScaleLocal <- lens (nameof Cached.ScaleLocal) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Offset <- lens (nameof Cached.Offset) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Angles <- lens (nameof Cached.Angles) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.AnglesLocal <- lens (nameof Cached.AnglesLocal) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Degrees <- lens (nameof Cached.Degrees) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.DegreesLocal <- lens (nameof Cached.DegreesLocal) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Size <- lens (nameof Cached.Size) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Elevation <- lens (nameof Cached.Elevation) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.ElevationLocal <- lens (nameof Cached.ElevationLocal) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Overflow <- lens (nameof Cached.Overflow) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.AffineMatrix <- lensReadOnly (nameof Cached.AffineMatrix) Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.AffineMatrixLocal <- lensReadOnly (nameof Cached.AffineMatrixLocal) Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Presence <- lens (nameof Cached.Presence) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Absolute <- lens (nameof Cached.Absolute) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.MountOpt <- lens (nameof Cached.MountOpt) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.PropagationSourceOpt <- lens (nameof Cached.PropagationSourceOpt) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Enabled <- lens (nameof Cached.Enabled) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.EnabledLocal <- lens (nameof Cached.EnabledLocal) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Visible <- lens (nameof Cached.Visible) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.VisibleLocal <- lens (nameof Cached.VisibleLocal) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.CastShadow <- lens (nameof Cached.CastShadow) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Pickable <- lens (nameof Cached.Pickable) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.AlwaysUpdate <- lens (nameof Cached.AlwaysUpdate) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.AlwaysRender <- lens (nameof Cached.AlwaysRender) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Persistent <- lens (nameof Cached.Persistent) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Is2d <- lensReadOnly (nameof Cached.Is2d) Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Is3d <- lensReadOnly (nameof Cached.Is3d) Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Static <- lens (nameof Cached.Static) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Physical <- lensReadOnly (nameof Cached.Physical) Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.LightProbe <- lens (nameof Cached.LightProbe) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Light <- lens (nameof Cached.Light) Unchecked.defaultof<_> Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Optimized <- lensReadOnly (nameof Cached.Optimized) Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Destroying <- lensReadOnly (nameof Cached.Destroying) Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.OverlayNameOpt <- lensReadOnly (nameof Cached.OverlayNameOpt) Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.FacetNames <- lensReadOnly (nameof Cached.FacetNames) Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.PropagatedDescriptorOpt <- lensReadOnly (nameof Cached.PropagatedDescriptorOpt) Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Order <- lensReadOnly (nameof Cached.Order) Unchecked.defaultof<_> Unchecked.defaultof<_>
            Cached.Id <- lensReadOnly (nameof Cached.Id) Unchecked.defaultof<_> Unchecked.defaultof<_>

        member this.RegisterEvent = Events.RegisterEvent --> this
        member this.UnregisteringEvent = Events.UnregisteringEvent --> this
        member this.ChangeEvent propertyName = Events.ChangeEvent propertyName --> this
        member this.UpdateEvent = Events.UpdateEvent --> this
        member this.MountEvent = Events.MountEvent --> this
        member this.UnmountEvent = Events.UnmountEvent --> this
        member this.BodyPenetrationEvent = Events.BodyPenetrationEvent --> this
        member this.BodySeparationExplicitEvent = Events.BodySeparationExplicitEvent --> this
        member this.BodySeparationImplicitEvent = Events.BodySeparationImplicitEvent --> Game.Handle
        member this.BodyTransformEvent = Events.BodyTransformEvent --> this

        /// Get the optional presence override.
        member this.GetPresenceOverride world =
            World.getEntityPresenceOverride this world

        /// Set the transform of an entity.
        member this.SetTransformByRef (value : Transform byref, world) =
            World.setEntityTransformByRef (&value, World.getEntityState this world, this, world)

        /// Set the transform of an entity without generating any change events.
        member this.SetTransformByRefWithoutEvent (value : Transform inref, world) =
            World.setEntityTransformByRefWithoutEvent (&value, World.getEntityState this world, this, world)

        /// Set the transform of an entity without generating any change events.
        member this.SetTransformWithoutEvent value world =
            World.setEntityTransformByRefWithoutEvent (&value, World.getEntityState this world, this, world)

        /// Set the transform of an entity snapped to the give position and rotation snaps.
        member this.SetTransformPositionSnapped positionSnap (value : Transform) world =
            let mutable transform = value
            Transform.snapPosition (positionSnap, &transform)
            this.SetTransform transform world

        /// Try to get a property value and type.
        member this.TryGetProperty propertyName world =
            let mutable property = Unchecked.defaultof<_>
            let found = World.tryGetEntityProperty (propertyName, this, world, &property)
            if found then Some property else None

        /// Get a property value and type.
        member this.GetProperty propertyName world =
            World.getEntityProperty propertyName this world

        /// Try to get an xtension property value.
        member this.TryGet<'a> propertyName world : 'a voption =
            World.tryGetEntityXtensionValue<'a> propertyName this world

        /// Get an xtension property value.
        member this.Get<'a> propertyName world : 'a =
            World.getEntityXtensionValue<'a> propertyName this world

        /// Try to set a property value with explicit type.
        member this.TrySetProperty propertyName property world =
            World.trySetEntityProperty propertyName property this world

        /// Set a property value with explicit type.
        member this.SetProperty propertyName property world =
            World.setEntityProperty propertyName property this world |> ignore<bool>

        /// To try set an xtension property value.
        member this.TrySet<'a> propertyName (value : 'a) world =
            World.trySetEntityXtensionValue propertyName value this world

        /// Set an xtension property value.
        member this.Set<'a> propertyName (value : 'a) world =
            World.setEntityXtensionValue<'a> propertyName value this world

        /// Set an xtension property value without publishing an event.
        member internal this.SetXtensionPropertyWithoutEvent<'a> propertyName (value : 'a) world =
            let property = { PropertyType = typeof<'a>; PropertyValue = value }
            World.setEntityXtensionPropertyWithoutEvent propertyName property this world |> ignore<struct (bool * bool)>

        /// Get an entity's sorting priority in 2d.
        member this.GetSortingPriority2d world = World.getEntitySortingPriority2d this world

        /// Get an entity's inferred attributes.
        member this.GetAttributesInferred world = World.getEntityAttributesInferred this world

        /// Check that an entity is in view irrespective of eye center.
        member this.GetInView2dAbsolute world = World.getEntityInView2dAbsolute this world

        /// Check that an entity is in relative to eye center.
        member this.GetInView2dRelative world = World.getEntityInView2dRelative this world

        /// Check that an entity is in the play area irrespective of eye center.
        member this.GetInPlay2dAbsolute world = World.getEntityInPlay2dAbsolute this world

        /// Check that an entity is in the play area relative to eye center.
        member this.GetInPlay2dRelative world = World.getEntityInPlay2dRelative this world

        /// Check that an entity is in the eye's view.
        member this.GetInView3d world = World.getEntityInView3d this world

        /// Check that an entity exists in the world.
        member this.GetExists world = World.getEntityExists this world

        /// Check that an entity is selected.
        member this.GetSelected world = World.getEntitySelected this world

        /// Check that this entity is mounted by another entity.
        member this.GetMounted world = World.getEntityMounted this world

        /// Attempt to get an entity on which this entity is mounted.
        member this.TryGetMountee world = Option.bind (flip tryResolve this) (this.GetMountOpt world)

        /// Check that this entity is mounted on another entity.
        member this.HasMountee world = Option.isSome (this.TryGetMountee world)

        /// Check that this entity is mounted on another entity.
        member this.IsMounter world = this.HasMountee world

        /// Check that an entity is intersected by a ray.
        member this.RayCast ray world = World.rayCastEntity ray this world

        /// Automatically change an entity's bounds using its inferred attributes.
        member this.AutoBounds world = World.autoBoundsEntity this world

        /// Set an entity's mount while adjusting its mount properties such that they do not change.
        member this.SetMountOptWithAdjustment (value : Entity Address option) world =
            match (Option.bind (flip tryResolve this) (this.GetMountOpt world), Option.bind (flip tryResolve this) value) with
            | (Some mountOld, Some mountNew) ->
                if mountOld <> mountNew && mountOld.GetExists world && mountNew.GetExists world then
                    let affineMatrixMount = World.getEntityAffineMatrix mountNew world
                    let affineMatrixMounter = World.getEntityAffineMatrix this world
                    let affineMatrixLocal = affineMatrixMounter * affineMatrixMount.Inverted
                    let mutable positionLocal = Unchecked.defaultof<_>
                    let mutable rotationLocal = Unchecked.defaultof<_>
                    let mutable scaleLocal = Unchecked.defaultof<_>
                    Matrix4x4.Decompose (affineMatrixLocal, &scaleLocal, &rotationLocal, &positionLocal) |> ignore<bool>
                    this.SetPositionLocal positionLocal world
                    this.SetRotationLocal rotationLocal world
                    this.SetScaleLocal scaleLocal world
                    let elevationLocal = this.GetElevation world - mountNew.GetElevation world
                    this.SetElevationLocal elevationLocal world
                    this.SetEnabled (this.GetEnabledLocal world && mountNew.GetEnabled world) world
                    this.SetVisible (this.GetVisibleLocal world && mountNew.GetVisible world) world
                    this.SetMountOpt value world
            | (Some mountOld, None) ->
                if mountOld.GetExists world then
                    this.SetMountOpt value world
                    let position = this.GetPosition world
                    let rotation = this.GetRotation world
                    let scale = this.GetScale world
                    this.SetPositionLocal v3Zero world
                    this.SetRotationLocal quatIdentity world
                    this.SetScaleLocal v3One world
                    this.SetPosition position world
                    this.SetRotation rotation world
                    this.SetScale scale world
                    let elevation = this.GetElevation world
                    this.SetElevationLocal 0.0f world
                    this.SetElevation elevation world
                    this.SetEnabled (this.GetEnabledLocal world) world // NOTE: redundant from SetMountOpt.
                    this.SetVisible (this.GetVisibleLocal world) world // NOTE: redundant from SetMountOpt.
            | (None, Some mountNew) ->
                if mountNew.GetExists world then
                    let affineMatrixMount = World.getEntityAffineMatrix mountNew world
                    let affineMatrixMounter = World.getEntityAffineMatrix this world
                    let affineMatrixLocal = affineMatrixMounter * affineMatrixMount.Inverted
                    let mutable positionLocal = Unchecked.defaultof<_>
                    let mutable rotationLocal = Unchecked.defaultof<_>
                    let mutable scaleLocal = Unchecked.defaultof<_>
                    Matrix4x4.Decompose (affineMatrixLocal, &scaleLocal, &rotationLocal, &positionLocal) |> ignore<bool>
                    this.SetPositionLocal positionLocal world
                    this.SetRotationLocal rotationLocal world
                    this.SetScaleLocal scaleLocal world
                    let elevationLocal = this.GetElevation world - mountNew.GetElevation world
                    this.SetElevationLocal elevationLocal world
                    this.SetEnabledLocal (this.GetEnabled world && mountNew.GetEnabled world) world
                    this.SetVisibleLocal (this.GetVisible world && mountNew.GetVisible world) world
                    this.SetMountOpt value world
            | (None, None) -> this.SetMountOpt value world

        /// Check whether the entity's mount exists.
        member this.MountExists world =
            match Option.bind (flip tryResolve this) (this.GetMountOpt world) with
            | Some mount -> mount.GetExists world
            | None -> false

        /// Check than an entity has any other entitiese mounted on it.
        member this.HasMounters world = World.getEntityHasMounters this world

        /// Get an entity's mounters.
        member this.GetMounters world = World.getEntityMounters this world

        /// Traverse an entity's mounters.
        member this.TraverseMounters effect world = World.traverseEntityMounters effect this world

        /// Check that an entity has children.
        member this.HasChildren world = World.getEntityHasChildren this world

        /// Get an entity's children.
        member this.GetChildren world = World.getEntityChildren this world

        /// Traverse an entity's children.
        member this.TraverseChildren effect world = World.traverseEntityChildren effect this world

        /// Get an entity's descendants.
        member this.GetDescendants world = World.getEntityDescendants this world

        /// Check that entity has entities to propagate its structure to.
        member this.HasPropagationTargets world = World.hasPropagationTargets this world

        /// Find all the entities to which an entity may propagate its structure.
        member this.GetPropagationTargets world = World.getPropagationTargets this world

        /// Apply physics changes to an entity.
        member this.Physics (center : Vector3) rotation linearVelocity angularVelocity world =
            let mutable transformOld = this.GetTransform world
            let mutable transformNew = transformOld
            if this.GetIs2d world then
                if  v3Neq transformOld.PerimeterCenter center ||
                    quatNeq transformOld.Rotation rotation then
                    transformNew.PerimeterCenter <- center
                    transformNew.Rotation <- rotation
                    this.SetTransformByRefWithoutEvent (&transformNew, world)
            else
                if  v3Neq transformOld.Position center ||
                    quatNeq transformOld.Rotation rotation then
                    transformNew.Position <- center
                    transformNew.Rotation <- rotation
                    this.SetTransformByRefWithoutEvent (&transformNew, world)
            this.SetXtensionPropertyWithoutEvent "LinearVelocity" linearVelocity world
            this.SetXtensionPropertyWithoutEvent "AngularVelocity" angularVelocity world
            let dispatcher = this.GetDispatcher world
            dispatcher.Physics (center, rotation, linearVelocity, angularVelocity, this, world)

        /// Propagate entity physics properties into the physics system.
        member this.PropagatePhysics world =
            if WorldModule.getSelected this world then
                World.propagateEntityPhysics this world

        /// Check that an entity uses a facet of the given type.
        member this.Has (facetType, world) = Array.exists (fun facet -> getType facet = facetType) (this.GetFacets world)

        /// Check that an entity uses a facet of the given type.
        member this.Has<'a> world = this.Has (typeof<'a>, world)

        /// Check that an entity dispatches in the same manner as the dispatcher with the given type.
        member this.Is (dispatcherType, world) = Reflection.dispatchesAs dispatcherType (this.GetDispatcher world)

        /// Check that an entity dispatches in the same manner as the dispatcher with the given type.
        member this.Is<'a> world = this.Is (typeof<'a>, world)

        /// Send a signal to an entity.
        member this.Signal (signal : Signal) world = (this.GetDispatcher world).Signal (signal, this, world)

        /// Notify the engine that an entity's MMCC model has changed in some automatically undetectable way (such as being mutated directly by user code).
        member this.NotifyModelChange world = World.notifyEntityModelChange this world

    type World with

        /// Rename an entity. Note that since this destroys the renamed entity immediately, you should not call this
        /// inside an event handler that involves the reassigned entity itself. Note this also renames all of its
        /// descendents accordingly.
        static member renameEntityImmediate source (destination : Entity) world =
            let entityStateOpt = World.getEntityStateOpt source world
            match entityStateOpt :> obj with
            | null -> ()
            | _ ->

                // transfer entity state to destination
                let entityState = { entityStateOpt with Id = Gen.id64; Surnames = destination.Surnames; Content = EntityContent.empty }
                let children = World.getEntityChildren source world
                let order = World.getEntityOrder source world
                World.destroyEntityImmediateInternal false source world
                World.addEntity entityState destination world

                // update order
                World.setEntityOrder order destination world |> ignore<bool>

                // rename children
                for child in children do
                    let destination = destination / child.Name
                    World.renameEntityImmediate child destination world

                // update publish update flag
                World.updateEntityPublishUpdateFlag destination world |> ignore<bool>

                // update presence property from override
                World.updateEntityPresenceOverride destination world

                // process if needed
                if WorldModule.UpdatingSimulants && World.getEntitySelected destination world then
                    WorldModule.tryProcessEntity true destination world

                // update propagation sources
                for target in World.getPropagationTargets source world do
                    if World.getEntityExists target world then
                        World.setEntityPropagationSourceOpt (Some destination) target world |> ignore<bool>

                // insert a propagated descriptor if needed
                match World.getEntityPropagatedDescriptorOpt destination world with
                | None when World.hasPropagationTargets destination world ->
                    let propagatedDescriptor = World.writeEntity false false EntityDescriptor.empty destination world
                    World.setEntityPropagatedDescriptorOpt (Some propagatedDescriptor) destination world |> ignore<bool>
                | Some _ | None -> ()

                // mount
                let mountOpt = World.getEntityMountOpt destination world
                if  source.Parent <> destination.Parent &&
                    Option.isSome mountOpt &&
                    World.getEntityAllowedToMount destination world then
                    destination.SetMountOptWithAdjustment None world // NOTE: we have to set mount to none in order to convince the engine it's changing.
                    destination.SetMountOptWithAdjustment mountOpt world

        /// Rename an entity.
        static member renameEntity source destination world =
            World.defer (World.renameEntityImmediate source destination) Game.Handle world

        static member internal updateEntity (entity : Entity) world =
            let facets = entity.GetFacets world
            if Array.notEmpty facets then // OPTIMIZATION: eliding iteration setup for speed.
                for facet in facets do
                    facet.Update (entity, world)
            let dispatcher = entity.GetDispatcher world
            dispatcher.Update (entity, world)
            if World.getEntityPublishUpdates entity world then
                let eventTrace = EventTrace.debug "World" "updateEntity" "" EventTrace.empty
                World.publishPlus () entity.UpdateEvent eventTrace entity false false world

        static member internal renderEntity renderPass (entity : Entity) world =
            let facets = entity.GetFacets world
            for facet in facets do
                facet.Render (renderPass, entity, world)
            let dispatcher = entity.GetDispatcher world
            dispatcher.Render (renderPass, entity, world)

        /// Edit an entity with the given operation using the ImGui APIs.
        /// Intended only to be called by editors like Gaia.
        static member editEntity operation (entity : Entity) world =
            let facets = entity.GetFacets world
            if Array.notEmpty facets then // OPTIMIZATION: iteration setup.
                for facet in facets do
                    facet.Edit (operation, entity, world)
            let dispatcher = entity.GetDispatcher world
            dispatcher.Edit (operation, entity, world)

        /// Attempt to truncate an entity model.
        static member tryTruncateEntityModel<'model> (model : 'model) (entity : Entity) world =
            let dispatcher = entity.GetDispatcher world
            dispatcher.TryTruncateModel<'model> model

        /// Attempt to untruncate an entity model.
        static member tryUntruncateEntityModel<'model> (model : 'model) (entity : Entity) world =
            let dispatcher = entity.GetDispatcher world
            dispatcher.TryUntruncateModel<'model> (model, entity, world)

        /// Get all the entities in a group with the given dispatcher type.
        static member getEntitiesAs<'d when 'd :> EntityDispatcher> (group : Group) (world : World) : Entity USet =
            match world.EntitiesIndexed.TryGetValue struct (group, typeof<'d>) with
            | (true, entities) -> entities
            | (false, _) -> USet.makeEmpty HashIdentity.Structural (World.getCollectionConfig world)

        /// Get all the entities in a group that have a given facet type.
        static member getEntitiesWith<'f when 'f :> Facet> (group : Group) (world : World) : Entity USet =
            match world.EntitiesIndexed.TryGetValue struct (group, typeof<'f>) with
            | (true, entities) -> entities
            | (false, _) -> USet.makeEmpty HashIdentity.Structural (World.getCollectionConfig world)

        /// Get all the entities in a group.
        static member getEntities (group : Group) (world : World) : Entity USet =
            match world.EntitiesIndexed.TryGetValue struct (group, typeof<EntityDispatcher>) with
            | (true, entities) -> entities
            | (false, _) -> USet.makeEmpty HashIdentity.Structural (World.getCollectionConfig world)

        /// Get all the entities in a group in depth-first order.
        static member getEntitiesDepthFirst (group : Group) (world : World) =
            match world.Simulants.TryGetValue group with
            | (true, childrenOpt) ->
                match childrenOpt with
                | Some children ->
                    seq {
                        for child in children do
                            let childEntity = child :?> Entity
                            yield childEntity
                            yield! childEntity.GetDescendants world }
                | None -> Seq.empty
            | (false, _) -> Seq.empty

        /// Get all the entities directly parented by the group.
        static member getSovereignEntities (group : Group) (world : World) =
            match world.Simulants.TryGetValue (group :> Simulant) with
            | (true, childrenOpt) ->
                match childrenOpt with
                | Some children -> children |> Seq.map cast<Entity>
                | None -> Seq.empty
            | (false, _) -> Seq.empty

        /// Destroy an entity in the world at the end of the current update.
        static member destroyEntity (entity : Entity) world =
            World.addSimulantToDestruction entity world

        /// Destroy multiple entities in the world immediately. Can be dangerous if existing in-flight publishing
        /// depends on any of the entities' existences. Consider using World.destroyEntities instead.
        static member destroyEntitiesImmediate (entities : Entity seq) world =
            for entity in entities |> Seq.rev |> List.ofSeq do
                World.destroyEntityImmediate entity world

        /// Destroy multiple entities in the world at the end of the current update.
        static member destroyEntities entities world =
            World.defer (World.destroyEntitiesImmediate entities) Game.Handle world

        /// Sort the given entities by 2d sorting priority.
        /// If there are a lot of entities, this may allocate in the LOH.
        static member sortEntities2d entities world =
            entities
            |> Array.ofSeq
            |> Array.rev
            |> Array.map (fun (entity : Entity) -> entity.GetSortingPriority2d world)
            |> Array.sortStableWith SortPriority.compare
            |> Array.map (fun p -> p.SortTarget :?> Entity)

        /// Attempt to pick an entity at the given position.
        static member tryPickEntity2d position entities world =
            let entitiesSorted = World.sortEntities2d entities world
            Array.tryFind (fun (entity : Entity) ->
                if entity.GetPickable world then
                    let absolute = entity.GetAbsolute world
                    let positionWorld = Viewport.mouseToWorld2d absolute world.Eye2dCenter world.Eye2dSize position world.RasterViewport
                    let bounds = (entity.GetBounds world).Box2
                    bounds.Intersects positionWorld
                else false)
                entitiesSorted

        /// Attempt to pick a 3d entity with the given ray.
        static member tryPickEntity3d position entities (world : World) =
            let intersectionses =
                Seq.map (fun (entity : Entity) ->
                    if entity.GetPickable world then
                        let rayWorld = Viewport.mouseToWorld3d world.Eye3dCenter world.Eye3dRotation world.Eye3dFieldOfView position world.RasterViewport
                        let bounds = entity.GetBounds world
                        let intersectionOpt = rayWorld.Intersects bounds
                        if intersectionOpt.HasValue then
                            entity.RayCast rayWorld world
                            |> Seq.filter _.IsHit
                            |> Seq.map (function Hit intersection -> (intersection, entity) | _ -> failwithumf ())
                            |> Seq.toArray
                        else [||]
                    else [||])
                    entities
            let intersections = intersectionses |> Seq.concat |> Seq.toArray
            let sorted = Array.sortBy fst intersections
            Array.tryHead sorted

        /// Try to find the entity among the given entity's peers with the closest previous order.
        static member tryGetPreviousEntity (entity : Entity) world =
            match entity.Parent with
            | :? Entity as parent ->
                let order = World.getEntityOrder entity world
                World.getEntityChildren parent world
                |> Seq.map (fun child -> (child.GetOrder world, child))
                |> Array.ofSeq
                |> Array.sortBy fst
                |> Array.rev
                |> Array.tryFind (fun (order', _) -> order' < order)
                |> Option.map snd
            | :? Group as parent ->
                let order = World.getEntityOrder entity world
                World.getSovereignEntities parent world
                |> Seq.map (fun child -> (child.GetOrder world, child))
                |> Array.ofSeq
                |> Array.sortBy fst
                |> Array.rev
                |> Array.tryFind (fun (order', _) -> order' < order)
                |> Option.map snd
            | _ -> failwithumf ()

        /// Try to find the entity among the given entity's peers with the closest next order.
        static member tryGetNextEntity (entity : Entity) world =
            match entity.Parent with
            | :? Entity as parent ->
                let order = World.getEntityOrder entity world
                World.getEntityChildren parent world
                |> Seq.map (fun child -> (child.GetOrder world, child))
                |> Array.ofSeq
                |> Array.sortBy fst
                |> Array.tryFind (fun (order', _) -> order' > order)
                |> Option.map snd
            | :? Group as parent ->
                let order = World.getEntityOrder entity world
                World.getSovereignEntities parent world
                |> Seq.map (fun child -> (child.GetOrder world, child))
                |> Array.ofSeq
                |> Array.sortBy fst
                |> Array.tryFind (fun (order', _) -> order' > order)
                |> Option.map snd
            | _ -> None

        /// Swap the orders of two entities.
        static member swapEntityOrders entity entity2 world =
            let order = World.getEntityOrder entity world
            World.setEntityOrder (World.getEntityOrder entity2 world) entity world |> ignore<bool>
            World.setEntityOrder order entity2 world |> ignore<bool>

        /// Insert an entity's order between optional previous entity and next entity.
        static member insertEntityOrder (entity : Entity) (previousOpt : Entity option) (next : Entity) world =
            let order = 
                match previousOpt with
                | Some previous -> (previous.GetOrder world + next.GetOrder world) / 2L
                | None -> next.GetOrder world / 2L
            World.setEntityOrder order entity world |> ignore<bool>

        static member private generateEntitySequentialName2 dispatcherName (entityNames : string HashSet) =
            let mutable name = Gen.nameForEditor dispatcherName
            if entityNames.Contains name 
            then World.generateEntitySequentialName2 dispatcherName entityNames
            else name

        /// Generate a sequential, editor-friendly entity name.
        static member generateEntitySequentialName dispatcherName group (world : World) =
            let entityNames =
                World.getEntities group world
                |> Seq.map _.Name
                |> hashSetPlus StringComparer.Ordinal
            World.generateEntitySequentialName2 dispatcherName entityNames

        /// Clear any entity on the world's clipboard.
        static member clearEntityFromClipboard (_ : World) =
            Clipboard <- None

        /// Copy an entity to the world's clipboard.
        static member copyEntityToClipboard entity world =
            let entityDescriptor = World.writeEntity false false EntityDescriptor.empty entity world
            Clipboard <- Some (false, entityDescriptor, entity)

        /// Cut an entity to the world's clipboard.
        static member cutEntityToClipboard (entity : Entity) world =
            let entityDescriptor = World.writeEntity false true EntityDescriptor.empty entity world
            Clipboard <- Some (true, entityDescriptor, entity)
            World.destroyEntityImmediate entity world

        /// Check that there's an entity on the world's clipboard to paste.
        static member canPasteEntityFromClipboard (_ : World) =
            Clipboard.IsSome
        
        /// Paste an entity from the given entity descriptor.
        static member pasteEntityFromDescriptor (distance : single) rightClickPosition positionSnapEir pasteType cut entityDescriptor (entitySource : Entity) (parent : Simulant) world =
            let nameOpt =
                if cut then // try to preserve name only if cut
                    match entityDescriptor.EntityProperties.TryGetValue Constants.Engine.NamePropertyName with
                    | (true, nameSymbol) ->
                        let name = symbolToValue nameSymbol
                        let entityProposed = parent.Names |> Array.add name |> Entity
                        if World.getEntityExists entityProposed world
                        then Some (World.generateEntitySequentialName entityDescriptor.EntityDispatcherName entityProposed.Group world)
                        else Some name
                    | (_, _) -> Log.info "EntityDescriptor missing its Name property."; None
                else
                    let group = Group (Array.take 3 parent.Names)
                    Some (World.generateEntitySequentialName entityDescriptor.EntityDispatcherName group world) // otherwise use generated name
            let entity = World.readEntity false false entityDescriptor nameOpt parent world
            let (position, positionSnapOpt) =
                let absolute = entity.GetAbsolute world
                if entity.GetIs2d world then
                    let position =
                        match pasteType with
                        | PasteAtMouse -> (Viewport.mouseToWorld2d absolute world.Eye2dCenter world.Eye2dSize rightClickPosition world.RasterViewport).V3
                        | PasteAtLook -> world.Eye2dCenter.V3
                        | PasteAt position -> position
                    match positionSnapEir with
                    | Left positionSnap -> (position, Some positionSnap)
                    | Right _ -> (position, None)
                else
                    let position =
                        match pasteType with
                        | PasteAtMouse ->
                            let ray = Viewport.mouseToWorld3d world.Eye3dCenter world.Eye3dRotation world.Eye3dFieldOfView rightClickPosition world.RasterViewport
                            let forward = world.Eye3dRotation.Forward
                            let plane = plane3 (world.Eye3dCenter + forward * distance) -forward
                            let intersectionOpt = ray.Intersection plane
                            intersectionOpt.Value
                        | PasteAtLook -> world.Eye3dCenter + v3Forward.Transform world.Eye3dRotation * distance
                        | PasteAt position -> position
                    match positionSnapEir with
                    | Right positionSnap -> (position, Some positionSnap)
                    | Left _ -> (position, None)
            let mutable transform = entity.GetTransform world
            transform.Position <- position
            match positionSnapOpt with Some positionSnap -> Transform.snapPosition (positionSnap, &transform) | None -> ()
            entity.SetTransform transform world
            if not cut then
                match entity.GetPropagationSourceOpt world with
                | None -> if entitySource.GetExists world then entity.SetPropagationSourceOpt (Some entitySource) world
                | Some _ -> ()
            else entity.SetPropagationSourceOpt None world
            let rec getDescendantPairs source entity world =
                [for child in World.getEntityChildren entity world do
                    let childSource = source / child.Name
                    yield (childSource, child)
                    yield! getDescendantPairs childSource child world]
            for (descendantSource, descendentEntity) in getDescendantPairs entitySource entity world do
                if descendentEntity.GetExists world then
                    World.setEntityPropagatedDescriptorOpt None descendentEntity world |> ignore<bool>
                    if descendantSource.GetExists world && descendantSource.HasPropagationTargets world then
                        World.setEntityPropagationSourceOpt (Some descendantSource) descendentEntity world |> ignore<bool>
            let mountOpt = match parent with :? Entity -> Some (Address.makeParent ()) | _ -> None
            entity.SetMountOptWithAdjustment mountOpt world
            entity

        /// Paste an entity.
        static member pasteEntity (distance : single) rightClickPosition positionSnapEir pasteType entity (parent : Simulant) world =
            let entityDescriptor = World.writeEntity false false EntityDescriptor.empty entity world
            World.pasteEntityFromDescriptor distance rightClickPosition positionSnapEir pasteType false entityDescriptor entity parent world

        /// Paste an entity from the world's clipboard.
        static member tryPasteEntityFromClipboard distance rightClickPosition positionSnapEir pasteType parent world =
            match Clipboard with
            | Some (cut, entityDescriptor, entitySource) ->
                let entity = World.pasteEntityFromDescriptor distance rightClickPosition positionSnapEir pasteType cut entityDescriptor entitySource parent world
                Some entity
            | None -> None