// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// Whose scenery a scenery entity is.
[<AutoOpen>]
module SceneryDispatcherExtensions =
    type Entity with
        member this.GetAreaTypeOpt world : AreaType option = this.Get (nameof this.AreaTypeOpt) world
        member this.SetAreaTypeOpt (value : AreaType option) world = this.Set (nameof this.AreaTypeOpt) value world
        member this.AreaTypeOpt = lens (nameof this.AreaTypeOpt) this this.GetAreaTypeOpt this.SetAreaTypeOpt

/// An area's static parts, created once as its own children when told whose scenery it is, and frozen into batches. The
/// area's dynamic parts, declared by the area each frame, aren't static, so the freezing passes them by.
type SceneryDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    /// Create a static part as a child of the scenery, its look set once.
    static let createPart index (part : Part) (scenery : Entity) world =
        let surnames = Array.add ("Part+" + string index) scenery.Surnames
        let entity = World.createEntity<StaticModelDispatcher> (Some Address.parent) NoOverlay (Some surnames) scenery.Group world
        entity.SetPositionLocal part.PartPosition world // local to the scenery, which sits at the origin
        entity.SetRotationLocal part.PartRotation world
        entity.SetScaleLocal part.PartScale world
        entity.SetStaticModel (PartShapeType.getModel part.PartShape) world
        entity.SetMaterial (SurfaceType.getMaterial part.PartSurface part.PartColor) world
        entity.SetRenderStyle (SurfaceType.getRenderStyle part.PartSurface part.PartColor part.PartUnreflected) world
        entity.SetMaterialProperties (SurfaceData.makeProperties Data.Surfaces[part.PartSurface] part.PartColor part.PartGlow) world
        entity.SetCastShadow part.CastsShadow world
        entity.SetStatic true world

    /// Create the scenery's static parts afresh from its area, and freeze them.
    static let buildScenery (scenery : Entity) world =
        if scenery.GetExists world then
            for child in scenery.GetChildren world |> Array.ofSeq do
                if child.Name.StartsWith "Part+" then World.destroyEntityImmediate child world
            match scenery.GetAreaTypeOpt world with
            | Some areaType ->
                for (index, part) in Data.Areas[areaType].StaticParts do
                    createPart index part scenery world
            | None -> ()
            // freeze afresh now its children are here
            scenery.SetFrozen false world
            scenery.SetFrozen true world

    /// Build afresh when told whose scenery it is.
    static let handleSceneryOfChange (evt : Event<ChangeData, Entity>) world =
        buildScenery evt.Subscriber world
        Cascade

    /// Build afresh when the assets or the code reload.
    static let handleReload (evt : Event<unit, Entity>) world =
        buildScenery evt.Subscriber world
        Cascade

    static member Facets =
        [typeof<Freezer3dFacet>]

    static member Properties =
        [define Entity.Pickable false
         define Entity.AreaTypeOpt None
         define Entity.Frozen true]

    override this.Register (entity, world) =
        World.sense (nameof handleSceneryOfChange) entity.AreaTypeOpt.ChangeEvent entity (nameof SceneryDispatcher) world
        World.sense (nameof handleReload) Game.AssetsReloadEvent entity (nameof SceneryDispatcher) world
        World.sense (nameof handleReload) Game.CodeReloadEvent entity (nameof SceneryDispatcher) world