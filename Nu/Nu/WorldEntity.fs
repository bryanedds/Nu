// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.IO
open System.Numerics
open FSharpx.Collections
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldEntityModule =

    type Entity with

        member this.GetDispatcher world = World.getEntityDispatcher this world
        member this.Dispatcher = lensReadOnly Property? Dispatcher this.GetDispatcher this
        member this.GetModelGeneric<'a> world = World.getEntityModel<'a> this world
        member this.SetModelGeneric<'a> value world = World.setEntityModel<'a> value this world |> snd'
        member this.ModelGeneric<'a> () = lens Property? Model this.GetModelGeneric<'a> this.SetModelGeneric<'a> this
        member this.GetEcs world = World.getScreenEcs this.Screen world
        member this.Ecs = lensReadOnly Property? Ecs this.GetEcs this
        member this.GetFacets world = World.getEntityFacets this world
        member this.Facets = lensReadOnly Property? Facets this.GetFacets this
        member this.GetTransform world = World.getEntityTransform this world
        member this.SetTransform value world = let mutable value = value in World.setEntityTransformByRef (&value, World.getEntityState this world, this, world) |> snd'
        member this.Transform = lens Property? Transform this.GetTransform this.SetTransform this
        member this.SetPerimeterUnscaled value world = World.setEntityPerimeterUnscaled value this world |> snd'
        member this.GetPerimeterUnscaled world = World.getEntityPerimeterUnscaled this world
        member this.PerimeterUnscaled = lens Property? PerimeterUnscaled this.GetPerimeterUnscaled this.SetPerimeterUnscaled this
        member this.SetPerimeter value world = World.setEntityPerimeter value this world |> snd'
        member this.GetPerimeter world = World.getEntityPerimeter this world
        member this.Perimeter = lens Property? Perimeter this.GetPerimeter this.SetPerimeter this
        member this.SetCenter value world = World.setEntityCenter value this world |> snd'
        member this.GetCenter world = World.getEntityCenter this world
        member this.Center = lens Property? Center this.GetCenter this.SetCenter this
        member this.SetBottom value world = World.setEntityBottom value this world |> snd'
        member this.GetBottom world = World.getEntityBottom this world
        member this.Bottom = lens Property? Bottom this.GetBottom this.SetBottom this
        member this.GetPerimeterOriented world = World.getEntityPerimeterOriented this world
        member this.PerimeterOriented = lensReadOnly Property? PerimeterOriented this.GetPerimeterOriented this
        member this.GetBounds world = World.getEntityBounds this world
        member this.Bounds = lensReadOnly Property? Bounds this.GetBounds this
        member this.GetPosition world = World.getEntityPosition this world
        member this.SetPosition value world = World.setEntityPosition value this world |> snd'
        member this.Position = lens Property? Position this.GetPosition this.SetPosition this
        member this.GetPositionLocal world = World.getEntityPositionLocal this world
        member this.SetPositionLocal value world = World.setEntityPositionLocal value this world |> snd'
        member this.PositionLocal = lens Property? PositionLocal this.GetPositionLocal this.SetPositionLocal this
        member this.GetRotation world = World.getEntityRotation this world
        member this.SetRotation value world = World.setEntityRotation value this world |> snd'
        member this.Rotation = lens Property? Rotation this.GetRotation this.SetRotation this
        member this.GetRotationLocal world = World.getEntityRotationLocal this world
        member this.SetRotationLocal value world = World.setEntityRotationLocal value this world |> snd'
        member this.RotationLocal = lens Property? RotationLocal this.GetRotationLocal this.SetRotationLocal this
        member this.GetScale world = World.getEntityScale this world
        member this.SetScale value world = World.setEntityScale value this world |> snd'
        member this.Scale = lens Property? Scale this.GetScale this.SetScale this
        member this.GetScaleLocal world = World.getEntityScaleLocal this world
        member this.SetScaleLocal value world = World.setEntityScaleLocal value this world |> snd'
        member this.ScaleLocal = lens Property? ScaleLocal this.GetScaleLocal this.SetScaleLocal this
        member this.GetOffset world = World.getEntityOffset this world
        member this.SetOffset value world = World.setEntityOffset value this world |> snd'
        member this.Offset = lens Property? Offset this.GetOffset this.SetOffset this
        member this.GetAngles world = World.getEntityAngles this world
        member this.SetAngles value world = World.setEntityAngles value this world |> snd'
        member this.Angles = lens Property? Angles this.GetAngles this.SetAngles this
        member this.GetAnglesLocal world = World.getEntityAnglesLocal this world
        member this.SetAnglesLocal value world = World.setEntityAnglesLocal value this world |> snd'
        member this.AnglesLocal = lens Property? AnglesLocal this.GetAnglesLocal this.SetAnglesLocal this
        member this.GetDegrees world = World.getEntityDegrees this world
        member this.SetDegrees value world = World.setEntityDegrees value this world |> snd'
        member this.Degrees = lens Property? Degrees this.GetDegrees this.SetDegrees this
        member this.GetDegreesLocal world = World.getEntityDegreesLocal this world
        member this.SetDegreesLocal value world = World.setEntityDegreesLocal value this world |> snd'
        member this.DegreesLocal = lens Property? DegreesLocal this.GetDegreesLocal this.SetDegreesLocal this
        member this.GetSize world = World.getEntitySize this world
        member this.SetSize value world = World.setEntitySize value this world |> snd'
        member this.Size = lens Property? Size this.GetSize this.SetSize this
        member this.GetElevation world = World.getEntityElevation this world
        member this.SetElevation value world = World.setEntityElevation value this world |> snd'
        member this.Elevation = lens Property? Elevation this.GetElevation this.SetElevation this
        member this.GetOverflow world = World.getEntityOverflow this world
        member this.SetOverflow value world = World.setEntityOverflow value this world |> snd'
        member this.Overflow = lens Property? Overflow this.GetOverflow this.SetOverflow this
        member this.GetElevationLocal world = World.getEntityElevationLocal this world
        member this.SetElevationLocal value world = World.setEntityElevationLocal value this world |> snd'
        member this.ElevationLocal = lens Property? ElevationLocal this.GetElevationLocal this.SetElevationLocal this
        member this.GetOmnipresent world = World.getEntityOmnipresent this world
        member this.SetOmnipresent value world = World.setEntityOmnipresent value this world |> snd'
        member this.Omnipresent = lens Property? Omnipresent this.GetOmnipresent this.SetOmnipresent this
        member this.GetAbsolute world = World.getEntityAbsolute this world
        member this.SetAbsolute value world = World.setEntityAbsolute value this world |> snd'
        member this.Absolute = lens Property? Absolute this.GetAbsolute this.SetAbsolute this
        member this.GetMountOpt world = World.getEntityMountOpt this world
        member this.SetMountOpt value world = World.setEntityMountOpt value this world |> snd'
        member this.MountOpt = lens Property? MountOpt this.GetMountOpt this.SetMountOpt this
        member this.GetImperative world = World.getEntityImperative this world
        member this.SetImperative value world = World.setEntityImperative value this world |> snd'
        member this.Imperative = lens Property? Imperative this.GetImperative this.SetImperative this
        member this.GetEnabled world = World.getEntityEnabled this world
        member this.SetEnabled value world = World.setEntityEnabled value this world |> snd'
        member this.Enabled = lens Property? Enabled this.GetEnabled this.SetEnabled this
        member this.GetEnabledLocal world = World.getEntityEnabledLocal this world
        member this.SetEnabledLocal value world = World.setEntityEnabledLocal value this world |> snd'
        member this.EnabledLocal = lens Property? EnabledLocal this.GetEnabledLocal this.SetEnabledLocal this
        member this.GetVisible world = World.getEntityVisible this world
        member this.SetVisible value world = World.setEntityVisible value this world |> snd'
        member this.Visible = lens Property? Visible this.GetVisible this.SetVisible this
        member this.GetVisibleLocal world = World.getEntityVisibleLocal this world
        member this.SetVisibleLocal value world = World.setEntityVisibleLocal value this world |> snd'
        member this.VisibleLocal = lens Property? VisibleLocal this.GetVisibleLocal this.SetVisibleLocal this
        member this.GetAlwaysUpdate world = World.getEntityAlwaysUpdate this world
        member this.SetAlwaysUpdate value world = World.setEntityAlwaysUpdate value this world |> snd'
        member this.AlwaysUpdate = lens Property? AlwaysUpdate this.GetAlwaysUpdate this.SetAlwaysUpdate this
        member this.GetPersistent world = World.getEntityPersistent this world
        member this.SetPersistent value world = World.setEntityPersistent value this world |> snd'
        member this.Persistent = lens Property? Persistent this.GetPersistent this.SetPersistent this
        member this.GetIgnorePropertyBindings world = World.getEntityIgnorePropertyBindings this world
        member this.SetIgnorePropertyBindings value world = World.setEntityIgnorePropertyBindings value this world |> snd'
        member this.IgnorePropertyBindings = lens Property? IgnorePropertyBindings this.GetIgnorePropertyBindings this.SetIgnorePropertyBindings this
        member this.GetIs2d world = World.getEntityIs2d this world
        member this.Is2d = lensReadOnly Property? Is2d this.GetIs2d this
        member this.GetCentered world = World.getEntityCentered this world
        member this.SetCentered value world = World.setEntityCentered value this world |> snd'
        member this.Centered = lens Property? Centered this.GetCentered this.SetCentered this
        member this.GetEnclosed world = World.getEntityEnclosed this world
        member this.SetEnclosed value world = World.setEntityEnclosed value this world |> snd'
        member this.Enclosed = lens Property? Enclosed this.GetEnclosed this.SetEnclosed this
        member this.GetPhysical world = World.getEntityPhysical this world
        member this.Physical = lensReadOnly Property? Physical this.GetPhysical this
        member this.GetOptimized world = World.getEntityOptimized this world
        member this.Optimized = lensReadOnly Property? Optimized this.GetOptimized this
        member this.GetDestroying world = World.getEntityDestroying this world
        member this.Destroying = lensReadOnly Property? Destroying this.GetDestroying this
        member this.GetScriptFrame world = World.getEntityScriptFrame this world
        member this.ScriptFrame = lensReadOnly Property? Script this.GetScriptFrame this
        member this.GetOverlayNameOpt world = World.getEntityOverlayNameOpt this world
        member this.OverlayNameOpt = lensReadOnly Property? OverlayNameOpt this.GetOverlayNameOpt this
        member this.GetFacetNames world = World.getEntityFacetNames this world
        member this.FacetNames = lensReadOnly Property? FacetNames this.GetFacetNames this
        member this.GetOrder world = World.getEntityOrder this world
        member this.Order = lensReadOnly Property? Order this.GetOrder this
        member this.GetId world = World.getEntityId this world
        member this.Id = lensReadOnly Property? Id this.GetId this

        member this.RegisterEvent = Events.Register --> this
        member this.UnregisteringEvent = Events.Unregistering --> this
        member this.ChangeEvent propertyName = Events.Change propertyName --> this
        member this.UpdateEvent = Events.Update --> this
#if !DISABLE_ENTITY_POST_UPDATE
        member this.PostUpdateEvent = Events.PostUpdate --> this
#endif

        /// The state of an entity.
        /// The only place this accessor should be used is in performance-sensitive code.
        /// Otherwise, you should get and set the required entity properties via the Entity interface.
        member this.State world =
            let entityState = World.getEntityState this world
#if DEBUG
            if World.getImperative world && not entityState.Optimized then
                failwith "Can get the entity state of an entity only if it is Optimized (Imperative, Omnipresent, and not PublishChangeEvents)."
#endif
            entityState

        /// The copied state of an entity.
        /// The only place this accessor should be used is in performance-sensitive code.
        /// Otherwise, you should get and set the required entity properties via the Entity interface.
        member this.StateReadOnly world =
            world |> World.getEntityState this |> EntityState.copy

        /// Optimize an entity by setting { Imperative = true; Omnipresent = true }.
        member this.Optimize world =
            let world = this.SetImperative true world
            let world = this.SetOmnipresent true world
            world

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
        member this.SetTransformSnapped positionSnap rotationSnap (value : Transform) world =
            let mutable transform = value
            transform.Snap (positionSnap, rotationSnap)
            this.SetTransform transform world

        /// Try to get a property value and type.
        member this.TryGetProperty propertyName world =
            let mutable property = Unchecked.defaultof<_>
            let found = World.tryGetEntityProperty (propertyName, this, world, &property)
            if found then Some property else None

        /// Get a property value and type.
        member this.GetProperty propertyName world =
            World.getEntityProperty propertyName this world

        /// Get an xtension property value.
        member this.TryGet<'a> propertyName world : 'a =
            let mutable property = Unchecked.defaultof<Property>
            if World.tryGetEntityXtensionProperty (propertyName, this, world, &property)
            then property.PropertyValue :?> 'a
            else Unchecked.defaultof<'a>

        /// Get an xtension property value.
        member this.Get<'a> propertyName world : 'a =
            World.getEntityXtensionValue<'a> propertyName this world

        /// Try to set a property value with explicit type.
        member this.TrySetProperty propertyName property world =
            World.trySetEntityProperty propertyName property this world

        /// Set a property value with explicit type.
        member this.SetProperty propertyName property world =
            World.setEntityProperty propertyName property this world |> snd'

        /// To try set an xtension property value.
        member this.TrySet<'a> propertyName (value : 'a) world =
            let property = { PropertyType = typeof<'a>; PropertyValue = value }
            World.trySetEntityXtensionProperty propertyName property this world

        /// Set an xtension property value.
        member this.Set<'a> propertyName (value : 'a) world =
            World.setEntityXtensionValue<'a> propertyName value this world

        /// Set an xtension property value without publishing an event.
        member internal this.SetXtensionPropertyWithoutEvent<'a> propertyName (value : 'a) world =
            let property = { PropertyType = typeof<'a>; PropertyValue = value }
            let struct (_, _, world) = World.setEntityXtensionPropertyWithoutEvent propertyName property this world
            world

        /// Attach a property.
        member this.AttachProperty propertyName property world =
            World.attachEntityProperty propertyName property this world

        /// Detach a property.
        member this.DetachProperty propertyName world =
            World.detachEntityProperty propertyName this world

        /// Get an entity's sorting priority in 2d.
        member this.GetSortingPriority2d world = World.getEntitySortingPriority2d this world

        /// Get an entity's quick size.
        member this.GetQuickSize world = World.getEntityQuickSize this world

        /// Check that an entity is in the camera's view.
        member this.GetInView2d world = World.getEntityInView2d this world

        /// Check that an entity is in the camera's view.
        member this.GetInView3d world = World.getEntityInView3d this world

        /// Check that an entity is selected.
        member this.IsSelected world =
            let gameState = World.getGameState world
            match gameState.OmniScreenOpt with
            | Some omniScreen when Address.head this.EntityAddress = Address.head omniScreen.ScreenAddress -> true
            | _ ->
                match gameState.SelectedScreenOpt with
                | Some screen when Address.head this.EntityAddress = Address.head screen.ScreenAddress -> true
                | _ -> false

        /// Check that an entity exists in the world.
        member this.Exists world = World.getEntityExists this world

        /// Set an entity's size by its quick size.
        member this.QuickSize world = World.setEntitySize (this.GetQuickSize world) this world

        /// Set an entity's mount while adjusting its mount properties such that they do not change.
        /// TODO: 3D: make this work 3-dimensionally!
        member this.SetMountOptWithAdjustment (value : Entity Relation option) world =
            let world =
                match
                    (Option.bind (tryResolve this) (this.GetMountOpt world),
                     Option.bind (tryResolve this) value) with
                | (Some mountOld, Some mountNew) ->
                    if mountOld.Exists world && mountNew.Exists world then
                        let positionLocal = this.GetPosition world - mountNew.GetPosition world
                        let elevationLocal = this.GetElevation world - mountNew.GetElevation world
                        let world = this.SetPositionLocal positionLocal world
                        let world = this.SetElevationLocal elevationLocal world
                        let world = this.SetVisible (this.GetVisibleLocal world && mountNew.GetVisible world) world
                        let world = this.SetEnabled (this.GetEnabledLocal world && mountNew.GetEnabled world) world
                        world
                    else world
                | (Some mountOld, None) ->
                    if mountOld.Exists world then
                        let world = this.SetPositionLocal v3Zero world
                        let world = this.SetElevationLocal 0.0f world
                        let world = this.SetVisible (this.GetVisibleLocal world) world
                        let world = this.SetEnabled (this.GetEnabledLocal world) world
                        world
                    else world
                | (None, Some mountNew) ->
                    if mountNew.Exists world then
                        let positionLocal = this.GetPosition world - mountNew.GetPosition world
                        let elevationLocal = this.GetElevation world - mountNew.GetElevation world
                        let world = this.SetPositionLocal positionLocal world
                        let world = this.SetElevationLocal elevationLocal world
                        let world = this.SetVisible (this.GetVisibleLocal world && mountNew.GetVisible world) world
                        let world = this.SetEnabled (this.GetEnabledLocal world && mountNew.GetEnabled world) world
                        world
                    else world
                | (None, None) -> world
            this.SetMountOpt value world

        /// Check whether the entity's mount exists.
        member this.MountExists world =
            match Option.bind (tryResolve this) (this.GetMountOpt world) with
            | Some mount -> mount.Exists world
            | None -> false

        /// Get an entity's mounters.
        member this.GetMounters world = World.getEntityMounters this world

        /// Traverse an entity's mounters.
        member this.TraverseMounters effect world = World.traverseEntityMounters effect this world

        /// Get an entity's children.
        member this.GetChildren world = World.getEntityEntities this world

        /// Traverse an entity's children.
        member this.TraverseChildren effect world = World.traverseEntityEntities effect this world

        /// Apply physics changes to an entity.
        member this.ApplyPhysics (position : Vector3) rotation linearVelocity angularVelocity world =
            let mutable oldTransform = this.GetTransform world
            let mutable newTransform = oldTransform
            let world =
                if  v3Neq oldTransform.Position position ||
                    quatNeq oldTransform.Rotation rotation then
                    newTransform.Position <- position
                    newTransform.Rotation <- rotation
                    this.SetTransformByRefWithoutEvent (&newTransform, world)
                else world
            let world = this.SetXtensionPropertyWithoutEvent Property? LinearVelocity linearVelocity world
            let world = this.SetXtensionPropertyWithoutEvent Property? AngularVelocity angularVelocity world
            let dispatcher = this.GetDispatcher world
            dispatcher.ApplyPhysics (position, rotation, linearVelocity, angularVelocity, this, world)

        /// Propagate entity physics properties into the physics system.
        member this.PropagatePhysics world =
            if WorldModule.isSelected this world
            then World.propagateEntityPhysics this world
            else world

        /// Check that an entity uses a facet of the given type.
        member this.Has (facetType, world) = Array.exists (fun facet -> getType facet = facetType) (this.GetFacets world)

        /// Check that an entity uses a facet of the given type.
        member this.Has<'a> world = this.Has (typeof<'a>, world)

        /// Check that an entity dispatches in the same manner as the dispatcher with the given type.
        member this.Is (dispatcherType, world) = Reflection.dispatchesAs dispatcherType (this.GetDispatcher world)

        /// Check that an entity dispatches in the same manner as the dispatcher with the given type.
        member this.Is<'a> world = this.Is (typeof<'a>, world)

        /// Get an entity's change event address.
        member this.GetChangeEvent propertyName = Events.Change propertyName --> this.EntityAddress

        /// Try to signal an entity's facet.
        member this.TrySignalFacet (signalObj : obj) facetName world = (this.GetDispatcher world).TrySignalFacet (signalObj, facetName, this, world)

        /// Try to signal an entity.
        member this.TrySignal signal world = (this.GetDispatcher world).TrySignal (signal, this, world)

    type World with

        static member internal updateEntity (entity : Entity) world =
            let dispatcher = entity.GetDispatcher world
            let world = dispatcher.Update (entity, world)
            let facets = entity.GetFacets world
            let world =
                if Array.notEmpty facets // OPTIMIZATION: avoid lambda allocation.
                then Array.fold (fun world (facet : Facet) -> facet.Update (entity, world)) world facets
                else world
            if World.getEntityPublishUpdates entity world then
                let eventTrace = EventTrace.debug "World" "updateEntity" "" EventTrace.empty
                World.publishPlus () entity.UpdateEvent eventTrace Simulants.Game false false world
            else world

#if !DISABLE_ENTITY_POST_UPDATE
        static member internal postUpdateEntity (entity : Entity) world =
            let dispatcher = entity.GetDispatcher world
            let world = dispatcher.PostUpdate (entity, world)
            let facets = entity.GetFacets world
            let world =
                if Array.notEmpty facets // OPTIMIZATION: avoid lambda allocation.
                then Array.fold (fun world (facet : Facet) -> facet.PostUpdate (entity, world)) world facets
                else world
            if World.getEntityPublishPostUpdates entity world then
                let eventTrace = EventTrace.debug "World" "postUpdateEntity" "" EventTrace.empty
                World.publishPlus () entity.PostUpdateEvent eventTrace Simulants.Game false false world
            else world
#endif

        static member internal actualizeEntity (entity : Entity) world =
            let dispatcher = entity.GetDispatcher world
            let world = dispatcher.Actualize (entity, world)
            let facets = entity.GetFacets world
            if Array.notEmpty facets // OPTIMIZATION: avoid lambda allocation.
            then Array.fold (fun world (facet : Facet) -> facet.Actualize (entity, world)) world facets
            else world

        /// Get all the entities in a group.
        [<FunctionBinding>]
        static member getEntitiesFlattened (group : Group) world =
            let rec getEntitiesRec parent world =
                let simulants = World.getSimulants world
                match simulants.TryGetValue parent with
                | (true, entitiesOpt) ->
                    match entitiesOpt with
                    | Some entities ->
                        seq {
                            yield! Seq.map cast<Entity> entities
                            for entity in entities do
                                yield! getEntitiesRec entity world }
                    | None -> Seq.empty
                | (false, _) -> Seq.empty
            getEntitiesRec (group :> Simulant) world |> SegmentedArray.ofSeq |> seq

        /// Get all the entities directly parented by the group.
        [<FunctionBinding>]
        static member getEntities (group : Group) world =
            let simulants = World.getSimulants world
            match simulants.TryGetValue (group :> Simulant) with
            | (true, entitiesOpt) ->
                match entitiesOpt with
                | Some entities -> entities |> Seq.map cast<Entity> |> seq
                | None -> Seq.empty
            | (false, _) -> Seq.empty

        /// Get all the entities not mounting another entity in a group.
        [<FunctionBinding>]
        static member getEntitiesSovereign group world =
            World.getEntitiesFlattened group world |>
            Seq.filter (fun entity -> Option.isNone (entity.GetMountOpt world))

        /// Destroy an entity in the world at the end of the current update.
        [<FunctionBinding>]
        static member destroyEntity (entity : Entity) world =
            World.addSimulantToDestruction entity world

        /// Destroy multiple entities in the world immediately. Can be dangerous if existing in-flight publishing
        /// depends on any of the entities' existences. Consider using World.destroyEntities instead.
        static member destroyEntitiesImmediate (entities : Entity seq) world =
            List.foldBack
                (fun entity world -> World.destroyEntityImmediate entity world)
                (List.ofSeq entities)
                world

        /// Destroy multiple entities in the world at the end of the current update.
        [<FunctionBinding>]
        static member destroyEntities entities world =
            World.frame (World.destroyEntitiesImmediate entities) Simulants.Game world

        /// Sort the given entities.
        /// If there are a lot of entities, this may allocate in the LOH.
        static member sortEntities entities world =
            entities |>
            Array.ofSeq |>
            Array.rev |>
            Array.map (fun (entity : Entity) -> entity.GetSortingPriority2d world) |>
            Array.sortStableWith SortPriority.compare |>
            Array.map (fun p -> p.SortTarget :?> Entity)

        /// Try to pick an entity at the given position.
        [<FunctionBinding>]
        static member tryPickEntity position entities world =
            // OPTIMIZATION: using arrays for speed
            let entitiesSorted = World.sortEntities entities world
            Array.tryFind
                (fun (entity : Entity) ->
                    let positionWorld = World.mouseToWorld2d (entity.GetAbsolute world) position world
                    let picked = Math.isPointInBounds2d positionWorld (entity.GetPerimeterOriented world).Box2
                    picked)
                entitiesSorted

        /// Try to find the entity in the given entity's group with the closest previous order.
        static member tryFindPreviousEntity (entity : Entity) world =
            let order = World.getEntityOrder entity world
            let mutable previousOrderDeltaOpt = ValueNone
            let mutable previousOpt = ValueNone
            let entities = World.getEntitiesFlattened entity.Group world
            for entity2 in entities do
                let order2 = World.getEntityOrder entity2 world
                let orderDelta = order - order2
                if orderDelta > 0L then
                    match previousOrderDeltaOpt with
                    | ValueSome orderDelta2 ->
                        if orderDelta < orderDelta2 then
                            previousOrderDeltaOpt <- ValueSome orderDelta
                            previousOpt <- ValueSome entity2
                    | ValueNone ->
                        previousOrderDeltaOpt <- ValueSome orderDelta
                        previousOpt <- ValueSome entity2
            match previousOpt with
            | ValueSome previous -> Some previous
            | ValueNone -> None

        /// Change an entity's order between that of before and after's.
        static member reorderEntity entityBefore entityAfter entity world =
            let orderBefore = World.getEntityOrder entityBefore world;
            let orderAfter = World.getEntityOrder entityAfter world;
            let order = (orderBefore + orderAfter) / 2L;
            World.setEntityOrder order entity world |> snd'

        /// Change an entity's order between that of target and its previous sibling's.
        static member insertEntity target entity world =
            match World.tryFindPreviousEntity target world with
            | Some previous -> World.reorderEntity previous target entity world
            | None -> world

        /// Write an entity to an entity descriptor.
        static member writeEntity (entity : Entity) entityDescriptor world =
            let overlayer = World.getOverlayer world
            let entityState = World.getEntityState entity world
            let entityDispatcherName = getTypeName entityState.Dispatcher
            let entityDescriptor = { entityDescriptor with EntityDispatcherName = entityDispatcherName }
            let entityFacetNames = World.getEntityFacetNamesReflectively entityState
            let overlaySymbolsOpt =
                match entityState.OverlayNameOpt with
                | Some overlayName -> Some (Overlayer.getOverlaySymbols overlayName entityFacetNames overlayer)
                | None -> None
            let shouldWriteProperty = fun propertyName propertyType (propertyValue : obj) ->
                if propertyName = "OverlayNameOpt" && propertyType = typeof<string option> then
                    let defaultOverlayNameOpt = World.getEntityDefaultOverlayName entityDispatcherName world
                    defaultOverlayNameOpt <> (propertyValue :?> string option)
                else
                    match overlaySymbolsOpt with
                    | Some overlaySymbols -> Overlayer.shouldPropertySerialize propertyName propertyType entityState overlaySymbols
                    | None -> true
            let entityProperties = Reflection.writePropertiesFromTarget shouldWriteProperty entityDescriptor.EntityProperties entityState
            let entityDescriptor = { entityDescriptor with EntityProperties = entityProperties }
            let entityDescriptor = EntityDescriptor.setNameOpt (Some entity.Name) entityDescriptor
            let entities = World.getEntityEntities entity world
            { entityDescriptor with EntityDescriptors = World.writeEntities entities world }

        /// Write multiple entities to a group descriptor.
        static member writeEntities entities world =
            entities |>
            Seq.sortBy (fun (entity : Entity) -> entity.GetOrder world) |>
            Seq.filter (fun (entity : Entity) -> entity.GetPersistent world) |>
            Seq.fold (fun entityDescriptors entity -> World.writeEntity entity EntityDescriptor.empty world :: entityDescriptors) [] |>
            Seq.rev |>
            Seq.toList

        /// Write an entity to a file.
        [<FunctionBinding>]
        static member writeEntityToFile (filePath : string) enity world =
            let filePathTmp = filePath + ".tmp"
            let prettyPrinter = (SyntaxAttribute.getOrDefault typeof<GameDescriptor>).PrettyPrinter
            let enityDescriptor = World.writeEntity enity EntityDescriptor.empty world
            let enityDescriptorStr = scstring enityDescriptor
            let enityDescriptorPretty = PrettyPrinter.prettyPrint enityDescriptorStr prettyPrinter
            File.WriteAllText (filePathTmp, enityDescriptorPretty)
            File.Delete filePath
            File.Move (filePathTmp, filePath)

        /// Read an entity from an entity descriptor.
        static member readEntity entityDescriptor (nameOpt : string option) (parent : Simulant) world =

            // make the dispatcher
            let dispatcherName = entityDescriptor.EntityDispatcherName
            let dispatchers = World.getEntityDispatchers world
            let (dispatcherName, dispatcher) =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> (dispatcherName, dispatcher)
                | None ->
                    Log.info ("Could not locate dispatcher '" + dispatcherName + "'.")
                    let dispatcherName = typeof<EntityDispatcher>.Name
                    let dispatcher =
                        match Map.tryFind dispatcherName dispatchers with
                        | Some dispatcher -> dispatcher
                        | None -> failwith ("Could not find an EntityDispatcher named '" + dispatcherName + "'.")
                    (dispatcherName, dispatcher)

            // get the default overlay name option
            let defaultOverlayNameOpt = World.getEntityDefaultOverlayName dispatcherName world

            // make the bare entity state with name as id
            let entityState = EntityState.make (World.getImperative world) None defaultOverlayNameOpt dispatcher

            // attach the entity state's intrinsic facets and their properties
            let entityState = World.attachIntrinsicFacetsViaNames entityState world

            // read the entity state's overlay and apply it to its facet names if applicable
            let overlayer = World.getOverlayer world
            let entityState = Reflection.tryReadOverlayNameOptToTarget id entityDescriptor.EntityProperties entityState
            let entityState = if Option.isNone entityState.OverlayNameOpt then { entityState with OverlayNameOpt = defaultOverlayNameOpt } else entityState
            let entityState =
                match (defaultOverlayNameOpt, entityState.OverlayNameOpt) with
                | (Some defaultOverlayName, Some overlayName) -> Overlayer.applyOverlayToFacetNames id defaultOverlayName overlayName entityState overlayer overlayer
                | (_, _) -> entityState

            // read the entity state's facet names
            let entityState = Reflection.readFacetNamesToTarget id entityDescriptor.EntityProperties entityState

            // attach the entity state's dispatcher properties
            let entityState = Reflection.attachProperties id entityState.Dispatcher entityState world
            
            // synchronize the entity state's facets (and attach their properties)
            let entityState =
                match World.trySynchronizeFacetsToNames Set.empty entityState None world with
                | Right (entityState, _) -> entityState
                | Left error -> Log.debug error; entityState

            // attempt to apply the entity state's overlay
            let entityState =
                match entityState.OverlayNameOpt with
                | Some overlayName ->
                    // OPTIMIZATION: applying overlay only when it will change something
                    if Overlay.dispatcherNameToOverlayName dispatcherName <> overlayName then
                        let facetNames = World.getEntityFacetNamesReflectively entityState
                        Overlayer.applyOverlay id dispatcherName overlayName facetNames entityState overlayer
                    else entityState
                | None -> entityState

            // try to read entity name
            let entityNameOpt = EntityDescriptor.getNameOpt entityDescriptor

            // read the entity state's values
            let entityState = Reflection.readPropertiesToTarget id entityDescriptor.EntityProperties entityState

            // configure the name and surnames
            let (name, surnames) =
                match nameOpt with
                | Some name -> (name, Array.add name parent.SimulantAddress.Names)
                | None ->
                    match entityNameOpt with
                    | Some entityName -> (entityName, Array.add entityName parent.SimulantAddress.Names)
                    | None ->
                        let name = Gen.name
                        let surnames = Array.add name parent.SimulantAddress.Names
                        (name, surnames)
            let entityState = { entityState with Surnames = surnames }

            // make entity address
            let entityAddress = parent.SimulantAddress.Names |> Array.add name |> rtoa

            // make entity reference
            let entity = Entity entityAddress

            // add entity's state to world
            let world =
                if World.getEntityExists entity world then
                    if World.getEntityDestroying entity world
                    then World.destroyEntityImmediate entity world
                    else failwith ("Entity '" + scstring entity + " already exists and cannot be created."); world
                else world
            let world = World.addEntity true entityState entity world

            // update mount hierarchy
            let mountOpt = World.getEntityMountOpt entity world
            let world = World.addEntityToMounts mountOpt entity world
            
            // read the entity's children
            let world = World.readEntities entityDescriptor.EntityDescriptors entity world |> snd
            (entity, world)

        /// Read an entity from a file.
        [<FunctionBinding>]
        static member readEntityFromFile (filePath : string) nameOpt group world =
            let entityDescriptorStr = File.ReadAllText filePath
            let entityDescriptor = scvalue<EntityDescriptor> entityDescriptorStr
            World.readEntity entityDescriptor nameOpt group world

        /// Read multiple entities.
        [<FunctionBinding>]
        static member internal readEntities (entityDescriptors : EntityDescriptor list) (parent : Simulant) world =
            List.foldBack
                (fun entityDescriptor (entities, world) ->
                    let nameOpt = EntityDescriptor.getNameOpt entityDescriptor
                    let (entity, world) = World.readEntity entityDescriptor nameOpt parent world
                    (entity :: entities, world))
                    entityDescriptors
                    ([], world)

        /// Turn an entity lens into a series of live entities.
        static member expandEntities (lens : Lens<obj, World>) sieve unfold mapper origin owner group world =
            let mapperGeneralized = fun i a w -> mapper i a w :> SimulantContent
            World.expandSimulants lens sieve unfold mapperGeneralized origin owner group world

        /// Turn entity content into a live entity.
        static member expandEntityContent content origin (owner : Simulant) group world =
            if World.getGroupExists group world then
                match EntityContent.expand content group world with
                | Choice1Of3 (lens, sieve, unfold, mapper) ->
                    let world = World.expandEntities lens sieve unfold mapper origin owner group world
                    (None, world)
                | Choice2Of3 (entityName, descriptor, handlers, binds, content) ->
                    let surnames =
                        match owner with
                        | :? Entity as ownerEntity -> Array.add entityName ownerEntity.Surnames
                        | _ -> [|entityName|]
                    let descriptor = { descriptor with SimulantSurnamesOpt = Some surnames }
                    let (entity, world) = World.createEntity4 DefaultOverlay descriptor group world
                    let handlers =
                        List.map (fun (handler, eventAddress, _) ->
                            let eventNameIndex = Address.findIndex (fun name -> name = "Event") eventAddress
                            let partialAddress = Address.take (inc eventNameIndex) eventAddress
                            (handler, partialAddress --> entity, entity :> Simulant)) handlers
                    let binds = List.map (fun (_, left, right, twoWay) -> (entity :> Simulant, left, right, twoWay)) binds
                    let world =
                        // only set mount if one was not specified by the descriptor properties
                        if not (List.exists (fun (name, _) -> name = Property? MountOpt) descriptor.SimulantProperties) then
                            let mountOpt = if owner :? Entity then Some (Relation.makeParent ()) else None
                            World.setEntityMountOpt mountOpt entity world |> snd'
                        else world
                    let world =
                        List.fold (fun world (simulant, left : World Lens, right, twoWay) ->
                            if twoWay then
                                let world = WorldModule.bind5 simulant left right world
                                WorldModule.bind5 simulant right left world
                            else WorldModule.bind5 simulant left right world)
                            world binds
                    let world =
                        List.fold (fun world (handler, address, simulant) ->
                            World.monitor (fun (evt : Event) world ->
                                let signal = handler evt
                                let world =
                                    match origin with
                                    | SimulantOrigin simulant -> WorldModule.trySignal signal simulant world
                                    | FacetOrigin (simulant, facetName) -> WorldModule.trySignalFacet signal facetName simulant world
                                (Cascade, world))
                                address simulant world)
                            world handlers
                    let world =
                        List.fold (fun world content ->
                            World.expandEntityContent content origin entity group world |> snd)
                            world (snd content)
                    (Some entity, world)
                | Choice3Of3 (entityName, filePath) ->
                    let (entity, world) = World.readEntityFromFile filePath (Some entityName) group world
                    let mountOpt = if owner :? Entity then Some (Relation.makeParent ()) else None
                    let world = World.setEntityMountOpt mountOpt entity world |> snd'
                    (Some entity, world)
            else (None, world)

namespace Debug
open Nu
type Entity =

    /// Provides a full view of all the properties of an entity. Useful for debugging such
    /// as with the Watch feature in Visual Studio.
    static member view entity world = World.viewEntityProperties entity world