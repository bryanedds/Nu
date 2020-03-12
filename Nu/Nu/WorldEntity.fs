// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open System.ComponentModel
open System.IO
open System.Reflection
open System.Runtime.CompilerServices
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldEntityModule =

    type Entity with

        member this.GetDispatcher world = World.getEntityDispatcher this world
        member this.Dispatcher = lensReadOnly Property? Dispatcher this.GetDispatcher this
        member this.GetFacets world = World.getEntityFacets this world
        member this.Facets = lensReadOnly Property? Facets this.GetFacets this
        member this.GetPosition world = World.getEntityPosition this world
        member this.SetPosition value world = World.setEntityPosition value this world
        member this.Position = lens Property? Position this.GetPosition this.SetPosition this
        member this.GetSize world = World.getEntitySize this world
        member this.SetSize value world = World.setEntitySize value this world
        member this.Size = lens Property? Size this.GetSize this.SetSize this
        member this.GetRotation world = World.getEntityRotation this world
        member this.SetRotation value world = World.setEntityRotation value this world
        member this.Rotation = lens Property? Rotation this.GetRotation this.SetRotation this
        member this.GetDepth world = World.getEntityDepth this world
        member this.SetDepth value world = World.setEntityDepth value this world
        member this.Depth = lens Property? Depth this.GetDepth this.SetDepth this
        member this.GetDepthLayered world = World.getEntityDepth this world + if not (World.getEntityIgnoreLayer this world) then World.getLayerDepth this.Parent world else 0.0f
        member this.DepthLayered = lensReadOnly Property? DepthLayered this.GetDepthLayered this
        member this.GetViewType world = World.getEntityViewType this world
        member this.SetViewType value world = World.setEntityViewType value this world
        member this.ViewType = lens Property? ViewType this.GetViewType this.SetViewType this
        member this.GetOmnipresent world = World.getEntityOmnipresent this world
        member this.SetOmnipresent value world = World.setEntityOmnipresent value this world
        member this.Omnipresent = lens Property? Omnipresent this.GetOmnipresent this.SetOmnipresent this
        member this.GetOverflow world = World.getEntityOverflow this world
        member this.SetOverflow value world = World.setEntityOverflow value this world
        member this.Overflow = lens Property? Overflow this.GetOverflow this.SetOverflow this
        member this.GetImperative world = World.getEntityImperative this world
        member this.SetImperative value world = World.setEntityImperative value this world
        member this.Imperative = lens Property? Imperative this.GetImperative this.SetImperative this
        member this.GetPublishChanges world = World.getEntityPublishChanges this world
        member this.SetPublishChanges value world = World.setEntityPublishChanges value this world
        member this.PublishChanges = lens Property? PublishChanges this.GetPublishChanges this.SetPublishChanges this
        member this.GetIgnoreLayer world = World.getEntityIgnoreLayer this world
        member this.SetIgnoreLayer value world = World.setEntityIgnoreLayer value this world
        member this.IgnoreLayer = lens Property? IgnoreLayer this.GetIgnoreLayer this.SetIgnoreLayer this
        member this.GetEnabled world = World.getEntityEnabled this world
        member this.SetEnabled value world = World.setEntityEnabled value this world
        member this.Enabled = lens Property? Enabled this.GetEnabled this.SetEnabled this
        member this.GetVisible world = World.getEntityVisible this world
        member this.SetVisible value world = World.setEntityVisible value this world
        member this.Visible = lens Property? Visible this.GetVisible this.SetVisible this
        member this.GetVisibleLayered world = World.getEntityVisible this world && if not (World.getEntityIgnoreLayer this world) then World.getLayerVisible this.Parent world else true
        member this.VisibleLayered = lensReadOnly Property? VisibleLayered this.GetVisibleLayered this
        member this.GetAlwaysUpdate world = World.getEntityAlwaysUpdate this world
        member this.SetAlwaysUpdate value world = World.setEntityAlwaysUpdate value this world
        member this.AlwaysUpdate = lens Property? AlwaysUpdate this.GetAlwaysUpdate this.SetAlwaysUpdate this
        member this.GetPersistent world = World.getEntityPersistent this world
        member this.SetPersistent value world = World.setEntityPersistent value this world
        member this.Persistent = lens Property? Persistent this.GetPersistent this.SetPersistent this
        member this.GetOverlayNameOpt world = World.getEntityOverlayNameOpt this world
        member this.OverlayNameOpt = lensReadOnly Property? OverlayNameOpt this.GetOverlayNameOpt this
        member this.GetFacetNames world = World.getEntityFacetNames this world
        member this.FacetNames = lensReadOnly Property? FacetNames this.GetFacetNames this
        member this.GetScriptFrame world = World.getEntityScriptFrame this world
        member this.ScriptFrame = lensReadOnly Property? Script this.GetScriptFrame this
        member this.GetCreationTimeStamp world = World.getEntityCreationTimeStamp this world
        member this.CreationTimeStamp = lensReadOnly Property? CreationTimeStamp this.GetCreationTimeStamp this
        member this.GetId world = World.getEntityId this world
        member this.Id = lensReadOnly Property? Id this.GetId this

        member this.GetCenter world = World.getEntityCenter this world
        member this.SetCenter value world = World.setEntityCenter value this world
        member this.Center = lens Property? Center this.GetCenter this.SetCenter this
        member this.GetTransform world = World.getEntityTransform this world
        member this.SetTransform value world = World.setEntityTransform value this world
        member this.Transform = lens Property? Transform this.GetTransform this.SetTransform this
        member this.GetStaticData<'a> world = World.getEntityStaticData<'a> this world
        member this.SetStaticData<'a> value world = World.setEntityStaticData<'a> value this world
        member this.UpdateStaticData<'a> updater world = this.SetStaticData<'a> (updater this.GetStaticData<'a> world) world
        member this.StaticData<'a> () = lens Property? StaticData this.GetStaticData<'a> this.SetStaticData<'a> this

        member this.ChangeEvent propertyName = Events.Change propertyName --> this
        member this.RegisterEvent = Events.Register --> this
        member this.UnregisteringEvent = Events.Unregistering --> this
        member this.UpdateEvent = Events.Update --> this
        member this.PostUpdateEvent = Events.PostUpdate --> this

        /// Set the transform of an entity snapped to the give position and rotation snaps.
        member this.SetTransformSnapped positionSnap rotationSnap transform world =
            let transform = Math.snapTransform positionSnap rotationSnap transform
            this.SetTransform transform world

        /// Try to get a property value and type.
        member this.TryGetProperty propertyName world =
            World.tryGetEntityProperty propertyName this world

        /// Get a property value and type.
        member this.GetProperty propertyName world =
            World.getEntityProperty propertyName this world

        /// Get a property value.
        member this.Get<'a> propertyName world : 'a =
            (World.getEntityProperty propertyName this world).PropertyValue :?> 'a

        /// Try to set a property value with explicit type.
        member this.TrySetProperty propertyName alwaysPublish nonPersistent property world =
            World.trySetEntityProperty propertyName alwaysPublish nonPersistent property this world

        /// Set a property value with explicit type.
        member this.SetProperty propertyName alwaysPublish nonPersistent property world =
            World.setEntityProperty propertyName alwaysPublish nonPersistent property this world

        /// Attach a property.
        member this.AttachProperty propertyName alwaysPublish nonPersistent property world =
            World.attachEntityProperty propertyName alwaysPublish nonPersistent property this world

        /// Detach a property.
        member this.DetachProperty propertyName world =
            World.detachEntityProperty propertyName this world

        /// Set a property value.
        member this.Set<'a> propertyName (value : 'a) world =
            let alwaysPublish = Reflection.isPropertyAlwaysPublishByName propertyName
            let nonPersistent = not (Reflection.isPropertyPersistentByName propertyName)
            this.SetFast propertyName alwaysPublish nonPersistent value world

        /// Set a property value.
        member this.SetFast<'a> propertyName alwaysPublish nonPersistent (value : 'a) world =
            World.setEntityProperty propertyName alwaysPublish nonPersistent { PropertyType = typeof<'a>; PropertyValue = value } this world

        /// Get an entity's sorting priority.
        member this.GetSortingPriority world = World.getEntitySortingPriority this world

        /// Get an entity's quick size.
        member this.GetQuickSize world = World.getEntityQuickSize this world

        /// Set an entity's size by its quick size.
        member this.QuickSize world = World.setEntitySize (this.GetQuickSize world) this world

        /// Get an entity's bounds, not taking into account its overflow.
        member this.GetBounds world = Math.makeBounds (this.GetPosition world) (this.GetSize world)

        /// Get an entity's bounds, taking into account its overflow.
        member this.GetBoundsOverflow world = Math.makeBoundsOverflow (this.GetPosition world) (this.GetSize world) (this.GetOverflow world)

        /// Get an entity's bounds maximum.
        member this.GetBoundsMax world = World.getEntityBoundsMax this world

        /// Check that an entity is selected.
        member this.GetSelected world =
            match (World.getGameState world).OmniScreenOpt with
            | Some omniScreen when Address.head this.EntityAddress = Address.head omniScreen.ScreenAddress -> true
            | _ ->
                match (World.getGameState world).SelectedScreenOpt with
                | Some screen when Address.head this.EntityAddress = Address.head screen.ScreenAddress -> true
                | _ -> false

        /// Check that an entity is in the camera's view.
        member this.GetInView world =
            if not (this.GetOmnipresent world) then
                World.isBoundsInView
                    (this.GetViewType world)
                    (this.GetBoundsOverflow world)
                    world
             else true

        /// Check that an entity exists in the world.
        member this.GetExists world = World.getEntityExists this world

        /// Propagate entity physics properties into the physics system.
        member this.PropagatePhysics world =
            World.withEventContext (fun world ->
                if WorldModule.isSimulantSelected this world then
                    let facets = this.GetFacets world
                    Array.fold (fun world (facet : Facet) ->
                        let world = facet.UnregisterPhysics (this, world)
                        facet.RegisterPhysics (this, world))
                        world facets
                else world)
                this world

        /// Check that an entity uses a facet of the given type.
        member this.FacetedAs (facetType, world) = Array.exists (fun facet -> getType facet = facetType) (this.GetFacets world)

        /// Check that an entity uses a facet of the given type.
        member this.FacetedAs<'a> world = this.FacetedAs (typeof<'a>, world)

        /// Check that an entity dispatches in the same manner as the dispatcher with the given type.
        member this.DispatchesAs (dispatcherType, world) = Reflection.dispatchesAs dispatcherType (this.GetDispatcher world)

        /// Check that an entity dispatches in the same manner as the dispatcher with the given type.
        member this.DispatchesAs<'a> world = this.DispatchesAs (typeof<'a>, world)

        /// Resolve a relation in the context of an entity.
        member this.Resolve relation = resolve<Entity> this relation

        /// Relate an entity to a simulant.
        member this.Relate simulant = relate<Entity> this simulant

        /// Get an entity's change event address.
        member this.GetChangeEvent propertyName = Events.Change propertyName --> this.EntityAddress

        /// Try to signal an entity's facet.
        member this.TrySignalFacet (signalObj : obj) facetName world = (this.GetDispatcher world).TrySignalFacet (signalObj, facetName, this, world)

        /// Try to signal an entity.
        member this.TrySignal signal world = (this.GetDispatcher world).TrySignal (signal, this, world)

    type World with

        static member internal updateEntity (entity : Entity) world =
            World.withEventContext (fun world ->
                let dispatcher = entity.GetDispatcher world
                let facets = entity.GetFacets world
                let world = dispatcher.Update (entity, world)
                let world =
                    // OPTIMIZATION: elide Array.fold overhead for empty arrays
                    if Array.isEmpty facets then world
                    else Array.fold (fun world (facet : Facet) -> facet.Update (entity, world)) world facets
                if World.getEntityPublishUpdates entity world then
                    let eventTrace = EventTrace.record "World" "updateEntity" EventTrace.empty
                    World.publishPlus World.sortSubscriptionsByHierarchy () entity.UpdateEventCached eventTrace Default.Game false world
                else world)
                entity
                world

        static member internal postUpdateEntity (entity : Entity) world =
            World.withEventContext (fun world ->
                let dispatcher = entity.GetDispatcher world
                let facets = entity.GetFacets world
                let world = dispatcher.PostUpdate (entity, world)
                let world =
                    // OPTIMIZATION: elide Array.fold overhead for empty arrays
                    if Array.isEmpty facets then world
                    else Array.fold (fun world (facet : Facet) -> facet.PostUpdate (entity, world)) world facets
                if World.getEntityPublishPostUpdates entity world then
                    let eventTrace = EventTrace.record "World" "postUpdateEntity" EventTrace.empty
                    World.publishPlus World.sortSubscriptionsByHierarchy () entity.PostUpdateEventCached eventTrace Default.Game false world
                else world)
                entity
                world

        static member internal actualizeEntity (entity : Entity) world =
            World.withEventContext (fun world ->
                let dispatcher = entity.GetDispatcher world
                let facets = entity.GetFacets world
                let world = dispatcher.Actualize (entity, world)
                let world =
                    // OPTIMIZATION: elide Array.fold overhead for empty arrays
                    if Array.isEmpty facets then world
                    else Array.fold (fun world (facet : Facet) -> facet.Actualize (entity, world)) world facets
                world)
                entity
                world

        /// Get all the entities contained by a layer.
        [<FunctionBinding>]
        static member getEntities (layer : Layer) world =
            match Address.getNames layer.LayerAddress with
            | [|screenName; layerName|] ->
                match UMap.tryFind screenName (World.getScreenDirectory world) with
                | Some layerDirectory ->
                    match UMap.tryFind layerName layerDirectory.Value with
                    | Some entityDirectory ->
                        UMap.fold (fun state _ (entityAddress : _ Address) -> Entity entityAddress :: state) [] entityDirectory.Value :> _ seq
                    | None -> failwith ("Invalid layer address '" + scstring layer.LayerAddress + "'.")
                | None -> failwith ("Invalid layer address '" + scstring layer.LayerAddress + "'.")
            | _ -> failwith ("Invalid layer address '" + scstring layer.LayerAddress + "'.")

        /// Destroy an entity in the world at the end of the current update.
        [<FunctionBinding>]
        static member destroyEntity entity world =
            World.schedule2 (World.destroyEntityImmediate entity) world

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
            World.schedule2 (World.destroyEntitiesImmediate entities) world

        /// Sort the given entities.
        static member sortEntities entities world =
            /// OPTIMIZATION: using arrays for speed
            entities |>
            Array.ofSeq |>
            Array.rev |>
            Array.map (fun (entity : Entity) -> entity.GetSortingPriority world) |>
            Array.sortStableWith SortPriority.compare |>
            Array.map (fun p -> p.SortTarget :?> Entity)

        /// Try to pick an entity at the given position.
        [<FunctionBinding>]
        static member tryPickEntity position entities world =
            /// OPTIMIZATION: using arrays for speed
            let entitiesSorted = World.sortEntities entities world
            Array.tryFind
                (fun (entity : Entity) ->
                    let positionWorld = World.mouseToWorld (entity.GetViewType world) position world
                    let picked = Math.isPointInBounds positionWorld (entity.GetBounds world)
                    picked)
                entitiesSorted

        /// Write multiple entities to a layer descriptor.
        static member writeEntities entities layerDescriptor world =
            entities |>
            Seq.sortBy (fun (entity : Entity) -> entity.GetCreationTimeStamp world) |>
            Seq.filter (fun (entity : Entity) -> entity.GetPersistent world) |>
            Seq.fold (fun entityDescriptors entity -> World.writeEntity entity EntityDescriptor.empty world :: entityDescriptors) layerDescriptor.EntitieDescriptors |>
            fun entityDescriptors -> { layerDescriptor with EntitieDescriptors = entityDescriptors }

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

        /// Read multiple entities from a layer descriptor.
        static member readEntities layerDescriptor layer world =
            List.foldBack
                (fun entityDescriptor (entities, world) ->
                    let entityNameOpt = EntityDescriptor.getNameOpt entityDescriptor
                    let (entity, world) = World.readEntity entityDescriptor entityNameOpt layer world
                    (entity :: entities, world))
                    layerDescriptor.EntitieDescriptors
                    ([], world)

        /// Turn an entity stream into a series of live entities.
        static member expandEntityStream (lens : Lens<obj, World>) indexerOpt mapper origin layer world =
            let mapperGeneralized = fun i lens world -> mapper i lens world :> SimulantContent
            World.expandSimulantStream lens indexerOpt mapperGeneralized origin layer world

        /// Turn entity content into a live entity.
        static member expandEntityContent guidOpt content origin layer world =
            match EntityContent.expand content layer world with
            | Choice1Of3 (lens, indexerOpt, mapper) ->
                World.expandEntityStream lens indexerOpt mapper origin layer world
            | Choice2Of3 (name, descriptor, handlers, fixes, content) ->
                let (entity, world) = World.readEntity descriptor (Some name) layer world
                let world = match guidOpt with Some guid -> World.addKeyedValue (scstring guid) entity world | None -> world
                let world =
                    match origin with
                    | SimulantOrigin simulant
                    | FacetOrigin (simulant, _) ->
                        World.monitor
                            (constant $ World.destroyEntity entity)
                            (Events.Unregistering --> simulant.SimulantAddress)
                            entity
                            world
                let world =
                    List.fold (fun world (simulant, left : World Lens, right, breaking) ->
                        WorldModule.fix5 simulant left right breaking world)
                        world fixes
                let world =
                    List.fold (fun world (handler, address, simulant) ->
                        World.monitor (fun (evt : Event) world ->
                            let signal = handler evt
                            match origin with
                            | SimulantOrigin owner -> WorldModule.trySignal signal owner world
                            | FacetOrigin (owner, facetName) -> WorldModule.trySignalFacet signal facetName owner world)
                            address simulant world)
                        world handlers
                let world =
                    List.fold (fun world content ->
                        World.expandEntityContent (Some Gen.id) content origin layer world)
                        world (snd content)
                world
            | Choice3Of3 (entityName, filePath) ->
                let (entity, world) = World.readEntityFromFile filePath (Some entityName) layer world
                let world = match guidOpt with Some guid -> World.addKeyedValue (scstring guid) entity world | None -> world
                let world =
                    match origin with
                    | SimulantOrigin simulant
                    | FacetOrigin (simulant, _) ->
                        World.monitor
                            (constant $ World.destroyEntity entity)
                            (Events.Unregistering --> simulant.SimulantAddress)
                            entity
                            world
                world

namespace Debug
open Nu
type Entity =

    /// Provides a full view of all the properties of an entity. Useful for debugging such
    /// as with the Watch feature in Visual Studio.
    static member view entity world = World.viewEntityProperties entity world