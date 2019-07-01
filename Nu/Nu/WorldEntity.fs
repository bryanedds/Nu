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

        member this.GetId world = World.getEntityId this world
        member this.Id = Lens.makeReadOnly Property? Id this.GetId this
        member this.GetName world = World.getEntityName this world
        member this.Name = Lens.makeReadOnly Property? Name this.GetName this
        member this.GetDispatcher world = World.getEntityDispatcher this world
        member this.Dispatcher = Lens.makeReadOnly Property? Dispatcher this.GetDispatcher this
        member this.GetImperative world = World.getEntityImperative this world
        member this.SetImperative value world = World.setEntityImperative value this world
        member this.Imperative = Lens.make Property? Imperative this.GetImperative this.SetImperative this
        member this.GetPersistent world = World.getEntityPersistent this world
        member this.SetPersistent value world = World.setEntityPersistent value this world
        member this.Persistent = Lens.make Property? Persistent this.GetPersistent this.SetPersistent this
        member this.GetCreationTimeStamp world = World.getEntityCreationTimeStamp this world
        member this.CreationTimeStamp = Lens.makeReadOnly Property? CreationTimeStamp this.GetCreationTimeStamp this
        member this.GetCacheable world = World.getEntityCachable this world
        member this.Cacheable = Lens.makeReadOnly Property? Cacheable this.GetCacheable this
        member this.GetOverlayNameOpt world = World.getEntityOverlayNameOpt this world
        member this.OverlayNameOpt = Lens.makeReadOnly Property? OverlayNameOpt this.GetOverlayNameOpt this
        member this.GetPosition world = World.getEntityPosition this world
        member this.SetPosition value world = World.setEntityPosition value this world
        member this.Position = Lens.make Property? Position this.GetPosition this.SetPosition this
        member this.GetSize world = World.getEntitySize this world
        member this.SetSize value world = World.setEntitySize value this world
        member this.Size = Lens.make Property? Size this.GetSize this.SetSize this
        member this.GetRotation world = World.getEntityRotation this world
        member this.SetRotation value world = World.setEntityRotation value this world
        member this.Rotation = Lens.make Property? Rotation this.GetRotation this.SetRotation this
        member this.GetDepth world = World.getEntityDepth this world
        member this.SetDepth value world = World.setEntityDepth value this world
        member this.Depth = Lens.make Property? Depth this.GetDepth this.SetDepth this
        member this.GetDepthLayered world = World.getEntityDepth this world + World.getLayerDepth (etol this) world
        member this.DepthLayered = Lens.makeReadOnly Property? DepthLayered this.GetDepthLayered this
        member this.GetOverflow world = World.getEntityOverflow this world
        member this.SetOverflow value world = World.setEntityOverflow value this world
        member this.Overflow = Lens.make Property? Overflow this.GetOverflow this.SetOverflow this
        member this.GetViewType world = World.getEntityViewType this world
        member this.SetViewType value world = World.setEntityViewType value this world
        member this.ViewType = Lens.make Property? ViewType this.GetViewType this.SetViewType this
        member this.GetVisible world = World.getEntityVisible this world
        member this.SetVisible value world = World.setEntityVisible value this world
        member this.Visible = Lens.make Property? Visible this.GetVisible this.SetVisible this
        member this.GetVisibleLayered world = World.getEntityVisible this world && World.getLayerVisible (etol this) world
        member this.VisibleLayered = Lens.makeReadOnly Property? VisibleLayered this.GetVisibleLayered this
        member this.GetEnabled world = World.getEntityEnabled this world
        member this.SetEnabled value world = World.setEntityEnabled value this world
        member this.Enabled = Lens.make Property? Enabled this.GetEnabled this.SetEnabled this
        member this.GetOmnipresent world = World.getEntityOmnipresent this world
        member this.SetOmnipresent value world = World.setEntityOmnipresent value this world
        member this.Omnipresent = Lens.make Property? Omnipresent this.GetOmnipresent this.SetOmnipresent this
        member this.GetAlwaysUpdate world = World.getEntityAlwaysUpdate this world
        member this.SetAlwaysUpdate value world = World.setEntityAlwaysUpdate value this world
        member this.AlwaysUpdate = Lens.make Property? AlwaysUpdate this.GetAlwaysUpdate this.SetAlwaysUpdate this
        member this.GetPublishChanges world = World.getEntityPublishChanges this world
        member this.SetPublishChanges value world = World.setEntityPublishChanges value this world
        member this.PublishChanges = Lens.make Property? PublishChanges this.GetPublishChanges this.SetPublishChanges this
        member this.GetFacetNames world = World.getEntityFacetNames this world
        member this.FacetNames = Lens.makeReadOnly Property? FacetNames this.GetFacetNames this
        member this.GetFacets world = World.getEntityFacets this world
        member this.Facets = Lens.makeReadOnly Property? Facets this.GetFacets this

        /// Try to get a property value and type.
        member this.TryGetProperty propertyName world = World.tryGetEntityProperty propertyName this world

        /// Get a property value and type.
        member this.GetProperty propertyName world = World.getEntityProperty propertyName this world

        /// Get a property value.
        member this.Get<'a> propertyName world : 'a = (World.getEntityProperty propertyName this world).PropertyValue :?> 'a

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

        /// Get an entity's transform.
        member this.GetTransform world = World.getEntityTransform this world
        
        /// Set an entity's transform.
        member this.SetTransform value world = World.setEntityTransform value this world

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

        /// Get the center position of an entity.
        member this.GetCenter world =
            let transform = this.GetTransform world
            transform.Position + transform.Size * 0.5f

        /// Set the center position of an entity.
        member this.SetCenter center world =
            let size = this.GetSize world
            this.SetPosition (center - size * 0.5f) world

        /// Set the transform of an entity snapped to the give position and rotation snaps.
        member this.SetTransformSnapped positionSnap rotationSnap transform world =
            let transform = Math.snapTransform positionSnap rotationSnap transform
            this.SetTransform transform world

        /// Check that an entity exists in the world.
        member this.GetExists world = World.getEntityExists this world

        /// Propagate entity physics properties into the physics system.
        member this.PropagatePhysics world =
            World.withEventContext (fun world ->
                let dispatcher = this.GetDispatcher world
                let facets = this.GetFacets world
                let world = dispatcher.PropagatePhysics (this, world)
                List.fold (fun world (facet : Facet) -> facet.PropagatePhysics (this, world)) world facets)
                this
                world

        /// Check that an entity uses a facet of the given type.
        member this.FacetedAs (facetType, world) = List.exists (fun facet -> getType facet = facetType) (this.GetFacets world)

        /// Check that an entity uses a facet of the given type.
        member this.FacetedAs<'a> world = this.FacetedAs (typeof<'a>, world)

        /// Check that an entity dispatches in the same manner as the dispatcher with the given type.
        member this.DispatchesAs (dispatcherType, world) = Reflection.dispatchesAs dispatcherType (this.GetDispatcher world)

        /// Check that an entity dispatches in the same manner as the dispatcher with the given type.
        member this.DispatchesAs<'a> world = this.DispatchesAs (typeof<'a>, world)

        /// Resolve a relation in the context of an entity.
        member this.Resolve relation = Entity (Relation.resolve this.EntityAddress relation)

        /// Get an entity's change event address.
        member this.GetChangeEvent propertyName = Events.Change propertyName --> this.EntityAddress

    type World with

        static member internal updateEntity (entity : Entity) world =
            World.withEventContext (fun world ->
                let dispatcher = entity.GetDispatcher world
                let facets = entity.GetFacets world
                let world = dispatcher.Update (entity, world)
                let world = List.foldBack (fun (facet : Facet) world -> facet.Update (entity, world)) facets world
                if World.getEntityPublishUpdates entity world then
                    let eventTrace = EventTrace.record "World" "updateEntity" EventTrace.empty
                    World.publishPlus World.sortSubscriptionsByHierarchy () entity.UpdateEvent eventTrace Default.Game false world
                else world)
                entity
                world

        static member internal postUpdateEntity (entity : Entity) world =
            World.withEventContext (fun world ->
                let dispatcher = entity.GetDispatcher world
                let facets = entity.GetFacets world
                let world = dispatcher.PostUpdate (entity, world)
                let world = List.foldBack (fun (facet : Facet) world -> facet.PostUpdate (entity, world)) facets world
                if World.getEntityPublishPostUpdates entity world then
                    let eventTrace = EventTrace.record "World" "postUpdateEntity" EventTrace.empty
                    World.publishPlus World.sortSubscriptionsByHierarchy () entity.PostUpdateEvent eventTrace Default.Game false world
                else world)
                entity
                world

        static member internal actualizeEntity (entity : Entity) world =
            World.withEventContext (fun world ->
                let dispatcher = entity.GetDispatcher world
                let facets = entity.GetFacets world
                let world = dispatcher.Actualize (entity, world)
                List.foldBack (fun (facet : Facet) world -> facet.Actualize (entity, world)) facets world)
                entity
                world

        /// Get all the entities contained by a layer.
        [<FunctionBinding>]
        static member getEntities (layer : Layer) world =
            match Address.getNames layer.LayerAddress with
            | [screenName; layerName] ->
                let layerDirectoryOpt = UMap.tryFindFast screenName (World.getScreenDirectory world)
                if FOption.isSome layerDirectoryOpt then
                    let layerDirectory = FOption.get layerDirectoryOpt
                    let entityDirectoryOpt = UMap.tryFindFast layerName layerDirectory.Value
                    if FOption.isSome entityDirectoryOpt then
                        let entityDirectory = FOption.get entityDirectoryOpt
                        UMap.fold (fun state _ (entityAddress : _ Address) -> Entity entityAddress :: state) [] entityDirectory.Value :> _ seq
                    else failwith ("Invalid layer address '" + scstring layer.LayerAddress + "'.")
                else failwith ("Invalid layer address '" + scstring layer.LayerAddress + "'.")
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
            Seq.fold (fun entityDescriptors entity -> World.writeEntity entity EntityDescriptor.empty world :: entityDescriptors) layerDescriptor.Entities |>
            fun entityDescriptors -> { layerDescriptor with Entities = entityDescriptors }

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
                    layerDescriptor.Entities
                    ([], world)

        /// Transform a stream into existing entities.
        static member streamEntities (mapper : 'a -> EntityContent) (ownerOpt : Entity option) (layer : Layer) (stream : Stream<'a list, World>) =
            stream |>
            Stream.optimize |>
            Stream.insert (makeGuid ()) |>
            Stream.map (fun (guid, list) -> List.mapi (fun i a -> PartialComparable.make (makeGuidDeterministic i guid) (mapper a)) list |> Set.ofList) |>
            Stream.fold (fun (p, _, _) c -> (c, Set.difference c p, Set.difference p c)) (Set.empty, Set.empty, Set.empty) |>
            Stream.mapEffect (fun evt world ->
                let (current, added, removed) = evt.Data
                let world =
                    Seq.fold (fun world guidAndContent ->
                        let (guid, content) = PartialComparable.unmake guidAndContent
                        match World.tryGetKeyedValue (scstring guid) world with
                        | None -> World.expandEntityContent (Some guid) content ownerOpt layer world
                        | Some _ -> world)
                        world added
                let world =
                    Seq.fold (fun world guidAndContent ->
                        let (guid, _) = PartialComparable.unmake guidAndContent
                        match World.tryGetKeyedValue (scstring guid) world with
                        | Some entityObj ->
                            let entity = entityObj :?> Entity
                            let world = World.removeKeyedValue (scstring guid) world
                            World.destroyEntity entity world
                        | None -> failwithumf ())
                        world removed
                (current, world))

        /// Turn an entity stream into a series of entities.
        static member expandEntityStream (lens : World Lens) mapper ownerOpt layer world =
            Stream.make (Events.Register --> lens.This.ParticipantAddress) |>
            Stream.sum (Stream.make lens.ChangeEvent) |>
            Stream.mapEvent (fun _ world -> lens.Get world |> Reflection.objToObjList) |>
            World.streamEntities mapper ownerOpt layer |>
            Stream.subscribe (fun _ value -> value) Default.Game $ world

        /// Turn entity content into an entity.
        static member expandEntityContent guidOpt content ownerOpt layer world =
            match EntityContent.expand content layer world with
            | Choice1Of3 (lens, mapper) ->
                World.expandEntityStream lens mapper ownerOpt layer world
            | Choice2Of3 (entityName, descriptor, equations, content) ->
                let (entity, world) = World.readEntity descriptor (Some entityName) layer world
                let world = match guidOpt with Some guid -> World.addKeyedValue (scstring guid) entity world | None -> world
                let world = match ownerOpt with Some owner -> World.monitor (constant $ World.destroyEntity entity) (Events.Unregistering --> owner) entity world | None -> world
                let world =
                    List.fold (fun world (name, simulant, property, breaking) ->
                        WorldModule.equate5 name simulant property breaking world)
                        world equations
                let world =
                    List.fold (fun world content ->
                        World.expandEntityContent (Some (makeGuid ())) content ownerOpt layer world)
                        world (snd content)
                world
            | Choice3Of3 (entityName, filePath) ->
                let (entity, world) = World.readEntityFromFile filePath (Some entityName) layer world
                let world = match guidOpt with Some guid -> World.addKeyedValue (scstring guid) entity world | None -> world
                let world = match ownerOpt with Some owner -> World.monitor (constant $ World.destroyEntity entity) (Events.Unregistering --> owner) entity world | None -> world
                world

    /// Represents the property value of an entity as accessible via reflection.
    type [<ReferenceEquality>] EntityPropertyValue =
        | EntityPropertyDescriptor of PropertyDescriptor
        | EntityPropertyInfo of PropertyInfo

        /// Check that an entity contains the given property.
        static member containsProperty (property : PropertyInfo) =
            let properties = typeof<EntityState>.GetProperties property.Name
            Seq.exists (fun item -> item = property) properties

        /// Attempt to get the entity's property value.
        static member tryGetValue property (entity : Entity) world =
            let propertyName =
                match property with
                | EntityPropertyDescriptor propertyDescriptor -> propertyDescriptor.PropertyName
                | EntityPropertyInfo propertyInfo -> propertyInfo.Name
            match World.tryGetEntityProperty propertyName entity world with
            | Some property -> Some property.PropertyValue
            | None -> None

        /// Attempt to set the entity's property value.
        static member trySetValue alwaysPublish nonPersistent property propertyValue (entity : Entity) world =
            let (propertyName, propertyType) =
                match property with
                | EntityPropertyDescriptor propertyDescriptor -> (propertyDescriptor.PropertyName, propertyDescriptor.PropertyType)
                | EntityPropertyInfo propertyInfo -> (propertyInfo.Name, propertyInfo.PropertyType)
            let property = { PropertyType = propertyType; PropertyValue = propertyValue }
            World.trySetEntityProperty propertyName alwaysPublish nonPersistent property entity world

        /// Get the property descriptors of as constructed from the given function in the given context.
        static member getPropertyDescriptors makePropertyDescriptor contextOpt =
            // OPTIMIZATION: seqs used for speed.
            let properties = typeof<EntityState>.GetProperties ()
            let typeConverterAttribute = TypeConverterAttribute typeof<SymbolicConverter>
            let properties = Seq.filter (fun (property : PropertyInfo) -> property.PropertyType <> typeof<Xtension>) properties
            let properties = Seq.filter (fun (property : PropertyInfo) -> Seq.isEmpty (property.GetCustomAttributes<ExtensionAttribute> ())) properties
            let properties = Seq.filter (fun (property : PropertyInfo) -> Reflection.isPropertyPersistentByName property.Name) properties
            let propertyDescriptors = Seq.map (fun property -> makePropertyDescriptor (EntityPropertyInfo property, [|typeConverterAttribute|])) properties
            let propertyDescriptors =
                match contextOpt with
                | Some (entity, world) ->
                    let properties' = World.getEntityXtensionProperties entity world
                    let propertyDescriptors' =
                        Seq.fold
                            (fun propertyDescriptors' (propertyName, property : Property) ->
                                let propertyType = property.PropertyType
                                if Reflection.isPropertyPersistentByName propertyName then
                                    let propertyDescriptor = EntityPropertyDescriptor { PropertyName = propertyName; PropertyType = propertyType }
                                    let propertyDescriptor : System.ComponentModel.PropertyDescriptor = makePropertyDescriptor (propertyDescriptor, [|typeConverterAttribute|])
                                    propertyDescriptor :: propertyDescriptors'
                                else propertyDescriptors')
                            []
                            properties'
                    Seq.append propertyDescriptors' propertyDescriptors
                | None -> propertyDescriptors
            List.ofSeq propertyDescriptors

namespace Debug
open Nu
type Entity =

    /// Provides a full view of all the properties of an entity. Useful for debugging such
    /// as with the Watch feature in Visual Studio.
    static member view entity world = World.viewEntityProperties entity world