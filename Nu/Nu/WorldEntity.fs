// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Nu
open System
open System.ComponentModel
open System.Reflection
open System.Runtime.CompilerServices
open Prime
open Nu

[<AutoOpen>]
module WorldEntityModule =

    type Entity with

        member this.GetId world = World.getEntityId this world
        member this.Id = PropertyTag.makeReadOnly this Property? Id this.GetId
        member this.GetName world = World.getEntityName this world
        member this.Name = PropertyTag.makeReadOnly this Property? Name this.GetName
        member this.GetDispatcherNp world = World.getEntityDispatcherNp this world
        member this.DispatcherNp = PropertyTag.makeReadOnly this Property? DispatcherNp this.GetDispatcherNp
        member this.GetSpecialization world = World.getEntitySpecialization this world
        member this.Specialization = PropertyTag.makeReadOnly this Property? Specialization this.GetSpecialization
        member this.GetClassification world = Classification.make (getTypeName ^ this.GetDispatcherNp world) (this.GetSpecialization world)
        member this.Classification = PropertyTag.makeReadOnly this Property? Classification this.GetClassification
        member this.GetPersistent world = World.getEntityPersistent this world
        member this.SetPersistent value world = World.setEntityPersistent value this world
        member this.Persistent = PropertyTag.make this Property? Persistent this.GetPersistent this.SetPersistent
        member this.GetCreationTimeStampNp world = World.getEntityCreationTimeStampNp this world
        member this.CreationTimeStampNp = PropertyTag.makeReadOnly this Property? CreationTimeStampNp this.GetCreationTimeStampNp
        member this.GetCacheableNp world = World.getEntityCachableNp this world
        member this.CacheableNp = PropertyTag.makeReadOnly this Property? CacheableNp this.GetCacheableNp
        member this.GetImperative world = World.getEntityImperative this world
        member this.Imperative = PropertyTag.makeReadOnly this Property? Imperative this.GetImperative
        member this.GetOverlayNameOpt world = World.getEntityOverlayNameOpt this world
        member this.SetOverlayNameOpt value world = World.setEntityOverlayNameOpt value this world
        member this.OverlayNameOpt = PropertyTag.make this Property? OverlayNameOpt this.GetOverlayNameOpt this.SetOverlayNameOpt
        member this.GetPosition world = World.getEntityPosition this world
        member this.SetPosition value world = World.setEntityPosition value this world
        member this.Position = PropertyTag.make this Property? Position this.GetPosition this.SetPosition
        member this.GetSize world = World.getEntitySize this world
        member this.SetSize value world = World.setEntitySize value this world
        member this.Size = PropertyTag.make this Property? Size this.GetSize this.SetSize
        member this.GetRotation world = World.getEntityRotation this world
        member this.SetRotation value world = World.setEntityRotation value this world
        member this.Rotation = PropertyTag.make this Property? Rotation this.GetRotation this.SetRotation
        member this.GetDepth world = World.getEntityDepth this world
        member this.SetDepth value world = World.setEntityDepth value this world
        member this.Depth = PropertyTag.make this Property? Depth this.GetDepth this.SetDepth
        member this.GetDepthLayered world = World.getEntityDepth this world + World.getLayerDepth (etol this) world
        member this.DepthLayered = PropertyTag.makeReadOnly this Property? DepthLayered this.GetDepthLayered
        member this.GetOverflow world = World.getEntityOverflow this world
        member this.SetOverflow value world = World.setEntityOverflow value this world
        member this.Overflow = PropertyTag.make this Property? Overflow this.GetOverflow this.SetOverflow
        member this.GetViewType world = World.getEntityViewType this world
        member this.SetViewType value world = World.setEntityViewType value this world
        member this.ViewType = PropertyTag.make this Property? ViewType this.GetViewType this.SetViewType
        member this.GetVisible world = World.getEntityVisible this world
        member this.SetVisible value world = World.setEntityVisible value this world
        member this.Visible = PropertyTag.make this Property? Visible this.GetVisible this.SetVisible
        member this.GetVisibleLayered world = World.getEntityVisible this world && World.getLayerVisible (etol this) world
        member this.VisibleLayered = PropertyTag.makeReadOnly this Property? VisibleLayered this.GetVisibleLayered
        member this.GetEnabled world = World.getEntityEnabled this world
        member this.SetEnabled value world = World.setEntityEnabled value this world
        member this.Enabled = PropertyTag.make this Property? Enabled this.GetEnabled this.SetEnabled
        member this.GetOmnipresent world = World.getEntityOmnipresent this world
        member this.SetOmnipresent value world = World.setEntityOmnipresent value this world
        member this.Omnipresent = PropertyTag.make this Property? Omnipresent this.GetOmnipresent this.SetOmnipresent
        member this.GetPublishChanges world = World.getEntityPublishChanges this world
        member this.SetPublishChanges value world = World.setEntityPublishChanges value this world
        member this.PublishChanges = PropertyTag.make this Property? PublishChanges this.GetPublishChanges this.SetPublishChanges
        member this.GetPublishUpdatesNp world = World.getEntityPublishUpdatesNp this world
        member private this.SetPublishUpdatesNp value world = World.setEntityPublishUpdatesNp value this world
        member this.PublishUpdatesNp = PropertyTag.makeReadOnly this Property? PublishUpdatesNp this.GetPublishUpdatesNp
        member this.GetPublishPostUpdatesNp world = World.getEntityPublishPostUpdatesNp this world
        member private this.SetPublishPostUpdatesNp value world = World.setEntityPublishPostUpdatesNp value this world
        member this.PublishPostUpdatesNp = PropertyTag.makeReadOnly this Property? PublishPostUpdatesNp this.GetPublishPostUpdatesNp
        member this.GetFacetNames world = World.getEntityFacetNames this world
        member this.FacetNames = PropertyTag.makeReadOnly this Property? FacetNames this.GetFacetNames
        member this.GetFacetsNp world = World.getEntityFacetsNp this world
        member this.FacetsNp = PropertyTag.makeReadOnly this Property? FacetsNp this.GetFacetsNp

        /// Try to get a property value and type.
        member this.TryGetProperty propertyName world = World.tryGetEntityProperty propertyName this world

        /// Get a property value and type.
        member this.GetProperty propertyName world = World.getEntityProperty propertyName this world

        /// Get a property value.
        member this.Get<'a> propertyName world : 'a = World.getEntityProperty propertyName this world |> snd :?> 'a

        /// Try to set a property value with explicit type.
        member this.TrySetProperty propertyName property world = World.trySetEntityProperty propertyName property this world

        /// Set a property value with explicit type.
        member this.SetProperty propertyName property world = World.setEntityProperty propertyName property this world

        /// Set a property value.
        member this.Set<'a> propertyName (value : 'a) world = World.setEntityProperty propertyName (typeof<'a>, value :> obj) this world

        /// Get an entity's transform.
        member this.GetTransform world = World.getEntityTransform this world
        
        /// Set an entity's transform.
        member this.SetTransform value world = World.setEntityTransform value this world

        /// Get an entity's sorting priority.
        member this.GetSortingPriority world = World.getEntitySortingPriority this world

        /// Get an entity's quick size.
        member this.GetQuickSize world = World.getEntityQuickSize this world

        /// Get an entity's bounds, not taking into account its overflow.
        member this.GetBounds world = Math.makeBounds (this.GetPosition world) (this.GetSize world)

        /// Get an entity's bounds, taking into account its overflow.
        member this.GetBoundsOverflow world = Math.makeBoundsOverflow (this.GetPosition world) (this.GetSize world) (this.GetOverflow world)

        /// Get an entity's bounds maximum.
        member this.GetBoundsMax world = World.getEntityBoundsMax this world

        /// Check that an entity is selected.
        member this.GetSelected world =
            match (World.getGameState world).SelectedScreenOpt with
            | Some screen -> Address.head this.EntityAddress = Address.head screen.ScreenAddress
            | None -> false

        /// Check that an entity is in the camera's view.
        member this.GetInView world =
            if not ^ this.GetOmnipresent world then
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
        member this.GetExists world = World.entityExists this world

        /// Propagate entity physics properties into the physics system.
        member this.PropagatePhysics world =
            World.withEventContext (fun world ->
                let dispatcher = this.GetDispatcherNp world
                let facets = this.GetFacetsNp world
                let world = dispatcher.PropagatePhysics (this, world)
                List.fold (fun world (facet : Facet) -> facet.PropagatePhysics (this, world)) world facets)
                this
                world

        /// Check that an entity uses a facet of type 'a.
        member this.HasFacet facetType world = List.exists (fun facet -> getType facet = facetType) (this.GetFacetsNp world)

        /// Check that an entity dispatches in the same manner as the dispatcher with the target type.
        member this.DispatchesAs dispatcherTargetType world = Reflection.dispatchesAs dispatcherTargetType (this.GetDispatcherNp world)

        /// Resolve a relation in the context of an entity.
        member this.Resolve relation = Entity (Relation.resolve this.EntityAddress relation)

        /// Get an entity's change event address.
        member this.GetChangeEvent propertyName = Events.EntityChange propertyName ->>- this.EntityAddress

    type World with

        static member internal updateEntity (entity : Entity) world =
            World.withEventContext (fun world ->
                let dispatcher = entity.GetDispatcherNp world
                let facets = entity.GetFacetsNp world
                let world = dispatcher.Update (entity, world)
                let world = List.foldBack (fun (facet : Facet) world -> facet.Update (entity, world)) facets world
                if entity.GetPublishUpdatesNp world then
                    let eventTrace = EventTrace.record "World" "updateEntity" EventTrace.empty
                    World.publishPlus World.sortSubscriptionsByHierarchy () entity.UpdateAddress eventTrace Simulants.Game false world
                else world)
                entity
                world

        static member internal postUpdateEntity (entity : Entity) world =
            World.withEventContext (fun world ->
                let dispatcher = entity.GetDispatcherNp world
                let facets = entity.GetFacetsNp world
                let world = dispatcher.PostUpdate (entity, world)
                let world = List.foldBack (fun (facet : Facet) world -> facet.PostUpdate (entity, world)) facets world
                if entity.GetPublishPostUpdatesNp world then
                    let eventTrace = EventTrace.record "World" "postUpdateEntity" EventTrace.empty
                    World.publishPlus World.sortSubscriptionsByHierarchy () entity.PostUpdateAddress eventTrace Simulants.Game false world
                else world)
                entity
                world

        static member internal actualizeEntity (entity : Entity) world =
            World.withEventContext (fun world ->
                let dispatcher = entity.GetDispatcherNp world
                let facets = entity.GetFacetsNp world
                let world = dispatcher.Actualize (entity, world)
                List.foldBack (fun (facet : Facet) world -> facet.Actualize (entity, world)) facets world)
                entity
                world

        /// Get all the entities contained by a layer.
        static member getEntities (layer : Layer) world =
            match Address.getNames layer.LayerAddress with
            | [screenName; layerName] ->
                let layerDirectoryOpt = UMap.tryFindFast screenName ^ World.getScreenDirectory world
                if FOption.isSome layerDirectoryOpt then
                    let layerDirectory = FOption.get layerDirectoryOpt
                    let entityDirectoryOpt = UMap.tryFindFast layerName layerDirectory.Value
                    if FOption.isSome entityDirectoryOpt then
                        let entityDirectory = FOption.get entityDirectoryOpt
                        UMap.fold (fun state _ entityAddress -> Entity entityAddress :: state) [] entityDirectory.Value :> _ seq
                    else failwith ^ "Invalid layer address '" + scstring layer.LayerAddress + "'."
                else failwith ^ "Invalid layer address '" + scstring layer.LayerAddress + "'."
            | _ -> failwith ^ "Invalid layer address '" + scstring layer.LayerAddress + "'."

        /// Destroy an entity in the world at the end of the current update.
        static member destroyEntity entity world =
            let tasklet =
                { ScheduledTime = World.getTickTime world
                  Command = { Execute = fun world -> World.destroyEntityImmediate entity world }}
            World.addTasklet tasklet world

        /// Destroy multiple entities in the world immediately. Can be dangerous if existing in-flight publishing
        /// depends on any of the entities' existences. Consider using World.destroyEntities instead.
        static member destroyEntitiesImmediate entities world =
            List.foldBack
                (fun entity world -> World.destroyEntityImmediate entity world)
                (List.ofSeq entities)
                world

        /// Destroy multiple entities in the world at the end of the current update.
        static member destroyEntities entities world =
            let tasklet =
                { ScheduledTime = World.getTickTime world
                  Command = { Execute = fun world -> World.destroyEntitiesImmediate entities world }}
            World.addTasklet tasklet world

        /// Sort the given entities.
        static member sortEntities entities world =
            /// OPTIMIZATION: using arrays for speed
            entities |>
            Array.ofSeq |>
            Array.rev |>
            Array.map (fun (entity : Entity) -> entity.GetSortingPriority world) |>
            Seq.sortWith SortPriority.compare |> // Seq.sort is stable, unlike Array.sort...
            Array.ofSeq |>
            Array.map (fun p -> p.SortTarget :?> Entity)

        /// Try to pick an entity at the given position.
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
            Seq.sortBy (fun (entity : Entity) -> entity.GetCreationTimeStampNp world) |>
            Seq.filter (fun (entity : Entity) -> entity.GetPersistent world) |>
            Seq.fold (fun entityDescriptors entity -> World.writeEntity entity EntityDescriptor.empty world :: entityDescriptors) layerDescriptor.Entities |>
            fun entityDescriptors -> { layerDescriptor with Entities = entityDescriptors }

        /// Read multiple entities from a layer descriptor.
        static member readEntities layerDescriptor layer world =
            List.foldBack
                (fun entityDescriptor (entities, world) ->
                    let (entity, world) = World.readEntity entityDescriptor None layer world
                    (entity :: entities, world))
                    layerDescriptor.Entities
                    ([], world)

    /// Represents the property value of an entity as accessible via reflection.
    type [<ReferenceEquality>] EntityPropertyValue =
        | EntityXPropertyDescriptor of XPropertyDescriptor
        | EntityPropertyInfo of PropertyInfo

        /// Check that an entity contains the given property.
        static member containsProperty (property : PropertyInfo) =
            let properties = typeof<EntityState>.GetProperties property.Name
            Seq.exists (fun item -> item = property) properties

        /// Get the entity's property value.
        static member getValue property (entity : Entity) world =
            let propertyName =
                match property with
                | EntityXPropertyDescriptor xfd -> xfd.PropertyName
                | EntityPropertyInfo propertyInfo -> propertyInfo.Name
            World.getEntityProperty propertyName entity world |> snd

        /// Set the entity's property value.
        static member setValue property propertyValue (entity : Entity) world =
            let (propertyName, propertyType) =
                match property with
                | EntityXPropertyDescriptor xfd -> (xfd.PropertyName, xfd.PropertyType)
                | EntityPropertyInfo propertyInfo -> (propertyInfo.Name, propertyInfo.PropertyType)
            World.setEntityProperty propertyName (propertyType, propertyValue) entity world

        /// Get the property descriptors of as constructed from the given function in the given context.
        static member getPropertyDescriptors makePropertyDescriptor contextOpt =
            // OPTIMIZATION: seqs used for speed.
            let properties = typeof<EntityState>.GetProperties ()
            let typeConverterAttribute = TypeConverterAttribute typeof<SymbolicConverter>
            let properties = Seq.filter (fun (property : PropertyInfo) -> property.PropertyType <> typeof<Xtension>) properties
            let properties = Seq.filter (fun (property : PropertyInfo) -> Seq.isEmpty ^ property.GetCustomAttributes<ExtensionAttribute> ()) properties
            let properties = Seq.filter (fun (property : PropertyInfo) -> Reflection.isPropertyPersistentByName property.Name) properties
            let propertyDescriptors = Seq.map (fun property -> makePropertyDescriptor (EntityPropertyInfo property, [|typeConverterAttribute|])) properties
            let propertyDescriptors =
                match contextOpt with
                | Some (entity, world) ->
                    let xProperties = World.getEntityXtensionProperties entity world
                    let xPropertyDescriptors =
                        Seq.fold
                            (fun xPropertyDescriptors (xPropertyName, xProperty : XProperty) ->
                                let xPropertyType = xProperty.PropertyType
                                if Reflection.isPropertyPersistentByName xPropertyName then
                                    let xPropertyDescriptor = EntityXPropertyDescriptor { PropertyName = xPropertyName; PropertyType = xPropertyType }
                                    let xPropertyDescriptor : System.ComponentModel.PropertyDescriptor = makePropertyDescriptor (xPropertyDescriptor, [|typeConverterAttribute|])
                                    xPropertyDescriptor :: xPropertyDescriptors
                                else xPropertyDescriptors)
                            []
                            xProperties
                    Seq.append xPropertyDescriptors propertyDescriptors
                | None -> propertyDescriptors
            List.ofSeq propertyDescriptors

namespace Debug
open Nu
type Entity =

    /// Provides a full view of all the properties of an entity. Useful for debugging such
    /// as with the Watch feature in Visual Studio.
    static member view entity world = World.viewEntityProperties entity world