// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.ComponentModel
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Runtime.CompilerServices
open OpenTK
open Prime
open Nu

[<AutoOpen>]
module WorldEntityModule =

    type Entity with

        member this.GetId world = World.getEntityId this world
        member this.TagId = PropertyTag.makeReadOnly this Property? Id this.GetId
        member this.GetName world = World.getEntityName this world
        member this.TagName = PropertyTag.makeReadOnly this Property? Name this.GetName
        member this.GetXtension world = World.getEntityXtension this world
        member this.TagXtension = PropertyTag.makeReadOnly this Property? Xtension this.GetXtension
        member this.GetDispatcherNp world = World.getEntityDispatcherNp this world
        member this.TagDispatcherNp = PropertyTag.makeReadOnly this Property? DispatcherNp this.GetDispatcherNp
        member this.GetSpecialization world = World.getEntitySpecialization this world
        member this.TagSpecialization = PropertyTag.makeReadOnly this Property? Specialization this.GetSpecialization
        member this.GetClassification world = Classification.make (getTypeName ^ this.GetDispatcherNp world) (this.GetSpecialization world)
        member this.TagClassification = PropertyTag.makeReadOnly this Property? Classification this.GetClassification
        member this.GetPersistent world = World.getEntityPersistent this world
        member this.SetPersistent value world = World.setEntityPersistent value this world
        member this.TagPersistent = PropertyTag.make this Property? Persistent this.GetPersistent this.SetPersistent
        member this.GetCreationTimeStampNp world = World.getEntityCreationTimeStampNp this world
        member this.TagCreationTimeStampNp = PropertyTag.makeReadOnly this Property? CreationTimeStampNp this.GetCreationTimeStampNp
        member this.GetOptOverlayName world = World.getEntityOptOverlayName this world
        member this.SetOptOverlayName value world = World.setEntityOptOverlayName value this world
        member this.TagOptOverlayName = PropertyTag.make this Property? OptOverlayName this.GetOptOverlayName this.SetOptOverlayName
        member this.GetPosition world = World.getEntityPosition this world
        member this.SetPosition value world = World.setEntityPosition value this world
        member this.TagPosition = PropertyTag.make this Property? Position this.GetPosition this.SetPosition
        member this.GetSize world = World.getEntitySize this world
        member this.SetSize value world = World.setEntitySize value this world
        member this.TagSize = PropertyTag.make this Property? Size this.GetSize this.SetSize
        member this.GetRotation world = World.getEntityRotation this world
        member this.SetRotation value world = World.setEntityRotation value this world
        member this.TagRotation = PropertyTag.make this Property? Rotation this.GetRotation this.SetRotation
        member this.GetDepth world = World.getEntityDepth this world
        member this.SetDepth value world = World.setEntityDepth value this world
        member this.TagDepth = PropertyTag.make this Property? Depth this.GetDepth this.SetDepth
        member this.GetOverflow world = World.getEntityOverflow this world
        member this.SetOverflow value world = World.setEntityOverflow value this world
        member this.TagOverflow = PropertyTag.make this Property? Overflow this.GetOverflow this.SetOverflow
        member this.GetViewType world = World.getEntityViewType this world
        member this.SetViewType value world = World.setEntityViewType value this world
        member this.TagViewType = PropertyTag.make this Property? ViewType this.GetViewType this.SetViewType
        member this.GetVisible world = World.getEntityVisible this world
        member this.SetVisible value world = World.setEntityVisible value this world
        member this.TagVisible = PropertyTag.make this Property? Visible this.GetVisible this.SetVisible
        member this.GetOmnipresent world = World.getEntityOmnipresent this world
        member this.SetOmnipresent value world = World.setEntityOmnipresent value this world
        member this.TagOmnipresent = PropertyTag.make this Property? Omnipresent this.GetOmnipresent this.SetOmnipresent
        member this.GetPublishChanges world = World.getEntityPublishChanges this world
        member this.SetPublishChanges value world = World.setEntityPublishChanges value this world
        member this.TagPublishChanges = PropertyTag.make this Property? PublishChanges this.GetPublishChanges this.SetPublishChanges
        member this.GetPublishUpdatesNp world = World.getEntityPublishUpdatesNp this world
        member private this.SetPublishUpdatesNp value world = World.setEntityPublishUpdatesNp value this world
        member this.TagPublishUpdatesNp = PropertyTag.makeReadOnly this Property? PublishUpdatesNp this.GetPublishUpdatesNp
        member this.GetPublishPostUpdatesNp world = World.getEntityPublishPostUpdatesNp this world
        member private this.SetPublishPostUpdatesNp value world = World.setEntityPublishPostUpdatesNp value this world
        member this.TagPublishPostUpdatesNp = PropertyTag.makeReadOnly this Property? PublishPostUpdatesNp this.GetPublishPostUpdatesNp
        member this.GetFacetNames world = World.getEntityFacetNames this world
        member this.TagFacetNames = PropertyTag.makeReadOnly this Property? FacetNames this.GetFacetNames
        member this.GetFacetsNp world = World.getEntityFacetsNp this world
        member this.TagFacetsNp = PropertyTag.makeReadOnly this Property? FacetsNp this.GetFacetsNp

        /// Get a property value and type.
        member this.GetProperty propertyName world = World.getEntityProperty propertyName this world

        /// Set a property value with explicit type.
        member this.SetProperty propertyName property world = World.setEntityProperty propertyName property this world

        /// Get a property value.
        member this.Get<'a> propertyName world : 'a = World.getEntityProperty propertyName this world |> fst :?> 'a

        /// Set a property value.
        member this.Set<'a> propertyName (value : 'a) world = World.setEntityProperty propertyName (value :> obj, typeof<'a>) this world

        /// Get an entity's transform.
        member this.GetTransform world = World.getEntityTransform this world
        
        /// Set an entity's transform.
        member this.SetTransform value world = World.setEntityTransform value this world

        /// Get an entity's quick size.
        member this.GetQuickSize world = World.getEntityQuickSize this world

        /// Get an entity's bounds, not taking into account its overflow.
        member this.GetBounds world = Math.makeBounds (this.GetPosition world) (this.GetSize world)

        /// Get an entity's bounds, taking into account its overflow.
        member this.GetBoundsOverflow world = Math.makeBoundsOverflow (this.GetPosition world) (this.GetSize world) (this.GetOverflow world)

        /// Get an entity's change event address.
        member this.GetChangeEvent propertyName = Events.EntityChange propertyName ->>- this.EntityAddress

        /// Query than an entity is in the camera's view.
        member this.InView world =
            if not ^ this.GetOmnipresent world then
                World.inView
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

        /// TODO: document!
        member this.SetPositionSnapped snap position world =
            let snapped = Math.snap2F snap position
            this.SetPosition snapped world

        /// TODO: document!
        member this.SetTransformSnapped positionSnap rotationSnap transform world =
            let transform = Math.snapTransform positionSnap rotationSnap transform
            this.SetTransform transform world

        /// Check that an entity dispatches in the same manner as the dispatcher with the target type.
        member this.DispatchesAs (dispatcherTargetType : Type) world =
            Reflection.dispatchesAs dispatcherTargetType (this.GetDispatcherNp world)

    type World with

        static member internal updateEntity (entity : Entity) world =
            let dispatcher = entity.GetDispatcherNp world : EntityDispatcher
            let facets = entity.GetFacetsNp world
            let world = dispatcher.Update (entity, world)
            let world = List.foldBack (fun (facet : Facet) world -> facet.Update (entity, world)) facets world
            if entity.GetPublishUpdatesNp world then
                let eventTrace = EventTrace.record "World" "updateEntity" EventTrace.empty
                World.publish7 World.getSubscriptionsSorted World.sortSubscriptionsByHierarchy () entity.UpdateAddress eventTrace Simulants.Game world
            else world

        static member internal postUpdateEntity (entity : Entity) world =
            let dispatcher = entity.GetDispatcherNp world : EntityDispatcher
            let facets = entity.GetFacetsNp world
            let world = dispatcher.PostUpdate (entity, world)
            let world = List.foldBack (fun (facet : Facet) world -> facet.PostUpdate (entity, world)) facets world
            if entity.GetPublishPostUpdatesNp world then
                let eventTrace = EventTrace.record "World" "postUpdateEntity" EventTrace.empty
                World.publish7 World.getSubscriptionsSorted World.sortSubscriptionsByHierarchy () entity.PostUpdateAddress eventTrace Simulants.Game world
            else world

        static member internal actualizeEntity (entity : Entity) world =
            let dispatcher = entity.GetDispatcherNp world : EntityDispatcher
            let facets = entity.GetFacetsNp world
            let world = dispatcher.Actualize (entity, world)
            List.foldBack (fun (facet : Facet) world -> facet.Actualize (entity, world)) facets world

        /// Get all the entities contained by a group.
        static member getEntities group world =
            match Address.getNames group.GroupAddress with
            | [screenName; groupName] ->
                match Vmap.tryFind screenName ^ World.getScreenDirectory world with
                | Some (_, groupDirectory) ->
                    match Vmap.tryFind groupName groupDirectory with
                    | Some (_, entityDirectory) ->
                        Vmap.fold (fun state _ entityAddress -> Entity.proxy entityAddress :: state) [] entityDirectory :> _ seq
                    | None -> failwith ^ "Invalid group address '" + scstring group.GroupAddress + "'."
                | None -> failwith ^ "Invalid group address '" + scstring group.GroupAddress + "'."
            | _ -> failwith ^ "Invalid group address '" + scstring group.GroupAddress + "'."

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

        /// Propagate an entity's physics properties to the physics subsystem.
        static member propagateEntityPhysics (entity : Entity) world =
            let dispatcher = entity.GetDispatcherNp world
            let facets = entity.GetFacetsNp world
            let world = dispatcher.PropagatePhysics (entity, world)
            List.fold (fun world (facet : Facet) -> facet.PropagatePhysics (entity, world)) world facets

        /// Sort subscriptions by their editor picking priority.
        static member sortSubscriptionsByPickingPriority subscriptions world =
            World.sortSubscriptionsBy World.getEntityPickingPriority subscriptions world

        /// TODO: document!
        static member pickingSortEntities entities world =
            let entities = List.ofSeq entities
            let prioritiesAndEntities = List.map (fun (entity : Entity) -> (World.getEntityPickingPriority entity world, entity)) entities
            let prioritiesAndEntities = List.sortWith Pair.sortFstDescending prioritiesAndEntities
            List.map snd prioritiesAndEntities

        /// TODO: document!
        static member tryPickEntity position entities world =
            let entitiesSorted = World.pickingSortEntities entities world
            List.tryFind
                (fun (entity : Entity) ->
                    let positionWorld = World.mouseToWorld (entity.GetViewType world) position world
                    let picked = Math.isPointInBounds positionWorld (entity.GetBounds world)
                    picked)
                entitiesSorted

        /// Write multiple entities to a group descriptor.
        static member writeEntities entities groupDescriptor world =
            entities |>
            Seq.sortBy (fun (entity : Entity) -> entity.GetCreationTimeStampNp world) |>
            Seq.filter (fun (entity : Entity) -> entity.GetPersistent world) |>
            Seq.fold (fun entityDescriptors entity -> World.writeEntity entity EntityDescriptor.empty world :: entityDescriptors) groupDescriptor.Entities |>
            fun entityDescriptors -> { groupDescriptor with Entities = entityDescriptors }

        /// Read multiple entities from a group descriptor.
        static member readEntities groupDescriptor group world =
            List.foldBack
                (fun entityDescriptor (entities, world) ->
                    let (entity, world) = World.readEntity entityDescriptor None group world
                    (entity :: entities, world))
                    groupDescriptor.Entities
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
            World.getEntityProperty propertyName entity world |> fst

        /// Set the entity's property value.
        static member setValue property propertyValue (entity : Entity) world =
            let (propertyName, propertyType) =
                match property with
                | EntityXPropertyDescriptor xfd -> (xfd.PropertyName, xfd.PropertyType)
                | EntityPropertyInfo propertyInfo -> (propertyInfo.Name, propertyInfo.PropertyType)
            World.setEntityProperty propertyName (propertyValue, propertyType) entity world

        // TODO: put this in a better place! And of course, document.
        static member getPropertyDescriptors makePropertyDescriptor optXtension =
            // OPTIMIZATION: seqs used for speed.
            let properties = typeof<EntityState>.GetProperties ()
            let typeConverterAttribute = TypeConverterAttribute (typeof<SymbolicConverter>) // TODO: make this static?
            let properties = Seq.filter (fun (property : PropertyInfo) -> property.PropertyType <> typeof<Xtension>) properties
            let properties = Seq.filter (fun (property : PropertyInfo) -> Seq.isEmpty ^ property.GetCustomAttributes<ExtensionAttribute> ()) properties
            let properties = Seq.filter (fun (property : PropertyInfo) -> Reflection.isPropertyPersistentByName property.Name) properties
            let propertyDescriptors = Seq.map (fun property -> makePropertyDescriptor (EntityPropertyInfo property, [|typeConverterAttribute|])) properties
            let propertyDescriptors =
                match optXtension with
                | Some xtension ->
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
                            (Xtension.toSeq xtension)
                    Seq.append xPropertyDescriptors propertyDescriptors
                | None -> propertyDescriptors
            List.ofSeq propertyDescriptors

namespace Debug
open Nu
type Entity =

    /// Provides a full view of all the properties of an entity. Useful for debugging such
    /// as with the Watch feature in Visual Studio.
    static member view entity world = World.viewEntityProperties entity world