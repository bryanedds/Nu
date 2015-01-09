namespace Nu
open System
open System.Collections.Generic
open System.ComponentModel
open System.Reflection
open System.Xml
open System.Xml.Serialization
open OpenTK
open TiledSharp
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants

[<AutoOpen>]
module EntityModule =

    type Entity with

        static member getName (entity : Entity) = entity.Name
        static member getFacetNames (entity : Entity) = entity.FacetNames
        static member getOptOverlayName (entity : Entity) = entity.OptOverlayName
        static member getPosition (entity : Entity) = entity.Position
        static member setPosition value (entity : Entity) = { entity with Position = value }
        static member getDepth (entity : Entity) = entity.Depth
        static member setDepth value (entity : Entity) = { entity with Depth = value }
        static member getSize (entity : Entity) = entity.Size
        static member setSize value (entity : Entity) = { entity with Size = value }
        static member getRotation (entity : Entity) = entity.Rotation
        static member setRotation value (entity : Entity) = { entity with Rotation = value }
        static member getVisible (entity : Entity) = entity.Visible
        static member setVisible value (entity : Entity) = { entity with Visible = value }
        static member getViewType (entity : Entity) = entity.ViewType
        static member setViewType value (entity : Entity) = { entity with ViewType = value }
        static member getPublishChanges (entity : Entity) = entity.PublishChanges
        static member setPublishChanges value (entity : Entity) = { entity with PublishChanges = value }
        static member getPersistent (entity : Entity) = entity.Persistent
        static member setPersistent value (entity : Entity) = { entity with Persistent = value }

        /// Register an entity when adding it to a group.
        static member register (entity : Entity) address world =
            let (entity, world) = entity.DispatcherNp.Register (entity, address, world)
            List.fold
                (fun (entity, world) (facet : Facet) -> facet.Register (entity, address, world))
                (entity, world)
                entity.FacetsNp
        
        /// Unregister an entity when removing it from a group.
        static member unregister (entity : Entity) address world =
            let (entity, world) = entity.DispatcherNp.Unregister (entity, address, world)
            List.fold
                (fun (entity, world) (facet : Facet) -> facet.Unregister (entity, address, world))
                (entity, world)
                entity.FacetsNp
        
        /// Propagate an entity's physics properties from the physics subsystem.
        static member propagatePhysics (entity : Entity) address world =
            let world = entity.DispatcherNp.PropagatePhysics (entity, address, world)
            List.fold
                (fun world (facet : Facet) -> facet.PropagatePhysics (entity, address, world))
                world
                entity.FacetsNp
        
        /// Get the render descriptors needed to render an entity.
        static member getRenderDescriptors (entity : Entity) world =
            let renderDescriptors = entity.DispatcherNp.GetRenderDescriptors (entity, world)
            List.fold
                (fun renderDescriptors (facet : Facet) ->
                    let descriptors = facet.GetRenderDescriptors (entity, world)
                    descriptors @ renderDescriptors)
                renderDescriptors
                entity.FacetsNp
        
        /// Get the quick size of an entity (the appropriate user-define size for an entity).
        static member getQuickSize  (entity : Entity) world =
            let quickSize = entity.DispatcherNp.GetQuickSize (entity, world)
            List.fold
                (fun (maxSize : Vector2) (facet : Facet) ->
                    let quickSize = facet.GetQuickSize (entity, world)
                    Vector2(
                        Math.Max (quickSize.X, maxSize.X),
                        Math.Max (quickSize.Y, maxSize.Y)))
                quickSize
                entity.FacetsNp
        
        /// Get the priority with which an entity is picked in the editor.
        static member getPickingPriority (entity : Entity) world =
            entity.DispatcherNp.GetPickingPriority (entity, world)

        /// Get the names of all facets used by an entity via reflection.
        /// TODO: see if this should be used as often as it is, and if it is needed in only one or
        /// two cases, just inline it.
        static member getFacetNamesReflectively entity =
            List.map Reflection.getTypeName entity.FacetsNp

        /// Query that a facet is compatible with those already being used by an entity.
        /// Note a facet is incompatible with any other facet if it contains any fields that has
        /// the same name but a different type.
        static member isFacetCompatible entityDispatcherMap facet (entity : Entity) =
            let facetType = facet.GetType ()
            let facetFieldDefinitions = Reflection.getFieldDefinitions facetType
            if Reflection.isFacetCompatibleWithDispatcher entityDispatcherMap facet entity then
                List.notExists
                    (fun definition ->
                        match Map.tryFind definition.FieldName entity.Xtension.XFields with
                        | Some field -> field.GetType () <> definition.FieldType
                        | None -> false)
                    facetFieldDefinitions
            else false

        /// Query that an entity dispatches in the same manner as the dispatcher with the target type.
        static member dispatchesAs (dispatcherTargetType : Type) (entity : Entity) =
            Reflection.dispatchesAs dispatcherTargetType entity.DispatcherNp

        /// Make an entity.
        static member make dispatcher optOverlayName optName =
            let id = Core.makeId ()
            { Id = id
              Name = match optName with None -> acstring id | Some name -> name
              Position = Vector2.Zero
              Depth = 0.0f
              Size = DefaultEntitySize
              Rotation = 0.0f
              Visible = true
              ViewType = Relative
              PublishChanges = true
              Persistent = true
              CreationTimeNp = DateTime.UtcNow
              DispatcherNp = dispatcher
              FacetNames = []
              FacetsNp = []
              OptOverlayName = optOverlayName
              Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true }}

    /// The data needed to describe a Tiled tile map.
    type [<StructuralEquality; NoComparison>] TileMapData =
        { Map : TmxMap
          MapSize : Vector2i
          TileSize : Vector2i
          TileSizeF : Vector2
          TileMapSize : Vector2i
          TileMapSizeF : Vector2
          TileSet : TmxTileset
          TileSetSize : Vector2i }

    /// The data needed to describe a Tiled tile.
    type [<StructuralEquality; NoComparison>] TileData =
        { Tile : TmxLayerTile
          I : int
          J : int
          Gid : int
          GidPosition : int
          Gid2 : Vector2i
          OptTileSetTile : TmxTilesetTile option
          TilePosition : Vector2i }

[<AutoOpen>]
module WorldEntityModule =

    type World with

        static member private optEntityFinder (address : Entity Address) world =
            match address.Names with
            | [screenName; groupName; entityName] ->
                let (_, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (_, groupMap) ->
                    match Map.tryFind groupName groupMap with
                    | Some (_, entityMap) -> Map.tryFind entityName entityMap
                    | None -> None
                | None -> None
            | _ -> failwith <| "Invalid entity address '" + acstring address + "'."

        static member private entityAdder (entity : Entity) (address : Entity Address) world =
            match address.Names with
            | [screenName; groupName; entityName] ->
                let (game, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (screen, groupMap) ->
                    match Map.tryFind groupName groupMap with
                    | Some (group, entityMap) ->
                        let entityMap = Map.add entityName entity entityMap
                        let groupMap = Map.add groupName (group, entityMap) groupMap
                        let screenMap = Map.add screenName (screen, groupMap) screenMap
                        { world with Simulants = (game, screenMap) }
                    | None -> failwith <| "Cannot add entity '" + acstring address + "' to non-existent group."
                | None -> failwith <| "Cannot add entity '" + acstring address + "' to non-existent screen."
            | _ -> failwith <| "Invalid entity address '" + acstring address + "'."

        static member private entityRemover (address : Entity Address) world =
            match address.Names with
            | [screenName; groupName; entityName] ->
                let (game, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (screen, groupMap) ->
                    match Map.tryFind groupName groupMap with
                    | Some (group, entityMap) ->
                        let entityMap = Map.remove entityName entityMap
                        let groupMap = Map.add groupName (group, entityMap) groupMap
                        let screenMap = Map.add screenName (screen, groupMap) screenMap
                        { world with Simulants = (game, screenMap) }
                    | None -> world
                | None -> world
            | _ -> failwith <| "Invalid entity address '" + acstring address + "'."

        /// Query that the world contains an entity at the given address.
        static member containsEntity address world =
            Option.isSome <| World.optEntityFinder address world

        /// Try to get an entity at the given address.
        static member getOptEntity address world =
            World.optEntityFinder address world

        /// Get an entity at the given address (failing with an exception otherwise), then
        /// transform it with the 'by' procudure.
        static member getEntityBy by address world =
            by ^^ Option.get ^^ World.getOptEntity address world

        /// Get an entity at the given address (failing with an exception otherwise).
        static member getEntity address world =
            World.getEntityBy id address world

        /// Get an entity with the given name in a group with the given address (failing with an
        /// exception if there isn't one).
        static member getEntityInGroup entityName groupAddress world =
            World.getEntity (gatoea groupAddress entityName) world

        /// Get an entity's address with the given name in the group with the given address
        /// (failing with an exception if there isn't one).
        static member getEntityAddressInGroup entityName groupAddress world =
            let address = gatoea groupAddress entityName
            ignore <| World.getEntity address world // ensure address is valid
            address

        static member private setEntityWithoutEvent entity address world =
            World.entityAdder entity address world

        static member private setOptEntityWithoutEvent optEntity address world =
            match optEntity with 
            | Some entity -> World.entityAdder entity address world
            | None -> World.entityRemover address world

        /// Set an entity at the given address (failing with an exception if one doesn't exist).
        static member setEntity entity address world =
            let oldEntity = Option.get <| World.optEntityFinder address world
            let world = World.entityAdder entity address world
            if entity.PublishChanges
            then World.publish4 { OldSimulant = oldEntity } (EntityChangeEventAddress ->>- address) address world
            else world

        /// Try to update an entity with the given 'updater' procedure at the given address. Also
        /// passes the current world value to the procedure.
        static member updateOptEntityW updater address world =
            match World.getOptEntity address world with
            | Some entity ->
                let entity = updater entity world
                World.setEntity entity address world
            | None -> world
            
        /// Try to update an entity with the given 'updater' procedure at the given addres
        static member updateOptEntity updater address world =
            World.updateOptEntityW (fun entity _ -> updater entity) address world
            
        /// Try to update the world with the given 'updater' procedure that uses the entity at
        /// given address in its computation.
        static member updateByOptEntity updater address world =
            match World.getOptEntity address world with
            | Some entity -> updater entity world
            | None -> world
            
        /// Update an entity with the given 'updater' procedure at the given address. Also passes
        /// the current world value to the procedure.
        static member updateEntityW updater address world =
            let entity = World.getEntity address world
            let entity = updater entity world
            World.setEntity entity address world
            
        /// Update an entity with the given 'updater' procedure at the given address.
        static member updateEntity updater address world =
            World.updateEntityW (fun entity _ -> updater entity) address world
            
        /// Update the world with the given 'updater' procedure that uses the entity at given
        /// address in its computation.
        static member updateByEntity updater address world : World =
            let entity = World.getEntity address world
            updater entity world

        /// Get all the entities at the given addresses as transformed them with the 'by'
        /// procedure.
        static member getEntitiesBy by addresses world =
            Seq.map (fun address -> by <| World.getEntity address world) addresses
            
        /// Get all the entities at the given addresses.
        static member getEntities addresses world =
            World.getEntitiesBy id addresses world

        /// Get all the entities in the group at the given address as mapped by their names.
        static member getEntityMapInGroup (groupAddress : Group Address) world =
            match groupAddress.Names with
            | [screenName; groupName] ->
                let (_, screenMap) = world.Simulants
                match Map.tryFind screenName screenMap with
                | Some (_, groupMap) ->
                    match Map.tryFind groupName groupMap with
                    | Some (_, entityMap) -> entityMap
                    | None -> Map.empty
                | None -> Map.empty
            | _ -> failwith <| "Invalid group address '" + acstring groupAddress + "'."

        /// Get all the entities in the group at the given address.
        static member getEntitiesInGroup (groupAddress : Group Address) world =
            Map.toValueSeq <| World.getEntityMapInGroup groupAddress world

        /// Get all the entity addresses in the group at the given address.
        static member getEntityAddressesInGroup groupAddress world =
            let entities = World.getEntitiesInGroup groupAddress world
            Seq.map (fun entity -> gatoea groupAddress entity.Name) entities

        /// Set the given entities to the respective addresses. Note, each address must already
        /// have an existing entity, otherwise will fail with an exception.
        static member setEntities entities addresses world =
            Seq.fold2 (fun world entity address -> World.setEntity entity address world) world entities addresses

        /// Set the given entities to the addresses as calculated by
        /// (fun entity -> gatoea groupAddress entity.Name) in the group with the given address.
        /// Note, each address must already have an existing entity, otherwise will fail with an
        /// exception.
        static member setEntitiesInGroup groupAddress entities world =
            Seq.fold (fun world (entity : Entity) -> World.setEntity entity (gatoea groupAddress entity.Name) world) world entities

        /// Update the entities at the given address with the given 'updater' procedure. Also
        /// passes the current world value to the procedure.
        static member updateEntitiesW updater addresses world =
            Seq.fold (fun world address -> World.updateEntityW updater address world) world addresses

        /// Update the entities at the given address with the given 'updater' procedure.
        static member updateEntities updater addresses world =
            World.updateEntitiesW (fun entity _ -> updater entity) addresses world

        /// Update all entities in the group at the given address with then given the 'updater'
        /// procedure. Also passes the current world value to the procedure.
        static member updateEntitiesInGroupW updater groupAddress world =
            let addresses = World.getEntityAddressesInGroup groupAddress world
            Seq.fold (fun world address -> World.updateEntityW updater address world) world addresses
            
        /// Update all entities in the group at the given address with then given the 'updater' procedure.
        static member updateEntitiesInGroup updater addresses world =
            World.updateEntitiesInGroupW (fun entity _ -> updater entity) addresses world

        /// Filter the given entity addresses by applying the 'pred' procedure to each entity at
        /// its respected address. Also passes the current world value to the procedure.
        static member filterEntityAddressesW pred addresses world =
            Seq.filter (fun address -> World.getEntityBy (fun entity -> pred entity world) address world) addresses
            
        /// Filter the given entity addresses by applying the 'pred' procedure to each entity at
        /// its respected address.
        static member filterEntityAddresses pred addresses world =
            World.filterEntityAddressesW (fun entity _ -> pred entity) addresses world

        static member private registerEntity entity address world =
            Entity.register entity address world

        static member private unregisterEntity entity address world =
            Entity.unregister entity address world

        /// Remove an entity from the world immediately. Can be dangerous if existing in-flight
        /// subscriptions depend on the entity's existence. Use with caution.
        static member removeEntityImmediate (address : Entity Address) world =
            let world = World.publish4 () (EntityRemovingEventAddress ->>- address) address world
            match World.getOptEntity address world with
            | Some entity ->
                let (entity, world) = World.unregisterEntity entity address world
                let world = World.setOptEntityWithoutEvent None address world
                (Some entity, world)
            | None -> (None, world)

        /// Remove an entity from the world on the next tick. Use this rather than
        /// removeEntityImmediate unless you need the latter's specific behavior.
        static member removeEntity address world =
            let task =
                { ScheduledTime = world.State.TickTime
                  Operation = fun world -> snd <| World.removeEntityImmediate address world }
            World.addTask task world
            
        /// Remove multiple entities from the world immediately. Can be dangerous if existing
        /// in-flight subscriptions depend on any of the entities' existences. Use with caution.
        static member removeEntitiesImmediate addresses world =
            List.foldBack
                (fun address (entities, world) ->
                    let (entity, world) = World.removeEntityImmediate address world
                    (entity :: entities, world))
                (List.ofSeq addresses)
                ([], world)
                
        /// Remove multiple entities from the world. Use this rather than removeEntitiesImmediate
        /// unless you need the latter's specific behavior.
        static member removeEntities addresses world =
            snd <| World.removeEntitiesImmediate addresses world

        /// Add an entity at the given address to the world.
        static member addEntity entity address world =
            if not <| World.containsEntity address world then
                let world = World.setEntityWithoutEvent entity address world
                let (entity, world) = World.registerEntity entity address world
                let world = World.publish4 () (EntityAddEventAddress ->>- address) address world
                (entity, world)
            else failwith <| "Adding an entity that the world already contains at address '" + acstring address + "'."

        /// Add multiple entities to the group at the given address.
        static member addEntities entities (groupAddress : Group Address) world =
            World.transformSimulants World.addEntity gatoea entities groupAddress world

        /// Make an entity (does NOT add the entity to the world!)
        static member makeEntity dispatcherName optName world =
            
            // find the entity's dispatcher
            let dispatcher = Map.find dispatcherName world.Components.EntityDispatchers
            
            // compute the default opt overlay name
            let intrinsicOverlayName = dispatcherName
            let defaultOptOverlayName = Map.find intrinsicOverlayName world.State.OverlayRouter
            
            // make the bare entity with name as id
            let entity = Entity.make dispatcher defaultOptOverlayName optName

            // attach the entity's intrinsic facets and their fields
            let entity = World.attachIntrinsicFacetsViaNames entity world
            
            // apply the entity's overlay to its facet names
            let entity =
                match defaultOptOverlayName with
                | Some defaultOverlayName ->
                    let overlayer = world.State.Overlayer
                    Overlayer.applyOverlayToFacetNames intrinsicOverlayName defaultOverlayName entity overlayer overlayer
                        
                    // synchronize the entity's facets (and attach their fields)
                    match World.trySynchronizeFacets [] entity None world with
                    | Right (entity, _) -> entity
                    | Left error -> debug error; entity
                | None -> entity

            // attach the entity's dispatcher fields
            Reflection.attachFields dispatcher entity

            // apply the entity's overlay
            match entity.OptOverlayName with
            | Some overlayName ->

                // OPTIMIZATION: apply overlay only when it will change something (EG - when it's not the intrinsic overlay)
                if intrinsicOverlayName <> overlayName then
                    let facetNames = Entity.getFacetNamesReflectively entity
                    Overlayer.applyOverlay intrinsicOverlayName overlayName facetNames entity world.State.Overlayer
                    entity
                else entity
            | None -> entity

        static member private tryGetFacet facetName world =
            match Map.tryFind facetName world.Components.Facets with
            | Some facet -> Right <| facet
            | None -> Left <| "Invalid facet name '" + facetName + "'."

        static member private getFacetNamesToAdd oldFacetNames newFacetNames =
            let newFacetNames = Set.ofList newFacetNames
            let oldFacetNames = Set.ofList oldFacetNames
            let facetNamesToAdd = Set.difference newFacetNames oldFacetNames
            List.ofSeq facetNamesToAdd

        static member private getFacetNamesToRemove oldFacetNames newFacetNames =
            let newFacetNames = Set.ofList newFacetNames
            let oldFacetNames = Set.ofList oldFacetNames
            let facetNamesToRemove = Set.difference oldFacetNames newFacetNames
            List.ofSeq facetNamesToRemove

        static member private getEntityFieldDefinitionNamesToDetach entity facetToRemove =

            // get the field definition name counts of the current, complete entity
            let fieldDefinitions = Reflection.getReflectiveFieldDefinitionMap entity
            let fieldDefinitionNameCounts = Reflection.getFieldDefinitionNameCounts fieldDefinitions

            // get the field definition name counts of the facet to remove
            let facetType = facetToRemove.GetType ()
            let facetFieldDefinitions = Map.singleton facetType.Name <| Reflection.getFieldDefinitions facetType
            let facetFieldDefinitionNameCounts = Reflection.getFieldDefinitionNameCounts facetFieldDefinitions

            // compute the difference of the counts
            let finalFieldDefinitionNameCounts =
                Map.map
                    (fun fieldName fieldCount ->
                        match Map.tryFind fieldName facetFieldDefinitionNameCounts with
                        | Some facetFieldCount -> fieldCount - facetFieldCount
                        | None -> fieldCount)
                    fieldDefinitionNameCounts

            // build a set of all field names where the final counts are negative
            Map.fold
                (fun fieldNamesToDetach fieldName fieldCount ->
                    if fieldCount = 0
                    then Set.add fieldName fieldNamesToDetach
                    else fieldNamesToDetach)
                Set.empty
                finalFieldDefinitionNameCounts

        /// Try to remove a facet with the given name from an entity.
        static member tryRemoveFacet syncing facetName entity optAddress world =
            match List.tryFind (fun facet -> Reflection.getTypeName facet = facetName) entity.FacetsNp with
            | Some facet ->
                let (entity, world) =
                    match optAddress with
                    | Some address -> facet.Unregister (entity, address, world)
                    | None -> (entity, world)
                let entity = { entity with Id = entity.Id } // hacky copy
                let fieldNames = World.getEntityFieldDefinitionNamesToDetach entity facet
                Reflection.detachFieldsViaNames fieldNames entity
                let entity =
                    if syncing then entity
                    else { entity with FacetNames = List.remove ((=) (Reflection.getTypeName facet)) entity.FacetNames }
                let entity = { entity with FacetsNp = List.remove ((=) facet) entity.FacetsNp }
                let world =
                    match optAddress with
                    | Some address -> World.setEntity entity address world
                    | None -> world
                Right (entity, world)
            | None -> Left <| "Failure to remove facet '" + facetName + "' from entity."

        /// Try to add a facet with the given name to an entity.
        static member tryAddFacet syncing facetName (entity : Entity) optAddress world =
            match World.tryGetFacet facetName world with
            | Right facet ->
                if Entity.isFacetCompatible world.Components.EntityDispatchers facet entity then
                    let entity = { entity with FacetsNp = facet :: entity.FacetsNp }
                    Reflection.attachFields facet entity
                    let entity =
                        if syncing then entity
                        else { entity with FacetNames = Reflection.getTypeName facet :: entity.FacetNames }
                    match optAddress with
                    | Some address ->
                        let (entity, world) = facet.Register (entity, address, world)
                        let world = World.setEntity entity address world
                        Right (entity, world)
                    | None -> Right (entity, world)
                else Left <| "Facet '" + Reflection.getTypeName facet + "' is incompatible with entity '" + entity.Name + "'."
            | Left error -> Left error

        /// Try to remove multiple facets from an entity.
        static member tryRemoveFacets syncing facetNamesToRemove entity optAddress world =
            List.fold
                (fun eitherEntityWorld facetName ->
                    match eitherEntityWorld with
                    | Right (entity, world) -> World.tryRemoveFacet syncing facetName entity optAddress world
                    | Left _ as left -> left)
                (Right (entity, world))
                facetNamesToRemove

        /// Try to add multiple facets to an entity.
        static member tryAddFacets syncing facetNamesToAdd entity optAddress world =
            List.fold
                (fun eitherEntityWorld facetName ->
                    match eitherEntityWorld with
                    | Right (entity, world) -> World.tryAddFacet syncing facetName entity optAddress world
                    | Left _ as left -> left)
                (Right (entity, world))
                facetNamesToAdd

        /// Try to set the facet names of an entity, synchronizing facet as needed.
        static member trySetFacetNames oldFacetNames newFacetNames entity optAddress world =
            let facetNamesToRemove = World.getFacetNamesToRemove oldFacetNames newFacetNames
            let facetNamesToAdd = World.getFacetNamesToAdd oldFacetNames newFacetNames
            match World.tryRemoveFacets false facetNamesToRemove entity optAddress world with
            | Right (entity, world) -> World.tryAddFacets false facetNamesToAdd entity optAddress world
            | Left _ as left -> left

        /// Try to synchronize the facets of an entity to its current facet names.
        static member trySynchronizeFacets oldFacetNames entity optAddress world =
            let facetNamesToRemove = World.getFacetNamesToRemove oldFacetNames entity.FacetNames
            let facetNamesToAdd = World.getFacetNamesToAdd oldFacetNames entity.FacetNames
            match World.tryRemoveFacets true facetNamesToRemove entity optAddress world with
            | Right (entity, world) -> World.tryAddFacets true facetNamesToAdd entity optAddress world
            | Left _ as left -> left

        static member private attachIntrinsicFacetsViaNames (entity : Entity) world =
            let components = world.Components
            let entity = { entity with Id = entity.Id } // hacky copy
            Reflection.attachIntrinsicFacets components.EntityDispatchers components.Facets entity.DispatcherNp entity
            entity
        
        static member internal handleBodyTransformMessage (message : BodyTransformMessage) (entity : Entity) address world =
            // OPTIMIZATION: entity is not changed (avoiding a change entity event) if position and rotation haven't changed.
            if entity.Position <> message.Position || entity.Rotation <> message.Rotation then
                let entity =
                    entity |>
                        // TODO: see if the following center-offsetting can be encapsulated within the Physics module!
                        Entity.setPosition (message.Position - entity.Size * 0.5f) |>
                        Entity.setRotation message.Rotation
                let world = World.setEntity entity address world
                (entity, world)
            else (entity, world)

        /// Write an entity to an xml writer.
        static member writeEntity (writer : XmlWriter) (entity : Entity) world =
            writer.WriteAttributeString (DispatcherNameAttributeName, (entity.DispatcherNp.GetType ()).Name)
            let shouldWriteProperty = fun propertyName propertyType (propertyValue : obj) ->
                if propertyName = "OptOverlayName" && propertyType = typeof<string option> then
                    let defaultOptOverlayName = Map.find (Reflection.getTypeName entity.DispatcherNp) world.State.OverlayRouter
                    defaultOptOverlayName <> (propertyValue :?> string option)
                else
                    let facetNames = Entity.getFacetNamesReflectively entity
                    Overlayer.shouldPropertySerialize5 facetNames propertyName propertyType entity world.State.Overlayer
            Reflection.writePropertiesFromTarget shouldWriteProperty writer entity

        /// Write multiple entities to an xml writer.
        static member writeEntities (writer : XmlWriter) entities world =
            let entitiesSorted =
                List.sortBy
                    (fun (entity : Entity) -> entity.CreationTimeNp)
                    (Map.toValueList entities)
            let entitiesFiltered = List.filter (fun (entity : Entity) -> entity.Persistent) entitiesSorted
            for entity in entitiesFiltered do
                writer.WriteStartElement typeof<Entity>.Name
                World.writeEntity writer entity world
                writer.WriteEndElement ()

        /// Read an entity from an xml node.
        static member readEntity (entityNode : XmlNode) defaultDispatcherName world =

            // read in the dispatcher name and create the dispatcher
            let dispatcherName = Reflection.readDispatcherName defaultDispatcherName entityNode
            let (dispatcherName, dispatcher) =
                match Map.tryFind dispatcherName world.Components.EntityDispatchers with
                | Some dispatcher -> (dispatcherName, dispatcher)
                | None ->
                    note <| "Could not locate dispatcher '" + dispatcherName + "'."
                    let dispatcherName = typeof<EntityDispatcher>.Name
                    let dispatcher = Map.find dispatcherName world.Components.EntityDispatchers
                    (dispatcherName, dispatcher)

            // compute the default overlay names
            let intrinsicOverlayName = dispatcherName
            let defaultOptOverlayName = Map.find intrinsicOverlayName world.State.OverlayRouter

            // make the bare entity with name as id
            let entity = Entity.make dispatcher defaultOptOverlayName None

            // attach the entity's intrinsic facets and their fields
            let entity = World.attachIntrinsicFacetsViaNames entity world

            // read the entity's overlay and apply it to its facet names if applicable
            Reflection.tryReadOptOverlayNameToTarget entityNode entity
            match (defaultOptOverlayName, entity.OptOverlayName) with
            | (Some defaultOverlayName, Some overlayName) ->
                let overlayer = world.State.Overlayer
                Overlayer.applyOverlayToFacetNames defaultOverlayName overlayName entity overlayer overlayer
            | (_, _) -> ()

            // read the entity's facet names
            Reflection.readFacetNamesToTarget entityNode entity
            
            // synchronize the entity's facets (and attach their fields)
            let entity =
                match World.trySynchronizeFacets [] entity None world with
                | Right (entity, _) -> entity
                | Left error -> debug error; entity

            // attach the entity's dispatcher fields
            Reflection.attachFields dispatcher entity

            // attempt to apply the entity's overlay
            match entity.OptOverlayName with
            | Some overlayName ->

                // OPTIMIZATION: applying overlay only when it will change something (EG - when it's not the default overlay)
                if intrinsicOverlayName <> overlayName then
                    let facetNames = Entity.getFacetNamesReflectively entity
                    Overlayer.applyOverlay intrinsicOverlayName overlayName facetNames entity world.State.Overlayer
                else ()
            | None -> ()

            // read the entity's properties
            Reflection.readPropertiesToTarget entityNode entity

            // return the initialized entity
            entity

        /// Read multiple entities from an xml node.
        static member readEntities (parentNode : XmlNode) defaultDispatcherName world =
            match parentNode.SelectSingleNode EntitiesNodeName with
            | null -> Map.empty
            | entitiesNode ->
                let entityNodes = entitiesNode.SelectNodes EntityNodeName
                Seq.fold
                    (fun entities entityNode ->
                        let entity = World.readEntity entityNode defaultDispatcherName world
                        Map.add entity.Name entity entities)
                    Map.empty
                    (enumerable entityNodes)