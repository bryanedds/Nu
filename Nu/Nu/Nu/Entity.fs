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

        static member setPosition value (entity : Entity) = { entity with Position = value }
        static member setDepth value (entity : Entity) = { entity with Depth = value }
        static member setSize value (entity : Entity) = { entity with Size = value }
        static member setRotation value (entity : Entity) = { entity with Rotation = value }
        static member setVisible value (entity : Entity) = { entity with Visible = value }
        static member setViewType value (entity : Entity) = { entity with ViewType = value }
        static member setPublishChanges value (entity : Entity) = { entity with PublishChanges = value }
        static member setPersistent value (entity : Entity) = { entity with Persistent = value }

        static member register address (entity : Entity) world =
            let (entity, world) = entity.DispatcherNp.Register (address, entity, world)
            List.fold
                (fun (entity, world) (facet : Facet) -> facet.Register (address, entity, world))
                (entity, world)
                entity.FacetsNp
        
        static member unregister address (entity : Entity) world =
            let (entity, world) = entity.DispatcherNp.Unregister (address, entity, world)
            List.fold
                (fun (entity, world) (facet : Facet) -> facet.Unregister (address, entity, world))
                (entity, world)
                entity.FacetsNp
        
        static member propagatePhysics address (entity : Entity) world =
            let world = entity.DispatcherNp.PropagatePhysics (address, entity, world)
            List.fold
                (fun world (facet : Facet) -> facet.PropagatePhysics (address, entity, world))
                world
                entity.FacetsNp
        
        static member getRenderDescriptors (entity : Entity) world =
            let renderDescriptors = entity.DispatcherNp.GetRenderDescriptors (entity, world)
            List.fold
                (fun renderDescriptors (facet : Facet) ->
                    let descriptors = facet.GetRenderDescriptors (entity, world)
                    descriptors @ renderDescriptors)
                renderDescriptors
                entity.FacetsNp
        
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
        
        static member getPickingPriority (entity : Entity) world =
            entity.DispatcherNp.GetPickingPriority (entity, world)

        static member getFacetNames entity =
            List.map Reflection.getTypeName entity.FacetsNp

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

        static member dispatchesAs (dispatcherTargetType : Type) (entity : Entity) =
            Reflection.dispatchesAs dispatcherTargetType entity.DispatcherNp

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
              PublishChanges = false
              Persistent = true
              CreationTimeNp = DateTime.UtcNow
              DispatcherNp = dispatcher
              FacetNames = []
              FacetsNp = []
              OptOverlayName = optOverlayName
              Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true } }

    type [<StructuralEquality; NoComparison>] TileMapData =
        { Map : TmxMap
          MapSize : Vector2i
          TileSize : Vector2i
          TileSizeF : Vector2
          TileMapSize : Vector2i
          TileMapSizeF : Vector2
          TileSet : TmxTileset
          TileSetSize : Vector2i }

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
                let optGroupMap = Map.tryFind screenName world.Entities
                match optGroupMap with
                | Some groupMap ->
                    let optEntityMap = Map.tryFind groupName groupMap
                    match optEntityMap with
                    | Some entityMap -> Map.tryFind entityName entityMap
                    | None -> None
                | None -> None
            | _ -> failwith <| "Invalid entity address '" + acstring address + "'."

        static member private entityAdder (address : Entity Address) (entity : Entity) world =
            match address.Names with
            | [screenName; groupName; entityName] ->
                match Map.tryFind screenName world.Entities with
                | Some groupMap ->
                    match Map.tryFind groupName groupMap with
                    | Some entityMap ->
                        let entityMap = Map.add entityName entity entityMap
                        let groupMap = Map.add groupName entityMap groupMap
                        { world with Entities = Map.add screenName groupMap world.Entities }
                    | None ->
                        let entityMap = Map.singleton entityName entity
                        let groupMap = Map.add groupName entityMap groupMap
                        { world with Entities = Map.add screenName groupMap world.Entities }
                | None ->
                    let entityMap = Map.singleton entityName entity
                    let groupMap = Map.singleton groupName entityMap
                    { world with Entities = Map.add screenName groupMap world.Entities }
            | _ -> failwith <| "Invalid entity address '" + acstring address + "'."

        static member private entityRemover (address : Entity Address)  world =
            match address.Names with
            | [screenName; groupName; entityName] ->
                let optGroupMap = Map.tryFind screenName world.Entities
                match optGroupMap with
                | Some groupMap ->
                    let optEntityMap = Map.tryFind groupName groupMap
                    match optEntityMap with
                    | Some entityMap ->
                        let entityMap = Map.remove entityName entityMap
                        let groupMap = Map.add groupName entityMap groupMap
                        { world with Entities = Map.add screenName groupMap world.Entities }
                    | None -> world
                | None -> world
            | _ -> failwith <| "Invalid entity address '" + acstring address + "'."

        static member getEntityBy by address world = by ^^ Option.get ^^ World.optEntityFinder address world
        static member getEntity address world = World.getEntityBy id address world

        static member private setEntityWithoutEvent address entity world = World.entityAdder address entity world
        static member setEntity address entity world =
            let oldEntity = Option.get <| World.optEntityFinder address world
            let world = World.entityAdder address entity world
            if entity.PublishChanges
            then World.publish4 { OldEntity = oldEntity } (EntityChangeEventAddress ->>- address) address world
            else world

        static member updateEntityW address updater world =
            let entity = World.getEntity address world
            let entity = updater entity world
            World.setEntity address entity world
        static member updateEntity address updater world =
            World.updateEntityW address (fun entity _ -> updater entity) world

        static member getOptEntity address world = World.optEntityFinder address world
        static member containsEntity address world = Option.isSome <| World.getOptEntity address world
        static member private setOptEntityWithoutEvent address optEntity world =
            match optEntity with 
            | Some entity -> World.entityAdder address entity world
            | None -> World.entityRemover address world

        static member getEntityMap (groupAddress : Group Address) world =
            match groupAddress.Names with
            | [screenName; groupName] ->
                match Map.tryFind screenName world.Entities with
                | Some groupMap ->
                    match Map.tryFind groupName groupMap with
                    | Some entityMap -> entityMap
                    | None -> Map.empty
                | None -> Map.empty
            | _ -> failwith <| "Invalid group address '" + acstring groupAddress + "'."

        static member getEntities groupAddress world =
            let entityMap = World.getEntityMap groupAddress world
            Map.toValueSeq entityMap

        static member getEntityMap3 entityNames (groupAddress : Group Address) world =
            let entityNames = Set.ofSeq entityNames
            let entityMap = World.getEntityMap groupAddress world
            Map.filter (fun entityName _ -> Set.contains entityName entityNames) entityMap

        static member getEntities3 entityNames groupAddress world =
            let entityMap = World.getEntityMap3 groupAddress entityNames world
            Map.toValueSeq entityMap

        static member setEntities groupAddress entities world =
            Seq.fold
                (fun world (entity : Entity) -> World.setEntity (gatoea groupAddress entity.Name) entity world)
                world
                entities

        static member private registerEntity address entity world =
            Entity.register address entity world

        static member private unregisterEntity address entity world =
            Entity.unregister address entity world

        static member removeEntityImmediate (address : Entity Address) entity world =
            let world = World.publish4 () (RemovingEventAddress ->>- address) address world
            let (entity, world) = World.unregisterEntity address entity world
            let world = World.setOptEntityWithoutEvent address None world
            (entity, world)

        static member removeEntity address (entity : Entity) world =
            let task =
                { ScheduledTime = world.State.TickTime
                  Operation = fun world ->
                    match World.getOptEntity address world with
                    | Some entity -> snd <| World.removeEntityImmediate address entity world
                    | None -> world }
            let world = World.addTask task world
            (entity, world)

        static member removeEntitiesImmediate (groupAddress : Group Address) entities world =
            World.transformSimulants World.removeEntityImmediate gatoea groupAddress entities world

        static member removeEntities (groupAddress : Group Address) entities world =
            World.transformSimulants World.removeEntity gatoea groupAddress entities world

        static member addEntity address entity world =
            if not <| World.containsEntity address world then
                let (entity, world) =
                    match World.getOptEntity address world with
                    | Some _ -> World.removeEntityImmediate address entity world
                    | None -> (entity, world)
                let world = World.setEntityWithoutEvent address entity world
                let (entity, world) = World.registerEntity address entity world
                let world = World.publish4 () (AddEventAddress ->>- address) address world
                (entity, world)
            else failwith <| "Adding an entity that the world already contains at address '" + acstring address + "'."

        static member addEntities (groupAddress : Group Address) entities world =
            World.transformSimulants World.addEntity gatoea groupAddress entities world

        static member makeEntity dispatcherName optName world =
            
            // find the entity's dispatcher
            let dispatcher = Map.find dispatcherName world.Components.EntityDispatchers
            
            // compute the opt overlay name
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
                    let overlayer = world.Subsystems.Overlayer
                    Overlayer.applyOverlayToFacetNames intrinsicOverlayName defaultOverlayName entity overlayer overlayer
                        
                    // synchronize the entity's facets (and attach their fields)
                    match World.trySynchronizeFacets [] None entity world with
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
                    let facetNames = Entity.getFacetNames entity
                    Overlayer.applyOverlay intrinsicOverlayName overlayName facetNames entity world.Subsystems.Overlayer
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

        static member private getEntityFieldDefinitions (entity : Entity) =
            let containers = objectify entity.DispatcherNp :: List.map objectify entity.FacetsNp
            let containerTypes = List.map getType containers
            Map.ofListBy (fun (aType : Type) -> (aType.Name, Reflection.getFieldDefinitions aType)) containerTypes

        static member private getEntityFieldDefinitionNamesToDetach entity facetToRemove =

            // get the field definition name counts of the current, complete entity
            let fieldDefinitions = World.getEntityFieldDefinitions entity
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

        static member tryRemoveFacet syncing facetName optAddress entity world =
            match List.tryFind (fun facet -> Reflection.getTypeName facet = facetName) entity.FacetsNp with
            | Some facet ->
                let (entity, world) =
                    match optAddress with
                    | Some address -> facet.Unregister (address, entity, world)
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
                    | Some address -> World.setEntity address entity world
                    | None -> world
                Right (entity, world)
            | None -> Left <| "Failure to remove facet '" + facetName + "' from entity."

        static member tryAddFacet syncing facetName optAddress (entity : Entity) world =
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
                        let (entity, world) = facet.Register (address, entity, world)
                        let world = World.setEntity address entity world
                        Right (entity, world)
                    | None -> Right (entity, world)
                else Left <| "Facet '" + Reflection.getTypeName facet + "' is incompatible with entity '" + entity.Name + "'."
            | Left error -> Left error

        static member tryRemoveFacets syncing facetNamesToRemove optAddress entity world =
            List.fold
                (fun eitherEntityWorld facetName ->
                    match eitherEntityWorld with
                    | Right (entity, world) -> World.tryRemoveFacet syncing facetName optAddress entity world
                    | Left _ as left -> left)
                (Right (entity, world))
                facetNamesToRemove

        static member tryAddFacets syncing facetNamesToAdd optAddress entity world =
            List.fold
                (fun eitherEntityWorld facetName ->
                    match eitherEntityWorld with
                    | Right (entity, world) -> World.tryAddFacet syncing facetName optAddress entity world
                    | Left _ as left -> left)
                (Right (entity, world))
                facetNamesToAdd

        static member trySetFacetNames oldFacetNames newFacetNames optAddress entity world =
            let facetNamesToRemove = World.getFacetNamesToRemove oldFacetNames newFacetNames
            let facetNamesToAdd = World.getFacetNamesToAdd oldFacetNames newFacetNames
            match World.tryRemoveFacets false facetNamesToRemove optAddress entity world with
            | Right (entity, world) -> World.tryAddFacets false facetNamesToAdd optAddress entity world
            | Left _ as left -> left

        static member trySynchronizeFacets oldFacetNames optAddress entity world =
            let facetNamesToRemove = World.getFacetNamesToRemove oldFacetNames entity.FacetNames
            let facetNamesToAdd = World.getFacetNamesToAdd oldFacetNames entity.FacetNames
            match World.tryRemoveFacets true facetNamesToRemove optAddress entity world with
            | Right (entity, world) -> World.tryAddFacets true facetNamesToAdd optAddress entity world
            | Left _ as left -> left

        static member private attachIntrinsicFacetsViaNames (entity : Entity) world =
            let components = world.Components
            let entity = { entity with Id = entity.Id } // hacky copy
            Reflection.attachIntrinsicFacets components.EntityDispatchers components.Facets entity.DispatcherNp entity
            entity
        
        static member internal handleBodyTransformMessage (message : BodyTransformMessage) address (entity : Entity) world =
            // OPTIMIZATION: entity is not changed (avoiding a change entity event) if position and rotation haven't changed.
            if entity.Position <> message.Position || entity.Rotation <> message.Rotation then
                let entity =
                    entity |>
                        // TODO: see if the following center-offsetting can be encapsulated within the Physics module!
                        Entity.setPosition (message.Position - entity.Size * 0.5f) |>
                        Entity.setRotation message.Rotation
                let world = World.setEntity address entity world
                (entity, world)
            else (entity, world)

        static member writeEntity (writer : XmlWriter) (entity : Entity) world =
            writer.WriteAttributeString (DispatcherNameAttributeName, (entity.DispatcherNp.GetType ()).Name)
            let shouldWriteProperty = fun propertyName propertyType (propertyValue : obj) ->
                if propertyName = "OptOverlayName" && propertyType = typeof<string option> then
                    let defaultOptOverlayName = Map.find (Reflection.getTypeName entity.DispatcherNp) world.State.OverlayRouter
                    defaultOptOverlayName <> (propertyValue :?> string option)
                else
                    let facetNames = Entity.getFacetNames entity
                    Overlayer.shouldPropertySerialize5 facetNames propertyName propertyType entity world.Subsystems.Overlayer
            Serialization.writePropertiesFromTarget shouldWriteProperty writer entity

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

        static member readEntity (entityNode : XmlNode) defaultDispatcherName world =

            // read in the dispatcher name and create the dispatcher
            let dispatcherName = Serialization.readDispatcherName defaultDispatcherName entityNode
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
            Serialization.tryReadOptOverlayNameToTarget entityNode entity
            match (defaultOptOverlayName, entity.OptOverlayName) with
            | (Some defaultOverlayName, Some overlayName) ->
                let overlayer = world.Subsystems.Overlayer
                Overlayer.applyOverlayToFacetNames defaultOverlayName overlayName entity overlayer overlayer
            | (_, _) -> ()

            // read the entity's facet names
            Serialization.readFacetNamesToTarget entityNode entity
            
            // synchronize the entity's facets (and attach their fields)
            let entity =
                match World.trySynchronizeFacets [] None entity world with
                | Right (entity, _) -> entity
                | Left error -> debug error; entity

            // attach the entity's dispatcher fields
            Reflection.attachFields dispatcher entity

            // attempt to apply the entity's overlay
            match entity.OptOverlayName with
            | Some overlayName ->

                // OPTIMIZATION: applying overlay only when it will change something (EG - when it's not the default overlay)
                if intrinsicOverlayName <> overlayName then
                    let facetNames = Entity.getFacetNames entity
                    Overlayer.applyOverlay intrinsicOverlayName overlayName facetNames entity world.Subsystems.Overlayer
                else ()
            | None -> ()

            // read the entity's properties
            Serialization.readPropertiesToTarget entityNode entity

            // return the initialized entity
            entity

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