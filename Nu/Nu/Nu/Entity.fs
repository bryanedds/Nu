namespace Nu
open System
open System.Collections.Generic
open System.ComponentModel
open System.Reflection
open System.Xml
open System.Xml.Serialization
open FSharpx
open OpenTK
open TiledSharp
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants

[<AutoOpen>]
module EntityModule =

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
                    | Some address ->
                        let entityRef = { EntityAddress = address }
                        let world = facet.Unregister (entityRef, world)
                        (World.getEntity address world, world)
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
                        let entityRep = { EntityAddress = address }
                        let world = facet.Register (entityRep, world)
                        let entity = World.getEntity address world
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
        static member readEntities (groupNode : XmlNode) defaultDispatcherName world =
            match groupNode.SelectSingleNode EntitiesNodeName with
            | null -> Map.empty
            | entitiesNode ->
                let entityNodes = entitiesNode.SelectNodes EntityNodeName
                Seq.fold
                    (fun entities entityNode ->
                        let entity = World.readEntity entityNode defaultDispatcherName world
                        Map.add entity.Name entity entities)
                    Map.empty
                    (enumerable entityNodes)