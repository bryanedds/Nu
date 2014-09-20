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
open Nu.NuConstants

[<AutoOpen>]
module EntityModule =

    type Entity with

        static member setPosition position (entity : Entity) = { entity with Position = position }
        static member setDepth depth (entity : Entity) = { entity with Depth = depth }
        static member setSize size (entity : Entity) = { entity with Size = size }
        static member setRotation rotation (entity : Entity) = { entity with Rotation = rotation }
        static member setVisible visible (entity : Entity) = { entity with Visible = visible }
        static member setViewType viewType (entity : Entity) = { entity with ViewType = viewType }

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
        
        static member handleBodyTransformMessage message address (entity : Entity) world =
            let (entity, world) = entity.DispatcherNp.HandleBodyTransformMessage (message, address, entity, world)
            List.fold
                (fun (entity, world) (facet : Facet) -> facet.HandleBodyTransformMessage (message, address, entity, world))
                (entity, world)
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
        
        static member isFacetCompatible facet (entity : Entity) =
            let facetType = facet.GetType ()
            let facetFieldNames = Reflection.getFieldDefinitionNames facetType
            let entityFieldNames = Map.toKeyList entity.Xtension.XFields
            let intersection = List.intersect facetFieldNames entityFieldNames
            Set.isEmpty intersection

        static member make dispatcherName dispatcher optName =
            let id = NuCore.makeId ()
            { Id = id
              Name = match optName with None -> string id | Some name -> name
              Position = Vector2.Zero
              Depth = 0.0f
              Size = DefaultEntitySize
              Rotation = 0.0f
              Visible = true
              ViewType = Relative
              DispatcherNp = dispatcher
              FacetNames = []
              FacetsNp = []
              OptOverlayName = Some dispatcherName
              Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true } }

    type [<StructuralEquality; NoComparison>] TileMapData =
        { Map : TmxMap
          MapSize : Vector2I
          TileSize : Vector2I
          TileSizeF : Vector2
          TileMapSize : Vector2I
          TileMapSizeF : Vector2
          TileSet : TmxTileset
          TileSetSize : Vector2I }

    type [<StructuralEquality; NoComparison>] TileData =
        { Tile : TmxLayerTile
          I : int
          J : int
          Gid : int
          GidPosition : int
          Gid2 : Vector2I
          OptTileSetTile : TmxTilesetTile option
          TilePosition : Vector2I }

[<AutoOpen>]
module WorldEntityModule =

    type World with

        static member private optEntityFinder address world =
            match address.AddrList with
            | [screenName; groupName; entityName] ->
                let optGroupMap = Map.tryFind screenName world.Entities
                match optGroupMap with
                | Some groupMap ->
                    let optEntityMap = Map.tryFind groupName groupMap
                    match optEntityMap with
                    | Some entityMap -> Map.tryFind entityName entityMap
                    | None -> None
                | None -> None
            | _ -> failwith <| "Invalid entity address '" + string address + "'."

        static member private entityAdder address world (entity : Entity) =
            match address.AddrList with
            | [screenName; groupName; entityName] ->
                let optGroupMap = Map.tryFind screenName world.Entities
                match optGroupMap with
                | Some groupMap ->
                    let optEntityMap = Map.tryFind groupName groupMap
                    match optEntityMap with
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
            | _ -> failwith <| "Invalid entity address '" + string address + "'."

        static member private entityRemover address world =
            match address.AddrList with
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
            | _ -> failwith <| "Invalid entity address '" + string address + "'."

        static member getEntity address world = Option.get <| World.optEntityFinder address world
        static member private setEntityWithoutEvent address entity world = World.entityAdder address world entity
        static member setEntity address entity world = 
                let oldEntity = Option.get <| World.optEntityFinder address world
                let world = World.entityAdder address world entity
                World.publish4 (ChangeEventName + address) address (EntityChangeData { OldEntity = oldEntity }) world

        static member getOptEntity address world = World.optEntityFinder address world
        static member containsEntity address world = Option.isSome <| World.getOptEntity address world
        static member private setOptEntityWithoutEvent address optEntity world =
            match optEntity with 
            | Some entity -> World.entityAdder address world entity
            | None -> World.entityRemover address world

        static member getEntities1 world =
            seq {
                for screenKvp in world.Entities do
                    for groupKvp in screenKvp.Value do
                        for entityKvp in groupKvp.Value do
                            let address = Address.make [screenKvp.Key; groupKvp.Key; entityKvp.Key]
                            yield (address, entityKvp.Value) }
    
        static member getEntities groupAddress world =
            match groupAddress.AddrList with
            | [screenName; groupName] ->
                match Map.tryFind screenName world.Entities with
                | Some groupMap ->
                    match Map.tryFind groupName groupMap with
                    | Some entityMap -> entityMap
                    | None -> Map.empty
                | None -> Map.empty
            | _ -> failwith <| "Invalid group address '" + string groupAddress + "'."

        static member getEntities3 groupAddress entityNames world =
            let entityNames = Set.ofSeq entityNames
            let entitys = World.getEntities groupAddress world
            Map.filter (fun entityName _ -> Set.contains entityName entityNames) entitys

        static member registerEntity address entity world =
            Entity.register address entity world

        static member unregisterEntity address entity world =
            Entity.unregister address entity world

        static member removeEntityImmediate address entity world =
            let world = World.publish4 (RemovingEventName + address) address (NoData ()) world
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

        static member removeEntitiesImmediate groupAddress entities world =
            World.transformSimulants World.removeEntityImmediate groupAddress entities world

        static member removeEntities groupAddress entities world =
            World.transformSimulants World.removeEntity groupAddress entities world

        static member addEntity address entity world =
            let (entity, world) =
                match World.getOptEntity address world with
                | Some _ -> World.removeEntityImmediate address entity world
                | None -> (entity, world)
            let world = World.setEntityWithoutEvent address entity world
            let (entity, world) = World.registerEntity address entity world
            let world = World.publish4 (AddEventName + address) address (NoData ()) world
            (entity, world)

        static member addEntities groupAddress entities world =
            World.transformSimulants World.addEntity groupAddress entities world

        static member tryGetFacet facetName world =
            match Map.tryFind facetName world.Components.Facets with
            | Some facet -> Right <| facet
            | None -> Left <| "Invalid facet name '" + facetName + "'."

        static member getFacetNamesToAdd oldFacetNames newFacetNames =
            let newFacetNames = Set.ofList newFacetNames
            let oldFacetNames = Set.ofList oldFacetNames
            let facetNamesToAdd = Set.difference newFacetNames oldFacetNames
            List.ofSeq facetNamesToAdd

        static member getFacetNamesToRemove oldFacetNames newFacetNames =
            let newFacetNames = Set.ofList newFacetNames
            let oldFacetNames = Set.ofList oldFacetNames
            let facetNamesToRemove = Set.difference oldFacetNames newFacetNames
            List.ofSeq facetNamesToRemove

        static member tryRemoveFacet syncing facetName optAddress entity world =
            match List.tryFind (fun facet -> Reflection.getTypeName facet = facetName) entity.FacetsNp with
            | Some facet ->
                let (entity, world) =
                    match optAddress with
                    | Some address -> facet.Unregister (address, entity, world)
                    | None -> (entity, world)
                let entity = { entity with Id = entity.Id } // hacky copy
                Reflection.detachFields facet entity
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
                if Entity.isFacetCompatible facet entity then
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
                else Left <| "Cannot add incompatible facet '" + Reflection.getTypeName facet + "'."
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

        static member attachIntrinsicFacetsViaNames (entity : Entity) world =
            let entity = { entity with Id = entity.Id } // hacky copy
            Reflection.attachIntrinsicFacets entity.DispatcherNp entity world.Components.Facets
            entity

        static member writeEntity overlayer (writer : XmlWriter) (entity : Entity) =
            writer.WriteStartElement typeof<Entity>.Name
            writer.WriteAttributeString (DispatcherNameAttributeName, (entity.DispatcherNp.GetType ()).Name)
            Serialization.writePropertiesFromTarget 
                (fun propertyName -> Overlayer.shouldPropertySerialize3 propertyName entity overlayer)
                writer
                entity
            writer.WriteEndElement ()

        static member writeEntities overlayer (writer : XmlWriter) (entities : Map<_, _>) =
            writer.WriteStartElement EntitiesNodeName
            for entityKvp in entities do
                World.writeEntity overlayer writer entityKvp.Value
            writer.WriteEndElement ()

        static member readEntity (entityNode : XmlNode) defaultDispatcherName world =

            // read in the dispatcher name and create the dispatcher
            let dispatcherName = Serialization.readDispatcherName defaultDispatcherName entityNode
            let (dispatcherName, dispatcher) =
                match Map.tryFind dispatcherName world.Components.Dispatchers with
                | Some dispatcher -> (dispatcherName, dispatcher :?> EntityDispatcher)
                | None ->
                    note <| "Could not locate dispatcher '" + dispatcherName + "'."
                    let dispatcherName = typeof<EntityDispatcher>.Name
                    let dispatcher = Map.find dispatcherName world.Components.Dispatchers
                    (dispatcherName, dispatcher :?> EntityDispatcher)

            // make the bare entity with name as id
            let entity = Entity.make dispatcherName dispatcher None

            // attach the entity's intrinsic facets
            let entity = World.attachIntrinsicFacetsViaNames entity world

            // read the entity's overlay and apply it to its facet names
            Serialization.readOptOverlayNameToTarget entityNode entity
            match entity.OptOverlayName with
            | Some overlayName ->
                let defaultOptDispatcherName = Some typeof<EntityDispatcher>.Name
                let overlayer = world.Components.Overlayer
                Overlayer.applyOverlayToFacetNames defaultOptDispatcherName overlayName entity overlayer overlayer
            | None -> ()

            // read the entity's facet names, and synchronize its facets 
            Serialization.readFacetNamesToTarget entityNode entity
            let entity =
                match World.trySynchronizeFacets [] None entity world with
                | Right (entity, _) -> entity
                | Left error -> debug error; entity

            // attach the entity's instrinsic fields from its dispatcher if any
            Reflection.attachFields dispatcher entity

            // apply the entity's overlay
            match entity.OptOverlayName with
            | Some overlayName -> Overlayer.applyOverlay None overlayName entity world.Components.Overlayer
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

        static member makeEntity dispatcherName optName world =
            let dispatcher = Map.find dispatcherName world.Components.Dispatchers :?> EntityDispatcher
            let entity = Entity.make dispatcherName dispatcher optName
            let entity = World.attachIntrinsicFacetsViaNames entity world
            Reflection.attachFields dispatcher entity
            match World.trySynchronizeFacets [] None entity world with
            | Right (entity, _) -> entity
            | Left error -> debug error; entity