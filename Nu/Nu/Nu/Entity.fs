namespace Nu
open System
open System.Collections.Generic
open System.ComponentModel
open System.Reflection
open System.Xml
open System.Xml.Serialization
open FSharpx
open FSharpx.Lens.Operators
open OpenTK
open TiledSharp
open Prime
open Nu
open Nu.NuCore
open Nu.NuConstants
open Nu.NuMath
open Nu.Physics
open Nu.Audio
open Nu.Rendering
open Nu.Metadata
open Nu.Camera
open Nu.Sim

[<AutoOpen>]
module EntityModule =

    type EntityDispatcher () =

        abstract member Init : Entity * IXDispatcherContainer -> Entity
        default this.Init (entity, dispatcherContainer) = entity

        abstract member Register : Address * Entity * World -> Entity * World
        default this.Register (address, entity, world) = (entity, world)

        abstract member Unregister : Address * Entity * World -> World
        default this.Unregister (address, entity, world) = world

        abstract member PropagatePhysics : Address * Entity * World -> World
        default this.PropagatePhysics (address, entity, world) = world

        abstract member ReregisterPhysicsHack : Address * Entity * World -> World
        default this.ReregisterPhysicsHack (groupAddress, entity, world) = world

        abstract member HandleBodyTransformMessage : BodyTransformMessage * Address * Entity * World -> World
        default this.HandleBodyTransformMessage (message, address, entity, world) = world

        abstract member GetRenderDescriptors : Matrix3 * Matrix3 * Entity * World -> RenderDescriptor list
        default this.GetRenderDescriptors (viewAbsolute, viewRelative, entity, world) = []

        abstract member GetQuickSize : Entity * World -> Vector2
        default this.GetQuickSize (entity, world) = DefaultEntitySize

        abstract member IsTransformRelative : Entity * World -> bool
        default this.IsTransformRelative (entity, world) = true

    type Entity2dDispatcher () =
        inherit EntityDispatcher ()
            
        override this.Init (entity2d, dispatcherContainer) =
            let entity2d' = base.Init (entity2d, dispatcherContainer)
            // perhaps a nice 'with' syntax macro would work better here -
            // http://fslang.uservoice.com/forums/245727-f-language/suggestions/5674940-implement-syntactic-macros
            entity2d'
                .SetPosition(Vector2.Zero)
                .SetDepth(0.0f)
                .SetSize(DefaultEntitySize)
                .SetRotation(0.0f)

module Entity =

    let entityIdLens =
        { Get = fun entity -> entity.Id
          Set = fun value entity -> { entity with Id = value }}

    let entityNameLens =
        { Get = fun entity -> entity.Name
          Set = fun value entity -> { entity with Name = value }}

    let entityEnabledLens =
        { Get = fun entity -> entity.Enabled
          Set = fun value entity -> { entity with Enabled = value }}

    let entityVisibleLens =
        { Get = fun entity -> entity.Visible
          Set = fun value entity -> { entity with Visible = value }}

    let entityXtensionLens =
        { Get = fun entity -> entity.Xtension
          Set = fun value entity -> { entity with Xtension = value }}

    let entityDynamicLens memberName =
        { Get = fun entity -> (?) (entity : Entity) memberName
          Set = fun value entity -> (?<-) entity memberName value }

    let entityPositionLens =
        { Get = fun (entity : Entity) -> entity.Position
          Set = fun value entity -> entity.SetPosition value }

    let entityDepthLens =
        { Get = fun (entity : Entity) -> entity.Depth
          Set = fun value entity -> entity.SetDepth value }

    let entityRotationLens =
        { Get = fun (entity : Entity) -> entity.Rotation
          Set = fun value entity -> entity.SetRotation value }

    let entitySizeLens =
        { Get = fun (entity : Entity) -> entity.Size
          Set = fun value entity -> entity.SetSize value }

    let private worldOptEntityFinder address world =
        let optGroupMap = Map.tryFind (List.at 0 address) world.Entities
        match optGroupMap with
        | None -> None
        | Some groupMap ->
            let optEntityMap = Map.tryFind (List.at 1 address) groupMap
            match optEntityMap with
            | None -> None
            | Some entityMap -> Map.tryFind (List.at 2 address) entityMap

    let private worldEntityAdder address world (child : Entity) =
        let optGroupMap = Map.tryFind (List.at 0 address) world.Entities
        match optGroupMap with
        | None ->
            let entityMap = Map.singleton (List.at 2 address) child
            let groupMap = Map.singleton (List.at 1 address) entityMap
            { world with Entities = Map.add (List.at 0 address) groupMap world.Entities }
        | Some groupMap ->
            let optEntityMap = Map.tryFind (List.at 1 address) groupMap
            match optEntityMap with
            | None ->
                let entityMap = Map.singleton (List.at 2 address) child
                let groupMap' = Map.add (List.at 1 address) entityMap groupMap
                { world with Entities = Map.add (List.at 0 address) groupMap' world.Entities }
            | Some entityMap ->
                let entityMap' = Map.add (List.at 2 address) child entityMap
                let groupMap' = Map.add (List.at 1 address) entityMap' groupMap
                { world with Entities = Map.add (List.at 0 address) groupMap' world.Entities }

    let private worldEntityRemover address world =
        let optGroupMap = Map.tryFind (List.at 0 address) world.Entities
        match optGroupMap with
        | None -> world
        | Some groupMap ->
            let optEntityMap = Map.tryFind (List.at 1 address) groupMap
            match optEntityMap with
            | None -> world
            | Some entityMap ->
                let entityMap' = Map.remove (List.at 2 address) entityMap
                let groupMap' = Map.add (List.at 1 address) entityMap' groupMap
                { world with Entities = Map.add (List.at 0 address) groupMap' world.Entities }

    let private getWorldEntityWithLens address world lens =
        get (getChild worldOptEntityFinder address world) lens

    let private setWorldEntityWithLens child address world lens =
        let entity = getChild worldOptEntityFinder address world
        let entity' = set child entity lens
        setChild worldEntityAdder worldEntityRemover address world entity'

    let worldEntityLens address =
        { Get = fun world -> Option.get <| worldOptEntityFinder address world
          Set = fun entity world -> worldEntityAdder address world entity }

    let worldOptEntityLens address =
        { Get = fun world -> worldOptEntityFinder address world
          Set = fun optEntity world -> match optEntity with None -> worldEntityRemover address world | Some entity -> worldEntityAdder address world entity }

    let worldEntitiesLens address =
        { Get = fun world ->
            match address with
            | [screenLun; groupLun] ->
                match Map.tryFind screenLun world.Entities with
                | None -> Map.empty
                | Some groupMap ->
                    match Map.tryFind groupLun groupMap with
                    | None -> Map.empty
                    | Some entityMap -> entityMap
            | _ -> failwith <| "Invalid entity address '" + addrToStr address + "'."
          Set = fun entities world ->
            match address with
            | [screenLun; groupLun] ->
                match Map.tryFind screenLun world.Entities with
                | None -> { world with Entities = Map.add screenLun (Map.singleton groupLun entities) world.Entities }
                | Some groupMap ->
                    match Map.tryFind groupLun groupMap with
                    | None -> { world with Entities = Map.add screenLun (Map.add groupLun entities groupMap) world.Entities }
                    | Some entityMap -> { world with Entities = Map.add screenLun (Map.add groupLun (Map.addMany (Map.toSeq entities) entityMap) groupMap) world.Entities }
            | _ -> failwith <| "Invalid entity address '" + addrToStr address + "'." }

    let mouseToScreen (position : Vector2) camera =
        let positionScreen =
            Vector2 (
                position.X - camera.EyeSize.X * 0.5f,
                -(position.Y - camera.EyeSize.Y * 0.5f)) // negation for right-handedness
        positionScreen

    let mouseToEntity (position : Vector2) world (entity : Entity) =
        let positionScreen = mouseToScreen position world.Camera
        let view = (if entity.IsTransformRelative world then Camera.getViewRelativeF else Camera.getViewAbsoluteF) world.Camera
        let positionEntity = positionScreen * view
        positionEntity

    // TODO: turn into a lens
    let getEntityPosition (entity : Entity) =
        entity.Position

    // TODO: turn into a lens
    let setEntityPosition snap position (entity : Entity) =
        let position' = snap2F snap position
        entity.SetPosition position'

    // TODO: turn into a lens
    let getEntityTransform (entity : Entity) =
        { Transform.Position = entity.Position
          Depth = entity.Depth
          Size = entity.Size
          Rotation = entity.Rotation }

    // TODO: turn into a lens
    let setEntityTransform positionSnap rotationSnap transform (entity : Entity) =
        let transform' = snapTransform positionSnap rotationSnap transform
        entity
            .SetPosition(transform'.Position)
            .SetDepth(transform'.Depth)
            .SetSize(transform'.Size)
            .SetRotation(transform'.Rotation)

    let sortFstAsc (priority, _) (priority2, _) =
        if priority = priority2 then 0
        elif priority > priority2 then -1
        else 1

    let getPickingPriority (entity : Entity) =
        entity.Depth

    let makeTileMapData tileMapAsset world =
        match tryGetTileMapMetadata tileMapAsset.TileMapAssetName tileMapAsset.PackageName world.AssetMetadataMap with
        | None -> failwith "Unexpected match failure in Nu.Entity.makeTileMapData."
        | Some (_, _, map) ->
            let mapSize = (map.Width, map.Height)
            let tileSize = (map.TileWidth, map.TileHeight)
            let tileSizeF = Vector2 (single <| fst tileSize, single <| snd tileSize)
            let tileMapSize = (fst mapSize * fst tileSize, fst mapSize * fst tileSize)
            let tileMapSizeF = Vector2 (single <| fst tileMapSize, single <| snd tileMapSize)
            let tileSet = map.Tilesets.[0] // MAGIC_VALUE: I'm not sure how to properly specify this
            let optTileSetWidth = tileSet.Image.Width
            let optTileSetHeight = tileSet.Image.Height
            let tileSetSize = (optTileSetWidth.Value / fst tileSize, optTileSetHeight.Value / snd tileSize)
            { Map = map; MapSize = mapSize; TileSize = tileSize; TileSizeF = tileSizeF; TileMapSize = tileMapSize; TileMapSizeF = tileMapSizeF; TileSet = tileSet; TileSetSize = tileSetSize }

    let makeTileLayerData tileMap tmd (layerIndex : int) =
        let layer = tmd.Map.Layers.[layerIndex]
        let tiles = layer.Tiles
        { Layer = layer; Tiles = tiles }

    let makeTileData (tileMap : Entity) tmd tld n =
        let (i, j) = (n % fst tmd.MapSize, n / snd tmd.MapSize)
        let tile = tld.Tiles.[n]
        let gid = tile.Gid - tmd.TileSet.FirstGid
        let gidPosition = gid * fst tmd.TileSize
        let gid2 = (gid % fst tmd.TileSetSize, gid / snd tmd.TileSetSize)
        let tilePosition = (
            int tileMap.Position.X + fst tmd.TileSize * i,
            int tileMap.Position.Y - snd tmd.TileSize * j) // subtraction for right-handedness
        let optTileSetTile = Seq.tryFind (fun (tileSetTile' : TmxTilesetTile) -> tile.Gid - 1 = tileSetTile'.Id) tmd.TileSet.Tiles
        { Tile = tile; I = i; J = j; Gid = gid; GidPosition = gidPosition; Gid2 = gid2; TilePosition = tilePosition; OptTileSetTile = optTileSetTile }

    let private makeDefaultEntity2 xTypeName optName =
        let id = getNuId ()
        { Id = id
          Name = match optName with None -> str id | Some name -> name
          Enabled = true
          Visible = true
          Xtension = { OptXTypeName = Some xTypeName; XFields = Map.empty; IsSealed = false }}

    let makeDefaultEntity xTypeName optName seal (dispatcherContainer : IXDispatcherContainer) =
        match Map.tryFind xTypeName <| dispatcherContainer.GetDispatchers () with
        | None -> failwith <| "Invalid XType name '" + xTypeName.LunStr + "'."
        | Some dispatcher ->
            let entity = makeDefaultEntity2 xTypeName optName
            let entity' = entity.Init dispatcherContainer
            { entity' with Xtension = { entity'.Xtension with IsSealed = seal }}

    let registerEntity address (entity : Entity) world =
        entity.Register (address, world)

    let unregisterEntity address world =
        let entity = get world <| worldEntityLens address
        entity.Unregister (address, world)

    let removeEntity address world =
        let world' = unregisterEntity address world
        set None world' <| worldOptEntityLens address

    let removeEntities address world =
        let entities = get world <| worldEntitiesLens address
        Map.fold
            (fun world' entityName _ -> removeEntity (address @ [entityName]) world')
            world
            entities

    let addEntity address entity world =
        let world' =
            match get world <| worldOptEntityLens address with
            | None -> world
            | Some _ -> removeEntity address world
        let (entity', world'') = registerEntity address entity world'
        set entity' world'' <| worldEntityLens address

    let addEntities address entities world =
        List.fold
            (fun world' entity -> addEntity (addrstr address entity.Name) entity world')
            world
            entities

    let writeEntityToXml (writer : XmlWriter) entity =
        writer.WriteStartElement typeof<Entity>.Name
        Xtension.writeProperties writer entity
        writer.WriteEndElement ()

    let readEntityFromXml (entityNode : XmlNode) seal (world : World) =
        let entity = makeDefaultEntity (Lun.make typeof<EntityDispatcher>.Name) None seal world
        Xtension.readProperties entityNode entity
        entity

    let pickingSort entities =
        let priorities = List.map getPickingPriority entities
        let prioritiesAndEntities = List.zip priorities entities
        let prioritiesAndEntitiesSorted = List.sortWith sortFstAsc prioritiesAndEntities
        List.map snd prioritiesAndEntitiesSorted

    let tryPickEntity position entities camera =
        let entitiesSorted = pickingSort entities
        List.tryFind
            (fun entity ->
                let positionEntity = mouseToEntity position camera entity
                let transform = getEntityTransform entity
                let picked = isInBox3 positionEntity transform.Position transform.Size
                picked)
            entitiesSorted