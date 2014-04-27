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
open Nu.DomainModel
open Nu.Camera

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

        abstract member GetRenderDescriptors : Matrix3 * Entity * World -> RenderDescriptor list
        default this.GetRenderDescriptors (view, entity, world) = []

        abstract member GetQuickSize : Entity * World -> Vector2
        default this.GetQuickSize (entity, world) = DefaultEntitySize

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
                .SetIsTransformRelative(true)

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

    let entityIsTransformRelativeLens =
        { Get = fun (entity : Entity) -> entity.IsTransformRelative
          Set = fun value entity -> entity.SetIsTransformRelative value }

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

    let getEntityTransformAbsolute (entity : Entity) =
        { Transform.Position = entity.Position
          Depth = entity.Depth
          Size = entity.Size
          Rotation = entity.Rotation }

    let getEntityTransformRelative (view : Matrix3) (entity : Entity) =
        let inverseView = Matrix3.getInverseViewMatrix view
        { Transform.Position = entity.Position * inverseView
          Depth = entity.Depth
          Size = entity.Size * Matrix3.getScaleMatrix inverseView
          Rotation = entity.Rotation }

    // TODO: turn into a lens
    let getEntityTransform optCamera (entity : Entity) =
        if entity.IsTransformRelative then
            let view =
                match optCamera with
                | None -> Matrix3.identity
                | Some camera -> getViewF camera
            getEntityTransformRelative view entity
        else getEntityTransformAbsolute entity

    // TODO: turn into a lens
    let setEntityTransformAbsolute positionSnap rotationSnap transform (entity : Entity) =
        let transform' = snapTransform positionSnap rotationSnap transform
        entity
            .SetPosition(transform'.Position)
            .SetDepth(transform'.Depth)
            .SetSize(transform'.Size)
            .SetRotation(transform'.Rotation)

    // TODO: turn into a lens
    let setEntityTransformRelative view positionSnap rotationSnap (transform : Transform) entity =
        let transform' = { transform with Position = transform.Position * view; Size = transform.Size * Matrix3.getScaleMatrix view }
        setEntityTransformAbsolute positionSnap rotationSnap transform' entity

    // TODO: turn into a lens
    let setEntityTransform optCamera positionSnap rotationSnap transform dispatcherContainer (entity : Entity) =
        if entity.IsTransformRelative then
            let view =
                match optCamera with
                | None -> Matrix3.identity
                | Some camera -> getViewF camera
            setEntityTransformRelative view positionSnap rotationSnap transform entity
        else setEntityTransformAbsolute positionSnap rotationSnap transform entity

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
            let tileSet = map.Tilesets.[0] // MAGIC_VALUE: I'm not sure how to properly specify this
            let optTileSetWidth = tileSet.Image.Width
            let optTileSetHeight = tileSet.Image.Height
            let tileSetSize = (optTileSetWidth.Value / fst tileSize, optTileSetHeight.Value / snd tileSize)
            { Map = map; MapSize = mapSize; TileSize = tileSize; TileSizeF = tileSizeF; TileSet = tileSet; TileSetSize = tileSetSize }

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
        let tilePosition = (int tileMap.Position.X + (fst tmd.TileSize * i), int tileMap.Position.Y + (snd tmd.TileSize * j))
        let optTileSetTile = Seq.tryFind (fun (tileSetTile' : TmxTilesetTile) -> tile.Gid - 1 = tileSetTile'.Id) tmd.TileSet.Tiles
        let tileSetPosition = (gidPosition % fst tmd.TileSetSize, gidPosition / snd tmd.TileSetSize * snd tmd.TileSize)
        { Tile = tile; I = i; J = j; Gid = gid; GidPosition = gidPosition; Gid2 = gid2; TilePosition = tilePosition; OptTileSetTile = optTileSetTile; TileSetPosition = tileSetPosition }

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
        writeModelProperties writer entity
        writer.WriteEndElement ()

    let loadEntityFromXml (entityNode : XmlNode) seal (world : World) =
        let entity = makeDefaultEntity (Lun.make typeof<EntityDispatcher>.Name) None seal world
        setModelProperties entityNode entity
        entity

    let pickingSort entities world =
        let priorities = List.map getPickingPriority entities
        let prioritiesAndEntities = List.zip priorities entities
        let prioritiesAndEntitiesSorted = List.sortWith sortFstAsc prioritiesAndEntities
        List.map snd prioritiesAndEntitiesSorted

    let tryPickEntity (position : Vector2) entities world =
        let entitiesSorted = pickingSort entities world
        List.tryFind
            (fun entity ->
                let transform = getEntityTransform (Some world.Camera) entity
                position.X >= transform.Position.X &&
                    position.X < transform.Position.X + transform.Size.X &&
                    position.Y >= transform.Position.Y &&
                    position.Y < transform.Position.Y + transform.Size.Y)
            entitiesSorted