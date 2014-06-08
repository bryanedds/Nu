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

    type Entity with

        [<XField>] member this.Position with get () = this?Position () : Vector2
        member this.SetPosition (value : Vector2) : Entity = this?Position <- value
        [<XField>] member this.Depth with get () = this?Depth () : single
        member this.SetDepth (value : single) : Entity = this?Depth <- value
        [<XField>] member this.Rotation with get () = this?Rotation () : single
        member this.SetRotation (value : single) : Entity = this?Rotation <- value
        [<XField>] member this.Size with get () = this?Size () : Vector2
        member this.SetSize (value : Vector2) : Entity = this?Size <- value

        member this.Init (dispatcherContainer : IXDispatcherContainer) : Entity = this?Init dispatcherContainer
        member this.Register (address : Address, world : World) : Entity * World = this?Register (address, world)
        member this.Unregister (address : Address, world : World) : World = this?Unregister (address, world)
        member this.PropagatePhysics (address : Address, world : World) : World = this?PropagatePhysics (address, world)
        member this.ReregisterPhysicsHack (address : Address, world : World) : World = this?ReregisterPhysicsHack (address, world)
        member this.HandleBodyTransformMessage (message : BodyTransformMessage, address : Address, world : World) : World = this?HandleBodyTransformMessage (message, address, world)
        member this.GetRenderDescriptors (viewAbsolute : Matrix3, viewRelative : Matrix3, world : World) : RenderDescriptor list = this?GetRenderDescriptors (viewAbsolute, viewRelative, world)
        member this.GetQuickSize (world : World) : Vector2 = this?GetQuickSize world
        member this.IsTransformRelative (world : World) : bool = this?IsTransformRelative world

module Entity =

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

    let getEntityPosition (entity : Entity) =
        entity.Position

    let setEntityPosition snap position (entity : Entity) =
        let position' = snap2F snap position
        entity.SetPosition position'

    let getEntityTransform (entity : Entity) =
        { Transform.Position = entity.Position
          Depth = entity.Depth
          Size = entity.Size
          Rotation = entity.Rotation }

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
            let tileMapSize = (fst mapSize * fst tileSize, snd mapSize * snd tileSize)
            let tileMapSizeF = Vector2 (single <| fst tileMapSize, single <| snd tileMapSize)
            let tileSet = map.Tilesets.[0] // MAGIC_VALUE: I'm not sure how to properly specify this
            let optTileSetWidth = tileSet.Image.Width
            let optTileSetHeight = tileSet.Image.Height
            let tileSetSize = (optTileSetWidth.Value / fst tileSize, optTileSetHeight.Value / snd tileSize)
            { Map = map; MapSize = mapSize; TileSize = tileSize; TileSizeF = tileSizeF; TileMapSize = tileMapSize; TileMapSizeF = tileMapSizeF; TileSet = tileSet; TileSetSize = tileSetSize }

    let makeTileLayerData tileMap tmd (layer : TmxLayer) =
        let tiles = layer.Tiles
        { Layer = layer; Tiles = tiles }

    let makeTileData (tileMap : Entity) tmd tld tileIndex =
        let mapRun = fst tmd.MapSize
        let tileSetRun = fst tmd.TileSetSize
        let (i, j) = (tileIndex % mapRun, tileIndex / mapRun)
        let tile = tld.Tiles.[tileIndex]
        let gid = tile.Gid - tmd.TileSet.FirstGid
        let gidPosition = gid * fst tmd.TileSize
        let gid2 = (gid % tileSetRun, gid / tileSetRun)
        let tilePosition = (
            int tileMap.Position.X + fst tmd.TileSize * i,
            int tileMap.Position.Y - snd tmd.TileSize * (j + 1)) // subtraction for right-handedness
        let optTileSetTile = Seq.tryFind (fun (tileSetTile' : TmxTilesetTile) -> tile.Gid - 1 = tileSetTile'.Id) tmd.TileSet.Tiles
        { Tile = tile; I = i; J = j; Gid = gid; GidPosition = gidPosition; Gid2 = gid2; TilePosition = tilePosition; OptTileSetTile = optTileSetTile }

    let private makeDefaultEntity2 dispatcherName optName =
        let id = getNuId ()
        { Id = id
          Name = match optName with None -> string id | Some name -> name
          Enabled = true
          Visible = true
          Xtension = { XFields = Map.empty; OptXDispatcherName = Some dispatcherName; CanDefault = true; Sealed = false }}

    let makeDefaultEntity dispatcherName optName seal (dispatcherContainer : IXDispatcherContainer) =
        match Map.tryFind dispatcherName <| dispatcherContainer.GetDispatchers () with
        | None -> failwith <| "Invalid XDispatcher name '" + dispatcherName + "'."
        | Some dispatcher ->
            let entity = makeDefaultEntity2 dispatcherName optName
            let entity' = entity.Init dispatcherContainer
            { entity' with Xtension = { entity'.Xtension with Sealed = seal }}

    let writeEntityToXml (writer : XmlWriter) entity =
        writer.WriteStartElement typeof<Entity>.Name
        Xtension.writePropertiesToXmlWriter writer entity
        writer.WriteEndElement ()

    let readEntityFromXml (entityNode : XmlNode) defaultDispatcherName seal (world : World) =
        let entity = makeDefaultEntity defaultDispatcherName None seal world
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