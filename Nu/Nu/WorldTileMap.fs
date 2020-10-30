// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Numerics
open Prime
open TiledSharp

[<RequireQualifiedAccess>]
module TmxMap =

    let rec importShape shape (tileSize : Vector2) (tileOffset : Vector2) =
        let tileExtent = tileSize * 0.5f
        match shape with
        | BodyEmpty as be -> be
        | BodyBox box -> BodyBox { box with Extent = box.Extent * tileExtent; Center = box.Center * tileSize + tileOffset }
        | BodyCircle circle -> BodyCircle { circle with Radius = circle.Radius * tileExtent.Y; Center = circle.Center * tileSize + tileOffset }
        | BodyCapsule capsule -> BodyCapsule { capsule with Height = tileSize.Y; Radius = capsule.Radius * tileExtent.Y; Center = capsule.Center * tileSize + tileOffset }
        | BodyPolygon polygon -> BodyPolygon { polygon with Vertices = Array.map (fun point -> point * tileSize) polygon.Vertices; Center = polygon.Center * tileSize + tileOffset }
        | BodyShapes shapes -> BodyShapes (List.map (fun shape -> importShape shape tileSize tileOffset) shapes)

    let tryGetTileMap (tileMapAsset : TileMap AssetTag) world =
        match World.tryGetTileMapMetadata tileMapAsset world with
        | Some (_, _, tileMap) -> Some tileMap
        | None -> None

    let tryGetDescriptor tileMapPosition (tileMap : TmxMap) =
        let tileSizeI = Vector2i (tileMap.TileWidth, tileMap.TileHeight)
        let tileSizeF = Vector2 (single tileSizeI.X, single tileSizeI.Y)
        let tileMapSizeM = Vector2i (tileMap.Width, tileMap.Height)
        let tileMapSizeI = Vector2i (tileMapSizeM.X * tileSizeI.X, tileMapSizeM.Y * tileSizeI.Y)
        let tileMapSizeF = Vector2 (single tileMapSizeI.X, single tileMapSizeI.Y)
        Some
            { TileMap = tileMap
              TileSizeI = tileSizeI; TileSizeF = tileSizeF
              TileMapSizeM = tileMapSizeM; TileMapSizeI = tileMapSizeI; TileMapSizeF = tileMapSizeF
              TileMapPosition = tileMapPosition }

    let tryGetTileDescriptor tileIndex (tl : TmxLayer) tmd =
        let tileMapRun = tmd.TileMapSizeM.X
        let (i, j) = (tileIndex % tileMapRun, tileIndex / tileMapRun)
        let tile = tl.Tiles.[tileIndex]
        if tile.Gid <> 0 then // not the empty tile
            let mutable tileOffset = 1 // gid 0 is the empty tile
            let mutable tileSetIndex = 0
            let mutable tileSetFound = false
            let mutable enr = tmd.TileMap.Tilesets.GetEnumerator ()
            while enr.MoveNext () && not tileSetFound do
                let set = enr.Current
                let tileCountOpt = set.TileCount
                let tileCount = if tileCountOpt.HasValue then tileCountOpt.Value else 0
                if  tile.Gid >= set.FirstGid && tile.Gid < set.FirstGid + tileCount ||
                    not tileCountOpt.HasValue then // HACK: when tile count is missing, assume we've found the tile...?
                    tileSetFound <- true
                else
                    tileSetIndex <- inc tileSetIndex
                    tileOffset <- tileOffset + tileCount
            let tileId = tile.Gid - tileOffset
            let tileSet = tmd.TileMap.Tilesets.[tileSetIndex]
            let tilePositionI =
                v2i
                    (int tmd.TileMapPosition.X + tmd.TileSizeI.X * i)
                    (int tmd.TileMapPosition.Y - tmd.TileSizeI.Y * inc j + tmd.TileMapSizeI.Y) // invert y coords
            let tilePositionF = v2 (single tilePositionI.X) (single tilePositionI.Y)
            let tileSetTileOpt =
                match tileSet.Tiles.TryGetValue tileId with
                | (true, tileSetTile) -> Some tileSetTile
                | (false, _) -> None
            Some { Tile = tile; I = i; J = j; TilePositionI = tilePositionI; TilePositionF = tilePositionF; TileSetTileOpt = tileSetTileOpt }
        else None

    let tryGetTileLayerBodyShape ti (tl : TmxLayer) tmd =
        match tryGetTileDescriptor ti tl tmd with
        | Some td ->
            match td.TileSetTileOpt with
            | Some tileSetTile ->
                match tileSetTile.Properties.TryGetValue Constants.TileMap.CollisionPropertyName with
                | (true, cexpr) ->
                    let tileCenter =
                        Vector2
                            (td.TilePositionF.X + tmd.TileSizeF.X * 0.5f,
                             td.TilePositionF.Y + tmd.TileSizeF.Y * 0.5f)
                    let tileBody =
                        match cexpr with
                        | "" -> BodyBox { Extent = tmd.TileSizeF * 0.5f; Center = tileCenter; PropertiesOpt = None }
                        | _ ->
                            let tileShape = scvalue<BodyShape> cexpr
                            let tileShapeImported = importShape tileShape tmd.TileSizeF tileCenter
                            tileShapeImported
                    Some tileBody
                | (false, _) -> None
            | None -> None
        | None -> None

    let getTileLayerBodyShapes (tileLayer : TmxLayer) tileMapDescriptor =
        Seq.foldi
            (fun i bodyShapes _ ->
                match tryGetTileLayerBodyShape i tileLayer tileMapDescriptor with
                | Some bodyShape -> bodyShape :: bodyShapes
                | None -> bodyShapes)
            [] tileLayer.Tiles |>
        Seq.toList

    let getBodyShapes tileMapDescriptor =
        tileMapDescriptor.TileMap.Layers |>
        Seq.fold (fun shapess tileLayer ->
            let shapes = getTileLayerBodyShapes tileLayer tileMapDescriptor
            shapes :: shapess)
            [] |>
        Seq.concat |>
        Seq.toList

    let getBodyProperties enabled friction restitution collisionCategories collisionMask bodyId tileMapDescriptor =
        let bodyProperties =
            { BodyId = bodyId
              Position = v2Zero
              Rotation = 0.0f
              BodyShape = BodyShapes (getBodyShapes tileMapDescriptor)
              BodyType = BodyType.Static
              Awake = false
              Enabled = enabled
              Density = Constants.Physics.NormalDensity
              Friction = friction
              Restitution = restitution
              FixedRotation = true
              AngularVelocity = 0.0f
              AngularDamping = 0.0f
              LinearVelocity = Vector2.Zero
              LinearDamping = 0.0f
              Inertia = 0.0f
              GravityScale = 0.0f
              CollisionCategories = PhysicsEngine.categorizeCollisionMask collisionCategories
              CollisionMask = PhysicsEngine.categorizeCollisionMask collisionMask
              IgnoreCCD = false
              IsBullet = false
              IsSensor = false }
        bodyProperties

    let getRenderMessages absolute (viewBounds : Vector4) tileLayerClearance tileMapPosition tileMapDepth tileMapParallax (tileMap : TmxMap) =
        let layers = List.ofSeq tileMap.Layers
        let tileSourceSize = Vector2i (tileMap.TileWidth, tileMap.TileHeight)
        let tileSize = Vector2 (single tileMap.TileWidth, single tileMap.TileHeight)
        let messagess =
            List.foldi
                (fun i messagess (layer : TmxLayer) ->
                    let messages =
                        Array.fold
                            (fun messages j ->
                                let yOffset = single (tileMap.Height - j - 1) * tileSize.Y
                                let position = tileMapPosition + v2 0.0f yOffset
                                let depthOffset =
                                    match layer.Properties.TryGetValue Constants.TileMap.DepthPropertyName with
                                    | (true, depth) -> scvalue depth
                                    | (false, _) -> single i * tileLayerClearance
                                let depth = tileMapDepth + depthOffset
                                let parallaxTranslation =
                                    if absolute then Vector2.Zero
                                    else tileMapParallax * depth * -viewBounds.Center
                                let parallaxPosition = position + parallaxTranslation
                                let size = Vector2 (tileSize.X * single tileMap.Width, tileSize.Y)
                                let transform =
                                    { Position = parallaxPosition
                                      Size = size
                                      Rotation = 0.0f
                                      Depth = depth
                                      Flags = 0 }
                                let tiles =
                                    layer.Tiles |>
                                    enumerable<_> |>
                                    Seq.skip (j * tileMap.Width) |>
                                    Seq.take tileMap.Width |>
                                    Seq.toArray
                                if Math.isBoundsIntersectingBounds (v4Bounds parallaxPosition size) viewBounds then
                                    let message =
                                        LayeredDescriptorMessage
                                            { Depth = transform.Depth
                                              PositionY = transform.Position.Y
                                              AssetTag = AssetTag.make "" "" // just disregard asset for render ordering
                                              RenderDescriptor =
                                                TileLayerDescriptor
                                                    { Transform = transform
                                                      MapSize = Vector2i (tileMap.Width, tileMap.Height)
                                                      Tiles = tiles
                                                      TileSourceSize = tileSourceSize
                                                      TileSize = tileSize
                                                      TileImageAssets = tileMap.ImageAssets }}
                                    message :: messages
                                else messages)
                            [] [|0 .. tileMap.Height - 1|]
                    messages :: messagess)
                [] layers
        List.concat messagess

    let getQuickSize (tileMap : TmxMap) =
        v2
            (single (tileMap.Width * tileMap.TileWidth))
            (single (tileMap.Height * tileMap.TileHeight))