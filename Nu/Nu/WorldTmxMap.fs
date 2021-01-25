// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open System.Xml.Linq
open Prime
open TiledSharp

[<RequireQualifiedAccess>]
module TmxMap =

    let makeLayerTile gid x y hflip vflip dflip =
        let tid =
            gid |||
            (if hflip then 0x80000000 else 0x0) |||
            (if vflip then 0x40000000 else 0x0) |||
            (if dflip then 0x20000000 else 0x0) |>
            uint
        TmxLayerTile (tid, x, y)

    let makeObject (id : int) (gid : int) (x : float) (y : float) (width : float) (height : float) =
        let xml = XElement (XName.op_Implicit "object")
        xml.Add (XAttribute (XName.op_Implicit "id", id))
        xml.Add (XAttribute (XName.op_Implicit "gid", gid))
        xml.Add (XAttribute (XName.op_Implicit "x", x))
        xml.Add (XAttribute (XName.op_Implicit "y", y))
        xml.Add (XAttribute (XName.op_Implicit "width", width))
        xml.Add (XAttribute (XName.op_Implicit "height", height))
        TmxObject xml

    let rec importShape shape (tileSize : Vector2) (tileOffset : Vector2) =
        let tileExtent = tileSize * 0.5f
        match shape with
        | BodyEmpty as be -> be
        | BodyBox box -> BodyBox { box with Extent = box.Extent * tileExtent; Center = box.Center * tileSize + tileOffset }
        | BodyCircle circle -> BodyCircle { circle with Radius = circle.Radius * tileExtent.Y; Center = circle.Center * tileSize + tileOffset }
        | BodyCapsule capsule -> BodyCapsule { capsule with Height = tileSize.Y; Radius = capsule.Radius * tileExtent.Y; Center = capsule.Center * tileSize + tileOffset }
        | BodyPolygon polygon -> BodyPolygon { polygon with Vertices = Array.map (fun point -> point * tileSize) polygon.Vertices; Center = polygon.Center * tileSize + tileOffset }
        | BodyShapes shapes -> BodyShapes (List.map (fun shape -> importShape shape tileSize tileOffset) shapes)

    let getDescriptor tileMapPosition (tileMap : TmxMap) =
        let tileSizeI = v2i tileMap.TileWidth tileMap.TileHeight
        let tileSizeF = v2 (single tileSizeI.X) (single tileSizeI.Y)
        let tileMapSizeM = v2i tileMap.Width tileMap.Height
        let tileMapSizeI = v2i (tileMapSizeM.X * tileSizeI.X) (tileMapSizeM.Y * tileSizeI.Y)
        let tileMapSizeF = v2 (single tileMapSizeI.X) (single tileMapSizeI.Y)
        { TileMap = tileMap
          TileSizeI = tileSizeI; TileSizeF = tileSizeF
          TileMapSizeM = tileMapSizeM; TileMapSizeI = tileMapSizeI; TileMapSizeF = tileMapSizeF
          TileMapPosition = tileMapPosition }

    let tryGetTileMap (tileMapAsset : TileMap AssetTag) world =
        match World.tryGetTileMapMetadata tileMapAsset world with
        | Some (_, _, tileMap) -> Some tileMap
        | None -> None

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

    let tryGetTileAnimationDescriptor tileIndex tileLayer tileMapDescriptor =
        match tryGetTileDescriptor tileIndex tileLayer tileMapDescriptor with
        | Some tileDescriptor ->
            match tileDescriptor.TileSetTileOpt with
            | Some tileSetTile ->
                match tileSetTile.Properties.TryGetValue Constants.TileMap.AnimationPropertyName with
                | (true, tileAnimationStr) ->
                    try Some (scvaluem<TileAnimationDescriptor> tileAnimationStr)
                    with _ -> None
                | (false, _) -> None
            | None -> None
        | None -> None

    let tryGetTileLayerBodyShape ti (tl : TmxLayer) tmd =
        match tryGetTileDescriptor ti tl tmd with
        | Some td ->
            match td.TileSetTileOpt with
            | Some tileSetTile ->
                match tileSetTile.Properties.TryGetValue Constants.TileMap.CollisionPropertyName with
                | (true, cexpr) ->
                    let tileCenter =
                        v2
                            (td.TilePositionF.X + tmd.TileSizeF.X * 0.5f)
                            (td.TilePositionF.Y + tmd.TileSizeF.Y * 0.5f)
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
              Density = Constants.Physics.DensityDefault
              Friction = friction
              Restitution = restitution
              FixedRotation = true
              AngularVelocity = 0.0f
              AngularDamping = 0.0f
              LinearVelocity = v2Zero
              LinearDamping = 0.0f
              Inertia = 0.0f
              GravityScale = 0.0f
              CollisionCategories = PhysicsEngine.categorizeCollisionMask collisionCategories
              CollisionMask = PhysicsEngine.categorizeCollisionMask collisionMask
              IgnoreCCD = false
              IsBullet = false
              IsSensor = false }
        bodyProperties

    let getLayeredDescriptors time absolute (viewBounds : Vector4) tileLayerClearance (tileMapPosition : Vector2) tileMapElevation tileMapParallax (tileMap : TmxMap) =
        let layers = List.ofSeq tileMap.Layers
        let tileSourceSize = v2i tileMap.TileWidth tileMap.TileHeight
        let tileSize = v2 (single tileMap.TileWidth) (single tileMap.TileHeight)
        let tileAssets = tileMap.ImageAssets
        let tileMapDescriptor = getDescriptor tileMapPosition tileMap
        let descriptorLists =
            List.foldi
                (fun i descriptorLists (layer : TmxLayer) ->

                    // compute elevation value
                    let elevationOffset =
                        match layer.Properties.TryGetValue Constants.TileMap.ElevationPropertyName with
                        | (true, elevation) -> scvalue elevation
                        | (false, _) -> single i * tileLayerClearance
                    let elevation = tileMapElevation + elevationOffset

                    // compute parallax position
                    let parallaxPosition =
                        if absolute
                        then tileMapPosition
                        else tileMapPosition + tileMapParallax * elevation * -viewBounds.Center

                    // compute positions relative to tile map
                    let (r, r2) =
                        if absolute then
                            let r = v2Zero
                            let r2 = viewBounds.Size
                            (r, r2)
                        else
                            let r = viewBounds.Position - parallaxPosition
                            let r2 = r + viewBounds.Size
                            (r, r2)

                    // accumulate decriptors
                    let descriptors = List ()
                    let mutable yC = 0
                    let mutable yO = r.Y + single yC * tileSize.Y
                    while r.Y + single yC * tileSize.Y < r2.Y + tileSize.Y do

                        // compute y index and ensure it's in bounds
                        let yI = tileMap.Height - 1 - int (yO / tileSize.Y)
                        if yO >= 0.0f && yI >= 0 && yI < tileMap.Height then

                            // accumulate strip tiles
                            let tiles = List ()
                            let mutable xS = 0.0f
                            let mutable xO = r.X
                            while xO < r2.X + tileSize.X do
                                let xI = int (xO / tileSize.X)
                                if xO >= 0.0f && xI >= 0 then
                                    if xI < tileMap.Width then
                                        let xTileIndex = xI + yI * tileMap.Width
                                        let xTile = layer.Tiles.[xTileIndex]
                                        let xTile =
                                            match tryGetTileAnimationDescriptor xTileIndex layer tileMapDescriptor with
                                            | Some xTileAnimationDescriptor ->
                                                let compressedTime = time / xTileAnimationDescriptor.TileAnimationDelay
                                                let xTileOffset = int compressedTime % xTileAnimationDescriptor.TileAnimationRun
                                                makeLayerTile (xTile.Gid + xTileOffset) xTile.X xTile.Y xTile.HorizontalFlip xTile.VerticalFlip xTile.DiagonalFlip
                                            | None -> xTile
                                        tiles.Add xTile
                                else xS <- xS + tileSize.X
                                xO <- xO + tileSize.X

                            // compute strip transform
                            let transform =
                                { Position = v2 (xS - modulus r.X tileSize.X) (single yC * tileSize.Y - modulus r.Y tileSize.Y) + viewBounds.Position
                                  Size = v2 (single tiles.Count * tileSize.X) tileSize.Y
                                  Rotation = 0.0f
                                  Elevation = elevation
                                  Flags = 0 }

                            // check if in view bounds
                            if Math.isBoundsIntersectingBounds (v4Bounds transform.Position transform.Size) viewBounds then

                                // accumulate descriptor
                                descriptors.Add
                                    { Elevation = transform.Elevation
                                      PositionY = transform.Position.Y
                                      AssetTag = AssetTag.make "" "" // just disregard asset for render ordering
                                      RenderDescriptor =
                                        TileLayerDescriptor
                                            { Transform = transform
                                              MapSize = Vector2i (tileMap.Width, tileMap.Height)
                                              Tiles = Seq.toArray tiles
                                              TileSourceSize = tileSourceSize
                                              TileSize = tileSize
                                              TileAssets = tileAssets }}

                        // loop
                        yC <- inc yC
                        yO <- r.Y + single yC * tileSize.Y
                                    
                    Seq.toList descriptors :: descriptorLists)
                [] layers
        List.concat descriptorLists

    let getQuickSize (tileMap : TmxMap) =
        v2
            (single (tileMap.Width * tileMap.TileWidth))
            (single (tileMap.Height * tileMap.TileHeight))