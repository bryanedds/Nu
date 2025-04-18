﻿// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open System.Numerics
open System.Text
open System.Xml.Linq
open TiledSharp
open Prime

[<RequireQualifiedAccess>]
module TmxMap =

    /// Make a TmxMap from the content of a stream.
    let makeFromStream (stream : Stream) =
        TmxMap stream

    /// Make a TmxMap from the content of a text fragment.
    let makeFromText (text : string) =
        use stream = new MemoryStream (UTF8Encoding.UTF8.GetBytes text)
        makeFromStream stream

    /// Make a TmxMap from the content of a .tmx file.
    let makeFromFilePath (filePath : string) =
        TmxMap filePath

    /// Make a default TmxMap.
    let makeDefault () =
        makeFromText
            """<?xml version="1.0" encoding="UTF-8"?>
            <map version="1.2" tiledversion="1.3.4" orientation="orthogonal" renderorder="right-down" width="8" height="8" tilewidth="48" tileheight="48" infinite="0" nextlayerid="3" nextobjectid="1">
             <tileset firstgid="1" name="TileSet" tilewidth="48" tileheight="48" tilecount="72" columns="8">
              <properties>
               <property name="Image" value="[Default TileSet]"/>
              </properties>
              <image source="TileSet.png" trans="ff00ff" width="384" height="434"/>
              <tile id="0"><properties><property name="C" value=""/></properties></tile>
              <tile id="1"><properties><property name="C" value=""/></properties></tile>
              <tile id="2"><properties><property name="C" value=""/></properties></tile>
              <tile id="8"><properties><property name="C" value=""/></properties></tile>
              <tile id="9"><properties><property name="C" value=""/></properties></tile>
              <tile id="10"><properties><property name="C" value=""/></properties></tile>
              <tile id="11"><properties><property name="C" value=""/></properties></tile>
              <tile id="12"><properties><property name="C" value=""/></properties></tile>
              <tile id="13"><properties><property name="C" value=""/></properties></tile>
              <tile id="16"><properties><property name="C" value=""/></properties></tile>
              <tile id="17"><properties><property name="C" value=""/></properties></tile>
              <tile id="18"><properties><property name="C" value=""/></properties></tile>
              <tile id="19"><properties><property name="C" value=""/></properties></tile>
              <tile id="20"><properties><property name="C" value=""/></properties></tile>
              <tile id="21"><properties><property name="C" value=""/></properties></tile>
             </tileset>
             <layer id="1" name="Layer" width="8" height="8">
              <data encoding="base64" compression="zlib">
               eJyTYWBgkCGApaAYF18LinHxKcW0tB8At+0HYQ==
              </data>
             </layer>
            </map>"""

    /// Make an empty TmxMap.
    let makeEmpty () =
        makeFromText
            """<?xml version="1.0" encoding="UTF-8"?>
            <map version="1.2" tiledversion="1.3.4" orientation="orthogonal" renderorder="right-down" width="1" height="1" tilewidth="48" tileheight="48" infinite="0" nextlayerid="2" nextobjectid="1">
             <layer id="1" name="Layer" width="1" height="1">
              <data encoding="base64" compression="zlib">
               eJxjYGBgAAAABAAB
              </data>
             </layer>
            </map>"""

    /// Make a TmxLayerTile.
    let makeLayerTile gid hflip vflip dflip =
        let tid =
            gid |||
            (if hflip then 0x80000000 else 0x0) |||
            (if vflip then 0x40000000 else 0x0) |||
            (if dflip then 0x20000000 else 0x0) |>
            uint
        TmxLayerTile tid

    /// Make a TmxObject.
    let makeObject (id : int) (gid : int) (x : float) (y : float) (width : float) (height : float) =
        let xml = XElement (XName.op_Implicit "object")
        xml.Add (XAttribute (XName.op_Implicit "id", id))
        xml.Add (XAttribute (XName.op_Implicit "gid", gid))
        xml.Add (XAttribute (XName.op_Implicit "x", x))
        xml.Add (XAttribute (XName.op_Implicit "y", y))
        xml.Add (XAttribute (XName.op_Implicit "width", width))
        xml.Add (XAttribute (XName.op_Implicit "height", height))
        TmxObject xml

    let rec importShape shape center (tileSize : Vector2) (tileOffset : Vector2) =
        let transformOpt = Some (Affine.makeTranslation (center * tileSize.V3 + tileOffset.V3))
        match shape with
        | EmptyShape as empty ->
            empty
        | BoxShape box ->
            if Option.isSome box.TransformOpt then Log.error "Transform of importing tile map shape should be None."
            BoxShape { box with Size = box.Size * tileSize.V3; TransformOpt = transformOpt }
        | SphereShape sphere ->
            if Option.isSome sphere.TransformOpt then Log.error "Transform of importing tile map shape should be None."
            SphereShape { sphere with Radius = sphere.Radius * tileSize.Y; TransformOpt = transformOpt }
        | CapsuleShape capsule ->
            if Option.isSome capsule.TransformOpt then Log.error "Transform of importing tile map shape should be None."
            CapsuleShape { capsule with Height = tileSize.Y; Radius = capsule.Radius * tileSize.Y; TransformOpt = transformOpt }
        | BoxRoundedShape boxRounded ->
            if Option.isSome boxRounded.TransformOpt then Log.error "Transform of importing tile map shape should be None."
            BoxRoundedShape { boxRounded with Size = boxRounded.Size * tileSize.V3; Radius = boxRounded.Radius; TransformOpt = transformOpt }
        | PointsShape points ->
            if Option.isSome points.TransformOpt then Log.error "Transform of importing tile map shape should be None."
            PointsShape { points with Points = Array.map (fun point -> point * tileSize.V3) points.Points; TransformOpt = transformOpt }
        | GeometryShape _ as geometry ->
            geometry
        | StaticModelSurfaceShape _ as staticModelSurface ->
            staticModelSurface
        | StaticModelShape _ as staticModel ->
            staticModel
        | TerrainShape _ as terrain ->
            terrain
        | BodyShapes shapes ->
            BodyShapes (List.map (fun shape -> importShape shape center tileSize tileOffset) shapes)

    let getDescriptor tileMapPosition tileSizeDivisor (tileMap : TmxMap) =
        let tileSizeDivisor = max 1 tileSizeDivisor
        let tileSizeI = v2i (tileMap.TileWidth / tileSizeDivisor) (tileMap.TileHeight / tileSizeDivisor)
        let tileSizeF = v2 (single tileSizeI.X) (single tileSizeI.Y)
        let tileMapSizeM = v2i tileMap.Width tileMap.Height
        let tileMapSizeI = v2i (tileMapSizeM.X * tileSizeI.X) (tileMapSizeM.Y * tileSizeI.Y)
        let tileMapSizeF = v2 (single tileMapSizeI.X) (single tileMapSizeI.Y)
        { TileMap = tileMap
          TileSizeI = tileSizeI; TileSizeF = tileSizeF
          TileMapSizeM = tileMapSizeM; TileMapSizeI = tileMapSizeI; TileMapSizeF = tileMapSizeF
          TileMapPosition = tileMapPosition }

    let tryGetTileMap (tileMapAsset : TileMap AssetTag) =
        match Metadata.tryGetTileMapMetadata tileMapAsset with
        | ValueSome tileMapMetadata -> Some tileMapMetadata.TileMap
        | ValueNone -> None

    let tryGetTileDescriptor tileIndex (tl : TmxLayer) tmd (tileDescriptor : TileDescriptor outref) =
        let tileMapRun = tmd.TileMapSizeM.X
        let i = tileIndex % tileMapRun
        let j = tileIndex / tileMapRun
        let tile = tl.Tiles.[tileIndex]
        if tile.Gid <> 0 then // not the empty tile
            let mutable tileOffset = 1 // gid 0 is the empty tile
            let mutable tileSetIndex = 0
            let mutable tileSetFound = false
            let mutable enr = tmd.TileMap.Tilesets.GetEnumerator () // TODO: try to figure out how to remove allocation here.
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
                let mutable tileSetTile = Unchecked.defaultof<_> // OPTIMIZATION: seems like TryGetValue allocates here if we use the tupling idiom (this may only be the case in Debug builds tho).
                if tileSet.Tiles.TryGetValue (tileId, &tileSetTile)
                then Some tileSetTile
                else None
            tileDescriptor.Tile <- tile
            tileDescriptor.TileI <- i
            tileDescriptor.TileJ <- j
            tileDescriptor.TilePositionI <- tilePositionI
            tileDescriptor.TilePositionF <- tilePositionF
            tileDescriptor.TileSetTileOpt <- tileSetTileOpt
            true
        else false

    let tryGetTileAnimationDescriptor tileIndex tileLayer tileMapDescriptor =
        let mutable tileDescriptor = Unchecked.defaultof<_>
        if tryGetTileDescriptor tileIndex tileLayer tileMapDescriptor &tileDescriptor then
            match tileDescriptor.TileSetTileOpt with
            | Some tileSetTile ->
                let mutable tileAnimationStr = Unchecked.defaultof<_> // OPTIMIZATION: seems like TryGetValue allocates here if we use the tupling idiom (this may only be the case in Debug builds tho).
                if tileSetTile.Properties.TryGetValue (Constants.TileMap.AnimationPropertyName, &tileAnimationStr) then
                    try ValueSome (scvalueMemo<TileAnimationDescriptor> tileAnimationStr)
                    with _ -> ValueNone
                else ValueNone
            | None -> ValueNone
        else ValueNone

    let getTileLayerBodyShapes (tileLayer : TmxLayer) tileMapDescriptor =
        
        // construct a list of body shapes
        let bodyShapes = List<BodyShape> ()
        let tileBoxes = dictPlus<single, Box3 List> HashIdentity.Structural []
        for i in 0 .. dec tileLayer.Tiles.Length do

            // construct a dictionary of tile boxes, adding non boxes to the result list
            let mutable tileDescriptor = Unchecked.defaultof<_>
            if tryGetTileDescriptor i tileLayer tileMapDescriptor &tileDescriptor then
                match tileDescriptor.TileSetTileOpt with
                | Some tileSetTile ->
                    match tileSetTile.Properties.TryGetValue Constants.TileMap.CollisionPropertyName with
                    | (true, cexpr) ->
                        let tileCenter =
                            v2
                                (tileDescriptor.TilePositionF.X + tileMapDescriptor.TileSizeF.X * 0.5f)
                                (tileDescriptor.TilePositionF.Y + tileMapDescriptor.TileSizeF.Y * 0.5f)
                        match cexpr with
                        | "" ->
                            match tileBoxes.TryGetValue tileCenter.Y with
                            | (true, l) ->
                                l.Add (box3 (tileCenter.V3 - tileMapDescriptor.TileSizeF.V3 * 0.5f) tileMapDescriptor.TileSizeF.V3)
                            | (false, _) ->
                                tileBoxes.Add (tileCenter.Y, List [box3 (tileCenter.V3 - tileMapDescriptor.TileSizeF.V3 * 0.5f) tileMapDescriptor.TileSizeF.V3])
                        | "Top" ->
                            let tileShape = BoxShape { Size = v3 1.0f 0.5f 0.0f; TransformOpt = None; PropertiesOpt = None }
                            let tileShapeImported = importShape tileShape (v3 0.0f 0.25f 0.0f) tileMapDescriptor.TileSizeF tileCenter
                            bodyShapes.Add tileShapeImported
                        | "Bottom" ->
                            let tileShape = BoxShape { Size = v3 1.0f 0.5f 0.0f; TransformOpt = None; PropertiesOpt = None }
                            let tileShapeImported = importShape tileShape (v3 0.0f -0.25f 0.0f) tileMapDescriptor.TileSizeF tileCenter
                            bodyShapes.Add tileShapeImported
                        | "Left" ->
                            let tileShape = BoxShape { Size = v3 0.5f 1.0f 0.0f; TransformOpt = None; PropertiesOpt = None }
                            let tileShapeImported = importShape tileShape (v3 -0.25f 0.0f 0.0f) tileMapDescriptor.TileSizeF tileCenter
                            bodyShapes.Add tileShapeImported
                        | "Right" ->
                            let tileShape = BoxShape { Size = v3 0.5f 1.0f 0.0f; TransformOpt = None; PropertiesOpt = None }
                            let tileShapeImported = importShape tileShape (v3 0.25f 0.0f 0.0f) tileMapDescriptor.TileSizeF tileCenter
                            bodyShapes.Add tileShapeImported
                        | "TopLeft" ->
                            let tileShape = BoxShape { Size = v3 0.5f 0.5f 0.0f; TransformOpt = None; PropertiesOpt = None }
                            let tileShapeImported = importShape tileShape (v3 -0.25f 0.25f 0.0f) tileMapDescriptor.TileSizeF tileCenter
                            bodyShapes.Add tileShapeImported
                        | "TopRight" ->
                            let tileShape = BoxShape { Size = v3 0.5f 0.5f 0.0f; TransformOpt = None; PropertiesOpt = None }
                            let tileShapeImported = importShape tileShape (v3 0.25f 0.25f 0.0f) tileMapDescriptor.TileSizeF tileCenter
                            bodyShapes.Add tileShapeImported
                        | "BottomLeft" ->
                            let tileShape = BoxShape { Size = v3 0.5f 0.5f 0.0f; TransformOpt = None; PropertiesOpt = None }
                            let tileShapeImported = importShape tileShape (v3 -0.25f -0.25f 0.0f) tileMapDescriptor.TileSizeF tileCenter
                            bodyShapes.Add tileShapeImported
                        | "BottomRight" ->
                            let tileShape = BoxShape { Size = v3 0.5f 0.5f 0.0f; TransformOpt = None; PropertiesOpt = None }
                            let tileShapeImported = importShape tileShape (v3 0.25f -0.25f 0.0f) tileMapDescriptor.TileSizeF tileCenter
                            bodyShapes.Add tileShapeImported
                        | _ ->
                            let tileShape = scvalue<BodyShape> cexpr
                            let tileShapeImported = importShape tileShape v3Zero tileMapDescriptor.TileSizeF tileCenter
                            bodyShapes.Add tileShapeImported
                    | (false, _) -> ()
                | None -> ()
            else ()

        // combine adjacent tiles on the same row into strips
        let strips = List ()
        for boxes in tileBoxes.Values do
            let mutable box = boxes.[0]
            let epsilon = box.Size.X * 0.001f
            if boxes.Count > 1 then
                for i in 1 .. dec boxes.Count do
                    let box2 = boxes.[i]
                    let distance = abs (box2.Left.X - box.Right.X)
                    if distance < epsilon then
                        box.Size <- v3 (box.Size.X + box2.Size.X) box.Size.Y 0.0f
                    else
                        strips.Add box
                        box <- box2
                    if i = dec boxes.Count then
                        strips.Add box
            else strips.Add box

        // convert strips into BodyShapes and add to the resulting list
        for strip in strips do
            strip |> BoxShape.ofBox3 |> BoxShape |> bodyShapes.Add

        // fin
        bodyShapes

    let getBodyShapes tileMapDescriptor =
        tileMapDescriptor.TileMap.TileLayers |>
        Seq.fold (fun shapess tileLayer ->
            let shapes = getTileLayerBodyShapes tileLayer tileMapDescriptor
            shapes :: shapess)
            [] |>
        Seq.concat |>
        Seq.toList

    let getBodyProperties enabled friction restitution collisionCategories collisionMask bodyIndex tileMapDescriptor =
        let bodyProperties =
            { Enabled = enabled
              Center = v3Zero
              Rotation = quatIdentity
              Scale = v3One
              BodyShape = BodyShapes (getBodyShapes tileMapDescriptor)
              BodyType = BodyType.Static
              SleepingAllowed = true
              Friction = friction
              Restitution = restitution
              LinearVelocity = v3Zero
              LinearDamping = 0.0f
              AngularVelocity = v3Zero
              AngularDamping = 0.0f
              AngularFactor = v3One
              Substance = Mass 0.0f
              GravityOverride = Some v3Zero
              CharacterProperties = CharacterProperties.defaultProperties
              CollisionDetection = Discontinuous
              CollisionCategories = Physics.categorizeCollisionMask collisionCategories
              CollisionMask = Physics.categorizeCollisionMask collisionMask
              Sensor = false
              Awake = false
              BodyIndex = bodyIndex }
        bodyProperties

    let getLayeredMessages2d time absolute (viewBounds : Box2) (tileMapPosition : Vector2) tileMapElevation tileMapColor tileMapEmission tileLayerClearance tileSizeDivisor tileIndexOffset tileIndexOffsetRange tileMapPackage (tileMap : TmxMap) =
        let layers = List.ofSeq tileMap.TileLayers
        let tileSourceSize = v2i tileMap.TileWidth tileMap.TileHeight
        let tileSizeDivisor = max 1 tileSizeDivisor
        let tileSize = v2 (single (tileMap.TileWidth / tileSizeDivisor)) (single (tileMap.TileHeight / tileSizeDivisor))
        let tileAssets = tileMap.GetImageAssets tileMapPackage
        let tileGidCount = Array.fold (fun count struct (tileSet : TmxTileset, _) -> let count2 = tileSet.TileCount in count + count2.GetValueOrDefault 0) 0 tileAssets // TODO: make this a public function!
        let tileMapDescriptor = getDescriptor tileMapPosition tileSizeDivisor tileMap
        let descriptorLists =
            List.foldi (fun i descriptorLists (layer : TmxLayer) ->

                // compute elevation value
                let elevationOffset =
                    match layer.Properties.TryGetValue Constants.TileMap.ElevationPropertyName with
                    | (true, elevation) -> scvalue elevation
                    | (false, _) -> single i * tileLayerClearance
                let elevation = tileMapElevation + elevationOffset

                // compute parallax position
                let parallax = v2 (single layer.ParallaxX) (single layer.ParallaxY)
                let parallaxPosition =
                    if absolute
                    then tileMapPosition
                    else tileMapPosition + (viewBounds.Center - parallax * viewBounds.Center)

                // compute positions relative to tile map
                let (r, r2) =
                    if absolute then
                        let r = v2Zero
                        let r2 = viewBounds.Size
                        (r, r2)
                    else
                        let r = viewBounds.Min - parallaxPosition
                        let r2 = r + viewBounds.Size
                        (r, r2)

                // accumulate decriptors
                let descriptors = List ()
                let mutable yC = 0
                let mutable yO = r.Y + single yC * tileSize.Y
                yO <- yO + 0.0001f |> floor // NOTE: fixes #766 and seems to provide more horizontal tile alignment stability.
                while r.Y + single yC * tileSize.Y < r2.Y + tileSize.Y do

                    // compute y index and ensure it's in bounds
                    let yI = tileMap.Height - 1 - int (yO / tileSize.Y)
                    if yO >= 0.0f && yI >= 0 && yI < tileMap.Height then

                        // accumulate strip tiles
                        let tiles = SList.make ()
                        let mutable xS = 0.0f
                        let mutable xO = r.X
                        //xO <- xO + 0.0001f |> floor // NOTE: attempted to fix #832, but caused more issues near the origin in Blaze Vector.
                        while xO < r2.X + tileSize.X do
                            let xI = int (xO / tileSize.X)
                            if xO >= 0.0f && xI >= 0 then
                                if xI < tileMap.Width then
                                    let xTileIndex = xI + yI * tileMap.Width
                                    let xTile = layer.Tiles.[xTileIndex]
                                    let xTileGid =
                                        if  xTile.Gid <> 0 && // never offset the zero tile!
                                            xTile.Gid >= fst tileIndexOffsetRange &&
                                            xTile.Gid < snd tileIndexOffsetRange then
                                            let xTileGidOffset = xTile.Gid + tileIndexOffset
                                            if xTileGidOffset > 0 && xTileGidOffset < tileGidCount then xTileGidOffset
                                            else xTile.Gid
                                        else xTile.Gid
                                    let xTile =
                                        match tryGetTileAnimationDescriptor xTileIndex layer tileMapDescriptor with
                                        | ValueSome xTileAnimationDescriptor ->
                                            let compressedTime =
                                                match (time, xTileAnimationDescriptor.TileAnimationDelay) with
                                                | (UpdateTime time, UpdateTime delay) -> time / delay
                                                | (TickTime time, TickTime delay) -> time / delay
                                                | (_, _) -> failwith "Cannot operate on incompatible GameTime values."
                                            let xTileOffset = int compressedTime % xTileAnimationDescriptor.TileAnimationRun * xTileAnimationDescriptor.TileAnimationStride
                                            makeLayerTile (xTileGid + xTileOffset) xTile.HorizontalFlip xTile.VerticalFlip xTile.DiagonalFlip
                                        | ValueNone ->
                                            makeLayerTile xTileGid xTile.HorizontalFlip xTile.VerticalFlip xTile.DiagonalFlip
                                    tiles.Add xTile
                            else xS <- xS + tileSize.X
                            xO <- xO + tileSize.X

                        // compute strip transform
                        let stripSize = v3 (single tiles.Length * tileSize.X) tileSize.Y 0.0f
                        let stripPosition = v3 (xS - modulus r.X tileSize.X) (single yC * tileSize.Y - modulus r.Y tileSize.Y) 0.0f + viewBounds.Min.V3 + stripSize * 0.5f
                        let mutable transform = Transform.makeDefault ()
                        transform.Position <- stripPosition
                        transform.Size <- stripSize
                        transform.Elevation <- elevation
                        transform.Absolute <- absolute

                        // check if strip in view bounds
                        let stripBounds = box2 transform.PerimeterMin.V2 transform.Size.V2
                        if stripBounds.Intersects viewBounds then

                            // accumulate descriptor
                            descriptors.Add
                                { Elevation = transform.Elevation
                                  Horizon = transform.HorizonUnscaled // ignoring scale and orientation for tile map
                                  AssetTag = AssetTag.makeEmpty () // just disregard asset for render ordering
                                  RenderOperation2d =
                                    RenderTiles
                                        { Transform = transform
                                          ClipOpt = ValueNone // TODO: implement clipping for tile maps.
                                          Color = tileMapColor
                                          Emission = tileMapEmission
                                          MapSize = Vector2i (tileMap.Width, tileMap.Height)
                                          Tiles = tiles
                                          TileSourceSize = tileSourceSize
                                          TileSize = tileSize
                                          TileAssets = tileAssets }}

                    // loop
                    yC <- inc yC
                    yO <- r.Y + single yC * tileSize.Y

                Seq.toList descriptors :: descriptorLists)
                [] layers
        List.concat descriptorLists

    let getAttributesInferred tileSizeDivisor (tileMap : TmxMap) =
        let tileSizeDivisor = max 1 tileSizeDivisor
        AttributesInferred.important
            (v3
                (single (tileMap.Width * tileMap.TileWidth / tileSizeDivisor))
                (single (tileMap.Height * tileMap.TileHeight / tileSizeDivisor))
                0.0f)
            v3Zero