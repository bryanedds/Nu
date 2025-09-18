// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open System.Numerics
open System.Text
open System.Xml.Linq
open Prime
open DotTiled
open DotTiled.Serialization

/// TmxMap functions for the world.
[<RequireQualifiedAccess>]
module TmxMap =

    /// Make a TmxMap from the content of a .tmx file.
    let makeFromFilePath (filePath : string) =
        Loader.Default().LoadMap filePath

    /// Make a default TmxMap.
    let makeDefault () = Metadata.getTileMapMetadata(Assets.Default.TileMap).TileMap

    /// Make an empty TmxMap.
    let makeEmpty () = Metadata.getTileMapMetadata(Assets.Default.EmptyTileMap).TileMap

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
            BoxRoundedShape { boxRounded with Size = boxRounded.Size * tileSize.V3; Radius = boxRounded.Radius * tileSize.Y; TransformOpt = transformOpt }
        | EdgeShape edge ->
            if Option.isSome edge.TransformOpt then Log.error "Transform of importing tile map shape should be None."
            EdgeShape { edge with Start = edge.Start * tileSize.V3; Stop = edge.Stop * tileSize.V3; TransformOpt = transformOpt }
        | ContourShape chain ->
            if Option.isSome chain.TransformOpt then Log.error "Transform of importing tile map shape should be None."
            ContourShape { chain with Links = Array.map (fun link -> link * tileSize.V3) chain.Links; TransformOpt = transformOpt }
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

    let getDescriptor tileMapPosition tileSizeDivisor (tileMap : Map) =
        let tileSizeDivisor = max 1u tileSizeDivisor
        let tileSizeI = v2i (int (tileMap.TileWidth / tileSizeDivisor)) (int (tileMap.TileHeight / tileSizeDivisor))
        let tileSizeF = v2 (single tileSizeI.X) (single tileSizeI.Y)
        let tileMapSizeM = v2i (int tileMap.Width) (int tileMap.Height)
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

    let tryGetTileDescriptor tileIndex (gids : uint array) tmd (tileDescriptor : TileDescriptor outref) =
        let tileMapRun = tmd.TileMapSizeM.X
        let i = tileIndex % tileMapRun
        let j = tileIndex / tileMapRun
        let tile = gids[tileIndex]
        if tile <> 0u then // not the empty tile
            let mutable tileOffset = 1u // gid 0 is the empty tile
            let mutable tileSetIndex = 0
            let mutable tileSetFound = false
            let mutable enr = tmd.TileMap.Tilesets.GetEnumerator () // TODO: try to figure out how to remove allocation here.
            while enr.MoveNext () && not tileSetFound do
                let set = enr.Current
                let firstGID = set.FirstGID.Value
                let tileCount = set.TileCount
                if  tile >= firstGID && tile < firstGID + tileCount then
                    tileSetFound <- true
                else
                    tileSetIndex <- inc tileSetIndex
                    tileOffset <- tileOffset + tileCount
            let tileId = tile - tileOffset
            let tileSet = tmd.TileMap.Tilesets.[tileSetIndex]
            let tilePositionI =
                v2i
                    (int tmd.TileMapPosition.X + tmd.TileSizeI.X * i)
                    (int tmd.TileMapPosition.Y - tmd.TileSizeI.Y * inc j + tmd.TileMapSizeI.Y) // invert y coords
            let tilePositionF = v2 (single tilePositionI.X) (single tilePositionI.Y)
            let tileSetTileOpt =
                tileSet.Tiles |> Seq.tryFind (fun t -> t.ID = tileId)
            tileDescriptor.Tile <- tile
            tileDescriptor.TileI <- i
            tileDescriptor.TileJ <- j
            tileDescriptor.TilePositionI <- tilePositionI
            tileDescriptor.TilePositionF <- tilePositionF
            tileDescriptor.TileSetTileOpt <- tileSetTileOpt
            true
        else false

    let tryGetTileAnimationDescriptor tileIndex gids tileMapDescriptor =
        let mutable tileDescriptor = Unchecked.defaultof<_>
        if tryGetTileDescriptor tileIndex gids tileMapDescriptor &tileDescriptor then
            match tileDescriptor.TileSetTileOpt with
            | Some tileSetTile ->
                let mutable tileAnimationStr = Unchecked.defaultof<StringProperty> // OPTIMIZATION: seems like TryGetValue allocates here if we use the tupling idiom (this may only be the case in Debug builds tho).
                if tileSetTile.TryGetProperty (Constants.TileMap.AnimationPropertyName, &tileAnimationStr) then
                    try ValueSome (scvalueMemo<TileAnimationDescriptor> tileAnimationStr.Value)
                    with _ -> ValueNone
                else ValueNone
            | None -> ValueNone
        else ValueNone

    let getTileLayerBodyShapes (tileLayer : TileLayer) tileMapDescriptor =
        
        // construct a list of body shapes
        let bodyShapes = List<BodyShape> ()
        let tileBoxes = dictPlus<single, Box3 List> HashIdentity.Structural []
        let gids = tileLayer.Data.Value.GlobalTileIDs.Value
        for i in 0 .. dec gids.Length do

            // construct a dictionary of tile boxes, adding non boxes to the result list
            let mutable tileDescriptor = Unchecked.defaultof<_>
            if tryGetTileDescriptor i gids tileMapDescriptor &tileDescriptor then
                match tileDescriptor.TileSetTileOpt with
                | Some tileSetTile ->
                    match tileSetTile.TryGetProperty Constants.TileMap.CollisionPropertyName with
                    | (true, cexpr : StringProperty) ->
                        let cexpr = cexpr.Value
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
        tileMapDescriptor.TileMap.Layers
        |> Seq.fold (fun shapess layer ->
            match layer with
            | :? TileLayer as tileLayer ->
                let shapes = getTileLayerBodyShapes tileLayer tileMapDescriptor
                shapes :: shapess
            | _ -> shapess)
            []
        |> Seq.concat
        |> Seq.toList

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
              VehicleProperties = VehiclePropertiesAbsent
              CollisionDetection = Continuous
              CollisionCategories = Physics.categorizeCollisionMask collisionCategories
              CollisionMask = Physics.categorizeCollisionMask collisionMask
              Sensor = false
              Awake = false
              BodyIndex = bodyIndex }
        bodyProperties

    let getLayeredMessages2d time absolute (viewBounds : Box2) (tileMapPosition : Vector2) tileMapElevation tileMapClipOpt tileMapColor tileMapEmission tileLayerClearance tileSizeDivisor tileIndexOffset tileIndexOffsetRange tileMapPackage (tileMap : Map) =
        let tileSizeDivisor = uint tileSizeDivisor
        let tileIndexOffset = uint tileIndexOffset
        let (tileIndexOffsetStart, tileIndexOffsetEnd) = (uint (fst tileIndexOffsetRange), uint (snd tileIndexOffsetRange))
        let layers = tileMap.Layers
        let tileSourceSize = v2i (int tileMap.TileWidth) (int tileMap.TileHeight)
        let tileSizeDivisor = max 1u tileSizeDivisor
        let tileSize = v2 (single (tileMap.TileWidth / tileSizeDivisor)) (single (tileMap.TileHeight / tileSizeDivisor))
        let layerSize = v2 (single tileMap.Width * tileSize.X) (single tileMap.Height * tileSize.Y)
        let tileAssets = tileMap.GetImageAssets tileMapPackage
        let tileGidCount = Array.fold (fun count struct (tileSet : Tileset, _) -> count + tileSet.TileCount) 0u tileAssets // TODO: make this a public function!
        let tileMapDescriptor = getDescriptor tileMapPosition tileSizeDivisor tileMap
        let descriptorLists =
            Seq.foldi (fun i descriptorLists (layer : BaseLayer) ->
                match layer with
                | :? TileLayer as layer ->
                    // compute elevation value
                    let elevationOffset =
                        match layer.TryGetProperty Constants.TileMap.ElevationPropertyName with
                        | (true, elevation : StringProperty) -> scvalue elevation.Value
                        | (false, _) -> single i * tileLayerClearance
                    let elevation = tileMapElevation + elevationOffset
                    
                    // compute parallax position
                    let parallax = v2 (single layer.ParallaxX) (single layer.ParallaxY)
                    let parallaxPosition =
                        if absolute
                        then tileMapPosition
                        else tileMapPosition + (viewBounds.Center - parallax * viewBounds.Center)
                    
                    // accumulate when layer is in view
                    let layerBounds = box2 parallaxPosition layerSize
                    if viewBounds.Intersects layerBounds then
                        let gids = layer.Data.Value.GlobalTileIDs.Value
                        let flips = layer.Data.Value.FlippingFlags.Value
                        // accumulate descriptors
                        let descriptors = SList.make ()
                        for tileY in 0u .. dec tileMap.Height do
                            let tileStripY = single (tileMap.Height - tileY - 1u) * tileSize.Y
                            let tileStripBounds = box2 (parallaxPosition + v2Up * tileStripY) (v2 layerBounds.Size.X tileSize.Y)
                            if viewBounds.Intersects tileStripBounds then
                                let tileStrip = SList.make ()
                                let offsetX = single (tileMap.Width / 2u) * tileSize.X
                                for tileX in 0u .. dec tileMap.Width do
                                    let tileIndex = tileY * tileMap.Width + tileX |> int
                                    let tile = gids[tileIndex]
                                    let tile =
                                        if  tile <> 0u && // never offset the zero tile!
                                            tile >= tileIndexOffsetStart &&
                                            tile < tileIndexOffsetEnd then
                                            let xTileGidOffset = tile + tileIndexOffset
                                            if xTileGidOffset > 0u && xTileGidOffset < tileGidCount then xTileGidOffset
                                            else tile
                                        else tile
                                    let tile =
                                        match tryGetTileAnimationDescriptor tileIndex gids tileMapDescriptor with
                                        | ValueSome xTileAnimationDescriptor ->
                                            let compressedTime =
                                                match (time, xTileAnimationDescriptor.TileAnimationDelay) with
                                                | (UpdateTime time, UpdateTime delay) -> time / delay
                                                | (TickTime time, TickTime delay) -> time / delay
                                                | (_, _) -> failwith "Cannot operate on incompatible GameTime values."
                                            let xTileOffset = int compressedTime % xTileAnimationDescriptor.TileAnimationRun * xTileAnimationDescriptor.TileAnimationStride
                                            tile + uint xTileOffset
                                        | ValueNone ->
                                            tile
                                    let flip =
                                        let horizontal = flips[tileIndex] &&& FlippingFlags.FlippedHorizontally = FlippingFlags.FlippedHorizontally
                                        let vertical = flips[tileIndex] &&& FlippingFlags.FlippedVertically = FlippingFlags.FlippedVertically
                                        match (horizontal, vertical) with
                                        | (true, false) -> FlipH
                                        | (false, true) -> FlipV
                                        | (true, true) -> FlipHV
                                        | (false, false) -> FlipNone
                                    tileStrip.Add struct (tile, flip)
                                let offsetY = tileStripY + tileSize.Y * 0.5f
                                let mutable transform = Transform.makeDefault ()
                                transform.Position <- parallaxPosition.V3 + v3Up * offsetY + v3Right * offsetX
                                transform.Size <- v3 (single tileMap.Width * tileSize.X) tileSize.Y 0.0f
                                transform.Elevation <- elevation
                                transform.Absolute <- absolute
                                descriptors.Add
                                    { Elevation = transform.Elevation
                                      Horizon = transform.HorizonUnscaled // ignoring scale and orientation for tile map
                                      AssetTag = AssetTag.makeEmpty () // just disregard asset for render ordering
                                      RenderOperation2d =
                                      RenderTiles
                                        { Transform = transform
                                          ClipOpt = tileMapClipOpt
                                          Color = tileMapColor
                                          Emission = tileMapEmission
                                          MapSize = Vector2i (int tileMap.Width, int tileMap.Height)
                                          Tiles = tileStrip
                                          TileSourceSize = tileSourceSize
                                          TileSize = tileSize
                                          TileAssets = tileAssets }}
                        Seq.toList descriptors :: descriptorLists
                    
                    else descriptorLists
                | _ -> descriptorLists)
                [] layers
        List.concat descriptorLists

    let getAttributesInferred tileSizeDivisor (tileMap : Map) =
        let tileSizeDivisor = max 1u tileSizeDivisor
        AttributesInferred.important
            (v3
                (single (tileMap.Width * tileMap.TileWidth / tileSizeDivisor))
                (single (tileMap.Height * tileMap.TileHeight / tileSizeDivisor))
                0.0f)
            v3Zero