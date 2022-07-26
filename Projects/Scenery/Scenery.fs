namespace Scenery
open System.Collections.Generic
open System.Numerics
open Prime
open TiledSharp
open Nu
open Nu.Declarative

[<RequireQualifiedAccess>]
module Simulants =

    // here we create an entity reference for SkyBox. This is useful for simulants that you want
    // to refer to from multiple places
    let Light = Simulants.Default.Group / "Light"

// this is a custom entity for performance testing
type CustomModelDispatcher () =
    inherit StaticModelDispatcher ()

    override this.Update (entity, world) =
        entity.SetRotation (entity.GetRotation world * Quaternion.CreateFromAxisAngle (v3Up, 0.01f)) world

[<RequireQualifiedAccess>]
module Field =

    let CachedDescriptors = dictPlus<StaticModel AssetTag, Vector3 * Map<Vector2i, single>> HashIdentity.Structural []

    let private createFieldSurfaceDescriptorAndSizeAndHeightMap tileMapWidth tileMapHeight (tileSets : TmxTileset array) (tileLayer : TmxLayer) (heightLayer : TmxLayer) =

        // compute bounds
        let heightScalar = 0.5f
        let heightMax = 16.0f * heightScalar
        let position = v3Zero
        let size = v3 (single tileMapWidth) (heightMax * 2.0f) (single tileMapHeight)
        let bounds = box3 position size

        // make positions array
        let positions = Array.zeroCreate<Vector3> (tileMapWidth * tileMapHeight * 6)

        // initialize positions flat, centered
        let offset = v3 (single tileMapWidth * -0.5f) 0.0f (single tileMapHeight * -0.5f)
        for i in 0 .. dec tileMapWidth do
            for j in 0 .. dec tileMapHeight do
                let t = j * tileMapWidth + i
                let tile = heightLayer.Tiles.[t]
                let mutable tileSetOpt = None
                for tileSet in tileSets do
                    let tileZero = tileSet.FirstGid
                    let tileCount = let opt = tileSet.TileCount in opt.GetValueOrDefault 0
                    match tileSetOpt with
                    | None ->
                        if  tile.Gid >= tileZero &&
                            tile.Gid < tileZero + tileCount then
                            tileSetOpt <- Some tileSet
                    | Some _ -> ()
                let height =
                    match tileSetOpt with
                    | None -> 0.0f
                    | Some tileSet -> single (tile.Gid - tileSet.FirstGid) * heightScalar
                let u = t * 6
                let position = v3 (single i) height (single j) + offset
                positions.[u] <- position
                positions.[u+1] <- position + v3Right
                positions.[u+2] <- position + v3Right + v3Forward
                positions.[u+3] <- position
                positions.[u+4] <- position + v3Right + v3Forward
                positions.[u+5] <- position + v3Forward

        // slope positions horizontal
        for i in 0 .. dec tileMapWidth do
            for j in 0 .. dec tileMapHeight do
                if j % 2 = 0 then
                    let t = j * tileMapWidth + i
                    let tNorth = t - tileMapWidth
                    if tNorth >= 0 then
                        let u = t * 6
                        let uNorth = tNorth * 6
                        positions.[u+5].Y <- positions.[uNorth].Y
                        positions.[u+2].Y <- positions.[uNorth+1].Y
                        positions.[u+4].Y <- positions.[uNorth+1].Y
                    let tSouth = t + tileMapWidth
                    if tSouth < tileMapWidth * tileMapHeight then
                        let u = t * 6
                        let uSouth = tSouth * 6
                        positions.[u].Y <- positions.[uSouth+5].Y
                        positions.[u+3].Y <- positions.[uSouth+5].Y
                        positions.[u+1].Y <- positions.[uSouth+2].Y

        // slope positions vertical
        for i in 0 .. dec tileMapWidth do
            if i % 2 = 0 then
                for j in 0 .. dec tileMapHeight do
                    let t = j * tileMapWidth + i
                    let tWest = t - 1
                    if tWest >= 0 then
                        let u = t * 6
                        let uWest = tWest * 6
                        positions.[u].Y <- positions.[uWest+1].Y
                        positions.[u+3].Y <- positions.[uWest+1].Y
                        positions.[u+5].Y <- positions.[uWest+2].Y
                    let tEast = t + 1
                    if tEast < tileMapWidth * tileMapHeight then
                        let u = t * 6
                        let uEast = tEast * 6
                        positions.[u+1].Y <- positions.[uEast].Y
                        positions.[u+2].Y <- positions.[uEast+5].Y
                        positions.[u+4].Y <- positions.[uEast+5].Y

        // make height map in-place
        let mutable heightMap = Map.empty

        // populate height map
        for i in 0 .. dec tileMapWidth do
            for j in 0 .. dec tileMapHeight do
                let t = j * tileMapWidth + i
                let u = t * 6
                let height =
                    (positions.[u].Y +
                     positions.[u+1].Y +
                     positions.[u+2].Y +
                     positions.[u+5].Y) *
                    0.25f
                heightMap <- Map.add (v2i i j) height heightMap

        // make tex coordses array
        let texCoordses = Array.zeroCreate<Vector2> (tileMapWidth * tileMapHeight * 6)

        // populate tex coordses
        let mutable albedoTileSetOpt = None
        for i in 0 .. dec tileMapWidth do
            for j in 0 .. dec tileMapHeight do
                let t = j * tileMapWidth + i
                let tile = tileLayer.Tiles.[t]
                let mutable tileSetOpt = None
                for tileSet in tileSets do
                    let tileZero = tileSet.FirstGid
                    let tileCount = let opt = tileSet.TileCount in opt.GetValueOrDefault 0
                    match albedoTileSetOpt with
                    | None ->
                        if tile.Gid = 0 then
                            tileSetOpt <- Some tileSet // just use the first tile set for the empty tile
                        elif tile.Gid >= tileZero && tile.Gid < tileZero + tileCount then
                            tileSetOpt <- Some tileSet
                            albedoTileSetOpt <- tileSetOpt // use tile set that is first to be non-zero
                    | Some _ -> tileSetOpt <- albedoTileSetOpt
                match tileSetOpt with
                | Some tileSet ->
                    let tileId = tile.Gid - tileSet.FirstGid
                    let tileImageWidth = let opt = tileSet.Image.Width in opt.Value
                    let tileImageHeight = let opt = tileSet.Image.Height in opt.Value
                    let tileWidthNormalized = single tileSet.TileWidth / single tileImageWidth
                    let tileHeightNormalized = single tileSet.TileHeight / single tileImageHeight
                    let tileXCount = let opt = tileSet.Columns in opt.Value
                    let tileX = tileId % tileXCount
                    let tileY = tileId / tileXCount + 1
                    let texCoordX = single tileX * tileWidthNormalized
                    let texCoordY = single tileY * tileHeightNormalized
                    let texCoordX2 = texCoordX + tileWidthNormalized
                    let texCoordY2 = texCoordY - tileHeightNormalized
                    let u = t * 6
                    texCoordses.[u] <- v2 texCoordX texCoordY
                    texCoordses.[u+1] <- v2 texCoordX2 texCoordY
                    texCoordses.[u+2] <- v2 texCoordX2 texCoordY2
                    texCoordses.[u+3] <- v2 texCoordX texCoordY
                    texCoordses.[u+4] <- v2 texCoordX2 texCoordY2
                    texCoordses.[u+5] <- v2 texCoordX texCoordY2
                | None -> ()

        // make normals array
        let normals = Array.zeroCreate<Vector3> (tileMapWidth * tileMapHeight * 6)

        // populate normals
        for i in 0 .. dec tileMapWidth do
            for j in 0 .. dec tileMapHeight do
                let t = j * tileMapWidth + i
                let u = t * 6
                let a = positions.[u]
                let b = positions.[u+1]
                let c = positions.[u+5]
                let normal = Vector3.Normalize (Vector3.Cross (b - a, c - a))
                normals.[u] <- normal
                normals.[u+1] <- normal
                normals.[u+2] <- normal
                normals.[u+3] <- normal
                normals.[u+4] <- normal
                normals.[u+5] <- normal

        // create indices
        let indices = Array.init (tileMapWidth * tileMapHeight * 6) id

        // ensure we've found an albedo tile set
        match albedoTileSetOpt with
        | Some albedoTileSet ->

            // create static model surface descriptor
            let descriptor =
                { Positions = positions
                  TexCoordses = texCoordses
                  Normals = normals
                  Indices = indices
                  AffineMatrix = m4Identity
                  Bounds = bounds
                  Albedo = Color.White
                  AlbedoImage = albedoTileSet.ImageAsset
                  Metalness = 0.0f
                  MetalnessImage = Assets.Default.MaterialMetalness
                  Roughness = 1.2f
                  RoughnessImage = Assets.Default.MaterialRoughness
                  AmbientOcclusion = 1.0f
                  AmbientOcclusionImage = albedoTileSet.ImageAsset
                  NormalImage = Assets.Default.MaterialNormal
                  TwoSided = false }
            
            // fin
            (descriptor, size, heightMap)

        // did not find albedo tile set
        | None -> failwith "Unable to find custom TmxLayer Image property; cannot create tactical map."

    type Field =
        private
            { FieldTickTime : uint64
              FieldTileMap : TileMap AssetTag }

        member this.UpdateTime = this.FieldTickTime

    let getFieldStaticModelAndSizeAndHeightMap (field : Field) world =
        let fieldModelAssetTag = asset field.FieldTileMap.PackageName (field.FieldTileMap.AssetName + "Model")
        match CachedDescriptors.TryGetValue fieldModelAssetTag with
        | (false, _) ->
            let (_, tileSetsAndImages, tileMap) = World.getTileMapMetadata field.FieldTileMap world
            let tileSets = Array.map fst tileSetsAndImages
            let untraversableLayer = tileMap.Layers.["Untraversable"] :?> TmxLayer
            let untraversableHeightLayer = tileMap.Layers.["UntraversableHeight"] :?> TmxLayer
            let traversableLayer = tileMap.Layers.["Traversable"] :?> TmxLayer
            let traversableHeightLayer = tileMap.Layers.["TraversableHeight"] :?> TmxLayer
            let (untraversableSurfaceDescriptor, _, _) = createFieldSurfaceDescriptorAndSizeAndHeightMap tileMap.Width tileMap.Height tileSets untraversableLayer untraversableHeightLayer
            let untraversableSurfaceDescriptor = { untraversableSurfaceDescriptor with Roughness = 0.1f }
            let (traversableSurfaceDescriptor, traversableSize, traversableHeightMap) = createFieldSurfaceDescriptorAndSizeAndHeightMap tileMap.Width tileMap.Height tileSets traversableLayer traversableHeightLayer
            let surfaceDescriptors = [|untraversableSurfaceDescriptor; traversableSurfaceDescriptor|]
            let bounds = let bounds = untraversableSurfaceDescriptor.Bounds in bounds.Combine traversableSurfaceDescriptor.Bounds
            World.enqueueRenderMessage3d (SetStaticModelMessage (surfaceDescriptors, bounds, fieldModelAssetTag)) world
            World.enqueueRenderMessage3d (SetImageMinFilter (OpenGL.TextureMinFilter.NearestMipmapNearest, traversableSurfaceDescriptor.AlbedoImage)) world
            World.enqueueRenderMessage3d (SetImageMagFilter (OpenGL.TextureMagFilter.Nearest, traversableSurfaceDescriptor.AlbedoImage)) world
            CachedDescriptors.Add (fieldModelAssetTag, (traversableSize, traversableHeightMap))
            (fieldModelAssetTag, traversableSize, traversableHeightMap)
        | (true, (traversableSize, traversableHeightMap)) -> (fieldModelAssetTag, traversableSize, traversableHeightMap)

    let advance field =
        { field with FieldTickTime = inc field.FieldTickTime }

    let make tileMap =
        { FieldTickTime = 0UL
          FieldTileMap = tileMap }

type Field = Field.Field

// this is our Elm-style message type
type Message =
    | UpdateMessage

// this is our Elm-style command type
type Command =
    | UpdateCommand

// this is our Elm-style game dispatcher
type SceneryDispatcher () =
    inherit GameDispatcher<Field, Message, Command> (Field.make (asset "Field" "Field"))

    // here we channel from events to signals
    override this.Channel (_, game) =
        [game.UpdateEvent => msg UpdateMessage
         game.UpdateEvent => cmd UpdateCommand]

    // here we handle the Elm-style messages
    override this.Message (field, message, _, _) =
        match message with
        | UpdateMessage -> just (Field.advance field)

    // here we handle the Elm-style commands
    override this.Command (_, command, _, world) =
        match command with
        | UpdateCommand ->
            let moveSpeed = if KeyboardState.isKeyDown KeyboardKey.Return then 0.5f elif KeyboardState.isShiftDown () then 0.02f else 0.12f
            let turnSpeed = if KeyboardState.isShiftDown () then 0.025f else 0.05f
            let position = World.getEyePosition3d world
            let rotation = World.getEyeRotation3d world
            let world =
                if KeyboardState.isKeyDown KeyboardKey.W
                then World.setEyePosition3d (position + Vector3.Transform (v3Forward, rotation) * moveSpeed) world
                else world
            let world =
                if KeyboardState.isKeyDown KeyboardKey.S
                then World.setEyePosition3d (position + Vector3.Transform (v3Back, rotation) * moveSpeed) world
                else world
            let world =
                if KeyboardState.isKeyDown KeyboardKey.A
                then World.setEyePosition3d (position + Vector3.Transform (v3Left, rotation) * moveSpeed) world
                else world
            let world =
                if KeyboardState.isKeyDown KeyboardKey.D
                then World.setEyePosition3d (position + Vector3.Transform (v3Right, rotation) * moveSpeed) world
                else world
            let world =
                if KeyboardState.isKeyDown KeyboardKey.Up
                then World.setEyePosition3d (position + Vector3.Transform (v3Up, rotation) * moveSpeed) world
                else world
            let world =
                if KeyboardState.isKeyDown KeyboardKey.Down
                then World.setEyePosition3d (position + Vector3.Transform (v3Down, rotation) * moveSpeed) world
                else world
            let world =
                if KeyboardState.isKeyDown KeyboardKey.Left
                then World.setEyeRotation3d (rotation * Quaternion.CreateFromAxisAngle (v3Up, turnSpeed)) world
                else world
            let world =
                if KeyboardState.isKeyDown KeyboardKey.Right
                then World.setEyeRotation3d (rotation * Quaternion.CreateFromAxisAngle (v3Down, turnSpeed)) world
                else world
            just world

    // here we describe the content of the game
    override this.Content (field, _) =
        [Content.screen Simulants.Default.Screen.Name Vanilla []
            [Content.group Simulants.Default.Group.Name []
                [Content.light3d Simulants.Light.Name
                    [Entity.Position == v3 0.0f 0.0f 0.0f
                     Entity.Color == Color.White
                     Entity.Brightness == 10.0f
                     Entity.Intensity == 1.0f]
                 Content.fps Gen.name
                    [Entity.Position == v3 250.0f -200.0f 0.0f]
                 Content.skyBox Gen.name
                    [Entity.CubeMap == Assets.Default.SkyBoxMap]
                 Content.staticBillboard Gen.name
                    [Entity.Position == v3 10.0f 0.0f -10.0f
                     Entity.Scale == v3Dup 10.0f]
                 Content.staticModelSurface Gen.name
                    [Entity.SurfaceIndex == 0
                     Entity.Size <== field --|> fun field world -> Triple.snd (Field.getFieldStaticModelAndSizeAndHeightMap field world)
                     Entity.StaticModel <== field --|> fun field world -> Triple.fst (Field.getFieldStaticModelAndSizeAndHeightMap field world)
                     Entity.InsetOpt <== field --> fun field -> Some (box2 (v2 (16.0f * (single (field.UpdateTime / 20UL % 3UL))) 0.0f) v2Zero)
                     Entity.RenderStyle == Forward 0.0f]
                 Content.staticModelSurface Gen.name
                    [Entity.SurfaceIndex == 1
                     Entity.Size <== field --|> fun field world -> Triple.snd (Field.getFieldStaticModelAndSizeAndHeightMap field world)
                     Entity.StaticModel <== field --|> fun field world -> Triple.fst (Field.getFieldStaticModelAndSizeAndHeightMap field world)
                     Entity.InsetOpt <== field --> fun field -> Some (box2 (v2 (16.0f * (single (field.UpdateTime / 20UL % 3UL))) 0.0f) v2Zero)
                     Entity.RenderStyle == Forward -1.0f]]]]

    override this.Register (game, world) =
        let world = base.Register (game, world)
#if DEBUG
        let population = 10
#else
        let population = 45
#endif
        let spread = 22.0f
        let offset = v3Dup spread * single population * 0.5f
        let positions = List ()
        for i in 0 .. population do
            for j in 0 .. population do
                for k in 0 .. population do
                    let random = v3 (Gen.randomf1 spread) (Gen.randomf1 spread) (Gen.randomf1 spread) - v3Dup (spread * 0.5f)
                    let position = v3 (single i) (single j) (single k) * spread + random - offset
                    positions.Add position
        let world =
            Seq.fold (fun world position ->
                let (staticModel, world) = World.createEntity<CustomModelDispatcher> None NoOverlay Simulants.Default.Group world
                staticModel.SetPosition position world)
                world positions
        world

    override this.PostUpdate (entity, world) =
        let world = base.PostUpdate (entity, world)
        let world = Simulants.Light.SetPosition (World.getEyePosition3d world + v3Up * 3.0f) world
        world
