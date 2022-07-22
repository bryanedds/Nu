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

module Field =

    let CachedGeometries = dictPlus HashIdentity.Structural []

    let private createGeometryFromLayers tileMapWidth tileMapHeight (tileSets : TmxTileset array) (tileLayer : TmxLayer) (heightLayer : TmxLayer) =
        
            let texCoords = Array.zeroCreate<Vector2> (tileMapWidth * tileMapHeight * 6)

            for i in 0 .. dec tileMapWidth do
                for j in 0 .. dec tileMapHeight do
                    let t = i * tileMapWidth + j
                    let tile = tileLayer.Tiles.[t]
                    let mutable tileSetOpt = None
                    for tileSet in tileSets do
                        match tileSetOpt with
                        | None ->
                            if tile.Gid = 0 then
                                tileSetOpt <- Some tileSet // just use the first tile set for the empty tile
                            elif tile.Gid >= tileSet.FirstGid && tile.Gid < tileSet.FirstGid + tileSet.TileCount.GetValueOrDefault 0 then
                                tileSetOpt <- Some tileSet
                        | Some _ -> ()
                    match tileSetOpt with
                    | Some tileSet ->
                        let tileId = tile.Gid - tileSet.FirstGid
                        let texCoordX = single (tileId / tileSet.Columns.Value) / single tileSet.Image.Width.Value
                        let texCoordY = single (tileId % tileSet.Columns.Value) / single tileSet.Image.Height.Value
                        let texCoordX2 = texCoordX + 1.0f / single tileSet.Image.Width.Value
                        let texCoordY2 = texCoordY + 1.0f / single tileSet.Image.Height.Value
                        let u = t * 6
                        texCoords.[u] <- v2 texCoordX texCoordY
                        texCoords.[u+1] <- v2 texCoordX2 texCoordY
                        texCoords.[u+2] <- v2 texCoordX2 texCoordY2
                        texCoords.[u+3] <- v2 texCoordX texCoordY
                        texCoords.[u+4] <- v2 texCoordX2 texCoordY2
                        texCoords.[u+5] <- v2 texCoordX texCoordY
                    | None -> ()

            let positions = Array.zeroCreate<Vector3> (tileMapWidth * tileMapHeight * 6)

            for i in 0 .. dec tileMapWidth do
                for j in 0 .. dec tileMapHeight do
                    let t = i * tileMapWidth + j
                    let tile = heightLayer.Tiles.[t]
                    let mutable tileSetOpt = None
                    for tileSet in tileSets do
                        match tileSetOpt with
                        | None ->
                            if tile.Gid = 0 then
                                tileSetOpt <- Some tileSet // just use the first tile set for the empty tile
                            elif tile.Gid >= tileSet.FirstGid && tile.Gid < tileSet.FirstGid + tileSet.TileCount.GetValueOrDefault 0 then
                                tileSetOpt <- Some tileSet
                        | Some _ -> ()
                    match tileSetOpt with
                    | Some tileSet ->
                        let height = tile.Gid - tileSet.FirstGid
                        let u = t * 3 * 6
                        let position = v3 (single i) (single j) (single height)
                        positions.[u] <- position
                        positions.[u+1] <- position
                        positions.[u+2] <- position
                        positions.[u+3] <- position
                        positions.[u+4] <- position
                        positions.[u+5] <- position
                    | None -> ()

            for i in 1 .. dec tileMapWidth do
                for j in 1 .. dec tileMapHeight do
                    let u = i * tileMapWidth + j
                    let positionM1 = &positions.[dec u]
                    let position = &positions.[u]
                    position.X <- (positionM1.X + position.X) / 2.0f
                    let positionM1 = &positions.[dec u+3]
                    let position = &positions.[u+3]
                    position.X <- (positionM1.X + position.X) / 2.0f
                    let positionM1 = &positions.[dec u+5]
                    let position = &positions.[u+5]
                    position.X <- (positionM1.X + position.X) / 2.0f

            let normals = Array.zeroCreate<Vector3> (tileMapWidth * tileMapHeight * 6)

            for i in 1 .. dec tileMapWidth do
                for j in 1 .. dec tileMapHeight do
                    let u = i * tileMapWidth + j
                    let normal = Vector3.Normalize (Vector3.Cross (positions.[u+1] - positions.[u], positions.[u+5] - positions.[u]))
                    normals.[u] <- normal
                    normals.[u+1] <- normal
                    normals.[u+2] <- normal
                    normals.[u+3] <- normal
                    normals.[u+4] <- normal
                    normals.[u+5] <- normal

            let vertices = Array.zeroCreate (tileMapWidth * tileMapHeight * 6 * 8)

            for u in 0 .. dec vertices.Length / 8 do
                vertices.[u] <- positions.[u].X
                vertices.[u+1] <- positions.[u].Y
                vertices.[u+2] <- positions.[u].Z
                vertices.[u+3] <- texCoords.[u].X
                vertices.[u+4] <- texCoords.[u].Y
                vertices.[u+5] <- normals.[u].X
                vertices.[u+6] <- normals.[u].Y
                vertices.[u+7] <- normals.[u].Z

            let indices = Array.init (tileMapWidth * tileMapHeight * 6) id

            let bounds = v3 (single tileMapWidth) 16.0f (single tileMapHeight)

            let geometry = OpenGL.PhysicallyBased.CreatePhysicallyBasedGeometry (true, vertices, indices, box3 v3Zero bounds)

            let material =
                { OpenGL.PhysicallyBased.PhysicallyBasedMaterial.Albedo = Color.White
                  AlbedoTexture : uint
                  Metalness : single
                  MetalnessTexture : uint
                  Roughness : single
                  RoughnessTexture : uint
                  AmbientOcclusion : single
                  AmbientOcclusionTexture : uint
                  NormalTexture : uint
                  TwoSided : bool }

            OpenGL.PhysicallyBased.PhysicallyBasedSurface.make [||] m4Identity bounds  

            geometry

    type Field =
        private
            { FieldTileMap : TileMap AssetTag }

    let getFieldGeometries (field : Field) world =
        match CachedGeometries.TryGetValue field.FieldTileMap with
        | (false, _) ->
            let (_, tileSetsAndImages, tileMap) = World.getTileMapMetadata field.FieldTileMap world
            let tileSets = Array.map fst tileSetsAndImages
            let untraversableLayer = tileMap.Layers.["Untraversable"] :?> TmxLayer
            let untraversableHeightLayer = tileMap.Layers.["UntraversableHeight"] :?> TmxLayer
            let traversableLayer = tileMap.Layers.["Traversable"] :?> TmxLayer
            let traversableHeightLayer = tileMap.Layers.["TraversableHeight"] :?> TmxLayer
            let untraversableGeometry = createGeometryFromLayers tileMap.Width tileMap.Height tileSets untraversableLayer untraversableHeightLayer
            let traversableGeometry = createGeometryFromLayers tileMap.Width tileMap.Height tileSets traversableLayer traversableHeightLayer
            let geometries = (untraversableGeometry, traversableGeometry)
            CachedGeometries.Add (field.FieldTileMap, geometries)
            geometries
        | (true, geometries) -> geometries

    let make tileMap =
        { FieldTileMap = tileMap }

// this is our Elm-style command type
type Command =
    | Update

// this is our Elm-style game dispatcher
type SceneryDispatcher () =
    inherit GameDispatcher<unit, unit, Command> (())

    // here we channel from events to signals
    override this.Channel (_, game) =
        [game.UpdateEvent => cmd Update]

    // here we handle the Elm-style commands
    override this.Command (_, command, _, world) =
        match command with
        | Update ->
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
    override this.Content (_, _) =
        [Content.screen Simulants.Default.Screen.Name Vanilla []
            [Content.group Simulants.Default.Group.Name []
                [Content.light3d Simulants.Light.Name
                    [Entity.Position == v3 0.0f 0.0f 0.0f
                     Entity.Color == Color.White
                     Entity.Brightness == 10.0f
                     Entity.Intensity == 1.0f]
                 Content.skyBox Gen.name
                    [Entity.Position == v3 0.0f 0.0f 0.0f]
                 Content.fps Gen.name
                    [Entity.Position == v3 250.0f -200.0f 0.0f]]]]

    // here we create the scenery in an imperative fashion
    // NOTE: performance goal: 60fps, current: 57fps.
    override this.Register (game, world) =
        let world = base.Register (game, world)
        let field = Field.make (asset "Field" "Field")
#if DEBUG
        let population = 25
#else
        let population = 50
#endif
        let spread = 20.0f
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
