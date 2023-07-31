namespace Tactics
open System
open System.Collections.Generic
open System.Numerics
open Prime
open TiledSharp
open Nu
open Tactics

type Occupant =
    | Character of Character
    | Chest of unit

type OccupantIndex =
    | AllyIndex of int
    | EnemyIndex of int
    | ChestIndex of int

type NarrativeState =
    | NarrativeResult of bool

type BattleState =
    | BattleReady of int64
    | BattleCharacterReady of int64 // eye moving to character
    | BattleCharacterMenu of CharacterIndex // using ring menu or AI
    | BattleCharacterMoving of int64 * int * Vector2i array * CharacterIndex // character moving to destination
    | BattleCharacterAttacking of CharacterIndex * CharacterIndex
    | BattleCharacterTeching
    | BattleCharacterConsuming
    | BattleResult of int64 * bool

type FieldState =
    | FieldReady of int64 // field fades in
    | NarrativeState of NarrativeState
    | BattleState of BattleState
    | FieldQuitting of int64 * bool // field fades out
    | FieldQuit

type FieldScript =
    | FieldToBattle
    | FieldToNarrative
    | FieldCondition of FieldScript * FieldScript
    | FieldScripts of FieldScript list
    static member empty = FieldScripts []

type FieldTileVertices =
    { FieldTileVertices : Vector3 array }
    member this.Center =
        (this.FieldTileVertices.[0] + this.FieldTileVertices.[1] + this.FieldTileVertices.[2] + this.FieldTileVertices.[3]) / 4.0f

type FieldMetadata =
    { FieldTileVerticesMap : Map<Vector2i, FieldTileVertices>
      FieldUntraversableSurfaceDescriptor : SurfaceDescriptor
      FieldTraversableSurfaceDescriptor : SurfaceDescriptor
      FieldBounds : Box3 }

[<RequireQualifiedAccess>]
module Field =

    type [<SymbolicExpansion>] Field =
        private
            { FieldState_ : FieldState
              FieldScript_ : FieldScript
              FieldTileMap_ : TileMap AssetTag
              OccupantPositions_ : Map<OccupantIndex, Vector2i>
              OccupantIndices_ : Map<Vector2i, OccupantIndex>
              Occupants_ : Map<OccupantIndex, Occupant>
              SelectedTile_ : Vector2i }

        member this.FieldState = this.FieldState_

    let private CachedFieldMetadata = dictPlus<TileMap AssetTag, FieldMetadata> HashIdentity.Structural []

    let private createFieldSurfaceDescriptorAndVertexMap tileMapWidth tileMapHeight (tileSets : TmxTileset array) (tileLayer : TmxLayer) (heightLayer : TmxLayer) tileMapPackageName =

        // compute bounds
        let heightScalar = 0.5f
        let heightMax = 16.0f * heightScalar
        let position = v3Zero
        let bounds = box3 position (v3 (single tileMapWidth) (heightMax * 2.0f) (single tileMapHeight))

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
                    | Some _ -> ()
                    | None ->
                        if  tile.Gid >= tileZero &&
                            tile.Gid < tileZero + tileCount then
                            tileSetOpt <- Some tileSet
                let height =
                    match tileSetOpt with
                    | Some tileSet -> single (tile.Gid - tileSet.FirstGid) * heightScalar
                    | None -> 0.0f
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

        // make tile vertices map in-place
        let mutable verticesMap = Map.empty

        // populate tile vertices map
        for i in 0 .. dec tileMapWidth do
            for j in 0 .. dec tileMapHeight do
                let t = j * tileMapWidth + i
                let u = t * 6
                let vertices =
                    [|positions.[u]
                      positions.[u+1]
                      positions.[u+2]
                      positions.[u+5]|]
                verticesMap <- Map.add (v2i i j) { FieldTileVertices = vertices } verticesMap

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
                    | Some _ -> tileSetOpt <- albedoTileSetOpt
                    | None ->
                        if tile.Gid >= tileZero && tile.Gid < tileZero + tileCount then
                            tileSetOpt <- Some tileSet
                            albedoTileSetOpt <- tileSetOpt // use tile set that is first to be non-zero
                let tileSet =
                    match tileSetOpt with
                    | Some tileSet -> tileSet
                    | None -> tileSets.[0] // use first tile set if the empty tile
                let tileId =
                    if tile.Gid = 0
                    then 0 // special case for the empty tile
                    else tile.Gid - tileSet.FirstGid
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
        let indices = Array.init (tileMapWidth * tileMapHeight * 6) id;

        // ensure we've found an albedo tile set
        match albedoTileSetOpt with
        | Some albedoTileSet ->

            // make material properties
            let properties : OpenGL.PhysicallyBased.PhysicallyBasedMaterialProperties =
                { Albedo = Color.White
                  Metallic = 0.0f
                  Roughness = 1.2f
                  Emission = 1.0f
                  AmbientOcclusion = 1.0f
                  Height = 1.0f
                  InvertRoughness = false }

            // make static model surface descriptor
            let descriptor =
                { MaterialProperties = properties
                  Positions = positions
                  TexCoordses = texCoordses
                  Normals = normals
                  Indices = indices
                  ModelMatrix = m4Identity
                  Bounds = bounds
                  AlbedoImage = albedoTileSet.GetImageAsset tileMapPackageName
                  MetallicImage = Assets.Default.MaterialMetallic
                  RoughnessImage = Assets.Default.MaterialRoughness
                  EmissionImage = albedoTileSet.GetImageAsset tileMapPackageName
                  AmbientOcclusionImage = albedoTileSet.GetImageAsset tileMapPackageName
                  NormalImage = Assets.Default.MaterialNormal
                  HeightImage = Assets.Default.MaterialHeight
                  TextureMinFilterOpt = Some OpenGL.TextureMinFilter.NearestMipmapNearest
                  TextureMagFilterOpt = Some OpenGL.TextureMagFilter.Nearest
                  TwoSided = false }

            // fin
            (descriptor, verticesMap)

        // did not find albedo tile set
        | None -> failwith "Unable to find custom TmxLayer Image property; cannot create tactical map."

    let private getFieldMetadataInternal fieldTileMap =
        match CachedFieldMetadata.TryGetValue fieldTileMap with
        | (false, _) ->
            let tileMapPackageName = fieldTileMap.PackageName
            let (_, tileSetsAndImages, tileMap) = Metadata.getTileMapMetadata fieldTileMap
            let tileSets = Array.map fst tileSetsAndImages
            let untraversableLayer = tileMap.Layers.["Untraversable"] :?> TmxLayer
            let untraversableHeightLayer = tileMap.Layers.["UntraversableHeight"] :?> TmxLayer
            let traversableLayer = tileMap.Layers.["Traversable"] :?> TmxLayer
            let traversableHeightLayer = tileMap.Layers.["TraversableHeight"] :?> TmxLayer
            let (untraversableSurfaceDescriptor, _) = createFieldSurfaceDescriptorAndVertexMap tileMap.Width tileMap.Height tileSets untraversableLayer untraversableHeightLayer tileMapPackageName
            let untraversableSurfaceDescriptor = { untraversableSurfaceDescriptor with MaterialProperties = { untraversableSurfaceDescriptor.MaterialProperties with Roughness = 0.1f }}
            let (traversableSurfaceDescriptor, traversableTileVerticesMap) = createFieldSurfaceDescriptorAndVertexMap tileMap.Width tileMap.Height tileSets traversableLayer traversableHeightLayer tileMapPackageName
            let bounds = let bounds = untraversableSurfaceDescriptor.Bounds in bounds.Combine traversableSurfaceDescriptor.Bounds
            let fieldMetadata =
                { FieldTileVerticesMap = traversableTileVerticesMap
                  FieldUntraversableSurfaceDescriptor = untraversableSurfaceDescriptor
                  FieldTraversableSurfaceDescriptor = traversableSurfaceDescriptor
                  FieldBounds = bounds }
            CachedFieldMetadata.Add (fieldTileMap, fieldMetadata)
            fieldMetadata
        | (true, fieldMetadata) -> fieldMetadata

    let private tryGetFieldTileVerticesInternal index fieldTileMap =
        let fieldMetadata = getFieldMetadataInternal fieldTileMap
        match Map.tryFind index fieldMetadata.FieldTileVerticesMap with
        | Some index -> Some index
        | None -> None

    let private getFieldTileVerticesInternal index fieldTileMap =
        match tryGetFieldTileVerticesInternal index fieldTileMap with
        | Some vertices -> vertices
        | None -> failwith ("Field vertex index '" + scstring index + "' out of range.")

    let getFieldMetadata field =
        getFieldMetadataInternal field.FieldTileMap_

    let tryGetFieldTileVertices index field =
        tryGetFieldTileVerticesInternal index field.FieldTileMap_

    let getFieldTileVertices index field =
        getFieldTileVerticesInternal index field.FieldTileMap_

    let tryGetFieldTileDataAtMouse field world =
        let mouseRay = World.getMouseRay3dWorld false world
        let fieldMetadata = getFieldMetadata field
        let indices = [|0; 1; 2; 0; 2; 3|]
        let intersectionMap =
            Map.map (fun _ vertices ->
                let intersections = mouseRay.Intersects (indices, vertices.FieldTileVertices)
                match Seq.tryHead intersections with
                | Some struct (_, intersection) -> Some (intersection, vertices)
                | None -> None)
                fieldMetadata.FieldTileVerticesMap
        let intersections =
            intersectionMap |>
            Seq.map (fun (kvp : KeyValuePair<_, _>) -> (kvp.Key, kvp.Value)) |>
            Seq.filter (fun (_, opt) -> opt.IsSome) |>
            Seq.map (fun (key, Some (a, b)) -> (key, a, b)) |>
            Seq.toArray |>
            Array.sortBy Triple.snd |>
            Array.tryHead
        intersections

    let getOccupants field =
        field.OccupantPositions_ |>
        Map.map (fun _ position -> getFieldTileVerticesInternal position field.FieldTileMap_)  |>
        flip Seq.zip field.Occupants_ |>
        Seq.map (fun (kvp, kvp2) -> (kvp.Key, (kvp.Value, kvp2.Value))) |>
        Map.ofSeq

    let rec advanceFieldScript field (world : World) =
        match field.FieldState_ with
        | BattleState (BattleResult (_, result))
        | NarrativeState (NarrativeResult result) ->
            match field.FieldScript_ with
            | FieldToBattle -> { field with FieldState_ = BattleState (BattleReady (world.UpdateTime + 60L)) }
            | FieldToNarrative -> { field with FieldState_ = NarrativeState (NarrativeResult false) }
            | FieldCondition (consequent, alternative) -> advanceFieldScript { field with FieldScript_ = if result then consequent else alternative } world
            | FieldScripts scripts ->
                match scripts with
                | _ :: scripts -> advanceFieldScript { field with FieldScript_ = FieldScripts scripts } world
                | _ -> { field with FieldState_ = FieldQuitting (world.UpdateTime + 60L, result) }
        | _ -> field

    let advance field world =
        let field = advanceFieldScript field world
        field

    let make updateTime fieldScript (occupants : (Vector2i * OccupantIndex * Occupant) list) tileMap =
        let occupantIndices = occupants |> List.map (fun (position, index, _) -> (position, index)) |> Map.ofList
        let occupantPositions = occupants |> List.map (fun (position, index, _) -> (index, position)) |> Map.ofList
        let occupants = occupants |> List.map (fun (_, index, occupant) -> (index, occupant)) |> Map.ofList
        { FieldState_ = FieldReady updateTime
          FieldScript_ = fieldScript
          FieldTileMap_ = tileMap
          OccupantIndices_ = occupantIndices
          OccupantPositions_ = occupantPositions
          Occupants_ = occupants
          SelectedTile_ = v2iZero }

    let debug (world : World) =
        let occupants =
            [(v2i 10 10, ChestIndex 0, Chest ())
             (v2i 12 10, ChestIndex 1, Chest ())
             (v2i 10 12, ChestIndex 2, Chest ())
             (v2i 12 12, ChestIndex 3, Chest ())]
        make world.UpdateTime FieldScript.empty occupants (asset "Field" "Field")

type Field = Field.Field