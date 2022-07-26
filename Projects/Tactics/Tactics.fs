namespace Tactics
open System
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

type Direction =
    | Upward
    | Rightward
    | Downward
    | Leftward

    member this.Opposite =
        match this with
        | Upward -> Downward
        | Rightward -> Leftward
        | Downward -> Upward
        | Leftward -> Rightward

    static member ofVector3 (v3 : Vector3) =
        let angle = double (atan2 v3.Y v3.X)
        let angle = if angle < 0.0 then angle + Math.PI * 2.0 else angle
        let direction =
            if      angle > Math.PI * 1.75 || angle <= Math.PI * 0.25 then  Rightward
            elif    angle > Math.PI * 0.75 && angle <= Math.PI * 1.25 then  Leftward
            elif    angle > Math.PI * 0.25 && angle <= Math.PI * 0.75 then  Upward
            else                                                            Downward
        direction

    static member ofVector3Biased (v3 : Vector3) =
        let angle = double (atan2 v3.Y v3.X)
        let angle = if angle < 0.0 then angle + Math.PI * 2.0 else angle
        let direction =
            if      angle > Math.PI * 1.74997 || angle <= Math.PI * 0.25003 then    Rightward
            elif    angle > Math.PI * 0.74997 && angle <= Math.PI * 1.25003 then    Leftward
            elif    angle > Math.PI * 0.25 && angle <= Math.PI * 0.75 then          Upward
            else                                                                    Downward
        direction

    static member toVector3 direction =
        match direction with
        | Upward -> v3Up
        | Rightward -> v3Right
        | Downward -> v3Down
        | Leftward -> v3Left

type CharacterIndex =
    | AllyIndex of int
    | EnemyIndex of int

    member this.IsAlly =
        match this with
        | AllyIndex _ -> true
        | EnemyIndex _ -> false

    member this.IsEnemy =
        not this.IsAlly

    static member isFriendly index index2 =
        match (index, index2) with
        | (AllyIndex _, AllyIndex _) -> true
        | (EnemyIndex _, EnemyIndex _) -> true
        | (_, _) -> false

    static member isUnfriendly index index2 =
        not (CharacterIndex.isFriendly index index2)

    static member toEntityName index =
        match index with
        | AllyIndex i -> "Ally+" + scstring i
        | EnemyIndex i -> "Enemy+" + scstring i

type AllyType =
    | Jinn
    | Shade
    | Mael
    | Riain
    | Peric

type EnemyType =
    | DebugGoblin

type CharacterType =
    | Ally of AllyType
    | Enemy of EnemyType

    static member getName characterType =
        match characterType with
        | Ally ty -> string ty
        | Enemy ty -> string ty

type AnimationType =
    | LoopedWithDirection
    | LoopedWithoutDirection
    | SaturatedWithDirection
    | SaturatedWithoutDirection

type PoiseType =
    | Poising
    | Defending
    | Charging

type CharacterAnimationType =
    | WalkAnimation
    | CelebrateAnimation
    | ReadyAnimation
    | PoiseAnimation of PoiseType
    | AttackAnimation
    | WoundAnimation
    | SpinAnimation
    | DamageAnimation
    | IdleAnimation
    | CastAnimation
    | Cast2Animation
    | SlashAnimation
    | WhirlAnimation
    | BuryAnimation // TODO: get rid of this

type [<ReferenceEquality; NoComparison>] CharacterAnimationState =
    { StartTime : int64
      AnimationSheet : Image AssetTag
      CharacterAnimationType : CharacterAnimationType
      Direction : Direction }

    static member face direction state =
        { state with Direction = direction }

    static member setCharacterAnimationType timeOpt characterAnimationType state =
        if state.CharacterAnimationType <> characterAnimationType then
            match timeOpt with
            | Some time -> { state with StartTime = time; CharacterAnimationType = characterAnimationType }
            | None -> { state with CharacterAnimationType = characterAnimationType }
        else state

    static member directionToInt direction =
        match direction with
        | Upward -> 0
        | Rightward -> 1
        | Downward -> 2
        | Leftward -> 3

    static member localTime time state =
        time - state.StartTime

    static member indexCel delay time state =
        let localTime = CharacterAnimationState.localTime time state
        int (localTime / delay)

    static member indexLooped run delay time state =
        CharacterAnimationState.indexCel delay time state % run

    static member indexSaturated run delay time state =
        let cel = CharacterAnimationState.indexCel delay time state
        if cel < dec run then cel else dec run

    static member indexLoopedWithDirection run delay offset time state =
        let position = CharacterAnimationState.directionToInt state.Direction * run
        let position = Vector2i (CharacterAnimationState.indexLooped run delay time state + position, 0)
        let position = position + offset
        position

    static member indexLoopedWithoutDirection run delay offset time state =
        let position = CharacterAnimationState.indexLooped run delay time state
        let position = v2i position 0 + offset
        position

    static member indexSaturatedWithDirection run delay offset time state =
        let position = CharacterAnimationState.directionToInt state.Direction * run
        let position = Vector2i (CharacterAnimationState.indexSaturated run delay time state + position, 0)
        let position = position + offset
        position

    static member indexSaturatedWithoutDirection run stutter offset time state =
        let position = CharacterAnimationState.indexSaturated run stutter time state
        let position = Vector2i (position, 0)
        let position = position + offset
        position

    static member index time state =
        match state.CharacterAnimationType with
        | WalkAnimation -> CharacterAnimationState.indexLoopedWithDirection 4 15 (v2i 0 0) time state
        | CelebrateAnimation -> CharacterAnimationState.indexLoopedWithDirection 4 10 (v2i 0 1) time state
        | ReadyAnimation -> CharacterAnimationState.indexSaturatedWithDirection 3 15 (v2i 0 5) time state
        | PoiseAnimation poiseType ->
            match poiseType with
            | Poising -> CharacterAnimationState.indexLoopedWithDirection 4 15 (v2i 0 3) time state
            | Defending -> CharacterAnimationState.indexLoopedWithDirection 1 10 (v2i 0 9) time state
            | Charging -> CharacterAnimationState.indexLoopedWithDirection 4 10 (v2i 0 2) time state
        | AttackAnimation -> CharacterAnimationState.indexSaturatedWithDirection 3 15 (v2i 0 6) time state
        | WoundAnimation -> CharacterAnimationState.indexLoopedWithDirection 1 10 (v2i 0 11) time state
        | SpinAnimation -> CharacterAnimationState.indexLoopedWithDirection 4 10 (v2i 0 10) time state
        | DamageAnimation -> CharacterAnimationState.indexSaturatedWithDirection 1 10 (v2i 0 8) time state
        | IdleAnimation -> CharacterAnimationState.indexLoopedWithDirection 1 10 (v2i 0 10) time state
        | CastAnimation -> CharacterAnimationState.indexLoopedWithDirection 4 5 (v2i 0 2) time state
        | Cast2Animation -> CharacterAnimationState.indexLoopedWithDirection 2 10 (v2i 0 7) time state
        | SlashAnimation -> CharacterAnimationState.indexSaturatedWithDirection 3 15 (v2i 0 6) time state
        | WhirlAnimation -> CharacterAnimationState.indexLoopedWithDirection 4 3 (v2i 0 12) time state
        | BuryAnimation -> CharacterAnimationState.indexSaturatedWithDirection 4 10 (v2i 0 12) time state

    static member inset time (celSize : Vector2) state =
        let index = CharacterAnimationState.index time state
        let offset = v2 (single index.X) (single index.Y) * celSize
        let inset = box2 offset celSize
        inset

    static member progressOpt time state =
        let localTime = CharacterAnimationState.localTime time state
        let lengthOpt =
            match state.CharacterAnimationType with
            | WalkAnimation -> None
            | CelebrateAnimation -> None
            | ReadyAnimation -> Some 60
            | PoiseAnimation poiseType ->
                match poiseType with
                | Poising -> None
                | Defending -> None
                | Charging -> None
            | AttackAnimation -> Some 60
            | WoundAnimation -> Some 60
            | SpinAnimation -> Some 40
            | DamageAnimation -> Some 40
            | IdleAnimation -> None
            | CastAnimation -> None
            | Cast2Animation -> None
            | SlashAnimation -> Some 120
            | WhirlAnimation -> None
            | BuryAnimation -> None
        match lengthOpt with
        | Some length -> Some (min 1.0f (single localTime / single length))
        | None -> None

    static member getFinished time state =
        match CharacterAnimationState.progressOpt time state with
        | Some progress -> progress = 1.0f
        | None -> true

    static member empty =
        { StartTime = 0L
          AnimationSheet = asset "Field" "Jinn"
          CharacterAnimationType = IdleAnimation
          Direction = Downward }

    static member initial =
        { CharacterAnimationState.empty with Direction = Upward }

[<RequireQualifiedAccess>]
module Character =

    type [<ReferenceEquality; NoComparison>] Character =
        private
            { CharacterIndex_ : CharacterIndex
              CharacterType_ : CharacterType
              CharacterState_ : PoiseType
              CharacterAnimationState_ : CharacterAnimationState
              CelSize_ : Vector2 }

        (* CharacterState Properties *)
        member this.Name = CharacterType.getName this.CharacterType_
        member this.CharacterIndex = this.CharacterIndex_
        member this.CharacterType = this.CharacterType_
        member this.PartyIndex = match this.CharacterIndex with AllyIndex index | EnemyIndex index -> index
        member this.IsAlly = match this.CharacterIndex with AllyIndex _ -> true | EnemyIndex _ -> false
        member this.IsEnemy = not this.IsAlly
        member this.CelSize = this.CelSize_

        (* Animation Properties *)
        member this.TimeStart = this.CharacterAnimationState_.StartTime
        member this.AnimationSheet = this.CharacterAnimationState_.AnimationSheet
        member this.CharacterAnimationType = this.CharacterAnimationState_.CharacterAnimationType
        member this.Direction = this.CharacterAnimationState_.Direction

    let isFriendly (character : Character) (character2 : Character) =
        CharacterIndex.isFriendly character.CharacterIndex character2.CharacterIndex

    let getPoiseType character =
        character.CharacterState_

    let getAnimationInset time (character : Character) =
        CharacterAnimationState.inset time character.CelSize_ character.CharacterAnimationState_

    let getAnimationIndex time character =
        CharacterAnimationState.index time character.CharacterAnimationState_

    let getAnimationProgressOpt time character =
        CharacterAnimationState.progressOpt time character.CharacterAnimationState_

    let getAnimationFinished time character =
        CharacterAnimationState.getFinished time character.CharacterAnimationState_

    let face direction character =
        { character with CharacterAnimationState_ = CharacterAnimationState.face direction character.CharacterAnimationState_ }

    let animate time characterAnimationType character =
        { character with CharacterAnimationState_ = CharacterAnimationState.setCharacterAnimationType (Some time) characterAnimationType character.CharacterAnimationState_ }

    let make characterIndex characterType (characterState : PoiseType) animationSheet celSize direction =
        let animationType = IdleAnimation
        let animationState = { StartTime = 0L; AnimationSheet = animationSheet; CharacterAnimationType = animationType; Direction = direction }
        { CharacterIndex_ = characterIndex
          CharacterType_ = characterType
          CharacterState_ = characterState
          CharacterAnimationState_ = animationState
          CelSize_ = celSize }

    let empty =
        let characterAnimationState = { StartTime = 0L; AnimationSheet = asset "Field " "Jinn"; CharacterAnimationType = PoiseAnimation Poising; Direction = Downward }
        { CharacterIndex_ = AllyIndex 0
          CharacterType_ = Ally Jinn
          CharacterState_ = Poising
          CharacterAnimationState_ = characterAnimationState
          CelSize_ = v2 48.0f 48.0f }

type Character = Character.Character

// this is a custom entity for performance testing
type CustomModelDispatcher () =
    inherit StaticModelDispatcher ()

    override this.Update (entity, world) =
        entity.SetRotation (entity.GetRotation world * Quaternion.CreateFromAxisAngle (v3Up, 0.01f)) world

[<AutoOpen>]
module CharacterDispatcher =

    type Entity with
        member this.GetCharacter world = this.GetModelGeneric<Character> world
        member this.SetCharacter value world = this.SetModelGeneric<Character> value world
        member this.Character = this.ModelGeneric<Character> ()

    type CharacterDispatcher () =
        inherit EntityDispatcher3d<Character, unit, unit> (true, false, Character.empty)

        static let getSpriteInset (character : Character) world =
            Character.getAnimationInset (World.getUpdateTime world) character

        override this.Initializers (_, _) =
            [Entity.Presence == Omnipresent]

        override this.View (character, entity, world) =
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let renderMaterial =
                    { AlbedoOpt = Some Color.White
                      MetalnessOpt = Some 0.0f
                      RoughnessOpt = Some 1.25f
                      AmbientOcclusionOpt = Some 1.0f }
                let albedoImage = asset "Field" "Jinn"

                World.enqueueRenderMessage3d (SetImageMinFilter (OpenGL.TextureMinFilter.NearestMipmapNearest, albedoImage)) world
                World.enqueueRenderMessage3d (SetImageMagFilter (OpenGL.TextureMagFilter.Nearest, albedoImage)) world

                let inset = getSpriteInset character world
                let characterView =
                    Render3d (
                        RenderBillboardMessage
                            (transform.Absolute, transform.AffineMatrix, ValueSome inset, renderMaterial,
                             albedoImage, Assets.Default.MaterialMetalness, Assets.Default.MaterialRoughness, Assets.Default.MaterialRoughness, albedoImage,
                             DeferredRenderType))
                characterView
            else View.empty

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
type TacticsDispatcher () =
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
                 Content.entity<CharacterDispatcher> Gen.name
                    [Entity.Position == v3 0.0f 2.5f 0.0f]
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
