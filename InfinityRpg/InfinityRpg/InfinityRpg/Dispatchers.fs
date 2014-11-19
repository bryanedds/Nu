namespace InfinityRpg
open System
open System.ComponentModel
open SDL2
open OpenTK
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open Nu.Observer
open InfinityRpg
open InfinityRpg.Constants

[<AutoOpen>]
module FieldDispatcherModule =

    type Entity with
    
        member entity.FieldMapNp = entity?FieldMapNp : FieldMap
        static member setFieldMapNp (value : FieldMap) (entity : Entity) = entity?FieldMapNp <- value

    type FieldDispatcher () =
        inherit EntityDispatcher ()

        static let [<Literal>] FieldTileSheetRun = 4
        static let DefaultRand = Rand.makeDefault ()
        static let DefaultSize = Vector2i (4, 4)
        static let DefaultPathEdges = [(Vector2i (1, 1), Vector2i (2, 2))]
        static let DefaultFieldMap = FieldMap.make FieldTileSheetImage DefaultSize DefaultPathEdges DefaultRand

        static let getOptTileInset (tileSheetSize : Vector2i) (tileSize : Vector2i) (tileSheetCoords : Vector2i) =
            let tileOffset = Vector2i.Multiply (tileSheetCoords, tileSize)
            let tileInset =
                Vector4 (
                    single tileOffset.X,
                    single tileOffset.Y,
                    single <| tileOffset.X + tileSize.X,
                    single <| tileOffset.Y + tileSize.Y)
            Some tileInset

        static let getOptTileBodyProperties (field : Entity) (tileCoords : Vector2i) tile world =
            match tile.FieldTileType with
            | Impassable ->
                let fieldMap = field.FieldMapNp
                let fieldTileSheetAssetTag = Image.toAssetTag fieldMap.FieldTileSheet
                match Metadata.tryGetTextureSize fieldTileSheetAssetTag world.State.AssetMetadataMap with
                | Some tileSheetSize ->
                    let tileSize = tileSheetSize / FieldTileSheetRun
                    let tileOffset = Vector2i.Multiply (tileSize, tileCoords)
                    let tilePosition = Vector2i field.Position + tileOffset + tileSize / 2
                    let bodyProperties =
                        { BodyId = intsToGuid tileCoords.X tileCoords.Y
                          Position = tilePosition.Vector2
                          Rotation = field.Rotation
                          Shape = BoxShape { Extent = tileSize.Vector2 * 0.5f; Center = Vector2.Zero }
                          BodyType = BodyType.Static
                          Density = NormalDensity
                          Friction = field.Friction
                          Restitution = field.Restitution
                          FixedRotation = true
                          LinearDamping = 0.0f
                          AngularDamping = 0.0f
                          GravityScale = 0.0f
                          CollisionCategories = Physics.toCollisionCategories field.CollisionCategories
                          CollisionMask = Physics.toCollisionCategories field.CollisionMask
                          IsBullet = false
                          IsSensor = false }
                    Some bodyProperties
                | None -> note <| "Could not find tile sheet asset '" + acstring fieldTileSheetAssetTag + "'."; None
            | Passable -> None
        
        static let registerTilePhysics address (field : Entity) world =
            let bodyPropertyList =
                Map.fold
                    (fun bodyPropertyList tileCoords tile ->
                        match getOptTileBodyProperties field tileCoords tile world with
                        | Some bodyProperties -> bodyProperties :: bodyPropertyList
                        | None -> bodyPropertyList)
                    []
                    field.FieldMapNp.FieldTiles
            World.createBodies address field.Id bodyPropertyList world

        static let unregisterTilePhysics (field : Entity) world =
            let physicsIds =
                Map.fold
                    (fun physicsIds (tileCoords : Vector2i) tile ->
                        match tile.FieldTileType with
                        | Impassable ->
                            let physicsId = { SourceId = field.Id; BodyId = intsToGuid tileCoords.X tileCoords.Y }
                            physicsId :: physicsIds
                        | Passable -> physicsIds)
                    []
                    field.FieldMapNp.FieldTiles
            World.destroyBodies physicsIds world

        static member FieldDefinitions =
            [define? Friction 0.0f
             define? Restitution 0.0f
             define? CollisionCategories "1"
             define? CollisionMask "*"
             define? FieldMapNp DefaultFieldMap]

        override dispatcher.Register (address, field, world) =
            let world = registerTilePhysics address field world
            (field, world)

        override dispatcher.Unregister (_, field, world) =
            let world = unregisterTilePhysics field world
            (field, world)
            
        override dispatcher.PropagatePhysics (address, tileMap, world) =
            world |>
                unregisterTilePhysics tileMap |>
                registerTilePhysics address tileMap

        override dispatcher.GetRenderDescriptors (field, world) =
            if field.Visible then
                let fieldMap = field.FieldMapNp
                let fieldTileSheetAssetTag = Image.toAssetTag fieldMap.FieldTileSheet
                match Metadata.tryGetTextureSize fieldTileSheetAssetTag world.State.AssetMetadataMap with
                | Some tileSheetSize ->
                    let tileSize = tileSheetSize / FieldTileSheetRun
                    let size = Vector2i.Multiply (tileSize, fieldMap.FieldSize)
                    if Camera.inView3 field.ViewType field.Position size.Vector2 world.Camera then
                        let sprites =
                            Map.fold
                                (fun sprites tileCoords tile ->
                                    let tileOffset = Vector2i.Multiply (tileCoords, tileSize)
                                    let tilePosition = Vector2i field.Position + tileOffset
                                    let sprite =
                                        { Position = tilePosition.Vector2
                                          Size = tileSize.Vector2
                                          Rotation = field.Rotation
                                          ViewType = field.ViewType
                                          OptInset = getOptTileInset tileSheetSize tileSize tile.FieldTileSheetCoords
                                          Image = fieldMap.FieldTileSheet
                                          Color = Vector4.One }
                                    sprite :: sprites)
                                []
                                fieldMap.FieldTiles
                        [LayerableDescriptor { Depth = field.Depth; LayeredDescriptor = SpritesDescriptor sprites }]
                    else []
                | None -> []
            else []

        override dispatcher.GetQuickSize (field, world) =
            let fieldMap = field.FieldMapNp
            let fieldTileSheetAssetTag = Image.toAssetTag fieldMap.FieldTileSheet
            match Metadata.tryGetTextureSize fieldTileSheetAssetTag world.State.AssetMetadataMap with
            | Some tileSheetSize ->
                let tileSize = tileSheetSize / FieldTileSheetRun
                let size = Vector2i.Multiply (tileSize, fieldMap.FieldSize)
                Vector2 (single size.X, single size.Y)
            | None -> DefaultEntitySize

[<AutoOpen>]
module CharacterAnimationFacetModule =

    type CharacterAnimationType =
        | CharacterAnimationFacing
        | CharacterAnimationActing

    type CharacterAnimationState =
        { CharacterAnimationType : CharacterAnimationType
          CharacterAnimationDirection : Direction
          CharacterAnimationStutter : int
          ChangeTime : int64 }

    type Entity with
    
        member entity.CharacterAnimationState = entity?CharacterAnimationState : CharacterAnimationState
        static member setCharacterAnimationState (value : CharacterAnimationState) (entity : Entity) = entity?CharacterAnimationState <- value
        member entity.CharacterAnimationSheet = entity?CharacterAnimationSheet : Image
        static member setCharacterAnimationSheet (value : Image) (entity : Entity) = entity?CharacterAnimationSheet <- value

    type CharacterAnimationFacet () =
        inherit Facet ()
        
        static let getOptSpriteInset (entity : Entity) world =
            let imageAssetTag = Image.toAssetTag entity.CharacterAnimationSheet
            let imageSize = Metadata.getTextureSizeAsVector2 imageAssetTag world.State.AssetMetadataMap
            let spriteSize = imageSize * 0.25f
            let animationState = entity.CharacterAnimationState
            let animationTypeCoordsOffset =
                match animationState.CharacterAnimationType with
                | CharacterAnimationFacing -> Vector2i (0, 0)
                | CharacterAnimationActing -> Vector2i (0, 2)
            let directionCoordsOffset =
                match animationState.CharacterAnimationDirection with
                | North -> Vector2i (0, 0)
                | East -> Vector2i (2, 0)
                | South -> Vector2i (0, 1)
                | West -> Vector2i (2, 1)
            let frameXOffset =
                (int world.State.TickTime - int animationState.ChangeTime) /
                animationState.CharacterAnimationStutter % 2
            let frameCoordsOffset = Vector2i (frameXOffset, 0)
            let spriteCoordsinates = animationTypeCoordsOffset + directionCoordsOffset + frameCoordsOffset
            let spriteOffset =
                Vector2 (
                    spriteSize.X * single spriteCoordsinates.X,
                    spriteSize.Y * single spriteCoordsinates.Y)
            let spriteInset =
                Vector4 (
                    spriteOffset.X,
                    spriteOffset.Y,
                    spriteOffset.X + spriteSize.X,
                    spriteOffset.Y + spriteSize.Y)
            Some spriteInset

        static member FieldDefinitions =
            [define?
                CharacterAnimationState
                { CharacterAnimationType = CharacterAnimationFacing
                  CharacterAnimationDirection = North
                  CharacterAnimationStutter = 16
                  ChangeTime = 0L }
             define? CharacterAnimationSheet PlayerImage]

        override facet.GetRenderDescriptors (entity, world) =
            if entity.Visible && Camera.inView3 entity.ViewType entity.Position entity.Size world.Camera then
                [LayerableDescriptor
                    { Depth = entity.Depth
                      LayeredDescriptor =
                        SpriteDescriptor
                            { Position = entity.Position
                              Size = entity.Size
                              Rotation = entity.Rotation
                              ViewType = entity.ViewType
                              OptInset = getOptSpriteInset entity world
                              Image = entity.CharacterAnimationSheet
                              Color = Vector4.One }}]
            else []

[<AutoOpen>]
module CharacterControlFacetModule =

    type CharacterControlFacet () =
        inherit Facet ()

        static let [<Literal>] WalkForce = 200.0f

        static let handleKeyboardKeyChange event world =
            let (address, character : Entity, keyData) = World.unwrapASD event world
            if not keyData.IsRepeat then
                let xForce =
                    if World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_RIGHT) world then WalkForce
                    elif World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_LEFT) world then -WalkForce
                    else 0.0f
                let yForce =
                    if World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_UP) world then WalkForce
                    elif World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_DOWN) world then -WalkForce
                    else 0.0f
                let force = Vector2 (xForce, yForce)
                let world = World.setBodyLinearVelocity force character.PhysicsId world
                let facingDirection =
                    if force.X > 0.0f then East
                    elif force.X < 0.0f then West
                    elif force.Y > 0.0f then North
                    elif force.Y < 0.0f then South
                    else character.CharacterAnimationState.CharacterAnimationDirection
                let characterState = { character.CharacterAnimationState with CharacterAnimationDirection = facingDirection }
                let character = Entity.setCharacterAnimationState characterState character
                let world = World.setEntity address character world
                (Cascade, world)
            else (Cascade, world)

        static member RequiredDispatcherName =
            "CharacterDispatcher"

        override facet.Register (address, entity, world) =
            let world = observe address KeyboardKeyChangeEventAddress |> monitor handleKeyboardKeyChange world |> snd
            (entity, world)

[<AutoOpen>]
module CharacterCameraFacetModule =

    type CharacterCameraFacet () =
        inherit Facet ()

        static let handleTick event world =
            let character : Entity = World.unwrapS event world
            let eyeCenter = character.Position + character.Size * 0.5f
            let eyeCenter =
                match World.getOptEntity FieldAddress world with
                | Some field ->
                    let eyeSize = world.Camera.EyeSize
                    let eyeCornerNegative = eyeCenter - eyeSize * 0.5f
                    let eyeCornerPositive = eyeCenter + eyeSize * 0.5f
                    let fieldCornerNegative = field.Position
                    let fieldCornerPositive = field.Position + field.Size
                    let fieldBoundsNegative = fieldCornerNegative + eyeSize * 0.5f
                    let fieldBoundsPositive = fieldCornerPositive - eyeSize * 0.5f
                    let eyeCenterX =
                        if eyeCornerNegative.X < fieldCornerNegative.X then fieldBoundsNegative.X
                        elif eyeCornerPositive.X > fieldCornerPositive.X then fieldBoundsPositive.X
                        else eyeCenter.X
                    let eyeCenterY =
                        if eyeCornerNegative.Y < fieldCornerNegative.Y then fieldBoundsNegative.Y
                        elif eyeCornerPositive.Y > fieldCornerPositive.Y then fieldBoundsPositive.Y
                        else eyeCenter.Y
                    Vector2 (eyeCenterX, eyeCenterY)
                | None -> eyeCenter
            let camera = { world.Camera with EyeCenter = eyeCenter }
            (Cascade, World.setCamera camera world)

        override facet.Register (address, entity, world) =
            let world = observe address TickEventAddress |> monitor handleTick world |> snd
            (entity, world)

[<AutoOpen>]
module CharacterDispatcherModule =

    type CharacterDispatcher () =
        inherit EntityDispatcher ()

        static member FieldDefinitions =
            [define? GravityScale 0.0f
             define? LinearDamping 0.0f
             define? FixedRotation true
             define? CollisionExpr "Circle"]

        static member IntrinsicFacetNames =
            [typeof<RigidBodyFacet>.Name
             typeof<CharacterAnimationFacet>.Name
             typeof<CharacterControlFacet>.Name
             typeof<CharacterCameraFacet>.Name]

[<AutoOpen>]
module PlayerCharacterDispatcherModule =

    type PlayerCharacterDispatcher () =
        inherit CharacterDispatcher ()

[<AutoOpen>]
module InfinityRpgModule =

    type Game with
    
        member game.Seed = game?Seed : uint64
        static member setSeed (value : uint64) (game : Game) = game?Seed <- value

    type InfinityRpgDispatcher () =
        inherit GameDispatcher ()

        static member FieldDefinitions =
            [define? Seed Rand.DefaultSeed]