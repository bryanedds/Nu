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

        static member FieldDefinitions =
            [define? FieldMapNp DefaultFieldMap]

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
module CharacterStateFacetModule =

    type Entity with
    
        member entity.CharacterType = entity?CharacterType : CharacterType
        static member setCharacterType (value : CharacterType) (entity : Entity) = entity?CharacterType <- value
        member entity.ActivityState = entity?ActivityState : ActivityState
        static member setActivityState (value : ActivityState) (entity : Entity) = entity?ActivityState <- value
        member entity.HitPoints = entity?HitPoints : int
        static member setHitPoints (value : int) (entity : Entity) = entity?HitPoints <- value
        member entity.SpecialPoints = entity?SpecialPoints : int
        static member setSpecialPoints (value : int) (entity : Entity) = entity?SpecialPoints <- value
        member entity.PowerBuff = entity?PowerBuff : single
        static member setPowerBuff (value : single) (entity : Entity) = entity?PowerBuff <- value
        member entity.ShieldBuff = entity?ShieldBuff : single
        static member setShieldBuff (value : single) (entity : Entity) = entity?ShieldBuff <- value
        member entity.MindBuff = entity?MindBuff : single
        static member setMindBuff (value : single) (entity : Entity) = entity?MindBuff <- value
        member entity.CounterBuff = entity?CounterBuff : single
        static member setCounterBuff (value : single) (entity : Entity) = entity?CounterBuff <- value
        member entity.Statuses = entity?Statuses : StatusType Set
        static member setStatuses (value : StatusType Set) (entity : Entity) = entity?Statuses <- value
        member entity.EquippedWeapon = entity?EquippedWeapon : WeaponType option
        static member setEquippedWeapon (value : WeaponType option) (entity : Entity) = entity?EquippedWeapon <- value
        member entity.EquippedArmor = entity?EquippedArmor : ArmorType option
        static member setEquippedArmor (value : ArmorType option) (entity : Entity) = entity?EquippedArmor <- value
        member entity.EquippedRelics = entity?EquippedRelics : RelicType list
        static member setEquippedRelics (value : RelicType list) (entity : Entity) = entity?EquippedRelics <- value
        member entity.AddedExperience = entity?AddedExperience : int
        static member setAddedExperience (value : int) (entity : Entity) = entity?AddedExperience <- value

    type CharacterStateFacet () =
        inherit Facet ()

        static member FieldDefinitions =
            [define? CharacterType Player
             define? ActivityState Standing
             define? HitPoints 1 // hp max is calculated
             define? SpecialPoints 1 // sp max is calculated
             define? PowerBuff 1.0f // rate at which power is buffed / debuffed
             define? ShieldBuff 1.0f // rate at which shield is buffed / debuffed
             define? MindBuff 1.0f // rate at which mind is buffed / debuffed
             define? CounterBuff 1.0f // rate at which counter is buffed / debuffed
             define? Statuses Set.empty<StatusType>
             define? EquippedWeapon Option<WeaponType>.None
             define? EquippedArmor Option<ArmorType>.None
             define? EquippedRelics list<RelicType>.Empty
             define? AddedExperience 0] // level is calculated from base experience + added experience

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
module CharacterControlFacetModule =

    type Arrival =
        | Arriving
        | Arrived

    type CharacterControlFacet () =
        inherit Facet ()

        static let [<Literal>] WalkSpeed = 4.0f
        static let [<Literal>] WalkLength = 64.0f // TODO: get this from map tile size?

        static let walk positive current destination =
            let (walkSpeed, delta) =
                if positive
                then (WalkSpeed, destination - current)
                else (-WalkSpeed, current - destination)
            let next = current + walkSpeed
            let newDelta = if positive then destination - next else next - destination
            if newDelta < WalkSpeed
            then (destination, Arrived)
            else (next, Arriving)

        static let checkOpenDirection currentTile fieldTiles direction =
            let directionVector = Direction.toVector2i direction
            match Map.tryFind (currentTile + directionVector) fieldTiles with
            | Some tile -> tile.FieldTileType = Passable
            | None -> true

        static let getOpenDirections (position : Vector2) world =
            match World.getOptEntity FieldAddress world with
            | Some field ->
                let currentTile =
                    Vector2i
                        (int <| position.X / WalkLength,
                         int <| position.Y / WalkLength)
                let fieldTiles = field.FieldMapNp.FieldTiles
                Set.ofSeq <|
                    seq {
                        if checkOpenDirection currentTile fieldTiles North then yield North
                        if checkOpenDirection currentTile fieldTiles East then yield East
                        if checkOpenDirection currentTile fieldTiles South then yield South
                        if checkOpenDirection currentTile fieldTiles West then yield West }
            | None -> Set.empty

        static let tickInput address (character : Entity) world =
            let openDirections = getOpenDirections character.Position world
            let optWalkDirection =
                if World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_UP) world then Some North
                elif World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_DOWN) world then Some South
                elif World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_RIGHT) world then Some East
                elif World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_LEFT) world then Some West
                else None
            let optWalkDestination =
                match optWalkDirection with
                | Some walkDirection ->
                    match walkDirection with
                    | North -> if Set.contains North openDirections then Some <| character.Position.Y + WalkLength else None
                    | East -> if Set.contains East openDirections then Some <| character.Position.X + WalkLength else None
                    | South -> if Set.contains South openDirections then Some <| character.Position.Y - WalkLength else None
                    | West -> if Set.contains West openDirections then Some <| character.Position.X - WalkLength else None
                | None -> None
            let characterAnimationState = character.CharacterAnimationState
            let characterAnimationState =
                match optWalkDirection with
                | Some walkDirection -> {characterAnimationState  with CharacterAnimationDirection = walkDirection }
                | None -> characterAnimationState
            let character = Entity.setCharacterAnimationState characterAnimationState character
            let character =
                match (optWalkDirection, optWalkDestination) with
                | (Some walkDirection, Some walkDestination) ->
                    let activityState = Walking { WalkDirection = walkDirection; WalkDestination = walkDestination }
                    Entity.setActivityState activityState character
                | (None, Some _) ->
                    failwith <|
                        "Unexpected match in InfinityRpg.CharacterControlFacet.tickInput. " +
                        "Logically, this match should never happen."
                | (Some _, None) -> character
                | (None, None) -> character
            World.setEntity address character world

        static let tickWalking address (character : Entity) walkDescriptor world =
            let (newPosition, arrival) =
                match walkDescriptor.WalkDirection with
                | North -> let (newY, arrival) = walk true character.Position.Y walkDescriptor.WalkDestination in (Vector2 (character.Position.X, newY), arrival)
                | East -> let (newX, arrival) = walk true character.Position.X walkDescriptor.WalkDestination in (Vector2 (newX, character.Position.Y), arrival)
                | South -> let (newY, arrival) = walk false character.Position.Y walkDescriptor.WalkDestination in (Vector2 (character.Position.X, newY), arrival)
                | West -> let (newX, arrival) = walk false character.Position.X walkDescriptor.WalkDestination in (Vector2 (newX, character.Position.Y), arrival)
            let character = Entity.setPosition newPosition character
            let character = match arrival with Arriving -> character | Arrived -> Entity.setActivityState Standing character
            let world = World.setEntity address character world
            match character.ActivityState with
            | Standing -> tickInput address character world
            | Walking _ | Acting _ -> world

        static let handleTick event world =
            let (address, character : Entity) = World.unwrapAS event world
            match character.ActivityState with
            | Standing -> (Cascade, tickInput address character world)
            | Walking walkDescriptor -> (Cascade, tickWalking address character walkDescriptor world)
            | Acting _ -> (Cascade, world)

        static member RequiredDispatcherName =
            "CharacterDispatcher"

        override facet.Register (address, entity, world) =
            let world = observe address TickEventAddress |> filter isSelectedScreenIdling |> monitor handleTick world |> snd
            (entity, world)

[<AutoOpen>]
module CharacterDispatcherModule =

    type CharacterDispatcher () =
        inherit EntityDispatcher ()

        static member IntrinsicFacetNames =
            [typeof<CharacterStateFacet>.Name
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