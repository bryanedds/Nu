namespace BlazeVector
open System
open OpenTK
open SDL2
open Prime
open Nu
open BlazeVector

[<AutoOpen>]
module BulletModule =

    type Entity with
    
        member this.GetAge world : int64 = (this.GetXtension world)?Age
        member this.SetAge (value : int64) world = this.UpdateXtension (fun xtension -> xtension?Age <- value) world

    type BulletDispatcher () =
        inherit EntityDispatcher ()

        static let [<Literal>] BulletLifetime = 27L

        static let handleUpdate event world =
            let bullet = event.Subscriber : Entity
            let world = bullet.SetAge (bullet.GetAge world + World.getTickRate world) world
            let world =
                if bullet.GetAge world > BulletLifetime
                then World.destroyEntity bullet world
                else world
            (Cascade, world)

        static let handleCollision event world =
            let bullet = event.Subscriber : Entity
            if World.isTicking world then
                let world = World.destroyEntity bullet world
                (Cascade, world)
            else (Cascade, world)

        static member FieldDefinitions =
            [define? Size ^ Vector2 (24.0f, 24.0f)
             define? Density 0.25f
             define? Restitution 0.5f
             define? LinearDamping 0.0f
             define? GravityScale 0.0f
             define? IsBullet true
             define? CollisionExpr "[BodyCircle [0.5 [0.0 0.0]]]"
             define? SpriteImage Constants.Assets.PlayerBulletImage
             define? Age 0L]

        static member IntrinsicFacetNames =
            [typeof<RigidBodyFacet>.Name
             typeof<SpriteFacet>.Name]

        override dispatcher.Register (bullet, world) =
            world |>
                World.monitor handleUpdate Events.Update bullet |>
                World.monitor handleCollision (Events.Collision ->- bullet) bullet

[<AutoOpen>]
module EnemyModule =

    type Entity with
    
        member this.GetHealth world : int = (this.GetXtension world)?Health
        member this.SetHealth (value : int) world = this.UpdateXtension (fun xtension -> xtension?Health <- value) world
        
        member this.IsOnScreen world =
            let cameraBounds = World.getCameraBy Camera.getViewBoundsRelative world
            Math.isPointInBounds (this.GetCenter world) cameraBounds

    type EnemyDispatcher () =
        inherit EntityDispatcher ()
        
        static let move (enemy : Entity) world =
            let tickRate = World.getTickRate world
            let force = Vector2 (-2000.0f, -20000.0f) * single tickRate
            World.applyBodyForce force (enemy.GetPhysicsId world) world

        static let die (enemy : Entity) world =
            let world = World.destroyEntity enemy world
            World.playSound 1.0f Constants.Assets.ExplosionSound world

        static let handleUpdate event world =
            let enemy = event.Subscriber : Entity
            let world = if enemy.IsOnScreen world then move enemy world else world
            let world = if enemy.GetHealth world <= 0 then die enemy world else world
            (Cascade, world)

        static let handleCollision event world =
            let enemy = event.Subscriber : Entity
            if World.isTicking world then
                let collidee = event.Data.Collidee
                let isBullet = collidee.DispatchesAs typeof<BulletDispatcher> world
                if isBullet then
                    let world = enemy.SetHealth (enemy.GetHealth world - 1) world
                    let world = World.playSound 1.0f Constants.Assets.HitSound world
                    (Cascade, world)
                else (Cascade, world)
            else (Cascade, world)

        static member FieldDefinitions =
            [define? Size ^ Vector2 (48.0f, 96.0f)
             define? FixedRotation true
             define? LinearDamping 3.0f
             define? GravityScale 0.0f
             define? CollisionExpr "[BodyCapsule [0.5 0.25 [0.0 0.0]]]"
             define? TileCount 6
             define? TileRun 4
             define? TileSize ^ Vector2 (48.0f, 96.0f)
             define? AnimationStutter 8L
             define? AnimationSheet Constants.Assets.EnemyImage
             define? Health 6]

        static member IntrinsicFacetNames =
            [typeof<RigidBodyFacet>.Name
             typeof<AnimatedSpriteFacet>.Name]

        override dispatcher.Register (enemy, world) =
            world |>
                World.monitor handleUpdate Events.Update enemy |>
                World.monitor handleCollision (Events.Collision ->- enemy) enemy

[<AutoOpen>]
module PlayerModule =

    type Entity with
    
        member this.GetLastTimeOnGroundNp world : int64 = (this.GetXtension world)?LastTimeOnGroundNp
        member this.SetLastTimeOnGroundNp (value : int64) world = this.UpdateXtension (fun xtension -> xtension?LastTimeOnGroundNp <- value) world
        member this.GetLastTimeJumpNp world : int64 = (this.GetXtension world)?LastTimeJumpNp
        member this.SetLastTimeJumpNp (value : int64) world = this.UpdateXtension (fun xtension -> xtension?LastTimeJumpNp <- value) world
        member this.HasFallen world = (this.GetPosition world).Y < -600.0f

    type PlayerDispatcher () =
        inherit EntityDispatcher ()

        static let [<Literal>] WalkForce = 8000.0f
        static let [<Literal>] FallForce = -30000.0f
        static let [<Literal>] ClimbForce = 12000.0f

        static let createBullet (playerTransform : Transform) world =
            let (bullet, world) = World.createEntity typeof<BulletDispatcher>.Name None None Simulants.GameplayScene world
            let bulletPosition = playerTransform.Position + Vector2 (playerTransform.Size.X * 0.9f, playerTransform.Size.Y * 0.4f)
            let world = bullet.SetPosition bulletPosition world
            let world = bullet.SetDepth playerTransform.Depth world
            let world = World.propagateEntityPhysics bullet world
            (bullet, world)

        static let propelBullet (bullet : Entity) world =
            let world = World.applyBodyLinearImpulse (Vector2 (50.0f, 0.0f)) (bullet.GetPhysicsId world) world
            World.playSound 1.0f Constants.Assets.ShotSound world

        static let shootBullet (player : Entity) world =
            let playerTransform = player.GetTransform world
            let (bullet, world) = createBullet playerTransform world
            propelBullet bullet world

        static let handleSpawnBullet event world =
            let player = event.Subscriber : Entity
            if World.isTicking world then
                if not ^ player.HasFallen world then
                    if World.getTickTime world % 6L = 0L then
                        let world = shootBullet player world
                        (Cascade, world)
                    else (Cascade, world)
                else (Cascade, world)
            else (Cascade, world)

        static let getLastTimeOnGround (player : Entity) world =
            if not ^ World.bodyOnGround (player.GetPhysicsId world) world
            then player.GetLastTimeOnGroundNp world
            else World.getTickTime world

        static let handleMovement event world =
            let player = event.Subscriber : Entity
            let lastTimeOnGround = getLastTimeOnGround player world
            let world = player.SetLastTimeOnGroundNp lastTimeOnGround world
            let physicsId = player.GetPhysicsId world
            let optGroundTangent = World.getBodyOptGroundContactTangent physicsId world
            let force =
                match optGroundTangent with
                | Some groundTangent ->
                    let downForce = if groundTangent.Y > 0.0f then ClimbForce else 0.0f
                    let vectorForce = Vector2.Multiply (groundTangent, Vector2 (WalkForce, downForce))
                    vectorForce * World.getTickRateF world
                | None -> Vector2 (WalkForce, FallForce)
            let world = World.applyBodyForce force physicsId world
            (Cascade, world)

        static let handleJump event world =
            let player = event.Subscriber : Entity
            let tickTime = World.getTickTime world
            if  tickTime >= player.GetLastTimeJumpNp world + 12L &&
                tickTime <= player.GetLastTimeOnGroundNp world + 10L then
                let world = player.SetLastTimeJumpNp tickTime world
                let world = World.applyBodyLinearImpulse (Vector2 (0.0f, 18000.0f)) (player.GetPhysicsId world) world
                let world = World.playSound 1.0f Constants.Assets.JumpSound world
                (Cascade, world)
            else (Cascade, world)

        static let handleJumpByKeyboardKey event world =
            if World.isSelectedScreenIdling world then
                match (enum<SDL.SDL_Scancode> event.Data.ScanCode, event.Data.Repeated) with
                | (SDL.SDL_Scancode.SDL_SCANCODE_SPACE, false) -> handleJump event world
                | _ -> (Cascade, world)
            else (Cascade, world)

        static member FieldDefinitions =
            [define? Size ^ Vector2 (48.0f, 96.0f)
             define? FixedRotation true
             define? LinearDamping 3.0f
             define? GravityScale 0.0f
             define? CollisionExpr "[BodyCapsule [0.5 0.25 [0.0 0.0]]]"
             define? TileCount 16
             define? TileRun 4
             define? TileSize ^ Vector2 (48.0f, 96.0f)
             define? AnimationStutter 3L
             define? AnimationSheet Constants.Assets.PlayerImage
             define? LastTimeOnGroundNp Int64.MinValue
             define? LastTimeJumpNp Int64.MinValue]

        static member IntrinsicFacetNames =
            [typeof<RigidBodyFacet>.Name
             typeof<AnimatedSpriteFacet>.Name]

        override dispatcher.Register (player, world) =
            world |>
                World.monitor handleSpawnBullet Events.Update player |>
                World.monitor handleMovement Events.Update player |>
                World.monitor handleJump Events.MouseLeftDown player |>
                World.monitor handleJumpByKeyboardKey Events.KeyboardKeyDown player

[<AutoOpen>]
module PlayerGroupModule =

    type PlayerGroupDispatcher () =
        inherit GroupDispatcher ()

        static let adjustCamera world =
            World.updateCamera
                (fun camera -> 
                    let playerPosition = Simulants.Player.GetPosition world
                    let playerSize = Simulants.Player.GetSize world
                    let eyeCenter = Vector2 (playerPosition.X + playerSize.X * 0.5f + camera.EyeSize.X * 0.33f, camera.EyeCenter.Y)
                    { camera with EyeCenter = eyeCenter })
                world

        static let handleAdjustCamera _ world =
            (Cascade, adjustCamera world)

        static let handlePlayerFall _ world =
            if Simulants.Player.HasFallen world && World.isSelectedScreenIdling world then
                let world = World.playSound 1.0f Constants.Assets.DeathSound world
                let world = World.transitionScreen Simulants.Title world
                (Cascade, world)
            else (Cascade, world)

        override dispatcher.Register (group, world) =
            world |>
                World.monitor handleAdjustCamera Events.Update group |>
                World.monitor handlePlayerFall Events.Update group

[<AutoOpen>]
module GameplayScreenModule =

    type GameplayScreenDispatcher () =
        inherit ScreenDispatcher ()

        static let [<Literal>] SectionName = "Section"
        static let [<Literal>] SectionXShift = 2048.0f

        static let shiftEntities xShift entities world =
            Seq.fold
                (fun world (entity : Entity) ->
                    let world = entity.SetPosition (entity.GetPosition world + Vector2 (xShift, 0.0f)) world
                    World.propagateEntityPhysics entity world)
                world
                entities

        static let createSectionFromFile filePath sectionName xShift world =
            let (section, world) = World.readGroupFromFile filePath (Some sectionName) Simulants.Gameplay world
            let sectionEntities = World.proxyEntities section world
            shiftEntities xShift sectionEntities world

        static let createSectionGroups world =
            let random = Random ()
            let sectionFilePaths = List.toArray Constants.FilePaths.Sections
            List.fold
                (fun world i ->
                    let sectionFilePathIndex = if i = 0 then 0 else random.Next () % sectionFilePaths.Length
                    let sectionFilePath = sectionFilePaths.[sectionFilePathIndex]
                    let sectionName = SectionName + acstring i
                    let sectionXShift = SectionXShift * single i
                    createSectionFromFile sectionFilePath sectionName sectionXShift world)
                world
                [0 .. Constants.BlazeVector.SectionCount - 1]

        static let createPlayerGroup world =
            World.readGroupFromFile Constants.FilePaths.PlayerGroup (Some Simulants.GameplayScene.GroupName) Simulants.Gameplay world |> snd

        static let handleStartPlay _ world =
            let world = createPlayerGroup world
            let world = createSectionGroups world
            let world = World.playSong 0 1.0f Constants.Assets.DeadBlazeSong world
            (Cascade, world)

        static let handleStoppingPlay _ world =
            let world = World.fadeOutSong Constants.Audio.DefaultTimeToFadeOutSongMs world
            (Cascade, world)

        static let handleStopPlay event world =
            let screen = event.Subscriber : Screen
            let sectionNames = [for i in 0 .. Constants.BlazeVector.SectionCount - 1 do yield SectionName + acstring i]
            let groupNames = Simulants.GameplayScene.GroupName :: sectionNames
            let groups = List.map (fun groupName -> screen => groupName) groupNames
            let world = World.destroyGroups groups world
            (Cascade, world)

        override dispatcher.Register (screen, world) =
            world |>
                World.monitor handleStartPlay (Events.Select ->- screen) screen |>
                World.monitor handleStoppingPlay (Events.OutgoingStart ->- screen) screen |>
                World.monitor handleStopPlay (Events.Deselect ->- screen) screen