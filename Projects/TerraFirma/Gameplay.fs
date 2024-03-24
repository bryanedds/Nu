namespace TerraFirma
open System
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module Gameplay =

    // this represents that state of the simulation during gameplay.
    type GameplayState =
        | Playing
        | Quitting
        | Quit

    // this is our MMCC model type representing gameplay.
    // this model representation uses update time, that is, time based on number of engine updates.
    // if you wish to use clock time instead (https://github.com/bryanedds/Nu/wiki/GameTime-and-its-Polymorphic-Nature),
    // you could use `GameplayTime : single` instead.
    type Gameplay =
        { GameplayTime : int64
          GameplayState : GameplayState
          Player : Character
          Enemies : HMap<Guid, Character> }

        static member initial =
            let enemies =
                [for i in 0 .. dec 7 do
                    for j in 0 .. dec 7 do
                        let enemy =
                            { Character.initial with
                                Position = v3 (single i * 8.0f - 8.0f) 2.0f (single j * 8.0f - 8.0f)
                                FollowTargetOpt = Some Simulants.GameplayPlayer }
                        (makeGuid (), enemy)]
            { GameplayTime = 0L
              GameplayState = Quit
              Player = { Character.initial with Position = v3 0.0f 2.0f 0.0f }
              Enemies = HMap.ofList enemies }

        static member start =
            let initial = Gameplay.initial
            { initial with GameplayState = Playing }

    // this is our MMCC message type.
    type GameplayMessage =
        | UpdatePlayerInputKey of KeyboardKeyData
        | UpdatePhysics of IntegrationData
        | Update
        | StartQuitting
        | FinishQuitting
        interface Message

    // this is our MMCC command type.
    type GameplayCommand =
        | JumpPlayer
        | PostUpdateEye
        interface Command

    // this extends the Screen API to expose the above Gameplay model.
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()

    // this is the screen dispatcher that defines the screen where gameplay takes place. Note that we just use the
    // empty Command type because there are no commands needed for this template.
    type GameplayDispatcher () =
        inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> (Gameplay.initial)

        static let [<Literal>] WalkSpeed = 0.06f
        static let [<Literal>] TurnSpeed = 0.035f
        static let [<Literal>] JumpSpeed = 6.0f

        static let computeCharacterTraversalAnimations (character : Character) =
            let linearVelocityAvg = (character.LinearVelocity + character.LinearVelocityPrevious) * 0.5f
            let angularVelocityAvg = (character.AngularVelocity + character.AngularVelocityPrevious) * 0.5f
            let forwardness = (Vector3.Dot (linearVelocityAvg * 32.0f, character.Rotation.Forward))
            let backness = (Vector3.Dot (linearVelocityAvg * 32.0f, -character.Rotation.Forward))
            let rightness = (Vector3.Dot (linearVelocityAvg * 32.0f, character.Rotation.Right))
            let leftness = (Vector3.Dot (linearVelocityAvg * 32.0f, -character.Rotation.Right))
            let turnRightness = (angularVelocityAvg * v3Up).Length () * 48.0f
            let turnLeftness = -turnRightness
            let animations =
                [{ StartTime = 0L; LifeTimeOpt = None; Name = "Armature|Idle"; Playback = Loop; Rate = 1.0f; Weight = 0.5f; BoneFilterOpt = None }]
            let animations =
                if forwardness >= 0.01f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|WalkForward"; Playback = Loop; Rate = 1.0f; Weight = max 0.025f forwardness; BoneFilterOpt = None } :: animations
                elif backness >= 0.01f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|WalkBack"; Playback = Loop; Rate = 1.0f; Weight = max 0.025f backness; BoneFilterOpt = None } :: animations
                else animations
            let animations =
                if rightness >= 0.01f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|WalkRight"; Playback = Loop; Rate = 1.0f; Weight = max 0.025f rightness; BoneFilterOpt = None } :: animations
                elif leftness >= 0.01f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|WalkLeft"; Playback = Loop; Rate = 1.0f; Weight = max 0.025f leftness; BoneFilterOpt = None } :: animations
                else animations
            let animations =
                if turnRightness >= 0.01f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|TurnRight"; Playback = Loop; Rate = 1.0f; Weight = max 0.025f turnRightness; BoneFilterOpt = None } :: animations
                elif turnLeftness >= 0.01f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|TurnLeft"; Playback = Loop; Rate = 1.0f; Weight = max 0.025f turnLeftness; BoneFilterOpt = None } :: animations
                else animations
            animations

        static let tryComputeCharacterActionAnimation time attackOpt world =
            match attackOpt with
            | Some attack ->
                let localTime = time - attack.AttackTime
                let world =
                    match localTime with
                    | 0L -> World.playSound 1.0f Assets.Default.Sound world
                    | 55L -> if attack.FollowUpBuffered then World.playSound 1.0f Assets.Default.Sound world else world
                    | _ -> world
                let animationStartTime = GameTime.ofUpdates (world.UpdateTime - localTime % 55L)
                let animationName = if localTime <= 55 then "Armature|AttackVertical" else "Armature|AttackHorizontal"
                let animation = { StartTime = animationStartTime; LifeTimeOpt = None; Name = animationName; Playback = Once; Rate = 1.0f; Weight = 32.0f; BoneFilterOpt = None }
                Some animation
            | None -> None

        static let computePlayerMovement walkSpeed turnSpeed (player : Character) grounded world =
            if player.AttackOpt.IsNone || not grounded then

                // compute position
                let forward = player.Rotation.Forward
                let right = player.Rotation.Right
                let walkSpeed = if grounded then walkSpeed else walkSpeed * 0.75f
                let walkVelocity =
                    (if World.isKeyboardKeyDown KeyboardKey.W world || World.isKeyboardKeyDown KeyboardKey.Up world then forward * walkSpeed else v3Zero) +
                    (if World.isKeyboardKeyDown KeyboardKey.S world || World.isKeyboardKeyDown KeyboardKey.Down world then -forward * walkSpeed else v3Zero) +
                    (if World.isKeyboardKeyDown KeyboardKey.A world then -right * walkSpeed else v3Zero) +
                    (if World.isKeyboardKeyDown KeyboardKey.D world then right * walkSpeed else v3Zero)
                let position = if walkVelocity <> v3Zero then player.Position + walkVelocity else player.Position

                // compute rotation
                let turnSpeed = if grounded then turnSpeed else turnSpeed * 0.75f
                let turnVelocity =
                    (if World.isKeyboardKeyDown KeyboardKey.Right world then -turnSpeed else 0.0f) +
                    (if World.isKeyboardKeyDown KeyboardKey.Left world then turnSpeed else 0.0f)
                let rotation = if turnVelocity <> 0.0f then player.Rotation * Quaternion.CreateFromAxisAngle (v3Up, turnVelocity) else player.Rotation
                (position, rotation)

            else (player.Position, player.Rotation)

        static let updateCharacterPhysics time character (entity : Entity) world =
            let bodyId = entity.GetBodyId world
            let linearVelocity = World.getBodyLinearVelocity bodyId world
            let angularVelocity = World.getBodyAngularVelocity bodyId world
            let grounded = World.getBodyGrounded bodyId world
            { character with
                LinearVelocity = linearVelocity
                LinearVelocityPrevious = character.LinearVelocity
                AngularVelocity = angularVelocity
                AngularVelocityPrevious = character.AngularVelocity
                Jump.LastTimeOnGround = if grounded then time else character.Jump.LastTimeOnGround }

        // here we define the screen's properties and event handling
        override this.Initialize (_, _) =
            [Game.KeyboardKeyDownEvent =|> fun evt -> UpdatePlayerInputKey evt.Data
             Game.IntegrationEvent =|> fun evt -> UpdatePhysics evt.Data
             Screen.UpdateEvent => Update
             Screen.PostUpdateEvent => PostUpdateEye
             Screen.DeselectingEvent => FinishQuitting]

        // here we handle the above messages
        override this.Message (gameplay, message, _, world) =

            match message with
            | UpdatePlayerInputKey keyboardKeyData ->
                let time = gameplay.GameplayTime
                let sinceJump = time - gameplay.Player.Jump.LastTime
                let sinceOnGround = time - gameplay.Player.Jump.LastTimeOnGround
                if keyboardKeyData.KeyboardKey = KeyboardKey.Space && not keyboardKeyData.Repeated && sinceJump >= 12L && sinceOnGround < 10L then
                    let gameplay = { gameplay with Player.Jump.LastTime = time }
                    withSignal JumpPlayer gameplay
                elif keyboardKeyData.KeyboardKey = KeyboardKey.Rshift && not keyboardKeyData.Repeated then
                    let gameplay =
                        match gameplay.Player.AttackOpt with
                        | Some attack ->
                            let localTime = time - attack.AttackTime
                            if localTime > 15L && not attack.FollowUpBuffered
                            then { gameplay with Player.AttackOpt = Some { attack with FollowUpBuffered = true }}
                            else gameplay
                        | None ->
                            { gameplay with Player.AttackOpt = Some (AttackState.make time) }
                    just gameplay
                else just gameplay

            | UpdatePhysics integrationData ->
                let gameplay =
                    SArray.fold (fun gameplay integrationMessage ->
                        match integrationMessage with
                        | BodyTransformMessage bodyTrandformMessage ->
                            let bodyId = bodyTrandformMessage.BodyId
                            match bodyId.BodySource with
                            | :? Entity as entity ->
                                if entity.Name = Simulants.GameplayPlayer.Name
                                then { gameplay with Player.Position = bodyTrandformMessage.Center; Player.Rotation = bodyTrandformMessage.Rotation }
                                else
                                    let enemyId = scvalueMemo entity.Name
                                    match HMap.tryFind enemyId gameplay.Enemies with
                                    | Some enemy -> { gameplay with Enemies = HMap.add enemyId { enemy with Position = bodyTrandformMessage.Center; Rotation = bodyTrandformMessage.Rotation } gameplay.Enemies}
                                    | None -> gameplay
                            | _ -> gameplay
                        | _ -> gameplay)
                        gameplay integrationData.IntegrationMessages
                just gameplay

            | Update ->
                
                // update time
                let gameplay = { gameplay with GameplayTime = inc gameplay.GameplayTime }

                // update played input scan
                let bodyId = Simulants.GameplayPlayer.GetBodyId world
                let grounded = World.getBodyGrounded bodyId world
                let (position, rotation) = computePlayerMovement WalkSpeed TurnSpeed gameplay.Player grounded world
                let gameplay = { gameplay with Player.Position = position; Player.Rotation = rotation }

                // update player physics
                let player = updateCharacterPhysics gameplay.GameplayTime gameplay.Player Simulants.GameplayPlayer world
                let gameplay = { gameplay with Player = player }

                // update player attack
                let attackOpt =
                    match gameplay.Player.AttackOpt with
                    | Some attack ->
                        let localTime = gameplay.GameplayTime - attack.AttackTime
                        if localTime < 55 || localTime < 110 && attack.FollowUpBuffered
                        then Some attack
                        else None
                    | None -> None
                let gameplay = { gameplay with Player.AttackOpt = attackOpt }

                // update player animation
                let traversalAnimations = computeCharacterTraversalAnimations gameplay.Player
                let actionAnimationOpt = tryComputeCharacterActionAnimation gameplay.GameplayTime gameplay.Player.AttackOpt world
                let animations = Array.append (Option.toArray actionAnimationOpt) (Array.ofList traversalAnimations)
                let gameplay = { gameplay with Player.Animations = animations }

                // update enemy physics
                let enemies =
                    HMap.map (fun enemyId enemy ->
                        updateCharacterPhysics gameplay.GameplayTime enemy (Simulants.GameplayEnemy enemyId) world)
                        gameplay.Enemies
                let gameplay = { gameplay with Enemies = enemies }

                // update enemy animations
                let enemies =
                    HMap.map (fun _ enemy ->
                        let traversalAnimations = computeCharacterTraversalAnimations enemy
                        let actionAnimationOpt = tryComputeCharacterActionAnimation gameplay.GameplayTime enemy.AttackOpt world
                        let animations = Array.append (Option.toArray actionAnimationOpt) (Array.ofList traversalAnimations)
                        { enemy with Animations = animations })
                        gameplay.Enemies
                let gameplay = { gameplay with Enemies = enemies }

                // fin
                just gameplay

            | StartQuitting ->
                just { gameplay with GameplayState = Quitting }

            | FinishQuitting ->
                just { gameplay with GameplayState = Quit }

        override this.Command (gameplay, command, _, world) =
            match command with
            | JumpPlayer ->
                let bodyId = Simulants.GameplayPlayer.GetBodyId world
                let world = World.jumpBody true JumpSpeed bodyId world
                just world
            | PostUpdateEye ->
                let world = World.setEye3dCenter (gameplay.Player.Position + v3Up * 1.5f - gameplay.Player.Rotation.Forward * 3.0f) world
                let world = World.setEye3dRotation gameplay.Player.Rotation world
                just world

        // here we describe the content of the game including the level, the hud, and the player
        override this.Content (gameplay, _) =

            [// the gui group
             Content.group Simulants.GameplayGui.Name []

                [// quit
                 Content.button Simulants.GameplayQuit.Name
                    [Entity.Position == v3 336.0f -216.0f 0.0f
                     Entity.Elevation == 10.0f
                     Entity.Text == "Quit"
                     Entity.ClickEvent => StartQuitting]]

             // the scene group while playing or quitting
             match gameplay.GameplayState with
             | Playing | Quitting ->
                Content.groupFromFile Simulants.GameplayScene.Name "Assets/Gameplay/Scene.nugroup" []
                    [Content.entity<CharacterDispatcher> Simulants.GameplayPlayer.Name
                        [Entity.Character := gameplay.Player]
                     for (enemyId, enemy) in gameplay.Enemies do
                        Content.entity<CharacterDispatcher> (string enemyId)
                            [Entity.Character := enemy]]
             | Quit -> ()]