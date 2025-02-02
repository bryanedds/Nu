namespace TerraFirma
open System
open System.Numerics
open Prime
open Nu
open TerraFirma

type CharacterType =
    | Player
    | Enemy

type ObstructedState =
    { ObstructedTime : int64 }

type AttackState =
    { AttackTime : int64
      FollowUpBuffered : bool
      AttackedCharacters : Entity Set }

    static member make time =
        { AttackTime = time
          FollowUpBuffered = false
          AttackedCharacters = Set.empty }

type InjuryState =
    { InjuryTime : int64 }

type WoundState =
    { WoundTime : int64 }

type ActionState =
    | NormalState
    | ObstructedState of ObstructedState
    | AttackState of AttackState
    | InjuryState of InjuryState
    | WoundState of WoundState

[<AutoOpen>]
module CharacterExtensions =
    type Entity with
        member this.GetCharacterType world : CharacterType = this.Get (nameof this.CharacterType) world
        member this.SetCharacterType (value : CharacterType) world = this.Set (nameof this.CharacterType) value world
        member this.CharacterType = lens (nameof this.CharacterType) this this.GetCharacterType this.SetCharacterType
        member this.GetPositionHistory world : Vector3 FQueue = this.Get (nameof this.PositionHistory) world
        member this.SetPositionHistory (value : Vector3 FQueue) world = this.Set (nameof this.PositionHistory) value world
        member this.PositionHistory = lens (nameof this.PositionHistory) this this.GetPositionHistory this.SetPositionHistory
        member this.GetRotationHistory world : Quaternion FQueue = this.Get (nameof this.RotationHistory) world
        member this.SetRotationHistory (value : Quaternion FQueue) world = this.Set (nameof this.RotationHistory) value world
        member this.RotationHistory = lens (nameof this.RotationHistory) this this.GetRotationHistory this.SetRotationHistory
        member this.GetLinearVelocityHistory world : Vector3 FQueue = this.Get (nameof this.LinearVelocityHistory) world
        member this.SetLinearVelocityHistory (value : Vector3 FQueue) world = this.Set (nameof this.LinearVelocityHistory) value world
        member this.LinearVelocityHistory = lens (nameof this.LinearVelocityHistory) this this.GetLinearVelocityHistory this.SetLinearVelocityHistory
        member this.GetAngularVelocityHistory world : Vector3 FQueue = this.Get (nameof this.AngularVelocityHistory) world
        member this.SetAngularVelocityHistory (value : Vector3 FQueue) world = this.Set (nameof this.AngularVelocityHistory) value world
        member this.AngularVelocityHistory = lens (nameof this.AngularVelocityHistory) this this.GetAngularVelocityHistory this.SetAngularVelocityHistory
        member this.GetLastTimeOnGround world : int64 = this.Get (nameof this.LastTimeOnGround) world
        member this.SetLastTimeOnGround (value : int64) world = this.Set (nameof this.LastTimeOnGround) value world
        member this.LastTimeOnGround = lens (nameof this.LastTimeOnGround) this this.GetLastTimeOnGround this.SetLastTimeOnGround
        member this.GetLastTimeJump world : int64 = this.Get (nameof this.LastTimeJump) world
        member this.SetLastTimeJump (value : int64) world = this.Set (nameof this.LastTimeJump) value world
        member this.LastTimeJump = lens (nameof this.LastTimeJump) this this.GetLastTimeJump this.SetLastTimeJump
        member this.GetHitPoints world : int = this.Get (nameof this.HitPoints) world
        member this.SetHitPoints (value : int) world = this.Set (nameof this.HitPoints) value world
        member this.HitPoints = lens (nameof this.HitPoints) this this.GetHitPoints this.SetHitPoints
        member this.GetActionState world : ActionState = this.Get (nameof this.ActionState) world
        member this.SetActionState (value : ActionState) world = this.Set (nameof this.ActionState) value world
        member this.ActionState = lens (nameof this.ActionState) this this.GetActionState this.SetActionState
        member this.GetCharacterCollisions world : Entity Set = this.Get (nameof this.CharacterCollisions) world
        member this.SetCharacterCollisions (value : Entity Set) world = this.Set (nameof this.CharacterCollisions) value world
        member this.CharacterCollisions = lens (nameof this.CharacterCollisions) this this.GetCharacterCollisions this.SetCharacterCollisions
        member this.GetWeaponCollisions world : Entity Set = this.Get (nameof this.WeaponCollisions) world
        member this.SetWeaponCollisions (value : Entity Set) world = this.Set (nameof this.WeaponCollisions) value world
        member this.WeaponCollisions = lens (nameof this.WeaponCollisions) this this.GetWeaponCollisions this.SetWeaponCollisions
        member this.GetWalkSpeed world : single = this.Get (nameof this.WalkSpeed) world
        member this.SetWalkSpeed (value : single) world = this.Set (nameof this.WalkSpeed) value world
        member this.WalkSpeed = lens (nameof this.WalkSpeed) this this.GetWalkSpeed this.SetWalkSpeed
        member this.GetTurnSpeed world : single = this.Get (nameof this.TurnSpeed) world
        member this.SetTurnSpeed (value : single) world = this.Set (nameof this.TurnSpeed) value world
        member this.TurnSpeed = lens (nameof this.TurnSpeed) this this.GetTurnSpeed this.SetTurnSpeed
        member this.GetJumpSpeed world : single = this.Get (nameof this.JumpSpeed) world
        member this.SetJumpSpeed (value : single) world = this.Set (nameof this.JumpSpeed) value world
        member this.JumpSpeed = lens (nameof this.JumpSpeed) this this.GetJumpSpeed this.SetJumpSpeed
        member this.GetWeaponModel world : StaticModel AssetTag = this.Get (nameof this.WeaponModel) world
        member this.SetWeaponModel (value : StaticModel AssetTag) world = this.Set (nameof this.WeaponModel) value world
        member this.WeaponModel = lens (nameof this.WeaponModel) this this.GetWeaponModel this.SetWeaponModel
        member this.AttackEvent = Events.AttackEvent --> this
        member this.DieEvent = Events.DieEvent --> this

        member this.GetPositionInterp world =
            let position = this.GetPosition world
            let positionHistory = this.GetPositionHistory world
            if FQueue.notEmpty positionHistory then
                let positions = FQueue.conj position positionHistory
                Seq.sum positions / single positions.Length
            else position

        member this.GetRotationInterp world =
            let rotation = this.GetRotation world
            let rotationHistory = this.GetRotationHistory world
            if FQueue.notEmpty rotationHistory then
                let rotations = FQueue.conj rotation rotationHistory
                if rotations.Length > 1 then
                    let unnormalized = Quaternion.Slerp (Seq.head rotations, Seq.last rotations, 0.5f)
                    unnormalized.Normalized
                else rotation
            else rotation

        member this.GetLinearVelocityInterp world =
            let linearVelocity = this.GetLinearVelocity world
            let linearVelocityHistory = this.GetLinearVelocityHistory world
            if FQueue.notEmpty linearVelocityHistory then
                let linearVelocities = FQueue.conj linearVelocity linearVelocityHistory
                Seq.sum linearVelocities / single linearVelocities.Length
            else linearVelocity

        member this.GetAngularVelocityInterp world =
            let angularVelocity = this.GetAngularVelocity world
            let angularVelocityHistory = this.GetAngularVelocityHistory world
            if FQueue.notEmpty angularVelocityHistory then
                let angularVelocities = FQueue.conj angularVelocity angularVelocityHistory
                Seq.sum angularVelocities / single angularVelocities.Length
            else angularVelocity

        member this.GetCharacterProperties world =
            match this.GetCharacterType world with
            | Player -> CharacterProperties.defaultProperties
            | Enemy -> { CharacterProperties.defaultProperties with CollisionTolerance = 0.005f }

type CharacterDispatcher () =
    inherit Entity3dDispatcherImNui (true, false, false)

    static let computeTraversalAnimations (entity : Entity) world =
        match entity.GetActionState world with
        | NormalState ->
            let rotationInterp = entity.GetRotationInterp world
            let linearVelocityInterp = entity.GetLinearVelocityInterp world
            let angularVelocityInterp = entity.GetAngularVelocityInterp world
            let forwardness = (linearVelocityInterp * 32.0f).Dot rotationInterp.Forward
            let backness = (linearVelocityInterp * 32.0f).Dot -rotationInterp.Forward
            let rightness = (linearVelocityInterp * 32.0f).Dot rotationInterp.Right
            let leftness = (linearVelocityInterp * 32.0f).Dot -rotationInterp.Right
            let turnRightness = if angularVelocityInterp.Y < 0.0f then -angularVelocityInterp.Y * 48.0f else 0.0f
            let turnLeftness = if angularVelocityInterp.Y > 0.0f then angularVelocityInterp.Y * 48.0f else 0.0f
            let animations =
                [Animation.make 0L None "Armature|Idle" Loop 1.0f 0.5f None]
            let animations =
                if forwardness >= 0.01f then Animation.make 0L None "Armature|WalkForward" Loop 1.0f forwardness None :: animations
                elif backness >= 0.01f then Animation.make 0L None "Armature|WalkBack" Loop 1.0f backness None :: animations
                else animations
            let animations =
                if rightness >= 0.01f then Animation.make 0L None "Armature|WalkRight" Loop 1.0f rightness None :: animations
                elif leftness >= 0.01f then Animation.make 0L None "Armature|WalkLeft" Loop 1.0f leftness None :: animations
                else animations
            let animations =
                if turnRightness >= 0.01f then Animation.make 0L None "Armature|TurnRight" Loop 1.0f turnRightness None :: animations
                elif turnLeftness >= 0.01f then Animation.make 0L None "Armature|TurnLeft" Loop 1.0f turnLeftness None :: animations
                else animations
            Array.ofList animations
        | _ -> [||]

    static let tryComputeActionAnimation animations (entity : Entity) world =
        match entity.GetActionState world with
        | NormalState ->
            (true, animations, world)
        | ObstructedState obstructed ->
            let animation = Animation.loop obstructed.ObstructedTime None "Armature|Idle"
            (true, [|animation|], world)
        | AttackState attack ->
            let localTime = world.UpdateTime - attack.AttackTime
            match localTime with
            | 7L -> World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.SlashSound world
            | 67L -> World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.Slash2Sound world
            | _ -> ()
            let (animationTime, animationName) =
                if localTime <= 55L
                then (attack.AttackTime, "Armature|AttackVertical")
                else (attack.AttackTime + 55L, "Armature|AttackHorizontal")
            let animation = Animation.once animationTime None animationName
            (true, [|animation|], world)
        | InjuryState injury ->
            let animation = Animation.once injury.InjuryTime None "Armature|WalkBack"
            (true, [|animation|], world)
        | WoundState wound ->
            let localTime = world.UpdateTime - wound.WoundTime
            let visible = localTime / 5L % 2L <> 0L
            let animation = Animation.loop wound.WoundTime None "Armature|WalkBack"
            (visible, [|animation|], world)

    static let updatePlayerInput (entity : Entity) world =

        // action
        let world =

            // jumping
            if World.isKeyboardKeyPressed KeyboardKey.Space world then
                let actionState = entity.GetActionState world
                let sinceOnGround = world.UpdateTime - entity.GetLastTimeOnGround world
                let sinceJump = world.UpdateTime - entity.GetLastTimeJump world
                if sinceJump >= 12L && sinceOnGround < 10L && actionState = NormalState then
                    let world = entity.SetLinearVelocity (entity.GetLinearVelocity world + v3 0.0f (entity.GetJumpSpeed world) 0.0f) world
                    let world = entity.SetLastTimeJump world.UpdateTime world
                    world
                else world

            // attacking
            elif World.isKeyboardKeyPressed KeyboardKey.RShift world then
                match entity.GetActionState world with
                | NormalState -> entity.SetActionState (AttackState (AttackState.make world.UpdateTime)) world
                | AttackState attack ->
                    let localTime = world.UpdateTime - attack.AttackTime
                    if localTime > 10L && not attack.FollowUpBuffered
                    then entity.SetActionState (AttackState { attack with FollowUpBuffered = true }) world
                    else world
                | ObstructedState _ | InjuryState _ | WoundState _ -> world

            // do nothing
            else world

        // movement
        let bodyId = entity.GetBodyId world
        let grounded = World.getBodyGrounded bodyId world
        if entity.GetActionState world = NormalState || not grounded then

            // compute new position
            let rotation = entity.GetRotation world
            let forward = rotation.Forward
            let right = rotation.Right
            let walkSpeed = entity.GetWalkSpeed world * if grounded then 1.0f else 0.75f
            let walkVelocity =
                (if World.isKeyboardKeyDown KeyboardKey.W world || World.isKeyboardKeyDown KeyboardKey.Up world then forward * walkSpeed else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.S world || World.isKeyboardKeyDown KeyboardKey.Down world then -forward * walkSpeed else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.A world then -right * walkSpeed else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.D world then right * walkSpeed else v3Zero)

            // compute new rotation
            let turnSpeed = entity.GetTurnSpeed world * if grounded then 1.0f else 0.75f
            let turnVelocity =
                (if World.isKeyboardKeyDown KeyboardKey.Right world then -turnSpeed else 0.0f) +
                (if World.isKeyboardKeyDown KeyboardKey.Left world then turnSpeed else 0.0f)
            let rotation = if turnVelocity <> 0.0f then rotation * Quaternion.CreateFromAxisAngle (v3Up, turnVelocity) else rotation

            // apply changes
            let world = entity.SetLinearVelocity (entity.GetLinearVelocity world + walkVelocity) world
            let world = entity.SetAngularVelocity (v3 0.0f turnVelocity 0.0f) world
            let world = entity.SetRotation rotation world
            world

        // no movement
        else world

    static let updateEnemyInput (playerPosition : Vector3) (entity : Entity) world =

        // attacking
        let world =
            match entity.GetActionState world with
            | NormalState ->
                let position = entity.GetPosition world
                let positionFlat = position.WithY 0.0f
                let rotation = entity.GetRotation world
                let rotationForwardFlat = rotation.Forward.WithY(0.0f).Normalized
                let playerPositionFlat = playerPosition.WithY 0.0f
                if position.Y - playerPosition.Y >= 0.25f then // above player
                    if  Vector3.Distance (playerPositionFlat, positionFlat) < 1.0f &&
                        rotationForwardFlat.AngleBetween (playerPositionFlat - positionFlat) < 0.1f then
                        entity.SetActionState (AttackState (AttackState.make world.UpdateTime)) world
                    else world
                elif playerPosition.Y - position.Y < 1.3f then // at or a bit below player
                    if  Vector3.Distance (playerPositionFlat, positionFlat) < 1.75f &&
                        rotationForwardFlat.AngleBetween (playerPositionFlat - positionFlat) < 0.15f then
                        entity.SetActionState (AttackState (AttackState.make world.UpdateTime)) world
                    else world
                else world
            | _ -> world

        // navigation
        let (navSpeedsOpt, world) =
            let actionState = entity.GetActionState world
            match actionState with
            | NormalState | ObstructedState _ ->
                let order =
                    entity.GetCharacterCollisions world |>
                    Array.ofSeq |>
                    Array.filter (fun character -> character.GetExists world) |>
                    Array.map (fun character -> (false, character.GetPosition world)) |>
                    Array.cons (true, entity.GetPosition world) |>
                    Array.sortBy (fun (_, position) -> Vector3.DistanceSquared (position, playerPosition)) |>
                    Array.findIndex fst
                let canUnobstruct =
                    match actionState with
                    | ObstructedState obstructed ->
                        let localTime = world.UpdateTime - obstructed.ObstructedTime
                        order = 0 && localTime >= 10L
                    | _ -> order = 0
                let actionState =
                    if canUnobstruct then NormalState
                    elif actionState = NormalState then ObstructedState { ObstructedTime = world.UpdateTime }
                    else actionState
                let navSpeed =
                    if actionState = NormalState
                    then (entity.GetWalkSpeed world, entity.GetTurnSpeed world)
                    else (0.0f, entity.GetTurnSpeed world * 3.0f)
                let world = entity.SetActionState actionState world
                (Some navSpeed, world)
            | _ -> (None, world)
        match navSpeedsOpt with
        | Some (moveSpeed, turnSpeed) ->
            let position = entity.GetPosition world
            let rotation = entity.GetRotation world
            let sphere =
                if position.Y - playerPosition.Y >= 0.25f
                then Sphere (playerPosition, 0.1f) // when above player
                else Sphere (playerPosition, 0.7f) // when at or below player
            let nearest = sphere.Nearest position
            let followOutput = World.nav3dFollow (Some 1.0f) (Some 12.0f) moveSpeed turnSpeed position rotation nearest Simulants.Gameplay world    
            let world = entity.SetLinearVelocity (entity.GetLinearVelocity world + followOutput.NavLinearVelocity) world
            let world = entity.SetAngularVelocity followOutput.NavAngularVelocity world
            let world = entity.SetRotation followOutput.NavRotation world
            world
        | None -> world

    static member Facets =
        [typeof<RigidBodyFacet>]

    static member Properties =
        [define Entity.Size (v3Dup 2.0f)
         define Entity.Offset (v3 0.0f 1.0f 0.0f)
         define Entity.Static false
         define Entity.BodyType KinematicCharacter
         define Entity.BodyShape (CapsuleShape { Height = 1.0f; Radius = 0.35f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.85f 0.0f)); PropertiesOpt = None })
         define Entity.Substance (Mass 50.0f)
         define Entity.Observable true
         define Entity.CharacterType Enemy
         nonPersistent Entity.PositionHistory FQueue.empty
         nonPersistent Entity.RotationHistory FQueue.empty
         nonPersistent Entity.LinearVelocityHistory FQueue.empty
         nonPersistent Entity.AngularVelocityHistory FQueue.empty
         define Entity.LastTimeOnGround 0L
         define Entity.LastTimeJump 0L
         define Entity.HitPoints Constants.Gameplay.EnemyHitPoints
         define Entity.ActionState NormalState
         define Entity.CharacterCollisions Set.empty
         define Entity.WeaponCollisions Set.empty
         define Entity.WalkSpeed 0.75f
         define Entity.TurnSpeed 0.1f
         define Entity.JumpSpeed 5.0f
         define Entity.WeaponModel Assets.Gameplay.GreatSwordModel]

    override this.Process (entity, world) =

        // process history for the frame
        let world = entity.PositionHistory.Map (fun history -> (if history.Length >= Constants.Gameplay.CharacterInterpolationSteps then FQueue.tail history else history) |> FQueue.conj (entity.GetPosition world)) world
        let world = entity.RotationHistory.Map (fun history -> (if history.Length >= Constants.Gameplay.CharacterInterpolationSteps then FQueue.tail history else history) |> FQueue.conj (entity.GetRotation world)) world
        let world = entity.LinearVelocityHistory.Map (fun history -> (if history.Length >= Constants.Gameplay.CharacterInterpolationSteps then FQueue.tail history else history) |> FQueue.conj (entity.GetLinearVelocity world)) world
        let world = entity.AngularVelocityHistory.Map (fun history -> (if history.Length >= Constants.Gameplay.CharacterInterpolationSteps then FQueue.tail history else history) |> FQueue.conj (entity.GetAngularVelocity world)) world

        // ensure position history isn't stale (such as when an entity is moved in the editor)
        let world =
            let position = entity.GetPosition world
            let positionInterp = entity.GetPositionInterp world
            if Vector3.Distance (positionInterp, position) > Constants.Gameplay.CharacterPositionInterpDistanceMax then
                let positionHistory = List.init Constants.Gameplay.CharacterInterpolationSteps (fun _ -> position) |> FQueue.ofList
                entity.SetPositionHistory positionHistory world
            else world

        // process last time on ground
        let bodyId = entity.GetBodyId world
        let world =
            if World.getBodyGrounded bodyId world
            then entity.SetLastTimeOnGround world.UpdateTime world
            else world

        // process character penetrations
        let (characterPenetrations, world) = World.doSubscription "CharacterPenetration" entity.BodyPenetrationEvent world
        let world =
            FQueue.fold (fun world penetration ->
                match penetration.BodyShapePenetratee.BodyId.BodySource with
                | :? Entity as penetratee when penetratee.Is<CharacterDispatcher> world ->
                    match (entity.GetCharacterType world, penetratee.GetCharacterType world) with
                    | (Enemy, Enemy) -> entity.CharacterCollisions.Map (Set.add penetratee) world
                    | (_, _) -> world
                | _ -> world)
                world characterPenetrations

        // process character separations (explicit)
        let (characterSeparationExplicit, world) = World.doSubscription "CharacterSeparationExplicit" entity.BodySeparationExplicitEvent world
        let world =
            FQueue.fold (fun world separation ->
                match separation.BodyShapeSeparatee.BodyId.BodySource with
                | :? Entity as separatee when separatee.Is<CharacterDispatcher> world && separatee <> entity ->
                    entity.CharacterCollisions.Map (Set.remove separatee) world
                | _ -> world)
                world characterSeparationExplicit

        // process character separations (implicit)
        let (characterSeparationImplicit, world) = World.doSubscription "CharacterSeparationImplicit" entity.BodySeparationImplicitEvent world
        let world =
            FQueue.fold (fun world (separation : BodySeparationImplicitData) ->
                match separation.BodyId.BodySource with
                | :? Entity as separatee -> entity.CharacterCollisions.Map (Set.remove separatee) world
                | _ -> world)
                world characterSeparationImplicit

        // process input
        let world =
            if world.Advancing then
                match entity.GetCharacterType world with
                | Player -> updatePlayerInput entity world
                | Enemy ->
                    if Simulants.GameplayPlayer.GetExists world
                    then updateEnemyInput (Simulants.GameplayPlayer.GetPosition world) entity world
                    else world
            else world

        // process action state
        let world =
            let actionState =
                match entity.GetActionState world with
                | NormalState | ObstructedState _ | WoundState _ as actionState ->
                    actionState
                | AttackState attack ->
                    let localTime = world.UpdateTime - attack.AttackTime
                    if localTime < 55 || localTime < 130 && attack.FollowUpBuffered
                    then AttackState attack
                    else NormalState
                | InjuryState injury ->
                    let localTime = world.UpdateTime - injury.InjuryTime
                    let injuryTime = match entity.GetCharacterType world with Player -> 30 | Enemy -> 40
                    if localTime < injuryTime
                    then InjuryState injury
                    else NormalState
            entity.SetActionState actionState world

        // declare animated model
        let animations = computeTraversalAnimations entity world
        let (visible, animations, world) = tryComputeActionAnimation animations entity world
        let world =
            World.doEntity<AnimatedModelDispatcher> Constants.Gameplay.CharacterAnimatedModelName
                [Entity.Position @= entity.GetPositionInterp world
                 Entity.Rotation @= entity.GetRotationInterp world
                 Entity.Size .= v3Dup 2.0f
                 Entity.Offset .= v3 0.0f 1.0f 0.0f
                 Entity.MountOpt .= None
                 Entity.Visible @= visible
                 Entity.Pickable .= false
                 Entity.MaterialProperties .= MaterialProperties.defaultProperties
                 Entity.Animations @= animations
                 Entity.AnimatedModel .= Assets.Gameplay.JoanModel]
                world
        let animatedModel = world.RecentEntity

        // declare weapon
        let weaponTransform =
            match animatedModel.TryGetBoneTransformByName Constants.Gameplay.CharacterWeaponHandBoneName world with
            | Some weaponHandBoneTransform ->
                Matrix4x4.CreateTranslation (v3 -0.1f 0.0f 0.02f) *
                Matrix4x4.CreateFromAxisAngle (v3Forward, MathF.PI_OVER_2) *
                weaponHandBoneTransform
            | None -> m4Identity
        let world =
            World.doEntity<RigidModelDispatcher> Constants.Gameplay.CharacterWeaponName
                [Entity.Position @= weaponTransform.Translation
                 Entity.Rotation @= weaponTransform.Rotation
                 Entity.Offset .= v3 0.0f 0.5f 0.0f
                 Entity.MountOpt .= None
                 Entity.Visible @= visible
                 Entity.Pickable .= false
                 Entity.StaticModel @= entity.GetWeaponModel world
                 Entity.BodyType .= Static
                 Entity.BodyShape .= BoxShape { Size = v3 0.3f 1.2f 0.3f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.6f 0.0f)); PropertiesOpt = None }
                 Entity.Sensor .= true
                 Entity.NavShape .= EmptyNavShape]
                world
        let weapon = world.RecentEntity

        // process weapon penetrations
        let (weaponPenetrations, world) = World.doSubscription "WeaponPenetration" weapon.BodyPenetrationEvent world
        let world =
            FQueue.fold (fun world penetration ->
                match penetration.BodyShapePenetratee.BodyId.BodySource with
                | :? Entity as penetratee when penetratee.Is<CharacterDispatcher> world && penetratee <> entity ->
                    if entity.GetCharacterType world <> penetratee.GetCharacterType world then
                        entity.WeaponCollisions.Map (Set.add penetratee) world
                    else world
                | _ -> world)
                world weaponPenetrations

        // process weapon separations (explicit)
        let (weaponSeparationExplicit, world) = World.doSubscription "WeaponSeparationExplicit" weapon.BodySeparationExplicitEvent world
        let world =
            FQueue.fold (fun world separation ->
                match separation.BodyShapeSeparatee.BodyId.BodySource with
                | :? Entity as separatee when separatee.Is<CharacterDispatcher> world && separatee <> entity ->
                    entity.WeaponCollisions.Map (Set.remove separatee) world
                | _ -> world)
                world weaponSeparationExplicit

        // process weapon separations (implicit)
        let (weaponSeparationImplicit, world) = World.doSubscription "WeaponSeparationImplicit" weapon.BodySeparationImplicitEvent world
        let world =
            FQueue.fold (fun world (separation : BodySeparationImplicitData) ->
                match separation.BodyId.BodySource with
                | :? Entity as separatee -> entity.WeaponCollisions.Map (Set.remove separatee) world
                | _ -> world)
                world weaponSeparationImplicit

        // process attacks
        let (attacks, world) =
            match entity.GetActionState world with
            | AttackState attack ->
                let localTime = world.UpdateTime - attack.AttackTime
                let attack =
                    match localTime with
                    | 55L -> { attack with AttackedCharacters = Set.empty } // reset attack tracking at start of buffered attack
                    | _ -> attack
                if localTime >= 20 && localTime < 30 || localTime >= 78 && localTime < 88 then
                    let weaponCollisions = entity.GetWeaponCollisions world
                    let attackeds = Set.difference weaponCollisions attack.AttackedCharacters
                    let attack = { attack with AttackedCharacters = Set.union attack.AttackedCharacters weaponCollisions }
                    let world = entity.SetActionState (AttackState attack) world
                    (attackeds, world)
                else
                    let world = entity.SetActionState (AttackState attack) world
                    (Set.empty, world)
            | _ -> (Set.empty, world)
        let world = Set.fold (fun world attack -> World.publish attack entity.AttackEvent entity world) world attacks

        // declare player hearts
        let world =
            match entity.GetCharacterType world with
            | Player ->
                let hitPoints = entity.GetHitPoints world
                Seq.fold (fun world i ->
                    World.doStaticSprite ("Heart+" + string i)
                        [Entity.Position .= v3 (-284.0f + single i * 32.0f) -144.0f 0.0f
                         Entity.Size .= v3 32.0f 32.0f 0.0f
                         Entity.MountOpt .= None
                         Entity.StaticImage @= if hitPoints >= inc i then Assets.Gameplay.HeartFull else Assets.Gameplay.HeartEmpty]
                        world)
                    world [0 .. dec Constants.Gameplay.PlayerHitPoints]
            | Enemy -> world

        // process death
        let world =
            match entity.GetActionState world with
            | WoundState wound when wound.WoundTime = world.UpdateTime - 60L ->
                World.publish entity entity.DieEvent entity world
            | _ -> world

        // fin
        world

    // custom definition of ray cast to utilize animated model and weapon
    override this.RayCast (ray, entity, world) =
        let animatedModel = entity / Constants.Gameplay.CharacterAnimatedModelName
        match animatedModel.RayCast ray world with
        | [||] ->
            let weapon = entity / Constants.Gameplay.CharacterWeaponName
            weapon.RayCast ray world
        | intersections -> intersections

type EnemyDispatcher () =
    inherit CharacterDispatcher ()

type PlayerDispatcher () =
    inherit CharacterDispatcher ()

    static member Properties =
        [define Entity.Persistent false // don't serialize player when saving scene
         define Entity.CharacterType Player
         define Entity.HitPoints Constants.Gameplay.PlayerHitPoints
         define Entity.WalkSpeed 1.0f
         define Entity.TurnSpeed 0.05f]