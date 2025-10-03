namespace TerraFirma
open System
open System.Numerics
open Prime
open Nu
open TerraFirma

type CharacterType =
    | Enemy
    | Player

    member this.Persistent =
        not this.IsPlayer

    member this.HitPointsMax =
        match this with
        | Enemy -> 3
        | Player -> 5

    member this.WalkSpeed =
        match this with
        | Enemy -> 2.0f
        | Player -> 3.0f

    member this.TurnSpeed =
        match this with
        | Enemy -> 5.0f
        | Player -> 3.0f

    member this.InjuryTime =
        match this with
        | Enemy -> 40
        | Player -> 30

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
    | AttackState of AttackState
    | InjuryState of InjuryState
    | WoundState of WoundState

[<AutoOpen>]
module CharacterExtensions =
    type Entity with
        member this.GetCharacterType world : CharacterType = this.Get (nameof this.CharacterType) world
        member this.SetCharacterType (value : CharacterType) world = this.Set (nameof this.CharacterType) value world
        member this.CharacterType = lens (nameof this.CharacterType) this this.GetCharacterType this.SetCharacterType
        member this.GetActionState world : ActionState = this.Get (nameof this.ActionState) world
        member this.SetActionState (value : ActionState) world = this.Set (nameof this.ActionState) value world
        member this.ActionState = lens (nameof this.ActionState) this this.GetActionState this.SetActionState
        member this.GetHitPoints world : int = this.Get (nameof this.HitPoints) world
        member this.SetHitPoints (value : int) world = this.Set (nameof this.HitPoints) value world
        member this.HitPoints = lens (nameof this.HitPoints) this this.GetHitPoints this.SetHitPoints
        member this.GetLastTimeGrounded world : int64 = this.Get (nameof this.LastTimeGrounded) world
        member this.SetLastTimeGrounded (value : int64) world = this.Set (nameof this.LastTimeGrounded) value world
        member this.LastTimeGrounded = lens (nameof this.LastTimeGrounded) this this.GetLastTimeGrounded this.SetLastTimeGrounded
        member this.GetLastTimeJump world : int64 = this.Get (nameof this.LastTimeJump) world
        member this.SetLastTimeJump (value : int64) world = this.Set (nameof this.LastTimeJump) value world
        member this.LastTimeJump = lens (nameof this.LastTimeJump) this this.GetLastTimeJump this.SetLastTimeJump
        member this.GetWeaponCollisions world : Entity Set = this.Get (nameof this.WeaponCollisions) world
        member this.SetWeaponCollisions (value : Entity Set) world = this.Set (nameof this.WeaponCollisions) value world
        member this.WeaponCollisions = lens (nameof this.WeaponCollisions) this this.GetWeaponCollisions this.SetWeaponCollisions
        member this.GetWeaponModel world : StaticModel AssetTag = this.Get (nameof this.WeaponModel) world
        member this.SetWeaponModel (value : StaticModel AssetTag) world = this.Set (nameof this.WeaponModel) value world
        member this.WeaponModel = lens (nameof this.WeaponModel) this this.GetWeaponModel this.SetWeaponModel
        member this.AttackEvent = Events.AttackEvent --> this
        member this.DeathEvent = Events.DeathEvent --> this

type CharacterDispatcher () =
    inherit Entity3dDispatcherImSim (true, false, false)

    static let computeTraversalAnimations (entity : Entity) world =
        match entity.GetActionState world with
        | NormalState ->
            let rotation = entity.GetRotationInterpolated world
            let linearVelocity = entity.GetLinearVelocityInterpolated world
            let angularVelocity = entity.GetAngularVelocityInterpolated world
            let forwardness = linearVelocity.Dot rotation.Forward
            let backness = linearVelocity.Dot -rotation.Forward
            let rightness = linearVelocity.Dot rotation.Right
            let leftness = linearVelocity.Dot -rotation.Right
            let turnRightness = if angularVelocity.Y < 0.0f then -angularVelocity.Y * 0.5f else 0.0f
            let turnLeftness = if angularVelocity.Y > 0.0f then angularVelocity.Y * 0.5f else 0.0f
            let animations =
                [Animation.make 0L None "Idle" Loop 1.0f 1.0f None]
            let animations =
                if forwardness >= 0.01f then Animation.make 0L None "WalkForward" Loop 1.0f forwardness None :: animations
                elif backness >= 0.01f then Animation.make 0L None "WalkBack" Loop 1.0f backness None :: animations
                else animations
            let animations =
                if rightness >= 0.01f then Animation.make 0L None "WalkRight" Loop 1.0f rightness None :: animations
                elif leftness >= 0.01f then Animation.make 0L None "WalkLeft" Loop 1.0f leftness None :: animations
                else animations
            let animations =
                if turnRightness >= 0.01f then Animation.make 0L None "TurnRight" Loop 1.0f turnRightness None :: animations
                elif turnLeftness >= 0.01f then Animation.make 0L None "TurnLeft" Loop 1.0f turnLeftness None :: animations
                else animations
            Array.ofList animations
        | _ -> [||]

    static let tryComputeActionAnimation animations (entity : Entity) world =
        match entity.GetActionState world with
        | NormalState ->
            (true, animations)
        | AttackState attack ->
            let localTime = world.UpdateTime - attack.AttackTime
            match localTime with
            | 7L -> World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.SlashSound world
            | 67L -> World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.Slash2Sound world
            | _ -> ()
            let (animationTime, animationName) =
                if localTime <= 55L
                then (attack.AttackTime, "AttackVertical")
                else (attack.AttackTime + 55L, "AttackHorizontal")
            let animation = Animation.once animationTime None animationName
            (true, [|animation|])
        | InjuryState injury ->
            let animation = Animation.once injury.InjuryTime None "WalkBack"
            (true, [|animation|])
        | WoundState wound ->
            let localTime = world.UpdateTime - wound.WoundTime
            let visible = localTime / 5L % 2L <> 0L
            let animation = Animation.loop wound.WoundTime None "WalkBack"
            (visible, [|animation|])

    static let processEnemyInput (playerPosition : Vector3) (entity : Entity) world =

        // attacking
        match entity.GetActionState world with
        | NormalState ->
            let position = entity.GetPosition world
            let positionFlat = position.WithY 0.0f
            let rotation = entity.GetRotation world
            let rotationForwardFlat = rotation.Forward.WithY(0.0f).Normalized
            let playerPositionFlat = playerPosition.WithY 0.0f
            if position.Y - playerPosition.Y >= 0.25f then // above player
                if  playerPositionFlat.Distance positionFlat < 1.0f &&
                    rotationForwardFlat.AngleBetween (playerPositionFlat - positionFlat) < 0.1f then
                    entity.SetActionState (AttackState (AttackState.make world.UpdateTime)) world
                    entity.SetLinearVelocity (entity.GetLinearVelocity world * v3Up) world
            elif playerPosition.Y - position.Y < 1.3f then // at or a bit below player
                if  playerPositionFlat.Distance positionFlat < 1.75f &&
                    rotationForwardFlat.AngleBetween (playerPositionFlat - positionFlat) < 0.15f then
                    entity.SetActionState (AttackState (AttackState.make world.UpdateTime)) world
                    entity.SetLinearVelocity (entity.GetLinearVelocity world * v3Up) world
        | _ -> ()

        // navigation
        let navSpeedsOpt =
            match entity.GetActionState world with
            | NormalState ->
                let walkSpeed = Enemy.WalkSpeed
                let turnSpeed = Enemy.TurnSpeed
                Some (walkSpeed, turnSpeed)
            | _ -> None
        match navSpeedsOpt with
        | Some (walkSpeed, turnSpeed) ->
            let position = entity.GetPosition world
            let rotation = entity.GetRotation world
            let sphere =
                if position.Y - playerPosition.Y >= 0.25f
                then Sphere (playerPosition, 0.1f) // when above player
                else Sphere (playerPosition, 0.7f) // when at or below player
            let nearest = sphere.Nearest position
            let followOutput = World.nav3dFollow (Some 1.0f) (Some 12.0f) walkSpeed turnSpeed position rotation nearest Simulants.Gameplay world    
            entity.SetLinearVelocity (followOutput.NavLinearVelocity.WithY 0.0f + entity.GetLinearVelocity world * v3Up) world
            entity.SetAngularVelocity followOutput.NavAngularVelocity world
            entity.SetRotation followOutput.NavRotation world
        | None -> ()

    static let processPlayerInput (entity : Entity) world =

        // jumping
        let bodyId = entity.GetBodyId world
        let grounded = World.getBodyGrounded bodyId world
        if World.isKeyboardKeyPressed KeyboardKey.Space world then
            let actionState = entity.GetActionState world
            let sinceGrounded = world.UpdateTime - entity.GetLastTimeGrounded world
            let sinceJump = world.UpdateTime - entity.GetLastTimeJump world
            if sinceJump >= 12L && sinceGrounded < 10L && actionState = NormalState then
                entity.SetLinearVelocity (entity.GetLinearVelocity world + v3Up * 5.0f) world // TODO: use jump velocity constant.
                entity.SetLastTimeJump world.UpdateTime world

        // attacking
        elif World.isKeyboardKeyPressed KeyboardKey.RShift world then
            match entity.GetActionState world with
            | NormalState ->
                entity.SetActionState (AttackState (AttackState.make world.UpdateTime)) world
                entity.SetLinearVelocity (entity.GetLinearVelocity world * v3Up) world
            | AttackState attack ->
                let localTime = world.UpdateTime - attack.AttackTime
                if localTime > 10L && not attack.FollowUpBuffered then
                    entity.SetActionState (AttackState { attack with FollowUpBuffered = true }) world
            | InjuryState _ | WoundState _ -> ()

        // process movement - can move only when in normal state or in air
        match entity.GetActionState world with
        | AttackState _ when grounded ->

            // stop movement
            entity.SetLinearVelocity (entity.GetLinearVelocity world * v3Up) world

        | actionState when actionState.IsNormalState || not grounded ->

            // compute new position
            let rotation = entity.GetRotation world
            let forward = rotation.Forward
            let right = rotation.Right
            let walkSpeed = Player.WalkSpeed * if grounded then 1.0f else 0.75f
            let walkVelocity =
                (if World.isKeyboardKeyDown KeyboardKey.W world || World.isKeyboardKeyDown KeyboardKey.Up world then forward * walkSpeed else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.S world || World.isKeyboardKeyDown KeyboardKey.Down world then -forward * walkSpeed else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.A world then -right * walkSpeed else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.D world then right * walkSpeed else v3Zero)

            // compute new rotation
            let turnSpeed = Player.TurnSpeed * if grounded then 1.0f else 0.75f
            let turnVelocity =
                (if World.isKeyboardKeyDown KeyboardKey.Right world then -turnSpeed else 0.0f) +
                (if World.isKeyboardKeyDown KeyboardKey.Left world then turnSpeed else 0.0f)
            let rotation = if turnVelocity <> 0.0f then rotation * Quaternion.CreateFromAxisAngle (v3Up, turnVelocity * world.GameDelta.Seconds) else rotation

            // apply changes
            entity.SetLinearVelocity (walkVelocity.WithY 0.0f + entity.GetLinearVelocity world * v3Up) world
            entity.SetAngularVelocity (v3 0.0f turnVelocity 0.0f) world
            entity.SetRotation rotation world

        | _ -> ()

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<TraversalInterpolatedFacet>]

    static member Properties =
        let characterType = Enemy
        [define Entity.Size (v3Dup 2.0f)
         define Entity.Offset (v3 0.0f 1.0f 0.0f)
         define Entity.Persistent characterType.Persistent
         define Entity.MountOpt None
         define Entity.BodyType KinematicCharacter
         define Entity.BodyShape (CapsuleShape { Height = 1.0f; Radius = 0.35f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.85f 0.0f)); PropertiesOpt = None })
         define Entity.Substance (Mass 50.0f)
         define Entity.CharacterType characterType
         define Entity.ActionState NormalState
         define Entity.HitPoints characterType.HitPointsMax
         define Entity.LastTimeGrounded 0L
         define Entity.LastTimeJump 0L
         define Entity.WeaponCollisions Set.empty
         define Entity.WeaponModel Assets.Gameplay.GreatSwordModel]

    override this.Process (entity, world) =

        // process last time on ground
        let bodyId = entity.GetBodyId world
        if World.getBodyGrounded bodyId world then
            entity.SetLastTimeGrounded world.UpdateTime world

        // process input
        let characterType = entity.GetCharacterType world
        if world.Advancing then
            match characterType with
            | Enemy -> if Simulants.GameplayPlayer.GetExists world then processEnemyInput (Simulants.GameplayPlayer.GetPosition world) entity world
            | Player -> processPlayerInput entity world

        // process action state
        if world.Advancing then
            match entity.GetActionState world with
            | NormalState -> ()
            | AttackState attack ->
                let localTime = world.UpdateTime - attack.AttackTime
                let actionState =
                    if localTime < 55 || localTime < 130 && attack.FollowUpBuffered
                    then AttackState attack
                    else NormalState
                entity.SetActionState actionState world
            | InjuryState injury ->
                let localTime = world.UpdateTime - injury.InjuryTime
                let injuryTime = characterType.InjuryTime
                let actionState = if localTime < injuryTime then InjuryState injury else NormalState
                entity.SetActionState actionState world
            | WoundState _ -> ()

        // declare animated model
        let animations = computeTraversalAnimations entity world
        let (visible, animations) = tryComputeActionAnimation animations entity world
        World.doEntity<AnimatedModelDispatcher> Constants.Gameplay.CharacterAnimatedModelName
            [Entity.Position @= entity.GetPositionInterpolated world
             Entity.Rotation @= entity.GetRotationInterpolated world
             Entity.Size .= entity.GetSize world
             Entity.Offset .= entity.GetOffset world
             Entity.MountOpt .= None
             Entity.Visible @= visible
             Entity.Pickable .= false
             Entity.Animations @= animations
             Entity.AnimatedModel .= Assets.Gameplay.JoanModel] world
        let animatedModel = world.DeclaredEntity

        // declare weapon
        let weaponTransform =
            match animatedModel.TryGetBoneTransformByName Constants.Gameplay.CharacterWeaponHandBoneName world with
            | Some weaponHandBoneTransform ->
                Matrix4x4.CreateTranslation (v3 -0.1f 0.0f 0.02f) *
                Matrix4x4.CreateFromAxisAngle (v3Forward, MathF.PI_OVER_2) *
                weaponHandBoneTransform
            | None -> m4Identity
        let (_, results) =
            World.doRigidModel Constants.Gameplay.CharacterWeaponName
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
                 Entity.NavShape .= EmptyNavShape] world

        // process weapon collisions
        for result in results do
            match result with
            | BodyPenetrationData penetration ->
                match penetration.BodyShapePenetratee.BodyId.BodySource with
                | :? Entity as penetratee when penetratee.Is<CharacterDispatcher> world && penetratee <> entity ->
                    if characterType <> penetratee.GetCharacterType world then
                        entity.WeaponCollisions.Map (Set.add penetratee) world
                | _ -> ()
            | BodySeparationExplicitData separation ->
                match separation.BodyShapeSeparatee.BodyId.BodySource with
                | :? Entity as separatee when separatee.Is<CharacterDispatcher> world && separatee <> entity ->
                    entity.WeaponCollisions.Map (Set.remove separatee) world
                | _ -> ()
            | BodySeparationImplicitData separation ->
                match separation.BodyId.BodySource with
                | :? Entity as separatee -> entity.WeaponCollisions.Map (Set.remove separatee) world
                | _ -> ()
            | BodyTransformData _ -> ()

        // process attacks
        match entity.GetActionState world with
        | AttackState attack ->
            let localTime = world.UpdateTime - attack.AttackTime
            let attack =
                match localTime with
                | 55L -> { attack with AttackedCharacters = Set.empty } // reset attack tracking at start of buffered attack
                | _ -> attack
            if localTime >= 20 && localTime < 30 || localTime >= 78 && localTime < 88 then
                let weaponCollisions = entity.GetWeaponCollisions world
                let attackedCharacters = Set.difference weaponCollisions attack.AttackedCharacters
                entity.SetActionState (AttackState { attack with AttackedCharacters = Set.union attack.AttackedCharacters weaponCollisions }) world
                for character in attackedCharacters do
                    World.publish character entity.AttackEvent entity world
            else entity.SetActionState (AttackState attack) world
        | _ -> ()

        // declare player hearts
        match characterType with
        | Player ->
            let hitPoints = entity.GetHitPoints world
            for i in 0 .. dec characterType.HitPointsMax do
                World.doStaticSprite ("Heart+" + string i)
                    [Entity.Position .= v3 (-284.0f + single i * 32.0f) -144.0f 0.0f
                     Entity.Size .= v3 32.0f 32.0f 0.0f
                     Entity.MountOpt .= None
                     Entity.StaticImage @= if hitPoints >= inc i then Assets.Gameplay.HeartFullImage else Assets.Gameplay.HeartEmptyImage] world
        | Enemy -> ()

        // process death
        match entity.GetActionState world with
        | WoundState wound when wound.WoundTime = world.UpdateTime - 60L ->
            World.publish entity entity.DeathEvent entity world
        | _ -> ()

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

    static member Properties =
        let characterType = Enemy
        [define Entity.Persistent characterType.Persistent
         define Entity.CharacterType characterType
         define Entity.HitPoints characterType.HitPointsMax]

type PlayerDispatcher () =
    inherit CharacterDispatcher ()

    static member Properties =
        let characterType = Player
        [define Entity.Persistent characterType.Persistent
         define Entity.CharacterType characterType
         define Entity.HitPoints characterType.HitPointsMax]