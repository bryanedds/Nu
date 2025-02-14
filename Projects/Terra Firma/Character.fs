namespace TerraFirma
open System
open System.Numerics
open Prime
open Nu
open TerraFirma

type CharacterType =
    | Enemy
    | Player

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
        member this.GetLastTimeOnGround world : int64 = this.Get (nameof this.LastTimeOnGround) world
        member this.SetLastTimeOnGround (value : int64) world = this.Set (nameof this.LastTimeOnGround) value world
        member this.LastTimeOnGround = lens (nameof this.LastTimeOnGround) this this.GetLastTimeOnGround this.SetLastTimeOnGround
        member this.GetLastTimeJump world : int64 = this.Get (nameof this.LastTimeJump) world
        member this.SetLastTimeJump (value : int64) world = this.Set (nameof this.LastTimeJump) value world
        member this.LastTimeJump = lens (nameof this.LastTimeJump) this this.GetLastTimeJump this.SetLastTimeJump
        member this.GetCharacterCollisions world : Entity Set = this.Get (nameof this.CharacterCollisions) world
        member this.SetCharacterCollisions (value : Entity Set) world = this.Set (nameof this.CharacterCollisions) value world
        member this.CharacterCollisions = lens (nameof this.CharacterCollisions) this this.GetCharacterCollisions this.SetCharacterCollisions
        member this.GetWeaponCollisions world : Entity Set = this.Get (nameof this.WeaponCollisions) world
        member this.SetWeaponCollisions (value : Entity Set) world = this.Set (nameof this.WeaponCollisions) value world
        member this.WeaponCollisions = lens (nameof this.WeaponCollisions) this this.GetWeaponCollisions this.SetWeaponCollisions
        member this.GetWeaponModel world : StaticModel AssetTag = this.Get (nameof this.WeaponModel) world
        member this.SetWeaponModel (value : StaticModel AssetTag) world = this.Set (nameof this.WeaponModel) value world
        member this.WeaponModel = lens (nameof this.WeaponModel) this this.GetWeaponModel this.SetWeaponModel
        member this.AttackEvent = Events.AttackEvent --> this
        member this.DieEvent = Events.DieEvent --> this

        member this.GetCharacterProperties world =
            match this.GetCharacterType world with
            | Enemy -> { CharacterProperties.defaultProperties with CollisionTolerance = 0.005f }
            | Player -> CharacterProperties.defaultProperties

type CharacterDispatcher () =
    inherit Entity3dDispatcherImNui (true, false, false)

    static let computeTraversalAnimations (entity : Entity) world =
        match entity.GetActionState world with
        | NormalState ->
            let rotation = entity.GetRotation world
            let linearVelocity = entity.GetLinearVelocity world
            let angularVelocity = entity.GetAngularVelocity world
            let forwardness = linearVelocity.Dot rotation.Forward
            let backness = linearVelocity.Dot -rotation.Forward
            let rightness = linearVelocity.Dot rotation.Right
            let leftness = linearVelocity.Dot -rotation.Right
            let turnRightness = if angularVelocity.Y < 0.0f then -angularVelocity.Y * 0.5f else 0.0f
            let turnLeftness = if angularVelocity.Y > 0.0f then angularVelocity.Y * 0.5f else 0.0f
            let animations =
                [Animation.make 0L None "Armature|Idle" Loop 1.0f 1.0f None]
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

    static let processEnemyInput (playerPosition : Vector3) (entity : Entity) world =

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
                        let world = entity.SetActionState (AttackState (AttackState.make world.UpdateTime)) world
                        entity.SetLinearVelocity (v3Up * entity.GetLinearVelocity world) world
                    else world
                elif playerPosition.Y - position.Y < 1.3f then // at or a bit below player
                    if  Vector3.Distance (playerPositionFlat, positionFlat) < 1.75f &&
                        rotationForwardFlat.AngleBetween (playerPositionFlat - positionFlat) < 0.15f then
                        let world = entity.SetActionState (AttackState (AttackState.make world.UpdateTime)) world
                        entity.SetLinearVelocity (v3Up * entity.GetLinearVelocity world) world
                    else world
                else world
            | _ -> world

        // navigation
        let (navSpeedsOpt, world) =
            let actionState = entity.GetActionState world
            match actionState with
            | NormalState ->
                let walkSpeed = Constants.Gameplay.EnemyWalkSpeed * if actionState.IsNormalState then 1.0f else 0.0f
                let turnSpeed = Constants.Gameplay.EnemyTurnSpeed * if actionState.IsNormalState then 1.0f else 3.0f
                let world = entity.SetActionState actionState world
                (Some (walkSpeed, turnSpeed), world)
            | _ -> (None, world)
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
            let world = entity.SetLinearVelocity (followOutput.NavLinearVelocity.WithY 0.0f + v3Up * entity.GetLinearVelocity world) world
            let world = entity.SetAngularVelocity followOutput.NavAngularVelocity world
            let world = entity.SetRotation followOutput.NavRotation world
            world
        | None -> world

    static let processPlayerInput (entity : Entity) world =

        // action
        let world =

            // jumping
            if World.isKeyboardKeyPressed KeyboardKey.Space world then
                let actionState = entity.GetActionState world
                let sinceOnGround = world.UpdateTime - entity.GetLastTimeOnGround world
                let sinceJump = world.UpdateTime - entity.GetLastTimeJump world
                if sinceJump >= 12L && sinceOnGround < 10L && actionState = NormalState then
                    let world = entity.SetLinearVelocity (entity.GetLinearVelocity world + v3Up * 5.0f) world
                    let world = entity.SetLastTimeJump world.UpdateTime world
                    world
                else world

            // attacking
            elif World.isKeyboardKeyPressed KeyboardKey.RShift world then
                match entity.GetActionState world with
                | NormalState ->
                    let world = entity.SetActionState (AttackState (AttackState.make world.UpdateTime)) world
                    entity.SetLinearVelocity (v3Up * entity.GetLinearVelocity world) world
                | AttackState attack ->
                    let localTime = world.UpdateTime - attack.AttackTime
                    if localTime > 10L && not attack.FollowUpBuffered
                    then entity.SetActionState (AttackState { attack with FollowUpBuffered = true }) world
                    else world
                | InjuryState _ | WoundState _ -> world

            // do nothing
            else world

        // movement
        let bodyId = entity.GetBodyId world
        let grounded = World.getBodyGrounded bodyId world
        let actionState = entity.GetActionState world
        if actionState.IsNormalState || not grounded then

            // compute new position
            let rotation = entity.GetRotation world
            let forward = rotation.Forward
            let right = rotation.Right
            let walkSpeed = Constants.Gameplay.PlayerWalkSpeed * if grounded then 1.0f else 0.75f
            let walkVelocity =
                (if World.isKeyboardKeyDown KeyboardKey.W world || World.isKeyboardKeyDown KeyboardKey.Up world then forward * walkSpeed else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.S world || World.isKeyboardKeyDown KeyboardKey.Down world then -forward * walkSpeed else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.A world then -right * walkSpeed else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.D world then right * walkSpeed else v3Zero)

            // compute new rotation
            let turnSpeed = Constants.Gameplay.PlayerTurnSpeed * if grounded then 1.0f else 0.75f
            let turnVelocity =
                (if World.isKeyboardKeyDown KeyboardKey.Right world then -turnSpeed else 0.0f) +
                (if World.isKeyboardKeyDown KeyboardKey.Left world then turnSpeed else 0.0f)
            let rotation = if turnVelocity <> 0.0f then rotation * Quaternion.CreateFromAxisAngle (v3Up, turnVelocity * world.GameDelta.Seconds) else rotation

            // apply changes
            let world = entity.SetLinearVelocity (walkVelocity.WithY 0.0f + v3Up * entity.GetLinearVelocity world) world
            let world = entity.SetAngularVelocity (v3 0.0f turnVelocity 0.0f) world
            let world = entity.SetRotation rotation world
            world

        // no movement
        else world

    static member Facets =
        [typeof<RigidBodyFacet>]

    static member Properties =
        [define Entity.Size (v3Dup 2.0f)
         define Entity.Offset (v3 0.0f 1.0f 0.0f)
         define Entity.BodyType KinematicCharacter
         define Entity.BodyShape (CapsuleShape { Height = 1.0f; Radius = 0.35f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.85f 0.0f)); PropertiesOpt = None })
         define Entity.Substance (Mass 50.0f)
         define Entity.Observable true
         define Entity.CharacterType Enemy
         define Entity.ActionState NormalState
         define Entity.HitPoints 1
         define Entity.LastTimeOnGround 0L
         define Entity.LastTimeJump 0L
         define Entity.CharacterCollisions Set.empty
         define Entity.WeaponCollisions Set.empty
         define Entity.WeaponModel Assets.Gameplay.GreatSwordModel]

    override this.Process (entity, world) =

        // process last time on ground
        let bodyId = entity.GetBodyId world
        let world =
            if World.getBodyGrounded bodyId world
            then entity.SetLastTimeOnGround world.UpdateTime world
            else world

        // process penetration
        let (penetrations, world) = World.doSubscription "Penetration" entity.BodyPenetrationEvent world
        let world =
            FQueue.fold (fun world penetration ->
                match penetration.BodyShapePenetratee.BodyId.BodySource with
                | :? Entity as penetratee when penetratee.Is<CharacterDispatcher> world ->
                    match (entity.GetCharacterType world, penetratee.GetCharacterType world) with
                    | (Enemy, Enemy) -> entity.CharacterCollisions.Map (Set.add penetratee) world
                    | (_, _) -> world
                | _ -> world)
                world penetrations

        // process separation (explicit)
        let (separationsExplicit, world) = World.doSubscription "SeparationExplicit" entity.BodySeparationExplicitEvent world
        let world =
            FQueue.fold (fun world separation ->
                match separation.BodyShapeSeparatee.BodyId.BodySource with
                | :? Entity as separatee when separatee.Is<CharacterDispatcher> world && separatee <> entity ->
                    entity.CharacterCollisions.Map (Set.remove separatee) world
                | _ -> world)
                world separationsExplicit

        // process separation (implicit)
        let (separationsImplicit, world) = World.doSubscription "SeparationImplicit" entity.BodySeparationImplicitEvent world
        let world =
            FQueue.fold (fun world (separation : BodySeparationImplicitData) ->
                match separation.BodyId.BodySource with
                | :? Entity as separatee -> entity.CharacterCollisions.Map (Set.remove separatee) world
                | _ -> world)
                world separationsImplicit

        // process input
        let world =
            if world.Advancing then
                match entity.GetCharacterType world with
                | Enemy ->
                    if Simulants.GameplayPlayer.GetExists world
                    then processEnemyInput (Simulants.GameplayPlayer.GetPosition world) entity world
                    else world
                | Player -> processPlayerInput entity world
            else world

        // process action state
        let world =
            let actionState =
                match entity.GetActionState world with
                | NormalState | WoundState _ as actionState ->
                    actionState
                | AttackState attack ->
                    let localTime = world.UpdateTime - attack.AttackTime
                    if localTime < 55 || localTime < 130 && attack.FollowUpBuffered
                    then AttackState attack
                    else NormalState
                | InjuryState injury ->
                    let localTime = world.UpdateTime - injury.InjuryTime
                    let injuryTime = match entity.GetCharacterType world with Enemy -> 40 | Player -> 30
                    if localTime < injuryTime
                    then InjuryState injury
                    else NormalState
            entity.SetActionState actionState world

        // declare animated model
        let animations = computeTraversalAnimations entity world
        let (visible, animations, world) = tryComputeActionAnimation animations entity world
        let world =
            World.doEntity<AnimatedModelDispatcher> Constants.Gameplay.CharacterAnimatedModelName
                [Entity.Position @= entity.GetPosition world
                 Entity.Rotation @= entity.GetRotation world
                 Entity.Size .= entity.GetSize world
                 Entity.Offset .= entity.GetOffset world
                 Entity.MountOpt .= None
                 Entity.Visible @= visible
                 Entity.Pickable .= false
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
        let (_, results, world) =
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
                 Entity.NavShape .= EmptyNavShape]
                world

        // process weapon collisions
        let world =
            FQueue.fold (fun world result ->
                match result with
                | BodyPenetration penetration ->
                    match penetration.BodyShapePenetratee.BodyId.BodySource with
                    | :? Entity as penetratee when penetratee.Is<CharacterDispatcher> world && penetratee <> entity ->
                        if entity.GetCharacterType world <> penetratee.GetCharacterType world
                        then entity.WeaponCollisions.Map (Set.add penetratee) world
                        else world
                    | _ -> world
                | BodySeparationExplicit separation ->
                    match separation.BodyShapeSeparatee.BodyId.BodySource with
                    | :? Entity as separatee when separatee.Is<CharacterDispatcher> world && separatee <> entity ->
                        entity.WeaponCollisions.Map (Set.remove separatee) world
                    | _ -> world
                | BodySeparationImplicit separation ->
                    match separation.BodyId.BodySource with
                    | :? Entity as separatee -> entity.WeaponCollisions.Map (Set.remove separatee) world
                    | _ -> world
                | BodyTransform _ -> world)
                world results

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
                    let attacks = Set.difference weaponCollisions attack.AttackedCharacters
                    let attack = { attack with AttackedCharacters = Set.union attack.AttackedCharacters weaponCollisions }
                    let world = entity.SetActionState (AttackState attack) world
                    (attacks, world)
                else
                    let world = entity.SetActionState (AttackState attack) world
                    (Set.empty, world)
            | _ -> (Set.empty, world)
        let world = Set.fold (fun world attack -> World.publish attack entity.AttackEvent entity world) world attacks

        // declare player hearts
        let world =
            if (entity.GetCharacterType world).IsPlayer then
                let hitPoints = entity.GetHitPoints world
                Seq.fold (fun world i ->
                    World.doStaticSprite ("Heart+" + string i)
                        [Entity.Position .= v3 (-284.0f + single i * 32.0f) -144.0f 0.0f
                         Entity.Size .= v3 32.0f 32.0f 0.0f
                         Entity.MountOpt .= None
                         Entity.StaticImage @= if hitPoints >= inc i then Assets.Gameplay.HeartFull else Assets.Gameplay.HeartEmpty]
                        world)
                    world [0 .. dec Constants.Gameplay.PlayerHitPointsMax]
            else world

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

    static member Properties =
        [define Entity.CharacterType Enemy
         define Entity.HitPoints Constants.Gameplay.EnemyHitPointsMax]

type PlayerDispatcher () =
    inherit CharacterDispatcher ()

    static member Properties =
        [define Entity.Persistent false // don't serialize player when saving scene
         define Entity.CharacterType Player
         define Entity.HitPoints Constants.Gameplay.PlayerHitPointsMax]