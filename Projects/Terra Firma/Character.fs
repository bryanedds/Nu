﻿namespace TerraFirma
open System
open System.Numerics
open Prime
open Nu
open TerraFirma

type CharacterType =
    | Player
    | Enemy

type JumpState =
    { LastTime : int64
      LastTimeOnGround : int64 }

    static member initial =
        { LastTime = 0L
          LastTimeOnGround = 0L }

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

type [<ReferenceEquality; SymbolicExpansion>] Character =
    { CharacterType : CharacterType
      PositionPrevious : Vector3 FQueue
      RotationPrevious : Quaternion FQueue
      LinearVelocityPrevious : Vector3 FQueue
      AngularVelocityPrevious : Vector3 FQueue
      HitPoints : int
      ActionState : ActionState
      JumpState : JumpState
      CharacterCollisions : Entity Set
      WeaponCollisions : Entity Set
      WalkSpeed : single
      TurnSpeed : single
      JumpSpeed : single
      WeaponModel : StaticModel AssetTag }

    member this.PositionInterp position =
        if not (FQueue.isEmpty this.PositionPrevious) then
            let positions = FQueue.conj position this.PositionPrevious
            Seq.sum positions / single positions.Length
        else position

    member this.RotationInterp rotation =
        if not (FQueue.isEmpty this.RotationPrevious) then
            let rotations = FQueue.conj rotation this.RotationPrevious
            if rotations.Length > 1 then
                let unnormalized = Quaternion.Slerp (Seq.head rotations, Seq.last rotations, 0.5f)
                unnormalized.Normalized
            else rotation
        else rotation

    member this.LinearVelocityInterp linearVelocity =
        if not (FQueue.isEmpty this.LinearVelocityPrevious) then
            let linearVelocities = FQueue.conj linearVelocity this.LinearVelocityPrevious
            Seq.sum linearVelocities / single linearVelocities.Length
        else linearVelocity

    member this.AngularVelocityInterp angularVelocity =
        if not (FQueue.isEmpty this.AngularVelocityPrevious) then
            let angularVelocities = FQueue.conj angularVelocity this.AngularVelocityPrevious
            Seq.sum angularVelocities / single angularVelocities.Length
        else angularVelocity

    member this.CharacterProperties =
        match this.CharacterType with
        | Player -> CharacterProperties.defaultProperties
        | Enemy -> { CharacterProperties.defaultProperties with CollisionTolerance = 0.005f }

    static member private computeTraversalAnimations rotation linearVelocity angularVelocity character =
        match character.ActionState with
        | NormalState ->
            let rotationInterp = character.RotationInterp rotation
            let linearVelocityInterp = character.LinearVelocityInterp linearVelocity
            let angularVelocityInterp = character.AngularVelocityInterp angularVelocity
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
            animations
        | _ -> []

    static member private tryUpdateActionAnimation time character world =
        match character.ActionState with
        | NormalState -> None
        | ObstructedState obstructed ->
            let animation = Animation.loop obstructed.ObstructedTime None "Armature|Idle"
            Some (animation, false)
        | AttackState attack ->
            let localTime = time - attack.AttackTime
            match localTime with
            | 7L -> World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.SlashSound world
            | 67L -> World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.Slash2Sound world
            | _ -> ()
            let (animationTime, animationName) =
                if localTime <= 55L
                then (attack.AttackTime, "Armature|AttackVertical")
                else (attack.AttackTime + 55L, "Armature|AttackHorizontal")
            let animation = Animation.once animationTime None animationName
            Some (animation, false)
        | InjuryState injury ->
            let animation = Animation.once injury.InjuryTime None "Armature|WalkBack"
            Some (animation, false)
        | WoundState wound ->
            let localTime = time - wound.WoundTime
            let animation = Animation.loop wound.WoundTime None "Armature|WalkBack"
            let invisible = localTime / 5L % 2L = 0L
            Some (animation, invisible)

    static member private updateInterps position rotation linearVelocity angularVelocity character =

        // update interps
        let character =
            { character with
                PositionPrevious = (if character.PositionPrevious.Length >= Constants.Gameplay.CharacterInterpolationSteps then character.PositionPrevious |> FQueue.tail else character.PositionPrevious) |> FQueue.conj position
                RotationPrevious = (if character.RotationPrevious.Length >= Constants.Gameplay.CharacterInterpolationSteps then character.RotationPrevious |> FQueue.tail else character.RotationPrevious) |> FQueue.conj rotation
                LinearVelocityPrevious = (if character.LinearVelocityPrevious.Length >= Constants.Gameplay.CharacterInterpolationSteps then character.LinearVelocityPrevious |> FQueue.tail else character.LinearVelocityPrevious) |> FQueue.conj linearVelocity
                AngularVelocityPrevious = (if character.AngularVelocityPrevious.Length >= Constants.Gameplay.CharacterInterpolationSteps then character.AngularVelocityPrevious |> FQueue.tail else character.AngularVelocityPrevious) |> FQueue.conj angularVelocity }

        // ensure previous positions interp aren't stale (such as when an entity is moved in the editor with existing previous position state)
        let character =
            let positionInterp = character.PositionInterp position
            if Vector3.Distance (positionInterp, position) > Constants.Gameplay.CharacterPositionInterpDistanceMax
            then { character with PositionPrevious = List.init Constants.Gameplay.CharacterInterpolationSteps (fun _ -> position) |> FQueue.ofList }
            else character

        // fin
        character

    static member private updateJumpState time grounded character =
        let lastTimeOnGround = if grounded then time else character.JumpState.LastTimeOnGround
        { character with Character.JumpState.LastTimeOnGround = lastTimeOnGround }

    static member private updateInput time (position : Vector3) (rotation : Quaternion) grounded (playerPosition : Vector3) character world =
        match character.CharacterType with
        | Player ->

            // action
            let (jump, character) =

                // jumping
                if World.isKeyboardKeyPressed KeyboardKey.Space world then
                    let sinceJump = time - character.JumpState.LastTime
                    let sinceOnGround = time - character.JumpState.LastTimeOnGround
                    if sinceJump >= 12L && sinceOnGround < 10L && character.ActionState = NormalState then
                        let character = { character with Character.JumpState.LastTime = time }
                        (true, character)
                    else (false, character)

                // attacking
                elif World.isKeyboardKeyPressed KeyboardKey.RShift world then
                    let character =
                        match character.ActionState with
                        | NormalState ->
                            { character with ActionState = AttackState (AttackState.make time) }
                        | AttackState attack ->
                            let localTime = time - attack.AttackTime
                            if localTime > 10L && not attack.FollowUpBuffered
                            then { character with ActionState = AttackState { attack with FollowUpBuffered = true }}
                            else character
                        | ObstructedState _ | InjuryState _ | WoundState _ -> character
                    (false, character)
                else (false, character)

            // movement
            if character.ActionState = NormalState || not grounded then

                // compute new position
                let forward = rotation.Forward
                let right = rotation.Right
                let walkSpeed = character.WalkSpeed * if grounded then 1.0f else 0.75f
                let walkVelocity =
                    (if World.isKeyboardKeyDown KeyboardKey.W world || World.isKeyboardKeyDown KeyboardKey.Up world then forward * walkSpeed else v3Zero) +
                    (if World.isKeyboardKeyDown KeyboardKey.S world || World.isKeyboardKeyDown KeyboardKey.Down world then -forward * walkSpeed else v3Zero) +
                    (if World.isKeyboardKeyDown KeyboardKey.A world then -right * walkSpeed else v3Zero) +
                    (if World.isKeyboardKeyDown KeyboardKey.D world then right * walkSpeed else v3Zero)

                // compute new rotation
                let turnSpeed = character.TurnSpeed * if grounded then 1.0f else 0.75f
                let turnVelocity =
                    (if World.isKeyboardKeyDown KeyboardKey.Right world then -turnSpeed else 0.0f) +
                    (if World.isKeyboardKeyDown KeyboardKey.Left world then turnSpeed else 0.0f)
                let rotation = if turnVelocity <> 0.0f then rotation * Quaternion.CreateFromAxisAngle (v3Up, turnVelocity) else rotation
                (jump, rotation, walkVelocity, v3 0.0f turnVelocity 0.0f, character)

            else (jump, rotation, v3Zero, v3Zero, character)

        | Enemy ->
            
            // attacking
            let character =
                match character.ActionState with
                | NormalState ->
                    let rotationForwardFlat = rotation.Forward.WithY(0.0f).Normalized
                    let positionFlat = position.WithY 0.0f
                    let playerPositionFlat = playerPosition.WithY 0.0f
                    if position.Y - playerPosition.Y >= 0.25f then // above player
                        if  Vector3.Distance (playerPositionFlat, positionFlat) < 1.0f &&
                            rotationForwardFlat.AngleBetween (playerPositionFlat - positionFlat) < 0.1f then
                            { character with ActionState = AttackState (AttackState.make time) }
                        else character
                    elif playerPosition.Y - position.Y < 1.3f then // at or a bit below player
                        if  Vector3.Distance (playerPositionFlat, positionFlat) < 1.75f &&
                            rotationForwardFlat.AngleBetween (playerPositionFlat - positionFlat) < 0.15f then
                            { character with ActionState = AttackState (AttackState.make time) }
                        else character
                    else character
                | _ -> character

            // navigation
            let (navSpeedsOpt, character) =
                match character.ActionState with
                | NormalState | ObstructedState _ ->
                    let order =
                        character.CharacterCollisions |>
                        Array.ofSeq |>
                        Array.map (fun character -> (false, character.GetPosition world)) |>
                        Array.cons (true, position) |>
                        Array.sortBy (fun (_, position) -> Vector3.DistanceSquared (position, playerPosition)) |>
                        Array.findIndex fst
                    let canUnobstruct =
                        match character.ActionState with
                        | ObstructedState obstructed ->
                            let localTime = time - obstructed.ObstructedTime
                            order = 0 && localTime >= 10L
                        | _ -> order = 0
                    let character =
                        if canUnobstruct then { character with ActionState = NormalState }
                        elif character.ActionState = NormalState then { character with ActionState = ObstructedState { ObstructedTime = time }}
                        else character
                    let navSpeed = if character.ActionState = NormalState then (character.WalkSpeed, character.TurnSpeed) else (0.0f, character.TurnSpeed * 3.0f)
                    (Some navSpeed, character)
                | _ -> (None, character)
            match navSpeedsOpt with
            | Some (moveSpeed, turnSpeed) ->
                let sphere =
                    if position.Y - playerPosition.Y >= 0.25f
                    then Sphere (playerPosition, 0.1f) // when above player
                    else Sphere (playerPosition, 0.7f) // when at or below player
                let nearest = sphere.Nearest position
                let followOutput = World.nav3dFollow (Some 1.0f) (Some 12.0f) moveSpeed turnSpeed position rotation nearest Simulants.Gameplay world
                (false, followOutput.NavRotation, followOutput.NavLinearVelocity, followOutput.NavAngularVelocity, character)
            | None -> (false, rotation, v3Zero, v3Zero, character)

    static member private updateActionState time character =
        match character.ActionState with
        | NormalState -> character
        | ObstructedState _ -> character
        | AttackState attack ->
            let actionState =
                let localTime = time - attack.AttackTime
                if localTime < 55 || localTime < 130 && attack.FollowUpBuffered
                then AttackState attack
                else NormalState
            { character with ActionState = actionState }
        | InjuryState injury ->
            let actionState =
                let localTime = time - injury.InjuryTime
                let injuryTime = match character.CharacterType with Player -> 30 | Enemy -> 40
                if localTime < injuryTime
                then InjuryState injury
                else NormalState
            { character with ActionState = actionState }
        | WoundState _ -> character

    static member private updateAnimations time rotation linearVelocity angularVelocity character world =
        let traversalAnimations = Character.computeTraversalAnimations rotation linearVelocity angularVelocity character
        let (animations, invisible) =
            match Character.tryUpdateActionAnimation time character world with
            | Some (animation, invisible) -> (animation :: traversalAnimations, invisible)
            | None -> (traversalAnimations, false)
        (animations, invisible)

    static member private updateAttackedCharacters time character =
        match character.ActionState with
        | AttackState attack ->
            let localTime = time - attack.AttackTime
            let attack =
                match localTime with
                | 55L -> { attack with AttackedCharacters = Set.empty } // reset attack tracking at start of buffered attack
                | _ -> attack
            if localTime >= 20 && localTime < 30 || localTime >= 78 && localTime < 88 then
                let attackingCharacters = Set.difference character.WeaponCollisions attack.AttackedCharacters
                let attack = { attack with AttackedCharacters = Set.union attack.AttackedCharacters character.WeaponCollisions }
                (attackingCharacters, { character with ActionState = AttackState attack })
            else (Set.empty, { character with ActionState = AttackState attack })
        | _ -> (Set.empty, character)

    static member update time position rotation linearVelocity angularVelocity grounded playerPosition character world =
        let character = Character.updateInterps position rotation linearVelocity angularVelocity character
        let character = Character.updateJumpState time grounded character
        let (jump, rotation, linearVelocity, angularVelocity, character) = Character.updateInput time position rotation grounded playerPosition character world
        let character = Character.updateActionState time character
        let (attackedCharacters, character) = Character.updateAttackedCharacters time character
        let (animations, invisible) = Character.updateAnimations time rotation linearVelocity angularVelocity character world
        (animations, invisible, attackedCharacters, jump, linearVelocity, angularVelocity, rotation, character)

    static member initial characterType =
        { CharacterType = characterType
          PositionPrevious = FQueue.empty
          RotationPrevious = FQueue.empty
          LinearVelocityPrevious = FQueue.empty
          AngularVelocityPrevious = FQueue.empty
          HitPoints = 5
          ActionState = NormalState
          JumpState = JumpState.initial
          CharacterCollisions = Set.empty
          WeaponCollisions = Set.empty
          WalkSpeed = 0.75f
          TurnSpeed = 0.1f
          JumpSpeed = 5.0f
          WeaponModel = Assets.Gameplay.GreatSwordModel }

    static member initialPlayer =
        { Character.initial Player with WalkSpeed = 1.0f; TurnSpeed = 0.05f }

    static member initialEnemy =
        { Character.initial Enemy with HitPoints = 3 }