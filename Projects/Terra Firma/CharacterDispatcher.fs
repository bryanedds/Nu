namespace TerraFirma
open System
open System.Numerics
open Prime
open Nu
open TerraFirma

type CharacterMessage =
    | CharacterPenetration of BodyPenetrationData
    | CharacterSeparationExplicit of BodySeparationExplicitData
    | CharacterSeparationImplicit of BodySeparationImplicitData
    | WeaponPenetration of BodyPenetrationData
    | WeaponSeparationExplicit of BodySeparationExplicitData
    | WeaponSeparationImplicit of BodySeparationImplicitData
    | Update
    interface Message

type CharacterCommand =
    | Register
    | UpdateTransform of Vector3 * Quaternion
    | UpdateAnimations of Vector3 * Quaternion * Animation array * bool
    | SyncWeaponTransform
    | PublishAttacks of Entity Set
    | PublishDie
    | Jump
    interface Command

[<AutoOpen>]
module CharacterExtensions =
    type Entity with
        member this.GetCharacter world = this.GetModelGeneric<Character> world
        member this.SetCharacter value world = this.SetModelGeneric<Character> value world
        member this.Character = this.ModelGeneric<Character> ()
        member this.AttackEvent = Events.AttackEvent --> this
        member this.DieEvent = Events.DieEvent --> this

type CharacterDispatcher (character : Character) =
    inherit Entity3dDispatcher<Character, CharacterMessage, CharacterCommand> (true, false, false, character)

    static member Facets =
        [typeof<RigidBodyFacet>]

    override this.Definitions (character, _) =
        [Entity.Size == v3Dup 2.0f
         Entity.Offset == v3 0.0f 1.0f 0.0f
         Entity.Static == false
         Entity.BodyType == KinematicCharacter
         Entity.SleepingAllowed == true
         Entity.CharacterProperties == character.CharacterProperties
         Entity.BodyShape == CapsuleShape { Height = 1.0f; Radius = 0.35f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.85f 0.0f)); PropertiesOpt = None }
         Entity.Observable == true
         Entity.FollowTargetOpt := match character.CharacterType with Enemy -> Some Simulants.GameplayPlayer | Player -> None
         Entity.RegisterEvent => Register
         Entity.UpdateEvent => Update
         Entity.BodyPenetrationEvent =|> fun evt -> CharacterPenetration evt.Data
         Entity.BodySeparationExplicitEvent =|> fun evt -> CharacterSeparationExplicit evt.Data
         Entity.BodySeparationImplicitEvent =|> fun evt -> CharacterSeparationImplicit evt.Data
         Game.PostUpdateEvent => SyncWeaponTransform]

    override this.Message (character, message, entity, world) =

        match message with
        | Update ->

            // update character
            let time = world.UpdateTime
            let position = entity.GetPosition world
            let rotation = entity.GetRotation world
            let linearVelocity = entity.GetLinearVelocity world
            let angularVelocity = entity.GetAngularVelocity world
            let bodyId = entity.GetBodyId world
            let grounded = World.getBodyGrounded bodyId world
            let playerPosition = Simulants.GameplayPlayer.GetPosition world
            let (animations, invisible, attackedCharacters, jump, position, rotation, character) =
                Character.update time position rotation linearVelocity angularVelocity grounded playerPosition character world

            // deploy signals from update
            let signals = if jump then [Jump :> Signal] else []
            let signals = UpdateTransform (position, rotation) :> Signal :: UpdateAnimations (position, rotation, Array.ofList animations, invisible) :: signals
            let signals = match character.ActionState with WoundState wound when wound.WoundTime = world.UpdateTime - 60L -> PublishDie :> Signal :: signals | _ -> signals
            let signals = if attackedCharacters.Count > 0 then PublishAttacks attackedCharacters :> Signal :: signals else signals
            withSignals signals character

        | CharacterPenetration penetrationData ->
            match penetrationData.BodyShapePenetratee.BodyId.BodySource with
            | :? Entity as penetratee when penetratee.Is<CharacterDispatcher> world ->
                let characterPenetratee = penetratee.GetCharacter world
                match (character.CharacterType, characterPenetratee.CharacterType) with
                | (Enemy, Enemy) ->
                    let character = { character with CharacterCollisions = Set.add penetratee character.CharacterCollisions }
                    just character
                | (_, _) -> just character
            | _ -> just character

        | CharacterSeparationExplicit separationData ->
            match separationData.BodyShapeSeparatee.BodyId.BodySource with
            | :? Entity as separatee when separatee.Is<CharacterDispatcher> world && separatee <> entity ->
                let character = { character with CharacterCollisions = Set.remove separatee character.CharacterCollisions }
                just character
            | _ -> just character

        | CharacterSeparationImplicit separationData ->
            match separationData.BodyId.BodySource with
            | :? Entity as separatee when separatee.Is<CharacterDispatcher> world && separatee <> entity ->
                let character = { character with CharacterCollisions = Set.remove separatee character.CharacterCollisions }
                just character
            | _ -> just character

        | WeaponPenetration penetrationData ->
            match penetrationData.BodyShapePenetratee.BodyId.BodySource with
            | :? Entity as penetratee when penetratee.Is<CharacterDispatcher> world && penetratee <> entity ->
                let characterPenetratee = penetratee.GetCharacter world
                if character.CharacterType <> characterPenetratee.CharacterType then
                    let character = { character with WeaponCollisions = Set.add penetratee character.WeaponCollisions }
                    just character
                else just character
            | _ -> just character

        | WeaponSeparationExplicit separationData ->
            match separationData.BodyShapeSeparatee.BodyId.BodySource with
            | :? Entity as separatee when separatee.Is<CharacterDispatcher> world && separatee <> entity ->
                let character = { character with WeaponCollisions = Set.remove separatee character.WeaponCollisions }
                just character
            | _ -> just character

        | WeaponSeparationImplicit separationData ->
            match separationData.BodyId.BodySource with
            | :? Entity as separatee when separatee.Is<CharacterDispatcher> world ->
                let character = { character with WeaponCollisions = Set.remove separatee character.WeaponCollisions }
                just character
            | _ -> just character

    override this.Command (character, command, entity, world) =

        match command with
        | Register ->
            let animatedModel = entity / Constants.Gameplay.CharacterAnimatedModelName
            let weapon = entity / Constants.Gameplay.CharacterWeaponName
            let world = animatedModel.SetAnimations [|Animation.loop GameTime.zero None "Armature|Idle"|] world
            let world = animatedModel.AnimateBones world
            let world = weapon.AutoBounds world
            withSignal SyncWeaponTransform world

        | UpdateTransform (position, rotation) ->
            let world = entity.SetPosition position world
            let world = entity.SetRotation rotation world
            just world

        | UpdateAnimations (position, rotation, animations, invisible) ->
            let animatedModel = entity / Constants.Gameplay.CharacterAnimatedModelName
            let weapon = entity / Constants.Gameplay.CharacterWeaponName
            let world = animatedModel.SetPosition (character.PositionInterp position) world
            let world = animatedModel.SetRotation (character.RotationInterp rotation) world
            let world = animatedModel.SetAnimations animations world
            let world = animatedModel.SetVisible (not invisible) world
            let world = weapon.SetVisible (not invisible) world
            just world

        | SyncWeaponTransform ->
            let animatedModel = entity / Constants.Gameplay.CharacterAnimatedModelName
            let weapon = entity / Constants.Gameplay.CharacterWeaponName
            match animatedModel.TryGetBoneTransformByName Constants.Gameplay.CharacterWeaponHandBoneName world with
            | Some weaponHandBoneTransform ->
                let weaponTransform =
                    Matrix4x4.CreateTranslation (v3 -0.1f 0.0f 0.02f) *
                    Matrix4x4.CreateFromAxisAngle (v3Forward, MathF.PI_OVER_2) *
                    weaponHandBoneTransform
                let world = weapon.SetPosition weaponTransform.Translation world
                let world = weapon.SetRotation weaponTransform.Rotation world
                just world
            | None -> just world

        | PublishAttacks attackedCharacters ->
            let world =
                Set.fold (fun world attackedCharacter ->
                    World.publish attackedCharacter entity.AttackEvent entity world)
                    world attackedCharacters
            just world

        | PublishDie ->
            let world = World.publish entity entity.DieEvent entity world
            just world

        | Jump ->
            let bodyId = entity.GetBodyId world
            let world = World.jumpBody true character.JumpSpeed bodyId world
            just world

    override this.RayCast (ray, entity, world) =
        let animatedModel = entity / Constants.Gameplay.CharacterAnimatedModelName
        match animatedModel.RayCast ray world with
        | [||] ->
            let weapon = entity / Constants.Gameplay.CharacterWeaponName
            weapon.RayCast ray world
        | intersections -> intersections

    override this.Content (character, _) =

        [// hearts
         if character.CharacterType = Player then
            for i in 0 .. dec 5 do
                Content.staticSprite ("Heart+" + string i)
                    [Entity.Position == v3 (-284.0f + single i * 32.0f) -144.0f 0.0f
                     Entity.Size == v3 32.0f 32.0f 0.0f
                     Entity.StaticImage := if character.HitPoints >= inc i then Assets.Gameplay.HeartFull else Assets.Gameplay.HeartEmpty
                     Entity.MountOpt == None]

         // animated model
         Content.entity<AnimatedModelDispatcher> Constants.Gameplay.CharacterAnimatedModelName
            [Entity.Size == v3Dup 2.0f
             Entity.Offset == v3 0.0f 1.0f 0.0f
             Entity.MaterialProperties == MaterialProperties.defaultProperties
             Entity.AnimatedModel == Assets.Gameplay.JoanModel
             Entity.Pickable == false]

         // weapon
         Content.entity<RigidModelDispatcher> Constants.Gameplay.CharacterWeaponName
            [Entity.Offset == v3 0.0f 0.5f 0.0f
             Entity.StaticModel := character.WeaponModel
             Entity.BodyType == Static
             Entity.BodyShape == BoxShape { Size = v3 0.3f 1.2f 0.3f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.6f 0.0f)); PropertiesOpt = None }
             Entity.Sensor == true
             Entity.NavShape == EmptyNavShape
             Entity.Pickable == false
             Entity.BodyPenetrationEvent =|> fun evt -> WeaponPenetration evt.Data
             Entity.BodySeparationExplicitEvent =|> fun evt -> WeaponSeparationExplicit evt.Data
             Entity.BodySeparationImplicitEvent =|> fun evt -> WeaponSeparationImplicit evt.Data]]

type EnemyDispatcher () =
    inherit CharacterDispatcher (Character.initialEnemy)

type PlayerDispatcher () =
    inherit CharacterDispatcher (Character.initialPlayer)