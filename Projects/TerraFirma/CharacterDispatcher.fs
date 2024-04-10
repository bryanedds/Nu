namespace TerraFirma
open System
open System.Numerics
open Prime
open Nu

type CharacterMessage =
    | WeaponCollide of BodyCollisionData
    | WeaponSeparateExplicit of BodySeparationExplicitData
    | WeaponSeparateImplicit of BodySeparationImplicitData
    | UpdateInputKey of KeyboardKeyData
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
    | Destroy
    | PlaySound of int64 * single * Sound AssetTag
    interface Command

[<AutoOpen>]
module CharacterDispatcher =

    type Entity with
        member this.GetCharacter world = this.GetModelGeneric<Character> world
        member this.SetCharacter value world = this.SetModelGeneric<Character> value world
        member this.Character = this.ModelGeneric<Character> ()
        member this.AttackEvent = Events.AttackEvent --> this
        member this.DieEvent = Events.DieEvent --> this

    type CharacterDispatcher (character : Character) =
        inherit Entity3dDispatcher<Character, CharacterMessage, CharacterCommand> (true, character)

        static member Facets =
            [typeof<RigidBodyFacet>]

        override this.Definitions (character, _) =
            [Entity.Size == v3Dup 2.0f
             Entity.Offset == v3 0.0f 1.0f 0.0f
             Entity.BodyType == KinematicCharacter
             Entity.SleepingAllowed == true
             Entity.CharacterProperties == character.CharacterProperties
             Entity.BodyShape == CapsuleShape { Height = 1.0f; Radius = 0.35f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.85f 0.0f)); PropertiesOpt = None }
             Entity.FollowTargetOpt := match character.CharacterType with Enemy -> Some Simulants.GameplayPlayer | Player -> None
             Entity.RegisterEvent => Register
             Game.KeyboardKeyDownEvent =|> fun evt -> UpdateInputKey evt.Data
             Entity.UpdateEvent => Update
             Game.PostUpdateEvent => SyncWeaponTransform]

        override this.Content (character, _) =

            [// animated model
             Content.entity<AnimatedModelDispatcher> Constants.Gameplay.CharacterAnimatedModelName
                [Entity.Size == v3Dup 2.0f
                 Entity.Offset == v3 0.0f 1.0f 0.0f
                 Entity.MaterialProperties == MaterialProperties.defaultProperties
                 Entity.AnimatedModel == Assets.Gameplay.JoanModel]

             // weapon
             Content.entity<RigidModelDispatcher> Constants.Gameplay.CharacterWeaponName
                [Entity.Offset == v3 0.0f 0.5f 0.0f
                 Entity.StaticModel == character.WeaponModel
                 Entity.BodyType == Static
                 Entity.BodyShape == BoxShape { Size = v3 0.3f 1.2f 0.3f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.6f 0.0f)); PropertiesOpt = None }
                 Entity.Sensor == true
                 Entity.NavShape == EmptyNavShape
                 Entity.Pickable == false
                 Entity.BodyCollisionEvent =|> fun evt -> WeaponCollide evt.Data
                 Entity.BodySeparationExplicitEvent =|> fun evt -> WeaponSeparateExplicit evt.Data
                 Entity.BodySeparationImplicitEvent =|> fun evt -> WeaponSeparateImplicit evt.Data]]

        override this.Message (character, message, entity, world) =

            match message with
            | UpdateInputKey keyboardKeyData ->
                let (jump, character) = Character.updateInputKey world.UpdateTime keyboardKeyData character
                withSignals (if jump then [Jump] else []) character

            | Update ->

                // update character
                let isKeyboardKeyDown keyboardKey = World.isKeyboardKeyDown keyboardKey world
                let nav3dFollow a b c d e f g = World.nav3dFollow a b c d e f g entity.Screen world
                let time = world.UpdateTime
                let position = entity.GetPosition world
                let rotation = entity.GetRotation world
                let linearVelocity = entity.GetLinearVelocity world
                let angularVelocity = entity.GetAngularVelocity world
                let bodyId = entity.GetBodyId world
                let grounded = World.getBodyGrounded bodyId world
                let playerPosition = Simulants.GameplayPlayer.GetPosition world
                let (soundOpt, animations, invisible, destroy, attackedCharacters, position, rotation, character) =
                    Character.update isKeyboardKeyDown nav3dFollow time position rotation linearVelocity angularVelocity grounded playerPosition character

                // deploy signals from update
                let signals = match soundOpt with Some sound -> [PlaySound (0L, Constants.Audio.SoundVolumeDefault, sound) :> Signal] | None -> []
                let signals = UpdateTransform (position, rotation) :> Signal :: UpdateAnimations (position, rotation, Array.ofList animations, invisible) :: signals
                let signals = match character.ActionState with WoundState _ -> PublishDie :> Signal :: signals | _ -> signals
                let signals = if attackedCharacters.Count > 0 then PublishAttacks attackedCharacters :> Signal :: signals else signals
                let signals = if destroy then Destroy :> Signal :: signals else signals
                withSignals signals character

            | WeaponCollide collisionData ->
                match collisionData.BodyShapeCollidee.BodyId.BodySource with
                | :? Entity as collidee when collidee.Is<CharacterDispatcher> world && collidee <> entity ->
                    let collideeCharacter = collidee.GetCharacter world
                    if character.CharacterType <> collideeCharacter.CharacterType then
                        let character = { character with WeaponCollisions = Set.add collidee character.WeaponCollisions }
                        just character
                    else just character
                | _ -> just character

            | WeaponSeparateExplicit separationData ->
                match separationData.BodyShapeSeparatee.BodyId.BodySource with
                | :? Entity as separatee when separatee.Is<CharacterDispatcher> world && separatee <> entity ->
                    let character = { character with WeaponCollisions = Set.remove separatee character.WeaponCollisions }
                    just character
                | _ -> just character

            | WeaponSeparateImplicit separationData ->
                match separationData.BodyId.BodySource with
                | :? Entity as separatee when separatee.Is<CharacterDispatcher> world ->
                    let character = { character with WeaponCollisions = Set.remove separatee character.WeaponCollisions }
                    just character
                | _ -> just character

        override this.Command (character, command, entity, world) =

            match command with
            | Register ->
                let animatedModel = entity / Constants.Gameplay.CharacterAnimatedModelName
                let world = animatedModel.SetAnimations [|Animation.loop GameTime.zero None "Armature|Idle"|] world
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
                let world = World.publish () entity.DieEvent entity world
                just world

            | Jump ->
                let bodyId = entity.GetBodyId world
                let world = World.jumpBody true character.JumpSpeed bodyId world
                just world

            | Destroy ->
                let world = World.destroyEntity entity world
                just world

            | CharacterCommand.PlaySound (delay, volume, sound) ->
                let world = World.schedule delay (World.playSound volume sound) entity world
                just world

        override this.RayCast (ray, entity, world) =
            let animatedModel = entity / Constants.Gameplay.CharacterAnimatedModelName
            match animatedModel.RayCast ray world with
            | [||] ->
                let weapon = entity / Constants.Gameplay.CharacterWeaponName
                weapon.RayCast ray world
            | intersections -> intersections

    type EnemyDispatcher () =
        inherit CharacterDispatcher (Character.initialEnemy)

    type PlayerDispatcher () =
        inherit CharacterDispatcher (Character.initialPlayer)