namespace TerraFirma
open System
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module CharacterDispatcher =

    type Entity with
        member this.GetCharacter world = this.GetModelGeneric<Character> world
        member this.SetCharacter value world = this.SetModelGeneric<Character> value world
        member this.Character = this.ModelGeneric<Character> ()

    type CharacterDispatcher () =
        inherit Entity3dDispatcher<Character, CharacterMessage, CharacterCommand> (true, Character.initial v3Zero quatIdentity)

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<FollowerFacet>]

        override this.Initialize (character, entity) =
            [Entity.BodyType == KinematicCharacter
             Entity.SleepingAllowed == true
             Entity.CharacterProperties == if not character.Player then { CharacterProperties.defaultProperties with PenetrationDepthMax = 0.1f } else CharacterProperties.defaultProperties
             Entity.BodyShape == CapsuleShape { Height = 1.0f; Radius = 0.35f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.85f 0.0f)); PropertiesOpt = None }
             Entity.FollowTargetOpt := if not character.Player then Some Simulants.GameplayPlayer else None
             Game.KeyboardKeyChangeEvent =|> fun evt -> UpdateInputKey evt.Data
             Entity.UpdateEvent => UpdateMessage
             entity.Group.PostUpdateEvent => SyncWeaponTransform
             Entity.Transform.ChangeEvent => SyncChildTransformsWhileHalted]

        override this.Content (character, _) =
            [Content.entity<AnimatedModelDispatcher> "AnimatedModel"
                [Entity.Size == v3Dup 2.0f
                 Entity.Offset == v3 0.0f 1.0f 0.0f
                 Entity.MountOpt == None
                 Entity.MaterialProperties == MaterialProperties.defaultProperties
                 Entity.AnimatedModel == Assets.Gameplay.JoanModel
                 Entity.Transform.ChangeEvent => SyncTransformWhileHalted]
             Content.entity<RigidModelDispatcher> "Weapon"
                [Entity.Scale == v3 1.0f 1.0f 1.0f
                 Entity.MountOpt == None
                 Entity.StaticModel == character.WeaponModel
                 Entity.BodyType == Static
                 Entity.BodyShape == BoxShape { Size = v3 0.3f 1.2f 0.3f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.6f 0.0f)); PropertiesOpt = None }
                 Entity.Sensor == true
                 Entity.Pickable == false]]

        override this.Message (character, message, entity, world) =

            match message with
            | UpdateMessage ->
                let mutable transform = entity.GetTransform world
                let position = transform.Position
                let rotation = transform.Rotation
                let linearVelocity = entity.GetLinearVelocity world
                let angularVelocity = entity.GetAngularVelocity world
                let bodyId = entity.GetBodyId world
                let grounded = World.getBodyGrounded bodyId world
                let character = Character.updateInterps position rotation (entity.GetLinearVelocity world) (entity.GetAngularVelocity world) character
                let character = Character.updateJumpState world.UpdateTime grounded character
                let (attackedCharacters, animations, character) = Character.update world.UpdateTime rotation linearVelocity angularVelocity character world
                let (position, rotation) = Character.updateInputScan position rotation character Simulants.GameplayPlayer world
                let character =
                    match character.ActionState with
                    | NormalState when not character.Player ->
                        let playerPosition = Simulants.GameplayPlayer.GetPosition world
                        if  Vector3.Distance (position, playerPosition) < 1.5f &&
                            rotation.Forward.AngleBetween (playerPosition - position) < 0.5f then
                            { character with ActionState = AttackState (AttackState.make world.UpdateTime) }
                        else character
                    | _ -> character
                withSignals [UpdateAnimatedModel (position, rotation, animations); PublishCharactersAttacked attackedCharacters] character

            | UpdateInputKey keyboardKeyData ->
                let (jump, character) = Character.updateInputKey world.UpdateTime keyboardKeyData character
                withSignals (if jump then [Jump] else []) character

        override this.Command (character, command, entity, world) =

            match command with
            | UpdateAnimatedModel (position, rotation, animations) ->
                let animatedModel = entity / "AnimatedModel"
                let world = animatedModel.SetPosition position world
                let world = animatedModel.SetRotation rotation world
                let world = animatedModel.SetAnimations animations world
                just world

            | SyncWeaponTransform ->
                let animatedModel = entity / "AnimatedModel"
                let weapon = entity / "Weapon"
                match (animatedModel.GetBoneOffsetsOpt world, animatedModel.GetBoneTransformsOpt world) with
                | (Some offsets, Some transforms) ->
                    let weaponHand =
                        Matrix4x4.CreateTranslation (v3 0.0f 0.0f 0.02f) *
                        Matrix4x4.CreateFromAxisAngle (v3Forward, MathF.PI_OVER_2) *
                        offsets.[Constants.Gameplay.CharacterWeaponHandBoneIndex].Inverted *
                        transforms.[Constants.Gameplay.CharacterWeaponHandBoneIndex] *
                        animatedModel.GetAffineMatrix world
                    let world = weapon.SetPosition weaponHand.Translation world
                    let world = weapon.SetRotation weaponHand.Rotation world
                    just world
                | (_, _) -> just world

            | PublishCharactersAttacked attackedCharacters ->
                let world = World.publish attackedCharacters (Events.CharactersAttacked --> entity) entity world
                just world

            | SyncChildTransformsWhileHalted ->
                if world.Halted then
                    let animatedModel = entity / "AnimatedModel"
                    let mutable transform = entity.GetTransform world
                    let world = animatedModel.SetPosition transform.Position world
                    let world = animatedModel.SetRotation transform.Rotation world
                    withSignals [SyncWeaponTransform] world
                else just world

            | SyncTransformWhileHalted ->
                if world.Halted then
                    let animatedModel = entity / "AnimatedModel"
                    let mutable transform = animatedModel.GetTransform world
                    let world = entity.SetPosition transform.Position world
                    let world = entity.SetRotation transform.Rotation world
                    just world
                else just world

            | Jump ->
                let bodyId = entity.GetBodyId world
                let world = World.jumpBody true character.JumpSpeed bodyId world
                just world