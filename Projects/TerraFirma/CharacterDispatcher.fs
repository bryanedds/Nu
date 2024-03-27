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
        inherit Entity3dDispatcher<Character, Message, CharacterCommand> (true, Character.initial v3Zero quatIdentity)

        static member Facets =
            [typeof<RigidBodyFacet>]

        override this.Initialize (character, entity) =
            [Entity.Position := character.Position
             Entity.Rotation := character.Rotation
             Entity.LinearVelocity := character.LinearVelocity
             Entity.AngularVelocity := character.AngularVelocity
             Entity.MountOpt == None
             Entity.Persistent == false
             Entity.BodyType == KinematicCharacter
             Entity.SleepingAllowed == true
             Entity.CharacterProperties := character.CharacterProperties
             Entity.BodyShape := character.BodyShape
             Entity.ModelDriven == true
             entity.Group.PostUpdateEvent => PostUpdate]

        override this.Content (character, _) =
            [Content.entity<AnimatedModelDispatcher> "AnimatedModel"
                [Entity.Position := character.PositionInterp
                 Entity.Rotation := character.RotationInterp
                 Entity.Size == v3Dup 2.0f
                 Entity.Offset == v3 0.0f 1.0f 0.0f
                 Entity.MountOpt == None
                 Entity.MaterialProperties == MaterialProperties.defaultProperties
                 Entity.Animations := character.Animations
                 Entity.AnimatedModel := character.AnimatedModel]
             Content.entity<RigidModelDispatcher> "Weapon"
                [Entity.Scale == v3 1.0f 0.1f 0.01f
                 Entity.MountOpt == None
                 Entity.BodyType == Static
                 Entity.BodyShape == SphereShape { Radius = 0.5f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.25f 0.0f)); PropertiesOpt = None }
                 Entity.Sensor == true
                 Entity.ModelDriven == true]]

        override this.Command (_, command, entity, world) =
            match command with
            | PostUpdate ->
                let animatedModel = entity / "AnimatedModel"
                let weapon = entity / "Weapon"
                match (animatedModel.GetBoneOffsetsOpt world, animatedModel.GetBoneTransformsOpt world) with
                | (Some offsets, Some transforms) ->
                    let weaponHand =
                        Matrix4x4.CreateTranslation (v3 0.4f 0.0f 0.02f) *
                        offsets.[Constants.Gameplay.CharacterWeaponHandBoneIndex].Inverted *
                        transforms.[Constants.Gameplay.CharacterWeaponHandBoneIndex] *
                        animatedModel.GetAffineMatrix world
                    let world = weapon.SetPosition weaponHand.Translation world
                    let world = weapon.SetRotation weaponHand.Rotation world
                    just world
                | (_, _) -> just world