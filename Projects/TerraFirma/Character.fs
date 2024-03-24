namespace TerraFirma
open System
open System.Numerics
open Prime
open Nu

type JumpState =
    { LastTime : int64
      LastTimeOnGround : int64 }

    static member initial =
        { LastTime = 0L
          LastTimeOnGround = 0L }

type AttackState =
    { AttackTime : int64
      FollowUpBuffered : bool }

    static member make time =
        { AttackTime = time
          FollowUpBuffered = false }

type Character =
    { Position : Vector3
      Rotation : Quaternion
      LinearVelocity : Vector3
      LinearVelocityPrevious : Vector3
      AngularVelocity : Vector3
      AngularVelocityPrevious : Vector3
      Animations : Animation array
      Jump : JumpState
      AttackOpt : AttackState option
      AnimatedModel : AnimatedModel AssetTag }

    static member initial =
        { Position = v3Zero
          Rotation = quatIdentity
          LinearVelocity = v3Zero
          LinearVelocityPrevious = v3Zero
          AngularVelocity = v3Zero
          AngularVelocityPrevious = v3Zero
          Animations = [||]
          Jump = JumpState.initial
          AttackOpt = None
          AnimatedModel = Assets.Gameplay.JoanModel }

[<AutoOpen>]
module CharacterDispatcher =

    type Entity with
        member this.GetCharacter world = this.GetModelGeneric<Character> world
        member this.SetCharacter value world = this.SetModelGeneric<Character> value world
        member this.Character = this.ModelGeneric<Character> ()

    type CharacterDispatcher () =
        inherit Entity3dDispatcher<Character, Message, Command> (true, Character.initial)

        static member Facets =
            [typeof<AnimatedModelFacet>
             typeof<RigidBodyFacet>]

        override this.Initialize (character, _) =
            [Entity.Position := character.Position
             Entity.Rotation := character.Rotation
             Entity.LinearVelocity := character.LinearVelocity
             Entity.AngularVelocity := character.AngularVelocity
             Entity.MaterialProperties == MaterialProperties.defaultProperties
             Entity.Animations := character.Animations
             Entity.AnimatedModel := character.AnimatedModel
             Entity.BodyType == KinematicCharacter
             Entity.SleepingAllowed == false
             Entity.BodyShape == CapsuleShape { Height = 1.0f; Radius = 0.35f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.85f 0.0f)); PropertiesOpt = None }
             Entity.ModelDriven == true]

        override this.Update (entity, world) =
            let world = base.Update (entity, world)
            let position = entity.GetPosition world
            let rotation = entity.GetRotation world
            let linearVelocity = entity.GetLinearVelocity world
            let angularVelocity = entity.GetAngularVelocity world
            let bodyId = entity.GetBodyId world
            let world = World.setBodyCenter position bodyId world
            let world = World.setBodyRotation rotation bodyId world
            let world = World.setBodyLinearVelocity linearVelocity bodyId world
            let world = World.setBodyAngularVelocity angularVelocity bodyId world
            world