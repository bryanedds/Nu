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
      PositionPrevious : Vector3 Queue
      Rotation : Quaternion
      RotationPrevious : Quaternion Queue
      LinearVelocity : Vector3
      LinearVelocityPrevious : Vector3 Queue
      AngularVelocity : Vector3
      AngularVelocityPrevious : Vector3 Queue
      Animations : Animation array
      Jump : JumpState
      AttackOpt : AttackState option
      FollowTargetOpt : Entity option
      WeaponHand : Matrix4x4
      BodyShape : BodyShape
      CharacterProperties : CharacterProperties
      AnimatedModel : AnimatedModel AssetTag }

    member this.PositionInterp =
        if not (Queue.isEmpty this.PositionPrevious) then
            let positions = Queue.conj this.Position this.PositionPrevious
            Seq.sum positions / single positions.Length
        else this.Position

    member this.RotationInterp =
        if not (Queue.isEmpty this.RotationPrevious) then
            let rotations = Queue.conj this.Rotation this.RotationPrevious
            Seq.reduce (fun a b -> Quaternion.Slerp (a, b, 0.5f)) rotations
        else this.Rotation

    member this.LinearVelocityInterp =
        if not (Queue.isEmpty this.LinearVelocityPrevious) then
            let linearVelocities = Queue.conj this.LinearVelocity this.LinearVelocityPrevious
            Seq.sum linearVelocities / single linearVelocities.Length
        else this.LinearVelocity

    member this.AngularVelocityInterp =
        if not (Queue.isEmpty this.AngularVelocityPrevious) then
            let angularVelocities = Queue.conj this.AngularVelocity this.AngularVelocityPrevious
            Seq.sum angularVelocities / single angularVelocities.Length
        else this.AngularVelocity

    member this.AnimatedModelAffineMatrix =
        Matrix4x4.CreateFromTrs (this.PositionInterp, this.RotationInterp, v3One)

    static member initial =
        { Position = v3Zero
          PositionPrevious = Queue.empty
          Rotation = quatIdentity
          RotationPrevious = Queue.empty
          LinearVelocity = v3Zero
          LinearVelocityPrevious = Queue.empty
          AngularVelocity = v3Zero
          AngularVelocityPrevious = Queue.empty
          Animations = [||]
          Jump = JumpState.initial
          AttackOpt = None
          FollowTargetOpt = None
          WeaponHand = m4Identity
          BodyShape = CapsuleShape { Height = 1.0f; Radius = 0.35f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.85f 0.0f)); PropertiesOpt = None }
          CharacterProperties = CharacterProperties.defaultProperties
          AnimatedModel = Assets.Gameplay.JoanModel }

    static member initialPlayer =
        Character.initial

    static member initialEnemy =
        let enemy = Character.initial
        { enemy with
            BodyShape = CapsuleShape { Height = 1.3f; Radius = 0.2f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.85f 0.0f)); PropertiesOpt = None }
            Character.CharacterProperties.PenetrationDepthMax = 0.1f } // deeper penetration needed to make enemies able to climb stairs

[<AutoOpen>]
module CharacterDispatcher =

    type Entity with
        member this.GetCharacter world = this.GetModelGeneric<Character> world
        member this.SetCharacter value world = this.SetModelGeneric<Character> value world
        member this.Character = this.ModelGeneric<Character> ()

    type CharacterDispatcher () =
        inherit Entity3dDispatcher<Character, Message, Command> (true, Character.initialPlayer)

        static member Facets =
            [typeof<AnimatedModelFacet>
             typeof<RigidBodyFacet>
             typeof<FollowerFacet>]

        override this.Initialize (character, _) =
            [Entity.Position := character.Position
             Entity.Rotation := character.Rotation
             Entity.Persistent == false
             Entity.LinearVelocity := character.LinearVelocity
             Entity.AngularVelocity := character.AngularVelocity
             Entity.MaterialProperties == MaterialProperties.defaultProperties
             Entity.Animations := character.Animations
             Entity.AnimatedModel := character.AnimatedModel
             Entity.AnimatedModelAffineMatrixOverride := Some character.AnimatedModelAffineMatrix
             Entity.BodyType == KinematicCharacter
             Entity.SleepingAllowed == true
             Entity.CharacterProperties := character.CharacterProperties
             Entity.BodyShape := character.BodyShape
             Entity.ModelDriven == true
             Entity.FollowTargetOpt := character.FollowTargetOpt
             Entity.FollowDistanceMinOpt == Some 1.25f
             Entity.FollowDistanceMaxOpt == Some 10.0f]

        override this.Register (entity, world) =
            let world = base.Register (entity, world)
            entity.AutoBounds world

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