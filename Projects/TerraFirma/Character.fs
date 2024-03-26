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

type [<ReferenceEquality>] Character =
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
            if rotations.Length > 1 then
                let unnormalized = Quaternion.Slerp (Seq.head rotations, Seq.last rotations, 0.5f)
                unnormalized.Normalized
            else this.Rotation
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

    static member initial position rotation =
        { Position = position
          Rotation = rotation
          LinearVelocity = v3Zero
          AngularVelocity = v3Zero
          PositionPrevious = Array.init 3 (fun _ -> position) |> Queue.ofSeq
          RotationPrevious = Array.init 3 (fun _ -> rotation) |> Queue.ofSeq
          LinearVelocityPrevious = Array.init 3 (fun _ -> v3Zero) |> Queue.ofSeq
          AngularVelocityPrevious = Array.init 3 (fun _ -> v3Zero) |> Queue.ofSeq
          Animations = [||]
          Jump = JumpState.initial
          AttackOpt = None
          WeaponHand = Matrix4x4.CreateFromTrs (position, rotation, v3Zero)
          BodyShape = CapsuleShape { Height = 1.0f; Radius = 0.35f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.85f 0.0f)); PropertiesOpt = None }
          CharacterProperties = CharacterProperties.defaultProperties
          AnimatedModel = Assets.Gameplay.JoanModel }

    static member initialPlayer position rotation =
        Character.initial position rotation

    static member initialEnemy position rotation =
        let enemy = Character.initial position rotation
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
        inherit Entity3dDispatcher<Character, Message, Command> (true, Character.initialPlayer v3Zero quatIdentity)

        static member Facets =
            [typeof<RigidBodyFacet>]

        override this.Initialize (character, _) =
            [Entity.Position := character.Position
             Entity.Rotation := character.Rotation
             Entity.LinearVelocity := character.LinearVelocity
             Entity.AngularVelocity := character.AngularVelocity
             Entity.Persistent == false
             Entity.BodyType == KinematicCharacter
             Entity.SleepingAllowed == true
             Entity.CharacterProperties := character.CharacterProperties
             Entity.BodyShape := character.BodyShape
             Entity.ModelDriven == true]

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
                [Entity.Position := character.WeaponHand.Translation
                 Entity.Rotation := character.WeaponHand.Rotation
                 Entity.Scale := v3 1.0f 0.1f 0.1f
                 Entity.MountOpt == None
                 Entity.BodyType == Static
                 Entity.BodyShape == SphereShape { Radius = 0.5f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.25f 0.0f)); PropertiesOpt = None }
                 Entity.Sensor == true
                 Entity.ModelDriven == true]]