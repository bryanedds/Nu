namespace BlazeVector
open System
open System.Numerics
open Prime
open Nu
open BlazeVector

[<AutoOpen>]
module PlayerExtensions =
    type Entity with
        member this.GetLastTimeOnGround world : int64 = this.Get (nameof this.LastTimeOnGround) world
        member this.SetLastTimeOnGround (value : int64) world = this.Set (nameof this.LastTimeOnGround) value world
        member this.LastTimeOnGround = lens (nameof this.LastTimeOnGround) this this.GetLastTimeOnGround this.SetLastTimeOnGround
        member this.GetLastTimeJump world : int64 = this.Get (nameof this.LastTimeJump) world
        member this.SetLastTimeJump (value : int64) world = this.Set (nameof this.LastTimeJump) value world
        member this.LastTimeJump = lens (nameof this.LastTimeJump) this this.GetLastTimeJump this.SetLastTimeJump

type PlayerDispatcher () =
    inherit Entity2dDispatcherImNui (true, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<AnimatedSpriteFacet>]

    static member Properties =
        [define Entity.Size (v3 24.0f 48.0f 0.0f)
         define Entity.BodyType Dynamic
         define Entity.BodyShape (CapsuleShape { Height = 0.5f; Radius = 0.25f; TransformOpt = None; PropertiesOpt = None })
         define Entity.Friction 0.0f
         define Entity.LinearDamping 3.0f
         define Entity.AngularFactor v3Zero
         define Entity.GravityOverride (Some v3Zero)
         define Entity.CelCount 16
         define Entity.CelRun 4
         define Entity.CelSize (v2 48.0f 96.0f)
         define Entity.AnimationDelay (UpdateTime 3L)
         define Entity.AnimationSheet Assets.Gameplay.PlayerImage
         define Entity.LastTimeOnGround 0L
         define Entity.LastTimeJump 0L]

    override this.Process (entity, world) =

        // grab body id
        let bodyId = entity.GetBodyId world

        // process walking
        let world =
            if world.Advancing then
                let groundTangentOpt = World.getBodyToGroundContactTangentOpt bodyId world
                let force =
                    match groundTangentOpt with
                    | Some groundTangent ->
                        let downForce = if groundTangent.Y > 0.0f then Constants.Gameplay.PlayerClimbForce else 0.0f
                        Vector3.Multiply (groundTangent, v3 Constants.Gameplay.PlayerWalkForce downForce 0.0f)
                    | None -> v3 Constants.Gameplay.PlayerWalkForce Constants.Gameplay.PlayerFallForce 0.0f
                World.applyBodyForce force None bodyId world
            else world

        // process last time on ground
        let world =
            if World.getBodyGrounded bodyId world
            then entity.SetLastTimeOnGround world.UpdateTime world
            else world

        // process shooting
        let fallen = (entity.GetPosition world).Y <= -320.0f
        let world =
            if world.Advancing && not fallen && world.UpdateTime % 5L = 0L then
                let (bullet, world) = World.createEntity<BulletDispatcher> NoOverlay None entity.Group world // OPTIMIZATION: NoOverlay to avoid reflection.
                let world = bullet.SetPosition (entity.GetPosition world + v3 24.0f 1.0f 0.0f) world
                let world = bullet.SetElevation (entity.GetElevation world) world
                let world = bullet.SetCreationTime world.UpdateTime world
                let world = World.applyBodyLinearImpulse (v3 Constants.Gameplay.BulletForce 0.0f 0.0f) None (bullet.GetBodyId world) world
                World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.ShotSound world
                world
            else world

        // process jumping
        let world =
            if  world.Advancing && 
                world.UpdateTime >= entity.GetLastTimeJump world + 12L &&
                world.UpdateTime <= entity.GetLastTimeOnGround world + 10L &&
                World.isKeyboardKeyPressed KeyboardKey.Space world then
                let world = entity.SetLastTimeJump world.UpdateTime world
                let world = World.applyBodyLinearImpulse (v3 0.0f Constants.Gameplay.PlayerJumpForce 0.0f) None (entity.GetBodyId world) world
                World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.JumpSound world
                world
            else world

        // process death
        let world = if fallen then World.publish entity entity.DieEvent entity world else world

        // fin
        world