namespace BlazeVector
open System
open System.Numerics
open Prime
open Nu
open BlazeVector

[<AutoOpen>]
module PlayerExtensions =
    type Entity with
        member this.GetLastTimeGrounded world : int64 = this.Get (nameof this.LastTimeGrounded) world
        member this.SetLastTimeGrounded (value : int64) world = this.Set (nameof this.LastTimeGrounded) value world
        member this.LastTimeGrounded = lens (nameof this.LastTimeGrounded) this this.GetLastTimeGrounded this.SetLastTimeGrounded
        member this.GetLastTimeJump world : int64 = this.Get (nameof this.LastTimeJump) world
        member this.SetLastTimeJump (value : int64) world = this.Set (nameof this.LastTimeJump) value world
        member this.LastTimeJump = lens (nameof this.LastTimeJump) this this.GetLastTimeJump this.SetLastTimeJump

type PlayerDispatcher () =
    inherit Entity2dDispatcherImSim (true, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<AnimatedSpriteFacet>]

    static member Properties =
        [define Entity.Size (v3 24.0f 48.0f 0.0f)
         define Entity.MountOpt None
         define Entity.BodyType DynamicCharacter
         define Entity.BodyShape (CapsuleShape { Height = 0.5f; Radius = 0.25f; TransformOpt = None; PropertiesOpt = None })
         define Entity.CharacterProperties (PogoSpring { CharacterPogoSpringProperties.defaultProperties with AdditionalSoftCollisionMask = 2UL }) // soft collision with enemies
         define Entity.CollisionMask "1" // don't hard collide with enemies in group "10"
         define Entity.Friction 0.0f
         define Entity.LinearDamping 3.0f
         define Entity.AngularFactor v3Zero
         define Entity.Gravity GravityWorld
         define Entity.CelCount 16
         define Entity.CelRun 4
         define Entity.CelSize (v2 48.0f 96.0f)
         define Entity.AnimationDelay (UpdateTime 3L)
         define Entity.AnimationSheet Assets.Gameplay.PlayerImage
         define Entity.LastTimeGrounded 0L
         define Entity.LastTimeJump 0L]

    override this.Process (entity, world) =

        // process walking
        let bodyId = entity.GetBodyId world
        if world.Advancing then
            let x =
                if World.isKeyboardKeyDown KeyboardKey.A world then -1f
                elif World.isKeyboardKeyDown KeyboardKey.D world then 1f
                else 0f
            World.applyBodyForce (v3 (x * Constants.Gameplay.PlayerWalkForce) 0f 0f) None bodyId world

        // process last time on ground
        if World.getBodyGrounded bodyId world then
            entity.SetLastTimeGrounded world.UpdateTime world

        // process shooting
        let fallen = (entity.GetPosition world).Y <= -320.0f
        if world.Advancing && not fallen && world.UpdateTime % 5L = 0L then
            let bullet = World.createEntity<BulletDispatcher> None NoOverlay None entity.Group world // OPTIMIZATION: NoOverlay to avoid reflection.
            bullet.SetPosition (entity.GetPosition world + v3 24.0f 1.0f 0.0f) world
            bullet.SetElevation (entity.GetElevation world) world
            bullet.SetCreationTime world.UpdateTime world
            World.applyBodyLinearImpulse (v3 Constants.Gameplay.BulletImpulse 0.0f 0.0f) None (bullet.GetBodyId world) world
            World.playSound 0.0f 0.0f 1.0f Assets.Gameplay.ShotSound world

        // process jumping
        if  world.Advancing &&
            world.UpdateTime >= entity.GetLastTimeJump world + 12L &&
            world.UpdateTime <= entity.GetLastTimeGrounded world + 10L &&
            World.isKeyboardKeyPressed KeyboardKey.Space world then
            entity.SetLastTimeJump world.UpdateTime world
            World.jumpBody false Constants.Gameplay.PlayerJumpForce (entity.GetBodyId world) world
            World.playSound 0.0f -0.5f 1.0f Assets.Gameplay.JumpSound world

        // process death
        if fallen then
            World.publish () entity.DeathEvent entity world