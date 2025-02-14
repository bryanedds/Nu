namespace BlazeVector
open System
open System.Numerics
open Prime
open Nu
open BlazeVector

[<AutoOpen>]
module EnemyExtensions =
    type Entity with
        member this.GetHealth world : int = this.Get (nameof this.Health) world
        member this.SetHealth (value : int) world = this.Set (nameof this.Health) value world
        member this.Health = lens (nameof this.Health) this this.GetHealth this.SetHealth
        member this.DieEvent = Events.DieEvent --> this

type EnemyDispatcher () =
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
         define Entity.CelCount 6
         define Entity.CelRun 4
         define Entity.CelSize (v2 48.0f 96.0f)
         define Entity.AnimationDelay (UpdateTime 8L)
         define Entity.AnimationSheet Assets.Gameplay.EnemyImage
         define Entity.Observable true
         define Entity.Health 7]

    override this.Process (entity, world) =

        // process walking
        let world =
            let eyeBounds = World.getEye2dBounds world
            let entityBounds = entity.GetBounds world
            if world.Advancing && entityBounds.Box2.Intersects eyeBounds
            then World.applyBodyForce Constants.Gameplay.EnemyWalkForce None (entity.GetBodyId world) world
            else world

        // process hits
        let (penetrations, world) = World.doSubscription "Penetration" entity.BodyPenetrationEvent world
        let hits =
            Seq.filter (fun penetration ->
                match penetration.BodyShapePenetratee.BodyId.BodySource with
                | :? Entity as penetratee -> penetratee.Is<BulletDispatcher> world
                | _ -> false)
                penetrations
        let world =
            if Seq.notEmpty hits then
                let world = entity.Health.Map dec world
                World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.HitSound world
                world
            else world

        // process death
        let world =
            if entity.GetHealth world <= 0 then
                let world = World.publish entity entity.DieEvent entity world
                let world = World.destroyEntity entity world
                World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.ExplosionSound world
                world
            else world

        // fin
        world