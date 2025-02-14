namespace BlazeVector
open System
open System.Numerics
open Prime
open Nu
open BlazeVector

type [<SymbolicExpansion>] Enemy =
    { Health : int }

type EnemyMessage =
    | Penetration of BodyPenetrationData
    interface Message

type EnemyCommand =
    | Update
    | Hit
    interface Command

[<AutoOpen>]
module EnemyExtensions =
    type Entity with
        member this.GetEnemy world : Enemy = this.GetModelGeneric<Enemy> world
        member this.SetEnemy enemy world = this.SetModelGeneric<Enemy> enemy world
        member this.Enemy = this.ModelGeneric<Enemy> ()
        member this.DieEvent = Events.DieEvent --> this

type EnemyDispatcher () =
    inherit Entity2dDispatcher<Enemy, EnemyMessage, EnemyCommand> (true, false, false, { Health = 7 })

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<AnimatedSpriteFacet>]

    override this.Definitions (_, _) =
        [Entity.Size == v3 24.0f 48.0f 0.0f
         Entity.BodyType == Dynamic
         Entity.BodyShape == CapsuleShape { Height = 0.5f; Radius = 0.25f; TransformOpt = None; PropertiesOpt = None }
         Entity.Friction == 0.0f
         Entity.LinearDamping == 3.0f
         Entity.AngularFactor == v3Zero
         Entity.GravityOverride == Some v3Zero
         Entity.CelCount == 6
         Entity.CelRun == 4
         Entity.CelSize == v2 48.0f 96.0f
         Entity.AnimationDelay == UpdateTime 8L
         Entity.AnimationSheet == Assets.Gameplay.EnemyImage
         Entity.Observable == true
         Entity.UpdateEvent => Update
         Entity.BodyPenetrationEvent =|> fun evt -> Penetration evt.Data]

    override this.Message (enemy, message, _, world) =
        match message with
        | Penetration penetration ->
            match penetration.BodyShapePenetratee.BodyId.BodySource with
            | :? Entity as penetratee when penetratee.Is<BulletDispatcher> world ->
                let enemy = { enemy with Health = dec enemy.Health }
                withSignal Hit enemy
            | _ -> just enemy

    override this.Command (enemy, command, entity, world) =
        match command with
        | Update ->
            let world =
                let eyeBounds = World.getEye2dBounds world
                let entityBounds = entity.GetBounds world
                if entityBounds.Box2.Intersects eyeBounds
                then World.applyBodyForce Constants.Gameplay.EnemyWalkForce None (entity.GetBodyId world) world
                else world
            let world =
                if enemy.Health <= 0 then
                    let world = World.publish entity entity.DieEvent entity world
                    let world = World.destroyEntity entity world
                    World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.ExplosionSound world
                    world
                else world
            just world
        | Hit ->
            World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.HitSound world
            just world