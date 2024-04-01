namespace BlazeVector
open System
open Prime
open Nu

[<AutoOpen>]
module EnemyDispatcher =

    type [<SymbolicExpansion>] Enemy =
        { Health : int }

    type EnemyMessage =
        | Collision of BodyCollisionData
        interface Message

    type EnemyCommand =
        | Update
        | Hit
        interface Command

    type Entity with
        member this.GetEnemy world : Enemy = this.GetModelGeneric<Enemy> world
        member this.SetEnemy enemy world = this.SetModelGeneric<Enemy> enemy world
        member this.Enemy = this.ModelGeneric<Enemy> ()
        member this.DieEvent = Events.DieEvent --> this

    type EnemyDispatcher () =
        inherit Entity2dDispatcher<Enemy, EnemyMessage, EnemyCommand> (true, { Health = 7 })

        static let WalkForce = v3 -300.0f -1500.0f 0.0f

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<AnimatedSpriteFacet>]

        override this.Initialize (_, _) =
            [Entity.Size == v3 24.0f 48.0f 0.0f
             Entity.Friction == 0.0f
             Entity.AngularFactor == v3Zero
             Entity.LinearDamping == 3.0f
             Entity.GravityOverride == Some v3Zero
             Entity.BodyType == Dynamic
             Entity.BodyShape == CapsuleShape { Height = 0.5f; Radius = 0.25f; TransformOpt = None; PropertiesOpt = None }
             Entity.CelCount == 6
             Entity.CelRun == 4
             Entity.CelSize == v2 48.0f 96.0f
             Entity.AnimationDelay == UpdateTime 8L
             Entity.AnimationSheet == Assets.Gameplay.EnemyImage
             Entity.UpdateEvent => Update
             Entity.BodyCollisionEvent =|> fun evt -> Collision evt.Data]

        override this.Message (enemy, message, _, world) =

            match message with
            | Collision collision ->
                match collision.BodyShapeCollidee.BodyId.BodySource with
                | :? Entity as collidee when collidee.Is<BulletDispatcher> world ->
                    let enemy = { enemy with Health = dec enemy.Health }
                    withSignal Hit enemy
                | _ -> just enemy

        override this.Command (enemy, command, entity, world) =

            match command with
            | Update ->
                let world =
                    if entity.GetInView2dRelative world
                    then World.applyBodyForce WalkForce v3Zero (entity.GetBodyId world) world
                    else world
                let world =
                    if enemy.Health <= 0 then
                        let world = World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.ExplosionSound world
                        let world = World.publish () entity.DieEvent entity world
                        World.destroyEntity entity world
                    else world
                just world

            | Hit ->
                let world = World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.HitSound world
                just world