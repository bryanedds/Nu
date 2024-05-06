namespace BlazeVector
open System
open System.Numerics
open Prime
open Nu

type BulletCommand =
    | Update
    | Collision
    interface Command

type BulletDispatcher () =
    inherit Entity2dDispatcher<int64, Message, BulletCommand> (true, false, false, fun world -> world.UpdateTime)

    static let [<Literal>] BulletLifeTime = 27L

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<StaticSpriteFacet>]

    override this.Definitions (_, _) =
        [Entity.Size == v3 16.0f 16.0f 0.0f
         Entity.Presence == Omnipresent
         Entity.Substance == Density 0.1f
         Entity.Restitution == 0.5f
         Entity.LinearDamping == 0.0f
         Entity.GravityOverride == Some v3Zero
         Entity.BodyType == Dynamic
         Entity.BodyShape == SphereShape { Radius = 0.5f; TransformOpt = None; PropertiesOpt = None }
         Entity.Observable == true
         Entity.StaticImage == Assets.Gameplay.PlayerBulletImage
         Entity.UpdateEvent => Update
         Entity.BodyCollisionEvent => Collision]

    override this.Command (startTime, command, entity, world) =
        match command with
        | Update ->
            let localTime = world.UpdateTime - startTime
            let world = if localTime = BulletLifeTime then World.destroyEntity entity world else world
            just world
        | Collision ->
            let world = World.destroyEntity entity world
            just world