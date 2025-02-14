namespace BlazeVector
open System
open System.Numerics
open Prime
open Nu
open BlazeVector

[<AutoOpen>]
module BulletExtensions =
    type Entity with
        member this.GetCreationTime world : int64 = this.Get (nameof this.CreationTime) world
        member this.SetCreationTime (value : int64) world = this.Set (nameof this.CreationTime) value world
        member this.CreationTime = lens (nameof this.CreationTime) this this.GetCreationTime this.SetCreationTime

type BulletDispatcher () =
    inherit Entity2dDispatcherImNui (true, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<StaticSpriteFacet>]

    static member Properties =
        [define Entity.Size (v3 16.0f 16.0f 0.0f)
         define Entity.BodyType Dynamic
         define Entity.BodyShape (SphereShape { Radius = 0.5f; TransformOpt = None; PropertiesOpt = None })
         define Entity.Restitution 0.5f
         define Entity.LinearDamping 0.0f
         define Entity.Substance (Density 0.1f)
         define Entity.GravityOverride (Some v3Zero)
         define Entity.Observable true
         define Entity.StaticImage Assets.Gameplay.PlayerBulletImage
         define Entity.CreationTime 0L]

    override this.Process (entity, world) =

        // process impact
        let localTime = world.UpdateTime - entity.GetCreationTime world
        let (penetrations, world) = World.doSubscription "Penetration" entity.BodyPenetrationEvent world
        if localTime = Constants.Gameplay.BulletLifeTime || FQueue.notEmpty penetrations
        then World.destroyEntity entity world
        else world