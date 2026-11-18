namespace TerraFirma
open System
open System.Numerics
open Prime
open Nu

// this extends the Entity API to expose the moving platform properties.
[<AutoOpen>]
module MovingPlatformExtensions =
    type Entity with
        member this.GetPlatformOriginOpt world : Vector3 option = this.Get (nameof this.PlatformOriginOpt) world
        member this.SetPlatformOriginOpt (value : Vector3 option) world = this.Set (nameof this.PlatformOriginOpt) value world
        member this.PlatformOriginOpt = lens (nameof this.PlatformOriginOpt) this this.GetPlatformOriginOpt this.SetPlatformOriginOpt
        member this.GetPlatformTravel world : Vector3 = this.Get (nameof this.PlatformTravel) world
        member this.SetPlatformTravel (value : Vector3) world = this.Set (nameof this.PlatformTravel) value world
        member this.PlatformTravel = lens (nameof this.PlatformTravel) this this.GetPlatformTravel this.SetPlatformTravel
        member this.GetPlatformPeriod world : single = this.Get (nameof this.PlatformPeriod) world
        member this.SetPlatformPeriod (value : single) world = this.Set (nameof this.PlatformPeriod) value world
        member this.PlatformPeriod = lens (nameof this.PlatformPeriod) this this.GetPlatformPeriod this.SetPlatformPeriod

// this is the dispatcher for a platform that travels back and forth, carrying whatever stands on it.
type MovingPlatformDispatcher () =
    inherit Entity3dDispatcher (true, false, false)

    // here we define the entity's facets
    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<StaticModelFacet>]

    // here we define default property values
    static member Properties =
        [define Entity.Scale (v3 3.0f 0.25f 3.0f)
         define Entity.BodyType Kinematic
         define Entity.BodyShape (BoxShape { Size = v3One; TransformOpt = None; PropertiesOpt = None })
         define Entity.StaticModel Assets.Default.StaticModel
         define Entity.PlatformOriginOpt None
         define Entity.PlatformTravel (v3 0.0f 4.0f 0.0f)
         define Entity.PlatformPeriod 8.0f]

    // here we define the platform's behavior
    override this.Update (entity, world) =
        if world.TimeAdvancing then

            // remember where the platform started from
            let origin =
                match entity.GetPlatformOriginOpt world with
                | Some origin -> origin
                | None ->
                    let origin = entity.GetPosition world
                    entity.SetPlatformOriginOpt (Some origin) world
                    origin

            // move the platform's body along its travel, easing in and out at each end. Moving its body rather than
            // setting its position gives it the velocity to carry whatever stands on it.
            let period = max (entity.GetPlatformPeriod world) 0.001f
            let progress = 0.5f - 0.5f * cos (MathF.Tau * world.ScaledTime.SecondsF / period)
            let center = origin + entity.GetPlatformTravel world * progress
            World.moveBody center (entity.GetRotation world) (entity.GetBodyId world) world
