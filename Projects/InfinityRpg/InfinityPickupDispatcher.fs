namespace InfinityRpg
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module PickupDispatcher =

    type Entity with
        member this.GetPickup = this.GetModelGeneric<Pickup>
        member this.SetPickup = this.SetModelGeneric<Pickup>
        member this.Pickup = this.ModelGeneric<Pickup> ()

    type PickupDispatcher () =
        inherit EntityDispatcher<Pickup, unit, unit> (Pickup.initial)

        static member Properties =
            [define Entity.Elevation Constants.Gameplay.PickupElevation
             define Entity.Omnipresent true]
        
        override this.Initializers (pickup, _) =
            [Entity.Position <== pickup --> fun pickup -> pickup.Position]
        
        override this.View (pickup, entity, world) =
            if entity.GetVisible world && entity.GetInView world then
                let transform = entity.GetTransform world
                Render (transform.Elevation, transform.Position.Y, AssetTag.generalize pickup.PickupImage,
                     SpriteDescriptor
                       { Transform = transform
                         Absolute = entity.GetAbsolute world
                         Offset = v2Zero
                         InsetOpt = None
                         Image = pickup.PickupImage
                         Color = Color.White
                         Blend = Transparent
                         Glow = Color.Zero
                         Flip = FlipNone })
            else View.empty