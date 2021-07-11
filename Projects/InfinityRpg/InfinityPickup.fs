namespace InfinityRpg
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

// TODO: see if this could be a PropType rather than its own type.
type [<StructuralEquality; NoComparison>] PickupType =
    | Health
    | Item of ItemType

type [<ReferenceEquality; NoComparison>] Pickup =
    { PickupType : PickupType
      PickupImage : Image AssetTag
      Position : Vector2 }

    static member initial =
        { PickupType = Health
          PickupImage = Assets.Gameplay.HealthPickupImage
          Position = v2Zero }

    static member makeHealth coordinates =
        { PickupType = Health
          PickupImage = Assets.Gameplay.HealthPickupImage
          Position = vctovf coordinates }

    static member makeMagicMissile coordinates =
        { PickupType = Item (Special MagicMissile)
          PickupImage = Assets.Gameplay.MagicMissile
          Position = vctovf coordinates }

    static member ofPickupType pickupType coordinates =
        match pickupType with
        | Health -> Pickup.makeHealth coordinates
        | Item _ -> Pickup.makeMagicMissile coordinates