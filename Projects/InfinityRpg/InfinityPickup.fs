namespace InfinityRpg
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

type PickupType =
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
        { Pickup.initial with Position = vctovf coordinates }

    static member makeMagicMissile coordinates =
        { PickupType = Item (Special MagicMissile)
          PickupImage = Assets.Gameplay.MagicMissile
          Position = vctovf coordinates }