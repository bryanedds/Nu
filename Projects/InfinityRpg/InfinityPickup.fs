namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

type PickupType =
    | Health

type [<ReferenceEquality; NoComparison>] Pickup =
    { PickupType : PickupType
      PickupSheet : Image AssetTag
      PickupSheetPositionM : Vector2i
      Position : Vector2 }

    static member initial =
        { PickupType = Health
          PickupSheet = Assets.PickupSheetImage
          PickupSheetPositionM = v2iZero
          Position = v2Zero }

    static member makeHealth positionM =
        { Pickup.initial with Position = vmtovf positionM }