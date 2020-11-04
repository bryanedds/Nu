namespace InfinityRpg
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

type PickupType =
    | Health

type [<ReferenceEquality; NoComparison>] Pickup =
    { PickupType : PickupType
      PickupSheet : Image AssetTag
      PickupSheetCoordinates : Vector2i
      Position : Vector2 }

    static member initial =
        { PickupType = Health
          PickupSheet = Assets.PickupSheetImage
          PickupSheetCoordinates = v2iZero
          Position = v2Zero }

    static member makeHealth coordinates =
        { Pickup.initial with Position = vctovf coordinates }