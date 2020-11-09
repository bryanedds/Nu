namespace InfinityRpg
open System
open Prime
open Nu

type [<ReferenceEquality; NoComparison>] MetaTile<'k when 'k : comparison> =
    { ClosedSides : Direction Set
      LockedSides : Map<Direction, 'k>
      Keys : 'k Set }

type [<ReferenceEquality; NoComparison>] MetaMap<'k when 'k : comparison>  =
    { NavigableSize : Vector2i
      PotentiallyNavigableTiles : Map<Vector2i, 'k MetaTile> }