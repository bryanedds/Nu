namespace InfinityRpg
open System
open OpenTK
open TiledSharp
open Prime
open Nu

type MetaTile<'k when 'k : comparison> =
    { ClosedSides : Direction Set
      LockedSides : Map<Direction, 'k>
      Keys : 'k Set }

type MetaMap<'k when 'k : comparison>  =
    { NavigableSize : Vector2i
      PotentiallyNavigableTiles : Map<Vector2i, 'k MetaTile> }