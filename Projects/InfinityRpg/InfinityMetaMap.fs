namespace InfinityRpg
open System
open Prime
open Nu

type [<ReferenceEquality; NoComparison>] MetaTile =
    { RandSeed : uint64
      OffsetCount : Vector2i
      IsHorizontal : bool
      PathStart : Vector2i
      PathEnd : Vector2i }

    static member make MetaTileOpt =
        let sysrandom = System.Random ()
        let randSeed = uint64 (sysrandom.Next ())
        let randResult = Gen.random1 (Constants.Layout.FieldMapSizeC.X - 4) // assumes X and Y are equal
        let pathEnd =
            if randResult % 2 = 0
            then v2i (randResult + 2) (Constants.Layout.FieldMapSizeC.Y - 2)
            else v2i (Constants.Layout.FieldMapSizeC.X - 2) (randResult + 2)
        let (offsetCount, pathStart) =
            match MetaTileOpt with
            | Some metaTile ->
                match metaTile.IsHorizontal with
                | true -> (metaTile.OffsetCount + v2iRight, v2i 1 metaTile.PathEnd.Y)
                | false -> (metaTile.OffsetCount + v2iUp, v2i metaTile.PathEnd.X 1)
            | None -> (v2iZero, v2iOne)
        { RandSeed = randSeed
          OffsetCount = offsetCount
          IsHorizontal = pathEnd.X > pathEnd.Y
          PathStart = pathStart
          PathEnd = pathEnd }

type [<ReferenceEquality; NoComparison>] MetaMap =
    { MetaTiles : Map<Vector2i, MetaTile>
      CurrentFieldOffset : Vector2i }

    static member empty =
        { MetaTiles = Map.empty
          CurrentFieldOffset = v2iZero }

    member this.AddMetaTile metaTile =
        let metaTiles = Map.add metaTile.OffsetCount metaTile this.MetaTiles
        { this with MetaTiles = metaTiles; CurrentFieldOffset = metaTile.OffsetCount }

    member this.Current =
        this.MetaTiles.[this.CurrentFieldOffset]

    member this.OffsetInDirection direction =
        this.CurrentFieldOffset + dtovc direction
    
    member this.ExistsInDirection direction =
        Map.containsKey (this.OffsetInDirection direction) this.MetaTiles
    
    member this.NextOffset =
        if this.Current.IsHorizontal
        then this.OffsetInDirection Rightward
        else this.OffsetInDirection Upward
    
    member this.NextOffsetInDirection direction =
        this.NextOffset = this.OffsetInDirection direction

    member this.PossibleInDirection direction =
        this.ExistsInDirection direction || this.NextOffsetInDirection direction
    
    member this.MoveCurrent direction =
        { this with CurrentFieldOffset = this.OffsetInDirection direction }
    
    member this.MakeMetaTile =
        this.AddMetaTile (MetaTile.make (Some this.Current))
    
    member this.Transition direction =
        if this.ExistsInDirection direction
        then this.MoveCurrent direction
        else this.MakeMetaTile
    
    static member make =
        MetaMap.empty.AddMetaTile (MetaTile.make None)