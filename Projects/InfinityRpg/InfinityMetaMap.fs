namespace InfinityRpg
open System
open Prime
open Nu

type [<ReferenceEquality; NoComparison>] MetaTile =
    { RandSeed : uint64
      DirectionNext : Direction
      PathStart : Vector2i
      PathEnd : Vector2i }

    static member make pathStart =
        let sysrandom = System.Random ()
        let randSeed = uint64 (sysrandom.Next ())
        let directionNext = if Gen.random1 2 = 0 then Upward else Rightward
        let randResult = Gen.random1 (Constants.Layout.FieldMapSizeC.X - 4) // assumes X and Y are equal
        let pathEnd =
            match directionNext with
            | Upward -> v2i (randResult + 2) (Constants.Layout.FieldMapSizeC.Y - 1)
            | Rightward -> v2i (Constants.Layout.FieldMapSizeC.X - 1) (randResult + 2)
            | _ -> failwithumf ()
        { RandSeed = randSeed
          DirectionNext = directionNext
          PathStart = pathStart
          PathEnd = pathEnd }

type [<ReferenceEquality; NoComparison>] MetaMap =
    { MetaTiles : Map<Vector2i, MetaTile>
      CurrentMetaCoordinates : Vector2i }

    member this.Current =
        this.MetaTiles.[this.CurrentMetaCoordinates]
    
    member this.MetaCoordinatesInDirection direction =
        this.CurrentMetaCoordinates + dtovc direction
    
    member this.NextMetaCoordinates =
        this.MetaCoordinatesInDirection this.Current.DirectionNext

    member this.NextPathStart =
        match this.Current.DirectionNext with
        | Upward -> v2i this.Current.PathEnd.X 0
        | Rightward -> v2i 0 this.Current.PathEnd.Y
        | _ -> failwithumf ()
    
    member this.ExistsInDirection direction =
        Map.containsKey (this.MetaCoordinatesInDirection direction) this.MetaTiles
    
    member this.NextMetaCoordinatesInDirection direction =
        this.NextMetaCoordinates = this.MetaCoordinatesInDirection direction

    member this.PossibleInDirection direction =
        this.ExistsInDirection direction || this.NextMetaCoordinatesInDirection direction

    member this.OnPathBoundary coordinates =
        this.Current.PathStart = coordinates || this.Current.PathEnd = coordinates
    
    static member updateMetaTiles updater metaMap =
        { metaMap with MetaTiles = updater metaMap.MetaTiles }

    static member updateCurrentMetaCoordinates updater metaMap =
        { metaMap with CurrentMetaCoordinates = updater metaMap.CurrentMetaCoordinates }
    
    static member addMetaTile metaCoordinates metaTile metaMap =
        let metaMap = MetaMap.updateMetaTiles (Map.add metaCoordinates metaTile) metaMap
        MetaMap.updateCurrentMetaCoordinates (constant metaCoordinates) metaMap
    
    static member moveCurrent direction (metaMap : MetaMap) =
        MetaMap.updateCurrentMetaCoordinates (constant (metaMap.MetaCoordinatesInDirection direction)) metaMap
    
    static member makeMetaTile (metaMap : MetaMap) =
        MetaMap.addMetaTile metaMap.NextMetaCoordinates (MetaTile.make metaMap.NextPathStart) metaMap
    
    static member transition direction (metaMap : MetaMap) =
        if metaMap.ExistsInDirection direction
        then MetaMap.moveCurrent direction metaMap
        else MetaMap.makeMetaTile metaMap
    
    static member empty =
        { MetaTiles = Map.empty
          CurrentMetaCoordinates = v2iZero }
    
    static member make =
        MetaMap.addMetaTile v2iZero (MetaTile.make v2iZero) MetaMap.empty