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
    
    static member metaCoordinatesInDirection direction (metaMap : MetaMap) =
        metaMap.CurrentMetaCoordinates + dtovc direction
    
    static member nextMetaCoordinates (metaMap : MetaMap) =
        MetaMap.metaCoordinatesInDirection metaMap.Current.DirectionNext metaMap

    member this.NextPathStart =
        match this.Current.DirectionNext with
        | Upward -> v2i this.Current.PathEnd.X 0
        | Rightward -> v2i 0 this.Current.PathEnd.Y
        | _ -> failwithumf ()
    
    static member existsInDirection direction (metaMap : MetaMap) =
        let key = MetaMap.metaCoordinatesInDirection direction metaMap
        Map.containsKey key metaMap.MetaTiles
    
    static member nextMetaCoordinatesInDirection direction (metaMap : MetaMap) =
        MetaMap.nextMetaCoordinates metaMap = MetaMap.metaCoordinatesInDirection direction metaMap

    static member possibleInDirection direction (metaMap : MetaMap) =
        MetaMap.existsInDirection direction metaMap || MetaMap.nextMetaCoordinatesInDirection direction metaMap

    static member onPathBoundary coordinates (metaMap : MetaMap) =
        metaMap.Current.PathStart = coordinates || metaMap.Current.PathEnd = coordinates
    
    static member updateMetaTiles updater metaMap =
        { metaMap with MetaTiles = updater metaMap.MetaTiles }

    static member updateCurrentMetaCoordinates updater metaMap =
        { metaMap with CurrentMetaCoordinates = updater metaMap.CurrentMetaCoordinates }
    
    static member addMetaTile metaCoordinates metaTile metaMap =
        let metaMap = MetaMap.updateMetaTiles (Map.add metaCoordinates metaTile) metaMap
        MetaMap.updateCurrentMetaCoordinates (constant metaCoordinates) metaMap
    
    static member moveCurrent direction (metaMap : MetaMap) =
        MetaMap.updateCurrentMetaCoordinates (constant (MetaMap.metaCoordinatesInDirection direction metaMap)) metaMap
    
    static member makeMetaTile (metaMap : MetaMap) =
        let metaCoordinates = MetaMap.nextMetaCoordinates metaMap
        MetaMap.addMetaTile metaCoordinates (MetaTile.make metaMap.NextPathStart) metaMap
    
    static member transition direction (metaMap : MetaMap) =
        if MetaMap.existsInDirection direction metaMap
        then MetaMap.moveCurrent direction metaMap
        else MetaMap.makeMetaTile metaMap
    
    static member empty =
        { MetaTiles = Map.empty
          CurrentMetaCoordinates = v2iZero }
    
    static member make () =
        MetaMap.addMetaTile v2iZero (MetaTile.make v2iZero) MetaMap.empty