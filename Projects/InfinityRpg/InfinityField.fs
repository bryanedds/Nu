namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

type [<ReferenceEquality; NoComparison>] Field =
    { FieldMapNp : FieldMap }

    static member setFieldMap fieldMap field =
        { field with FieldMapNp = fieldMap }
    
    static member initial =
        let DefaultRand = Rand.make ()
        let DefaultSizeC = v2i 4 4
        let DefaultPathEdgesC = [(v2i 1 1, v2i 2 2)]
        let DefaultFieldMap = fst (FieldMap.make Assets.FieldTileSheetImage v2iZero DefaultSizeC DefaultPathEdgesC DefaultRand)
        { FieldMapNp = DefaultFieldMap }