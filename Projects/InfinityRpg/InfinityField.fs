namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

type [<ReferenceEquality; NoComparison>] Prop' =
    | Obstruction of Guid

type [<StructuralEquality; NoComparison>] Occupant =
    | Character of Character
    | Prop' of Prop'

    static member getOccupantIndex occupant =
        match occupant with
        | Character character ->
            match character.CharacterIndex with
            | PlayerIndex -> PlayerIndex'
            | EnemyIndex index -> EnemyIndex' index
        | Prop' prop ->
            match prop with
            | Obstruction index -> PropIndex' index

and [<StructuralEquality; StructuralComparison>] OccupantIndex =
    | PlayerIndex'
    | EnemyIndex' of int
    | PropIndex' of Guid
    | PlaceholderIndex' // indicates where the avatar intends to step next so enemies won't move there

[<AutoOpen>]
module Field =

    type [<ReferenceEquality; NoComparison>] Field =
        private
            { FieldMap_ : FieldMap
              OccupantIndices : Map<Vector2i, OccupantIndex>
              OccupantPositions : Map<OccupantIndex, Vector2i>
              Occupants : Map<OccupantIndex, Occupant> }

        member this.FieldMap = this.FieldMap_

    let tryGetOccupant occupantIndex field =
        field.Occupants.TryFind occupantIndex

    let tryGetOccupantAt positionC field =
        match field.OccupantIndices.TryGetValue positionC with
        | (true, occupantIndex) -> tryGetOccupant occupantIndex field
        | (false, _) -> None

    let getOccupant occupantIndex field =
        field.Occupants.[occupantIndex]

    let getOccupantAt positionC field =
        field.Occupants.[field.OccupantIndices.[positionC]]

    let updateOccupant updater occupantIndex field =
        match field.Occupants.TryGetValue occupantIndex with
        | (true, occupant) ->
            let occupant = updater occupant
            { field with Occupants = Map.add occupantIndex occupant field.Occupants }
        | (false, _) -> field

    let updateOccupantAt updater (positionC : Vector2i) field =
        match field.OccupantIndices.TryGetValue positionC with
        | (true, occupantIndex) -> updateOccupant updater occupantIndex field
        | (false, _) -> field

    let removeOccupant occupantIndex field =
        match field.OccupantPositions.TryGetValue occupantIndex with
        | (true, positionC) ->
            { field with
                OccupantIndices = Map.remove positionC field.OccupantIndices
                OccupantPositions = Map.remove occupantIndex field.OccupantPositions
                Occupants = Map.remove occupantIndex field.Occupants }
        | (false, _) -> field

    let removeOccupantAt positionC field =
        match field.OccupantIndices.TryGetValue positionC with
        | (true, occupantIndex) ->
            { field with
                OccupantIndices = Map.remove positionC field.OccupantIndices
                OccupantPositions = Map.remove occupantIndex field.OccupantPositions
                Occupants = Map.remove occupantIndex field.Occupants }
        | (false, _) -> field

    let addOccupant (positionC : Vector2i) occupant field =
        if  positionC.X >= 0 && positionC.X < field.FieldMap_.FieldSizeC.X &&
            positionC.Y >= 0 && positionC.Y < field.FieldMap_.FieldSizeC.Y then
            let occupantIndex = Occupant.getOccupantIndex occupant
            { field with
                OccupantIndices = Map.add positionC occupantIndex field.OccupantIndices
                OccupantPositions = Map.add occupantIndex positionC field.OccupantPositions
                Occupants = Map.add occupantIndex occupant field.Occupants }
        else field

    let initial =
        { FieldMap_ = FieldMap.initial
          OccupantIndices = Map.empty
          OccupantPositions = Map.empty
          Occupants = Map.empty }

    let make fieldMap =
        let field =
            Map.fold (fun field positionC tile ->
                match tile.TileType with
                | Impassable -> addOccupant positionC (Prop' (Obstruction Gen.id)) field
                | Passable -> field)
                { initial with FieldMap_ = fieldMap }
                fieldMap.FieldTiles
        field

type Field = Field.Field